(*
This module implements wires between symbol ports. Wires can be autorouted, or manually routed by dragging segments.
Moving symbols causes the corresponding wires to move.
Wires are read and written from Issie as lists of wire vertices, whatever teh internal representation is.
*)


module BusWire

open CommonTypes
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers

//Static Vars
let minSegLen = 5.

//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

/// Represents the orientation of a wire segment
type Orientation =  Horizontal | Vertical

///
type SnapPosition = High | Mid | Low

///
type WireType = Radial | Modern | Jump

/// Represents how a wire segment is currently being routed
type RoutingMode = Manual | Auto

///
type Segment = 
    {
        Id : SegmentId
        Index: int
        Length : float
        HostId: ConnectionId
        /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
        IntersectCoordinateList: list<float * SegmentId>
        Draggable : bool
        Mode : RoutingMode
    }



///
type Wire =
    {
        Id: ConnectionId 
        InputPort: InputPortId
        OutputPort: OutputPortId
        Color: HighLightColor
        Width: int
        Segments: list<Segment>
        Type : WireType
        StartPos : XYPos
        EndPos : XYPos
        InitialOrientation : Orientation
    }

    with static member stickLength = 16.0



///
type Model =
    {
        Symbol: Symbol.Model
        Wires: Map<ConnectionId, Wire>
        FromVerticalToHorizontalSegmentIntersections: Map<SegmentId, list<ConnectionId*SegmentId>>
        FromHorizontalToVerticalSegmentIntersections: Map<SegmentId, list<ConnectionId*SegmentId>>
        CopiedWires: Map<ConnectionId, Wire> 
        SelectedSegment: SegmentId
        LastMousePos: XYPos
        ErrorWires: list<ConnectionId>
        Notifications: Option<string>
        Type : WireType
    }

//----------------------------Message Type-----------------------------------//

///
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (InputPortId * OutputPortId)
    | BusWidths
    | CopyWires of list<ConnectionId>
    | DeleteWires of list<ConnectionId>
    | SelectWires of list<ConnectionId>
    | UpdateWires of list<ComponentId> * XYPos
    | DragWire of ConnectionId * MouseT
    | ColorWires of list<ConnectionId> * HighLightColor
    | ErrorWires of list<ConnectionId>
    | ResetJumps of list<ConnectionId>
    | MakeJumps of list<ConnectionId>
    | ResetModel // For Issie Integration
    | LoadConnections of list<Connection> // For Issie Integration


/// <summary> Applies a function which requires the segment start and end positions to the segments in a wire, 
/// threading an accumulator argument through the computation. Essentially a List.fold applied to the list of segments of a wire. </summary>
/// <remarks> This is used in cases where absolute segment positions are required. 
/// These positions are computed on the fly and passed to the folder function. </remarks>
/// <param name="folder"> The function to update the state given the segment start and end positions, current state and segment itself.</param>
/// <param name="state"> The initial state.</param>
/// <param name="wire"> The wire containing the segment list we are folding over.</param>
/// <returns> The final state value </returns>
let foldOverSegs folder state wire =
    let initPos = wire.StartPos
    let initOrientation = wire.InitialOrientation
    ((state, initPos, initOrientation), wire.Segments)
    ||> List.fold (fun (currState, currPos, currOrientation) seg -> 
        let (nextPos, nextOrientation) = 
            match currOrientation with
            | Horizontal -> { currPos with X = currPos.X + seg.Length }, Vertical
            | Vertical -> { currPos with Y = currPos.Y + seg.Length }, Horizontal
        let nextState = folder currPos nextPos currState seg
        (nextState, nextPos, nextOrientation))
    |> (fun (state, _, _) -> state)

//-------------------------Debugging functions---------------------------------//

/// Formats a SegmentId for logging purposes.
let formatSegmentId (Id: SegmentId) =
    Id
    |> (fun (SegmentId str) -> str)
    |> (fun str -> str[0..2])

/// Formats a WireId for logging purposes
let formatWireId (Id: ConnectionId) =
    Id
    |> (fun (ConnectionId str) -> str)
    |> (fun str -> str[0..2])

/// Logs the given SegmentId and returns it unchanged. Used for debugging.
let logSegmentId (Id:SegmentId) =
    printfn $"{formatSegmentId Id}"; Id

/// Logs the given Segment and returns it unchanged. Used for debugging.
let logSegment (seg:Segment) =
    printfn $"|{seg.Index}:{formatSegmentId seg.Id}|"; seg

/// Logs the given ConnectionId and returns it unchanged. Used for debugging.
let logConnectionId (Id:ConnectionId) =
        Id
        |> (fun (ConnectionId str) -> str)
        |> (fun str -> printfn $"{str[0..2]}"; Id)

/// Formats an intersection map for logging purposes.
let formatIntersectionMap (m:Map<SegmentId, (ConnectionId * SegmentId) list>) =
    m
    |> Map.toList
    |> List.map (fun (segId, lst) ->
        List.map (snd >> formatSegmentId) lst
        |> (fun segs -> sprintf $"""<{formatSegmentId segId}->[{String.concat ";" segs}]"""))
        |> String.concat ";\n"

/// Logs the intersection maps of a given model and returns it unchanged. Used for debugging
let logIntersectionMaps (model:Model) =
    let intersections =
        let formatSegmentIntersections segments =
            segments
            |> List.collect (fun segment -> 
                segment.IntersectCoordinateList
                |> List.map (fun (_, id) -> formatSegmentId id))

        model.Wires
        |> Map.toList
        |> List.map (fun (_, wire) -> 
            sprintf $"Wire: {formatSegmentIntersections wire.Segments}")

    printfn "\n------------------\nFromHorizontalToVerticalSegmentIntersections:"
    printfn $"{formatIntersectionMap model.FromHorizontalToVerticalSegmentIntersections}"
    printfn "FromVerticalToHorizontalSegmentIntersections:"
    printfn $"{formatIntersectionMap model.FromVerticalToHorizontalSegmentIntersections}"
    printfn $"Intersections"
    printfn $"{intersections}"
    printfn "------------------"
    model

/// Formats an XYPos for logging purposes.
let formatXY (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"

/// Given a segment start and end position, finds the orientation of the segment. 
/// Returns None if the segment is neither horizontal nor vertical
let getSegmentOrientation (segStart: XYPos) (segEnd: XYPos) =
    let fpError = 0.000001
    if segStart.X - segEnd.X < fpError then
        Vertical
    else if segStart.Y - segStart.Y < fpError then
        Horizontal
    else
        failwithf "ERROR: Diagonal wire" // Should never happen

/// Tries to find and log a segment identified by segId in a wire identified by wireId in the current model.
/// Assumes wireId can be found in the current model. Returns unit, used for debugging.
let logSegmentInModel model wireId segId  = 
        let wire = model.Wires[wireId]
        let findAndFormatSeg segStart segEnd (_state: string option) (seg: Segment) =
            if seg.Id = segId then 
                let orientation = 
                    match getSegmentOrientation segStart segEnd with
                    | Vertical -> "V"
                    | Horizontal -> "H"
                Some (sprintf $"""[{formatSegmentId seg.Id}: {formatXY segStart}->{formatXY segEnd}]-{orientation}-{seg.Index}""")
            else None

        match foldOverSegs findAndFormatSeg None wire with
        | Some str -> printfn $"{str}"
        | _ -> printfn $"ERROR: Could not find segment {formatSegmentId segId} in wire {formatWireId wireId}"
        


/// Tries to find and log each segment to its corresponding wire identified in wireSegmentIdPairs in the current model.
/// Returns the model unchanged. Used for debugging.
let logSegmentsInModel (model: Model) (wireSegmentIdPairs: (ConnectionId * SegmentId) list)= 
    wireSegmentIdPairs
    |> List.map  ( fun (wireId, segId) -> logSegmentInModel model wireId segId)
    |> ignore
    model


//-------------------------------Implementation code----------------------------//



/// Wire to Connection
let segmentsToVertices (segList:Segment list) = 
    let firstCoord = (segList[0].Start.X, segList[0].Start.Y)
    let verticesExceptFirst = List.mapi (fun i seg -> (seg.End.X,seg.End.Y)) segList
    [firstCoord] @ verticesExceptFirst


/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, this function returns a list of
/// wire vertices
let makeInitialWireVerticesList (portCoords : XYPos * XYPos)  = 
    let xs, ys, Xt, Yt = snd(portCoords).X, snd(portCoords).Y, fst(portCoords).X, fst(portCoords).Y

    // adjust length of segments 0 and 6 - the sticks - so that when two ports are aligned and close you still get left-to-right routing.
    let adjStick = 
        let d = List.max [ abs (xs - Xt) ; abs (ys - Yt) ; Wire.stickLength / 4.0 ]
        if (Xt - xs > 0.0) then
            min d (Wire.stickLength / 2.0)
        else
            Wire.stickLength / 2.0

    // the simple case of a wire travelling from output to input in a left-to-right (positive X) direction
    let leftToRight = 
        [
            {X = xs; Y = ys};
            {X = xs+adjStick; Y = ys};
            {X = xs+adjStick; Y = ys};
            {X = (xs+Xt)/2.0; Y = ys};
            {X = (xs+Xt)/2.0; Y = Yt};
            {X = Xt-adjStick; Y = Yt}
            {X = Xt-adjStick; Y = Yt}
            {X = Xt; Y = Yt}
        ]
    // the case of a wire travelling from output to input in a right-to-left (negative X) direction. Thus must bend back on itself.
    let rightToLeft =
        [
            {X = xs; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = (ys+Yt)/2.0}
            {X = Xt-Wire.stickLength; Y = (ys+Yt)/2.0}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt; Y = Yt}
        ]

    // the special case of a wire travelling right-to-left where the two ends are vertically almost identical. 
    // In this case we ad an offset to the main horizontal segment so it is more visible and can be easily re-routed manually.
    let rightToLeftHorizontal =
        [
            {X = xs; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = ys + Wire.stickLength}
            {X = Xt-Wire.stickLength; Y = ys + Wire.stickLength}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt; Y = Yt}
        ]

    if Xt - xs >= adjStick * 2.0 then 
        leftToRight, true
    elif abs (ys - Yt) < 4.0 then 
        rightToLeftHorizontal, false
    else 
        rightToLeft, false 

let inferDirectionfromVertices (xyVerticesList: XYPos list) =
    if xyVerticesList.Length <> 8 then 
        failwithf $"Can't perform connection type inference except with 8 vertices: here given {xyVerticesList.Length} vertices"
    let getDir (vs:XYPos) (ve:XYPos) =
        match sign ((abs vs.X - abs ve.X)*(abs vs.X - abs ve.X) - (abs vs.Y - abs ve.Y)*(abs vs.Y - abs ve.Y)) with
        | 1 -> Some Horizontal
        | -1 -> Some Vertical
        | _ -> None
    let midS, midE = xyVerticesList[3], xyVerticesList[4]
    let first,last = xyVerticesList[1], xyVerticesList[5]
    let xDelta = abs last.X - abs first.X
    match getDir midS midE, abs xDelta > 20.0, xDelta > 0.0 with
    | Some Horizontal, _, _ when midE.X < midS.X -> Some Horizontal
    | Some Vertical, _, _ -> Some Vertical 
    | _, true, true -> Some Vertical
    | _, true, false -> Some Horizontal
    | _, false, _ -> None

/// this turns a list of vertices into a list of segments
let xyVerticesToSegments connId (isLeftToRight: bool) (xyVerticesList: XYPos list) =

    let dirs = 
        match isLeftToRight with
        | true -> 
            // for 5 adjustable segments left-to-right
            [Horizontal;Vertical;Horizontal;Vertical;Horizontal;Vertical;Horizontal]
        | false ->
            // for 3 adjustale segments right-to-left
            [Horizontal;Horizontal;Vertical;Horizontal;Vertical;Horizontal;Horizontal]

    List.pairwise xyVerticesList
    |> List.mapi (
        fun i ({X=startX; Y=startY},{X=endX; Y=endY}) ->    
            {
                Id = SegmentId(JSHelpers.uuid())
                Index = i
                Start = {X=startX;Y=startY};
                End = {X=endX;Y=endY};
                Dir = dirs[i]
                HostId  = connId;
                JumpCoordinateList = [];
                Draggable =
                    match i with
                    | 1 | 5 ->  isLeftToRight
                    | 0  | 6  -> false
                    | _ -> true
            })

/// Convert a (possibly legacy) issie Connection stored as a list of vertices to Wire
let issieVerticesToSegments 
        (connId) 
        (verticesList: list<float*float>) =
    let xyVerticesList =
        verticesList
        |> List.map (fun (x,y) -> {X=x;Y=y})

    let makeSegmentsFromVertices (xyList: XYPos list) =
        makeInitialWireVerticesList (xyList[0], xyList[xyList.Length - 1])
        |> (fun (vl, isLeftToRight) -> xyVerticesToSegments connId isLeftToRight vl)
        

    // segments lists must must be length 7, in case legacy vertex list does not conform check this
    // if there are problems reroute
        //vertex lists are one element longer than segment lists
    if xyVerticesList.Length <> 8 then  
        makeSegmentsFromVertices xyVerticesList
    else 
        match inferDirectionfromVertices xyVerticesList with
        | Some Vertical -> 
            printfn "Converting vertical"
            xyVerticesToSegments connId true xyVerticesList
        | Some Horizontal -> 
            printfn "Converting horizontal"
            xyVerticesToSegments connId false xyVerticesList
        | _ ->
            // can't work out what vertices are, so default to auto-routing
            printfn "Converting unknown"
            makeSegmentsFromVertices xyVerticesList
            

    
//----------------------interface to Issie-----------------------//
/// This function is given a ConnectionId and it
/// converts the corresponding BusWire.Wire type to a
/// Connection type, offering an interface
/// between our implementation and Issie.
let extractConnection (wModel : Model) (cId : ConnectionId) : Connection =
    let conn = wModel.Wires[cId]
    let ConnectionId strId, InputPortId strInputPort, OutputPortId strOutputPort = conn.Id, conn.InputPort, conn.OutputPort
    {
        Id = strId
        Source = { Symbol.getPort wModel.Symbol strOutputPort with PortNumber = None } // None for connections 
        Target = { Symbol.getPort wModel.Symbol strInputPort with PortNumber = None } // None for connections 
        Vertices = segmentsToVertices conn.Segments
    } // We don't use vertices

/// This function is given a list of ConnectionId and it
/// converts the corresponding BusWire.Wire(s) to a
/// list of Connectio, offering an interface
/// between our implementation and Issie.
let extractConnections (wModel : Model) : list<Connection> = 
    wModel.Wires
    |> Map.toList
    |> List.map (fun (key, _) -> extractConnection wModel key)

/// Given three points p, q, r, the function returns true if 
/// point q lies on line segment 'pr'. Otherwise it returns false.
let onSegment (p : XYPos) (q : XYPos) (r : XYPos) : bool = 
    (
        (q.X <= max (p.X) (r.X)) &&
        (q.X >= min (p.X) (r.X)) &&
        (q.Y <= max (p.Y) (r.Y)) &&
        (q.Y >= min (p.Y) (r.Y))
    )
  
/// Given three points p, q, r, the function returns:
/// - 0 if p, q and r are colinear;
/// - 1 if the path that you must follow when you start at p, you visit q and you end at r, is a CLOCKWISE path;
/// - 2 if the path that you must follow when you start at p, you visit q and you end at r, is a COUNTERCLOCKWISE path.
let orientation (p : XYPos) (q : XYPos) (r : XYPos) : int =
    let result = (q.Y - p.Y) * (r.X - q.X) - (q.X - p.X) * (r.Y - q.Y)
  
    if (result = 0.0) then 0 // colinear
    elif (result > 0.0) then 1 // clockwise
    else 2 //counterclockwise

///Returns the abs of an XYPos object
let getAbsXY (pos : XYPos) = 
    {X = abs pos.X; Y = abs pos.Y}
  
/// Given two sets of two points: (p1, q1) and (p2, q2)
/// that define two segments, the function returns true
/// if these two segments intersect and false otherwise.
let segmentIntersectsSegment ((p1, q1) : (XYPos * XYPos)) ((p2, q2) : (XYPos * XYPos)) : bool =
    // this is a terrible implementation
    // determining intersection should be done by finding intersection point and comparing with coords
    // since segments are always horizontal or vertical that is pretty easy.
    // in addition the way that coordinates can be positive or negative but are absed when used is appalling
    // the manual or auto route info per segment should be a separate field in Segmnet, not encoded in the sign of the coordinates
    // that is needed when writing out or reading from Issie, but the write/read process can easily translate to a sane internal data structure in the draw blokc model
    let p1,q1,p2,q2= getAbsXY p1, getAbsXY q1, getAbsXY p2, getAbsXY q2
    // Find the four orientations needed for general and 
    // special cases 
    let o1 = orientation (p1) (q1) (p2)
    let o2 = orientation (p1) (q1) (q2)
    let o3 = orientation (p2) (q2) (p1)
    let o4 = orientation (p2) (q2) (q1)
  
    // General case 
    if (o1 <> o2 && o3 <> o4)
        then true

    // Special Cases 
    // p1, q1 and p2 are colinear and p2 lies on segment p1q1 
    elif (o1 = 0 && onSegment (p1) (p2) (q1))
        then true
  
    // p1, q1 and q2 are colinear and q2 lies on segment p1q1 
    elif (o2 = 0 && onSegment (p1) (q2) (q1))
        then true
  
    // p2, q2 and p1 are colinear and p1 lies on segment p2q2 
    elif (o3 = 0 && onSegment (p2) (p1) (q2))
        then true
  
     // p2, q2 and q1 are colinear and q1 lies on segment p2q2 
    elif (o4 = 0 && onSegment (p2) (q1) (q2))
        then true
    else false



///Returns a segment with positive Start and End coordinates
let makeSegPos (seg : Segment) =
    {seg with
        Start = getAbsXY seg.Start
        End = getAbsXY seg.End }

/// Given two coordinates, this function returns the euclidean
/// distance between them.
let distanceBetweenTwoPoints (pos1 : XYPos) (pos2 : XYPos) : float =
    sqrt ( (pos1.X - pos2.X)*(pos1.X - pos2.X) + (pos1.Y - pos2.Y)*(pos1.Y - pos2.Y) )


/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, this function returns a list of
/// Segment(s).
let makeInitialSegmentsList (hostId : ConnectionId) (portCoords : XYPos * XYPos) : list<Segment> =
    let xyPairs, isLeftToRight = makeInitialWireVerticesList portCoords
    xyPairs
    |> xyVerticesToSegments hostId isLeftToRight


/// This function renders the given
/// segment (i.e. creates a ReactElement
/// using the data stored inside it),
/// using the colour and width properties given.
let renderSegment (segment : Segment) (colour : string) (width : string) : ReactElement = 
    let wOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match wOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    let halfWidth = (renderWidth/2.0) - (0.75)
    let lineParameters = { defaultLine with Stroke = colour; StrokeWidth = string renderWidth }
    let circleParameters = { defaultCircle with R = halfWidth; Stroke = colour; Fill = colour }

    if segment.Dir = Horizontal then
        let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }

        let renderWireSubSegment (vertex1 : XYPos) (vertex2 : XYPos) : list<ReactElement> =
            let Xa, Ya, Xb, Yb = vertex1.X, vertex1.Y, vertex2.X, vertex2.Y
            makeLine Xa Ya Xb Yb lineParameters
            ::
            makeCircle Xa Ya circleParameters
            ::
            [
                makeCircle Xb Yb circleParameters
            ]
        
        let segmentJumpHorizontalSize = 9.0
        let segmentJumpVerticalSize = 6.0
        
        let renderSingleSegmentJump (intersectionCoordinate : XYPos) : list<ReactElement> =
            let x, y = intersectionCoordinate.X, intersectionCoordinate.Y

            let startingPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y}
            let startingControlPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
            let endingControlPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
            let endingPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y}

            makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters
            ::
            makeCircle startingPoint.X startingPoint.Y circleParameters
            ::
            [
                makeCircle endingPoint.X endingPoint.Y circleParameters
            ]
        
        let rec renderMultipleSegmentJumps (segmentJumpCoordinateList : list<float>) (segmentJumpYCoordinate : float) : list<ReactElement> =
            
            match segmentJumpCoordinateList with

            | [] -> []


            | [singleElement] ->
                renderSingleSegmentJump {X = singleElement; Y = segmentJumpYCoordinate}


            | firstElement :: secondElement :: tailList ->

                if (segment.Start.X > segment.End.X) then
                    renderSingleSegmentJump {X = firstElement; Y = segmentJumpYCoordinate}
                    @
                    renderWireSubSegment {X = firstElement - segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} {X = secondElement + segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate}
                    @
                    renderMultipleSegmentJumps (secondElement :: tailList) (segmentJumpYCoordinate)
                
                else
                    renderSingleSegmentJump {X = firstElement; Y = segmentJumpYCoordinate}
                    @
                    renderWireSubSegment {X = firstElement + segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} {X = secondElement - segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate}
                    @
                    renderMultipleSegmentJumps (secondElement :: tailList) (segmentJumpYCoordinate)
            

        let completeWireSegmentRenderFunction (seg : Segment) : list<ReactElement> =
            
            let jumpCoordinateList =
                if (segment.Start.X > segment.End.X) then
                    seg.JumpCoordinateList
                    |> List.map fst
                    |> List.sortDescending
                    
                else
                    seg.JumpCoordinateList
                    |> List.map fst
                    |> List.sort
            
            match jumpCoordinateList with
                | [] -> renderWireSubSegment seg.Start seg.End

                | lst ->
                     let y = seg.Start.Y // SHOULD be equal to seg.End.Y since ONLY horizontal segments have jumps
                     let firstSegmentJumpCoordinate = lst[0]
                     let lastSegmentJumpCoordinate = lst[(List.length lst) - 1]

                     if (segment.Start.X > segment.End.X) then
                         renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y}
                         @
                         renderMultipleSegmentJumps lst y
                         @
                         renderWireSubSegment {X = lastSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y} seg.End

                     else
                         renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y}
                         @
                         renderMultipleSegmentJumps lst y
                         @
                         renderWireSubSegment {X = lastSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y} seg.End
        

        let wireSegmentReactElementList = segment
                                          |> completeWireSegmentRenderFunction

        g [] wireSegmentReactElementList
    
    else
        let Xa, Ya, Xb, Yb = segment.Start.X, segment.Start.Y, segment.End.X, segment.End.Y
        let segmentElements = 
            makeLine Xa Ya Xb Yb lineParameters
            ::
            makeCircle Xa Ya circleParameters
            ::
            [
                makeCircle Xb Yb circleParameters
            ]
        g [] segmentElements

///
type WireRenderProps =
    {
        key: string
        Segments: list<Segment>
        ColorP: HighLightColor
        StrokeWidthP: int
        OutputPortLocation: XYPos
    }


// ------------------------------redundant wire memoisation code------------------------------
// this code is not used because React (via Function.Of) does this caching anyway - better tha it can be
// done here
let mutable cache:Map<string,WireRenderProps*ReactElement> = Map.empty

/// not used
let memoOf (f: WireRenderProps -> ReactElement, _, _) =
    (fun props ->
        match Map.tryFind props.key cache with
        | None -> 
            let re = f props
            cache <- Map.add props.key (props,re) cache 
            re
        | Some (props',re) ->  
            if props' = props then re else
                let re = f props
                cache <- Map.add props.key (props,re) cache
                re)
//-------------------------------------------------------------------------------------------

let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let renderWireSegmentList : list<ReactElement> =
                props.Segments
                |> List.map
                    (
                        fun (segment : Segment) -> renderSegment segment (props.ColorP.Text()) (string props.StrokeWidthP)
                            //call a bunch of render helper functions to render the segment (*** DO NOT FORGET SEGMENT JUMPS ***)
                    )
            
            let renderWireWidthText : ReactElement =
                let textParameters =
                    {
                        TextAnchor = "left";
                        FontSize = "12px";
                        FontWeight = "Bold";
                        FontFamily = "Verdana, Arial, Helvetica, sans-serif";
                        Fill = props.ColorP.Text();
                        UserSelect = UserSelectOptions.None;
                        DominantBaseline = "middle";
                    }
                let textString = if props.StrokeWidthP = 1 then "" else string props.StrokeWidthP //Only print width > 1
                makeText (props.OutputPortLocation.X+1.0) (props.OutputPortLocation.Y-7.0) (textString) textParameters
            g [] ([ renderWireWidthText ] @ renderWireSegmentList)
        
    , "Wire"
    , equalsButFunctions
    )

///
let MapToSortedList map : Wire list = 
    let listSelected = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Purple) map
        |> Map.toList
        |> List.map snd
    let listErrorSelected =
        Map.filter (fun id wire -> wire.Color = HighLightColor.Brown) map
        |> Map.toList
        |> List.map snd
    let listErrorUnselected =
        Map.filter (fun id wire -> wire.Color = HighLightColor.Red) map
        |> Map.toList
        |> List.map snd
    let listUnSelected = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.DarkSlateGrey) map
        |> Map.toList
        |> List.map snd
    let listCopied = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Thistle) map
        |> Map.toList
        |> List.map snd
    let listWaves = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Blue) map
        |> Map.toList
        |> List.map snd

    listUnSelected @ listErrorUnselected @ listErrorSelected @ listSelected @ listWaves @ listCopied
   
let view (model : Model) (dispatch : Dispatch<Msg>) =
    let start = TimeHelpers.getTimeMs()
    let wires1 =
        model.Wires
        |> Map.toArray
        |> Array.map snd
    TimeHelpers.instrumentTime "WirePropsSort" start
    let rStart = TimeHelpers.getTimeMs()
    let wires =
        wires1
        |> Array.map
            (
                fun wire ->
                    let stringOutId =
                        match wire.OutputPort with
                        | OutputPortId stringId -> stringId
                        
                    let outputPortLocation = Symbol.getOnePortLocationNew model.Symbol stringOutId PortType.Output
                    let props =
                        {
                            key = match wire.Id with | ConnectionId s -> s
                            Segments = List.map makeSegPos wire.Segments
                            ColorP = wire.Color
                            StrokeWidthP = wire.Width
                            OutputPortLocation = outputPortLocation
                        }
                    singleWireView props)
    TimeHelpers.instrumentInterval "WirePrepareProps" rStart ()
    let symbols = Symbol.view model.Symbol (Symbol >> dispatch)
 
    g [] [(g [] wires); symbols]
    |> TimeHelpers.instrumentInterval "WireView" start

//---------------------------------------------------------------------------------//
//--------------------STS219 CODE SECTION STARTS-------------------------------------//
//---------------------------------------------------------------------------------//

/// Type used to simplify BoundingBox intersection calculations
type Rectangle = {
    TopLeft: XYPos
    BottomRight: XYPos
}

//-------------------------XYPos Helper Functions---------------------------------//

/// Returns the X-value of an XYPos
let toX (pos: XYPos) = pos.X

/// Returns the Y-value of an XYPos
let toY (pos: XYPos) = pos.X

/// Returns the X and Y fields of an XYPos as a pair of floats
let getXY (pos: XYPos) = pos.X, pos.Y

/// Returns p1 + p2 as an XYPos
let posAdd (p1: XYPos) (p2: XYPos) : XYPos =
    { X = p1.X + p2.X; Y = p1.Y + p2.Y }

/// Returns p1 - p2 as an XYPos
let posDiff (p1: XYPos) (p2: XYPos) : XYPos = 
    { X = p1.X - p2.X; Y = p1.Y - p2.Y }

let scalePos (factor: float) (pos: XYPos) : XYPos =
    { X = factor * pos.X; Y = factor * pos.Y}

/// Returns the dot product of 2 XYPos
let dotProduct (p1: XYPos) (p2: XYPos) : float = 
    p1.X * p2.X + p1.Y * p2.Y

/// Returns the squared distance between 2 points using Pythagoras
let squaredDistance (p1: XYPos) (p2: XYPos) = 
    let diff = posDiff p1 p2
    dotProduct diff diff

//--------------------------------------------------------------------------------//

/// Checks if 2 rectangles intersect
let rectanglesIntersect (rect1: Rectangle) (rect2: Rectangle) =
    /// Checks if there is an intersection in the X or Y dimension
    let intersect1D (xOrY: XYPos -> float): bool =
        let qHi = min (xOrY rect1.BottomRight) (xOrY rect2.BottomRight)
        let qLo = max (xOrY rect1.TopLeft) (xOrY rect2.TopLeft)
        qLo < qHi

    intersect1D toX && intersect1D toY 

/// Checks if a segment intersects a bounding box using the segment's start and end XYPos
let segmentIntersectsBoundingBox (bb: BoundingBox) segStart segEnd = 
    let toRect topLeft bottomRight =
        { TopLeft = topLeft
          BottomRight = bottomRight }
    
    let bbBottomRight = { X = bb.TopLeft.X + bb.W; Y = bb.TopLeft.Y + bb.H }
    let bbRect = toRect bb.TopLeft bbBottomRight
    let segRect = toRect segStart segEnd
    rectanglesIntersect bbRect segRect

/// Returns the distance between a point and a segment defined by a start and end XYPos
let distanceBetweenPointAndSegment (segStart : XYPos) (segEnd : XYPos) (point : XYPos) : float = 
    match squaredDistance segStart segEnd with
    | 0. -> failwithf "Segment of length 0" // This should never happen
    | l2 -> 
        // Extend the segment to line segStart + t (segEnd - segStart)
        // The projection of point on this line falls at tProjection
        let tProjection = dotProduct (posDiff point segStart) (posDiff segEnd segStart) / l2 
        let tBounded = max 0. (min 1. tProjection) // Bound tProjection to be within the segment
        let boundedProjection = 
            posDiff segEnd segStart
            |> scalePos tBounded
            |> posAdd segStart
        sqrt (squaredDistance point boundedProjection)

/// Finds the Id of the closest segment in a wire to a mouse click using euclidean distance
let getClickedSegment (model : Model) (wireId : ConnectionId) (mouse : XYPos) : SegmentId =
    let closestSegment segStart segEnd state (seg : Segment) =
        let currDistance = distanceBetweenPointAndSegment segStart segEnd mouse
        match state with
        | Some (minId, minDistance) -> if currDistance < minDistance then Some (seg.Id, currDistance) else Some (minId, minDistance)
        | None -> Some (seg.Id, currDistance) // Needed to deal with initial case
    
    match foldOverSegs closestSegment None model.Wires[wireId] with
    | Some (segmentId, _) -> segmentId
    | None -> failwithf "getClosestSegment was given a wire with no segments" // Should never happen

/// Returns a distance for a wire move that has been reduced if needed to enforce minimum segment lengths.
let getSafeDistanceForMove (segments: Segment list) (index: int) (distance: float) =
    let reduceDistance maxDistance =
        if sign maxDistance = -1 then
            max maxDistance
        else 
            min maxDistance

    if index <= 0 || index >= segments.Length - 1 then
        0. // We cannot move the first or last segment in a wire
    else
        let prev = segments[index - 1]
        let next = segments[index + 1]
        let prevMaxDistance = (-prev.Length + (float (sign prev.Length) * Wire.stickLength))
        let nextMaxDistance = (next.Length - (float (sign next.Length) * Wire.stickLength))

        distance
        |> reduceDistance prevMaxDistance
        |> reduceDistance nextMaxDistance

/// Returns a wire containing the updated list of segments after a segment is moved by 
/// a specified distance. The moved segment is tagged as manual so that it is no longer auto-routed.
/// Throws an error if the segment being moved is out of range.
let moveSegment (model:Model) (seg:Segment) (distance:float) = 
    let wire = model.Wires[seg.HostId]
    let segments = wire.Segments
    let idx = seg.Index

    if idx <= 0 || idx >= segments.Length - 1 then
        failwithf $"Trying to move wire segment {idx}, out of range in wire length {segments.Length}"

    let safeDistance = getSafeDistanceForMove segments idx distance
    
    let prevSeg = segments[idx - 1]
    let nextSeg = segments[idx + 1]
    let movedSeg = segments[idx]

    let newPrevSeg = { prevSeg with Length = prevSeg.Length + safeDistance }
    let newNextSeg = { nextSeg with Length = nextSeg.Length + safeDistance }
    let newMovedSeg = { movedSeg with Mode = Manual }
    
    let newSegments = segments[.. idx - 2] @ [newPrevSeg; newMovedSeg; newNextSeg] @ segments[idx + 2 ..]

    {wire with Segments = newSegments}

/// Initialises an empty Wire Model
let init () = // Doesn't seem that harmful, revisit if we change the model type
    let symbols,_ = Symbol.init()
    {   
        Wires = Map.empty;
        FromVerticalToHorizontalSegmentIntersections = Map.empty;
        FromHorizontalToVerticalSegmentIntersections = Map.empty;
        Symbol = symbols; 
        CopiedWires = Map.empty; 
        SelectedSegment = SegmentId(""); 
        LastMousePos = {X = 0.0; Y = 0.0};
        ErrorWires = []
        Notifications = None
        Type = Jump
    } , Cmd.none

/// Returns a list of all the wires in the given model
let getWireList (model: Model) =
    model.Wires
    |> Map.toList
    |> List.map snd

/// Returns a list of wire IDs that meet the given condition
let getFilteredIdList condition wireLst = 
    wireLst
    |> List.filter condition
    |> List.map (fun wire -> wire.Id)
    //|> List.distinct // TODO: Figure out if this is needed - I don't think it is

///Returns the IDs of the wires in the model connected to a list of components given by componentIds
let getConnectedWires (model: Model) (compIds: list<ComponentId>) =
    let containsPorts wire =
        let inputPorts, outputPorts =
            Symbol.getPortLocations model.Symbol compIds

        Map.containsKey wire.InputPort inputPorts
        || Map.containsKey wire.OutputPort outputPorts

    model
    |> getWireList
    |> getFilteredIdList containsPorts

/// Returns a tuple of: wires connected to inputs ONLY, wires connected to outputs ONLY, wires connected to both inputs and outputs
let filterWiresByCompMoved (model: Model) (compIds: list<ComponentId>) =
    let wireList = getWireList model

    let inputPorts, outputPorts =
        Symbol.getPortLocations model.Symbol compIds

    let containsInputPort wire =
        Map.containsKey wire.InputPort inputPorts

    let containsOutputPort wire =
        Map.containsKey wire.OutputPort outputPorts

    let containsBothPort wire =
        containsInputPort wire && containsOutputPort wire

    let inputWires =
        wireList |> getFilteredIdList containsInputPort

    let outputWires =
        wireList |> getFilteredIdList containsOutputPort

    let fullyConnected =
        wireList |> getFilteredIdList containsBothPort

    (inputWires, outputWires, fullyConnected)

/// Contains geometric information of a port
type PortInfo = {
    Edge: Symbol.Edge
    Position: XYPos
}

/// Returns a PortInfo object given a port edge and position
let genPortInfo edge position =
    { Edge = edge; Position = position }

/// Returns an edge rotated 90 degrees anticlockwise
let rotate90Edge (edge: Symbol.Edge) = 
    match edge with
    | Symbol.Top -> Symbol.Left
    | Symbol.Left -> Symbol.Bottom
    | Symbol.Bottom -> Symbol.Right
    | Symbol.Right -> Symbol.Top

/// Returns a port rotated 90 degrees anticlockwise
let rotate90Port (port: PortInfo) =
    let newEdge = rotate90Edge port.Edge

    let newPos =
        { X = -port.Position.Y
          Y = port.Position.X }

    genPortInfo newEdge newPos

/// Returns a function to rotate a segment list 90 degrees anticlockwise,
/// depending on its initial orientation
let rotateSegments90 initialOrientation =
    let horizontal i =
        match initialOrientation with
        | Horizontal -> i % 2 = 0
        | Vertical -> i % 2 = 1

    List.indexed
    >> List.map
        (fun (i, seg) ->
            if (horizontal i) then 
                { seg with Length = -seg.Length }
            else
                seg)

/// Returns a version of the start and destination ports rotated until the start edge matches the target edge.
let rec rotateStartDest (target: Symbol.Edge) ((start, dest): PortInfo * PortInfo) = 
    if start.Edge = target then
        (start, dest)
    else
        rotateStartDest target (rotate90Port start, rotate90Port dest)

/// Gets a wire orientation given a port edge
let getOrientation (edge: Symbol.Edge) = 
    match edge with
    | Symbol.Top | Symbol.Bottom -> Vertical
    | Symbol.Left | Symbol.Right -> Horizontal

/// Returns an anonymous record containing the starting symbol edge of a wire and its segment list that has been 
/// rotated to a target symbol edge.
let rec rotateSegments (target: Symbol.Edge) (wire: {| edge: Symbol.Edge; segments: Segment list |}) =
    if wire.edge = target then
        {| edge = wire.edge; segments = wire.segments |}
    else
        let rotatedSegs =
            rotateSegments90 (getOrientation wire.edge) wire.segments
        
        {| edge = rotate90Edge wire.edge; segments = rotatedSegs |}
        |> rotateSegments target 

/// Returns a newly autorouted version of a wire for the given model
let autorouteWire (model: Model) (wire: Wire) : Wire =
    let destPos, startPos =
        Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)

    let destEdge =
        Symbol.getInputPortOrientation model.Symbol wire.InputPort

    let startEdge =
        Symbol.getOutputPortOrientation model.Symbol wire.OutputPort

    let startPort = genPortInfo startEdge startPos
    let destPort = genPortInfo destEdge destPos

    let normalisedStart, normalisedEnd =
        rotateStartDest Symbol.Right (startPort, destPort)

    let initialSegments =
        makeInitialSegmentsList normalisedStart.Position normalisedEnd.Position normalisedEnd.Edge

    let segments =
        {| edge = Symbol.Right
           segments = initialSegments |}
        |> rotateSegments startEdge
        |> (fun wire -> wire.segments)

    { wire with
          Segments = segments
          InitialOrientation = getOrientation startEdge
          StartPos = startPos
          EndPos = destPos }

/// Reverses a segment list so that it may be processed in the opposite direction. This function is self-inverse.
let reverseSegments (segs:Segment list) =
    List.rev segs
    |> List.map (fun seg -> {seg with Length = -seg.Length})

//
//  ====================================================================================================================
//
//                                        WIRE SEGMENTS FOR ROUTING
//
//
// Segments, going from Start (output port) to End (input port) coords, are summarised as:
// H => Horizontal (incr X)
// V => Vertical (incr Y)
// 0 => zero length segment (never used)
//
// segment qualifiers:
// F => min length (next to output or input, cannot be shortened) // stick
//
// "Simple" case where output.X < input.X and 3 segment autoroute is possible
//  S0.FH  S1.0V  S2.H  S3.V  S4.H  S5.0V S6.FH
//
// "Complex" case where output.X > input.X and wire ends back for 5 segment autoroute
//  S0.FH  S1.V  S2.H  S3.V  S4.H  S5.0V S6.FH (not sure if H and V are correct here) // pretty sure this is wrong???
//
// To determine adjustment on End change we just reverse the segment and apply the Start change algorithm
// Adjustment => reverse list of segments, swap Start and End, and alter the sign of all coordinates
// For simplicity, due to the encoding of manual changes into coordinates by negating them (yuk!)
// we do not alter coordinate sign. Instead we invert all numeric comparisons.
// There are no constants used in the algorithm (if there were, they would need to be negated)
//
// ======================================================================================================================

let inline moveAll (mover: XYPos -> XYPos) (n : int) = // moves both start and end
    List.mapi (fun i (seg:Segment) -> if i = n then {seg with Start = mover seg.Start; End = mover seg.End} else seg)

let  transformXY tX tY (pos: XYPos) = // transforms the x and y values of a pos
    {pos with X = tX pos.X; Y = tY pos.Y}

let transformSeg tX tY (seg: Segment) = // Applies the tX and tY tranformations to the XY positions in the segment
    let trans = transformXY tX tY
    {seg with Start = trans seg.Start; End = trans seg.End }

/// Returns a tuple (X, Y), where X and Y are +/- 1. This encodes the following:
/// If pos1 has a bigger X or Y value than pos 2, the corresponding term in the tuple will be +1,
/// else the value will be -1. 
let topology (pos1: XYPos) (pos2:XYPos) = // This name is so bad
    sign (abs pos1.X - abs pos2.X), sign (abs pos1.Y - abs pos2.Y)

/// Returns None if full autoroute is required or Some segments with initial part of the segment list autorouted
/// up till the first dragged (manually routed) segment.
/// ReverseFun must equal not or id. not => the segments go from input to output (reverse of normal).
/// This allows the same code to work on both ends of the wire, with segment reversal done outside this
/// function to implement input -> output direction.
let partialAutoRoute (segs: Segment list) (newPortPos: XYPos) = // This is a phat functions
    let wirePos = segs[0].End
    let portPos = segs[0].Start
    let newWirePos = {newPortPos with X = newPortPos.X + (abs wirePos.X - portPos.X) }
    let (diff:XYPos) = {X=newPortPos.X-portPos.X; Y= newPortPos.Y - portPos.Y}
    let lastAutoIndex =
        segs
        |> List.takeWhile (fun seg -> seg.Mode = Auto)
        |> List.length
        |> (fun n -> if n > 5 then None else Some (n + 1))
    let scaleBeforeSegmentEnd segIndex = // Gonna need to figure out wtf this bs does
        let seg = segs[segIndex]
        let fixedPt = getAbsXY seg.End
        let scale x fx nx wx =
            if nx = fx then x else ((abs x - fx)*(nx-fx)/(abs wx - fx) + fx) * float (sign x)
        let startPos = if segIndex = 1 then portPos else wirePos
        let newStartPos = if segIndex = 1 then newPortPos else newWirePos
        let scaleX x = scale x fixedPt.X newStartPos.X startPos.X
        let scaleY y = scale y fixedPt.Y newStartPos.Y startPos.Y
        match List.splitAt (segIndex+1) segs, segIndex with
        | ((scaledSegs), otherSegs), 1 ->
            Some ((List.map (transformSeg scaleX scaleY) scaledSegs) @ otherSegs)
        | ((firstSeg :: scaledSegs), otherSegs), _ ->
            Some ((moveAll (posAdd diff) 0 [firstSeg] @ List.map (transformSeg scaleX scaleY) scaledSegs) @ otherSegs)
        | _ -> None

    let checkTopology index =
        let finalPt = segs[6].Start
        let oldTop x = topology (if index = 1 then portPos else wirePos) x
        let newTop x = topology (if index = 1 then newPortPos else newWirePos) x
        if oldTop finalPt <> newTop finalPt then
            // always aandon manual routing
            None 
        else
            let manSegEndPt = segs[index].End
            let oldT = oldTop manSegEndPt
            let newT = newTop manSegEndPt
            if oldT = newT then
                Some index
            else
                None
    lastAutoIndex
    |> Option.bind checkTopology
    |> Option.bind scaleBeforeSegmentEnd

/// Moves a wire by the XY amounts specified by displacement
let moveWire (wire: Wire) (displacement: XYPos) =
    { wire with
          StartPos = posAdd wire.StartPos displacement
          EndPos = posAdd wire.EndPos displacement }

/// Re-routes a single wire in the model when its ports move.
/// Tries to preserve manual routing when this makes sense, otherwise re-routes with autoroute.
/// Partial routing from input end is done by reversing segments and and swapping Start/End
/// inout = true => reroute input (target) side of wire.
let updateWire (model : Model) (wire : Wire) (inOut : bool) =
    let newPort = 
        match inOut with
        | true -> Symbol.getInputPortLocation model.Symbol wire.InputPort
        | false -> Symbol.getOutputPortLocation model.Symbol wire.OutputPort
    if inOut then
        partialAutoRoute (reverseSegments wire.Segments) newPort
        |> Option.map reverseSegments
    else 
        partialAutoRoute wire.Segments newPort
    |> Option.map (fun segs -> {wire with Segments = segs})
    |> Option.defaultValue (autorouteWire model wire)

//---------------------------------------------------------------------------------//
//--------------------STS219 CODE SECTION ENDS-------------------------------------//
//---------------------------------------------------------------------------------//

let makeAllJumps (wiresWithNoJumps: ConnectionId list) (model: Model) =
    let mutable neWX = model.Wires
    // Arrays are faster to check than lists
    let wiresWithNoJumpsA = List.toArray wiresWithNoJumps
    let changeJumps wid index jumps =
        let jumps = List.sortDescending jumps
        let changeSegment segs =
            List.mapi (fun i x -> if i <> index then x else { x with JumpCoordinateList = jumps }) segs

        newWX <- Map.add wid { newWX[wid] with Segments = changeSegment newWX[wid].Segments } newWX

    let segs =
        model.Wires
        |> Map.toArray
        |> Array.mapi (fun i (wid, w) -> List.toArray w.Segments)

    for w1 in 0 .. segs.Length - 1 do
        for h in segs[w1] do
            if h.Dir = Horizontal then
                // work out what jumps this segment should have
                let mutable jumps: (float * SegmentId) list = []
                
                if not (Array.contains h.HostId wiresWithNoJumpsA) then
                    for w2 in 0 .. segs.Length - 1 do
                        // everything inside the inner loop should be very highly optimised
                        // it is executed n^2 time where n is the number of segments (maybe 5000)
                        // the abs here are because segment coordinates my be negated to indicate manual routing
                        for v in segs[w2] do
                            if not (Array.contains v.HostId wiresWithNoJumpsA) then
                                match v.Dir with
                                | Vertical ->
                                    let x, x1, x2 = abs v.Start.X, abs h.Start.X, abs h.End.X
                                    let y, y1, y2 = abs h.Start.Y, abs v.Start.Y, abs v.End.Y
                                    let xhi, xlo = max x1 x2, min x1 x2
                                    let yhi, ylo = max y1 y2, min y1 y2
                                    //printfn $"{[xlo;x;xhi]}, {[ylo;y;yhi]}"
                                    if x < xhi - 5.0 && x > xlo + 5.0 && y < yhi - 5.0 && y > ylo + 5.0 then
                                        //printfn "found a jump!"
                                        jumps <- (x, v.Id) :: jumps
                                | _ -> ()
                    // compare jumps with what segment now has, and change newWX if need be
                // note that if no change is needed we do not update WX
                // simple cases are done without sort for speed, proably not necessary!
                // The jump list is sorted in model to enable easier rendering of segments
                match jumps, h.JumpCoordinateList with
                | [], [] -> ()
                | [ a ], [ b ] when a <> b -> changeJumps h.HostId h.Index jumps
                | [], _ -> changeJumps h.HostId h.Index jumps
                | _, [] -> // in this case we need to sort the jump list
                    changeJumps h.HostId h.Index (List.sort jumps)
                | newJumps, oldJ ->
                    let newJ = List.sort newJumps
                    // oldJ is already sorted (we only ever write newJ back to model)
                    if newJ <> oldJ then changeJumps h.HostId h.Index newJumps else ()

    { model with Wires = newWX }


let updateWireSegmentJumps (wireList: list<ConnectionId>) (wModel: Model) : Model =
    let startT = TimeHelpers.getTimeMs()
    let model = makeAllJumps [] wModel
    TimeHelpers.instrumentTime "UpdateJumps" startT
    model



/// This function updates the wire model by removing from the stored lists of intersections
/// all those generated by wireList wires.
/// intersetcions are stored in maps on the model and on the horizontal segments containing the jumps
let resetWireSegmentJumps (wireList : list<ConnectionId>) (wModel : Model) : Model =
    makeAllJumps wireList wModel



   
        



/// Re-routes the wires in the model based on a list of components that have been altered.
/// If the wire input and output ports are both in the list of moved components, does not re-route wire but instead translates it.
/// Keeps manual wires manual (up to a point).
/// Otherwise it will auto-route wires connected to components that have moved
let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =

    let (inputWires, outputWires, fullyConnected) = filterWiresByCompMoved model compIdList

    let newWires = 
        model.Wires
        |> Map.toList
        |> List.map (fun (cId, wire) -> 
            if List.contains cId fullyConnected //Translate wires that are connected to moving components on both sides
            then (cId, moveWire wire diff)
            elif List.contains cId inputWires //Only route wires connected to ports that moved for efficiency
            then (cId, updateWire model wire true)
            elif List.contains cId outputWires
            then (cId, updateWire model wire false)
            else (cId, wire))
        |> Map.ofList
        
    {model with Wires = newWires}

///
let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =
    
    match msg with
    | Symbol sMsg ->
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd


    | UpdateWires (componentIdList, diff) -> 
        updateWires model componentIdList diff, Cmd.none

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        let portOnePos, portTwoPos = Symbol.getTwoPortLocations model.Symbol inputId outputId
        let wireWidthFromSymbol = WireWidth.Configured 1
        let wireId = ConnectionId(JSHelpers.uuid())
        let segmentList = makeInitialSegmentsList wireId (portOnePos, portTwoPos)
        
        let newWire = 
            {
                Id = wireId
                InputPort = inputId
                OutputPort = outputId
                Color = HighLightColor.DarkSlateGrey
                Width = 1
                Segments = segmentList
            }
            
        let wireAddedMap = Map.add newWire.Id newWire model.Wires
        let newModel = updateWireSegmentJumps [wireId] {model with Wires = wireAddedMap}

        newModel, Cmd.ofMsg BusWidths

    | BusWidths ->

        let processConWidths (connWidths: ConnectionsWidth) =
            let addWireWidthFolder (wireMap: Map<ConnectionId, Wire>) _ wire  =
                let width =
                    match connWidths[wire.Id] with
                    | Some a -> a
                    | None -> wire.Width
                let newColor = if wire.Color = Purple || wire.Color = Brown then Purple else DarkSlateGrey
                wireMap.Add ( wire.Id, { wire with Width = width ; Color = newColor} )

            let addSymbolWidthFolder (m: Map<ComponentId,Symbol.Symbol>) (_: ConnectionId) (wire: Wire) =
                    let inPort = model.Symbol.Ports[match wire.InputPort with InputPortId ip -> ip]
                    let symId = ComponentId inPort.HostId
                    let symbol = m[symId]

                    match symbol.Compo.Type with
                    | SplitWire n ->
                        match inPort.PortNumber with 
                        | Some 0 -> {symbol with InWidth0 = Some wire.Width}
                        | x -> failwithf $"What? wire found with input port {x} other than 0 connecting to SplitWire"
                        |> (fun sym -> Map.add symId sym m)
                    | MergeWires ->
                        match inPort.PortNumber with 
                        | Some 0 -> 
                            Map.add symId  {symbol with InWidth0 = Some wire.Width} m
                        | Some 1 -> 
                            Map.add symId {symbol with InWidth1 = Some wire.Width} m
                        | x -> failwithf $"What? wire found with input port {x} other than 0 or 1 connecting to MergeWires"
                    | _ -> m

            let newWX = ((Map.empty, model.Wires) ||> Map.fold addWireWidthFolder)

            let symbolsWithWidths =
                (model.Symbol.Symbols, newWX) ||> Map.fold addSymbolWidthFolder

            { model with 
                Wires = newWX; Notifications = None ; 
                ErrorWires=[]; 
                Symbol = {model.Symbol with Symbols = symbolsWithWidths}}, Cmd.none    
        


        let canvasState = (Symbol.extractComponents model.Symbol, extractConnections model )
        
        
        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok connWidths ->
            processConWidths connWidths
        | Error e ->
                { model with 
                    Notifications = Some e.Msg }, Cmd.ofMsg (ErrorWires e.ConnectionsAffected)
    
    | CopyWires (connIds : list<ConnectionId>) ->
        let copiedWires = Map.filter (fun connId _ -> List.contains connId connIds) model.Wires
        { model with CopiedWires = copiedWires }, Cmd.none

    | ErrorWires (connectionIds : list<ConnectionId>) -> 
        let newWX =
            model.Wires
            |> Map.map
                (fun id wire -> 
                    if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Red}
                    else if List.contains id model.ErrorWires then 
                        {wire with Color = HighLightColor.DarkSlateGrey}
                    else wire
                ) 
        
        {model with Wires = newWX ; ErrorWires = connectionIds}, Cmd.none

    | SelectWires (connectionIds : list<ConnectionId>) -> //selects all wires in connectionIds, and also deselects all other wires
        let newWX =
            model.Wires
            |> Map.map
                (fun id wire -> 
                    if List.contains id model.ErrorWires then
                        if List.contains id connectionIds then 
                            {wire with Color = HighLightColor.Brown} 
                        else 
                            {wire with Color = HighLightColor.Red}
                    else if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Purple} 
                    else
                        {wire with Color = HighLightColor.DarkSlateGrey} 
                ) 
        
        {model with Wires = newWX}, Cmd.none

    | DeleteWires (connectionIds : list<ConnectionId>) -> 
        let newModel = resetWireSegmentJumps (connectionIds) (model)
        let newWX =
             newModel.Wires
             |> Map.filter (fun id wire -> not (List.contains id connectionIds))
        {newModel with Wires = newWX}, Cmd.ofMsg BusWidths

    | DragWire (connId : ConnectionId, mMsg: MouseT) ->
        match mMsg.Op with
        | Down ->
            let segId = getClickedSegment model connId mMsg.Pos
            {model with SelectedSegment = segId }, Cmd.none
        | Drag ->
            let segId = model.SelectedSegment
            let rec getSeg (segList: list<Segment>) = 
                match segList with
                | h::t -> if h.Id = segId then h else getSeg t
                | _ -> failwithf "segment Id not found in segment list"
            let seg = getSeg model.Wires[connId].Segments
            if seg.Draggable then
                let distanceToMove = 
                    match seg.Dir with
                    | Horizontal -> mMsg.Pos.Y - abs seg.Start.Y
                    | Vertical -> mMsg.Pos.X - abs seg.Start.X

                let newWire = moveSegment model seg distanceToMove
                let newWX = Map.add seg.HostId newWire model.Wires
 
                {model with Wires = newWX}, Cmd.none
            else
                model, Cmd.none
        
        | _ -> model, Cmd.none


    | ColorWires (connIds, color) -> // Just Changes the colour of the wires, Sheet calls pasteWires before this
        let newWires =
            (List.fold (fun prevWires cId -> 
                let oldWireOpt = Map.tryFind cId model.Wires
                match oldWireOpt with
                | None -> 
                    printfn "BusWire error: expected wire in ColorWires does not exist"
                    prevWires
                | Some oldWire ->
                    Map.add cId { oldWire with Color = color } prevWires) model.Wires connIds)
        { model with Wires = newWires }, Cmd.none
    
    | ResetJumps connIds ->
        printfn $"resetting jumps on {connIds.Length} wires"
        
        let newModel =
            model
            |> resetWireSegmentJumps connIds
        
        newModel, Cmd.none
    
    | MakeJumps connIds ->
        printfn $"making jumps on {connIds.Length} wires"

        let newModel =
            model
            |> updateWireSegmentJumps connIds
            
        newModel, Cmd.none
    
    | ResetModel -> { model with Wires = Map.empty; ErrorWires = []; Notifications = None }, Cmd.none
    
    | LoadConnections conns -> // we assume components (and hence ports) are loaded before connections
        let posMatchesVertex (pos:XYPos) (vertex: float*float) =
            let epsilon = 0.00001
            abs (abs pos.X - abs (fst vertex)) < epsilon &&
            abs (abs pos.Y - abs (snd vertex)) < epsilon
            |> (fun b -> if not b then printf $"Bad wire endpoint match on {pos} {vertex}"; b else b)
        let newWX =
            conns 
            |> List.map ( fun conn ->
                            let inputId = InputPortId conn.Target.Id
                            let outputId = OutputPortId conn.Source.Id
                            let connId = ConnectionId conn.Id
                            let segments = issieVerticesToSegments connId conn.Vertices
                            let makeWirePosMatchSymbol inOut (wire:Wire) =
                                match inOut with
                                | true -> posMatchesVertex 
                                            (Symbol.getInputPortLocation model.Symbol inputId)
                                            (List.head conn.Vertices)
                                | false ->
                                          posMatchesVertex 
                                            (Symbol.getOutputPortLocation model.Symbol outputId) 
                                            (List.last conn.Vertices)
                                |> (fun b -> 
                                    if b then 
                                        wire 
                                    else
                                        let getS (connId:string) = 
                                            Map.tryFind connId model.Symbol.Ports
                                            |> Option.map (fun port -> port.HostId)
                                            |> Option.bind (fun symId -> Map.tryFind (ComponentId symId) model.Symbol.Symbols)
                                            |> Option.map (fun sym -> sym.Compo.Label)
                                        printfn $"Updating loaded wire from {getS conn.Source.Id}->{getS conn.Target.Id} of wire "
                                        updateWire model wire inOut)
                                
                                
                            connId,
                            { Id = ConnectionId conn.Id
                              InputPort = inputId
                              OutputPort = outputId
                              Color = HighLightColor.DarkSlateGrey
                              Width = 1
                              Segments = segments}
                            |> makeWirePosMatchSymbol false
                            |> makeWirePosMatchSymbol true
                        )
            |> Map.ofList
        
        let connIds =
            conns
            |> List.map (fun conn -> ConnectionId conn.Id)
            
        { model with Wires = newWX }, Cmd.ofMsg (MakeJumps connIds)

//---------------Other interface functions--------------------//
/// Checks if a wire intersects a bounding box by checking if any of its segments intersect
let wireIntersectsBoundingBox (wire : Wire) (bb : BoundingBox) =
    let segmentIntersectsBox segStart segEnd state _seg =
        match state with
        | true -> true
        | false -> segmentIntersectsBoundingBox bb segStart segEnd
    
    foldOverSegs segmentIntersectsBox false wire

///
let getIntersectingWires (wModel : Model) (selectBox : BoundingBox) : list<ConnectionId> = 
    wModel.Wires
    |> Map.map (fun id wire -> wireIntersectsBoundingBox wire selectBox)
    |> Map.filter (fun id boolVal -> boolVal)
    |> Map.toList
    |> List.map (fun (id,bool) -> id)

///searches if the position of the cursor is on a wire in a model
///Where n is 5 pixels adjusted for top level zoom
let getWireIfClicked (wModel : Model) (pos : XYPos) (n : float) : ConnectionId Option =
    let boundingBox = {BoundingBox.X = pos.X - n; Y = pos.Y - n; H = n*2.; W = n*2.}
    let intersectingWires = getIntersectingWires (wModel : Model) boundingBox
    List.tryHead intersectingWires

///
let pasteWires (wModel : Model) (newCompIds : list<ComponentId>) : (Model * list<ConnectionId>) =
    let oldCompIds = Symbol.getCopiedSymbols wModel.Symbol
    
    let pastedWires =
        let createNewWire (oldWire : Wire) : list<Wire> =
            let newId = ConnectionId(JSHelpers.uuid())
    
            match Symbol.getEquivalentCopiedPorts wModel.Symbol oldCompIds newCompIds (oldWire.InputPort, oldWire.OutputPort) with
            | Some (newInputPort, newOutputPort) ->

                let portOnePos, portTwoPos = Symbol.getTwoPortLocations wModel.Symbol (InputPortId newInputPort) (OutputPortId newOutputPort)
                let segmentList = makeInitialSegmentsList newId (portOnePos, portTwoPos)
                [
                    {
                        oldWire with
                            Id = newId;
                            InputPort = InputPortId newInputPort;
                            OutputPort = OutputPortId newOutputPort;
                            Segments = segmentList;
                    }
                ]
            | None -> []
        
        wModel.CopiedWires
        |> Map.toList
        |> List.map snd
        |> List.collect createNewWire
        |> List.map (fun wire -> wire.Id, wire)
        |> Map.ofList
    
    let newWireMap = Map.fold ( fun acc newKey newVal -> Map.add newKey newVal acc ) pastedWires wModel.Wires
    let pastedConnIds =
        pastedWires
        |> Map.toList
        |> List.map fst
        
    { wModel with Wires = newWireMap }, pastedConnIds

///
let getPortIdsOfWires (model: Model) (connIds: ConnectionId list) : (InputPortId list * OutputPortId list) =
    (([], []), connIds)
    ||> List.fold (fun (inputPorts, outputPorts) connId ->
            (model.Wires[connId].InputPort :: inputPorts, model.Wires[connId].OutputPort :: outputPorts))
