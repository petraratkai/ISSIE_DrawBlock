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
        EndOrientation : Orientation
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
    | UpdateWireType of WireType
    | ResetModel // For Issie Integration
    | LoadConnections of list<Connection> // For Issie Integration
    | Rotate of list<ComponentId>

/// Returns an XYPos shifted by length in an X or Y direction defined by orientation.
let addLengthToPos (position: XYPos) orientation length =
    match orientation with
    | Horizontal -> { position with X = position.X + length }
    | Vertical -> { position with Y = position.Y + length }

/// Returns the opposite orientation of the input orientation. (i.e. Horizontal becomes Vertical and vice-versa)
let switchOrientation =
    function
    | Horizontal -> Vertical
    | Vertical -> Horizontal

/// <summary> Applies a function which requires the segment start and end positions to the segments in a wire, 
/// threading an accumulator argument through the computation. Essentially a List.fold applied to the list of segments of a wire, but with access to each segment's absolute positions. </summary>
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
        let nextPos = addLengthToPos currPos currOrientation seg.Length
        let nextOrientation = switchOrientation currOrientation
        let nextState = folder currPos nextPos currState seg
        (nextState, nextPos, nextOrientation))
    |> (fun (state, _, _) -> state)

//-------------------------Debugging functions---------------------------------//

/// Formats a SegmentId for logging purposes.
let formatSegmentId (id: SegmentId) =
    id
    |> (fun (SegmentId str) -> str)
    |> (fun str -> str[0..2])

/// Formats a WireId for logging purposes
let formatWireId (id: ConnectionId) =
    id
    |> (fun (ConnectionId str) -> str)
    |> (fun str -> str[0..2])

/// Logs the given SegmentId and returns it unchanged. Used for debugging.
let logSegmentId (id:SegmentId) =
    printfn $"{formatSegmentId id}"; id

/// Logs the given Segment and returns it unchanged. Used for debugging.
let logSegment (seg:Segment) =
    printfn $"|{seg.Index}:{formatSegmentId seg.Id}|-Length: {seg.Length}"; seg

/// Logs the given ConnectionId and returns it unchanged. Used for debugging.
let logConnectionId (id:ConnectionId) =
        id
        |> (fun (ConnectionId str) -> str)
        |> (fun str -> printfn $"{str[0..2]}"; id)

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
    printfn "---- --------------"
    model

/// Formats an XYPos for logging purposes.
let formatXY (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"

/// Given a segment start and end position, finds the orientation of the segment. 
/// Returns None if the segment is neither horizontal nor vertical
let getSegmentOrientation (segStart: XYPos) (segEnd: XYPos) =
    if abs (segStart.X - segEnd.X) < XYPos.epsilon then
        Vertical
    else if abs (segStart.Y - segStart.Y) < XYPos.epsilon then
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

/// Gets a wire orientation given a port edge
let getOrientation (edge: Symbol.Edge) = 
    match edge with
    | Symbol.Top | Symbol.Bottom -> Vertical
    | Symbol.Left | Symbol.Right -> Horizontal


//-------------------------------Implementation code----------------------------//

//---------------------------------------------------------------------------------//
//--------------------TL319 CODE SECTION STARTS-------------------------------------//
//---------------------------------------------------------------------------------//

/// Converts a segment list into a list of vertices
let segmentsToVertices (segList:Segment list) (wire:Wire) = 
    ((wire.StartPos, wire.InitialOrientation),segList)
    ||> List.scan(fun (currPos, currOrientation) seg ->
        let (nextPos, nextOrientation) =
            match currOrientation with
            | Horizontal -> { currPos with X = currPos.X + seg.Length}, Vertical
            | Vertical -> { currPos with Y = currPos.Y + seg.Length}, Horizontal
        let nextState = (nextPos,nextOrientation)
        nextState)
    |> List.map ( fun (pos,_) -> pos.X,pos.Y)

/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, as well as the final port orientation 
/// this function returns a list of wire vertices
let makeInitialWireVerticesList (wireStartPos : XYPos) (wireEndPos : XYPos) (portOrientation : Symbol.Edge) = 
    let xStart, yStart, xEnd, yEnd = wireStartPos.X, wireStartPos.Y, wireEndPos.X, wireEndPos.Y
    match xStart - xEnd < 0 with
    | true -> 
        match yStart - yEnd < 0 with
        | true -> 
            match portOrientation with
            | Symbol.Top  ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = xEnd; Y = yStart};
                    {X = xEnd; Y = yEnd-5.0}; 
                    {X = xEnd; Y = yEnd-5.0};// Length 0 horizontal
                    {X = xEnd; Y = yEnd}] // Stick vertical
            | Symbol.Right ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = xEnd+20.; Y = yStart};
                    {X = xEnd+20.; Y = yEnd};
                    {X = xEnd+Wire.stickLength/2.; Y = yEnd}; 
                    {X = xEnd+Wire.stickLength/2.; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | Symbol.Bottom->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd+20.};
                    {X = xEnd; Y = yEnd+20.};
                    {X = xEnd; Y = yEnd+Wire.stickLength/2.}; 
                    {X = xEnd; Y = yEnd+Wire.stickLength/2.}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | Symbol.Left ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd};
                    {X = xEnd-Wire.stickLength/2.; Y = yEnd}; 
                    {X = xEnd-Wire.stickLength/2.; Y = yEnd}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
        | false -> 
            match portOrientation with
            | Symbol.Bottom ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = xEnd; Y = yStart};
                    {X = xEnd; Y = yEnd+Wire.stickLength/2.}; 
                    {X = xEnd; Y = yEnd+Wire.stickLength/2.}; //Length 0 hortizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | Symbol.Right ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = xEnd+20.; Y = yStart};
                    {X = xEnd+20.; Y = yEnd};
                    {X = xEnd+Wire.stickLength/2.; Y = yEnd}; 
                    {X = xEnd+Wire.stickLength/2.; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | Symbol.Top ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd-20.};
                    {X = xEnd; Y = yEnd-20.};
                    {X = xEnd; Y = yEnd-Wire.stickLength/2.}; 
                    {X = xEnd; Y = yEnd-Wire.stickLength/2.}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | Symbol.Left ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = (xEnd+xStart)/2.; Y = yStart};
                    {X = (xEnd+xStart)/2.; Y = yEnd};
                    {X = xEnd-Wire.stickLength/2.; Y = yEnd}; 
                    {X = xEnd-Wire.stickLength/2.; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
    | false-> 
        match yStart - yEnd < 0 with
        | true ->
            match portOrientation with
            | Symbol.Bottom ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = xStart+Wire.stickLength/2.+10.; Y = yStart}; //Small horizontal for dragging  
                    {X = xStart+Wire.stickLength/2.+10.; Y = yEnd+20.};
                    {X = xEnd; Y = yEnd+20.};
                    {X = xEnd; Y = yEnd+Wire.stickLength/2.}; 
                    {X = xEnd; Y = yEnd+Wire.stickLength/2.}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | Symbol.Right ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = xStart+Wire.stickLength/2.+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+Wire.stickLength/2.+10.; Y = yEnd};
                    {X = xEnd+Wire.stickLength/2.; Y = yEnd}; 
                    {X = xEnd+Wire.stickLength/2.; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | Symbol.Top ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = (yStart+yEnd)/2.}; //Length 0 vertical
                    {X = xEnd; Y = (yStart+yEnd)/2.};
                    {X = xEnd; Y = yEnd-Wire.stickLength/2.}; 
                    {X = xEnd; Y = yEnd-Wire.stickLength/2.}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | Symbol.Left ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = xStart+Wire.stickLength/2.+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+Wire.stickLength/2.+10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-20.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-20.; Y = yEnd};
                    {X = xEnd-Wire.stickLength/2.; Y = yEnd}; 
                    {X = xEnd-Wire.stickLength/2.; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
        | false ->
            match portOrientation with
            | Symbol.Top ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yEnd-20.}; //Length 0 vertical
                    {X = xEnd; Y = yEnd-20.};
                    {X = xEnd; Y = yEnd-Wire.stickLength/2.}; 
                    {X = xEnd; Y = yEnd-Wire.stickLength/2.}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | Symbol.Right ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = xStart+Wire.stickLength/2.+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+Wire.stickLength/2.+10.; Y = yEnd};
                    {X = xEnd+Wire.stickLength/2.; Y = yEnd}; 
                    {X = xEnd+Wire.stickLength/2.; Y = yEnd}; //Lenght 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal
            | Symbol.Bottom ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = (yStart+yEnd)/2.}; //Length 0 vertical
                    {X = xEnd; Y = (yStart+yEnd)/2.};
                    {X = xEnd; Y = yEnd-Wire.stickLength/2.}; 
                    {X = xEnd; Y = yEnd-Wire.stickLength/2.}; //Length 0 horizontal
                    {X = xEnd; Y = yEnd}] //Stick vertical
            | Symbol.Left ->  [{X = xStart; Y = yStart};
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Stick horizontal
                    {X = xStart+Wire.stickLength/2.; Y = yStart}; //Length 0 vertical
                    {X = xStart+Wire.stickLength/2.+10.; Y = yStart}; //Small horizontal for dragging
                    {X = xStart+Wire.stickLength/2.+10.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-20.; Y = (yStart+yEnd)/2.}; 
                    {X = xEnd-20.; Y = yEnd};
                    {X = xEnd-Wire.stickLength/2.; Y = yEnd}; 
                    {X = xEnd-Wire.stickLength/2.; Y = yEnd}; //Length 0 vertical
                    {X = xEnd; Y = yEnd}] //Stick horizontal

/// Converts a list of vertices into a list of segments
let xyVerticesToSegments connId (xyVerticesList: XYPos list) =
    List.pairwise xyVerticesList
    |> List.mapi (
        fun i ({X=xStart; Y=yStart},{X=xEnd; Y=yEnd}) ->    
            let id = SegmentId(JSHelpers.uuid())
            {
                Id = id
                Index = i
                Length = xEnd-xStart+yEnd-yStart
                HostId  = connId;
                IntersectCoordinateList = [] ; // To test jump and modern wire types need to manually insert elements into this list.
                Mode = Auto
                Draggable =
                    if i = 0 || i = xyVerticesList.Length - 2 then
                        false
                    else
                        true
            })

/// Convert a (possibly legacy) issie Connection stored as a list of vertices to Wire
let issieVerticesToSegments 
        (connId) 
        (verticesList: list<float*float>) =
    let xyVerticesList =
        verticesList
        |> List.map (fun (x,y) -> {X=x;Y=y})

    let makeSegmentsFromVertices (xyList: XYPos list) =
        xyVerticesToSegments connId  xyList
        
    makeSegmentsFromVertices xyVerticesList


/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, as well as the orientation of the final port
/// this function returns a list of Segment(s).
let makeInitialSegmentsList (hostId : ConnectionId) (startPos : XYPos) (endPos : XYPos) (portOrientation : Symbol.Edge) : list<Segment> =
    let xyPairs = makeInitialWireVerticesList startPos endPos portOrientation
    xyPairs
    |> xyVerticesToSegments hostId 

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
        Vertices = segmentsToVertices conn.Segments conn
    }

/// This function is given a list of ConnectionId and it
/// converts the corresponding BusWire.Wire(s) to a
/// list of Connections, offering an interface
/// between our implementation and Issie.
let extractConnections (wModel : Model) : list<Connection> = 
    wModel.Wires
    |> Map.toList
    |> List.map (fun (key, _) -> extractConnection wModel key)


//----------------------Rendering Functions----------------------//

/// A pair of vertices as well as any intersections or jumps on that segment
type AbsSegment = 
    {
    Start: XYPos
    End: XYPos
    IntersectCoordinateList: list<float * SegmentId>
    }

/// Type passed to wire renderer functions
type WireRenderProps =
    {
        key: string
        AbsSegments: list<AbsSegment>
        ColorP: HighLightColor
        StrokeWidthP: int
        OutputPortEdge : Symbol.Edge
        OutputPortLocation: XYPos
        DisplayType : WireType
    }

///Function to create the SVG command required to path the entire wire if the display type is radial
let renderRadialWire (state : (string * Orientation)) (segmentpair :AbsSegment * AbsSegment)= 
    let startFirstSegment = fst(segmentpair).Start
    let endFirstSegment = fst(segmentpair).End
    let startSecondSegment = snd(segmentpair).Start
    let endSecondSegment = snd(segmentpair).End
    if startFirstSegment.X = endFirstSegment.X && 
       startFirstSegment.X = startSecondSegment.X &&
       startFirstSegment.X = endSecondSegment.X then
        let current = sprintf "L %f %f" endFirstSegment.X endFirstSegment.Y
        if snd(state) = Horizontal then
            (fst(state)+current, Vertical)
        else 
            (fst(state)+current, Horizontal)
    else if startFirstSegment.Y = endFirstSegment.Y && 
            startFirstSegment.Y = startSecondSegment.Y && 
            startFirstSegment.Y = endSecondSegment.Y then
        let current = sprintf "L %f %f" (endFirstSegment.X) (endFirstSegment.Y)
        if snd(state) = Horizontal then
            (fst(state)+current, Vertical)
        else 
            (fst(state)+current, Horizontal)           
    else
        if snd(state) = Horizontal then
            if startFirstSegment.X - endFirstSegment.X > 0 then
                if startSecondSegment.Y - endSecondSegment.Y > 0 then
                    let (current :string) =  sprintf "L %f %f A %i %i, %i, %i, %i, %f %f" (endFirstSegment.X+5.) (endFirstSegment.Y) 5 5 45 0 1 (startSecondSegment.X) (startSecondSegment.Y-5.)
                    let next = fst(state) + current
                    (next, Vertical)
                else
                    let (current :string) =  sprintf "L %f %f A %i %i, %i, %i, %i, %f %f" (endFirstSegment.X+5.) (endFirstSegment.Y) 5 5 45 0 0 (startSecondSegment.X) (startSecondSegment.Y+5.)
                    let next = fst(state) + current
                    (next, Vertical)
            else
                if startSecondSegment.Y - endSecondSegment.Y > 0 then
                    let (current :string) =  sprintf "L %f %f A %i %i, %i, %i, %i, %f %f"(endFirstSegment.X-5.) (endFirstSegment.Y) 5 5 45 0 0 (startSecondSegment.X) (startSecondSegment.Y-5.)
                    let next = fst(state) + current
                    (next, Vertical)
                else
                    let (current :string) =  sprintf "L %f %f A %i %i, %i, %i, %i, %f %f" (endFirstSegment.X-5.) (endFirstSegment.Y) 5 5 0 0 1 (startSecondSegment.X) (startSecondSegment.Y+5.)
                    let next = fst(state) + current
                    (next, Vertical)
        else
            if startFirstSegment.Y - endFirstSegment.Y > 0 then
                if startSecondSegment.X - endSecondSegment.X > 0 then
                    let (current :string) =  sprintf "L %f %f A %i %i, %i, %i, %i, %f %f"(endFirstSegment.X) (endFirstSegment.Y+5.) 5 5 0 0 0 (startSecondSegment.X-5.) (startSecondSegment.Y)
                    let next = fst(state) + current
                    (next, Horizontal)
                else
                    let (current :string) =  sprintf "L %f %f A %i %i, %i, %i, %i, %f %f" (endFirstSegment.X) (endFirstSegment.Y+5.) 5 5 0 0 1 (startSecondSegment.X+5.) (startSecondSegment.Y)
                    let next = fst(state) + current
                    (next, Horizontal)
            else
                if startSecondSegment.X - endSecondSegment.X > 0 then
                    let (current :string) =  sprintf "L %f %f A %i %i, %i, %i, %i, %f %f"(endFirstSegment.X) (endFirstSegment.Y-5.) 5 5 0 0 1 (startSecondSegment.X-5.) (startSecondSegment.Y)
                    let next = fst(state) + current
                    (next, Horizontal)
                else
                    let (current :string) =  sprintf "L %f %f A %i %i, %i, %i, %i, %f %f" (endFirstSegment.X) (endFirstSegment.Y-5.) 5 5 0 0 0 (startSecondSegment.X+5.) (startSecondSegment.Y)
                    let next = fst(state) + current
                    (next, Horizontal) 

///Function used to render a single wire if the display type is modern
let renderModernSegment (param : {| AbsSegment : AbsSegment; Colour :string; Width : string|}) = 
    let startVertex = param.AbsSegment.Start
    let endVertex = param.AbsSegment.End
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse param.Width
    let renderWidth = 
        match widthOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    let lineParameters = { defaultLine with Stroke = param.Colour; StrokeWidth = string renderWidth }
    let circleParameters = { defaultCircle with R = 2.5; Stroke = param.Colour;  Fill = param.Colour } 
    
    let circles =
        if (startVertex.X - endVertex.X > 0) then //Segment is right to left
            param.AbsSegment.IntersectCoordinateList 
            |> List.map (fun x -> startVertex.X - fst(x))
            |> List.map (fun x -> makeCircle x startVertex.Y circleParameters)
        else                                      //Segment is left to right
            param.AbsSegment.IntersectCoordinateList 
            |> List.map (fun x -> startVertex.X + fst(x))
            |> List.map (fun x -> makeCircle x startVertex.Y circleParameters)
    
    //Only ever render intersections on horizontal segments
    if getSegmentOrientation startVertex endVertex = Horizontal then 
        makeLine startVertex.X startVertex.Y endVertex.X endVertex.Y lineParameters
        :: circles
    else
        [makeLine startVertex.X startVertex.Y endVertex.X endVertex.Y lineParameters]
        
///Function used to render a single segment if the display type is jump
let renderJumpSegment (param : {| AbsSegment : AbsSegment; Colour :string; Width : string|}) = 
    let startVertex = param.AbsSegment.Start
    let endVertex = param.AbsSegment.End
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse param.Width
    let renderWidth = 
        match widthOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    let lineParameters = { defaultLine with Stroke = param.Colour; StrokeWidth = string renderWidth }
    let pathParameters = { defaultPath with Stroke = param.Colour; StrokeWidth = string renderWidth;}

    //Generate all sections of a left to right segment that don't have jumps
    let lefttoright (state : (float * ReactElement List)) xPos =
        let element =
            makeLine (fst(state)) startVertex.Y (xPos-5.) endVertex.Y lineParameters
        (xPos+5.,List.append (snd(state)) [element])

    //Generate all sections of a right to left segment that don't have jumps
    let righttoleft (state : (float * ReactElement List)) xPos =
        let element =
            makeLine (fst(state)) startVertex.Y (xPos+5.) endVertex.Y lineParameters
        (xPos-5.,List.append (snd(state)) [element])
    
    //If no jumps then straight line
    if List.isEmpty param.AbsSegment.IntersectCoordinateList then 
        [makeLine startVertex.X startVertex.Y endVertex.X endVertex.Y lineParameters]
    else
        if startVertex.X - endVertex.X < 0 then //Segment is left to right
            let jumps =
                param.AbsSegment.IntersectCoordinateList 
                |> List.map (fun x -> startVertex.X + fst(x))
                |> List.map (fun x -> makePath {X = x - 5.; Y = startVertex.Y} {X = x - 5.; Y = startVertex.Y - 7.} {X = x + 5.; Y = startVertex.Y - 7.} {X = x + 5.; Y = startVertex.Y} pathParameters)
            let lines =
                param.AbsSegment.IntersectCoordinateList
                |> List.map (fun x -> startVertex.X + fst(x))
                |> List.sort
                |> List.fold lefttoright (startVertex.X,[])
            let finalLines = 
                List.append (snd(lines)) [makeLine (fst(lines)) startVertex.Y endVertex.X endVertex.Y lineParameters]
            
            //Only ever render intersections on horizontal segments
            if getSegmentOrientation startVertex endVertex = Horizontal then
                List.append finalLines jumps
            else
                [makeLine startVertex.X startVertex.Y endVertex.X endVertex.Y lineParameters]
        
        else                                    //Segment is right to left
            let jumps =
                param.AbsSegment.IntersectCoordinateList 
                |> List.map (fun x -> startVertex.X - fst(x))
                |> List.map (fun x -> makePath {X = x - 5.; Y = startVertex.Y} {X = x - 5.; Y = startVertex.Y - 7.} {X = x + 5.; Y = startVertex.Y - 7.} {X = x + 5.; Y = startVertex.Y} pathParameters)
            let lines =
                param.AbsSegment.IntersectCoordinateList
                |> List.map (fun x -> startVertex.X - fst(x))
                |> List.sortDescending
                |> List.fold righttoleft (startVertex.X,[])
            let finalLines = 
                List.append (snd(lines)) [makeLine (fst(lines)) startVertex.Y endVertex.X endVertex.Y lineParameters]
            
            //Only ever render intersections on horizontal segments
            if getSegmentOrientation startVertex endVertex = Horizontal then
                List.append finalLines jumps
            else
                [makeLine startVertex.X startVertex.Y endVertex.X endVertex.Y lineParameters]


///Function used to render a single wire if the display type is jump
let singleWireJumpView props = 
    let firstVertex = props.AbsSegments.Head.Start
    let secondVertex = props.AbsSegments.Head.End
    let lastVertex = (List.last props.AbsSegments).Start
    let colour = props.ColorP.Text()
    let width = string props.StrokeWidthP
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match widthOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    
    let renderedSegmentList : ReactElement List = 
        props.AbsSegments
        |> List.map (fun x -> {|AbsSegment = x; Colour = colour; Width = width|})
        |> List.collect renderJumpSegment 

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
    
    g [] ([ renderWireWidthText ] @ renderedSegmentList)

///Function used to render a single wire if the display type is modern
let singleWireModernView props = 
    let firstVertex = props.AbsSegments.Head.Start
    let secondVertex = props.AbsSegments.Head.End
    let lastVertex = (List.last props.AbsSegments).End
    let colour = props.ColorP.Text()
    let width = string props.StrokeWidthP
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match widthOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    
    let renderedSegmentList : ReactElement List = 
        props.AbsSegments
        |> List.map (fun x -> {|AbsSegment = x; Colour = colour; Width = width|})
        |> List.collect renderModernSegment //colour width //(props.ColorP.Text()) (string props.StrokeWidthP)

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
    g [] ([ renderWireWidthText ] @ renderedSegmentList)

///Function used to render a single wire if the display type is radial
let singleWireRadialView props =
    let firstVertex = props.AbsSegments.Head.Start
    let secondVertex = props.AbsSegments.Head.End
    let lastVertex = (List.last props.AbsSegments).End

    let width = string props.StrokeWidthP
    let widthOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match widthOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5

    let pathParameters = { defaultPath with Stroke = props.ColorP.Text(); StrokeWidth = string renderWidth;}
    let initialMoveCommand = sprintf "M %f %f "  firstVertex.X firstVertex.Y
    let initialState = (initialMoveCommand, getSegmentOrientation firstVertex secondVertex )
    
    let radialPathCommands = fst(
        props.AbsSegments
        |> List.pairwise
        |> List.fold renderRadialWire (initialState) )
    let finalLineCommand = sprintf "L %f %f" lastVertex.X lastVertex.Y
    let fullPathCommand = radialPathCommands + finalLineCommand
    
    let makeCustomPath command (pathParameters: Path) =
        let dAttrribute = command 
        path [
            D dAttrribute
            SVGAttr.Stroke pathParameters.Stroke
            SVGAttr.StrokeWidth pathParameters.StrokeWidth
            SVGAttr.StrokeDasharray pathParameters.StrokeDashArray
            SVGAttr.StrokeLinecap pathParameters.StrokeLinecap
            SVGAttr.Fill pathParameters.Fill
        ]   []

    let renderedSVGPath = makeCustomPath fullPathCommand pathParameters
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

    g [] ([ renderWireWidthText ] @ [renderedSVGPath])

/// Function that will render all of the wires within the model, with the display type being set in Model.Type
let view (model : Model) (dispatch : Dispatch<Msg>) =
    let start = TimeHelpers.getTimeMs()
    let wires' =
        model.Wires
        |> Map.toArray
        |> Array.map snd
    TimeHelpers.instrumentTime "WirePropsSort" start
    let rStart = TimeHelpers.getTimeMs()
    let wires =
        wires'
        |> Array.map
            (
                fun wire ->
                    let stringOutId =
                        match wire.OutputPort with
                        | OutputPortId stringId -> stringId
                        
                    let outputPortLocation = Symbol.getPortLocation model.Symbol stringOutId 
                    let outputPortEdge = Symbol.getOutputPortOrientation model.Symbol wire.OutputPort 
                    let getAbsSegments wire : AbsSegment List= 
                        segmentsToVertices wire.Segments wire
                        |> List.map (fun x -> {X=fst(x); Y=snd(x)})
                        |> List.pairwise
                        |> List.zip wire.Segments
                        |> List.map (fun x -> fst(snd(x)),snd(snd(x)), fst(x).IntersectCoordinateList)
                        |> List.map (fun (startVertex,endVertex,intersectList) -> {Start=startVertex; End=endVertex; IntersectCoordinateList=intersectList})
                    
                    let props =
                        {
                            key = match wire.Id with | ConnectionId s -> s
                            AbsSegments = getAbsSegments wire
                            ColorP = wire.Color
                            StrokeWidthP = wire.Width
                            OutputPortEdge = outputPortEdge
                            OutputPortLocation = outputPortLocation
                            DisplayType = wire.Type
                        }
                    //To test other display types need to change 2nd match to the relevant 
                    //singleWire_View as section 3 has not yet implemented the model.Type properly
                    match  model.Type with    
                    | Radial -> singleWireRadialView props
                    | Jump -> singleWireJumpView props
                    | Modern -> singleWireModernView props
            )
    TimeHelpers.instrumentInterval "WirePrepareProps" rStart ()
    let symbols = Symbol.view model.Symbol (Symbol >> dispatch)
 
    g [] [(g [] wires); symbols]
    |> TimeHelpers.instrumentInterval "WireView" start

//---------------------------------------------------------------------------------//
//--------------------TL319 CODE SECTION ENDS-------------------------------------//
//---------------------------------------------------------------------------------//

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
let toY (pos: XYPos) = pos.Y

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

/// Returns true if p1 is less than or equal to p2 (has both smaller X and Y values
let lThanEqualPos (p1: XYPos) (p2: XYPos) : bool =
    p1.X <= p2.X && p1.Y <= p2.Y

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
        qLo <= qHi

    (intersect1D toX) && (intersect1D toY)

/// Checks if a segment intersects a bounding box using the segment's start and end XYPos
let segmentIntersectsBoundingBox (bb: BoundingBox) segStart segEnd =
    let toRect p1 p2 =
        let topLeft, bottomRight =
            if lThanEqualPos p1 p2 then
                p1, p2
            else
                p2, p1

        { TopLeft = topLeft
          BottomRight = bottomRight }

    let bbBottomRight =
        { X = bb.TopLeft.X + bb.W
          Y = bb.TopLeft.Y + bb.H }

    let bbRect = toRect bb.TopLeft bbBottomRight
    let segRect = toRect segStart segEnd

    rectanglesIntersect bbRect segRect

/// Returns the distance between a point and a segment defined by a start and end XYPos, and None if the segment is of 0 length (can't be clicked)
let distanceBetweenPointAndSegment (segStart : XYPos) (segEnd : XYPos) (point : XYPos) : float option = 
    match squaredDistance segStart segEnd with
    | 0. -> None
    | l2 -> 
        // Extend the segment to line segStart + t (segEnd - segStart)
        // The projection of point on this line falls at tProjection
        let tProjection = dotProduct (posDiff point segStart) (posDiff segEnd segStart) / l2 
        let tBounded = max 0. (min 1. tProjection) // Bound tProjection to be within the segment
        let boundedProjection = 
            posDiff segEnd segStart
            |> scalePos tBounded
            |> posAdd segStart
        Some (sqrt (squaredDistance point boundedProjection))

/// Finds the Id of the closest segment in a wire to a mouse click using euclidean distance
let getClickedSegment (model: Model) (wireId: ConnectionId) (mouse: XYPos) : SegmentId =
    let closestSegment segStart segEnd state (seg: Segment) =
        let currDist = 
            distanceBetweenPointAndSegment segStart segEnd mouse
        match state with
        | Some (minId, minDist) ->
            let dist = Option.defaultValue minDist currDist
            if dist < minDist then
                Some (seg.Id, dist)
            else
                Some (minId, minDist)
        | None -> Option.map (fun dist -> (seg.Id, dist)) currDist // Needed to deal with initial state

    match foldOverSegs closestSegment None model.Wires[wireId] with
    | Some (segmentId, dist) -> segmentId 
    | None -> failwithf "getClosestSegment was given a wire with no segments" // Should never happen

/// Returns a distance for a wire move that has been reduced if needed to enforce minimum first/last segment lengths.
let getSafeDistanceForMove (segments: Segment list) (index: int) (distance: float) =
    /// Returns a list of segments up to the first non-zero segment perpendicular to the segment leaving the port
    let findBindingSegments portIndex segList = 
        segList
        |> List.takeWhile (fun seg -> seg.Index % 2 = portIndex % 2 || seg.Length = 0)

    let findInputBindingIndex boundSegList =
        boundSegList
        |> List.length

    let findOutputBindingIndex boundSegList =
        boundSegList
        |> findInputBindingIndex
        |> (-) (segments.Length - 1)

    let findDistanceFromPort boundSegList =
        (0., boundSegList)
        ||> List.fold (fun dist seg -> dist + seg.Length) // Since the segments in perpendicular direction are 0 we can just sum up all the segments as if they are in the same direction
   
    let reduceDistance bindingSegs findBindingIndex distance = 
        if findBindingIndex bindingSegs <> index then 
            distance
        else
            findDistanceFromPort bindingSegs
            |> (fun dist -> 
                    if sign dist = -1 then 
                        max distance (dist + Wire.stickLength/2.)
                    else 
                        min distance (dist - Wire.stickLength/2.))

    let bindingInputSegs = 
        segments
        |> findBindingSegments 0
        |> List.map (fun seg -> { seg with Length = -seg.Length})

    let bindingOutputSegs =
        List.rev segments
        |> findBindingSegments (segments.Length - 1)

    distance
    |> reduceDistance bindingInputSegs findInputBindingIndex
    |> reduceDistance bindingOutputSegs findOutputBindingIndex

/// Returns a wire containing the updated list of segments after a segment is moved by 
/// a specified distance. The moved segment is tagged as manual so that it is no longer auto-routed.
/// Throws an error if the segment being moved is out of range.
let moveSegment (model:Model) (seg:Segment) (distance:float) = 
    let wire = model.Wires[seg.HostId]
    let segments = wire.Segments
    let idx = seg.Index

    if idx <= 0 || idx >= segments.Length - 1 then
        failwithf $"Trying to move wire segment {seg.Index}:{formatSegmentId seg.Id}, out of range in wire length {segments.Length}"

    let safeDistance = getSafeDistanceForMove segments idx distance
    
    let prevSeg = segments[idx - 1]
    let nextSeg = segments[idx + 1]
    let movedSeg = segments[idx]

    let newPrevSeg = { prevSeg with Length = prevSeg.Length + safeDistance }
    let newNextSeg = { nextSeg with Length = nextSeg.Length - safeDistance }
    let newMovedSeg = { movedSeg with Mode = Manual }
    
    let newSegments = segments[.. idx - 2] @ [newPrevSeg; newMovedSeg; newNextSeg] @ segments[idx + 2 ..]

    {wire with Segments = newSegments}

/// Initialises an empty Wire Model
let init () = 
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

///Returns the wires in the model connected to a list of components given by componentIds
let getConnectedWires (model: Model) (compIds: list<ComponentId>) =
    let containsPorts wire =
        let inputPorts, outputPorts =
            Symbol.getPortLocations model.Symbol compIds

        Map.containsKey wire.InputPort inputPorts
        || Map.containsKey wire.OutputPort outputPorts

    model
    |> getWireList
    |> List.filter containsPorts

/// Returns the IDs of the wires in the model connected to a list of components given by compIds
let getConnectedWireIds model compIds =
    getConnectedWires model compIds
    |> getFilteredIdList (fun _ -> true)

/// Given a model and a list of component Ids, returns an anaonymous record
/// containing the id of wires connected to input ports, output ports or both
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

    {| Inputs = inputWires; Outputs = outputWires; Both = fullyConnected |}

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

/// Returns a port rotated 90 degrees anticlockwise about the origin
let rotate90Port (port: PortInfo) =
    let newEdge = rotate90Edge port.Edge

    let newPos =
        { X = port.Position.Y
          Y = -port.Position.X }

    genPortInfo newEdge newPos

/// Returns a function to rotate a segment list 90 degrees anticlockwise (actually maybe clockwise? doesn't matter lol),
/// depending on its initial orientation
let rotateSegments90 initialOrientation =
    let horizontal i =
        match initialOrientation with
        | Horizontal -> i % 2 = 0
        | Vertical -> i % 2 = 1

    let rotateSegment (i, seg) =
        if (horizontal i) then
            { seg with Length = -seg.Length }
        else
            seg

    List.indexed
    >> List.map rotateSegment

/// Returns a version of the start and destination ports rotated until the start edge matches the target edge.
let rec rotateStartDest (target: Symbol.Edge) ((start, dest): PortInfo * PortInfo) = 
    if start.Edge = target then
        (start, dest)
    else
        rotateStartDest target (rotate90Port start, rotate90Port dest)


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
        makeInitialSegmentsList wire.Id normalisedStart.Position normalisedEnd.Position normalisedEnd.Edge

    let segments =
        {| edge = Symbol.Right
           segments = initialSegments |}
        |> rotateSegments startEdge
        |> (fun wire -> wire.segments)

    { wire with
          Segments = segments
          InitialOrientation = getOrientation startEdge
          EndOrientation = getOrientation destEdge
          StartPos = startPos
          EndPos = destPos }


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
//  S0.FH  S1.V  S2.H  S3.V  S4.H  S5.0V S6.FH (not sure if H and V are correct here) 
//
// To determine adjustment on End change we just reverse the segment and apply the Start change algorithm
// Adjustment => reverse list of segments, swap Start and End, and alter the sign of all coordinates
// For simplicity, due to the encoding of manual changes into coordinates by negating them (yuk!)
// we do not alter coordinate sign. Instead we invert all numeric comparisons.
// There are no constants used in the algorithm (if there were, they would need to be negated)
//
// ======================================================================================================================

/// Transforms the segments in segList. Applies tX to the horizontal segments and tY to the vertical segments.
let transformSegments tX tY (initialOrientation: Orientation) segList =
    // If a segment is horizontal, applies X transformation, if it is vertical applies Y transformation
    let transformSeg orientation (seg: Segment) =
        match orientation with
        | Horizontal -> { seg with Length = tX seg.Length }
        | Vertical -> { seg with Length = tY seg.Length }

    ((initialOrientation, []), segList)
    ||> List.fold (fun (orientation, segs) seg ->
        (switchOrientation orientation, segs @ [ transformSeg orientation seg ]))
    |> snd

/// Returns an anonymous record indicating the position of pos relative to origin.
/// The isAbove field indicates whether pos is above (true) or below (false) origin.
/// The isLeft field indicates whether pos is to the left (true) or to the right (false) of origin.
let relativePosition (origin: XYPos) (pos:XYPos) = 
    {| isLeft = origin.X > pos.X; isAbove = origin.Y > pos.Y |}

/// Reverses a wire so that it may be processed in the opposite direction. This function is self-inverse.
let reverseWire (wire: Wire) =
    let newSegs =
        List.rev wire.Segments
        |> List.indexed // I don't think we need to reverse the indices, test
        |> List.map (fun (i, seg) -> { seg with Length = -seg.Length; Index = i })

    { wire with
        Segments = newSegs
        StartPos = wire.EndPos
        EndPos = wire.StartPos
        InitialOrientation = wire.EndOrientation
        EndOrientation = wire.InitialOrientation }

/// Returns the tupel (startPos, endPos) of the segment at the target index in the given wire. Throws an error if the target index isn't found
let getAbsoluteSegmentPos (wire: Wire) (target: int) =
    (None, wire)
    ||> foldOverSegs
        (fun startPos endPos state seg ->
            if seg.Index = target then Some (startPos, endPos) else state)
    |> (function
        | None -> failwithf $"Couldn't find index {target} in wire"
        | Some pos -> pos)     

/// Returns the length to change a segment represented by startPos -> endPos in the appropriate dimension of the difference vector
let getLengthDiff difference startPos endPos =
    match getSegmentOrientation startPos endPos with
    | Horizontal -> toX difference
    | Vertical -> toY difference

/// Given a segment list, returns the first manual segment index
let getManualIndex segList =
    segList
    |> List.tryFind (fun seg -> seg.Mode = Manual)
    |> Option.map (fun seg -> seg.Index)
    |> Option.bind (fun index ->
        if index < 1 || index >= segList.Length - 1 then
            None
        else
            Some index)

/// Gets the start position for partial routing.
let getPartialRouteStart wire manualIndex =
    wire.Segments
    |> List.tryFind (fun seg -> seg.Index = manualIndex - 1)
    |> Option.map (fun seg -> seg.Index)
    |> Option.map (getAbsoluteSegmentPos wire >> fst)
    |> Option.defaultValue wire.StartPos

/// Partitions a segment list into sections 3 sections for partial autorouting
let partitionSegments segs manualIdx =
    let start, tmp =
        match manualIdx with
        | 1 -> ([], segs)
        | _ -> List.splitAt (manualIdx - 1) segs

    let changed, remaining = List.splitAt 2 tmp
    (start, changed, remaining)

/// Returns None if full autoroute is required or applies partial autorouting
/// from the start of the wire at newPortPos to the first manually routed segment 
/// and returns Some wire with the new segments.
let partialAutoRoute (wire: Wire) (newPortPos: XYPos) = 
    let segs = wire.Segments
    let newWire = { wire with StartPos = newPortPos }

    let eligibleForPartialRouting manualIdx =
        let oldStartPos = getPartialRouteStart wire manualIdx
        let newStartPos = getPartialRouteStart newWire manualIdx
        let fixedPoint = getAbsoluteSegmentPos wire manualIdx |> snd
        let relativeToFixed = relativePosition fixedPoint
        if relativeToFixed newStartPos = relativeToFixed oldStartPos then
            Some (manualIdx, posDiff newStartPos oldStartPos)
        else
            None
    
    let updateSegments (manualIdx, diff) =
        let start, changed, remaining = partitionSegments segs manualIdx
        let changed' = 
            changed
            |> List.map (fun seg -> 
                let (startPos, endPos) = getAbsoluteSegmentPos wire seg.Index
                { seg with Length = seg.Length - getLengthDiff diff startPos endPos })

        start @ changed' @ remaining
        
    segs
    |> getManualIndex
    |> Option.bind eligibleForPartialRouting
    |> Option.map updateSegments
    |> Option.map (fun segs -> { wire with Segments = segs; StartPos = newPortPos })

/// Moves a wire by the XY amounts specified by displacement
let moveWire (wire: Wire) (displacement: XYPos) =
    { wire with
          StartPos = posAdd wire.StartPos displacement
          EndPos = posAdd wire.EndPos displacement }

/// Returns a re-routed wire from the given model.
/// First attempts partial autorouting, and defaults to full autorouting if this is not possible.
/// Reverse indicates if the wire should be processed in backwards, used when an input port (end of wire) is moved.
let updateWire (model : Model) (wire : Wire) (reverse : bool) =
    let newPort = 
        match reverse with
        | true -> Symbol.getInputPortLocation model.Symbol wire.InputPort
        | false -> Symbol.getOutputPortLocation model.Symbol wire.OutputPort
    if reverse then
        partialAutoRoute (reverseWire wire) newPort
        |> Option.map reverseWire
    else 
        partialAutoRoute wire newPort
    |> Option.defaultValue (autorouteWire model wire)

//---------------------------------------------------------------------------------//
//--------------------STS219 CODE SECTION ENDS-------------------------------------//
//---------------------------------------------------------------------------------//

//---------------------------------------------------------------------------------//
//--------------------NH1019 CODE SECTION BEGINS-------------------------------------//
//---------------------------------------------------------------------------------//

let makeAllJumps (wiresWithNoJumps: ConnectionId list) (model: Model) =
    let mutable newWX = model.Wires
    // Arrays are faster to check than lists
    let wiresWithNoJumpsA = List.toArray wiresWithNoJumps
    let changeJumps wid index jumps =
        let jumps = List.sortDescending jumps
        let changeSegment (segs : Segment List)=
            List.mapi (fun i x -> if i <> index then x else { x with IntersectCoordinateList = jumps }) segs : Segment List

        newWX <- Map.add wid { newWX[wid] with Segments = changeSegment newWX[wid].Segments } newWX

    let wires =
        model.Wires
        |> Map.toArray
        |> Array.map (fun (_wid, w) -> w)

    let verticalSeg (segStart: XYPos) (segEnd: XYPos): bool =
            match getSegmentOrientation segStart segEnd with
            | Vertical -> true
            | Horizontal -> false


    for w1 in 0 .. wires.Length - 1 do
        let wire = wires[w1]
        if not (Array.contains wire.Id wiresWithNoJumpsA) then
            // Call folder function
            let findJumpsForSegment (segStart: XYPos) (segEnd: XYPos) (_state) (seg: Segment) =
                if not (verticalSeg segStart segEnd) then
                    let mutable jumps: (float * SegmentId) list = []
                    for w2 in 0 .. wires.Length - 1 do
                        let wire' = wires[w2]
                        if not (Array.contains wire'.Id wiresWithNoJumpsA) then
                            // Execute other folder function
                            let innerFold (segStart: XYPos) (segEnd: XYPos) (horizontalStart: XYPos, horizontalEnd: XYPos) (seg: Segment) =
                                if verticalSeg segStart segEnd then
                                    let x, x1, x2 = segStart.X, horizontalStart.X, horizontalEnd.X
                                    let y, y1, y2 = segStart.Y, horizontalStart.Y, horizontalEnd.Y
                                    let xhi, xlo = max x1 x2, min x1 x2
                                    let yhi, ylo = max y1 y2, min y1 y2

                                    if x < xhi - 5. && x > xlo + 5. && y < yhi - 5. && y > ylo + 5. then
                                        jumps <- (x, seg.Id) :: jumps
                                (horizontalStart, horizontalEnd)
                            foldOverSegs innerFold (segStart, segEnd) wire'
                            |> ignore

                    match jumps, seg.IntersectCoordinateList with
                    | [], [] -> ()
                    | [ a ], [ b ] when a <> b -> changeJumps seg.HostId seg.Index jumps
                    | [], _ -> changeJumps seg.HostId seg.Index jumps
                    | _, [] -> // in this case we need to sort the jump list
                        changeJumps seg.HostId seg.Index (List.sort jumps)
                    | newJumps, oldJ ->
                        let newJ = List.sort newJumps
                        if newJ <> oldJ then changeJumps seg.HostId seg.Index newJumps else ()
            foldOverSegs findJumpsForSegment () wire

    { model with Wires = newWX}

    (*
    let newJumps (segA: Segment) (segB: Segment) (segStart: XYPos) (segEnd: XYPos) =
         let mutable jumps: (float * SegmentId) list = []
         match verticalSeg segA segStart segEnd with
         | false -> 
            if not (Array.contains segA.HostId wiresWithNoJumpsA) then 
                 let y, x1, x2 = abs segStart.Y, abs segStart.X, abs segEnd.X
                 let xhi, xlo = max x1 x2, min x1 x2
                 match verticalSeg segB segStart segEnd with
                 | true -> if not (Array.contains segB.HostId wiresWithNoJumpsA) then
                             let x, y1, y2 = abs segStart.X, abs segStart.Y, abs segEnd.Y
                             let yhi, ylow = max y1 y2, min y1 y2
                             if x < xhi - 5. && x > xlo + 5. && y < yhi - 5. && y > ylow + 5. then 
                                 jumps <- (x, segB.Id) :: jumps
                 | _ -> ()
         | _ -> ()
         match jumps, segA.IntersectCoordinateList with
         | [], [] -> ()
         | [ a ], [ b ] when a <> b -> changeJumps segA.HostId segA.Index jumps
         | [], _ -> changeJumps segA.HostId segA.Index jumps
         | _, [] -> // in this case we need to sort the jump list
             changeJumps segA.HostId segA.Index (List.sort jumps)
         | newJumps, oldJ ->
             let newJ = List.sort newJumps
                // oldJ is already sorted (we only ever write newJ back to model)
             if newJ <> oldJ then changeJumps segA.HostId segA.Index newJumps else ()
    foldOverSegs 
    newJumps model.Wires
    { model with Wires = newWX } *)


let updateWireSegmentJumps (wireList: list<ConnectionId>) (wModel: Model) : Model =
    let startT = TimeHelpers.getTimeMs()
    //let model = makeAllJumps [] wModel
    TimeHelpers.instrumentTime "UpdateJumps" startT
    wModel



/// This function updates the wire model by removing from the stored lists of intersections
/// all those generated by wireList wires.
/// intersetcions are stored in maps on the model and on the horizontal segments containing the jumps
let resetWireSegmentJumps (wireList : list<ConnectionId>) (wModel : Model) : Model =
    wModel //makeAllJumps wireList wModel



   
        



/// Re-routes the wires in the model based on a list of components that have been altered.
/// If the wire input and output ports are both in the list of moved components, does not re-route wire but instead translates it.
/// Keeps manual wires manual (up to a point).
/// Otherwise it will auto-route wires connected to components that have moved
let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =

    let wires = filterWiresByCompMoved model compIdList

    let newWires = 
        model.Wires
        |> Map.toList
        |> List.map (fun (cId, wire) -> 
            if List.contains cId wires.Both //Translate wires that are connected to moving components on both sides
            then (cId, moveWire wire diff)
            elif List.contains cId wires.Inputs //Only route wires connected to ports that moved for efficiency
            then (cId, updateWire model wire true)
            elif List.contains cId wires.Outputs
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
        let wireWidthFromSymbol = WireWidth.Configured 1
        let wireId = ConnectionId(JSHelpers.uuid())
        let newWire = 
            {
                Id = wireId
                InputPort = inputId
                OutputPort = outputId
                Color = HighLightColor.DarkSlateGrey
                Width = 1
                Segments = []
                Type = Jump // CHANGE THIS
                StartPos = { X = 0; Y = 0 }
                EndPos = { X = 0; Y = 0 }
                InitialOrientation = Horizontal
                EndOrientation = Horizontal
            }
            |> autorouteWire model
        
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

                    match symbol.Component.Type with
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
                | _ -> failwithf $"segment Id {segId} not found in segment list"
            let seg = getSeg model.Wires[connId].Segments
            if seg.Draggable then
                let distanceToMove = 
                    let (segStart, segEnd) = getAbsoluteSegmentPos model.Wires[connId] seg.Index
                    match getSegmentOrientation segStart segEnd with
                    | Horizontal -> mMsg.Pos.Y - segStart.Y
                    | Vertical -> mMsg.Pos.X - segStart.X

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
                                            |> Option.map (fun sym -> sym.Component.Label)
                                        printfn $"Updating loaded wire from {getS conn.Source.Id}->{getS conn.Target.Id} of wire "
                                        updateWire model wire inOut)
                                
                                
                            connId,
                            { Id = ConnectionId conn.Id
                              InputPort = inputId
                              OutputPort = outputId
                              Color = HighLightColor.DarkSlateGrey
                              Width = 1
                              Segments = segments
                              Type = Jump // CHANGE THIS
                              StartPos = Symbol.getInputPortLocation model.Symbol inputId
                              EndPos = Symbol.getOutputPortLocation model.Symbol outputId
                              InitialOrientation = Symbol.getInputPortOrientation model.Symbol inputId |> getOrientation
                              EndOrientation = Symbol.getOutputPortOrientation model.Symbol outputId |> getOrientation }
                            |> makeWirePosMatchSymbol false
                            |> makeWirePosMatchSymbol true
                        )
            |> Map.ofList
        
        let connIds =
            conns
            |> List.map (fun conn -> ConnectionId conn.Id)
            
        { model with Wires = newWX }, Cmd.ofMsg (MakeJumps connIds)

    | UpdateWireType (style: WireType) ->
        let updateStyle =
            match style with
            | Jump -> model.Wires |> Map.map (fun id w -> {w with Type = Jump})
            | Radial -> model.Wires |> Map.map (fun id w -> {w with Type = Radial})
            | Modern -> model.Wires |> Map.map (fun id w -> {w with Type = Modern})

        { model with Wires = updateStyle }, Cmd.none

    | Rotate (componentIds: ComponentId list) ->
        let updatedWireEntries = 
            componentIds
            |> getConnectedWires model
            |> List.map (autorouteWire model)
            |> List.map (fun wire -> wire.Id, wire)
            |> Map.ofList
        
        let updatedWires = Map.fold (fun merged id wire -> Map.add id wire merged) model.Wires updatedWireEntries

        { model with Wires = updatedWires }, Cmd.none

//---------------Other interface functions--------------------//

/// Checks if a wire intersects a bounding box by checking if any of its segments intersect
let wireIntersectsBoundingBox (wire : Wire) (bb : BoundingBox) =
    let segmentIntersectsBox segStart segEnd state seg =
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
    let boundingBox = {BoundingBox.TopLeft = {X = pos.X - n; Y = pos.Y - n}; H = n*2.; W = n*2.}
    //printfn $"Click Bounding Box: {boundingBox}"
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
                [
                    {
                        oldWire with
                            Id = newId;
                            InputPort = InputPortId newInputPort;
                            OutputPort = OutputPortId newOutputPort;
                            Segments = [];
                            StartPos = { X = 0; Y = 0 };
                            EndPos = { X = 0; Y = 0 }
                    }
                    |> autorouteWire wModel
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

//---------------------------------------------------------------------------------//
//--------------------NH1019 CODE SECTION ENDS-------------------------------------//
//---------------------------------------------------------------------------------//