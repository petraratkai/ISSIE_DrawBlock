﻿(*
This module draws schematics component symbols. Each symbol is associated with a unique Issie component.
*)

module Symbol
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers
open CommonTypes
open System.Text.RegularExpressions


/// --------- STATIC VARIABLES --------- ///

let GridSize = 30 

/// ---------- SYMBOL TYPES ---------- ///
type Rotation = | Degree0 | Degree90 | Degree180 | Degree270

type STransform = {Rotation: Rotation; flipped: bool}

type Edge = | Top | Bottom | Left | Right

type PortId = | InputId of InputPortId | OutputId of OutputPortId

type Symbol =
    {
        Pos: XYPos
        InWidth0: int option
        InWidth1: int option
        Id : ComponentId       
        Component : Component                 
        Colour: string
        ShowInputPorts: bool
        ShowOutputPorts: bool
        Opacity: float
        Moving: bool
        STransform: STransform
        PortOrientation: Map<string, Edge>
        PortOrder: Map<Edge, string list> //stores the order of ports on each edge
        //APortOffsetsMap: Map<string, XYPos>
    }

type Model = {
    Symbols: Map<ComponentId, Symbol>
    CopiedSymbols: Map<ComponentId, Symbol>
    Ports: Map<string, Port>                            // string since it's for both input and output ports

    InputPortsConnected:  Set<InputPortId>              // we can use a set since we only care if an input port 
                                                        // is connected or not (if so it is included) in the set 

    OutputPortsConnected: Map<OutputPortId, int>        // map of output port id to number of wires connected to that port
    }

//----------------------------Message Type-----------------------------------//


type Msg =
    | MouseMsg of MouseT
    | AddSymbol of pos:XYPos * compType:ComponentType * lbl: string
    | CopySymbols of ComponentId list
    | DeleteSymbols of sIds:ComponentId list
    | ShowAllInputPorts | ShowAllOutputPorts | DeleteAllPorts 
    | MoveSymbols of compList: ComponentId list * move: XYPos
    | ShowPorts of ComponentId list
    | SelectSymbols of ComponentId list// Issie interface
    | SymbolsHaveError of sIds: ComponentId list
    | ChangeLabel of sId : ComponentId * newLabel : string
    | PasteSymbols of sIds: ComponentId list
    | ColorSymbols of compList : ComponentId list * colour : HighLightColor
    | ErrorSymbols of errorIds: ComponentId list * selectIds: ComponentId list * isDragAndDrop: bool
    | ChangeNumberOfBits of compId:ComponentId * NewBits:int 
    | ChangeLsb of compId: ComponentId * NewBits:int64 
    | ChangeConstant of compId: ComponentId * NewBits:int64 * NewText:string
    | ResetModel // For Issie Integration
    | LoadComponents of  Component list // For Issie Integration
    | WriteMemoryLine of ComponentId * int64 * int64 // For Issie Integration 
    | WriteMemoryType of ComponentId * ComponentType
    | RotateLeft of compList : ComponentId list
    | RotateRight of compList: ComponentId list

//---------------------------------helper types and functions----------------//

let posDiff (a:XYPos) (b:XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd (a:XYPos) (b:XYPos) =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}


// ----- helper functions for titles ----- //

///Insert titles compatible with greater than 1 buswidth
let title t (n) =  
        if n = 1 then t else t + "(" + string(n-1) + "..0)"

///Insert titles for bus select
let bustitle wob lsb = 
    if wob <> 1 then"(" + string(wob + lsb - 1) + ".." + string(lsb) +  ")" else string(lsb)

///Decodes the component type into component labels
let prefix compType = 
    match compType with
    | Not | And | Or | Xor | Nand | Nor | Xnor -> "G"
    | Mux2 -> "MUX"
    | Demux2 -> "DM"
    | NbitsAdder _ -> "A"
    | NbitsXor _ -> "XOR"
    | DFF | DFFE -> "FF"
    | Register _ | RegisterE _ -> "REG"
    | AsyncROM1 _ -> "AROM"
    | ROM1 _ -> "ROM"
    | RAM1 _ -> "RAM"
    | AsyncRAM1 _ -> "ARAM"
    | Custom c ->
        c.Name.ToUpper() + (if c.Name |> Seq.last |> System.Char.IsDigit then "." else "")
    | Constant1 _ -> "C"
    | BusCompare _ -> "EQ"
    | Decode4 -> "DEC"
    | BusSelection _ -> "SEL"
    | _ -> ""


//-----------------------------Skeleton Model Type for symbols----------------//

// Text to be put inside different Symbols depending on their ComponentType
let gateDecoderType (comp:Component) =
    match comp.Type with
    | And | Nand-> "&"
    | Or | Nor-> "≥1"
    | Xor | Xnor -> "=1"
    | Not -> "1"
    | Decode4 -> "Decode"
    | NbitsAdder n -> title "Adder" n
    | Register n | RegisterE n-> title "Register" n
    | AsyncROM1 _ -> "Async-ROM"
    | ROM1 _ -> "Sync-ROM"
    | RAM1 _ -> "Sync-RAM"
    | AsyncRAM1 _ -> "Async-RAM"
    | DFF -> "DFF"
    | DFFE -> "DFFE"
    | NbitsXor (x)->   title "N-bits-Xor" x
    | Custom x -> x.Name
    | _ -> ""

// Input and Output names of the ports depending on their ComponentType
let portDecName (comp:Component) = //(input port names, output port names)
    match comp.Type with
    | Decode4 -> (["Sel";"Data"],["0"; "1";"2"; "3"])
    | NbitsAdder _ -> (["Cin";"A";"B"],["Sum "; "Cout"])
    | Register _ -> (["D"],["Q"])
    | RegisterE _ -> (["D"; "EN"],["Q"])
    | ROM1 _ |AsyncROM1 _ -> (["Addr"],["Dout"])
    | RAM1 _ -> (["Addr"; "Din";"Wen" ],["Dout"])
    | AsyncRAM1 _ -> (["Addr"; "Din";"Wen" ],["Dout"])
    | DFF -> (["D"],["Q"])
    | DFFE -> (["D";"EN"],["Q"])
    | Mux2 -> (["0"; "1";"SEL"],["OUT"])
    | Demux2 -> (["IN" ; "SEL"],["0"; "1"])
    | NbitsXor _ -> (["P"; "Q"], ["Out"])
    | Custom x -> (List.map fst x.InputLabels), (List.map fst x.OutputLabels)
    |_ -> ([],[])
   // |Mux4 -> (["0"; "1"; "2"; "3" ;"SEL"],["OUT"])
   // |Demux4 -> (["IN"; "SEL"],["0"; "1";"2"; "3";])
   // |Demux8 -> (["IN"; "SEL"],["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7"])
   // |Mux8 -> (["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7";"SEL"],["OUT"])
   // |_ -> ([],[])
   // EXTENSION: Extra Components made that are not currently in Issie. Can be extended later by using this code as it is .

/// Genererates a list of ports:
let portLists numOfPorts hostID portType =
    if numOfPorts < 1 
    then []
    else
        [0..(numOfPorts-1)]
        |> List.collect (fun x ->
            [{
                Id = JSHelpers.uuid ()
                PortNumber = Some x
                PortType = portType
                HostId = hostID
            }])


//-----------------------Skeleton Message type for symbols---------------------//

///Rounds an integer to any given number. The first parameter is the number to round to, the second parameter is the input number that will be rounded
let roundToN (n : int) (x : int) =
    x + abs((x % n) - n)

let customToLength (lst : (string * int) list) =
    let labelList = List.map (fst >> String.length) lst
    if List.isEmpty labelList then 0 //if a component has no inputs or outputs list max will fail
    else List.max labelList

// helper function to initialise each type of component
let makeComp (pos: XYPos) (comptype: ComponentType) (id:string) (label:string) : Component =

    // function that helps avoid dublicate code by initialising parameters that are the same for all component types and takes as argument the others
    let makeComponent (n, nout, h, w) label : Component=  
        {
            Id = id 
            Type = comptype 
            Label = label 
            InputPorts = portLists n id PortType.Input 
            OutputPorts  = portLists nout id PortType.Output 
            X  = int (pos.X - float w / 2.0) 
            Y = int (pos.Y - float h / 2.0) 
            H = h 
            W = w
        }
    
    // match statement for each component type. the output is a 4-tuple that is used as an input to makecomponent (see below)
    // 4-tuple of the form ( number of input ports, number of output ports, Height, Width)
    let args = 
        match comptype with
        | ROM _ | RAM _ | AsyncROM _ -> 
            failwithf "What? Legacy RAM component types should never occur"
        | And | Nand | Or | Nor | Xnor | Xor ->  (2 , 1, 2*GridSize , 2*GridSize) 
        | Not -> ( 1 , 1, 2*GridSize ,  2*GridSize) 
        | ComponentType.Input (a) -> ( 0 , 1, GridSize ,  2*GridSize)                
        | ComponentType.Output (a) -> (  1 , 0, GridSize ,  2*GridSize) 
        | ComponentType.Viewer a -> (  1 , 0, GridSize ,  GridSize) 
        | ComponentType.IOLabel  ->(  1 , 1, GridSize ,  2*GridSize) 
        | Decode4 ->( 2 , 4 , 4*GridSize  , 3*GridSize) 
        | Constant1 (a, b,_) | Constant(a, b) -> (  0 , 1, GridSize ,  2*GridSize) 
        | MergeWires -> ( 2 , 1, 2*GridSize ,  2*GridSize) 
        | SplitWire (a) ->(  1 , 2 , 2*GridSize ,  2*GridSize) 
        | Mux2 -> ( 3  , 1, 3*GridSize ,  2*GridSize) 
        // EXTENSION:    | Mux4 -> ( 5  , 1, 5*GridSize ,  2*GridSize)   
        // EXTENSION:    | Mux8 -> ( 9  , 1, 7*GridSize ,  2*GridSize) 
        | Demux2 ->( 2  , 2, 3*GridSize ,  2*GridSize) 
        // EXTENSION:   | Demux4 -> ( 2  , 4, 150 ,  50) 
        // EXTENSION:    | Demux8 -> ( 2  , 8, 200 ,  50) 
        | BusSelection (a, b) -> (  1 , 1, GridSize,  2*GridSize) 
        | BusCompare (a, b) -> ( 1 , 1, GridSize ,  2*GridSize) 
        | DFF -> (  1 , 1, 3*GridSize  , 3*GridSize) 
        | DFFE -> ( 2  , 1, 3*GridSize  , 3*GridSize) 
        | Register (a) -> ( 1 , 1, 3*GridSize  , 4*GridSize )
        | RegisterE (a) -> ( 2 , 1, 3*GridSize  , 4*GridSize) 
        | AsyncROM1 (a)  -> (  1 , 1, 3*GridSize  , 4*GridSize) 
        | ROM1 (a) -> (   1 , 1, 3*GridSize  , 4*GridSize) 
        | RAM1 (a) | AsyncRAM1 a -> ( 3 , 1, 3*GridSize  , 4*GridSize) 
        | NbitsXor (n) -> (  2 , 1, 3*GridSize  , 4*GridSize) 
        | NbitsAdder (n) -> (  3 , 2, 3*GridSize  , 4*GridSize) 
        | Custom x -> 
            let h = GridSize + GridSize * (List.max [List.length x.InputLabels; List.length x.OutputLabels])
            let maxInLength, maxOutLength = customToLength x.InputLabels, customToLength x.OutputLabels
            let maxW = maxInLength + maxOutLength + label.Length
            let scaledW = roundToN GridSize (maxW * GridSize / 5) //Divide by 5 is just abitrary as otherwise the symbols would be too wide 
            let w = max scaledW (GridSize * 4) //Ensures a minimum width if the labels are very small
            ( List.length x.InputLabels, List.length x.OutputLabels, h ,  w)
                
    makeComponent args label
   
// Function to generate a new symbol
let createNewSymbol (pos: XYPos) (comptype: ComponentType) (label:string) =
    let id = JSHelpers.uuid ()
    let comp = makeComp pos comptype id label
    { 
      Pos = { X = pos.X - float comp.W / 2.0; Y = pos.Y - float comp.H / 2.0 }
      ShowInputPorts = false
      ShowOutputPorts = false
      InWidth0 = None // set by BusWire
      InWidth1 = None
      Colour = "lightgrey"
      Id = ComponentId id
      Compo = comp
      Opacity = 1.0
      Moving = false
    }

// Function to add ports to port model     
let addToPortModel (model: Model) (sym: Symbol) =
    let addOnePort (currentPorts: Map<string, Port>) (port: Port) =
        Map.add port.Id port currentPorts
    
    let addedInputPorts = (model.Ports, sym.Compo.InputPorts) ||> List.fold addOnePort
    (addedInputPorts, sym.Compo.OutputPorts) ||> List.fold addOnePort

//-----------------------------------------GET PORT POSITION---------------------------------------------------
// Function that calculates the positions of the ports 

/// hack so that bounding box of splitwire, mergewires can be smaller height relative to ports
let inline getPortPosEdgeGap (ct: ComponentType) =
    match ct with
    | MergeWires | SplitWire _  -> 0.25
    | _ -> 1.0

let getPortPos (comp: Component) (port:Port) = 
    let (ports, posX) =
        if port.PortType = (PortType.Input) then
            (comp.InputPorts, 0.0)
        else 
            (comp.OutputPorts, float( comp.W ))
    let index = float( List.findIndex (fun (p:Port)  -> p = port) ports )
    let gap = getPortPosEdgeGap comp.Type 
    let posY = (float(comp.H))* (( index + gap )/( float( ports.Length ) + 2.0*gap - 1.0))  // the ports are created so that they are equidistant
    {X = posX; Y = posY}
let getPortPosModel (model: Model) (port:Port) =
    getPortPos (Map.find (ComponentId port.HostId) model.Symbols).Compo port


//-----------------------------------------DRAWING HELPERS ---------------------------------------------------
// Text adding function with many parameters (such as bold, position and text)
let private addText posX posY name txtPos weight size=
    let text =
            {defaultText with TextAnchor = txtPos; FontWeight = weight; FontSize = size}
    [makeText posX posY name text]

// Generate circles
let private portCircles x y  = 
    [makeCircle x y portCircle]
// Define the name of each port 
let private portText x y name portType=
    let xPos = 
        if portType = PortType.Output
        then x - 5.
        else x + 5.
    let test = if portType = PortType.Output then "end" else "start"
    (addText xPos (y - 7.0) name test "normal" "12px")

// Print the name of each port 
let private drawPortsText (portList: Port List) (listOfNames: string List) (comp: Component)= 
    if listOfNames.Length < 1
        then  []
        else 
            [0..(portList.Length-1)]
            |> List.map2 (fun name x -> (portText (getPortPos comp portList[x]).X (getPortPos comp portList[x]).Y name (portList.Head.PortType))) listOfNames 
            |> List.collect id

// Function to draw ports using getPortPos. The ports are equidistant     
let private drawPorts (portList: Port List) (printPorts:bool) (comp: Component)= 
    if (portList.Length)  < 1 
    then []
    else
        if printPorts
        then [0..(portList.Length-1)] |> List.collect (fun x -> (portCircles (getPortPos comp portList[x]).X (getPortPos comp portList[x]).Y))
        else []

//------------------------------HELPER FUNCTIONS FOR DRAWING SYMBOLS-------------------------------------
let private createPolygon points colour opacity = 
    [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity}]

let createBiColorPolygon points colour strokeColor opacity strokeWidth= 
    if strokeColor <> "black" then 
        [makePolygon points {defaultPolygon with Fill = colour; Stroke = strokeColor; FillOpacity = opacity; StrokeWidth=strokeWidth}]
    else   
        [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity; StrokeWidth = strokeWidth}]

let addInvertor posX posY colour opacity =
    let points = (sprintf "%i,%i %i,%i %i,%i" posX (posY) (posX+9) (posY) posX (posY-8))
    createPolygon points colour opacity

let addClock posX posY colour opacity =
    let points = (sprintf "%i,%i %i,%i %i,%i" posX (posY-1) (posX+8) (posY-7) posX (posY-13))
    createPolygon points colour opacity
    |> List.append (addText (float(posX+10)) (float(posY-13)) " clk" "start" "normal" "12px")

let addHorizontalLine posX1 posX2 posY opacity = // TODO: Line instead of polygon?
    let points = (sprintf "%i,%f %i,%f" posX1 posY posX2 posY)
    createPolygon points "lightgray" opacity

let outlineColor (color:string) =
    match color.ToLower() with
    | "lightgray" | "lightgrey" -> "black"
    | c -> 
        printfn $"color={color}"
        c

let addHorizontalColorLine posX1 posX2 posY opacity (color:string) = // TODO: Line instead of polygon?
    let points = (sprintf "%i,%f %i,%f" posX1 posY posX2 posY)
    let olColor = outlineColor color
    [makePolygon points {defaultPolygon with Fill = "olcolor"; Stroke=olColor; StrokeWidth = "2.0"; FillOpacity = opacity}]



/// --------------------------------------- SYMBOL DRAWING ------------------------------------------------------ ///   

let compSymbol (symbol:Symbol) (comp:Component) (colour:string) (showInputPorts:bool) (showOutputPorts:bool) (opacity: float)= 
    let h = comp.H
    let w = comp.W
    let halfW = comp.W/2
    let halfH = (comp.H)/2

    let mergeSplitLine posX1 posX2 posY msb lsb =
        let text = 
            match msb = lsb, msb >= lsb with
            | _, false -> ""
            | true, _ -> sprintf $"({msb})"
            | false, _ -> sprintf $"({msb}:{lsb})"
        addHorizontalColorLine posX1 posX2 (posY*float(h)) opacity colour @
        addText (float (posX1 + posX2)/2.0) (posY*float(h)-11.0) text "middle" "bold" "9px"



    let points =            // Points that specify each symbol 
        match comp.Type with
        | Input _ -> (sprintf "%i,%i %i,%i %f,%i %i,%i %f,%i" 0 0 0 h (float(w)*(0.66)) h w halfH (float(w)*(0.66)) 0)
        | Constant1 _ -> (sprintf "%i,%i %i,%i %i,%i" 0 comp.H halfW halfH 0 0)
        | IOLabel -> (sprintf "%f,%i %i,%i %f,%i %f,%i %i,%i %f,%i"  (float(w)*(0.33)) 0 0 halfH (float(w)*(0.33)) h (float(w)*(0.66)) h w halfH (float(w)*(0.66)) 0)
        | Output _ -> (sprintf "%f,%i %i,%i %f,%i %i,%i %i,%i" (float(w)*(0.2)) 0 0 halfH (float(w)*(0.2)) h w h w 0)
        | Viewer _ -> (sprintf "%f,%i %i,%i %f,%i %i,%i %i,%i" (float(w)*(0.2)) 0 0 halfH (float(w)*(0.2)) h w h w 0)
        | MergeWires -> (sprintf "%i,%f %i,%f " halfW ((1.0/6.0)*float(h)) halfW ((5.0/6.0)*float(h)))
        | SplitWire _ ->  (sprintf "%i,%f %i,%f " halfW ((1.0/6.0)*float(h)) halfW ((5.0/6.0)*float(h)))
        | Demux2 -> (sprintf "%i,%f %i,%f %i,%i %i,%i" 0 (float(h)*0.2) 0 (float(h)*0.8) w h w 0)
        | Mux2 -> (sprintf "%i,%i %i,%f  %i,%f %i,%i" 0 0 w (float(h)*0.2) w (float(h)*0.8) 0 h )
        // EXTENSION: |Mux4|Mux8 ->(sprintf "%i,%i %i,%f  %i,%f %i,%i" 0 0 w (float(h)*0.2) w (float(h)*0.8) 0 h )
        // EXTENSION: | Demux4 |Demux8 -> (sprintf "%i,%f %i,%f %i,%i %i,%i" 0 (float(h)*0.2) 0 (float(h)*0.8) w h w 0)
        | BusSelection _ |BusCompare _ -> (sprintf "%i,%i %i,%i %f,%i %f,%f %i,%f %i,%f %f,%f %f,%i ")0 0 0 h (0.6*float(w)) h (0.8*float(w)) (0.7*float(h)) w (0.7*float(h)) w (0.3*float(h)) (0.8*float(w)) (0.3*float(h)) (0.6*float(w)) 0
        | _ -> (sprintf "%i,%i %i,%i %i,%i %i,%i" 0 (comp.H) comp.W (comp.H) comp.W 0 0 0)
    let additions =       // Helper function to add certain characteristics on specific symbols (inverter, enables, clocks)
        match comp.Type with
        | Constant1 (_,_,txt) -> (addHorizontalLine halfW w (float(halfH)) opacity @ addText (float (halfW)-5.0) (float(h)-8.0) txt "middle" "normal" "12px") 
        | Nand | Nor | Xnor |Not -> (addInvertor w halfH colour opacity)
        | MergeWires -> 
            let lo, hi = 
                match symbol.InWidth0, symbol.InWidth1  with 
                | Some n, Some m  -> n, m
                | _ -> -1,-1
            let msb = hi + lo - 1
            let midb = lo
            let midt = lo - 1
            mergeSplitLine 0 halfW (1.0/6.0) midt 0 @ 
            mergeSplitLine 0 halfW (5.0/6.0) msb midb @ 
            mergeSplitLine halfW w 0.5 msb 0
        | SplitWire mid -> 
            let msb, mid' = match symbol.InWidth0 with | Some n -> n - 1, mid | _ -> -100, -50
            let midb = mid'
            let midt = mid'-1
            mergeSplitLine halfW w (1.0/6.0) midt 0 @ 
            mergeSplitLine halfW w (5.0/6.0) msb midb @ 
            mergeSplitLine 0 halfW 0.5 msb 0
        | DFF |DFFE -> (addClock 0 h colour opacity)
        | Register _ |RegisterE _ -> (addClock 0 h colour opacity)
        | ROM1 _ |RAM1 _ | AsyncRAM1 _ -> (addClock 0 h colour opacity)
        | BusSelection(x,y) -> (addText  (float(w/2)-5.0) ((float(h)/2.7)-2.0) (bustitle x y) "middle" "normal" "12px")
        | BusCompare (_,y) -> (addText  (float(w/2)-6.0) (float(h)/2.7-1.0) ("=" + NumberHelpers.hex(int y)) "middle" "bold" "10px")
        | Input (x) -> (addText  (float(w/2)-5.0) ((float(h)/2.7)-3.0) (title "" x) "middle" "normal" "12px")
        | Output (x) -> (addText  (float(w/2)) ((float(h)/2.7)-3.0) (title "" x) "middle" "normal" "12px")
        | Viewer (x) -> (addText  (float(w/2)) ((float(h)/2.7)-1.25) (title "" x) "middle" "normal" "9px")
        | _ -> []

    let olColour, strokeWidth =
        match comp.Type with
        | SplitWire _ | MergeWires -> outlineColor colour, "2.0"
        | _ -> "black", "1.0"
   
    // Put everything together 
    
    (drawPorts comp.OutputPorts showOutputPorts comp)
    |> List.append (drawPorts comp.InputPorts showInputPorts comp)
    |> List.append (drawPortsText comp.InputPorts (fst(portDecName comp)) comp)
    |> List.append (drawPortsText comp.OutputPorts (snd(portDecName comp)) comp)  
    |> List.append (addText (float halfW) (+5.0) (gateDecoderType comp) "middle" "bold" "14px") 
    |> List.append (addText (float halfW) (-20.0) comp.Label "middle" "normal" "16px")
    |> List.append (additions)
    |> List.append (createBiColorPolygon points colour olColour opacity strokeWidth)

let init () = 
    { Symbols = Map.empty; CopiedSymbols = Map.empty; Ports = Map.empty ; InputPortsConnected= Set.empty ; OutputPortsConnected = Map.empty}, Cmd.none

//----------------------------View Function for Symbols----------------------------//
type private RenderSymbolProps =
    {
        Symbol : Symbol 
        Dispatch : Dispatch<Msg>
        key: string 
    }

/// View for one symbol. Using FunctionComponent.Of to improve efficiency (not printing all symbols but only those that are changing)
let private renderSymbol =
    
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let symbol = props.Symbol
            let ({X=fX; Y=fY}:XYPos) = symbol.Pos
            g ([ Style [ Transform(sprintf "translate(%fpx, %fpx)" fX fY) ] ]) (compSymbol props.Symbol props.Symbol.Compo symbol.Colour symbol.ShowInputPorts symbol.ShowOutputPorts symbol.Opacity)
            
        , "Symbol"
        , equalsButFunctions
        )
    
/// View function for symbol layer of SVG
let MapsIntoLists map =
    let listMoving = 
        Map.filter (fun _ sym -> not sym.Moving) map
        |>Map.toList
        |>List.map snd
    let listNotMoving =
        Map.filter (fun _ sym -> sym.Moving) map
        |>Map.toList
        |>List.map snd
    listMoving @ listNotMoving


let view (model : Model) (dispatch : Msg -> unit) = 
    let start = TimeHelpers.getTimeMs()
    model.Symbols
    |> MapsIntoLists
    |> List.map (fun ({Id = ComponentId id} as symbol) ->
        renderSymbol
            {
                Symbol = symbol
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList
    |> TimeHelpers.instrumentInterval "SymbolView" start

//------------------------GET BOUNDING BOXES FUNCS--------------------------------used by sheet.
/// Returns the bounding box of a symbol. It is defined by the height and the width as well as the x,y position of the symbol. TODO: handle rotation -> should be good
let getSymbolBoundingBox (sym:Symbol): BoundingBox =
    let h,w = //might need to redo, bounding box should be top left, and maybe we should just return h and w as they are
        match sym.STransform.Rotation with
        | Degree0 | Degree180 -> sym.Component.H, sym.Component.W
        | _ -> sym.Component.W, sym.Component.H

    {TopLeft = sym.Pos; H = float(h) ; W = float(w)}

/// Returns all the bounding boxes of all components in the model
let getBoundingBoxes (symModel: Model): Map<ComponentId, BoundingBox> =
    Map.map (fun sId (sym:Symbol) -> (getSymbolBoundingBox sym)) symModel.Symbols

/// Returns bounding box of a component based on component id
let getBoundingBox (symModel: Model) (compid: ComponentId ): BoundingBox = 
    let symb = Map.find compid symModel.Symbols
    getSymbolBoundingBox symb


//--------------------- GETTING PORTS AND THEIR LOCATIONS INTERFACE FUNCTIONS-------------------------------
// Helpers
/// Returns the center coordinates of a Symbol
let getSymbolPos (symbolModel: Model) compId = //makes sense or should we have getSymbol?
    let symbol = Map.find compId symbolModel.Symbols
    symbol.Pos

///Returns the port object associated with a given portId
let getPort (symModel: Model) (portId: string) =
    symModel.Ports[portId]

/// Interface function to get componentIds of the copied symbols
let getCopiedSymbols (symModel: Model) : (ComponentId list) =
    symModel.CopiedSymbols
    |> Map.toList
    |> List.map fst

/// returns the string of a PortId
let getPortIdStr (portId: PortId) = 
    match portId with
    | InputId (InputPortId id) -> id
    | OutputId (OutputPortId id) -> id

/// returns what side of the symbol the port is on
let getPortOrientation (model: Model)  (portId: PortId) : Edge =
    let portIdStr = getPortIdStr portId
    let port = model.Ports[portIdStr]
    let sId = ComponentId port.HostId
    model.Symbols[sId].PortOrientation[portIdStr]

let getInputPortOrientation (model: Model) (portId: InputPortId): Edge =
    getPortOrientation model (InputId portId)

let getOutputPortOrientation (model: Model) (portId: OutputPortId): Edge =
    getPortOrientation model (OutputId portId)

/// Returns the height and width of a symbol
let getHAndW sym =
    match sym.STransform.Rotation with
    | Degree0 | Degree180 -> sym.Component.H, sym.Component.W
    | _ -> sym.Component.W, sym.Component.H

//Given a symbol and a Port, it returns the orientation of the port
let getSymbolPortOrientation (sym: Symbol) (port: Port): Edge =
    let portId = port.Id
    sym.PortOrientation[portId]

//Returns the x offset of a side relative to the symbol orientation
let getPortBaseOffset (sym: Symbol) (side: Edge): XYPos=
    let h,w = getHAndW sym
    match side with 
    | Right -> {X = 0.0; Y = w}
    | Left -> {X = 0.0; Y = 0.0}
    | Top -> {X = 0.0; Y = 0.0}
    | Bottom -> {X = h; Y = 0.0}

/// Returns true if an edge has the select port of a mux
let isMuxSel (sym:Symbol) (side:Edge): bool =
    match (sym.STransform.Rotation, side) with
    | (Degree0, Bottom )-> true
    | (Degree90, Right) -> true
    | (Degree180, Top) -> true
    | (Degree270, Left)-> true
    | _ -> false
///based on a symbol and an edge, if the port is a mux select, return an extra offset required for the port (because of the weird shape of the mux)
let getMuxSelOffset (sym: Symbol) (side: Edge): XYPos =
    if isMuxSel sym side then
        match side with 
            | Top -> {X = 0.0; Y = 0.25}
            | Bottom -> {X = 0.0; Y = -0.25}
            | Left -> {X = -0.25; Y = 0.0}
            | Right -> {X = 0.25; Y = 0.0}
    else
        {X=0.0; Y=0.0}

//Given a symbol and a port, it returns the offset of the port from the top left corner of the symbol
let getPortPos2 (sym: Symbol) (port: Port) : XYPos =
    //get ports on the same edge first
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortOrder[side] //list of ports on the same side as port
    let index = float( List.findIndex (fun (p:string)  -> p = port.Id) ports )
    let gap = getPortPosEdgeGap sym.Component.Type 
    let baseOffset = getPortBaseOffset sym side  //offset of the side component is on
    let baseOffset' = baseOffset + getMuxSelOffset sym side
    match side with
    | Left | Right ->
        let xOffset = (float(sym.Component.H))* (( index + gap )/( float( ports.Length ) + 2.0*gap - 1.0))
        baseOffset' + {X = xOffset; Y = 0.0 }
    | _ -> 
        let yOffset = (float(sym.Component.W))* ((index + gap)/(float (ports.Length) + 2.0*gap - 1.0))
        baseOffset' + {X = 0.0; Y = yOffset }

/// Returns the location of a given portId, with good efficiency
let getPortLocation (model: Model) (portId : string) : XYPos=
    let port = model.Ports[portId]
    let symbolId = ComponentId port.HostId
    let sym = model.Symbols[symbolId]
    getPortPos2 sym port + sym.Pos

/// Returns the location of an input port based on their portId
let getInputPortLocation (model:Model) (portId: InputPortId)  = 
    let id = getPortIdStr (InputId portId)
    getPortLocation model id

/// Returns the location of an output port based on their portId
let getOutputPortLocation (model:Model) (portId : OutputPortId) =
    let id = getPortIdStr (OutputId portId)
    getPortLocation model id

/// Returns the locations of a given input port and output port based on their portId
let getTwoPortLocations (model: Model) (inputPortId: InputPortId ) (outputPortId: OutputPortId) =
    (getInputPortLocation model inputPortId, getOutputPortLocation model outputPortId)

///Returns the input port positions of the specified symbols in model
///only called in getPortLocations, might need more refactoring
let getInputPortsLocationMap (model: Model) (symbols: Symbol list)  = 
    let getSymbolInputPortsLoc sym =
        sym.Component.InputPorts |> List.map (fun port -> (InputPortId port.Id, (getPortPos2 sym port) + (sym.Pos)))
        
    symbols
    |> List.collect getSymbolInputPortsLoc
    |> Map.ofList

/// Returns the output port positions of the specified symbols in model
/// only called in getPortLocations might need more refactoring
let getOutputPortsLocationMap (model: Model) (symbols: Symbol list)  =
    let getSymbolOutputPortsLoc sym =
        sym.Component.OutputPorts |> List.map (fun port -> (OutputPortId port.Id, (getPortPos2 sym port) + (sym.Pos)))
        
    symbols
    |> List.collect getSymbolOutputPortsLoc
    |> Map.ofList


///Returns all the port locations of the given components   
let getPortLocations (model: Model) (symbolIds: ComponentId list) = 
    let symbols = 
        model.Symbols 
        |> Map.filter (fun symbolId _  -> List.contains symbolId symbolIds)
        |> Map.toList
        |> List.map snd
        
    let getInputPortMap = getInputPortsLocationMap model symbols
    let getOutputPortMap = getOutputPortsLocationMap model symbols
       
    getInputPortMap , getOutputPortMap 
 
//--------------------- GENERATING LABEL FUNCTIONS-------------------------------

///Returns the number of the component label (i.e. the number 1 from IN1 or ADDER16.1)
let getLabelNumber (str : string) = 
    let index = Regex.Match(str, @"\d+$")
    match index with
    | null -> 0
    | _ -> int index.Value

/// generates the label number for compType (i.e. the number 1 in IN1 or ADDER16.1)
let generateLabelNumber listSymbols compType =
    let samePrefix (target: ComponentType) (symbol: Symbol) : bool =
        let compType = symbol.Component.Type
        (prefix target) = (prefix compType)

    let samePrefixLst = 
        listSymbols
        |> List.filter (samePrefix compType)

    match compType with
    | MergeWires | SplitWire _ -> ""
    | _ ->
        if List.isEmpty samePrefixLst then 1 
        else samePrefixLst
            |> List.map (fun sym -> getLabelNumber sym.Component.Label)
            |> List.max
            |> (+) 1
        |> string

///Generates the label for a component type
let generateLabel (model: Model) (compType: ComponentType) : string =
    let listSymbols = List.map snd (Map.toList model.Symbols) 
    let prefix = prefix compType
    match compType with
    | IOLabel -> prefix
    | _ -> prefix + (generateLabelNumber listSymbols compType)

/// Interface function to paste symbols. Is a function instead of a message because we want an output.
/// Currently drag-and-drop.
/// Pastes a list of symbols into the model and returns the new model and the id of the pasted modules.
let pasteSymbols (symModel: Model) (newBasePos: XYPos) : (Model * ComponentId list) =
    let addNewSymbol (basePos: XYPos) ((currSymbolModel, pastedIdsList) : Model * ComponentId List) (oldSymbol: Symbol): Model * ComponentId List =
        let newId = JSHelpers.uuid()
        let newPos = oldSymbol.Pos - basePos + newBasePos
        let compType = oldSymbol.Component.Type
        let newLabel = 
            compType
            |> generateLabel { symModel with Symbols = currSymbolModel.Symbols}

        let newComp = makeComp newPos compType newId newLabel
        let newSymbol =
            { oldSymbol with
                Id = ComponentId newId
                Component = newComp
                Pos = newPos
                ShowInputPorts = false
                ShowOutputPorts = false }
             
        let newSymbolMap = currSymbolModel.Symbols.Add (ComponentId newId, newSymbol)
        let newPorts = addToPortModel currSymbolModel newSymbol
        let newModel = { currSymbolModel with Symbols = newSymbolMap; Ports = newPorts }
        let newPastedIdsList = pastedIdsList @ [ newSymbol.Id ]
        newModel, newPastedIdsList
        
    let oldSymbolsList =
        symModel.CopiedSymbols
        |> Map.toList
        |> List.map snd

    match oldSymbolsList with
    | [] -> symModel, []
    | _ -> 
        let baseSymbol = List.minBy (fun sym -> sym.Pos.X) oldSymbolsList
        let basePos = baseSymbol.Pos + { X = (float baseSymbol.Component.W) / 2.0; Y = (float baseSymbol.Component.H) / 2.0 }
        ((symModel, []), oldSymbolsList) ||> List.fold (addNewSymbol basePos)
 
/// Returns the hostId of the port in model
let getPortHostId (model: Model) portId =
   model.Ports[portId].HostId

/// Tries to find the target in copiedIds, and tries to return the item at the same index in pastedIds
let tryGetPastedEl copiedIds pastedIds target =
    // try to look for a symbol in copiedIds, get the index and return pastedIds[index]
    let indexedTarget = 
        copiedIds
        |> List.indexed
        |> List.filter (fun (_, id) -> id = target)
        |> List.tryExactlyOne
    match indexedTarget with
    | Some (index, _) -> List.tryItem index pastedIds
    | _ -> None

/// Returns a tuple of the list of input ports of a given input symbol, and list of output ports of a given output symbol
let getPortIds (input: Symbol) (output: Symbol) : (string list * string list)=
    let inPortIds = 
        input.Component.InputPorts
        |> List.map (fun port -> port.Id)
    let outPortIds =
        output.Component.OutputPorts
        |> List.map (fun port -> port.Id)
    inPortIds, outPortIds

/// Given a tuple of options, returns an Some (v1, v2) if both tuple elements are some, else None
let mergeOptions =
    function
    | Some v1, Some v2 -> Some (v1, v2)
    | _ -> None

/// Returns the symbol containing the given portId in the model's CopiedSymbols map
let getCopiedSymbol model portId =
    let symbolId = getPortHostId model portId
    model.CopiedSymbols[ComponentId symbolId]

/// Given two componentId list of same length and input / output ports that are in list 1, return the equivalent ports in list 2.
/// ComponentIds at same index in both list 1 and list 2 need to be of the same ComponentType.
/// CompIds1 need to be in model.CopiedSymbols.
/// Assumes ports are in the same order in equivalent symbols
let getEquivalentCopiedPorts (model: Model) copiedIds (pastedIds: string list) (InputPortId copiedInputPort, OutputPortId copiedOutputPort) =
    let getPastedSymbol copiedPort =
        getPortHostId model copiedPort
        |> tryGetPastedEl copiedIds pastedIds
        |> Option.map (fun id -> model.Symbols[ComponentId id])

    let copiedInSymbol = getCopiedSymbol model copiedInputPort
    let copiedOutSymbol = getCopiedSymbol model copiedOutputPort
    let copiedInPortIds, copiedOutPortIds = getPortIds copiedInSymbol copiedOutSymbol

    let pastedInSymbol = getPastedSymbol copiedInputPort
    let pastedOutSymbol = getPastedSymbol copiedOutputPort
    match pastedInSymbol, pastedOutSymbol with
    | Some inSymbol, Some outSymbol ->
        let pastedInPortIds, pastedOutPortIds = getPortIds inSymbol outSymbol
        let equivInPorts = tryGetPastedEl copiedInPortIds pastedInPortIds copiedInputPort
        let equivOutPorts = tryGetPastedEl copiedOutPortIds pastedOutPortIds copiedOutputPort 
        mergeOptions (equivInPorts, equivOutPorts)
    | _ -> None

/// Creates and adds a symbol into model, returns the updated model and the component id
let addSymbol (model: Model) pos compType lbl =
    let newSym = createNewSymbol pos compType lbl
    let newPorts = addToPortModel model newSym
    let newSymModel = Map.add newSym.Id newSym model.Symbols
    { model with Symbols = newSymModel; Ports = newPorts }, newSym.Id

/// Helper function to change the number of bits expected in a port of each component type, could return the model instead no?
let changeNumberOfBitsf (symModel:Model) (compId:ComponentId) (newBits : int) =
    let symbol = Map.find compId symModel.Symbols

    let newcompotype = 
        match symbol.Component.Type with
        | Input _ -> Input newBits
        | Output _ -> Output newBits
        | Viewer _ -> Viewer newBits
        | NbitsAdder _ -> NbitsAdder newBits
        | NbitsXor _ -> NbitsXor newBits
        | Register _ -> Register newBits
        | RegisterE _ -> RegisterE newBits
        | SplitWire _ -> SplitWire newBits
        | BusSelection (_,b) -> BusSelection (newBits,b)
        | BusCompare (_,b) -> BusCompare (newBits,b)
        | Constant1 (_,b,txt) -> Constant1 (newBits,b,txt)
        | c -> c

    let newcompo = {symbol.Component with Type = newcompotype}
    {symbol with Component = newcompo}

/// Helper function to change the number of bits expected in the LSB port of BusSelection and BusCompare
let changeLsbf (symModel:Model) (compId:ComponentId) (newLsb:int64) =
    let symbol = Map.find compId symModel.Symbols

    let newcompotype = 
        match symbol.Component.Type with
        | BusSelection (w, _) -> BusSelection (w, int32(newLsb))
        | BusCompare (w, _) -> BusCompare (w, uint32(newLsb)) 
        | Constant1(w, _,txt) -> Constant1 (w, newLsb,txt)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"

    let newcompo = {symbol.Component with Type = newcompotype}
    {symbol with Component = newcompo}

/// Updates the value of a constant1 component and returns the updated symbol
let changeConstantf (symModel:Model) (compId:ComponentId) (constantVal:int64) (constantText: string) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Component.Type with
        | Constant1 (w, _, _) -> Constant1 (w, constantVal,constantText)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    let newcompo = {symbol.Component with Type = newcompotype}
    printfn "Changing symbol to: %A" newcompotype
    {symbol with Component = newcompo}

//Helper functions for the upadte function

/// initialises the port positions of a component that are needed in Symbol
let initPortOrientation (comp: Component) =
    
    let movePortToBottom (res: Map<Edge, string list>*Map<string, Edge>) index =
        let leftPorts = (fst res)[Left]
        let portId = leftPorts |> List.item index //get id of sel

        let newBottomPorts = [portId]
        let newLeftPorts = (fst res)[Left] |> List.removeAt index
        let newPortOrder =
            fst res
            |> Map.add Bottom newBottomPorts
            |> Map.add Left newLeftPorts
        let newPortOrientation =
            snd res |> Map.add portId Bottom
        newPortOrder, newPortOrientation

    let addPortToMaps (edge: Edge) (portOrder, portOrientation) (port: Port) =
        let portOrder' = portOrder |> Map.add edge (portOrder[edge] @ [port.Id])
        portOrder', (portOrientation |> Map.add port.Id edge)

    let inputMaps =
        ((Map.empty, Map.empty), comp.InputPorts)
        ||> List.fold (addPortToMaps Left)

    let res = 
        (inputMaps, comp.OutputPorts)
        ||> List.fold (addPortToMaps Right)
    match comp.Type with //need to put some ports to different edges
    | Mux2 -> //need to remove select port from left and move to right
        movePortToBottom res 2
    | NbitsAdder _ -> 
        movePortToBottom res 0
    | DFFE ->
        movePortToBottom res 1
    | RegisterE _ ->
        movePortToBottom res 1
    | _ -> res

/// Given a model and a list of component ids deletes the specified components from the model and returns the updated model
let inline deleteSymbols (model: Model) compIds =
    let newSymbols = 
        (model.Symbols, compIds)
        ||> List.fold (fun prevModel sId -> Map.remove sId prevModel) 
    { model with Symbols = newSymbols }

/// Given a model and a list of component ids copies the specified components and returns the updated model
let inline copySymbols (model: Model) compIds =
    let copiedSymbols = 
        model.Symbols
        |> Map.filter (fun compId _ -> List.contains compId compIds) 

    { model with CopiedSymbols = copiedSymbols }

/// Given a model it shows all input ports and hides all output ports, then returns the updated model
let inline showAllInputPorts (model: Model) =
    let showSymbolInPorts _ sym = 
        {sym with ShowInputPorts = true; ShowOutputPorts = false}

    let newSymbols = 
        model.Symbols
        |> Map.map showSymbolInPorts

    { model with Symbols = newSymbols }

/// Given a model it shows all output ports and hides all input ports, then returns the updated model |  MAKE INLINE
let inline showAllOutputPorts (model: Model) =
    let showSymbolOutPorts _ sym = 
        {sym with ShowInputPorts = false; ShowOutputPorts = true}

    let newSymbols = 
        model.Symbols
        |> Map.map showSymbolOutPorts

    { model with Symbols = newSymbols }

/// Given a model it hides all ports and returns the updated model
let inline deleteAllPorts (model: Model) =
    let hideSymbolPorts _ sym = 
        {sym with ShowInputPorts = false; ShowOutputPorts = false}

    let updatedSymbols = 
        model.Symbols
        |> Map.map hideSymbolPorts

    { model with Symbols = updatedSymbols}

let inline showPorts (model: Model) compList =
    let hideSymbolPorts _ sym =
        {sym with ShowInputPorts = false; ShowOutputPorts = false}

    let showSymbolPorts sym =
        {sym with ShowInputPorts = true; ShowOutputPorts = true}

    let resetSymbols = 
        model.Symbols
        |> Map.map hideSymbolPorts

    let addUpdatedSymbol prevSymbols sId =
        prevSymbols |>
        Map.add sId (showSymbolPorts resetSymbols[sId])

    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold addUpdatedSymbol

    { model with Symbols = newSymbols }

/// Given a model, a component id list and an offset, moves the components by offset and returns the updated model
let inline moveSymbols (model:Model) (compList: ComponentId list) (offset: XYPos)=
    let resetSymbols = 
        model.Symbols
        |> Map.map (fun _ sym -> { sym with Moving = false}) 

    let moveSymbol prevSymbols sId =
        let newX = model.Symbols[sId].Pos.X + offset.X;
        let newY = model.Symbols[sId].Pos.Y + offset.Y;
        let newComp = 
            { model.Symbols[sId].Component with 
                X = int newX;
                Y = int newY }

        prevSymbols
        |> Map.add sId 
            { model.Symbols[sId] with 
                Moving = true; 
                Pos = { X = newX; Y = newY };
                Component = newComp } 

    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold moveSymbol

    { model with Symbols = newSymbols }

///given a model and a component id list, sets the color of the sepcified symbols to red and every other symbol's color to light gray
let inline symbolsHaveError model compList =
    let resetSymbols = 
        model.Symbols
        |> Map.map (fun _ sym -> {sym with Colour = "Lightgray"}) 

    let setSymColorToRed prevSymbols sId =
        Map.add sId {resetSymbols[sId] with Colour = "Red"} prevSymbols

    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold setSymColorToRed 
    { model with Symbols = newSymbols }

/// Given a model and a component id list, it updates the specified symbols' colour to light green, and every other symbols' colour to lightgray with max opacity.
let inline selectSymbols model compList =
    let resetSymbols = 
        model.Symbols
        |> Map.map (fun _ sym -> 
            { sym with Colour = "Lightgray"; Opacity = 1.0 }) 

    let updateSymbolColour prevSymbols sId =
        Map.add sId {resetSymbols[sId] with Colour = "lightgreen"} prevSymbols
    
    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold updateSymbolColour 

    { model with Symbols = newSymbols }

/// Given a model, an error component list, a selected component id list, it updates the selected symbols' color to green if they are not selected, and changes the symbols with errors to red. It returns the updated model.
let inline errorSymbols model (errorCompList,selectCompList,isDragAndDrop) =
    let resetSymbols = 
        model.Symbols
        |> Map.map 
            (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 })
            
    let updateSymbolStyle prevSymbols sId =
        if not isDragAndDrop then 
            Map.add sId {resetSymbols[sId] with Colour = "lightgreen"} prevSymbols
        else 
            Map.add sId { resetSymbols[sId] with Opacity = 0.2 } prevSymbols

    let selectSymbols =
        (resetSymbols, selectCompList)
        ||> List.fold updateSymbolStyle 

    let setSymColourToRed prevSymbols sId =
        Map.add sId {resetSymbols[sId] with Colour = "Red"} prevSymbols

    let newSymbols = 
        (selectSymbols, errorCompList)
        ||> List.fold setSymColourToRed
        
    { model with Symbols = newSymbols }

/// Given a model, a symbol id and a new label changes the label of the symbol to the new label and returns the updated model.
let inline changeLabel (model: Model) sId newLabel=
    let oldSym = model.Symbols[sId]
    let newComp = {oldSym.Component with Label = newLabel}
    let newSym = {oldSym with Component = newComp}
    { model with Symbols = Map.add sId newSym model.Symbols }

/// Given a model, a component id list and a color, updates the color of the specified symbols and returns the updates model.
let inline colorSymbols (model: Model) compList colour =
    let changeSymColour (prevSymbols: Map<ComponentId, Symbol>) (sId: ComponentId) =
        let newSymbol = {prevSymbols[sId] with Colour = string colour}
        prevSymbols |> Map.add sId newSymbol

    let newSymbols =
        (model.Symbols, compList)
        ||> List.fold changeSymColour

    { model with Symbols = newSymbols }

/// Given a map of current symbols and a component, initialises a symbol containing the component and returns the updated symbol map containing the new symbol
let inline createSymbol prevSymbols comp =
        let (portOrder, portOrientation) = initPortOrientation comp
        let xyPos = {X = float comp.X; Y = float comp.Y}
        let (h,w) =
            if comp.H = -1 && comp.W = -1 then
                let comp' = makeComp xyPos comp.Type comp.Id comp.Label
                comp'.H,comp'.W
            else
                comp.H, comp.W
        prevSymbols
        |> Map.add (ComponentId comp.Id)
            { Pos = xyPos
              ShowInputPorts = false //do not show input ports initially
              ShowOutputPorts = false //do not show output ports initially
              Colour = "lightgrey"     // initial color 
              Id = ComponentId comp.Id
              Component = {comp with H=h ; W = w}
              Opacity = 1.0
              Moving = false
              InWidth0 = None
              InWidth1 = None
              STransform = {Rotation= Degree0; flipped= false}
              PortOrientation = portOrientation
              PortOrder = portOrder
            }

/// Given a model and a list of components, it creates and adds the symbols containing the specified components and returns the updated model.
let inline loadComponents model comps=
    let symbolMap =
        (model.Symbols, comps) ||> List.fold createSymbol
    
    let addPortsToModel currModel _ sym =
        { currModel with Ports = addToPortModel currModel sym }
        
    let newModel = ( model, symbolMap ) ||> Map.fold addPortsToModel

    { newModel with Symbols = symbolMap }

/// Given a model, a component id, an address and a value it updates the data in the component and returns the new model.
let inline writeMemoryLine model (compId, addr, value) =
    let symbol = model.Symbols[compId]
    let comp = symbol.Component

    let newCompType =
        match comp.Type with
        | RAM1 mem -> RAM1 { mem with Data = Map.add addr value mem.Data }
        | AsyncRAM1 mem -> AsyncRAM1 { mem with Data = Map.add addr value mem.Data }
        | ROM1 mem -> ROM1 { mem with Data = Map.add addr value mem.Data }
        | AsyncROM1 mem -> AsyncROM1 { mem with Data = Map.add addr value mem.Data }
        | _ -> comp.Type

    let newComp = { comp with Type = newCompType }
    
    let newSymbols = Map.add compId { symbol with Component = newComp } model.Symbols
    
    { model with Symbols = newSymbols }

/// Given a model, a component Id and a memory component type, updates the type of the component to the specified memory type and returns the updated model.
let inline writeMemoryType model compId memory =
    let symbol = model.Symbols[compId]
    let comp = symbol.Component 
    
    let newCompType =
        match comp.Type with
        | RAM1 _ | AsyncRAM1 _ | ROM1 _ | AsyncROM1 _ -> memory
        | _ -> 
            printfn $"Warning: improper use of WriteMemoryType on {comp} ignored"
            comp.Type
    
    let newComp = { comp with Type = newCompType }
    
    let newSymbols = Map.add compId { symbol with Component = newComp } model.Symbols
    
    { model with Symbols = newSymbols }

let rotateSideLeft (side:Edge) :Edge =
    match side with
    | Top -> Left
    | Left -> Bottom
    | Bottom -> Right
    | Right -> Top


let rotateSymbolLeft (sym: Symbol) : Symbol =
    // update comp w h
    let h,w = getHAndW sym
    let newXY = sym.Pos + { X = w/2.0 - h/2.0 ;Y = h/2.0 - w/2.0 }

    //need to update portOrientation and portOrder
    let newPortOrientation = sym.PortOrientation |> Map.map rotateSideLeft





/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | DeleteSymbols compIds ->
        (deleteSymbols model compIds), Cmd.none

    | AddSymbol (pos,compType, lbl) ->
        let (newModel, _) = addSymbol model pos compType lbl
        newModel, Cmd.none

    | CopySymbols compIds ->
        (copySymbols model compIds), Cmd.none

    | ShowAllInputPorts ->
        (showAllInputPorts model), Cmd.none

    | ShowAllOutputPorts ->
        (showAllOutputPorts model), Cmd.none

    | DeleteAllPorts ->
        (deleteAllPorts model), Cmd.none 

    | ShowPorts compList ->
        (showPorts model compList), Cmd.none

    | MoveSymbols (compList, move) -> 
        (moveSymbols model compList move), Cmd.none

    | SymbolsHaveError compList ->
        (symbolsHaveError model compList), Cmd.none

    | SelectSymbols compList ->
        (selectSymbols model compList), Cmd.none  

    | ErrorSymbols (errorCompList,selectCompList,isDragAndDrop) -> 
        (errorSymbols model (errorCompList,selectCompList,isDragAndDrop)), Cmd.none 
        
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messages

    | ChangeLabel (sId, newLabel) ->
        (changeLabel model sId newLabel), Cmd.none

    | PasteSymbols compList ->
        let newSymbols =
            (model.Symbols, compList)
            ||> List.fold (fun prevSymbols sId -> Map.add sId { model.Symbols[sId] with Opacity = 0.4 } prevSymbols) 
        { model with Symbols = newSymbols }, Cmd.none  
    
    | ColorSymbols (compList, colour) -> 
        (colorSymbols model compList colour), Cmd.none 
    
    | ChangeNumberOfBits (compId, newBits) ->
        let newsymbol = changeNumberOfBitsf model compId newBits
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none
    
    | ChangeLsb (compId, newLsb) -> 
        let newsymbol = changeLsbf model compId newLsb
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none

    | ChangeConstant (compId, newVal, newText) -> 
        let newsymbol = changeConstantf model compId newVal newText
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none
    
    | ResetModel -> 
        { model with Symbols = Map.empty; Ports = Map.empty }, Cmd.none
    
    | LoadComponents comps ->
        (loadComponents model comps), Cmd.none
 
    | WriteMemoryLine (compId, addr, value) ->
        writeMemoryLine model (compId, addr, value), Cmd.none
    | WriteMemoryType (compId, memory) ->
        (writeMemoryType model compId memory), Cmd.none
    | RotateLeft compList ->
        failwithf "not implemented yet"
    | RotateRight compList ->
        failwithf "not implemented yet"
        
// ----------------------interface to Issie----------------------------- //
let extractComponent (symModel: Model) (sId:ComponentId) : Component = 
    symModel.Symbols[sId].Component

let extractComponents (symModel: Model) : Component list =
    symModel.Symbols
    |> Map.toList
    |> List.map (fun (key, _) -> extractComponent symModel key)
