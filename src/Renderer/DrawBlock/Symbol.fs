(*
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

/// default block size 
let blockSize = 30 


/// ---------- SYMBOL TYPES ---------- ///
type Rotation = | Degree0 | Degree90 | Degree180 | Degree270 
    with
    member self.rotateLeft =
        match self with
        | Degree0 -> Degree270
        | Degree90 -> Degree0
        | Degree180 -> Degree90
        | Degree270 -> Degree180

    member self.rotateRight =
        match self with
        | Degree0 -> Degree90
        | Degree90 -> Degree180
        | Degree180 -> Degree270
        | Degree270 -> Degree0



type STransform = {Rotation: Rotation; flipped: bool} 
    with
    member this.rotateLeft =
        {Rotation = this.Rotation.rotateLeft; flipped = this.flipped}
    
    member this.rotateRight =
        {Rotation = this.Rotation.rotateRight; flipped = this.flipped}

type Edge = | Top | Bottom | Left | Right 
    with
    member self.rotateLeft = 
        match self with
        | Top -> Left
        | Left -> Bottom
        | Bottom -> Right
        | Right -> Top

    member self.rotateRight = 
        match self with
        | Top -> Right
        | Left -> Top
        | Bottom -> Left
        | Right -> Bottom

    member self.flip = 
        match self with
        | Top -> Bottom
        | Left -> Right
        | Bottom -> Top
        | Right -> Left

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
    | Flip of compList: ComponentId list


///Insert titles compatible with greater than 1 buswidth
let title (t:string) (n:int) : string =  
    match n with
    | 1 -> t
    | _ when n > 1 -> $"{t}({n-1})"
    | _ -> failwith "non positive bus width"
    //    if n = 1 then t else t + "(" + string(n-1) + "..0)"

///Insert titles for bus select
/// used once 
let bustitle (wob:int) (lsb:int) : string = 
    match wob with
    | 1 -> $"{lsb}"
    | _ when wob > 1 -> $"({wob+lsb-1}..{lsb})"
    | _ -> failwith "non positive bus width in bustitle"
    //if wob <> 1 then"(" + string(wob + lsb - 1) + ".." + string(lsb) +  ")" else string(lsb)

///Decodes the component type into component labels
let getPrefix (compType:ComponentType) : string = 
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
let getComponentLabel (componentType : ComponentType) : string =
    match componentType with
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
let portNames (componentType:ComponentType) : list<string> = //(input port names, output port names)
    match componentType with
    | Decode4 -> (["Sel";"Data"] @ ["0"; "1";"2"; "3"])
    | NbitsAdder _ -> (["Cin";"A";"B"] @ ["Sum "; "Cout"])
    | Register _ -> (["D"] @ ["Q"])
    | RegisterE _ -> (["EN";"D"] @ ["Q"])
    | ROM1 _ |AsyncROM1 _ -> (["Addr"] @ ["Dout"])
    | RAM1 _ -> (["Addr"; "Din";"Wen" ] @ ["Dout"])
    | AsyncRAM1 _ -> (["Addr"; "Din";"Wen" ] @ ["Dout"])
    | DFF -> (["D"] @ ["Q"])
    | DFFE -> (["EN";"D"] @ ["Q"])
    | Mux2 -> (["SEL";"0"; "1"] @ ["OUT"])
    | Demux2 -> (["SEL";"IN"] @ ["0"; "1"])
    | NbitsXor _ -> (["P"; "Q"] @ ["Out"])
    | Custom x -> (List.map fst x.InputLabels) @ (List.map fst x.OutputLabels)
    |_ -> []
   // |Mux4 -> (["0"; "1"; "2"; "3" ;"SEL"],["OUT"])
   // |Demux4 -> (["IN"; "SEL"],["0"; "1";"2"; "3";])
   // |Demux8 -> (["IN"; "SEL"],["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7"])
   // |Mux8 -> (["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7";"SEL"],["OUT"])
   // |_ -> ([],[])
   // EXTENSION: Extra Components made that are not currently in Issie. Can be extended later by using this code as it is .

//-----------------------Skeleton Message type for symbols---------------------//

///Rounds an integer to any given number. The first parameter is the number to round to, the second parameter is the input number that will be rounded

// Makes component based on given position, type, id, and label
let makeComponent (pos: XYPos) (compType: ComponentType) (id:string) (label:string) : Component =
    /// makes a list of ports for component
    let makePorts (numOfPorts:int) (hostId:string) (portType:PortType) : list<Port> =
        if numOfPorts < 1 
        then []
        else
            [0..(numOfPorts-1)]
            |> List.collect (fun x ->
                [{
                    Id = JSHelpers.uuid ()
                    PortNumber = Some x
                    PortType = portType
                    HostId = hostId
                }])

    /// rounds to next multiple of n from x
    let roundToN (n : int) (x : int) : int =
        x + abs((x % n) - n)

    ///finds length of longest string in list returns it
    let customToLength (lst : list<string*int>) : int =
        let labelList = List.map (fst >> String.length) lst
        if List.isEmpty labelList then 0 //if a component has no inputs or outputs list max will fail
        else List.max labelList

    // function that helps avoid dublicate code by initialising parameters that are the same for all component types and takes as argument the others
    let makeComponent' (n, nout, h, w) label : Component=  
        {
            Id = id 
            Type = compType 
            Label = label 
            InputPorts = makePorts n id PortType.Input 
            OutputPorts  = makePorts nout id PortType.Output 
            X  = int (pos.X - float w / 2.0) 
            Y = int (pos.Y - float h / 2.0) 
            H = h 
            W = w
        }   
    // match statement for each component type. the output is a 4-tuple that is used as an input to makecomponent (see below)
    // 4-tuple of the form ( number of input ports, number of output ports, Height, Width)
    let args = 
        match compType with
        | ROM _ | RAM _ | AsyncROM _ -> 
            failwithf "What? Legacy RAM component types should never occur"
        | And | Nand | Or | Nor | Xnor | Xor ->  (2 , 1, 2*blockSize , 2*blockSize) 
        | Not -> (1, 1, 2*blockSize, 2*blockSize) 
        | ComponentType.Input (a) -> (0, 1, blockSize, 2*blockSize)                
        | ComponentType.Output (a) -> (1 , 0, blockSize, 2*blockSize) 
        | ComponentType.Viewer a -> (1 , 0, blockSize, blockSize) 
        | ComponentType.IOLabel  ->(1 , 1, blockSize, 2*blockSize) 
        | Decode4 ->(2, 4, 4*blockSize, 3*blockSize) 
        | Constant1 (a, b,_) | Constant(a, b) -> (0, 1, blockSize, 2*blockSize) 
        | MergeWires -> (2, 1, 2*blockSize, 2*blockSize) 
        | SplitWire (a) ->(1, 2, 2*blockSize, 2*blockSize) 
        | Mux2 -> (3, 1, 3*blockSize, 2*blockSize) 
        // EXTENSION:    | Mux4 -> ( 5  , 1, 5*blockSize ,  2*blockSize)   
        // EXTENSION:    | Mux8 -> ( 9  , 1, 7*blockSize ,  2*blockSize) 
        | Demux2 ->(2, 2, 3*blockSize, 2*blockSize) 
        // EXTENSION:   | Demux4 -> ( 2  , 4, 150 ,  50) 
        // EXTENSION:    | Demux8 -> ( 2  , 8, 200 ,  50) 
        | BusSelection (a, b) -> (1, 1, blockSize, 2*blockSize) 
        | BusCompare (a, b) -> (1, 1, blockSize, 2*blockSize) 
        | DFF -> ( 1, 1, 3*blockSize, 3*blockSize) 
        | DFFE -> (2, 1, 3*blockSize, 3*blockSize) 
        | Register (a) -> (1, 1, 3*blockSize, 4*blockSize)
        | RegisterE (a) -> (2, 1, 3*blockSize, 4*blockSize) 
        | AsyncROM1 (a)  -> (1, 1, 3*blockSize, 4*blockSize) 
        | ROM1 (a) -> (1, 1, 3*blockSize, 4*blockSize) 
        | RAM1 (a) | AsyncRAM1 a -> (3, 1, 3*blockSize, 4*blockSize) 
        | NbitsXor (n) -> (2, 1, 3*blockSize, 4*blockSize) 
        | NbitsAdder (n) -> (3, 2, 3*blockSize, 4*blockSize) 
        | Custom x -> 
            let h = blockSize + blockSize * (List.max [List.length x.InputLabels; List.length x.OutputLabels])
            let maxInLength, maxOutLength = customToLength x.InputLabels, customToLength x.OutputLabels
            let maxW = maxInLength + maxOutLength + label.Length
            let scaledW = roundToN blockSize (maxW * blockSize / 5) //Divide by 5 is just abitrary as otherwise the symbols would be too wide 
            let w = max scaledW (blockSize * 4) //Ensures a minimum width if the labels are very small
            ( List.length x.InputLabels, List.length x.OutputLabels, h ,  w)
                
    makeComponent' args label

/// converts PortOrientation into PortOrder keeping port in order on each edge i.e Port with lowest number will be first 0,1,2 
let findPortOrder (idToSides : Map<string,Edge>) (comp:Component) : Map<Edge,list<string>> =
    let removeEdgeFromTuple ((edge, list) : Edge * list<string*Edge>) : Edge * list<string> =
        edge,List.map (fun x -> fst x) list
    let getPortNumber (portId:string) : int =
        let port = List.tryFind (fun (x:Port) -> x.Id = portId) (comp.InputPorts @ comp.OutputPorts)
        match port with
        | Some x -> match x.PortNumber with
                    | Some x -> x
                    | None -> failwithf "port number doesnt exist"
        | None -> failwithf "port doesnt exist"
    let sortPortsByNumber (edge:Edge,idList:list<string>)  : Edge*list<string> =
        let sortedIds = List.sortBy getPortNumber idList
        edge,sortedIds

    idToSides
    |> Map.toList
    |> List.groupBy (fun x -> snd x) 
    |> List.map removeEdgeFromTuple 
    |> List.map sortPortsByNumber
    |> Map.ofList

/// assigns default portOrientation for symbols, with sel and others on bottom
let defaultPortOrientation (comp : Component) : Map<string,Edge> = 
    let assignEdge (side:Edge) (port:Port) =
        (port.Id,side)

    let defaultEdges : Map<string,Edge> = 
        List.map (assignEdge Left) comp.InputPorts @ List.map (assignEdge Right) comp.OutputPorts
        |> Map.ofList             

    // made any ports that defualt to bottom have that port as 0
    let portOnBottom =
        let inputPort0Id = 
            let port = List.tryFind (fun x -> x.PortNumber = Some 0) comp.InputPorts
            match port with
            | Some x -> x.Id
            | None -> failwithf "error"
        Map.change inputPort0Id (fun x -> Some Bottom) defaultEdges


    match comp.Type with
    | Mux2 | DFFE | Demux2 -> portOnBottom
    | NbitsAdder width | RegisterE width -> portOnBottom
    | Custom x -> defaultEdges
    | _ -> defaultEdges


/// Function to generate a new symbol
let createNewSymbol (pos: XYPos) (comptype: ComponentType) (label:string) : Symbol =
    let id = JSHelpers.uuid ()
    let comp = makeComponent pos comptype id label
    let orientation = defaultPortOrientation comp
    {
        Pos = { X = pos.X - float comp.W / 2.0; Y = pos.Y - float comp.H / 2.0 }
        ShowInputPorts = false
        ShowOutputPorts = false
        InWidth0 = None // set by BusWire
        InWidth1 = None
        Colour = "lightgrey"
        Id = ComponentId id
        Component = comp
        Opacity = 1.0
        Moving = false
        STransform = {Rotation = Degree0; flipped = false}
        PortOrientation = orientation
        PortOrder = findPortOrder orientation comp
    }

/// function that rotates a symbol, currently takes direction as an edge as they have conveniently named left and right however could be a bool 
/// or other type, returns a rotated symbol
let rotateSymbol direction (symb:Symbol) : Symbol = 
    let rotatedPortOrientation = 
        match direction with
        | Left -> Map.map (fun k (v:Edge) -> v.rotateLeft) symb.PortOrientation
        | Right -> Map.map (fun k (v:Edge) -> v.rotateRight) symb.PortOrientation
        | _ -> failwithf "can only rotate left or right"      
    let rotatedPortOrder = findPortOrder rotatedPortOrientation symb.Component
    let newSTransform =
        match direction with
        | Left -> symb.STransform.rotateLeft
        | Right -> symb.STransform.rotateRight
        | _ -> failwithf "can only implement left or right"    
    let newComponent = {symb.Component with H = symb.Component.W; W = symb.Component.H}
    {symb with STransform = newSTransform; PortOrientation = rotatedPortOrientation; PortOrder = rotatedPortOrder; Component = newComponent}

/// adds Ports to Model    
let addPortsToModel (modelPorts: Map<string, Port>) (portsToAdd: list<Port>) = 
    let addOnePort (currentPorts: Map<string, Port>) (port: Port) : Map<string,Port> =
        Map.add port.Id port currentPorts   
    
    (modelPorts, portsToAdd)
    ||> List.fold addOnePort

//-----------------------------------------GET PORT POSITION---------------------------------------------------
// Function that calculates the positions of the ports 

/// returns Port position relative to the symbol
let getPortPos (symb: Symbol) (port:Port) : XYPos =
    let portSide = symb.PortOrientation[port.Id]
    let edgePortList = symb.PortOrder[portSide]
    let numOfPortsOnSide = List.length edgePortList
    let index = List.findIndex (fun x -> x = port.Id) edgePortList
    let portSpacing =   match portSide with
                        | Top | Bottom -> {X = float(symb.Component.W)/float(1+numOfPortsOnSide); Y = 0}
                        | Left | Right -> {X = 0; Y = float(symb.Component.H/(1+numOfPortsOnSide))}                    
    let portCorner =
        match portSide with
        | Left -> {X = 0.; Y = 0.}
        | Top -> {X = symb.Component.W; Y = 0.}
        | Right -> {X = symb.Component.W; Y = symb.Component.H}
        | Bottom -> {X = 0.; Y = symb.Component.H}

    match symb.Component.Type with
    | MergeWires -> 
                    match portSide with
                    | Left -> portCorner + portSpacing * 2.0 * float(index) + {X = 0; Y = 10.}
                    | Top -> portCorner - portSpacing * float(index+1)
                    | Right -> portCorner - portSpacing * float(index+1) 
                    | Bottom -> portCorner + portSpacing * float(index+1)
    | SplitWire _ -> 
                    match portSide with
                    | Left -> portCorner + portSpacing * float(index+1)
                    | Top -> portCorner - portSpacing * float(index+1)
                    | Right -> portCorner - (portSpacing * float(index) * 2.0 + {X = 0; Y = 10.})
                    | Bottom -> portCorner + portSpacing * float(index+1)
    | Mux2 | Demux2 -> 
                    match symb.STransform.Rotation with 
                    | Degree0 | Degree180 -> 
                                            match portSide with
                                            | Left -> portCorner + portSpacing * float(index+1)
                                            | Top -> portCorner - portSpacing * float(index+1) + {X = 0; Y = 9.}
                                            | Right -> portCorner - portSpacing * float(index+1) 
                                            | Bottom -> portCorner + portSpacing * float(index+1) + {X = 0; Y = -9.}
                    | Degree90 | Degree270 ->    
                                            match portSide with
                                            | Left -> portCorner + portSpacing * float(index+1) + {X = 9.; Y = 0}
                                            | Top -> portCorner - portSpacing * float(index+1) 
                                            | Right -> portCorner - portSpacing * float(index+1) + {X = -9.; Y = 0}
                                            | Bottom -> portCorner + portSpacing * float(index+1) 

    | _ ->
        match portSide with
        | Left -> portCorner + portSpacing * float(index+1)
        | Top -> portCorner - portSpacing * float(index+1)
        | Right -> portCorner - portSpacing * float(index+1) 
        | Bottom -> portCorner + portSpacing * float(index+1)
    
/// Returns the height and width of a symbol
let getHAndW sym =
    match sym.STransform.Rotation with
    | Degree0 | Degree180 -> sym.Component.H, sym.Component.W
    | _ -> sym.Component.W, sym.Component.H

/// Finds port position using Model and Port
let getPortPosModel (model: Model) (port:Port) : XYPos =
    getPortPos (Map.find (ComponentId port.HostId) model.Symbols) port

//-----------------------------------------DRAWING HELPERS ---------------------------------------------------
// Text adding function with many parameters (such as bold, position and text)
let private addText (pos: XYPos) name alignment weight size =
    let text =
            {defaultText with TextAnchor = alignment; FontWeight = weight; FontSize = size}
    [makeText pos.X pos.Y name text]

/// to deal with additional component text such as label / symbols like &,1,=1
let private addComponentLabel height width yOffset name weight size rotation = 
    match rotation with 
    | Degree0 -> addText {X = (float width/2.); Y = yOffset} name "middle" weight size
    | Degree90 -> addText {X = float width + 5.; Y = float height/2. + yOffset} name "start" weight size
    | Degree180 -> addText {X = float width/2.; Y = float height - yOffset - 15.} name "middle" weight size
    | Degree270 -> addText {X = -5.; Y = float height/2. + yOffset} name "end" weight size
/// Generate circles on ports
let private portCircles (pos: XYPos) = 
    [makeCircle pos.X pos.Y portCircle]

/// Puts name on ports
let private portText (pos: XYPos) name edge =
    let pos' = 
            match edge with 
            | Left -> pos + {X = 5.; Y = -5.}
            | Top -> pos + {X = -5.; Y = 6.}
            | Right -> pos + {X = -4.; Y = -8.}
            | Bottom -> pos + {X = -5.; Y = -15.}

    let align = 
            match edge with
            | Right -> "end"
            | _ -> "start"
    (addText pos' name align "normal" "12px")

/// Print the name of each port 
let private drawPortsText (portList: list<Port>) (listOfNames: list<string>) (symb: Symbol) = 
    let getPortName name x = portText (getPortPos symb portList[x]) name (symb.PortOrientation[portList.[x].Id])
    if listOfNames.Length < 1
    then []
    else 
        [0..(portList.Length-1)]
        |> List.map2 getPortName listOfNames 
        |> List.collect id

/// Function to draw ports using getPortPos. The ports are equidistant     
let private drawPorts (portList: Port List) (printPorts:bool) (symb: Symbol)= 
    if not (portList.Length < 1) && printPorts 
    then [0..(portList.Length-1)] |> List.collect (fun x -> (portCircles (getPortPos symb portList[x])))
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
    let points = sprintf $"{posX},{posY},{posX+9.},{posY},{posX},{posY-8.}"
    createPolygon points colour opacity

let addClock (pos: XYPos) colour opacity =
    let points = sprintf $"{pos.X},{pos.Y-1.},{pos.X+8.},{pos.Y-7.},{pos.X},{pos.Y-13.}"
    createPolygon points colour opacity
    |> List.append (addText (pos + {X = 10.; Y = -13.} ) " clk" "start" "normal" "12px")

let addHorizontalLine posX1 posX2 posY opacity = // TODO: Line instead of polygon?
    let points = sprintf $"{posX1},{posY},{posX2},{posY}"
    createPolygon points "lightgray" opacity

let outlineColor (color:string) =
    match color.ToLower() with
    | "lightgray" | "lightgrey" -> "black"
    | c -> 
        printfn $"color={color}"
        c

let addHorizontalColorLine posX1 posX2 posY opacity (color:string) = // TODO: Line instead of polygon?
    let points = sprintf $"{posX1},{posY} {posX2},{posY}"
    let outlineColor = outlineColor color
    [makePolygon points {defaultPolygon with Fill = "olcolor"; Stroke=outlineColor; StrokeWidth = "2.0"; FillOpacity = opacity}]

/// --------------------------------------- SYMBOL DRAWING ------------------------------------------------------ ///   

/// Puts together all Symbol react elements
let drawSymbol (symbol:Symbol) (colour:string) (showInputPorts:bool) (showOutputPorts:bool) (opacity: float) : list<ReactElement> = 
    let comp = symbol.Component
    let h,w = getHAndW symbol
    let halfW = comp.W/2
    let halfH = (comp.H)/2

    let mergeSplitLine posX1 posX2 posY msb lsb =
        let text = 
            match msb = lsb, msb >= lsb with
            | _, false -> ""
            | true, _ -> sprintf $"({msb})"
            | false, _ -> sprintf $"({msb}:{lsb})"
        addHorizontalColorLine posX1 posX2 (posY*float(h)) opacity colour @
        addText {X = float((posX1 + posX2)/2); Y = (posY*float(h)-11.)} text "middle" "bold" "9px"

    let points =            // Points that specify each symbol 
        match comp.Type with
        | Input _ -> sprintf $"0,0 0,{h} {float(w*2)/3.},{h} {w},{halfH} {float(w*2)/3.},0"
        | Constant1 _ -> sprintf $"0,{comp.H} {halfW},{halfH} 0,0"
        | IOLabel -> sprintf $"{float(w)/3.},0 0,{halfH} {float(w)/3.},{h} {float(w*2)/3.},{h} {w},{halfH} {float(w*2)/3.},0"
        | Output _ -> sprintf $"{float(w)/5.},0 0,{halfH} {float(w)/5.},{h} {w},{h} {w},0"
        | Viewer _ -> sprintf $"{float(w)/5.},0 0,{halfH} {float(w)/5.},{h} {w},{h} {w},0"
        | MergeWires -> sprintf $"{halfW},{float(h)/6.} {halfW},{float(5*h)/6.}"
        | SplitWire _ -> sprintf $"{halfW},{float(h)/6.} {halfW},{float(5*h)/6.}"
        //| Demux2 -> sprintf $"0,{float(h)/5.} 0,{float(h)*0.8} {w},{h} {w},0"
        //| Mux2 -> sprintf $"0,0 {w},{float(h)/5.} {w},{float(h)*0.8} 0,{h}"
        // EXTENSION: |Mux4|Mux8 ->(sprintf "%i,%i %i,%f  %i,%f %i,%i" 0 0 w (float(h)*0.2) w (float(h)*0.8) 0 h )
        // EXTENSION: | Demux4 |Demux8 -> (sprintf "%i,%f %i,%f %i,%i %i,%i" 0 (float(h)*0.2) 0 (float(h)*0.8) w h w 0)
        | Demux2 ->
            match symbol.STransform.Rotation with
            | Degree0 ->
                sprintf  $"0,{float(h)/5.} 0,{float(h)*0.8} {w},{h} {w},0"
            | Degree90 ->       
                (sprintf "%f,%i %f,%i %i,%i %i,%i" (float(h)*0.2) 0 (float(h)*0.8) 0 h w 0 w)
            | Degree180 ->
                sprintf $"0,0 {w},{float(h)/5.} {w},{float(h)*0.8} 0,{h}"
            | Degree270 ->
                (sprintf "%i,%i %i,%i %f,%i %f,%i" 0 0 h 0 (float(h)*0.8) w (float(h)*0.2) w)
        | Mux2 ->
            match symbol.STransform.Rotation with 
            | Degree0 -> 
                sprintf  $"0,0 {w},{float(h)/5.} {w},{float(h)*0.8} 0,{h}"
            | Degree180 ->
                sprintf $"0,{float(h)/5.} 0,{float(h)*0.8} {w},{h} {w},0"
            | Degree90 ->
                (sprintf "%i,%i %i,%i  %f,%i %f,%i" 0 0 h 0 (float(h)*0.8) w (float(h)*0.2 ) w)
            | Degree270 ->
                (sprintf "%f,%i %f,%i  %i,%i %i,%i" (float(h)*0.2) 0 (float(h)*0.8) 0 h w 0 w )                

        // EXTENSION: |Mux4|Mux8 ->(sprintf "%i,%i %i,%f  %i,%f %i,%i" 0 0 w (float(h)*0.2) w (float(h)*0.8) 0 h )
        // EXTENSION: | Demux4 |Demux8 -> (sprintf "%i,%f %i,%f %i,%i %i,%i" 0 (float(h)*0.2) 0 (float(h)*0.8) w h w 0)

        | BusSelection _ |BusCompare _ -> sprintf $"0,0 0,{h} {0.6*float(w)},{h} {0.8*float(w)},{0.7*float(h)} {w},{0.7*float(h)} {w},{0.3*float(h)} {0.8*float(w)},{0.3*float(h)} {0.6*float(w)},0"
        | _ -> sprintf $"0,{comp.H} {comp.W},{comp.H} {comp.W},0 0,0"
        
    let additions =       // Helper function to add certain characteristics on specific symbols (inverter, enables, clocks)
        match comp.Type with
        | Constant1 (_,_,txt) -> (addHorizontalLine halfW w (float(halfH)) opacity @ addText {X = float(halfW-5); Y = float(h-8)} txt "middle" "normal" "12px")
        | Nand | Nor | Xnor |Not -> (addInvertor w halfH colour opacity)
        | MergeWires -> 
            let lo, hi = 
                match symbol.InWidth0, symbol.InWidth1  with 
                | Some n, Some m  -> n, m
                | _ -> -1,-1
            let msb = hi + lo - 1
            let midb = lo
            let midt = lo - 1
            mergeSplitLine 0 halfW (1./6.) midt 0 @ 
            mergeSplitLine 0 halfW (5./6.) msb midb @ 
            mergeSplitLine halfW w 0.5 msb 0
        | SplitWire mid -> 
            let msb, mid' = match symbol.InWidth0 with | Some n -> n - 1, mid | _ -> -100, -50
            let midb = mid'
            let midt = mid'-1
            mergeSplitLine halfW w (1./6.) midt 0 @ 
            mergeSplitLine halfW w (5./6.) msb midb @ 
            mergeSplitLine 0 halfW 0.5 msb 0
        | DFF |DFFE -> (addClock {X = 0.; Y = h} colour opacity)
        | Register _ |RegisterE _ -> (addClock {X=0.; Y = h} colour opacity)
        | ROM1 _ |RAM1 _ | AsyncRAM1 _ -> (addClock {X = 0.; Y = h} colour opacity)
        | BusSelection(x,y) -> (addText {X = (float(w/2)-5.); Y = ((float(h)/2.7)-2.)} (bustitle x y) "middle" "normal" "12px")
        | BusCompare (_,y) -> (addText {X = (float(w/2)-6.); Y = (float(h)/2.7-1.)} ("=" + NumberHelpers.hex(int y)) "middle" "bold" "10px")
        | Input (x) -> (addText {X = float(w/2)-5.; Y = (float(h)/2.7)-3.} (title "" x) "middle" "normal" "12px")
        | Output (x) -> (addText {X = float(w/2); Y = (float(h)/2.7)-3.} (title "" x) "middle" "normal" "12px")
        | Viewer (x) -> (addText {X = float(w/2); Y = (float(h)/2.7)-1.25} (title "" x) "middle" "normal" "9px")
        | _ -> []

    let outlineColour, strokeWidth =
        match comp.Type with
        | SplitWire _ | MergeWires -> outlineColor colour, "2.0"
        | _ -> "black", "1.0"
   
    // Put everything together 
    
    (drawPorts comp.OutputPorts showOutputPorts symbol)
    |> List.append (drawPorts comp.InputPorts showInputPorts symbol)
    |> List.append (drawPortsText (comp.InputPorts @ comp.OutputPorts) (portNames comp.Type) symbol)
    //|> List.append (drawPortsText comp.OutputPorts (snd(portNames comp)) symbol)  
    |> List.append (addComponentLabel comp.H comp.W 5. (getComponentLabel comp.Type) "bold" "14px" symbol.STransform.Rotation) 
    |> List.append (addComponentLabel comp.H comp.W -20. comp.Label  "normal" "16px" symbol.STransform.Rotation)
    |> List.append (additions)
    |> List.append (createBiColorPolygon points colour outlineColour opacity strokeWidth)

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
            g ([ Style [ Transform(sprintf $"translate({fX}px, {fY}px)") ] ]) (drawSymbol props.Symbol symbol.Colour symbol.ShowInputPorts symbol.ShowOutputPorts symbol.Opacity)
            
        , "Symbol"
        , equalsButFunctions
        )


let view (model : Model) (dispatch : Msg -> unit) =    
    /// View function for symbol layer of SVG
    let toListOfMovingAndNot map =
        let listMoving = 
            Map.filter (fun _ sym -> not sym.Moving) map
            |> Map.toList
            |> List.map snd
        let listNotMoving =
            Map.filter (fun _ sym -> sym.Moving) map
            |> Map.toList
            |> List.map snd
        listMoving @ listNotMoving

    let start = TimeHelpers.getTimeMs()
    model.Symbols
    |> toListOfMovingAndNot
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

let init () = 
    {Symbols = Map.empty; CopiedSymbols = Map.empty; Ports = Map.empty; InputPortsConnected= Set.empty; OutputPortsConnected = Map.empty}, Cmd.none

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


/// Returns the location of a given portId, with good efficiency
let getPortLocation (model: Model) (portId : string) : XYPos=
    let port = model.Ports[portId]
    let symbolId = ComponentId port.HostId
    let sym = model.Symbols[symbolId]
    getPortPos sym port + sym.Pos

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
        sym.Component.InputPorts |> List.map (fun port -> (InputPortId port.Id, (getPortPos sym port) + (sym.Pos)))
        
    symbols
    |> List.collect getSymbolInputPortsLoc
    |> Map.ofList

/// Returns the output port positions of the specified symbols in model
/// only called in getPortLocations might need more refactoring
let getOutputPortsLocationMap (model: Model) (symbols: Symbol list)  =
    let getSymbolOutputPortsLoc sym =
        sym.Component.OutputPorts |> List.map (fun port -> (OutputPortId port.Id, (getPortPos sym port) + (sym.Pos)))
        
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
        (getPrefix target) = (getPrefix compType)

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
    let prefix = getPrefix compType
    match compType with
    | IOLabel -> prefix
    | _ -> prefix + (generateLabelNumber listSymbols compType)

let initCopiedPorts (oldSymbol:Symbol) (newComp: Component) =
    let inPortIds = List.map (fun (p:Port) -> p.Id)  newComp.InputPorts
    let outPortIds = List.map (fun (p:Port) -> p.Id) newComp.OutputPorts
    let oldInPortIds =  
        List.map (fun (p:Port) -> p.Id) oldSymbol.Component.InputPorts
    let oldOutPortIds =
        List.map (fun (p:Port) -> p.Id) oldSymbol.Component.OutputPorts
    let equivPortIds = 
        List.zip oldInPortIds inPortIds @ List.zip oldOutPortIds outPortIds
        |> Map.ofList
    let portOrientation = 
        (Map.empty,oldSymbol.PortOrientation)
        ||> Map.fold 
            (fun currMap oldPortId edge -> Map.add equivPortIds[oldPortId] edge currMap)

    let emptyPortOrder = 
        (Map.empty, [Top; Bottom; Left; Right])
        ||> List.fold (fun currMap side -> Map.add side [] currMap)
    let portOrder =
        (emptyPortOrder, oldSymbol.PortOrder)
        ||> Map.fold 
            (fun currMap side oldList -> 
                let newList =
                    ([], oldList)
                    ||> List.fold 
                        (fun currList oldPortId ->
                            currList @ [equivPortIds[oldPortId]])
                Map.add side newList currMap)
    portOrientation, portOrder

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
        let newComp = makeComponent newPos compType newId newLabel
        let orientation = defaultPortOrientation newComp
        let newSymbol =
            { oldSymbol with
                Id = ComponentId newId
                Component = newComp
                Pos = newPos
                ShowInputPorts = false
                ShowOutputPorts = false
                PortOrientation = orientation 
                PortOrder = findPortOrder orientation newComp
            }
             
        let newSymbolMap = currSymbolModel.Symbols.Add (ComponentId newId, newSymbol)
        let newPorts = addPortsToModel currSymbolModel.Ports (newComp.InputPorts @ newComp.OutputPorts)
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
let getEquivalentCopiedPorts (model: Model) (copiedIds: ComponentId list) (pastedIds: ComponentId list) (InputPortId copiedInputPort, OutputPortId copiedOutputPort) =
    let getPastedSymbol copiedPort =
        ComponentId (getPortHostId model copiedPort)
        |> tryGetPastedEl copiedIds pastedIds
        |> Option.map (fun id -> model.Symbols[id])

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
    let newPorts = addPortsToModel model.Ports (newSym.Component.InputPorts @ newSym.Component.OutputPorts)
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
        //let (portOrder, portOrientation) = initPortOrientation comp
        let xyPos = {X = float comp.X; Y = float comp.Y}
        let orientation = defaultPortOrientation comp
        let (h,w) =
            if comp.H = -1 && comp.W = -1 then
                let comp' = makeComponent xyPos comp.Type comp.Id comp.Label
                comp'.H,comp'.W
            else
                comp.H, comp.W
        prevSymbols
        |> Map.add (ComponentId comp.Id)
            {
                Pos = xyPos
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
                PortOrientation = orientation
                PortOrder = findPortOrder orientation comp                        
                //PortOrientation = portOrientation
                //PortOrder = portOrder
            }

/// Given a model and a list of components, it creates and adds the symbols containing the specified components and returns the updated model.
let inline loadComponents model comps=
    let symbolMap =
        (model.Symbols, comps) ||> List.fold createSymbol
    
    let addPortsToModel currModel _ sym =
        { currModel with Ports = addPortsToModel currModel.Ports (sym.Component.InputPorts @ sym.Component.OutputPorts)}
        
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


// let flipAngleHorizontal (rotation: Rotation): Rotation =
//     match rotation with
//     | Degree90 | Degree270 -> 
//         rotation
//         |> rotateAngleRight
//         |> rotateAngleRight
//     | _ -> rotation


// let flipSideHorizontal (edge: Edge) : Edge =
//     match edge with
//     | Left | Right ->
//         edge
//         |> rotateSideRight
//         |> rotateSideRight
//     | _ -> edge

// let flipSymbolHorizontal (sym:Symbol) : Symbol =
//     match sym.Component.Type with
//     | Custom _ -> sym
//     | _ ->
//         let newPortOrientation = 
//             sym.PortOrientation |> Map.map (fun id side -> flipSideHorizontal side)

//         let flipPortList currPortOrder side =
//             currPortOrder |> Map.add (flipSideHorizontal side ) sym.PortOrder[side]

//         let newPortOrder = 
//             (Map.empty, [Top; Left; Bottom; Right]) ||> List.fold flipPortList
//             |> Map.map (fun edge order -> List.rev order)

//         let newSTransform = 
//             {flipped= not sym.STransform.flipped;
//             Rotation= flipAngleHorizontal sym.STransform.Rotation}

//         { sym with
//             PortOrientation = newPortOrientation
//             PortOrder = newPortOrder
//             STransform = newSTransform
//         }
        

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
        let rotatedSymbols = 
            compList |> List.map (fun id-> rotateSymbol Left model.Symbols[id])
        let newSymbolMap = 
            (model.Symbols, rotatedSymbols) 
            ||> List.fold (fun currSymMap sym -> currSymMap |> Map.add sym.Id sym)
        { model with Symbols = newSymbolMap }, Cmd.none
    | RotateRight compList ->
        let rotatedSymbols = 
            compList |> List.map (fun id-> rotateSymbol Right model.Symbols[id])
        let newSymbolMap = 
            (model.Symbols, rotatedSymbols) 
            ||> List.fold (fun currSymMap sym -> currSymMap |> Map.add sym.Id sym)
        { model with Symbols = newSymbolMap }, Cmd.none

    | Flip compList ->
        let flippedSymbols = 
            compList |> List.map (fun id-> rotateSymbol Left model.Symbols[id])
        let newSymbolMap = 
            (model.Symbols, flippedSymbols) 
            ||> List.fold (fun currSymMap sym -> currSymMap |> Map.add sym.Id sym)
        { model with Symbols = newSymbolMap }, Cmd.none
        
// ----------------------interface to Issie----------------------------- //
let extractComponent (symModel: Model) (sId:ComponentId) : Component = 
    symModel.Symbols[sId].Component

let extractComponents (symModel: Model) : Component list =
    symModel.Symbols
    |> Map.toList
    |> List.map (fun (key, _) -> extractComponent symModel key)