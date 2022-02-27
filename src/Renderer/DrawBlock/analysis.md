Analysis - Symbol
==========
Types
----------
Symbol -> struct that contains a Component, if component has ports maybe move out ports directly into symbol, not super neccessary

Model:
- contains Symbols (map of components) and copiedSymbols
- map of string to ports to represent Ports - why string? maybe id?
- input/outputportsconnected -> set of ports connected
	why is this in Model and not in Symbol?
- Msg - DU of different events
	many tuples - should we make them records?
	refactor all the messages, use (anonymous) records!
- posOf should be called toXYPos
- title and bustitle -> wire(3..0) format printing, better var names maybe
	wob: width of bus
- prefix: fine but custom is confusing
- gateDecoderType: rename? gateLabel?
- portDecName: return tuple of input and output port labels -> probably fine as tuples
- roundToN?????

need to add:
- rotation
- adjustable edge for custom components
- drag port to reorder
- if we do this need to add arrows on wires/ports
- clock input signal!
- STransform: field in Symbol
	type to say how symbol is
	rotated/flipped
- PortOrientation: type which
	says which side of symbol
	port is on
- Store port positions in Symbol as one of: • 1. Transformed offsets and orientations of
ports relative to symbol • APortOffsetsMap: Map to
determine offset and orientation of
port relative to symbol
• genAPortOffsets: Helper function
to determine APortOffsetsMap
• 



: helper to generate port
position and orientation from
symbol and port
• 2. Transform
-invariant offsets of ports
relative from symbol • TIPortPosMap: Map to determine
transform
-invariant offset and
orientation of port relative to
symbol
• genTIPortOffsets: Helper to
determine RIPortOffsetMap from
symbol
• getPortOffset: Helper to generate
the absolute offset and orientation
of a port from the Symbol
idea: store orientation in ports : T,B,L,R, have a map of ports to orientations instead!
	?: but then why do we need a portOffsetMap
STransform: struct containing a DU - rotation degree (0|90|180|240) anticlockwise
	would that be part of symbol or component? presumably symbol





Symbols2
---------
- getOneBoundingBox rename to getBoundingBox
- getInputPortsPositionMap -> redo so that it only returns one port position to make it more efficient
	check if it is only used for this in one function
	same for getOutputPortsPositionMap
- getInputPortLocation
- getOnePortLocationNew:
	no need for Option.map just apply function on res 
- getTwoPortLocations: i don't think it's needed
- filterString: only filters out terminal numbers not all non-letters, that is probably fine though
- regex: rename
- getCompList: very long, hard to read, how to refactor?
	afaik: only used to determine the index of a new component. In this case we couls store the number of components for each compType
	so maybe could get rid of getComplist and just read from the map -> would greatly simplify code
	memory vs speed??
	what if they delete a component? our counter would always just keep increasing
	okay: not exactly a counter, just store the bigges label, when we are deleting if the number if smaller then biggest index stored, if it is the same, then ahhh idk
- getIndex gets a new index for a new component presumably
- labelGenNumber: move into genLabel
- uuid: universally unique identifier
- in PasteSymbol need to increase the counter

Component has top left X,Y, symbol has center

Extension idea:
when component selected it can be moved with the arrow keys