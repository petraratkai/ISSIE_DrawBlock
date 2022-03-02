jyc119 Analysis - Symbol 1
==========
Types
----------
Symbol:
- New type Rotation tells us how many degrees a symbol has been rotated left 
- Added field STransform which has type {Rotation:Rotation and flipped:bool} to symbol where flipped tells us if a symbol is flipped horizontally
- In Symbol, added field PortOrientation which tells us which ports are on each edge and PortOrder which stores the order of ports on each edge


Analysis - Symbol 1
-------------------

Rotation (rotateSymbolRight, rotateSymbolLeft):
- Implemented two functions rotateSymbolLeft and rotateSymbolRight 
- These two functions take a symbol and a rotateby parameter that determines how many degrees the symbol will rotate by
- During rotation, we update port positions, so I created a rotateSide function that takes an edge and rotates it according to the rotateby parameter. 
- To obtain the rotated orientation of the symbol, I made a updateOrientation variable which looks at the current orientation and rotates the symbol by the rotateby parameter. 
- To output the new symbol, we need to update the Pos, STransform, Component, Portorientation and Portorder field in symbol. 
- We need to update the width and height of the symbol if we rotate the symbol by 90 or 270 degrees since they will be swapped.

getTopLeftRotation:
- To update position of rotated symbol, we use helper function getTopLeftRotation
- Centre of symbol is used to determine top left position of rotated symbol

getTopLeftFlipped:
- Helper function to determine new position of flipped symbol

getCentre:
- Helper function to determine centre of symbol given it's position

reverseList:
- Recursive function used to reverse the port list when flipped

Flip (flipSymbolHorizontal):
- flipSymbolHorizontal function takes a symbol and flips it horizontally
- Top and Bottom edges have their port order reversed while Left and Right edges have their ports swapped
- Position of symbol is updated and flipped bool is not current flipped

createNewSymbol:
- Created a function called getOrientationOrderMap which takes in the port with the control input and the input list without the controlport. This function creates the
updated orientation map by creating a map with the inputportids and Left edge and the outputports and Right edge. I concatenate these maps together and add another entry into the map
with the control port and Bottom edge. The output is a tuple with the first element being the port orientation and second element is the port order.
- For the portOrderMap, I created a map mapping Left to inputportids, Right to outputportids and Bottom to the control port id. 
- The output symbol is changed to initially have an STransform of {Rotation=Degree0; flipped=false} while the pororder and portorientation are updated.

-Removed helper functions posDiff and posOf since it is unused

title:
- Changed function name to getTitle
- Changed parameter t to title and n to widthOfBus

bustitle:
- Changed parameter name wob to widthOfBus to be more specific

prefix:
- Changed Mux2 prefixe to Mux2 instead of Mux since we added Mux4 and Mux8
- Changed Demux2 prefixe to DM2 instead of DM since we added Demux4 and Demux8

portDecName:
- Renamed function to portNames

customToLength:
- Renamed customToLength to customComponentLength

makeComp:
- Renamed makeComp to makeComponent
- Removed args variable
- Removed makeComponent function in makeComp as it is unecessary and instead made more readable variables to put into output component.

portMap:
- Made helper function portMap which makes a mapping of string id to edge
- This is used to map the input ports to the left edge and output ports to right edge

concatMap:
- Made helper function concatMap to concatenate two maps to make a single map with all the keys

getId:
- Made helper function getId of a port

compSymbol:
- removed parameter component as can be declared from symbol
- Declared new points for Mux and Demux components
- Renamed characteristics to additionalInput

inverseMap:
- Used to invert key and value for port order and orientation
