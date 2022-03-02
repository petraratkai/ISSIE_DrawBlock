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

Rotation:
- Implemented two functions rotateSymbolLeft and rotateSymbolRight 
- These two functions take a symbol and a rotateby parameter that determines how many degrees the symbol will rotate by
- During rotation, we update port positions, so I created a rotateSide function that takes an edge and rotates it according to the rotateby parameter. 
- To obtain the rotated orientation of the symbol, I made a updateOrientation variable which looks at the current orientation and rotates the symbol by the rotateby parameter. 
- To output the new symbol, we need to update the Pos, STransform, Component, Portorientation and Portorder field in symbol. 
- We need to update the width and height of the symbol if we rotate the symbol by 90 or 270 degrees since they will be swapped.

createNewSymbol:
- Created a function called getOrientationOrderMap which takes in the port with the control input and the input list without the controlport. This function creates the
updated orientation map by creating a map with the inputportids and Left edge and the outputports and Right edge. I concatenate these maps together and add another entry into the map
with the control port and Bottom edge.
- For the portOrderMap, I created a map mapping Left to inputportids, Right to outputportids and Bottom to the control port id. 
- The output symbol is changed to initially have an STransform of {Rotation=Degree0; flipped=false} while the pororder and portorientation are updated.

Refactor:

makeComp 

-Renamed makeComp to makeComponent
-Renamed args to argument to make it more specific what it represents
-Removed makeComponent function in makeComp as it is unecessary and instead made more readable variables to put into output component.