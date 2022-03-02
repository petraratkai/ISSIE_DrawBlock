# Example README for individual code submission

## Instructions

* This file should be submitted (changed) on branch `hlp22-indiv-assess-<login>` of either your own repo or your group repo
   * replace `<login>` in filename `README-<login>.md` by your login - in his example `<login> = tomcl`
   * name the branch as above, including your login. This branch is used only for your submission.
* A link to the repo and branch must be on the `indiv` sheet of Tom Clarke's Team [google spreadsheet](https://docs.google.com/spreadsheets/d/1prQ5usnpu36FgtbsMO8j6_mwbdd34haSMOQKN2OkLBA/edit?usp=sharing)
* The repo you use **must have your marker added as collaborator** (github login is on indiv assessment spreadsheet page)
* Delete these instructions from your version of this readme
* Delete my comments from your version of the readme (it is an example, not something you add lines to). 
Keep the headings and **replace the admin section links and notes with correct ones**.
* Link to the sourcefile your code is contained in (drawblock or symbol) with an absolute hyperlink 
to your repo and branch
* Specify which code section and file you are doing as in my ppt (1,2,3), (buswire,symbol)
* Add any changes to my section code allocations. This must be consistent with what has been 
recorded in your team's file in my Team contributions repo](https://github.com/tomcl/hlp22docs/blob/main/README.md)  
main branch ./TeamN.md (N = 1 - 9) file. The team contrib repo is as official record. This file will be 
used marking and should have line numbers for easy access. Expect to be marked down if your marker
cannot easily find everything via links from this README.

## Team Shared Team.md

[Team Contribution Repo](https://github.com/tomcl/hlp22docs/blob/main/README.md)

* A file in this repo file is common to the team contains who is responsible for which parts of code
* Fork and clone it
* Any team member can make agreed by team commits. First commit before Wed 23 Fen 16:00. 
* Changes in who does what are recorded by altering list of functions AND 
as extra lines in this file. See sample file in [README](https://github.com/tomcl/hlp22docs/blob/main/README.md)

## Admin and quick access links

[Common repo TeamN file](https://github.com/tomcl/hlp22docs/blob/main/Team9.md)

[Symbol (section 2)](src/renderer/drawblock/symbol.fs)

Section 2 on my file is lines : 678-1510
I am also responsible for lines 
* 215-264 (function initPortOrientation)
* 365-439 (functions getSymbolPortOrientation, getHAndW, getPortBaseOffset, isMuxSel, getMuxSelOffset, getPortPos)
Also wrote the updated types and the documentation for them (lines 20-106)






## Code Quality

*Highlights*

* greatly simplified the getPortLocation functions and made getOnePortLocation(New) much more efficient
* new function boundaries for the generateLabel call tree
* greatly simplified all the functions originating from generateLabel, especially getComplist


## Analysis

### Issues in Existing Code

#### Bad Function list

* getInputPortLocation, getOutputPortLocation 
    * highly inefficient, O(n) complexity, where n is the number of ports in the entire model
    * does not use regularity, almost all the code is shared between the two functions
* getCompList
    * very long
    * lots of repeated code
* labelGenNumber, generateLabel
    * non-descriptive, confusing names
    * no point in calling prefix in generateLabel, labelGenNumber could just take the component label in
    * labelGenNumber only does one function call, so no need to make that the top level function

* if function is poorly documented say why this is necessary (what is not obvious from a good name + 
* parameter names + types).
* if function has bad name say why this is confusing
* if a function is poorly written indicate what rewriting would improve this (briefly). You can 
refer to your code if this helps.

#### Other problems

* Lack of documentation for some of the fields in Symbol, which point does Symbol.Pos represent for example?
* Lack of XML comments / incorrect usage of XML comments
* PasteSymbols is a function, not a message, so it changes the model outside of a message which is bad, the pasteSymbols message does not do anything else but change the color of the pasted symbols

### Analysis of how/why code works

The following features can be demonstrated in the interview:

* Calculating the port positions correctly
* Copying and pasting of symbols works
* Label generation of new symbols work
* Bounding boxes work
* Saving and reloading symbols
* All the different updates work

# Extensions

I implemented the following extension, which can all be demonstrated in the interview:

1. Add rotation, adjustable edge for custom comp ports (this also changes port
orientation). Adjustable order for ports
    a. Created `Edge`, `Rotation` and `STransform` types
    b. added `PortOrientation`, `PortOrder` and `STransform` fields to Symbol
    c. PortOrientation is a map of strings to Edges, representing which port is on which side of the symbol
    d. PortOrder is a map of edges to string lists, which represents the order of ports on each edge, the order of ports is always the anticlockwise order.
    e. STransform represents the rotation of the symbol (0, 90, 180 or 270 degrees anticlockwise), and whether the component is flipped
    f. This data structure ensures that any port can be easily placed on any edge later, and easily calculate the port positions
2. Locate port position and orientation based on symbol orientation and position
    a. Rewrote the `getPortPos` function such that it considers rotation and orientation
3. UI to rotate and flip symbol
    a. Added three new edit menu options: Rotate Left, Rotate Right, Flip Horizontally with keyboard shortcuts
    b. This required a small change in renderer and Sheet
    c. New message type was added for the rotations and flipping in Sheet and in Symbol
4. Symbol bounding box works with rotation
    a. Changed the `getSymbolBoundingBox` function to allow this
    b. Added the `getHAndW` function to calculate the width and height of a component based on the rotation
5. Make Mux and other symbols better with ports on other edges
    a. Added `initPortOrientation`, which initialises the ports of the symbols based on the component type
    b. For Mux and Demux the select port is at the bottom of the symbol by default
    c. For RegisterE and DFFE, the enable port is on the bottom edge by default
    d. For the NbitAdder, the carry is at the bottom by default
6. Add 90, 180, 270 degree rotation and flipping for simple symbols
    a. added `rotateSymbolLeft`, `rotateSymbolRight` and `rotateSymbolHorizontal`
    b. helper functions: `rotateSideLeft`, `rotateSideRight`, `rotateAngleLeft`, `rotateAngleRight`, `flipSide`, `flipAngle`
    c. Extended `compSymbol` to support the rotation of the rectangular components and the Mux and Demux to be able to show all the above extensions



