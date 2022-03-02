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

*link to your teamN.md file*
[Common repo TeamN file](https://github.com/tomcl/hlp22docs/blob/main/Team9.md)

[Symbol (section 1)](src/renderer/drawblock/Symbol.fs)

Section 1 on my file is lines : 85-339 (creating new symbol)
I am also responsible for lines 493-1004 (rotation and flipping)

I am not responsible for port positions and only worked on creating symbols with ports on other edges, rotation and flipping.

## Analysis

### Issues in Existing Code

#### Bad Function list

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

### Analysis of how/why code works

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

getCentre:
- Helper function to determine centre of symbol given it's position

# Extensions

Flip (flipSymbolHorizontal):
- flipSymbolHorizontal function takes a symbol and flips it horizontally
- Top and Bottom edges have their port order reversed while Left and Right edges have their ports swapped
- Position of symbol is updated and flipped bool is not current flipped

getTopLeftFlipped:
- Helper function to determine new position of flipped symbol

reverseList:
- Recursive function used to reverse the port list when flipped
