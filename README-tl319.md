# Inidividual Submission for tl319

## Admin and quick access links

*link to your teamN.md file*
[Common repo Team9 file](https://github.com/tomcl/hlp22docs/blob/main/Team9.md)

[Buswire (section 1)](src/renderer/drawblock/buswire.fs)

Section 1 on my file is lines: **UPDATE WITH FINAL LINES**

Changes to the types used in Buswire were decided as a team with the people working on the other sections.

## Code Quality

**Highlights**
* Helper function `foldOverSegs` is really useful, allowing us to implement functions which require absolute segment positions by generating them on the fly.
* Reworking how intersections between segments and bounding boxes are calculated, especially the `intersect1D` higher order function.
* Normalizing the routing problems and converting back in `makeInitialSegmentsList` to minimise the cases needed in `makeInitialSegmentsList`. Reduces the number of cases needed by a factor of 4.


## Analysis

My original function list consisted of the following functions:
segmentsToVertices
makeInitialWireVerticesList
inferDirectionFromVertices
xyVerticesToSegments
issieVerticesToSegments
extractConnection
extractConnections
onSegment
orientation
getAbsXY
segmentIntersectsSegment
makeSegPos
distanceBetweenTwoPoints
makeInitialSegmentsList
renderSegment
memoOf
singleWireView
MapToSortedList
view

**segmentsToVertices**
The original document of this function was misleading, as it stated in convert a wire to a connection when in reality it was converting a segment list into a list of vertices. It also uses List.mapi when List.map would have sufficed.

**makeInitialWireVerticesList**
The only issue with this function is it uses tuple parameters when it could easily use curried parameters, which goes against Issie guidelines.

**inferDirectionFromVertices**
This function has no xML comments whatsoever which goes against Issie guidelines. 

xyVerticesToSegments
issieVerticesToSegments

onSegment
orientation
getAbsXY
segmentIntersectsSegment

makeSegPos
distanceBetweenTwoPoints
makeInitialSegmentsList

**renderSegment**
This is likely the worst function in the section of code, it is highly unreadable consisting of around 150 lines. This function could have been simplified significantly by breaking up subsections of its functionality into external functoins. Beyond this its actual implementation is also overly complex. 

**memoOf**
This code is entirely unused, and has so should be removed

**singleWireView**
This wire is entirely undocumented, and is a very important function for the rendering so it is essential that it is easy to understand.

MapToSortedList
view
This wire is entirely undocumented, and is a very important function for the rendering so it is essential that it is easy to understand.

**other problems**
There is a fundamental underlying problem on the original codebase relating to the fact that the segment type is very poor. Segments all contain both a start and end position so duplication information, the only method to identify whether a segment is being manually routed is by negating it, causing 'abs' being used everywhere. Our team decided to make the following improvements to fix this: Segments contain a length and every segment is by definition at a right angle to the prior segment, furthermore the sign of the length indicates whether it is increasing or decreasing. Segments also contain a 'Mode' attribute indicating whether how it is routed. Finally the overall start and end positions are stored in the wire itself. 

Another fundamental problem is that the original code relied entirely on having wires only be of 7-segment length. This is highly prohibitive to new port orientations. This requirement was removed in our code to allow for every possible orientation of input and output port, thus exhibiting a significant improvement in functionality.

Finally the functionality of segmentIntersectsSegment, as well as all of its subfunctions are entirely useless and can be entirely removed.

**Analysis of how/why code works**
Final function list:
segmentsToVertices 
makeInitialWireVerticesList
xyVerticesToSegments
issieVerticesToSegments
makeInitialSegmentsList
MapToSortedList
extractConnection
extractConnections
renderRadialWire
renderModernSegment
renderJumpSegment
singleWireJumpView
singleWireModernView
singleWireRadialView
view


**Extensions**

1.  Radial display type for wires

     a. singleWireRadialView this is a new function that creates a custom path from a string created in renderRadialWire and makes an SVG element from it
     b. renderRadialWire this is a new function that generates a path command string according to the vertices of the wire
     c. view changed to support switching between wire displays according to Model.Type 
     (NOTE: the code to change the Model.Type is currently not correctly implemented from section 3, as such it will always be the Modern type, in order to test different display types line X can be changed to a different singleWireBlankView

2. Modern display type for wires
     a. singleWireModernView this is a new function that creates a react element that contains all of the segments and intersections for any given wire. 
     b. renderModern this is a new function that creates the react element of any given segment as well as circles for any intersection. The location of these circle is passed via the Segment.IntersectCoordinateList
     (NOTE : at time of writing the code to populate the IntersectCoordinateList in section3  is not fully functional, as such I have manually placed 2 intersections at fixed distances on every segment in order to test functionality. This does cause issues in the case that the segment is shorter than the fixed distance, however when IntersectCoordinateList is populated properly it would be impossible for the intersection to be a distance greater than the segment length)
     c. view changed to support switching between wire displays according to Model.Type 
     (NOTE: the code to change the Model.Type is currently not correctly implemented from section 3, as such it will always be the Modern type, in order to test different display types line X can be changed to a different singleWireBlankView)


3. Arbitrary orientations of endpoint
   a. makeInitialWireVerticesList
   b. makeInitialSegmentsList
   


I have extended the preexisting codebase to allow for a radial wire display as well as the original jump wire display. This is controlled by the type of the model, which can be selected as a drop down menu.

I have also allowed for any possible orientation of the input and output port, as well as all rotations of both. This was done in conjunction with section 2: My makeInitialWireVerticesList creates the vertices for all possible orientations, assuming that the the source port is to the right. Subsequently this is converted to segments by my function makeInitialSegmentsList. This is then used by autorouting. The autorouting normalises the input to my functions so that the source port is always to the right and then rotates what it recieves back from the function.



### Issues in Existing Code

#### Bad Function list

List any problems with the existing code **concisely**  as numbered points one function per point. State why
the problem exists. List only functions with **significant problems**. You can should refer to XML comments 
in your refactored code where this helps. You may note the worst 3 functions even if they have problems that are not
significant.

* if function is poorly documented say why this is necessary (what is not obvious from a good name + 
* parameter names + types).
* if function has bad name say why this is confusing
* if a function is poorly written indicate what rewriting would improve this (briefly). You can 
refer to your code if this helps.

#### Other problems

State **concisely** Issues with existing code, or in refactoring for new types, that do not fit into per function list. 
Again numbered points, at most 3. Choose the most inmportant if you have too much to say. You can should
refer to documentation (not XML docs) in code where this helps.

### Analysis of how/why code works

This section need not contain analysis if the code can be demonstarted working. In 
that case list as bullet points the features you will demonstarte (quickly) in the 5 min
interview.

* If code works fully and can be demonstrated in the 5 minute feedback interview no analysis is needed. 
* If code will be manually tested in interview say what the tests are that supplement your analysis
* Interview code must be the assessed branch (not something else, or using later group code)
* A good way to show code works is to explain how it differs from existing working code and how existing
functionality is preserved.

