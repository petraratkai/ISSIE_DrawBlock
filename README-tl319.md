# Inidividual Submission for tl319

## Admin and quick access links

*link to your teamN.md file*
[Common repo Team9 file](https://github.com/tomcl/hlp22docs/blob/main/Team9.md)

[Buswire (section 1)](src/Renderer/DrawBlock/BusWire.fs)

Section 1 on my file is lines: **UPDATE WITH FINAL LINES**

Changes to the types used in Buswire were decided as a team with the people working on the other sections.

## Code Quality

**Highlights**
* The rendering of a single wire for display type, I believe that the creation of a single react element that renders the entire is a particularly elegant and efficienet solution.
* The AbsSegments type is particularly useful for rendering each wire. This type is suitable for each different type of rendering (radial, modern, jump) and provides a useful conversion from the overall relative segment implementation. 
* Name and division of functionality for each rendering function

## Analysis
### Bad function list
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
This code is entirely unused, and so should be removed

**singleWireView**
This function is entirely undocumented, and is a very important function for the rendering so it is essential that it is easy to understand.

MapToSortedList
**view**
This function is entirely undocumented, and is a very important function for the rendering so it is essential that it is easy to understand.

**other problems**
There is a fundamental underlying problem on the original codebase relating to the fact that the segment type is very poor. Segments all contain both a start and end position so duplication information, the only method to identify whether a segment is being manually routed is by negating it, causing 'abs' being used everywhere. Our team decided to make the following improvements to fix this: Segments contain a length and every segment is by definition at a right angle to the prior segment, furthermore the sign of the length indicates whether it is increasing or decreasing. Segments also contain a 'Mode' attribute indicating whether how it is routed. Finally the overall start and end positions are stored in the wire itself. 

Another fundamental problem is that the original code relied entirely on having wires only be of 7-segment length. This is highly prohibitive to new port orientations. This requirement was removed in our code to allow for every possible orientation of input and output port, thus exhibiting a significant improvement in functionality.

Finally the functionality of segmentIntersectsSegment, as well as all of its subfunctions are entirely useless and can be entirely removed.

### Analysis of how/why code works
### Analysis of how/why code works

* Will demo all orientations of end point.
* Will demo radial wire rendering
* Will demo jump wire rendering
* Will demo modern wire rendering

This section need not contain analysis if the code can be demonstarted working. In 
that case list as bullet points the features you will demonstarte (quickly) in the 5 min
interview.

* If code works fully and can be demonstrated in the 5 minute feedback interview no analysis is needed. 
* If code will be manually tested in interview say what the tests are that supplement your analysis
* Interview code must be the assessed branch (not something else, or using later group code)
* A good way to show code works is to explain how it differs from existing working code and how existing
functionality is preserved.

Final function list:
segmentsToVertices - Implicitly shown in demo - changes from previous functionality are simply refactoring so that it works on the relative segments.

makeInitialWireVerticesList - Explicitly shown in demo - has a manual case for all differention endpoint orientations in all quadrants relative to the startpoint. Each different orientation and position can be shown. The previous functionality is a subset of the current functionality.

xyVerticesToSegments - Implicitly shown in demo - Calculates the length for each segment by calculating the difference in both X and Y. It then creates segments using this distance. It also ensures that the first and last segment are not draggable, preserving original functionality. 

issieVerticesToSegments - Implicitly shown in demo - converts the vertices from issie (two floats) to an XYPos that can then be used with xyVerticesToSegments to create segments as before.

makeInitialSegmentsList - Implicitly shown in demo - Creates the verticeslist by using makeinitialWireVerticesList with the correct endpoint orientation, this list is then passed to xyVerticesToSegents to create all initial segment list.

extractConnection - Implicitly shown in demo - Same functionality as original

extractConnections - Implicitly shown in demo - Same functionality as original

renderRadialWire - Explicitly shown in demo - Creates an SVG command that renders an entire radial wire apart from the first move and the final line.

renderModernSegment  - Explicitly shown in demo , have to manually test however as requires section 3 that is not yet implemented - renders a segment as well as any intersections along that segment.

renderJumpSegment - Explicitly shown in demo , have to manually test however as requires section 3 that is not yet implemented - renders a segment as well as any jumps along that segment, is capable of supporting multiple jumps as in original function.

singleWireJumpView - Explicitly shown in demo , have to manually test however as requires section 3 that is not yet implemented - calls renderJumpSegment for each segment of the given wire as well as rendering the text.

singleWireModernView - Explicitly shown in demo , have to manually test however as requires section 3 that is not yet implemented - calls renderModernSegment for each segment of the given wire as well as rendering the text.

singleWireRadialView  - Explicitly shown in demo - Uses the SVG command to generate the ReactElements that render the entire radial wire

view - Implicitly shown in demo - Calls singleWire_____View on each wire in the model, according to the Model.Type

## Extensions

1.  Radial display type for wires

     a. singleWireRadialView this is a new function that creates a custom path from a string created in renderRadialWire and makes an SVG element from it

     b. renderRadialWire this is a new function that generates a path command string according to the vertices of the wire
     
     c. view changed to support switching between wire displays according to Model.Type 
     (NOTE: the code to change the Model.Type is currently not correctly implemented from section 3, as such it will always be the Jump type, in order to test different display types line X can be changed to different singleWireRadialView

2. Modern display type for wires
     a. singleWireModernView this is a new function that creates a react element that contains all of the segments and intersections for any given wire. 
     
     b. renderModern this is a new function that creates the react element of any given segment as well as circles for any intersection. The location of these circle is passed via the Segment.IntersectCoordinateList
     (NOTE : at time of writing the code to populate the IntersectCoordinateList in section3  is not fully functional, in order to test this we can manually place intersections at fixed distances on every segment. This does cause incorrect rendering in the case that the segment is shorter than the fixed distance that was manually entered, however when IntersectCoordinateList is populated properly it would be impossible for the intersection to be a distance greater than the segment length, so this problem would be avoided and the final functionality would work)
     
     c. view changed to support switching between wire displays according to Model.Type 
     (NOTE: the code to change the Model.Type is currently not correctly implemented from section 3, as such it will always be the Jump type, in order to test different display types line X can be changed to a different singleWireModernView)


3. Arbitrary orientations of endpoint
   a. makeInitialWireVerticesList the overall implementation was simplified to support any orientation of the endpoint in any location, it also simplified the logic such that it directly passes the complete and final list of vertices to ensure a clear division of functionality.

   b. makeInitialSegmentsList as a result of makeInitialWireVerticesList being simplified a significant amount of the logic for this function could be removed so that it directly does what it states, creates an initial segment list. 

   c. these functions were used in conjunction with section 2 in order to allow for an arbitrary orientaiton of starting point as well.
   