# Inidividual Submission for tl319

## Admin and quick access links

*link to your teamN.md file*
[Common repo Team9 file](https://github.com/tomcl/hlp22docs/blob/main/Team9.md)

[Buswire (section 1)](src/Renderer/DrawBlock/BusWire.fs)

Section 1 on my file is lines: **UPDATE WITH FINAL LINES**

Changes to the types used in Buswire were decided as a team with the people working on the other sections.

## Code Quality

**Highlights**
* renderRadialWire: The rendering of a single wire for display type, I believe that the creation of a single react element that renders the entire wire is a particularly elegant and efficient solution.
* The AbsSegments type is particularly useful for rendering each wire. This type is suitable for each different type of rendering (radial, modern, jump) and provides a useful conversion from the overall relative segment implementation. 

## Analysis
### Bad function list

**segmentsToVertices**
The original documentation of this function was misleading, as it stated it converted a wire to a connection when in reality it was converting a segment list into a list of vertices. It also used List.mapi when List.map would have sufficed.

**inferDirectionFromVertices**
This function had no documentation whatsoever.

**xyVerticesToSegments**
This function was made overcomplicated by the requirement for wires to have 7 segments. This meant that instead of simply the first and last segment of a wire being undraggable more considerations had to be done.

**issieVerticesToSegments**
The documentation of this function was again misleading, as it stated it converted an issie Connection to a wire, when it reality it was converting it to a segment list.

**onSegment**, **orientation**
Both functions are global when they are only used in one other function and could very easily be a subfunction as both are very small.

**segmentIntersectsSegment**
The implementation of this function is very poor. Because of using negative position values to indicate the routing method of a segment it forces the function to get the absolute value. This is obviously problematic as it limits everything to being in the top right quadrant in order to work properly. Finally it would also be simpler to find whether two segments by simply finding the theoretical intersection point (as segments can only be horizontal or vertical), and then checking whether that point lies between the coordinates.

**makeSegPos**
Similar problems to onSegment and orientation, very small function that could be defined locally without causing significant clutter in the code.

**renderSegment**
This is likely the worst function in the section of code, it is highly unreadable consisting of around 150 lines. This function could have been simplified significantly by breaking up subsections of its functionality into external functions. Beyond this its actual implementation is also overly complex. 

**singleWireView**, **view**
Both functions are nearly entirely undocumented, this is especially bad as these two functions dictate the rendering and so their ease of use and understanding are critical.

**other problems**

The segments type:
* Contains a start and end position on every single segment, duplicating a large amount of information.
* The method to determine routing of a segment is whether or not it is negative which is extremely unintuitive and leads to messier code later on. 
* This was improved by changed segments to have a relative representation, only storing a length. Every segment is by defintion and a right angle to the previous segment, and the sign of the lenght indiciates whether the wires runs in an increasing or decreasing direction which is very logical.
* Segments have a 'mode' attribute, which is either 'Manual' or 'Auto' that indicates whether or not it is being manually routed.
* Wire contains both the start and final positions which allow us to produce absolute XY positions of segments whenever they are needed.

Segment list of wires:
* Originally there were only 2 types of segment lists, 3 and 5 seg, to route between various symbols. These were also implemented with the assumption of 7 segments in any given wire. 
* This led not only to hard coding that made inclusion of new cases significantly harder, but also required extra data in the situation that a wire could be represented with fewer segments.
* This was changed such that wires can have a variety of length of segment lists. 

Unused / Made redundant functions:
* Many of the original functions were either never used, or could very easily be made redundant. 
* segmentIntersectsSegment as well as a variety of global subfunctions it uses (which have no need to be global) were made redundant due to changes in Implementation.
* MapToSortedList was never used.
* MemoOf was never used.
* distanceBetweenTwoPoints was never used.
### Analysis of how/why code works

* Will demo all orientations of end point.
* Will demo radial wire rendering
* Will demo jump wire rendering
* Will demo modern wire rendering

Final function list:
segmentsToVertices - Implicitly shown in demo - changes from previous functionality are simply refactoring so that it works using the new relative segments.

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

   c. Each singleWire_____View function was also changed to correctly render the bit width in relation to the new orientation.


   d. these functions were used in conjunction with section 2 in order to allow for an arbitrary orientaiton of starting point as well.
   