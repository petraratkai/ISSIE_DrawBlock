# Inidividual Submission for sts219

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

## Admin and quick access links

*link to your teamN.md file*
[Common repo Team9 file](https://github.com/tomcl/hlp22docs/blob/main/Team9.md)

[Buswire (section 2)](src/renderer/drawblock/buswire.fs)

Section 2 on my file is lines: **UPDATE WITH FINAL LINES**
I am also responsible for: 
- lines 104-226 (foldOverSegs, Debugging functions)
- Changing the BoundingBox type (and updating parts of Sheet.fs to update references to it)

Changes to the types used in Buswire were decided as a team with the people working on the other sections.

## Code Quality

**Highlights**
* Helper function `foldOverSegs` is really useful, allowing us to implement functions which require absolute segment positions by generating them on the fly.
* Reworking how intersections between segments and bounding boxes are calculated, especially the `intersect1D` higher order function.
* Normalizing the routing problems and converting back in `makeInitialSegmentsList` to minimise the cases needed in `makeInitialSegmentsList`. Reduces the number of cases needed by a factor of 4.


## Analysis

To first understand the existing code, I created call graphs of the functions in my section of the code. Their interdependencies with other functions are defined by the following key:
![]( sts219/key.png)

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

**getClickedSegment**
This function was an overcomplicated and confusing mess:
![]( sts219/getClickedSegment.png)
This function has a simple purpose, which is to find the closest segment to a mouse-click, but has decided to do this in 2 different ways:
- `getIntersectingSegments`: returns a list of segments which intersect the bounding box created by a mouseclick
- `getClosestSegment`: returns the closest segment
If the list of segments returned by `getIntersectingSegments` is empty, the segment returned by defined by `getClosestSegment`, otherwise the first segment in the list is returned. This is problematic as:
- The first segment in this list has no guarantee on being the closest segment (although the difference is probably inperceptible by a user)
- Since `getClosestSegment` is returned in the case where no segments intersect the bouding box, there is no reason not to use this function exclusively (since it is more accurate than the other sub-function, and isn't more computationally expensive)

These issues have been addressed in my refactored code by completely removing `getIntersectingSegments`, and instead relying on `getClosestSegment`

**segmentIntersectsBoundingBoxCoordinates**
This function was one of the sub-functions called by `getClickedSegment` (see call graph above), and returns *how* a segment intersects a bounding box using a tuple of (bool, XYPos option). This return type is unintuitive, is not well reflected by the function name, and is never even properly used. In both functions that call it, it is simply being used to check *if* segments intersect a bounding box, and not *how*. This means that the entire function is not needed, as `segmentIntersectsBoundingBox` can be used instead.

**partialAutoRoute / topology**
This function is part of the top level `updateWire` function, which has the following call graph:
![]( sts219/updateWire.png)
This function is too long, and uses many sub-function / other functions, some of which have particularly bad names. `topology` is one of the worst offenders, with no documentation as to what the function is meant to do. The `scaleBeforeSegmentEnd` sub-function is also very confusing, with unhelpful variable names and long lines of code.

#### Other problems

State **concisely** Issues with existing code, or in refactoring for new types, that do not fit into per function list. 
Again numbered points, at most 3. Choose the most inmportant if you have too much to say. You can should
refer to documentation (not XML docs) in code where this helps.

**Segments**
This type is terrible in a few ways:
- Segments containing both a start and an end position is duplicate information
- Negating the XYPos of a segment is an awful way of indicating that a wire is being manually routed, and leads to `abs` being used everywhere in the code
This was improved as follows:
- Segments only contain a length, and alternating segments are in opposite orientations. The sign of the length indicates whether the wire runs in an increasing or decreasing X/Y direction.
- Segments contain a `Mode` attribute, which is either `Manual` or `Auto` to indicate how the wire is being routed.
- The initial / final positions and orientations are stored in the wire. Both endpoints are stored to allow the wire to be processed from either direction.

**BoundingBox**
The main issue with this type is what its `X` and `Y` fields represent. Is it the center of the box, the top left, or something else entirely. There is no documentation on the type itself, and quite literally no-one seemed to know definitively (our advisor, TC, etc.). From it's usage in the code, it was determined that these fields represent an `XYPos` of the top left corner of the boundingbox. I therefore switched these `X` and `Y` fields for a single `TopLeft: XYPos` field, which should avoid any future confusion.

**Wires (Segment list)**
There were 2 types of segment lists considered in the existing Issie functionality ('3'-seg and '5'-seg) to route between different IO configurations. A lot of their functionality was hard coded in using this assumed 7-segment length, which is bad as it makes it more difficult to include new cases for alternate port orientations. In both cases, the wires have small 'nubs' at both ports. This allows all the visible segments in the 3-segment wire to be dragged:
![]( sts219/dragged3seg.png)

**Unused functions**
There were a significant amount of functions, some quite complex, that were never called anywhere in the code (see below):
![]( sts219/unused.png)
Those with debugging utility were moved into the debugging section at the top of the code, functional ones were removed.

### Analysis of how/why code works

This section need not contain analysis if the code can be demonstarted working. In 
that case list as bullet points the features you will demonstarte (quickly) in the 5 min
interview.

* If code works fully and can be demonstrated in the 5 minute feedback interview no analysis is needed. 
* If code will be manually tested in interview say what the tests are that supplement your analysis
* Interview code must be the assessed branch (not something else, or using later group code)
* A good way to show code works is to explain how it differs from existing working code and how existing
functionality is preserved.

**moveSegment**
This function allows individual segments of a wire in Issie to be dragged in a direction perpendicular to its orientation. It's original call tree is below:
![]( sts219/moveSegment.png)
`getSafeDistanceForMove` is meant to prevent users from dragging wire segments perpendicular to the port orientation too close to the port.

# Extensions

1.  List as numbered points the extensions (features) your code will support

     a. Use nested letters for the functions you have written extra, 
     or changed, to allow this, and for concise comments concise comments about why they work.

