
## Admin and quick access links

(https://github.com/tomcl/hlp22docs/blob/main/Team9.md)

[Buswire (section 3)](https://github.com/sts219/HLP_Project/blob/hlp22-indiv-assess-nh1019/src/Renderer/DrawBlock/BusWire.fs)

Section 3 on my file is lines : 1371-1808

## Analysis

### Changes 
* Renamed `getWireIfClicked` to `getClickedWire`
* Deleted `getPortIdsOfWires` - it wasn't used anywhere
* Removed the use of `abs` everywhere as it was no longer needed with the type changes in `Segment` 
* `DragWire` in `update` was changed slightly to use `getSegmentOrientation` instead of segment coordinates
* Option to change `WireType` (options being `Jump`, `Radial`, `Modern`) implemented using messages triggered by clicking an option in the Edit menu
* Rest of the functions in this section generally have good names

#### Bad Function list


* The subfunction `createNewWire` (l. 1774 - 1793, part of `pasteWires`) has issues with readability in its match statement due to the number of external functions and parameters it needs to use - however these are essential and the expressions themselves have no issue making sense/providing context (thanks to good names), they are just long.
* `makeAllJumps` still has issues with readability after refactoring due to the use of a nested for loop, which themselves contain a few, slightly complex, if-else statements. The use of a mutable map (`newWX`) and a mutable list (`jumps`) are also not ideal by ISSIE guideline standards.
* `updateWires` contains a subfunction, `newWires` (l. 1456-1467), whose parameters are well named and overall it is readable, however the use of a long combination of if-elif-else statements is not ideal. The simplicity and readability of these expressions allows it to remain unchanged (apart from refactoring for new types).


#### Other problems

* The parameter `style` on l. 1724 is not ideal, however the more logical suggestion of `type` is a keyword, and `style` should quickly make sense in the given context.
* `processConWidths` in `BusWidths` (part of `update`) is also not a good name but its only parameter (`connWidths: ConnectionsWidth`) gives it enough context so we do not need to rename.

### Analysis of how/why code works

* makeAllJumps (l. 1371-1429) needed to be quite heavily refactored as it used many types which had been changed. Our segments had no direction, start position, or end position fields, so these had to be found within the function using helper functions. The following changes were made:
    * Removed `h.Dir` and wrote the boolean helper function `verticalSeg` which uses `getSegmentOrientation` to determine whether the current segment is vertical or not.
    * If the wires intersect, `jumps` is updated - the expression `foldOverSegs innerFold (segStart, segEnd) wire'` applies this process to every segment in the given wire
    * The pattern match expression is essentially the same as that used in the original
    * Finally, `foldOverSegs` is used once more to apply the whole process to the wire being iterated over in the outer loop - `newWX` is updated every time it needs to be and the function finally returns the `model` with `newWX` as its `Wires`

* A menu option to switch between wire types was also added in the Edit menu. This works by using the new `type WireTypeMsg = | Jump | Radial | Modern` - a message that is dispatched from Renderer to Sheet when the option is clicked, and implemented in `BusWire.update` (specifically l. 1725-1732).
* `update` also contains a `Rotate` message, which comes from Symbol and updates the necessary wires' routing by finding those connected to the affected symbol and applying the `getConnectedWires` function.
* The rest of `update` has been kept largely the same in terms of functionality, but constant refactoring was needed as it calls a lot of external functions and types that were changed.


