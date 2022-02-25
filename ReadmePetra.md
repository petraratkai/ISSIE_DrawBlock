Updates on Sheet 2
=========
Types
-------
Updated type of symbol
- added fields: PortOrientation, STransform and PortOrder
- PortOrientation and PortOrder are used to determine which edge and what order the ports are in
- STransform represents the rotation of the symbol and whether it is flipped or not
- changed Compo field to Component to be more clear

Changes
-------
- updated Compo to Component
- added function initPortOrientation to use it in loadcomponents msg, currently initialises all the inputs on the left, all the outputs on the right
- renamed getOneBoundingBox to getBoundingBox
- getBoundingBoxofSymbol renamed to getSymbolBoundingBox + handles rotation now
- removed the old getOnePortLocation because wasn't used
- getOnePortLocationNew updated so that it is now O(logn) complexity instead of O(n), renaming it to getPortLocation
- getInputPortLocation, getOutputPortLocation made them more efficient by using the new getPortLocation + simplified them by removing code duplication
- getTwoPortLocations - now just uses getInputPortLocation and getOutputPortLocation
- might make sense to only call getPortLocation
- getInputPortsPositionMap changed so that it only goes through the outputports once not twice
- getInputPortsPositionMap changed so that it only goes through the outputports once not twice
- can we change these such that there is no code duplication between them?
	- probably with the wrapper we can?
- got rid of the use of posAdd and posDiff, using + and - operator on XYPoses
- getPortLocations rename variables: model, sIds getSymbol
- filterString renamed to removeCount and removed capitalisation, so that the fn only does one thing + most prefixes are already capitalised. Moved toUpper into generatePrefix
- regex renamed to getCount, non-descriptive name originally
- added samePrefix which checks if a component has same component type as target
- moved everything from labelGenNumber to generateLabel, removed labelGenNumber
- removed all the match cases from samePrefix, now it only uses the prefix function to comptare the type of components
- removed removeCount
- renamed getCount to getLabelNumber
- renamed createNewSymbol to addNewSymbol
- pasteSymbols now instead of sorting oldsymbols, just using minBy to find minimum element, complexity O(nlogn) to O(n)
- getEquivalent copied ports removed the checking of whether lists are empty, tryfind returns none anyway
- getEquivalent copied ports completely refactored, made more efficient, not checking every symbol in the list, finds the copied symbol from the port
- color symbols now O(nlogn) instead of O(n2)
- noticed: pasteSymbols
- added ui for rotation, new option in edit menu (idk if it works)
- bounding boxes should just work with current repr of symbol pos
- port position hmmmmmmmmmm
- getPortPos2 rewritten, now it works with rotations as well. Haven't done mux top and pottom coordinates yet though

                                                                                                                                                                                                                                                                                                                                


