module Notes

-Changed compo to component

-Added string.toUpper to custom component in prefix

-Create new symbol

Created new variables orientation and port order which shows the orientation of each port and the port order on each edge of the symbol respectively
Created new functions getId to get the id number of ports
Created portMap function which maps the port list 
Created concatMap to concatenate two maps together

Created APortOffsetsMap which takes a symbol and outputs a map of the port and the offset from the centre
Depending on the rotation of the symbol, 

Created getPortEdge function which outputs x and y coordinates of ports depending on the orientation

Created genAPortOffsets which generates a list of offsets depending on the edge
helper function centreOffset to calculate a vector offset from the centre depending on the edge ports are located