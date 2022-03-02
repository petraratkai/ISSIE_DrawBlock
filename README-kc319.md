# Example README for individual code submission

## Team Shared Team.md

[Team Contribution Repo](https://github.com/sts219/HLP_Project)

## Admin and quick access links

*link to your teamN.md file*
[Common repo TeamN file](https://github.com/sts219/hlp22docs/blob/main/Team9.md)

[Symbol (section 1)](src/renderer/drawblock/Symbol.fs)

Section 1 on my file is lines : 0-700

Anything else you need to say about what you are or are not responsible for.

Some support for rotateSymbol (Interfacing with sheet to provide the rotate message)
and some other small functions such as getHAndW done by Petra.
Rotation points for MUX implemented by Petra (in drawSymbol), I modified to its current state and got working with my functions.

## Analysis

### Issues in Existing Code

#### Bad Function list

compSymbol
Very indescriptive name and unnecessary argument in Comp (it is passed inside symbol anyway)
There is no use of interpolated strings. Poor documentation for a big function so needs reading to comprehend.

getPortPos
only assumes ports go on left / right of symbol so needs refactoring to account for other edges

drawPortsText / portText
also with the assumption ports are always left or right so needs updating for possibility of different edges

#### Other problems

Many exisiting functions and variables have names that do not describe their purpose clearly such as
'gateDecoderType', 'portDecName', 'customToLength'.
Some functions pass in unecessary arguments e.g. 'gateDecoderType' uses Component when only ComponentType is accessed
in the function. 
No use of advanced helpful features e.g. interpolated strings / custom types like XYPos


### Analysis of how/why code works

Demonstrable

Rotation in left and right direction
Port on different edges (not just left right)
Clear port text in all the cases of rotation / port on bottom

# Extensions

1.  List as numbered points the extensions (features) your code will support

     a. Use nested letters for the functions you have written extra, 
     or changed, to allow this, and for concise comments concise comments about why they work.

1. Rotation of basic symols + mux
    a. rotateSymbol
    b. member functions for new types

2. Support for non standard port placement for mux/reg etc


