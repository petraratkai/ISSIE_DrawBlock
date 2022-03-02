# Example README for individual code submission

## Team Shared Team.md

[Team Contribution Repo](https://github.com/sts219/HLP_Project)


## Admin and quick access links

*link to your teamN.md file*
[Common repo TeamN file](https://github.com/sts219/hlp22docs/blob/main/Team9.md)

[Symbol (section 1)](src/renderer/drawblock/Symbol.fs)

Section 1 on my file is lines : 0-700

Anything else you need to say about what you are or are not responsible for.

## Code partitioning notes (don't copy this section in your submitted README - delete it)

Insert clear comments in the source files indicating where each person's code starts and stops

```
//---------------------------------------------------------------------------------//
//--------------------TJWC CODE SECTION STARTS-------------------------------------//
//---------------------------------------------------------------------------------//
```

## Note on assessment

The aim of assessment if that the first two sections should be fairly straightforward and demonstrate basic knowledge
of how to write FP code. They can achieve overall marks up to 70% without extension work. Those working on 
significant  extensions should put extra effort into these and not complex analysis section since the 
analysis marks can accept well-written code supporting extensions as evidence of understanding. 
The aim of this is so that you get marks for good code, 
if you can write signiifcant good code without being motivated to write reports.

There are signiifcant differences between different code sections, and also things which 
change dependning on what your base types are and how ambitious your code rewrite it. Ask early
if you are concerned about how this assessment will impact you. There will be ways to reward code 
writing which can be found if need be in individual cases.

## Code Quality

This will be assessed based on the code. You can **but do not have to** highlight here things you are particularly proud of and want markers to look at (up to 3) as short bullet points:

* Naming of `myGoodName`, `myWonderfulLongName`
* New function boundaries: `topfun` -> `firstfun`, `topfun` -> `secondFun`
* New types: MyGoodType
* Helper function `myHelper` which is higher order and really helpful
* I cleaned up all the sprintf and printf statements to use interpolated strings

Your code will all be assessed anyway, so this is optional.

## Analysis

This combines analysis of **issues in existing code** with **proof or analysis of how/why the new code works**. 
The marking combines both parts. If new code already works and is well written (especially if it is 
demonstrated to have significant value in terms of ability to support extensions (see Extensions below)
less evidence will be needed for a high mark in this section.
The analysis here demonstrates that you understand the problems in the old code. 
If the old code has no or few problems you can say this.

Anything you say here may if necessary be tested in the 5 min feedback interview to check understanding.

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

**moveSegment**
This function allows individual segments of a wire in Issie to be dragged in a direction perpendicular to its orientation. It's original call tree is below:
`getSafeDistanceForMove` is meant to prevent users from dragging wire segments perpendicular to the port orientation too close to the port.

# Extensions

1.  List as numbered points the extensions (features) your code will support

     a. Use nested letters for the functions you have written extra, 
     or changed, to allow this, and for concise comments concise comments about why they work.


