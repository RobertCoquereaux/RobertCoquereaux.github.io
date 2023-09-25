(* Content-type: application/vnd.wolfram.cdf.text *)

(*** Wolfram CDF File ***)
(* http://www.wolfram.com/cdf *)

(* CreatedBy='Mathematica 11.1' *)

(*************************************************************************)
(*                                                                       *)
(*  The Mathematica License under which this file was created prohibits  *)
(*  restricting third parties in receipt of this file from republishing  *)
(*  or redistributing it by any means, including but not limited to      *)
(*  rights management or terms of use, without the express consent of    *)
(*  Wolfram Research, Inc. For additional information concerning CDF     *)
(*  licensing and redistribution see:                                    *)
(*                                                                       *)
(*        www.wolfram.com/cdf/adopting-cdf/licensing-options.html        *)
(*                                                                       *)
(*************************************************************************)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[      1064,         20]
NotebookDataLength[    119938,       2315]
NotebookOptionsPosition[    119690,       2283]
NotebookOutlinePosition[    120223,       2306]
CellTagsIndexPosition[    120180,       2303]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{

Cell[CellGroupData[{
Cell["\<\
DECOMPOSITION INTO A DIRECT SUM OF THE PRODUCT OF IRREDUCIBLE REPRESENTATIONS \
OF SU(n)  - Code\
\>", "Subsection", "PluginEmbeddedContent",
 CellFrame->{{0, 0}, {1, 0}},
 CellFrameColor->RGBColor[0.87, 0.87, 0.87],
 FontFamily->"Helvetica",
 FontSize->12,
 FontWeight->"Bold",
 FontColor->RGBColor[
  0.597406, 0, 
   0.0527047],ExpressionUUID->"00000000-0000-0000-0000-000000000000"],

Cell[BoxData[
 DynamicModuleBox[{$CellContext`rows$$ = 2, $CellContext`cols$$ = 
  3, $CellContext`mat$$ = {{1, 0, 0}, {3, 2, 2}}}, 
  DynamicBox[ToBoxes[
    Manipulate[
     Row[{"Tensor product decomposition", 
       MatrixForm[$CellContext`mat$$], " = ", 
       (CheckAbort[
        If[Length[#] > 800, 
         Abort[], #], 
        Print[
        "Output too large: the number of inequivalent irreps should be \
smaller than ", 800]]& )[
        TimeConstrained[
         shorttensorProdIrrepPackage`tenspromatform[$CellContext`mat$$], 1.5, 
         
         Print[
         "More time needed: the computation is terminated if it takes more \
than ", 1.5, " seconds"]]]}], 
     Evaluate[
      With[{$CellContext`makeRow$ = Function[{$CellContext`rowIndex$}, 
          Map[
           Function[{$CellContext`colIndex$}, 
            InputField[
             Dynamic[
              
              Part[$CellContext`mat$$, $CellContext`rowIndex$, \
$CellContext`colIndex$]], Number, FieldSize -> 3]], 
           Range[$CellContext`cols$$]], HoldAll]}, 
       Row[{
         Grid[
          Map[$CellContext`makeRow$, 
           Range[$CellContext`rows$$]]], 
         Invisible["xxxxxx"], 
         Column[{
          "Tensor product of two irreducible representations specified by \
their higest weight.", 
           "Lines refer to the components of the chosen highest weights in \
the basis of fundamental weights (Dynkin basis)."}]}]]], 
     Row[{
       Panel[
        Style[1 + Part[
           Dimensions[$CellContext`mat$$], 2], FontSize -> 14, Bold], 
        Style["SU(n), with n =", Bold, FontSize -> 14], Left, 
        BaselinePosition -> Bottom], 
       Panel[
        Style[
         Column[{
          "Product of irreducible representations", "of SU(n)", 
           "Decomposition into direct sum"}, Center], "Title"]]}, 
      Invisible["xxxxxx"]], 
     Column[{
      "The result is a list of pairs:", 
       "The first element gives the components of each term of the \
decomposition.", "the second element gives its multiplicity."}], 
     Button["Decrease n", 
      If[$CellContext`rows$$ > 
       1, $CellContext`mat$$ = Map[Drop[#, -1]& , $CellContext`mat$$]; 
       Decrement[$CellContext`cols$$]; Null]], 
     Button["Increase n", $CellContext`mat$$ = Transpose[
         Append[
          Transpose[$CellContext`mat$$], 
          Table[0, {1 + 1}]]]; Increment[$CellContext`cols$$]; Null]], 
    StandardForm],
   ImageSizeCache->{904., {210., 216.}}],
  SynchronousUpdating -> False,
  DynamicModuleValues:>{},
  Initialization:>{shorttensorProdIrrepPackage`tenspromatform[
      Pattern[Private`mat, 
       Blank[]]] := 
    Apply[shorttensorProdIrrepPackage`tensorproduct, Private`mat], 
    TagSet[shorttensorProdIrrepPackage`tenspromatform, 
     MessageName[shorttensorProdIrrepPackage`tenspromatform, "usage"], 
     "tenspromatform[{lambda,mu}] returns tensorproduct[lambda,mu]"], 
    shorttensorProdIrrepPackage`tensorproduct[{
       Pattern[Private`lam, 
        BlankSequence[]]}, {
       Pattern[Private`mu, 
        BlankSequence[]]}] := 
    If[Length[{Private`lam}] == Length[{Private`mu}], 
      Switch[
       ValueQ[shorttensorProdIrrepPackage`currentgroup], False, 
       shorttensorProdIrrepPackage`oblade[Length[{Private`lam}] + 1]; 
       shorttensorProdIrrepPackage`tensorproduct[{Private`lam}, {Private`mu}],
        True, 
       If[
       Length[{Private`lam}] == shorttensorProdIrrepPackage`currentgroup - 1, 
        
        SortBy[
         Tally[
          Map[Reverse, 
           Private`tensorproductCC[{Private`lam}, {Private`mu}]]], Last], 
        shorttensorProdIrrepPackage`oblade[Length[{Private`lam}] + 1]; 
        shorttensorProdIrrepPackage`tensorproduct[{Private`lam}, {
          Private`mu}]]], 
      Print["Weights should have the same number of components"]], 
    TagSet[shorttensorProdIrrepPackage`tensorproduct, 
     MessageName[shorttensorProdIrrepPackage`tensorproduct, "usage"], 
     "tensorproduct[lambda,mu] returns the decomposition into irreducible \
components nu of a tensor product of two irreps lambda, mu, of the Lie group \
SU(n), given by the components of their highest weights along the basis of \
fundamental weights.  The last entry of each term is its multiplicity."], 
    shorttensorProdIrrepPackage`currentgroup = 4, 
    TagSet[shorttensorProdIrrepPackage`currentgroup, 
     MessageName[shorttensorProdIrrepPackage`currentgroup, "usage"], 
     "currentgroup returns the argument n of the group SU(n) currently in \
use."], shorttensorProdIrrepPackage`oblade[
      Pattern[Private`var, 
       Blank[]]] := 
    With[{Private`NN = Private`var}, Private`lambdaequations = And[
         Apply[And, 
          Table[Private`lambda[Private`p, 0, 0] == Sum[
             Private`j[
             Private`p, Private`NN - (Private`s + Private`p), Private`s], {
             Private`s, 0, Private`NN - Private`p}], {
           Private`p, 1, Private`NN - 1}]], 
         Apply[And, 
          Table[Private`lambda[0, Private`p, 0] == Sum[
             Private`j[
             Private`s, Private`p, Private`NN - (Private`s + Private`p)], {
             Private`s, 0, Private`NN - Private`p}], {
           Private`p, 1, Private`NN - 1}]], 
         Apply[And, 
          Table[Private`lambda[0, 0, Private`p] == Sum[
             Private`j[
             Private`s, Private`NN - (Private`s + Private`p), Private`p], {
             Private`s, 0, Private`NN - Private`p}], {
           Private`p, 1, Private`NN - 1}]]]; 
      Private`edgesEW[Private`NN] = Table[
         FoldList[Plus][
          Table[
           Private`j[
           Private`t, Private`NN - Private`s, Private`s - Private`t], {
           Private`s, Private`t, Private`NN - 1}]], {
         Private`t, 1, Private`NN - 1}]; Private`edgesNESW[Private`NN] = Table[
         FoldList[Plus][
          Table[
           Private`j[
           Private`s - Private`p, Private`p, Private`NN - Private`s], {
           Private`s, Private`p, Private`NN - 1}]], {
         Private`p, 1, Private`NN - 1}]; Private`edgesNWSE[Private`NN] = Table[
         FoldList[Plus][
          Table[
           Private`j[
           Private`NN - Private`s, Private`s - Private`p, Private`p], {
           Private`s, Private`p, Private`NN - 1}]], {
         Private`p, 1, Private`NN - 1}]; Private`edgesEWlabels[
         Pattern[Private`lev, 
          Blank[]]] := Table[
         Take[
          
          Table[(1/2) (4 + Private`p + Private`p^2) + (Private`lev - 1), {
           Private`p, Private`lev, Private`NN}], {
          Private`z, Private`z + 1}], {
         Private`z, 1, Private`NN - Private`lev}]; Private`edgesNESWlabels[
         Pattern[Private`lev, 
          Blank[]]] := Table[
         Take[
          
          Table[(1/2) (3 Private`p + Private`p^2) + (1/2) (Private`lev - 1) 
            Private`lev + (Private`lev - 1) Private`p, {
           Private`p, 1, Private`NN}], {Private`z, Private`z + 1}], {
         Private`z, 1, Private`NN - Private`lev}]; Private`edgesNWSElabels[
         Pattern[Private`lev, 
          Blank[]]] := Table[
         Take[
          Range[
          1 + (1/2) (-Private`lev + Private`NN) (1 - Private`lev + 
             Private`NN), 1 - Private`lev + 
           Private`NN + (1/2) (-Private`lev + Private`NN) (1 - Private`lev + 
             Private`NN)], {Private`z, Private`z + 1}], {
         Private`z, 1, Private`NN - Private`lev}]; Private`components = Join[
         Table[
          Table[
           Private`j[
           Private`p, Private`NN - (Private`s + Private`p), Private`s], {
           Private`s, 0, Private`NN - Private`p}], {
          Private`p, 1, Private`NN - 1}], 
         Table[
          Table[
           Private`j[
           Private`s, Private`p, Private`NN - (Private`s + Private`p)], {
           Private`s, 0, Private`NN - Private`p}], {
          Private`p, 1, Private`NN - 1}], 
         Table[
          Table[
           Private`j[
           Private`s, Private`NN - (Private`s + Private`p), Private`p], {
           Private`s, 0, Private`NN - Private`p}], {
          Private`p, 1, Private`NN - 1}]]; 
      Private`fundamentalcomponents = Union[
         Flatten[Private`components]]; Private`positivityconstraints = And[
         Apply[And, 
          Map[# >= 0& , 
           Flatten[
            Private`edgesEW[Private`NN]]]], 
         Apply[And, 
          Map[# >= 0& , 
           Flatten[
            Private`edgesNESW[Private`NN]]]], 
         Apply[And, 
          Map[# >= 0& , 
           Flatten[
            Private`edgesNWSE[Private`NN]]]]]; Private`buildlambdainput[{
          Pattern[Private`l1, 
           BlankSequence[]]}, {
          Pattern[Private`l2, 
           BlankSequence[]]}] := Flatten[{
          MapThread[Rule, {
            Table[
             Private`lambda[Private`p, 0, 0], {
             Private`p, 1, Private`NN - 1}], {Private`l1}}], 
          MapThread[Rule, {
            Table[
             Private`lambda[0, Private`p, 0], {
             Private`p, 1, Private`NN - 1}], {Private`l2}}]}]; 
      Private`buildlambdainput[{
          Pattern[Private`l1, 
           BlankSequence[]]}, {
          Pattern[Private`l2, 
           BlankSequence[]]}, {
          Pattern[Private`l3, 
           BlankSequence[]]}] := Flatten[{
          MapThread[Rule, {
            Table[
             Private`lambda[Private`p, 0, 0], {
             Private`p, 1, Private`NN - 1}], {Private`l1}}], 
          MapThread[Rule, {
            Table[
             Private`lambda[0, Private`p, 0], {
             Private`p, 1, Private`NN - 1}], {Private`l2}}], 
          MapThread[Rule, {
            Table[
             Private`lambda[0, 0, Private`p], {
             Private`p, 1, Private`NN - 1}], {Private`l3}}]}]; 
      Private`componentsEdgesAndnuc[{
          Pattern[Private`lam, 
           BlankSequence[]]}, {
          Pattern[Private`mu, 
           BlankSequence[]]}] := (
        Private`componentsEdgesAndnuc[{Private`lam}, {Private`mu}] = 
        Module[{Private`redsol, Private`locrules, Private`nuclist, 
           Private`fundcompval, Private`edgesEWValues, 
           Private`edgesNESWValues, Private`edgesNWSEValues}, 
          Private`redsol = Solve[
             And[
              ReplaceAll[Private`lambdaequations, 
               Private`buildlambdainput[{Private`lam}, {Private`mu}]], 
              Private`positivityconstraints], Integers]; 
          Private`nuclist = ReplaceAll[
             Table[
              Private`lambda[0, 0, Private`p], {
              Private`p, 1, Private`NN - 1}], 
             Private`redsol]; {{}, {}, {}, {}, Private`nuclist}]); 
      shorttensorProdIrrepPackage`currentgroup = Private`NN], 
    TagSet[shorttensorProdIrrepPackage`oblade, 
     MessageName[shorttensorProdIrrepPackage`oblade, "usage"], 
     "Evaluation of oblade[n] initializes all relevant data for SU(n). It \
returns n and set the variable currentgroup to n."], Private`lambdaequations = 
    And[Private`lambda[1, 0, 0] == 
      Private`j[1, 0, 3] + Private`j[1, 1, 2] + Private`j[1, 2, 1] + 
       Private`j[1, 3, 0], Private`lambda[2, 0, 0] == 
      Private`j[2, 0, 2] + Private`j[2, 1, 1] + Private`j[2, 2, 0], 
      Private`lambda[3, 0, 0] == Private`j[3, 0, 1] + Private`j[3, 1, 0], 
      Private`lambda[0, 1, 0] == 
      Private`j[0, 1, 3] + Private`j[1, 1, 2] + Private`j[2, 1, 1] + 
       Private`j[3, 1, 0], Private`lambda[0, 2, 0] == 
      Private`j[0, 2, 2] + Private`j[1, 2, 1] + Private`j[2, 2, 0], 
      Private`lambda[0, 3, 0] == Private`j[0, 3, 1] + Private`j[1, 3, 0], 
      Private`lambda[0, 0, 1] == 
      Private`j[0, 3, 1] + Private`j[1, 2, 1] + Private`j[2, 1, 1] + 
       Private`j[3, 0, 1], Private`lambda[0, 0, 2] == 
      Private`j[0, 2, 2] + Private`j[1, 1, 2] + Private`j[2, 0, 2], 
      Private`lambda[0, 0, 3] == Private`j[0, 1, 3] + Private`j[1, 0, 3]], 
    Private`edgesEW[2] = {{
       Private`j[1, 1, 0]}}, Private`edgesEW[3] = {{
       Private`j[1, 2, 0], Private`j[1, 1, 1] + Private`j[1, 2, 0]}, {
       Private`j[2, 1, 0]}}, Private`edgesEW[4] = {{
       Private`j[1, 3, 0], Private`j[1, 2, 1] + Private`j[1, 3, 0], 
       Private`j[1, 1, 2] + Private`j[1, 2, 1] + Private`j[1, 3, 0]}, {
       Private`j[2, 2, 0], Private`j[2, 1, 1] + Private`j[2, 2, 0]}, {
       Private`j[3, 1, 0]}}, Private`edgesEW[5] = {{
       Private`j[1, 4, 0], Private`j[1, 3, 1] + Private`j[1, 4, 0], 
       Private`j[1, 2, 2] + Private`j[1, 3, 1] + Private`j[1, 4, 0], 
       Private`j[1, 1, 3] + Private`j[1, 2, 2] + Private`j[1, 3, 1] + 
       Private`j[1, 4, 0]}, {
       Private`j[2, 3, 0], Private`j[2, 2, 1] + Private`j[2, 3, 0], 
       Private`j[2, 1, 2] + Private`j[2, 2, 1] + Private`j[2, 3, 0]}, {
       Private`j[3, 2, 0], Private`j[3, 1, 1] + Private`j[3, 2, 0]}, {
       Private`j[4, 1, 0]}}, Private`edgesNESW[2] = {{
       Private`j[0, 1, 1]}}, Private`edgesNESW[3] = {{
       Private`j[0, 1, 2], Private`j[0, 1, 2] + Private`j[1, 1, 1]}, {
       Private`j[0, 2, 1]}}, Private`edgesNESW[4] = {{
       Private`j[0, 1, 3], Private`j[0, 1, 3] + Private`j[1, 1, 2], 
       Private`j[0, 1, 3] + Private`j[1, 1, 2] + Private`j[2, 1, 1]}, {
       Private`j[0, 2, 2], Private`j[0, 2, 2] + Private`j[1, 2, 1]}, {
       Private`j[0, 3, 1]}}, Private`edgesNESW[5] = {{
       Private`j[0, 1, 4], Private`j[0, 1, 4] + Private`j[1, 1, 3], 
       Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2], 
       Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
       Private`j[3, 1, 1]}, {
       Private`j[0, 2, 3], Private`j[0, 2, 3] + Private`j[1, 2, 2], 
       Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1]}, {
       Private`j[0, 3, 2], Private`j[0, 3, 2] + Private`j[1, 3, 1]}, {
       Private`j[0, 4, 1]}}, Private`edgesNWSE[2] = {{
       Private`j[1, 0, 1]}}, Private`edgesNWSE[3] = {{
       Private`j[2, 0, 1], Private`j[1, 1, 1] + Private`j[2, 0, 1]}, {
       Private`j[1, 0, 2]}}, Private`edgesNWSE[4] = {{
       Private`j[3, 0, 1], Private`j[2, 1, 1] + Private`j[3, 0, 1], 
       Private`j[1, 2, 1] + Private`j[2, 1, 1] + Private`j[3, 0, 1]}, {
       Private`j[2, 0, 2], Private`j[1, 1, 2] + Private`j[2, 0, 2]}, {
       Private`j[1, 0, 3]}}, Private`edgesNWSE[5] = {{
       Private`j[4, 0, 1], Private`j[3, 1, 1] + Private`j[4, 0, 1], 
       Private`j[2, 2, 1] + Private`j[3, 1, 1] + Private`j[4, 0, 1], 
       Private`j[1, 3, 1] + Private`j[2, 2, 1] + Private`j[3, 1, 1] + 
       Private`j[4, 0, 1]}, {
       Private`j[3, 0, 2], Private`j[2, 1, 2] + Private`j[3, 0, 2], 
       Private`j[1, 2, 2] + Private`j[2, 1, 2] + Private`j[3, 0, 2]}, {
       Private`j[2, 0, 3], Private`j[1, 1, 3] + Private`j[2, 0, 3]}, {
       Private`j[1, 0, 4]}}, Private`edgesEWlabels[
      Pattern[Private`lev$, 
       Blank[]]] := Table[
      Take[
       Table[(1/2) (4 + Private`p + Private`p^2) + (Private`lev$ - 1), {
        Private`p, Private`lev$, 4}], {Private`z, Private`z + 1}], {
      Private`z, 1, 4 - Private`lev$}], 
    Attributes[Private`lev$] = {Temporary}, Private`edgesNESWlabels[
      Pattern[Private`lev$, 
       Blank[]]] := Table[
      Take[
       Table[(1/2) (3 Private`p + Private`p^2) + ((1/2) (Private`lev$ - 1)) 
         Private`lev$ + (Private`lev$ - 1) Private`p, {Private`p, 1, 4}], {
       Private`z, Private`z + 1}], {Private`z, 1, 4 - Private`lev$}], 
    Private`edgesNWSElabels[
      Pattern[Private`lev$, 
       Blank[]]] := Table[
      Take[
       Range[
       1 + ((1/2) (-Private`lev$ + 4)) (1 - Private`lev$ + 4), 1 - 
        Private`lev$ + 
        4 + ((1/2) (-Private`lev$ + 4)) (1 - Private`lev$ + 4)], {
       Private`z, Private`z + 1}], {Private`z, 1, 4 - Private`lev$}], 
    Private`components = {{
       Private`j[1, 3, 0], 
       Private`j[1, 2, 1], 
       Private`j[1, 1, 2], 
       Private`j[1, 0, 3]}, {
       Private`j[2, 2, 0], 
       Private`j[2, 1, 1], 
       Private`j[2, 0, 2]}, {
       Private`j[3, 1, 0], 
       Private`j[3, 0, 1]}, {
       Private`j[0, 1, 3], 
       Private`j[1, 1, 2], 
       Private`j[2, 1, 1], 
       Private`j[3, 1, 0]}, {
       Private`j[0, 2, 2], 
       Private`j[1, 2, 1], 
       Private`j[2, 2, 0]}, {
       Private`j[0, 3, 1], 
       Private`j[1, 3, 0]}, {
       Private`j[0, 3, 1], 
       Private`j[1, 2, 1], 
       Private`j[2, 1, 1], 
       Private`j[3, 0, 1]}, {
       Private`j[0, 2, 2], 
       Private`j[1, 1, 2], 
       Private`j[2, 0, 2]}, {
       Private`j[0, 1, 3], 
       Private`j[1, 0, 3]}}, Private`fundamentalcomponents = {
      Private`j[0, 1, 3], 
      Private`j[0, 2, 2], 
      Private`j[0, 3, 1], 
      Private`j[1, 0, 3], 
      Private`j[1, 1, 2], 
      Private`j[1, 2, 1], 
      Private`j[1, 3, 0], 
      Private`j[2, 0, 2], 
      Private`j[2, 1, 1], 
      Private`j[2, 2, 0], 
      Private`j[3, 0, 1], 
      Private`j[3, 1, 0]}, Private`positivityconstraints = 
    And[Private`j[1, 3, 0] >= 0, Private`j[1, 2, 1] + Private`j[1, 3, 0] >= 0,
       Private`j[1, 1, 2] + Private`j[1, 2, 1] + Private`j[1, 3, 0] >= 0, 
      Private`j[2, 2, 0] >= 0, Private`j[2, 1, 1] + Private`j[2, 2, 0] >= 0, 
      Private`j[3, 1, 0] >= 0, Private`j[0, 1, 3] >= 0, 
      Private`j[0, 1, 3] + Private`j[1, 1, 2] >= 0, 
      Private`j[0, 1, 3] + Private`j[1, 1, 2] + Private`j[2, 1, 1] >= 0, 
      Private`j[0, 2, 2] >= 0, Private`j[0, 2, 2] + Private`j[1, 2, 1] >= 0, 
      Private`j[0, 3, 1] >= 0, Private`j[3, 0, 1] >= 0, 
      Private`j[2, 1, 1] + Private`j[3, 0, 1] >= 0, 
      Private`j[1, 2, 1] + Private`j[2, 1, 1] + Private`j[3, 0, 1] >= 0, 
      Private`j[2, 0, 2] >= 0, Private`j[1, 1, 2] + Private`j[2, 0, 2] >= 0, 
      Private`j[1, 0, 3] >= 0], Private`buildlambdainput[{
       Pattern[Private`l1$, 
        BlankSequence[]]}, {
       Pattern[Private`l2$, 
        BlankSequence[]]}] := Flatten[{
       MapThread[Rule, {
         Table[
          Private`lambda[Private`p, 0, 0], {Private`p, 1, 4 - 1}], {
         Private`l1$}}], 
       MapThread[Rule, {
         Table[
          Private`lambda[0, Private`p, 0], {Private`p, 1, 4 - 1}], {
         Private`l2$}}]}], Private`buildlambdainput[{
       Pattern[Private`l1$, 
        BlankSequence[]]}, {
       Pattern[Private`l2$, 
        BlankSequence[]]}, {
       Pattern[Private`l3$, 
        BlankSequence[]]}] := Flatten[{
       MapThread[Rule, {
         Table[
          Private`lambda[Private`p, 0, 0], {Private`p, 1, 4 - 1}], {
         Private`l1$}}], 
       MapThread[Rule, {
         Table[
          Private`lambda[0, Private`p, 0], {Private`p, 1, 4 - 1}], {
         Private`l2$}}], 
       MapThread[Rule, {
         Table[
          Private`lambda[0, 0, Private`p], {Private`p, 1, 4 - 1}], {
         Private`l3$}}]}], Attributes[Private`l1$] = {Temporary}, 
    Attributes[Private`l2$] = {Temporary}, 
    Attributes[Private`l3$] = {Temporary}, 
    Private`componentsEdgesAndnuc[{1}, {3}] = {{}, {}, {}, {}, {{2}, {4}}}, 
    Private`componentsEdgesAndnuc[{1}, {6}] = {{}, {}, {}, {}, {{5}, {7}}}, 
    Private`componentsEdgesAndnuc[{2}, {5}] = {{}, {}, {}, {}, {{3}, {5}, {
      7}}}, Private`componentsEdgesAndnuc[{7}, {3}] = {{}, {}, {}, {}, {{4}, {
      6}, {8}, {10}}}, 
    Private`componentsEdgesAndnuc[{1, 0}, {6, 0}] = {{}, {}, {}, {}, {{1, 
      5}, {0, 7}}}, 
    Private`componentsEdgesAndnuc[{1, 1}, {0, 3}] = {{}, {}, {}, {}, {{3, 
      0}, {4, 1}, {1, 1}, {2, 2}}}, 
    Private`componentsEdgesAndnuc[{1, 1}, {6, 3}] = {{}, {}, {}, {}, {{4, 
      4}, {2, 5}, {5, 5}, {3, 6}, {3, 6}, {4, 7}, {1, 7}, {2, 8}}}, 
    Private`componentsEdgesAndnuc[{1, 2}, {3, 1}] = {{}, {}, {}, {}, {{2, 
      0}, {0, 1}, {3, 1}, {1, 2}, {1, 2}, {4, 2}, {2, 3}, {2, 3}, {0, 4}, {3, 
      4}, {1, 5}}}, 
    Private`componentsEdgesAndnuc[{1, 6}, {3, 1}] = {{}, {}, {}, {}, {{3, 
      0}, {6, 0}, {4, 1}, {4, 1}, {2, 2}, {7, 1}, {5, 2}, {5, 2}, {3, 3}, {8, 
      2}, {6, 3}, {6, 3}, {4, 4}, {7, 4}, {5, 5}}}, 
    Private`componentsEdgesAndnuc[{3, 1}, {1, 2}] = {{}, {}, {}, {}, {{2, 
      0}, {0, 1}, {3, 1}, {1, 2}, {4, 2}, {2, 3}, {1, 2}, {2, 3}, {3, 4}, {0, 
      4}, {1, 5}}}, 
    Private`componentsEdgesAndnuc[{3, 1}, {1, 3}] = {{}, {}, {}, {}, {{0, 
      0}, {3, 0}, {1, 1}, {4, 1}, {2, 2}, {5, 2}, {3, 3}, {1, 1}, {2, 2}, {3, 
      3}, {4, 4}, {0, 3}, {1, 4}, {2, 5}}}, 
    Private`componentsEdgesAndnuc[{7, 0}, {3, 0}] = {{}, {}, {}, {}, {{3, 
      4}, {2, 6}, {1, 8}, {0, 10}}}, 
    Private`componentsEdgesAndnuc[{0, 0, 1}, {1, 3, 1}] = {{}, {}, {}, {}, {{
      1, 3, 0}, {2, 3, 1}, {0, 4, 1}, {1, 2, 2}}}, 
    Private`componentsEdgesAndnuc[{0, 0, 1}, {2, 0, 0}] = {{}, {}, {}, {}, {{
      0, 0, 1}, {1, 0, 2}}}, 
    Private`componentsEdgesAndnuc[{0, 0, 2}, {0, 0, 0}] = {{}, {}, {}, {}, {{
      2, 0, 0}}}, 
    Private`componentsEdgesAndnuc[{0, 0, 2}, {3, 3, 2}] = {{}, {}, {}, {}, {{
      2, 3, 1}, {3, 3, 2}, {1, 4, 2}, {2, 2, 3}, {4, 3, 3}, {2, 4, 3}, {0, 5, 
      3}, {3, 2, 4}, {1, 3, 4}, {2, 1, 5}}}, 
    Private`componentsEdgesAndnuc[{0, 0, 3}, {3, 2, 1}] = {{}, {}, {}, {}, {{
      1, 2, 0}, {2, 2, 1}, {0, 3, 1}, {1, 1, 2}, {3, 2, 2}, {1, 3, 2}, {2, 1, 
      3}, {0, 2, 3}, {4, 2, 3}, {2, 3, 3}, {1, 0, 4}, {3, 1, 4}, {1, 2, 4}, {
      2, 0, 5}, {0, 1, 5}}}, 
    Private`componentsEdgesAndnuc[{0, 1, 1}, {0, 3, 0}] = {{}, {}, {}, {}, {{
      1, 2, 0}, {1, 4, 0}, {0, 1, 1}, {2, 2, 1}, {0, 3, 1}, {1, 1, 2}}}, 
    Private`componentsEdgesAndnuc[{0, 1, 2}, {0, 0, 3}] = {{}, {}, {}, {}, {{
      5, 1, 0}, {3, 2, 0}, {1, 3, 0}, {4, 0, 1}, {2, 1, 1}, {0, 2, 1}}}, 
    Private`componentsEdgesAndnuc[{0, 1, 2}, {0, 1, 2}] = {{}, {}, {}, {}, {{
      4, 0, 0}, {2, 1, 0}, {4, 2, 0}, {0, 2, 0}, {2, 3, 0}, {0, 4, 0}, {5, 0, 
      1}, {3, 1, 1}, {3, 1, 1}, {1, 2, 1}, {1, 2, 1}, {2, 0, 2}, {0, 1, 2}}}, 
    Private`componentsEdgesAndnuc[{0, 1, 2}, {0, 2, 0}] = {{}, {}, {}, {}, {{
      2, 1, 0}, {2, 3, 0}, {1, 0, 1}, {3, 1, 1}, {1, 2, 1}, {2, 0, 2}, {0, 1, 
      2}}}, Private`componentsEdgesAndnuc[{0, 1, 2}, {3, 1, 
      2}] = {{}, {}, {}, {}, {{3, 1, 0}, {1, 2, 0}, {2, 0, 1}, {4, 1, 1}, {2, 
      2, 1}, {2, 2, 1}, {0, 3, 1}, {3, 0, 2}, {3, 0, 2}, {1, 1, 2}, {1, 1, 
      2}, {5, 1, 2}, {3, 2, 2}, {3, 2, 2}, {1, 3, 2}, {1, 3, 2}, {4, 0, 3}, {
      4, 0, 3}, {2, 1, 3}, {2, 1, 3}, {2, 1, 3}, {4, 2, 3}, {0, 2, 3}, {0, 2, 
      3}, {2, 3, 3}, {0, 4, 3}, {1, 0, 4}, {5, 0, 4}, {3, 1, 4}, {3, 1, 4}, {
      1, 2, 4}, {1, 2, 4}, {2, 0, 5}, {0, 1, 5}}}, 
    Private`componentsEdgesAndnuc[{0, 1, 2}, {3, 3, 0}] = {{}, {}, {}, {}, {{
      1, 3, 0}, {0, 2, 1}, {2, 3, 1}, {0, 4, 1}, {1, 2, 2}, {1, 2, 2}, {3, 3, 
      2}, {1, 4, 2}, {0, 1, 3}, {2, 2, 3}, {2, 2, 3}, {0, 3, 3}, {2, 4, 3}, {
      1, 1, 4}, {1, 1, 4}, {3, 2, 4}, {1, 3, 4}, {0, 0, 5}, {2, 1, 5}, {0, 2, 
      5}, {1, 0, 6}}}, 
    Private`componentsEdgesAndnuc[{0, 1, 3}, {2, 3, 2}] = {{}, {}, {}, {}, {{
      3, 2, 0}, {1, 3, 0}, {5, 3, 0}, {3, 4, 0}, {3, 4, 0}, {1, 5, 0}, {1, 5, 
      0}, {2, 1, 1}, {4, 2, 1}, {4, 2, 1}, {2, 3, 1}, {2, 3, 1}, {2, 3, 1}, {
      6, 3, 1}, {4, 4, 1}, {0, 4, 1}, {0, 4, 1}, {4, 4, 1}, {2, 5, 1}, {2, 5, 
      1}, {0, 6, 1}, {3, 1, 2}, {3, 1, 2}, {1, 2, 2}, {1, 2, 2}, {5, 2, 2}, {
      5, 2, 2}, {3, 3, 2}, {3, 3, 2}, {3, 3, 2}, {1, 4, 2}, {5, 4, 2}, {1, 4, 
      2}, {1, 4, 2}, {3, 5, 2}, {1, 6, 2}, {2, 0, 3}, {4, 1, 3}, {4, 1, 3}, {
      2, 2, 3}, {2, 2, 3}, {2, 2, 3}, {6, 2, 3}, {4, 3, 3}, {0, 3, 3}, {0, 3, 
      3}, {4, 3, 3}, {2, 4, 3}, {2, 4, 3}, {0, 5, 3}, {3, 0, 4}, {3, 0, 4}, {
      1, 1, 4}, {1, 1, 4}, {5, 1, 4}, {3, 2, 4}, {3, 2, 4}, {1, 3, 4}, {1, 3, 
      4}, {4, 0, 5}, {2, 1, 5}, {2, 1, 5}, {0, 2, 5}, {1, 0, 6}}}, 
    Private`componentsEdgesAndnuc[{0, 1, 3}, {3, 3, 3}] = {{}, {}, {}, {}, {{
      3, 2, 0}, {5, 3, 0}, {3, 4, 0}, {3, 4, 0}, {1, 5, 0}, {4, 2, 1}, {4, 2, 
      1}, {2, 3, 1}, {2, 3, 1}, {6, 3, 1}, {4, 4, 1}, {4, 4, 1}, {2, 5, 1}, {
      2, 5, 1}, {0, 6, 1}, {3, 1, 2}, {5, 2, 2}, {5, 2, 2}, {3, 3, 2}, {3, 3, 
      2}, {3, 3, 2}, {7, 3, 2}, {5, 4, 2}, {1, 4, 2}, {1, 4, 2}, {5, 4, 2}, {
      3, 5, 2}, {3, 5, 2}, {1, 6, 2}, {1, 6, 2}, {4, 1, 3}, {4, 1, 3}, {2, 2, 
      3}, {2, 2, 3}, {6, 2, 3}, {6, 2, 3}, {4, 3, 3}, {4, 3, 3}, {4, 3, 3}, {
      2, 4, 3}, {6, 4, 3}, {2, 4, 3}, {2, 4, 3}, {4, 5, 3}, {0, 5, 3}, {0, 5, 
      3}, {2, 6, 3}, {0, 7, 3}, {3, 0, 4}, {5, 1, 4}, {5, 1, 4}, {3, 2, 4}, {
      3, 2, 4}, {3, 2, 4}, {7, 2, 4}, {5, 3, 4}, {1, 3, 4}, {1, 3, 4}, {5, 3, 
      4}, {3, 4, 4}, {3, 4, 4}, {1, 5, 4}, {1, 5, 4}, {4, 0, 5}, {4, 0, 5}, {
      2, 1, 5}, {2, 1, 5}, {6, 1, 5}, {4, 2, 5}, {4, 2, 5}, {2, 3, 5}, {2, 3, 
      5}, {0, 4, 5}, {5, 0, 6}, {3, 1, 6}, {3, 1, 6}, {1, 2, 6}, {2, 0, 7}}}, 
    Private`componentsEdgesAndnuc[{0, 2, 0}, {0, 1, 1}] = {{}, {}, {}, {}, {{
      1, 1, 0}, {1, 3, 0}, {0, 0, 1}, {2, 1, 1}, {0, 2, 1}, {1, 0, 2}}}, 
    Private`componentsEdgesAndnuc[{0, 2, 1}, {0, 1, 0}] = {{}, {}, {}, {}, {{
      1, 1, 0}, {1, 3, 0}, {2, 1, 1}, {0, 2, 1}}}, 
    Private`componentsEdgesAndnuc[{0, 2, 1}, {0, 1, 1}] = {{}, {}, {}, {}, {{
      2, 1, 0}, {0, 2, 0}, {2, 3, 0}, {0, 4, 0}, {1, 0, 1}, {3, 1, 1}, {1, 2, 
      1}, {1, 2, 1}, {2, 0, 2}, {0, 1, 2}}}, 
    Private`componentsEdgesAndnuc[{0, 2, 1}, {2, 1, 0}] = {{}, {}, {}, {}, 
      ReplaceAll[{
        Private`lambda[0, 0, 1], 
        Private`lambda[0, 0, 2], 
        Private`lambda[0, 0, 3], 
        Private`lambda[0, 0, 4]}, {
        ToRules[
         Reduce[
          And[
           ReplaceAll[
            And[
            Private`lambda[1, 0, 0] == 
             Private`j[1, 0, 4] + Private`j[1, 1, 3] + Private`j[1, 2, 2] + 
              Private`j[1, 3, 1] + Private`j[1, 4, 0], 
             Private`lambda[2, 0, 0] == 
             Private`j[2, 0, 3] + Private`j[2, 1, 2] + Private`j[2, 2, 1] + 
              Private`j[2, 3, 0], Private`lambda[3, 0, 0] == 
             Private`j[3, 0, 2] + Private`j[3, 1, 1] + Private`j[3, 2, 0], 
             Private`lambda[4, 0, 0] == 
             Private`j[4, 0, 1] + Private`j[4, 1, 0], Private`lambda[0, 1, 0] == 
             Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
              Private`j[3, 1, 1] + Private`j[4, 1, 0], 
             Private`lambda[0, 2, 0] == 
             Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] + 
              Private`j[3, 2, 0], Private`lambda[0, 3, 0] == 
             Private`j[0, 3, 2] + Private`j[1, 3, 1] + Private`j[2, 3, 0], 
             Private`lambda[0, 4, 0] == 
             Private`j[0, 4, 1] + Private`j[1, 4, 0], Private`lambda[0, 0, 1] == 
             Private`j[0, 4, 1] + Private`j[1, 3, 1] + Private`j[2, 2, 1] + 
              Private`j[3, 1, 1] + Private`j[4, 0, 1], 
             Private`lambda[0, 0, 2] == 
             Private`j[0, 3, 2] + Private`j[1, 2, 2] + Private`j[2, 1, 2] + 
              Private`j[3, 0, 2], Private`lambda[0, 0, 3] == 
             Private`j[0, 2, 3] + Private`j[1, 1, 3] + Private`j[2, 0, 3], 
             Private`lambda[0, 0, 4] == 
             Private`j[0, 1, 4] + Private`j[1, 0, 4]], {
             MapThread[Rule, {{
                Private`lambda[1, 0, 0], 
                Private`lambda[2, 0, 0], 
                Private`lambda[3, 0, 0], 
                Private`lambda[4, 0, 0]}, {0, 2, 1}}], 
             MapThread[Rule, {{
                Private`lambda[0, 1, 0], 
                Private`lambda[0, 2, 0], 
                Private`lambda[0, 3, 0], 
                Private`lambda[0, 4, 0]}, {2, 1, 0}}]}], Private`j[1, 4, 0] >= 
           0, Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 2, 2] + Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 1, 3] + Private`j[1, 2, 2] + Private`j[1, 3, 1] + 
            Private`j[1, 4, 0] >= 0, Private`j[2, 3, 0] >= 0, 
           Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[2, 1, 2] + Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[3, 2, 0] >= 0, Private`j[3, 1, 1] + Private`j[3, 2, 0] >= 
           0, Private`j[4, 1, 0] >= 0, Private`j[0, 1, 4] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
            Private`j[3, 1, 1] >= 0, Private`j[0, 2, 3] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] >= 0, 
           Private`j[0, 3, 2] >= 0, Private`j[0, 3, 2] + Private`j[1, 3, 1] >= 
           0, Private`j[0, 4, 1] >= 0, Private`j[4, 0, 1] >= 0, 
           Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[2, 2, 1] + Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[1, 3, 1] + Private`j[2, 2, 1] + Private`j[3, 1, 1] + 
            Private`j[4, 0, 1] >= 0, Private`j[3, 0, 2] >= 0, 
           Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[1, 2, 2] + Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[2, 0, 3] >= 0, Private`j[1, 1, 3] + Private`j[2, 0, 3] >= 
           0, Private`j[1, 0, 4] >= 0], {
           Private`j[0, 1, 4], 
           Private`j[0, 2, 3], 
           Private`j[0, 3, 2], 
           Private`j[0, 4, 1], 
           Private`j[1, 0, 4], 
           Private`j[1, 1, 3], 
           Private`j[1, 2, 2], 
           Private`j[1, 3, 1], 
           Private`j[1, 4, 0], 
           Private`j[2, 0, 3], 
           Private`j[2, 1, 2], 
           Private`j[2, 2, 1], 
           Private`j[2, 3, 0], 
           Private`j[3, 0, 2], 
           Private`j[3, 1, 1], 
           Private`j[3, 2, 0], 
           Private`j[4, 0, 1], 
           Private`j[4, 1, 0]}, Integers]]}]}, 
    Private`componentsEdgesAndnuc[{0, 2, 1}, {2, 1, 2}] = {{}, {}, {}, {}, {{
      3, 0, 0}, {1, 1, 0}, {5, 1, 0}, {3, 2, 0}, {3, 2, 0}, {1, 3, 0}, {1, 3, 
      0}, {4, 0, 1}, {4, 0, 1}, {2, 1, 1}, {2, 1, 1}, {2, 1, 1}, {4, 2, 1}, {
      2, 3, 1}, {0, 2, 1}, {0, 2, 1}, {2, 3, 1}, {0, 4, 1}, {1, 0, 2}, {5, 0, 
      2}, {3, 1, 2}, {3, 1, 2}, {3, 1, 2}, {1, 2, 2}, {1, 2, 2}, {3, 3, 2}, {
      1, 2, 2}, {1, 4, 2}, {2, 0, 3}, {2, 0, 3}, {4, 1, 3}, {2, 2, 3}, {0, 1, 
      3}, {0, 1, 3}, {2, 2, 3}, {0, 3, 3}, {3, 0, 4}, {1, 1, 4}, {1, 1, 4}, {
      0, 0, 5}}}, 
    Private`componentsEdgesAndnuc[{0, 2, 1}, {3, 3, 1}] = {{}, {}, {}, {}, {{
      3, 3, 0}, {1, 4, 0}, {2, 2, 1}, {0, 3, 1}, {4, 3, 1}, {2, 4, 1}, {2, 4, 
      1}, {0, 5, 1}, {1, 1, 2}, {3, 2, 2}, {3, 2, 2}, {1, 3, 2}, {1, 3, 2}, {
      1, 3, 2}, {3, 4, 2}, {1, 5, 2}, {1, 5, 2}, {2, 1, 3}, {2, 1, 3}, {0, 2, 
      3}, {0, 2, 3}, {4, 2, 3}, {2, 3, 3}, {2, 3, 3}, {2, 3, 3}, {0, 4, 3}, {
      0, 4, 3}, {2, 5, 3}, {0, 6, 3}, {1, 0, 4}, {3, 1, 4}, {3, 1, 4}, {1, 2, 
      4}, {1, 2, 4}, {1, 2, 4}, {3, 3, 4}, {1, 4, 4}, {1, 4, 4}, {2, 0, 5}, {
      0, 1, 5}, {4, 1, 5}, {2, 2, 5}, {2, 2, 5}, {0, 3, 5}, {3, 0, 6}, {1, 1, 
      6}}}, 
    Private`componentsEdgesAndnuc[{0, 2, 2}, {1, 2, 3}] = {{}, {}, {}, {}, {{
      4, 0, 0}, {2, 1, 0}, {6, 1, 0}, {4, 2, 0}, {4, 2, 0}, {2, 3, 0}, {6, 3, 
      0}, {4, 4, 0}, {2, 3, 0}, {4, 4, 0}, {2, 5, 0}, {0, 4, 0}, {2, 5, 0}, {
      0, 6, 0}, {5, 0, 1}, {5, 0, 1}, {3, 1, 1}, {3, 1, 1}, {3, 1, 1}, {7, 1, 
      1}, {5, 2, 1}, {5, 2, 1}, {3, 3, 1}, {1, 2, 1}, {1, 2, 1}, {5, 2, 1}, {
      3, 3, 1}, {3, 3, 1}, {5, 4, 1}, {3, 3, 1}, {1, 4, 1}, {1, 4, 1}, {3, 5, 
      1}, {1, 4, 1}, {1, 6, 1}, {2, 0, 2}, {6, 0, 2}, {6, 0, 2}, {4, 1, 2}, {
      4, 1, 2}, {4, 1, 2}, {4, 1, 2}, {2, 2, 2}, {2, 2, 2}, {6, 2, 2}, {4, 3, 
      2}, {2, 2, 2}, {2, 2, 2}, {4, 3, 2}, {2, 4, 2}, {0, 3, 2}, {0, 3, 2}, {
      2, 4, 2}, {0, 5, 2}, {3, 0, 3}, {3, 0, 3}, {7, 0, 3}, {5, 1, 3}, {3, 2, 
      3}, {1, 1, 3}, {1, 1, 3}, {5, 1, 3}, {3, 2, 3}, {3, 2, 3}, {1, 3, 3}, {
      1, 3, 3}, {4, 0, 4}, {2, 1, 4}, {2, 1, 4}, {0, 2, 4}, {1, 0, 5}}}, 
    Private`componentsEdgesAndnuc[{0, 2, 3}, {0, 0, 1}] = {{}, {}, {}, {}, {{
      4, 2, 0}, {2, 3, 0}, {3, 1, 1}}}, 
    Private`componentsEdgesAndnuc[{0, 2, 3}, {2, 0, 1}] = {{}, {}, {}, {}, {{
      6, 0, 0}, {4, 1, 0}, {2, 2, 0}, {4, 1, 0}, {2, 2, 0}, {0, 3, 0}, {3, 0, 
      1}, {1, 1, 1}, {5, 1, 1}, {3, 2, 1}, {3, 2, 1}, {1, 3, 1}, {4, 0, 2}, {
      2, 1, 2}, {4, 2, 2}, {2, 3, 2}, {3, 1, 3}}}, 
    Private`componentsEdgesAndnuc[{0, 2, 3}, {2, 2, 1}] = {{}, {}, {}, {}, {{
      2, 0, 0}, {0, 1, 0}, {4, 1, 0}, {2, 2, 0}, {2, 2, 0}, {0, 3, 0}, {6, 2, 
      0}, {4, 3, 0}, {2, 4, 0}, {4, 3, 0}, {2, 4, 0}, {0, 5, 0}, {3, 0, 1}, {
      3, 0, 1}, {1, 1, 1}, {1, 1, 1}, {1, 1, 1}, {5, 1, 1}, {3, 2, 1}, {5, 1, 
      1}, {3, 2, 1}, {1, 3, 1}, {3, 2, 1}, {1, 3, 1}, {3, 2, 1}, {1, 3, 1}, {
      5, 3, 1}, {3, 4, 1}, {3, 4, 1}, {1, 5, 1}, {0, 0, 2}, {4, 0, 2}, {4, 0, 
      2}, {2, 1, 2}, {4, 0, 2}, {2, 1, 2}, {2, 1, 2}, {2, 1, 2}, {0, 2, 2}, {
      2, 1, 2}, {0, 2, 2}, {6, 1, 2}, {4, 2, 2}, {4, 2, 2}, {2, 3, 2}, {4, 2, 
      2}, {2, 3, 2}, {2, 3, 2}, {0, 4, 2}, {4, 4, 2}, {2, 5, 2}, {1, 0, 3}, {
      1, 0, 3}, {5, 0, 3}, {5, 0, 3}, {3, 1, 3}, {3, 1, 3}, {1, 2, 3}, {3, 1, 
      3}, {3, 1, 3}, {1, 2, 3}, {1, 2, 3}, {5, 2, 3}, {3, 3, 3}, {3, 3, 3}, {
      1, 4, 3}, {2, 0, 4}, {2, 0, 4}, {0, 1, 4}, {6, 0, 4}, {4, 1, 4}, {2, 2, 
      4}, {4, 1, 4}, {2, 2, 4}, {0, 3, 4}, {3, 0, 5}, {1, 1, 5}}}, 
    Private`componentsEdgesAndnuc[{0, 3, 0}, {0, 0, 3}] = {{}, {}, {}, {}, {{
      3, 3, 0}, {2, 2, 1}, {1, 1, 2}, {0, 0, 3}}}, 
    Private`componentsEdgesAndnuc[{0, 3, 0}, {0, 1, 3}] = {{}, {}, {}, {}, {{
      3, 2, 0}, {3, 4, 0}, {2, 1, 1}, {4, 2, 1}, {2, 3, 1}, {1, 0, 2}, {3, 1, 
      2}, {1, 2, 2}, {2, 0, 3}, {0, 1, 3}}}, 
    Private`componentsEdgesAndnuc[{0, 3, 0}, {3, 2, 2}] = {{}, {}, {}, {}, {{
      5, 2, 0}, {3, 3, 0}, {1, 4, 0}, {4, 1, 1}, {2, 2, 1}, {4, 3, 1}, {0, 3, 
      1}, {2, 4, 1}, {0, 5, 1}, {3, 0, 2}, {1, 1, 2}, {5, 1, 2}, {3, 2, 2}, {
      3, 2, 2}, {1, 3, 2}, {3, 4, 2}, {1, 3, 2}, {1, 5, 2}, {4, 0, 3}, {2, 1, 
      3}, {2, 1, 3}, {4, 2, 3}, {2, 3, 3}, {0, 2, 3}, {2, 3, 3}, {2, 5, 3}, {
      0, 4, 3}, {1, 0, 4}, {5, 0, 4}, {3, 1, 4}, {3, 1, 4}, {1, 2, 4}, {3, 3, 
      4}, {1, 2, 4}, {1, 4, 4}, {2, 0, 5}, {4, 1, 5}, {0, 1, 5}, {2, 2, 5}, {
      0, 3, 5}, {3, 0, 6}, {1, 1, 6}}}, 
    Private`componentsEdgesAndnuc[{0, 3, 1}, {1, 0, 2}] = {{}, {}, {}, {}, 
      ReplaceAll[{
        Private`lambda[0, 0, 1], 
        Private`lambda[0, 0, 2], 
        Private`lambda[0, 0, 3], 
        Private`lambda[0, 0, 4]}, {
        ToRules[
         Reduce[
          And[
           ReplaceAll[
            And[
            Private`lambda[1, 0, 0] == 
             Private`j[1, 0, 4] + Private`j[1, 1, 3] + Private`j[1, 2, 2] + 
              Private`j[1, 3, 1] + Private`j[1, 4, 0], 
             Private`lambda[2, 0, 0] == 
             Private`j[2, 0, 3] + Private`j[2, 1, 2] + Private`j[2, 2, 1] + 
              Private`j[2, 3, 0], Private`lambda[3, 0, 0] == 
             Private`j[3, 0, 2] + Private`j[3, 1, 1] + Private`j[3, 2, 0], 
             Private`lambda[4, 0, 0] == 
             Private`j[4, 0, 1] + Private`j[4, 1, 0], Private`lambda[0, 1, 0] == 
             Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
              Private`j[3, 1, 1] + Private`j[4, 1, 0], 
             Private`lambda[0, 2, 0] == 
             Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] + 
              Private`j[3, 2, 0], Private`lambda[0, 3, 0] == 
             Private`j[0, 3, 2] + Private`j[1, 3, 1] + Private`j[2, 3, 0], 
             Private`lambda[0, 4, 0] == 
             Private`j[0, 4, 1] + Private`j[1, 4, 0], Private`lambda[0, 0, 1] == 
             Private`j[0, 4, 1] + Private`j[1, 3, 1] + Private`j[2, 2, 1] + 
              Private`j[3, 1, 1] + Private`j[4, 0, 1], 
             Private`lambda[0, 0, 2] == 
             Private`j[0, 3, 2] + Private`j[1, 2, 2] + Private`j[2, 1, 2] + 
              Private`j[3, 0, 2], Private`lambda[0, 0, 3] == 
             Private`j[0, 2, 3] + Private`j[1, 1, 3] + Private`j[2, 0, 3], 
             Private`lambda[0, 0, 4] == 
             Private`j[0, 1, 4] + Private`j[1, 0, 4]], {
             MapThread[Rule, {{
                Private`lambda[1, 0, 0], 
                Private`lambda[2, 0, 0], 
                Private`lambda[3, 0, 0], 
                Private`lambda[4, 0, 0]}, {0, 3, 1}}], 
             MapThread[Rule, {{
                Private`lambda[0, 1, 0], 
                Private`lambda[0, 2, 0], 
                Private`lambda[0, 3, 0], 
                Private`lambda[0, 4, 0]}, {1, 0, 2}}]}], Private`j[1, 4, 0] >= 
           0, Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 2, 2] + Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 1, 3] + Private`j[1, 2, 2] + Private`j[1, 3, 1] + 
            Private`j[1, 4, 0] >= 0, Private`j[2, 3, 0] >= 0, 
           Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[2, 1, 2] + Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[3, 2, 0] >= 0, Private`j[3, 1, 1] + Private`j[3, 2, 0] >= 
           0, Private`j[4, 1, 0] >= 0, Private`j[0, 1, 4] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
            Private`j[3, 1, 1] >= 0, Private`j[0, 2, 3] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] >= 0, 
           Private`j[0, 3, 2] >= 0, Private`j[0, 3, 2] + Private`j[1, 3, 1] >= 
           0, Private`j[0, 4, 1] >= 0, Private`j[4, 0, 1] >= 0, 
           Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[2, 2, 1] + Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[1, 3, 1] + Private`j[2, 2, 1] + Private`j[3, 1, 1] + 
            Private`j[4, 0, 1] >= 0, Private`j[3, 0, 2] >= 0, 
           Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[1, 2, 2] + Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[2, 0, 3] >= 0, Private`j[1, 1, 3] + Private`j[2, 0, 3] >= 
           0, Private`j[1, 0, 4] >= 0], {
           Private`j[0, 1, 4], 
           Private`j[0, 2, 3], 
           Private`j[0, 3, 2], 
           Private`j[0, 4, 1], 
           Private`j[1, 0, 4], 
           Private`j[1, 1, 3], 
           Private`j[1, 2, 2], 
           Private`j[1, 3, 1], 
           Private`j[1, 4, 0], 
           Private`j[2, 0, 3], 
           Private`j[2, 1, 2], 
           Private`j[2, 2, 1], 
           Private`j[2, 3, 0], 
           Private`j[3, 0, 2], 
           Private`j[3, 1, 1], 
           Private`j[3, 2, 0], 
           Private`j[4, 0, 1], 
           Private`j[4, 1, 0]}, Integers]]}]}, 
    Private`componentsEdgesAndnuc[{0, 3, 2}, {0, 0, 2}] = {{}, {}, {}, {}, {{
      4, 3, 0}, {2, 4, 0}, {0, 5, 0}, {3, 2, 1}, {1, 3, 1}, {2, 1, 2}}}, 
    Private`componentsEdgesAndnuc[{0, 3, 2}, {2, 3, 3}] = {{}, {}, {}, {}, 
      ReplaceAll[{
        Private`lambda[0, 0, 1], 
        Private`lambda[0, 0, 2], 
        Private`lambda[0, 0, 3], 
        Private`lambda[0, 0, 4]}, {
        ToRules[
         Reduce[
          And[
           ReplaceAll[
            And[
            Private`lambda[1, 0, 0] == 
             Private`j[1, 0, 4] + Private`j[1, 1, 3] + Private`j[1, 2, 2] + 
              Private`j[1, 3, 1] + Private`j[1, 4, 0], 
             Private`lambda[2, 0, 0] == 
             Private`j[2, 0, 3] + Private`j[2, 1, 2] + Private`j[2, 2, 1] + 
              Private`j[2, 3, 0], Private`lambda[3, 0, 0] == 
             Private`j[3, 0, 2] + Private`j[3, 1, 1] + Private`j[3, 2, 0], 
             Private`lambda[4, 0, 0] == 
             Private`j[4, 0, 1] + Private`j[4, 1, 0], Private`lambda[0, 1, 0] == 
             Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
              Private`j[3, 1, 1] + Private`j[4, 1, 0], 
             Private`lambda[0, 2, 0] == 
             Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] + 
              Private`j[3, 2, 0], Private`lambda[0, 3, 0] == 
             Private`j[0, 3, 2] + Private`j[1, 3, 1] + Private`j[2, 3, 0], 
             Private`lambda[0, 4, 0] == 
             Private`j[0, 4, 1] + Private`j[1, 4, 0], Private`lambda[0, 0, 1] == 
             Private`j[0, 4, 1] + Private`j[1, 3, 1] + Private`j[2, 2, 1] + 
              Private`j[3, 1, 1] + Private`j[4, 0, 1], 
             Private`lambda[0, 0, 2] == 
             Private`j[0, 3, 2] + Private`j[1, 2, 2] + Private`j[2, 1, 2] + 
              Private`j[3, 0, 2], Private`lambda[0, 0, 3] == 
             Private`j[0, 2, 3] + Private`j[1, 1, 3] + Private`j[2, 0, 3], 
             Private`lambda[0, 0, 4] == 
             Private`j[0, 1, 4] + Private`j[1, 0, 4]], {
             MapThread[Rule, {{
                Private`lambda[1, 0, 0], 
                Private`lambda[2, 0, 0], 
                Private`lambda[3, 0, 0], 
                Private`lambda[4, 0, 0]}, {0, 3, 2}}], 
             MapThread[Rule, {{
                Private`lambda[0, 1, 0], 
                Private`lambda[0, 2, 0], 
                Private`lambda[0, 3, 0], 
                Private`lambda[0, 4, 0]}, {2, 3, 3}}]}], Private`j[1, 4, 0] >= 
           0, Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 2, 2] + Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 1, 3] + Private`j[1, 2, 2] + Private`j[1, 3, 1] + 
            Private`j[1, 4, 0] >= 0, Private`j[2, 3, 0] >= 0, 
           Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[2, 1, 2] + Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[3, 2, 0] >= 0, Private`j[3, 1, 1] + Private`j[3, 2, 0] >= 
           0, Private`j[4, 1, 0] >= 0, Private`j[0, 1, 4] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
            Private`j[3, 1, 1] >= 0, Private`j[0, 2, 3] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] >= 0, 
           Private`j[0, 3, 2] >= 0, Private`j[0, 3, 2] + Private`j[1, 3, 1] >= 
           0, Private`j[0, 4, 1] >= 0, Private`j[4, 0, 1] >= 0, 
           Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[2, 2, 1] + Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[1, 3, 1] + Private`j[2, 2, 1] + Private`j[3, 1, 1] + 
            Private`j[4, 0, 1] >= 0, Private`j[3, 0, 2] >= 0, 
           Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[1, 2, 2] + Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[2, 0, 3] >= 0, Private`j[1, 1, 3] + Private`j[2, 0, 3] >= 
           0, Private`j[1, 0, 4] >= 0], {
           Private`j[0, 1, 4], 
           Private`j[0, 2, 3], 
           Private`j[0, 3, 2], 
           Private`j[0, 4, 1], 
           Private`j[1, 0, 4], 
           Private`j[1, 1, 3], 
           Private`j[1, 2, 2], 
           Private`j[1, 3, 1], 
           Private`j[1, 4, 0], 
           Private`j[2, 0, 3], 
           Private`j[2, 1, 2], 
           Private`j[2, 2, 1], 
           Private`j[2, 3, 0], 
           Private`j[3, 0, 2], 
           Private`j[3, 1, 1], 
           Private`j[3, 2, 0], 
           Private`j[4, 0, 1], 
           Private`j[4, 1, 0]}, Integers]]}]}, 
    Private`componentsEdgesAndnuc[{0, 3, 3}, {0, 1, 2}] = {{}, {}, {}, {}, {{
      5, 2, 0}, {3, 3, 0}, {5, 4, 0}, {1, 4, 0}, {3, 5, 0}, {1, 6, 0}, {4, 1, 
      1}, {6, 2, 1}, {4, 3, 1}, {2, 2, 1}, {4, 3, 1}, {2, 4, 1}, {2, 4, 1}, {
      0, 5, 1}, {3, 0, 2}, {5, 1, 2}, {3, 2, 2}, {3, 2, 2}, {1, 3, 2}, {4, 0, 
      3}, {2, 1, 3}}}, 
    Private`componentsEdgesAndnuc[{0, 3, 3}, {0, 3, 2}] = {{}, {}, {}, {}, {{
      5, 0, 0}, {3, 1, 0}, {5, 2, 0}, {1, 2, 0}, {3, 3, 0}, {5, 4, 0}, {1, 4, 
      0}, {3, 5, 0}, {5, 6, 0}, {1, 6, 0}, {3, 7, 0}, {1, 8, 0}, {6, 0, 1}, {
      4, 1, 1}, {4, 1, 1}, {2, 2, 1}, {6, 2, 1}, {4, 3, 1}, {2, 2, 1}, {0, 3, 
      1}, {4, 3, 1}, {2, 4, 1}, {6, 4, 1}, {4, 5, 1}, {2, 4, 1}, {0, 5, 1}, {
      4, 5, 1}, {2, 6, 1}, {2, 6, 1}, {0, 7, 1}, {3, 0, 2}, {7, 0, 2}, {5, 1, 
      2}, {3, 2, 2}, {1, 1, 2}, {5, 1, 2}, {3, 2, 2}, {1, 3, 2}, {7, 2, 2}, {
      5, 3, 2}, {3, 4, 2}, {3, 2, 2}, {1, 3, 2}, {5, 3, 2}, {3, 4, 2}, {1, 5, 
      2}, {3, 4, 2}, {1, 5, 2}, {4, 0, 3}, {2, 1, 3}, {8, 0, 3}, {6, 1, 3}, {
      4, 2, 3}, {2, 3, 3}, {2, 1, 3}, {0, 2, 3}, {6, 1, 3}, {4, 2, 3}, {2, 3, 
      3}, {4, 2, 3}, {2, 3, 3}, {0, 4, 3}, {1, 0, 4}, {5, 0, 4}, {3, 1, 4}, {
      1, 2, 4}, {3, 1, 4}, {1, 2, 4}, {2, 0, 5}, {0, 1, 5}}}, 
    Private`componentsEdgesAndnuc[{0, 3, 3}, {1, 0, 3}] = {{}, {}, {}, {}, {{
      7, 2, 0}, {5, 3, 0}, {5, 3, 0}, {3, 4, 0}, {3, 4, 0}, {1, 5, 0}, {1, 5, 
      0}, {6, 1, 1}, {4, 2, 1}, {6, 3, 1}, {4, 2, 1}, {2, 3, 1}, {4, 4, 1}, {
      2, 3, 1}, {0, 4, 1}, {2, 5, 1}, {0, 6, 1}, {5, 0, 2}, {3, 1, 2}, {5, 2, 
      2}, {3, 1, 2}, {1, 2, 2}, {3, 3, 2}, {1, 4, 2}, {2, 0, 3}, {4, 1, 3}, {
      2, 2, 3}, {3, 0, 4}}}, 
    Private`componentsEdgesAndnuc[{0, 3, 3}, {1, 3, 1}] = {{}, {}, {}, {}, {{
      3, 0, 0}, {1, 1, 0}, {5, 1, 0}, {3, 2, 0}, {3, 2, 0}, {1, 3, 0}, {5, 3, 
      0}, {3, 4, 0}, {3, 4, 0}, {1, 5, 0}, {5, 5, 0}, {3, 6, 0}, {3, 6, 0}, {
      1, 7, 0}, {4, 0, 1}, {4, 0, 1}, {2, 1, 1}, {2, 1, 1}, {2, 1, 1}, {0, 2, 
      1}, {6, 1, 1}, {4, 2, 1}, {4, 2, 1}, {2, 3, 1}, {4, 2, 1}, {2, 3, 1}, {
      2, 3, 1}, {0, 4, 1}, {6, 3, 1}, {4, 4, 1}, {4, 4, 1}, {2, 5, 1}, {4, 4, 
      1}, {2, 5, 1}, {2, 5, 1}, {0, 6, 1}, {4, 6, 1}, {2, 7, 1}, {1, 0, 2}, {
      5, 0, 2}, {5, 0, 2}, {3, 1, 2}, {3, 1, 2}, {1, 2, 2}, {3, 1, 2}, {3, 1, 
      2}, {1, 2, 2}, {1, 2, 2}, {7, 1, 2}, {5, 2, 2}, {5, 2, 2}, {3, 3, 2}, {
      3, 3, 2}, {1, 4, 2}, {5, 2, 2}, {3, 3, 2}, {3, 3, 2}, {1, 4, 2}, {1, 4, 
      2}, {5, 4, 2}, {3, 5, 2}, {3, 5, 2}, {1, 6, 2}, {2, 0, 3}, {2, 0, 3}, {
      0, 1, 3}, {6, 0, 3}, {6, 0, 3}, {4, 1, 3}, {4, 1, 3}, {2, 2, 3}, {2, 2, 
      3}, {4, 1, 3}, {4, 1, 3}, {2, 2, 3}, {2, 2, 3}, {0, 3, 3}, {0, 3, 3}, {
      6, 2, 3}, {4, 3, 3}, {2, 4, 3}, {4, 3, 3}, {2, 4, 3}, {0, 5, 3}, {3, 0, 
      4}, {3, 0, 4}, {1, 1, 4}, {1, 1, 4}, {7, 0, 4}, {5, 1, 4}, {3, 2, 4}, {
      1, 3, 4}, {5, 1, 4}, {3, 2, 4}, {1, 3, 4}, {4, 0, 5}, {2, 1, 5}, {0, 2, 
      5}}}, Private`componentsEdgesAndnuc[{1, 0, 0}, {0, 0, 
      1}] = {{}, {}, {}, {}, {{0, 0, 0}, {1, 0, 1}}}, 
    Private`componentsEdgesAndnuc[{1, 0, 0}, {1, 1, 2}] = {{}, {}, {}, {}, {{
      2, 2, 0}, {3, 0, 1}, {1, 1, 1}, {2, 1, 2}}}, 
    Private`componentsEdgesAndnuc[{1, 0, 0}, {1, 3, 3}] = {{}, {}, {}, {}, {{
      3, 4, 0}, {4, 2, 1}, {2, 3, 1}, {3, 3, 2}}}, 
    Private`componentsEdgesAndnuc[{1, 0, 0}, {3, 1, 0}] = {{}, {}, {}, {}, {{
      0, 2, 2}, {1, 0, 3}, {0, 1, 4}}}, 
    Private`componentsEdgesAndnuc[{1, 0, 0}, {3, 1, 3}] = {{}, {}, {}, {}, {{
      3, 2, 2}, {4, 0, 3}, {2, 1, 3}, {3, 1, 4}}}, 
    Private`componentsEdgesAndnuc[{1, 0, 0}, {3, 2, 2}] = {{}, {}, {}, {}, {{
      2, 3, 2}, {3, 1, 3}, {1, 2, 3}, {2, 2, 4}}}, 
    Private`componentsEdgesAndnuc[{1, 0, 1}, {1, 2, 1}] = {{}, {}, {}, {}, {{
      2, 1, 0}, {0, 2, 0}, {2, 3, 0}, {1, 2, 1}, {0, 4, 0}, {3, 1, 1}, {1, 2, 
      1}, {1, 2, 1}, {2, 2, 2}, {0, 3, 2}, {2, 0, 2}, {0, 1, 2}, {1, 1, 3}}}, 
    Private`componentsEdgesAndnuc[{1, 0, 2}, {2, 1, 0}] = {{}, {}, {}, {}, {{
      1, 0, 0}, {1, 2, 0}, {0, 1, 1}, {2, 0, 1}, {0, 1, 1}, {2, 2, 1}, {1, 1, 
      2}, {3, 0, 2}, {1, 1, 2}, {0, 0, 3}, {2, 1, 3}, {1, 0, 4}}}, 
    Private`componentsEdgesAndnuc[{1, 0, 2}, {2, 1, 1}] = {{}, {}, {}, {}, {{
      2, 0, 0}, {0, 1, 0}, {2, 2, 0}, {1, 1, 1}, {0, 3, 0}, {3, 0, 1}, {1, 1, 
      1}, {1, 1, 1}, {3, 2, 1}, {2, 1, 2}, {1, 3, 1}, {0, 2, 2}, {0, 0, 2}, {
      4, 0, 2}, {2, 1, 2}, {1, 0, 3}, {2, 1, 2}, {0, 2, 2}, {3, 1, 3}, {1, 2, 
      3}, {1, 0, 3}, {2, 0, 4}, {0, 1, 4}}}, 
    Private`componentsEdgesAndnuc[{1, 0, 2}, {3, 1, 3}] = {{}, {}, {}, {}, {{
      3, 2, 0}, {4, 0, 1}, {2, 1, 1}, {4, 2, 1}, {3, 1, 2}, {2, 3, 1}, {5, 0, 
      2}, {3, 1, 2}, {3, 1, 2}, {5, 2, 2}, {4, 1, 3}, {1, 2, 2}, {3, 3, 2}, {
      2, 2, 3}, {1, 4, 2}, {2, 0, 3}, {6, 0, 3}, {4, 1, 3}, {3, 0, 4}, {4, 1, 
      3}, {2, 2, 3}, {5, 1, 4}, {2, 2, 3}, {3, 2, 4}, {0, 3, 3}, {1, 3, 4}, {
      3, 0, 4}, {4, 0, 5}, {1, 1, 4}, {2, 1, 5}}}, 
    Private`componentsEdgesAndnuc[{1, 0, 3}, {0, 2, 3}] = {{}, {}, {}, {}, 
      ReplaceAll[{
        Private`lambda[0, 0, 1], 
        Private`lambda[0, 0, 2], 
        Private`lambda[0, 0, 3], 
        Private`lambda[0, 0, 4]}, {
        ToRules[
         Reduce[
          And[
           ReplaceAll[
            And[
            Private`lambda[1, 0, 0] == 
             Private`j[1, 0, 4] + Private`j[1, 1, 3] + Private`j[1, 2, 2] + 
              Private`j[1, 3, 1] + Private`j[1, 4, 0], 
             Private`lambda[2, 0, 0] == 
             Private`j[2, 0, 3] + Private`j[2, 1, 2] + Private`j[2, 2, 1] + 
              Private`j[2, 3, 0], Private`lambda[3, 0, 0] == 
             Private`j[3, 0, 2] + Private`j[3, 1, 1] + Private`j[3, 2, 0], 
             Private`lambda[4, 0, 0] == 
             Private`j[4, 0, 1] + Private`j[4, 1, 0], Private`lambda[0, 1, 0] == 
             Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
              Private`j[3, 1, 1] + Private`j[4, 1, 0], 
             Private`lambda[0, 2, 0] == 
             Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] + 
              Private`j[3, 2, 0], Private`lambda[0, 3, 0] == 
             Private`j[0, 3, 2] + Private`j[1, 3, 1] + Private`j[2, 3, 0], 
             Private`lambda[0, 4, 0] == 
             Private`j[0, 4, 1] + Private`j[1, 4, 0], Private`lambda[0, 0, 1] == 
             Private`j[0, 4, 1] + Private`j[1, 3, 1] + Private`j[2, 2, 1] + 
              Private`j[3, 1, 1] + Private`j[4, 0, 1], 
             Private`lambda[0, 0, 2] == 
             Private`j[0, 3, 2] + Private`j[1, 2, 2] + Private`j[2, 1, 2] + 
              Private`j[3, 0, 2], Private`lambda[0, 0, 3] == 
             Private`j[0, 2, 3] + Private`j[1, 1, 3] + Private`j[2, 0, 3], 
             Private`lambda[0, 0, 4] == 
             Private`j[0, 1, 4] + Private`j[1, 0, 4]], {
             MapThread[Rule, {{
                Private`lambda[1, 0, 0], 
                Private`lambda[2, 0, 0], 
                Private`lambda[3, 0, 0], 
                Private`lambda[4, 0, 0]}, {1, 0, 3}}], 
             MapThread[Rule, {{
                Private`lambda[0, 1, 0], 
                Private`lambda[0, 2, 0], 
                Private`lambda[0, 3, 0], 
                Private`lambda[0, 4, 0]}, {0, 2, 3}}]}], Private`j[1, 4, 0] >= 
           0, Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 2, 2] + Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 1, 3] + Private`j[1, 2, 2] + Private`j[1, 3, 1] + 
            Private`j[1, 4, 0] >= 0, Private`j[2, 3, 0] >= 0, 
           Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[2, 1, 2] + Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[3, 2, 0] >= 0, Private`j[3, 1, 1] + Private`j[3, 2, 0] >= 
           0, Private`j[4, 1, 0] >= 0, Private`j[0, 1, 4] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
            Private`j[3, 1, 1] >= 0, Private`j[0, 2, 3] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] >= 0, 
           Private`j[0, 3, 2] >= 0, Private`j[0, 3, 2] + Private`j[1, 3, 1] >= 
           0, Private`j[0, 4, 1] >= 0, Private`j[4, 0, 1] >= 0, 
           Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[2, 2, 1] + Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[1, 3, 1] + Private`j[2, 2, 1] + Private`j[3, 1, 1] + 
            Private`j[4, 0, 1] >= 0, Private`j[3, 0, 2] >= 0, 
           Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[1, 2, 2] + Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[2, 0, 3] >= 0, Private`j[1, 1, 3] + Private`j[2, 0, 3] >= 
           0, Private`j[1, 0, 4] >= 0], {
           Private`j[0, 1, 4], 
           Private`j[0, 2, 3], 
           Private`j[0, 3, 2], 
           Private`j[0, 4, 1], 
           Private`j[1, 0, 4], 
           Private`j[1, 1, 3], 
           Private`j[1, 2, 2], 
           Private`j[1, 3, 1], 
           Private`j[1, 4, 0], 
           Private`j[2, 0, 3], 
           Private`j[2, 1, 2], 
           Private`j[2, 2, 1], 
           Private`j[2, 3, 0], 
           Private`j[3, 0, 2], 
           Private`j[3, 1, 1], 
           Private`j[3, 2, 0], 
           Private`j[4, 0, 1], 
           Private`j[4, 1, 0]}, Integers]]}]}, 
    Private`componentsEdgesAndnuc[{1, 0, 3}, {3, 2, 2}] = {{}, {}, {}, {}, {{
      3, 1, 0}, {1, 2, 0}, {3, 3, 0}, {2, 2, 1}, {1, 4, 0}, {4, 1, 1}, {2, 2, 
      1}, {2, 2, 1}, {4, 3, 1}, {3, 2, 2}, {0, 3, 1}, {2, 4, 1}, {1, 3, 2}, {
      0, 5, 1}, {3, 0, 2}, {1, 1, 2}, {5, 1, 2}, {3, 2, 2}, {2, 1, 3}, {3, 2, 
      2}, {1, 3, 2}, {5, 3, 2}, {4, 2, 3}, {1, 3, 2}, {3, 4, 2}, {2, 3, 3}, {
      1, 5, 2}, {0, 4, 3}, {4, 0, 3}, {2, 1, 3}, {2, 1, 3}, {6, 1, 3}, {4, 2, 
      3}, {3, 1, 4}, {0, 2, 3}, {4, 2, 3}, {2, 3, 3}, {1, 2, 4}, {5, 2, 4}, {
      2, 3, 3}, {0, 4, 3}, {3, 3, 4}, {1, 4, 4}, {1, 0, 4}, {5, 0, 4}, {3, 1, 
      4}, {2, 0, 5}, {3, 1, 4}, {1, 2, 4}, {4, 1, 5}, {1, 2, 4}, {2, 2, 5}, {
      0, 3, 5}, {2, 0, 5}, {3, 0, 6}, {0, 1, 5}, {1, 1, 6}}}, 
    Private`componentsEdgesAndnuc[{1, 1, 1}, {0, 3, 1}] = {{}, {}, {}, {}, {{
      3, 1, 0}, {1, 2, 0}, {1, 2, 0}, {3, 3, 0}, {1, 4, 0}, {2, 2, 1}, {1, 4, 
      0}, {0, 3, 1}, {2, 4, 1}, {0, 5, 1}, {2, 0, 1}, {0, 1, 1}, {4, 1, 1}, {
      2, 2, 1}, {1, 1, 2}, {2, 2, 1}, {0, 3, 1}, {3, 2, 2}, {1, 3, 2}, {1, 3, 
      2}, {3, 0, 2}, {1, 1, 2}, {2, 1, 3}, {0, 2, 3}}}, 
    Private`componentsEdgesAndnuc[{1, 1, 1}, {3, 0, 1}] = {{}, {}, {}, {}, {{
      2, 1, 0}, {0, 2, 0}, {1, 0, 1}, {3, 1, 1}, {1, 2, 1}, {2, 0, 2}, {1, 2, 
      1}, {0, 1, 2}, {2, 0, 2}, {0, 1, 2}, {2, 2, 2}, {3, 0, 3}, {1, 1, 3}, {
      0, 3, 2}, {1, 1, 3}, {1, 1, 3}, {0, 0, 4}, {2, 1, 4}, {0, 2, 4}, {1, 0, 
      5}}}, Private`componentsEdgesAndnuc[{1, 1, 1}, {3, 0, 
      3}] = {{}, {}, {}, {}, {{4, 1, 0}, {2, 2, 0}, {3, 0, 1}, {5, 1, 1}, {3, 
      2, 1}, {4, 0, 2}, {1, 1, 1}, {3, 2, 1}, {2, 1, 2}, {1, 3, 1}, {4, 0, 
      2}, {2, 1, 2}, {4, 2, 2}, {5, 0, 3}, {3, 1, 3}, {2, 1, 2}, {2, 3, 2}, {
      3, 1, 3}, {0, 2, 2}, {1, 2, 3}, {1, 0, 3}, {3, 1, 3}, {2, 0, 4}, {4, 1, 
      4}, {1, 2, 3}, {2, 2, 4}, {2, 0, 4}, {3, 0, 5}, {0, 1, 4}, {1, 1, 5}}}, 
    Private`componentsEdgesAndnuc[{1, 1, 2}, {0, 1, 0}] = {{}, {}, {}, {}, {{
      3, 1, 0}, {1, 2, 0}, {2, 0, 1}, {2, 2, 1}, {3, 0, 2}, {1, 1, 2}}}, 
    Private`componentsEdgesAndnuc[{1, 1, 2}, {2, 1, 0}] = {{}, {}, {}, {}, {{
      0, 0, 1}, {0, 1, 3}, {0, 2, 1}, {0, 2, 1}, {1, 0, 2}, {1, 0, 2}, {1, 1, 
      0}, {1, 1, 0}, {1, 1, 4}, {1, 2, 2}, {1, 2, 2}, {1, 3, 0}, {2, 0, 3}, {
      2, 0, 3}, {2, 1, 1}, {2, 1, 1}, {2, 1, 1}, {2, 2, 3}, {2, 3, 1}, {3, 0, 
      0}, {3, 0, 4}, {3, 1, 2}, {3, 1, 2}, {3, 2, 0}, {4, 0, 1}}}, 
    Private`componentsEdgesAndnuc[{1, 1, 3}, {1, 1, 2}] = {{}, {}, {}, {}, {{
      3, 0, 0}, {7, 0, 0}, {5, 1, 0}, {5, 1, 0}, {3, 2, 0}, {4, 0, 1}, {1, 1, 
      0}, {5, 1, 0}, {3, 2, 0}, {3, 2, 0}, {1, 3, 0}, {2, 1, 1}, {5, 3, 0}, {
      6, 1, 1}, {4, 2, 1}, {3, 2, 0}, {1, 3, 0}, {1, 3, 0}, {0, 2, 1}, {3, 4, 
      0}, {4, 2, 1}, {2, 3, 1}, {1, 5, 0}, {2, 3, 1}, {0, 4, 1}, {4, 0, 1}, {
      4, 0, 1}, {2, 1, 1}, {6, 1, 1}, {4, 2, 1}, {5, 0, 2}, {5, 0, 2}, {3, 1, 
      2}, {2, 1, 1}, {2, 1, 1}, {0, 2, 1}, {4, 2, 1}, {2, 3, 1}, {3, 1, 2}, {
      3, 1, 2}, {1, 2, 2}, {5, 2, 2}, {2, 3, 1}, {0, 4, 1}, {1, 2, 2}, {1, 2, 
      2}, {3, 3, 2}, {1, 4, 2}, {1, 0, 2}, {5, 0, 2}, {3, 1, 2}, {2, 0, 3}, {
      6, 0, 3}, {4, 1, 3}, {3, 1, 2}, {1, 2, 2}, {0, 1, 3}, {4, 1, 3}, {2, 2, 
      3}, {2, 2, 3}, {0, 3, 3}, {2, 0, 3}, {3, 0, 4}, {1, 1, 4}}}, 
    Private`componentsEdgesAndnuc[{1, 1, 3}, {2, 1, 0}] = {{}, {}, {}, {}, {{
      4, 0, 0}, {2, 1, 0}, {2, 1, 0}, {0, 2, 0}, {1, 0, 1}, {4, 2, 0}, {2, 3, 
      0}, {3, 1, 1}, {1, 2, 1}, {5, 0, 1}, {3, 1, 1}, {3, 1, 1}, {1, 2, 1}, {
      2, 0, 2}, {2, 0, 2}, {0, 1, 2}, {3, 3, 1}, {4, 1, 2}, {2, 2, 2}, {4, 1, 
      2}, {2, 2, 2}, {3, 0, 3}, {3, 0, 3}, {1, 1, 3}, {3, 2, 3}, {4, 0, 4}, {
      2, 1, 4}}}, 
    Private`componentsEdgesAndnuc[{1, 2, 0}, {0, 1, 0}] = {{}, {}, {}, {}, {{
      1, 2, 0}, {0, 1, 1}, {0, 3, 1}, {1, 1, 2}}}, 
    Private`componentsEdgesAndnuc[{1, 2, 1}, {0, 1, 0}] = {{}, {}, {}, {}, {{
      2, 2, 0}, {0, 3, 0}, {1, 1, 1}, {1, 3, 1}, {2, 1, 2}, {0, 2, 2}}}, 
    Private`componentsEdgesAndnuc[{1, 2, 1}, {3, 3, 
      1}] = {{}, {}, {}, {}, CompressedData["
1:eJw1kNENAzEMQgHj3BxdqSN0gX51/+Jze3qxOMsYJY/X+/kSgE9O5VhQDTBs
qFHFVBzQLJIaWkw/pClpRUCpmjx0SVZEKlrZisib/chaEUvY+ZAFIavXiH+S
jsrFLlyJr8JA/uiZ+BuZYK8OUK7h9XoChujUuaGtNo6xPxgvJ3VEjGsZMzsP
sa6IUIn1DboxA8LZMfCYRxpQZ/q4yOsLNLoFjA==
      "]}, 
    Private`componentsEdgesAndnuc[{1, 2, 2}, {3, 0, 1}] = {{}, {}, {}, {}, {{
      3, 0, 0}, {1, 1, 0}, {5, 1, 0}, {3, 2, 0}, {1, 3, 0}, {4, 0, 1}, {2, 1, 
      1}, {3, 2, 0}, {1, 3, 0}, {2, 1, 1}, {0, 2, 1}, {4, 0, 1}, {2, 1, 1}, {
      0, 2, 1}, {1, 0, 2}, {4, 2, 1}, {2, 3, 1}, {5, 0, 2}, {3, 1, 2}, {1, 2, 
      2}, {2, 3, 1}, {0, 4, 1}, {3, 1, 2}, {1, 2, 2}, {3, 1, 2}, {1, 2, 2}, {
      2, 0, 3}, {0, 1, 3}, {3, 3, 2}, {4, 1, 3}, {2, 2, 3}, {1, 4, 2}, {2, 2, 
      3}, {0, 3, 3}, {2, 2, 3}, {3, 0, 4}, {1, 1, 4}, {3, 2, 4}, {1, 3, 4}, {
      2, 1, 5}}}, 
    Private`componentsEdgesAndnuc[{1, 2, 2}, {3, 1, 
      3}] = {{}, {}, {}, {}, CompressedData["
1:eJw1kotxxjAIgxFgu2t0pY7wL9AVO1o/rORO4cRThOT78/vzyYj44yk/J2Jp
UBkd0stBSidjVVTHljpFUDVwkEiWdLlWDF8RLTxA+1fIjbaU3KokBRzEt60x
uXFbR7ky6y4BXDDzac2kIJdcQBDFuBFcSp9dX61HMcoz3bVZWWVLVjNmrLOA
ONR2dW54ldWtAqDjrexpmNeI4halzYpzo3vFR8WjKki0p53opQHNPqtrZhc1
8FgXgFVIliUseTu6utEavtrz9Y4lATj8JseB7lg3Eswb5P1w77WGs1S8XYq9
Ys/73E8H4fsMYic/js4/W3QH9A==
      "]}, 
    Private`componentsEdgesAndnuc[{1, 3, 0}, {1, 0, 0}] = {{}, {}, {}, {}, {{
      0, 4, 0}, {1, 2, 1}, {0, 3, 2}}}, 
    Private`componentsEdgesAndnuc[{1, 3, 0}, {3, 1, 2}] = {{}, {}, {}, {}, {{
      6, 0, 0}, {4, 1, 0}, {4, 1, 0}, {2, 2, 0}, {4, 3, 0}, {5, 1, 1}, {2, 2, 
      0}, {0, 3, 0}, {2, 4, 0}, {3, 2, 1}, {0, 5, 0}, {1, 3, 1}, {3, 0, 1}, {
      5, 1, 1}, {3, 2, 1}, {4, 0, 2}, {1, 1, 1}, {3, 2, 1}, {1, 3, 1}, {2, 1, 
      2}, {3, 4, 1}, {4, 2, 2}, {1, 3, 1}, {0, 2, 2}, {1, 5, 1}, {2, 3, 2}, {
      0, 4, 2}, {4, 0, 2}, {2, 1, 2}, {4, 2, 2}, {2, 3, 2}, {5, 0, 3}, {3, 1, 
      3}, {2, 1, 2}, {0, 2, 2}, {2, 3, 2}, {3, 1, 3}, {1, 2, 3}, {2, 5, 2}, {
      3, 3, 3}, {0, 4, 2}, {1, 2, 3}, {1, 4, 3}, {1, 0, 3}, {3, 1, 3}, {1, 2, 
      3}, {2, 0, 4}, {3, 3, 3}, {4, 1, 4}, {2, 2, 4}, {1, 2, 3}, {0, 1, 4}, {
      1, 4, 3}, {2, 2, 4}, {2, 4, 4}, {0, 3, 4}, {2, 0, 4}, {0, 1, 4}, {2, 2, 
      4}, {3, 0, 5}, {1, 1, 5}, {3, 2, 5}, {0, 3, 4}, {1, 1, 5}, {1, 3, 5}, {
      1, 1, 5}, {0, 0, 6}, {2, 1, 6}, {0, 2, 6}, {1, 0, 7}}}, 
    Private`componentsEdgesAndnuc[{1, 3, 0}, {3, 3, 0}] = {{}, {}, {}, {}, {{
      4, 2, 0}, {2, 3, 0}, {2, 5, 0}, {3, 3, 1}, {3, 1, 1}, {1, 2, 1}, {3, 3, 
      1}, {1, 4, 1}, {2, 2, 2}, {1, 6, 1}, {2, 4, 2}, {2, 0, 2}, {0, 1, 2}, {
      4, 1, 2}, {2, 2, 2}, {0, 3, 2}, {1, 1, 3}, {2, 4, 2}, {0, 5, 2}, {3, 2, 
      3}, {1, 3, 3}, {0, 7, 2}, {1, 5, 3}, {3, 0, 3}, {1, 1, 3}, {0, 0, 4}, {
      3, 2, 3}, {1, 3, 3}, {2, 1, 4}, {0, 2, 4}, {1, 5, 3}, {2, 3, 4}, {0, 4, 
      4}, {0, 6, 4}, {4, 0, 4}, {2, 1, 4}, {1, 0, 5}, {2, 3, 4}, {3, 1, 5}, {
      1, 2, 5}, {1, 4, 5}, {3, 1, 5}, {2, 0, 6}, {2, 2, 6}, {3, 0, 7}}}, 
    Private`componentsEdgesAndnuc[{1, 3, 2}, {0, 3, 
      2}] = {{}, {}, {}, {}, CompressedData["
1:eJw1kNFxBDEIQyUE3msjLaWEayCfaT/P9mXG7OBnpAW+3j/f75L0S+TGWCm1
7JM4KnuitJqMijK5RxXPKEvdVEhtcj+qcS+TaLke62XKkcJw2KotLQhWl3AF
rBgr5FOVEMYTfiGE6ukKz3R57Mi9zuG3x5b6Uh4Ux2TRuLfXfWKey9FevirN
nTgQonwIR8MOoj3Zv8/uqR81U2a3lisBXsJ2Pt9TyUZbdLmPTt9/UwoFfA==

      "]}, 
    Private`componentsEdgesAndnuc[{1, 3, 3}, {2, 3, 
      1}] = {{}, {}, {}, {}, CompressedData["
1:eJxdkuttxDAMg0X5lTW6QkfpCLdA9//XT0efGwQgGIWyLEbR1+v355UR8a2I
xpPHIFJBGRlyAEJaipHR2mb16FKmzqsZhfrFMZSxWTNmqjdl11HMFmNooKyY
XX0op4h1heNYdoEbKtQ/TC9ziwSqm/5FbJh5AxdVbw/HyfajbJlmtXxkMWZ2
dp95m7S3RS1638w4rNv2EXcKhXgmbmCKj3Nz1AzKjPkYc18mB4jMD+dmfR5c
bLZDO7ENpktozrb5fqCs3lL8JLPNT/Re4FJ/hWMQs/XZcrWHVTsRKxP9imrt
7r6/uvOraxc2A+b+OEMGnKy5jrVtZlGeBQSg3q3j6i6CQWIUNPtpfS7M2vFC
aPhOF85gSUetMAPKwkndxdrsNmZWA+yYY+xaHyhPMVlJwNYPMbWaygl6LCYb
uf4AuxYLlg==
      "]}, 
    Private`componentsEdgesAndnuc[{2, 0, 0}, {1, 2, 2}] = {{}, {}, {}, {}, {{
      3, 2, 0}, {1, 3, 0}, {2, 3, 1}, {4, 0, 1}, {2, 1, 1}, {3, 1, 2}, {0, 2, 
      1}, {1, 2, 2}, {2, 2, 3}}}, 
    Private`componentsEdgesAndnuc[{2, 0, 0}, {2, 2, 3}] = {{}, {}, {}, {}, {{
      3, 4, 0}, {4, 2, 1}, {2, 3, 1}, {3, 3, 2}, {5, 0, 2}, {3, 1, 2}, {4, 1, 
      3}, {1, 2, 2}, {2, 2, 3}, {3, 2, 4}}}, 
    Private`componentsEdgesAndnuc[{2, 0, 0}, {2, 3, 2}] = {{}, {}, {}, {}, {{
      2, 5, 0}, {3, 3, 1}, {1, 4, 1}, {2, 4, 2}, {4, 1, 2}, {2, 2, 2}, {3, 2, 
      3}, {0, 3, 2}, {1, 3, 3}, {2, 3, 4}}}, 
    Private`componentsEdgesAndnuc[{2, 0, 0}, {3, 0, 0}] = {{}, {}, {}, {}, {{
      0, 2, 1}, {0, 1, 3}, {0, 0, 5}}}, 
    Private`componentsEdgesAndnuc[{2, 0, 1}, {0, 0, 0}] = {{}, {}, {}, {}, {{
      1, 0, 2}}}, 
    Private`componentsEdgesAndnuc[{2, 1, 0}, {2, 0, 3}] = {{}, {}, {}, {}, {{
      3, 1, 0}, {3, 3, 0}, {4, 1, 1}, {1, 2, 0}, {2, 2, 1}, {2, 0, 1}, {2, 2, 
      1}, {3, 0, 2}, {3, 2, 2}, {4, 0, 3}, {0, 1, 1}, {1, 1, 2}, {2, 1, 3}, {
      1, 1, 2}, {2, 1, 3}, {3, 1, 4}, {0, 0, 3}, {1, 0, 4}, {2, 0, 5}}}, 
    Private`componentsEdgesAndnuc[{2, 1, 2}, {1, 2, 2}] = {{}, {}, {}, {}, 
      ReplaceAll[{
        Private`lambda[0, 0, 1], 
        Private`lambda[0, 0, 2], 
        Private`lambda[0, 0, 3], 
        Private`lambda[0, 0, 4]}, {
        ToRules[
         Reduce[
          And[
           ReplaceAll[
            And[
            Private`lambda[1, 0, 0] == 
             Private`j[1, 0, 4] + Private`j[1, 1, 3] + Private`j[1, 2, 2] + 
              Private`j[1, 3, 1] + Private`j[1, 4, 0], 
             Private`lambda[2, 0, 0] == 
             Private`j[2, 0, 3] + Private`j[2, 1, 2] + Private`j[2, 2, 1] + 
              Private`j[2, 3, 0], Private`lambda[3, 0, 0] == 
             Private`j[3, 0, 2] + Private`j[3, 1, 1] + Private`j[3, 2, 0], 
             Private`lambda[4, 0, 0] == 
             Private`j[4, 0, 1] + Private`j[4, 1, 0], Private`lambda[0, 1, 0] == 
             Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
              Private`j[3, 1, 1] + Private`j[4, 1, 0], 
             Private`lambda[0, 2, 0] == 
             Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] + 
              Private`j[3, 2, 0], Private`lambda[0, 3, 0] == 
             Private`j[0, 3, 2] + Private`j[1, 3, 1] + Private`j[2, 3, 0], 
             Private`lambda[0, 4, 0] == 
             Private`j[0, 4, 1] + Private`j[1, 4, 0], Private`lambda[0, 0, 1] == 
             Private`j[0, 4, 1] + Private`j[1, 3, 1] + Private`j[2, 2, 1] + 
              Private`j[3, 1, 1] + Private`j[4, 0, 1], 
             Private`lambda[0, 0, 2] == 
             Private`j[0, 3, 2] + Private`j[1, 2, 2] + Private`j[2, 1, 2] + 
              Private`j[3, 0, 2], Private`lambda[0, 0, 3] == 
             Private`j[0, 2, 3] + Private`j[1, 1, 3] + Private`j[2, 0, 3], 
             Private`lambda[0, 0, 4] == 
             Private`j[0, 1, 4] + Private`j[1, 0, 4]], {
             MapThread[Rule, {{
                Private`lambda[1, 0, 0], 
                Private`lambda[2, 0, 0], 
                Private`lambda[3, 0, 0], 
                Private`lambda[4, 0, 0]}, {2, 1, 2}}], 
             MapThread[Rule, {{
                Private`lambda[0, 1, 0], 
                Private`lambda[0, 2, 0], 
                Private`lambda[0, 3, 0], 
                Private`lambda[0, 4, 0]}, {1, 2, 2}}]}], Private`j[1, 4, 0] >= 
           0, Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 2, 2] + Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 1, 3] + Private`j[1, 2, 2] + Private`j[1, 3, 1] + 
            Private`j[1, 4, 0] >= 0, Private`j[2, 3, 0] >= 0, 
           Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[2, 1, 2] + Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[3, 2, 0] >= 0, Private`j[3, 1, 1] + Private`j[3, 2, 0] >= 
           0, Private`j[4, 1, 0] >= 0, Private`j[0, 1, 4] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
            Private`j[3, 1, 1] >= 0, Private`j[0, 2, 3] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] >= 0, 
           Private`j[0, 3, 2] >= 0, Private`j[0, 3, 2] + Private`j[1, 3, 1] >= 
           0, Private`j[0, 4, 1] >= 0, Private`j[4, 0, 1] >= 0, 
           Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[2, 2, 1] + Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[1, 3, 1] + Private`j[2, 2, 1] + Private`j[3, 1, 1] + 
            Private`j[4, 0, 1] >= 0, Private`j[3, 0, 2] >= 0, 
           Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[1, 2, 2] + Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[2, 0, 3] >= 0, Private`j[1, 1, 3] + Private`j[2, 0, 3] >= 
           0, Private`j[1, 0, 4] >= 0], {
           Private`j[0, 1, 4], 
           Private`j[0, 2, 3], 
           Private`j[0, 3, 2], 
           Private`j[0, 4, 1], 
           Private`j[1, 0, 4], 
           Private`j[1, 1, 3], 
           Private`j[1, 2, 2], 
           Private`j[1, 3, 1], 
           Private`j[1, 4, 0], 
           Private`j[2, 0, 3], 
           Private`j[2, 1, 2], 
           Private`j[2, 2, 1], 
           Private`j[2, 3, 0], 
           Private`j[3, 0, 2], 
           Private`j[3, 1, 1], 
           Private`j[3, 2, 0], 
           Private`j[4, 0, 1], 
           Private`j[4, 1, 0]}, Integers]]}]}, 
    Private`componentsEdgesAndnuc[{2, 1, 2}, {3, 2, 
      1}] = {{}, {}, {}, {}, CompressedData["
1:eJxNkIttQzEMA0l9nK7RlTpCFugyHbgnKw1q0A+CaN6z/Pn8/nqGpB92sksK
jxSqUOQobYcpxi5RVylaiVEOVktH1EqibhnnlQJwC48THSa1wY2yOLudbQFB
2a6MqKCcwDG1bh4mgnYUJN4cbnovm+/Oou5dcmk9+Vwmc4wu9vUXAMFsxekx
IDVOqeoNR7zCcviMp1qU1f/Ji1pOAnLPA9wEBcoZsjc+o6hR6++kDtpa5n1P
+USMlGPRST3skfTxCxS2Biw=
      "]}, 
    Private`componentsEdgesAndnuc[{2, 1, 2}, {3, 3, 0}] = {{}, {}, {}, {}, {{
      3, 1, 0}, {1, 2, 0}, {3, 3, 0}, {1, 4, 0}, {1, 4, 0}, {2, 2, 1}, {0, 3, 
      1}, {3, 5, 0}, {1, 6, 0}, {2, 4, 1}, {0, 5, 1}, {1, 3, 2}, {2, 0, 1}, {
      4, 1, 1}, {2, 2, 1}, {2, 2, 1}, {0, 3, 1}, {1, 1, 2}, {4, 3, 1}, {2, 4, 
      1}, {2, 4, 1}, {0, 5, 1}, {3, 2, 2}, {1, 3, 2}, {1, 3, 2}, {0, 2, 3}, {
      2, 6, 1}, {3, 4, 2}, {1, 5, 2}, {2, 3, 3}, {0, 4, 3}, {3, 0, 2}, {3, 0, 
      2}, {1, 1, 2}, {5, 1, 2}, {3, 2, 2}, {3, 2, 2}, {1, 3, 2}, {1, 3, 2}, {
      2, 1, 3}, {2, 1, 3}, {0, 2, 3}, {3, 4, 2}, {1, 5, 2}, {4, 2, 3}, {2, 3, 
      3}, {2, 3, 3}, {0, 4, 3}, {1, 2, 4}, {1, 2, 4}, {2, 5, 3}, {3, 3, 4}, {
      1, 4, 4}, {4, 0, 3}, {4, 0, 3}, {2, 1, 3}, {2, 1, 3}, {0, 2, 3}, {1, 0, 
      4}, {4, 2, 3}, {2, 3, 3}, {0, 4, 3}, {3, 1, 4}, {3, 1, 4}, {1, 2, 4}, {
      1, 2, 4}, {0, 1, 5}, {3, 3, 4}, {1, 4, 4}, {2, 2, 5}, {2, 2, 5}, {0, 3, 
      5}, {2, 4, 5}, {5, 0, 4}, {3, 1, 4}, {1, 2, 4}, {2, 0, 5}, {2, 0, 5}, {
      0, 1, 5}, {4, 1, 5}, {2, 2, 5}, {0, 3, 5}, {1, 1, 6}, {1, 1, 6}, {3, 2, 
      6}, {1, 3, 6}, {3, 0, 6}, {1, 1, 6}, {0, 0, 7}, {2, 1, 7}, {0, 2, 7}, {
      1, 0, 8}}}, 
    Private`componentsEdgesAndnuc[{2, 1, 3}, {0, 1, 0}] = {{}, {}, {}, {}, {{
      4, 1, 1}, {2, 2, 1}, {3, 0, 2}, {3, 2, 2}, {4, 0, 3}, {2, 1, 3}}}, 
    Private`componentsEdgesAndnuc[{2, 1, 3}, {2, 3, 
      0}] = {{}, {}, {}, {}, CompressedData["
1:eJxNj0sOwzAIROcDzjl6pR4hmy57/12HOKoiPWSDGQa/zu/7FIBPwjtINGEN
9KCUxDZcAxslygOKlLxQRTVtsZSWENUWJhZT5lN1ncmHeGx5Z5S1h+QaYLtV
KZUbCk/hteOsuIVjnVH0XxvbsOUT13vMFu62mY/yfGMAunm35cchnqWWu3Cn
xArmopZwgMcPI2gE1A==
      "]}, 
    Private`componentsEdgesAndnuc[{2, 2, 0}, {1, 0, 3}] = {{}, {}, {}, {}, 
      ReplaceAll[{
        Private`lambda[0, 0, 1], 
        Private`lambda[0, 0, 2], 
        Private`lambda[0, 0, 3], 
        Private`lambda[0, 0, 4]}, {
        ToRules[
         Reduce[
          And[
           ReplaceAll[
            And[
            Private`lambda[1, 0, 0] == 
             Private`j[1, 0, 4] + Private`j[1, 1, 3] + Private`j[1, 2, 2] + 
              Private`j[1, 3, 1] + Private`j[1, 4, 0], 
             Private`lambda[2, 0, 0] == 
             Private`j[2, 0, 3] + Private`j[2, 1, 2] + Private`j[2, 2, 1] + 
              Private`j[2, 3, 0], Private`lambda[3, 0, 0] == 
             Private`j[3, 0, 2] + Private`j[3, 1, 1] + Private`j[3, 2, 0], 
             Private`lambda[4, 0, 0] == 
             Private`j[4, 0, 1] + Private`j[4, 1, 0], Private`lambda[0, 1, 0] == 
             Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
              Private`j[3, 1, 1] + Private`j[4, 1, 0], 
             Private`lambda[0, 2, 0] == 
             Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] + 
              Private`j[3, 2, 0], Private`lambda[0, 3, 0] == 
             Private`j[0, 3, 2] + Private`j[1, 3, 1] + Private`j[2, 3, 0], 
             Private`lambda[0, 4, 0] == 
             Private`j[0, 4, 1] + Private`j[1, 4, 0], Private`lambda[0, 0, 1] == 
             Private`j[0, 4, 1] + Private`j[1, 3, 1] + Private`j[2, 2, 1] + 
              Private`j[3, 1, 1] + Private`j[4, 0, 1], 
             Private`lambda[0, 0, 2] == 
             Private`j[0, 3, 2] + Private`j[1, 2, 2] + Private`j[2, 1, 2] + 
              Private`j[3, 0, 2], Private`lambda[0, 0, 3] == 
             Private`j[0, 2, 3] + Private`j[1, 1, 3] + Private`j[2, 0, 3], 
             Private`lambda[0, 0, 4] == 
             Private`j[0, 1, 4] + Private`j[1, 0, 4]], {
             MapThread[Rule, {{
                Private`lambda[1, 0, 0], 
                Private`lambda[2, 0, 0], 
                Private`lambda[3, 0, 0], 
                Private`lambda[4, 0, 0]}, {2, 2, 0}}], 
             MapThread[Rule, {{
                Private`lambda[0, 1, 0], 
                Private`lambda[0, 2, 0], 
                Private`lambda[0, 3, 0], 
                Private`lambda[0, 4, 0]}, {1, 0, 3}}]}], Private`j[1, 4, 0] >= 
           0, Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 2, 2] + Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 1, 3] + Private`j[1, 2, 2] + Private`j[1, 3, 1] + 
            Private`j[1, 4, 0] >= 0, Private`j[2, 3, 0] >= 0, 
           Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[2, 1, 2] + Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[3, 2, 0] >= 0, Private`j[3, 1, 1] + Private`j[3, 2, 0] >= 
           0, Private`j[4, 1, 0] >= 0, Private`j[0, 1, 4] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
            Private`j[3, 1, 1] >= 0, Private`j[0, 2, 3] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] >= 0, 
           Private`j[0, 3, 2] >= 0, Private`j[0, 3, 2] + Private`j[1, 3, 1] >= 
           0, Private`j[0, 4, 1] >= 0, Private`j[4, 0, 1] >= 0, 
           Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[2, 2, 1] + Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[1, 3, 1] + Private`j[2, 2, 1] + Private`j[3, 1, 1] + 
            Private`j[4, 0, 1] >= 0, Private`j[3, 0, 2] >= 0, 
           Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[1, 2, 2] + Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[2, 0, 3] >= 0, Private`j[1, 1, 3] + Private`j[2, 0, 3] >= 
           0, Private`j[1, 0, 4] >= 0], {
           Private`j[0, 1, 4], 
           Private`j[0, 2, 3], 
           Private`j[0, 3, 2], 
           Private`j[0, 4, 1], 
           Private`j[1, 0, 4], 
           Private`j[1, 1, 3], 
           Private`j[1, 2, 2], 
           Private`j[1, 3, 1], 
           Private`j[1, 4, 0], 
           Private`j[2, 0, 3], 
           Private`j[2, 1, 2], 
           Private`j[2, 2, 1], 
           Private`j[2, 3, 0], 
           Private`j[3, 0, 2], 
           Private`j[3, 1, 1], 
           Private`j[3, 2, 0], 
           Private`j[4, 0, 1], 
           Private`j[4, 1, 0]}, Integers]]}]}, 
    Private`componentsEdgesAndnuc[{2, 2, 0}, {3, 0, 1}] = {{}, {}, {}, {}, {{
      2, 1, 0}, {2, 3, 0}, {3, 1, 1}, {0, 4, 0}, {1, 2, 1}, {1, 2, 1}, {2, 0, 
      2}, {1, 4, 1}, {2, 2, 2}, {3, 0, 3}, {0, 3, 2}, {1, 1, 3}, {0, 3, 2}, {
      1, 1, 3}, {1, 3, 3}, {2, 1, 4}, {0, 2, 4}, {0, 2, 4}, {1, 0, 5}, {1, 2, 
      5}, {0, 1, 6}}}, 
    Private`componentsEdgesAndnuc[{2, 2, 2}, {0, 1, 3}] = {{}, {}, {}, {}, {{
      3, 1, 0}, {5, 2, 0}, {3, 3, 0}, {4, 1, 1}, {6, 2, 1}, {4, 3, 1}, {5, 1, 
      2}, {1, 2, 0}, {3, 3, 0}, {1, 4, 0}, {2, 2, 1}, {4, 3, 1}, {2, 4, 1}, {
      3, 2, 2}, {5, 3, 2}, {1, 4, 0}, {0, 3, 1}, {2, 4, 1}, {1, 3, 2}, {3, 4, 
      2}, {0, 5, 1}, {1, 5, 2}, {2, 0, 1}, {4, 1, 1}, {2, 2, 1}, {3, 0, 2}, {
      5, 1, 2}, {3, 2, 2}, {4, 0, 3}, {6, 1, 3}, {4, 2, 3}, {2, 2, 1}, {0, 3, 
      1}, {1, 1, 2}, {3, 2, 2}, {1, 3, 2}, {2, 1, 3}, {4, 2, 3}, {2, 3, 3}, {
      1, 3, 2}, {0, 2, 3}, {2, 3, 3}, {0, 4, 3}, {3, 0, 2}, {1, 1, 2}, {4, 0, 
      3}, {2, 1, 3}, {5, 0, 4}, {3, 1, 4}, {2, 1, 3}, {0, 2, 3}, {3, 1, 4}, {
      1, 2, 4}, {1, 2, 4}, {1, 0, 4}, {2, 0, 5}, {0, 1, 5}}}, 
    Private`componentsEdgesAndnuc[{2, 2, 2}, {0, 3, 0}] = {{}, {}, {}, {}, 
      ReplaceAll[{
        Private`lambda[0, 0, 1], 
        Private`lambda[0, 0, 2], 
        Private`lambda[0, 0, 3], 
        Private`lambda[0, 0, 4]}, {
        ToRules[
         Reduce[
          And[
           ReplaceAll[
            And[
            Private`lambda[1, 0, 0] == 
             Private`j[1, 0, 4] + Private`j[1, 1, 3] + Private`j[1, 2, 2] + 
              Private`j[1, 3, 1] + Private`j[1, 4, 0], 
             Private`lambda[2, 0, 0] == 
             Private`j[2, 0, 3] + Private`j[2, 1, 2] + Private`j[2, 2, 1] + 
              Private`j[2, 3, 0], Private`lambda[3, 0, 0] == 
             Private`j[3, 0, 2] + Private`j[3, 1, 1] + Private`j[3, 2, 0], 
             Private`lambda[4, 0, 0] == 
             Private`j[4, 0, 1] + Private`j[4, 1, 0], Private`lambda[0, 1, 0] == 
             Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
              Private`j[3, 1, 1] + Private`j[4, 1, 0], 
             Private`lambda[0, 2, 0] == 
             Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] + 
              Private`j[3, 2, 0], Private`lambda[0, 3, 0] == 
             Private`j[0, 3, 2] + Private`j[1, 3, 1] + Private`j[2, 3, 0], 
             Private`lambda[0, 4, 0] == 
             Private`j[0, 4, 1] + Private`j[1, 4, 0], Private`lambda[0, 0, 1] == 
             Private`j[0, 4, 1] + Private`j[1, 3, 1] + Private`j[2, 2, 1] + 
              Private`j[3, 1, 1] + Private`j[4, 0, 1], 
             Private`lambda[0, 0, 2] == 
             Private`j[0, 3, 2] + Private`j[1, 2, 2] + Private`j[2, 1, 2] + 
              Private`j[3, 0, 2], Private`lambda[0, 0, 3] == 
             Private`j[0, 2, 3] + Private`j[1, 1, 3] + Private`j[2, 0, 3], 
             Private`lambda[0, 0, 4] == 
             Private`j[0, 1, 4] + Private`j[1, 0, 4]], {
             MapThread[Rule, {{
                Private`lambda[1, 0, 0], 
                Private`lambda[2, 0, 0], 
                Private`lambda[3, 0, 0], 
                Private`lambda[4, 0, 0]}, {2, 2, 2}}], 
             MapThread[Rule, {{
                Private`lambda[0, 1, 0], 
                Private`lambda[0, 2, 0], 
                Private`lambda[0, 3, 0], 
                Private`lambda[0, 4, 0]}, {0, 3, 0}}]}], Private`j[1, 4, 0] >= 
           0, Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 2, 2] + Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 1, 3] + Private`j[1, 2, 2] + Private`j[1, 3, 1] + 
            Private`j[1, 4, 0] >= 0, Private`j[2, 3, 0] >= 0, 
           Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[2, 1, 2] + Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[3, 2, 0] >= 0, Private`j[3, 1, 1] + Private`j[3, 2, 0] >= 
           0, Private`j[4, 1, 0] >= 0, Private`j[0, 1, 4] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
            Private`j[3, 1, 1] >= 0, Private`j[0, 2, 3] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] >= 0, 
           Private`j[0, 3, 2] >= 0, Private`j[0, 3, 2] + Private`j[1, 3, 1] >= 
           0, Private`j[0, 4, 1] >= 0, Private`j[4, 0, 1] >= 0, 
           Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[2, 2, 1] + Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[1, 3, 1] + Private`j[2, 2, 1] + Private`j[3, 1, 1] + 
            Private`j[4, 0, 1] >= 0, Private`j[3, 0, 2] >= 0, 
           Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[1, 2, 2] + Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[2, 0, 3] >= 0, Private`j[1, 1, 3] + Private`j[2, 0, 3] >= 
           0, Private`j[1, 0, 4] >= 0], {
           Private`j[0, 1, 4], 
           Private`j[0, 2, 3], 
           Private`j[0, 3, 2], 
           Private`j[0, 4, 1], 
           Private`j[1, 0, 4], 
           Private`j[1, 1, 3], 
           Private`j[1, 2, 2], 
           Private`j[1, 3, 1], 
           Private`j[1, 4, 0], 
           Private`j[2, 0, 3], 
           Private`j[2, 1, 2], 
           Private`j[2, 2, 1], 
           Private`j[2, 3, 0], 
           Private`j[3, 0, 2], 
           Private`j[3, 1, 1], 
           Private`j[3, 2, 0], 
           Private`j[4, 0, 1], 
           Private`j[4, 1, 0]}, Integers]]}]}, 
    Private`componentsEdgesAndnuc[{2, 2, 2}, {1, 1, 1}] = {{}, {}, {}, {}, {{
      4, 1, 0}, {2, 2, 0}, {2, 2, 0}, {0, 3, 0}, {3, 0, 1}, {1, 1, 1}, {4, 3, 
      0}, {2, 4, 0}, {5, 1, 1}, {3, 2, 1}, {3, 2, 1}, {1, 3, 1}, {4, 0, 2}, {
      2, 1, 2}, {2, 4, 0}, {0, 5, 0}, {3, 2, 1}, {1, 3, 1}, {1, 3, 1}, {2, 1, 
      2}, {0, 2, 2}, {3, 4, 1}, {4, 2, 2}, {2, 3, 2}, {1, 5, 1}, {2, 3, 2}, {
      0, 4, 2}, {3, 2, 1}, {1, 3, 1}, {4, 0, 2}, {2, 1, 2}, {2, 1, 2}, {0, 2, 
      2}, {1, 0, 3}, {4, 2, 2}, {2, 3, 2}, {5, 0, 3}, {3, 1, 3}, {3, 1, 3}, {
      1, 2, 3}, {2, 3, 2}, {0, 4, 2}, {3, 1, 3}, {1, 2, 3}, {1, 2, 3}, {3, 3, 
      3}, {1, 4, 3}, {3, 1, 3}, {1, 2, 3}, {2, 0, 4}, {2, 0, 4}, {0, 1, 4}, {
      4, 1, 4}, {2, 2, 4}, {2, 2, 4}, {0, 3, 4}, {3, 0, 5}, {1, 1, 5}}}, 
    Private`componentsEdgesAndnuc[{2, 2, 3}, {0, 0, 2}] = {{}, {}, {}, {}, {{
      3, 2, 0}, {4, 2, 1}, {5, 2, 2}, {2, 3, 1}, {3, 3, 2}, {1, 4, 2}, {3, 1, 
      2}, {4, 1, 3}, {2, 2, 3}, {3, 0, 4}}}, 
    Private`componentsEdgesAndnuc[{2, 2, 3}, {3, 0, 
      2}] = {{}, {}, {}, {}, CompressedData["
1:eJxdkMsNwzAMQ/mRnDm6UkfIAu3+p9IREAS1oYOpJ4Ly6/y8TwH4ppwqQExh
ESXIaNAkxWVU4SBbtLmgonIGy+RgYdQYhsVhomBhlLxg3baZBvXn6W3mLq74
Wwfc6ctPz5iESSNAQgxwuXBnas3zaZiUgYes7MdqeyWy9h0+YlI4d0vFHrbu
EaLTyopmd4IHZWeDK8oW83lX8HzY8QPLtQVQ
      "]}, 
    Private`componentsEdgesAndnuc[{2, 3, 1}, {0, 3, 
      3}] = {{}, {}, {}, {}, CompressedData["
1:eJxNUYttxUAI8wcuc3SljvAW6DIduIaToio5hTNgg/P1+fn+CMBvjnMSFyHl
wiOU0aTFSmZxkgFVCJjnFKrRpou1bUnRDKiDgLK66cMqYUHuVa2KwAM+1CMP
MV8tDcbQpjC0DUeBm8032Vcx/AmPFf6WvYUhSDZ0mildQzG5rhwHxyLcK454
sruuLuH/ovHgodN9UCNIperVDfkV3VmyUJVL0zS6M0xZF5xuYwN0PPbLf5nj
9INY1ZcWrMuZNQbMuGN66GLfvPfnFE4KskCc/QOmWAam
      "]}, 
    Private`componentsEdgesAndnuc[{2, 3, 1}, {2, 2, 
      2}] = {{}, {}, {}, {}, CompressedData["
1:eJxdk9lxAzEMQwEekn9cRFpKCW4g/f8FFNfMxjOY0UGIetRyv14/3y8D8CTg
GjWkZizRYOAm0krupQUGS2Z0WNskkONhlNojacyAZ2kZw0sWTJqbjV/Ovlg2
rlLbJK/LzDdCZxZT8zAZFEKWFOUDCtW+OHbRHC7x8c58YUNYdofZMOW+kMzE
K8k7p5pNK2nAxl8A7+EOKR2okkod4DlyWHzIpcvp7ssi3NKvcvRSUeK22lw+
hbA+gN9hhl+ins0/SVKb6Orjg7+RmucP5ji77KZSUCbpH9sxKFr7UXJ9++U1
XzFIp3H0FjEwk//0WnbyhVT2c0H2FcMAqkuyb7kM6rKCSu1o3g2npZLdM+Od
sF4QS92RXPoAfURLniW91AZgB7axBNMP8fgF2C0Jgw==
      "]}, 
    Private`componentsEdgesAndnuc[{2, 3, 2}, {1, 0, 2}] = {{}, {}, {}, {}, {{
      3, 2, 0}, {1, 3, 0}, {3, 4, 0}, {4, 2, 1}, {2, 3, 1}, {4, 4, 1}, {5, 2, 
      2}, {3, 3, 2}, {1, 5, 0}, {2, 3, 1}, {0, 4, 1}, {2, 5, 1}, {3, 3, 2}, {
      1, 4, 2}, {0, 6, 1}, {1, 4, 2}, {2, 3, 1}, {3, 1, 2}, {1, 2, 2}, {3, 3, 
      2}, {4, 1, 3}, {2, 2, 3}, {4, 3, 3}, {1, 4, 2}, {2, 2, 3}, {0, 3, 3}, {
      2, 4, 3}, {0, 5, 3}, {2, 2, 3}, {3, 0, 4}, {1, 1, 4}, {3, 2, 4}, {1, 3, 
      4}, {2, 1, 5}}}, 
    Private`componentsEdgesAndnuc[{2, 3, 3}, {1, 0, 3}] = {{}, {}, {}, {}, {{
      5, 2, 0}, {3, 3, 0}, {5, 4, 0}, {6, 2, 1}, {4, 3, 1}, {6, 4, 1}, {7, 2, 
      2}, {5, 3, 2}, {3, 3, 0}, {1, 4, 0}, {3, 5, 0}, {4, 3, 1}, {2, 4, 1}, {
      4, 5, 1}, {5, 3, 2}, {3, 4, 2}, {1, 6, 0}, {2, 4, 1}, {0, 5, 1}, {2, 6, 
      1}, {3, 4, 2}, {1, 5, 2}, {0, 7, 1}, {1, 5, 2}, {4, 1, 1}, {2, 2, 1}, {
      4, 3, 1}, {5, 1, 2}, {3, 2, 2}, {5, 3, 2}, {6, 1, 3}, {4, 2, 3}, {6, 3, 
      3}, {2, 4, 1}, {3, 2, 2}, {1, 3, 2}, {3, 4, 2}, {4, 2, 3}, {2, 3, 3}, {
      4, 4, 3}, {1, 5, 2}, {2, 3, 3}, {0, 4, 3}, {2, 5, 3}, {0, 6, 3}, {3, 2, 
      2}, {4, 0, 3}, {2, 1, 3}, {4, 2, 3}, {5, 0, 4}, {3, 1, 4}, {5, 2, 4}, {
      2, 3, 3}, {3, 1, 4}, {1, 2, 4}, {3, 3, 4}, {1, 4, 4}, {3, 1, 4}, {2, 0, 
      5}, {4, 1, 5}, {2, 2, 5}, {3, 0, 6}}}, 
    Private`componentsEdgesAndnuc[{2, 3, 3}, {1, 1, 1}] = {{}, {}, {}, {}, {{
      5, 2, 0}, {3, 3, 0}, {3, 3, 0}, {1, 4, 0}, {4, 1, 1}, {2, 2, 1}, {5, 4, 
      0}, {3, 5, 0}, {6, 2, 1}, {4, 3, 1}, {4, 3, 1}, {2, 4, 1}, {5, 1, 2}, {
      3, 2, 2}, {3, 5, 0}, {1, 6, 0}, {4, 3, 1}, {2, 4, 1}, {2, 4, 1}, {0, 5, 
      1}, {3, 2, 2}, {1, 3, 2}, {4, 5, 1}, {5, 3, 2}, {3, 4, 2}, {2, 6, 1}, {
      3, 4, 2}, {1, 5, 2}, {4, 3, 1}, {2, 4, 1}, {5, 1, 2}, {3, 2, 2}, {3, 2, 
      2}, {1, 3, 2}, {4, 0, 3}, {2, 1, 3}, {5, 3, 2}, {3, 4, 2}, {6, 1, 3}, {
      4, 2, 3}, {4, 2, 3}, {2, 3, 3}, {3, 4, 2}, {1, 5, 2}, {4, 2, 3}, {2, 3, 
      3}, {2, 3, 3}, {0, 4, 3}, {4, 4, 3}, {2, 5, 3}, {4, 2, 3}, {2, 3, 3}, {
      5, 0, 4}, {3, 1, 4}, {3, 1, 4}, {1, 2, 4}, {5, 2, 4}, {3, 3, 4}, {3, 3, 
      4}, {1, 4, 4}, {4, 1, 5}, {2, 2, 5}}}, 
    Private`componentsEdgesAndnuc[{2, 3, 3}, {1, 3, 
      2}] = {{}, {}, {}, {}, CompressedData["
1:eJxllItRxDAMRPXxJ1cGLVECDdAe5fHkzXlMyLzJQeTdSBsnH1/fn19hZj9u
lvzy9zBrXkRwyS+3EUXLIppN8+5FhjcurJVQq93PlaBl4OnhcaWNVrRexLAZ
3rPI5sOjRWyt9UJa4B6nEKQCukEYGWNYmzab917k8JHRWmwJ3YMkgMSoTovL
tB58OpLo0aePHm0EVXuZSkCJRPLyKjHAyznbFQSwMvAzIvW/BgoSeFENP6eG
i9Y87vHXIMwN1E8TZQi2bns66H6nA1DM0qQCof+LDDPuTNaMk1tEwfFw2z5E
BPiAfAjhNKmslgOHt1RoE8NW4HumqvQqwLUYFOO9kiZGdtwV6foXcqZfuWNU
CG75P7ozPY1P9xpcDfAkmfocnIONA80LnB6GSmAbej3qtg0fVvdQbytQFMyy
tkxBHfiVA2k85BKyU0FaQjiFMFBXxFXaF60XXOdtqRdmtkca/p6HEdT5/dT+
JrBexJ71Bne1rVtM69P0enWShu1watW5dtLWnqra2tl57J0Baoy7+bOqEiKd
1Z7Md2/AR0stSbg+MCO8uGzwDSCIYFe+F6ikL8Ku6kwG0ta2sgnd2Hm88/eZ
TclH8ReKkBGY
      "]}, 
    Private`componentsEdgesAndnuc[{2, 3, 3}, {3, 1, 
      2}] = {{}, {}, {}, {}, CompressedData["
1:eJxlU9F1xCAMk20M6RhdqRP03QLd/+/kiHPy0j6qMwbJMoTv19/PywH8GhD8
HYAzdBzANAyHxxUkLMzM7XDMwBgbPfEFW2bpFnEFEz7M+dcKLNCBpFpB2Fwb
V7BFuGdiheWwSDvMp/sI/ru4mBubKCQX4eT6grg2rblMslsllSG/O+2ArmHe
9rrE1eMniOos5PNhtYuuKhjh0cHDs6TK9iegptQe/oUSIVwB93A1fVE8IkaI
eG9TqP0bl5FCo6JYhtyDKue022xj6lfIe5WxR5vtqu7fhvBuT+UO0rnqG1ly
35Eu5SMipAK9CO+et89WODGZYczZrRcOrfLjE2qT86BXEDGHiv43X8eA5FBR
ldu1UNJC81TdxYzXqF7HVXrXPXcKZWDv5MnRe83KjKYckWkz7+XE5bvkANdO
+iTHavB0ps9q71RQpr7ZqCTPu55H7p3BR1YPcWWdQg1+zgMH3/0bCQgN5A==

      "]}, 
    Private`componentsEdgesAndnuc[{3, 0, 3}, {1, 0, 2}] = {{}, {}, {}, {}, {{
      3, 1, 0}, {2, 0, 1}, {4, 1, 1}, {3, 0, 2}, {5, 1, 2}, {4, 0, 3}, {2, 2, 
      1}, {1, 1, 2}, {3, 2, 2}, {2, 1, 3}, {1, 3, 2}, {0, 2, 3}, {3, 0, 2}, {
      4, 0, 3}, {5, 0, 4}, {2, 1, 3}, {3, 1, 4}, {1, 2, 4}}}, 
    Private`componentsEdgesAndnuc[{3, 0, 3}, {2, 2, 1}] = {{}, {}, {}, {}, {{
      3, 0, 0}, {1, 1, 0}, {5, 1, 0}, {3, 2, 0}, {1, 3, 0}, {4, 0, 1}, {2, 1, 
      1}, {3, 2, 0}, {1, 3, 0}, {2, 1, 1}, {0, 2, 1}, {5, 3, 0}, {3, 4, 0}, {
      4, 2, 1}, {2, 3, 1}, {3, 1, 2}, {1, 2, 2}, {3, 4, 0}, {1, 5, 0}, {2, 3, 
      1}, {0, 4, 1}, {1, 2, 2}, {4, 4, 1}, {3, 3, 2}, {2, 2, 3}, {2, 5, 1}, {
      1, 4, 2}, {0, 3, 3}, {4, 0, 1}, {2, 1, 1}, {0, 2, 1}, {1, 0, 2}, {6, 1, 
      1}, {4, 2, 1}, {2, 3, 1}, {5, 0, 2}, {3, 1, 2}, {1, 2, 2}, {2, 0, 3}, {
      4, 2, 1}, {2, 3, 1}, {0, 4, 1}, {3, 1, 2}, {1, 2, 2}, {0, 1, 3}, {5, 2, 
      2}, {3, 3, 2}, {4, 1, 3}, {2, 2, 3}, {1, 1, 4}, {3, 3, 2}, {1, 4, 2}, {
      2, 2, 3}, {0, 3, 3}, {4, 3, 3}, {3, 2, 4}, {2, 4, 3}, {1, 3, 4}, {5, 0, 
      2}, {3, 1, 2}, {1, 2, 2}, {2, 0, 3}, {0, 1, 3}, {6, 0, 3}, {4, 1, 3}, {
      2, 2, 3}, {3, 0, 4}, {1, 1, 4}, {4, 1, 3}, {2, 2, 3}, {0, 3, 3}, {1, 1, 
      4}, {5, 1, 4}, {3, 2, 4}, {2, 1, 5}, {3, 2, 4}, {1, 3, 4}, {0, 2, 5}, {
      4, 2, 5}, {2, 3, 5}, {3, 0, 4}, {1, 1, 4}, {0, 0, 5}, {4, 0, 5}, {2, 1, 
      5}, {1, 0, 6}, {2, 1, 5}, {0, 2, 5}, {3, 1, 6}, {1, 2, 6}, {1, 0, 6}, {
      2, 0, 7}, {0, 1, 7}}}, 
    Private`componentsEdgesAndnuc[{3, 1, 0}, {1, 0, 0}] = {{}, {}, {}, {}, {{
      0, 2, 2}, {1, 0, 3}, {0, 1, 4}}}, 
    Private`componentsEdgesAndnuc[{3, 1, 1}, {1, 3, 
      3}] = {{}, {}, {}, {}, CompressedData["
1:eJxNkIFxRCEQQoFdvWsjLaWEayC9pOK81Vwm/+OMIwisH6+vz1ckfbOK1dJT
2h50tOS89yClsnepWw97ZVDlNtyvAHAHQZYuC5Ksdi3vpGtw/pN3lGhw8dHA
pge9sqqqS1vDbs/5zpxMdlEPcPWW2QqgDCfA/+r1VMhNXxNSxCPwEfjShyUC
CtxoWgAEquiwd6SaGZA23y3DhPkroHpoUprx3ul+5/I822NbFMzYTpmaLAzP
0FfYbHhr98IQK14UE65zsX2jcNg/eFEGiA==
      "]}, 
    Private`componentsEdgesAndnuc[{3, 1, 1}, {2, 0, 1}] = {{}, {}, {}, {}, {{
      1, 3, 0}, {2, 1, 1}, {0, 2, 1}, {1, 0, 2}, {2, 3, 1}, {3, 1, 2}, {1, 2, 
      2}, {2, 0, 3}, {0, 4, 1}, {1, 2, 2}, {0, 1, 3}, {1, 2, 2}, {2, 0, 3}, {
      0, 1, 3}, {2, 2, 3}, {3, 0, 4}, {1, 1, 4}, {0, 3, 3}, {1, 1, 4}, {1, 1, 
      4}, {0, 0, 5}, {2, 1, 5}, {0, 2, 5}, {1, 0, 6}}}, 
    Private`componentsEdgesAndnuc[{3, 2, 0}, {0, 0, 2}] = {{}, {}, {}, {}, {{
      0, 2, 1}, {1, 2, 2}, {2, 2, 3}, {0, 1, 3}, {1, 1, 4}, {0, 0, 5}}}, 
    Private`componentsEdgesAndnuc[{3, 2, 0}, {1, 3, 1}] = {{}, {}, {}, {}, {{
      3, 0, 0}, {1, 1, 0}, {5, 1, 0}, {3, 2, 0}, {4, 0, 1}, {2, 1, 1}, {3, 2, 
      0}, {1, 3, 0}, {2, 1, 1}, {0, 2, 1}, {3, 4, 0}, {4, 2, 1}, {2, 3, 1}, {
      3, 1, 2}, {1, 2, 2}, {1, 5, 0}, {2, 3, 1}, {0, 4, 1}, {1, 2, 2}, {2, 5, 
      1}, {3, 3, 2}, {1, 4, 2}, {2, 2, 3}, {0, 6, 1}, {1, 4, 2}, {0, 3, 3}, {
      1, 6, 2}, {2, 4, 3}, {0, 5, 3}, {4, 0, 1}, {2, 1, 1}, {1, 0, 2}, {4, 2, 
      1}, {5, 0, 2}, {3, 1, 2}, {2, 0, 3}, {2, 3, 1}, {3, 1, 2}, {1, 2, 2}, {
      0, 1, 3}, {3, 3, 2}, {4, 1, 3}, {2, 2, 3}, {1, 1, 4}, {1, 4, 2}, {2, 2, 
      3}, {0, 3, 3}, {2, 4, 3}, {3, 2, 4}, {1, 3, 4}, {0, 5, 3}, {1, 3, 4}, {
      1, 5, 4}, {3, 1, 2}, {2, 0, 3}, {4, 1, 3}, {3, 0, 4}, {2, 2, 3}, {1, 1, 
      4}, {3, 2, 4}, {2, 1, 5}, {1, 3, 4}, {0, 2, 5}, {2, 3, 5}, {0, 4, 5}, {
      3, 0, 4}, {4, 0, 5}, {2, 1, 5}, {3, 1, 6}, {1, 2, 6}}}, 
    Private`componentsEdgesAndnuc[{3, 2, 0}, {2, 2, 0}] = {{}, {}, {}, {}, {{
      3, 2, 0}, {1, 3, 0}, {4, 0, 1}, {2, 1, 1}, {0, 2, 1}, {1, 5, 0}, {2, 3, 
      1}, {0, 4, 1}, {3, 1, 2}, {1, 2, 2}, {0, 6, 1}, {1, 4, 2}, {2, 2, 3}, {
      2, 3, 1}, {3, 1, 2}, {1, 2, 2}, {2, 0, 3}, {0, 1, 3}, {1, 4, 2}, {2, 2, 
      3}, {0, 3, 3}, {1, 1, 4}, {0, 5, 3}, {1, 3, 4}, {2, 2, 3}, {3, 0, 4}, {
      1, 1, 4}, {0, 0, 5}, {1, 3, 4}, {2, 1, 5}, {0, 2, 5}, {0, 4, 5}, {2, 1, 
      5}, {1, 0, 6}, {1, 2, 6}, {2, 0, 7}}}, 
    Private`componentsEdgesAndnuc[{3, 2, 0}, {3, 2, 0}] = {{}, {}, {}, {}, {{
      4, 1, 0}, {2, 2, 0}, {0, 3, 0}, {2, 4, 0}, {0, 5, 0}, {3, 2, 1}, {1, 3, 
      1}, {0, 7, 0}, {1, 5, 1}, {2, 3, 2}, {3, 2, 1}, {1, 3, 1}, {4, 0, 2}, {
      2, 1, 2}, {0, 2, 2}, {1, 5, 1}, {2, 3, 2}, {0, 4, 2}, {3, 1, 3}, {1, 2, 
      3}, {0, 6, 2}, {1, 4, 3}, {2, 2, 4}, {2, 3, 2}, {3, 1, 3}, {1, 2, 3}, {
      2, 0, 4}, {0, 1, 4}, {1, 4, 3}, {2, 2, 4}, {0, 3, 4}, {1, 1, 5}, {0, 5, 
      4}, {1, 3, 5}, {2, 2, 4}, {3, 0, 5}, {1, 1, 5}, {0, 0, 6}, {1, 3, 5}, {
      2, 1, 6}, {0, 2, 6}, {0, 4, 6}, {2, 1, 6}, {1, 0, 7}, {1, 2, 7}, {2, 0, 
      8}}}, Private`componentsEdgesAndnuc[{3, 2, 1}, {0, 0, 
      3}] = {{}, {}, {}, {}, {{1, 2, 0}, {2, 2, 1}, {3, 2, 2}, {4, 2, 3}, {0, 
      3, 1}, {1, 3, 2}, {2, 3, 3}, {1, 1, 2}, {2, 1, 3}, {3, 1, 4}, {0, 2, 
      3}, {1, 2, 4}, {1, 0, 4}, {2, 0, 5}, {0, 1, 5}}}, 
    Private`componentsEdgesAndnuc[{3, 2, 3}, {0, 0, 1}] = {{}, {}, {}, {}, {{
      3, 2, 2}, {4, 2, 3}, {2, 3, 3}, {3, 1, 4}}}, 
    Private`componentsEdgesAndnuc[{3, 2, 3}, {2, 0, 
      3}] = {{}, {}, {}, {}, CompressedData["
1:eJw9kMFxAzEMAwEQpNxGWnIJbsAlpPXgjpN7aKQhF0tJP5/v+yMAv1mV1dkI
Ci1UYUCTEtsY0cUDNVXSmEfq0gs1LKvC01g+lmqEl7k8S26GD7p8EhwEQHOB
bbC1XRzmfB1cqy0oe7TrNCq1OFd4D/RUHfpy+F975SrEasPEYVeYVJ8RKeY9
V8V5bSg/Iwg//vxNgoNOctSPEHlc+dZ13InngGtIJ7HBdPOpzUmLnPvik4px
xPMHNsUGOw==
      "]}, 
    Private`componentsEdgesAndnuc[{3, 3, 0}, {2, 1, 
      3}] = {{}, {}, {}, {}, CompressedData["
1:eJxFUMlxAzEMw0Wt20hLKcENpIPUHmhlOw/OEMRBSl/Pn++nAPy23GofQipg
DAVDWkzbwZgOFxVp4Mq2jsUF1VGUFqqpQFblXkw0MgYVNJrDUrs6tZGm+47h
CckW6RPSRXttjtwLKXN2fdK88Svw9sXjODu8ZHpqlF6Zl6v3flzIvwtT13D2
2tvbbzh2cprQefn9Qs/x4m3k7QqWuHJ+ocd5CtWrtQpxs8RlXnuiq73wAB9/
1NYFzw==
      "]}, 
    Private`componentsEdgesAndnuc[{3, 3, 1}, {3, 0, 0}] = {{}, {}, {}, {}, {{
      1, 6, 0}, {2, 4, 1}, {0, 5, 1}, {3, 2, 2}, {1, 3, 2}, {4, 0, 3}, {2, 1, 
      3}, {1, 5, 2}, {2, 3, 3}, {0, 4, 3}, {3, 1, 4}, {1, 2, 4}, {1, 4, 4}, {
      2, 2, 5}, {0, 3, 5}, {1, 3, 6}}}, 
    Private`componentsEdgesAndnuc[{3, 3, 2}, {0, 3, 
      2}] = {{}, {}, {}, {}, CompressedData["
1:eJxdkottAjEQBd9+TRtpKSXQQFpMaZn1IhQhncH3PLsebL6eP99Pl/TLiBkm
c7msXBGyVJq5W8hPqFJRarcM87QyD3eyJcGoB1/MWovF1M/otKSmrcIjPT0W
g1EZjIgvwCoGs14e7ZlBqGOjlUFix70iO5ioY+Qesao4YMv0w5P+ePF1S0fg
7hPXAjAXxuQtjM+S7LskGFBHjFIFMdO37f7gXSKnZfFSySuSdufW83wosZEp
P3xo5arDmd1dmuOfbWqZOYfLcFcL0H+Bhhm12pCE+9iER1X/+1P+7oylWR91
GWfaG1JLuAl3+Pq8pHRSZy7DDn8Q0+MPT90IyQ==
      "]}, 
    Private`componentsEdgesAndnuc[{3, 3, 3}, {1, 0, 2}] = {{}, {}, {}, {}, {{
      3, 4, 0}, {4, 2, 1}, {2, 3, 1}, {4, 4, 1}, {5, 2, 2}, {3, 3, 2}, {5, 4, 
      2}, {6, 2, 3}, {4, 3, 3}, {2, 5, 1}, {3, 3, 2}, {1, 4, 2}, {3, 5, 2}, {
      4, 3, 3}, {2, 4, 3}, {1, 6, 2}, {2, 4, 3}, {0, 5, 3}, {3, 3, 2}, {4, 1, 
      3}, {2, 2, 3}, {4, 3, 3}, {5, 1, 4}, {3, 2, 4}, {5, 3, 4}, {2, 4, 3}, {
      3, 2, 4}, {1, 3, 4}, {3, 4, 4}, {1, 5, 4}, {3, 2, 4}, {4, 0, 5}, {2, 1, 
      5}, {4, 2, 5}, {2, 3, 5}, {3, 1, 6}}}, 
    Private`componentsEdgesAndnuc[{3, 3, 3}, {3, 3, 
      2}] = {{}, {}, {}, {}, CompressedData["
1:eJx1l42V3DAIhEHox96rIi2lhDSQnlJpBo9uluhy75E5rW0Bgs+y8uPX75+/
mpn96WZhzz/3NPxdZsMt2laYh3Xz5mlu7cPsdlt4IN4DavStl/l0720rrIUP
g6TBK53DMxXW0neafMLP4RYuqHQLX1QYBI5hN5LB82NrzK0v9wv5YEp/D6ht
+IK0FrG1e+CvoiPikQCMoa1vhSH6ERTVpCoiYmlAZUSshJpxMX2lxWUXksST
c2tbfiMK8sTz4z2YLXrmHUdcmMKhahpQGRFmEQwH89sUi6ooPt+DHQ4P47HL
12hjZsmuiNmjj2gz6A1GP3ZtlRMq/KC+8TL4abfTD5okP/5huI4W8jov2gpi
KTjFkPA7OBQwRFFAonEP6SFIDkJYt9saOdHgwGaCDg8qEcKrE7n6HfdIQ/Aw
BzHMV+IrpdSK65GAKGJ0ajp20NnJEpt7sKQWUy+s4iFKgzdg3vGHeiQmrmDK
50iMhIM3KqzmU2E7IFECGlCfl7VTK4oHjWJpJT740TVQaJD5FU6hbiM0oD5N
7hXag1sqo8Dve4BncHfF6n0MLKlXvCvhVM7aejdMxMI40Vf3V8NPu4M/K9tU
odX2jp50fccw21RJpgonIh2533Xyw5axbrCXbWwOeFi9ZQlPVeGEvZPWfHwH
9sE2VTiRc9iREvv4FWbqQTVVUCEZ2rHFscvs1O0bJ0FFnUnTqNqyb/9kdSQj
unJLLGlQYZZdTTv2SWZSCRdvVEavWiEkThfyxN2+FUHqlksaYYdnvjNoZtVK
KcmUT+rAFYznqPTCeBcfIWpcPZ9Z2H+xTw27xn+xJAAVTiqOM+x7hfDgUE1n
kSuHVOxo9bvGdrPOLEVuA5Ytpqrdu+NYxmP4AlCPZNju/+JXCUTTqcduphzY
i4M3KizyS5Lmbav2H/adDWL9F1LFA4/mJv2o9qUjYg0kALK0bcq0X9UoiUHx
j88AlWAsXO9pmdp8s9E+K1qnwMjJfh4FgBtU+mGGP2E4Xfk12ffadPVdTWep
+T3TO4WbbHHtr1qs/rK8LQ+wS+8U0n6OS7uqXDJsGk58iT4Vhm1V4Y5ALOzR
RxhPewp0hNj1+XROhbHCKA4+m6gUDG9GT6yWXsPDCSdabIXRA2pbp8MmnKCH
zy1d5NEW1wEXzNYSqFiXyqh1sYYwrE40JiH2T7nyQ2QXDYc4LodBcfqGDU/L
LaldclKncy2YS9P0OtHzm3bhC4BdKXCeir2iepe3sBNQmST9Kz0Y/hfFrDix
2w3D/35gl90zN7u7xa0HeMvy7Pm+S7W+5+5Tsr2GvcJf3rY2+zD/+AsJAzHQ

      "]}, 
    Private`componentsEdgesAndnuc[{7, 0, 0}, {3, 0, 0}] = {{}, {}, {}, {}, {{
      0, 3, 4}, {0, 2, 6}, {0, 1, 8}, {0, 0, 10}}}, 
    Private`componentsEdgesAndnuc[{7, 0, 0}, {3, 0, 2}] = {{}, {}, {}, {}, {{
      0, 3, 2}, {1, 3, 3}, {2, 3, 4}, {0, 2, 4}, {1, 2, 5}, {2, 2, 6}, {0, 1, 
      6}, {1, 1, 7}, {2, 1, 8}, {0, 0, 8}, {1, 0, 9}, {2, 0, 10}}}, 
    Private`componentsEdgesAndnuc[{0, 0, 0, 0}, {0, 0, 0, 
      0}] = {{}, {}, {}, {}, {{0, 0, 0, 0}}}, 
    Private`componentsEdgesAndnuc[{1, 0, 0, 0}, {0, 0, 1, 
      0}] = {{}, {}, {}, {}, {{1, 0, 0, 0}, {0, 1, 0, 1}}}, 
    Private`componentsEdgesAndnuc[{1, 3, 1, 2}, {2, 1, 0}] = {{}, {}, {}, {}, 
      
      ReplaceAll[{
        Private`lambda[0, 0, 1], 
        Private`lambda[0, 0, 2], 
        Private`lambda[0, 0, 3], 
        Private`lambda[0, 0, 4]}, {
        ToRules[
         Reduce[
          And[
           ReplaceAll[
            And[
            Private`lambda[1, 0, 0] == 
             Private`j[1, 0, 4] + Private`j[1, 1, 3] + Private`j[1, 2, 2] + 
              Private`j[1, 3, 1] + Private`j[1, 4, 0], 
             Private`lambda[2, 0, 0] == 
             Private`j[2, 0, 3] + Private`j[2, 1, 2] + Private`j[2, 2, 1] + 
              Private`j[2, 3, 0], Private`lambda[3, 0, 0] == 
             Private`j[3, 0, 2] + Private`j[3, 1, 1] + Private`j[3, 2, 0], 
             Private`lambda[4, 0, 0] == 
             Private`j[4, 0, 1] + Private`j[4, 1, 0], Private`lambda[0, 1, 0] == 
             Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
              Private`j[3, 1, 1] + Private`j[4, 1, 0], 
             Private`lambda[0, 2, 0] == 
             Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] + 
              Private`j[3, 2, 0], Private`lambda[0, 3, 0] == 
             Private`j[0, 3, 2] + Private`j[1, 3, 1] + Private`j[2, 3, 0], 
             Private`lambda[0, 4, 0] == 
             Private`j[0, 4, 1] + Private`j[1, 4, 0], Private`lambda[0, 0, 1] == 
             Private`j[0, 4, 1] + Private`j[1, 3, 1] + Private`j[2, 2, 1] + 
              Private`j[3, 1, 1] + Private`j[4, 0, 1], 
             Private`lambda[0, 0, 2] == 
             Private`j[0, 3, 2] + Private`j[1, 2, 2] + Private`j[2, 1, 2] + 
              Private`j[3, 0, 2], Private`lambda[0, 0, 3] == 
             Private`j[0, 2, 3] + Private`j[1, 1, 3] + Private`j[2, 0, 3], 
             Private`lambda[0, 0, 4] == 
             Private`j[0, 1, 4] + Private`j[1, 0, 4]], {
            Private`lambda[1, 0, 0] -> 1, Private`lambda[2, 0, 0] -> 3, 
             Private`lambda[3, 0, 0] -> 1, Private`lambda[4, 0, 0] -> 2, 
             MapThread[Rule, {{
                Private`lambda[0, 1, 0], 
                Private`lambda[0, 2, 0], 
                Private`lambda[0, 3, 0], 
                Private`lambda[0, 4, 0]}, {2, 1, 0}}]}], Private`j[1, 4, 0] >= 
           0, Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 2, 2] + Private`j[1, 3, 1] + Private`j[1, 4, 0] >= 0, 
           Private`j[1, 1, 3] + Private`j[1, 2, 2] + Private`j[1, 3, 1] + 
            Private`j[1, 4, 0] >= 0, Private`j[2, 3, 0] >= 0, 
           Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[2, 1, 2] + Private`j[2, 2, 1] + Private`j[2, 3, 0] >= 0, 
           Private`j[3, 2, 0] >= 0, Private`j[3, 1, 1] + Private`j[3, 2, 0] >= 
           0, Private`j[4, 1, 0] >= 0, Private`j[0, 1, 4] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] >= 0, 
           Private`j[0, 1, 4] + Private`j[1, 1, 3] + Private`j[2, 1, 2] + 
            Private`j[3, 1, 1] >= 0, Private`j[0, 2, 3] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] >= 0, 
           Private`j[0, 2, 3] + Private`j[1, 2, 2] + Private`j[2, 2, 1] >= 0, 
           Private`j[0, 3, 2] >= 0, Private`j[0, 3, 2] + Private`j[1, 3, 1] >= 
           0, Private`j[0, 4, 1] >= 0, Private`j[4, 0, 1] >= 0, 
           Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[2, 2, 1] + Private`j[3, 1, 1] + Private`j[4, 0, 1] >= 0, 
           Private`j[1, 3, 1] + Private`j[2, 2, 1] + Private`j[3, 1, 1] + 
            Private`j[4, 0, 1] >= 0, Private`j[3, 0, 2] >= 0, 
           Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[1, 2, 2] + Private`j[2, 1, 2] + Private`j[3, 0, 2] >= 0, 
           Private`j[2, 0, 3] >= 0, Private`j[1, 1, 3] + Private`j[2, 0, 3] >= 
           0, Private`j[1, 0, 4] >= 0], {
           Private`j[0, 1, 4], 
           Private`j[0, 2, 3], 
           Private`j[0, 3, 2], 
           Private`j[0, 4, 1], 
           Private`j[1, 0, 4], 
           Private`j[1, 1, 3], 
           Private`j[1, 2, 2], 
           Private`j[1, 3, 1], 
           Private`j[1, 4, 0], 
           Private`j[2, 0, 3], 
           Private`j[2, 1, 2], 
           Private`j[2, 2, 1], 
           Private`j[2, 3, 0], 
           Private`j[3, 0, 2], 
           Private`j[3, 1, 1], 
           Private`j[3, 2, 0], 
           Private`j[4, 0, 1], 
           Private`j[4, 1, 0]}, Integers]]}]}, 
    Private`componentsEdgesAndnuc[{2, 0, 0, 0}, {0, 0, 0, 
      0}] = {{}, {}, {}, {}, {{0, 0, 0, 2}}}, 
    Private`componentsEdgesAndnuc[{2, 0, 3, 0}, {0, 0, 0, 
      0}] = {{}, {}, {}, {}, {{0, 3, 0, 2}}}, 
    Private`componentsEdgesAndnuc[{2, 0, 3, 0}, {1, 0, 0, 
      0}] = {{}, {}, {}, {}, {{0, 3, 0, 3}, {0, 3, 1, 1}, {1, 2, 0, 2}}}, 
    Private`componentsEdgesAndnuc[{2, 0, 3, 0}, {1, 1, 0, 
      0}] = {{}, {}, {}, {}, {{0, 2, 0, 3}, {0, 2, 1, 1}, {0, 3, 1, 3}, {0, 3,
       2, 1}, {0, 4, 0, 2}, {0, 4, 1, 0}, {1, 1, 0, 2}, {1, 2, 0, 4}, {1, 2, 
      1, 2}, {1, 2, 1, 2}, {1, 2, 2, 0}, {1, 3, 0, 1}, {2, 1, 0, 3}, {2, 1, 1,
       1}}}, Private`componentsEdgesAndnuc[{2, 0, 3, 0}, {1, 1, 0, 
      5}] = {{}, {}, {}, {}, CompressedData["
1:eJxV0YGRgzAMRFHtSiZ1pKUrIQ1cH1f1faEBE+AxY8bI8vr9+f35OCL+UNFX
KWSHMsKS+hVlxYoeSU6J5+Lr9vWbHPbIGJWOpS63maLpreJEM/tjl32g1KAr
sFRXz5G6TZMPdHHLTBXF40HVi21Uaf2c2OlTdVbScBWzR3V6YMqN4k8E823S
Hc6i28qootltEmQhNU7Co1NlfFKRsEfGYqub9IWjXQSx2PhiA4umD2I4OIGD
aAffWPIg9uE4CPxF5C/O7x/2pAZs
      "]}, 
    Private`componentsEdgesAndnuc[{2, 4, 3, 0}, {1, 1, 0, 
      5}] = {{}, {}, {}, {}, CompressedData["
1:eJyN1Y154yAMgGGQkHDH6EodoQvcjjfZfSDXhEDtq583T5rENujPn99/vr4l
pfSXl5LanzrvSjAJfJTcUqoajtzlXFMWG1SHkk8erKxc9mq6CDfvchlETjWo
7RW9Z3lL+3Ekzb6Sck9lj3A2Jdks60pyxw4IoLMoAqm818oXNjPd87yqXOeQ
8EH6kuWcFUfO4istz1joLcpiZsF1VfNCUh2y3RO9xy1mHkrZM7lHWb9aMsmq
hhoosC3Ve7+Fn8RefjtEVq1FYVR110KdK01lw2sBE45Ag1qZuezRmpeDa3wQ
CL7oXouEZg6VQNmq6LOH0sheQpXZkTo2iYMPfCXlGaffamNrYsF0j7LcUQby
hOpZEMnBg5a9tvQ77yO1L78ykWzGXbYo/ndMvcCWOoKfxBm6ZeDkCwUaTJPr
jJZ6xZCjs21gl5frYNiUsmrhfPL+SKhc98itQc6Q131CaeJHjIxbVoLLrKbu
CgoNsSCVCyI9WOCnW21EbbQnXowip1zKjF1tcdIP42F1Xe7sdj4thHOwPHjp
5dcdTCSaehrarxHj7GCUqK7ayHzTB1rWc0x6a5DV/xxXkZ9aeGpuRRZ2odkl
rpRAi008dWwvNk4BciN+qDN2/4oQ/kyeYq2r+JcxMjhRFpuzTLQuxiVcQk0t
XVxKzmFk76sMnPVO2+0bKnpBprt2OHekLEgKQSORfZNgtf2GrQv5FY8/J20t
g4O1p5B661oixIoleGsSDb1w25OhZ6eyw0pH/QOEth+D
      "]}, 
    Private`componentsEdgesAndnuc[{7, 0, 0, 0}, {3, 0, 0, 
      0}] = {{}, {}, {}, {}, {{0, 0, 3, 4}, {0, 0, 2, 6}, {0, 0, 1, 8}, {0, 0,
       0, 10}}}, 
    Private`componentsEdgesAndnuc[{7, 0, 0, 0}, {3, 0, 2, 
      0}] = {{}, {}, {}, {}, {{2, 0, 3, 2}, {1, 1, 3, 3}, {0, 2, 3, 4}, {2, 0,
       2, 4}, {1, 1, 2, 5}, {0, 2, 2, 6}, {2, 0, 1, 6}, {1, 1, 1, 7}, {0, 2, 
      1, 8}, {2, 0, 0, 8}, {1, 1, 0, 9}, {0, 2, 0, 10}}}, 
    Private`componentsEdgesAndnuc[{7, 0, 0, 1}, {3, 0, 0, 
      0}] = {{}, {}, {}, {}, {{1, 0, 3, 4}, {0, 0, 2, 5}, {1, 0, 2, 6}, {0, 0,
       1, 7}, {1, 0, 1, 8}, {0, 0, 0, 9}, {1, 0, 0, 10}}}, 
    Private`componentsEdgesAndnuc[{7, 0, 0, 1}, {3, 0, 2, 
      0}] = {{}, {}, {}, {}, {{3, 0, 3, 2}, {1, 1, 3, 2}, {2, 0, 2, 3}, {2, 1,
       3, 3}, {0, 2, 3, 3}, {1, 1, 2, 4}, {1, 2, 3, 4}, {0, 2, 2, 5}, {1, 0, 
      4, 3}, {0, 1, 4, 4}, {3, 0, 2, 4}, {1, 1, 2, 4}, {2, 0, 1, 5}, {2, 1, 2,
       5}, {0, 2, 2, 5}, {1, 1, 1, 6}, {1, 2, 2, 6}, {0, 2, 1, 7}, {1, 0, 3, 
      5}, {0, 1, 3, 6}, {3, 0, 1, 6}, {1, 1, 1, 6}, {2, 0, 0, 7}, {2, 1, 1, 
      7}, {0, 2, 1, 7}, {1, 1, 0, 8}, {1, 2, 1, 8}, {0, 2, 0, 9}, {1, 0, 2, 
      7}, {0, 1, 2, 8}, {3, 0, 0, 8}, {1, 1, 0, 8}, {2, 1, 0, 9}, {0, 2, 0, 
      9}, {1, 2, 0, 10}, {1, 0, 1, 9}, {0, 1, 1, 10}}}, 
    Private`componentsEdgesAndnuc[{1, 1, 2}, {2, 1, 0}, {2, 1, 
      1}] = {{}, {}, {}, {}, {{2, 1, 1}, {2, 1, 1}, {2, 1, 1}}}, 
    Private`componentsEdgesAndnuc[{1, 1, 2}, {2, 1, 0}, {2, 5, 1}] = 
    Return[{0, 0, 0, 0, 0}], 
    Private`componentsEdgesAndnuc[{1, 1, 2}, {2, 1, 0}, {3, 0, 
      0}] = {{}, {}, {}, {}, {{3, 0, 0}}}, 
    Private`componentsEdgesAndnuc[{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
      0}] = {{}, {}, {}, {}, {{0, 0, 0, 0}}}, 
    Private`componentsEdgesAndnuc[{2, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}] = 
    Return[{0, 0, 0, 0, 0}], 
    Private`componentsEdgesAndnuc[{2, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
      2}] = {{}, {}, {}, {}, {{0, 0, 0, 2}}}, 
    Private`componentsEdgesAndnuc[{2, 0, 0, 0}, {0, 2, 0, 0}, {0, 0, 0, 2}] = 
    Return[{0, 0, 0, 0, 0}], 
    Private`componentsEdgesAndnuc[{2, 0, 3, 0}, {0, 2, 0, 0}, {0, 0, 0, 2}] = 
    Return[{0, 0, 0, 0, 0}], 
    Private`componentsEdgesAndnuc[{2, 0, 3, 0}, {1, 1, 0, 0}, {0, 0, 0, 2}] = 
    Return[{0, 0, 0, 0, 0}], 
    Private`componentsEdgesAndnuc[{2, 0, 3, 0}, {1, 1, 0, 0}, {0, 0, 1, 2}] = 
    Return[{0, 0, 0, 0, 0}], 
    Private`componentsEdgesAndnuc[{2, 0, 3, 0}, {1, 1, 0, 0}, {0, 2, 1, 2}] = 
    Return[{0, 0, 0, 0, 0}], 
    Private`componentsEdgesAndnuc[{2, 0, 3, 0}, {1, 1, 0, 0}, {1, 2, 1, 
      2}] = {{}, {}, {}, {}, {{1, 2, 1, 2}, {1, 2, 1, 2}}}, 
    Private`componentsEdgesAndnuc[{2, 0, 3, 0}, {1, 2, 0, 0}, {0, 0, 0, 2}] = 
    Return[{0, 0, 0, 0, 0}], Private`componentsEdgesAndnuc[{
       Pattern[Private`lam$, 
        BlankSequence[]]}, {
       Pattern[Private`mu$, 
        BlankSequence[]]}] := (
     Private`componentsEdgesAndnuc[{Private`lam$}, {Private`mu$}] = 
     Module[{Private`redsol$, Private`locrules$, Private`nuclist$, 
        Private`fundcompval$, Private`edgesEWValues$, 
        Private`edgesNESWValues$, Private`edgesNWSEValues$}, 
       Private`redsol$ = Solve[
          And[
           ReplaceAll[Private`lambdaequations, 
            Private`buildlambdainput[{Private`lam$}, {Private`mu$}]], 
           Private`positivityconstraints], Integers]; 
       Private`nuclist$ = ReplaceAll[
          Table[
           Private`lambda[0, 0, Private`p], {Private`p, 1, 4 - 1}], 
          Private`redsol$]; {{}, {}, {}, {}, Private`nuclist$}]), 
    Private`componentsEdgesAndnuc[{
       Pattern[Private`lam$, 
        BlankSequence[]]}, {
       Pattern[Private`mu$, 
        BlankSequence[]]}, {
       Pattern[Private`nuc$, 
        BlankSequence[]]}] := (
     Private`componentsEdgesAndnuc[{Private`lam$}, {Private`mu$}, {
       Private`nuc$}] = 
     Module[{Private`redsol$, Private`locrules$, Private`nuclist$, 
        Private`fundcompval$, Private`edgesEWValues$, 
        Private`edgesNESWValues$, Private`edgesNWSEValues$}, 
       Private`redsol$ = Reduce[
          And[
           ReplaceAll[Private`lambdaequations, 
            
            Private`buildlambdainput[{Private`lam$}, {Private`mu$}, {
             Private`nuc$}]], Private`positivityconstraints], 
          Private`fundamentalcomponents, Integers]; 
       Private`redsol$ = And[Private`redsol$, 
          Apply[And, 
           Table[
           Private`lambda[0, 0, Private`p] == 
            Part[{Private`nuc$}, Private`p], {Private`p, 1, 5 - 1}]]]; 
       If[Private`redsol$ =!= False, Private`locrules$ = 
         ToRules[Private`redsol$], 
         Return[{0, 0, 0, 0, 0}]]; Private`nuclist$ = ReplaceAll[
          Table[
           Private`lambda[0, 0, Private`p], {Private`p, 1, 5 - 1}], {
          Private`locrules$}]; {{}, {}, {}, {}, Private`nuclist$}]), 
    Attributes[Private`lam$] = {Temporary}, 
    Attributes[Private`mu$] = {Temporary}, 
    Attributes[Private`redsol$] = {Temporary}, 
    Attributes[Private`locrules$] = {Temporary}, 
    Attributes[Private`nuclist$] = {Temporary}, 
    Attributes[Private`fundcompval$] = {Temporary}, 
    Attributes[Private`edgesEWValues$] = {Temporary}, 
    Attributes[Private`edgesNESWValues$] = {Temporary}, 
    Attributes[Private`edgesNWSEValues$] = {Temporary}, 
    Attributes[Private`nuc$] = {Temporary}, Private`tensorproductCC[{
       Pattern[Private`lam, 
        BlankSequence[]]}, {
       Pattern[Private`mu, 
        BlankSequence[]]}] := Part[
      Private`componentsEdgesAndnuc[{Private`lam}, {Private`mu}], 5], 
    Private`tensorproductCC[{
       Pattern[Private`lam, 
        BlankSequence[]]}, {
       Pattern[Private`mu, 
        BlankSequence[]]}, {
       Pattern[Private`nuc, 
        BlankSequence[]]}] := Part[
      Private`componentsEdgesAndnuc[{Private`lam}, {Private`mu}, {
       Private`nuc}], 5], Attributes[$CellContext`makeRow$] = {Temporary}, 
    Attributes[$CellContext`rowIndex$] = {Temporary}, 
    Attributes[$CellContext`colIndex$] = {Temporary}}]], "Output", \
"PluginEmbeddedContent",ExpressionUUID->"00000000-0000-0000-0000-\
000000000000"]
}, Open  ]],

Cell[CellGroupData[{

Cell["CAPTION", "Subsection", "PluginEmbeddedContent",
 CellFrame->{{0, 0}, {1, 0}},
 CellFrameColor->RGBColor[0.87, 0.87, 0.87],
 FontFamily->"Helvetica",
 FontSize->12,
 FontWeight->"Bold",
 FontColor->RGBColor[
  0.597406, 0, 
   0.0527047],ExpressionUUID->"00000000-0000-0000-0000-000000000000"],

Cell["\<\
The program decomposes the product of two irreducible representations of \
SU(n) into a direct sum.\
\>", "Text", \
"PluginEmbeddedContent",ExpressionUUID->"00000000-0000-0000-0000-\
000000000000"]
}, Open  ]],

Cell[CellGroupData[{

Cell["DETAILS", "Subsection", "PluginEmbeddedContent",
 CellFrame->{{0, 0}, {1, 0}},
 CellFrameColor->RGBColor[0.87, 0.87, 0.87],
 FontFamily->"Helvetica",
 FontSize->12,
 FontWeight->"Bold",
 FontColor->RGBColor[
  0.597406, 0, 
   0.0527047],ExpressionUUID->"00000000-0000-0000-0000-000000000000"],

Cell[TextData[{
 "The decomposition of the tensor product of irreducible representations is \
done by using an improved hive technique developed by the author and inspired \
from [1], [2], [3]. \nIn order to speed up the calculation we use a basis of \
elementary pictographs where syzygies (constraints) are eliminated before the \
actual reduction of the system of inequalities takes place.\nPictographs \
themselves (BZ-triangles, KT-honeycombs, O-blades), corresponding to a chosen \
space of intertwiners,  are not displayed here :  see the companion programs \
HYPERLINK. \n\n[1] ",
 Cell[BoxData[
  TagBox[
   ButtonBox[
    PaneSelectorBox[{False->
     TagBox["\<\"Triple multiplicities for sl(r+1) and the spectrum of the \
exterior algebra of the adjoint representation,A.D. Berenstein and A.Z. \
Zelevinsky (1992)\"\>",
      StatusArea[#, "J. Algebraic Combinatorics 1, 7 (1992)"]& ,
      TagBoxNote->"J. Algebraic Combinatorics 1, 7 (1992)"], True->
     StyleBox[
      TagBox["\<\"Triple multiplicities for sl(r+1) and the spectrum of the \
exterior algebra of the adjoint representation,A.D. Berenstein and A.Z. \
Zelevinsky (1992)\"\>",
       StatusArea[#, "J. Algebraic Combinatorics 1, 7 (1992)"]& ,
       TagBoxNote->"J. Algebraic Combinatorics 1, 7 (1992)"], 
      "HyperlinkActive"]}, Dynamic[
      CurrentValue["MouseOver"]],
     BaseStyle->{"Hyperlink"},
     FrameMargins->0,
     ImageSize->Automatic],
    BaseStyle->"Hyperlink",
    ButtonData->{
      URL[
      "http://emis.ams.org/journals/JACO/Volume1_1/h7x307752t449r38.fulltext.\
pdf"], None},
    ButtonNote->
     "http://emis.ams.org/journals/JACO/Volume1_1/h7x307752t449r38.fulltext.\
pdf"],
   Annotation[#, 
    "http://emis.ams.org/journals/JACO/Volume1_1/h7x307752t449r38.fulltext.\
pdf", "Hyperlink"]& ]],
  CellChangeTimes->{3.714119849614003*^9},ExpressionUUID->
  "0125ca03-c7e2-4afb-97cd-f95545468b42"],
 "\n[2] ",
 Cell[BoxData[
  TagBox[
   ButtonBox[
    PaneSelectorBox[{False->
     TagBox["\<\"Berenstein-Zelevinsky triangles, elementary couplings and \
fusion rules, L. B\[EAcute]gin, A.N. Kirillov, P. Mathieu and M.A. Walton \
(1993)\"\>",
      StatusArea[#, 
       "Letters in Mathematical Physics, Volume 28, Issue 4, (1993) pp \
257-268"]& ,
      TagBoxNote->
       "Letters in Mathematical Physics, Volume 28, Issue 4, (1993) pp \
257-268"], True->
     StyleBox[
      TagBox["\<\"Berenstein-Zelevinsky triangles, elementary couplings and \
fusion rules, L. B\[EAcute]gin, A.N. Kirillov, P. Mathieu and M.A. Walton \
(1993)\"\>",
       StatusArea[#, 
        "Letters in Mathematical Physics, Volume 28, Issue 4, (1993) pp \
257-268"]& ,
       TagBoxNote->
        "Letters in Mathematical Physics, Volume 28, Issue 4, (1993) pp \
257-268"], "HyperlinkActive"]}, Dynamic[
      CurrentValue["MouseOver"]],
     BaseStyle->{"Hyperlink"},
     FrameMargins->0,
     ImageSize->Automatic],
    BaseStyle->"Hyperlink",
    ButtonData->{
      URL["https://arxiv.org/abs/hep-th/9301075"], None},
    ButtonNote->"https://arxiv.org/abs/hep-th/9301075"],
   Annotation[#, "https://arxiv.org/abs/hep-th/9301075", "Hyperlink"]& ]],
  CellChangeTimes->{
   3.714117744715974*^9, {3.71411778107472*^9, 3.7141178076777773`*^9}, 
    3.714117838620057*^9, 3.7141179102260942`*^9, {3.714117995649395*^9, 
    3.714118013338854*^9}, 3.714118157897002*^9},ExpressionUUID->
  "8251d30a-09c4-4ba9-843d-687a38dbe80a"],
 "\n[3] ",
 Cell[BoxData[
  TagBox[
   ButtonBox[
    PaneSelectorBox[{False->
     TagBox["\<\"The honeycomb model of GLn(C)tensor products I: proof of the \
saturation conjecture, A.Knutson and T.Tao (1999)\"\>",
      StatusArea[#, "J.Amer.Math.Soc.12 (1999)1055-1090"]& ,
      TagBoxNote->"J.Amer.Math.Soc.12 (1999)1055-1090"], True->
     StyleBox[
      TagBox["\<\"The honeycomb model of GLn(C)tensor products I: proof of \
the saturation conjecture, A.Knutson and T.Tao (1999)\"\>",
       StatusArea[#, "J.Amer.Math.Soc.12 (1999)1055-1090"]& ,
       TagBoxNote->"J.Amer.Math.Soc.12 (1999)1055-1090"], "HyperlinkActive"]},
      Dynamic[
      CurrentValue["MouseOver"]],
     BaseStyle->{"Hyperlink"},
     FrameMargins->0,
     ImageSize->Automatic],
    BaseStyle->"Hyperlink",
    ButtonData->{
      URL["https://arxiv.org/abs/math/9807160"], None},
    ButtonNote->"https://arxiv.org/abs/math/9807160"],
   Annotation[#, "https://arxiv.org/abs/math/9807160", "Hyperlink"]& ]],
  CellChangeTimes->{{3.7141193542094316`*^9, 3.714119377260558*^9}},
  ExpressionUUID->"7b4956c6-ef60-4111-b8e0-aab94234b969"]
}], "Text", "PluginEmbeddedContent",
 FontSize->12,ExpressionUUID->"00000000-0000-0000-0000-000000000000"],

Cell[TextData[{
 StyleBox["The code is made of two parts :\n- A Mathematica package called \
shorttensorProdIrrepPackage available there HYPERLINK. It can be run into any \
Mathematica session. It does not contain any limitation.\n The main symbol \
exported by the package is the function tensorproduct (see also the related \
command tenspromatform).\n- A dynamic module that builds the user interface. \
This latter code is a bit convoluted because it is supposed to run not only \
in Mathematica but also with the various versions of the CDF Reader. As of \
September 2017, the IOS version of the CDF Reader does not accept nested \
manipulate commands (this would have been the easiest solution).",
  FontSize->12,
  FontWeight->"Regular"],
 "\n"
}], "Text", \
"PluginEmbeddedContent",ExpressionUUID->"00000000-0000-0000-0000-\
000000000000"]
}, Open  ]],

Cell[CellGroupData[{

Cell["CAVEATS", "Subsection", "PluginEmbeddedContent",
 CellFrame->{{0, 0}, {1, 0}},
 CellFrameColor->RGBColor[0.87, 0.87, 0.87],
 FontFamily->"Helvetica",
 FontSize->12,
 FontWeight->"Bold",
 FontColor->RGBColor[
  0.597406, 0, 
   0.0527047],ExpressionUUID->"00000000-0000-0000-0000-000000000000"],

Cell[CellGroupData[{

Cell[BoxData[
 InterpretationBox[
  RowBox[{
   StyleBox["\<\"Web version: The number of inequivalent irreps generated by \
the program should be smaller than \"\>",
    StripOnInput->False,
    FontSize->12,
    FontWeight->Bold], "\[InvisibleSpace]", 
   StyleBox["800",
    StripOnInput->False,
    FontSize->12,
    FontWeight->Bold]}],
  SequenceForm[
   Style["Web version: The number of inequivalent irreps generated by the \
program should be smaller than ", 12, Bold], 
   Style[800, 12, Bold]],
  Editable->False]], "Print", \
"PluginEmbeddedContent",ExpressionUUID->"00000000-0000-0000-0000-\
000000000000"],

Cell[BoxData[
 InterpretationBox[
  RowBox[{
   StyleBox["\<\"Web version: the computation is terminated if it takes more \
than \"\>",
    StripOnInput->False,
    FontSize->12,
    FontWeight->Bold], "\[InvisibleSpace]", 
   StyleBox["1.5`",
    StripOnInput->False,
    FontSize->12,
    FontWeight->Bold], "\[InvisibleSpace]", 
   StyleBox["\<\" seconds\"\>",
    StripOnInput->False,
    FontSize->12,
    FontWeight->Bold]}],
  SequenceForm[
   Style["Web version: the computation is terminated if it takes more than ", 
    12, Bold], 
   Style[1.5, 12, Bold], 
   Style[" seconds", 12, Bold]],
  Editable->False]], "Print", \
"PluginEmbeddedContent",ExpressionUUID->"00000000-0000-0000-0000-\
000000000000"]
}, Open  ]]
}, Open  ]],

Cell[CellGroupData[{

Cell["CITATION", "Subsection", "PluginEmbeddedContent",
 CellFrame->{{0, 0}, {1, 0}},
 CellFrameColor->RGBColor[0.87, 0.87, 0.87],
 FontFamily->"Helvetica",
 FontSize->12,
 FontWeight->"Bold",
 FontColor->RGBColor[
  0.597406, 0, 
   0.0527047],ExpressionUUID->"00000000-0000-0000-0000-000000000000"],

Cell["\<\
If you use this program in a scientific publication or talk, please give \
proper academic credit to the author :\
\>", "Text", \
"PluginEmbeddedContent",ExpressionUUID->"00000000-0000-0000-0000-\
000000000000"],

Cell[TextData[{
 Cell[BoxData[
  TagBox[
   ButtonBox[
    PaneSelectorBox[{False->
     TagBox["\<\"Robert Coquereaux\"\>",
      StatusArea[#, "Web page of Robert Coquereaux"]& ,
      TagBoxNote->"Web page of Robert Coquereaux"], True->
     StyleBox[
      TagBox["\<\"Robert Coquereaux\"\>",
       StatusArea[#, "Web page of Robert Coquereaux"]& ,
       TagBoxNote->"Web page of Robert Coquereaux"], "HyperlinkActive"]}, 
     Dynamic[
      CurrentValue["MouseOver"]],
     BaseStyle->{"Hyperlink"},
     FrameMargins->0,
     ImageSize->Automatic],
    BaseStyle->"Hyperlink",
    ButtonData->{
      URL["http://www.cpt.univ-mrs.fr/~coque"], None},
    ButtonNote->"http://www.cpt.univ-mrs.fr/~coque"],
   Annotation[#, "http://www.cpt.univ-mrs.fr/~coque", "Hyperlink"]& ]],
  CellChangeTimes->{3.713765397463574*^9},
  FontSize->12,ExpressionUUID->"07cb93ff-6585-477d-980b-d42d3ae723c6"],
 StyleBox["\n",
  FontSize->12],
 Cell[BoxData[
  TagBox[
   ButtonBox[
    PaneSelectorBox[{False->
     PaneSelectorBox[{False->
      TagBox["\<\"Product of irreducible representations for SU(n)\"\>",
       StatusArea[#, "cdf file tensorProdIrrep"]& ,
       TagBoxNote->"cdf file tensorProdIrrep"], True->
      TagBox["\<\"Product of irreducible representations for SU(n)\"\>",
       StatusArea[#, "cdf file tensorProdIrrep"]& ,
       TagBoxNote->"cdf file tensorProdIrrep"]}, Dynamic[
       CurrentValue["MouseOver"]],
      BaseStyle->{"Hyperlink"},
      FrameMargins->0,
      ImageSize->Automatic], True->
     StyleBox[
      PaneSelectorBox[{False->
       TagBox["\<\"Product of irreducible representations for SU(n)\"\>",
        StatusArea[#, "cdf file tensorProdIrrep"]& ,
        TagBoxNote->"cdf file tensorProdIrrep"], True->
       TagBox["\<\"Product of irreducible representations for SU(n)\"\>",
        StatusArea[#, "cdf file tensorProdIrrep"]& ,
        TagBoxNote->"cdf file tensorProdIrrep"]}, Dynamic[
        CurrentValue["MouseOver"]],
       BaseStyle->{"Hyperlink"},
       FrameMargins->0,
       ImageSize->Automatic], "HyperlinkActive"]}, Dynamic[
      CurrentValue["MouseOver"]],
     BaseStyle->{"Hyperlink"},
     FrameMargins->0,
     ImageSize->Automatic],
    BaseStyle->"Hyperlink",
    ButtonData->{
      URL["http://www.cpt.univ-mrs.fr/~coque/tensorProdIrrep.cdf"], None},
    ButtonNote->"http://www.cpt.univ-mrs.fr/~coque/tensorProdIrrep.cdf"],
   Annotation[#, "http://www.cpt.univ-mrs.fr/~coque/tensorProdIrrep.cdf", 
    "Hyperlink"]& ]],
  CellChangeTimes->{3.714287246644162*^9},ExpressionUUID->
  "6c1f706d-c14c-4547-918a-3924b8c3a9e9"],
 "\n",
 StyleBox["September 2017",
  FontSize->12,
  FontWeight->"Regular"]
}], "Text", \
"PluginEmbeddedContent",ExpressionUUID->"00000000-0000-0000-0000-\
000000000000"]
}, Open  ]]
},
WindowSize->{1087.8333333333333`, 841.},
Visible->True,
AuthoredSize->{1088, 841},
ScrollingOptions->{"HorizontalScrollRange"->Fit,
"VerticalScrollRange"->Fit},
ShowCellBracket->False,
Deployed->True,
CellContext->Notebook,
TrackCellChangeTimes->False,
FrontEndVersion->"11.1 for Mac OS X x86 (32-bit, 64-bit Kernel) (March 16, \
2017)",
StyleDefinitions->"Default.nb"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[CellGroupData[{
Cell[1486, 35, 396, 11, 22, "Subsection"],
Cell[1885, 48, 106362, 1919, 428, "Output"]
}, Open  ]],
Cell[CellGroupData[{
Cell[108284, 1972, 299, 8, 22, "Subsection"],
Cell[108586, 1982, 207, 5, 16, "Text"]
}, Open  ]],
Cell[CellGroupData[{
Cell[108830, 1992, 299, 8, 22, "Subsection"],
Cell[109132, 2002, 4651, 107, 128, "Text"],
Cell[113786, 2111, 847, 15, 101, "Text"]
}, Open  ]],
Cell[CellGroupData[{
Cell[114670, 2131, 299, 8, 22, "Subsection"],
Cell[CellGroupData[{
Cell[114994, 2143, 618, 18, 16, "Print"],
Cell[115615, 2163, 715, 23, 16, "Print"]
}, Open  ]]
}, Open  ]],
Cell[CellGroupData[{
Cell[116379, 2192, 300, 8, 22, "Subsection"],
Cell[116682, 2202, 221, 5, 16, "Text"],
Cell[116906, 2209, 2768, 71, 55, "Text"]
}, Open  ]]
}
]
*)

(* End of internal cache information *)

(* NotebookSignature 3vp8dAdJeS76AAKtHcA@qesG *)
