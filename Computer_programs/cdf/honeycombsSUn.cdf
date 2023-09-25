(* Content-type: application/vnd.wolfram.cdf.text *)

(*** Wolfram CDF File ***)
(* http://www.wolfram.com/cdf *)

(* CreatedBy='Mathematica 11.2' *)

(***************************************************************************)
(*                                                                         *)
(*                                                                         *)
(*  Under the Wolfram FreeCDF terms of use, this file and its content are  *)
(*  bound by the Creative Commons BY-SA Attribution-ShareAlike license.    *)
(*                                                                         *)
(*        For additional information concerning CDF licensing, see:        *)
(*                                                                         *)
(*         www.wolfram.com/cdf/adopting-cdf/licensing-options.html         *)
(*                                                                         *)
(*                                                                         *)
(***************************************************************************)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[      1088,         20]
NotebookDataLength[     65312,       1460]
NotebookOptionsPosition[     65816,       1454]
NotebookOutlinePosition[     66284,       1475]
CellTagsIndexPosition[     66241,       1472]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[
 DynamicModuleBox[{$CellContext`rows$$ = 3, $CellContext`cols$$ = 
  3, $CellContext`mat$$ = {{21, 13, 5}, {7, 10, 12}, {20, 11, 
  9}}, $CellContext`coupling$$ = 309, $CellContext`mul$$ = Dynamic[
   Apply[obladePackage`multiplicity, $CellContext`mat$$]], \
$CellContext`myRange$$, $CellContext`myPart$$, $CellContext`tp$$ = 
  0, $CellContext`tpval$$}, 
  DynamicBox[ToBoxes[
    Manipulate[
     Which[$CellContext`tp$$ == 0, 
      Row[{
        Invisible[$CellContext`doit], 
        (CheckAbort[
         If[Length[#] > 800, 
          Abort[], #], 
         Print[$CellContext`limittext, 800]]& )[
         TimeConstrained[
          If[Setting[$CellContext`mul$$] == 0, $CellContext`notadmissibletext, 
           TableForm[{
             PopupMenu[
              Dynamic[$CellContext`coupling$$], 
              Range[1, 
               Setting[$CellContext`mul$$]]], 
             Show[
              honeycombPackage`plotHoneycombs[
               $CellContext`myPart$$[$CellContext`mat$$, 1], 
               $CellContext`myPart$$[$CellContext`mat$$, 2], 
               $CellContext`myPart$$[$CellContext`mat$$, 
                3], $CellContext`coupling$$], ImageSize -> Full]}]], 1.5, 
          Print[1.5, 1.5, " seconds"]]]}], $CellContext`tp$$ == 
      1, $CellContext`tpval$$ = obladePackage`tensorproduct[
        $CellContext`myPart$$[$CellContext`mat$$, 1], 
        $CellContext`myPart$$[$CellContext`mat$$, 2]], $CellContext`tp$$ == 
      2, $CellContext`tp$$ = 
       0; $CellContext`mat$$ = {{3, 2, 3, 2, 2}, {2, 3, 2, 2, 3}, {4, 5, 4, 2,
         5}}; TableForm[{
         PopupMenu[
          Dynamic[$CellContext`coupling$$], 
          Range[1, 
           Setting[$CellContext`mul$$]]], 
         Show[
          honeycombPackage`plotHoneycombs[
           $CellContext`myPart$$[$CellContext`mat$$, 1], 
           $CellContext`myPart$$[$CellContext`mat$$, 2], 
           $CellContext`myPart$$[$CellContext`mat$$, 
            3], $CellContext`coupling$$], ImageSize -> Large]}]], 
     Evaluate[
      With[{$CellContext`makeRow$ = Function[{$CellContext`rowIndex$}, 
          Map[
           Function[{$CellContext`colIndex$}, 
            InputField[
             Dynamic[
              
              Part[$CellContext`mat$$, $CellContext`rowIndex$, \
$CellContext`colIndex$]], Number, FieldSize -> 3]], 
           $CellContext`myRange$$[$CellContext`cols$$]], HoldAll]}, 
       Row[{
         TextGrid[{{"\[Omega]1", ""}, {}, {"\[Omega]2", ""}, {}, {
            OverBar["\[Omega]3"], ""}}], 
         Grid[
          Map[$CellContext`makeRow$, 
           $CellContext`myRange$$[$CellContext`rows$$]], 
          Dividers -> {{False, False, False}, {False, False, Red}}], 
         Invisible["xxxxxx"], 
         Button[
          Style[
          "Update components", FontSize -> 14, Bold, 
           Blue], $CellContext`mat$$; $CellContext`coupling$$ = 1; 
          Increment[$CellContext`cols$$]; 
          Decrement[$CellContext`cols$$]; $CellContext`tp$$ = 0], 
         Column[{
           Button[
            Style["Previous pictograph", Bold], 
            
            If[$CellContext`coupling$$ > 1, 
             Decrement[$CellContext`coupling$$]; Null]], 
           Button[
            Style["Next pictograph", Bold], 
            
            If[$CellContext`coupling$$ < Setting[$CellContext`mul$$], 
             Increment[$CellContext`coupling$$]; Null]]}], 
         Column[$CellContext`abouttabletext]}]]], 
     Column[{
       Panel[
        Style[$CellContext`cols$$ + 1, FontSize -> 14, Bold], 
        Style["SU(n), with n =", Bold, FontSize -> 14], Left, 
        BaselinePosition -> Bottom], 
       Panel[
        Style[
         Column[{
          "Pictographs for admissible triples", 
           "of irreducible representations", "of SU(n)", 
           "The honeycomb version"}, Center], "Title"]]}, 
      Invisible["xxxxxx"]], 
     Panel[$CellContext`mul$$, 
      Style["Multiplicity is", FontSize -> 14, Bold], Left], 
     Panel[$CellContext`coupling$$, 
      Style["The count index of this pictograph is", Bold], Left], 
     Column[$CellContext`aboutobladetext], 
     Button[
      Style["SU(n): decrease n", FontSize -> 14, Bold], 
      If[$CellContext`rows$$ > 
       1, $CellContext`mat$$ = 
        Map[Drop[#, -1]& , $CellContext`mat$$]; $CellContext`coupling$$ = 1; 
       Decrement[$CellContext`cols$$]; Null]], 
     Button[
      Style[
      "SU(n): increase n", FontSize -> 14, Bold], $CellContext`mat$$ = 
       Transpose[
         Append[
          Transpose[$CellContext`mat$$], 
          Table[0, {3}]]]; $CellContext`coupling$$ = 1; 
      Increment[$CellContext`cols$$]; Null], 
     Button[
      Style[
      "Tensor product decomposition of \[Omega]1 \[CircleTimes] \[Omega]2 ", 
       FontSize -> 14, Bold, Blue], $CellContext`coupling$$ = 
       1; $CellContext`tp$$ = 1; Increment[$CellContext`cols$$]; 
      Decrement[$CellContext`cols$$]; Null], 
     Button[
      Style[
      "An SU(6) example with multiplicity 592", FontSize -> 10, Bold, Brown, 
       TextAlignment -> Left], $CellContext`coupling$$ = 
       1; $CellContext`cols$$ = 6 - 1; $CellContext`tp$$ = 2; 
      Increment[$CellContext`cols$$]; Decrement[$CellContext`cols$$]; Null], 
     Button[
      Style[
      "Reset random SU(3) example", FontSize -> 10, Bold, Brown, 
       TextAlignment -> Left], $CellContext`cols$$ = 2; $CellContext`mat$$ = 
       With[{$CellContext`mat12 = Table[
            RandomInteger[7], {2}, {3 - 1}]}, 
         Append[$CellContext`mat12, 
          First[
           RandomChoice[
            Apply[
            obladePackage`tensorproduct, $CellContext`mat12]]]]]; \
$CellContext`coupling$$ = 1; Increment[$CellContext`cols$$]; 
      Decrement[$CellContext`cols$$]; $CellContext`tp$$ = 
       0], {{$CellContext`doit, 0, ""}, {0}, ControlType -> None}, 
     TrackedSymbols :> {$CellContext`doit}, ContentSize -> {610, 600}], 
    StandardForm],
   ImageSizeCache->{1262., {437., 443.}}],
  SynchronousUpdating -> False,
  DynamicModuleValues:>{{DownValues[$CellContext`myRange$$] = {HoldPattern[
         $CellContext`myRange$$[
          Pattern[$CellContext`s, 
           Blank[]]]] :> Range[$CellContext`s]}}, {
    DownValues[$CellContext`myPart$$] = {HoldPattern[
         $CellContext`myPart$$[
          Pattern[$CellContext`expr, 
           Blank[]], 
          Pattern[$CellContext`s, 
           BlankSequence[]]]] :> Part[$CellContext`expr, $CellContext`s]}}},
  Initialization:>{obladePackage`multiplicity[{
       Pattern[Private`lam, 
        BlankSequence[]]}, {
       Pattern[Private`mu, 
        BlankSequence[]]}, {
       Pattern[Private`nu, 
        BlankSequence[]]}] := Switch[
      ValueQ[obladePackage`currentgroup], False, 
      obladePackage`oblade[Length[{Private`lam}] + 1]; 
      obladePackage`multiplicity[{Private`lam}, {Private`mu}, {Private`nu}], 
      True, 
      If[Length[{Private`lam}] == obladePackage`currentgroup - 1, 
       Length[
        Private`tensorproductCC[{Private`lam}, {Private`mu}, 
         Reverse[{Private`nu}]]], 
       obladePackage`oblade[Length[{Private`lam}] + 1]; 
       obladePackage`multiplicity[{Private`lam}, {Private`mu}, {
         Private`nu}]]], 
    TagSet[obladePackage`multiplicity, 
     MessageName[obladePackage`multiplicity, "usage"], 
     "multiplicity[lambda,mu,nu] gives the multiplicity of the term nu in the \
decomposition into irreducible components of a tensor product lambda \
\[CircleTimes] mu of two irreps of the Lie group SU(n), given by the \
components of their highest weights along the basis of fundamental weights. \
This integer is also called Littlewood-Richardson coefficient. Equivalently \
it is the multiplicity of the trivial representation in the triple tensor \
product: lambda \[CircleTimes] mu \[CircleTimes] Conjugate[nu]."], 
    obladePackage`currentgroup = 3, 
    TagSet[obladePackage`currentgroup, 
     MessageName[obladePackage`currentgroup, "usage"], 
     "currentgroup returns the argument n of the group SU(n) currently in \
use."], obladePackage`oblade[
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
          
          Table[(1/2) (3 Private`p + Private`p^2) + ((1/2) (Private`lev - 1)) 
            Private`lev + (Private`lev - 1) Private`p, {
           Private`p, 1, Private`NN}], {Private`z, Private`z + 1}], {
         Private`z, 1, Private`NN - Private`lev}]; Private`edgesNWSElabels[
         Pattern[Private`lev, 
          Blank[]]] := Table[
         Take[
          Range[
          1 + ((1/2) (-Private`lev + Private`NN)) (1 - Private`lev + 
             Private`NN), 1 - Private`lev + 
           Private`NN + ((1/2) (-Private`lev + Private`NN)) (1 - Private`lev + 
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
      Private`fundamentalcomponentsLabels = 
       ToString[Private`fundamentalcomponents]; Private`componentsEdgesAndnuc[{
          Pattern[Private`lam, 
           BlankSequence[]]}, {
          Pattern[Private`mu, 
           BlankSequence[]]}] := (
        Private`componentsEdgesAndnuc[{Private`lam}, {Private`mu}] = 
        Module[{Private`redsol, Private`locrules, Private`nuclist, 
           Private`fundcompval, Private`edgesEWValues, 
           Private`edgesNESWValues, Private`edgesNWSEValues}, 
          Private`redsol = Reduce[
             And[
              ReplaceAll[Private`lambdaequations, 
               Private`buildlambdainput[{Private`lam}, {Private`mu}]], 
              Private`positivityconstraints], Private`fundamentalcomponents, 
             Integers]; Private`locrules = ToRules[Private`redsol]; 
          Private`fundcompval = 
           ReplaceAll[Private`fundamentalcomponents, {Private`locrules}]; 
          Private`nuclist = ReplaceAll[
             Table[
              Private`lambda[0, 0, Private`p], {
              Private`p, 1, Private`NN - 1}], {Private`locrules}]; 
          Private`edgesEWValues = Table[
             ReplaceAll[
              Private`edgesEW[Private`NN], 
              MapThread[Rule, {Private`fundamentalcomponents, 
                Part[Private`fundcompval, Private`s]}]], {Private`s, 1, 
              Length[Private`fundcompval]}]; Private`edgesNESWValues = Table[
             ReplaceAll[
              Private`edgesNESW[Private`NN], 
              MapThread[Rule, {Private`fundamentalcomponents, 
                Part[Private`fundcompval, Private`s]}]], {Private`s, 1, 
              Length[Private`fundcompval]}]; Private`edgesNWSEValues = Table[
             ReplaceAll[
              Private`edgesNWSE[Private`NN], 
              MapThread[Rule, {Private`fundamentalcomponents, 
                Part[Private`fundcompval, Private`s]}]], {Private`s, 1, 
              Length[Private`fundcompval]}]; {
           Private`fundcompval, Private`edgesEWValues, 
            Private`edgesNESWValues, Private`edgesNWSEValues, 
            Private`nuclist}]); Private`componentsEdgesAndnuc[{
          Pattern[Private`lam, 
           BlankSequence[]]}, {
          Pattern[Private`mu, 
           BlankSequence[]]}, {
          Pattern[Private`nuc, 
           BlankSequence[]]}] := (
        Private`componentsEdgesAndnuc[{Private`lam}, {Private`mu}, {
          Private`nuc}] = 
        Module[{Private`redsol, Private`locrules, Private`nuclist, 
           Private`fundcompval, Private`edgesEWValues, 
           Private`edgesNESWValues, Private`edgesNWSEValues}, 
          Private`redsol = Reduce[
             And[
              ReplaceAll[Private`lambdaequations, 
               
               Private`buildlambdainput[{Private`lam}, {Private`mu}, {
                Private`nuc}]], Private`positivityconstraints], 
             Private`fundamentalcomponents, Integers]; 
          Private`redsol = And[Private`redsol, 
             Apply[And, 
              Table[
              Private`lambda[0, 0, Private`p] == 
               Part[{Private`nuc}, Private`p], {
               Private`p, 1, Private`NN - 1}]]]; 
          If[Private`redsol =!= False, Private`locrules = 
            ToRules[Private`redsol], 
            Return[{0, 0, 0, 0, 0}]]; 
          Private`fundcompval = 
           ReplaceAll[Private`fundamentalcomponents, {Private`locrules}]; 
          Private`nuclist = ReplaceAll[
             Table[
              Private`lambda[0, 0, Private`p], {
              Private`p, 1, Private`NN - 1}], {Private`locrules}]; 
          Private`edgesEWValues = Table[
             ReplaceAll[
              Private`edgesEW[Private`NN], 
              MapThread[Rule, {Private`fundamentalcomponents, 
                Part[Private`fundcompval, Private`s]}]], {Private`s, 1, 
              Length[Private`fundcompval]}]; Private`edgesNESWValues = Table[
             ReplaceAll[
              Private`edgesNESW[Private`NN], 
              MapThread[Rule, {Private`fundamentalcomponents, 
                Part[Private`fundcompval, Private`s]}]], {Private`s, 1, 
              Length[Private`fundcompval]}]; Private`edgesNWSEValues = Table[
             ReplaceAll[
              Private`edgesNWSE[Private`NN], 
              MapThread[Rule, {Private`fundamentalcomponents, 
                Part[Private`fundcompval, Private`s]}]], {Private`s, 1, 
              Length[Private`fundcompval]}]; {
           Private`fundcompval, Private`edgesEWValues, 
            Private`edgesNESWValues, Private`edgesNWSEValues, 
            Private`nuclist}]); 
      Private`AdjMatbis = adjmatPackage`adjmatfct[Private`NN]; 
      Private`rs = adjmatPackage`nntoseed[Private`NN]; If[
        MissingQ[
         adjmatPackage`nntoseed[Private`NN]], 
        Print[
        "The nntoseed A-list in the adjmatPackage should be extended for \
plotOblade to work with this rank"]]; Private`plotSUNoblade[
         Pattern[Private`mytripleofweights, 
          Blank[]], 
         Pattern[Private`loccoupling, 
          Blank[]]] := 
       Module[{Private`adj, Private`vert = Private`mytripleofweights, 
          Private`grid, Private`gr2, Private`grla, Private`grmu, Private`grnu,
           Private`grEW, Private`grNESW, Private`grNWSE}, 
         Private`adj = Private`AdjMatbis; 
         Private`fullinfo = Private`componentsEdgesAndnuc[
            Part[Private`mytripleofweights, 1], 
            Part[Private`mytripleofweights, 2], 
            Reverse[
             Part[Private`mytripleofweights, 3]]]; 
         Private`grid = 
          GraphPlot[
           Private`adj, Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
         Private`grla = 
          GraphPlot[Private`adj, VertexRenderingFunction -> (Apply[Which, 
              Flatten[
               Table[{#2 == Private`lambdalabel[Private`NN, Private`s], {
                  Text[
                   Style[
                    Part[Private`vert, 1, Private`s], Large, Blue], #, {
                   Right, Bottom}], 
                  Disk[#, 0.03], {Black}}}, {Private`s, 1, Private`NN - 1}], 
               1]]& ), Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
         Private`grmu = 
          GraphPlot[Private`adj, VertexRenderingFunction -> (Apply[Which, 
              Flatten[
               Table[{#2 == Private`mulabel[Private`NN, Private`s], {
                  Text[
                   Style[
                    Part[Private`vert, 2, Private`s], Large, Blue], #, {
                   Left, Bottom}], 
                  Disk[#, 0.03], {Black}}}, {Private`s, 1, Private`NN - 1}], 
               1]]& ), Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
         Private`grnu = 
          GraphPlot[Private`adj, VertexRenderingFunction -> (Apply[Which, 
              Flatten[
               Table[{#2 == Private`nulabel[Private`NN, Private`s], {
                  Text[
                   Style[
                    Part[Private`vert, 3, Private`NN - Private`s], Large, 
                    Blue], #, {Right, Top}], 
                  Disk[#, 0.03], {Black}}}, {Private`s, 1, Private`NN - 1}], 
               1]]& ), Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
         Private`grEW = 
          GraphPlot[Private`adj, EdgeRenderingFunction -> (Apply[Which, 
              Flatten[
               Table[
                Flatten[
                 Table[{
                   And[
                   First[#2] == First[Private`u], Last[#2] == 
                    Last[Private`u]], {
                    Text[
                    Style[
                    Part[
                    Reverse[
                    Part[
                    Part[Private`fullinfo, 2, Private`loccoupling], 
                    Private`f]], 
                    First[
                    Flatten[
                    Position[
                    Private`edgesEWlabels[Private`f], Private`u]]]], 20], (
                    Part[#, 1] + Part[#, 2])/2]}}, {Private`u, 
                   Private`edgesEWlabels[Private`f]}], 1], {
                Private`f, 1, Private`NN - 1}], 1]]& ), 
            Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
         Private`grNESW = 
          GraphPlot[Private`adj, EdgeRenderingFunction -> (Apply[Which, 
              Flatten[
               Table[
                Flatten[
                 Table[{
                   And[
                   Last[#2] == First[Private`u], First[#2] == 
                    Last[Private`u]], {
                    Text[
                    Style[
                    Part[
                    Part[
                    Part[Private`fullinfo, 3, Private`loccoupling], 
                    Private`f], 
                    First[
                    Flatten[
                    Position[
                    Private`edgesNESWlabels[Private`f], Private`u]]]], 20], (
                    Part[#, 1] + Part[#, 2])/2]}}, {Private`u, 
                   Private`edgesNESWlabels[Private`f]}], 1], {
                Private`f, 1, Private`NN - 1}], 1]]& ), 
            Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
         Private`grNWSE = 
          GraphPlot[Private`adj, EdgeRenderingFunction -> (Apply[Which, 
              Flatten[
               Table[
                Flatten[
                 Table[{
                   And[
                   First[#2] == First[Private`u], Last[#2] == 
                    Last[Private`u]], {
                    Text[
                    Style[
                    Part[
                    Reverse[
                    Part[
                    Part[Private`fullinfo, 4, Private`loccoupling], 
                    Private`f]], 
                    First[
                    Flatten[
                    Position[
                    Private`edgesNWSElabels[Private`f], Private`u]]]], 20], (
                    Part[#, 1] + Part[#, 2])/2]}}, {Private`u, 
                   Private`edgesNWSElabels[Private`f]}], 1], {
                Private`f, 1, Private`NN - 1}], 1]]& ), 
            Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
         Show[Private`grid, Private`grla, Private`grmu, Private`grnu, 
           Private`grEW, Private`grNESW, Private`grNWSE]]; 
      Private`plotAllOblades[
         Pattern[Private`myweights, 
          Blank[]]] := Table[
         Private`plotSUNoblade[Private`myweights, Private`pos], {
         Private`pos, 1, 
          Apply[obladePackage`multiplicity, Private`myweights]}]; 
      obladePackage`currentgroup = Private`NN], 
    TagSet[obladePackage`oblade, 
     MessageName[obladePackage`oblade, "usage"], 
     "Evaluation of oblade[n] initializes all relevant data for SU(n). It \
returns n and set the variable currentgroup to n."], Private`lambdaequations = 
    And[Private`lambda[1, 0, 0] == 
      Private`j[1, 0, 2] + Private`j[1, 1, 1] + Private`j[1, 2, 0], 
      Private`lambda[2, 0, 0] == Private`j[2, 0, 1] + Private`j[2, 1, 0], 
      Private`lambda[0, 1, 0] == 
      Private`j[0, 1, 2] + Private`j[1, 1, 1] + Private`j[2, 1, 0], 
      Private`lambda[0, 2, 0] == Private`j[0, 2, 1] + Private`j[1, 2, 0], 
      Private`lambda[0, 0, 1] == 
      Private`j[0, 2, 1] + Private`j[1, 1, 1] + Private`j[2, 0, 1], 
      Private`lambda[0, 0, 2] == Private`j[0, 1, 2] + Private`j[1, 0, 2]], 
    Private`edgesEW[3] = {{
       Private`j[1, 2, 0], Private`j[1, 1, 1] + Private`j[1, 2, 0]}, {
       Private`j[2, 1, 0]}}, Private`edgesNESW[3] = {{
       Private`j[0, 1, 2], Private`j[0, 1, 2] + Private`j[1, 1, 1]}, {
       Private`j[0, 2, 1]}}, Private`edgesNWSE[3] = {{
       Private`j[2, 0, 1], Private`j[1, 1, 1] + Private`j[2, 0, 1]}, {
       Private`j[1, 0, 2]}}, Private`edgesEWlabels[
      Pattern[Private`lev$, 
       Blank[]]] := Table[
      Take[
       Table[(1/2) (4 + Private`p + Private`p^2) + (Private`lev$ - 1), {
        Private`p, Private`lev$, 3}], {Private`z, Private`z + 1}], {
      Private`z, 1, 3 - Private`lev$}], 
    Attributes[Private`lev$] = {Temporary}, Private`edgesNESWlabels[
      Pattern[Private`lev$, 
       Blank[]]] := Table[
      Take[
       Table[(1/2) (3 Private`p + Private`p^2) + ((1/2) (Private`lev$ - 1)) 
         Private`lev$ + (Private`lev$ - 1) Private`p, {Private`p, 1, 3}], {
       Private`z, Private`z + 1}], {Private`z, 1, 3 - Private`lev$}], 
    Private`edgesNWSElabels[
      Pattern[Private`lev$, 
       Blank[]]] := Table[
      Take[
       Range[
       1 + ((1/2) (-Private`lev$ + 3)) (1 - Private`lev$ + 3), 1 - 
        Private`lev$ + 
        3 + ((1/2) (-Private`lev$ + 3)) (1 - Private`lev$ + 3)], {
       Private`z, Private`z + 1}], {Private`z, 1, 3 - Private`lev$}], 
    Private`components = {{
       Private`j[1, 2, 0], 
       Private`j[1, 1, 1], 
       Private`j[1, 0, 2]}, {
       Private`j[2, 1, 0], 
       Private`j[2, 0, 1]}, {
       Private`j[0, 1, 2], 
       Private`j[1, 1, 1], 
       Private`j[2, 1, 0]}, {
       Private`j[0, 2, 1], 
       Private`j[1, 2, 0]}, {
       Private`j[0, 2, 1], 
       Private`j[1, 1, 1], 
       Private`j[2, 0, 1]}, {
       Private`j[0, 1, 2], 
       Private`j[1, 0, 2]}}, Private`fundamentalcomponents = {
      Private`j[0, 1, 2], 
      Private`j[0, 2, 1], 
      Private`j[1, 0, 2], 
      Private`j[1, 1, 1], 
      Private`j[1, 2, 0], 
      Private`j[2, 0, 1], 
      Private`j[2, 1, 0]}, Private`positivityconstraints = 
    And[Private`j[1, 2, 0] >= 0, Private`j[1, 1, 1] + Private`j[1, 2, 0] >= 0,
       Private`j[2, 1, 0] >= 0, Private`j[0, 1, 2] >= 0, 
      Private`j[0, 1, 2] + Private`j[1, 1, 1] >= 0, Private`j[0, 2, 1] >= 0, 
      Private`j[2, 0, 1] >= 0, Private`j[1, 1, 1] + Private`j[2, 0, 1] >= 0, 
      Private`j[1, 0, 2] >= 0], Private`buildlambdainput[{
       Pattern[Private`l1$, 
        BlankSequence[]]}, {
       Pattern[Private`l2$, 
        BlankSequence[]]}] := Flatten[{
       MapThread[Rule, {
         Table[
          Private`lambda[Private`p, 0, 0], {Private`p, 1, 3 - 1}], {
         Private`l1$}}], 
       MapThread[Rule, {
         Table[
          Private`lambda[0, Private`p, 0], {Private`p, 1, 3 - 1}], {
         Private`l2$}}]}], Private`buildlambdainput[{
       Pattern[Private`l1$, 
        BlankSequence[]]}, {
       Pattern[Private`l2$, 
        BlankSequence[]]}, {
       Pattern[Private`l3$, 
        BlankSequence[]]}] := Flatten[{
       MapThread[Rule, {
         Table[
          Private`lambda[Private`p, 0, 0], {Private`p, 1, 3 - 1}], {
         Private`l1$}}], 
       MapThread[Rule, {
         Table[
          Private`lambda[0, Private`p, 0], {Private`p, 1, 3 - 1}], {
         Private`l2$}}], 
       MapThread[Rule, {
         Table[
          Private`lambda[0, 0, Private`p], {Private`p, 1, 3 - 1}], {
         Private`l3$}}]}], Attributes[Private`l1$] = {Temporary}, 
    Attributes[Private`l2$] = {Temporary}, 
    Attributes[Private`l3$] = {Temporary}, 
    Private`fundamentalcomponentsLabels = 
    "{Private`j[0, 1, 2], Private`j[0, 2, 1], Private`j[1, 0, 2], \
Private`j[1, 1, 1], Private`j[1, 2, 0], Private`j[2, 0, 1], Private`j[2, 1, \
0]}", Private`componentsEdgesAndnuc[{0, 3}, {0, 3}] = {{{3, 0, 0, -3, 3, 3, 
      0}, {2, 1, 0, -2, 2, 3, 0}, {1, 2, 0, -1, 1, 3, 0}, {0, 3, 0, 0, 0, 3, 
      0}}, {{{3, 0}, {0}}, {{2, 0}, {0}}, {{1, 0}, {0}}, {{0, 0}, {0}}}, {{{3,
         0}, {0}}, {{2, 0}, {1}}, {{1, 0}, {2}}, {{0, 0}, {3}}}, {{{3, 0}, {
        0}}, {{3, 1}, {0}}, {{3, 2}, {0}}, {{3, 3}, {0}}}, {{0, 3}, {2, 2}, {
      4, 1}, {6, 0}}}, 
    Private`componentsEdgesAndnuc[{5, 3}, {3, 0}] = {{{0, 0, 5, 0, 0, 0, 3}, {
      1, 0, 5, 0, 0, 1, 2}, {0, 0, 4, 1, 0, 1, 2}, {2, 0, 5, 0, 0, 2, 1}, {1, 
      0, 4, 1, 0, 2, 1}, {3, 0, 5, 0, 0, 3, 0}, {0, 0, 3, 2, 0, 2, 1}, {2, 0, 
      4, 1, 0, 3, 0}, {1, 0, 3, 2, 0, 3, 0}, {0, 0, 2, 3, 0, 3, 
      0}}, {{{0, 0}, {3}}, {{0, 0}, {2}}, {{0, 1}, {2}}, {{0, 0}, {1}}, {{0, 
        1}, {1}}, {{0, 0}, {0}}, {{0, 2}, {1}}, {{0, 1}, {0}}, {{0, 2}, {
        0}}, {{0, 3}, {0}}}, {{{0, 0}, {0}}, {{1, 1}, {0}}, {{0, 1}, {0}}, {{
        2, 2}, {0}}, {{1, 2}, {0}}, {{3, 3}, {0}}, {{0, 2}, {0}}, {{2, 3}, {
        0}}, {{1, 3}, {0}}, {{0, 3}, {0}}}, {{{0, 0}, {5}}, {{1, 1}, {5}}, {{
        1, 2}, {4}}, {{2, 2}, {5}}, {{2, 3}, {4}}, {{3, 3}, {5}}, {{2, 4}, {
        3}}, {{3, 4}, {4}}, {{3, 5}, {3}}, {{3, 6}, {2}}}, {{0, 5}, {1, 6}, {
      2, 4}, {2, 7}, {3, 5}, {3, 8}, {4, 3}, {4, 6}, {5, 4}, {6, 2}}}, 
    Private`componentsEdgesAndnuc[{6, 6}, {6, 1}, {5, 13}] = {{{7, 0, 6, -1, 
      1, 6, 0}}, {{{1, 0}, {0}}}, {{{7, 6}, {0}}}, {{{6, 5}, {6}}}, {{5, 
      13}}}, Private`componentsEdgesAndnuc[{
       Pattern[Private`lam$, 
        BlankSequence[]]}, {
       Pattern[Private`mu$, 
        BlankSequence[]]}] := (
     Private`componentsEdgesAndnuc[{Private`lam$}, {Private`mu$}] = 
     Module[{Private`redsol$, Private`locrules$, Private`nuclist$, 
        Private`fundcompval$, Private`edgesEWValues$, 
        Private`edgesNESWValues$, Private`edgesNWSEValues$}, 
       Private`redsol$ = Reduce[
          And[
           ReplaceAll[Private`lambdaequations, 
            Private`buildlambdainput[{Private`lam$}, {Private`mu$}]], 
           Private`positivityconstraints], Private`fundamentalcomponents, 
          Integers]; Private`locrules$ = ToRules[Private`redsol$]; 
       Private`fundcompval$ = 
        ReplaceAll[Private`fundamentalcomponents, {Private`locrules$}]; 
       Private`nuclist$ = ReplaceAll[
          Table[
           Private`lambda[0, 0, Private`p], {Private`p, 1, 3 - 1}], {
          Private`locrules$}]; Private`edgesEWValues$ = Table[
          ReplaceAll[
           Private`edgesEW[3], 
           MapThread[Rule, {Private`fundamentalcomponents, 
             Part[Private`fundcompval$, Private`s]}]], {Private`s, 1, 
           Length[Private`fundcompval$]}]; Private`edgesNESWValues$ = Table[
          ReplaceAll[
           Private`edgesNESW[3], 
           MapThread[Rule, {Private`fundamentalcomponents, 
             Part[Private`fundcompval$, Private`s]}]], {Private`s, 1, 
           Length[Private`fundcompval$]}]; Private`edgesNWSEValues$ = Table[
          ReplaceAll[
           Private`edgesNWSE[3], 
           MapThread[Rule, {Private`fundamentalcomponents, 
             Part[Private`fundcompval$, Private`s]}]], {Private`s, 1, 
           Length[Private`fundcompval$]}]; {
        Private`fundcompval$, Private`edgesEWValues$, 
         Private`edgesNESWValues$, Private`edgesNWSEValues$, 
         Private`nuclist$}]), Private`componentsEdgesAndnuc[{
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
            Part[{Private`nuc$}, Private`p], {Private`p, 1, 3 - 1}]]]; 
       If[Private`redsol$ =!= False, Private`locrules$ = 
         ToRules[Private`redsol$], 
         Return[{0, 0, 0, 0, 0}]]; 
       Private`fundcompval$ = 
        ReplaceAll[Private`fundamentalcomponents, {Private`locrules$}]; 
       Private`nuclist$ = ReplaceAll[
          Table[
           Private`lambda[0, 0, Private`p], {Private`p, 1, 3 - 1}], {
          Private`locrules$}]; Private`edgesEWValues$ = Table[
          ReplaceAll[
           Private`edgesEW[3], 
           MapThread[Rule, {Private`fundamentalcomponents, 
             Part[Private`fundcompval$, Private`s]}]], {Private`s, 1, 
           Length[Private`fundcompval$]}]; Private`edgesNESWValues$ = Table[
          ReplaceAll[
           Private`edgesNESW[3], 
           MapThread[Rule, {Private`fundamentalcomponents, 
             Part[Private`fundcompval$, Private`s]}]], {Private`s, 1, 
           Length[Private`fundcompval$]}]; Private`edgesNWSEValues$ = Table[
          ReplaceAll[
           Private`edgesNWSE[3], 
           MapThread[Rule, {Private`fundamentalcomponents, 
             Part[Private`fundcompval$, Private`s]}]], {Private`s, 1, 
           Length[Private`fundcompval$]}]; {
        Private`fundcompval$, Private`edgesEWValues$, 
         Private`edgesNESWValues$, Private`edgesNWSEValues$, 
         Private`nuclist$}]), Attributes[Private`lam$] = {Temporary}, 
    Attributes[Private`mu$] = {Temporary}, 
    Attributes[Private`redsol$] = {Temporary}, 
    Attributes[Private`locrules$] = {Temporary}, 
    Attributes[Private`nuclist$] = {Temporary}, 
    Attributes[Private`fundcompval$] = {Temporary}, 
    Attributes[Private`edgesEWValues$] = {Temporary}, 
    Attributes[Private`edgesNESWValues$] = {Temporary}, 
    Attributes[Private`edgesNWSEValues$] = {Temporary}, 
    Attributes[Private`nuc$] = {Temporary}, 
    Private`AdjMatbis = {{0, 1, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 1, 1, 0, 0, 0,
      0, 0, 0}, {1, 0, 0, 0, 1, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 1, 0, 1, 0, 0, 
     0}, {0, 1, 0, 0, 0, 1, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 0, 0, 0, 1, 0}, {0, 
     0, 0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 1, 0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 
     1, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1, 0, 0, 0, 0}}, 
    adjmatPackage`adjmatfct[3] = {{0, 1, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 1, 1,
      0, 0, 0, 0, 0, 0}, {1, 0, 0, 0, 1, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 1, 0, 1,
      0, 0, 0}, {0, 1, 0, 0, 0, 1, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 0, 0, 0, 1, 
     0}, {0, 0, 0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 1, 0, 0, 0, 0, 1, 0}, {0, 
     0, 0, 0, 1, 0, 0, 0, 0, 1}, {0, 0, 0, 0, 0, 1, 0, 0, 0, 0}}, 
    adjmatPackage`adjmatfct[
      Pattern[Private`NN, 
       Blank[]]] := (adjmatPackage`adjmatfct[Private`NN] = 
     Module[{Private`levelplus, Private`rk, Private`One, Private`iii, 
        Private`SparseAdjTens, Private`normalAdjTens, Private`lab, 
        Private`partlab, Private`adjacencyMatrices}, 
       Private`levelplus = Private`NN + 1; 
       Private`rk = (Private`NN + 1) ((Private`NN + 2)/2); 
       Private`One = IdentityMatrix[Private`rk]; 
       Private`SparseAdjTens = SparseArray[{Condition[{
              Pattern[Private`a, 
               Blank[]], 
              Pattern[Private`b, 
               Blank[]], 
              Pattern[Private`c, 
               Blank[]], 
              Pattern[Private`d, 
               Blank[]]}, 
             Or[
              And[Private`c == Private`a + 1, Private`d == Private`b], 
              And[Private`c == Private`a, Private`d == Private`b - 1], 
              And[Private`c == Private`a - 1, Private`d == Private`b + 1]]] -> 
           1}, {Private`levelplus, Private`levelplus, Private`levelplus, 
           Private`levelplus}]; 
       Private`normalAdjTens = Normal[Private`SparseAdjTens]; 
       Private`lab = {{0, 0}, {1, 0}, {0, 1}}; 
       Do[Do[Private`iii = Private`i + 1; 
           Private`partlab[Private`iii] = {
             Private`j - Private`i, Private`i}, {Private`i, 0, Private`j}]; 
         Do[Private`lab = Append[Private`lab, 
             Private`partlab[Private`i]], {Private`i, 1, Private`j + 1}], {
         Private`j, 2, Private`levelplus - 1}]; 
       Private`adjacencyMatrices = 
        Module[{Private`v, Private`crit0, Private`crit1, Private`crit2, 
           Private`indices0, Private`indices1, Private`indices2, 
           Private`indices, Private`orderedindices, Private`smallAdjTens, 
           Private`matbid, Private`trisort, Private`tritolab, 
           Private`permutritolab, Private`permutritolabinverse, 
           Private`AdjMatTriality, Private`AdjMatLevel}, 
          Private`indices = Flatten[
             Table[
              Private`v[Private`i, Private`j], {
              Private`i, 1, Private`levelplus}, {
              Private`j, 1, Private`levelplus - Private`i + 1}]]; 
          Private`crit0[
             Private`v[
              Pattern[Private`p, 
               Blank[]], 
              Pattern[Private`q, 
               Blank[]]]] := Mod[Private`p + 2 Private`q - 3, 3] == 0; 
          Private`crit1[
             Private`v[
              Pattern[Private`p, 
               Blank[]], 
              Pattern[Private`q, 
               Blank[]]]] := Mod[Private`p + 2 Private`q - 3, 3] == 1; 
          Private`crit2[
             Private`v[
              Pattern[Private`p, 
               Blank[]], 
              Pattern[Private`q, 
               Blank[]]]] := Mod[Private`p + 2 Private`q - 3, 3] == 2; 
          Private`indices0 = Select[Private`indices, Private`crit0]; 
          Private`indices1 = Select[Private`indices, Private`crit1]; 
          Private`indices2 = Select[Private`indices, Private`crit2]; 
          Private`orderedindices = 
           Join[Private`indices0, Private`indices1, Private`indices2]; 
          Private`smallAdjTens = Table[
             Part[
             Private`normalAdjTens, Private`i, Private`j, Private`k, 
              Private`l], {Private`i, 1, Private`levelplus}, {
             Private`j, 1, Private`levelplus - Private`i + 1}, {
             Private`k, 1, Private`levelplus}, {
             Private`l, 1, Private`levelplus - Private`k + 1}]; Private`matbid[
             Private`v[
              Pattern[Private`a, 
               Blank[]], 
              Pattern[Private`b, 
               Blank[]]], 
             Private`v[
              Pattern[Private`c, 
               Blank[]], 
              Pattern[Private`d, 
               Blank[]]]] := 
           Part[Private`smallAdjTens, Private`a, Private`b, Private`c, 
             Private`d]; Private`AdjMatTriality = Table[
             Private`matbid[
              Part[Private`orderedindices, Private`i], 
              Part[Private`orderedindices, Private`j]], {
             Private`i, 1, Private`rk}, {Private`j, 1, Private`rk}]; 
          Private`trisort = 
           ReplaceAll[Private`orderedindices, {Private`v -> List}] - 
            ConstantArray[{1, 1}, Private`rk]; Private`tritolab = Table[
             Flatten[{Private`s, 
               Position[Private`lab, 
                Part[Private`trisort, Private`s]]}], {
             Private`s, 1, Private`rk}]; Private`permutritolab = SparseArray[
             MapThread[Rule, {Private`tritolab, 
               ConstantArray[1, Private`rk]}], {Private`rk, Private`rk}]; 
          Private`permutritolabinverse = SparseArray[
             Inverse[Private`permutritolab]]; 
          Private`AdjMatLevel = 
           Dot[Private`permutritolabinverse, Private`AdjMatTriality, 
             Private`permutritolab]; {
           Private`AdjMatTriality, Private`AdjMatLevel}]; 
       Part[Private`adjacencyMatrices, 2]]), 
    TagSet[adjmatPackage`adjmatfct, 
     MessageName[adjmatPackage`adjmatfct, "usage"], 
     "adjmatfct[n] returns the adjacency matrix of SU(3) at level n"], 
    Private`v = {(-Sqrt[3])/2, 1/2}, Private`rs = 1, adjmatPackage`nntoseed = 
    Association[
     1 -> 22, 2 -> 8, 3 -> 1, 4 -> 2, 5 -> 3, 6 -> 0, 7 -> 4, 8 -> 2, 9 -> 0, 
      10 -> 10, 11 -> 1, 12 -> 9], 
    TagSet[adjmatPackage`nntoseed, 
     MessageName[adjmatPackage`nntoseed, "usage"], 
     "nntoseed is association list and nntoseed[NN] gives the RandomSeed to \
be used in GraphPlot in order to display correctly (orientation) the graph of \
adjacency matrices. The list nntoseed should be completed manually."], 
    Private`plotSUNoblade[
      Pattern[Private`mytripleofweights$, 
       Blank[]], 
      Pattern[Private`loccoupling$, 
       Blank[]]] := 
    Module[{Private`adj$, Private`vert$ = Private`mytripleofweights$, 
       Private`grid$, Private`gr2$, Private`grla$, Private`grmu$, 
       Private`grnu$, Private`grEW$, Private`grNESW$, Private`grNWSE$}, 
      Private`adj$ = Private`AdjMatbis; 
      Private`fullinfo = Private`componentsEdgesAndnuc[
         Part[Private`mytripleofweights$, 1], 
         Part[Private`mytripleofweights$, 2], 
         Reverse[
          Part[Private`mytripleofweights$, 3]]]; 
      Private`grid$ = 
       GraphPlot[
        Private`adj$, Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
      Private`grla$ = 
       GraphPlot[Private`adj$, VertexRenderingFunction -> (Apply[Which, 
           Flatten[
            Table[{#2 == Private`lambdalabel[3, Private`s], {
               Text[
                Style[
                 Part[Private`vert$, 1, Private`s], Large, Blue], #, {
                Right, Bottom}], 
               Disk[#, 0.03], {Black}}}, {Private`s, 1, 3 - 1}], 1]]& ), 
         Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
      Private`grmu$ = 
       GraphPlot[Private`adj$, VertexRenderingFunction -> (Apply[Which, 
           Flatten[
            Table[{#2 == Private`mulabel[3, Private`s], {
               Text[
                Style[
                 Part[Private`vert$, 2, Private`s], Large, Blue], #, {
                Left, Bottom}], 
               Disk[#, 0.03], {Black}}}, {Private`s, 1, 3 - 1}], 1]]& ), 
         Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
      Private`grnu$ = 
       GraphPlot[Private`adj$, VertexRenderingFunction -> (Apply[Which, 
           Flatten[
            Table[{#2 == Private`nulabel[3, Private`s], {
               Text[
                Style[
                 Part[Private`vert$, 3, 3 - Private`s], Large, Blue], #, {
                Right, Top}], 
               Disk[#, 0.03], {Black}}}, {Private`s, 1, 3 - 1}], 1]]& ), 
         Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
      Private`grEW$ = 
       GraphPlot[Private`adj$, EdgeRenderingFunction -> (Apply[Which, 
           Flatten[
            Table[
             Flatten[
              Table[{
                And[
                First[#2] == First[Private`u], Last[#2] == Last[Private`u]], {
                
                 Text[
                  Style[
                   Part[
                    Reverse[
                    Part[
                    Part[Private`fullinfo, 2, Private`loccoupling$], 
                    Private`f]], 
                    First[
                    Flatten[
                    Position[
                    Private`edgesEWlabels[Private`f], Private`u]]]], 20], (
                   Part[#, 1] + Part[#, 2])/2]}}, {Private`u, 
                Private`edgesEWlabels[Private`f]}], 1], {
             Private`f, 1, 3 - 1}], 1]]& ), 
         Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
      Private`grNESW$ = 
       GraphPlot[Private`adj$, EdgeRenderingFunction -> (Apply[Which, 
           Flatten[
            Table[
             Flatten[
              Table[{
                And[
                Last[#2] == First[Private`u], First[#2] == Last[Private`u]], {
                
                 Text[
                  Style[
                   Part[
                    Part[
                    Part[Private`fullinfo, 3, Private`loccoupling$], 
                    Private`f], 
                    First[
                    Flatten[
                    Position[
                    Private`edgesNESWlabels[Private`f], Private`u]]]], 20], (
                   Part[#, 1] + Part[#, 2])/2]}}, {Private`u, 
                Private`edgesNESWlabels[Private`f]}], 1], {
             Private`f, 1, 3 - 1}], 1]]& ), 
         Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
      Private`grNWSE$ = 
       GraphPlot[Private`adj$, EdgeRenderingFunction -> (Apply[Which, 
           Flatten[
            Table[
             Flatten[
              Table[{
                And[
                First[#2] == First[Private`u], Last[#2] == Last[Private`u]], {
                
                 Text[
                  Style[
                   Part[
                    Reverse[
                    Part[
                    Part[Private`fullinfo, 4, Private`loccoupling$], 
                    Private`f]], 
                    First[
                    Flatten[
                    Position[
                    Private`edgesNWSElabels[Private`f], Private`u]]]], 20], (
                   Part[#, 1] + Part[#, 2])/2]}}, {Private`u, 
                Private`edgesNWSElabels[Private`f]}], 1], {
             Private`f, 1, 3 - 1}], 1]]& ), 
         Method -> {Automatic, "RandomSeed" -> Private`rs}]; 
      Show[Private`grid$, Private`grla$, Private`grmu$, Private`grnu$, 
        Private`grEW$, Private`grNESW$, Private`grNWSE$]], 
    Attributes[Private`mytripleofweights$] = {Temporary}, 
    Attributes[Private`loccoupling$] = {Temporary}, 
    Attributes[Private`adj$] = {Temporary}, 
    Attributes[Private`vert$] = {Temporary}, 
    Attributes[Private`grid$] = {Temporary}, 
    Attributes[Private`gr2$] = {Temporary}, 
    Attributes[Private`grla$] = {Temporary}, 
    Attributes[Private`grmu$] = {Temporary}, 
    Attributes[Private`grnu$] = {Temporary}, 
    Attributes[Private`grEW$] = {Temporary}, 
    Attributes[Private`grNESW$] = {Temporary}, 
    Attributes[Private`grNWSE$] = {Temporary}, Private`lambdalabel[
      Pattern[Private`n, 
       Blank[]], 
      Pattern[Private`p, 
       Blank[]]] := (1/2) (2 + 3 Private`p + Private`p^2), Private`mulabel[
      Pattern[Private`n, 
       Blank[]], 
      Pattern[Private`p, 
       Blank[]]] := (Private`n + 1) ((Private`n + 2)/2) - Private`p, 
    Private`nulabel[
      Pattern[Private`n, 
       Blank[]], 
      Pattern[Private`p, 
       Blank[]]] := (1/2) (Private`n + Private`n^2 - (2 Private`n) 
      Private`p + (-1 + Private`p) Private`p + 2), 
    Private`u = {Sqrt[3]/2, 1/2}, Private`plotAllOblades[
      Pattern[Private`myweights, 
       Blank[]]] := Table[
      Private`plotSUNoblade[Private`myweights, Private`pos], {
      Private`pos, 1, 
       Apply[obladePackage`multiplicity, Private`myweights]}], 
    Private`tensorproductCC[{
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
       Private`nuc}], 5], $CellContext`limittext = SequenceForm[
      Style[
      "Web version: CHANGE the computation is terminated if it takes more \
than ", 12, Bold], 
      Style[1.5, 12, Bold], 
      Style[" seconds", 12, Bold]], $CellContext`notadmissibletext = 
    "(\[Omega]1, \[Omega]2, \[Omega]3) is not an admissible triple: Modify \
the components and press \"Update\" \n\nTo see all possible choices for \
\!\(\*OverscriptBox[\(\[Omega]3\), \(_\)]\), using the displayed values of \
\[Omega]1 and \[Omega]2 (the list may be long):\n\n Press the button \"Tensor \
product decomposition of \[Omega]1 \[CircleTimes] \[Omega]2\" ", 
    honeycombPackage`plotHoneycombs[{
       Pattern[Private`lam, 
        BlankSequence[]]}, {
       Pattern[Private`mu, 
        BlankSequence[]]}, {
       Pattern[Private`nu, 
        BlankSequence[]]}] := Switch[
      ValueQ[obladePackage`currentgroup], False, 
      obladePackage`oblade[Length[{Private`lam}] + 1]; 
      honeycombPackage`plotHoneycombs[{Private`lam}, {Private`mu}, {
        Private`nu}], True, 
      If[Length[{Private`lam}] == obladePackage`currentgroup - 1, 
       Private`plotAllHoneycombs[{{Private`lam}, {Private`mu}, {Private`nu}}],
        obladePackage`oblade[Length[{Private`lam}] + 1]; 
       honeycombPackage`plotHoneycombs[{Private`lam}, {Private`mu}, {
         Private`nu}]]], honeycombPackage`plotHoneycombs[{
       Pattern[Private`lamb, 
        BlankSequence[]]}, {
       Pattern[Private`muu, 
        BlankSequence[]]}, {
       Pattern[Private`nuu, 
        BlankSequence[]]}, 
      Pattern[Private`pos, 
       Blank[]]] := Switch[
      ValueQ[obladePackage`currentgroup], False, 
      obladePackage`oblade[Length[{Private`lamb}] + 1]; 
      honeycombPackage`plotHoneycombs[{Private`lamb}, {Private`muu}, {
        Private`nuu}, Private`pos], True, 
      If[Length[{Private`lamb}] == obladePackage`currentgroup - 1, 
       Private`plotSUNhoneycomb[{{Private`lamb}, {Private`muu}, {
         Private`nuu}}, Private`pos], 
       obladePackage`oblade[Length[{Private`lamb}] + 1]; 
       honeycombPackage`plotHoneycombs[{Private`lamb}, {Private`muu}, {
         Private`nuu}, Private`pos]]], 
    TagSet[honeycombPackage`plotHoneycombs, 
     MessageName[honeycombPackage`plotHoneycombs, "usage"], 
     "plotHoneycombs[lambda,mu,nu]returns the list of all honeycombs \
associated with the branching rule lambda,mu -> nu. To display this list as a \
tableform (with no braces) one can do as follows:  li = \
plotOblades[lambda,mu,nu];  GraphicsRow[li,ImageSize\[Rule] Length[li]*300]. \
The command admits and optional argument pos which is an integer chosen \
between 1 and multiplicity[lambda, mu, nu]; if specified, \
plotHoneycombs[lambda,mu,nu, pos] displays only the particular honeycomb \
numbered pos."], Private`plotAllHoneycombs[
      Pattern[Private`myweights, 
       Blank[]]] := Table[
      Private`plotSUNhoneycomb[Private`myweights, Private`pos], {
      Private`pos, 1, 
       Apply[obladePackage`multiplicity, Private`myweights]}], 
    Private`plotSUNhoneycomb[
      Pattern[Private`myweights, 
       Blank[]], 
      Pattern[Private`pos, 
       Blank[]]] := 
    Module[{Private`la = Part[Private`myweights, 1], Private`mu = 
       Part[Private`myweights, 2], Private`nu = Part[Private`myweights, 3], 
       Private`NN, Private`ptg, Private`sidesparts, Private`gra1, 
       Private`gra2}, 
      Private`sidesparts = 
       Private`sidespartsFct[Private`la, Private`mu, Private`nu, Private`pos]; 
      Private`NN = obladePackage`currentgroup; Private`ptg[1, 1] = {0, 0}; 
      Private`ptg[1, 
         Pattern[Private`s, 
          Blank[]]] := 
       Condition[
        Private`ptg[1, Private`s - 1] + 
         Private`u Part[Private`sidesparts, 3, (-Private`s)/2, -1], 
         EvenQ[Private`s]]; Private`ptg[1, 
         Pattern[Private`s, 
          Blank[]]] := 
       Condition[
        Private`ptg[1, Private`s - 1] - Private`v 
         Part[Private`sidesparts, 2, (Private`s - 1)/2, 1], 
         OddQ[Private`s]]; Private`ptg[
         Pattern[Private`f, 
          Blank[]], 1] := 
       Private`ptg[Private`f - 1, 2] - Private`w 
        Part[Private`sidesparts, 1, Private`f - 1, Private`NN - (Private`f - 
          1)]; Private`ptg[
         Pattern[Private`f, 
          Blank[]], 
         Pattern[Private`s, 
          Blank[]]] := 
       Condition[
        Private`ptg[Private`f, Private`s - 1] + 
         Private`u 
          Part[Private`sidesparts, 3, -(Private`s/2 + Private`f - 1), 
            Private`s/2], 
         EvenQ[Private`s]]; Private`ptg[
         Pattern[Private`f, 
          Blank[]], 
         Pattern[Private`s, 
          Blank[]]] := 
       Condition[
        Private`ptg[Private`f, Private`s - 1] - Private`v 
         Part[Private`sidesparts, 2, (Private`s - 1)/2, Private`f], 
         OddQ[Private`s]]; Private`gra1 = Graphics[
         Join[
          Table[{Black, 
            Point[
             Table[
              Private`ptg[Private`f, Private`s], {
              Private`s, 1, 2 Private`NN - 2 Private`f + 1}]]}, {
           Private`f, 1, Private`NN}], 
          Table[{Blue, 
            Line[
             Table[{
               Private`ptg[Private`f, Private`j], 
               Private`ptg[Private`f, Private`j + 1]}, {
              Private`j, 1, 2 (Private`NN - Private`f)}]]}, {
           Private`f, 1, Private`NN - 1}], 
          Table[{Blue, 
            Line[
             Table[{
               Private`ptg[Private`f, Private`j], 
               Private`ptg[Private`f + 1, Private`j - 1]}, {
              Private`j, 2, 2 (Private`NN - Private`f), 2}]]}, {
           Private`f, 1, Private`NN - 1}], 
          Table[{Red, 
            Table[
             Private`affineSegment[
              Private`ptg[Private`f, Private`j], 2 Private`w], {
             Private`j, 1, 2 Private`NN - Private`f, 2}]}, {Private`f, 1, 1}], 
          Table[{Red, 
            Table[
             Private`affineSegment[
              Private`ptg[Private`f, Private`j], 2 Private`v], {
             Private`j, 1, 1}]}, {Private`f, 1, Private`NN}], 
          Table[{Red, 
            Table[
             Private`affineSegment[
              Private`ptg[Private`f, Private`j], 2 Private`u], {
             Private`j, 2 (Private`NN - Private`f) + 1, 
              2 (Private`NN - Private`f) + 1}]}, {Private`f, 1, Private`NN}]],
          AspectRatio -> 1]; Private`gra2 = Graphics[{
          Table[
           Table[
            Private`segmentLabel[
             Part[Private`sidesparts, 1, Private`f, -Private`j], Black, 
             Private`ptg[Private`f, 2 Private`j], 
             Private`ptg[Private`f + 1, 2 Private`j - 1], {5, 0}], {
            Private`j, 1, Private`NN - Private`f}], {
           Private`f, 1, Private`NN - 1}], 
          Table[
           Table[
            Private`segmentLabel[
             Part[Private`sidesparts, 2, Private`f, Private`j], Black, 
             Private`ptg[Private`j, 2 Private`f], 
             Private`ptg[Private`j, 2 Private`f + 1], {-6, -6}], {
            Private`j, 1, Private`NN - Private`f}], {
           Private`f, 1, Private`NN - 1}], 
          Table[
           Table[
            Private`segmentLabel[
             Part[Private`sidesparts, 3, Private`f, -Private`j], Black, 
             Private`ptg[
             Private`j, 2 Private`NN - 2 Private`j - 2 Private`f + 1], 
             Private`ptg[
             Private`j, 2 Private`NN - 2 Private`j - 2 Private`f + 1 + 1], {
             0, -6}], {Private`j, 1, Private`NN - Private`f}], {
           Private`f, 1, Private`NN - 1}], 
          Table[
           Private`segmentLabel[
            Part[Private`la, Private`f], Blue, Bold, 
            Private`ptg[Private`f, 1] - Private`w, 
            Private`ptg[Private`f + 1, 1] - Private`w, {25, -10}], {
           Private`f, 1, Private`NN - 1}], 
          Table[
           Private`segmentLabel[
            Part[Private`mu, Private`NN - Private`f], Blue, Bold, 
            Private`ptg[Private`f, 2 Private`NN - 2 Private`f + 1] - 
            Private`w, 
            Private`ptg[Private`f + 1, 2 Private`NN - 2 Private`f - 1] - 
            Private`w, {-40, -10}], {Private`f, 1, Private`NN - 1}], 
          Table[
           Private`segmentLabel[
            Part[Private`nu, Private`NN - Private`f], Blue, Bold, 
            Private`ptg[1, 2 Private`NN - 2 Private`f - 1] - Private`w, 
            Private`ptg[1, 2 Private`NN - 2 Private`f + 1] - Private`w, {-10, 
            60}], {Private`f, 1, Private`NN - 1}]}]; 
      Show[Private`gra1, Private`gra2]], Private`sidespartsFct[
      Pattern[Private`lambda, 
       Blank[]], 
      Pattern[Private`mu, 
       Blank[]], 
      Pattern[Private`nu, 
       Blank[]], 
      Pattern[Private`pos, 
       Blank[]]] := 
    Module[{Private`ew, Private`nesw, Private`nwse}, Private`ew = Part[
         obladePackage`edgesEWvalues[Private`lambda, Private`mu, 
          Reverse[Private`nu]], Private`pos]; Private`nesw = Part[
         obladePackage`edgesNESWvalues[Private`lambda, Private`mu, 
          Reverse[Private`nu]], Private`pos]; Private`nwse = Part[
         obladePackage`edgesNWSEvalues[Private`lambda, Private`mu, 
          Reverse[Private`nu]], Private`pos]; {
       Private`ew, Private`nesw, Private`nwse}], 
    obladePackage`edgesEWvalues[{
       Pattern[Private`lam, 
        BlankSequence[]]}, {
       Pattern[Private`mu, 
        BlankSequence[]]}] := Part[
      Private`componentsEdgesAndnuc[{Private`lam}, {Private`mu}], 2], 
    obladePackage`edgesEWvalues[{
       Pattern[Private`lam, 
        BlankSequence[]]}, {
       Pattern[Private`mu, 
        BlankSequence[]]}, {
       Pattern[Private`nuc, 
        BlankSequence[]]}] := Part[
      Private`componentsEdgesAndnuc[{Private`lam}, {Private`mu}, {
       Private`nuc}], 2], 
    TagSet[obladePackage`edgesEWvalues, 
     MessageName[obladePackage`edgesEWvalues, "usage"], 
     "edgesEWvalues[lambda,mu,nucc] returns the list of East-West edges \
values for the branching rule lambda \[CircleTimes] mu \[CircleTimes] nucc -> \
1 or la \[CircleTimes] mu \[Rule] nu with nuc = Reverse[nu]."], 
    obladePackage`edgesNESWvalues[{
       Pattern[Private`lam, 
        BlankSequence[]]}, {
       Pattern[Private`mu, 
        BlankSequence[]]}] := Part[
      Private`componentsEdgesAndnuc[{Private`lam}, {Private`mu}], 3], 
    obladePackage`edgesNESWvalues[{
       Pattern[Private`lam, 
        BlankSequence[]]}, {
       Pattern[Private`mu, 
        BlankSequence[]]}, {
       Pattern[Private`nuc, 
        BlankSequence[]]}] := Part[
      Private`componentsEdgesAndnuc[{Private`lam}, {Private`mu}, {
       Private`nuc}], 3], 
    TagSet[obladePackage`edgesNESWvalues, 
     MessageName[obladePackage`edgesNESWvalues, "usage"], 
     "edgesEWvalues[lambda,mu,nucc] returns the list of NorthEast-SouthWest \
edges values for the branching rule lambda \[CircleTimes] mu \[CircleTimes] \
nucc -> 1 or la \[CircleTimes] mu \[Rule] nu with nuc = Reverse[nu]."], 
    obladePackage`edgesNWSEvalues[{
       Pattern[Private`lam, 
        BlankSequence[]]}, {
       Pattern[Private`mu, 
        BlankSequence[]]}] := Part[
      Private`componentsEdgesAndnuc[{Private`lam}, {Private`mu}], 4], 
    obladePackage`edgesNWSEvalues[{
       Pattern[Private`lam, 
        BlankSequence[]]}, {
       Pattern[Private`mu, 
        BlankSequence[]]}, {
       Pattern[Private`nuc, 
        BlankSequence[]]}] := Part[
      Private`componentsEdgesAndnuc[{Private`lam}, {Private`mu}, {
       Private`nuc}], 4], 
    TagSet[obladePackage`edgesNWSEvalues, 
     MessageName[obladePackage`edgesNWSEvalues, "usage"], 
     "edgesEWvalues[lambda,mu,nucc] returns the list of NorthWest-SouthEast \
edges values for the branching rule lambda \[CircleTimes] mu \[CircleTimes] \
nucc -> 1 or la \[CircleTimes] mu \[Rule] nu with nuc = Reverse[nu]."], 
    Private`w = {0, -1}, Private`affineSegment[
      Pattern[Private`point, 
       Blank[]], 
      Pattern[Private`vect, 
       Blank[]]] := Line[{Private`point, Private`point + Private`vect}], 
    Private`segmentLabel[
      Pattern[Private`txt, 
       Blank[]], 
      Pattern[Private`color, 
       Blank[]], 
      Pattern[Private`p, 
       Blank[]], 
      Pattern[Private`q, 
       Blank[]], 
      Pattern[Private`off, 
       Blank[]]] := Text[
      Style[Private`txt, Private`color, 15], (Private`p + Private`q)/2, 
      Offset[Private`off, {0, -1}]], Private`segmentLabel[
      Pattern[Private`txt, 
       Blank[]], 
      Pattern[Private`color, 
       Blank[]], Bold, 
      Pattern[Private`p, 
       Blank[]], 
      Pattern[Private`q, 
       Blank[]], 
      Pattern[Private`off, 
       Blank[]]] := Text[
      Style[Private`txt, Private`color, Bold, 24], (Private`p + Private`q)/2, 
      
      Offset[Private`off, {0, 0}]], Private`segmentLabel[
      Pattern[Private`txt, 
       Blank[]], 
      Pattern[Private`color, 
       Blank[]], "SemiBold", 
      Pattern[Private`p, 
       Blank[]], 
      Pattern[Private`q, 
       Blank[]], 
      Pattern[Private`off, 
       Blank[]]] := Text[
      Style[
      Private`txt, Private`color, "SemiBold", 24], (Private`p + Private`q)/2, 
      
      Offset[Private`off, {0, 0}]], obladePackage`tensorproduct[{
       Pattern[Private`lam, 
        BlankSequence[]]}, {
       Pattern[Private`mu, 
        BlankSequence[]]}] := Switch[
      ValueQ[obladePackage`currentgroup], False, 
      obladePackage`oblade[Length[{Private`lam}] + 1]; 
      obladePackage`tensorproduct[{Private`lam}, {Private`mu}], True, 
      If[Length[{Private`lam}] == obladePackage`currentgroup - 1, 
       SortBy[
        Tally[
         Map[Reverse, 
          Private`tensorproductCC[{Private`lam}, {Private`mu}]]], Last], 
       obladePackage`oblade[Length[{Private`lam}] + 1]; 
       obladePackage`tensorproduct[{Private`lam}, {Private`mu}]]], 
    obladePackage`tensorproduct[
      Pattern[$CellContext`mat, 
       Blank[]]] := Apply[obladePackage`tensorproduct, $CellContext`mat], 
    TagSet[obladePackage`tensorproduct, 
     MessageName[obladePackage`tensorproduct, "usage"], 
     "tensorproduct[lambda,mu] returns the decomposition into irreducible \
components nu of a tensor product of two irreps lambda, mu, of the Lie group \
SU(n), given by the components of their highest weights along the basis of \
fundamental weights.  The last entry of each term is its multiplicity."], 
    Attributes[$CellContext`makeRow$] = {Temporary}, 
    Attributes[$CellContext`rowIndex$] = {Temporary}, 
    Attributes[$CellContext`colIndex$] = {
     Temporary}, $CellContext`abouttabletext = {
     "A triple of irreducible representations (\[Omega]1, \[Omega]2, \
\[Omega]3) specified by their higest weights is admissible if \[Omega]1 \
\[CircleTimes] \[Omega]2 \[CircleTimes] \[Omega]3 contains \
\[DoubleStruckCapitalC] (the scalars),", 
      "or equivalently if the decomposition of \[Omega]1 \[CircleTimes] \
\[Omega]2 into a direct sum contains \!\(\*OverscriptBox[\(\[Omega]3\), \
\(_\)]\) (the conjugate of \[Omega]3).", 
      "Lines of the above table are the components of three highest weights (\
\[Omega]1, \[Omega]2; \!\(\*OverscriptBox[\(\[Omega]3\), \(_\)]\)) in the \
basis of fundamental weights (Dynkin basis).", 
      "The triple (\[Omega]1, \[Omega]2, \[Omega]3) should be admissible.", 
      "", "FILL THE ABOVE TABLE (USE TABS) AND HIT \"Update Components\".", 
      "If needed, the decomposition of \[Omega]1 \[CircleTimes] \[Omega]2 as \
a direct sum is obtained by clicking the button \"Tensor product \
decomposition of \[Omega]1 \[CircleTimes] \[Omega]2 \"; the last entry of \
each term returned by this command is its multiplicity; warning: the output \
can be quite long. "}, $CellContext`aboutobladetext = {
     "A triplet of multiplicity m is associated with m distinct couplings \
represented by pictographs.", 
      "There are several types of pictographs: BZ-triangles, KT-honeycombs, \
O-blades. Here we display O-blades (or oblades).", 
      "Display all pictographs, one at a time, by increasing or descreasing \
the count index (buttons Next pictograph, Previous pictograph, or the popup \
menu)."}}]], "Output",ExpressionUUID->"5fea6883-1b69-49ae-8fc3-87e4e445543b"]
},
WindowSize->{1375, 852},
Visible->True,
ScrollingOptions->{"VerticalScrollRange"->Fit},
ShowCellBracket->Automatic,
Deployed->True,
CellContext->Notebook,
TrackCellChangeTimes->False,
FrontEndVersion->"11.2 for Mac OS X x86 (32-bit, 64-bit Kernel) (September \
10, 2017)",
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
Cell[1488, 33, 64324, 1419, 899, "Output",ExpressionUUID->"5fea6883-1b69-49ae-8fc3-87e4e445543b"]
}
]
*)

(* End of internal cache information *)

(* NotebookSignature @wpP5KZFRlhAED1C@Cz5kTmg *)
