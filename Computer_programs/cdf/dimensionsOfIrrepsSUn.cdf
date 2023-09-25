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
NotebookDataLength[      4254,        117]
NotebookOptionsPosition[      4762,        111]
NotebookOutlinePosition[      5229,        132]
CellTagsIndexPosition[      5186,        129]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[
 DynamicModuleBox[{$CellContext`rows$$ = 1, $CellContext`cols$$ = 
  3, $CellContext`mat$$ = {{2, 3, 4}}, $CellContext`myRange$$}, 
  DynamicBox[ToBoxes[
    Manipulate[
     Row[{"Dimension of", 
       MatrixForm[$CellContext`mat$$], " = ", 
       $CellContext`dimensionIrrepSU[
        First[$CellContext`mat$$]]}], 
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
         Grid[
          Map[$CellContext`makeRow$, 
           $CellContext`myRange$$[$CellContext`rows$$]]], 
         Invisible["xxxxxx"], 
         Column[{
          "Dimension of an irreducible representations specified by its \
higest weight.", 
           "Entries refer to the n-1 components of the chosen highest weights \
in the basis of fundamental weights (Dynkin basis).", 
           Style["Use TAB or RETURN to update.", Brown], 
           Style["On IOS, use the Update button.", Brown]}]}]]], 
     Row[{
       Column[{
         Panel[
          Style[$CellContext`cols$$ + 1, FontSize -> 14, Bold], 
          Style["SU(n), with n =", Bold, FontSize -> 12], Left, 
          BaselinePosition -> Bottom], 
         Button[
          Style["Update", Bold, FontSize -> 12, Blue], 
          Increment[$CellContext`rows$$]; Decrement[$CellContext`rows$$]; 
          Null]}], 
       Panel[
        Style[
         Column[{"Dimension of an irreducible", "representation of SU(n)"}, 
          Center], "Title"]]}, 
      Invisible["xxxxxx"]], 
     Button[
     "Decrease n", $CellContext`mat$$ = 
       Map[Drop[#, -1]& , $CellContext`mat$$]; Decrement[$CellContext`cols$$]; 
      Null], 
     Button["Increase n", (AppendTo[
         Part[$CellContext`mat$$, 1], 0]; Null); 
      Increment[$CellContext`cols$$]; Null]], StandardForm],
   ImageSizeCache->{673., {202., 208.}}],
  SynchronousUpdating -> False,
  DynamicModuleValues:>{{DownValues[$CellContext`myRange$$] = {HoldPattern[
         $CellContext`myRange$$[
          Pattern[$CellContext`s, 
           Blank[]]]] :> Range[$CellContext`s]}}},
  Initialization:>{$CellContext`dimensionIrrepSU[
      Pattern[$CellContext`ruplet, 
       Blank[]]] := 
    With[{$CellContext`rk = Length[$CellContext`ruplet]}, Product[
        With[{$CellContext`p = $CellContext`pval}, 
         Product[$CellContext`p + Sum[
            
            Part[$CellContext`ruplet, $CellContext`t], {$CellContext`t, \
$CellContext`s, $CellContext`s + $CellContext`p - 1}], {$CellContext`s, 
           1, $CellContext`rk - ($CellContext`p - 1)}]], {$CellContext`pval, 
         1, $CellContext`rk}]/Product[
       Factorial[$CellContext`x], {$CellContext`x, 1, $CellContext`rk}]], 
    Attributes[$CellContext`makeRow$] = {Temporary}, 
    Attributes[$CellContext`rowIndex$] = {Temporary}, 
    Attributes[$CellContext`colIndex$] = {Temporary}}]], "Output",
 GeneratedCell->False,
 CellAutoOverwrite->
  False,ExpressionUUID->"60f562a6-3fb7-46ba-ab57-cbfea01d6198"]
},
WindowSize->{808, 752},
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
Cell[1488, 33, 3270, 76, 429, "Output",ExpressionUUID->"60f562a6-3fb7-46ba-ab57-cbfea01d6198"]
}
]
*)

(* End of internal cache information *)

(* NotebookSignature oupB4oORg1yQ5D16zoeNHm64 *)
