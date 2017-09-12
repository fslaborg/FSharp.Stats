(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../packages/build/FSharp.Plotly/lib/net40/Fsharp.Plotly.dll"
open FSharp.Plotly
(**
#Statistical testing


FSharp.Stats a provides 
A hypothesis test is a statistical test that is used to determine whether there is enough evidence 
in a sample of data to infer that a certain condition is true for the entire population. 
A hypothesis test examines two opposing hypotheses about a population: the null hypothesis and the alternative hypothesis.

<a name="TestStatistics"></a>

##Test Statistics

<a name="Anova"></a>

##Anova

<a name="TTest"></a>

##T-Test

<a name="ChiSquareTest"></a>

##Chi-Squared Test

<a name="Bartlett"></a>

##Bartlett

<a name="PostHoc"></a>

##PostHoc

<a name="PvalueAdjust"></a>

##Adjusting P-Values

*)
#r "FSharp.Stats.dll"
open FSharp.Stats
open FSharp.Stats.Testing


let sample1 = [|-0.2268419965; -0.3772357485|] |> FSharp.Stats.Vector.ofArray
let sample2 = [|-0.6076633409; -0.1781469665|] |> FSharp.Stats.Vector.ofArray

Testing.TTest.twoSample false sample1  sample2



let contrastMatrix = 
    [| [|1.0;-1.0;0.0;|] ; [|0.;-1.0;1.0;|] ; [|-1.0;0.;1.0;|] ; |]


Testing.Hays contrastMatrix 

