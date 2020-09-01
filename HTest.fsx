(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r @"C:\Users\Selly\source\repos\FSharp.Stats\bin\FSharp.Stats\netstandard2.0\FSharp.Stats.dll"
#r @"C:\Users\Selly\source\repos\FSharp.Plotly-developer\FSharp.Plotly-developer\bin\FSharp.Plotly\netstandard2.0\FSharp.Plotly.dll"


open FSharp.Plotly
open FSharp.Plotly.Axis
open FSharp.Plotly.StyleParam

let myAxis title = LinearAxis.init(Title=title,Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=true)
let myAxisRange title range = LinearAxis.init(Title=title,Range=Range.MinMax range,Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=true)
let styleChart x y chart = chart |> Chart.withX_Axis (myAxis x) |> Chart.withY_Axis (myAxis y)
let styleChartRange x y rx ry chart = chart |> Chart.withX_Axis (myAxisRange x rx) |> Chart.withY_Axis (myAxisRange y ry)

(**
#Statistical testing
FSharp.Stats provides hypothesis tests for different applications.
A hypothesis test is a statistical test that is used to determine whether there is enough evidence 
in a sample of data to infer that a certain condition is true for the entire population. 
A hypothesis test examines two opposing hypotheses about a population: the null hypothesis and the alternative hypothesis.
<a name="TestStatistics"></a>
##Test Statistics
<a name="Anova"></a>
##Anova
*)

open FSharp.Stats
open FSharp.Stats.Testing

(** 
< a name = "HTest"></a>
##H-Test
The H test is also known as Kruskal-Wallis one-way analysis-of-variance-by-ranks and is the nonparametric equivalent of one-way ANOVA. 
It is a non-parametric test for comparing the means of more than two independent samples (equal or different sample size), and therefor is an extension of Wilcoxon-Mann-Whitney two sample test.
Testing with H test gives information whether the samples are from the same distribution.
A benefit of the H-test is, that it does not require normal distribution of the samples.
The downside is that there is no information which samples are different from each other, or how many differences occur. For further investigation a Post Hoc test is recommended. 
    Prerequisite : 
        - random and independent samples
        - observations are from populations with same shape of distribution
        - nominal scale, ordinal scale, ratio scale or interval scale data
The distribution of the H test statistic is approximated by chi-square distribution with degrees of freedom - 1. 

References : 
        - E. Ostertagová,  Methodology and Application of the Kruskal-Wallis Test (2014)
        - Y. Chan, RP Walmsley, Learning and understanding the Kruskal-Wallis one-way analysis-of-variance-by-ranks test for differences among three or more independent groups (1997)

*H-test*
input : seq{seq<float>} 
*)

let groupA = seq {23.;41.;54.;66.;78.} 
let groupB = seq {45.;55.;60.;70.;72.}
let groupC = seq {18.;30.;34.;40.;44.} 
let samples = seq{groupA;groupB;groupC}

// calculation of p-Value