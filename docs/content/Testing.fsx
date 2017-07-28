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



Testing.TTest.twoSample false ([|-0.2268419965; -0.3772357485|]|> FSharp.Stats.Vector.ofArray)  ([|-0.6076633409; -0.1781469665|]|> FSharp.Stats.Vector.ofArray)


/// Returns SummeryStats of vector with N, mean, sum-of-squares, minimum and maximum
let inline stats (a:Vector<'T>) =
    let zero = LanguagePrimitives.GenericZero< 'T > 
    let one = LanguagePrimitives.GenericOne< 'T >        
        
    let rec loop index n (minimum) (maximum) m1 m2 =
        if index < a.Length then            
            let current  = a.[index]
            let delta    = current - m1               
            let delta_n  = (delta / n)
            let delta_n2 = delta_n * delta_n
            let m1'    = m1 + delta_n            
            let m2' = m2 + delta * delta_n * (n-one)
            loop (index+1) (n + one) (min current minimum) (max current maximum) m1' m2'
        else
            SummeryStats.createSummeryStats (n-one) m1 m2 minimum maximum
    //Init by fist value
    if a.Length > 1 then
        loop 0 one a.[0] a.[0] zero zero 
    else
        let uNan = zero / zero 
        SummeryStats.createSummeryStats zero uNan uNan uNan uNan



vector [|-0.2268419965; -0.3772357485|]  |> stats |> SummeryStats.var

[|-0.2268419965; -0.3772357485|] |> Seq.average

let inline dbyint a =
    a / 6



