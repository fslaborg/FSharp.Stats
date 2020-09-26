(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Stats/netstandard2.0"
#r @"FSharp.Stats.dll"
#r "netstandard.dll"
#r @"../../lib/Formatting/FSharp.Plotly.dll"

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

// http://astatsa.com/OneWay_Anova_with_TukeyHSD/
let dataOneWay =
    [|
    [|0.28551035; 0.338524035; 0.088313218; 0.205930807; 0.363240102;|];
    [|0.52173913; 0.763358779; 0.32546786; 0.425305688; 0.378071834; |];
    [|0.989119683; 1.192718142; 0.788288288; 0.549176236; 0.544588155;|];
    [|1.26705653; 1.625320787; 1.266108976; 1.154187629; 1.268498943;|]// 1.069518717;|];
    |]

let contrastMatrix = 
    [| 
    [|1.0;-1.0;0.0;0.0;|]
    [|1.0;0.0;-1.0;0.0;|]
    [|1.0;0.0;0.0;-1.0;|]
    [|0.0;1.0;-1.0;0.0;|]
    [|0.0;1.0;0.0;-1.0;|]
    [|0.0;0.0;1.0;-1.0;|]
    |]

let oneWayResult = Anova.oneWayAnova dataOneWay

(*** include-value:oneWayResult ***)

(*
anovaResult.Factor.Statistic = 27.758
The factor statistic indicates how much more variability there is between the the samples 
than within the samples.
anovaResult.Factor.PValue = 1.406712119e-06
A strong significant p value in the factor field indicates that one or more means differ from each other
*)

// https://www.wessa.net/rwasp_Two%20Factor%20ANOVA.wasp

let data =
    [
        (0.28, 'A', 'M');
        (0.95, 'A', 'M');
        (0.96, 'A', 'M');
        (0.97, 'A', 'M');
        (0.40, 'A', 'M');
        (0.18, 'A', 'M');
        (0.12, 'A', 'M');
        (0.62, 'A', 'M');
        (1.81, 'A', 'F');
        (1.51, 'A', 'F');
        (1.41, 'A', 'F');
        (1.39, 'A', 'F');
        (1.20, 'A', 'F');
        (1.55, 'A', 'F');
        (1.48, 'A', 'F');
        (1.25, 'A', 'F');
        (0.95, 'B', 'M');
        (1.33, 'B', 'M');
        (0.92, 'B', 'M');
        (0.85, 'B', 'M');
        (1.06, 'B', 'M');
        (0.69, 'B', 'M');
        (0.70, 'B', 'M');
        (0.79, 'B', 'M');
        (2.93, 'B', 'F');
        (3.24, 'B', 'F');
        (3.42, 'B', 'F');
        (2.79, 'B', 'F');
        (2.54, 'B', 'F');
        (3.28, 'B', 'F');
        (2.80, 'B', 'F');
        (3.40, 'B', 'F');
    ]
    //f1
    |> Seq.groupBy (fun (v,f1,f2) -> f1)
    //f2
    |> Seq.map (fun (k,vls) -> 
        vls 
        |> Seq.groupBy (fun (v,f1,f2) -> f2)
        |> Seq.map (fun (k,vls') -> vls' |> Seq.map (fun (v,f1,f2) -> v) |> Seq.toArray)
        |> Seq.toArray
        ) 
    |> Seq.toArray
    

Anova.twoWayANOVA Anova.TwoWayAnovaModel.Mixed data


// http://statweb.stanford.edu/~susan/courses/s141/exanova.pdf
// http://scistatcalc.blogspot.de/2013/11/two-factor-anova-test-calculator.html#

let data' =
    [|
      // super
        // cold super
      [|[|4.;5.;6.;5.;|];
        // warm super
        [|7.;9.;8.;12.;|];
        // hot super
        [|10.;12.;11.;9.; |]|];
      // best 
        // cold best
      [|[|6.;6.;4.;4.;|];
        // warm best
        [|13.;15.;12.;12.;|];
        // hot best
        [|12.;13.;10.;13.;|]|]
    |]

Anova.twoWayANOVA Anova.TwoWayAnovaModel.Mixed data'


(**
<a name="TTest"></a>
##T-Test

By using a t test a difference of means can be evaluated. There are different kinds of t test designs implemented in FSharp.Stats.

1. One sample t test:

  - Is the population mean equal to the value of H0?

  - e.g. “Is my grade different from the distribution mean?”
  
2. Two sample t test with equal variances:

  - Prerequisite: equal variances
  
  - Is the mean of Population A equal to the mean of Population B?
  
  - 2.1 unpaired t test:

    - e.g.: Does the cell size differ between wildtype and mutant?

  - 2.2 paired t test:

    - Bsp.: Does the medication influence the blood pressure? Measurement of the same persons before and after medication.

3. Two sample t test with unequal variances
  
  - Welch test (unpaired)
  


*Case 1: One sample t test*

*)

let sampleA = vector [|4.5; 5.1; 4.8; 4.4; 5.0|]

// calculates a one sample t test with a given sample and the fixed value the sample should be compared with
let oneSampleTTest = TTest.oneSample sampleA 5.

(*
    The test returns no significant p value:
    oneSampleTTest.PValue = 0.1533
*)


(**

*Case 2: Two sample t test with equal variances (unpaired)*

A standard two sample t test expects the samples to be taken from populations with equal standard deviations.
Violations of this requirement result in an inflated false positive rate.

*)

let sample1 = vector [|4.9;5.0;6.7;4.8;5.2|]
let sample2 = vector [|3.9;4.9;3.8;4.5;4.5|]

let twoSampleTTest = TTest.twoSample true sample1 sample2

(*
    The test returns a significant p value:
    twoSampleTTest.PValue = 0.0396
*)


(**

*Case 3: Two sample t test with equal variances (paired)*


A paired t-test is used to compare two population means where you have two samples in
which observations in one sample can be paired with observations in the other sample.
Examples of where this might occur are:

-   Before-and-after observations on the same subjects (e.g. students’ diagnostic test
    results before and after a particular module or course).
-   A comparison of two different methods of measurement or two different treatments
    where the measurements/treatments are applied to the same subjects (e.g. blood
    pressure measurements using a stethoscope and a dynamap).
*)


let sampleP1 = vector [18.;21.;16.;22.;19.;24.;17.;21.;23.;18.;14.;16.;16.;19.;18.;20.;12.;22.;15.;17.;]
let sampleP2 = vector [22.;25.;17.;24.;16.;29.;20.;23.;19.;20.;15.;15.;18.;26.;18.;24.;18.;25.;19.;16.;]

let paired = TTest.twoSamplePaired sampleP1 sampleP2

(*
    The test returns a significant p value:
    paired.PValue = 0.00439
*)


(**

*Case 4: Two sample t test with unequal variances (Welch test)*

If you are unsure about the nature of the underlying population, you may ask if the theoretical population distributions 
you want to compare do have the same standard deviations. 

If not the welch test can serve as a appropriate hypothesis test for mean differences.

*)


let sampleW1 = vector [0.8;0.9;1.0;1.1;1.2]
let sampleW2 = vector [0.8;1.1;1.3;1.5;2.0]

let welch = TTest.twoSample false sampleW1 sampleW2

(*
    The test returns a not significant p value:
    welch.PValue = 0.1725626595
*)

(**
<a name = "FTest"></a>
##F-Test
The F-test is a method to determine if the variances of two samples are homogeneous.
Also, ANOVA (analysis of variance) is based on the F-test and is used for the comparison of more than two samples.
Knowing if your variances are equal (H<sub>0</sub> is true) helps to decide which test should be performed next.
To determine if your variances are in fact equal you can perform a F-test. 

Prerequisites: 

  -   population distributions are normally distributed 

  -   samples are independent

Possible further tests:

  -   two sample t-test with equal variances

  -   two sample t-test with unequal variances (Welch-test)

  -   ...

Note that there is no information about the direction of variance difference.
In this implemented test the larger variance is always the numerator, therefore the comparison to F<sub>df1,df2,1-(alpha/2)</sub> is used for a two sided test. 

_Important note: The effectiveness of a preliminary test of variances is discussed. 
The F-test is extremely sensitive to normality-violations, and even if the samples follow a normal distribution, it often does not detect situations where a t-test should be avoided._ 

References:

  - Jürgen Bortz & Christof Schuster, Statistik für Human- und Sozialwissenschaftler (2010) Chapter 8.6

  - Markowski, Conditions for the Effectiveness of a Preliminary Test of Variance (1990)

  - Shlomo S. Sawilowsky, The Probable Difference Between Two Means When σ<sub>1</sub><sup>2</sup>≠σ<sub>2</sub><sup>2</sup> (2002)

  - Ronald Ley, F curves have two tails but the F test is a one-tailed two-tailed test (1979) + Reviewer comments
  
*F-Test from data:*
*)

let sampleFA = vector [|5.0; 6.0; 5.8; 5.7|] 
let sampleFB = vector [|3.5; 3.7; 4.0; 3.3; 3.6|]

// comparison of sample variances 
let fTestFromData = FTest.testVariances sampleFA sampleFB
(* 
    { Statistic = 2.823383085
    DegreesOfFreedom1 = 3.0
    DegreesOfFreedom2 = 4.0
    PValue = 0.1708599931 }
    Using a significance level of 0.05 the sample variances do differ significantly.
*)
(** 
*F-Test from given parameters:*
*)

// sample properties are given as (variance,degree of freedom) 
let sampleF1 = (0.1, 15.)
let sampleF2 = (0.05, 7.)

// comparison of sample variances 
let fTestFromParameters = FTest.testVariancesFromVarAndDof sampleF1 sampleF2
(*
    { Statistic = 2.0
    DegreesOfFreedom1 = 15.0
    DegreesOfFreedom2 = 7.0
    PValue = 0.17963663 }
    Using a significance level of 0.05 the sample variances do differ significantly.
*)

(** 
##H Test
The H test is also known as Kruskal-Wallis one-way analysis-of-variance-by-ranks and is the non-parametric equivalent of one-way ANOVA. 
It is a non-parametric test for comparing the means of more than two independent samples (equal or different sample size), and therefore is an extension of Wilcoxon-Mann-Whitney two sample test.
Testing with H test gives information whether the samples are from the same distribution.

A benefit of the H test is that it does not require normal distribution of the samples.
The downside is that there is no information which samples are different from each other, or how many differences occur. For further investigation a post hoc test is recommended. 

The distribution of the H test statistic is approximated by chi-square distribution with degrees of freedom = sample count - 1. 

The implemented H-test is testing for duplicate values in the data. 
Duplicates lead to ties in the ranking, and are corrected by using a correction factor. 

Prerequisites:

  - random and independent samples

  - observations are from populations with same shape of distribution

  - nominal scale, ordinal scale, ratio scale or interval scale data

References:

  - E. Ostertagová,  Methodology and Application of the Kruskal-Wallis Test (2014)

  - Y. Chan, RP Walmsley, Learning and understanding the Kruskal-Wallis one-way analysis-of-variance-by-ranks test for differences among three or more independent groups (1997)

*)

let groupA = [44.;44.;54.;32.;21.;28.]
let groupB = [70.;77.;48.;64.;71.;75.]
let groupC = [80.;76.;34.;80.;73.;80.] 
let samples = [groupA;groupB;groupC]

// calculation of the H test 
let hResult = HTest.createHTest samples 

(*** include-value:hResult ***)

(**
_PValueRight is significant at a alpha level of 0.05_
*)


(**
<a name="ChiSquareTest"></a>
##Chi-Squared Test
<a name="Bartlett"></a>
##Bartlett
<a name="PostHoc"></a>
##PostHoc

ANOVA provides the user with a global statement if samples differ from each other. It does not provide detailed information regarding
differences of the single samples.

If the H<sub>0</sub> hypothesis is neglected (so significant differences are assumed), a post hoc test (multiple comparison test) allows the pairwise comparison of the 
individual groups.

Reference: What is the proper way to apply the multiple comparison test?, Sangseok Lee and Dong Kyu Lee, 2018

###Fisher's LSD

The most simple method is Fisher's least significant difference (Fisher's LSD). It calculates Student's t tests for all pairwise comparisons. But instead of 
estimating the variance for each sample separately it takes all groups into account. Violations of the homogeneity of variances reduce the test power.
Since no correction for multiple comparisons is performed, the resulting p values must be corrected (for example with Benjamini-Hochberg method).


*)
open PostHoc

let lsd = Testing.PostHoc.fishersLSD contrastMatrix dataOneWay

// For multi comparison correction, the p values are adjusted by the Benjamini-Hochberg approach
let (index,pValue,pValueAdj) = 
    lsd
    |> Testing.MultipleTesting.benjaminiHochbergFDRBy (fun x -> x,x.Significance)
    |> List.sortBy (fun (x,_) -> x.Index)
    |> List.map (fun (x,pValAdj) -> x.Index, x.Significance, pValAdj)
    |> List.unzip3

(*** hide ***)

let lsd_Corrected =
    let header = ["<b>Contrast index</b>";"<b>p Value</b>";"<b>p Value adj</b>"]
    let rows = 
        [
            [sprintf "%i" index.[0];sprintf "%.6f" pValue.[0];sprintf "%.6f" pValueAdj.[0];]    
            [sprintf "%i" index.[1];sprintf "%.6f" pValue.[1];sprintf "%.6f" pValueAdj.[1];]    
            [sprintf "%i" index.[2];sprintf "%.6f" pValue.[2];sprintf "%.6f" pValueAdj.[2];]       
            [sprintf "%i" index.[3];sprintf "%.6f" pValue.[3];sprintf "%.6f" pValueAdj.[3];]       
            [sprintf "%i" index.[4];sprintf "%.6f" pValue.[4];sprintf "%.6f" pValueAdj.[4];]    
            [sprintf "%i" index.[5];sprintf "%.6f" pValue.[5];sprintf "%.6f" pValueAdj.[5];]
        ]
    
    Chart.Table(
        header, 
        rows,
        ColorHeader = "#45546a",
        ColorCells = ["#deebf7";"lightgrey"],
        FontHeader = Font.init(Color="white")
        )

(*** include-value:lsd_Corrected ***)

(**
###Hays
*)

Testing.PostHoc.hays contrastMatrix dataOneWay 


//let m1 = Seq.mean dataOneWay.[0]
//let m2 = Seq.mean dataOneWay.[1]

//let s1 = Seq.sum dataOneWay.[0]
//let s2 = Seq.sum dataOneWay.[1]
//let s3 = Seq.sum dataOneWay.[2]


//let d = (m1-m2)**2.0
//d / (3.926003843 * (1./5. + 1./5.))

// http://www.statisticslectures.com/topics/posthoconewayanova/
let dmg = 
    [|
     [|9.;8.;7.;8.;8.;9.;8.;|];
     [|7.;6.;6.;7.;8.;7.;6.;|];
     [|4.;3.;2.;3.;4.;3.;2.;|] ; 
    |]

let contrastMatrixDmg = 
    [| 
     [|1.0;-1.0;0.0;|] ; 
     [|1.0;0.0;-1.0;|] ; 
     [|0.0;1.0;-1.0;|]   
    |]


Testing.PostHoc.hays contrastMatrixDmg dmg 

Anova.oneWayAnova dmg

(**
###Tukey HSD 

Tukeys honestly significant difference (HSD) can be used to inspect a significant ANOVA result for underlying causes.

_Important note: Various discussions question the application of Tukeys HSD only to significant ANOVA results (Anova-protected post-hoc test), since Tukeys HSD already controls for multiple testing._
_Inform yourself prior to using an post hoc test appropriate to your experimental design._

Using this post hoc test you can determine which of the means differ significantly from each other.
In the classis Tukeys HSD approach, the population variances are pooled for a more robust estimation (especially with small sample sizes). If the population variances
differ, Tukeys HSD is not appropriate. 

The implemented Tukey-Kramer-Method can be applied on unequal sample sizes and estimates the variance based on the meas to compare.
The Tukey-Kramer method can be used as stand-alone method for comparing multiple means.

A comparison of ANOVA and Tukey-Kramer-HSD with simulations for the robustness of normality-violations of the data can be found in:

_Robustness of the ANOVA and Tukey-Kramer Statistical Tests, Wade C. Driscoll, Computers ind. Engng Vol 31, No. 1/2, pp. 265 - 268, 1996_


Multiple-to-one comparisons can alternatively performed with Dunnet's test, which was designed for performing k-1 tests, while Tukeys
HSD performs k((k-1)/2) tests.

Task: It should be tested if one or more means of samples taken from different mutants differ from each other.

*)


// Example values from: https://brownmath.com/stat/anova1.htm
let hsdExample = 
    [|
        [|64.; 72.; 68.; 77.; 56.; 95.;|] // sample of mutant/drug/factor 1 
        [|78.; 91.; 97.; 82.; 85.; 77.;|] // sample of mutant/drug/factor 2 
        [|75.; 93.; 78.; 71.; 63.; 76.;|] // sample of mutant/drug/factor 3 
        [|55.; 66.; 49.; 64.; 70.; 68.;|] // sample of mutant/drug/factor 4    
    |]


let anovaResult = Anova.oneWayAnova hsdExample


(*
    anovaResult.Factor.Statistic = 5.41
    The factor statistic indicates how much more variability there is between the the samples 
    than within the samples.
    anovaResult.Factor.PValue = 0.0069
    A significant p value in the factor field indicates that one or more means differ from each other
*)

(**
For tukey HSD calculations you need a contrast matrix, that defines the groups you want to compare for 
more detailed information.

Every contrast has as many entries as there are groups (samples). The groups, that should be compared are labeled with -1 or 1 respectively.

*)

// In this contrast matrix every possible scenario is covered.
let contrastMatrixHSD = 
    [| 
    [|1.;-1.; 0.; 0.;|] // sample 1 is compared to sample 2
    [|1.; 0.;-1.; 0.;|] // sample 1 is compared to sample 3
    [|1.; 0.; 0.;-1.;|] 
    [|0.; 1.;-1.; 0.;|]
    [|0.; 1.; 0.;-1.;|]
    [|0.; 0.; 1.;-1.;|] // sample 3 is compared to sample 4
    |]


let hsdExample' = 
    [|
        [|0.0571;0.0813;0.0831;0.0976;0.0817;0.0859;0.0735;0.0659;0.0923;0.0836;|] 
        [|0.0873;0.0662;0.0672;0.0819;0.0749;0.0649;0.0835;0.0725;|] 
        [|0.0974;0.1352;0.0817;0.1016;0.0968;0.1064;0.105;|] 
        [|0.1033;0.0915;0.0781;0.0685;0.0677;0.0697;0.0764;0.0689;|]    
        [|0.0703;0.1026;0.0956;0.0973;0.1039;0.1045;|]
    |]

let contrastMatrixHSD' = 
    [| 
    [|1.;-1.; 0.; 0.; 0.;|] // sample 1 is compared to sample 2
    [|1.; 0.;-1.; 0.; 0.;|] // sample 1 is compared to sample 3
    [|1.; 0.; 0.;-1.; 0.;|] 
    [|1.; 0.; 0.; 0.;-1.;|] 
    [|0.; 1.;-1.; 0.; 0.;|]
    [|0.; 1.; 0.;-1.; 0.;|]
    [|0.; 1.; 0.; 0.;-1.;|]
    [|0.; 0.; 1.;-1.; 0.;|] // sample 3 is compared to sample 4
    [|0.; 0.; 1.; 0.;-1.;|] // sample 3 is compared to sample 4
    [|0.; 0.; 0.; 1.;-1.;|] // sample 3 is compared to sample 4
    |]

let hsdResult = tukeyHSD contrastMatrixHSD' hsdExample' 

tukeyHSD contrastMatrix dataOneWay



(*
    For every generated contrast an output p value is calculated.
    e.g.
    hsdResult.[0].Significance = 0.0364
    hsdResult.[1].Significance = 0.4983 
    hsdResult.[2].Significance = 0.1001
    hsdResult.[3].Significance = 0.1364
    hsdResult.[4].Significance = 0.0008
    hsdResult.[5].Significance = 0.0255
*)

TTest.twoSample true (vector hsdExample'.[0]) (vector hsdExample'.[1]) //0.07290  //0.0661  
TTest.twoSample true (vector hsdExample'.[0]) (vector hsdExample'.[2]) //0.56924  //0.5681  
TTest.twoSample true (vector hsdExample'.[0]) (vector hsdExample'.[3]) //0.15523  //0.1491  
TTest.twoSample true (vector hsdExample'.[1]) (vector hsdExample'.[2]) //0.11167  //0.1099  
TTest.twoSample true (vector hsdExample'.[1]) (vector hsdExample'.[3]) //0.00055  //0.00055 
TTest.twoSample true (vector hsdExample'.[2]) (vector hsdExample'.[3]) //0.02419  //0.02357 

(**
###Dunnetts test

When there is one control group which should be compared with all treatment-groups, you can use Dunnett's test. It is a multiple-to-one post hoc test
that has a higher power than Tukey's HSD since fewer comparisons have to be performed.

###Fisher Hotelling

*)

let d1 = [159.;179.;100.;45.;384.;230.;100.;320.;80.;220.;320.;210.;]
let d2 = [14.4;15.2;11.3;2.5;22.7;14.9;1.41;15.81;4.19;15.39;17.25;9.52; ]
    
Testing.FisherHotelling.test d1 d2

(**
<a name="PvalueAdjust"></a>

##Multiple Testing

When conducting multiple hypothesis test the &alpha;-error accumulates. This is because the p value just describes the probability for 
a false positive for one single test. If you perform 10 t-test at an &alpha; level of 0.05, the probability of getting a significant result by chance
is 40.1% [ (1-(1-&alpha;)<sup>k</sup> ].

*)
(*** hide ***)

let aErrorAcc = 
    [1. .. 100.]
    |> List.map (fun x -> x,(1. - 0.95**x))
    |> Chart.Line
    |> styleChart "number of tests (k)" "probability of at least one false positive test"

(*** include-value:aErrorAcc ***)

(**

To compensate this inflation, several multiple testing corrections were published. 
The most conservative method is the Bonferroni correction, where the used &alpha; level is divided by the number of performed tests.

A modern correction approach is the Benjamini-Hochberg method also known as FDR (false discovery rate).

*)


(**
### Benjamini-Hochberg

*)
let pValues =
    [|
        0.000002;0.000048;0.000096;0.000096;0.000351;0.000368;0.000368;0.000371;0.000383;0.000383;0.000705;0.000705;0.000705;0.000705;0.000739;0.00101;0.001234;0.001509;0.001509;0.001509;0.001509;0.001686;0.001686;0.001964;0.001964;0.001964;0.001964;0.001964;0.001964;0.001964;0.002057;0.002295;0.002662;0.002662;
        0.002662;0.002662;0.002662;0.002662;0.002662;0.002672;0.002714;0.002922;0.00348;0.004066;0.004176;0.004176;0.004562;0.004562;0.005848;0.005848;0.006277;0.007024;0.007614;0.007614;0.007614;0.007614;0.007614;0.00979;0.01049;0.01049;0.012498;0.012498;0.012498;0.017908;0.018822;0.019003;0.019003;0.019003;
        0.020234;0.02038;0.021317;0.023282;0.026069;0.026773;0.027255;0.027255;0.027255;0.027255;0.0274;0.030338;0.03128;0.034516;0.034516;0.037267;0.037267;0.040359;0.042706;0.043506;0.04513;0.04513;0.047135;0.049261;0.049261;0.049261;0.049261;0.049333;0.050457;0.052112;0.052476;0.060504;0.063031;0.063031;
        0.063031;0.063031;0.065316;0.065316;0.066751;0.067688;0.069676;0.073043;0.078139;0.078594;0.078594;0.095867;0.098913;0.102606;0.102606;0.102606;0.107444;0.116213;0.126098;0.135099;0.135099;0.159786;0.179654;0.199372;0.203542;0.203542;0.211249;0.211968;0.226611;0.228287;0.238719;0.247204;0.263942;
        0.263942;0.289175;0.306064;0.330191;0.330191;0.340904;0.343869;0.350009;0.355614;0.355614;0.359354;0.386018;0.386018;0.434486;0.438791;0.464694;0.471015;0.4715;0.479307;0.490157;0.505652;0.539465;0.539465;0.558338;0.558338;0.601991;0.61052;0.634365;0.637835;0.677506;0.678222;0.727881;0.748533;
        0.753718;0.758701;0.810979;0.838771;0.854833;0.872159;0.878727;0.890621;0.916361;0.954779;0.98181;0.985365;0.986261;0.98958;0.99861;0.99861;0.999602;0.999895
    |] |> Array.sort

let pValsAdj =
    MultipleTesting.benjaminiHochbergFDRBy (fun x -> x,x) pValues
    |> List.rev

(*** hide ***)
let bhValues =
    [
        Chart.Line(pValues,pValues,Name="diagonal")
        Chart.Line(pValsAdj,Name="adj")
    ]
    |> Chart.Combine
    |> styleChartRange "pValue" "BH corrected pValue" (0.,1.) (0.,1.)

(*** include-value:bhValues ***)


(**

### Q Value

*)
let pi0 = 
    pValues
    |> MultipleTesting.Qvalues.pi0Bootstrap 

let qValues = 
    pValues
    |> MultipleTesting.Qvalues.ofPValues pi0

let qValuesRob =
    pValues
    |> MultipleTesting.Qvalues.ofPValuesRobust pi0 

(*** hide ***)
let qChart =    
    [
        Chart.Line(pValues,qValues,Name="qValue")
        Chart.Line(pValues,qValuesRob,Name="qValueRobust")
    ]
    |> Chart.Combine
    |> styleChartRange "pValues" "qValues" (0.,1.) (0.,1.)

let qHisto =
    [
        Chart.Histogram(pValues,Xbins=Bins.init(0.,1.,0.05),Name="pValues",HistNorm=HistNorm.Density)
        Chart.Line([(0.,pi0);(1.,pi0)],Name="pi<sub>0</sub>",Dash=DrawingStyle.Dash)
    ]
    |> Chart.Combine
    |> styleChart "p value" "density"

(*** include-value:qChart ***)
(*** include-value:qHisto ***)

