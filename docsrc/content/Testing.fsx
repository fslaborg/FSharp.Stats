(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Stats/net47"
#r @"FSharp.Stats.dll"
#r "netstandard.dll"

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
    [|1.26705653; 1.625320787; 1.266108976; 1.154187629; 1.268498943; 1.069518717;|];
    |]

let contrastMatrix = 
    [| [|1.0;-1.0;0.0;0.0;|] ; [|1.0;0.0;-1.0;0.0;|] ; [|1.0;0.;0.0;-1.0;|] ; 
    [|0.0;1.0;-1.0;0.0;|]    ; [|0.0;1.0;0.0;-1.0;|] ;
    [|0.0;0.0;1.0;-1.0;|]
    |]

Anova.oneWayAnova dataOneWay

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
Standard example for using the two sample T-Test.
*)



let sample1 = [|-0.2268419965; -0.3772357485|] |> FSharp.Stats.Vector.ofArray
let sample2 = [|-0.6076633409; -0.1781469665|] |> FSharp.Stats.Vector.ofArray

Testing.TTest.twoSample false sample1 sample2

(**
Link to example can be found [here](http://www.statstutor.ac.uk/resources/uploaded/paired-t-test.pdf).
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

Testing.TTest.twoSamplePaired sampleP1 sampleP2

(**
<a name="ChiSquareTest"></a>
##Chi-Squared Test
<a name="Bartlett"></a>
##Bartlett
<a name="PostHoc"></a>
##PostHoc

This test uses the data shown for Anova.
*)


open PostHoc

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


//TukeyHSD contrastMatrixDmg dmg 
// https://brownmath.com/stat/anova1.htm

let dsd = 
    [|
     //[|7.;4.;6.;8.;6.;6.;2.;9.|];
     //[|5.;5.;3.;4.;4.;7.;2.;2.|];
     //[|2.;4.;7.;1.;2.;1.;5.;5.|] ; 
        [|64.; 72.; 68.; 77.; 56.; 95.;|] ;
        [|78.; 91.; 97.; 82.; 85.; 77.;|] ;
        [|75.; 93.; 78.; 71.; 63.; 76.;|] ;
        [|55.; 66.; 49.; 64.; 70.; 68.;|] ;   
    |]

let contrastMatrixDsd = contrastMatrix //used in docs for "Anova"

Anova.oneWayAnova dsd

tukeyHSD contrastMatrixDsd dsd 


let d1 = [159.;179.;100.;45.;384.;230.;100.;320.;80.;220.;320.;210.;]
let d2 = [14.4;15.2;11.3;2.5;22.7;14.9;1.41;15.81;4.19;15.39;17.25;9.52; ]
    

Testing.FisherHotelling.test d1 d2

//Testing.Hays contrastMatrix 

(**
<a name="PvalueAdjust"></a>

##Multiple Testing

### Q Value
*)


let pValues =
    [|
        0.000002;0.000048;0.000096;0.000096;0.000351;0.000368;0.000368;0.000371;0.000383;0.000383;0.000705;0.000705;0.000705;0.000705;0.000739;0.00101;0.001234;0.001509;0.001509;0.001509;0.001509;0.001686;0.001686;0.001964;0.001964;0.001964;0.001964;0.001964;0.001964;0.001964;0.002057;0.002295;0.002662;0.002662
        ;0.002662;0.002662;0.002662;0.002662;0.002662;0.002672;0.002714;0.002922;0.00348;0.004066;0.004176;0.004176;0.004562;0.004562;0.005848;0.005848;0.006277;0.007024;0.007614;0.007614;0.007614;0.007614;0.007614;0.00979;0.01049;0.01049;0.012498;0.012498;0.012498;0.017908;0.018822;0.019003;0.019003;0.019003
        ;0.020234;0.02038;0.021317;0.023282;0.026069;0.026773;0.027255;0.027255;0.027255;0.027255;0.0274;0.030338;0.03128;0.034516;0.034516;0.037267;0.037267;0.040359;0.042706;0.043506;0.04513;0.04513;0.047135;0.049261;0.049261;0.049261;0.049261;0.049333;0.050457;0.052112;0.052476;0.060504;0.063031;0.063031
        ;0.063031;0.063031;0.065316;0.065316;0.066751;0.067688;0.069676;0.073043;0.078139;0.078594;0.078594;0.095867;0.098913;0.102606;0.102606;0.102606;0.107444;0.116213;0.126098;0.135099;0.135099;0.159786;0.179654;0.199372;0.203542;0.203542;0.211249;0.211968;0.226611;0.228287;0.238719;0.247204;0.263942
        ;0.263942;0.289175;0.306064;0.330191;0.330191;0.340904;0.343869;0.350009;0.355614;0.355614;0.359354;0.386018;0.386018;0.434486;0.438791;0.464694;0.471015;0.4715;0.479307;0.490157;0.505652;0.539465;0.539465;0.558338;0.558338;0.601991;0.61052;0.634365;0.637835;0.677506;0.678222;0.727881;0.748533
        ;0.753718;0.758701;0.810979;0.838771;0.854833;0.872159;0.878727;0.890621;0.916361;0.954779;0.98181;0.985365;0.986261;0.98958;0.99861;0.99861;0.999602;0.999895
    |] |> Array.sort

let pi0 = 
    pValues
    |> MultipleTesting.Qvalues.pi0Bootstrap 

pValues
|> MultipleTesting.Qvalues.ofPValues pi0

pValues
|> MultipleTesting.Qvalues.ofPValuesRobust pi0 

(**
### Benajmini-Hochberg

*)

MultipleTesting.benjaminiHochbergFDRBy (fun x -> x,x) pValues
|> List.rev



