module CorrelationTests
open System
open FSharp.Stats.Correlation
open Expecto

module TestData = 
    let doubles = 
        [
            {|  
                X = [| 0.769975279369337; -0.26975129370715756; -0.22164107602804684; -0.37964372892225584; 1.7976931348623157E+308; 0.6956489946628831; 0.8498674478461568; 0.007870060694074144 |]
                Y = [| 8.05529523804792; -9.648443925108909; -1.215500483344818; 5E-324; -4.337558555754166; infinity; -7.497611995486394; -9.039643739188005 |]
                Spearman = 0.09523809523809525
                KendallA = 0.07142857142857142
                KendallB = 0.07142857142857142
                KendallC = 0.07142857142857142
                Pearson = nan
            |}
            {|
                X = [| -1.3946407056008117; -1.7976931348623157E+308; 0.02665139354486956; 0.16752887114290516; 0.6510630080261284 |]
                Y = [| -5.934146660251358; -7.514325777080982; -2.869708043284536; -0.6743782342678939; -2.2164107602804686 |]
                Spearman = 0.9
                KendallA = 0.8
                KendallB = 0.8
                KendallC = 0.8
                Pearson = nan //R returns 0.0
            |}
            {|
                X = [| -infinity; 3.2160411307302565 |]
                Y = [| -3.8511452553484538; -5.393177399524884 |]
                Spearman = -1.0
                KendallA = -1.0
                KendallB = -1.0
                KendallC = -1.0
                Pearson = nan
            |}
            {|
                X = [| 5E-324; 0.4310933883901359; 1.1782225200518512; 4.490557012680512; -infinity; -0.05931977813647893 |]
                Y = [| -1.7431196366262147; -3.3100232065058477; -infinity; 6.432082261460513; 8.025230948524591; 5E-324 |]
                Spearman = -0.42857142857142855
                KendallA = -0.4666666666666667
                KendallB = -0.4666666666666667
                KendallC = -0.4666666666666667
                Pearson = nan
            |}
            {|
                X = [| -0.6237678376055525; -0.02398140791055825; -0.33238783674585126; 5E-324; -0.9617738169271464; -0.6402018172171572; -0.7944049915885085 |]
                Y = [| 7.487700704756412; 2.882382571594094; 0.6608761209968983; -1.7976931348623157E+308; 3.7699648024572516; -5.349991331399306; -6.943140018463384 |]
                Spearman = -0.28571428571428564
                KendallA = -0.14285714285714285
                KendallB = -0.14285714285714285
                KendallC = -0.14285714285714285
                Pearson = nan // R returns -0.5693503001745431
            |}
            {|
                X = [| infinity; -0.4380145079632394; 0.2525563106400899; -0.7097994161043718; -infinity; 0.6891193732603421; -1.7976931348623157E+308; 3.3026058744137248 |]
                Y = [| -4.619190203598879; -6.830939838589383; 4.262013366906972; -1.719153567018289; -5.8337600091398345; 3.631337095047412; 1.7976931348623157E+308; 1.7976931348623157E+308 |]
                Spearman = 0.17964393928698885
                KendallA = 0.10714285714285714
                KendallB = 0.10910894511799618
                KendallC = 0.109375
                Pearson = nan
            |}

            {|
                X = [| -1.0; 1.0; -3.0; 0.0; 0.0; 2.0; -2.0 |]
                Y = [| -3.0; -3.0; 0.0; 2.0; -2.0; -2.0; 1.0 |]
                Spearman = -0.35781322366606727
                KendallA = -0.19047619047619047
                KendallB = -0.20519567041703082
                KendallC = -0.20408163265306123
                Pearson = -0.43649077143553344
            |}
            {|
                X = [| 1.0; 3.0; 3.0; -1.0 |]
                Y = [| 3.0; -1.0; -1.0; 1.0 |]
                Spearman = -0.7777777777777779
                KendallA = -0.5
                KendallB = -0.5999999999999999
                KendallC = -0.5625
                Pearson = -0.6363636363636365
            |}
            {|
                X = [| 0.0; 2.0; -2.0; 1.0; 1.0; 3.0; -1.0; 2.0 |]
                Y = [| -2.0; -2.0; 1.0; 3.0; 3.0; -1.0; 2.0; -3.0 |]
                Spearman = -0.3719512195121951
                KendallA = -0.17857142857142858
                KendallB = -0.19230769230769235
                KendallC = -0.1875
                Pearson = -0.41619003555011974
            |}
            {|
                X = [| 2.0; -3.0; -3.0; 0.0; 3.0 |]
                Y = [| -3.0; 0.0; 0.0; 2.0; -2.0 |]
                Spearman = -0.5789473684210528
                KendallA = -0.3
                KendallB = -0.3333333333333334
                KendallC = -0.32
                Pearson = -0.5823356699841468
            |}
            {|
                X = [| 1.0; 3.0; -1.0; 2.0; 2.0; -2.0; 0.0; 3.0; 3.0 |]
                Y = [| -1.0; -1.0; 2.0; -3.0; -3.0; 0.0; 3.0; -2.0; -2.0 |]
                Spearman = -0.6293337301361106
                KendallA = -0.3611111111111111
                KendallB = -0.40004734568283135
                KendallC = -0.3851851851851852
                Pearson = -0.6851039625605218
            |}
            {|
                X = [| 3.0; -2.0; -2.0; 1.0; -3.0; -3.0 |]
                Y = [| -2.0; 1.0; 1.0; 3.0; -1.0; 2.0 |]
                Spearman = -0.14927035850663303
                KendallA = -0.06666666666666667
                KendallB = -0.07412493166611012
                KendallC = -0.07407407407407407
                Pearson = -0.2631174057921088
            |}
            {|
                X = [| 2.0; -3.0; 0.0 |]
                Y = [| -3.0; 0.0; 3.0 |]
                Spearman = -0.5
                KendallA = -0.3333333333333333
                KendallB = -0.33333333333333337
                KendallC = -0.3333333333333333
                Pearson = -0.39735970711951313
            |}
            {|
                X = [| -3.0; -1.0; -1.0; 2.0; -2.0; -2.0; 0.0 |]
                Y = [| -1.0; 2.0; 2.0; -3.0; 0.0; 3.0; 3.0 |]
                Spearman = 0.00925925925925926
                KendallA = 0.09523809523809523
                KendallB = 0.10526315789473686
                KendallC = 0.10204081632653061
                Pearson = -0.3150360061726043
            |}
            {|
                X = [| 2.0; 2.0; -1.0 |]
                Y = [| -2.0; 1.0; -3.0 |]
                Spearman = 0.8660254037844387
                KendallA = 0.6666666666666666
                KendallB = 0.8164965809277261
                KendallC = 0.8888888888888888
                Pearson = 0.6933752452815365
            |}
            {|
                X = [| -3.0; 1.0; 0.0; -2.0 |]
                Y = [| -2.0; 0.0; 2.0; 2.0 |]
                Spearman = 0.31622776601683794
                KendallA = 0.16666666666666666
                KendallB = 0.18257418583505536
                KendallC = 0.1875
                Pearson = 0.3813850356982369
            |}
        ]
    let ints = 
        [
            {|
                X = [| 1; 3; -1; 2; 2; -2; 0; 3; 3|]
                Y = [| -1; -1; 2; -3; -3; 0; 3; -2; -2|]
                Spearman = -0.6293337301361106
                KendallA = -0.3611111111111111
                KendallB = -0.40004734568283135
                KendallC = -0.3851851851851852
                Pearson = -0.6851039625605218
            |}
            {|
                X = [| 3; -2; -2; 1; -3; -3|]
                Y = [| -2; 1; 1; 3; -1; 2|]
                Spearman = -0.14927035850663303
                KendallA = -0.06666666666666667
                KendallB = -0.07412493166611012
                KendallC = -0.07407407407407407
                Pearson = -0.2631174057921088
            |}
            {|
                X = [| 2; -3; 0|]
                Y = [| -3; 0; 3|]
                Spearman = -0.5
                KendallA = -0.3333333333333333
                KendallB = -0.33333333333333337
                KendallC = -0.3333333333333333
                Pearson = -0.39735970711951313
            |}
            {|
                X = [| -3; -1; -1; 2; -2; -2; 0|]
                Y = [| -1; 2; 2; -3; 0; 3; 3|]
                Spearman = 0.00925925925925926
                KendallA = 0.09523809523809523
                KendallB = 0.10526315789473686
                KendallC = 0.10204081632653061
                Pearson = -0.3150360061726043
            |}
            {|
                X = [| 2; 2; -1|]
                Y = [| -2; 1; -3|]
                Spearman = 0.8660254037844387
                KendallA = 0.6666666666666666
                KendallB = 0.8164965809277261
                KendallC = 0.8888888888888888
                Pearson = 0.6933752452815365
            |}
            {|
                X = [| -3; 1; 0; -2|]
                Y = [| -2; 0; 2; 2|]
                Spearman = 0.31622776601683794
                KendallA = 0.16666666666666666
                KendallB = 0.18257418583505536
                KendallC = 0.1875
                Pearson = 0.3813850356982369
            |}
        ]

let inline makeTestList listName caseName corr prop cases = 
    let getX x = ( ^a : (member X : ^t[]) x)
    let getY x = ( ^a : (member Y : ^t[]) x)
    cases
    |> List.mapi
        (fun i x ->
            let i = i + 1
            [
                testCase $"{caseName} Case {i}" <| fun () -> 
                    let corr = corr (getX x) (getY x)
                    if Double.IsNaN (prop x) then
                        Expect.isTrue (Double.IsNaN corr) "Should be equal (double precision)"
                    else
                        Expect.floatClose Accuracy.high corr (prop x) "Should be equal (double precision)"
            ]
        )
    |> List.concat
    |> testList $"Correlation.Seq.{listName}"

[<Tests>]
let kendallTauADoubles = TestData.doubles |> makeTestList "kendallTauA" "Double" Seq.kendallTauA (fun x -> x.KendallA)
[<Tests>]
let kendallTauAInts = TestData.ints |> makeTestList "kendallTauA" "Int" Seq.kendallTauA (fun x -> x.KendallA)
[<Tests>]
let kendallTauBDoubles = TestData.doubles |> makeTestList "kendallTauB" "Double" Seq.kendall (fun x -> x.KendallB)
[<Tests>]
let kendallTauBInts = TestData.ints |> makeTestList "kendallTauB" "Int" Seq.kendall (fun x -> x.KendallB)
[<Tests>]
let kendallTauCDoubles = TestData.doubles |> makeTestList "kendallTauC" "Double" Seq.kendallTauC (fun x -> x.KendallC)
[<Tests>]
let kendallTauCInts = TestData.ints |> makeTestList "kendallTauC" "Int" Seq.kendallTauC (fun x -> x.KendallC)
[<Tests>]
let pearsonDoubles = TestData.doubles |> makeTestList "pearson" "Double" Seq.pearson (fun x -> x.Pearson)
[<Tests>]
let pearsonInts = TestData.ints |> makeTestList "pearson" "Int" Seq.pearson (fun x -> x.Pearson)  
[<Tests>]
let spearmanDoubles = TestData.doubles |> makeTestList "spearman" "Double" Seq.spearman (fun x -> x.Spearman)
[<Tests>]
let spearmanInts = TestData.ints |> makeTestList "spearman" "Int" Seq.spearman (fun x -> x.Spearman)


[<Tests>]
let kendallCorrelationTests =
    // tested with R Kendall(x,y) function
    testList "Correlation.Seq" [
        testCase "kendall" <| fun () ->
            let xs = [|-0.5;-0.4 ;0.  ;0.7;0.65;0.9649|]
            let ys = [|-0.3;-0.25;-0.1;-0.46;0.103;0.409|]
            let tau = Seq.kendall xs ys
            Expect.floatClose Accuracy.high tau 0.4666666667 "Should be equal (double precision)"
    //ToDo ties tau_a,tau_b,tau_c

        testCase "kendallOfPairs" <| fun() ->
            let testCase1 = 
                [-0.5, -0.3; -0.4, -0.25; 0., -0.1; 0.7, -0.46; 0.65, 0.103; 0.9649, 0.409] |> Seq.kendallOfPairs
            Expect.floatClose Accuracy.high testCase1 0.4666666667 "Should be equal (double precision)"

        testCase "kendallBy" <| fun() ->
            let testCase2 = 
                [ {| xs = -0.5; ys = -0.3 |}
                  {| xs = -0.4; ys = -0.25 |}
                  {| xs = 0.; ys = -0.1 |}
                  {| xs = 0.7; ys = -0.46 |}
                  {| xs = 0.65; ys = 0.103 |}
                  {| xs = 0.9649; ys = 0.409 |} ]
                |> Seq.kendallBy (fun x -> x.xs, x.ys)
            Expect.floatClose Accuracy.high testCase2 0.4666666667 "Should be equal (double precision)"
    ]

[<Tests>]
let pearsonCorrelationTests =
    // examples from R
    // cor(x,y)
    let testCase1 =
        let seq1 = [44.4; 45.9; 41.9; 53.3; 44.7; 44.1; 50.7; 45.2; 60.1;]
        let seq2 = [ 2.6;  3.1;  2.5;  5.0;  3.6;  4.0;  5.2;  2.8;  3.8;]
        Seq.pearson seq1 seq2

    let testCase2 =
        let seq1 = [312.7; 104.2; 104.; 34.7]
        let seq2 = [315.5; 101.3; 108.; 32.2]
        Seq.pearson seq1 seq2

    let testCase3 = 
        [312.7, 315.5; 104.2, 101.3; 104., 108.; 34.7, 32.2]
        |> Seq.pearsonOfPairs

    let testCase4 = 
        [ {| A = 312.7; B = 315.5 |}
          {| A = 104.2; B = 101.3 |}
          {| A = 104.; B = 108. |}
          {| A = 34.7; B = 32.2 |} ]
        |> Seq.pearsonBy(fun x -> x.A, x.B)

    testList "Correlation.Seq" [
        testCase "pearson" <| fun () -> 
            Expect.isTrue (0.571181558 = Math.Round(testCase1,9)) "pearson correlation coefficient should be equal"
            Expect.isTrue (0.999705373 = Math.Round(testCase2,9)) "pearson correlation coefficient should be equal"
        testCase "pearsonOfPairs" <| fun () -> 
            Expect.isTrue (0.999705373 = Math.Round(testCase3,9)) "pearson correlation coefficient should be equal"
        testCase "pearsonBy" <| fun () -> 
            Expect.isTrue (0.999705373 = Math.Round(testCase4,9)) "pearson correlation coefficient should be equal"
     ]

   
[<Tests>]
let spearmanCorrelationTests = 
    // tested with R cor(x,y,method = "spearman")
    let seq1 = [5.05;6.75;3.21;2.66]
    let seq2 = [1.65;2.64;2.64;6.95]
    let seq3 = [2.0; 47.4; 42.0; 10.8; 60.1; 1.7; 64.0; 63.1; 1.0; 1.4; 7.9; 0.3; 3.9; 0.3; 6.7]
    let seq4 = [22.6; 8.3; 44.4; 11.9; 24.6; 0.6; 5.7; 41.6; 0.0; 0.6; 6.7; 3.8; 1.0; 1.2; 1.4]

    let ab = [ {| A = 5.05; B = 1.65 |}
               {| A = 6.75; B = 2.64 |}
               {| A = 3.21; B = 2.64 |}
               {| A = 2.66; B = 6.95 |} ]

    let testCase1 =
        (seq1, seq2)
        ||> Seq.spearman

    let testCase2 = 
        (seq3, seq4)
        ||> Seq.spearman

    let testCase3 = 
        (seq1 |> Seq.map decimal,
         seq2 |> Seq.map decimal)
        ||> Seq.spearman

    let testCase4 = 
        Seq.zip seq1 seq2
        |> Seq.spearmanOfPairs

    let testCase5 = 
        Seq.zip seq3 seq4
        |> Seq.spearmanOfPairs

    let testCase6 = 
        ab
        |> Seq.spearmanBy(fun x -> x.A, x.B)

    testList "Correlation.Seq" [
        testCase "spearman" <| fun () ->
            Expect.floatClose Accuracy.high testCase1 -0.632455532 "Should be equal (double precision)"
            Expect.floatClose Accuracy.high testCase2 0.6887298748 "Should be equal (double precision)"
            Expect.floatClose Accuracy.high testCase3 -0.632455532 "Should be equal (double precision)"
            Expect.floatClose Accuracy.high testCase4 -0.632455532 "Should be equal (double precision)"
            Expect.floatClose Accuracy.high testCase5 0.6887298748 "Should be equal (double precision)"
            Expect.floatClose Accuracy.high testCase6 -0.632455532 "Should be equal (double precision)"            
    ]
