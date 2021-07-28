module CorrelationTests
open System
open FSharp.Stats.Correlation
open Expecto

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

    testList "Correlation.Seq" [
        testCase "pearson" <| fun () -> 
            Expect.isTrue (0.571181558 = Math.Round(testCase1,9)) "pearson correlation coefficient should be equal"
            Expect.isTrue (0.999705373 = Math.Round(testCase2,9)) "pearson correlation coefficient should be equal"
    ]
   
[<Tests>]
let spearmanCorrelationTests = 
    // tested with R cor(x,y,method = "spearman")
    let seq1 = [5.05;6.75;3.21;2.66]
    let seq2 = [1.65;2.64;2.64;6.95]
    let testCase1 =
        (seq1, seq2)
        ||> Seq.spearman

    let testCase2 = 
        let seq1 = [2.0; 47.4; 42.0; 10.8; 60.1; 1.7; 64.0; 63.1; 1.0; 1.4; 7.9; 0.3; 3.9; 0.3; 6.7]
        let seq2 = [22.6; 8.3; 44.4; 11.9; 24.6; 0.6; 5.7; 41.6; 0.0; 0.6; 6.7; 3.8; 1.0; 1.2; 1.4]
        (seq1, seq2)
        ||> Seq.spearman

    let testCase3 = 
        (seq1 |> Seq.map decimal,
         seq2 |> Seq.map decimal)
        ||> Seq.spearman

    testList "Correlation.Seq" [
        testCase "spearman" <| fun () ->
            Expect.floatClose Accuracy.high testCase1 -0.632455532 "Should be equal (double precision)"
            Expect.floatClose Accuracy.high testCase2 0.6887298748 "Should be equal (double precision)"
            Expect.floatClose Accuracy.high testCase3 -0.632455532 "Should be equal (double precision)"
    ]
