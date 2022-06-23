module QuantileTests

open System
open FSharp.Stats
open Expecto
open TestExtensions

let rnd = System.Random(1)
let testSeq1   = seq {20.;-0.5;0.9649;-0.4;0.0;0.1;0.7;12.;4.7;100.;0.0;0.65}
let testList1  = FSharp.Collections.List.ofSeq testSeq1
let testArray1 = FSharp.Collections.Array.ofList testList1
let testArrayLong = Array.init 10000 (fun _ -> rnd.NextDouble())
let testArrayNaN  = Array.append testArray1 [|nan|]
let testArrayDuplicates = Array.append (Array.init 100 (fun _ -> 0.)) testArray1
let percentiles = [|-1.;0.;0.1;0.5;0.9;1.;1.1|]

let expectedShort = [|nan; -0.5; -0.4433333333; 0.675; 54.66666667; 100.0; nan|] //type 8
//Type=1; Inverse of empirical distribution function
let expected1 = [|nan;5.634874108e-05;9.657269255e-02;4.949744681e-01;8.972069658e-01;9.999589436e-01;nan|]
//Type=2; Similar to type 1 but with averaging at discontinuities.
let expected2 = [|nan;5.634874108e-05;9.664607728e-02;4.950177730e-01;8.972569624e-01;9.999589436e-01;nan|]
//Type=3; SAS definition: nearest even order statistic
let expected3 = [|nan;5.634874108e-05;9.657269255e-02;4.949744681e-01;8.972069658e-01;9.999589436e-01;nan|]
//Type=4; linear interpolation of the empirical cdf.
let expected4 = [|nan;5.634874108e-05;9.657269255e-02;4.949744681e-01;8.972069658e-01;9.999589436e-01;nan|]
//Type=5; That is a piecewise linear function where the knots are the values midway through the steps of the empirical cdf
let expected5 = [|nan;5.634874108e-05;9.664607728e-02;4.950177730e-01;8.972569624e-01;9.999589436e-01;nan|]
//Type=6; This is used by Minitab and by SPSS
let expected6 = [|nan;5.634874108e-05;9.658736950e-02;4.950177730e-01;8.972969598e-01;9.999589436e-01;nan|]
//Type=7; This is used by S
let expected7 = [|nan;5.634874108e-05;9.670478506e-02;4.950177730e-01;8.972169651e-01;9.999589436e-01;nan|]
//Type=8; The resulting quantile estimates are approximately median-unbiased regardless of the distribution of x
let expected8 = [|nan;5.634874108e-05;9.662650802e-02;4.950177730e-01;8.972702949e-01;9.999589436e-01;nan|]
//Type=9; The resulting quantile estimates are approximately unbiased for the expected order statistics if x is normally distributed.
let expected9 = [|nan;5.634874108e-05;9.663140033e-02;4.950177730e-01;8.972669618e-01;9.999589436e-01;nan|]
    

[<Tests>]
let quantileDefaultTests =
    //tested against R stats (3.6.2) quantile()
    testList "Quantile.compute" [

        testCase "testSeq" <| fun () ->
            let expected = expectedShort
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.compute x testSeq1
                    )
            TestExtensions.sequenceEqualRoundedNaN 8 expected actual "Quantiles should be equal"

        testCase "testList" <| fun () ->
            let expected = expectedShort
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.compute x testList1
                    )
            TestExtensions.sequenceEqualRoundedNaN 8 expected actual "Quantiles should be equal"

        testCase "testArray" <| fun () ->
            let expected = expectedShort
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.compute x testArray1
                    )
            TestExtensions.sequenceEqualRoundedNaN 8 expected actual "Quantiles should be equal"

        testCase "testArrayLong" <| fun () ->
            let expected = expected8
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.compute x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"

        testCase "testArrayNaN" <| fun () ->
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.compute x testArrayNaN
                    )
            let checkNan = actual |> Array.map (fun k -> nan.Equals k)
            let expected = Array.init 7 (fun t -> true)
            Expect.sequenceEqual expected checkNan "Quantiles should be equal"
        
        testCase "testArrayDuplicates" <| fun () ->
            let expected = [|nan; -0.5; 0.0; 0.0; 0.0; 100.0; nan|] //r type 8
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.compute x testArrayDuplicates
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"

    ]
    
[<Tests>]
let quantileTests =
    //tested against R stats (3.6.2) quantile()
    testList "Quantile" [
        let rnd = System.Random(1)
        let testArrayLong = Array.init 10000 (fun _ -> rnd.NextDouble())
        let percentiles = [|-1.;0.;0.1;0.5;0.9;1.;1.1|]
            
        //Type=1; Inverse of empirical distribution function
        let expected1 = [|nan;5.634874108e-05;9.657269255e-02;4.949744681e-01;8.972069658e-01;9.999589436e-01;nan|]
        //Type=2; Similar to type 1 but with averaging at discontinuities.
        let expected2 = [|nan;5.634874108e-05;9.664607728e-02;4.950177730e-01;8.972569624e-01;9.999589436e-01;nan|]
        //Type=3; SAS definition: nearest even order statistic
        let expected3 = [|nan;5.634874108e-05;9.657269255e-02;4.949744681e-01;8.972069658e-01;9.999589436e-01;nan|]
        //Type=4; linear interpolation of the empirical cdf.
        let expected4 = [|nan;5.634874108e-05;9.657269255e-02;4.949744681e-01;8.972069658e-01;9.999589436e-01;nan|]
        //Type=5; That is a piecewise linear function where the knots are the values midway through the steps of the empirical cdf
        let expected5 = [|nan;5.634874108e-05;9.664607728e-02;4.950177730e-01;8.972569624e-01;9.999589436e-01;nan|]
        //Type=6; This is used by Minitab and by SPSS
        let expected6 = [|nan;5.634874108e-05;9.658736950e-02;4.950177730e-01;8.972969598e-01;9.999589436e-01;nan|]
        //Type=7; This is used by S
        let expected7 = [|nan;5.634874108e-05;9.670478506e-02;4.950177730e-01;8.972169651e-01;9.999589436e-01;nan|]
        //Type=8; The resulting quantile estimates are approximately median-unbiased regardless of the distribution of x
        let expected8 = [|nan;5.634874108e-05;9.662650802e-02;4.950177730e-01;8.972702949e-01;9.999589436e-01;nan|]
        //Type=9; The resulting quantile estimates are approximately unbiased for the expected order statistics if x is normally distributed.
        let expected9 = [|nan;5.634874108e-05;9.663140033e-02;4.950177730e-01;8.972669618e-01;9.999589436e-01;nan|]
            
        testCase "empiricalInvCdf" <| fun () ->
            let expected = expected1
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.empiricalInvCdf x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"

        testCase "empiricalInvCdfAverage" <| fun () ->
            let expected = expected2
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.empiricalInvCdfAverage x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"

        testCase "nearest" <| fun () ->
            let expected = expected3
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.nearest x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"

        testCase "nist" <| fun () ->
            let expected = expected6
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.nist x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"

        testCase "mode" <| fun () ->
            let expected = expected7
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.mode x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"

        testCase "normal" <| fun () ->
            let expected = expected9
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.normal x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"
    ]



[<Tests>]
let quantileOfSortedTests =
    //tested against R stats (3.6.2) quantile()
    testList "Quantile.OfSorted" [

        testCase "empiricalInvCdf" <| fun () ->
            let expected = expected1
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.empiricalInvCdf x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"

        testCase "empiricalInvCdfAverage" <| fun () ->
            let expected = expected2
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.empiricalInvCdfAverage x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"

        testCase "nearest" <| fun () ->
            let expected = expected3
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.nearest x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"

        testCase "nist" <| fun () ->
            let expected = expected6
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.nist x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"

        testCase "mode" <| fun () ->
            let expected = expected7
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.mode x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"

        testCase "normal" <| fun () ->
            let expected = expected9
            let actual = 
                percentiles
                |> Array.map (fun x -> 
                    Quantile.normal x testArrayLong
                    )
            TestExtensions.sequenceEqualRoundedNaN 10 expected actual "Quantiles should be equal"
    ]
