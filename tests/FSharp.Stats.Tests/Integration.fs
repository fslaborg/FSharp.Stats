module IntegrationTests
open Expecto
open System
open FSharp.Stats

open System.IO

open FSharp.Stats.Integration

[<Tests>]
let numericalIntegrationTests =

    /// f(x) = x^3
    let f1 x = x * x * x
    /// f(x) = 1/x
    let f2 x = 1. / x

    let observations1 = [|0. .. 0.0001 ..  1.|] |> Array.map (fun x -> x, f1 x)
    let observations2 = [|1. .. 0.001 .. 100.|] |> Array.map (fun x -> x, f2 x)

    //
    let fNaN x = nan 
    let fPosInf x = infinity 
    let fNegInf x = -infinity

    let observationsNaN = [|0. .. 0.0001 ..  1.|] |> Array.map (fun x -> x, fNaN x)
    let observationsPosInf = [|0. .. 0.0001 ..  1.|] |> Array.map (fun x -> x, fPosInf x)
    let observationsNegInf = [|0. .. 0.0001 ..  1.|] |> Array.map (fun x -> x, fNegInf x)

    testList "NumericalIntegration" [ 
        testList "function integration" [
            testCase "LeftEndpoint x^3" (fun _ ->
                let actual = f1 |> NumericalIntegration.definiteIntegral(LeftEndpoint, 0., 1., 10000)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.low actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "RightEndpoint x^3" (fun _ ->
                let actual = f1 |> NumericalIntegration.definiteIntegral(RightEndpoint, 0., 1., 10000)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.low actual expected "RightEndpoint did not return the correct result"
            )
            testCase "Midpoint x^3" (fun _ ->
                let actual = f1 |> NumericalIntegration.definiteIntegral(Midpoint, 0., 1., 10000)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.high actual expected "Midpoint did not return the correct result"
            )
            testCase "Trapezoidal x^3" (fun _ ->
                let actual = f1 |> NumericalIntegration.definiteIntegral(Trapezoidal, 0., 1., 10000)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.high actual expected "Trapezoidal did not return the correct result"
            )
            testCase "Simpson x^3" (fun _ ->
                let actual = f1 |> NumericalIntegration.definiteIntegral(Simpson, 0., 1., 10000)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.veryHigh actual expected "Simpson did not return the correct result"
            )
            testCase "LeftEndpoint 1/x" (fun _ ->
                let actual = f2 |> NumericalIntegration.definiteIntegral(LeftEndpoint, 1., 100., 100000)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.low actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "RightEndpoint 1/x" (fun _ ->
                let actual = f2 |> NumericalIntegration.definiteIntegral(RightEndpoint, 1., 100., 100000)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.low actual expected "RightEndpoint did not return the correct result"
            )
            testCase "Midpoint 1/x" (fun _ ->
                let actual = f2 |> NumericalIntegration.definiteIntegral(Midpoint, 1., 100., 100000)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.high actual expected "Midpoint did not return the correct result"
            )
            testCase "Trapezoidal 1/x" (fun _ ->
                let actual = f2 |> NumericalIntegration.definiteIntegral(Trapezoidal, 1., 100., 100000)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.high actual expected "Trapezoidal did not return the correct result"
            )
            testCase "Simpson 1/x" (fun _ ->
                let actual = f2 |> NumericalIntegration.definiteIntegral(Simpson, 1., 100., 100000)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.high actual expected "Simpson did not return the correct result"
            )
        ]
        testList "function integration parallel" [
            testCase "LeftEndpoint x^3 parallel" (fun _ ->
                let actual = f1 |> NumericalIntegration.definiteIntegral(LeftEndpoint, 0., 1., 10000, Parallel = true)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.low actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "RightEndpoint x^3 parallel" (fun _ ->
                let actual = f1 |> NumericalIntegration.definiteIntegral(RightEndpoint, 0., 1., 10000, Parallel = true)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.low actual expected "RightEndpoint did not return the correct result"
            )
            testCase "Midpoint x^3 parallel" (fun _ ->
                let actual = f1 |> NumericalIntegration.definiteIntegral(Midpoint, 0., 1., 10000, Parallel = true)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.high actual expected "Midpoint did not return the correct result"
            )
            testCase "Trapezoidal x^3 parallel" (fun _ ->
                let actual = f1 |> NumericalIntegration.definiteIntegral(Trapezoidal, 0., 1., 10000, Parallel = true)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.high actual expected "Trapezoidal did not return the correct result"
            )
            testCase "Simpson x^3 parallel" (fun _ ->
                let actual = f1 |> NumericalIntegration.definiteIntegral(Simpson, 0., 1., 10000, Parallel = true)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.veryHigh actual expected "Simpson did not return the correct result"
            )
            testCase "LeftEndpoint 1/x parallel" (fun _ ->
                let actual = f2 |> NumericalIntegration.definiteIntegral(LeftEndpoint, 1., 100., 100000, Parallel = true)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.low actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "RightEndpoint 1/x parallel" (fun _ ->
                let actual = f2 |> NumericalIntegration.definiteIntegral(RightEndpoint, 1., 100., 100000, Parallel = true)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.low actual expected "RightEndpoint did not return the correct result"
            )
            testCase "Midpoint 1/x parallel" (fun _ ->
                let actual = f2 |> NumericalIntegration.definiteIntegral(Midpoint, 1., 100., 100000, Parallel = true)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.high actual expected "Midpoint did not return the correct result"
            )
            testCase "Trapezoidal 1/x parallel" (fun _ ->
                let actual = f2 |> NumericalIntegration.definiteIntegral(Trapezoidal, 1., 100., 100000, Parallel = true)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.high actual expected "Trapezoidal did not return the correct result"
            )
            testCase "Simpson 1/x parallel" (fun _ ->
                let actual = f2 |> NumericalIntegration.definiteIntegral(Simpson, 1., 100., 100000, Parallel = true)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.high actual expected "Simpson did not return the correct result"
            )
        ]
        testList "observation integration" [ 
            testCase "LeftEndpoint x^3" (fun _ ->
                let actual = observations1 |> NumericalIntegration.definiteIntegral(LeftEndpoint)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.low actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "RightEndpoint x^3" (fun _ ->
                let actual = observations1 |> NumericalIntegration.definiteIntegral(RightEndpoint)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.low actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "Midpoint x^3" (fun _ ->
                let actual = observations1 |> NumericalIntegration.definiteIntegral(Midpoint)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.high actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "Trapezoidal x^3" (fun _ ->
                let actual = observations1 |> NumericalIntegration.definiteIntegral(Trapezoidal)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.high actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "Simpson x^3" (fun _ ->
                let actual = observations1 |> NumericalIntegration.definiteIntegral(Simpson)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.high actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "LeftEndpoint 1/x" (fun _ ->
                let actual = observations2 |> NumericalIntegration.definiteIntegral(LeftEndpoint)
                //exact result is 0.25
                let expected = Ops.roundTo 5 (log 100.)
                Expect.floatClose Accuracy.low (Ops.roundTo 5 actual) expected "LeftEndpoint did not return the correct result"
            )
            testCase "RightEndpoint 1/x" (fun _ ->
                let actual = observations2 |> NumericalIntegration.definiteIntegral(RightEndpoint)
                //exact result is 0.25
                let expected = Ops.roundTo 5 (log 100.)
                Expect.floatClose Accuracy.low (Ops.roundTo 5 actual) expected "LeftEndpoint did not return the correct result"
            )
            testCase "Midpoint 1/x" (fun _ ->
                let actual = observations2 |> NumericalIntegration.definiteIntegral(Midpoint)
                //exact result is 0.25
                let expected = Ops.roundTo 5 (log 100.)
                Expect.floatClose Accuracy.high (Ops.roundTo 5 actual) expected "LeftEndpoint did not return the correct result"
            )
            testCase "Trapezoidal 1/x" (fun _ ->
                let actual = observations2 |> NumericalIntegration.definiteIntegral(Trapezoidal)
                //exact result is 0.25
                let expected = Ops.roundTo 5 (log 100.)
                Expect.floatClose Accuracy.high (Ops.roundTo 5 actual) expected "LeftEndpoint did not return the correct result"
            )
            testCase "Simpson 1/x" (fun _ ->
                let actual = observations2 |> NumericalIntegration.definiteIntegral(Simpson)
                //exact result is 0.25
                let expected = Ops.roundTo 5 (log 100.)
                Expect.floatClose Accuracy.high (Ops.roundTo 5 actual) expected "LeftEndpoint did not return the correct result"
            )
        ]
        testList "integrating nan function returns nan" [
            testCase "LeftEndpoint" (fun _ -> Expect.isTrue (nan.Equals(fNaN |> NumericalIntegration.definiteIntegral(LeftEndpoint,0.,1.,1000))) "did not return nan")
            testCase "RightEndpoint" (fun _ -> Expect.isTrue (nan.Equals(fNaN |> NumericalIntegration.definiteIntegral(RightEndpoint,0.,1.,1000)))  "did not return nan")
            testCase "Midpoint" (fun _ -> Expect.isTrue (nan.Equals(fNaN |> NumericalIntegration.definiteIntegral(Midpoint,0.,1.,1000)))  "did not return nan")
            testCase "Trapezoidal" (fun _ -> Expect.isTrue (nan.Equals(fNaN |> NumericalIntegration.definiteIntegral(Trapezoidal,0.,1.,1000)))  "did not return nan")
            testCase "Simpson" (fun _ -> Expect.isTrue (nan.Equals(fNaN |> NumericalIntegration.definiteIntegral(Simpson,0.,1.,1000)))  "did not return nan")
        ]
        testList "integrating +infinity function returns +infinity" [
            testCase "LeftEndpoint" (fun _ -> Expect.equal (fPosInf |> NumericalIntegration.definiteIntegral(LeftEndpoint,0.,1.,1000)) infinity "did not return infinity")
            testCase "RightEndpoint" (fun _ -> Expect.equal (fPosInf |> NumericalIntegration.definiteIntegral(RightEndpoint,0.,1.,1000)) infinity "did not return infinity")
            testCase "Midpoint" (fun _ -> Expect.equal (fPosInf |> NumericalIntegration.definiteIntegral(Midpoint,0.,1.,1000)) infinity "did not return infinity")
            testCase "Trapezoidal" (fun _ -> Expect.equal (fPosInf |> NumericalIntegration.definiteIntegral(Trapezoidal,0.,1.,1000)) infinity "did not return infinity")
            testCase "Simpson" (fun _ -> Expect.equal (fPosInf |> NumericalIntegration.definiteIntegral(Simpson,0.,1.,1000)) infinity "did not return infinity")
        ]
        testList "integrating +infinity function returns -infinity" [
            testCase "LeftEndpoint" (fun _ -> Expect.equal (fNegInf |> NumericalIntegration.definiteIntegral(LeftEndpoint,0.,1.,1000)) -infinity "did not return -infinity")
            testCase "RightEndpoint" (fun _ -> Expect.equal (fNegInf |> NumericalIntegration.definiteIntegral(RightEndpoint,0.,1.,1000)) -infinity "did not return -infinity")
            testCase "Midpoint" (fun _ -> Expect.equal (fNegInf |> NumericalIntegration.definiteIntegral(Midpoint,0.,1.,1000)) -infinity "did not return -infinity")
            testCase "Trapezoidal" (fun _ -> Expect.equal (fNegInf |> NumericalIntegration.definiteIntegral(Trapezoidal,0.,1.,1000)) -infinity "did not return -infinity")
            testCase "Simpson" (fun _ -> Expect.equal (fNegInf |> NumericalIntegration.definiteIntegral(Simpson,0.,1.,1000)) -infinity "did not return -infinity")
        ]
        testList "integrating nan observations returns nan" [
            testCase "LeftEndpoint" (fun _ -> Expect.isTrue (nan.Equals(observationsNaN |> NumericalIntegration.definiteIntegral(LeftEndpoint))) "did not return nan")
            testCase "RightEndpoint" (fun _ -> Expect.isTrue (nan.Equals(observationsNaN |> NumericalIntegration.definiteIntegral(RightEndpoint))) "did not return nan")
            testCase "Midpoint" (fun _ -> Expect.isTrue (nan.Equals(observationsNaN |> NumericalIntegration.definiteIntegral(Midpoint))) "did not return nan")
            testCase "Trapezoidal" (fun _ -> Expect.isTrue (nan.Equals(observationsNaN |> NumericalIntegration.definiteIntegral(Trapezoidal))) "did not return nan")
            testCase "Simpson" (fun _ -> Expect.isTrue (nan.Equals(observationsNaN |> NumericalIntegration.definiteIntegral(Simpson))) "did not return nan")
        ]
        testList "integrating +infinity observations returns +infinity" [
            testCase "LeftEndpoint" (fun _ -> Expect.equal (observationsPosInf |> NumericalIntegration.definiteIntegral(LeftEndpoint)) infinity "did not return infinity")
            testCase "RightEndpoint" (fun _ -> Expect.equal (observationsPosInf |> NumericalIntegration.definiteIntegral(RightEndpoint)) infinity "did not return infinity")
            testCase "Midpoint" (fun _ -> Expect.equal (observationsPosInf |> NumericalIntegration.definiteIntegral(Midpoint)) infinity "did not return infinity")
            testCase "Trapezoidal" (fun _ -> Expect.equal (observationsPosInf |> NumericalIntegration.definiteIntegral(Trapezoidal)) infinity "did not return infinity")
            testCase "Simpson" (fun _ -> Expect.equal (observationsPosInf |> NumericalIntegration.definiteIntegral(Simpson)) infinity "did not return infinity")
        ]
        testList "integrating +infinity observations returns -infinity" [
            testCase "LeftEndpoint" (fun _ -> Expect.equal (observationsNegInf |> NumericalIntegration.definiteIntegral(LeftEndpoint)) -infinity "did not return -infinity")
            testCase "RightEndpoint" (fun _ -> Expect.equal (observationsNegInf |> NumericalIntegration.definiteIntegral(RightEndpoint)) -infinity "did not return -infinity")
            testCase "Midpoint" (fun _ -> Expect.equal (observationsNegInf |> NumericalIntegration.definiteIntegral(Midpoint)) -infinity "did not return -infinity")
            testCase "Trapezoidal" (fun _ -> Expect.equal (observationsNegInf |> NumericalIntegration.definiteIntegral(Trapezoidal)) -infinity "did not return -infinity")
            testCase "Simpson" (fun _ -> Expect.equal (observationsNegInf |> NumericalIntegration.definiteIntegral(Simpson)) -infinity "did not return -infinity")
        ]
    ]