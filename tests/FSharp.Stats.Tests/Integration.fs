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
                let actual = f1 |> NumericalIntegration.integrateFunction(LeftEndpoint, 0., 1., 10000)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.low actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "RightEndpoint x^3" (fun _ ->
                let actual = f1 |> NumericalIntegration.integrateFunction(RightEndpoint, 0., 1., 10000)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.low actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "MidPoint x^3" (fun _ ->
                let actual = f1 |> NumericalIntegration.integrateFunction(MidPoint, 0., 1., 10000)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.high actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "Trapezoidal x^3" (fun _ ->
                let actual = f1 |> NumericalIntegration.integrateFunction(Trapezoidal, 0., 1., 10000)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.high actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "Simpson x^3" (fun _ ->
                let actual = f1 |> NumericalIntegration.integrateFunction(Simpson, 0., 1., 10000)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.veryHigh actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "LeftEndpoint 1/x" (fun _ ->
                let actual = f2 |> NumericalIntegration.integrateFunction(LeftEndpoint, 1., 100., 100000)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.low actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "RightEndpoint 1/x" (fun _ ->
                let actual = f2 |> NumericalIntegration.integrateFunction(RightEndpoint, 1., 100., 100000)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.low actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "MidPoint 1/x" (fun _ ->
                let actual = f2 |> NumericalIntegration.integrateFunction(MidPoint, 1., 100., 100000)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.high actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "Trapezoidal 1/x" (fun _ ->
                let actual = f2 |> NumericalIntegration.integrateFunction(Trapezoidal, 1., 100., 100000)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.high actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "Simpson 1/x" (fun _ ->
                let actual = f2 |> NumericalIntegration.integrateFunction(Simpson, 1., 100., 100000)
                //exact result is 0.25
                let expected = log(100.)
                Expect.floatClose Accuracy.high actual expected "LeftEndpoint did not return the correct result"
            )
        ]
        testList "observation integration" [ 
            testCase "LeftEndpoint x^3" (fun _ ->
                let actual = observations1 |> NumericalIntegration.integrateObservations(LeftEndpoint)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.low actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "RightEndpoint x^3" (fun _ ->
                let actual = observations1 |> NumericalIntegration.integrateObservations(RightEndpoint)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.low actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "MidPoint x^3" (fun _ ->
                let actual = observations1 |> NumericalIntegration.integrateObservations(MidPoint)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.high actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "Trapezoidal x^3" (fun _ ->
                let actual = observations1 |> NumericalIntegration.integrateObservations(Trapezoidal)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.high actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "Simpson x^3" (fun _ ->
                let actual = observations1 |> NumericalIntegration.integrateObservations(Simpson)
                //exact result is 0.25
                let expected = 0.25
                Expect.floatClose Accuracy.high actual expected "LeftEndpoint did not return the correct result"
            )
            testCase "LeftEndpoint 1/x" (fun _ ->
                let actual = observations2 |> NumericalIntegration.integrateObservations(LeftEndpoint)
                //exact result is 0.25
                let expected = round 5 (log 100.)
                Expect.floatClose Accuracy.low (round 5 actual) expected "LeftEndpoint did not return the correct result"
            )
            testCase "RightEndpoint 1/x" (fun _ ->
                let actual = observations2 |> NumericalIntegration.integrateObservations(RightEndpoint)
                //exact result is 0.25
                let expected = round 5 (log 100.)
                Expect.floatClose Accuracy.low (round 5 actual) expected "LeftEndpoint did not return the correct result"
            )
            testCase "MidPoint 1/x" (fun _ ->
                let actual = observations2 |> NumericalIntegration.integrateObservations(MidPoint)
                //exact result is 0.25
                let expected = round 5 (log 100.)
                Expect.floatClose Accuracy.high (round 5 actual) expected "LeftEndpoint did not return the correct result"
            )
            testCase "Trapezoidal 1/x" (fun _ ->
                let actual = observations2 |> NumericalIntegration.integrateObservations(Trapezoidal)
                //exact result is 0.25
                let expected = round 5 (log 100.)
                Expect.floatClose Accuracy.high (round 5 actual) expected "LeftEndpoint did not return the correct result"
            )
            testCase "Simpson 1/x" (fun _ ->
                let actual = observations2 |> NumericalIntegration.integrateObservations(Simpson)
                //exact result is 0.25
                let expected = round 5 (log 100.)
                Expect.floatClose Accuracy.high (round 5 actual) expected "LeftEndpoint did not return the correct result"
            )
        ]
        testList "integrating nan function returns nan" [
            testCase "LeftEndpoint" (fun _ -> Expect.isTrue (nan.Equals(fNaN |> NumericalIntegration.integrateFunction(LeftEndpoint,0.,1.,1000))) "did not return nan")
            testCase "RightEndpoint" (fun _ -> Expect.isTrue (nan.Equals(fNaN |> NumericalIntegration.integrateFunction(RightEndpoint,0.,1.,1000)))  "did not return nan")
            testCase "MidPoint" (fun _ -> Expect.isTrue (nan.Equals(fNaN |> NumericalIntegration.integrateFunction(MidPoint,0.,1.,1000)))  "did not return nan")
            testCase "Trapezoidal" (fun _ -> Expect.isTrue (nan.Equals(fNaN |> NumericalIntegration.integrateFunction(Trapezoidal,0.,1.,1000)))  "did not return nan")
            testCase "Simpson" (fun _ -> Expect.isTrue (nan.Equals(fNaN |> NumericalIntegration.integrateFunction(Simpson,0.,1.,1000)))  "did not return nan")
        ]
        testList "integrating +infinity function returns +infinity" [
            testCase "LeftEndpoint" (fun _ -> Expect.equal (fPosInf |> NumericalIntegration.integrateFunction(LeftEndpoint,0.,1.,1000)) infinity "did not return infinity")
            testCase "RightEndpoint" (fun _ -> Expect.equal (fPosInf |> NumericalIntegration.integrateFunction(RightEndpoint,0.,1.,1000)) infinity "did not return infinity")
            testCase "MidPoint" (fun _ -> Expect.equal (fPosInf |> NumericalIntegration.integrateFunction(MidPoint,0.,1.,1000)) infinity "did not return infinity")
            testCase "Trapezoidal" (fun _ -> Expect.equal (fPosInf |> NumericalIntegration.integrateFunction(Trapezoidal,0.,1.,1000)) infinity "did not return infinity")
            testCase "Simpson" (fun _ -> Expect.equal (fPosInf |> NumericalIntegration.integrateFunction(Simpson,0.,1.,1000)) infinity "did not return infinity")
        ]
        testList "integrating +infinity function returns -infinity" [
            testCase "LeftEndpoint" (fun _ -> Expect.equal (fNegInf |> NumericalIntegration.integrateFunction(LeftEndpoint,0.,1.,1000)) -infinity "did not return -infinity")
            testCase "RightEndpoint" (fun _ -> Expect.equal (fNegInf |> NumericalIntegration.integrateFunction(RightEndpoint,0.,1.,1000)) -infinity "did not return -infinity")
            testCase "MidPoint" (fun _ -> Expect.equal (fNegInf |> NumericalIntegration.integrateFunction(MidPoint,0.,1.,1000)) -infinity "did not return -infinity")
            testCase "Trapezoidal" (fun _ -> Expect.equal (fNegInf |> NumericalIntegration.integrateFunction(Trapezoidal,0.,1.,1000)) -infinity "did not return -infinity")
            testCase "Simpson" (fun _ -> Expect.equal (fNegInf |> NumericalIntegration.integrateFunction(Simpson,0.,1.,1000)) -infinity "did not return -infinity")
        ]
        testList "integrating nan observations returns nan" [
            testCase "LeftEndpoint" (fun _ -> Expect.isTrue (nan.Equals(observationsNaN |> NumericalIntegration.integrateObservations(LeftEndpoint))) "did not return nan")
            testCase "RightEndpoint" (fun _ -> Expect.isTrue (nan.Equals(observationsNaN |> NumericalIntegration.integrateObservations(RightEndpoint))) "did not return nan")
            testCase "MidPoint" (fun _ -> Expect.isTrue (nan.Equals(observationsNaN |> NumericalIntegration.integrateObservations(MidPoint))) "did not return nan")
            testCase "Trapezoidal" (fun _ -> Expect.isTrue (nan.Equals(observationsNaN |> NumericalIntegration.integrateObservations(Trapezoidal))) "did not return nan")
            testCase "Simpson" (fun _ -> Expect.isTrue (nan.Equals(observationsNaN |> NumericalIntegration.integrateObservations(Simpson))) "did not return nan")
        ]
        testList "integrating +infinity observations returns +infinity" [
            testCase "LeftEndpoint" (fun _ -> Expect.equal (observationsPosInf |> NumericalIntegration.integrateObservations(LeftEndpoint)) infinity "did not return infinity")
            testCase "RightEndpoint" (fun _ -> Expect.equal (observationsPosInf |> NumericalIntegration.integrateObservations(RightEndpoint)) infinity "did not return infinity")
            testCase "MidPoint" (fun _ -> Expect.equal (observationsPosInf |> NumericalIntegration.integrateObservations(MidPoint)) infinity "did not return infinity")
            testCase "Trapezoidal" (fun _ -> Expect.equal (observationsPosInf |> NumericalIntegration.integrateObservations(Trapezoidal)) infinity "did not return infinity")
            testCase "Simpson" (fun _ -> Expect.equal (observationsPosInf |> NumericalIntegration.integrateObservations(Simpson)) infinity "did not return infinity")
        ]
        testList "integrating +infinity observations returns -infinity" [
            testCase "LeftEndpoint" (fun _ -> Expect.equal (observationsNegInf |> NumericalIntegration.integrateObservations(LeftEndpoint)) -infinity "did not return -infinity")
            testCase "RightEndpoint" (fun _ -> Expect.equal (observationsNegInf |> NumericalIntegration.integrateObservations(RightEndpoint)) -infinity "did not return -infinity")
            testCase "MidPoint" (fun _ -> Expect.equal (observationsNegInf |> NumericalIntegration.integrateObservations(MidPoint)) -infinity "did not return -infinity")
            testCase "Trapezoidal" (fun _ -> Expect.equal (observationsNegInf |> NumericalIntegration.integrateObservations(Trapezoidal)) -infinity "did not return -infinity")
            testCase "Simpson" (fun _ -> Expect.equal (observationsNegInf |> NumericalIntegration.integrateObservations(Simpson)) -infinity "did not return -infinity")
        ]
    ]