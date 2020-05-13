namespace FSharp.Stats.Tests
open Expecto

module DistributionsTests =
    open FSharp.Stats.Distributions
    open Distance.OneDimensional
    [<Tests>]
    let testDistanceFunctions =
        // Tests taken directly from the source implementation in scipy
        //
        // WassersteinDistance: https://github.com/scipy/scipy/blob/master/scipy/stats/stats.py#L6986
        // EnergyDistance: https://github.com/scipy/scipy/blob/master/scipy/stats/stats.py#L7068
        testList "Distributions.Distance" [
            testCase "test_WassersteinDistance" <| fun () ->
                let xs = [|3.4; 3.9; 7.5; 7.8|]
                let ys = [|4.5; 1.4|]
                let xWeights = [|1.4; 0.9; 3.1; 7.2|]
                let yWeights = [|3.2; 3.5|]
                let distance = wassersteinDistanceWeighted xs ys xWeights yWeights
                Expect.floatClose Accuracy.high distance 4.0781331438047861 "Should be equal (double precision)"
            testCase "test_EnergyDistance" <| fun () ->
                let xs =        [|0.7; 7.4; 2.4; 6.8|]
                let ys =        [|1.4; 8. |]
                let xWeights =  [|2.1; 4.2; 7.4; 8. |]
                let yWeights =  [|7.6; 8.8|]
                let distance = energyDistanceWeighted xs ys xWeights yWeights
                Expect.floatClose Accuracy.high distance 0.88003340976158217 "Should be equal (double precision)"
        ]