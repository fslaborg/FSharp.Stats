module SpecialFunctionsTests
open Expecto
    
open FSharp.Stats.SpecialFunctions
open FSharp.Stats

[<Tests>]
let gammaFunctionsTests =

    testList "SpecialFunctions.Gamma" [
        testCase "test_gamma" <| fun () ->
            let gam = Gamma.gamma 5.
            Expect.floatClose Accuracy.high gam  24. "Should be equal (double precision)"

        testCase "test_gammaln" <| fun () ->
            let gamln = Gamma.gammaLn 3.
            let lngam = Gamma.gamma 3. |> log
            Expect.floatClose Accuracy.high gamln lngam "Should be equal (double precision)"
 
        testCase "test_gammainc" <| fun () ->
            let gam = Gamma.lowerIncomplete 0.5 0.5
            Expect.floatClose Accuracy.low gam 0.682689 "Should be equal (low precision)"

        testCase "test_gammaincnan" <| fun () ->
            let gam = Gamma.lowerIncomplete -1. 1.
            Expect.isTrue (nan.Equals(gam)) "IsNan"

        testCase "test_gammainczero" <| fun () ->
            let gam = Gamma.lowerIncomplete -1. 0.
            Expect.floatClose Accuracy.high gam 0.0 "IsZero"

        testCase "test_gammaincinf" <| fun () ->
            let gam = Gamma.lowerIncomplete 0.5 Ops.inf
            Expect.equal gam 1.0 "Should be equal"

        testCase "test_gammaincupper" <| fun () ->
            let gamu = Gamma.upperIncomplete 0.5 0.5
            let gam  = 1. - Gamma.lowerIncomplete 0.5 0.5
            Expect.floatClose Accuracy.medium gamu gam "Should be equal (medium precision)"

        testCase "test_gammaincuppernan" <| fun () ->
            let gam = Gamma.upperIncomplete -1. 1.
            Expect.isTrue (nan.Equals(gam)) "IsNan"

        testCase "test_gammaincupperinf" <| fun () ->
            let gam = Gamma.upperIncomplete 0.5 Ops.inf
            Expect.equal gam 1.0 "Should be equal"

    ]    




[<Tests>]
let betaFunctionsTests =

    testList "SpecialFunctions.Beta.betaLn" [
        testCase "equality1" <| fun () ->
            let result = Beta.betaLn 1. 1. 
            Expect.floatClose Accuracy.veryHigh result 0.0 "Should be equal (double precision)" //rtol=1e-14, atol=0
            //Expect.equal result 0.0 "Should be equal"
            //
        //testCase "equality2" <| fun () ->
        //    let result = Beta.betaLn -100.3 1e-200
        //    Expect.equal result (Gamma.gammaLn 1e-200) "Should be equal"

        testCase "equality3" <| fun () ->
            let result = Beta.betaLn 0.0342 170.
            Expect.floatClose Accuracy.veryHigh result 3.1811881124242447 "Should be equal (double precision)" //rtol=1e-14, atol=0
         
    ]