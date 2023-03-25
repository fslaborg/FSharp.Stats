module DistributionsEmpiricalTests

open Expecto
open System
open FSharp.Stats
open FSharp.Stats.Distributions
open TestExtensions

[<Tests>]
let empiricalTests =

   
    let mySmallAlphabet = "abcdefghijklmnopqrstuvwxyz" |> Set.ofSeq
    let myAlphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" |> Set.ofSeq
    let myAlphabetNum = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" |> Set.ofSeq
    let mySeqInt = [1;0;-1;5;10;10;10;-1]
    let mySeqFloat = [0.1;0.0;0.1;0.5;1.0;1.;1.;0.25;0.35;0.35;0.30;0.8;0.8]
    let myText = "Hello World, I am a test Text with all kind of Characters!11"

    testList "Distributions.Empirical" [
                
        testCase "create" <| fun () ->
            let expectedKeys,expectedValues = 
                Map.ofSeq [(0.05, 0.07692307692); (0.15, 0.1538461538); (0.25, 0.07692307692);
                     (0.35, 0.2307692308); (0.55, 0.07692307692); (0.85, 0.1538461538);
                     (1.05, 0.2307692308)]
                |> Map.toArray
                |> Array.unzip
            let actualKeys,actualValues =    
                Empirical.create 0.1 mySeqFloat
                |> Map.toArray
                |> Array.unzip
            TestExtensions.sequenceEqual(Accuracy.high) expectedKeys actualKeys
                "Empirical.create leads to a wrong PMF map keys" 
            TestExtensions.sequenceEqual(Accuracy.high) expectedValues actualValues
                "Empirical.create leads to a wrong PMF map values" 

            
            let expectedKeys',expectedValues' = 
                Map.ofSeq [(0.5, 0.7692307692); (1.5, 0.2307692308)]
                |> Map.toArray
                |> Array.unzip
            let actualKeys',actualValues' =    
                Empirical.create 1 mySeqFloat
                |> Map.toArray
                |> Array.unzip

            TestExtensions.sequenceEqual(Accuracy.high) expectedKeys' actualKeys'
                "Empirical.create leads to a wrong PMF map keys" 
            TestExtensions.sequenceEqual(Accuracy.high) expectedValues' actualValues'
                "Empirical.create leads to a wrong PMF map values" 
        
        testCase "createNominal" <| fun () ->
            let expectedKeys,expectedValues = 
                Map.ofSeq [(' ', 0.1833333333); ('!', 0.01666666667); (',', 0.01666666667);
                    ('1', 0.03333333333); ('C', 0.01666666667); ('H', 0.01666666667);
                    ('I', 0.01666666667); ('T', 0.01666666667); ('W', 0.01666666667);
                    ('a', 0.08333333333); ('c', 0.01666666667); ('d', 0.03333333333);
                    ('e', 0.06666666667); ('f', 0.01666666667); ('h', 0.03333333333);
                    ('i', 0.03333333333); ('k', 0.01666666667); ('l', 0.08333333333);
                    ('m', 0.01666666667); ('n', 0.01666666667); ('o', 0.05); ('r', 0.05);
                    ('s', 0.03333333333); ('t', 0.08333333333); ('w', 0.01666666667);
                    ('x', 0.01666666667)]
                |> Map.toArray
                |> Array.unzip
            let actualKeys,actualValues = 
                EmpiricalDistribution.createNominal() myText
                |> Map.toArray
                |> Array.unzip
            Expect.equal expectedKeys actualKeys
                "Empirical.createNominal leads to a wrong PMF map keys" 
            TestExtensions.sequenceEqual(Accuracy.high) expectedValues actualValues
                "Empirical.createNominal leads to a wrong PMF map values" 
                
        testCase "createNominalTemplate" <| fun () ->
            let expectedKeys,expectedValues = 
                Map.ofSeq [('A', 0.0); ('B', 0.0); ('C', 0.02222222222); ('D', 0.0); ('E', 0.0);
                    ('F', 0.0); ('G', 0.0); ('H', 0.02222222222); ('I', 0.02222222222);
                    ('J', 0.0); ('K', 0.0); ('L', 0.0); ('M', 0.0); ('N', 0.0); ('O', 0.0);
                    ('P', 0.0); ('Q', 0.0); ('R', 0.0); ('S', 0.0); ('T', 0.02222222222);
                    ('U', 0.0); ('V', 0.0); ('W', 0.02222222222); ('X', 0.0); ('Y', 0.0);
                    ('Z', 0.0); ('a', 0.1111111111); ('b', 0.0); ('c', 0.02222222222);
                    ('d', 0.04444444444); ('e', 0.08888888889); ('f', 0.02222222222);
                    ('g', 0.0); ('h', 0.04444444444); ('i', 0.04444444444); ('j', 0.0);
                    ('k', 0.02222222222); ('l', 0.1111111111); ('m', 0.02222222222);
                    ('n', 0.02222222222); ('o', 0.06666666667); ('p', 0.0); ('q', 0.0);
                    ('r', 0.06666666667); ('s', 0.04444444444); ('t', 0.1111111111);
                    ('u', 0.0); ('v', 0.0); ('w', 0.02222222222); ('x', 0.02222222222);
                    ('y', 0.0); ('z', 0.0)]
                |> Map.toArray
                |> Array.unzip
            let actualKeys,actualValues = 
                EmpiricalDistribution.createNominal(myAlphabet) myText
                |> Map.toArray
                |> Array.unzip
            Expect.equal expectedKeys actualKeys
                "Empirical.createNominal leads to a wrong PMF map keys" 
            TestExtensions.sequenceEqual(Accuracy.high) expectedValues actualValues
                "Empirical.createNominal leads to a wrong PMF map values" 

        testCase "createNominalTemplateNum" <| fun () ->
            let expectedKeys,expectedValues = 
                Map.ofSeq [|('0', 0.0); ('1', 0.04255319149); ('2', 0.0); ('3', 0.0); ('4', 0.0);
                    ('5', 0.0); ('6', 0.0); ('7', 0.0); ('8', 0.0); ('9', 0.0); ('A', 0.0);
                    ('B', 0.0); ('C', 0.02127659574); ('D', 0.0); ('E', 0.0); ('F', 0.0);
                    ('G', 0.0); ('H', 0.02127659574); ('I', 0.02127659574); ('J', 0.0);
                    ('K', 0.0); ('L', 0.0); ('M', 0.0); ('N', 0.0); ('O', 0.0); ('P', 0.0);
                    ('Q', 0.0); ('R', 0.0); ('S', 0.0); ('T', 0.02127659574); ('U', 0.0);
                    ('V', 0.0); ('W', 0.02127659574); ('X', 0.0); ('Y', 0.0); ('Z', 0.0);
                    ('a', 0.1063829787); ('b', 0.0); ('c', 0.02127659574);
                    ('d', 0.04255319149); ('e', 0.08510638298); ('f', 0.02127659574);
                    ('g', 0.0); ('h', 0.04255319149); ('i', 0.04255319149); ('j', 0.0);
                    ('k', 0.02127659574); ('l', 0.1063829787); ('m', 0.02127659574);
                    ('n', 0.02127659574); ('o', 0.06382978723); ('p', 0.0); ('q', 0.0);
                    ('r', 0.06382978723); ('s', 0.04255319149); ('t', 0.1063829787);
                    ('u', 0.0); ('v', 0.0); ('w', 0.02127659574); ('x', 0.02127659574);
                    ('y', 0.0); ('z', 0.0)|]
                |> Map.toArray
                |> Array.unzip
            let actualKeys,actualValues = 
                EmpiricalDistribution.createNominal(myAlphabetNum) myText
                |> Map.toArray
                |> Array.unzip
            Expect.equal expectedKeys actualKeys
                "Empirical.createNominal leads to a wrong PMF map keys" 
            TestExtensions.sequenceEqual(Accuracy.high) expectedValues actualValues
                "Empirical.createNominal leads to a wrong PMF map values" 

        testCase "createNominalTemplateTransform" <| fun () ->
            let expectedKeys,expectedValues = 
                Map.ofSeq [|('a', 0.1111111111); ('b', 0.0); ('c', 0.04444444444);
                    ('d', 0.04444444444); ('e', 0.08888888889); ('f', 0.02222222222);
                    ('g', 0.0); ('h', 0.06666666667); ('i', 0.06666666667); ('j', 0.0);
                    ('k', 0.02222222222); ('l', 0.1111111111); ('m', 0.02222222222);
                    ('n', 0.02222222222); ('o', 0.06666666667); ('p', 0.0); ('q', 0.0);
                    ('r', 0.06666666667); ('s', 0.04444444444); ('t', 0.1333333333);
                    ('u', 0.0); ('v', 0.0); ('w', 0.04444444444); ('x', 0.02222222222);
                    ('y', 0.0); ('z', 0.0)|]
                |> Map.toArray
                |> Array.unzip
            let actualKeys,actualValues = 
                EmpiricalDistribution.createNominal(mySmallAlphabet,System.Char.ToLower) myText
                |> Map.toArray
                |> Array.unzip
            Expect.equal expectedKeys actualKeys
                "Empirical.createNominal leads to a wrong PMF map keys" 
            TestExtensions.sequenceEqual(Accuracy.high) expectedValues actualValues
                "Empirical.createNominal leads to a wrong PMF map values" 
    ]

