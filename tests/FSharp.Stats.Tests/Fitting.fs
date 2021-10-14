module FittingTests

open Expecto

open FSharp.Stats
open FSharp.Stats.Fitting.NonLinearRegression

let compareSeq (a:seq<float>) (b:seq<float>) (str:string) =
    Seq.iter2 (fun a b -> Expect.floatClose Accuracy.high a b str) a b

[<Tests>]
let nonLinearRegressionTests = 
    let time = [|0.083;0.25;0.5;0.75;1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0;9.0;10.0|]
    let observedYields = [|9.193782;9.502359;9.804080;9.959691;10.010291;9.672974;9.085818;8.553107;8.131273;7.808959;7.562701;7.371855;7.221084;7.099587|]
    testList "Fitting.NonLinearRegression.Table" [
        testCase "nelsonSiegel" <| fun () -> 
            //R's implementaion curve fits exactly 
            //https://cran.r-project.org/web/packages/NMOF/vignettes/DEnss.pdf
            let coefficientsNS = 
                let solverOptionsNS = createSolverOption 0.0001 0.0001 5000 [|13.;1.;3.;1.|]
                LevenbergMarquardt.estimatedParams Table.Finances.nelsonSiegel solverOptionsNS 0.001 10. time observedYields
            let expectedCoefficients = vector [6.;3.;8.;1.]
            Expect.floatClose Accuracy.low coefficientsNS.[0] expectedCoefficients.[0] "Coefficient should be equal (double precision)"
            Expect.floatClose Accuracy.low coefficientsNS.[1] expectedCoefficients.[1] "Coefficient should be equal (double precision)"
            Expect.floatClose Accuracy.low coefficientsNS.[2] expectedCoefficients.[2] "Coefficient should be equal (double precision)"
            Expect.floatClose Accuracy.low coefficientsNS.[3] expectedCoefficients.[3] "Coefficient should be equal (double precision)"
    ]

open FSharp.Stats.Fitting.Spline
[<Tests>]
let splineTests = 
    testList "Fitting.Spline" [
            testCase "smoothingSpline" <| fun () -> 
                let xDataS = [|1.;2.; 3.; 4.|]
                let yDataS = [|5.;14.;65.;75.|]

                let data = Array.zip xDataS yDataS

                //in every x position a knot should be located
                let knots = xDataS

                let spline lambda x = (smoothingSpline data knots) lambda x

                let fits = 
                    [|0.001;0.02 ;1.|]
                    |> Array.collect (fun lambda -> 
                        [|1. .. 0.1 .. 4.|]
                        |> Array.map (spline lambda) )
                let results = 
                    [|
                        [|4.690215414; 4.435792393; 4.258815519; 4.236730939; 4.446984798;
                          4.967023244; 5.874292422; 7.246238481; 9.160307566; 11.69394582;
                          14.9245994; 18.89118947; 23.4785373; 28.53293919; 33.90069144;
                          39.42809033; 44.96143218; 50.34701327; 55.43112989; 60.06007836;
                          64.08015496; 67.37598286; 69.98549275; 71.98494218; 73.45058872;
                          74.45868991; 75.08550332; 75.40728649; 75.500297; 75.44079239;
                          75.30503023|];
                        [|2.40588539; 3.800441404; 5.227423851; 6.719259164; 8.308373774;
                          10.02719411; 11.90814662; 13.98365772; 16.28615385; 18.84806144;
                          21.70180692; 24.8637713; 28.28615385; 31.90510842; 35.65678885;
                          39.47734899; 43.30294269; 47.0697238; 50.71384615; 54.1714636;
                          57.37872999; 60.28767682; 62.91384615; 65.28865772; 67.44353123;
                          69.40988642; 71.219143; 72.9027207; 74.49203924; 76.01851833; 77.5135777|];
                        [|0.6748194736; 3.241767296; 5.809796413; 8.37998812; 10.95342371;
                          13.53118449; 16.11435174; 18.70400676; 21.30123084; 23.90710529;
                          26.52271139; 29.14860867; 31.78326951; 34.42464454; 37.07068437;
                          39.71933962; 42.36856091; 45.01629886; 47.66050408; 50.29912718;
                          52.9301188; 55.55193245; 58.16503331; 60.77038947; 63.36896902;
                          65.96174004; 68.54967063; 71.13372886; 73.71488283; 76.29410063;
                          78.87235034|]
                    |]
                    |> Array.concat
                
                compareSeq fits results "The fitted spline does not yield the expected predictions."   
                
        ]

