module FinancialsTests

open Expecto

open FSharp.Stats
open FSharp.Stats.Fitting.NonLinearRegression

//R's implementaion curve fits excatly however much the parameters do not match
//https://cran.r-project.org/web/packages/NMOF/vignettes/DEnss.pdf

[<Tests>]
let nelsonSiegelTests =
    let time = [|0.083;0.25;0.5;0.75;1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0;9.0;10.0|]
    let observedYields = [|9.193782;9.502359;9.804080;9.959691;10.010291;9.672974;9.085818;8.553107;8.131273;7.808959;7.562701;7.371855;7.221084;7.099587|]

     let solverOptionsNS = createSolverOption 0.0001 0.0001 5000 [|0.01;0.01;0.01;1.0|]
     let coefficientsNS = LevenbergMarquardt.estimatedParams nelsonSiegel solverOptionsNS 0.001 10. time observedYields
     let fittingFunctionNS = nelsonSiegel.GetFunctionValue coefficientsNS
     let expected = time |> Array.map fittingFunctionNS
     let estimated =  [[|9.8606; 9.8055; 9.7235; 9.6418; 9.5605; 9.2395; 8.9251; 8.6172; 8.3158;8.021; 7.7328; 7.4511; 7.1759; 6.9073|]

    Expect.equal actual expected
