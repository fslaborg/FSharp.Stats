module FittingTests

open Expecto

open FSharp.Stats
open FSharp.Stats.Fitting
open FSharp.Stats.Fitting.NonLinearRegression

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

[<Tests>]
let leastSquaresCholeskyTests = 
    // Create random input 
    let normal = Distributions.Continuous.normal 0.0 1.0
    let n = 1000
    let xs = [|for _ in [1..n] do 4.0 * normal.Sample()|]
    let us = [|for _ in [1..n] do 0.5 * normal.Sample()|]
    let xVector = vector xs
    let yVector = vector (Array.map (fun x -> 3.0 + 2.0 * x) xs)
    let yData = vector (Array.map2 (fun x u -> 3.0 + 2.0 * x + u) xs us)
    let xData = Matrix.ofJaggedArray (Array.zip xs us |> Array.map (fun (a,b) -> [|a;b|]))

    testList "Least Squares with Cholesky" [
        testCase "Univariable Regression" (fun () ->
            let expectedCoefficients = [3.; 2.]
            let coeffcientsCholesky = LinearRegression.OrdinaryLeastSquares.Linear.Univariable.coefficientCholesky (vector xs) yData
            Expect.floatClose Accuracy.low coeffcientsCholesky.[0] expectedCoefficients.[0] "Coefficient should be equal (double precision)"
            Expect.floatClose Accuracy.low coeffcientsCholesky.[1] expectedCoefficients.[1] "Coefficient should be equal (double precision)"
        )
        testCase "Multivariable Regression" (fun () ->
            let expectedCoefficients = [3.; 2.; 1.]
            let coeffcientsCholesky = LinearRegression.OrdinaryLeastSquares.Linear.Multivariable.coefficientsCholesky xData yData
            Expect.floatClose Accuracy.low coeffcientsCholesky.[0] expectedCoefficients.[0] "Coefficient should be equal (double precision)"
            Expect.floatClose Accuracy.low coeffcientsCholesky.[1] expectedCoefficients.[1] "Coefficient should be equal (double precision)"
            Expect.floatClose Accuracy.low coeffcientsCholesky.[2] expectedCoefficients.[2] "Coefficient should be equal (double precision)"
        )
    ]