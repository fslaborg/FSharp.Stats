(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "netstandard.dll"
#I "../../src/FSharp.Stats/bin/Release/net461"
#r "../../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r @"../../lib/Formatting/FSharp.Plotly.dll"
open FSharp.Plotly


(**

#Fitting
##Linear Regression

In Linear Regression a linear system of equations is generated. The coefficients obtained by the solution to this equation 
system minimize the squared distances from the regression curve to the data points. These distances are also known as residuals (or least squares).

###Simple Linear Regression

Simple linear regression aims to fit a straight regression line to the data. While the least squares approach efficiently minimizes the sum of squared residuals it is prone to outliers. 
An alternative is a robust simple linear regression like Theil's incomplete method or the Theil-Sen estimator, that are outlier resistant.

*)

open FSharp.Stats
open FSharp.Plotly
open FSharp.Stats.Fitting.LinearRegression

let x_Data = vector [|1. .. 10.|]
let y_Data = vector [|4.;7.;9.;12.;15.;17.;16.;23.;5.;30.|]

//Least squares simple linear regression
let coefficientsLinearLS = 
    OrdinaryLeastSquares.Linear.Univariable.coefficient x_Data y_Data
let fittingFunctionLinearLS x = 
    OrdinaryLeastSquares.Linear.Univariable.fit coefficientsLinearLS x

//Robust simple linear regression
let coefficientsLinearRobust = 
    RobustRegression.Linear.theilSenEstimator x_Data y_Data 
let fittingFunctionLinearRobust x = 
    RobustRegression.Linear.fit coefficientsLinearRobust x

//least squares simple linear regression through the origin
let coefficientsLinearRTO = 
    OrdinaryLeastSquares.Linear.RTO.coefficientOfVector x_Data y_Data 
let fittingFunctionLinearRTO x = 
    OrdinaryLeastSquares.Linear.RTO.fit coefficientsLinearRTO x


let rawChart = 
    Chart.Point(x_Data,y_Data)
    |> Chart.withTraceName "raw data"
    
let fittingLS = 
    let fit = 
        [|0. .. 11.|] 
        |> Array.map (fun x -> x,fittingFunctionLinearLS x)
    Chart.Line(fit)
    |> Chart.withTraceName "least squares (LS)"

let fittingRobust = 
    let fit = 
        [|0. .. 11.|] 
        |> Array.map (fun x -> x,fittingFunctionLinearRobust x)
    Chart.Line(fit)
    |> Chart.withTraceName "TheilSen estimator"

let fittingRTO = 
    let fit = 
        [|0. .. 11.|] 
        |> Array.map (fun x -> x,fittingFunctionLinearRTO x)
    Chart.Line(fit)
    |> Chart.withTraceName "LS through origin"

let simpleLinearChart =
    [rawChart;fittingLS;fittingRTO;fittingRobust;] 
    |> Chart.Combine

(*** include-value:simpleLinearChart ***)

//Multivariate simple linear regression
let xVectorMulti =
    [
    [1.; 1. ;2.  ]
    [2.; 0.5;6.  ]
    [3.; 0.8;10. ]
    [4.; 2. ;14. ]
    [5.; 4. ;18. ]
    [6.; 3. ;22. ]
    ]
    |> Matrix.ofSeq

let yVectorMulti = 
    let transformX (x:Matrix<float>) =
        x
        |> Matrix.mapiRows (fun _ v -> 100. + (v.[0] * 2.5) + (v.[1] * 4.) + (v.[2] * 0.5))
    xVectorMulti
    |> transformX
    |> vector

let coefficientsMV = 
    OrdinaryLeastSquares.Linear.Multivariable.coefficients xVectorMulti yVectorMulti
let fittingFunctionMV x = 
    OrdinaryLeastSquares.Linear.Multivariable.fit coefficientsMV x


(**

###Polynomial Regression

In polynomial regression a higher degree (d > 1) polynomial is fitted to the data. The coefficients are chosen that the sum of squared residuals is minimized.

*)

open FSharp.Stats
open FSharp.Stats.Fitting.LinearRegression
open FSharp.Plotly

let x_DataP = vector [|1. .. 10.|]
let y_DataP = vector [|4.;7.;9.;8.;6.;3.;2.;5.;6.;8.;|]

//Least squares polynomial regression

//define the order the polynomial should have (order 3: f(x) = ax^3 + bx^2 + cx + d)
let order = 3
let coefficientsPol = 
    OrdinaryLeastSquares.Polynomial.coefficient order x_DataP y_DataP 
let fittingFunctionPol x = 
    OrdinaryLeastSquares.Polynomial.fit order coefficientsPol x

//weighted least squares polynomial regression
//If heteroscedasticity is assumed or the impact of single datapoints should be 
//increased/decreased you can use a weighted version of the polynomial regression.

//define the order the polynomial should have (order 3: f(x) = ax^3 + bx^2 + cx + d)
let orderP = 3

//define the weighting vector
let weights = y_DataP |> Vector.map (fun y -> 1. / y)
let coefficientsPolW = 
    OrdinaryLeastSquares.Polynomial.coefficientsWithWeighting orderP weights x_DataP y_DataP 
let fittingFunctionPolW x = 
    OrdinaryLeastSquares.Polynomial.fit orderP coefficientsPolW x

let rawChartP = 
    Chart.Point(x_DataP,y_DataP)
    |> Chart.withTraceName "raw data"
    
let fittingPol = 
    let fit = 
        [|1. .. 0.1 .. 10.|] 
        |> Array.map (fun x -> x,fittingFunctionPol x)
    Chart.Line(fit)
    |> Chart.withTraceName "order = 3"

let fittingPolW = 
    let fit = 
        [|1. .. 0.1 .. 10.|] 
        |> Array.map (fun x -> x,fittingFunctionPolW x)
    Chart.Line(fit)
    |> Chart.withTraceName "order = 3 weigthed"

let polRegressionChart =
    [rawChartP;fittingPol;fittingPolW] 
    |> Chart.Combine

(*** include-value:polRegressionChart ***)


(**
##Nonlinear Regression

Nonlinear Regression is used if a known model should to be fitted to the data that cannot be represented in a linear system of equations. 
Common examples are: 

 - gaussian functions

 - log functions

 - exponential functions

To fit such models to your data the `NonLinearRegression` module can be used. Two solver-methods are availiable to iteratively converge to a minimal least squares value.

 - GaussNewton

 - LevenbergMarquardt

For solving a nonlinear problem the model function has to be converted to a `NonLinearRegression.Model` type consisting of 

 - parameter names,

 - the function itself, and

 - partial derivatives of all unknown parameters.

For clarification a exponential relationship in the form of `y = a * exp(b * x)` should be solved:

*)

open System
open FSharp.Stats.Fitting
open FSharp.Stats.Fitting.LinearRegression
open FSharp.Stats.Fitting.NonLinearRegression


let x_DataN = [|1.;2.; 3.; 4.|]
let y_DataN = [|5.;14.;65.;100.|]

//search for:  y = a * exp(b * x)

// 1. create the model
// 1.1 define parameter names
let parameterNames = [|"a";"b"|]

// 1.2 define the exponential function that gets a parameter vector containing the 
//searched parameters and the x_value and gives the corresponding y_value
let getFunctionValue =                 
    fun (parameterVector: Vector<float>) x -> 
        parameterVector.[0] * Math.Exp(parameterVector.[1] * x)
      //a                   *      exp(b                   * x)

// 1.3 Define partial derivatives of the exponential function. 
//     Take partial derivatives for every unknown parameter and
//     insert it into the gradient vector sorted by parameterNames.
let getGradientValues =
    fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValueN -> 
        // partial derivative of y=a*exp(b*x) in respect to the first parameter (a)   --> exp(b*x)
        gradientVector.[0] <- Math.Exp(parameterVector.[1] * xValueN)  
        // partial derivative of y=a*exp(b*x) in respect to the second parameter (b)  --> a*x*exp(b*x)
        gradientVector.[1] <- parameterVector.[0] * xValueN * Math.Exp(parameterVector.[1] * xValueN)  

        gradientVector

// 1.4 create the model
let model = createModel parameterNames getFunctionValue getGradientValues



// 2. define the solver options
// 2.1 define the stepwidth of the x_value change
let deltaX = 0.0001

// 2.2 define the stepwidth of the parameter change
let deltaP = 0.0001

// 2.3 define the number of iterations
let k = 1000

// 2.4 define an initial guess
//     For many problems you can set a default value or let the user decide to choose an 
//     appropriate guess. In the case of an exponential or log model you can use the 
//     solution of the linearized problem as a first guess.
let initialParamGuess (x_data:float []) (y_data:float [])=
    //gets the linear representation of the problem and solves it by simple linear regression 
    //(prone to least-squares-deviations at high y_Values)
    let y_ln = y_data |> Array.map (fun x -> Math.Log(x)) |> vector
    let linearReg = 
        LinearRegression.OrdinaryLeastSquares.Linear.Univariable.coefficient (vector x_data) y_ln
    //calculates the parameters back into the exponential representation
    let a = exp linearReg.[0]
    let b = linearReg.[1]
    [|a;b|]

// 2.5 create the solverOptions
let solverOptions = 
    let guess = initialParamGuess x_DataN y_DataN
    NonLinearRegression.createSolverOption 0.0001 0.0001 1000 guess


// 3. get coefficients
let coefficientsExp = GaussNewton.estimatedParams model solverOptions x_DataN y_DataN
//val coefficients = vector [|5.68867298; 0.7263428835|]


// 4. create fitting function
let fittingFunction x = coefficientsExp.[0] * Math.Exp(coefficientsExp.[1] * x)

(**

##Smoothing spline

A smoothing spline aims to minimize a function consisting of two error terms: 

 - error1: sum of squared residuals

    - Similar to the OrdinaryLeastSquares regression this error term ensures the fidelity to the data.

 - error2: integral of the second derivative of the fitting function

    - This error term ensures the smoothness of the resulting curve.

A smoothing parameter (lambda) mediates between the two error terms.

 - E = error1 + (lambda * error2)

    - If lambda = 0, the the resulting curve minimizes the sum of squared residuals and results in an interpolating curve.

    - If lambda = infinity, the resulting curve is punished by the smoothness measurement and results in a straight regression line.

The spline is constructed out of piecewise cubic polynomials that meet at knots. In the defined knots the function is continuous. 
Depending on the used smoothing factor and the defined knots the smoothing spline has a unique solution. The resulting curve is just defined within the interval defined in the x values of the data.

The right amount of smoothing can be determined by cross validation or generalized cross validation.
*)

open FSharp.Stats.Fitting

let x_DataS = [|1.;2.; 3.; 4.|]
let y_DataS = [|5.;14.;65.;75.|]

let data = Array.zip x_DataS y_DataS

//in every x position a knot should be located
let knots = x_DataS

let spline lambda x = (Spline.smoothingSpline data knots) lambda x

let fit lambda = 
    [|1. .. 0.1 .. 4.|]
    |> Array.map (fun x -> x,spline lambda x)
    |> Chart.Line
    |> Chart.withTraceName (sprintf "lambda: %.3f" lambda)

let rawChartS = Chart.Point(data)

let smoothingSplines =
    [
    rawChartS
    fit 0.001
    fit 0.02
    fit 1.
    ]
    |> Chart.Combine

(*** include-value:smoothingSplines ***)