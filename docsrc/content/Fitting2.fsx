(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../src/FSharp.Stats/bin/Release/net461"
#r "../../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "../../packages/build/FSharp.Plotly/lib/net45/Fsharp.Plotly.dll"
open FSharp.Plotly
#r "netstandard.dll"




(**

#Fitting
##Linear Regression

In Linear Regression a linear system of equations is generated. The coefficients obtained by the solution to this equation 
system minimizes the distance from the regression curve to the data points. These distances are also known as residuals (or least squares).

###Simple Linear Regression

Simple linear regression aims to fit a straight regression line to the data

While the least squares approach efficiently minimizes the sum of squared residuals it is prone to outliers. 
An alternative is a robust simple linear regression like Theil's incomplete method or the Theil-Sen estimator, that are outlier resistant.

*)
(*** do-not-eval***)
open FSharp.Stats
open FSharp.Stats.Fitting.LinearRegression
open FSharp.Plotly

let x_Data = vector [|1.;2.;3.;|]
let y_Data = vector [|4.;7.;9.;|]

//Least squares simple linear regression
let coefficientsLinearLS = OrdinaryLeastSquares.Linear.Univariable.coefficient x_Data y_Data
let fittingFunctionLinearLS x = OrdinaryLeastSquares.Linear.Univariable.fit coefficientsLinearLS x

//Robust simple linear regression
let coefficientsLinearRobust = RobustRegression.Linear.theilSenEstimator x_Data y_Data 
let fittingFunctionLinearRobust x = RobustRegression.Linear.fit coefficientsLinearRobust x

//least squares simple linear regression through the origin
let coefficientsLinearRTO = OrdinaryLeastSquares.Linear.RTO.coefficientOfVector x_Data y_Data 
let fittingFunctionLinearRobust x = OrdinaryLeastSquares.Linear.RTO.fit coefficientsLinearRTO x


let rawChart = 
    Chart.Point(x_Data,y_Data)
    |> Chart.withTraceName "raw data"
    
let fittingLS = 
    let fit = [|0. .. 4.|] |> Array.map (fun x -> x,fittingFunctionLinearLS x)
    Chart.Line(fit)
    |> Chart.withTraceName "least squares"

let fittingRobust = 
    let fit = [|0. .. 4.|] |> Array.map (fun x -> x,fittingFunctionLinearRobust x)
    Chart.Line(fit)
    |> Chart.withTraceName "TheilSen estimator"

let fittingRTO = 
    let fit = [|0. .. 4.|] |> Array.map (fun x -> x,fittingFunctionLinearRobust x)
    Chart.Line(fit)
    |> Chart.withTraceName "through origin"

[rawChart;fittingLS;fittingRobust;fittingRTO] 
|> Chart.Combine
|> Chart.Show


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
let coefficientsMV = LinearRegression.OrdinaryLeastSquares.Linear.Multivariable.coefficients xVectorMulti yVectorMulti
let fittingFunctionMV x= LinearRegression.OrdinaryLeastSquares.Linear.Multivariable.fit coefficientsMV x


(**

###Polynomial Regression

In polynomial regression a higher degree (d > 1) polynomial is fitted to the data. The coefficients are chosen that the sum of squared residuals is minimized.

*)
(*** do-not-eval***)
open FSharp.Stats
open FSharp.Stats.Fitting.LinearRegression
open FSharp.Plotly

let x_Data = vector [|1. .. 10.|]
let y_Data = vector [|4.;7.;9.;8.;6.;3.;2.;5.;6.;8.;|]

//Least squares polynomial regression

//define the order the polynomial should have (order 3: f(x) = ax³ + bx² + cx + d)
let order = 3
let coefficientsPol = OrdinaryLeastSquares.Polynomial.coefficient order x_Data y_Data 
let fittingFunctionPol x = OrdinaryLeastSquares.Polynomial.fit order coefficientsPol x

//weighted least squares polynomial regression
//If heteroscedasticity is assumed or the impact of single datapoints should be increased/decreased you can use a weighted version of the polynomial regression.

//define the order the polynomial should have (order 3: f(x) = ax³ + bx² + cx + d)
let order = 3

//define the weighting vector
let weights = y_Data |> Vector.map (fun y -> 1. / y)
let coefficientsPolW = OrdinaryLeastSquares.Polynomial.coefficientsWithWeighting order weights x_Data y_Data 
let fittingFunctionPolW x = OrdinaryLeastSquares.Polynomial.fit order coefficientsPolW x

let rawChart = 
    Chart.Point(x_Data,y_Data)
    |> Chart.withTraceName "raw data"
    
let fittingPol = 
    let fit = [|1. .. 0.01 .. 10.|] |> Array.map (fun x -> x,fittingFunctionPol x)
    Chart.Line(fit)
    |> Chart.withTraceName "order = 3"

let fittingPolW = 
    let fit = [|1. .. 0.01 .. 10.|] |> Array.map (fun x -> x,fittingFunctionPolW x)
    Chart.Line(fit)
    |> Chart.withTraceName "order = 3 weigthed"

[rawChart;fittingPol;fittingPolW] 
|> Chart.Combine
|> Chart.Show

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

For solving a nonlinear problem the model function has to be converted to a `NonLinearRegression.Model` type consisting of the parameter names, the function itself, and partial derivatives of all unknown parameters.
For clarification a exponential relationship in the form of `y = a * exp(b * x)` should be solved:

*)
open System
open FSharp.Stats.Fitting
open FSharp.Stats.Fitting.LinearRegression
open FSharp.Stats.Fitting.NonLinearRegression


let x_Data = [|1.;2.; 3.; 4.|]
let y_Data = [|5.;14.;65.;100.|]

//search for:  y = a * exp(b * x)

// 1. create the model
// 1.1 define parameter names
let parameterNames = [|"a";"b"|]

// 1.2 define the exponential function that gets a parameter vector containing the searched parameters and the x_value and gives the corresponding y_value
let getFunctionValue =                 
    fun (parameterVector: Vector<float>) x -> parameterVector.[0] * Math.Exp(parameterVector.[1] * x)
                                            //a                   *      exp(b                   * x)

// 1.3 Define partial derivatives of the exponential function. Take partial derivatives for every unknown parameter 
//     and insert it into the gradient vector sorted by parameterNames.
let getGradientValues =
    fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue -> 
        // partial derivative of y=a*exp(b*x) in respect to the first parameter (a)   --> exp(b*x)
        gradientVector.[0] <- Math.Exp(parameterVector.[1] * xValue)  
        // partial derivative of y=a*exp(b*x) in respect to the second parameter (b)  --> a*x*exp(b*x)
        gradientVector.[1] <- parameterVector.[0] * xValue * Math.Exp(parameterVector.[1] * xValue)  

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
//     For many problems you can set a default value or let the user decide to choose an appropriate guess.
//     In the case of an exponential or log model you can use the solution of the linearized problem as a first guess.
let initialParamGuess (x_data:float []) (y_data:float [])=
    //gets the linear representation of the problem and solves it by simple linear regression (prone to least-squares-deviations at high y_Values)
    let y_ln = y_data |> Array.map (fun x -> Math.Log(x)) |> vector
    let linearReg = LinearRegression.OrdinaryLeastSquares.Linear.Univariable.coefficient (vector x_data) y_ln
    //calculates the parameters back into the exponential representation
    let a = exp linearReg.[0]
    let b = linearReg.[1]
    [|a;b|]

// 2.5 create the solverOptions
let solverOptions = 
    let guess = initialParamGuess x_Data y_Data
    NonLinearRegression.createSolverOption 0.0001 0.0001 1000 guess


// 3. get coefficients
let coefficientsExp = GaussNewton.estimatedParams model solverOptions x_Data y_Data
//val coefficients = vector [|5.68867298; 0.7263428835|]


// 4. create fitting function
let fittingFunction x = coefficientsExp.[0] * Math.Exp(coefficientsExp.[1] * x)

(**

##Smoothing spline

A smoothing spline aims to minimize a function with two error terms: 
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
(*** do-not-eval***)
open FSharp.Stats.Fitting

let x_Data = [|1.;2.; 3.; 4.|]
let y_Data = [|5.;14.;65.;100.|]

let data = Array.zip x_Data y_Data

//in every x position a knot should be located
let knots = x_Data

let spline lambda x = Spline.smoothingSpline data knots

let fit lambda = 
    [|1. .. 0.1 .. 4.|]
    |> Array.map (fun x -> x,spline lambda x)
    |> Chart.Line
    |> Chart.withTraceName (sprintf "lambda: %.2f" lambda)

let rawChart = Chart.Point(data)

[
rawChart
fit 0.001
fit 0.1
fit 10.
]
|> Chart.Combine
|> Chart.Show

