(*** hide ***)

(*** condition: prepare ***)
#r "../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-beta3"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta8"
#r "nuget: Plotly.NET.Interactive, 2.0.0-beta8"
#r "nuget: FSharp.Stats"
#endif // IPYNB

(**

# Fitting

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?filepath=Fitting.ipynb)

_Summary:_ this tutorial will walk through several ways of fitting data with FSharp.Stats.

### Table of contents
 - [Linear Regression](#Linear-Regression)
    - [Simple Linear Regression](#Simple-Linear-Regression)
        - [Univariable](#Univariable)
        - [Multivariable](#Multivariable)
 - [Polynomial Regression](#Polynomial-Regression)
 - [Nonlinear Regression](#Nonlinear-Regression)
 - [LevenbergMarquardtConstrained](#LevenbergMarquardtConstrained)
 - [Smoothing spline](#Smoothing-spline)

## Linear Regression

In Linear Regression a linear system of equations is generated. The coefficients obtained by the solution to this equation 
system minimize the squared distances from the regression curve to the data points. These distances are also known as residuals (or least squares).

### Simple Linear Regression

Simple linear regression aims to fit a straight regression line to the data. While the least squares approach efficiently minimizes the sum of squared residuals it is prone to outliers. 
An alternative is a robust simple linear regression like Theil's incomplete method or the Theil-Sen estimator, that are outlier resistant.

#### Univariable

*)

open FSharp.Stats
open FSharp.Stats.Fitting.LinearRegression

let xData = vector [|1. .. 10.|]
let yData = vector [|4.;7.;9.;12.;15.;17.;16.;23.;5.;30.|]

//Least squares simple linear regression
let coefficientsLinearLS = 
    OrdinaryLeastSquares.Linear.Univariable.coefficient xData yData
let fittingFunctionLinearLS x = 
    OrdinaryLeastSquares.Linear.Univariable.fit coefficientsLinearLS x

//Robust simple linear regression
let coefficientsLinearRobust = 
    RobustRegression.Linear.theilSenEstimator xData yData 
let fittingFunctionLinearRobust x = 
    RobustRegression.Linear.fit coefficientsLinearRobust x

//least squares simple linear regression through the origin
let coefficientsLinearRTO = 
    OrdinaryLeastSquares.Linear.RTO.coefficientOfVector xData yData 
let fittingFunctionLinearRTO x = 
    OrdinaryLeastSquares.Linear.RTO.fit coefficientsLinearRTO x

open Plotly.NET

//some axis styling
let myAxis title = Axis.LinearAxis.init(Title=title,Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=false)
let styleChart xt yt c = c |> Chart.withX_Axis (myAxis xt) |> Chart.withY_Axis (myAxis yt)


let rawChart = 
    Chart.Point(xData,yData)
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
    |> styleChart "" ""

(*** condition: ipynb ***)
#if IPYNB
simpleLinearChart
#endif // IPYNB

(***hide***)
simpleLinearChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
#### Multivariable
*)

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
    |> Matrix.ofJaggedSeq

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

### Polynomial Regression

In polynomial regression a higher degree (d > 1) polynomial is fitted to the data. The coefficients are chosen that the sum of squared residuals is minimized.

*)

open FSharp.Stats
open FSharp.Stats.Fitting.LinearRegression

let xDataP = vector [|1. .. 10.|]
let yDataP = vector [|4.;7.;9.;8.;6.;3.;2.;5.;6.;8.;|]

//Least squares polynomial regression

//define the order the polynomial should have (order 3: f(x) = ax^3 + bx^2 + cx + d)
let order = 3
let coefficientsPol = 
    OrdinaryLeastSquares.Polynomial.coefficient order xDataP yDataP 
let fittingFunctionPol x = 
    OrdinaryLeastSquares.Polynomial.fit order coefficientsPol x

//weighted least squares polynomial regression
//If heteroscedasticity is assumed or the impact of single datapoints should be 
//increased/decreased you can use a weighted version of the polynomial regression.

//define the order the polynomial should have (order 3: f(x) = ax^3 + bx^2 + cx + d)
let orderP = 3

//define the weighting vector
let weights = yDataP |> Vector.map (fun y -> 1. / y)
let coefficientsPolW = 
    OrdinaryLeastSquares.Polynomial.coefficientsWithWeighting orderP weights xDataP yDataP 
let fittingFunctionPolW x = 
    OrdinaryLeastSquares.Polynomial.fit orderP coefficientsPolW x

let rawChartP = 
    Chart.Point(xDataP,yDataP)
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
    |> styleChart "" ""

(*** condition: ipynb ***)
#if IPYNB
polRegressionChart
#endif // IPYNB

(***hide***)
polRegressionChart |> GenericChart.toChartHTML
(***include-it-raw***)


(**
## Nonlinear Regression

Nonlinear Regression is used if a known model should be fitted to the data that cannot be represented in a linear system of equations. 
Common examples are: 

 - gaussian functions

 - log functions

 - exponential functions

To fit such models to your data the `NonLinearRegression` module can be used. Three solver-methods are available to iteratively converge to a minimal least squares value.

 - GaussNewton

 - LevenbergMarquardt

 - LevenbergMarquardtConstrained

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

let xDataN = [|1.;2.; 3.; 4.|]
let yDataN = [|5.;14.;65.;100.|]

//search for:  y = a * exp(b * x)

// 1. create the model
// 1.1 define parameter names
let parameterNames = [|"a";"b"|]

// 1.2 define the exponential function that gets a parameter vector containing the 
//searched parameters and the x value and gives the corresponding y value
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
// 2.1 define the stepwidth of the x value change
let deltaX = 0.0001

// 2.2 define the stepwidth of the parameter change
let deltaP = 0.0001

// 2.3 define the number of iterations
let k = 1000

// 2.4 define an initial guess
//     For many problems you can set a default value or let the user decide to choose an 
//     appropriate guess. In the case of an exponential or log model you can use the 
//     solution of the linearized problem as a first guess.
let initialParamGuess (xData:float []) (yData:float [])=
    //gets the linear representation of the problem and solves it by simple linear regression 
    //(prone to least-squares-deviations at high y_Values)
    let yLn = yData |> Array.map (fun x -> Math.Log(x)) |> vector
    let linearReg = 
        LinearRegression.OrdinaryLeastSquares.Linear.Univariable.coefficient (vector xData) yLn
    //calculates the parameters back into the exponential representation
    let a = exp linearReg.[0]
    let b = linearReg.[1]
    [|a;b|]

// 2.5 create the solverOptions
let solverOptions = 
    let guess = initialParamGuess xDataN yDataN
    NonLinearRegression.createSolverOption 0.0001 0.0001 1000 guess

// 3. get coefficients
let coefficientsExp = GaussNewton.estimatedParams model solverOptions xDataN yDataN
//val coefficients = vector [|5.68867298; 0.7263428835|]

// 4. create fitting function
let fittingFunction x = coefficientsExp.[0] * Math.Exp(coefficientsExp.[1] * x)

let rawChartNLR = 
    Chart.Point(xDataN,yDataN)
    |> Chart.withTraceName "raw data"

let fittingNLR = 
    let fit = 
        [|1. .. 0.1 .. 10.|] 
        |> Array.map (fun x -> x,fittingFunction x)
    Chart.Line(fit)
    |> Chart.withTraceName "NLR"

let NLRChart =
    [rawChartNLR;fittingNLR] 
    |> Chart.Combine
    |> styleChart "" ""

(*** condition: ipynb ***)
#if IPYNB
NLRChart
#endif // IPYNB

(***hide***)
NLRChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**
### LevenbergMarquardtConstrained

For nonlinear regression using the LevenbergMarquardtConstrained module, you have to follow similar steps as in the example shown above.
In this example, a logistic function of the form `y = L/(1+e^(-k(t-x)))` should be fitted to count data:

*)
open FSharp.Stats.Fitting.NonLinearRegression

let xHours = [|0.; 19.5; 25.5; 30.; 43.; 48.5; 67.75|]

let yCount = [|0.0; 2510000.0; 4926400.0; 9802600.0; 14949400.0; 15598800.0; 16382000.0|]

// 1. Choose a model
// The model we need already exists in FSharp.Stats and can be taken from the "Table" module.
let model' = Table.LogisticFunctionAscending

// 2. Define the solver options
// 2.1 Initial parameter guess
// The solver needs an initial parameter guess. This can be done by the user or with an estimator function.
// The cutoffPercentage says at which percentage of the y-Range the lower part of the slope is. 
// Manual curation of parameter guesses can be performed in this step by editing the param array.
let initialParamGuess' = LevenbergMarquardtConstrained.initialParam xHours yCount 0.1

// 2.2 Create the solver options
let solverOptions' = Table.lineSolverOptions initialParamGuess'

// 3. Estimate parameters for a possible solution based on residual sum of squares
// Besides the solverOptions, an upper and lower bound for the parameters are required.
// It is recommended to define them depending on the initial param guess
let lowerBound =
    initialParamGuess'
    |> Array.map (fun param ->
        // Initial paramters -20%
        param - (abs param) * 0.2
    )
    |> vector

let upperBound =
    initialParamGuess'
    |> Array.map (fun param ->
        param + (abs param) * 0.2
    )
    |> vector

let estParams =
    LevenbergMarquardtConstrained.estimatedParams model' solverOptions' 0.001 10. lowerBound upperBound xHours yCount

// 3.1 Estimate multiple parameters and pick the one with the least residual sum of squares (RSS)
// For a more "global" minimum. it is possible to estimate multiple possible parameters for different initial guesses.

//3.1.1 Generation of parameters with varying steepnesses
let multipleSolverOptions =
    LevenbergMarquardtConstrained.initialParamsOverRange xHours yCount [|0. .. 0.1 .. 2.|]
    |> Array.map Table.lineSolverOptions

let estParamsRSS =
    multipleSolverOptions
    |> Array.map (fun solvO ->
        let lowerBound =
            solvO.InitialParamGuess
            |> Array.map (fun param -> param - (abs param) * 0.2)
            |> vector
        let upperBound =
            solvO.InitialParamGuess
            |> Array.map (fun param -> param + (abs param) * 0.2)
            |> vector
        LevenbergMarquardtConstrained.estimatedParamsWithRSS 
            model' solvO 0.001 10. lowerBound upperBound xHours yCount
    )
    |> Array.minBy snd
    |> fst

// The result is the same as for 'estParams', but tupled with the corresponding RSS, which can be taken
// as a measure of quality for the estimate.

// 4. Create fitting function
let fittingFunction' = Table.LogisticFunctionAscending.GetFunctionValue estParamsRSS

let fittedY = Array.zip [|1. .. 68.|] ([|1. .. 68.|] |> Array.map fittingFunction')

let fittedLogisticFunc =
    [
    Chart.Point (Array.zip xHours yCount)
    |> Chart.withTraceName"Data Points"
    Chart.Line fittedY
    |> Chart.withTraceName "Fit"
    ]
    |> Chart.Combine
    |> styleChart "Time" "Count"

(*** condition: ipynb ***)
#if IPYNB
fittedLogisticFunc
#endif // IPYNB

(***hide***)
fittedLogisticFunc |> GenericChart.toChartHTML
(***include-it-raw***)

(**

## Smoothing spline

A smoothing spline aims to minimize a function consisting of two error terms: 

 - error1: sum of squared residuals

    - Similar to the OrdinaryLeastSquares regression this error term ensures the fidelity to the data.

 - error2: integral of the second derivative of the fitting function

    - This error term ensures the smoothness of the resulting curve.

A smoothing parameter (lambda) mediates between the two error terms.

 - E = error1 + (lambda * error2)

    - If lambda = 0, the resulting curve minimizes the sum of squared residuals and results in an interpolating curve.

    - If lambda = infinity, the resulting curve is punished by the smoothness measurement and results in a straight regression line.

The spline is constructed out of piecewise cubic polynomials that meet at knots. In the defined knots the function is continuous. 
Depending on the used smoothing factor and the defined knots the smoothing spline has a unique solution. The resulting curve is just defined within the interval defined in the x values of the data.

The right amount of smoothing can be determined by cross validation or generalized cross validation.
*)

open FSharp.Stats.Fitting

let xDataS = [|1.;2.; 3.; 4.|]
let yDataS = [|5.;14.;65.;75.|]

let data = Array.zip xDataS yDataS

//in every x position a knot should be located
let knots = xDataS

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
    |> styleChart "" ""

(*** condition: ipynb ***)
#if IPYNB
smoothingSplines
#endif // IPYNB

(***hide***)
smoothingSplines |> GenericChart.toChartHTML
(***include-it-raw***)





(*** hide ***)


//// Test versus http://www.cyclismo.org/tutorial/R/linearLeastSquares.html
//let xVector =
//    vector [2000.;   2001.;  2002.;  2003.;   2004.;]

//let xVectorM =
//    vector [2000.;   2001.;  2002.;  2003.;   2004.;]
//    |> Matrix.ofVector
//let yVector = vector [9.34;   8.50;  7.62;  6.93;  6.60;]




//let coeffM   = Regression.OrdinaryLeastSquares.Linear.MultiVariateCoefficient xVectorM yVector

//let coeff   = Regression.OrdinaryLeastSquares.Linear.coefficient xVector yVector


//let fit     = Regression.OrdinaryLeastSquares.Linear.fit coeff
//let fitM     = Regression.OrdinaryLeastSquares.Linear.fit' coeffM

//fit 2002.
//fitM (vector [2002.])    

//let regLine = xVector |> Vector.map fit

//let xVectorMulti =
//    [
//    [1.; 1. ;2.  ]
//    [2.; 0.5;6.  ]
//    [3.; 0.8;10. ]
//    [4.; 2. ;14. ]
//    [5.; 4. ;18. ]
//    [6.; 3. ;22. ]
//    ]
//    |> Matrix.ofSeq

//let f (x:Matrix<float>) =
//    x
//    |> Matrix.mapiRows (fun i v ->
//        //let v = v.Transpose
//        100. + (v.[0] * 2.5) + (v.[1] * 4.) + (v.[2] * 0.5))

//let y = vector <| f xVectorMulti

//let coeffM2   = Regression.OrdinaryLeastSquares.Linear.MultiVariateCoefficient xVectorMulti y

//let fitM2     = Regression.OrdinaryLeastSquares.Linear.fit' coeffM2

//fitM2 (vector [3.; 0.8;10. ]) 

//let MultiVariateCoefficient (xData : Matrix<float>) (yData : Vector<float>) =
//    if xData.NumRows <> yData.Length then
//        raise (System.ArgumentException("vector x and y have to be the same size!"))
//    let m = xData.NumRows
//    let n = xData.NumCols
//    let X = Matrix.init m (n+1) (fun m n ->  if n = 0 then 1. else xData.[m,n-1] )
//    //Algebra.LinearAlgebra.LeastSquares X yData
//    X
//let X = MultiVariateCoefficient xVectorMulti y
//Algebra.LinearAlgebra.LeastSquares X y

//let summary = Regression.calulcateSumOfSquares fit xVector yVector

//let rsquared = Regression.calulcateDetermination summary

//let sigIntercept = Regression.ttestIntercept coeff.[0] summary
//let sigSlope     = Regression.ttestSlope coeff.[1] summary


//let anova = Regression.Linear.calculateANOVA coeff xVector yVector


//let aic = Regression.calcAIC 2. summary.Count summary.Error
//let bic = Regression.calcBIC 2. summary.Count summary.Error

//Regression.getResiduals fit xVector yVector
//Regression.calculateSSE fit xVector yVector

//(*** define:regression1 ***)
//[
//    Chart.Point(Seq.zip xVector yVector,Name="data points");
//    Chart.Line(Seq.zip xVector regLine,Name ="regression")
//]
//|> Chart.Combine
//(***hide***)
//regression1 |> GenericChart.toChartHTML
//(***include-it-raw***)







//let xVector' = vector [1290.;1350.;1470.;1600.;1710.;1840.;1980.;2230.;2400.;2930.;]
//let yVector' = vector [1182.;1172.;1264.;1493.;1571.;1711.;1804.;1840.;1956.;1954.;]


//let coeff'   = Regression.Polynomial.coefficient 2 xVector' yVector'

//let fit'     = Regression.Polynomial.fit 2 coeff'
//let regLine' = vector xVector' |> Vector.map fit'


//Regression.Polynomial.calculateANOVA 2 coeff' xVector' yVector'

//(*** define:polynomial1 ***)
//[
//    Chart.Point(Seq.zip xVector' yVector',Name="data points");
//    Chart.Spline(Seq.zip xVector' regLine',Name ="regression")
//]
//|> Chart.Combine
//(***hide***)
//polynomial1 |> GenericChart.toChartHTML
//(***include-it-raw***)





//(**
//Hermite Spline Regression
//-----------------
//*)




//let x = vector [0.0;1.0;2.0;3.0;4.5;5.1;7.6;] 
//let y = vector [2.2;5.5;7.7;9.9;11.1;12.3;13.9]

//// Weigth matrix with equal weights
//let W = Matrix.diag (Vector.ones y.Length)

//let a,e,b,c = Hermite.splineIncreasing x y W 10.0

//let t = [0.0 .. 0.01 .. 7.5]

//let eval = Hermite.initEvalAt x a c

//(*** define:hermitespline1 ***)
//[
//    Chart.Point(x,y)
//    Chart.Point(t,t |> Seq.map eval)
//]
//|> Chart.Combine
//(***hide***)
//hermitespline1 |> GenericChart.toChartHTML
//(***include-it-raw***)


//let m1 = vector [|14.69707479;49.40105967;4.63026942;7.728633952;4.077401498;3.847039793;3.294171442;4.837594998;0.345542383;7.141212053|]
//let m2 = vector [|1.156069364;10.69364162;9.248554913;2.312138728;13.00578035;9.826589595;12.42774566;7.225433526;1.156069364;32.94797688|]
//let m3 = vector [|0.139580499;5.089121731;3.153427595;4.343756039;5.899058788;6.242208666;7.593415919;5.831556204;1.551657515;60.15621704|]

//let t1 = vector [|0.129230005;8.82273372;6.149632627;7.616209201;9.226621001;9.247565753;9.274231037;7.417205731;2.153290552;39.96328037|]
//let t2 = vector [|1.020411515;18.39983282;11.69962593;11.3877188;8.877225494;6.802075352;6.42866738;6.570529817;2.237481955;26.57643094|]
//let t3 = vector [|1.514770998;15.24095695;7.727525997;9.372988152;9.944448026;8.774023957;7.817375851;7.23063126;2.16513451;30.2121443|]


//let N = m1.Length
//let oneVec = Vector.create N 1. //:> Vector<float>
//let X = Matrix.ofSeq  [ m1; m2; m3] 

//let r1 = Regression.Linear.coefficientOfMatrix (X.Transpose) t1 // Glucose
//let r2 = Regression.Linear.coefficientOfMatrix (X.Transpose) t2 // Fructose
//let r3 = Regression.Linear.coefficientOfMatrix (X.Transpose) t3 // Sucrose


//open FSharp.Stats
//let bwm1 = 1.//Distributions.Bandwidth.nrd0 m1.Values
//let km1 = Distributions.KernelDensity.estimate Distributions.KernelDensity.Kernel.biweight bwm1 m1.Values

//let bwm2 = 1.//Distributions.Bandwidth.nrd0 m2.Values
//let km2 = Distributions.KernelDensity.estimate Distributions.KernelDensity.Kernel.biweight bwm2 m2.Values

//let bwm3 = 1.//Distributions.Bandwidth.nrd0 m3.Values
//let km3 = Distributions.KernelDensity.estimate Distributions.KernelDensity.Kernel.biweight bwm3 m3.Values

////[
////Chart.Point ([1..10], m1 ,Name = "Chlorophyll")
////Chart.Point ([1..10], m2,Name = "UGPase")
////Chart.Point ([1..10], m3,Name = "Nitrat")
////]
////|> Chart.Combine
////|> Chart.Show



//[
//Chart.Point (km1 ,Name = "Chlorophyll")
//Chart.Point (km2,Name = "UGPase")
//Chart.Point (km3,Name = "Nitrat")
//]
//|> Chart.Combine
////|> Chart.Show










