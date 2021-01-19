(*** hide ***)

(*** condition: prepare ***)
#r "../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-beta3"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-beta3"
#r "nuget: Plotly.NET.Interactive, 2.0.0-beta3"
#r "nuget: FSharp.Stats"
#endif // IPYNB

(**
# Cross validation

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?filepath=CrossValidation.ipynb)

_Summary:_ this tutorial demonstrates how to perform several types of cross validtion with FSharp.Stats.

### Table of contents

 - [Leave-one-out cross validation (LOOCV)](#Leave-one-out-cross-validation-LOOCV)
    - [Procedure](#Procedure)
    - [Polynomial loocv](#Polynomial-loocv)
    - [Smoothing spline loocv](#Smoothing-spline-loocv)
 - [k fold cross validation](#k-fold-cross-validation)
 - [Shuffle and split cross validation](#Shuffle-and-split-cross-validation)

## Leave-one-out cross validation (LOOCV)

When fitting a data set it often comes down to the selection of the optimal fitting parameter(s).
A method to determine these is given by the leave-one-out cross validation (LOOCV). Thereby, the data set ist fitted with a
given parameter range (smoothing strength, polynomial order etc.) in order to select the best. 

### Procedure
In each iteration, one data point is excluded from the fitting procedure. The coefficients are determined 
based on the remaining (n-1) data points. The difference of the excluded point with its corresponding fitted point is measured. 
In a two-dimensional problem it is the y-intercept of f(xi) and the y_orig at xi.

After every data point was excluded once, the average (squared) distance is calculated and assigned to the corresponding fitting parameter (polynomial order or smoothing strength).
The parameter of the model that shows the minimal average error is the best under the given assumptions. It shows the best compromise between over- and underfitting respectively.

### Polynomial loocv

let's first create some polynomial fits to cross validate:
*)
open FSharp.Stats
open FSharp.Stats.Fitting
open LinearRegression.OrdinaryLeastSquares

let xV = vector [1. .. 10.]                            
let yV = vector [1.;20.;51.;40.;37.;6.;-10.;-5.;0.;10.]

// the fitting function fits a polynomial of order 'order' to the training data set (xTrain and yTrain) and applies it to xTest
let getFitFuncPolynomial xTrain yTrain (xTest:RowVector<float>) order = 
    let xDat = xTrain |> Matrix.toVector
    let coeffs  = Polynomial.coefficient order xDat yTrain
    let fit     = Polynomial.fit order coeffs (xTest.[0])
    fit

open Plotly.NET

//Some axis styling
let myAxis title = Axis.LinearAxis.init(Title=title,Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=false)
let myLogAxis title = Axis.LinearAxis.init(StyleParam.AxisType.Log,Title=title,Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=false)
let styleChart x y chart = chart |> Chart.withX_Axis (myAxis x) |> Chart.withY_Axis (myAxis y)

let rawchart() = 
    Chart.Point (xV,yV) 
    |> Chart.withTraceName "raw data"

let chartOrderOpt = 
    [1 .. 2 .. 10]
    |> List.map (fun order -> 
        let coeffs = Polynomial.coefficient order xV yV
        let fit = Polynomial.fit order coeffs
        [1. .. 0.2 .. 10.]
        |> List.map (fun x -> x,fit x)
        |> Chart.Line
        |> Chart.withTraceName (sprintf "order=%i" order)
        )
    |> fun x -> Chart.Combine (rawchart()::x)
    |> Chart.withTitle "polynomial fits"
    |> Chart.withX_Axis (myAxis "x")
    |> Chart.withY_Axis (myAxis "y")

(*** condition: ipynb ***)
#if IPYNB
chartOrderOpt
#endif // IPYNB

(***hide***)
chartOrderOpt |> GenericChart.toChartHTML 
(***include-it-raw***)

(**
And then crossvalidate across the polynomial orders:
*)

// the error is calculated as the squared difference of fitted and original y value
let error (f1:float) f2 = pown (f1 - f2) 2

/// Leave-one-out cross validation. Returns the mean squared error of each leave-out at the 
/// specific polynomial order. Minimize for model selection.
let loocvPolynomial (xData:Vector<float>) (yData:Vector<float>) order =
    let xDataMat = Matrix.ofVector xData
    let getFitFuncPol xTrain yTrain (xTest:RowVector<float>) = 
        getFitFuncPolynomial xTrain yTrain xTest order
    let meanSquaredError = CrossValidation.loocv xDataMat yData getFitFuncPol error
    
    meanSquaredError

// polynomial orders that should be checked
let ordersToCheck = [|1 .. 10|]

let errorPol = 
    ordersToCheck 
    |> Array.map (fun order -> 
        let error = loocvPolynomial xV yV order
        order,error)

let chartPol = 
    errorPol 
    |> Chart.Line 
    |> Chart.withX_Axis (myAxis "polynomial order") 
    |> Chart.withY_Axis (myLogAxis "mean error" )
    |> Chart.withTitle "leave one out cross validation (polynomial)"
    
let result = sprintf "The minimal error is obtained by order=%i" (errorPol |> Seq.minBy snd |> fst)

(*** condition: ipynb ***)
#if IPYNB
chartPol
#endif // IPYNB

(***hide***)
chartPol |> GenericChart.toChartHTML
(***include-it-raw***)
(*** include-value:result ***)

(**
### Smoothing spline loocv
A smoothing spline is a non-parametric fitting procedure, fitting cubic polynomials in each interval given by the basis points.

let's first create some smoothing splines to cross validate:

*)

// the fitting function fits a polynomial of order 'order' to the training data set (xTrain and yTrain) and applies it to xTest
let getFitFuncSpline xDat yDat (xDatTrain: RowVector<float>) lambda =
    let xDatVec = xDat |> Matrix.toVector
    let zippedData = Seq.zip xDatVec yDat |> Array.ofSeq
    let xValTest = xDatTrain.[0]
    Spline.smoothingSpline zippedData (xDat |> Array.ofSeq) lambda xValTest

    /// in loocv the border points are chosen so that the support range of the training data set does not cover the test point.
    /// if splines are used, that are not defined outside the border points use the following:
    //let xDatSupport = Intervals.create (xDatVec |> Seq.min) (xDatVec |> Seq.max)
    //if Intervals.liesInInterval xValTest xDatSupport then 
    //Spline.smoothingSpline zippedData (xDat |> Array.ofSeq) lambda xValTest
    //else nan

let chartSpline = 
    [0.0002;0.002;0.0216;0.2;2.;20.]
    |> List.map (fun lambda -> 
        let fit = Spline.smoothingSpline (Seq.zip xV yV |> Array.ofSeq) (Array.ofSeq xV) lambda
        [1. .. 0.2 .. 10.]
        |> List.map (fun x -> x,fit x)
        |> Chart.Line
        |> Chart.withTraceName (sprintf "l=%.4f" lambda)
        )
    |> fun x -> 
        Chart.Combine (rawchart()::x)
    |> Chart.withX_Axis (myAxis "x") 
    |> Chart.withY_Axis (myAxis "y")
    |> Chart.withTitle "smoothing splines"

(*** condition: ipynb ***)
#if IPYNB
chartSpline
#endif // IPYNB

(***hide***)
chartSpline |> GenericChart.toChartHTML
(***include-it-raw***)

(**
And then crossvalidate across different lambda values:
*)

// the error is calculated as the squared difference of fitted and original y value
let errorSpl (f1:float) f2 = 
    /// if xValue is outside of support area of the fitted model (some smoothing spline algorithms), the error should report 0.
    //if nan.Equals f1 then 0.
    //else pown (f1 - f2) 2
    pown (f1 - f2) 2

/// Leave-one-out cross validation. Returns the mean squared error of each leave-out at the 
/// specific regularization parameter (lambda). Minimize the (MSE) for model selection.
let loocvSmoothingSpline (xData:Vector<float>) (yData:Vector<float>) lambda =
    let xDataMat = Matrix.ofVector xData
    let getFitFuncSpl xDat yDat (xDatTrain: RowVector<float>) =
        getFitFuncSpline xDat yDat xDatTrain lambda
    
    CrossValidation.loocv xDataMat yData getFitFuncSpl errorSpl

// smoothing parameter = lambda = regularization parameter
let lambdasToCheck = [|1. .. 15.|] |> Array.map (fun i -> 0.0001 * i**3.)

let errorSpline = 
    lambdasToCheck 
    |> Array.map (fun lambda -> 
        //basisPoints define, where the knots of the spline are located
        let error = loocvSmoothingSpline xV yV lambda
        lambda,error)

let chartSplineError = 
    errorSpline 
    |> Chart.Line 
    |> Chart.withX_Axis (myAxis "lambda") 
    |> Chart.withY_Axis (myAxis "mean error")
    |> Chart.withTitle "leave one out cross validation (smoothing spline)"
    
let resultSpline = sprintf "The minimal error is obtained by lambda=%f" (errorSpline |> Seq.minBy snd |> fst)

(*** condition: ipynb ***)
#if IPYNB
chartSplineError
#endif // IPYNB

(***hide***)
chartSplineError |> GenericChart.toChartHTML
(***include-it-raw***)
(*** include-value:resultSpline ***)

(**
## k fold cross validation

The k fold cross validation (kfcv) is a generalized form of the loocv. Rather than excluding every data point seperately, kfcv
allows the exclusion of data chunks with a defined fraction of the data points. When using k=10, the data ist split up into 10 chunks of sub data sets each 
containing 10% of the data set. 

In each loop one chunk is excluded (test data), while the other 9 chunks serve as training data. After 10 (k) loops every single point was evaluated as test data set once
and k-1 times as training data. The selection of the subset chunks is random and can be repeated in several iterations.
The output contains the average error together with the standardDeviation computed by the given function.
*)

//repeated k fold cross validation for polynomials
let repeatedKFoldPolynomial k (xData: Vector<float>) (yData: Vector<float>) order =
    let xDataMat = xData |> Matrix.Generic.ofVector
    
    let getFitFuncPol xTrain yTrain (xTest:RowVector<float>) = 
        getFitFuncPolynomial xTrain yTrain xTest order
        
    CrossValidation.repeatedKFold k 10 xDataMat yData getFitFuncPol error Seq.stDev

//creates an output for 10 iterations where defined 20 % of the data set are taken as testing data set
let kfPolynomial order = repeatedKFoldPolynomial 5 xV yV order

//repeated k fold cross validation for smoothing splines
let repeatedKFoldSpline k (xData: Vector<float>) (yData: Vector<float>) lambda =
    let xDataMat = xData |> Matrix.ofVector
    
    let getFitFuncSpl xDat yDat (xDatTrain: RowVector<float>) =
        getFitFuncSpline xDat yDat xDatTrain lambda

    CrossValidation.repeatedKFold k 10 xDataMat yData getFitFuncSpl errorSpl Seq.stDev

//creates an output for 10 iterations where defined 20 % of the data set are taken as testing data set
let kfSpline lambda = repeatedKFoldSpline 5 xV yV lambda

(**
The given data set is small and therefore the mean errors show a high variability
*)

let kfp = 
    let errorSplinekf = 
        ordersToCheck 
        |> Array.map (fun order -> 
            //basisPoints define, where the knots of the spline are located
            let error = kfPolynomial order
            (order,error.Error),error.ErrorStDev)
        |> Array.unzip

    fst errorSplinekf 
    |> Chart.Line 
    |> Chart.withX_Axis (myAxis "order") 
    |> Chart.withY_Axis (myLogAxis "mean error")
    |> Chart.withYErrorStyle (snd errorSplinekf)
    |> Chart.withTitle "kfoldPolynomial error"

(*** condition: ipynb ***)
#if IPYNB
kfp
#endif // IPYNB

(***hide***)
kfp |> GenericChart.toChartHTML
(***include-it-raw***)

let kfs = 
    let errorSplinekf = 
        lambdasToCheck 
        |> Array.map (fun lambda -> 
            //basisPoints define, where the knots of the spline are located
            let error = kfSpline lambda
            (lambda,error.Error),error.ErrorStDev)
        |> Array.unzip

    fst errorSplinekf 
    |> Chart.Line 
    |> Chart.withX_Axis (myAxis "lambda") 
    |> Chart.withY_Axis (myAxis "mean error")
    |> Chart.withYErrorStyle (snd errorSplinekf)
    |> Chart.withTitle "kfoldSpline error"

(*** condition: ipynb ***)
#if IPYNB
kfs
#endif // IPYNB

(***hide***)
kfs |> GenericChart.toChartHTML
(***include-it-raw***)

(**
## Shuffle and split cross validation

The shuffle and split cross validation (sap) is a modified kfcv version. As in kfcv, sap
allows the exclusion of data chunks with a defined fraction of the data points. When using p=0.3, 30% of the data are taken as testing data set 
while 70% serve as training data set. In sap by default only one testing set is evaluated (unlike to kfcv 
where every data point is once part of a training data set and k-1 times part of testing data set).

Sap can be performed multiple times. Each time the training data fraction is taken randomly from the original data set. Unlike in kfcv overlaps may occur.
The output contains the average error together with the standardDeviation computed by the given function.
*)

let shuffleAndSplitPolynomial p iterations (xData: Vector<float>) (yData: Vector<float>) order =
   let xDataMat = xData |> Matrix.ofVector
   
   let getFitFuncPol xTrain yTrain (xTest:RowVector<float>) = 
       getFitFuncPolynomial xTrain yTrain xTest order
   
   CrossValidation.shuffelAndSplit p iterations xDataMat yData getFitFuncPol error Seq.stDev

//creates an output for 5 iterations where random 20 % of the data set are taken as testing data set
let sasPolynomial order = shuffleAndSplitPolynomial 0.2 5 xV yV order

let shuffleAndSplitSpline p iterations (xData: Vector<float>) (yData: Vector<float>) lambda =
    let xDataMat = xData |> Matrix.ofVector
   
    let getFitFuncSpl xDat yDat (xDatTrain: RowVector<float>) =
        getFitFuncSpline xDat yDat xDatTrain lambda
   
    CrossValidation.shuffelAndSplit p iterations xDataMat yData getFitFuncSpl errorSpl Seq.stDev

//creates an output for 5 iterations where random 20 % of the data set are taken as testing data set
let sasSpline lambda = shuffleAndSplitSpline 0.2 5 xV yV lambda


(**
The given data set is small and therefore the mean errors show a high variability
*)

let sasp = 
    let errorSplinekf = 
        ordersToCheck 
        |> Array.map (fun order -> 
            //basisPoints define, where the knots of the spline are located
            let error = sasPolynomial order
            (order,error.Error),error.ErrorStDev)
        |> Array.unzip

    fst errorSplinekf 
    |> Chart.Line 
    |> Chart.withX_Axis (myAxis "order") 
    |> Chart.withY_Axis (myLogAxis "mean error")
    |> Chart.withYErrorStyle (snd errorSplinekf)
    |> Chart.withTitle "shuffle_and_split polynomial error"

(*** condition: ipynb ***)
#if IPYNB
sasp
#endif // IPYNB

(***hide***)
sasp |> GenericChart.toChartHTML
(***include-it-raw***)

let sass = 
    let errorSplinekf = 
        lambdasToCheck 
        |> Array.map (fun lambda -> 
            //basisPoints define, where the knots of the spline are located
            let error = sasSpline lambda
            (lambda,error.Error),error.ErrorStDev)
        |> Array.unzip

    fst errorSplinekf 
    |> Chart.Line 
    |> Chart.withX_Axis (myAxis "lambda") 
    |> Chart.withY_Axis (myAxis "mean error")
    |> Chart.withYErrorStyle (snd errorSplinekf)
    |> Chart.withTitle "shuffle_and_split spline error"

(*** condition: ipynb ***)
#if IPYNB
sass
#endif // IPYNB

(***hide***)
sass |> GenericChart.toChartHTML
(***include-it-raw***)
