(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "../../packages/build/FSharp.Plotly/lib/net45/Fsharp.Plotly.dll"
open FSharp.Plotly
(**


#Fitting

<a name="Linear"></a>

##Linear Regression

<a name="Polynomial"></a>

##Polynomial Regression

<a name="Hermite"></a>

##Hermite Spline Regression





*)
#r "FSharp.Stats.dll"
open FSharp.Stats
open FSharp.Stats.Fitting

(**
Linear Regression
-----------------
*)




// Test versus http://www.cyclismo.org/tutorial/R/linearLeastSquares.html
let xVector = vector [2000.;   2001.;  2002.;  2003.;   2004.;]
let yVector = vector [9.34;   8.50;  7.62;  6.93;  6.60;]

let coeff   = Regression.Linear.coefficient xVector yVector
let fit     = Regression.Linear.fit coeff
let regLine = xVector |> Vector.map fit



let summary = Regression.calulcateSumOfSquares fit xVector yVector

let rsquared = Regression.calulcateDetermination summary

let sigIntercept = Regression.ttestIntercept coeff.[0] summary
let sigSlope     = Regression.ttestSlope coeff.[1] summary


let anova = Regression.Linear.calculateANOVA coeff xVector yVector


let aic = Regression.calcAIC 2. summary.Count summary.Error
let bic = Regression.calcBIC 2. summary.Count summary.Error

Regression.getResiduals fit xVector yVector
Regression.calculateSSE fit xVector yVector

(*** define-output:regression1 ***)
[
    Chart.Point(Seq.zip xVector yVector,Name="data points");
    Chart.Line(Seq.zip xVector regLine,Name ="regression")
]
|> Chart.Combine
(*** include-it:regression1 ***)







let xVector' = vector [1290.;1350.;1470.;1600.;1710.;1840.;1980.;2230.;2400.;2930.;]
let yVector' = vector [1182.;1172.;1264.;1493.;1571.;1711.;1804.;1840.;1956.;1954.;]


let coeff'   = Regression.Polynomial.coefficient 2 xVector' yVector'

let fit'     = Regression.Polynomial.fit 2 coeff'
let regLine' = vector xVector' |> Vector.map fit'


Regression.Polynomial.calculateANOVA 2 coeff' xVector' yVector'

(*** define-output:polynomial1 ***)
[
    Chart.Point(Seq.zip xVector' yVector',Name="data points");
    Chart.Spline(Seq.zip xVector' regLine',Name ="regression")
]
|> Chart.Combine
(*** include-it:polynomial1 ***)




(**
Hermite Spline Regression
-----------------
*)




let x = vector [0.0;1.0;2.0;3.0;4.5;5.1;7.6;] 
let y = vector [2.2;5.5;7.7;9.9;11.1;12.3;13.9]

// Weigth matrix with equal weights
let W = Matrix.diag (Vector.ones y.Length)

let a,e,b,c = Hermite.splineIncreasing x y W 10.0

let t = [0.0 .. 0.01 .. 7.5]

let eval = Hermite.initEvalAt x a c

(*** define-output:hermitespline1 ***)
[
    Chart.Point(x,y)
    Chart.Point(t,t |> Seq.map eval)
]
|> Chart.Combine
(*** include-it:hermitespline1 ***)









