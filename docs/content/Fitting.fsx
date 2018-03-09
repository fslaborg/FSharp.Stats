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
|> Chart.Show


let m1 = vector [|14.69707479;49.40105967;4.63026942;7.728633952;4.077401498;3.847039793;3.294171442;4.837594998;0.345542383;7.141212053|]
let m2 = vector [|1.156069364;10.69364162;9.248554913;2.312138728;13.00578035;9.826589595;12.42774566;7.225433526;1.156069364;32.94797688|]
let m3 = vector [|0.139580499;5.089121731;3.153427595;4.343756039;5.899058788;6.242208666;7.593415919;5.831556204;1.551657515;60.15621704|]

let t1 = vector [|0.129230005;8.82273372;6.149632627;7.616209201;9.226621001;9.247565753;9.274231037;7.417205731;2.153290552;39.96328037|]
let t2 = vector [|1.020411515;18.39983282;11.69962593;11.3877188;8.877225494;6.802075352;6.42866738;6.570529817;2.237481955;26.57643094|]
let t3 = vector [|1.514770998;15.24095695;7.727525997;9.372988152;9.944448026;8.774023957;7.817375851;7.23063126;2.16513451;30.2121443|]


let N = m1.Length
let oneVec = Vector.create N 1. //:> Vector<float>
let X = Matrix.ofSeq  [ m1; m2; m3] 

let r1 = Regression.Linear.coefficientOfMatrix (X.Transpose) t1 // Glucose
let r2 = Regression.Linear.coefficientOfMatrix (X.Transpose) t2 // Fructose
let r3 = Regression.Linear.coefficientOfMatrix (X.Transpose) t3 // Sucrose


open FSharp.Stats
let bwm1 = 1.//Distributions.Bandwidth.nrd0 m1.Values
let km1 = Distributions.KernelDensity.estimate Distributions.KernelDensity.Kernel.biweight bwm1 m1.Values

let bwm2 = 1.//Distributions.Bandwidth.nrd0 m2.Values
let km2 = Distributions.KernelDensity.estimate Distributions.KernelDensity.Kernel.biweight bwm2 m2.Values

let bwm3 = 1.//Distributions.Bandwidth.nrd0 m3.Values
let km3 = Distributions.KernelDensity.estimate Distributions.KernelDensity.Kernel.biweight bwm3 m3.Values

//[
//Chart.Point ([1..10], m1 ,Name = "Chlorophyll")
//Chart.Point ([1..10], m2,Name = "UGPase")
//Chart.Point ([1..10], m3,Name = "Nitrat")
//]
//|> Chart.Combine
//|> Chart.Show



[
Chart.Point (km1 ,Name = "Chlorophyll")
Chart.Point (km2,Name = "UGPase")
Chart.Point (km3,Name = "Nitrat")
]
|> Chart.Combine
|> Chart.Show










