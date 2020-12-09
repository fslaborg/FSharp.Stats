(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "netstandard.dll"
#I "../../src/FSharp.Stats/bin/Release/net461"
#r "../../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r @"../../lib/Formatting/FSharp.Plotly.dll"
open FSharp.Plotly
open FSharp.Plotly.Axis
open FSharp.Plotly.StyleParam
let myAxis title = LinearAxis.init(Title=title,Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=false)


(**

#Goodness of fit

##Linear regression report

*)
open FSharp.Stats
open FSharp.Stats.Fitting
open LinearRegression.OrdinaryLeastSquares
open GoodnessOfFit.OrdinaryLeastSquares.Linear.Univariable

//data sorted by x values
let x = vector [|1. .. 10.|]
let y = vector [|4.;10.;9.;7.;13.;17.;16.;23.;15.;30.|]

///linear regression line fitting function
let coefficients = Linear.Univariable.coefficient x y
let fitFunc = Linear.Univariable.fit coefficients

let fittedValues = x |> Seq.map fitFunc


//In the following some quality/interval/significance values are computed:
let sos         = GoodnessOfFit.calculateSumOfSquares fitFunc x y
let n           = sos.Count
let meanX       = sos.MeanX
let meanY       = sos.MeanY
let slope       = coefficients.[1]
let intercept   = coefficients.[0]
//coefficient of determination
let rSq        = GoodnessOfFit.calculateDeterminationFromValue y fittedValues
//adjusted coefficient of determination; variable=number of coefficints (excluding intercept)
let rSqAdj    = GoodnessOfFit.calculateDeterminationAdj y fittedValues 1
//pearson correlation coefficient
let r           = sqrt rSq
//total sum of squares
let ssTotal     = sos.Total
//regression sum of squares
let ssReg       = sos.Regression
//residual sum of squares
let ssResidual  = sos.Error
//sum of squares xx
let ssxx        = sos.SSxx
//sum of products xy
let ssxy        = sos.SSxy
//standard error of the regression slope
let stdErrSlope = GoodnessOfFit.standardErrorSlope sos 
//standard error of the regression intercept
let stdErrIntercept = GoodnessOfFit.standardErrorIntercept sos 
//standard error of the estimate (S)
let stdErrEstimate  = GoodnessOfFit.standardErrorEstimate sos 
//confidence intervals (df = n-#coefficients; a=5%)
let criticalT   = Distributions.Continuous.getCriticalTValue (n - 2.) 0.05 Distributions.Continuous.TwoTailed
let lowerS      = slope - criticalT * stdErrSlope
let upperS      = slope + criticalT * stdErrSlope
let lowerI      = intercept - criticalT * stdErrIntercept
let upperI      = intercept + criticalT * stdErrIntercept
//significance tests
let testSlope   = GoodnessOfFit.ttestSlope slope sos
let testInterc  = GoodnessOfFit.ttestIntercept intercept sos

(***hide***)

let chart =
    [
    Chart.Point(x,y) |> Chart.withTraceName "raw"
    Chart.Line(fittedValues|> Seq.mapi (fun i y -> x.[i],y)) |> Chart.withTraceName "fit"
    ]
    |> Chart.Combine
    |> Chart.withX_Axis(myAxis "")
    |> Chart.withY_Axis(myAxis "")
  
let outputTable = 
    let header = ["<b>ParameterName</b>";"Value";"StandardError (SE Coeff)"]
    let rows = 
        let print f = sprintf "%.3f" f
        [
        ["n";               sprintf "%.0f" n;       "-"] 
        ["meanX";           print meanX;            "-"]
        ["meanY";           print meanY;            "-"]
        ["slope";           print slope;            print stdErrSlope]
        ["intercept" ;      print intercept;        print stdErrIntercept]
        ["<b>Goodness of fit</b>";"";               ""]
        ["SS_total";        print ssTotal;          ""]
        ["SS_regression";   print ssReg;            ""]
        ["SS_residual";     print ssResidual;       ""]
        ["r (pearson cor. coef.";               print r;                ""]
        ["r_squared";       print rSq;             ""]      
        ["r_squared_adj";   print rSqAdj;         ""]
        ["SE Estimate";     print stdErrEstimate;   ""]   
        ["<b>95% Confidence interval</b>";"<b>min</b>";    "<b>max</b>"]
        ["slope";           print lowerS;           print upperS]
        ["intercept";       print lowerI;           print upperI]
        ["<b>significances</b>";"";                 ""]
        ["slope p Value";   print testSlope.PValue; ""]
        ["intercept p Value";print testInterc.PValue;""]
        ]
    Chart.Table(
        header, 
        rows, 
        ColorHeader = "#deebf7",
        ColorCells  = ["#deebf7";"white";"white"],
        AlignCells  = [HorizontalAlign.Left;HorizontalAlign.Center]
        )
    |> Chart.withTitle "Regression report"
    
(*** include-value:chart ***)
(*** include-value:outputTable ***)

(**
##Confidence bands

A confidence band shows the uncertainty of an curve estimate. It widens towards the periphery. 

A prediction band shows the uncertainty of a value of a new data point.

In both cases homoscedasticity is assumed.


*)


//data sorted by x values
let xData = vector [|1. .. 10.|]
let yData = vector [|4.;10.;9.;7.;13.;17.;16.;23.;15.;30.|]
//let xData = vector [|1.47;1.50;1.52;1.55;1.57;1.60;1.63;1.65;1.68;1.70;1.73;1.75;1.78;1.80;1.83|]
//let yData = vector [|52.21;53.12;54.48;55.84;57.20;58.57;59.93;61.29;63.11;64.47;66.28;68.10;69.92;72.19;74.46|]
let values = Seq.zip xData yData

///linear regression line fitting function
let coeffs = Linear.Univariable.coefficient xData yData
let fit = Linear.Univariable.fit coeffs

let fitValues = xData |> Seq.map (fun xi -> xi,(fit xi))

///calculate confidence band errors for every x value
let confidence = 
    xData
    |> Vector.map (calculateConfidenceBandError xData yData 0.95)

///lower and upper bounds of the 95% confidence band sorted according to x values
let (lower,upper) = 
    xData 
    |> Seq.mapi (fun i xi -> (fit xi) - confidence.[i],(fit xi) + confidence.[i]) 
    |> Seq.unzip

let rangePlot = 
    [
    Chart.Range (
        fitValues,
        lower,
        upper,
        mode = StyleParam.Mode.Lines,
        Color = Colors.toWebColor Colors.Table.Office.blue,
        RangeColor = Colors.toWebColor Colors.Table.Office.lightBlue)
        |> Chart.withTraceName "CI95"
    Chart.Point (values,Color="#000000") |> Chart.withTraceName "raw"
    ]
    |> Chart.Combine
    |> Chart.withY_Axis (myAxis "")
    |> Chart.withX_Axis (myAxis "")
    |> Chart.withTitle "Confidence band 95%"

(*** include-value:rangePlot ***)


(**
The confidence band calculation is not limited to the original x values. To get a smooth confidence band, introduce additional x values in small steps.
*)

let newXValues =
    vector [|1. .. 0.5 .. 11.|]

///calculate confidence band errors for every x value
let newConfidence = 
    newXValues
    |> Vector.map (calculateConfidenceBandError xData yData 0.95)

///lower and upper bounds of the 95% confidence band sorted according to x values
let (newLower,newUpper) = 
    newXValues 
    |> Seq.mapi (fun i xi -> (fit xi) - newConfidence.[i],(fit xi) + newConfidence.[i]) 
    |> Seq.unzip

let linePlot =
    [
    Chart.Point(xData,yData) |> Chart.withTraceName (sprintf "%.2f+%.4fx" coeffs.[0] coeffs.[1])
    Chart.Line(fitValues) |> Chart.withTraceName "linear regression"
    Chart.Line(newXValues,newLower,Color= "#C1C1C1") |> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash)|> Chart.withTraceName "lower"
    Chart.Line(newXValues,newUpper,Color= "#C1C1C1") |> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash)|> Chart.withTraceName "upper"
    ]
    |> Chart.Combine
    |> Chart.withX_Axis(myAxis "")
    |> Chart.withY_Axis(myAxis "")
    |> Chart.withTitle "Confidence band 95%"

(*** include-value:linePlot ***)

(**

##Prediction bands

*)

let predictionXValues = vector [|1. .. 0.5 .. 15.|]
///calculate preditcion band errors for every x value
let prediction = 
    predictionXValues
    |> Vector.map (calculatePredictionBandError xData yData 0.95)

///lower and upper bounds of the 95% prediction band sorted according to x values
let (pLower,pUpper) = 
    predictionXValues 
    |> Seq.mapi (fun i xi -> (fit xi) - prediction.[i],(fit xi) + prediction.[i]) 
    |> Seq.unzip

let predictionPlot =
    [
    Chart.Point(xData,yData) |> Chart.withTraceName (sprintf "%.2f+%.4fx" coeffs.[0] coeffs.[1])
    Chart.Line(fitValues) |> Chart.withTraceName "linear regression"
    Chart.Line(predictionXValues,pLower,Color= "#C1C1C1") |> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash)|> Chart.withTraceName "pLower"
    Chart.Line(predictionXValues,pUpper,Color= "#C1C1C1") |> Chart.withLineStyle(Dash=StyleParam.DrawingStyle.Dash)|> Chart.withTraceName "pUpper"
    ]
    |> Chart.Combine
    |> Chart.withX_Axis(myAxis "")
    |> Chart.withY_Axis(myAxis "")
    |> Chart.withTitle "Prediction band"


(*** include-value:predictionPlot ***)


(**
##Cook's distance

Leverage: Leverage describes the potential impact of data points regarding their regression line. Points that show a great dependent-variable-distance to all other points, have a 
higher potential to distort the regression line coefficients (high-leverage points).

Cooks distance (D) is a measure to describe the influence of each data point to the regression line.
If D is low, the influence is low, while a high D indicates an 'influencial observation' that is worth taking a closer look.
Cooks distance is a mixture of the residual sum of squares at the particular point and its leverage.

A linear threshold is arbitrarily defined by either 1, 4/n, or 3*mean(D).
Because outliers have a strong influence to D of all other points as well, the thresholds should not be applied without checking the issues by eye.

*)

open LinearRegression.OrdinaryLeastSquares.Linear

let xD = vector [|1. .. 10.|]
let yD = vector [|4.;6.;9.;7.;13.;17.;16.;23.;14.;26.|]

let cooksDistance = Univariable.cooksDistance xD yD

let nD         = float xD.Length
let meanCook   = Seq.mean cooksDistance
let threshold1 = 1.
let threshold2 = 4. / nD
let threshold3 = 3. * meanCook

(*** hide ***)
let cook = 
    [
    Chart.Column (Seq.zip xD cooksDistance) |> Chart.withTraceName "cook's distance"
    Chart.Line([0.5,threshold1;10.5,threshold1])|> Chart.withLineStyle(Dash=DrawingStyle.Dash)|> Chart.withTraceName "t=1"
    Chart.Line([0.5,threshold2;10.5,threshold2])|> Chart.withLineStyle(Dash=DrawingStyle.Dash)|> Chart.withTraceName "t=4/n"
    Chart.Line([0.5,threshold3;10.5,threshold3])|> Chart.withLineStyle(Dash=DrawingStyle.Dash)|> Chart.withTraceName "t=3*mean(D)"
    ]
    |> Chart.Combine
    |> Chart.withX_Axis(myAxis "x")
    |> Chart.withY_Axis(myAxis "cook's distance")
    |> fun l -> Chart.Stack 1 [(Chart.Point(xD,yD) |> Chart.withTraceName "raw");l]
    |> Chart.withX_Axis(myAxis "")
    |> Chart.withY_Axis(myAxis "")
    |> Chart.withTitle "Cook's distance"
    |> Chart.withSize (650.,650.)

(*** include-value: cook ***)

