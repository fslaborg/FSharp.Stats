(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "netstandard.dll"
#I "../../src/FSharp.Stats/bin/Release/net461"
#r "../../bin/FSharp.Stats.MSF/net47/FSharp.Stats.MSF.dll"
#r "../../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r @"../../lib/Formatting/FSharp.Plotly.dll"
open FSharp.Plotly
open FSharp.Plotly.Axis
open FSharp.Plotly.StyleParam
let myAxis() = LinearAxis.init(Mirror=Mirror.All,Ticks=TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=false)



(**

#Temporal Classification

When working with time series data it often proves difficult to cluster the signals. While most clustering algorithms work based on distances, it often is *not* of interest how the 
signals amplitude changes over time. The shape and thereby the location of the extrema in the signal is a more reasonable measure for similarities of protein- or transcript kinetics.

In order to classify signals by their shape, it is beneficial to use constrained smoothing splines to fit the data and extract the location of extrema.
Monotonicity can be enforced for every interval separately, thereby constraining a predefined shape.

The validity of the shape assumption is checked via a modified GCV and corrected AIC, both corrected for small sample sizes. If the shape assumption holds, the signal is classified according to the extremum positions.


For further information visit:

- Lancaster, P. (1986): Curve and surface fitting : An introduction 

- Wood, S. N. (1994): Monotonic Smoothing Splines Fitted by Cross Validation

- Meyer, Mary C. (2012): Constrained penalized splines

- Lukas, Mark A. et al. (2016): Practical use of robust GCV and modified GCV for spline smoothing. 

In the following snipplet, the main features of the temporal classification approach are covered.

*)

open FSharp.Stats
open FSharp.Plotly
open FSharp.Stats.Fitting

//In order to use the temporal classifiaction method, you have to add the 
//FSharp.Stats/lib folder to the environment variables (nullspace dependent on the full SVD)
FSharp.Stats.ServiceLocator.setEnvironmentPathVariable (__SOURCE_DIRECTORY__ + @"..\..\lib")
FSharp.Stats.Algebra.LinearAlgebra.Service()

//defining the x values
let xValues = vector [0. .. 7.]

//defining function values
let replicate1 = [| 159.30; 446.07; 858.34; 799.95; 1126.07;  826.56;  950.01; 2844.94;|]
let replicate2 = [| 198.60; 535.39; 697.22; 847.68;  769.67; 1009.07; 1118.19; 2794.19;|]
let replicate3 = [| 170.90; 474.36;1012.73; 194.19;  709.53;  756.68; 1108.56; 2732.82;|]

//merge the replicates
let yValues = 
    Array.init xValues.Length (fun i -> [|replicate1.[i];replicate2.[i];replicate3.[i]|])
    |> Array.concat
    |> vector


let rawDataChart =   
    //calculates the means and standard deviations of the three replicates and plots it as line
    let line =
        Seq.getMeanOfReplicates 3 yValues
        |> Seq.indexed
        |> Chart.Line
        |> Chart.withTraceName (sprintf "means")
        |> Chart.withYErrorStyle(Array=Seq.getStDevOfReplicates 3 yValues)

    //plots all measurements as points 
    let points =
        yValues
        |> Seq.mapi (fun i x -> float (i-i%3) / 3.,x)
        |> Array.ofSeq
        |> fun d -> Chart.Line(d,Name="measurements",ShowMarkers=true)
        |> Chart.withMarkerStyle(Color="#0c507f",Opacity=0.5)
        |> Chart.withLine (Line.init(0))

    [line;points]
    |> Chart.Combine
    |> Chart.withX_Axis (myAxis())
    |> Chart.withY_Axis (myAxis())
    |> Chart.withX_AxisStyle "timepoints"
    |> Chart.withY_AxisStyle "intensity"



(*** include-value:rawDataChart ***)

(**

The spline takes the variance of the measured replicates in account. If there is a high inaccuracy in the data, the algorithm assigns
a lower weight to these points as their importance in the shape determination should be decreased.

*)
let weightingMethod = TemporalClassification.Fitting.WeightingMethod.StandardDeviationAdj

//calculate the points weighting according to the specified weightingmethod
let weighting = TemporalClassification.Fitting.getWeighting xValues yValues weightingMethod 3

let weightingColumnChart = 
    Matrix.getDiag weighting
    |> Seq.indexed
    |> Chart.Column
    |> Chart.withX_Axis (myAxis())
    |> Chart.withY_Axis (myAxis())
    |> Chart.withX_AxisStyle "timepoints"
    |> Chart.withY_AxisStyle "weight"


(*** include-value:weightingColumnChart ***)



let splineChart =

    let means = 
        Seq.getMeanOfReplicates 3 yValues
        |> vector

    //plot the means of the replicates as gray dots
    let origPoints = 
        means
        |> Seq.indexed
        |> fun d -> Chart.Point(d,Color="#808080")
        |> Chart.withTraceName "means"
        
    //defines a function that gives the unconstrained spline value for a given x value
    //the AICc is reported as model selection criterion
    let unconstrainedSpline = 
        TemporalClassification.Fitting.getInitialEstimateOfWeighting weighting means xValues
        |> fun result -> result.AICc,result.SplineFunction

    //defines a function that gives the constrained spline value for a given x value
    //Constraint: One minimum
    //the AICc is reported as model selection criterion
    let monotoneSpline = 
        TemporalClassification.Fitting.splineIncreasing xValues means weighting 0
        |> fun (constraintMatrices,result) -> result.AICc,result.SplineFunction

    //defines a function that gives the constrained spline value for a given x value
    //Constraint: A maximum, then a minimum
    //the AICc is reported as model selection criterion
    let maxMinSpline = 
        TemporalClassification.Fitting.splineIncreasing xValues means weighting 2  
        |> fun (constraintMatrices,result) -> result.AICc,result.SplineFunction

    //defines the x values, the functions should be fitted with
    let xVec = [0.01 .. 0.01 .. 6.99]          
    
    let monotoneSplineChart =
        xVec
        |> List.map (fun x -> x,(snd monotoneSpline) x)
        |> Chart.Line
        |> Chart.withTraceName (sprintf "Monotone AICc: %.2f" (fst monotoneSpline))                              
    let maxMinSplineChart =                                             
        xVec                   
        |> List.map (fun x -> x,(snd maxMinSpline) x)         
        |>  Chart.Line           
        |> Chart.withTraceName (sprintf "MaxMin AICc: %.2f" (fst maxMinSpline))    
    let unconstrainedChart =                                             
        xVec                        
        |> List.map (fun x -> x,(snd unconstrainedSpline) x)     
        |> Chart.Line             
        |> Chart.withTraceName (sprintf "Unconstrained AICc: %.2f" (fst unconstrainedSpline))

    [origPoints;unconstrainedChart;monotoneSplineChart;maxMinSplineChart]
    |> Chart.Combine
    |> Chart.withX_Axis (myAxis())
    |> Chart.withY_Axis (myAxis())
    |> Chart.withX_AxisStyle "timepoints"
    |> Chart.withY_AxisStyle "intensity"
    
(*** include-value:splineChart ***)

(**
The presented method automatically finds the spline, that describes the data at best, including the measurement variability. In the previous 
case a monotonically increasing spline is the model, that is selected.
To define the shape class, the signal should be sorted to, the extrema are isolated and serves as class description. In the presented case,
the signal is sorted to 'monotonically increasing' since there is no extremum present.
As an example the Extrema of the red spline above are specified:

*)

//
//Example for extrema isolation:
//

///fits a spline with two extrema present, where the first one is a maximum
let minMaxSpline = 
    TemporalClassification.Fitting.splineIncreasing xValues (vector (Seq.getMeanOfReplicates 3 yValues)) weighting 2  
    |> fun (constraintMatrices,result) -> result

//returns a list of tuples that describe extrema. The first item defines if the extremum
//is a maximum (1) or minimum (-1) and the second item defines the corresponding x value.
let isolateExtrema = 
    TemporalClassification.Fitting.getExtrema xValues minMaxSpline.TraceA minMaxSpline.TraceC
    |> fun extrema -> sprintf "Location of extrema are: %A" (TemporalClassification.Fitting.extremaToString extrema)

(*** include-value:isolateExtrema ***)

(**

*)

// helper snipplet for visualization of function derivatives
let plotAll (xVec:Vector<float>) traceA traceC = 
    
    let iterateOverX (fu: float -> float) chartName =
        [xVec.[0] + 0.001 .. 0.01 .. Seq.last xVec - 0.001] 
        |> List.map (fun x -> x,fu x) 
        |> Chart.Line 
        |> Chart.withTraceName chartName
    
    let originalSignal = 
        let fu = TemporalClassification.Fitting.initEvalAt xVec traceA traceC
        iterateOverX fu "signal"
    let fstDerivative = 
        let fu = TemporalClassification.Fitting.initEvalFstDerivativeAt xVec traceA traceC
        iterateOverX fu "fst deriv"
    let sndDerivative = 
        let fu = TemporalClassification.Fitting.initEvalSndDerivativeAt xVec traceA traceC
        iterateOverX fu "snd deriv"
    let trdDerivative = 
        let fu = TemporalClassification.Fitting.initEvalTrdDerivativeAt xVec traceA traceC
        iterateOverX fu "trd deriv"
    [
    originalSignal |> Chart.withX_Axis (myAxis()) |> Chart.withY_Axis (myAxis())
    [fstDerivative;sndDerivative;trdDerivative]|> Chart.Combine |> Chart.withX_Axis (myAxis()) |> Chart.withY_Axis (myAxis())
    ]
    |> Chart.Stack 1

let derivativeChart = plotAll xValues minMaxSpline.TraceA minMaxSpline.TraceC

(*** include-value:derivativeChart ***)


(**
To classify a signal according to its shape, a specially tailored function can be used. In this case the extrema list is empty because the 
best spline is monotonically and since the parentClass is In0 the spline is monotonically increasing.
*)

//returns the shape class that matches the given case best. (constraints for 2 extrema are investigated)
let bestFit = 
    TemporalClassification.Fitting.getBestFit xValues yValues 3 weightingMethod TemporalClassification.Fitting.Minimizer.AICc

let reportBestFit =
    bestFit
    |> fun spline ->
        let parentShape = TemporalClassification.Fitting.getParentShape xValues spline.TraceA spline.TraceC
        let extrema     = TemporalClassification.Fitting.getExtrema xValues spline.TraceA spline.TraceC
        sprintf "ParentShape: %A\nLocation of extrema are: %A" parentShape extrema


(*** include-value:reportBestFit ***)

(**
Subclassifaction returns pseudoextrema (plateaus and spots of little slope)
*)

let classification =
    TemporalClassification.Classification.getClassification xValues bestFit.TraceA bestFit.TraceC 0.05 1.

(*** include-value:classification ***)