(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#r "../bin/FSharp.Stats/netstandard2.0/FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-alpha5"

open Plotly.NET
open Plotly.NET.Axis
open Plotly.NET.StyleParam
open FSharp.Stats
(** 

#Summary

Short documentation of very basic statistical operations

<a name="Central Tendency"></a>

##Central Tendency

###Mean

"Mean" stands here for the arithmetic mean (also called average) is the sum of numbers in a collection divided by the count of those numbers.  
The mean function is usually located in the module of the respective collection:

*)
let v = vector [|1.;2.;5.|]
let mean = Vector.mean v

(*** include-value:mean ***)

(**


###Median

If you sort the values of a collection by size, the median is the value in central position. Therefore there are as many bigger values as smaller values than the median in the collection.
The median function is usually located in the module of the respective collection:
*)

let arr = [|1.;3.;5.;4.;2.;8.|]
let median = Array.median arr

(*** include-value:median ***)
(**
###Truncated/Trimmed mean

Computes the truncated (trimmed) mean where a given percentage of the highest and lowest values are discarded. In total 2 times the given percentage are discarded.

*)

let seq = seq [1.;3.;5.;4.;2.;8.]
let truMean = Seq.meanTruncated 0.2 arr

(*** include-value:truMean ***)

(**

<a name="Dispersion"></a>

##Dispersion

###Variance/Standard Deviation

The variance and standard deviation are measures of dispersion the values of a collection have. While the standard deviation has the same unit as the values of the collection the variance has the squared unit. 
If the full population is **not** given, the calculation lacks in one degree of freedom, so the Bessel corrected version of the calculation has to be used (results in higher values).
*)

let data =          [|1.;3.;5.;4.;2.;8.|]
let varSample =     Seq.var data
let varPopulation = Seq.varPopulation data
let stdSample =     Seq.stDev data
let stdPopulation = Seq.stDevPopulation data

(*** hide ***)
let printStd = sprintf "\r\nstdSample:     %.3f\r\nstdPopulation: %.3f" stdSample stdPopulation
(*** include-value:printStd ***)

(**
###Coefficient of variation

The coefficient of variation is the mean-normalized standard deviation. It describes the ratio of the standard devation to the mean. It assists in comparing measurement variability
with varying amplitudes. Use only if data is measured with a ratio scale (meaningful zero values and meaningful intervals).

*)
let sample1 =   [1.;4.;2.;6.;5.;3.;2.;]
let sample2 =   [13.;41.;29.;8.;52.;34.;25.;]
let cvSample1 = Seq.cv sample1
let cvSample2 = Seq.cv sample2

//use if data is complete (whole population was measured)
//let cvPopulation = Seq.cvPopulation data

(*** hide ***)
let printCvS = sprintf "\r\ncvSample1: %.3f\r\ncvSample2: %.3f" cvSample1 cvSample2
(*** include-value:printCvS ***)

