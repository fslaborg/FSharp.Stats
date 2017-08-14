(*** hide ***)
#I "../../bin"

(** 

#Summary

Short documentation of very basic statistical operations

*)
#r @"FSharp.Stats.dll"
open FSharp.Stats

(**
<a name="Mean"></a>

##Mean

"Mean" stands here for the arithmetic mean (also called average) is the sum of numbers in a collection divided by the count of those numbers.  
The mean function is usually located in the module of the respective collection:

*)
let v = vector [|1.;2.;5.|]

let mean = Vector.mean v

(**

<a name="Median"></a>

##Median

If you sort the values of a collection by size, the median is the value in central position. Therefore there are as many bigger values as smaller values than the median in the collection.
The median function is usually located in the module of the respective collection:
*)

let arr = [|1.;3.;5.;4.;2.;8.|]
let median = Array.median arr

(**

<a name="Std"></a>

##Standard Deviation

The standard deviation(std) is a measure of dispersion the values of a collection have. It has the same unit as the values of the collection. 

snippet coming soon
*)

