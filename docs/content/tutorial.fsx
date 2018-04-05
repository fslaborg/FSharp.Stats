(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Introducing your project
========================

Say more

*)
#r "FSharp.Stats.dll"
open FSharp.Stats



let t  = [|1.1 .. 0.5 .. 10.1|]
let t'  = [|-1.1 .. -0.5 .. -10.1|]
let tt = Array.concat [|t;t'|] |> Array.sort


Array.findIndexBack (fun x -> x <= 5.) 

open System
//open System.Collections.Generic

/// Counts in a sorted arrray value <= a
let countLefterSegment (arr:'T []) a =
    let zero = 0.
    let tmp = Array.BinarySearch(arr, a)
    let idx = if tmp < 0 then ~~~tmp-1 else tmp
    idx+1

countLefterSegment t 5.


t |> Array.filter (fun x -> x <= 0.0)  |> Array.length


/// Counts in a sorted arrray value >= a
let countRighterSegment (arr:'T []) a =
    let len = Array.length arr
    let tmp = Array.BinarySearch(arr, a)
    let idx = if tmp < 0 then ~~~tmp-1 else tmp
    if idx = -1 then len else len - idx


countRighterSegment t 1.1

t |> Array.filter (fun x -> x >= 0.0)  |> Array.length


[|1.1 .. 0.5 .. 10.1|].Length

(**
Some more info

*)
