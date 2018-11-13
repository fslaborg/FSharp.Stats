(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Stats/netstandard2.0"
#r "../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/Fsharp.Plotly.dll"
#r "netstandard"
open FSharp.Plotly

#r "FSharp.Stats.dll"
open FSharp.Stats

(**
#Integration

## Differentiation

FSharp.Stats implements a three point differentiation method. This method takes a set of values and their function values. For a given value xT of the set, one defines three other points which should be considered to calculate the differential at xT.
Here follows a small snippet.

First, we create our data. In this case our function is f(x) = x ^ 3.
*)

// x data
let xs = Array.init 100 (fun x -> float x / 8.)
// y data
let ys = Array.map (fun x -> x ** 3.) xs

(*
Now we apply the threePointDifferentiation to every point starting with the second and ending with the second last. We can't do it for every point because for every point we need to have three other points in close proximity.
*)

let y's = 
    [|
    for i = 1 to xs.Length - 2 do
        yield xs.[i],Integration.Differentiation.differentiateThreePoint xs ys i (i-1) (i) (i+1)
    |]

(*
We compare the resulting values with the values of the known differential f'(x) = 3x^2, here called g(x)
*)


(* TO-DO: Include task*)
[
Chart.Point(xs,ys,Name = "f(x)")
Chart.Point(y's,Name = "f'(x)")
Chart.Point(Array.map (fun x -> x,(x ** 2.) * 3.) xs,Name = "g(x)")
]
|> Chart.Combine

