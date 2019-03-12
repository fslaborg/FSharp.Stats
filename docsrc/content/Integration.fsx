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
# Integration

## Differentiation

<a name="ThreePointDifferentiation"></a>

### Three-Point Differentiation
FSharp.Stats implements a three point differentiation method. This method takes a set of values and their function values. For a given value xT of the set, one defines three other points which should be considered to calculate the differential at xT.
Here follows a small snippet.

First, we create our data. In this case our function is f(x) = x ^ 3.
*)

// x data
let xs = Array.init 100 (fun x -> float x / 8.)
// y data
let ys = Array.map (fun x -> x ** 3.) xs

(**
Now we apply the threePointDifferentiation to every point starting with the second and ending with the second last. We can't do it for every point because for every point we need to have three other points in close proximity.
*)

let y's = 
    [|
    for i = 1 to xs.Length - 2 do
        yield xs.[i],Integration.Differentiation.differentiateThreePoint xs ys i (i-1) (i) (i+1)
    |]

(**
We compare the resulting values with the values of the known differential f'(x) = 3x^2, here called g(x)
*)


(* TO-DO: Include task*)
[
Chart.Point(xs,ys,Name = "f(x)")
Chart.Point(y's,Name = "f'(x)")
Chart.Point(Array.map (fun x -> x,(x ** 2.) * 3.) xs,Name = "g(x)")
]
|> Chart.Combine

(**
<a name="TwoPointDifferentiation"></a>

### Two-Point Differentiation

Numerical differentiation is used to estimate the derivative of a mathematical function using values of the function and perhaps other knowledge about the function.
To achieve this, it calculates the difference of f(x) at x and f(x) at x+h and correlates it to h. This will give better approximations the smaller h is. 
The function uses two different mathematical approaches to decrease the error, one for h > 1. and one for h < 1.

</br>

![Data model](img/Derivative.svg.png)

</br>
*)

#r "FSharp.Stats.dll"
open FSharp.Stats.Integration.Differentiation.TwoPointDifferentiation
open FSharp.Stats.Integration.Differentiation

let testFunction x = x**2. 

differentiate 0.5 testFunction 2. //results in 4., the correct result for f(x) = x**2.
differentiate 3. testFunction 2. //results in 7.; the approximation error increases as h increases
differentiate 0.1 testFunction 2. // results in 4.

(**
You can try and find an optimal h - value with the "optimalStepSize" - function. This function uses the first h-value it assumes to give good results.
Due to this, possible error due to float precision is avoided.
In the following example this is not really necessary, as values are quite big.

**This function is set private but still used in the follow-up functions explained later**

*)
let hArray = [|0.1 .. 0.1 .. 2.|]

optimalStepSize hArray testFunction 2. //results in 0.2

differentiate 0.2 testFunction 2. //results in 4.

(**
The previous two functions are already combined in "differentiateTryFindH".
*)
differentiateOptimalHBy hArray testFunction 2. //results in 4.
(**
If you want to use a presuggested hArray then you can use the "differentiateOptimalH".
This function uses an array from 0.01 to 5e^-100 in [|0.01; 0.005; 0.001; 0.0005; 0.0001 ..|]-increments as hArray.
*)
differentiateOptimalH testFunction 2. //results in 4.