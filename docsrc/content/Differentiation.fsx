(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Stats/netstandard2.0"
(**
Differentiation
========================
Numerical differentiation is used to estimate the derivative of a mathematical function using values of the function and perhaps other knowledge about the function.
To achieve this, it calculates the difference of f(x) at x and f(x) at x+h and correlates it to h. This will give better approximations the smaller h is. 
The function uses two different mathematical approaches to decrease the error, one for h > 1. and one for h < 1.

</br>

![Data model]https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/Derivative.svg/800px-Derivative.svg.png

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
*)
let hArray = [|0.1 .. 0.1 .. 2.|]

optimalStepSize hArray testFunction 2. //results in 0.2

differentiate 0.2 testFunction 2. //results in 4.

(**
The previous two functions are already combined in "differentiateTryFindH".
*)
differentiateTryFindH hArray testFunction 2. //results in 4.
(**
If you want to use a presuggested hArray then you can use the "differentiateOptimalH".
This function uses an array from 0.01 to 5e^-100 in [|0.01; 0.005; 0.001; 0.0005; 0.0001 ..|]-increments as hArray.
*)
differentiateOptimalH testFunction 2. //results in 4.
