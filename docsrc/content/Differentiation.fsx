(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FSharp.Stats/netstandard2.0"
(**
Differentiation
========================
Numerical differentiation on two points. The function uses two different mathematical approaches to decrease the error, one for h > 1. and one for h < 1.
*)
#r "FSharp.Stats.dll"
open FSharp.Stats.Integration.Differentiation

let testFunction x = x**2. 

differentiateTwoPoint 0.5 testFunction 2. //results in 4., the correct result for f(x) = x**2.
differentiateTwoPoint 3. testFunction 2. //results in 7.; the approximation error increases as h increases
differentiateTwoPoint 0.1 testFunction 2. // also results in 4.

(**
You can try and find an optimal h - value with the "optimalStepSize" - function. This function uses the first h-value it assumes to give good results.
Due to this, possible error due to float precision is avoided.
In the following example this is not really necessary, as values are quite big.
*)
let hArray = [|0.1 .. 0.1 .. 2.|]

optimalStepSize hArray testFunction 2. //results in 0.2

differentiateTwoPoint 0.2 testFunction 2. //results in 4.

(**
The previous two functions are already combined in "differentiateTwoPointTryFindH".
*)
differentiateTwoPointTryFindH hArray testFunction 2. //results in 4.
(**
If you want to use a presuggested hArray then you can use the "differentiateTwoPointTryFindHFromPremadeArr".
This function uses an array from 0.01 to 5e^-100 in [|0.01; 0.005; 0.001; 0.0005; 0.0001 ..|]-increments as hArray.
*)
differentiateTwoPointTryFindHFromPremadeArr testFunction 2. //results in 4.

