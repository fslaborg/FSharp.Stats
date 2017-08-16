(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
# FSharp.Stats


FSharp.Stats aims to be a user-friendly library for numerical and statistical computation written in F#.


## Example


This example demonstrates using a function defined in the FSharp.Stats library.

*)
#r "FSharp.Stats.dll"
open FSharp.Stats


let A = 
    matrix [[ 1.0; 7.0; 2.0 ]
            [ 1.0; 3.0; 1.0 ]
            [ 2.0; 9.0; 1.0 ] ]

let v = vector [|2.0; 20.0; 1.|]


let v' = A * v


Vector.mean v'

(**

## Documentation

The API reference is automatically generated from Markdown comments in the library implementation.

It can be found [here](reference/index.html).
*)