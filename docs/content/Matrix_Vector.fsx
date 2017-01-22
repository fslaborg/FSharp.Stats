(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
//#I "../../bin"

(**
dfdf
*)
#r "D:/Source/FSharp.Stats/bin/FSharp.Stats.dll"
//#r "FSharp.Stats.dll"
//open FSharp
open Microsoft.FSharp.Math




// http://fdatamining.blogspot.de/2010/03/matrix-and-linear-algebra-in-f-part-i-f.html
// 1.9.7.8. 
// http://fdatamining.blogspot.de/search/label/linear%20algebra

let A = 
    matrix [ [ 1.0; 7.0; 2.0 ]
             [ 1.0; 3.0; 1.0 ]
             [ 2.0; 9.0; 1.0 ] ]

let B = 
    matrix [ [ 10.0; 70.0; 20.0 ]
             [ 10.0; 30.0; 10.0 ]
             [ 20.0; 90.0; 10.0 ] ]


let v = 
    vector [|2.0; 20.0; 1.|]


let rv = 
    rowvec [|2.0; 20.0; 1.|]



A+B
A-B
A*B // matrix product
A.*B  // element-wise product
A * 2.0 // scalar product
2.0 * A // this is also ok
-A // negation of a matrix

//  matrix-vector product
A * v

Matrix.dot A B



// F# Numerics Interface 
// http://fdatamining.blogspot.de/2010/03/f-inumerics-interface-and-matrix-class.html


// Service Provider model pattern
// 
// http://blog.ploeh.dk/2011/04/27/Providerisnotapattern/
// http://blog.ploeh.dk/2011/04/27/Providerisnotapattern/
