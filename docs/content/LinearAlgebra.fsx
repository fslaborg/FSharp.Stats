(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"
#r "FSharp.Stats.dll"

(**
#Linear Algebra
*)

open FSharp.Stats

// Consider a system of linear equation writen in matrix form
//   x1 +   x2 -   x3 =  4.
//   x1 - 2.x2 - 3.x3 = -6.
// 2.x1 + 3.x2 +   x3 =  7.
// -> AX = B

let A = 
    matrix [ [ 1.0;  1.0; -1.0 ]
             [ 1.0; -2.0; -3.0 ]
             [ 2.0;  3.0;  1.0 ] ]

let B = 
    matrix [ [  4.0; ]
             [ -6.0; ]
             [  7.0; ] ]


// Let A = LU (LU - decomposition) and substitute into AX = B
// Solve LUX = B for X to solve the system.
// Let UX = Y 
// LY = B and UX = Y
// First solve LY = B for Y and then solve UX = Y for X.

let P,L,U = Algebra.LinearAlgebraManaged.LU A


open FSharp.Stats.Algebra
open FSharp.Stats.Algebra.LinearAlgebraManaged

let SolveLinearSystemsByQR (A:matrix) (B:matrix) =
    let (n,m) = A.Dimensions
    //if n <> m then invalidArg "Matrix" "Matrix must be square." 
    let q,r = QR A
    let y = q.Transpose * B
    (SolveTriangularLinearSystems r y true)


let A = 
    matrix [ [ 2.0; 1.0; 1.0 ]
             [ 1.0; 3.0; 2.0 ]
             [ 1.0; 0.0; 0.0 ] ]



let B = 
    matrix [ [ 4.0; ]
             [ 5.0; ]
             [ 6.0; ] ]

// [  6.  15. -23.]
SolveLinearSystems A B


