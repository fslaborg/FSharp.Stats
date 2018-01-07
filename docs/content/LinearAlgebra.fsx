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

open Algebra.LinearAlgebraManaged

let matrixDims (m:Matrix<_>) = m.NumRows, m.NumCols

/// Calculates the pseudo inverse of the matrix
let pseudoInvers (matrix:Matrix<float>) =
    let (m,n) = matrixDims matrix
    // Is this an overdetermined or underdetermined system?
    if m > n then
        let qm,R = QR matrix
        let i = Matrix.identity m
        let Qtb = qm.Transpose * i
        SolveTriangularLinearSystems R.[0..n-1,0..n-1] Qtb.[0..n-1,0..m-1] false
    else
        let qm,R = QR matrix.Transpose
        let i = Matrix.identity n
        let Qtb = qm.Transpose * i        
        let s = SolveTriangularLinearSystems R.[0..m-1,0..m-1] Qtb.[0..m-1,0..n-1] false
        s.Transpose


let im =
       [[1.0; -15.0; 225.0; -3375.0; 50625.0]
        [1.0; -14.0; 196.0; -2744.0; 38416.0]
        [1.0; -13.0; 169.0; -2197.0; 28561.0]
        [1.0; -12.0; 144.0; -1728.0; 20736.0]
        [1.0; -11.0; 121.0; -1331.0; 14641.0]
        [1.0; -10.0; 100.0; -1000.0; 10000.0]
        [1.0; -9.0; 81.0; -729.0; 6561.0]
        [1.0; -8.0; 64.0; -512.0; 4096.0]
        [1.0; -7.0; 49.0; -343.0; 2401.0]
        [1.0; -6.0; 36.0; -216.0; 1296.0]
        [1.0; -5.0; 25.0; -125.0; 625.0]
        [1.0; -4.0; 16.0; -64.0; 256.0]
        [1.0; -3.0; 9.0; -27.0; 81.0]
        [1.0; -2.0; 4.0; -8.0; 16.0]
        [1.0; -1.0; 1.0; -1.0; 1.0]
        [1.0; 0.0; 0.0; 0.0; 0.0]
        [1.0; 1.0; 1.0; 1.0; 1.0]
        [1.0; 2.0; 4.0; 8.0; 16.0]
        [1.0; 3.0; 9.0; 27.0; 81.0]
        [1.0; 4.0; 16.0; 64.0; 256.0]
        [1.0; 5.0; 25.0; 125.0; 625.0]
        [1.0; 6.0; 36.0; 216.0; 1296.0]
        [1.0; 7.0; 49.0; 343.0; 2401.0]
        [1.0; 8.0; 64.0; 512.0; 4096.0]
        [1.0; 9.0; 81.0; 729.0; 6561.0]
        [1.0; 10.0; 100.0; 1000.0; 10000.0]
        [1.0; 11.0; 121.0; 1331.0; 14641.0]
        [1.0; 12.0; 144.0; 1728.0; 20736.0]
        [1.0; 13.0; 169.0; 2197.0; 28561.0]
        [1.0; 14.0; 196.0; 2744.0; 38416.0]
        [1.0; 15.0; 225.0; 3375.0; 50625.0]]
        |> matrix


pseudoInvers im

