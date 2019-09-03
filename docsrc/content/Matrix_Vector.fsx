(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
//#r "D:/Source/FSharp.Stats/bin/FSharp.Stats.dll"
#I "../../bin/FSharp.Stats/netstandard2.0"
#r "../../packages/formatting/FSharp.Plotly/lib/netstandard2.0/Fsharp.Plotly.dll"
#r "netstandard"
open FSharp.Plotly

(**
#Matrix and Vector


<a name="Vector"></a>

##Vector
*)
#r "FSharp.Stats.dll"
open FSharp.Stats

let v = 
    vector [|2.0; 20.0; 1.|]

(*** include-value:v ***)

let rv = 
    rowvec [|2.0; 20.0; 1.|]

(*** include-value:rv ***)

(***hide***)
let exmp13 = v + 1.
let exmp14 = rv + 1.

(**Addition to every element of vector.*)
v + 1.

(*** include-value:exmp13 ***)

(**Addition to every element of row vector.*)
rv + 1.

(*** include-value:exmp14 ***)


(**
<a name="Matrix"></a>

##Matrix

###Creating Matrices with FSharp.Stats. 
Matrices will also be printed with the FSharp.Stats.FSIPrinters.matrix function to get a nice console output.
*)
#r "FSharp.Stats.dll"
open FSharp.Stats

// http://fdatamining.blogspot.de/2010/03/matrix-and-linear-algebra-in-f-part-i-f.html
// http://fdatamining.blogspot.de/search/label/linear%20algebra

let A = 
    matrix [ [ 1.0; 7.0; 2.0 ]
             [ 1.0; 3.0; 1.0 ]
             [ 2.0; 9.0; 1.0 ] ]

(***hide***)
let APrint =
    FSharp.Stats.FSIPrinters.matrix A

(*** include-value:APrint ***)

let B = 
    matrix [ [ 10.0; 70.0; 20.0 ]
             [ 10.0; 30.0; 10.0 ]
             [ 20.0; 90.0; 10.0 ] ]

(***hide***)
let BPrint =
    FSharp.Stats.FSIPrinters.matrix B

(*** include-value:BPrint ***)


(**
###Calculation Examples for Matrices
*)

(***hide***)
let exmp1 =
    A
    |> Matrix.Generic.enumerateRowWise (Seq.sum)

(**Get sum of each row.*)
A
|> Matrix.Generic.enumerateRowWise (Seq.sum)

(*** include-value:exmp1 ***)

(***hide***)
let exmp2 =
    A
    |> Matrix.mapiCols (fun i r -> r)

(**Create seq of vectors of matrix columns.*)
A
|> Matrix.mapiCols (fun i r -> r)

(*** include-value:exmp2 ***)

(***hide***)
let exmp3 = A + B |> FSharp.Stats.FSIPrinters.matrix
let exmp4 = A - B|> FSharp.Stats.FSIPrinters.matrix
let exmp5 = A * B|> FSharp.Stats.FSIPrinters.matrix
let exmp6 = A.*B|> FSharp.Stats.FSIPrinters.matrix
let exmp7 = A * 2.0|> FSharp.Stats.FSIPrinters.matrix
let exmp9 = -A|> FSharp.Stats.FSIPrinters.matrix
let exmp10 = A * v 
let exmp11 = Matrix.dot A B 
let exmp12 = A + 1.|> FSharp.Stats.FSIPrinters.matrix


(**Sum of two matrices.*)
A + B

(*** include-value:exmp3 ***)

(**Difference between two matrices.*)
A - B

(*** include-value:exmp4 ***)

(**Product of two matrices.*)
A * B 

(*** include-value:exmp5 ***)

(**Element-wise product of two matrices*)
A .* B 

(*** include-value:exmp6 ***)

(**Scalar product of a matrix.*)
A * 2.0 
2.0 * A // also ok, gives same result

(*** include-value:exmp7 ***)

(**Negation of a matrix*)
-A 

(*** include-value:exmp9 ***)

(**Product of a matrix-vector.*)
A * v

(*** include-value:exmp10 ***)

(**Dot product of two matrices.*)
Matrix.dot A B

(*** include-value:exmp11 ***)

(**Addition to every element of matrix.*)
A + 1.

(*** include-value:exmp12 ***)


(***hide***)
//let () =
//    let assemblyProbeDirectory =  "D:/Source/FSharp.Stats/lib"
//    System.IO.Directory.SetCurrentDirectory(assemblyProbeDirectory)

(***hide***)
//// Linear regression
//let cost theta X (y:vector) =
//    let tmp = Matrix.dot X theta 
//    y
//    |> Vector.fold (fun sum v -> 
//        let error = v - tmp
//        sum + (error * error) ) 0. 
//    |> (/) (float y.Length * 2.0)

(***hide***)
//let inline (.-) a b =
//    a |> Matrix.map (fun x -> x - b)

(***hide***)
//let estimateCoefficientsBySGD (X:matrix) y theta alpha = // rate nEpoch (data:array<array<'a>>)
//    //let delta = Vector.create (X.NumCols) 1.
//    let cost' = cost theta X y
//    let rec loop delta =
//        match (delta.Values|> Array.maxBy abs > 1e-6) with
//        | false -> theta
//        | true -> 
//            let error = 
//                let tmp = Matrix.dot X theta 
//                y |> Vector.map (fun v -> v - y)
//            let delta' = 
//                Matrix.dot X.Transpose error 
//                |> (/) (float y.Length)
//            let trial_theta = theta - alpha * delta
//            let trial_cost = cost trial_theta X y
//            loop delta'
//    let rec wloop (theta:vector) (ttheta:vector) (tcost:vector) (pcost:vector) =
//        let ttheta' = (theta + ttheta) / 2.
//        let tcost'  = cost ttheta X y      
//    //while (np.max(np.abs(delta)) > 1e-6) do
//    //    1.



(***hide***)
//// F# Numerics Interface 
//// http://fdatamining.blogspot.de/2010/03/f-inumerics-interface-and-matrix-class.html

(***hide***)
//// Service Provider model pattern
//// 
//// http://blog.ploeh.dk/2011/04/27/Providerisnotapattern/
//// http://blog.ploeh.dk/2011/04/27/Providerisnotapattern/

(***hide***)
//open System.Runtime.InteropServices

(***hide***)
//// [<DllImport("C:/Program Files (x86)/Sho 2.1/bin/bin64/mkl.dll",EntryPoint="dgemm_")>]
//[<DllImport("D:\\libopenblas.dll",EntryPoint="dgemm_")>]
//extern void dgemm_
//  ( char *transa, char *transb, 
//    int *m, int *n, int *k, 
//    double *alpha, double *a, int *lda, 
//    double *b, int *ldb, double *beta, 
//    double *c, int *ldc );



(***hide***)
//#nowarn "51" 
//open Microsoft.FSharp.NativeInterop

(***hide***)
//let matmul_blas (a:float[,]) (b:float[,]) = 
//    // Get dimensions of the input matrices
//    let m = Array2D.length1 a
//    let k = Array2D.length2 a
//    let n = Array2D.length2 b
//    // Allocate array for the result
//    let c = Array2D.create n m 0.0
//    // Declare arguments for the call
//    let mutable arg_transa = 't'
//    let mutable arg_transb = 't'
//    let mutable arg_m = m
//    let mutable arg_n = n
//    let mutable arg_k = k
//    let mutable arg_alpha = 1.0
//    let mutable arg_ldk = k
//    let mutable arg_ldn = n
//    let mutable arg_beta = 1.0
//    let mutable arg_ldm = m

(***hide***)
//    // Temporarily pin the arrays
//    use arg_a = PinnedArray2.of_array2D(a)
//    use arg_b = PinnedArray2.of_array2D(b)
//    use arg_c = PinnedArray2.of_array2D(c)
//    // Invoke the native routine
//    dgemm_( &&arg_transa, &&arg_transb,
//            &&arg_m, &&arg_n, &&arg_k,
//            &&arg_alpha, arg_a.Ptr, &&arg_ldk,
//            arg_b.Ptr, &&arg_ldn, &&arg_beta,
//            arg_c.Ptr, &&arg_ldm )
//    // Transpose the result to get m*n matrix 
//    Array2D.init m n (fun i j -> c.[j,i])



(***hide***)
//matmul_blas (A.ToArray2D()) (B.ToArray2D())

