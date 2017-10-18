(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
#Matrix and Vector


<a name="Vector"></a>

##Vector

<a name="Matrix"></a>

##Matrix



*)
//#r "D:/Source/FSharp.Stats/bin/FSharp.Stats.dll"
#r "FSharp.Stats.dll"
//open FSharp
open Microsoft.FSharp.Math
open FSharp.Stats



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




//let f x = x**2.

//"2.;3.;".Split(';') 
//|> Array.filter (fun x -> x <> "") 
//|> Array.map float 
//|> Array.map f



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

A + 1.
v + 1.
rv + 1.

let () =
    let assemblyProbeDirectory =  "D:/Source/FSharp.Stats/lib"
    System.IO.Directory.SetCurrentDirectory(assemblyProbeDirectory)

LinearAlgebraMKL.dgesdd_ A





//// Linear regression
//let cost theta X (y:vector) =
//    let tmp = Matrix.dot X theta 
//    y
//    |> Vector.fold (fun sum v -> 
//        let error = v - tmp
//        sum + (error * error) ) 0. 
//    |> (/) (float y.Length * 2.0)
 

//let inline (.-) a b =
//    a |> Matrix.map (fun x -> x - b)    

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




//// F# Numerics Interface 
//// http://fdatamining.blogspot.de/2010/03/f-inumerics-interface-and-matrix-class.html


//// Service Provider model pattern
//// 
//// http://blog.ploeh.dk/2011/04/27/Providerisnotapattern/
//// http://blog.ploeh.dk/2011/04/27/Providerisnotapattern/


//open System.Runtime.InteropServices

//// [<DllImport("C:/Program Files (x86)/Sho 2.1/bin/bin64/mkl.dll",EntryPoint="dgemm_")>]
//[<DllImport("D:\\libopenblas.dll",EntryPoint="dgemm_")>]
//extern void dgemm_
//  ( char *transa, char *transb, 
//    int *m, int *n, int *k, 
//    double *alpha, double *a, int *lda, 
//    double *b, int *ldb, double *beta, 
//    double *c, int *ldc );




//#nowarn "51" 

//open Microsoft.FSharp.NativeInterop


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




//matmul_blas (A.ToArray2D()) (B.ToArray2D())

