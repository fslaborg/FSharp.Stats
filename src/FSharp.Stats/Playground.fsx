#I "bin/Release/.net8.0"
#r "FSharp.Stats.dll"
//#r "nuget: Plotly.NET"

open System
open FSharp.Stats
open FSharp.Stats.Distributions
//open Plotly.NET
open FSharp.Stats.Algebra
open FSharp.Stats.Distributions.Continuous

let canonicalizeQR (Q: Matrix<float>, R: Matrix<float>) =
    let m, n = R.NumRows, R.NumCols
    for j = 0 to min m n - 1 do
        if R.[j, j] < 0.0 then
            // Flip column j of Q
            for i = 0 to Q.NumRows - 1 do
                Q.[i, j] <- -Q.[i, j]
            // Flip row j of R
            for k = 0 to R.NumCols - 1 do
                R.[j, k] <- -R.[j, k]
    Q, R

// TODO: Refector in Householder module
let normalize (v: Vector<float>) : Vector<float> =
    let norm = Vector.norm v
    if norm = 0. then v
    else
        Array.map (fun x -> x / norm) v


let leastSquares (A : Matrix<float>) (b: Vector<float>) =
// Maybe rename to leastSquaresQR?
    let (m,n) = A.NumRows, A.NumCols
        
    // Is this an overdetermined or underdetermined system?
    if m >= n then
        //printfn "Least squares: solving %dx%d system with %d equations." m n n
        let Qm, R = LinearAlgebra.qrDecompose A
        let Qtb   = Qm.Transpose() * b
        LinearAlgebra.solveTriangularLinearSystem R.[0..n-1,0..n-1] Qtb.[0..n-1] false
        

    else
        // underdetermined: solve A^T * x = 0 with min ||x||
        let AT = A.Transpose()
        let Q, R = LinearAlgebra.qrDecompose AT
        let RT   = R.Transpose()
        let s    = LinearAlgebra.solveTriangularLinearSystem RT.[0..m-1, 0..m-1] b true
        Q.[0.., 0..m-1] * s
        


let A = Matrix(2, 3, [| 1.0; 2.0; 0.0;
                        0.0; 1.0; 1.0 |])
let b = [| 3.0; 2.0 |]
let x = leastSquares A b



let Q, R = LinearAlgebra.qrDecompose A
Q * R

//let A = Matrix(2, 2, [| 2.0; 1.0;
//                        1.0; 3.0 |])
//let b = [| 5.0; 10.0 |]
//let x = LinearAlgebra.leastSquares A b
//let expected = [| 1.0; 3.0 |]

//let Q, R = LinearAlgebra.qrDecompose A


//let A = Matrix(2, 2, [| 2.0; 1.0;
//                        1.0; 3.0 |])
//let Q, R = LinearAlgebra.qrDecompose A |> canonicalizeQR
//let QR = Q * R







////let UpdateQ_Debug (Q : Matrix<float>) (v : float[]) =
////    let nQ, mQ = Q.NumRows, Q.NumCols
////    let n = v.Length
////    let Qv = Array.zeroCreate<float> nQ

////    printfn "=== UpdateQ_Debug ==="
////    printfn "Input matrix Q:"
////    printfn "%A" Q
////    printfn "Householder vector v: [%s]" (String.Join("; ", v |> Array.map (fun x -> x.ToString("F3"))))

////    // Step 1: Compute Qv = Q * v over trailing n columns
////    for i = 0 to nQ - 1 do
////        let rowOffset = i * mQ + (mQ - n)
////        let dot = Acceleration.SIMDRangeUtils.dotRange Q.Data rowOffset v 0 n
////        //let rowOffset = i * mQ + i
////        //let dot = Acceleration.SIMDRangeUtils.dotRange Q.Data rowOffset v 0 n        
////        Qv.[i] <- dot
////        printfn $"Row {i}: dot(Q[{i}, {mQ-n}..], v) = {dot:F3}"

////    // Step 2: Apply in-place update to Q
////    for i = 0 to nQ - 1 do
////        let alpha = 2.0 * Qv.[i]
////        let rowOffset = i * mQ + (mQ - n)
////        let tmp1 = (Q.Data.[rowOffset .. rowOffset + n - 1] |> Array.map (fun x -> x.ToString()) |> String.concat("; "))
////        printfn $"Row {i}: alpha = 2 * {Qv.[i]:F3} = {alpha:F3}"
////        printfn $"Before update Q[{i}, {mQ-n}..]: [{tmp1}]" 
            
       
////        LinearAlgebra.subScaledRowInPlace alpha rowOffset 0 (n - i) Q.Data v//alpha rowOffset 0 n Q.Data v
////        let tmp2 = (Q.Data.[rowOffset .. rowOffset + n - 1] |> Array.map (fun x -> x.ToString()) |> String.concat("; "))
////        printfn $"After update Q[{i}, {mQ-n}..]: [{tmp2}]" 
            

////    printfn "Updated Q matrix:"
////    printfn "%A" (Q)
////    printfn "=== End UpdateQ_Debug ==="



////let applyHouseholderLeft (R: Matrix<'T>) (v: Vector<'T>) (i: int) =
////    let m, n = R.NumRows, R.NumCols
////    let nV = v.Length

////    for col = i to n - 1 do
////        // Compute projection of column onto v
////        let mutable dot = LanguagePrimitives.GenericZero<'T>
////        for k = 0 to nV - 1 do
////            let row = i + k
////            if row < m then
////                dot <- dot + v.[k] * R.[row, col]

////        let alpha = dot + dot

////        // Update R[i.., col]
////        for k = 0 to nV - 1 do
////            let row = i + k
////            if row < m then
////                R.[row, col] <- R.[row, col] - alpha * v.[k]

///// Apply Householder reflector from the left: R ← H * R
///// Only modifies the submatrix R[i.., i..]
//let applyHouseholderLeft (R: Matrix<float>) (v: Vector<float>) (i: int) =
//    let m, n = R.NumRows, R.NumCols
//    let nV = v.Length

//    for col = i to n - 1 do
//        // Compute dot product: vᵗ * R[i.., col]
//        let mutable dot = 0.0
//        for k = 0 to nV - 1 do
//            let row = i + k
//            if row < m then
//                dot <- dot + v.[k] * R.[row, col]

//        let alpha = 2.0 * dot

//        // Apply reflection to R[i.., col]
//        for k = 0 to nV - 1 do
//            let row = i + k
//            if row < m then
//                R.[row, col] <- R.[row, col] - alpha * v.[k]



//let UpdateQ_Debug (Q : Matrix<'T>) (v : Vector<'T>) =
//    let nQ, mQ = Q.NumRows, Q.NumCols
//    let n = v.Length                    // Length of Householder vector
//    let i = mQ - n                      // Starting column (pivot position)
//    let Qv = Vector.zeroCreate<'T> nQ  // Holds dot products for each row

//    // Compute Qv[i] = Q[i, i..] ⋅ v
//    for row = 0 to nQ - 1 do
//        let rowOffset = row * mQ + i
//        Qv.[row] <- Acceleration.SIMDRangeUtils.dotRange Q.Data rowOffset v 0 n

//    // Apply the Householder update to the trailing part of each row
//    for row = 0 to nQ - 1 do
//        let alpha = Qv.[row] + Qv.[row]  // 2 * dot product
//        let rowOffset = row * mQ + i
//        LinearAlgebra.subScaledRowInPlace alpha rowOffset 0 n Q.Data v




//type Testing() =
//    /// QR decomposition using Householder reflections
//    static member inline qrDecompose (A : Matrix<'T>) : (Matrix<'T> * Matrix<'T>) =
//    // former QR
//        let UpdateQ (Q : Matrix<'T>) (v : Vector<'T>) =
//            let nQ, mQ = Q.NumRows, Q.NumCols
//            let n = v.Length
//            let Qv = Vector.zeroCreate<'T> nQ
//            for i = 0 to nQ - 1 do
//                // offset in Q.Data for row i is i*mQ
//                let rowOffset = i * mQ + (mQ - n)
//                // Dot the subrange Q[i, mQ-n..mQ-1] with v[0..n-1]
//                Qv.[i] <- Acceleration.SIMDRangeUtils.dotRange Q.Data rowOffset v 0 n

//            // Update each row i in the subrange of columns [mQ-n..mQ-1]
//            //    Q[i, j] -= 2 * Qv[i] * v[j - (mQ - n)]
//            for i = 0 to nQ - 1 do
//                let alpha = Qv.[i] + Qv.[i]
//                // We want to do a row operation: Q[i, (mQ-n)..(mQ-1)] 
//                // = Q[i, (mQ-n)..(mQ-1)] - alpha * v[0..n-1].
//                let rowOffset = i * mQ + (mQ - n)
//                //LinearAlgebra.subScaledRowInPlace Q.Data rowOffset v 0 n alpha
//                LinearAlgebra.subScaledRowInPlace alpha rowOffset 0 n Q.Data v 

//        let normalize (v: Vector<'T>) : Vector<'T> =
//            let norm = Vector.norm v
//            if norm = 'T.Zero then
//                invalidArg "v" "Cannot normalize a zero vector."
//            Array.map (fun x -> x / norm) v

//        let (n, m) = (A.NumRows, A.NumCols)

//        // Q starts as identity(n)
//        let Q = Matrix.identity n
//        let R = Matrix.copy A

//        for i = 0 to (min n m) - 1 do
//            // 1) Compute Householder transform v for column i
//            let v = LinearAlgebra.householderTransform R i |> normalize
//            // 2) Update Q
//            UpdateQ Q v
//            applyHouseholderLeft R v i

//        Q, R


//    //static member inline householderTransform
//    //    (A: Matrix<'T>) (i: int) : Vector<'T> =
//    //    let n = A.NumRows
        
//    //    let v = Vector.zeroCreate<'T> n
//    //    let aCol = Matrix.getCol i A
//    //    let norm = Vector.norm aCol  // ToDO: use a more efficient norm calculation 
//    //    v.[i] <- aCol.[i] + if aCol.[i] >= 'T.Zero then norm else -norm
//    //    for j = i + 1 to n - 1 do
//    //        v.[j] <- aCol.[j]
//    //    v 
        

////let Q :Matrix<float> = Matrix.identity 3
////let v = [| 1.0; 2.0; 3.0 |]
////UpdateQ Q v

////Q =
////[[ 1.073  -2.0    -2.0  ]
//// [ 1.464  -1.0    -2.0  ]
//// [ 1.464  -2.0    -1.0  ]]

//module Testing =

//    open System

//    /// Compute normalized Householder vector from a subcolumn x
//    let householderVector (x: float[]) : float[] =
//        let norm = sqrt (Array.sumBy (fun xi -> xi * xi) x)
//        let v = Array.copy x
//        v.[0] <- v.[0] + (if x.[0] >= 0.0 then norm else -norm)
//        let norm_v = sqrt (Array.sumBy (fun vi -> vi * vi) v)
//        if norm_v = 0.0 then v
//        else Array.map (fun vi -> vi / norm_v) v

//    let householderTransform
//        (A: Matrix<float>) (i: int) : Vector<float> =
//        let n = A.NumRows
        
//        let aCol = Matrix.getCol i A
//        let v = aCol[i..]
//        let norm = Vector.norm v // ToDO: use a more efficient norm calculation 
//        v.[0] <- v.[0] + (if v.[0] >= 0. then norm else -norm)
//        v
 
//    let normalize (v: Vector<float>) : Vector<float> =
//        let norm = Vector.norm v
//        if norm = 0. then v
//        else
//            Array.map (fun x -> x / norm) v

//    /// Update Q: Q ← Q * Hᵢ using Householder vector v (from column i)
//    let updateQ (Q: Matrix<float>) (v: float[]) (i: int) =
//        let nQ, mQ = Q.NumRows, Q.NumCols
//        for row = 0 to nQ - 1 do
//            let mutable dot = 0.0
//            for k = 0 to v.Length - 1 do
//                dot <- dot + Q.[row, i + k] * v.[k]
//            let alpha = 2.0 * dot
//            for k = 0 to v.Length - 1 do
//                Q.[row, i + k] <- Q.[row, i + k] - alpha * v.[k]


//    let update_Q (Q : Matrix<'T>) (v : Vector<'T>) =
//        let nQ, mQ = Q.NumRows, Q.NumCols
//        let n = v.Length
//        let Qv = Vector.zeroCreate<'T> nQ
//        for i = 0 to nQ - 1 do
//            // offset in Q.Data for row i is i*mQ
//            let rowOffset = i * mQ + (mQ - n)
//            // Dot the subrange Q[i, mQ-n..mQ-1] with v[0..n-1]
//            Qv.[i] <- Acceleration.SIMDRangeUtils.dotRange Q.Data rowOffset v 0 n

//        // Update each row i in the subrange of columns [mQ-n..mQ-1]
//        //    Q[i, j] -= 2 * Qv[i] * v[j - (mQ - n)]
//        for i = 0 to nQ - 1 do
//            let alpha = Qv.[i] + Qv.[i]
//            // We want to do a row operation: Q[i, (mQ-n)..(mQ-1)] 
//            // = Q[i, (mQ-n)..(mQ-1)] - alpha * v[0..n-1].
//            let rowOffset = i * mQ + (mQ - n)
//            //LinearAlgebra.subScaledRowInPlace Q.Data rowOffset v 0 n alpha
//            LinearAlgebra.subScaledRowInPlace alpha rowOffset 0 n Q.Data v 

//    /// Apply Hᵢ to R from the left: R ← H * R
//    let applyHouseholderLeft (R: Matrix<float>) (v: float[]) (i: int) =
//        let m, n = R.NumRows, R.NumCols
//        for col = i to n - 1 do
//            let mutable dot = 0.0
//            for k = 0 to v.Length - 1 do
//                let row = i + k
//                if row < m then
//                    dot <- dot + v.[k] * R.[row, col]
//            let alpha = 2.0 * dot
//            for k = 0 to v.Length - 1 do
//                let row = i + k
//                if row < m then
//                    R.[row, col] <- R.[row, col] - alpha * v.[k]


   

//    /// Main QR decomposition function
//    let qrDecompose (A: Matrix<float>) : Matrix<float> * Matrix<float> =
//        let m, n = A.NumRows, A.NumCols
//        let Q = Matrix.identity m
//        let R = Matrix.copy A

//        for i = 0 to n - 1 do
//            //let x = [| for k in i .. m - 1 -> R.[k, i] |]
//            let x = Matrix.getCol i R
//            let hh = Householder.create x.[i..] // Create Householder reflector for column i
//            let v = hh.V |> normalize 
//            //let v = householderTransform R i |> normalize // householderVector x
//            update_Q Q v
//            //applyHouseholderLeft R v i
//            Householder.applyLeft(hh, R, i)

//        Q, R



//let A = Matrix(3, 2, [| 1.0; 1.0;
//                        1.0; 2.0;
//                        1.0; 3.0 |])
//let Q, R = Testing.qrDecompose A

//let QR = Matrix.matmul Q R

//let t:Vector<float> = Matrix.getCol 1 A
//t.[0] <- nan

//let i = 1
//let m, n = A.NumRows, A.NumCols
//[| for k in i .. m - 1 -> A.[k, i] |]

//let v = Testing.householderTransform A 0 |> normalize

//Vector.norm v

//let H = 
//    let n = v.Length
//    let I : Matrix<float> = Matrix.identity n
//    let outer = Matrix.init n n (fun i j -> v.[i] * v.[j])
//    I - (outer * 2.0) 
//    (outer * 2.0) 

//let R1 = Matrix.matmul H A

//let (n, m) = (A.NumRows, A.NumCols)

//let Q : Matrix<float> = Matrix.identity n
//let R = Matrix.copy A
//UpdateQ_Debug Q v
//applyHouseholderLeft R v 0






//let Q : Matrix<float> = Matrix.identity 3
//let v = [| 1.0; 2.0; 3.0 |]
//UpdateQ_Debug Q v

//let A = Matrix(3, 2, [| 1.0; 1.0;
//                        1.0; 2.0;
//                        1.0; 3.0 |])

//let v = LinearAlgebra.householderTransform A 1

//let d = [| 4.0; 3.0 |]
//let e = [| 2.0 |]

//let bidiag = {
//    D = d
//    E = e
//}

//let sigma = GolubKahan.diagonalize bidiag 


//let A =
//    matrix [|
//        [| 4.0; 1.0; 2.0 |]
//        [| 3.0; 1.0; 0.0 |]
//        [| 5.0; 1.0; 3.0 |]
//    |]

//// Extract column 0
//let colVector = [| A.[0, 0]; A.[1, 0]; A.[2, 0] |]
//let h = Householder.create colVector

//// Overwrite A[0..,0] with [β; 0; 0]
//A.[0, 0] <- h.Beta
//for i = 1 to 2 do
//    A.[i, 0] <- 0.0

//Householder.applyLeft(h, A, 0)

//A.[1, 0] 

////let input =
////    matrix [|
////        [| 1.0; 2.0; 3.0 |]
////        [| 4.0; 5.0; 6.0 |]
////        [| 7.0; 8.0; 9.0 |]
////    |]

////let A = input |> Matrix.copy

////Bidiagonalization.bidiagonalizeInPlace A
////A



//// Apply reflector from the left to matrix A starting at row 0

//let alpha = 5.0
//let beta  = 1.0
//let p     = 0.95
//let x     = Gamma.InvCDF alpha beta p


//let Categorical_SampleUnchecked (probabilities: float[]) =
//    let rnd = Random.rndgen.NextFloat()
//    let rec search i acc =
//        if i >= probabilities.Length then probabilities.Length - 1
//        elif acc + probabilities[i] >= rnd then i
//        else search (i + 1) (acc + probabilities[i])
//    search 0 0.0




///// Helper function to sample many times and collect counts.
//let sampleMany (times: int) (probabilities: float[]) =
//    let counts = Array.zeroCreate probabilities.Length
//    for _ in 1 .. times do
//        let idx = Categorical_SampleUnchecked probabilities
//        counts.[idx] <- counts.[idx] + 1
//    counts



//let p = [| 0.2; 0.3; 0.5 |]
//let iterations = 10000000

//Array.init iterations (fun _ -> Categorical_SampleUnchecked p)
//|> Array.countBy id



//// Act
//let counts = sampleMany iterations p
//float counts.[2] / float iterations


//// Assert
//// All indices should be between 0 and p.Length - 1
//// If out of range, an exception would occur while incrementing counts.
//// So, if we got this far without an exception, the function is generating valid indices.
//// We can add an extra check that the sum of counts equals 'iterations'.
//let total = Array.sum counts


//let SampleUnchecked (p : float[]) n =          
//    //let cp = Discrete.Multinomial.ProbabilityMassToCumulativeDistribution p
//    let ret = Array.zeroCreate p.Length
//    for _ = 1 to n do
//        let idx = Discrete.Categorical.SampleUnchecked p
//        ret[idx] <- ret[idx] + 1
//    ret

//let n = 100000  // Large n to reduce variance
//let probabilities = [| 0.2; 0.3; 0.5 |]
////// Act
////let sampleCounts = Categorical_SampleUnchecked probabilities

////probabilities
////|> Array.iteri (fun i p ->
////    let observedProportion = float sampleCounts.[i] / float n
////    printfn $"Probability of {i}: {p}, Observed proportion: {observedProportion}")



////let n = 100000  // Larger n to reduce sampling variance
//let sample = SampleUnchecked probabilities n

//let () = 
//    probabilities
//    |> Array.iteri (fun i p ->
//        let observedProportion = float sample.[i] / float n
//        printfn $"Probability of {i}: {p}, Observed proportion: {observedProportion}")



//Discrete.Multinomial.ProbabilityMassToCumulativeDistribution probabilities


//let KDiagonal1 =
//    [|
//        [|1.;0.;0.|]
//        [|0.;1.;0.|]
//        [|0.;0.;1.|]
//    |]
//    |> Matrix.ofJaggedArray

//let BNegInf =
//    [|
//        [|-infinity;-infinity;-infinity|]
//        [|-infinity;-infinity;-infinity|]
//        [|-infinity;-infinity;-infinity|]
//    |]
//    |> Matrix.ofJaggedArray


//Algebra.LinearAlgebra.solveTriangularLinearSystems KDiagonal1 BNegInf false


//KDiagonal1 
//|> Matrix.mapiCols (fun i v -> 
//                        let m = Vector.norm v
//                        [|m|])

//let expected =
//    matrix [|
//        [|nan;nan;nan|];
//        [|nan;nan;nan|];
//        [|-infinity;-infinity;-infinity|]
//    |]


////|> fun res ->
////    let expected =
////        matrix [|
////            [|nan;nan;nan|];
////            [|nan;nan;nan|];
////            [|-infinity;-infinity;-infinity|]
////        |]



//let alpha = 9.9 //0.4 
//let beta  = 31 //4.2
//// https://keisan.casio.com/exec/system/1180573216
//let pdfs = [| 0.987113653; 0.635929273; 0.486870787; 0.400046182; 0.341683319;
//                0.299071263; 0.266235685; 0.239955525; 0.218322701; 0.200126249;
//                0.184555971; 0.171046668; 0.159190450; 0.148684554; 0.139298865;
//                0.130854902; 0.123211796; 0.116256647; 0.109897748; 0.104059710;
//                0.098679897; 0.093705765; 0.089092854; 0.084803247; 0.080804376;
//                0.077068078; 0.073569861; 0.070288299; 0.067204554; 0.064301989;
//                0.061565838; 0.058982949; 0.056541557; 0.054231102; 0.052042076;
//                0.049965886; 0.047994748; 0.046121587; 0.044339960; 0.042643979;
//                0.041028256; 0.039487846; 0.038018205; 0.036615142; 0.035274793;
//                0.033993583; 0.032768200; 0.031595571; 0.030472842; 0.029397355;
//                0.028366635; 0.027378369; 0.026430398; 0.025520703; 0.024647389;
//                0.023808683; 0.023002918; 0.022228528; 0.021484040; 0.020768066;
//                0.020079300; 0.019416507; 0.018778524; 0.018164249; 0.017572643;
//                0.017002719; 0.016453546; 0.015924240; 0.015413961; 0.014921914;
//                0.014447344; 0.013989532; 0.013547795; 0.013121484; 0.012709981;
//                0.012312696; 0.011929068; 0.011558563; 0.011200670; 0.010854903;
//                0.010520795; 0.010197904; 0.009885805; 0.009584092; 0.009292377;
//                0.009010290; 0.008737475; 0.008473592; 0.008218316; 0.007971333;
//                0.007732346; 0.007501068; 0.007277223; 0.007060548; 0.006850789;
//                0.006647704; 0.006451059; 0.006260630; 0.006076203; 0.005897569; |]

////let xy_pdf = pdfs |> Array.mapi (fun i v -> (float (i + 1) / 10.), Continuous.Gamma.PDF alpha beta (float (i + 1) / 10.))
//let samplesHisto = Array.init 999999 (fun _ -> Continuous.Gamma.Sample alpha beta)
 
//let bw       = 1.//FSharp.Stats.Distributions.Bandwidth.forHistogram samplesHisto
//let histo    = FSharp.Stats.Distributions.KernelDensity.estimate KernelDensity.Kernel.gaussian bw samplesHisto
//let histoPdf = histo |> Seq.map (fun (x,y) -> x,Continuous.Gamma.PDF alpha beta x)
////[
////    Chart.Column(histo)
////    Chart.Point(histoPdf)
////]
////|> Chart.combine
////|> Chart.show


//let alpha', beta' = Continuous.Gamma.Fit samplesHisto

//let d = Continuous.Gamma.Estimate pdfs
//d.Mean












