namespace FSharp.Stats.Algebra


open System
open FSharp.Stats

type LinearAlgebra =

    /// Subtract `scaleVal * src[srcOffset..srcOffset+count-1]` from
    /// `dst[dstOffset..dstOffset+count-1]` in place.
    static member inline subScaledRowInPlace
        (scaleVal   : 'T)
        (dstOffset  : int)
        (srcOffset  : int)
        (count      : int)
        (dst        : 'T[])
        (src        : 'T[]) =
        
        let scaleVec = Numerics.Vector<'T>(scaleVal)

        // Vector-level callback:  dstVec - (scaleVal * srcVec)
        let fv (dstVec: Numerics.Vector<'T>) (srcVec: Numerics.Vector<'T>) =
            dstVec - (scaleVec * srcVec)
        
        // Scalar fallback: d - (scaleVal * s)
        let f (d: 'T) (s: 'T) =
            d - (scaleVal * s)
        Acceleration.SIMDRangeUtils.map2RangeInPlace fv f dstOffset srcOffset count dst src


    //static member inline householderTransform
    //    (A: Matrix<'T>) (i: int) : Vector<'T> =
    //    let n = A.NumRows
        
    //    let v = Vector.zeroCreate<'T> n
    //    let aCol = Matrix.getCol i A
    //    let norm = Vector.norm aCol.[i..]  // ToDO: use a more efficient norm calculation 
    //    v.[i] <- aCol.[i] + if aCol.[i] >= 'T.Zero then norm else -norm
    //    for j = i + 1 to n - 1 do
    //        v.[j] <- aCol.[j]
    //    v        
        
    

    /// <summary>QR decomposition using modified Gram-Schmidt</summary>
    /// <remarks>Returns Q and R such that A = QR</remarks>
    static member inline qrModifiedGramSchmidt<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T : equality
                and 'T :> ValueType
                and 'T :> Numerics.IRootFunctions<'T>>
        (A: Matrix<'T>) : Matrix<'T> * Matrix<'T> =

        let m, n = A.NumRows, A.NumCols
        
        if m < n then
            invalidArg "A" $"QR decomposition via Modified Gram-Schmidt requires m ≥ n, but got {m}×{n} matrix."


        let r = Matrix.zeroCreate n n
        let qCols: Vector<'T>[] = Array.zeroCreate n
        let aCols = Matrix.getCols A

        for j = 0 to n - 1 do
            let v = Array.copy aCols.[j]

            for i = 0 to j - 1 do
                let qi = qCols.[i]
                let rij = Vector.dot qi v
                r.[i, j] <- rij
                for k = 0 to m - 1 do
                    v.[k] <- v.[k] - rij * qi.[k]

            let norm = Vector.norm v 
            r.[j, j] <- norm
            let qj = Vector.divideScalar norm v 
            qCols.[j] <- qj

        let qData = Array.zeroCreate (m * n)
        for j = 0 to n - 1 do
            for i = 0 to m - 1 do
                qData.[i * n + j] <- qCols.[j].[i]

        Matrix(m, n, qData), r

    /// <summary>Back substitute to solve R * x = y</summary>
    /// <remarks>R is upper triangular</remarks>
    static member inline backSubstitute<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T : equality
                and 'T :> ValueType>
        (r: Matrix<'T>) 
        (y: Vector<'T>) : Vector<'T> =

        let n = r.NumRows

        if r.NumCols <> n || y.Length <> n then
            invalidArg "dimensions" "R must be square and match the length of y"

        let x = Array.zeroCreate<'T> n

        for i = n - 1 downto 0 do
            let mutable sum = y.[i]
            for j = i + 1 to n - 1 do
                sum <- sum - r.[i, j] * x.[j]
            let diag = r.[i, i]
            if diag = 'T.Zero then
                invalidArg $"r[{i},{i}]" "Diagonal element is zero. Cannot divide."            
            x.[i] <- sum / diag

        x




    /// Solve A * x = b for x, where A is a square matrix (n×n) and b is a vector (length n).
    static member inline solveLinearQR<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T : equality
                and 'T :> ValueType
                and 'T :> Numerics.IRootFunctions<'T>>
        (A: Matrix<'T>) 
        (b: Vector<'T>) : Vector<'T> =

        if A.NumRows <> b.Length then
            invalidArg "b" "Vector length must match number of rows in matrix A"

        let Q, R = LinearAlgebra.qrModifiedGramSchmidt A
        let Qt = Q.Transpose()
        let y = Matrix.muliplyVector Qt b
        LinearAlgebra.backSubstitute R y



    /// Solve K * X = B for X, where K is a triangular matrix (lower or upper).
    /// K is square of size n×n; B is n×m. Returns an n×m result X.
    static member inline solveTriangularLinearSystems
        (K       : Matrix<'T>)
        (B       : Matrix<'T>)
        (isLower : bool)
        : Matrix<'T> =

        /// Multiply `arr[offset..offset+count-1]` by `scaleVal` in place
        let inline scaleRowInPlace
            (scaleVal  : 'T)
            (offset    : int)
            (count     : int)
            (arr       : 'T[]) =
        
            let scaleVec = Numerics.Vector<'T>(scaleVal)

            // Vector-level callback: chunk * scaleVal
            let fv (chunk: Numerics.Vector<'T>) =
                chunk * scaleVec

            // Scalar fallback: x * scaleVal
            let f (x: 'T) = x * scaleVal

            // mapRangeInPlace fv f offset count arr
            Acceleration.SIMDRangeUtils.mapRangeInPlace fv f offset count arr  

        // Check shape consistency
        let nK, mK = K.NumRows, K.NumCols
        let nB, mB = B.NumRows, B.NumCols
        if nK <> mK || nB <> nK then
            invalidArg "Matrix" "K must be square and B must have matching row count for K"

        // Copy B into a new matrix X (the solution)
        let X = Matrix<'T>(B.NumRows, B.NumCols, Array.copy B.Data)

        let n = nK  // size of the NxN triangular matrix K
        let m = mB  // # of columns in B (and thus X)

        let Kdata = K.Data
        let Xdata = X.Data

        // For row i, the contiguous slice in Xdata is [i*m .. i*m + m - 1]
        // => offset = i*m, length = m

        if isLower then
            // ------ Forward Substitution ------
            // for i in [0..n-1]:
            //   X[i,*] <- X[i,*] - Σ_{j=0..i-1} [K[i,j] * X[j,*]]
            //   then scale row i by 1/K[i,i]
            for i = 0 to n - 1 do
                let baseI = i * m
                // Subtract scaled rows for j in [0..i-1]
                for j = 0 to i - 1 do
                    let kij = Kdata.[i * n + j]  // K[i,j]
                    //if kij <> 'T.Zero then // normaly speed things up, but -inf * 0 = nan
                    let baseJ = j * m
                    // subScaledRowInPlace scaleVal dstOffset srcOffset count dst src
                    LinearAlgebra.subScaledRowInPlace
                        kij 
                        baseI  // row i offset in X
                        baseJ  // row j offset in X
                        m      // number of columns
                        Xdata  
                        Xdata
                    //LinearAlgebra.subScaledRowInPlace
                        //    Xdata
                        //    baseI  // row i offset in X
                        //    Xdata
                        //    baseJ  // row j offset in X
                        //    m      // number of columns
                        //    kij  
                            
                            
    
                // Divide row i by the diagonal K[i,i]
                let diag = Kdata.[i * n + i]
                let invDiag = 'T.One / diag
                // scaleRowInPlace scaleVal offset count arr
                scaleRowInPlace
                    invDiag
                    baseI
                    m
                    Xdata
        else
            // ------ Backward Substitution ------
            // for i in [n-1..0]:
            //   X[i,*] <- X[i,*] - Σ_{j=i+1..n-1} [K[i,j] * X[j,*]]
            //   then scale row i by 1/K[i,i]
            for i = n - 1 downto 0 do
                let baseI = i * m
                for j = i + 1 to n - 1 do
                    let kij = Kdata.[i * n + j]  // K[i,j]
                    //if kij <> 'T.Zero then // normaly speed things up, but -inf * 0 = nan
                    let baseJ = j * m
                    LinearAlgebra.subScaledRowInPlace
                        kij 
                        baseI
                        baseJ
                        m
                        Xdata
                        Xdata
                let diag = Kdata.[i * n + i]
                let invDiag = 'T.One / diag
                scaleRowInPlace
                    invDiag
                    baseI
                    m
                    Xdata

        X



    /// Solve K * x = v (triangular system) in-place, returning a copy of x.
    /// K must be n×n, v must be length n. 
    /// isLower = true => forward substitution
    /// isLower = false => backward substitution
    static member inline solveTriangularLinearSystem
        (K       : Matrix<'T>)
        (v       : Vector<'T>)
        (isLower : bool)
        : Vector<'T> =

        let nK, mK = K.NumRows, K.NumCols
        let nV = v.Length
        if nK <> mK || nV <> nK then
            invalidArg (nameof K) "K must be square, and v must match its dimension."

        let x = Array.copy v
        let Kdata = K.Data  // row-major flattened

        // Forward or backward substitution
        if isLower then
            // For i in [0..n-1]:
            //   x[i] <- ( x[i] - sum_{j=0..i-1}(K[i,j] * x[j]) ) / K[i,i]
            for i = 0 to nK - 1 do
                let mutable s = x.[i]
                let rowOffset = i * nK
                for j = 0 to i - 1 do
                    s <- s - (Kdata.[rowOffset + j] * x.[j])
                let diag = Kdata.[rowOffset + i]
                if diag = 'T.Zero then
                    invalidArg $"K[{i},{i}]" "Diagonal element is zero. Cannot divide."
                x.[i] <- s / diag
        else
            // For i in [n-1..downto..0]:
            //   x[i] <- ( x[i] - sum_{j=i+1..n-1}(K[i,j] * x[j]) ) / K[i,i]
            for i = nK - 1 downto 0 do
                let mutable s = x.[i]
                let rowOffset = i * nK
                for j = i + 1 to nK - 1 do
                    s <- s - (Kdata.[rowOffset + j] * x.[j])
                let diag = Kdata.[rowOffset + i]
                if diag = 'T.Zero then
                    invalidArg $"K[{i},{i}]" "Diagonal element is zero. Cannot divide."
                x.[i] <- s / diag

        x



    ///// QR decomposition using Householder reflections
    //static member inline qrDecompose (A : Matrix<'T>) : (Matrix<'T> * Matrix<'T>) =
    //// former QR
    //    let updateQ (Q : Matrix<'T>) (v : Vector<'T>) =
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

    //    // TODO: Refector in Householder module
    //    let normalize (v: Vector<'T>) : Vector<'T> =
    //        let norm = Vector.norm v
    //        if norm = 'T.Zero then v
    //        else
    //            Array.map (fun x -> x / norm) v

    //    let m, n = A.NumRows, A.NumCols

    //    // Q starts as identity(n)
    //    let Q = Matrix.identity m
    //    let R = Matrix.copy A

    //    for i = 0 to (min n m) - 1 do
    //        let x = [| for k in i .. m - 1 -> R.[k, i] |]
    //        //let a = Matrix.getCol i R
    //        let hh = Householder.create x //a.[i..] Create Householder reflector for column i
    //        let v = hh.V |> normalize             
    //        updateQ Q v
    //        Householder.applyLeft(hh, R, i)            


    //    Q, R


    /// QR decomposition using Householder reflections
    static member inline qrDecompose (A : Matrix<'T>) : (Matrix<'T> * Matrix<'T>) =
    // former QR

        /// Compute normalized Householder vector from a subcolumn x
        let householderVector (x: 'T[]) : 'T[] =
            let norm = sqrt (Array.sumBy (fun xi -> xi * xi) x)
            let v = Array.copy x
            v.[0] <- v.[0] + (if x.[0] >= 'T.Zero then norm else -norm)
            let norm_v = sqrt (Array.sumBy (fun vi -> vi * vi) v)
            if norm_v = 'T.Zero then v
            else Array.map (fun vi -> vi / norm_v) v

        /// Update Q: Q ← Q * Hᵢ using Householder vector v (from column i)
        let updateQ (Q: Matrix<'T>) (v: 'T[]) (i: int) =
            let nQ, mQ = Q.NumRows, Q.NumCols
            for row = 0 to nQ - 1 do
                let mutable dot = 'T.Zero
                for k = 0 to v.Length - 1 do
                    dot <- dot + Q.[row, i + k] * v.[k]
                let alpha = dot + dot
                for k = 0 to v.Length - 1 do
                    Q.[row, i + k] <- Q.[row, i + k] - alpha * v.[k]

        /// Apply Hᵢ to R from the left: R ← H * R
        let applyHouseholderLeft (R: Matrix<'T>) (v: 'T[]) (i: int) =
            let m, n = R.NumRows, R.NumCols
            for col = i to n - 1 do
                let mutable dot = 'T.Zero
                for k = 0 to v.Length - 1 do
                    let row = i + k
                    if row < m then
                        dot <- dot + v.[k] * R.[row, col]
                let alpha = dot + dot
                for k = 0 to v.Length - 1 do
                    let row = i + k
                    if row < m then
                        R.[row, col] <- R.[row, col] - alpha * v.[k]

        /// Main QR decomposition function
        let qrDecompose (A: Matrix<'T>) : Matrix<'T> * Matrix<'T> =
            let m, n = A.NumRows, A.NumCols
            let Q = Matrix.identity m
            let R = Matrix.copy A

            for i = 0 to min m n - 1 do
                let x = [| for k in i .. m - 1 -> R.[k, i] |]
                let v = householderVector x
                updateQ Q v i
                applyHouseholderLeft R v i

            Q, R
        
        qrDecompose A




    /// <summary>Given A[m,n] and B[m] solves AX = B for X[n].<br />
    /// When m =&gt; n, have over constrained system, finds least squares solution for X.<br />
    /// When m &lt; n, have under constrained system, finds least norm solution for X.</summary>
    static member inline leastSquares 
        (A : Matrix<'T>) 
        (b: Vector<'T>) =
    // Maybe rename to leastSquaresQR?
        let (m,n) = A.NumRows, A.NumCols
        
        // Is this an overdetermined or underdetermined system?
        if m >= n then
            //printfn "Least squares: solving %dx%d system with %d equations." m n n
            let Qm, R = LinearAlgebra.qrDecompose A
            let Qtb = Qm.Transpose() * b
            LinearAlgebra.solveTriangularLinearSystem R.[0..n-1,0..n-1] Qtb.[0..n-1] false
        else
            // underdetermined: solve A^T * x = 0 with min ||x||
            //printfn "underdetermined- Least squares: solving %dx%d system with %d equations." m n n
            let AT = A.Transpose()
            let Q, R = LinearAlgebra.qrDecompose AT
            let RT   = R.Transpose()
            let s    = LinearAlgebra.solveTriangularLinearSystem RT.[0..m-1, 0..m-1] b true
            Q.[0.., 0..m-1] * s

            //let AT = A.Transpose()
            //let Q, R = LinearAlgebra.qrDecompose AT
            //let y = Q.Transpose() * b
            //let s = LinearAlgebra.solveTriangularLinearSystem R.[0..m-1, 0..m-1] y.[0..m-1] false
          
            //Q.[0..n,0..m] * s




            //let AT = A.Transpose()
            //let QT, RT = LinearAlgebra.qrDecompose AT
            //let y = QT.Transpose() * b
            //let s = LinearAlgebra.solveTriangularLinearSystem RT.[0..m-1, 0..m-1] y.[0..m-1] false
            //let s = LinearAlgebra.solveTriangularLinearSystem R.[0..m-1,0..m-1] Qtb false
            //Vector.init n (fun i -> if i < m then s.[i] else 'T.Zero)

            //// underdetermined: solve min ||x|| such that Ax = b
            //let AT = A.Transpose()
            //let QT, RT = LinearAlgebra.qrDecompose AT
            //let y = LinearAlgebra.solveTriangularLinearSystem (RT.Transpose().[0..m-1, 0..m-1]) b false
            //QT.[*, 0..m-1] * y


    /// <summary>
    /// Computes the Cholesky factor L of a symmetric positive-definite matrix A,
    /// returning L as a lower-triangular <see cref="Matrix{T}"/> such that A = L * L^T.
    /// Throws if A is not positive-definite (i.e., if any diagonal element ≤ 0).
    /// </summary>
    static member inline cholesky
            (A : Matrix<'T>)
            : Matrix<'T> =

        let n = A.NumRows
        let m = A.NumCols
        if n <> m then
            invalidArg (nameof A) "Cholesky: matrix must be square."

        let dataA = A.Data
        // We'll create an n×n zeroed matrix for L
        let Ldata = Array.zeroCreate<'T> (n * n)

        // Helper function for indexing row-major arrays:
        let inline idx r c = r * n + c

        for j = 0 to n - 1 do
            // 1) Diagonal element L[j,j]
            //    L[j,j] = sqrt(A[j,j] - ∑(k=0..j-1) L[j,k]^2)
            let mutable sumjj = dataA.[idx j j]
            for k = 0 to j - 1 do
                let ljk = Ldata.[idx j k]
                sumjj <- sumjj - (ljk * ljk)

            // Check positivity
            if sumjj <= 'T.Zero then
                invalidArg "A" "Cholesky: matrix not positive-definite (diagonal <= 0)."

            let ljj = GenericMath.sqrt sumjj
            Ldata.[idx j j] <- ljj

            // 2) Off-diagonal: L[i,j] for i=j+1..n-1
            //    L[i,j] = (A[i,j] - ∑(k=0..j-1) L[i,k]*L[j,k]) / L[j,j]
            for i = j + 1 to n - 1 do
                let mutable sumij = dataA.[idx i j]
                for k = 0 to j - 1 do
                    sumij <- sumij - (Ldata.[idx i k] * Ldata.[idx j k])
                Ldata.[idx i j] <- sumij / ljj

        Matrix<'T>(n, n, Ldata)


    /// <summary>Given A[m,n] and b[m] solves AX = b for X[n].<br />
    /// When the system is under constrained,<br />
    /// for example when the columns of A are not linearly independent,<br />
    /// then it will not give sensible results.</summary>
    static member inline leastSquaresCholesky 
        (A : Matrix<'T>) 
        (b: Vector<'T>) =

        if b.Length <> A.NumRows then
            invalidArg "b" "Length of b must match the number of rows of A."

        let AT = A.Transpose()

        let upper = (AT * A) |> LinearAlgebra.cholesky
        let gamma =
            LinearAlgebra.solveTriangularLinearSystem(upper) (AT * b) true
        let beta =
            LinearAlgebra.solveTriangularLinearSystem (upper.Transpose()) gamma false
        beta


    /// <summary>computes the hat matrix by the QR decomposition of the designmatrix used in ordinary least squares approaches</summary>
    static member inline hatMatrix 
        (designMatrix: Matrix<'T>) = 
        let qm,R = LinearAlgebra.qrDecompose designMatrix
        let q1 = qm.GetSlice ((Some 0),(Some (qm.NumRows-1)),(Some 0),(Some (R.NumCols-1)))
        // computes the hatmatrix 
        q1 * q1.Transpose()

    
    /// <summary>computes the leverages of every dataPoint of a dataSet given by the diagonal of the hat matrix. </summary>
    static member inline leverageBy 
        (hatMatrix: Matrix<'T>) = 

        Matrix.getDiagonal hatMatrix


    /// <summary>computes the leverage directly by QR decomposition of the designmatrix used in ordinary least squares approaches<br />
    /// and computing of the diagnonal entries of the Hat matrix, known as the leverages of the regressors</summary>
    static member inline leverage 
        (designMatrix: Matrix<'T>) = 
        
        let qm,R = LinearAlgebra.qrDecompose designMatrix
        let q1 = qm.GetSlice ((Some 0),(Some (qm.NumRows-1)),(Some 0),(Some (R.NumCols-1)))
        
        Vector.init q1.NumRows (fun i ->
            let mutable sumOfSquares = 'T.Zero
            for j = 0 to q1.NumCols - 1 do
                let x = q1.[i, j]
                sumOfSquares <- sumOfSquares + x * x
            sumOfSquares
        )


    /// <summary>
    /// Computes the LU factorization of a square matrix A with partial pivoting.
    /// That is, P * A = L * U, where P is a permutation, and L, U are lower/upper triangular.
    /// Returns (Permutation P, L, U).
    /// </summary>
    static member inline luDecompose (A : Matrix<'T>) =
        let n, m = A.NumRows, A.NumCols
        if n <> m then
            invalidArg (nameof A) "LU: A must be square."

        let U = Matrix.copy A           // We'll transform U in place.
        let L = Matrix.zeroCreate n n   // We'll fill L below the diagonal.
        let P = [| 0 .. n-1 |]          // Pivot array

        // Row-swap function for U: swap entire row i, row j
        let swapRows (M: Matrix<'T>) i j =
            let rowI = i
            let rowJ = j
            for col = 0 to m - 1 do
                let tmp = M.[rowI, col]
                M.[rowI, col] <- M.[rowJ, col]
                M.[rowJ, col] <- tmp

        // Row-swap function for L: only swap the columns up to i-1 (since the rest is not yet set).
        // Because in Doolittle's method, columns [0..i-1] of L are already determined at pivot step i.
        let swapRowsPartial (M: Matrix<'T>) i j pivotCol =
            for col = 0 to pivotCol - 1 do
                let tmp = M.[i, col]
                M.[i, col] <- M.[j, col]
                M.[j, col] <- tmp

        // Perform the decomposition
        for i = 0 to n - 2 do
            // 1) Find pivot row by max absolute value in U column i
            let mutable pivotRow = i
            let mutable pivotVal = abs(U.[pivotRow, i])
            for r = i + 1 to n - 1 do
                let candidate = abs(U.[r, i])
                if candidate > pivotVal then
                    pivotVal <- candidate
                    pivotRow <- r

            // 2) If pivotRow != i, swap rows in U, swap partial in L, and update pivot array
            if pivotRow <> i then
                swapRows U i pivotRow
                swapRowsPartial L i pivotRow i
                let tmp = P.[i]
                P.[i] <- P.[pivotRow]
                P.[pivotRow] <- tmp

            // 3) Eliminate below pivot
            for j = i + 1 to n - 1 do
                // L[j,i] = U[j,i] / U[i,i]
                L.[j,i] <- U.[j,i] / U.[i,i]
                // Update row j of U
                for k = i + 1 to n - 1 do
                    U.[j,k] <- U.[j,k] - L.[j,i] * U.[i,k]
                U.[j,i] <- 'T.Zero

        // Add identity to L (so diagonal = 1.0)
        // i.e., L[i,i] <- 1.0
        for i = 0 to n - 1 do
            L.[i,i] <- 'T.One

        // Return the permutation (as an array or your custom Permutation type),
        // plus the final L and U
        (Permutation.ofArray P, L, U)

    /// <summary>
    /// Solves the system of linear equations A * X = B using LU factorization.
    /// A must be square; B must have the same number of rows as A. 
    /// Returns a matrix X s.t. A*X = B.
    /// </summary>
    static member inline solveLinearSystems (A: Matrix<'T>) (B: Matrix<'T>) =
        let nA, mA = A.NumRows, A.NumCols
        let nB, mB = B.NumRows, B.NumCols

        // 1) Check shape: A must be square, B's rows must match A's rows
        if nA <> mA then
            invalidArg (nameof A) "Matrix A must be square."
        if nB <> nA then
            invalidArg (nameof B) "Matrix B must have same number of rows as A."

        // 2) Factor A -> (P, L, U)
        let (P, L, U) = LinearAlgebra.luDecompose A

        // 3) Permute B according to P (i.e., reorder rows)
        let Bpermuted = B |> Matrix.permuteRowsBy P

        // 4) Forward substitution: solve L * Y = Bpermuted
        let Y = LinearAlgebra.solveTriangularLinearSystems L Bpermuted true  // isLower = true

        // 5) Back substitution: solve U * X = Y
        let X = LinearAlgebra.solveTriangularLinearSystems U Y false         // isLower = false

        X

    /// <summary>
    /// Solves the system A * x = b for x, using LU factorization with partial pivoting.
    /// A must be square, and b must have length = A.NumRows.
    /// </summary>
    static member inline solveLinearSystem (A : Matrix<'T>) (b : Vector<'T>) =
        let n, m = A.NumRows, A.NumCols
        if n <> m then
            invalidArg (nameof A) "Matrix A must be square."
        if b.Length <> n then
            invalidArg (nameof b) "Vector b must have length = A.NumRows."

        // 1) Factor A => (P, L, U)
        let (P, L, U) = LinearAlgebra.luDecompose A

        // 2) Permute b according to P
        let bPermuted = b |> Vector.permuteBy P 

        // 3) Forward solve: L * y = bPermuted
        let y = LinearAlgebra.solveTriangularLinearSystem L bPermuted true  // isLower = true

        // 4) Back solve: U * x = y
        let x = LinearAlgebra.solveTriangularLinearSystem U y false         // isLower = false

        x

    /// <summary>
    /// Computes the inverse of a square matrix A using its LU factorization.
    /// If A is n×n, we factor A = P * L * U, then solve A * X = I for X, returning X = A^-1.
    /// </summary>
    /// <param name="A">A square matrix of size n×n.</param>
    /// <returns>The inverse of A, an n×n matrix.</returns>
    /// <exception cref="System.ArgumentException">
    /// Thrown if A is not square, or if factorization fails (e.g., if A is singular).
    /// </exception>
    static member inline inverse (A: Matrix<'T>) : Matrix<'T> =
        let n, m = A.NumRows, A.NumCols
        if n <> m then
            invalidArg (nameof A) "Matrix must be square when computing its inverse."

        // 1) Factor A => P, L, U
        let (P, L, U) = LinearAlgebra.luDecompose A

        // 2) Build the identity matrix I, size n
        let I = Matrix.identity n

        // 3) Permute I's rows by P (so effectively P*I)
        let IPerm = I |> Matrix.permuteRowsBy P

        // 4) Forward substitution: solve L * Y = IPerm
        let Y = LinearAlgebra.solveTriangularLinearSystems L IPerm true

        // 5) Back substitution: solve U * X = Y
        let X = LinearAlgebra.solveTriangularLinearSystems U Y false

        // X is A^-1
        X


    /// <summary>
    /// Computes the Moore-Penrose pseudoinverse of a matrix using a QR-based approach. 
    /// If the matrix is overdetermined (m > n), returns (R⁻¹ Qᵀ) for A = Q R with A[m×n]. 
    /// If underdetermined (m < n), uses the transpose trick, then returns the transpose of the partial solution.
    /// </summary>
    /// <param name="matrix">An m×n matrix.</param>
    /// <returns>The (n×m) pseudoinverse of <paramref name="matrix"/>.</returns>
    static member inline pseudoInvers (matrix: Matrix<'T>) =
        let m, n = matrix.NumRows, matrix.NumCols

        // Overdetermined: A is m×n with m > n
        if m > n then
            // A = Q (m×m) * R (m×n)  [or economy size Q (m×n), R (n×n)]
            let qm, R = LinearAlgebra.qrDecompose matrix
            // Instead of multiplying qm.Transpose by identity(m×m), just use qm.Transpose
            // Next, we want sub-blocks: R is m×n, but we only need the top n×n portion,
            // and we want the top-left n×m portion of qm.Transpose.
            let Qt = qm.Transpose()
            // Solve R[0..n-1, 0..n-1] * X = Qt[0..n-1, 0..m-1]  (back-substitution)
            LinearAlgebra.solveTriangularLinearSystems 
                R.[0..n-1, 0..n-1] 
                Qt.[0..n-1, 0..m-1] 
                false

        // Underdetermined: A is m×n with m < n
        else
            // We do matrix.Transpose => n×m
            // Then QR => qm (n×n?), R (n×m?), etc.
            let qm, R = LinearAlgebra.qrDecompose (matrix.Transpose())
            // Again skip identity multiply: just use qm.Transpose
            let Qt = qm.Transpose()
            // Solve R[0..m-1, 0..m-1] * X = Qt[0..m-1, 0..n-1]
            let s = 
                LinearAlgebra.solveTriangularLinearSystems 
                    R.[0..m-1, 0..m-1]
                    Qt.[0..m-1, 0..n-1]
                    false
            // Return sᵀ => the actual pseudoinverse shape (n×m)
            s.Transpose()


    /// <summary>
    /// Computes the determinant of a square matrix A by factoring A = P * L * U,
    /// then det(A) = sign(P) * ∏ diag(U). 
    /// (Assumes L has diag=1, as in Doolittle.)
    /// </summary>
    static member inline determinant (A: Matrix<float>) : float =
        let n = A.NumRows
        if n <> A.NumCols then
            invalidArg (nameof A) "Matrix must be square."

        // 1) Factor A => (P, L, U) 
        let (P, L, U) = LinearAlgebra.luDecompose A

        // 2) Compute sign from the permutation P with domain size n
        let permSign = Permutation.sign n P

        // 3) Product of diag(U)
        let diagProd =
            let mutable product = 1.0
            for i = 0 to n - 1 do
                product <- product * U.Data.[i*n + i]
            product

        // 4) Return sign(P) * product
        permSign * diagProd


    static member inline SVD (a:Matrix<float>) =
        let (umatrix,s,vmatrix) = SVD.computeInPlace (a.toArray2D())
        //Matrix.diag
        s,Matrix.ofArray2D umatrix,Matrix.ofArray2D vmatrix
        //(Matrix.ofArray2D umatrix,s,Matrix.ofArray2D vmatrix)
    
    static member inline symmetricEigenspectrum (a:Matrix<float>) = 
        let (e,v,d) = EVD.symmetricEvd (a.toArray2D())
        (Matrix.ofArray2D v, d)

    ///// Synonym: kernel / right null space. Returns an orthonormal basis for the null space of matrix A (Ax = 0).<br />The accuracy defines a threshold whether a singular value is considered as zero (default: 1e-08).
    //static member nullspace(?Accuracy :float ) = 

    //    let accuracy = defaultArg Accuracy 1e-08

    //    fun (a: Matrix<float>) -> 
                        
    //        // Either MKL or fallback implementation of the full SVD
    //        let (sigma,U,Vt) = LinearAlgebra.SVD a

    //        // The rank is the number of nonzero singular values
    //        let rank = 
    //            sigma
    //            |> Seq.sumBy (fun x -> if x >= accuracy then 1 else 0)

    //        let count = Vt.NumRows - rank 

    //        Matrix.getRows Vt rank count
    //        |> Matrix.transpose