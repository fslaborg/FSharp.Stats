namespace FSharp.Stats.Algebra

open FSharp.Stats
open System


module LinearAlgebra = 
    
    type Factorization = matrix -> (matrix*matrix*matrix)

    //let private LinearAlgebraService = 
    //    let tmp = new ServiceLocator.ServiceProvider<ILinearAlgebra>([ProviderService.MKLProvider])
    //    tmp.Start()
    //    tmp
    let private MKLService = new ServiceLocator.ServiceProvider<ILinearAlgebra>([ProviderService.LAPACKProvider;(*ProviderService.MKLProvider*)])


    let Service() = 
        match MKLService.Service() with
        | Some svc -> svc
        | None     -> failwith "MKL service either not available, or not started"
    
    let private HaveService() =
        MKLService.Available()
    
    let SolveTriangularLinearSystem (A:matrix) (b:vector) (isLower:bool) =
        //if HaveService() then LinearAlgebraService.solveTriangularForVector A b isLower
        //                    else LinearAlgebraManaged.SolveTriangularLinearSystem A b isLower
        LinearAlgebraManaged.SolveTriangularLinearSystem A b isLower
                   
    let SolveTriangularLinearSystems (A:matrix) (B:matrix) (isLower:bool) =
        //if HaveService() then LinearAlgebraService.solveTriangularForMatrix A B isLower
        //                    else LinearAlgebraManaged.SolveTriangularLinearSystems A B isLower
        LinearAlgebraManaged.SolveTriangularLinearSystems A B isLower
  
    let SolveLinearSystem (A:matrix) (b:vector) =
        //if HaveService() then LinearAlgebraService.preDivideByVector A b
        //                    else LinearAlgebraManaged.SolveLinearSystem A b
        LinearAlgebraManaged.SolveLinearSystem A b
                   
    let SolveLinearSystems (A:matrix) (B:matrix) =
        //if HaveService() then LinearAlgebraService.preDivideByMatrix A B
        //                    else LinearAlgebraManaged.SolveLinearSystems A B
        LinearAlgebraManaged.SolveLinearSystems A B

    /// <summary>Given A[n,m] and B[n] solve for x[m] such that Ax = B<br />This call may fail.</summary>
    /// <remarks></remarks>
    /// <param name="A"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let preDivideByVector A b = 
        //if HaveService() then LinearAlgebraService.preDivideByVector A b
        //                    else LinearAlgebraManaged.SolveLinearSystem A b
        LinearAlgebraManaged.SolveLinearSystem A b
    

    /// <summary>Given A[n,m] and B[n,k] solve for X[m,k] such that AX = B<br />This call may fail.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let preDivideByMatrix a b = 
        //if HaveService() then LinearAlgebraService.preDivideByMatrix a b
        //                    else LinearAlgebraManaged.SolveLinearSystems a b
        LinearAlgebraManaged.SolveLinearSystems a b
    
    ///Compoutes for an N-by-N real nonsymmetric matrix A, the
    ///eigenvalue decomposition eigenvalues and right eigenvectors.
    ///The right eigenvector v(j) of A satisfies
    ///
    ///                 A * v(j) = lambda(j) * v(j)
    ///
    ///where lambda(j) is its eigenvalue.
    ///The computed eigenvectors are normalized to have Euclidean norm
    ///equal to 1 and largest component real. Uses the LAPACK subroutine dgeev with arguments JOBVR = 'V' and JOBVL = 'N'
    ///
    /// <summary>Returns the real (first array) and imaginary (second array) parts of the eigenvalues and a matrix containing the corresponding eigenvectors</summary>
    /// <remarks></remarks>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let EVD m = 
        //if HaveService() then 
            //Service().dgeev_(m)
        LinearAlgebraManaged.eigenvectors m
                           
    /// <summary>Compute eigenvalues of a square real matrix.<br />Returns arrays containing the eigenvalues which may be complex.<br />This call may fail.</summary>
    /// <remarks></remarks>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let EigenValues m =
        //if HaveService() then let evals = LinearAlgebraService.eigenvalues m
        //                        let n = evals.Length
        //                        Vector.Generic.init n (fun i -> evals.[i])
        //                    else LinearAlgebraManaged.eigenvalues m
        LinearAlgebraManaged.eigenvalues m

    /// <summary>Compute eigenvalues for a real symmetric matrix.<br />Returns array of real eigenvalues.<br />This call may fail.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let EigenValuesWhenSymmetric a =
        //if HaveService() then let evals = LinearAlgebraService.symmetricEigenvalues a
        //                        let n = evals.Length
        //                        Vector.init n (fun i -> evals.[i])
        //                    else LinearAlgebraManaged.symmetricEigenvalues a
        LinearAlgebraManaged.symmetricEigenvalues a
    
    /// <summary>Compute eigenvalues and eigenvectors for a real symmetric matrix.<br />Returns arrays of the values and vectors (both based on reals).<br />This call may fail.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let EigenSpectrumWhenSymmetric a =
        if HaveService() then 
            Service().dsyevd_(a)
        else 
            LinearAlgebraManaged.symmetricEigenvectors a

    
    /// <summary>Given A[n,n] find it's inverse.<br />This call may fail.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let Inverse a = 
        //if HaveService() then LinearAlgebraService.inverse a
        //                    else LinearAlgebraManaged.Inverse a
        LinearAlgebraManaged.Inverse a

    /// <summary>Given A[m,n] and B[m] solves AX = B for X[n].<br />When m =&gt; n, have over constrained system, finds least squares solution for X.<br />When m &lt; n, have under constrained system, finds least norm solution for X.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let LeastSquares a b =
        //if HaveService() then LinearAlgebraService.leastSquares a b
        //                    else LinearAlgebraManaged.leastSquares a b
        LinearAlgebraManaged.leastSquares a b
    
    /// <summary>Given A[m,n] and b[m] solves AX = b for X[n].<br />When the system is under constrained,<br />for example when the columns of A are not linearly independent,<br />then it will not give sensible results.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let LeastSquaresCholesky (a : Matrix<float>) (b : Vector<float>) = 
        LinearAlgebraManaged.leastSquaresCholesky a b

    /// <summary>Given A[n,n] real symmetric positive definite.<br />Finds the cholesky decomposition L such that L' * L = A.<br />May fail if not positive definite.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let Cholesky a = 
        //if HaveService() then LinearAlgebraService.Cholesky a
        //                    else LinearAlgebraManaged.Cholesky a
        LinearAlgebraManaged.Cholesky a
      
    /// <summary>Given A[n,n] real matrix.<br />Finds P,L,U such that L*U = P*A with L,U lower/upper triangular.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let LU a = 
        //if HaveService() then LinearAlgebraService.LU a
        //                    else LinearAlgebraManaged.LU a
        LinearAlgebraManaged.LU a
      

    /// <summary>Given A[m,n] finds Q[m,m] and R[k,n] where k = min m n.<br />Have A = Q.R  when m &lt; =n.<br />Have A = Q.RX when m &gt; n and RX[m,n] is R[n,n] row extended with (m-n) zero rows.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let QR a = 
        //if HaveService() then LinearAlgebraService.QR a
        //                    else LinearAlgebraManaged.QR a
        LinearAlgebraManaged.QR a

    /// Performs QR decomposition using an alternative algorithm.
    /// Returns the orthogonal matrix Q and the upper triangular matrix R.
    let qrAlternative (A: Matrix<float>) =
        let m: int = A.NumRows
        let n: int = A.NumCols

        let q: Matrix<float> = Matrix.zero m n
        let r: Matrix<float> = Matrix.zero n n
        let qLengths: Vector<float> = Vector.zeroCreate n

        let getVectorLength (v: Vector<float>) = Vector.fold (fun folder i -> folder+(i*i)) 0. v

        let setqOfA (n: int) =
            let aN: Vector<float> =  Matrix.getCol A n
            let qN = 
                if n = 0 then 
                    aN 
                else 
                    Array.init (n) (fun i -> 
                        let denominator = qLengths[i]
                        let forNominator: Vector<float> = Matrix.getCol q i 
                        let nominator: float = Vector.dot aN forNominator
                        r.[i, n] <- nominator
                        (nominator/denominator) * forNominator
                    )
                    |> Array.fold (fun folder  e -> folder-e ) aN
            Matrix.setCol q n qN
            qN  

        for i=0 to n-1 do
            let qN = setqOfA i 
            let qLength = getVectorLength qN
            let rValue = sqrt(qLength)
            r[i,i] <- rValue
            qLengths[i] <- qLength

        for i=0 to n-1 do
            let qN: Vector<float> = Matrix.getCol q i
            let updateQ = (1./sqrt( qLengths[i]  )) * qN 
            Matrix.setCol q i updateQ
            for j=i+1 to n-1 do
                let denominator = r[i, i]
                let nominator = r[i, j]
                r[i, j] <- (nominator/denominator)

        q, r

    /// Solves a linear system of equations using QR decomposition.
    /// 
    /// Parameters:
    ///   - A: The coefficient matrix of the linear system.
    ///   - t: The target vector of the linear system.
    /// 
    /// Returns:
    ///   - mX: The solution vector of the linear system.
    ///   - r: The upper triangular matrix obtained from QR decomposition.
    let solveLinearQR (A: Matrix<float>) (t: Vector<float>) =
        let m = A.NumRows
        let n = A.NumCols

        System.Diagnostics.Debug.Assert(m >= n) 

        let q,r = qrAlternative A 

        let QT = q.Transpose

        let mX = Vector.zeroCreate n

        let c: Vector<float> = QT * t

        let rec build_mX_inner cross_prod i j =
            if j=n then 
                cross_prod
            else
                let newCrossprod = cross_prod + (r[i, j] * mX[j])
                build_mX_inner newCrossprod i (j+1)
    
        let rec build_mX_outer i =
            if i<0 then 
                ()
            else
                let crossProd = build_mX_inner 0. i (i+1)
                mX[i] <- (c[i] - crossProd) / r[i, i]
                build_mX_outer (i-1)
    
        build_mX_outer (n-1)
    
        mX,r
    

    ///Returns the full Singular Value Decomposition of the input MxN matrix 
    ///
    ///A : A = U * SIGMA * V**T in the tuple (S, U, V**T), 
    ///
    ///where S is an array containing the diagonal elements of SIGMA.
    /// <summary>uses the LAPACK routine dgesdd with the argument JOBZ = 'A'</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let SVD (a:Matrix<float>) = 
        if HaveService() then 
            let S,U,Vt = Service().dgesdd_ a
            Vector.ofArray S,U,Vt
        else
            LinearAlgebraManaged.SVD a

    /// <summary>spectral norm of a matrix (for Frobenius norm use Matrix.norm)</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let spectralNorm (a:Matrix<float>) =
            //let maxEigenVal = LinearAlgebra.EigenValues mat |> Seq.max
            let maxEigenVal = 
                SVD (a * a.Transpose) 
                |> fun (S,U,V') -> S 
                |> Seq.max
            maxEigenVal |> sqrt

    ///Returns the thin Singular Value Decomposition of the input MxN matrix A 
    ///
    ///A = U * SIGMA * V**T in the tuple (S, U, V), 
    ///
    ///where S is an array containing the diagonal elements of SIGMA.
    ///The first min(M,N) columns of U and the first min(M,N) rows of V**T are returned in the arrays U and VT;
    /// <summary>uses the LAPACK routine dgesdd with the argument JOBZ = 'S'</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let thinSVD a =
        if HaveService() then 
            let S,U,Vt = Service().dgesdd_thin_ a
            Vector.ofArray S, U, Vt
        else
            failwith "managed version not implemented"

    let Hessenberg A =
        //if HaveService() then failwith "Not implemented yet."// REVIEW LinearAlgebraService.Hessenberg A
        //                    else LinearAlgebraManaged.Hessenberg A
        LinearAlgebraManaged.Hessenberg A
        
    /// <summary>computes the hat matrix by the QR decomposition of the designmatrix used in ordinary least squares approaches</summary>
    /// <remarks></remarks>
    /// <param name="A"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let hatmatrix A = 
        LinearAlgebraManaged.hatMatrix A
        
    /// <summary>computes the hat matrix by the QR decomposition of the designmatrix used in ordinary least squares approaches</summary>
    /// <remarks></remarks>
    /// <param name="A"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let leverageBy A = 
        LinearAlgebraManaged.leverageBy A
        
    /// <summary>computes the leverage directly by QR decomposition of the designmatrix used in ordinary least squares approaches<br />and computing of the diagnonal entries of the Hat matrix, known as the leverages of the regressors</summary>
    /// <remarks></remarks>
    /// <param name="A"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let leverage A = 
        LinearAlgebraManaged.leverage A
    ///// This method computes the condition number by just dividing the largest singular value
    ///// by the smallest.
    //let Condition (A:matrix) =
    //    let _,D,_ = SVD A
    //    D.[0] / D.[D.Length-1]
    
    /// <summary>Compute the determinant of a matrix by performing an LU decomposition since if A = P'LU,<br />then det(A) = det(P') * det(L) * det(U).</summary>
    /// <remarks></remarks>
    /// <param name="A"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let Determinant A =
        let P,_,U = LU A
        // Compute the sign of a permutation REVIEW maybe this should go into permutation?
        let PermutationSign (len,p) =
            let a = Array.init len (fun i -> p i)                          // Get an array representation of the permutation
            let v = Array.init len                                         // Find the positions of all the positions in the permutation
                                (fun i -> Array.findIndex (fun x -> x = i) a)
            let mutable sign = 1.0                                         // Remember the sign
            for i=0 to len-2 do                                            // Start transposing elements keeping track of how many
                if a.[i] <> i then                                         // transpositions we have taken.
                    a.[v.[i]] <- a.[i]
                    a.[i] <- i
                    v.[a.[v.[i]]] <- v.[i]
                    v.[i] <- i
                    sign <- -sign
            assert(a = [|0 .. len-1|])
            assert(v = [|0 .. len-1|])
            sign
        let n = A.NumRows
        let P = (fun i -> P i)
        (PermutationSign (n,P)) * (Vector.prod U.Diagonal)
        
    //// matrix multiplication
    //let MM A B =
    //    if HaveService() then
    //        LinearAlgebraService.MM A B
    //    else
    //        A * B

type LinearAlgebra() =

    /// Synonym: kernel / right null space. Returns an orthonormal basis for the null space of matrix A (Ax = 0).<br />The accuracy defines a threshold whether a singular value is considered as zero (default: 1e-08).
    static member nullspace(?Accuracy :float ) = 

        let accuracy = defaultArg Accuracy 1e-08

        fun (a: Matrix<float>) -> 
                        
            // Either MKL or fallback implementation of the full SVD
            let (sigma,U,Vt) = LinearAlgebra.SVD a

            // The rank is the number of nonzero singular values
            let rank = 
                sigma
                |> Seq.sumBy (fun x -> if x >= accuracy then 1 else 0)

            let count = Vt.NumRows - rank 

            Matrix.getRows Vt rank count
            |> Matrix.transpose



