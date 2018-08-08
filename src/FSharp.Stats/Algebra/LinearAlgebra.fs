namespace FSharp.Stats.Algebra

open FSharp.Stats



module LinearAlgebra = 
    open System

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

    /// Given A[n,m] and B[n] solve for x[m] such that Ax = B
    /// This call may fail.
    let preDivideByVector A b = 
        //if HaveService() then LinearAlgebraService.preDivideByVector A b
        //                    else LinearAlgebraManaged.SolveLinearSystem A b
        LinearAlgebraManaged.SolveLinearSystem A b
    

    /// Given A[n,m] and B[n,k] solve for X[m,k] such that AX = B
    /// This call may fail.
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
    ///Returns the real (first array) and imaginary (second array) parts of the eigenvalues and a matrix containing the corresponding eigenvectors
    let EVD m = 
        //if HaveService() then 
            //Service().dgeev_(m)
        LinearAlgebraManaged.eigenvectors m
                           
    /// Compute eigenvalues of a square real matrix.
    /// Returns arrays containing the eigenvalues which may be complex.
    /// This call may fail.
    let EigenValues m =
        //if HaveService() then let evals = LinearAlgebraService.eigenvalues m
        //                        let n = evals.Length
        //                        Vector.Generic.init n (fun i -> evals.[i])
        //                    else LinearAlgebraManaged.eigenvalues m
        LinearAlgebraManaged.eigenvalues m

    /// Compute eigenvalues for a real symmetric matrix.
    /// Returns array of real eigenvalues.
    /// This call may fail.
    let EigenValuesWhenSymmetric a =
        //if HaveService() then let evals = LinearAlgebraService.symmetricEigenvalues a
        //                        let n = evals.Length
        //                        Vector.init n (fun i -> evals.[i])
        //                    else LinearAlgebraManaged.symmetricEigenvalues a
        LinearAlgebraManaged.symmetricEigenvalues a
    
    /// Compute eigenvalues and eigenvectors for a real symmetric matrix.
    /// Returns arrays of the values and vectors (both based on reals).
    /// This call may fail.
    let EigenSpectrumWhenSymmetric a =
        if HaveService() then 
            Service().dsyevd_(a)
        else 
            LinearAlgebraManaged.symmetricEigenvectors a

    
    /// Given A[n,n] find it's inverse.
    /// This call may fail.
    let Inverse a = 
        //if HaveService() then LinearAlgebraService.inverse a
        //                    else LinearAlgebraManaged.Inverse a
        LinearAlgebraManaged.Inverse a

    /// Given A[m,n] and B[m] solves AX = B for X[n].
    /// When m=>n, have over constrained system, finds least squares solution for X.
    /// When m<n, have under constrained system, finds least norm solution for X.
    let LeastSquares a b =
        //if HaveService() then LinearAlgebraService.leastSquares a b
        //                    else LinearAlgebraManaged.leastSquares a b
        LinearAlgebraManaged.leastSquares a b
    
    /// Given A[n,n] real symmetric positive definite.
    /// Finds the cholesky decomposition L such that L' * L = A.
    /// May fail if not positive definite.
    let Cholesky a = 
        //if HaveService() then LinearAlgebraService.Cholesky a
        //                    else LinearAlgebraManaged.Cholesky a
        LinearAlgebraManaged.Cholesky a
      
    /// Given A[n,n] real matrix.
    /// Finds P,L,U such that L*U = P*A with L,U lower/upper triangular.
    let LU a = 
        //if HaveService() then LinearAlgebraService.LU a
        //                    else LinearAlgebraManaged.LU a
        LinearAlgebraManaged.LU a
      

    /// Given A[m,n] finds Q[m,m] and R[k,n] where k = min m n.
    /// Have A = Q.R  when m<=n.
    /// Have A = Q.RX when m>n and RX[m,n] is R[n,n] row extended with (m-n) zero rows.
    let QR a = 
        //if HaveService() then LinearAlgebraService.QR a
        //                    else LinearAlgebraManaged.QR a
        LinearAlgebraManaged.QR a

    ///Returns the full Singular Value Decomposition of the input MxN matrix 
    ///
    ///A : A = U * SIGMA * V**T in the tuple (S, U, V**T), 
    ///
    ///where S is an array containing the diagonal elements of SIGMA.
    ///uses the LAPACK routine dgesdd with the argument JOBZ = 'A'
    let SVD (a:Matrix<float>) = 
        if HaveService() then 
            let S,U,Vt = Service().dgesdd_ a
            Vector.ofArray S,U,Vt
        else
            LinearAlgebraManaged.SVD a

    ///Returns the thin Singular Value Decomposition of the input MxN matrix A 
    ///
    ///A = U * SIGMA * V**T in the tuple (S, U, V**T), 
    ///
    ///where S is an array containing the diagonal elements of SIGMA.
    ///The first min(M,N) columns of U and the first min(M,N) rows of V**T are returned in the arrays U and VT;
    ///uses the LAPACK routine dgesdd with the argument JOBZ = 'S'
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
        
    /// computes the hat matrix by the QR decomposition of the designmatrix used in ordinary least squares approaches
    let hatmatrix A = 
        LinearAlgebraManaged.hatMatrix A
        
    /// computes the hat matrix by the QR decomposition of the designmatrix used in ordinary least squares approaches
    let leverageBy A = 
        LinearAlgebraManaged.leverageBy A
        
    /// computes the leverage directly by QR decomposition of the designmatrix used in ordinary least squares approaches
    /// and computing of the diagnonal entries of the Hat matrix, known as the leverages of the regressors
    let leverage A = 
        LinearAlgebraManaged.leverage A
    ///// This method computes the condition number by just dividing the largest singular value
    ///// by the smallest.
    //let Condition (A:matrix) =
    //    let _,D,_ = SVD A
    //    D.[0] / D.[D.Length-1]
    
    /// Compute the determinant of a matrix by performing an LU decomposition since if A = P'LU,
    /// then det(A) = det(P') * det(L) * det(U).
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
