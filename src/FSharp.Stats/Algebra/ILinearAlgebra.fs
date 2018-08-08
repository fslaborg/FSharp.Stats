namespace FSharp.Stats.Algebra

//namespace Microsoft.FSharp.Math // old namespace
open FSharp.Stats


///This is an internal interface and not for user usage.
///It exposes a specialised subset of BLAS/LAPACK functionality.
///This functionality is used by us to build the exposed APIs.
///It is those exposed APIs that should be used.
type ILinearAlgebra = 
    //Matrix-Matrix Multiplication
    abstract dgemm_ : Matrix<float> * Matrix<float> -> Matrix<float>

    ////Matrix-Vector Multiplication
    //abstract dgemv_ : Matrix<float> * Vector<float> -> Vector<float>

    ////Solve (linear equations)
    //abstract dgesv_ : Matrix<float> * Matrix<float> -> Matrix<float> * int array * Matrix<float>

    ////Solve symmetric positive definite matrix (linear equations)
    //abstract dposv_ : Matrix<float> * Matrix<float> -> Matrix<float> * Matrix<float>

    ////Solve triangular (linear equations)
    //abstract dtrsv_ : char * Matrix<float> * Vector<float> -> Vector<float>

    ////Solve triangular (linear equations)
    //abstract dtrsm_ : char * Matrix<float> * Matrix<float> -> Matrix<float>

    ////Solve (linear equations) using LU factorization
    //abstract dgesvx_ :
    //  Matrix<float> * Matrix<float> ->
    //  Matrix<float> * Matrix<float> * int array * char * double array * double array *
    //  Matrix<float> * Matrix<float> * float * double array * double array

    ////Compoutes for an N-by-N real nonsymmetric matrix A, the
    ////eigenvalue decomposition eigenvalues and right eigenvectors.
    ////The right eigenvector v(j) of A satisfies
    ////
    ////                 A * v(j) = lambda(j) * v(j)
    ////
    ////where lambda(j) is its eigenvalue.
    ////The computed eigenvectors are normalized to have Euclidean norm
    ////equal to 1 and largest component real. Uses the LAPACK subroutine dgeev with arguments JOBVR = 'V' and JOBVL = 'N'
    ////
    ///Returns the real (first array) and imaginary (second array) parts of the eigenvalues and a matrix containing the corresponding eigenvectors
    //abstract dgeev_ : Matrix<float> -> double array * double array * Matrix<float>   

    ////Eigen Value of Symmetric Matrix
    //abstract dsyev_ : char * char * Matrix<float> -> Matrix<float> * double array


    ///Computes for a N-by-N real symmetric matrix A, the
    ///eigenvalue decomposition of eigenvalues and right eigenvectors.
    ///The right eigenvector v(j) of A satisfies
    ///
    ///                 A * v(j) = lambda(j) * v(j)
    ///
    ///where lambda(j) is its eigenvalue.
    abstract dsyevd_ : Matrix<float> -> Matrix<float> * double array

    ////Eigen Value for a pair of general matrices
    //abstract dggev_ :
    //  Matrix<float> * Matrix<float> ->
    //  Matrix<float> * Matrix<float> * double array * double array * double array *
    //  double [,]

    ////Solve least-squares/min-norm.
    ////Note the dimension requirements on second input to match second output.
    //abstract dgels_ : Matrix<float> * Matrix<float> -> Matrix<float> * Matrix<float>

    ////Solve least-squares/min-norm (with linear equality constraint)
    //abstract dgglse_ :
    //  Matrix<float> * Matrix<float> * Vector<float> * Vector<float> ->
    //  Matrix<float> * Vector<float> * double array

    ////Singular Value Decomposition
    //abstract dgesvd_ :
    //  Matrix<float> -> double array * Matrix<float> * Matrix<float>


    ///Returns the full Singular Value Decomposition of the input MxN matrix 
    ///
    ///A : A = U * SIGMA * V**T in the tuple (S, U, V**T), 
    ///
    ///where S is an array containing the diagonal elements of SIGMA.
    ///uses the LAPACK routine dgesdd with the argument JOBZ = 'A'
    abstract dgesdd_ : Matrix<float> -> double array * Matrix<float> * Matrix<float>


    ///Returns the thin Singular Value Decomposition of the input MxN matrix A 
    ///
    ///A = U * SIGMA * V**T in the tuple (S, U, V**T), 
    ///
    ///where S is an array containing the diagonal elements of SIGMA.
    ///The first min(M,N) columns of U and the first min(M,N) rows of V**T are returned in the arrays U and VT;
    ///uses the LAPACK routine dgesdd with the argument JOBZ = 'S'
    abstract dgesdd_thin_ : Matrix<float> -> double array * Matrix<float> * Matrix<float>

    ////Single Value Decomposition for Symmetric Matrices
    //abstract dsygv_ :
    //  Matrix<float> * Matrix<float> -> Matrix<float> * Matrix<float> * double array

    ////Single Value Decomposition for Symetric Matrices Divide and Conquer
    //abstract dsygvd_ :
    //  Matrix<float> * Matrix<float> -> Matrix<float> * Matrix<float> * double array

    ////Cholesky Factorisation
    //abstract dpotrf_ : char * Matrix<float> -> Matrix<float>
    
    //abstract dgetrf_ : matrix -> matrix * int[]

    ////Cholesky Factorisation - Expert
    //abstract dposvx_ :
    //  Matrix<float> * Matrix<float> ->
    //  Matrix<float> * Matrix<float> * char * double array * Matrix<float> * Matrix<float> *
    //  float * double array * double array

    ////QR Factorisation
    //abstract dgeqrf_ : Matrix<float> -> Matrix<float> * double array
