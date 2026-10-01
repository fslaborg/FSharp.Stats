namespace FSharp.Stats

open FsMath
open FsMath.Algebra

/// <summary>
///   Provides matrix decomposition algorithms for linear algebra operations.
/// </summary>
module LinearAlgebra =

    /// <summary>
    ///   Specifies the algorithm used for QR decomposition.
    /// </summary>
    /// <remarks>
    ///   The two algorithms differ in the shape of the output matrices:
    ///   <list type="bullet">
    ///     <item><description>
    ///       <b>Householder</b>: full (uneconomised) decomposition.
    ///       For an m×n input, produces Q (m×m) and R (m×n).
    ///     </description></item>
    ///     <item><description>
    ///       <b>GramSchmidt</b>: thin (economy) decomposition.
    ///       For an m×n input with m ≥ n, produces Q (m×n) and R (n×n).
    ///       Useful when only the column space of A is needed and memory is
    ///       important, or when you want Q to be exactly m×n rather than m×m.
    ///     </description></item>
    ///   </list>
    ///   Both satisfy A = Q * R; both produce an orthonormal Q (Q^T Q = I).
    /// </remarks>
    type QRMethod =
        /// Full decomposition via Householder reflections. Q is m×m, R is m×n.
        | Householder
        /// Thin (economy) decomposition via modified Gram-Schmidt orthogonalisation. Q is m×n, R is n×n.
        | GramSchmidt

    /// <summary>
    ///   QR decomposition of a matrix A into an orthogonal matrix Q and an upper-triangular matrix R such that A = Q * R.
    /// </summary>
    module QR =

        /// <summary>
        ///   Decomposes matrix A into Q * R using the specified method.
        /// </summary>
        /// <param name="method">
        ///   <see cref="QRMethod.Householder"/> for the full decomposition (Q is m×m);
        ///   <see cref="QRMethod.GramSchmidt"/> for the thin/economy decomposition (Q is m×n).
        /// </param>
        /// <param name="A">The input matrix to decompose. Must have at least as many rows as columns for Gram-Schmidt.</param>
        /// <returns>A tuple (Q, R) satisfying A = Q * R.</returns>
        /// <example>
        /// <code>
        ///   open FsMath                       // for matrix / vector literals
        ///   open FSharp.Stats.LinearAlgebra
        ///
        ///   let A = matrix [[12.;-51.;4.];[6.;167.;-68.];[-4.;24.;-41.]]
        ///
        ///   // Gram-Schmidt – thin Q (3×3 for a square input)
        ///   let (qGS, rGS) = QR.decompose GramSchmidt A
        ///
        ///   // Householder – full Q (3×3 for a square input)
        ///   let (qHH, rHH) = QR.decompose Householder A
        ///
        ///   // For a 4×3 input the difference is more visible:
        ///   let B = matrix [[1.;2.];[3.;4.];[5.;6.];[7.;8.]]
        ///   let (qGS4x2, rGS2x2) = QR.decompose GramSchmidt B   // Q: 4×2, R: 2×2
        ///   let (qHH4x4, rHH4x2) = QR.decompose Householder B   // Q: 4×4, R: 4×2
        /// </code>
        /// </example>
        let decompose (method: QRMethod) (A: Matrix<float>) : Matrix<float> * Matrix<float> =
            match method with
            | Householder -> LinearAlgebra.qrDecompose A
            | GramSchmidt -> LinearAlgebra.qrModifiedGramSchmidt A

        /// <summary>
        ///   Decomposes matrix A using Householder reflections (full QR).
        ///   For an m×n matrix, Q is m×m and R is m×n.
        /// </summary>
        /// <param name="A">The input matrix to decompose.</param>
        /// <returns>A tuple (Q, R) satisfying A = Q * R.</returns>
        let householder (A: Matrix<float>) : Matrix<float> * Matrix<float> =
            LinearAlgebra.qrDecompose A

        /// <summary>
        ///   Decomposes matrix A using modified Gram-Schmidt orthogonalisation (thin/economy QR).
        ///   For an m×n matrix with m ≥ n, Q is m×n and R is n×n.
        /// </summary>
        /// <param name="A">The input matrix to decompose. m must be ≥ n.</param>
        /// <returns>A tuple (Q, R) satisfying A = Q * R.</returns>
        let gramSchmidt (A: Matrix<float>) : Matrix<float> * Matrix<float> =
            LinearAlgebra.qrModifiedGramSchmidt A
