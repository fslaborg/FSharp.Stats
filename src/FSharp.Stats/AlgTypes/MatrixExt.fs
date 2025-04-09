namespace FSharp.Stats

open System
open System.Runtime.InteropServices


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Matrix =

//    /// Indexed fold over a matrix
//    let inline foldi<'T when 'T :> Numerics.INumber<'T>> f (state: 'T) (m: Matrix<'T>) : 'T =
//        let mutable acc = state
//        for i = 0 to m.NumRows - 1 do
//            for j = 0 to m.NumCols - 1 do
//                acc <- f i j acc m.[i, j]
//        acc

    
    /// <summary>
    /// Creates and returns a new <see cref="Matrix{T}"/> with the same dimensions 
    /// as this matrix and a copy of the underlying data.
    /// </summary>
    let inline copy (m:Matrix<'T>) : Matrix<'T> =
        // Make a copy of the data array
        let newData = Array.copy m.Data
        Matrix<'T>(m.NumRows, m.NumCols, newData)


    /// <summary>Splits a matrix along row direction according to given indices. Returns (matrix including rows according to indices, rest)</summary>
    /// <remarks></remarks>
    /// <param name="indices"></param>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let splitRows (indices:int[]) (m:Matrix<'T>) =

        let nRows,nCols = m.NumRows,m.NumCols
        //let nm  = Matrix.Generic.zero (nRows-indices.Length) nCols
        //let nmi = Matrix.Generic.zero indices.Length nCols
        let nm  = Matrix.zeroCreate (nRows-indices.Length) nCols
        let nmi = Matrix.zeroCreate indices.Length nCols
        indices |> Array.sortInPlace
        let rec loop nRowI nRowIi rowI =
            match rowI with
            | i as rowI when rowI < 0 -> nmi,nm
            | i as rowI when nRowIi >= 0 && rowI = indices.[nRowIi] ->
                for colI=0 to nCols-1 do
                    nmi.[nRowIi,colI] <- m.[rowI,colI]
                loop (nRowI) (nRowIi-1) (rowI-1)
            | _ -> //i as rowI when rowI <> indices.[ii] ->
                for colI=0 to nCols-1 do
                    nm.[nRowI,colI] <- m.[rowI,colI]
                loop (nRowI-1) (nRowIi) (rowI-1)

        loop (nRows-1-indices.Length) (indices.Length-1) (nRows-1)

    /// <summary>Splits a matrix along column direction according to given indices. Returns (matrix including cols according to indices, rest)</summary>
    /// <remarks></remarks>
    /// <param name="indices"></param>
    /// <param name="m"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let splitCols (indices:int[]) (m:Matrix<_>) =
        let nRows,nCols = m.NumRows,m.NumCols
        //let nm  = Matrix.Generic.zero nRows (nCols-indices.Length)
        //let nmi = Matrix.Generic.zero nRows indices.Length
        let nm  = Matrix.zeroCreate nRows (nCols-indices.Length)
        let nmi = Matrix.zeroCreate nRows indices.Length
        indices |> Array.sortInPlace
        let rec loop nColI nColIi colI =
            match colI with
            | i as colI when colI < 0 -> nmi,nm
            | i as colI when nColIi >= 0 && colI = indices.[nColIi] ->
                for rowI=0 to nRows-1 do
                    nmi.[rowI,nColIi] <- m.[rowI,colI]
                loop (nColI) (nColIi-1) (colI-1)
            | _ -> //i as rowI when rowI <> indices.[ii] ->
                for rowI=0 to nRows-1 do
                    nm.[rowI,nColI] <- m.[rowI,colI]
                loop (nColI-1) (nColIi) (colI-1)

        loop (nCols-1-indices.Length) (indices.Length-1) (nCols-1)

    /// <summary>
    /// Creates a new matrix by permuting the rows of matrix <paramref name="M"/> according
    /// to the permutation <paramref name="P"/>. That is, row <c>i</c> of the result is row 
    /// <c>P(i)</c> of <paramref name="M"/>. 
    /// </summary>
    /// <param name="P">A permutation function that maps row indices. Should be valid
    /// <param name="M">An <c>r x c</c> matrix whose rows will be reordered.</param>
    /// for all <c>i</c> in <c>[0..r-1]</c>.</param>
    /// <returns>
    /// A new matrix of the same dimensions <c>r x c</c>, with rows permuted by <paramref name="P"/>.
    /// </returns>
    let permuteRowsBy (P: Permutation) (M: Matrix<'T>) : Matrix<'T> =
        let r, c = M.NumRows, M.NumCols
        let newData = Array.zeroCreate<'T> (r * c)
        for i = 0 to r - 1 do
            let srcRow = P i
            // Copy row srcRow from M into row i of the new matrix
            Array.blit M.Data (srcRow * c) newData (i * c) c
        Matrix<'T>(r, c, newData)