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
    let permuteRowsBy (P: Permutation) (m: Matrix<'T>) : Matrix<'T> =
        let r, c = m.NumRows, m.NumCols
        let newData = Array.zeroCreate<'T> (r * c)
        for i = 0 to r - 1 do
            let srcRow = P i
            // Copy row srcRow from M into row i of the new matrix
            Array.blit m.Data (srcRow * c) newData (i * c) c
        Matrix<'T>(r, c, newData)

    let ofRows (rows : 'T[][]) : Matrix<'T> =
         Matrix.ofJaggedArray rows

    /// Creates a matrix from a sequence of row sequences
    let inline ofRowSeq (rows: seq<#seq<'T>>) : Matrix<'T> =
        // Convert the outer seq to an array of row-seqs
        let rowArr = rows |> Seq.toArray
        if rowArr.Length = 0 then
            invalidArg "rows" "Cannot create a matrix from an empty sequence of rows."

        // Determine the number of columns by checking the first row’s length
        let colCount = rowArr.[0] |> Seq.length

        // Verify all rows have the same length
        for i in 1 .. rowArr.Length - 1 do
            let currentLen = rowArr.[i] |> Seq.length
            if currentLen <> colCount then
                invalidArg "rows" (sprintf "Row %d has length %d, expected %d." i currentLen colCount)

        // Flatten all row-seqs into a single array (row-major order)
        let data =
            rowArr
            |> Array.collect (fun rowSeq -> rowSeq |> Seq.toArray)

        // Construct the matrix
        Matrix<'T>(rowArr.Length, colCount, data)

    let mapiRows (f: int -> Vector<'T> -> Vector<'U>)  (m:Matrix<'T>) : Matrix<'U> =
        Matrix.getRows m
        |> Array.mapi (fun i v -> f i v)   
        |> ofRows

    let ofCols (cols : 'T[][]) : Matrix<'T> =
        Matrix.ofJaggedArray cols
        |> fun m -> m.Transpose()

    let mapiCols (f: int -> Vector<'T> -> Vector<'U>)  (m:Matrix<'T>) : Matrix<'U> =
        let cols =
            Array.init m.NumCols (fun j ->
                let col = Array.zeroCreate<'T> m.NumRows
                let mutable offset = j
                for i = 0 to m.NumRows - 1 do
                    col.[i] <- m.Data.[offset]
                    offset <- offset + m.NumCols
                f j col
            )
        ofCols cols

    /// <summary>
    /// Applies a function <paramref name="f"/> to each element of the matrix, 
    /// returning a new matrix with updated values. The iteration occurs in 
    /// row-major order for performance.
    /// </summary>
    /// <param name="f">
    /// A function taking (rowIndex, colIndex, oldValue) 
    /// and returning the new value for that position.
    /// </param>
    /// <returns>A new Matrix with updated elements.</returns>
    let mapi (f: int -> int -> 'T -> 'U) (A: Matrix<'T>) : Matrix<'U> =
        let newData = Array.zeroCreate<'U> A.Data.Length
        let rows, cols = A.NumRows, A.NumCols
        for i in 0 .. rows - 1 do
            let rowOffset = i * cols
            for j in 0 .. cols - 1 do
                let idx = rowOffset + j
                newData.[idx] <- f i j A.Data.[idx]
        Matrix<'U>(rows, cols, newData)


    let map (f: 'T -> 'U)  (m:Matrix<'T>) : Matrix<'U> = 
        let r, c = m.NumRows, m.NumCols
        let newData = 
            m.Data |> Array.map f
        Matrix<'U>(r, c, newData)


    /// <summary>
    /// Folds over each element in row-major order, accumulating a result of type 'State.
    /// The callback <paramref name="f"/> receives (currentState, rowIndex, colIndex, elementValue),
    /// and returns the new state.
    /// </summary>
    /// <param name="f">A function taking (state, i, j, value) -> newState.</param>
    /// <param name="initialState">The initial accumulation state.</param>
    /// <param name="A">The matrix to fold over.</param>
    /// <returns>The final accumulated state.</returns>
    let foldi
        (f : int -> int -> 'State -> 'T -> 'State )
        (initialState : 'State)
        (A : Matrix<'T>)
        : 'State =

        let mutable acc = initialState
        let rows, cols = A.NumRows, A.NumCols
        for i in 0 .. rows - 1 do
            let rowOffset = i * cols
            for j in 0 .. cols - 1 do
                let idx = rowOffset + j
                acc <- f i j acc A.Data.[idx]
        acc

    /// <summary> Sums all elements in the matrix. </summary>
    let sum (m:Matrix<'T>) : 'T =
        m.Data |> Vector.sum   