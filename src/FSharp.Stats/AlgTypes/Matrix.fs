namespace FSharp.Stats

open System
open System.Runtime.InteropServices

// let blockSize =
//     match sizeof<'T> with
//     | 4 -> 32  // float32 or int
//     | 8 -> 16  // float64
//     | _ -> 16  // fallback


/// Matrix type to hold values of a matrix in a 1D Arrays (Flattened Representation)
type Matrix<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T : equality
                and 'T :> ValueType> 
                (rows: int, cols: int, data: Vector<'T>) =

    /// Exposes the raw underlying data array (row-major flattened).
    member _.Data = data

    /// Number of rows in the matrix.
    member _.NumRows = rows

    /// Number of columns in the matrix.
    member _.NumCols = cols

    /// Matrix indexer for getting and setting elements at (i, j).
    member this.Item
        with get (i, j) =
            if i < 0 || i >= rows || j < 0 || j >= cols then
                invalidArg "index" $"Index out of range: ({i}, {j})"
            data.[i * cols + j]
        and set (i, j) value =
            if i < 0 || i >= rows || j < 0 || j >= cols then
                invalidArg "index" $"Index out of range: ({i}, {j})"
            data.[i * cols + j] <- value
    
    // Implement IEquatable<T> so that F# structural equality can use it
    interface IEquatable<Matrix<'T>> with
        member this.Equals(other: Matrix<'T>) =

            // 1) Check dimension
            if rows <> other.NumRows || cols <> other.NumCols then
                false
            else
                // 2) Compare all elements
                let otherData = other.Data
                let mutable i = 0
                let mutable eq = true
                while eq && i < data.Length do
                    if data[i] <> otherData[i] then
                        eq <- false
                    i <- i + 1
                eq

    // Override Object.Equals
    override this.Equals(obj: obj) =
        match obj with
        | :? Matrix<'T> as other ->
            (this :> IEquatable<Matrix<'T>>).Equals(other)
        | _ -> false

    // Override Object.GetHashCode
    override this.GetHashCode() =
        // We'll combine the row/col count plus some portion of the data to avoid huge cost.
        // There's no perfect hashing for big arrays, but here's a simple example:

        let mutable hash = HashCode()
        hash.Add(rows)
        hash.Add(cols)
        // Optionally: incorporate some or all elements
        // For big arrays, consider sampling or a rolling hash approach.
        for i in 0 .. data.Length - 1 do
            hash.Add(data[i])
        hash.ToHashCode()

    /// <summary>
    /// Returns a new Matrix<'T> that is the slice of rows [rowStart..rowEnd] 
    /// and columns [colStart..colEnd]. If any of these bounds is omitted, 
    /// it defaults to the full range in that dimension.
    /// </summary>
    /// <param name="rowStart">Optional start row (inclusive)</param>
    /// <param name="rowEnd">Optional end row (inclusive)</param>
    /// <param name="colStart">Optional start column (inclusive)</param>
    /// <param name="colEnd">Optional end column (inclusive)</param>
    /// <returns>A new submatrix copy.</returns>
    member this.GetSlice
        (rowStart: int option, rowEnd: int option,
         colStart: int option, colEnd: int option) : Matrix<'T> =

        // 1) Determine actual start/end indices
        let r1 = defaultArg rowStart 0
        let r2 = defaultArg rowEnd (rows - 1)
        let c1 = defaultArg colStart 0
        let c2 = defaultArg colEnd (cols - 1)

        // 2) Validate them
        if r1 < 0 || r2 < r1 || r2 >= rows then
            invalidArg "row range" $"Invalid row slice range: {r1}..{r2}"
        if c1 < 0 || c2 < c1 || c2 >= cols then
            invalidArg "col range" $"Invalid column slice range: {c1}..{c2}"

        let subRows = r2 - r1 + 1
        let subCols = c2 - c1 + 1

        // 3) Allocate new array for the submatrix
        let subData = Array.zeroCreate<'T> (subRows * subCols)

        // 4) Copy row by row
        for rr in 0 .. subRows - 1 do
            let srcIndex = (r1 + rr) * cols + c1
            let dstIndex = rr * subCols
            // Copy from data[srcIndex..(srcIndex+subCols-1)] 
            //   to subData[dstIndex..(dstIndex+subCols-1)]
            Array.blit data srcIndex subData dstIndex subCols

        // 5) Return a new Matrix with the submatrix data
        Matrix<'T>(subRows, subCols, subData)

    /// <summary>
    /// Convenience "slice indexer" for F# syntax: mat.[r1..r2, c1..c2].
    /// Internally calls GetSlice(...).
    /// </summary>
    member this.GetSlice
        (rowRange: Range, colRange: Range) : Matrix<'T> =
        
        // Convert the F# Range type into int option pairs
        let rowStart, rowEnd =
            (if rowRange.Start = System.Index.Start then None else Some rowRange.Start.Value),
            (if rowRange.End = System.Index.End then None else Some rowRange.End.Value)

        let colStart, colEnd =
            (if colRange.Start = System.Index.Start then None else Some colRange.Start.Value),
            (if colRange.End = System.Index.End then None else Some colRange.End.Value)

        // Use the primary GetSlice overload
        this.GetSlice(rowStart, rowEnd, colStart, colEnd)


    static member inline map2Unchecked<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (fVec: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>)
                (fScalar: 'T -> 'T -> 'T)
                (a: Matrix<'T>)
                (b: Matrix<'T>)
                : Matrix<'T> =

        // if a.Rows <> b.Rows || a.Cols <> b.Cols then
        //     invalidArg "b" "Matrix dimensions must match"

        let len = a.Data.Length
        let result = Array.zeroCreate<'T> len

        let simdSize = Numerics.Vector<'T>.Count
        let simdCount = len / simdSize
        let tailStart = simdCount * simdSize

        // Cast arrays to Vector<'T> spans
        let aVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(a.Data.AsSpan())
        let bVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(b.Data.AsSpan())
        let rVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(result.AsSpan())

        for i = 0 to simdCount - 1 do
            rVec.[i] <- fVec aVec.[i] bVec.[i]

        for i = tailStart to len - 1 do
            result.[i] <- fScalar a.Data.[i] b.Data.[i]

        Matrix(a.NumRows, a.NumCols, result)


    static member inline mapScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>)
                (f: 'T -> 'T -> 'T)
                (matrix: Matrix<'T>)
                (scalar: 'T)
                : Matrix<'T> =

        let length = matrix.Data.Length
        let result = Array.zeroCreate<'T> length

        let slotSize = Numerics.Vector<'T>.Count
        let slotCount = length / slotSize
        let ceiling = slotCount * slotSize

        let scalarVec = Numerics.Vector<'T>(scalar)

        let mSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(matrix.Data.AsSpan())
        let rSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(result.AsSpan())

        // SIMD chunk-wise operation
        for i = 0 to slotCount - 1 do
            rSpan.[i] <- fv mSpan.[i] scalarVec

        // Scalar fallback for remainder
        for i = ceiling to length - 1 do
            result.[i] <- f matrix.Data.[i] scalar

        Matrix(matrix.NumRows, matrix.NumCols, result)



     /// Creates a new matrix by initializing each element with a function `f(row, col)`.
    static member inline transpose<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (rows : int) 
                (cols : int)
                (data: 'T[]) 
                (blockSize: int) =

        //let blockSize = defaultArg blockSize 16

        let src = data
        let dst = Array.zeroCreate<'T> (rows * cols)

        let vectorSize = Numerics.Vector<'T>.Count

        // Process the matrix in blocks
        for i0 in 0 .. blockSize .. rows - 1 do
            for j0 in 0 .. blockSize .. cols - 1 do

                let iMax = min (i0 + blockSize) rows
                let jMax = min (j0 + blockSize) cols

                for i in i0 .. iMax - 1 do
                    let srcOffset = i * cols
                    for j in j0 .. jMax - 1 do
                        let v = src.[srcOffset + j]
                        dst.[j * rows + i] <- v

        dst

     member this.Transpose() =
        let blocksize = 16
        Matrix(this.NumCols, this.NumRows, Matrix.transpose this.NumRows this.NumCols this.Data blocksize)
     
    static member init<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (rows: int)
                (cols: int)
                (f: int -> int -> 'T) =
        let data = Array.init (rows * cols) (fun idx -> f (idx / cols) (idx % cols))
        Matrix(rows, cols, data)

    static member create<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (rows: int)
                (cols: int)
                (data: 'T[]) : Matrix<'T> =
        
        Matrix(rows, cols, data)


    /// Constructs a matrix from a 2D array.
    static member ofArray2D<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (arr2D: 'T[,]) =
        let rows = arr2D.GetLength(0)
        let cols = arr2D.GetLength(1)
        let length = rows * cols

        let flat = Array.zeroCreate<'T> length
        let sourceSpan = MemoryMarshal.CreateSpan(&arr2D.[0, 0], length)
        sourceSpan.CopyTo(flat.AsSpan())

        Matrix(rows, cols, flat)

    /// Converts this matrix into a 2D array.
    member this.toArray2D() =
        let arr2D = Array2D.zeroCreate<'T> this.NumRows this.NumCols
        let targetSpan = MemoryMarshal.CreateSpan(&arr2D.[0, 0], this.NumRows * this.NumCols)
        this.Data.AsSpan().CopyTo(targetSpan)
        arr2D

    
    /// Constructs a matrix from a jagged array (`'T[][]`), assuming a rectangular structure.
    static member ofJaggedArray<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (jagged: 'T[][]) =
        let rows = jagged.Length
        let cols = if rows > 0 then jagged.[0].Length else 0

        // Optional safety check for rectangular shape
        for row in jagged do
            if row.Length <> cols then
                invalidArg "jagged" "All rows in jagged array must have the same length."

        let flat = Array.zeroCreate<'T> (rows * cols)

        for i = 0 to rows - 1 do
            let source = jagged.[i].AsSpan()
            let target = flat.AsSpan(i * cols, cols)
            source.CopyTo(target)

        Matrix(rows, cols, flat)

    /// Converts this matrix into a jagged array (`'T[][]`).
    member this.toJaggedArray() =
        let result = Array.zeroCreate<'T[]> this.NumRows
        let dataSpan = this.Data.AsSpan()

        for i = 0 to this.NumRows - 1 do
            let row = Array.zeroCreate<'T> this.NumCols
            dataSpan.Slice(i * this.NumCols, this.NumCols).CopyTo(row.AsSpan())
            result.[i] <- row

        result


    /// Pretty string representation with formatting and truncation, using StringBuilder.
    /// Supports optional scientific notation for floating point types.
    member this.toFormattedString(?maxRows: int, ?maxCols: int, ?floatFormat: string, ?useScientific: bool) =
        let maxRows = defaultArg maxRows 10
        let maxCols = defaultArg maxCols 10
        let useScientific = defaultArg useScientific false
        let floatFormat = 
            match floatFormat with
            | Some f -> f
            | None -> if useScientific then "0.##E+0" else "0.##"

        if this.NumRows = 0 || this.NumCols = 0 then
            "Matrix (empty)"
        else
            let rowsToShow = min this.NumRows maxRows
            let colsToShow = min this.NumCols maxCols

            // Format a single cell
            let formatCell (value: 'T) =
                match box value with
                | :? float as f    -> f.ToString(floatFormat)
                | :? float32 as f  -> f.ToString(floatFormat)
                | _                -> string value

            let formattedCells =
                Array.init rowsToShow (fun i ->
                    Array.init colsToShow (fun j ->
                        formatCell this.[i, j]
                    )
                )

            let colWidths =
                Array.init colsToShow (fun j ->
                    Array.init rowsToShow (fun i -> formattedCells.[i].[j].Length)
                    |> Array.max
                )

            let divider =
                colWidths
                |> Array.map (fun w -> String.replicate w "─")
                |> String.concat "─┼─"

            let sb = Text.StringBuilder()
            sb.AppendLine($"Matrix {this.NumRows}x{this.NumCols}:") |> ignore
            sb.AppendLine("┌ " + divider + " ┐") |> ignore

            for i = 0 to rowsToShow - 1 do
                sb.Append("│ ") |> ignore
                for j = 0 to colsToShow - 1 do
                    let cell = formattedCells.[i].[j].PadLeft(colWidths.[j])
                    sb.Append(cell) |> ignore
                    if j < colsToShow - 1 then sb.Append(" │ ") |> ignore
                sb.AppendLine(" │") |> ignore

            if this.NumRows > rowsToShow || this.NumCols > colsToShow then
                sb.AppendLine($"│ ... truncated ({this.NumRows}x{this.NumCols}) │") |> ignore

            sb.AppendLine("└ " + divider + " ┘") |> ignore
            sb.ToString()


    /// Default string representation (truncated, human-readable)
    override this.ToString() = this.toFormattedString()

    static member inline checkSameShape<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
        (a: Matrix<'T>) (b: Matrix<'T>) =
        if a.NumRows <> b.NumRows || a.NumCols <> b.NumCols then
            invalidArg "b" $"Matrix dimensions must match. A is {a.NumRows}x{a.NumCols}, B is {b.NumRows}x{b.NumCols}"

    /// Element-wise addition
    static member inline add<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
        (a: Matrix<'T>) 
        (b: Matrix<'T>) : Matrix<'T> =

        Matrix.checkSameShape a b
        let data = 
            Acceleration.SIMDUtils.map2Unchecked (+) (+) a.Data b.Data
        Matrix(a.NumRows, a.NumCols, data)  

    static member inline subtract<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
        (a: Matrix<'T>) 
        (b: Matrix<'T>) : Matrix<'T> =

        Matrix.checkSameShape a b
        Matrix.map2Unchecked (-) (-) a b


    /// Hadamard product
    static member inline multiply<'T 
        when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T :> ValueType> 
        (a: Matrix<'T>) 
        (b: Matrix<'T>) : Matrix<'T> =

        Matrix.checkSameShape a b
        Matrix.map2Unchecked (*) (*) a b


    static member inline divide<'T 
        when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T :> ValueType> 
        (a: Matrix<'T>) 
        (b: Matrix<'T>) : Matrix<'T> =

        Matrix.checkSameShape a b
        Matrix.map2Unchecked (/) (/) a b


    static member inline addScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (m: Matrix<'T>) (scalar: 'T) : Matrix<'T> =
        
        Matrix.mapScalar (+) (+) m scalar



    static member inline subtractScalar<'T 
        when 'T :> Numerics.INumber<'T> 
        and 'T : (new: unit -> 'T) 
        and 'T : struct 
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (scalar: 'T) : Matrix<'T> =

        Matrix.mapScalar (-) (-) m scalar


    static member inline multiplyScalar<'T 
        when 'T :> Numerics.INumber<'T> 
        and 'T : (new: unit -> 'T) 
        and 'T : struct 
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (scalar: 'T) : Matrix<'T> =

        Matrix.mapScalar (*) (*) m scalar


    static member inline divideScalar<'T 
        when 'T :> Numerics.INumber<'T> 
        and 'T : (new: unit -> 'T) 
        and 'T : struct 
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (scalar: 'T) : Matrix<'T> =

        Matrix.mapScalar (/) (/) m scalar


    /// Standard matrix-vector product
    static member inline muliplyVector<'T 
        when 'T :> Numerics.INumber<'T> 
        and 'T : (new: unit -> 'T) 
        and 'T : struct 
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (v: Vector<'T>) : Vector<'T> =

        if m.NumCols <> v.Length then
            invalidArg "v" $"Vector length {v.Length} must match matrix column count {m.NumCols}"

        let result: Vector<'T> = Array.zeroCreate m.NumRows
        let zero = LanguagePrimitives.GenericZero<'T>

        let simdSize = Numerics.Vector<'T>.Count
        let vSpan = v.AsSpan()
        let vSimd = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(vSpan)

        let tailStart = (v.Length / simdSize) * simdSize

        for row = 0 to m.NumRows - 1 do
            let rowOffset = row * m.NumCols
            let rowSpan = m.Data.AsSpan(rowOffset, m.NumCols)
            let rowSimd = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(rowSpan)

            let mutable acc = Numerics.Vector<'T>(zero)

            for i = 0 to vSimd.Length - 1 do
                acc <- acc + rowSimd.[i] * vSimd.[i]

            let mutable tail = zero
            for i = tailStart to m.NumCols - 1 do
                tail <- tail + m.Data.[rowOffset + i] * v.[i]

            result.[row] <- Numerics.Vector.Sum(acc) + tail

        result

    static member inline addRowVector<'T
        when 'T :> Numerics.INumber<'T>
        and 'T : struct
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (v: Vector<'T>) : Matrix<'T> =

        if v.Length <> m.NumCols then
            invalidArg "v" $"Row vector length {v.Length} must match matrix column count {m.NumCols}"

        let resultData = Array.zeroCreate<'T> (m.NumRows * m.NumCols)
        let simdSize = Numerics.Vector<'T>.Count
        let simdCount = m.NumCols / simdSize
        let scalarStart = simdCount * simdSize

        let vSimd = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v.AsSpan())

        for row = 0 to m.NumRows - 1 do
            let offset = row * m.NumCols
            let srcSpan = m.Data.AsSpan(offset, m.NumCols)
            let dstSpan = resultData.AsSpan(offset, m.NumCols)

            let srcSimd = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(srcSpan)
            let dstSimd = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(dstSpan)

            // SIMD part
            for i = 0 to simdCount - 1 do
                dstSimd.[i] <- srcSimd.[i] + vSimd.[i]

            // Tail (scalar)
            for i = scalarStart to m.NumCols - 1 do
                dstSpan.[i] <- srcSpan.[i] + v.[i]

        Matrix(m.NumRows, m.NumCols, resultData)


    static member inline addColVector<'T
        when 'T :> Numerics.INumber<'T>
        and 'T : struct
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (m: Matrix<'T>) 
        (v: Vector<'T>) : Matrix<'T> =

        if v.Length <> m.NumRows then
            invalidArg "v" $"Column vector length {v.Length} must match matrix row count {m.NumRows}"

        let cols = m.NumCols
        let resultData = Array.zeroCreate<'T> (m.NumRows * cols)
        let simdSize = Numerics.Vector<'T>.Count

        for row = 0 to m.NumRows - 1 do
            let rowOffset = row * cols
            let srcRow = m.Data.AsSpan(rowOffset, cols)
            let dstRow = resultData.AsSpan(rowOffset, cols)

            let simdRow = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(srcRow)
            let simdDst = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(dstRow)

            let broadcast = Numerics.Vector<'T>(v.[row])
            let simdCount = cols / simdSize
            let scalarStart = simdCount * simdSize

            // SIMD-add: row + broadcast vector
            for i = 0 to simdCount - 1 do
                simdDst.[i] <- simdRow.[i] + broadcast

            // Scalar tail
            for i = scalarStart to cols - 1 do
                dstRow.[i] <- srcRow.[i] + v.[row]

        Matrix(m.NumRows, cols, resultData)


    /// <summary>
    /// Computes row-vector v (length = mat.NumRows) times matrix mat (size = [NumRows × NumCols]),
    /// returning a new vector of length mat.NumCols. Uses chunk-based SIMD with manual gather.
    /// </summary>
    static member inline multiplyRowVector<'T
            when 'T :> Numerics.INumber<'T>
            and 'T : struct
            and 'T : (new : unit -> 'T)
            and 'T :> ValueType>
            (v: Vector<'T>)
            (mat: Matrix<'T>) : Vector<'T> =
        
        let n = mat.NumRows
        let m = mat.NumCols
        if v.Length <> n then
            invalidArg (nameof v) "Vector length must match mat.NumRows."

        let result = Vector.zeroCreate<'T> m

        // We'll chunk over v in blocks of 'simdSize'. For each column j, 
        // we gather that column's slice [ (i*m + j) for i in 0..n-1 ] in blocks.
        let simdSize = Numerics.Vector<'T>.Count

        for j in 0 .. m - 1 do
            // sumVec accumulates partial sums in vector-lanes
            let mutable sumVec = Numerics.Vector<'T>.Zero

            // 1) Process as many full 'simdSize' blocks as possible
            let blockCount = n / simdSize
            let tailStart = blockCount * simdSize

            for blockIndex in 0 .. blockCount - 1 do
                let baseIdx = blockIndex * simdSize

                // Gather the column’s elements for this block into 'gatherArr'
                // These are mat.Data.[(baseIdx + s)*m + j] for s in [0..simdSize-1]
                let gatherArr = Vector.zeroCreate<'T> simdSize
                for s in 0 .. simdSize - 1 do
                    gatherArr.[s] <- mat.Data.[(baseIdx + s) * m + j]

                let colVec = Numerics.Vector<'T>(gatherArr)      // column-block chunk
                let rowVec = Numerics.Vector<'T>(v, baseIdx)     // row vector chunk
                sumVec <- sumVec + (rowVec * colVec)

            // 2) Reduce sumVec’s lanes into a scalar
            let mutable colSum = 'T.Zero
            for lane in 0 .. simdSize - 1 do
                colSum <- colSum + sumVec.[lane]

            // 3) Handle any leftover elements (remainder)
            for i in tailStart .. n - 1 do
                colSum <- colSum + v.[i] * mat.Data.[i*m + j]

            result.[j] <- colSum

        result

    /// <summary>
    /// Standard matrix multiplication (A x B).
    /// A is (M x K), B is (K x N) => result is (M x N).
    /// Then each (row of A) .dot. (row of B^T) is done with Vector<'T> chunks.
    /// </summary>
    static member matmul
            (A: Matrix<'T>) (B: Matrix<'T>) : Matrix<'T> =
        
        // 1) Dimension checks
        if A.NumCols <> B.NumRows then
            invalidArg (nameof B)
                $"Inner dimensions mismatch. A is {A.NumRows}x{A.NumCols}, B is {B.NumRows}x{B.NumCols}"

        let M = A.NumRows
        let K = A.NumCols
        let N = B.NumCols

        // 2) Transpose B to get B^T => shape [N x K], row j of B^T is col j of B
        let Btrans = B.Transpose()
        let bTData = Btrans.Data // Now each "row" in bTData is length=K, contiguous

        // 3) We'll allocate result
        let resultData = Array.zeroCreate<'T> (M * N)
        let aData = A.Data

        // 4) For each row i in A, row j in B^T => element in [i*N + j]
        //    The row i in A is contiguous of length K => offset i*K in aData
        //    The row j in B^T is contiguous of length K => offset j*K in bTData
        for i in 0 .. M - 1 do
            let aRowOffset = i * K
            let aRowSpan = aData.AsSpan(aRowOffset, K)
            // Convert A's row to a "Vector<'T>" span
            let aVecSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(aRowSpan)

            for j in 0 .. N - 1 do
                let bTRowOffset = j * K
                let bTRowSpan = bTData.AsSpan(bTRowOffset, K)
                // Convert B^T's row j to a "Vector<'T>" span
                let bTVecSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(bTRowSpan)

                // 4.a) Do the chunkwise vector multiply-add
                let mutable accum = Numerics.Vector<'T>.Zero
                for chunk in 0 .. aVecSpan.Length - 1 do
                    accum <- accum + (aVecSpan.[chunk] * bTVecSpan.[chunk])

                // 4.b) Sum up the vector lanes to a single scalar
                let mutable sum = Numerics.Vector.Sum(accum)

                // 4.c) Handle remainder elements if K not multiple of Vector<'T>.Count
                let remainderStart = aVecSpan.Length * Numerics.Vector<'T>.Count
                for r in remainderStart .. K - 1 do
                    sum <- sum + aRowSpan.[r] * bTRowSpan.[r]

                // 4.d) Place result into the final matrix
                resultData.[i*N + j] <- sum

        Matrix(M, N, resultData)


    // Matrix - matrix operations
    static member inline ( + ) (a: Matrix<'T>, b: Matrix<'T>)  = Matrix.add a b
    static member inline ( - ) (a: Matrix<'T>, b: Matrix<'T>)  = Matrix.subtract a b
    static member inline ( .* ) (a: Matrix<'T>, b: Matrix<'T>) = Matrix.multiply a b
    static member inline ( * ) (a: Matrix<'T>, b: Matrix<'T>)  = Matrix.matmul a b
    static member inline ( / ) (a: Matrix<'T>, b: Matrix<'T>)  = Matrix.divide a b

    // Matrix - vector operations
    static member inline ( * ) (m: Matrix<'T>, v: Vector<'T>) = Matrix.muliplyVector m v
    static member inline ( * ) (v: Vector<'T>, m: Matrix<'T>) = Matrix.multiplyRowVector v m
    // static member inline ( + ) (m: Matrix<'T>, colVector: Vector<'T>) = Matrix.addColVector m colVector
    // static member inline ( +| ) (m: Matrix<'T>, rowVector: Vector<'T>) = Matrix.addRowVector m rowVector

    // Matrix - scalar operations
    static member inline (+) (m: Matrix<'T>, s: 'T) = Matrix.addScalar m s
    static member inline (+) (s: 'T, m: Matrix<'T>) = Matrix.addScalar m s

    static member inline (-) (m: Matrix<'T>, s: 'T) = Matrix.subtractScalar m s
    static member inline (-) (s: 'T, m: Matrix<'T>) = Matrix.subtractScalar m s

    static member inline (*) (m: Matrix<'T>, s: 'T) = Matrix.multiplyScalar m s
    static member inline (*) (s: 'T, m: Matrix<'T>) = Matrix.multiplyScalar m s

    static member inline (/) (m: Matrix<'T>, s: 'T) = Matrix.divideScalar m s
    static member inline (/) (s: 'T, m: Matrix<'T>) = Matrix.divideScalar m s


    
    static member inline diagonal
        (diag: Vector<'T>) : Matrix<'T> =

        let n = diag.Length
        let data = Array.zeroCreate<'T> (n * n)
        for i = 0 to n - 1 do
            data.[i * n + i] <- diag.[i]
        Matrix(n, n, data)

    /// <summary>
    /// Returns the diagonal elements of this matrix as an array of length = min(NumRows, NumCols).
    /// </summary>
    static member inline getDiagonal
        (m:Matrix<'T>) : Vector<'T> =
        let n = min m.NumRows m.NumCols
        let diag = Array.zeroCreate<'T> n
        for i = 0 to n - 1 do
            // The diagonal element at row i, col i is stored at offset i * cols + i
            diag.[i] <- m.Data.[i * m.NumCols + i]
        diag


    static member inline identity<'T
        when 'T :> Numerics.INumber<'T>
        and 'T : struct
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (n: int) : Matrix<'T> =

        let data = Array.zeroCreate<'T> (n * n)
        for i = 0 to n - 1 do
            data.[i * n + i] <- 'T.One
        Matrix(n, n, data)

    static member inline zeroCreate<'T 
        when 'T :> Numerics.INumber<'T>
        and 'T : struct
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (rows: int) 
        (cols: int) : Matrix<'T> =

        let data = Array.zeroCreate<'T> (rows * cols)
        Matrix(rows, cols, data)

    static member inline ones<'T
        when 'T :> Numerics.INumber<'T>
        and 'T : struct
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (rows: int) 
        (cols: int) : Matrix<'T> =

        let size = rows * cols
        let data = Array.zeroCreate<'T> size
        let simdSize = Numerics.Vector<'T>.Count
        let simdCount = size / simdSize

        let vOne = Numerics.Vector<'T>('T.One)
        let span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(data.AsSpan())

        for i = 0 to simdCount - 1 do
            span.[i] <- vOne

        for i = simdCount * simdSize to size - 1 do
            data.[i] <- 'T.One

        Matrix(rows, cols, data)

    static member inline getRow<'T 
        when 'T :> Numerics.INumber<'T>
        and 'T : struct 
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (i: int)
        (m: Matrix<'T>) : Vector<'T> =

        if i < 0 || i >= m.NumRows then
            invalidArg "i" $"Row index out of bounds: {i}"

        let rowSpan = m.Data.AsSpan(i * m.NumCols, m.NumCols)
        let result = Array.zeroCreate<'T> m.NumCols
        rowSpan.CopyTo(result.AsSpan())
        result

    static member inline getRows (m: Matrix<'T>) : Vector<'T>[] =
        Array.init m.NumRows (fun i -> Matrix.getRow i m)


    static member inline getCol<'T 
        when 'T :> Numerics.INumber<'T>
        and 'T : struct 
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (j: int)
        (m: Matrix<'T>) : Vector<'T> =

        if j < 0 || j >= m.NumCols then
            invalidArg "j" $"Column index out of bounds: {j}"

        let result = Array.zeroCreate<'T> m.NumRows
        let mutable offset = j
        for i = 0 to m.NumRows - 1 do
            result.[i] <- m.Data.[offset]
            offset <- offset + m.NumCols

        result

    static member inline getCols<'T 
        when 'T :> Numerics.INumber<'T>
        and 'T : struct 
        and 'T : (new : unit -> 'T)
        and 'T :> ValueType>
        (m: Matrix<'T>) : Vector<'T>[] =

        Array.init m.NumCols (fun j ->
            let col = Array.zeroCreate<'T> m.NumRows
            let mutable offset = j
            for i = 0 to m.NumRows - 1 do
                col.[i] <- m.Data.[offset]
                offset <- offset + m.NumCols
            col
        )


    /// <summary>
    /// Sets row <paramref name="i"/> of this matrix to the contents of <paramref name="rowData"/>.
    /// The length of <paramref name="rowData"/> must match the number of columns in this matrix.
    /// </summary>
    /// <param name="i">The zero-based row index to set.</param>
    /// <param name="rowData">An array of new values for the row, of length equal to <see cref="Cols"/>.</param>
    /// <exception cref="ArgumentOutOfRangeException">Thrown if <paramref name="i"/> is out of range.</exception>
    /// <exception cref="ArgumentException">Thrown if <paramref name="rowData"/> has incorrect length.</exception>
    member this.SetRow(i: int, rowData: 'T[]) =
        if i < 0 || i >= rows then
            invalidArg (nameof i) "Row index out of range."
        if rowData.Length <> cols then
            invalidArg (nameof rowData) $"rowData must have length = {cols}."

        let offset = i * cols
        Array.blit rowData 0 data offset cols





    // static member MatrixMultiply
    
    //     (a: Matrix<'T>, b: Matrix<'T>, ?blockSize: int) : Matrix<'T>
    //     when 'T : struct
    //     and 'T : (new : unit -> 'T)
    //     and 'T :> INumber<'T>
    //     and 'T :> ValueType =

    //     if a.Cols <> b.Rows then
    //         invalidArg "b" "Matrix dimensions must agree: a.Cols must equal b.Rows."

    //     let blockSize = defaultArg blockSize 32

    //     let m = a.Rows
    //     let n = b.Cols
    //     let k = a.Cols

    //     let result = Array.zeroCreate<'T> (m * n)
    //     let aData = a.Data
    //     let bData = b.Data
    //     let rData = result

    //     let vSize = Vector<'T>.Count

    //     // Blocked GEMM
    //     for i0 in 0 .. blockSize .. m - 1 do
    //         let iMax = min (i0 + blockSize) m
    //         for j0 in 0 .. blockSize .. n - 1 do
    //             let jMax = min (j0 + blockSize) n
    //             for k0 in 0 .. blockSize .. k - 1 do
    //                 let kMax = min (k0 + blockSize) k

    //                 // Iterate over block
    //                 for i in i0 .. iMax - 1 do
    //                     for j in j0 .. jMax - 1 do
    //                         let mutable sum = 'T.Zero
    //                         let mutable kk = k0

    //                         // SIMD accumulation
    //                         while kk <= kMax - vSize do
    //                             let aVec = Vector<'T>(aData, i * k + kk)
    //                             let bVec = Vector<'T>([| for offset in 0 .. vSize - 1 -> bData.[(kk + offset) * n + j] |])
    //                             sum <- sum + Vector.Dot(aVec, bVec)
    //                             kk <- kk + vSize

    //                         // Scalar tail
    //                         while kk < kMax do
    //                             sum <- sum + aData.[i * k + kk] * bData.[kk * n + j]
    //                             kk <- kk + 1

    //                         rData.[i * n + j] <- rData.[i * n + j] + sum
