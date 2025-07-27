namespace FSharp.Stats.Acceleration

open System
open System.Runtime.InteropServices
open System.Runtime.CompilerServices





/// <summary>
/// A utility class for performing SIMD (Single Instruction, Multiple Data) operations on arrays.
/// </summary>
type SIMDUtils() =

    /// <summary>
    /// Applies a safe element-wise unary operation on an array using SIMD for performance.
    /// </summary>
    /// <typeparam name="'T">The numeric type of the array elements.</typeparam>
    /// <param name="fv">A function that performs the SIMD operation on a vector.</param>
    /// <param name="f">A fallback function for scalar operations.</param>
    /// <param name="v">The input array.</param>
    /// <returns>An array with the operation applied to each element.</returns>
    static member inline map<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)                              
                and 'T : struct
                and 'T :> ValueType>
                (fv:Numerics.Vector<'T> -> Numerics.Vector<'T>)
                (f: 'T -> 'T)
                (v: 'T[]) : 'T[] =

        let length = v.Length
        let slotSize = Numerics.Vector<'T>.Count
        let slotCount = length / slotSize
        let ceiling = slotSize * slotCount

        let results = Array.zeroCreate<'T> length
        let rsSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(results.AsSpan())
        let vSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v.AsSpan())

        for i = 0 to slotCount - 1 do
            rsSpan.[i] <- fv vSpan[i]

        for i = ceiling to length - 1 do
            results.[i] <- f v.[i]

        results

    /// <summary>
    /// Applies an element-wise binary operation on two arrays using SIMD for performance.
    /// </summary>
    /// <typeparam name="'T">The numeric type of the array elements.</typeparam>
    /// <param name="fv">A function that performs the SIMD operation on two vectors.</param>
    /// <param name="f">A fallback function for scalar operations.</param>
    /// <param name="v1">The first input array.</param>
    /// <param name="v2">The second input array.</param>
    /// <returns>An array with the operation applied to each pair of elements.</returns>
    static member inline map2Unchecked<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (fv:Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>) 
                (f:'T -> 'T -> 'T) 
                (v1: 'T[]) 
                (v2: 'T[])  =

        let length = v1.Length
        let slotSize= Numerics.Vector<'T>.Count
        let slotCount = length / slotSize
        let ceiling = slotSize * slotCount

        let results = Array.zeroCreate<'T> length
        let rsSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(results.AsSpan())
        let v1Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v1.AsSpan())
        let v2Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v2.AsSpan())

        for i=0 to slotCount - 1 do
            rsSpan.[i] <- fv v1Span[i] v2Span[i]

        for i=ceiling to v1.Length - 1 do
            results.[i] <- f v1.[i] v2[i]

        results

    /// <summary>
    /// Reduces an array to a single value using a binary operation, with SIMD optimization.
    /// </summary>
    /// <typeparam name="'T">The numeric type of the array elements.</typeparam>
    /// <param name="fv">A function that performs the SIMD reduction on two vectors.</param>
    /// <param name="f">A fallback function for scalar reduction.</param>
    /// <param name="zero">The initial value for the reduction.</param>
    /// <param name="v">The input array.</param>
    /// <returns>The result of the reduction.</returns>
    static member inline fold<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (fv:Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>)
                (f: 'T -> 'T -> 'T)
                (zero: 'T)
                (v: 'T[]) : 'T =

        let length = v.Length
        let slotSize = Numerics.Vector<'T>.Count
        let slotCount = length / slotSize
        let ceiling = slotSize * slotCount

        let vSpan: Span<Numerics.Vector<'T>> = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v.AsSpan())

        let mutable accVec = Numerics.Vector<'T>(zero)

        for i = 0 to slotCount - 1 do
            accVec <- fv accVec vSpan[i]

        let mutable acc = accVec.[0]
        for i = 1 to slotSize - 1 do
            acc <- f acc accVec.[i]

        for i = ceiling to length - 1 do
            acc <- f acc v[i]

        acc

    /// <summary>
    /// Applies an element-wise binary operation between an array and a scalar using SIMD for performance.
    /// </summary>
    /// <typeparam name="'T">The numeric type of the array elements.</typeparam>
    /// <param name="fv">A function that performs the SIMD operation on a vector and a scalar vector.</param>
    /// <param name="f">A fallback function for scalar operations.</param>
    /// <param name="v1">The input array.</param>
    /// <param name="scalar">The scalar value.</param>
    /// <returns>An array with the operation applied to each element and the scalar.</returns>
    static member inline mapScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (fv:Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T> ) 
                (f:'T -> 'T -> 'T) 
                (v1:'T[]) 
                (scalar:'T) : 'T[] =
        
        let length = v1.Length
        let slotSize= Numerics.Vector<'T>.Count
        let slotCount = length / slotSize
        let ceiling = slotSize * slotCount

        let results = Array.zeroCreate<'T> length
        let rsSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(results.AsSpan())
        let v1Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v1.AsSpan())
        let scalarVec = Numerics.Vector(scalar)

        for i=0 to slotCount - 1 do
            rsSpan.[i] <- fv v1Span[i] scalarVec

        for i=ceiling to v1.Length - 1 do
            results.[i] <- f v1.[i] scalar

        results


    static member inline mapFoldUnchecked<'T
            when 'T :> Numerics.INumber<'T>
            and 'T : (new: unit -> 'T)
            and 'T : struct
            and 'T :> ValueType>
            (fMapSimd: Numerics.Vector<'T> -> Numerics.Vector<'T>,
             fMap: 'T -> 'T,
             fReduceSimd: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>,
             fReduceScalar: 'T -> 'T -> 'T,
             zero: 'T,
             input: ReadOnlySpan<'T>)
            : 'T =

            let len = input.Length
            let simdSize = Numerics.Vector<'T>.Count
            let simdBlocks = len / simdSize
            let tailStart = simdBlocks * simdSize

            // SIMD map + accumulation
            let mutable accVec = Numerics.Vector<'T>(zero)
            let inputVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(input)

            for i = 0 to simdBlocks - 1 do
                let mapped = fMapSimd inputVec[i]
                accVec <- fReduceSimd accVec mapped

            // Reduce SIMD accumulator to scalar
            let mutable acc = accVec.[0]
            for i = 1 to simdSize - 1 do
                acc <- fReduceScalar acc accVec.[i]

            // Scalar tail
            for i = tailStart to len - 1 do
                acc <- fReduceScalar acc (fMap input[i])

            acc



type SIMDRangeUtils =
    
    /// In-place transformation of v[startIndex..(startIndex + count - 1)]
    /// using a SIMD operation (fv) for chunked parts and a scalar function (f) for leftovers.
    static member inline mapRangeInPlace<'T when 'T :> Numerics.INumber<'T> 
                and 'T : (new: unit -> 'T)
                and 'T : struct 
                and 'T :> ValueType>
        (fv : Numerics.Vector<'T> -> Numerics.Vector<'T>)
        (f  : 'T -> 'T)
        (startIndex : int)
        (count : int)
        (v : 'T[]) : unit =
        
        // 1) Validate parameters
        if startIndex < 0 || count < 0 || (startIndex + count) > v.Length then
            invalidArg (nameof count) "Invalid start index or count."

        // 2) Early exit if there's nothing to do
        if count = 0 then
            // nothing to do
            ()

        else
            // 3) Compute how many full Vector<'T>.Count chunks we can process
            let simdSize = Numerics.Vector<'T>.Count
            let simdCount = count / simdSize
            let tailStart = simdCount * simdSize

            // 4) Slice the relevant portion of v
            let span = v.AsSpan(startIndex, count)
            // 5) Interpret that span as a span of Vector<'T>
            let vecSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(span)

            // 6) Apply fv to each SIMD chunk
            for i in 0 .. simdCount - 1 do
                vecSpan.[i] <- fv vecSpan.[i]

            // 7) Handle leftover elements with f
            for i in tailStart .. count - 1 do
                let idx = startIndex + i
                v.[idx] <- f v.[idx]



    /// In-place combination of two arrays (dst, src) over 'count' elements,
    /// starting from 'dstStartIndex' and 'srcStartIndex' respectively.
    /// Uses 'fv' for chunked SIMD processing and 'f' for leftover scalar elements.
    static member inline map2RangeInPlace<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and  'T : struct
                and  'T :> ValueType>
        (fv  : Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>) 
        (f   : 'T -> 'T -> 'T)
        (dstStartIndex : int)
        (srcStartIndex : int)
        (count         : int)
        (dst : 'T[])
        (src : 'T[]) : unit =
        
        // 1) Validate parameters
        if dstStartIndex < 0 || srcStartIndex < 0 || count < 0 then
            invalidArg (nameof count) "Start indices and count must be non-negative."
        if dstStartIndex + count > dst.Length then
            invalidArg (nameof dstStartIndex) "Destination range out of bounds."
        if srcStartIndex + count > src.Length then
            invalidArg (nameof srcStartIndex) "Source range out of bounds."

        // 2) Early exit if there's nothing to do
        if count = 0 then
            ()
        else
            // 3) How many full 'Vector<'T>' chunks fit into 'count'?
            let simdSize  = Numerics.Vector<'T>.Count
            let simdCount = count / simdSize
            let tailStart = simdCount * simdSize
            
            // 4) Create Spans for the subranges
            let dstSpan = dst.AsSpan(dstStartIndex, count)
            let srcSpan = src.AsSpan(srcStartIndex, count)

            // 5) Cast to Span<Vector<'T>> for chunked SIMD ops
            let dstVecSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(dstSpan)
            let srcVecSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(srcSpan)

            // 6) Apply 'fv' to each pair of SIMD chunks
            for i = 0 to simdCount - 1 do
                dstVecSpan.[i] <- fv dstVecSpan.[i] srcVecSpan.[i]

            // 7) Handle leftover scalar elements
            for i = tailStart to count - 1 do
                let dstIdx = dstStartIndex + i
                let srcIdx = srcStartIndex + i
                dst.[dstIdx] <- f dst.[dstIdx] src.[srcIdx]


    /// Dot product of arr1[off1..off1+count-1] and arr2[off2..off2+count-1].
    /// Uses SIMD if available; otherwise a naive loop.
    static member inline dotRange<'T when 'T :> Numerics.INumber<'T> 
                    and 'T : (new: unit -> 'T)
                    and 'T : struct 
                    and 'T :> ValueType>
        (arr1: 'T[]) (off1: int)
        (arr2: 'T[]) (off2: int)
        (count: int) =
    
        if Numerics.Vector.IsHardwareAccelerated && count >= Numerics.Vector<'T>.Count then
            let simdSize  = Numerics.Vector<'T>.Count
            let simdCount = count / simdSize
            let tailStart = simdCount * simdSize

            let mutable sumVec = Numerics.Vector<'T>.Zero

            let span1 = arr1.AsSpan(off1, count)
            let span2 = arr2.AsSpan(off2, count)

            let vec1 = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(span1)
            let vec2 = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(span2)

            for i = 0 to simdCount - 1 do
                sumVec <- sumVec + (vec1.[i] * vec2.[i])

            // Reduce the SIMD lanes
            let mutable simdSum = LanguagePrimitives.GenericZero<'T>
            for lane = 0 to simdSize - 1 do
                simdSum <- simdSum + sumVec.[lane]

            // Handle leftover tail
            let mutable tailSum = LanguagePrimitives.GenericZero<'T>
            for i = tailStart to count - 1 do
                tailSum <- tailSum + arr1.[off1 + i] * arr2.[off2 + i]

            simdSum + tailSum
        else
            // Fallback naive loop
            let mutable acc = LanguagePrimitives.GenericZero<'T>
            for i = 0 to count - 1 do
                acc <- acc + arr1.[off1 + i] * arr2.[off2 + i]
            acc


// 

//     // 
//     static member map2Unchecked<'T when 'T :> Numerics.INumber<'T>
//                 and 'T : (new: unit -> 'T)
//                 and 'T : struct
//                 and 'T :> ValueType>
//                 (fv:Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>) 
//                 (f:'T -> 'T -> 'T) 
//                 (v1: 'T[]) 
//                 (v2: 'T[]) : 'T[] =
        

//         // Gets the total number of elements of this vector
//         let length = v1.Length
//         // 
//         let slotSize= Numerics.Vector<'T>.Count
//         // Number of slots (rasterized vectors)
//         let slotCount = length / slotSize
//         // Index at which the non rasterd remain starts
//         let ceiling = slotSize * slotCount

//         // Allocate the result array
//         let results = Array.zeroCreate<'T> length
//         let rsSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(results.AsSpan())
//         let v1Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v1.AsSpan())
//         let v2Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v2.AsSpan())

//         // Perform SIMD addition for chunks
//         for i=0 to slotCount - 1 do
//             rsSpan.[i] <- fv v1Span[i] v2Span[i]

//         // Handle the remaining elements
//         for i=ceiling to v1.Length - 1 do
//             results.[i] <- f v1.[i] v2[i]

//         results

// type SIMDMatrixUtils() =

//     static member map2<'T when 'T :> Numerics.INumber<'T>
//                 and 'T : (new: unit -> 'T)
//                 and 'T : struct
//                 and 'T :> ValueType> 
//                 (fVec: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>)
//                 (fScalar: 'T -> 'T -> 'T)
//                 (a: Matrix<'T>)
//                 (b: Matrix<'T>)
//                 : Matrix<'T> =

//         if a.Rows <> b.Rows || a.Cols <> b.Cols then
//             invalidArg "b" "Matrix dimensions must match"

//         let len = a.Data.Length
//         let result = Array.zeroCreate<'T> len

//         let simdSize = Numerics.Vector<'T>.Count
//         let simdCount = len / simdSize
//         let tailStart = simdCount * simdSize

//         // Cast arrays to Vector<'T> spans
//         let aVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(a.Data.AsSpan())
//         let bVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(b.Data.AsSpan())
//         let rVec = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(result.AsSpan())

//         for i = 0 to simdCount - 1 do
//             rVec.[i] <- fVec aVec.[i] bVec.[i]

//         for i = tailStart to len - 1 do
//             result.[i] <- fScalar a.Data.[i] b.Data.[i]

//         Matrix(a.Rows, a.Cols, result)


//     static member mapScalar<'T when 'T :> Numerics.INumber<'T>
//                 and 'T : (new: unit -> 'T)
//                 and 'T : struct
//                 and 'T :> ValueType> 
//                 (fv: Numerics.Vector<'T> -> Numerics.Vector<'T> -> Numerics.Vector<'T>)
//                 (f: 'T -> 'T -> 'T)
//                 (matrix: Matrix<'T>)
//                 (scalar: 'T)
//                 : Matrix<'T> =

//         let length = matrix.Data.Length
//         let result = Array.zeroCreate<'T> length

//         let slotSize = Numerics.Vector<'T>.Count
//         let slotCount = length / slotSize
//         let ceiling = slotCount * slotSize

//         let scalarVec = Numerics.Vector<'T>(scalar)

//         let mSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(matrix.Data.AsSpan())
//         let rSpan = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(result.AsSpan())

//         // SIMD chunk-wise operation
//         for i = 0 to slotCount - 1 do
//             rSpan.[i] <- fv mSpan.[i] scalarVec

//         // Scalar fallback for remainder
//         for i = ceiling to length - 1 do
//             result.[i] <- f matrix.Data.[i] scalar

//         Matrix(matrix.Rows, matrix.Cols, result)
