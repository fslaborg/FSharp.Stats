namespace FSharp.Stats

open System
open System.Runtime.InteropServices

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector =
    
    /// <summary>Computes the vector cross product a^Tb</summary>
    /// <param name="colvec">The first vector is column vector.</param>
    /// <param name="rowvec">The second vector is the row vector.</param> 
    /// <returns>The cross product (or outer product) of the two vectors.</returns>
    let inline cross<'T when 'T :> Numerics.INumber<'T>                
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T : equality
                and 'T :> ValueType> (colvec: Vector<'T>) (rowvec:Vector<'T>)  : Matrix<'T> =
        //TODO: SIMD Acceleration!
        
        if colvec.Length <> rowvec.Length then
            invalidArg "" "Vector must have the same length to compute the dot product."
        let n = colvec.Length
        let m = rowvec.Length
    
        // Allocate the n*m storage
        let data = Array.zeroCreate<'T> (n * m)
    
        // Fill with colvec[i] * rowvec[j], typical row-major arrangement
        for i in 0 .. n - 1 do
            for j in 0 .. m - 1 do
                data.[i * m + j] <- colvec.[i] * rowvec.[j]
    
        // Build the matrix (n x m)
        Matrix<'T>(n, m, data)


    /// Indexed fold over a vector
    let inline foldi<'T when 'T :> Numerics.INumber<'T>> f (state: 'T) (v: Vector<'T>) : 'T =
        let mutable acc = state
        for i = 0 to v.Length - 1 do
            acc <- f i acc v.[i]
        acc

    /// Indexed map over a vector
    let inline mapi<'T when 'T :> Numerics.INumber<'T>> (f: int -> 'T -> 'T) (v: Vector<'T>) : Vector<'T> =
        Array.mapi f v

    /// Filter vector elements by predicate
    let inline filter<'T when 'T :> Numerics.INumber<'T>> (predicate: 'T -> bool) (v: Vector<'T>) : Vector<'T> =
        Array.filter predicate v

    /// Initialize vector by index-based generator 
    let inline init<'T when 'T :> Numerics.INumber<'T>> (n: int) (f: int -> 'T) : Vector<'T> =
        Array.init n f

    /// Extract a slice from a vector
    let inline slice<'T when 'T :> Numerics.INumber<'T>> (start: int) (length: int) (v: Vector<'T>) : Vector<'T> =
        Array.sub v start length

    /// Find index of maximum value
    let inline argmax<'T when 'T :> Numerics.INumber<'T> and 'T : comparison> (v: Vector<'T>) : int =
        let mutable maxIdx = 0
        let mutable maxVal = v.[0]
        for i = 1 to v.Length - 1 do
            if v.[i] > maxVal then
                maxVal <- v.[i]
                maxIdx <- i
        maxIdx

    /// Find index of minimum value
    let inline argmin<'T when 'T :> Numerics.INumber<'T> and 'T : comparison> (v: Vector<'T>) : int =
        let mutable minIdx = 0
        let mutable minVal = v.[0]
        for i = 1 to v.Length - 1 do
            if v.[i] < minVal then
                minVal <- v.[i]
                minIdx <- i
        minIdx

    /// Pad vector to a given length with a constant value
    let inline padRight<'T when 'T :> Numerics.INumber<'T>> (length: int) (value: 'T) (v: Vector<'T>) : Vector<'T> =
        if v.Length >= length then v
        else
            Array.init length (fun i -> if i < v.Length then v.[i] else value)

    /// Zip two vectors into (index, left, right)
    let inline zip<'T when 'T :> Numerics.INumber<'T>> (v1: Vector<'T>) (v2: Vector<'T>) : (int * 'T * 'T)[] =
        Array.init (min v1.Length v2.Length) (fun i -> i, v1.[i], v2.[i])



    /// Find index of minimum value using projection
    let inline argminBy<'T, 'U
        when 'T :> Numerics.INumber<'T> and 'U : comparison>
        (f: 'T -> 'U) (v: Vector<'T>) : int =
        let mutable minIdx = 0
        let mutable minVal = f v.[0]
        for i = 1 to v.Length - 1 do
            let candidate = f v.[i]
            if candidate < minVal then
                minVal <- candidate
                minIdx <- i
        minIdx

    /// Find index of maximum value using projection
    let inline argmaxBy<'T, 'U
        when 'T :> Numerics.INumber<'T> and 'U : comparison>
        (f: 'T -> 'U) (v: Vector<'T>) : int =
        let mutable maxIdx = 0
        let mutable maxVal = f v.[0]
        for i = 1 to v.Length - 1 do
            let candidate = f v.[i]
            if candidate > maxVal then
                maxVal <- candidate
                maxIdx <- i
        maxIdx

    /// Try to find index of first matching element
    let inline tryFindIndex<'T when 'T :> Numerics.INumber<'T>> (predicate: 'T -> bool) (v: Vector<'T>) : int option =
        let mutable i = 0
        let mutable found = false
        while i < v.Length && not found do
            if predicate v.[i] then found <- true
            else i <- i + 1
        if found then Some i else None

    /// Find index of first matching element or throw
    let inline findIndex<'T when 'T :> Numerics.INumber<'T>> (predicate: 'T -> bool) (v: Vector<'T>) : int =
        match tryFindIndex predicate v with
        | Some i -> i
        | None -> failwith "Element not found."

    /// Enumerate non-zero elements with index
    let inline enumerateNonZero<'T when 'T :> Numerics.INumber<'T> and 'T : equality> (v: Vector<'T>) : (int * 'T)[] =
        v
        |> Array.mapi (fun i x -> i, x)
        |> Array.filter (fun (_, x) -> x <> 'T.Zero)

    /// Split a vector into prefix and suffix at a given index
    let inline split<'T when 'T :> Numerics.INumber<'T>> (index: int) (v: Vector<'T>) : Vector<'T> * Vector<'T> =
        Array.sub v 0 index, Array.sub v index (v.Length - index)

    /// Chunk a vector into equally sized pieces (last may be shorter)
    let inline chunk<'T when 'T :> Numerics.INumber<'T>> (chunkSize: int) (v: Vector<'T>) : Vector<'T>[] =
        let len = v.Length
        [| for i in 0 .. chunkSize .. len - 1 ->
            let size = min chunkSize (len - i)
            Array.sub v i size |]

    /// Sliding window over the vector
    let inline windowed<'T when 'T :> Numerics.INumber<'T>> (windowSize: int) (v: Vector<'T>) : Vector<'T>[] =
        if windowSize > v.Length then [||]
        else
            [| for i in 0 .. v.Length - windowSize ->
                Array.sub v i windowSize |]


    /// <summary>
    /// Splits the vector `v` into two parts based on `indices`.
    /// The elements at positions in `indices` end up in `nvi`,
    /// all others go into `nv`.
    /// </summary>
    /// <param name="indices">Zero-based positions to extract from `v`.</param>
    /// <param name="v">The full vector to split.</param>
    /// <returns>(nvi, nv): nvi contains the elements at `indices`, nv contains the rest.</returns>
    let splitVector (indices: int[]) (v: Vector<'T>) =
        
        Array.sortInPlace indices

        let n = v.Length
        let k = indices.Length

        let nvi = Vector.zeroCreate<'T> k
        let nv  = Vector.zeroCreate<'T> (n - k)

        let mutable iInd = 0
        let mutable iNvi = 0
        let mutable iNv  = 0

        for i = 0 to n - 1 do
            // If we've not exhausted the `indices` array AND the current i matches indices.[iInd]
            if iInd < k && i = indices.[iInd] then
                nvi.[iNvi] <- v.[i]
                iNvi <- iNvi + 1
                iInd <- iInd + 1
            else
                nv.[iNv] <- v.[i]
                iNv <- iNv + 1

        nvi, nv

    /// <summary>
    /// Creates a new vector that is the result of applying the permutation <paramref name="P"/>
    /// to the vector <paramref name="b"/>. The element <c>result.[i]</c> becomes <c>b.[P(i)]</c>.
    /// </summary>
    /// <param name="P">A permutation function of type <c>int -&gt; int</c>, 
    /// <param name="b">The original vector to be permuted. Must have length n.</param>
    /// typically created by <c>Permutation.ofArray</c> or <c>Permutation.ofFreshArray</c>.</param>
    /// <returns>
    /// A new vector of the same length n, reordered according to <paramref name="P"/>.
    /// </returns>
    let permuteBy (P: Permutation) (b: Vector<'T>) : Vector<'T> =
        let n = b.Length
        let result = Array.zeroCreate<'T> n
        for i = 0 to n - 1 do
            result.[i] <- b.[P i]
        result

    ///
    let inline ofSeq (s:seq<'T>) : Vector<'T> =
        s |> Array.ofSeq


    ///
    let inline pow (power: 'T) (v:Vector<'T>) : Vector<'T> =
        v |> Array.map (fun x -> GenericMath.pow x power)


    /// Returns a subvector from offset i to the end of the vector.
    let inline sub (i: int) (v: Vector<'T>) : Vector<'T> =
        if i < 0 || i > v.Length then
            invalidArg (nameof i) "Index out of bounds."
        let len = v.Length - i
        let res = Array.zeroCreate<'T> len
        Array.blit v i res 0 len
        res