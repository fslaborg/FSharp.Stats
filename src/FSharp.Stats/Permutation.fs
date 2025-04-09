namespace FSharp.Stats



/// <summary>
/// A permutation represented as a function from indices to indices.
/// If <c>P</c> is a <c>Permutation</c>, then <c>P(i)</c> gives the
/// new row index for row <c>i</c> (or vice versa).
/// </summary>
type Permutation = int -> int


/// <summary>
/// The <c>Permutation</c> module defines a type and helper functions
/// for representing and constructing permutations from integer arrays.
/// </summary>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Permutation =

    /// <summary>
    /// Validates that the given integer array <paramref name="arr"/> is a valid
    /// permutation of length <c>n</c> (i.e., it contains every integer from
    /// <c>0</c> to <c>n-1</c> exactly once), then returns a function 
    /// <c>P(i) = arr[i]</c>. <br/>
    /// Throws an exception if <paramref name="arr"/> is invalid.
    /// </summary>
    /// <param name="arr">
    /// The array containing the permutation. Each element <c>arr[i]</c> must
    /// be a unique integer in <c>[0..n-1]</c>.
    /// </param>
    /// <returns>
    /// A permutation function <c>Permutation</c> where <c>P(i) = arr[i]</c>.
    /// </returns>
    let ofFreshArray (arr: int[]) : Permutation =
        let n = arr.Length
        let visited = Array.create n false

        // Validate that arr[i] is unique and in [0..n-1].
        for i = 0 to n - 1 do
            let x = arr.[i]
            if x < 0 || x >= n then
                invalidArg (nameof arr) "Permutation array contains out-of-range index."
            if visited.[x] then
                invalidArg (nameof arr) "Permutation array contains duplicate indices."
            visited.[x] <- true

        // Return a function P(i) = arr[i].
        fun i ->
            if i < 0 || i >= n then
                invalidArg "i" "Permutation function called with out-of-range index."
            arr.[i]

    /// <summary>
    /// Makes a copy of <paramref name="arr"/>, then calls <see cref="ofFreshArray"/>.
    /// Useful for preserving the original array. 
    /// </summary>
    /// <param name="arr">An array of length <c>n</c> that should represent 
    /// a valid permutation of <c>0..n-1</c>.</param>
    /// <returns>
    /// A <c>Permutation</c> function representing the same reordering as
    /// <paramref name="arr"/>.
    /// </returns>
    let ofArray (arr: int[]) : Permutation =
        // Defensive copy
        let copy = Array.copy arr
        ofFreshArray copy


    let ofPairs  (mappings: seq<int * int>) = 
      let p = dict mappings 
      (fun k -> if p.ContainsKey k then p.[k] else k)
    

    let swap (n:int) (m:int) = 
      (fun k -> if k = n then m elif k = m then n else k)

    let reversal size = 
      if size <= 0 then invalidArg "size" "a permutation size must be positive";
      (fun k -> (size - 1 - k))

    let rotation size distance = 
      if size <= 0 then invalidArg "size" "a permutation size must be positive";
      if abs distance >= size then invalidArg "distance" "the absolute value of the distance must be less than the size of the permutation";
      (fun k -> (k + size + distance) % size)

    let identity (k:int) = k
    
    let inverse size p =
        if size <= 0 then invalidArg "size" "a permutation size must be positive";
        let arr2 = Array.zeroCreate size
        for i = 0 to size - 1 do
             arr2.[p i] <- i
        ofFreshArray arr2
