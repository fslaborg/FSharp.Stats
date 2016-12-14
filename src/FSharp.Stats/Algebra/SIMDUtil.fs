namespace FSharp.Stats.Algebra


// SIMD enhanced Array operations for F# 
module private SIMDUtil =

    open System.Numerics
    

    /// <summary>Builds a new array whose elements are the results of applying the given function
    /// to each of the elements of the array.</summary>
    ///
    /// <remarks></remarks>
    ///
    /// <param name="mapping">A function to transform items from the input sequence.</param>
    /// <param name="source">The input array.</param>
    ///
    /// <returns>The result array.</returns>
    ///
    /// <exception cref="System.ArgumentNullException">Thrown when the input sequence is null.</exception>            
    let inline map 
        (mapping : 'T Vector -> 'U Vector) (source : 'T[]) : 'U[] =
            let count = System.Numerics.Vector< 'T>.Count
            let result = Array.zeroCreate source.Length

            if source.Length < count then
                let tmpT = Array.init count (fun i -> if i < source.Length then source.[i] else (Unchecked.defaultof< 'T>) )
                let tmpU = Array.zeroCreate count 
                let cVec = System.Numerics.Vector< 'T>(tmpT,0)
                (mapping cVec).CopyTo(tmpU,0)
                Array.blit tmpU 0 result 0 source.Length
            
            else
            
                let rec loop i =
                    if i <= source.Length-count then 
                        let cVec = System.Numerics.Vector< 'T>(source,i)
                        (mapping cVec).CopyTo(result,i)
                        loop (i+count)
                    else 
                        let cVec = System.Numerics.Vector< 'T>(source,source.Length-count)
                        (mapping cVec).CopyTo(result,source.Length-count)
                loop 0
        
            result


    let inline map2 
        (vf : ^T Vector -> ^T Vector -> ^U Vector) (v1 : ^T[]) (v2 : ^T[]) : ^U[] =
            let count = System.Numerics.Vector< ^T>.Count
            if v1.Length <> v2.Length then invalidArg "" "Vectors must not have different dimensions."
            let result = Array.zeroCreate v1.Length

            if v1.Length < count then
                let tmpT1 = Array.init count (fun i -> if i < v1.Length then v1.[i] else (Unchecked.defaultof< ^T>) )
                let tmpT2 = Array.init count (fun i -> if i < v2.Length then v2.[i] else (Unchecked.defaultof< ^T>) )
                let tmpU = Array.zeroCreate count 
                let cVec1 = System.Numerics.Vector< 'T>(tmpT1,0)
                let cVec2 = System.Numerics.Vector< 'T>(tmpT2,0)
                (vf cVec1 cVec2).CopyTo(tmpU,0)
                Array.blit tmpU 0 result 0 v1.Length
            
            else
            
                let rec loop i =
                    if i <= v1.Length-count then 
                        let cVec1 = System.Numerics.Vector< 'T>(v1,i)
                        let cVec2 = System.Numerics.Vector< 'T>(v2,i)
                        (vf cVec1 cVec2).CopyTo(result,i)
                        loop (i+count)
                    else 
                        let cVec1 = System.Numerics.Vector< 'T>(v1,v2.Length-count)
                        let cVec2 = System.Numerics.Vector< 'T>(v2,v2.Length-count)
                        (vf cVec1 cVec2).CopyTo(result,v1.Length-count)
                loop 0
        
            result


    /// <summary>Applies a function to each element of the collection, threading an accumulator argument
    /// through the computation. If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes 
    /// <c>f (... (f s i0)...) iN</c></summary>
    /// <param name="folder">The function to update the state given the input elements.</param>
    /// <param name="stateIndex">The index to start from.</param>
    /// <param name="state">The initial state.</param>
    /// <param name="array">The input array.</param>
    /// <returns>The final state.</returns>
    /// <exception cref="System.ArgumentNullException">Thrown when the input array is null.</exception>
    let inline foldFrom
        (folder: 'State Vector -> 'T Vector -> 'State Vector)
        (startIndex : int)
        (state : 'State Vector)
        (array: 'T[]) : 'State Vector =

        //checkNonNull array
        
        let count = Vector< 'T>.Count
   
        if array.Length < count then
            let tmpT = 
                Array.init count 
                    (fun i -> if i+startIndex < array.Length then array.[i+startIndex] else (Unchecked.defaultof< 'T>) )
            let cVec = System.Numerics.Vector< 'T>(tmpT,0)
            folder state cVec
        else
            
            let rec loop i acc =
                if i <= array.Length-count then 
                    let cVec = System.Numerics.Vector< 'T>(array,i)
                    let acc' = folder acc cVec
                    loop (i+count) acc'
                else
                    let tmpT = 
                        Array.init count 
                            (fun ii -> if i+ii < array.Length then array.[i+ii] else (Unchecked.defaultof< 'T>) )                    
                    let cVec = System.Numerics.Vector< 'T>(tmpT,0)
                    folder acc cVec
            
            loop startIndex state
        
        

    let inline sum
        (array: 'T[]) : 'T =
        let state = System.Numerics.Vector< 'T>(Unchecked.defaultof< 'T>)
        foldFrom (+) 0 state array
        |> NumericsSimdVector.fold (+) (Unchecked.defaultof< 'T>)


    let inline sumBy
        (f: Vector< 'T> -> Vector< 'U>) 
        (array: 'T[]) : 'U =
        let state = System.Numerics.Vector< 'U>(Unchecked.defaultof< 'U>)
        let f' a b = System.Numerics.Vector.Add(a,(f b)) 
        foldFrom (f') 0 state array
        |> NumericsSimdVector.fold (+) (Unchecked.defaultof< 'U>)

