namespace FSharp.Stats

open System
open System.Runtime.InteropServices

/// Vector as an array type alias
type Vector<'T when 'T :> Numerics.INumber<'T>> = 'T []


type Vector =
    
    
    static member inline zeroCreate<'T when 'T :> Numerics.INumber<'T>> count : Vector<'T> =
        Array.zeroCreate<'T> count


    /// Adds two vectors element-wise.
    /// <param name="v1">The first vector.</param>
    /// <param name="v2">The second vector.</param>
    /// <returns>A new vector containing the element-wise sum of the two input vectors.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the vectors have different lengths.</exception>    
    static member inline add<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1 : Vector<'T>) (v2 : Vector<'T>) : Vector<'T> =
        
        
        if v1.Length <> v2.Length then
            invalidArg "" "Cannot add two vectors of different dimensions."

        if Numerics.Vector.IsHardwareAccelerated then
            Acceleration.SIMDUtils.map2Unchecked (+) (+) v1 v2
        else
            Array.map2 (+) v1 v2


    /// Subtracts the second vector from the first vector element-wise.
    /// <param name="v1">The first vector.</param>
    /// <param name="v2">The second vector.</param>
    /// <returns>A new vector containing the element-wise difference of the two input vectors.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the vectors have different lengths.</exception> 
    static member inline subtract<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1 : Vector<'T>) (v2 : Vector<'T>) : Vector<'T> =

        if v1.Length <> v2.Length then
            invalidArg "" "Cannot subtract two vectors of different dimensions."

        if Numerics.Vector.IsHardwareAccelerated then
            Acceleration.SIMDUtils.map2Unchecked (-) (-) v1 v2
        else
            Array.map2 (-) v1 v2        
        

    /// Multiplies two vectors element-wise.
    /// <param name="v1">The first vector.</param>
    /// <param name="v2">The second vector.</param>
    /// <returns>A new vector containing the element-wise product of the two input vectors.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the vectors have different lengths.</exception>
    static member inline multiply<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1 : Vector<'T>) (v2 : Vector<'T>) : Vector<'T> =

        if v1.Length <> v2.Length then
            invalidArg "" "Cannot multiply two vectors of different dimensions."

        if Numerics.Vector.IsHardwareAccelerated then
            Acceleration.SIMDUtils.map2Unchecked ( * ) ( * ) v1 v2
        else
            Array.map2 ( * ) v1 v2          
        

    /// Divides the first vector by the second vector element-wise.
    /// <param name="v1">The first vector.</param>
    /// <param name="v2">The second vector.</param>
    /// <returns>A new vector containing the element-wise division of the two input vectors.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the vectors have different lengths.</exception>
    static member inline divide<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1 : Vector<'T>) (v2 : Vector<'T>) : Vector<'T> =
        if v1.Length <> v2.Length then
            invalidArg "" "Cannot divide two vectors of different dimensions."
  
        if Numerics.Vector.IsHardwareAccelerated then
            Acceleration.SIMDUtils.map2Unchecked ( / ) ( / ) v1 v2
        else
            Array.map2 ( / ) v1 v2   
        

    /// Adds a scalar to each element of the vector.
    /// <param name="v">The vector.</param>
    /// <param name="scalar">The scalar value to add.</param>
    /// <returns>A new vector with the scalar added to each element.</returns>
    static member inline addScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
            (scalar:'T) (v:Vector<'T>) : Vector<'T> =
        
        if Numerics.Vector.IsHardwareAccelerated then
            Acceleration.SIMDUtils.mapScalar (+) (+) v scalar
        else
            Array.map (fun x -> x + scalar) v          
        

    /// Subtracts a scalar from each element of the vector.
    /// <param name="v">The vector.</param>
    /// <param name="scalar">The scalar value to subtract.</param>
    /// <returns>A new vector with the scalar subtracted from each element.</returns>
    static member inline subtractScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
            (scalar:'T) (v:Vector<'T>) : Vector<'T> =
        if Numerics.Vector.IsHardwareAccelerated then
            Acceleration.SIMDUtils.mapScalar (-) (-) v scalar
        else
            Array.map (fun x -> x - scalar) v  
        

    /// Multiplies each element of the vector by a scalar.
    /// <param name="v">The vector.</param>
    /// <param name="scalar">The scalar value to multiply by.</param>
    /// <returns>A new vector with each element multiplied by the scalar.</returns>
    static member inline multiplyScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
            (scalar:'T) (v:Vector<'T>) : Vector<'T> =

        if Numerics.Vector.IsHardwareAccelerated then
            Acceleration.SIMDUtils.mapScalar ( * ) ( * ) v scalar
        else
            Array.map (fun x -> x * scalar) v        
        

    /// Divides each element of the vector by a scalar.
    /// <param name="v">The vector.</param>
    /// <param name="scalar">The scalar value to divide by.</param>
    /// <returns>A new vector with each element divided by the scalar.</returns>
    static member inline divideScalar<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
            (scalar:'T) (v:Vector<'T>) : Vector<'T> =
        
        if Numerics.Vector.IsHardwareAccelerated then
            Acceleration.SIMDUtils.mapScalar ( / ) ( / ) v scalar
        else
            Array.map (fun x -> x / scalar) v  

    /// Computes the sum of all elements in the vector.
    /// <param name="v">The vector.</param>
    /// <returns>The sum of all elements in the vector.</returns>
    static member inline sum<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (v:Vector<'T>) : 'T =
        let zero =  LanguagePrimitives.GenericZero<'T>
        if Numerics.Vector.IsHardwareAccelerated then
           Acceleration.SIMDUtils.fold (+) (+) zero v
        else
            Array.sum v

    /// Computes the product of all elements in the vector.
    /// <param name="v">The vector.</param>
    /// <returns>The product of all elements in the vector.</returns>
    static member inline product<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> 
                (v:Vector<'T>) : 'T =
        let one =  LanguagePrimitives.GenericOne<'T>
        if Numerics.Vector.IsHardwareAccelerated then
            Acceleration.SIMDUtils.fold (*) (*) one v
        else
            Array.fold ( * ) one v
        

    /// Computes the mean of the elements in the vector.
    /// <param name="v">The vector.</param>
    /// <returns>The mean of the elements in the vector.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the vector is empty.</exception>
    static member inline mean<'T when 'T :> Numerics.INumber<'T>
                and 'T : (static member DivideByInt : 'T * int -> 'T)
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType>
                (v: Vector<'T>) : 'T =
        if v.Length = 0 then invalidArg "v" "Cannot compute mean of empty vector."
        let sum = Vector.sum v
        LanguagePrimitives.DivideByInt<'T> sum v.Length

    /// Computes the dot product ab^T of two vectors. 
    /// <param name="v1">The first vector.</param>
    /// <param name="v2">The second vector.</param>
    /// <returns>The dot product of the two vectors.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the vectors have different lengths.</exception>
    static member inline dot<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType> (v1 : Vector<'T>) (v2 : Vector<'T>) : 'T =

        if v1.Length <> v2.Length then
            invalidArg "v2" "Vector must have the same length to compute the dot product."
        if Numerics.Vector.IsHardwareAccelerated then
            let length = v1.Length
            let slotSize = Numerics.Vector<'T>.Count
            let slotCount = length / slotSize
            let ceiling = slotSize * slotCount

            let v1Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v1.AsSpan())
            let v2Span = MemoryMarshal.Cast<'T, Numerics.Vector<'T>>(v2.AsSpan())
            let mutable result = Numerics.Vector<'T>.Zero
            //let mutable scalarResult = LanguagePrimitives.GenericZero<'T>

            for i = 0 to slotCount - 1 do
                result <- Numerics.Vector.Add(result, Numerics.Vector.Multiply(v1Span.[i], v2Span.[i])) 
                //scalarResult <- Numerics.Vector.Sum result
            
            let mutable scalarResult = Numerics.Vector.Sum result

            for i = ceiling to v1.Length - 1 do
                scalarResult <- scalarResult + (v1.[i] * v2.[i])

            scalarResult
        else
            Array.fold2 (fun acc x y -> acc + (x * y)) LanguagePrimitives.GenericZero v1 v2


    /// Computes the Euclidean norm (magnitude) of the vector.
    /// <param name="v">The vector.</param>
    /// <returns>The Euclidean norm of the vector.</returns>
    static member inline norm<'T when 'T :> Numerics.INumber<'T>
                and 'T : (new: unit -> 'T)
                and 'T : struct
                and 'T :> ValueType
                and 'T :> Numerics.IRootFunctions<'T>>
                (v:Vector<'T>) : 'T =
        
        let sumSquares  = Vector.dot v v
        'T.Sqrt sumSquares

    static member inline min<'T when 'T :> Numerics.INumber<'T>
                 and 'T : (new: unit -> 'T)
                 and 'T : struct
                 and 'T : comparison
                 and 'T :> ValueType> 
                 (v:Vector<'T>) : 'T =
        if v.Length = 0 then invalidArg "v" "Cannot compute min of empty vector."
        Acceleration.SIMDUtils.fold (fun a b -> Numerics.Vector.Min(a, b)) min v.[0] v

    static member inline max<'T when 'T :> Numerics.INumber<'T>
                 and 'T : (new: unit -> 'T)
                 and 'T : struct
                 and 'T : comparison
                 and 'T :> ValueType> 
                 (v:Vector<'T>) : 'T =
        if v.Length = 0 then invalidArg "v" "Cannot compute min of empty vector."
        Acceleration.SIMDUtils.fold (fun a b -> Numerics.Vector.Max(a, b)) max v.[0] v


