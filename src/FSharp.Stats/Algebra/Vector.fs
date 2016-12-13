namespace FSharp.Stats.Algebra


    
/// Generic vector type
[<NoEquality; NoComparison>]
type Vector<'T when 'T : (static member Zero : 'T)
                and 'T : (static member One : 'T)
                and 'T : (static member (+) : 'T * 'T -> 'T)
                and 'T : (static member (-) : 'T * 'T -> 'T)
                and 'T : (static member (*) : 'T * 'T -> 'T)
                and 'T : (static member (/) : 'T * 'T -> 'T)
                and 'T : (static member (~-) : 'T -> 'T)
                and 'T : (static member Abs : 'T -> 'T)
                and 'T : (static member Pow : 'T * 'T -> 'T)
                and 'T : (static member Sqrt : 'T -> 'T)
                and 'T : (static member op_Explicit : 'T -> float)
                and 'T : comparison> =

    | ZeroVector of 'T
    | Vector of 'T[]

    /// ZeroVector
    static member inline Zero = ZeroVector LanguagePrimitives.GenericZero<'T>

    /// Converts vector `v` to float[]
    static member inline op_Explicit(v:Vector<'T>) =
        match v with
        | Vector v -> Array.map float v
        | ZeroVector _ -> [||]


    /// The element of this vector at the given position `i`
    member inline v.Item
        with get i =
            match v with
            | Vector v -> v.[i]
            | ZeroVector z -> z
        and set i vv =
            match v with
            | Vector v -> v.[i] <- vv
            | ZeroVector _ -> ()


    /// Gets a subvector between bounds `lower` and `upper`
    member inline v.GetSlice(lower, upper) =
        match v with
        | Vector v ->
            let l = defaultArg lower 0
            let u = defaultArg upper (v.Length - 1)
            if l > u then invalidArg "" "Given slice bounds are invalid."
            Vector v.[l..u]
        | ZeroVector _ -> invalidArg "" "Cannot get slice of a ZeroVector."


    /// Gets the first element of this vector
    member inline v.FirstItem =
        match v with
        | Vector v -> v.[0]
        | ZeroVector z -> z


    /// Gets the total number of elements of this vector
    member inline v.Length =
        match v with
        | Vector v -> v.Length
        | ZeroVector _ -> 0




    //[<RequireQualifiedAccess>]
    module Vector =
    

        let IsHardwareAccelerated = 
            System.Numerics.Vector.IsHardwareAccelerated

        let inline min v =
            match v with        
            | Vector v ->         
                let count = System.Numerics.Vector< 'T>.Count
                let rec loop i cMinVec =
                    if i <= v.Length-count then 
                        let cVec = System.Numerics.Vector< 'T>(v,i)                   
                        loop (i+count) (System.Numerics.Vector.Min(cMinVec,cVec))
                    else 
                        let cVec = System.Numerics.Vector< 'T>(v,v.Length-count)
                        System.Numerics.Vector.Min(cMinVec,cVec)
            
                let minVec = 
                    loop count (System.Numerics.Vector< 'T>(v,0))
            
                let mutable cmin = minVec.[0]
                for i=1 to count-1 do 
                    cmin <- if cmin < minVec.[i] then cmin else minVec.[i]  
                cmin
        
            | ZeroVector t -> t


        let inline neg v  =
             match v with
             | Vector v' -> SIMDUtil.map (System.Numerics.Vector.Negate) v' |> Vector   
             | ZeroVector t -> ZeroVector (-t)


        let inline abs v  =
            match v with
            | Vector v -> 
                SIMDUtil.map (System.Numerics.Vector.Abs) v 
                |> Vector   
            | ZeroVector t -> ZeroVector (abs t)


        let inline sqrt v  =
            match v with
            | Vector v -> 
                SIMDUtil.map (System.Numerics.Vector.SquareRoot) v 
                |> Vector   
            | ZeroVector t -> ZeroVector (sqrt t)



        let inline add a b =
            match a, b with
            | Vector a, Vector b -> 
                SIMDUtil.map2 (+) a b
                |> Vector
            | Vector _, ZeroVector _ -> a
            | ZeroVector _, Vector _ -> b
            | ZeroVector _, ZeroVector _ -> Vector.Zero        


        let inline subtract a b =
            match a, b with
            | Vector a, Vector b -> 
                SIMDUtil.map2 (-) a b
                |> Vector
            | Vector _, ZeroVector _ -> a
            | ZeroVector _, Vector _ -> b
            | ZeroVector _, ZeroVector _ -> Vector.Zero     

                
                