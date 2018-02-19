namespace FSharp.Stats

// SIMD enhanced Array operations for F# 
module internal SIMDUtil =

    open System.Numerics
    
            
    let inline map 
        (vf : ^T Vector -> ^U Vector) (v : ^T[]) : ^U[] =
            let count = System.Numerics.Vector< ^T>.Count
            let result = Array.zeroCreate v.Length

            if v.Length < count then
                let tmpT = Array.init count (fun i -> if i < v.Length then v.[i] else (Unchecked.defaultof< ^T>) )
                let tmpU = Array.zeroCreate count 
                let cVec = System.Numerics.Vector< 'T>(tmpT,0)
                (vf cVec).CopyTo(tmpU,0)
                Array.blit tmpU 0 result 0 v.Length
            
            else
            
                let rec loop i =
                    if i <= v.Length-count then 
                        let cVec = System.Numerics.Vector< 'T>(v,i)
                        (vf cVec).CopyTo(result,i)
                        loop (i+count)
                    else 
                        let cVec = System.Numerics.Vector< 'T>(v,v.Length-count)
                        (vf cVec).CopyTo(result,v.Length-count)
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



module Algebra = 
  
    
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

                
                

                                            


