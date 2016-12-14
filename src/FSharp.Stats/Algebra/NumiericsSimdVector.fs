namespace FSharp.Stats.Algebra



module NumericsSimdVector =
    
    /// Folds over System.Numerics.Vector
    let inline fold
        (folder: 'State -> 'T -> 'State)
        (state : 'State)
        (v: 'T System.Numerics.Vector) : 'State =
        
        let count = System.Numerics.Vector< 'T>.Count
        let rec loop c acc =
            if c < count then
                loop (c+1) (folder acc v.[c])
            else
                acc 
        loop 0 state


    let inline ofArrayAt index (array : 'T[]) =
        let count = System.Numerics.Vector< 'T>.Count
        if index+count <= array.Length then
            System.Numerics.Vector< 'T>(array,index)
        else
            let tmpT = 
                Array.init count 
                    (fun i -> 
                        if i+index < array.Length then array.[i+index] 
                        else (Unchecked.defaultof< 'T>) )
            System.Numerics.Vector< 'T>(tmpT,0)
                    
