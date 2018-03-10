namespace FSharp.Stats


/// Module to Calculate the rank. The rank of a number is its size relative to other values in a sequence
module Rank =

    /// Ranks each entry of the given unsorted data array. Use 'breakTies function to break ties
    let inline private rank (breakTies: int -> int -> 'b) (convert: int -> 'b) (data:array<'a>) : array<'b> =
        let zero = LanguagePrimitives.GenericZero< 'b > 
        //let ranks  = Array.copy data
        let data' = Array.copy data
        let ranks  = Array.create data.Length zero
        let index = Array.init data.Length id
        System.Array.Sort(data',index)

        let setTies a b =
            let tmp = breakTies a b
            for j = a to b-1 do
                ranks.[index.[j]] <- tmp

        let rec loop i pi =
            if i < data.Length then
                if (abs (data'.[i] - data'.[pi]) <= zero) then
                    loop (i+1) pi
                else
                    if (i = pi + 1) then
                        ranks.[index.[pi]] <- convert i
                    else
                        //break ties
                        setTies pi i

                    loop (i+1) (i)
            else
                //break ties if left over
                setTies pi i

        loop 1 0 |> ignore
        ranks


    /// Ranks each entry of the given unsorted data array.
    /// Permutation with increasing values at each index of ties.
    let inline rankFirst (data:array<_>) =        
        //let ranks  = Array.copy data
        let data' = Array.copy data
        let ranks  = Array.create data.Length 0
        let index = Array.init data.Length id     
        System.Array.Sort(data',index)

        for i=0 to ranks.Length-1 do
            ranks.[index.[i]] <- (i + 1)    

        ranks


    /// Ranks each entry of the given unsorted data array.
    /// Ties are replaced by their minimum  
    let inline rankMin (data:array<_>) =    
        let minTies a _ = (a + 1)
        rank minTies id data


    /// Ranks each entry of the given unsorted data array.
    /// Ties are replaced by their maximum  
    let inline rankMax (data:array<_>) =    
        let maxTies _ b = b
        rank maxTies id data


    /// Ranks each entry of the given unsorted data array.
    /// Ties are replaced by their mean
    let inline rankAverage (data:array<_>) =    
        let averageTies a b = float (b + a - 1) / float (b-a)
        rank averageTies float data



