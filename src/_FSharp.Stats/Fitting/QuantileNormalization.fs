namespace FSharp.Stats.Fitting

(*
Module for quantile normalization
*)
module QuantileNormalization =    

    open FSharp.Stats

    /// Computes the quantile normalization of a given dataset  
    // http://en.wikipedia.org/wiki/Quantile_normalization
    // technique for making two distributions or more identical in statistical properties.
    // to normalize two or more distributions to each other, rank the original values and group them by rank, then set to the average of the original values.         
    let quantileNorm (colSeq : seq<array<float>>) =
        
        // Helper function to group RankedValue by rank and calculate average of orignal values
        let groupByAndAverage (input: seq<float*float>) =
            input
            |> Seq.groupBy fst
            |> Seq.map (fun (key, values) -> key, values |> Seq.averageBy snd)
        

        // Transform values to their ranks, value pairs
        let rawRanks = 
            colSeq
            |> Seq.map (fun col -> 
                let colRank = Rank.rankAverage col
                col |> Array.mapi (fun i v -> colRank.[i],v)
                ) 
                    
        // Calculate rank to average value mappin
        let rankValueMap =
            rawRanks
            |> Seq.concat
            |> groupByAndAverage
            |> Map.ofSeq
        
        // Get normalized values based on the rank of the original values
        rawRanks
        |> Seq.map (fun col -> 
                        col |> Array.map (fun (r,v) -> if rankValueMap.ContainsKey r then rankValueMap.[r] else nan ))
            


