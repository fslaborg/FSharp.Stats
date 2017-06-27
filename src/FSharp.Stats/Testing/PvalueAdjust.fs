namespace FSharp.Stats.Testing


module PvalueAdjust = 

    /// Benjamini-Hochberg Correction (BH)
    let Benjamini_Hochberg (rawp:seq<_*float>) =        
        // recursive function calculating cumulative minimum
        let rec cummin (pValues:List<_*float>) (nrPvalues:int) (i:int) (min:float) =      
            match pValues with
            | [] -> []
            | x::rest when System.Double.IsNaN(snd(x)) -> (fst(x),nan)::(cummin rest nrPvalues (i) min)//[(fst(x),nan)] @ (cummin rest nrPvalues (i) min)
            | x::rest  -> let prd = (float(nrPvalues)/float(nrPvalues-i))*snd(x)
                          let prd = if(prd > 1.0) then 1.0 else prd
                          let value = if(prd<=min) then prd else min
                          (fst(x),value) :: (cummin rest nrPvalues (i+1) value)//[(fst(x),value)] @ (cummin rest nrPvalues (i+1) value)
    
        let rawpListMinusNan = rawp |> Seq.filter (fun (_,x) -> not (System.Double.IsNaN x)) |> Seq.toList
        let rawpListNan = rawp |> Seq.filter (fun (_,x) -> (System.Double.IsNaN x)) |> Seq.toList
        let npval = Seq.length (rawpListMinusNan)
        //let npval = Seq.length rawp
        let sortedRawp =
            rawpListMinusNan
            |> List.sortWith(fun (_,x) (_,y) -> if (x) > (y) then -1 else 1)                           
        let adjp = cummin sortedRawp npval 0 System.Double.PositiveInfinity
        adjp @ rawpListNan









