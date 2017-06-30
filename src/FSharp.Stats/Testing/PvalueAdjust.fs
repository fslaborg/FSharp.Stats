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



    module Qvalue = 
        
        let a = nan 
 
//        //let lambda = [|0.0 ..0.05..0.9|]
//        let pi0_Bootstrap (lambda:float[]) (p:float[])  =
//            let pi0 = Array.init lambda.Length ( fun i -> ( (Descriptive.StatisticalMeasure.mean((p |> Array.map (fun v -> if (v >= lambda.[i]) then 1. else 0. ))) ) / (1. - lambda.[i]) ) )
//            let minpi0 = Array.min(pi0)    
//
//            let rnd = System.Random()
//
//            let rec floop (counter:int) (result:float[]) =    
//                if counter > 0 then
//                    let pboot    = Fitting.Bootstrap.sampleWithReplacement rnd p p.Length            
//                    let pi0boot = Array.init lambda.Length ( fun i -> ( (Descriptive.StatisticalMeasure.mean(pboot |> Array.map (fun v -> if (v >= lambda.[i]) then 1. else 0. ))) ) / (1. - lambda.[i]) ) 
//                    let mse = Array.map2 (fun m p -> m + ((p - minpi0)**2.0) ) result pi0boot
//                    floop (counter - 1) mse
//                else
//                    result
//
//            let mse = floop (100) (Array.zeroCreate (lambda.Length))    
//            let tmp = pi0.[(mse |> Array.findIndex(fun v -> v = (Array.min mse)))]
//            min (tmp) (1.0)
//
//
//        let pValueOfQValue (qvalues:float[]) =
//            // estimate of pi0 used to create the q-value that's the maximum q-value (or very, very close to it)
//            let pi0 = Array.max qvalues
//            // m0, the estimated number of true nulls
//            let m0 = float qvalues.Length * pi0
//            qvalues
//            |> Statistics.Descriptive.Rank.breakByMean
//            |> Seq.sortBy (fun r -> r.Position)
//            // multiply each q-value by the proportion of true nulls expected to be under it (the inverse of how you get there from the p-value)
//            |> Seq.map (fun r -> r.Value * r.RankIndex / m0)    
//            |> Array.ofSeq
//    
//
//
//        let qValueOfPValue (pi0:float) (pvalues:float[]) =        
//            let m0 = float pvalues.Length * pi0
//            pvalues
//            |> Statistics.Descriptive.Rank.breakByMean
//            |> Seq.sortBy (fun r -> r.Position)    
//            |> Seq.map (fun r -> r.Value / r.RankIndex * m0)    
//            |> Array.ofSeq
//    
//
//
//
//
//
//
//        let inline checkNonNull argName arg = 
//            match box arg with 
//            | null -> nullArg argName 
//            | _ -> ()
//
//
//        let pairwiseWithLast (source: seq<'T>) =
//            checkNonNull "source" source
//            seq { use ie = source.GetEnumerator() 
//                  if ie.MoveNext() then
//                        let iref = ref ie.Current
//                        while ie.MoveNext() do
//                            let j = ie.Current 
//                            yield (!iref, j)
//                            iref := j 
//                        yield (!iref,!iref)}
//
//
//        let qValueOfPValue' (pi0:float) (pvalues:float[]) =        
//            let m0 = float pvalues.Length * pi0
//            pvalues
//            |> Statistics.Descriptive.Rank.breakByMean
//            |> Seq.map (fun r -> let v =  r.Value / r.RankIndex * m0
//                                 {r with Value = v}
//                        )
//            |> pairwiseWithLast
//            |> Seq.map (fun (f,s) -> let v = min (min f.Value s.Value) 1.
//                                     {f with Value = v}
//                        )
//            |> Seq.sortBy (fun r -> r.Position)
//            |> Seq.map (fun r -> r.Value)
//            |> Array.ofSeq
//
//
//
//    
//        //// Example
//        //let pvalues = Seq.fromFile "D:\\output.txt" |> Seq.map float |> Seq.toArray
//        //
//        //let pi0 = pi0_Bootstrap [|0.0 ..0.05..0.9|] pvalues
//        //
//        //let qvalues  = qValueOfPValue pi0 pvalues
//        //let qvalues' = qValueOfPValue' pi0 pvalues
//        //
//        //
//        //let bw = Statistics.Fitting.Bandwidth.nrd0 pvalues
//        //let histo = Descriptive.Histogram.create (bw/2.) pvalues |> Descriptive.EmpiricalPMF.ofHistogram |> Descriptive.EmpiricalPMF.getZip |> Chart.Point |> Chart.ShowChart
//        //
//        //
//        //
//        //Chart.Point(Seq.zip pvalues qvalues ) |> Chart.ShowChart
//        //   





