namespace FSharp.Stats.Testing

open FSharp.Stats

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

    
    // Estimate the q-values for a given set of p-values. The q-value of a test measures the proportion of false positives incurred (called the false discovery rate) when that particular test is called significant. 
    // John D. Storey
    // http://dldcc-web.brc.bcm.edu/lilab/liguow/CGI/R/library/qvalue/html/qvalue.html
    // http://qvalue.princeton.edu/
    module Qvalues = 

        /// Estimates pi0 from given p-Values by Storeys bootstrap method
        let pi0_BootstrapWithLambda (lambda:float[]) (p:float[])  =
            let pi0 = Array.init lambda.Length ( fun i -> 
                let tmp =
                    p 
                    |> Array.averageBy (fun v -> if (v >= lambda.[i]) then 1. else 0. )
                tmp / (1. - lambda.[i]) 
                )
            let minpi0 = Array.min(pi0)    

            let rnd = System.Random()

            let rec floop (counter:int) (result:float[]) =    
                if counter > 0 then
                    let pboot    = Array.sampleWithReplacement rnd p p.Length            
                    let pi0boot = Array.init lambda.Length ( fun i -> 
                        let tmp = 
                            pboot
                            |> Array.averageBy (fun v -> if (v >= lambda.[i]) then 1. else 0. )
                            
                        tmp / (1. - lambda.[i]) 
                        ) 
                    let mse = Array.map2 (fun m p -> m + ((p - minpi0)**2.0) ) result pi0boot
                    floop (counter - 1) mse
                else
                    result

            let mse = floop (100) (Array.zeroCreate (lambda.Length))    
            let tmp = pi0.[(mse |> Array.findIndex(fun v -> v = (Array.min mse)))]
            min (tmp) (1.0)

        /// Estimates pi0 from given p-Values by Storeys bootstrap method using default lambda's
        let pi0_Bootstrap (p:float[]) = pi0_BootstrapWithLambda [|0.0 ..0.05..0.9|] p



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

        //let private bind (arr:float[]) =
        //    let arr' = Array.copy arr
        //    for i=1 to arr'.Length-1 do
        //        if arr'.[i] < arr'.[i-1] then
        //            arr'.[i] <- arr'.[i-1]
        //    arr'

        let private bindBy (objArr:float[]) (arr:float[]) =
            let arr' = Array.copy arr
            let index = Array.init arr.Length id
            System.Array.Sort(objArr,index)
            for i=1 to arr'.Length-1 do
                if arr'.[index.[i]] < arr'.[index.[i-1]] then
                    arr'.[index.[i]] <- arr'.[index.[i-1]]
            arr'
               
    
        /// Calculates the robust version of the q-value. See Storey JD (2002) JRSS-B 64: 479-498.
        let ofPValuesRobust (pi0:float) (pvalues:float[]) =
            let m  = float pvalues.Length
            let m0 = m * pi0
            pvalues
            |> Rank.rankFirst
            |> Array.map float
            |> Array.mapi (fun i r -> 
                let qval = 
                    let p = pvalues.[i]
                    p * m0 / (r * (1. - (1. - p)**m))
                min qval 1.
                )
            |> bindBy pvalues


        /// Calculates q-values from given p-values.
        let ofPValues (pi0:float) (pvalues:float[]) =        
            let m0 = float pvalues.Length * pi0
            pvalues
            |> Rank.rankFirst
            |> Array.map float
            |> Array.mapi (fun i r -> 
                let qval = pvalues.[i] / r * m0 
                min qval 1.
                )            
            |> bindBy pvalues





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





