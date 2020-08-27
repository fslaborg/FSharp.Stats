namespace FSharp.Stats.Testing

open FSharp.Stats

/// This module contains functions to adjust for multiple testing errors in statistical tests.
module MultipleTesting = 


    /// Benjamini-Hochberg Correction (BH)
    /// 'projection' should return a tuple of any identifier and the pValues as float, when applied to 'rawP'
    /// This function applies the Benjamini-Hochberg multiple testing correcture and returns all False Discovery Rates to which the given p-values are still 
    /// significant.
    /// Note: corrected pValues are not sorted in original order!
    let benjaminiHochbergFDRBy (projection:'a -> 'b*float) (rawP:seq<'a>) = 

        let pVals = rawP |> (Array.ofSeq >> Array.map projection) 
        // recursive function calculating cumulative minimum
        let rec cummin (pValues:List<_*float>) (nrPvalues:int) (i:int) (min:float) =      
            match pValues with
            | [] -> []
            | x::rest when System.Double.IsNaN(snd(x)) -> (fst(x),nan)::(cummin rest nrPvalues (i) min)//[(fst(x),nan)] @ (cummin rest nrPvalues (i) min)
            | x::rest  -> let prd = (float(nrPvalues)/float(nrPvalues-i))*snd(x)
                          let prd = if(prd > 1.0) then 1.0 else prd
                          let value = if(prd<=min) then prd else min
                          (fst(x),value) :: (cummin rest nrPvalues (i+1) value)//[(fst(x),value)] @ (cummin rest nrPvalues (i+1) value)
    
        let rawpListMinusNan = pVals |> Seq.filter (fun (_,x) -> not (System.Double.IsNaN x)) |> Seq.toList
        let rawpListNan = pVals |> Seq.filter (fun (_,x) -> (System.Double.IsNaN x)) |> Seq.toList
        let npval = Seq.length (rawpListMinusNan)
        //let npval = Seq.length rawp
        let sortedRawp =
            rawpListMinusNan
            |> List.sortWith(fun (_,x) (_,y) -> if (x) > (y) then -1 else 1)                           
        let adjp = cummin sortedRawp npval 0 System.Double.PositiveInfinity
        adjp @ rawpListNan


    /// Benjamini-Hochberg Correction (BH)
    /// This function applies the Benjamini-Hochberg multiple testing correcture and returns all False Discovery Rates to which the given p-values are still 
    /// significant.
    let benjaminiHochbergFDR (rawPValues:seq<float>) =
        let pValsIndexed =
            rawPValues
            |> Seq.indexed
        benjaminiHochbergFDRBy id pValsIndexed
        |> List.sortBy fst
        |> List.map snd
        |> Seq.ofList


    // John D. Storey
    // http://dldcc-web.brc.bcm.edu/lilab/liguow/CGI/R/library/qvalue/html/qvalue.html
    // http://qvalue.princeton.edu/
    /// Estimate the q-values for a given set of p-values. The q-value of a test measures the proportion of false positives incurred (called the false discovery rate) when that particular test is called significant. 
    module Qvalues = 

        //https://genomicsclass.github.io/book/pages/multiple_testing.html <- differt method to calculate pi0, than shown below
        

        /// Estimates pi0 from given p-Values by Storeys bootstrap .
        /// pi0 is the value to which, one can find certain values of the distribution at random.
        /// If this returns 0. lambda needs to be smaller.
        /// Math Definition for lambda = [0,1) . Contains values from 0 to 1, but not 1.
        let pi0BootstrapWithLambda (lambda:float[]) (pValues:float[])  =

            // checks how many pVals are higher than a certain lambdaVal 'a' and gives those a score of 1 and otherwise one of 0.
            // Then takes avg of those numbers and divides the avg by (1 - lambda 'a').
            let pi0 = Array.init lambda.Length ( fun i -> 
                let tmp =
                    pValues 
                    |> Array.averageBy (fun v -> if (v >= lambda.[i]) then 1. else 0. )
                tmp / (1. - lambda.[i]) 
                )
            let minpi0 = Array.min(pi0)    

            let rnd = System.Random()

            // Takes random values from the pVal [] and creates a new arr of the same length of those random pVals.
            // Then applies system of 'pi0' from above and uses the new created pi0 bootstrap [] to weight previous 
            // iterations of this function (seen in the 'mse' of this scope).
            // Repeated 100 times.
            let rec floop (counter:int) (result:float[]) =    
                if counter > 0 then
                    let pboot    = Array.sampleWithReplacement rnd pValues pValues.Length            
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

            // returns the value of the original pi0 arr, at the position of the smallest value found in the mse arr after 100 floop iterations.
            let tmp = pi0.[(mse |> Array.findIndex(fun v -> v = (Array.min mse)))]
            min (tmp) (1.0)


        /// Estimates pi0 from given p-Values by Storeys bootstrap method using default lambda's
        /// pi0 is the value to which, one can find certain values of the distribution at random.
        /// If this returns 0. default lambda needs to be smaller. One should use 'pi0BootstrapWithLambda' in this case.
        let pi0Bootstrap (pValues: float []) = 
            pi0BootstrapWithLambda [|0.0 ..0.05..0.9|] pValues

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


        /// When used for QValues.ofPValues(Robust) it iterates through the pValues in ascending order and compares their new QValues.
        /// To do this it compares the QValues in the ascending order of the PValues (QValues are sorted by their related PValues so that the first QValue is the one which is related to the previously smalles PValue). 
        /// Should the QValue related to the second PValue (QValue.[1]) be smaller the the QValue related to the smallest PValue (QValue.[0]) the later QValue is 
        /// replaced by the first QValue (QValue.[1] <- (QValue.[0])). Should all following QValues be smaller than that QVal, all will be replaced by it.
        let private bindBy (objArr:float []) (arr:float[]) =
            let arr' = Array.copy arr
            let objArr' = Array.copy objArr
            let index = Array.init arr.Length id
            System.Array.Sort(objArr',index)
            for i=1 to arr'.Length-1 do
                if arr'.[index.[i]] < arr'.[index.[i-1]] then
                    arr'.[index.[i]] <- arr'.[index.[i-1]]
            arr'
               

        /// Calculates q-values from given p-values and returns an array of qValues in the same order.
        /// 'pi0' can be calculated with 'pi0Bootstrap' or 'pi0BootstrapWithLambda'.
        /// See Storey JD (2002) JRSS-B 64: 479-498.
        let ofPValuesRobust (pi0:float) (pValues: float []) =

            let m  = float pValues.Length
            let m0 = m * pi0
            pValues
            // replaces all values with ints ordered by their size. The smallest value will get 1, 
            // while the largest value of the arr will get arr.Length as new rank
            |> Rank.rankFirst
            |> Array.map float
            |> Array.mapi (fun i r -> 
                let qVal = 
                    let pVal = pValues.[i]
                    pVal * m0 / (r * (1. - (1. - pVal)**m))
                min qVal 1.
                )
            |> bindBy pValues


        /// Calculates q-values from given p-values and returns an array of qValues in the same order.
        /// 'pi0' can be calculated with 'pi0Bootstrap' or 'pi0BootstrapWithLambda'.
        let ofPValues (pi0:float) (pValues:float[]) =

            let m0 = float pValues.Length * pi0
            pValues
            // replaces all values with ints ordered by their size. The smallest value will get 1, 
            // while the largest value of the arr will get arr.Length as new rank
            |> Rank.rankFirst
            |> Array.map float
            |> Array.mapi (fun i r -> 
                let qVal = 
                    let pVal = pValues.[i]
                    pVal / r * m0 
                min qVal 1.
                )            
            |> bindBy pValues


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
