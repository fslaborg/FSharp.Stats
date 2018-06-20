namespace FSharp.Stats

module Correlation =

    /// Pearson correlation 
    let inline pearson (seq1:seq<'T>) (seq2:seq<'T>) : float =
        use e = seq1.GetEnumerator()
        use e2 = seq2.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'T > 
        let one = LanguagePrimitives.GenericOne<'T> 
        let rec loop n (sumX: 'T) (sumY: 'T) (sumXY: 'T) (sumXX: 'T) (sumYY: 'T) = 
            match (e.MoveNext() && e2.MoveNext()) with
                | true  -> 
                    loop (n + one) (sumX + e.Current) (sumY + e2.Current) (sumXY + (e.Current * e2.Current)) (sumXX + (e.Current * e.Current)) (sumYY + (e2.Current * e2.Current))
                | false -> 
                    if n > zero then  
                        // Covariance
                        let cov = float ((sumXY * n) - (sumX * sumY))
                        // Standard Deviation
                        let stndDev1 = sqrt (float ((n * sumXX) - (sumX * sumX)))
                        let stndDev2 = sqrt (float ((n * sumYY) - (sumY * sumY)))
                        // Correlation
                        let tmp = cov / (stndDev1 * stndDev2)
                        // TODO: solve in a prettier coding fashion
                        if tmp >= 1. then 1. 
                        elif tmp <= -1. then -1.
                        else tmp              
                        
                    else nan
        loop zero zero zero zero zero zero
    
    /// Spearman Correlation (with ranks)
    let spearman array1 array2 =
    
        let spearRank1 = FSharp.Stats.Rank.rankFirst array1 
        let spearRank2 = FSharp.Stats.Rank.rankFirst array2

        pearson spearRank1 spearRank2

    /// Kendall Correlation Coefficient 
    let kendall (setA:_[]) (setB:_[]) =
        let lengthArray = Array.length setA
        let inline kendallCorrFun (setA:_[]) (setB:_[]) =
            let rec loop i j cCon cDisc cTieA cTieB cPairs =      
                if i < lengthArray - 1 then
                    if j <= lengthArray - 1 then
                        if j > i then
                            if (setA.[i] > setA.[j] && setB.[i] > setB.[j]) || (setA.[i] < setA.[j] && setB.[i] < setB.[j]) then
                                loop i (j+1) (cCon + 1.0) cDisc cTieA cTieB (cPairs + 1.0)

                            elif (setA.[i] > setA.[j] && setB.[i] < setB.[j]) || (setA.[i] < setA.[j] && setB.[i] > setB.[j]) then
                                loop i (j+1) cCon (cDisc + 1.0) cTieA cTieB (cPairs + 1.0)

                            else
                                if (setA.[i] = setA.[j]) then
                                    loop i (j+1) cCon cDisc (cTieA + 1.0) cTieB (cPairs + 1.0)

                                else
                                    loop i (j+1) cCon cDisc cTieA (cTieB + 1.0) (cPairs + 1.0)
                        else
                            loop i (j+1) cCon cDisc cTieA cTieB cPairs

                    else 
                        loop (i+1) 1 cCon cDisc cTieA cTieB cPairs

                else
                    let floatLength = lengthArray |> float

                    if (cTieA <> 0.0) || (cTieB <> 0.0) then
                        let n = (floatLength * (floatLength - 1.0)) / 2.0
                        let n1 = (cTieA * (cTieA - 1.0)) / 2.0
                        let n2 = (cTieB * (cTieB - 1.0)) / 2.0
                        (cCon - cDisc) / (sqrt ((n - n1) * (n - n2)))
                
                    else
                        (cCon - cDisc) / ((floatLength * (floatLength - 1.0)) / 2.0)
                
            loop 0 1 0.0 0.0 0.0 0.0 0.0

        kendallCorrFun (FSharp.Stats.Rank.rankFirst setA ) (FSharp.Stats.Rank.rankFirst setB )

    /// computes the sample correlation of two signal at a given lag.
    /// was tested in comparison to: https://www.wessa.net/rwasp_autocorrelation.wasp
    let correlationOf lag (seq1:seq<float>) (seq2:seq<float>) = 
        let seq1C = Seq.length seq1
        let seq2C = Seq.length seq2
        if seq1C <> seq2C then failwithf "Both inputs must have the same length"
        if lag >= seq1C then   failwithf "lag must be smaller than input length"
        let seq1' = seq1 |> Seq.take (seq1C - lag)
        let seq2' = seq2 |> Seq.skip (lag)
        pearson seq1' seq2' 
 
    /// computes the sample auto correlation of a signal at a given lag.
    let acfOf lag seq = 
        correlationOf lag seq seq

    // Implemented according to the R package "MatrixCorrelation"
    /// Computes the rv2 coefficient.  
    let rv2 (x: matrix) (y: matrix) =
        let xxt = x*x.Transpose 
        let yyt = y*y.Transpose 
        xxt |> Matrix.inplace_mapi (fun r c x -> if r = c then 0. else x)
        yyt |> Matrix.inplace_mapi (fun r c x -> if r = c then 0. else x)
        let num = (xxt*yyt).Diagonal |> Vector.sum
        let deno1 = xxt |> Matrix.map (fun x -> x**2.) |> Matrix.sum |> sqrt 
        let deno2 = yyt |> Matrix.map (fun x -> x**2.) |> Matrix.sum |> sqrt 
        num / (deno1 * deno2)
