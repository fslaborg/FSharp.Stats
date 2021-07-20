namespace FSharp.Stats
/// Contains correlation functions for different data types 
module Correlation =

    /// This module contains normalization and helper functions for the biweighted midcorrelation
    module private bicorHelpers = 

        let sumBy2 f (a1 : float []) (a2 : float []) = 
            let mutable sum = 0.
            for i = 0 to a1.Length - 1 do
                sum <- sum + (f a1.[i] a2.[i])
            sum

        let sumBy4 f (a1 : float []) (a2 : float []) (a3 : float []) (a4 : float [])= 
            let mutable sum = 0.
            for i = 0 to a1.Length - 1 do
                sum <- sum + (f a1.[i] a2.[i] a3.[i] a4.[i])
            sum    

        let identity (x: float) = 
            if x > 0. then 1. else 0.

        let deviation med mad value = 
            (value - med) / (9. * mad)

        let weight dev = ((1. - (dev ** 2.)) ** 2.) * (identity (1. - (abs dev)))

        let getNormalizationFactor (values: float []) (weights:float[]) (med:float) = 
            sumBy2 (fun value weight -> 
                ((value - med) * weight) ** 2.
                )
                values
                weights
            |> sqrt

        let normalize (value:float) (weight:float) normalizationFactor med =   
            normalizationFactor
            |> (/) ((value - med) * weight)

    /// Contains correlation functions optimized for sequences
    [<AutoOpen>]
    module Seq = 
        /// Calculates the pearson correlation of two samples. Homoscedasticity must be assumed.
        let inline pearson (seq1:seq<'T>) (seq2:seq<'T>) : float =
            if (Seq.length seq1) <> (Seq.length seq2) then failwith "Inputs need to have the same length." 
            let seq1' = seq1 |> Seq.map float
            let seq2' = seq2 |> Seq.map float
            use e = seq1'.GetEnumerator()
            use e2 = seq2'.GetEnumerator()
            let zero = float (LanguagePrimitives.GenericZero<'T>)
            let one = float (LanguagePrimitives.GenericOne<'T>)
            let rec loop n sumX sumY sumXY sumXX sumYY = 
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

        /// weighted pearson correlation (http://sci.tech-archive.net/Archive/sci.stat.math/2006-02/msg00171.html)
        let inline pearsonWeighted (seq1:seq<'T>) (seq2:seq<'T>) (weights:seq<'T>) : float =
            // TODO: solve in a prettier coding fashion
            if Seq.length seq1 <> Seq.length seq2 || Seq.length seq2 <> Seq.length weights then failwithf "input arguments are not the same length"
            let zero = LanguagePrimitives.GenericZero< 'T > 
            let one = LanguagePrimitives.GenericOne<'T> 
            let weightedMean xVal wVal = 
                let a = Seq.fold2 (fun acc xi wi -> acc + (xi * wi)) zero xVal wVal |> float
                let b = Seq.sum wVal|> float
                a / b
            let weightedCoVariance xVal yVal wVal = 
                let weightedMeanXW = weightedMean xVal wVal
                let weightedMeanYW = weightedMean yVal wVal
                let a = 
                    Seq.map3 (fun xi yi wi -> 
                        (float wi) * ((float xi) - weightedMeanXW) * ((float yi) - weightedMeanYW)
                            ) xVal yVal wVal
                    |> Seq.sum
                let b = 
                    Seq.sum wVal 
                    |> float
                a / b
            let weightedCorrelation xVal yVal wVal =
                let a = weightedCoVariance xVal yVal wVal
                let b = 
                    (weightedCoVariance xVal xVal wVal) * (weightedCoVariance yVal yVal wVal)
                    |> sqrt
                a / b          
            weightedCorrelation seq1 seq2 weights
    
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

        /// Biweighted Midcorrelation. This is a median based correlation measure which is more robust against outliers.
        let bicor seq1 seq2 = 
            
            let xs,ys  = seq1 |> Array.ofSeq, seq2 |> Array.ofSeq

            let xMed = xs |> Array.median
            let xMad = xs |> Array.medianAbsoluteDev
            let xWeights = xs |> Array.map ((bicorHelpers.deviation xMed xMad) >> bicorHelpers.weight)
            let xNF = bicorHelpers.getNormalizationFactor xs xWeights xMed

            let yMed = ys |> Array.median
            let yMad = ys |> Array.medianAbsoluteDev
            let yWeights = ys |> Array.map ((bicorHelpers.deviation yMed yMad) >> bicorHelpers.weight)
            let yNF = bicorHelpers.getNormalizationFactor ys yWeights yMed

            bicorHelpers.sumBy4 (fun xVal xWeight yVal yWeight ->
                (bicorHelpers.normalize xVal xWeight xNF xMed) * (bicorHelpers.normalize yVal yWeight yNF yMed)
                )
                xs xWeights ys yWeights 

    /// Contains correlation functions optimized for vectors
    [<AutoOpen>]
    module Vector =

        /// computes the sample correlation of two signal at a given lag.
        /// was tested in comparison to: https://www.wessa.net/rwasp_autocorrelation.wasp
        let correlationOf (corrF: vector -> vector -> float) lag (v1:vector) (v2:vector) = 
            if v1.Length <> v2.Length then failwithf "Vectors need to have the same length."
            if lag >= v1.Length then failwithf "lag must be smaller than input length"
            let v1' = v1.[0..(v1.Length-1 - lag)]
            let v2' = v2.[lag..] 
            corrF v1' v2'

        /// computes the sample auto correlation (using pearson correlation) of a signal at a given lag.
        let autoCorrelation lag v1 = 
            correlationOf pearson lag v1 v1

        /// computes the sample auto corvariance of a signal at a given lag.
        let autoCovariance lag seq = 
            correlationOf Vector.cov lag seq seq

        /// computes the normalized (using pearson correlation) cross-correlation of signals v1 and v2 at a given lag.
        let normalizedXCorr lag v1 v2 = 
            correlationOf pearson lag v1 v2

        /// computes the unnormalized (using only the dot product) cross-correlation of signals v1 and v2 at a given lag.
        let xCorr lag v1 v2 = 
            correlationOf Vector.dot lag v1 v2

        /// Biweighted Midcorrelation. This is a median based correlation measure which is more robust against outliers.
        let bicor (vec1:vector) (vec2:vector) = 
            
            let xs,ys  = vec1.Values, vec2.Values

            let xMed = xs |> Array.median
            let xMad = xs |> Array.medianAbsoluteDev
            let xWeights = xs |> Array.map ((bicorHelpers.deviation xMed xMad) >> bicorHelpers.weight)
            let xNF = bicorHelpers.getNormalizationFactor xs xWeights xMed

            let yMed = ys |> Array.median
            let yMad = ys |> Array.medianAbsoluteDev
            let yWeights = ys |> Array.map ((bicorHelpers.deviation yMed yMad) >> bicorHelpers.weight)
            let yNF = bicorHelpers.getNormalizationFactor ys yWeights yMed

            bicorHelpers.sumBy4 (fun xVal xWeight yVal yWeight ->
                (bicorHelpers.normalize xVal xWeight xNF xMed) * (bicorHelpers.normalize yVal yWeight yNF yMed)
                )
                xs xWeights ys yWeights 

    /// Contains correlation functions optimized for matrices
    [<AutoOpen>]
    module Matrix =
    
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
        
        ///computes a matrix that contains the metric given by the corrFunction parameter applied rowwise for every row against every other row of the input matrix
        let rowWiseCorrelationMatrix (corrFunction : seq<float> -> seq<float> -> float) (m : matrix) =
            let vectors = Matrix.toJaggedArray m
            let result : float [] [] = [|for i=0 to vectors.Length-1 do yield (Array.init vectors.Length (fun innerIndex -> if i=innerIndex then 1. else 0.))|]
            for i=0 to vectors.Length-1 do
                for j=i+1 to vectors.Length-1 do
                    let corr = corrFunction vectors.[i] vectors.[j]
                    result.[i].[j] <- corr
                    result.[j].[i] <- corr
            result |> matrix

        ///computes a matrix that contains the metric given by the corrFunction parameter applied columnwise for every column against every other column of the input matrix
        let columnWiseCorrelationMatrix (corrFunction : seq<float> -> seq<float> -> float) (m : Matrix<float>) =
            m
            |> Matrix.transpose
            |> (rowWiseCorrelationMatrix corrFunction)

        ///computes the rowwise pearson correlation matrix for the input matrix
        let rowWisePearson (m:Matrix<float>) =
            m
            |> rowWiseCorrelationMatrix Seq.pearson

       ///computes the columnwise pearson correlation matrix for the input matrix
        let columnWisePearson (m:Matrix<float>) =
            m
            |> columnWiseCorrelationMatrix Seq.pearson

        /// Computes the rowwise biweighted midcorrelation matrix for the input matrix 
        let rowWiseBicor (m : matrix) =

            let vectors = Matrix.toJaggedArray m
            let result : float [] [] = [|for i=0 to vectors.Length-1 do yield (Array.init vectors.Length (fun innerIndex -> if i=innerIndex then 1. else 0.))|]

            let meds : float [] = Array.zeroCreate vectors.Length
            let mads : float [] = Array.zeroCreate vectors.Length
            let weightss : float [][] = Array.zeroCreate vectors.Length
            let nfs : float [] = Array.zeroCreate vectors.Length

            for i=0 to vectors.Length-1 do
                let xs = vectors.[i]
                let xMed = xs |> Array.median
                let xMad = xs |> Array.medianAbsoluteDev
                let xWeights = xs |> Array.map ((bicorHelpers.deviation xMed xMad) >> bicorHelpers.weight)
                let xNF = bicorHelpers.getNormalizationFactor xs xWeights xMed

                meds.[i] <- xMed
                mads.[i] <- xMad
                weightss.[i] <- xWeights
                nfs.[i] <- xNF

                for j=0 to i - 1 do

                    let corr = 
                        bicorHelpers.sumBy4 (fun xVal xWeight yVal yWeight ->
                            (bicorHelpers.normalize xVal xWeight xNF xMed) * (bicorHelpers.normalize yVal yWeight nfs.[j] meds.[j]))
                            xs xWeights vectors.[j] weightss.[j]
                    result.[i].[j] <- corr
                    result.[j].[i] <- corr
            result |> matrix

        /// Computes the columnwise biweighted midcorrelation matrix for the input matrix 
        let columnWiseBicor (m : matrix) =
            m
            |> Matrix.transpose
            |> rowWiseBicor

        ///// Computes rowise pearson correlation
        //// TODO: TEST
        //let corr (x: matrix) =
        //    let exp = x |> Matrix.map (fun i -> i*i) |> Matrix.sumRows
        //    let z = exp * exp.Transpose |> Matrix.map sqrt
        //    let cov = x * x.Transpose
            
        //    cov ./ z

