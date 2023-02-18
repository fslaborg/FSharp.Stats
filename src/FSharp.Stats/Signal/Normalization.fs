namespace FSharp.Stats.Signal

open FSharp.Stats

module Normalization =

    /// z normalization using the population standard deviation
    //Bortz J., Schuster C., Statistik für Human- und Sozialwissenschaftler, 7 (2010), p. 35
    let zScoreTransformPopulation (yVal:Vector<float>) =
        let yMean = Seq.mean yVal 
        let std   = Seq.stDevPopulation yVal
        yVal |> Vector.map (fun x -> (x - yMean) / std) 

    /// z normalization using the sample standard deviation
    //Bortz J., Schuster C., Statistik für Human- und Sozialwissenschaftler, 7 (2010), p. 35
    let zScoreTransform (yVal:Vector<float>) =
        let yMean = Seq.mean yVal
        let std   = Seq.stDev yVal
        yVal |> Vector.map (fun x -> (x - yMean) / std) 

    /// Summary of the median of ratios (mor) normalization with normed data and determined correctionfactors.
    type MorResult = {
        CorrFactors : seq<float>
        NormedData : Matrix<float>
        NormFunction : matrix -> matrix
    } with static member Create cf nd f = {CorrFactors=cf;NormedData=nd;NormFunction=f}

    /// As used by Deseq2, see: https://github.com/hbctraining/DGE_workshop/blob/master/lessons/02_DGE_count_normalization.md 
    ///
    /// Rows are genes, columns are samples
    ///
    /// The additional function is applied on all values of the matrix when calculating the normalization factors. By this, a zero in the original dataset will still remain zero.
    let medianOfRatiosBy (f: float -> float) (data:Matrix<float>) =
        let sampleWiseCorrectionFactors =            
            data
            |> Matrix.mapiRows (fun _ v ->
                let v = RowVector.map f v
                let geometricMean = Seq.meanGeometric v           
                RowVector.map (fun s -> s / geometricMean) v
                ) 
            |> Matrix.ofRows
            |> Matrix.mapiCols (fun _ v -> Vector.median v)
        let normData m = 
            m
            |> Matrix.mapi (fun r c v ->
                v / sampleWiseCorrectionFactors.[c]
            )
        MorResult.Create sampleWiseCorrectionFactors (normData data) normData

    /// As used by Deseq2, see: https://github.com/hbctraining/DGE_workshop/blob/master/lessons/02_DGE_count_normalization.md 
    ///
    /// Rows are genes, columns are samples
    let medianOfRatios (data:Matrix<float>) =
        medianOfRatiosBy id data

    /// As used by Deseq2, see: https://github.com/hbctraining/DGE_workshop/blob/master/lessons/02_DGE_count_normalization.md 
    ///
    /// Columns are genes, rows are samples
    ///
    /// The additional function is applied on all values of the matrix when calculating the normalization factors. By this, a zero in the original dataset will still remain zero.
    let medianOfRatiosWideBy (f: float -> float) (data:Matrix<float>) =
        let sampleWiseCorrectionFactors =
            data
            |> Matrix.mapiCols (fun _ v -> 
                let v = Vector.map f v
                let geometricMean = Seq.meanGeometric v           
                Vector.map (fun s -> s / geometricMean) v
                ) 
            |> Matrix.ofCols
            |> Matrix.mapiRows (fun _ v -> Seq.median v)
        let normData m = 
            m
            |> Matrix.mapi (fun r c v ->
                v / sampleWiseCorrectionFactors.[c]
            )
        MorResult.Create sampleWiseCorrectionFactors (normData data) normData

    /// As used by Deseq2, see: https://github.com/hbctraining/DGE_workshop/blob/master/lessons/02_DGE_count_normalization.md 
    ///
    /// Columns are genes, rows are samples
    let medianOfRatiosWide (data:Matrix<float>) =
        medianOfRatiosWideBy id data

    /// Quantile normalization with equal number of elements for each sample.
    ///
    /// Rows are genes, columns are samples
    let quantile (data:Matrix<float>)  = 
        data
        |> Matrix.mapCols (Seq.indexed >> Seq.sortBy snd)
        |> Matrix.Generic.ofColSeq
        |> Matrix.Generic.mapRows (fun row -> 
            let avg = Seq.meanBy snd row
            row |> RowVector.Generic.map (fun (i,_) -> i,avg)
            )
        |> Matrix.Generic.ofSeq
        |> Matrix.Generic.mapCols (Seq.sortBy fst >> Seq.map snd >> vector)
        |> Matrix.ofCols
