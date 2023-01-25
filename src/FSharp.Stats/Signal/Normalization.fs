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
    } with static member Create cf nd = {CorrFactors=cf;NormedData=nd}

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
        let normedData = 
            data
            |> Matrix.mapi (fun r c v ->
                v / sampleWiseCorrectionFactors.[c]
            )
        MorResult.Create sampleWiseCorrectionFactors normedData

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
        let normedData = 
            data
            |> Matrix.mapi (fun r c v ->
                v / sampleWiseCorrectionFactors.[r]
            )
        MorResult.Create sampleWiseCorrectionFactors normedData

    /// As used by Deseq2, see: https://github.com/hbctraining/DGE_workshop/blob/master/lessons/02_DGE_count_normalization.md 
    ///
    /// Columns are genes, rows are samples
    let medianOfRatiosWide (data:Matrix<float>) =
        medianOfRatiosWideBy id data