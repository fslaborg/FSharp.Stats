namespace FSharp.Stats.Signal

open FSharp.Stats
open FsMath

module Normalization =

    /// <summary>
    ///   z score normalization/transformation using the population standard deviation.
    /// </summary>
    /// <param name="yData">collection of values to be transformed</param>
    /// <returns>transformed yData in unchanged order</returns>
    /// <example> 
    /// <code> 
    ///   // transform data, such that data has zero mean and population standard deviation of  
    ///   Normalization.zScoreTransformPopulation (vector [|1.1;5.3;-9.0;13.2;17.3;-2.3|])
    /// </code> 
    /// </example>
    /// <remarks>Bortz J., Schuster C., Statistik für Human- und Sozialwissenschaftler, 7 (2010), p. 35</remarks>
    let zScoreTransformPopulation (yData:Vector<float>) : Vector<float> =
        let yMean = Seq.mean yData 
        let std   = Seq.stDevPopulation yData
        yData |> Array.map (fun x -> (x - yMean) / std) 

    /// <summary>
    ///   z score normalization/transformation using the sample standard deviation. Rarely used since variance is not equal to 1.
    /// </summary>
    /// <param name="yData">collection of values to be transformed</param>
    /// <returns>transformed yData in unchanged order</returns>
    /// <example> 
    /// <code> 
    ///   // transform data, such that data has zero mean and sample standard deviation of 1
    ///   Normalization.zScoreTransform (vector [|1.1;5.3;-9.0;13.2;17.3;-2.3|])
    /// </code> 
    /// </example>
    /// <remarks>Bortz J., Schuster C., Statistik für Human- und Sozialwissenschaftler, 7 (2010), p. 35</remarks>
    let zScoreTransform (yData:Vector<float>) : Vector<float> =
        let yMean = Seq.mean yData
        let std   = Seq.stDev yData
        yData |> Array.map (fun x -> (x - yMean) / std) 

    /// Summary of the median of ratios (mor) normalization with normed data, determined correctionfactors, and transformation function.
    type MorResult = {
        CorrFactors : seq<float>
        NormedData : Matrix<float>
        NormFunction : Matrix<float> -> Matrix<float>
    } with static member Create cf nd f = {CorrFactors=cf;NormedData=nd;NormFunction=f}
    
    /// <summary>
    ///   Median of ratios normalization As used by Deseq2, see: https://github.com/hbctraining/DGE_workshop/blob/master/lessons/02_DGE_count_normalization.md . 
    ///   Rows are genes, columns are samples
    /// </summary>
    /// <param name="f">The transformation function is applied on all values of the matrix before calculating the normalization factors.</param>
    /// <param name="data">data matrix with columns as features (samples,time points) and rows as measured entities (genes,proteins).</param>
    /// <returns>Normalized data matrix with correction factors and normalization function.</returns>
    /// <example> 
    /// <code> 
    ///   // raw data with proteins as rows and samples as columns
    ///   let myData = Matrix.init 500 5 (fun _ _ -> rnd.NextDouble())
    ///   let normedData = Normalization.medianOfRatiosBy (fun x -> ln (x+1)) myData
    /// </code> 
    /// </example>
    let medianOfRatiosBy (f: float -> float) (data: Matrix<float>) =
        let sampleWiseCorrectionFactors =            
            data
            |> Matrix.mapiRows (fun _ v ->
                let v = Array.map f v
                let geometricMean = Seq.meanGeometric v           
                Array.map (fun s -> s / geometricMean) v
                ) 
            |> Matrix.getCols 
            |> Array.map (fun (v:Vector<float>) -> Vector.median v)

        let normData m = 
            m
            |> Matrix.mapi (fun r c v ->
                v / sampleWiseCorrectionFactors.[c]
            )
        MorResult.Create sampleWiseCorrectionFactors (normData data) normData

    
    /// <summary>
    ///   Median of ratios normalization As used by Deseq2, see: https://github.com/hbctraining/DGE_workshop/blob/master/lessons/02_DGE_count_normalization.md . 
    ///   Rows are genes, columns are samples
    /// </summary>
    /// <param name="data">data matrix with columns as features (samples,time points) and rows as measured entities (genes,proteins).</param>
    /// <returns>Normalized data matrix with correction factors and normalization function.</returns>
    /// <example> 
    /// <code> 
    ///   // raw data with proteins as rows and samples as columns
    ///   let myData = Matrix.init 500 5 (fun _ _ -> rnd.NextDouble())
    ///   let normedData = Normalization.medianOfRatios myData
    /// </code> 
    /// </example>
    let medianOfRatios (data:Matrix<float>) =
        medianOfRatiosBy id data

    /// <summary>
    ///   Median of ratios normalization As used by Deseq2, see: https://github.com/hbctraining/DGE_workshop/blob/master/lessons/02_DGE_count_normalization.md . 
    ///   Columns are genes, rows are samples
    /// </summary>
    /// <param name="f">The transformation function is applied on all values of the matrix before calculating the normalization factors.</param>
    /// <param name="data">data matrix with columns as measured entities and rows as features (samples,time points) (genes,proteins).</param>
    /// <returns>Normalized data matrix with correction factors and normalization function.</returns>
    /// <example> 
    /// <code> 
    ///   // raw data with proteins as columns and samples as rows
    ///   let myData = Matrix.init 5 500 (fun _ _ -> rnd.NextDouble())
    ///   let normedData = Normalization.medianOfRatiosWideBy (fun x -> ln (x+1)) myData
    /// </code> 
    /// </example>
    let medianOfRatiosWideBy (f: float -> float) (data:Matrix<float>) =
        let sampleWiseCorrectionFactors =
            data
            |> Matrix.mapiCols (fun _ v -> 
                let v = Array.map f v
                let geometricMean = Seq.meanGeometric v           
                Array.map (fun s -> s / geometricMean) v
                ) 
            |> Matrix.getCols
            |> Array.map (fun v -> Vector.median v)
        let normData m = 
            m
            |> Matrix.mapi (fun r c v ->
                v / sampleWiseCorrectionFactors.[r]
            )
        MorResult.Create sampleWiseCorrectionFactors (normData data) normData

    /// <summary>
    ///   Median of ratios normalization As used by Deseq2, see: https://github.com/hbctraining/DGE_workshop/blob/master/lessons/02_DGE_count_normalization.md . 
    ///   Columns are genes, rows are samples
    /// </summary>
    /// <param name="data">data matrix with columns as measured entities and rows as features (samples,time points) (genes,proteins).</param>
    /// <returns>Normalized data matrix with correction factors and normalization function.</returns>
    /// <example> 
    /// <code> 
    ///   // raw data with proteins as columns and samples as rows
    ///   let myData = Matrix.init 5 500 (fun _ _ -> rnd.NextDouble())
    ///   let normedData = Normalization.medianOfRatiosWide myData
    /// </code> 
    /// </example>
    let medianOfRatiosWide (data:Matrix<float>) =
        medianOfRatiosWideBy id data

    /// <summary>
    ///   Quantile normalization with equal number of elements (rows) for each sample (column). 
    ///   Column mean and column standard deviation are qual after normalization.
    ///   Rows are genes, columns are samples.
    /// </summary>
    /// <param name="data">data matrix with columns as measured entities and rows as features (samples,time points) (genes,proteins).</param>
    /// <returns>Normalized data matrix.</returns>
    /// <example> 
    /// <code> 
    ///   // raw data with proteins as rows and samples as columns
    ///   let myData = Matrix.init 500 5 (fun _ _ -> rnd.NextDouble())
    ///   let normedData = Normalization.quantile myData
    /// </code> 
    /// </example>
    let quantile (data:Matrix<float>)  = 
        data
        |> Matrix.getCols
        |> Array.map (fun v -> 
            v |> Seq.indexed |> Seq.sortBy snd |> Array.ofSeq
            )
        |> JaggedArray.transpose
        |> Array.map (fun row -> 
            
            let avg = Seq.meanBy snd row
            row |> Array.map (fun (i,_) -> i,avg)
            )
        |> JaggedArray.transpose
        |> Array.map (Seq.sortBy fst >> Seq.map snd >> Vector.ofSeq)
        |> Matrix.ofCols
        //|> Matrix.Generic.mapRows (fun row -> 
        //    let avg = Seq.meanBy snd row
        //    row |> RowVector.Generic.map (fun (i,_) -> i,avg)
        //    )
        //|> Matrix.Generic.ofSeq
        //|> Matrix.Generic.mapCols (Seq.sortBy fst >> Seq.map snd >> vector)
        //|> Matrix.ofCols
