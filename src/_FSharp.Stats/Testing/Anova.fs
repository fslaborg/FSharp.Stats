namespace FSharp.Stats.Testing

module Anova =
    
    open FSharp.Stats
    open TestStatistics
    
    // #################################################################
    // #################################################################    
    type VariationSource =
        | Total
        | BetweenGroups
        | WithinGroups
        | Regression
        | Residual


    type TwoWayAnovaModel = Fixed | Mixed | Random


    type AnovaVariationSource =
        {   DegreesOfFreedom : float;
            MeanSquares      : float;
            Significance     : float;
            Source           : VariationSource;
            Statistic        : float;
            SumOfSquares     : float;
        }


    let createAnovaVariationSource degreesOfFreedom meanSquares significance source statistic sumOfSquares =
        { DegreesOfFreedom = degreesOfFreedom; MeanSquares = meanSquares; Significance = significance; Source = source; Statistic = statistic; SumOfSquares = sumOfSquares;}

    type OneWayAnovaVariationSources =
        {
            // first factor source
            Factor   : AnovaVariationSource
            // Error (within-variance) source
            Error       : AnovaVariationSource
            // Total source of variance
            Total       : AnovaVariationSource
        }


    let createOneWayAnovaVariationSources factor error total =
        { Factor = factor;Error=error;Total=total}



    type TwoWayAnovaVariationSources =
        {
            // first factor source
            FactorFst   : AnovaVariationSource
            // first factor source
            FactorSnd   : AnovaVariationSource
            // Interaction factor (fstxsnd) source
            Interaction : AnovaVariationSource
            // Error (within-variance) source
            Error       : AnovaVariationSource
            // Grouped (cells) variance source
            Cells       : AnovaVariationSource
            // Total source of variance
            Total       : AnovaVariationSource
        }


    let createTwoWayAnovaVariationSources ffst fsnd inter error cells total =
        { FactorFst = ffst;FactorSnd=fsnd;Interaction=inter;Error=error;Cells=cells;Total=total}


    // #################################################################
    // #################################################################
    /// Calculates one-way analysis of variance (one-way ANOVA) which is a technique used to compare means of two or more samples (using the F distribution)
    /// The ANOVA tests the null hypothesis that samples in two or more groups are drawn from populations with the same mean values.
    let oneWayAnova (samples : seq<#seq<float>>) =
        let sizes = samples |> Seq.map Seq.length
        let totalSize = sizes |> Seq.sum
        let groupCount = Seq.length samples
        
        let Db = float(groupCount - 1)
        let Dw = float(totalSize - groupCount)
        let Dt = groupCount * totalSize - 1

        // Step 1. Calculate the mean within each group
        let means = samples |> Seq.map (fun x -> Seq.mean(x))
        // Step 2. Calculate the overall mean
        let totalMean = Seq.mean(means)
        // Step 3. Calculate the "between-group" sum of squares        
        let Sb = Seq.map2 (fun v size -> ((v - totalMean)**2.0) * float(size)) means sizes |> Seq.sum
        // Step 4. Calculate the "within-group" sum of squares
        let Sw = Seq.map2 (fun ar mean -> ar |> Seq.fold (fun acc elem -> acc + ((elem-mean)**2.0)) 0.0) samples means |> Seq.sum
        
        let St = Sb + Sw // total sum of squares
        
        // Step 5. Calculate the F statistic
        let MSb = Sb / Db // between-group mean square
        let MSw = Sw / Dw // within-group mean square

        let FTest = Testing.TestStatistics.createFTest (MSb / MSw) Db Dw            

        let dfT = float (totalSize - 1)
        let sst = 
            samples
            |> Seq.concat
            |> Seq.sumBy (fun v -> 
                            let u = v - totalMean
                            u * u )            

        let factor = createAnovaVariationSource Db (Sb / Db) FTest.PValue VariationSource.BetweenGroups FTest.Statistic Sb
        let error  = createAnovaVariationSource Dw (Sw / Dw) nan VariationSource.WithinGroups nan Sw
        let total  = createAnovaVariationSource dfT (sst / dfT) nan VariationSource.Total nan sst
        
        createOneWayAnovaVariationSources factor error total


    // #################################################################
    // #################################################################
    /// Calculates two-way ANOVA as an extension of the one-way ANOVA for two independent variables.
    let twoWayANOVA (anovaType:TwoWayAnovaModel) (samples : float array array array) =

        /// Fold TODO: refector
        let inline fold f state (arr: 'T [,]) =
            let rowCount,colCount = arr.GetLength(0),arr.GetLength(1) 
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let mutable state' = state
            for k=0 to colCount-1 do       
                for i=0 to rowCount-1 do
                    state' <- f.Invoke(state', arr.[i,k])
            state'

        let fstFactorCount = samples.Length
        let sndFactorCount = samples.[0].Length
        let replCount      = samples.[0].[0].Length
        let totalCount     = fstFactorCount * sndFactorCount * replCount
    
        // Calculate cell means
        let cellMeans = 
            Array2D.init fstFactorCount sndFactorCount (fun i j -> Array.average samples.[i].[j])
        let totalSum = 
            samples |> Array.sumBy (fun arr -> arr |> Array.sumBy (Array.sum)) 
    
        // Calculate the total mean (grand mean)
        let totalMean = 
            let tmp = cellMeans |> fold (+) 0.
            tmp / float (fstFactorCount * sndFactorCount)

        // Calculate factor means
        let fstFactorMean =
            //Array.init sndFactorCount (fun i -> 
            //    let mutable sum = 0.0
            //    for j=0 to samples.[i].Length-1 do
            //        for k=0 to samples.[i].[j].Length-1 do
            //        sum <- sum + samples.[i].[j].[k]
            //    sum / float (sndFactorCount * replCount)
            //    )

            samples
            |> Array.map (fun arr -> 
                let sum = arr |> Array.sumBy Array.sum
                sum / float (sndFactorCount * replCount))

        let sndFactorMean =
            Array.init sndFactorCount (fun j -> 
                let mutable sum = 0.0
                for i=0 to samples.Length-1 do
                    for k=0 to samples.[i].[j].Length-1 do
                    sum <- sum + samples.[i].[j].[k]
                sum / float (fstFactorCount * replCount)
                )

        // Calculate total sum of squares
        let totalSumOfSquares =
            let mutable ssum = 0.0
            for i=0 to samples.Length-1 do
                for j=0 to samples.[i].Length-1 do
                    for k=0 to samples.[i].[j].Length-1 do
                        let u = samples.[i].[j].[k] - totalMean
                        ssum <- ssum + u * u
            ssum


        // Calculate the cell sum of squares
        let cellSumOfSquares =
            let mutable ssum = 0.0
            for i=0 to fstFactorCount-1 do
                for j=0 to sndFactorCount-1 do
                    let u = cellMeans.[i, j] - totalMean
                    ssum <- ssum + u * u
            ssum * float replCount

        // Compute within-cells error sum of squares
        let errorSumOfSquares =
            let mutable ssum = 0.0
            for i=0 to samples.Length-1 do
                for j=0 to samples.[i].Length-1 do
                    for k=0 to samples.[i].[j].Length-1 do
                        let u = samples.[i].[j].[k] - cellMeans.[i, j]
                        ssum <- ssum + u * u
            ssum

        // Compute factors sum of squares
        let fstFactorSumOfSquares =
            let tmp =
                fstFactorMean
                |> Array.fold ( fun ssum mean -> 
                    let u = mean - totalMean
                    ssum + u * u) 0.
            tmp * float (sndFactorCount * replCount)

        let sndFactorSumOfSquares =
            let tmp =
                sndFactorMean
                |> Array.fold ( fun ssum mean -> 
                    let u = mean - totalMean
                    ssum + u * u) 0.
            tmp * float (fstFactorCount * replCount)

        // Compute interaction sum of squares
        let factorSumOfSquares = cellSumOfSquares - fstFactorSumOfSquares - sndFactorSumOfSquares        

        // Compute degrees of freedom
        let cellDf = fstFactorCount * sndFactorCount - 1 |> float
        let fstFactorDf = fstFactorCount - 1 |> float
        let sndFactorDf = sndFactorCount - 1 |> float
        let factorDf = cellDf - fstFactorDf - sndFactorDf |> float
        let errorDf = fstFactorCount * sndFactorCount * (replCount - 1) |> float
        let totalDf = totalCount - 1 |> float


        // Compute mean squares
        let fstFactorMeanSquares = float fstFactorSumOfSquares / fstFactorDf
        let sndFactorMeanSquares = float sndFactorSumOfSquares / sndFactorDf
        let factorMeanSquares = float factorSumOfSquares / factorDf
        let errorMeanSquares = float errorSumOfSquares / errorDf

        // Create the F-Statistics
        let fstFactorSig,sndFactorSig,factorSig =
            match anovaType with
            | Fixed -> 
                // Model 1: Factors A and B fixed
                (
                 (Testing.TestStatistics.createFTest  (fstFactorMeanSquares / factorMeanSquares) fstFactorDf factorDf),
                 (Testing.TestStatistics.createFTest  (sndFactorMeanSquares / factorMeanSquares) sndFactorDf factorDf),
                 (Testing.TestStatistics.createFTest  (factorMeanSquares / errorMeanSquares) factorDf errorDf))
            | Mixed ->  
                // Model 2: Factors A and B random
                (
                 (Testing.TestStatistics.createFTest (fstFactorMeanSquares / errorMeanSquares) fstFactorDf errorDf),
                 (Testing.TestStatistics.createFTest (sndFactorMeanSquares / errorMeanSquares) sndFactorDf errorDf),
                 (Testing.TestStatistics.createFTest (factorMeanSquares / errorMeanSquares) factorDf errorDf)
                )
            | Random -> 
                // Model 3: Factor A fixed, factor B random
                (
                 (Testing.TestStatistics.createFTest(fstFactorMeanSquares / factorMeanSquares) fstFactorDf factorDf),
                 (Testing.TestStatistics.createFTest(sndFactorMeanSquares / errorMeanSquares) sndFactorDf errorDf),
                 (Testing.TestStatistics.createFTest(factorMeanSquares / errorMeanSquares) factorDf errorDf)
                )

        
        let ffst  = createAnovaVariationSource fstFactorDf fstFactorMeanSquares fstFactorSig.PValue VariationSource.Residual fstFactorSig.Statistic fstFactorSumOfSquares
        let fsnd  = createAnovaVariationSource sndFactorDf sndFactorMeanSquares sndFactorSig.PValue VariationSource.Residual sndFactorSig.Statistic sndFactorSumOfSquares
        let inter = createAnovaVariationSource factorDf factorMeanSquares factorSig.PValue VariationSource.BetweenGroups factorSig.Statistic factorSumOfSquares
        let error = createAnovaVariationSource errorDf errorMeanSquares nan VariationSource.WithinGroups nan errorSumOfSquares
        let cells = createAnovaVariationSource cellDf nan nan VariationSource.WithinGroups nan cellSumOfSquares
        let total = createAnovaVariationSource totalDf nan nan VariationSource.WithinGroups nan totalSumOfSquares

        createTwoWayAnovaVariationSources ffst fsnd inter error cells total

