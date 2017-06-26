namespace FSharp.Stats.Testing

module Anova =
    
    open FSharp.Stats
    open TestStatistics
    
    // #################################################################
    // #################################################################    
    type VariationSource =
        | Total
        | BetweenGroups
        | Regression
        | Residual

    type AnovaVariationSource =
        { DegreesOfFreedom : float;
            MeanSquares      : float;
            Significance     : float;
            Source           : VariationSource;
            Statistic        : float;
            SumOfSquares     : float;
            }

    let createAnovaVariationSource degreesOfFreedom meanSquares significance source statistic sumOfSquares =
        { DegreesOfFreedom = degreesOfFreedom; MeanSquares = meanSquares; Significance = significance; Source = source; Statistic = statistic; SumOfSquares = sumOfSquares;}

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

        { DegreesOfFreedom = Db; MeanSquares = (Sb / Db); Significance = FTest.PValue; Source = VariationSource.BetweenGroups; Statistic = FTest.Statistic; SumOfSquares = Sb; }                 

