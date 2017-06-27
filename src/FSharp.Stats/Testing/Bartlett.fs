namespace FSharp.Stats.Testing

module Bartlett =

    open FSharp.Stats

    /// Bartlett's test for equality of variances
    /// Tests the null hypothesis that all group variances are equal
    let compute (samples : seq<#seq<float>>) =
        let sizes        = samples |> Seq.map Seq.length        
        let k            = Seq.length samples |> float
        let popVariances = samples |> Seq.map Seq.varPopulation
        let Sp           =  Seq.UtilityFunctions.pooledVarPopulationOf sizes popVariances
        let numeratorSum,N = 
            Seq.zip popVariances sizes
            |> Seq.fold (fun (varAcc,nAcc) (variance,size) ->
                let n      = float (size)
                let logVar = log (variance * (n - 1.))
                (varAcc + logVar,nAcc + n)) (0., 0.) 

        let denominatorSum = sizes |> Seq.sumBy (fun n -> 1. / float (n - 1))
        
        let num = (N - k) * log(Sp) - numeratorSum
        let den = 1. + (1. / (3.0 * (k - 1.))) * (denominatorSum - 1.0 / (N - k))

        let W = num / den
        let df = k - 1.
        
        TestStatistics.createChiSquare W (float df)
