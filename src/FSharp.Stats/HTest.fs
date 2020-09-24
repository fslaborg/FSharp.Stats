namespace FSharp.Stats.Testing


module HTest =

    open FSharp.Stats
    // H-test / one-way ANOVA of ranks 
    // input : seq{seq<float>} 
    let createHTest (samples : seq<#seq<float>>) = 
        // calculating n for each group 
        let n = Seq.map Seq.length samples |> Seq.map float  
    
        // preparing samples for ranking and calculating samplesize 
        let allValues = 
            samples 
            |> Seq.concat
            |> Seq.toArray
    
        let samplesize = Seq.length allValues |> float
    
        // ranking all values 
        let ranked = FSharp.Stats.Rank.rankAverage allValues
    
        let valuesAndRanks = Array.zip allValues ranked 
    
        // match ranks with each group 
        let rankingOfEachGroup data = 
            data 
            |> Seq.choose (fun x ->   
                Array.tryFind (fun (i,j) -> i = x) valuesAndRanks
                )
            |> Seq.map snd
            |> Seq.sum 
            |> float 
        let ranks = Seq.map rankingOfEachGroup samples 
    
        // counting ties in data 
    
        let ties = 
            valuesAndRanks
            |> Seq.countBy id 
            |> Seq.filter (fun (i,j) -> j > 1 )
            |> Seq.map snd
            |> Seq.map float 
    
        if ties = seq [] then 
            let totalties = 
                ties 
                |> Seq.map (fun x -> x**3. - x )
                |> Seq.sum 
            // correction factor for ties in data
            let correctionFactor = 1. - ((totalties)/((samplesize**3.)-samplesize))           
            let parts data x = 
                Seq.map2 (fun data x -> (data**2. /x)) data x
            let sums = parts ranks n |> Seq.sum 
            let dof =
                let length = 
                    samples 
                    |> Seq.length 
                    |> float 
                length - 1. 
            let testWithoutBindings = (12./(samplesize*(samplesize+1.))) * (sums) - 3.*(samplesize + 1.)
            let statistic = testWithoutBindings / correctionFactor
            FSharp.Stats.Testing.TestStatistics.createChiSquare statistic dof 
        else 
            // test statistic for H-test without ties 
               let parts data x = 
                    Seq.map2 (fun data x -> (data**2. /x)) data x
               let sums = parts ranks n |> Seq.sum 
               let dof =
                   let length = 
                       samples 
                       |> Seq.length 
                       |> float 
                   length - 1. 
               let statistic = (12./(samplesize*(samplesize+1.))) * (sums) - 3.*(samplesize + 1.)
               FSharp.Stats.Testing.TestStatistics.createChiSquare statistic dof 