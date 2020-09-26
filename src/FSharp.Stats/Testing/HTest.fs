namespace FSharp.Stats.Testing


module HTest =

    open FSharp.Stats

    //https://astatsa.com/KruskalWallisTest/

    /// H-test / one-way ANOVA of ranks 
    let createHTest (samples : seq<#seq<float>>) = 
        // calculating n for each group 
        let n = Seq.map (Seq.length >> float) samples  
    
        if Seq.exists (fun x -> x <= 5.) n then failwithf "H test based on chi squared distribution is only valid for sample sizes > 5"

        // preparing samples for ranking and calculating samplesize 
        let allElements = 
            samples 
            |> Seq.concat
            |> Seq.toArray
    
        let samplesize = allElements.Length |> float
    
    
        // associating ranks to each element
        let valuesAndRanks = 
            let ranks = Rank.rankAverage allElements
            Array.zip allElements ranks 
    
        // match ranks with each group 
        let rankingOfEachGroup data = 
            data 
            |> Seq.choose (fun x ->   
                Array.tryFind (fun (i,j) -> i = x) valuesAndRanks
                )
            |> Seq.sumBy snd
            |> float 

        let ranks = Seq.map rankingOfEachGroup samples 
    
    
        // counting ties in data 
        let ties = 
            valuesAndRanks
            |> Seq.countBy id 
            |> Seq.filter (fun (i,j) -> j > 1 )
            |> Seq.map (snd >> float)
    
        let parts data x = 
            Seq.map2 (fun data x -> data**2. /x) data x
        let sums = parts ranks n |> Seq.sum 
        let dof =
            let samplecount = 
                samples 
                |> Seq.length 
                |> float 
            samplecount - 1. 

        let statistic = (12./(samplesize*(samplesize+1.))) * sums - 3.*(samplesize + 1.)

        // test statistic for H-test with ties 
        if Seq.isEmpty ties then 
            FSharp.Stats.Testing.TestStatistics.createChiSquare statistic dof 
        // test statistic for H-test without ties 
        else 
            let totalties = 
                ties 
                |> Seq.map (fun x -> x**3. - x )
                |> Seq.sum 
            // correction factor for ties in data
            let correctionFactor = 1. - (totalties/((samplesize**3.)-samplesize))           
            let statisticWithoutBindings = statistic / correctionFactor
            Testing.TestStatistics.createChiSquare statisticWithoutBindings dof 
