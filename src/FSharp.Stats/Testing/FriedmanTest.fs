namespace FSharp.Stats.Testing


module FriedmanTest =

    open FSharp.Stats
    let createFriedmanTest (samples : seq<#seq<float>>) =
        
        // efficient rank calculations are based on arrays
        let samples = samples |> Seq.map Seq.toArray

        let samplesize = Seq.length samples |> float 
        let groupsize = 
            Seq.map Seq.length samples 
            |> Seq.head
            |> float
    
        let dof = groupsize - 1. 
    
        // rank all groups individually
        let ranksAll = 
            samples
            |> Seq.map Rank.rankAverage

        // get every first, second, third,...., value for ranking 
        let groups = 
            ranksAll
            |> Array.ofSeq
            |> JaggedArray.transpose
    
        // calculate the sum of each treatment
        let sums = 
            groups
            |> Seq.map Seq.sum
    
        // calculating statistics without ties
        let statistics = 
            let firstpart = 
                12./((samplesize*groupsize)*(groupsize+1.))
            let values = 
                sums
                |> Seq.map (fun x -> x*x)
                |> Seq.sum
            let lastpart = 
                3. * (samplesize* (groupsize+1.))
            let finalformula = 
                (firstpart*values)-lastpart
            finalformula
     
        let ties = 
            ranksAll 
            |> Seq.map (
                Array.countBy id
                >> Array.filter (fun (i,j) -> j > 1 ) 
                >> Array.map snd
                >> Array.map float
                )
            |> Seq.filter (fun x -> x <> [||])
            |> Seq.concat
    
        if ties = seq [] then
            Testing.TestStatistics.createChiSquare statistics dof
        else 
            let totalties = 
                ties
                |> Seq.countBy id
            let equalvalues = Seq.map fst totalties 
            let amountofties = Seq.map snd totalties |> Seq.map float 
            let functionforTies x y  = y * (x**3.-x)
            let newtieslist = 
                Seq.map2 functionforTies equalvalues amountofties
                |> Seq.sum  
            let corrected =  
                (statistics / (1. - (1./(samplesize*groupsize*(groupsize**2.-1.))*newtieslist)))
            Testing.TestStatistics.createChiSquare corrected dof 