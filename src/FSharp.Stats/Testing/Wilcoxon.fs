namespace FSharp.Stats.Testing

module WilcoxonTest = 
    open FSharp.Stats
    open FSharp.Stats.Testing
    let private createWilcoxon difference correction = 
        let createTest = 
            // sort to positive, negative and zero (maybe throw zero out)
            let greaterthenzero x =  x > 0. 
            let pos = Seq.filter (greaterthenzero) difference 
            let lesserthanzero x = x < 0. 
            let neg = Seq.filter (lesserthanzero) difference 
            let iszero x = x = 0. 
            let zero = Seq.filter (iszero) difference

            //calculating absolute values for ranking
            // ranking with average for ties 
            let absolutevalueneg x = 
                (fun x -> x* (-1.)) x
            let abs = Seq.append pos (Seq.map absolutevalueneg neg) |> Seq.toArray
            let ranks = Rank.rankAverage abs

            // separating positive and negative values
            let lengthpos = Seq.length pos
            let lengthneg = Seq.length neg

            let subpos = 
                Array.sub ranks 0 lengthpos
            let sumpos = Array.sum subpos 

            let subneg = 
                Array.sub ranks lengthpos lengthneg
            let sumneg = Array.sum subneg

            // calculating total ranksum
            let n = Array.length ranks |> float 

            // calculating teststatistic
            let wmin = 
                if sumpos < sumneg then
                    sumpos 
                else 
                    sumneg

            //calculating ties
            // rank that is multiple 
            let tieValue = 
                ranks 
                |> Array.countBy id 
                |> Array.filter (fun (i,j)-> j>1)
                |> Array.map fst
                |> Array.map float 
            // how many times it's tied (2 equal, 3 equal, etc)
            let tieAmount = 
                ranks 
                |> Array.countBy id 
                |> Array.filter (fun (i,j)-> j>1)
                |> Array.map snd
                |> Array.map float

            let ties = Array.zip tieValue tieAmount 

            let tieCorrectionFormula (i,j) = (
                if (j = 2.0) then ((j**3. - j) / 48.)
                    else (i*((j**3. - j) / 48.))
                )
            let tieStatistic =
                ties 
                |> Array.map tieCorrectionFormula
                |> Array.sum

            let firstPart = (wmin-(n*(n+1.)/4.))

            let secondPart = ((n*(n+1.)*(2.*n+1.))/24.)


            let statisticwithties = 

                let statistic = 
                    if Seq.isEmpty ties then (firstPart / (sqrt (secondPart)))
                        else ((firstPart )/ (sqrt (secondPart - tieStatistic)))
                statistic
            let mu = ((n*(n+1.))/4.)
            let sigma = sqrt(((n*(n+1.))*((2.*n)+1.))/24.)
            let wminless = ((wmin+0.5)-(n*(n+1.)/4.)) / (sqrt (secondPart - tieStatistic))
            let wmingreater = ((wmin-0.5)-(n*(n+1.)/4.)) / (sqrt (secondPart - tieStatistic))
            let stats = 
                if correction = true then 
                    (if wmin < mu then wminless else wmingreater)
                    else 
                    statisticwithties 
            let absstat stats = 
                if stats < 0. then (stats * (- 1.))
                    else stats
            let finalstats = 
                absstat stats 
            Testing.TestStatistics.createWilcoxon finalstats correction
        createTest


    // Creates a Wilcoxon signed-rank test. Using continuity correction leads to improved accuracy in calculations involving normal distribution approximations like in this test.
    let createWilcoxonTest data1 data2 correction = 
        let difference = Seq.map2 (-) data1 data2 
        createWilcoxon difference correction
    // Applies wilcoxon test on pairwise differences. Using continuity correction leads to improved accuracy in calculations involving normal distribution approximations like in this test.
    let createWilcoxonTestFromDifferences differences correction =
        createWilcoxon differences correction 

