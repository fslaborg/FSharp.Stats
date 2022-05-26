namespace FSharp.Stats.Testing

module WilcoxonTest = 
    open FSharp.Stats
    open FSharp.Stats.Testing
    
    // Applies wilcoxon test on pairwise differences. Using continuity correction leads to improved accuracy in calculations involving normal distribution approximations like in this test.
    let createWilcoxonTestFromDifferences difference correction = 
        // partition into positive and negative subsets
        let greaterthenzero x =  x > 0. 
        let pos = Seq.filter greaterthenzero difference 
        let lessthanzero x = x < 0. 
        let neg = Seq.filter lessthanzero difference 

        //calculating absolute values for ranking
        // ranking with average for ties 
        let absolutevalueneg x = x * -1.
        let abs = Seq.append pos (Seq.map absolutevalueneg neg) |> Seq.toArray
        let ranks = Rank.RankAverage() abs

        // separating positive and negative values
        let lengthpos = Seq.length pos
        let lengthneg = Seq.length neg
        let subpos = Array.sub ranks 0 lengthpos
        let sumpos = Array.sum subpos 
        let subneg = Array.sub ranks lengthpos lengthneg
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
        // lists rankvalues that aren't unique with its number of occurence
        let ties = 
            ranks 
            |> Array.countBy id 
            |> Array.filter (fun (i,j)-> j>1)
            |> Array.map (fun (i,j) -> float i,float j)

        let tieCorrection (i,j) = 
            if j = 2.0 then 
                (j**3. - j) / 48.
            else i * ((j**3. - j) / 48.)
                
        let tieStatistic =
            ties 
            |> Array.sumBy tieCorrection

        let numerator = wmin-(n*(n+1.)/4.)
        let denuminator = (n*(n+1.)*(2.*n+1.))/24.

        let statisticwithties = 
            let statistic = 
                if Seq.isEmpty ties then
                    numerator / (sqrt denuminator)
                else numerator / (sqrt (denuminator - tieStatistic))
            statistic
        let mu = (n*(n+1.))/4.
        let wminless = ((wmin+0.5)-(n*(n+1.)/4.)) / (sqrt (denuminator - tieStatistic))
        let wmingreater = ((wmin-0.5)-(n*(n+1.)/4.)) / (sqrt (denuminator - tieStatistic))
        let stats = 
            if correction then 
                if wmin < mu then 
                    wminless 
                else wmingreater
            else statisticwithties 
        let absstat stats = 
            if stats < 0. then 
                stats * (- 1.)
            else stats
        let finalstats = absstat stats 
        Testing.TestStatistics.createWilcoxon finalstats 


    // Creates a Wilcoxon signed-rank test. Using continuity correction leads to improved accuracy in calculations involving normal distribution approximations like in this test.
    let createWilcoxonTest data1 data2 correction = 
        let difference = Seq.map2 (-) data1 data2 
        createWilcoxonTestFromDifferences difference correction
