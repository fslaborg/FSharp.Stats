namespace FSharp.Stats.Testing

module Correlation =
    open System
    open FSharp.Stats
    
    /// A correlation significance is obtained by usage of students t-distribution
    // Medical Statistics; Machin, Campbell, Walters; Vol 4; 2007; 9.2 p 155
    let inline testPearson (seq1:seq<'T>) (seq2:seq<'T>) =
        let n = Seq.length seq1
        // pearson correlation coefficient
        let pearson = Correlation.Seq.pearson seq1 seq2
        
        let computeUsingTDist n r =
            let df = n - 2 |> float
            let stat = r * sqrt(df/(1. - pown r 2))
            TestStatistics.createTTest stat df  

        computeUsingTDist n pearson

    /// Calculates a two tailed permutation test p value for correlation coefficients
    let inline private testCorrelationByPermutation (seq1:'a[]) (seq2:'a[]) permutations (correlationMeasure: 'a[] -> 'a[] -> float) =
        
        let n = Seq.length seq1
        
        // pearson correlation coefficient
        let pearson = correlationMeasure seq1 seq2
        
        let shuffledR() =
            let shuffled = seq2 |> Array.shuffleFisherYates
            correlationMeasure seq1 shuffled
        
        Array.init permutations (fun _ -> shuffledR())
        |> Array.filter (fun x -> Math.Abs(x) > Math.Abs(pearson))
        |> Array.length
        |> fun countOfMoreExtreme -> float countOfMoreExtreme / float permutations

    /// Calculates a two tailed permutation test p value for spearman correlation coefficient
    let inline testSpearmanByPermutation (seq1:'a[]) (seq2:'a[]) permutations =
        testCorrelationByPermutation seq1 seq2 permutations Correlation.Seq.spearman
        
    /// Calculates a two tailed permutation test p value for kendall correlation coefficient
    let inline testKendallByPermutation (seq1:'a[]) (seq2:'a[]) permutations =
        testCorrelationByPermutation seq1 seq2 permutations Correlation.Seq.kendall
    
    /// Calculates a two tailed permutation test p value for pearson correlation coefficient
    let inline testPearsonByPermutation (seq1:'a[]) (seq2:'a[]) permutations =
        testCorrelationByPermutation seq1 seq2 permutations Correlation.Seq.pearson

