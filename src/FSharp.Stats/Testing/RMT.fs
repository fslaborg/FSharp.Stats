namespace FSharp.Stats.Testing


module RMT =

    open FSharp.Stats

    let spectralUnfolding egvalues =  
        let x = [0. .. 0.01 .. 1.] 
        let y = 
            egvalues
            |> Quantile.computePercentiles (Quantile.OfSorted.compute) x
        
        Interpolation.Approximation.approx x y egvalues (Seq.min)
        
    let private computeBandwidth (arr:float[]) quantile =
        let sortArr = Array.sort arr
        let computedQuantile = (int ((float arr.Length) * quantile))
        Array.init computedQuantile (fun i -> sortArr.[(arr.Length-computedQuantile)/2 + i])
        |> Distributions.Bandwidth.nrd0
