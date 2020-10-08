namespace FSharp.Stats.Testing


module RMT =
    // implementation from:
    // Luo et al., Constructing gene co-expression networks and predicting functions of unknown genes by random matrix theory. BMC Bioinformatics 8, 299 (2007).
    open FSharp.Stats

    let private spectralUnfolding egvalues =  
        let y = [0. .. 0.01 .. 1.] 
        let x = 
            egvalues
            |> Quantile.computePercentiles (Quantile.OfSorted.compute) y
        
        Interpolation.Approximation.approx x y egvalues (Seq.min)
        
    let private computeBandwidth (arr:float[]) quantile =
        let sortArr = Array.sort arr
        let computedQuantile = (int ((float arr.Length) * quantile))
        Array.init computedQuantile (fun i -> sortArr.[(arr.Length-computedQuantile)/2 + i])
        |> Distributions.Bandwidth.nrd0


    let private evaluateWigner (xValue:float) =
        System.Math.PI*0.5*xValue*System.Math.E**(-System.Math.PI*0.25*xValue*xValue)    

    let private nearestNeighbourSpacing egvLength (uEgv:seq<float>) = 
        uEgv
        |> Seq.map (fun x -> x * float egvLength)
        |> Seq.sort
        |> Seq.windowed 2
        |> Seq.map (fun a -> a.[1] - a.[0])

    let computeChiSquared (bwQuantile : float)  (m:float[,]) =
                
        let unfoldedEgv = 
            FSharp.Stats.Algebra.LinearAlgebra.EigenSpectrumWhenSymmetric (Matrix.ofArray2D m)
            |> fun (eigenvectors,eigenvalues) -> 
                let spUnfold =
                    eigenvalues 
                    |> spectralUnfolding
                    |> Seq.toArray
                spUnfold


        let nnSpacing =
            unfoldedEgv
            |> nearestNeighbourSpacing unfoldedEgv.Length
            |> Seq.toArray
        
        let histo =
            let bw = computeBandwidth nnSpacing bwQuantile
            FSharp.Stats.Distributions.Frequency.create bw nnSpacing
            |> FSharp.Stats.Distributions.Frequency.getZip
            |> Seq.filter (fun (x,count) -> evaluateWigner x > 1. / ((float nnSpacing.Length) * bw)) // WICHTIG
            |> Seq.map (fun (x,y) -> x , (float y) / ((float nnSpacing.Length)*bw))
            |> Seq.toArray

        let chiSquared = 
            let wignerSurmise = Array.map (fst >> evaluateWigner) histo
            Array.map snd histo
            |> FSharp.Stats.Testing.ChiSquareTest.compute (histo.Length-1) wignerSurmise

        chiSquared

        
    /// Computes the critical Threshold for which the NNSD of the matrix significantly abides from the Wigner-Surmise
    /// bwQuantile uses % data to calculate a more robust histogram //0.9 0.01 0.05
    /// to reduce the search space for the threshold you can restrict the range to [leftBorder,rightBorder]
    let computeWithInterval (bwQuantile : float) accuracy (sigCriterion : float) (m:float[,]) (leftBorder,rightBorder)=

        let rec stepSearch previousChi left right =
            let thr = (left + right) / 2.
            let chi =
                m
                |> Array2D.map (fun v -> if abs v > thr then v else 0.)
                |> computeChiSquared bwQuantile

            if right - left > accuracy then 
                if chi.PValueRight <= sigCriterion  then
                    // jump left
                    stepSearch chi left thr
                else
                    stepSearch previousChi thr right
            else
                if chi.PValueRight <= sigCriterion  then
                    thr,chi
                else 
                    right,previousChi
                
        stepSearch (TestStatistics.createChiSquare 0. 1.) leftBorder rightBorder


    ///Computes the critical Threshold for which the NNSD of the matrix significantly abides from the Wigner-Surmise
    /// bwQuantile uses % data to calculate a more robust histogram //0.9 0.01 0.05
    let compute (bwQuantile : float) accuracy (sigCriterion : float) (m:float[,]) =
        computeWithInterval bwQuantile accuracy sigCriterion m (0.,1.)
                
            






        