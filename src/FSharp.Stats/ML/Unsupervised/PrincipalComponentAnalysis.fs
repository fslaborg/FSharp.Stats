namespace FSharp.Stats.ML.Unsupervised

open FSharp.Stats


/// Principle component analysis 
module PCA =
    //The Implementation was compared to the R function prcomp(). The implementation is based on remarks found in https://stats.stackexchange.com/a/134283
    //Signs of loadings and principal components (scores) can differ from the R implementation due to different svd implementations being used internally.
    //Colab workbook for direct comparison to prcomps output is accessible at: https://colab.research.google.com/drive/1DJ4ky5F5kBM87JprmAbx_gTHqSdz3vqU?usp=sharing

    type PCA = {    
        VarianceOfComponent: vector
        ///Variance explained by principal components
        VarExplainedByComponentIndividual    : vector
        ///Cumulative Variance explained by principal components 
        VarExplainedByComponentCumulative    : vector
        ///Matrix with columns representing individual principal components ("raw" principal components, projections on principal directions) and rows representing samples.
        ///Also reffered to as "scores". Corresponds to the attribute "x" of the result object of Rs prcomp() function.
        PrincipalComponents         : Matrix<float>
        ///Matrix with columns representing component loadings and rows individual features. 
        ///Corresponds to the attribute "rotation" of the result object of Rs prcomp() function.
        Loadings                    : Matrix<float>
        }
    

    /// Normalizes each feature by substracting the corresponing mean followed by a division by its standard deviation.
    /// The centered features of the matrix are centered around 0 and possess a standard deviation of 1.
    /// Expects a data matrix with rows representing observations and columns representing features.
    let center m = 
        if m |> Matrix.exists (fun x -> nan.Equals(x) || infinity.Equals(x) || infNeg.Equals(x)) then 
            failwith "Computation not possible. Matrix contains invalid entries. Check for the existence of values equal to nan, infinity or -infinity."
        else
        let columnMeans =
            m 
            |> Matrix.mapiCols (fun i x -> Seq.mean x)
            |> vector

        let columnStabw =
            m 
            |> Matrix.mapiCols (fun i x -> Seq.stDev x)
            |> vector

        let substractionMatrix = 
            let colV = Vector.init m.NumRows (fun i -> 1.)
            colV*columnMeans.Transpose

        let stabwMatrix = 
            let colV = Vector.init m.NumRows (fun i -> 1.)
            colV*columnStabw.Transpose

        let centeredM = 
            m - substractionMatrix
            |> Matrix.mapi (fun i j x -> x / stabwMatrix.[i,j] )
        centeredM

    /// Computes the PCA of a column centered data matrix m.
    /// Expects a column centered data matrix m, with rows representing observations (a.k.a. samples) and columns representing features.
    let compute m =  
        if m |> Matrix.exists (fun x -> nan.Equals(x) || infinity.Equals(x) || infNeg.Equals(x)) then 
            failwith "Computation not possible. Matrix contains invalid entries. Check for the existence of values equal to nan, infinity or -infinity."
        else
        let s,u,v = FSharp.Stats.Algebra.LinearAlgebra.SVD (m) 
        let n = m.NumRows |> float
        let varOfComp =
            let e = Vector.map (fun x -> x**2.) s  
            e 
            |> Vector.map (fun x -> x / (n-1.))
            |> Vector.map sqrt
        let varExplained =
            let e = Vector.map (fun x -> x**2.) s  
            let sum = Seq.sum e
            Vector.map (fun l -> abs l / sum) e
        let varExplainedCum = 
            varExplained
            |> Vector.foldi (fun i (sum,(v:vector)) x -> 
                                let c = x+sum
                                v.[i] <- c
                                c,v
                            ) (0.,Vector.zeroCreate varExplained.Length)
            |> snd
        let pc = u * (Matrix.diag (Vector.init u.NumCols (fun i -> if i <= s.Length-1 then s.[i] else 0.)));
        let loadings = Matrix.getCols v.Transpose 0 varExplained.Length
        //Update:
        // There seems to be a lot of ambuiguity when it comes to the use of the term loading. Here we use the term loading to 
        // refer to the eigenvectors, also termed "unit scaled loading". see: https://stats.stackexchange.com/questions/143905/loadings-vs-eigenvectors-in-pca-when-to-use-one-or-another
        // proper loadings when https://stats.stackexchange.com/a/141531 is right, however the described scaling does
        // not result in vectors of norm 1 (the columns of principal axes do) and differ from rs prcomp() "rotation" propertie which is 
        // described here: https://stats.stackexchange.com/a/510465 and here https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/prcomp as loadings
        //let principalAxes = Matrix.getCols v.Transpose 0 varExplained.Length
        //let loadings = 
        //    principalAxes*(Matrix.diag (Vector.init principalAxes.NumCols (fun i -> if i <= s.Length-1 then s.[i] else 0.)))
        //    |> Matrix.map (fun x -> x / sqrt(n-1.))
        // Note: Rs biplot function with param scale=0 applies this scaling to loadings: //principalAxes*(Matrix.diag (Vector.init principalAxes.NumCols (fun i -> if i <= s.Length-1 then s.[i] else 0.)))
        {
        VarianceOfComponent=varOfComp
        VarExplainedByComponentIndividual = varExplained
        VarExplainedByComponentCumulative = varExplainedCum
        PrincipalComponents         = pc
        Loadings                    = loadings
        }
