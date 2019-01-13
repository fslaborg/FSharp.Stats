namespace FSharp.Stats.Fitting

open FSharp.Stats

/// Functions for creating and using logistic regression models.
///Implementation taken from Mathias Brandewinder 
///
///https://github.com/mathias-brandewinder/Machine-Learning-In-Action/blob/master/MachineLearningInAction/MachineLearningInAction/LogisticRegression.fs
module LogisticRegression =

    open System
    
    // Weights have 1 element more than observations, for constant
    let private predict (weights: vector) (obs: vector) =
        Vector.init (obs.Length+1) (fun i -> if i = 0 then 1. else obs.[i-1])  
        |> Vector.dot weights 
        |> FSharp.Stats.SpecialFunctions.Logistic.standard

    let private error (weights: vector) (obs: vector) label =
        label - predict weights obs

    let private update alpha (weights: vector) (obs: vector) label =      
        Vector.add weights (Vector.scale (alpha * (error weights obs label)) (Vector.init (obs.Length+1) (fun i -> if i = 0 then 1. else obs.[i-1])))

    // simple training: returns vector of weights
    // after fixed number of passes / iterations over dataset, 
    // with constant alpha
    let private simpleTrain (dataset: (float * vector) seq) passes alpha =
        let rec descent iter curWeights =
            match iter with 
            | 0 -> curWeights
            | _ ->
                dataset
                |> Seq.fold (fun w (label, observ) -> 
                    update alpha w observ label) curWeights
                |> descent (iter - 1)

        let vars = dataset |> Seq.item 1 |> snd |> Vector.length
        let weights = Vector.zero (vars+1) // 1 more weight for constant

        descent passes weights
    
    // 2-Norm of Vector (length)
    let private norm (vector: float list) = 
        vector |> List.sumBy (fun e -> e * e) |> sqrt

    // rate of change in the weights vector,
    // computed as the % change in norm
    let private changeRate (before:vector) (after:vector) =
        let numerator = 
            Vector.sub before after
            |> Vector.norm
        let denominator = Vector.norm before
        numerator / denominator


    module Univariable = 

        /// Calculates the weights for logistic regression.
        let coefficient epsilon alpha (x_data : Vector<float>) (y_data : Vector<float>) =
            if x_data.Length <> y_data.Length then
                raise (System.ArgumentException("vector x and y have to be the same size!"))

            let len = x_data.Length
            let cooling = 0.9
            let rng = new Random()
            let indices = Seq.initInfinite(fun _ -> rng.Next(len))

            let rec descent curWeights alpha =
                let updatedWeights =
                    indices
                    |> Seq.take len
                    |> Seq.fold (fun w i -> 
                        let (label, observ) = y_data.[i], Vector.singleton x_data.[i]
                        update alpha w observ label) curWeights
                if changeRate curWeights updatedWeights <= epsilon
                then updatedWeights
                else 
                    let coolerAlpha = max epsilon cooling * alpha
                    descent updatedWeights coolerAlpha

            let weights = Vector.zero (2) // 1 more weight for constant

            descent weights alpha


        /// Returns the regression function
        let fit (coef: Vector<float>) x= 
            predict coef (Vector.singleton x)

        let estimateAlpha epsilon (x_data : Vector<float>) (y_data : Vector<float>) = 
            let fR2 alpha = 
                let weight = coefficient epsilon alpha x_data y_data
                let f = fit weight
                let r2 = GoodnessOfFit.calculateSSE f x_data y_data
                r2
            Optimization.Brent.minimizeWith fR2 0. 1. 0.001 100

    module Multivariable = 

        /// Calculates the weights for logistic regression.
        let coefficient epsilon alpha (x_data : Matrix<float>) (y_data : Vector<float>) =
            if (x_data.NumRows) <> y_data.Length then
                raise (System.ArgumentException("columns of matrix x and vector y have to be the same size!"))

            let len = x_data.NumRows
            let cooling = 0.9
            let rng = new Random()
            let indices = Seq.initInfinite(fun _ -> rng.Next(len))

            let rec descent curWeights alpha =
                let updatedWeights =
                    indices
                    |> Seq.take len
                    |> Seq.fold (fun w i -> 
                        let (label, observ) = y_data.[i], vector (Matrix.getRow x_data i)
                        update alpha w observ label) curWeights
                if changeRate curWeights updatedWeights <= epsilon
                then updatedWeights
                else 
                    let coolerAlpha = max epsilon cooling * alpha
                    descent updatedWeights coolerAlpha
            let vars = x_data.NumCols
            let weights = Vector.zero (vars+1) // 1 more weight for constant

            descent weights alpha
    
        /// Returns the regression function
        let fitFunc (coef: Vector<float>) = 
            fun (x:Vector<float>) -> predict coef x

        /// Returns the regression function
        let fit (coef: Vector<float>) (x:Vector<float>)= 
            predict coef x

        //let estimateAlpha epsilon (x_data : Matrix<float>) (y_data : Vector<float>) = 
        //    let fR2 alpha = 
        //        let weight = coefficient epsilon alpha x_data y_data
        //        let f = fitFunc weight
        //        let r2 = GoodnessOfFit.calculateSSE f x_data y_data
        //        r2
        //    Optimization.Brent.minimizeWith fR2 0. 1. 0.001 100