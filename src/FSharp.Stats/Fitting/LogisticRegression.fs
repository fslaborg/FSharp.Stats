namespace FSharp.Stats.Fitting

open FSharp.Stats

/// Functions for creating and using logistic regression models.
///Implementation taken from Mathias Brandewinder 
///
///https://github.com/mathias-brandewinder/Machine-Learning-In-Action/blob/master/MachineLearningInAction/MachineLearningInAction/LogisticRegression.fs
module LogisticRegression =

    open System
    
    // Weights have 1 element more than observations, for constant
    let internal predict (weights: vector) (obs: vector) =
        Vector.init (obs.Length+1) (fun i -> if i = 0 then 1. else obs.[i-1])  
        |> Vector.dot weights 
        |> FSharp.Stats.SpecialFunctions.Logistic.standard

    let internal error (weights: vector) (obs: vector) label =
        label - predict weights obs

    let internal update alpha (weights: vector) (obs: vector) label =      
        Vector.add weights (Vector.scale (alpha * (error weights obs label)) (Vector.init (obs.Length+1) (fun i -> if i = 0 then 1. else obs.[i-1])))

    // simple training: returns vector of weights
    // after fixed number of passes / iterations over dataset, 
    // with constant alpha
    let internal simpleTrain (dataset: (float * vector) seq) passes alpha =
        let rec descent iter curWeights =
            match iter with 
            | 0 -> curWeights
            | _ ->
                dataset
                |> Seq.fold (fun w (label, observ) -> 
                    update alpha w observ label) curWeights
                |> descent (iter - 1)

        let vars = dataset |> Seq.item 1 |> snd |> Vector.length
        let weights = Vector.zeroCreate (vars+1) // 1 more weight for constant

        descent passes weights
    
    // 2-Norm of Vector (length)
    let internal norm (vector: float list) = 
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

        /// <summary>Calculates the weights for logistic regression.</summary>
        /// <remarks></remarks>
        /// <param name="epsilon"></param>
        /// <param name="alpha"></param>
        /// <param name="xData"></param>
        /// <param name="yData"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let fit epsilon alpha (xData : Vector<float>) (yData : Vector<float>) =
            if xData.Length <> yData.Length then
                raise (System.ArgumentException("vector x and y have to be the same size!"))

            let len = xData.Length
            let cooling = 0.9
            let rng = new Random()
            let indices = Seq.initInfinite(fun _ -> rng.Next(len))

            let rec descent curWeights alpha =
                let updatedWeights =
                    indices
                    |> Seq.take len
                    |> Seq.fold (fun w i -> 
                        let (label, observ) = yData.[i], Vector.singleton xData.[i]
                        update alpha w observ label) curWeights
                if changeRate curWeights updatedWeights <= epsilon
                then updatedWeights
                else 
                    let coolerAlpha = max epsilon cooling * alpha
                    descent updatedWeights coolerAlpha

            let weights = Vector.zeroCreate (2) // 1 more weight for constant

            descent weights alpha

        [<Obsolete("Use Univariable.fit instead.")>]
        let coefficient epsilon alpha (xData : Vector<float>) (yData : Vector<float>) = 
            fit epsilon alpha xData yData

        /// <summary>Returns the regression function</summary>
        /// <remarks></remarks>
        /// <param name="coef"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let predict (coef: Vector<float>) x= 
            predict coef (Vector.singleton x)

        let estimateAlpha epsilon (xData : Vector<float>) (yData : Vector<float>) = 
            let fR2 alpha = 
                let weight = fit epsilon alpha xData yData
                let f = predict weight
                let r2 = GoodnessOfFit.calculateSSE f xData yData
                r2
            Optimization.Brent.minimizeWith fR2 0. 1. 0.001 100

    module Multivariable = 

        /// <summary>Calculates the weights for logistic regression.</summary>
        /// <remarks></remarks>
        /// <param name="epsilon"></param>
        /// <param name="alpha"></param>
        /// <param name="xData"></param>
        /// <param name="yData"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let fit epsilon alpha (xData : Matrix<float>) (yData : Vector<float>) =
            if (xData.NumRows) <> yData.Length then
                raise (System.ArgumentException("columns of matrix x and vector y have to be the same size!"))

            let len = xData.NumRows
            let cooling = 0.9
            let rng = new Random()
            let indices = Seq.initInfinite(fun _ -> rng.Next(len))

            let rec descent curWeights alpha =
                let updatedWeights =
                    indices
                    |> Seq.take len
                    |> Seq.fold (fun w i -> 
                        let (label, observ) = yData.[i], vector (Matrix.getRow xData i)
                        update alpha w observ label) curWeights
                if changeRate curWeights updatedWeights <= epsilon
                then updatedWeights
                else 
                    let coolerAlpha = max epsilon cooling * alpha
                    descent updatedWeights coolerAlpha
            let vars = xData.NumCols
            let weights = Vector.zeroCreate (vars+1) // 1 more weight for constant

            descent weights alpha
    
        [<Obsolete("Use Multivariable.fit instead.")>]
        let coefficient epsilon alpha (xData : Matrix<float>) (yData : Vector<float>) = 
            fit epsilon alpha xData yData

        /// <summary>Returns the regression function</summary>
        /// <remarks></remarks>
        /// <param name="coef"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let predictFunc (coef: Vector<float>) = 
            fun (x:Vector<float>) -> predict coef x
                
        [<Obsolete("Use Multivariable.predictFunc instead.")>]
        let fitFunc (coef: Vector<float>) = 
            predictFunc coef

        /// <summary>Returns the regression function</summary>
        /// <remarks></remarks>
        /// <param name="coef"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let predict (coef: Vector<float>) (x: Vector<float>)= 
            predict coef x

        //let estimateAlpha epsilon (xData : Matrix<float>) (yData : Vector<float>) = 
        //    let fR2 alpha = 
        //        let weight = coefficient epsilon alpha xData yData
        //        let f = fitFunc weight
        //        let r2 = GoodnessOfFit.calculateSSE f xData yData
        //        r2
        //    Optimization.Brent.minimizeWith fR2 0. 1. 0.001 100