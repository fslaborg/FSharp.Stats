namespace FSharp.Stats.Fitting

open FSharp.Stats

/// Functions for creating and using logistic regression models.
///Implementation taken from Mathias Brandewinder 
///
///https://github.com/mathias-brandewinder/Machine-Learning-In-Action/blob/master/MachineLearningInAction/MachineLearningInAction/LogisticRegression.fs
module LogisticRegression =

    open System
    open FsMath
    
    /// Creates a new vector (length = obs.Length + 1) whose
    /// first element is 1.0 (for the intercept), followed by obs's elements.
    /// If obs = [x0; x1; ...; xN-1], then withIntercept obs = [1.0; x0; x1; ...; xN-1].
    let internal withIntercept (obs: Vector<float>) : Vector<float> =
        let n = obs.Length
        let result = Vector.zeroCreate (n + 1)
        result.[0] <- 1.0
        // copy obs into result[1..], leveraging the library's copy or a loop
        Array.blit obs 0 result 1 n
        result    

    /// Logistic regression prediction:
    ///   weights : Vector<float> of length obs.Length+1
    ///   obs     : Vector<float> (the features, excluding intercept)
    /// Returns logistic(dot(weights, [1.0; obs]))
    let internal predict (weights: Vector<float>) (obs: Vector<float>) =
        // Build intercept-augmented obs
        let iobs = withIntercept obs
        // Dot product => logistic function
        Vector.dot weights iobs
        |> FSharp.Stats.SpecialFunctions.Logistic.standard

    /// Error = (label - prediction)
    let internal error (weights: Vector<float>) (obs: Vector<float>) label =
        label - predict weights obs

    /// Update rule: weights <- weights + alpha * error * [1.0; obs]
    let internal update alpha (weights: Vector<float>) (obs: Vector<float>) label =
        // 1) compute scalar = alpha * error
        let e = alpha * error weights obs label
        // 2) build intercept obs
        let iobs = withIntercept obs
        // 3) scaled = e * iobs
        let scaled = e .* iobs
        // 4) add => new weights
        Vector.add weights scaled


    // simple training: returns vector of weights
    // after fixed number of passes / iterations over dataset, 
    // with constant alpha
    let internal simpleTrain (dataset: (float * Vector<float>) seq) passes alpha =
        let rec descent iter curWeights =
            match iter with 
            | 0 -> curWeights
            | _ ->
                dataset
                |> Seq.fold (fun w (label, observ) -> 
                    update alpha w observ label) curWeights
                |> descent (iter - 1)

        let vars = dataset |> Seq.item 1 |> snd |> Array.length
        let weights = Vector.zeroCreate (vars+1) // 1 more weight for constant

        descent passes weights
    
    // 2-Norm of Vector (length)
    let internal norm (vector: float list) = 
        vector |> List.sumBy (fun e -> e * e) |> sqrt

    // rate of change in the weights vector,
    // computed as the % change in norm
    let private changeRate (before:Vector<float>) (after:Vector<float>) =
        let numerator = 
            Vector.subtract before after
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
                        let (label, observ) = yData.[i],  [|xData.[i]|]
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
        /// <param name="x"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let predict (coef: Vector<float>) x= 
            predict coef ([|x|])

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
                        let (label, observ) = yData.[i],  (Matrix.getRow i xData)
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
        /// <param name="x"></param>
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
        /// <param name="x"></param>
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