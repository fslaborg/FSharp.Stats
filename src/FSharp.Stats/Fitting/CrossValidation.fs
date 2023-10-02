namespace FSharp.Stats.Fitting


(*


*)


module CrossValidation =    
    open System
    open FSharp.Stats
    open FSharpAux

    module Error =
    
        /// <summary>Computes sum of squared residuals (SSR)</summary>
        /// <remarks></remarks>
        /// <param name="y"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let ssr (y:Vector<float>) (p:Vector<float>)=
            let residuals = y-p 
            residuals.Transpose * residuals

        /// <summary>Computes root mean square error (RMSE)</summary>
        /// <remarks></remarks>
        /// <param name="y"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let rmse (y:Vector<float>) (p:Vector<float>)=
            sqrt((ssr y p)/(double y.Length))

    type CrossValidationResult<'a> =
        {
        Error           : 'a
        ErrorStDev      : 'a
        }

    let createCrossValidationResult error errorStDev = {Error=error;ErrorStDev=errorStDev}

    /// Computes a k fold cross-validation (in parallel)
    [<Obsolete("Use CrossValidation.kFold instead")>]
    let inline kFoldParallel< ^T when ^T : (static member ( + ) : ^T * ^T -> ^T) 
            and  ^T : (static member DivideByInt : ^T*int -> ^T) 
            and  ^T : (static member Zero : ^T)>
        
            k iterations degreeOfParallelism (xData:Matrix< ^T >) (yData:Vector< ^T >)
                (fit: Matrix< ^T > -> Vector< ^T > -> Matrix< ^T > -> Vector< ^T >)
                    (error: Vector< ^T > -> Vector< ^T >-> ^T) 
                
                    =
    
        let chunkSize = int (ceil (float yData.Length / float k))
        let chunks =
            Seq.init iterations (fun _ ->
                Array.init chunkSize (fun i -> FSharp.Stats.Random.rndgen.NextInt(i) )
             )
    
        chunks
        |> PSeq.map (fun chunk ->
            let xTest,xTrain =
                xData
                |> Matrix.splitRows chunk
            let yTest,yTrain =
                yData
                |> Vector.splitVector chunk
    
            let preds = fit xTrain yTrain xTest
            let error = error preds yTest
            error
        )
        |> PSeq.withDegreeOfParallelism degreeOfParallelism
        |> Seq.average

    /// <summary>Computes a repeated k fold cross-validation,<br />k: training set size (and number of iterations),<br />iterations: number of random subset creation,<br />xData: rowwise x-coordinate matrix,<br />yData: yData vector<br />fit: x and y data lead to function that maps a xData row vector to a y-coordinate,<br />error: defines the error of the fitted y-coordinate and the actual y-coordinate,<br />getStDev: function that calculates the standard deviation from a seq&lt;^T&gt;. (Seq.stDev)</summary>
    /// <remarks></remarks>
    /// <param name="repeatedKFold"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline repeatedKFold< ^T when ^T : (static member ( + ) : ^T * ^T -> ^T) 
                    and  ^T : (static member DivideByInt : ^T*int -> ^T) 
                    and  ^T : (static member Zero : ^T)> 
    
            k (iterations: int) (xData:Matrix< ^T >) (yData:Vector< ^T >)
                (fit: Matrix< ^T > -> Vector< ^T > -> RowVector< ^T > -> ^T)
                (error: ^T -> ^T -> ^T) 
                (getStDev: seq< ^T > -> ^T) =
        let chunkSize = int (ceil (float yData.Length / float k))
        [1..iterations]
        |> Seq.map (fun _ -> 
            let chunkIndices =
                [|0 .. yData.Length-1|]
                |> FSharp.Stats.Array.shuffleFisherYates
                |> Seq.chunkBySize chunkSize

            chunkIndices
            //|> PSeq.map (fun indices -> 
            |> Seq.map (fun indices -> 
                let xTest,xTrain =
                    xData
                    |> Matrix.splitRows indices
                let yTest,yTrain =
                    yData
                    |> Vector.splitVector indices
                xTest
                |> Matrix.Generic.mapiRows (fun i xSingle -> 
                    let preds = fit xTrain yTrain xSingle
                    let error = error preds yTest.[i]
                    error
                    )
                |> Seq.average
                )    
            |> Seq.average
            )
        |> Array.ofSeq
        |> fun errorSeq -> 
            createCrossValidationResult (Seq.average errorSeq) (getStDev errorSeq)

    /// <summary>Computes a k fold cross-validation,<br />k: training set size (and number of iterations),<br />xData: rowwise x-coordinate matrix,<br />yData: yData vector<br />fit: x and y data lead to function that maps a rowwise matrix of xCoordinates to a y-coordinate,<br />error: defines the error of the fitted y-coordinate and the actual y-coordinate</summary>
    /// <remarks></remarks>
    /// <param name="k"></param>
    /// <param name="xData"></param>
    /// <param name="yData"></param>
    /// <param name="fit"></param>
    /// <param name="error"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline kFold k (xData:Matrix< ^T >) (yData:Vector< ^T >)
        (fit: Matrix< ^T > -> Vector< ^T > -> RowVector< ^T > -> ^T)
        (error: ^T -> ^T -> ^T) =
        repeatedKFold k 1 xData yData fit error (fun s -> Seq.head s)
        |> fun r -> r.Error

    /// <summary>Computes a leave one out cross-validation<br />xData: rowwise x-coordinate matrix,<br />yData: yData vector<br />fit: x and y data lead to function that maps an xData row vector to a y-coordinate,<br />error: defines the error of the fitted y-coordinate and the actual y-coordinate</summary>
    /// <remarks></remarks>
    /// <param name="loocv"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline loocv< ^T when ^T : (static member ( + ) : ^T * ^T -> ^T) 
            and  ^T : (static member DivideByInt : ^T*int -> ^T) 
            and  ^T : (static member Zero : ^T)> 
            (xData:Matrix< ^T >) (yData:Vector< ^T >) (fitFunc:Matrix< ^T > -> Vector< ^T > -> (RowVector< ^T > -> ^T)) 
            (error: ^T -> ^T -> ^T) =
        
        let n = xData.NumRows
    
        [0..n-1]
        |> List.map (fun i ->
            let (xTest,xTrain) = 
                Matrix.splitRows [|i|] xData
                |> fun (y,x) -> Matrix.Generic.toRowVector y,x
            let (yTest,yTrain) = 
                Vector.splitVector [|i|] yData
                |> fun (y,x) -> y.[0],x            
            let fit = fitFunc xTrain yTrain
            let yFit = fit xTest
            error yFit yTest
            )
        |> List.average    

    /// <summary>Computes a repeated shuffel-and-split cross validation<br />p: percentage of training set size from original size,<br />iterations: number of random subset creation,<br />xData: rowwise x-coordinate matrix,<br />yData: yData vector<br />fit: x and y data lead to function that maps a xData row vector to a y-coordinate,<br />error: defines the error of the fitted y-coordinate and the actual y-coordinate,<br />getStDev: function that calculates the standard deviation from a seq&lt;^T&gt;. (Seq.stDev)</summary>
    /// <remarks></remarks>
    /// <param name="shuffelAndSplit"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline shuffelAndSplit< ^T when ^T : (static member ( + ) : ^T * ^T -> ^T) 
                    and  ^T : (static member DivideByInt : ^T*int -> ^T) 
                    and  ^T : (static member Zero : ^T)>
            p (iterations: int) (xData:Matrix< ^T >) (yData:Vector< ^T >)
                (fit: Matrix< ^T > -> Vector< ^T > -> RowVector< ^T > -> ^T)
                (error: ^T -> ^T -> ^T) 
                (getStDev: seq< ^T > -> ^T) =
        let n = xData.NumRows
        // size of training data set
        let m = float n * p |> ceil |> int
        [1..iterations]
        |> Seq.map (fun _ -> 
            let chunkIndices =
                [|0 .. n-1|]
                |> FSharp.Stats.Array.shuffleFisherYates
                |> Array.take m
            let xTest,xTrain =
                xData
                |> Matrix.splitRows chunkIndices
            let yTest,yTrain =
                yData
                |> Vector.splitVector chunkIndices
            xTest
            |> Matrix.Generic.mapiRows (fun i xSingle -> 
                let preds = fit xTrain yTrain xSingle
                let error = error preds yTest.[i]
                error
                )    
            |> Seq.average
            )
        |> Array.ofSeq
        |> fun errorSeq -> 
            createCrossValidationResult (Seq.average errorSeq) (getStDev errorSeq)
