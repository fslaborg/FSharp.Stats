namespace FSharp.Stats.Fitting


(*


*)


module CrossValidation =    
    open FSharp.Stats
    open FSharpAux

    module Error =
    
        /// Computes sum of squared residuals (SSR)
        let ssr (y:Vector<float>) (p:Vector<float>)=
            let residuals = y-p 
            residuals .* residuals
            |> Vector.sum

        /// Computes root mean square error (RMSE)
        let rmse (y:Vector<float>) (p:Vector<float>)=
            sqrt(((y-p).Transpose * (y-p))/(double y.Length))
    


    /// Computes a k fold cross-validation (in parallel)
    let inline kFoldParallel< ^T when ^T : (static member ( + ) : ^T * ^T -> ^T) 
            and  ^T : (static member DivideByInt : ^T*int -> ^T) 
            and  ^T : (static member Zero : ^T)>
        
            k iterations degreeOfParallelism (xData:Matrix< ^T >) (yData:Vector< ^T >)
                (fit: Matrix< ^T > -> Vector< ^T > -> Matrix< ^T > -> Vector< ^T >)
                    (error: Vector< ^T > -> Vector< ^T >-> ^T) 
                
                    =
    
        let chunkSize = yData.Length / k
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
    

    /// Computes a k fold cross-validation
    let inline kFold< ^T when ^T : (static member ( + ) : ^T * ^T -> ^T) 
            and  ^T : (static member DivideByInt : ^T*int -> ^T) 
            and  ^T : (static member Zero : ^T)> 
        
            k iterations (xData:Matrix< ^T >) (yData:Vector< ^T >)
                (fit: Matrix< ^T > -> Vector< ^T > -> Matrix< ^T > -> Vector< ^T >)
                    (error: Vector< ^T > -> Vector< ^T >-> ^T) 
                
                    =
    
        let chunkSize = yData.Length / k
        let chunks =
            Seq.init iterations (fun _ ->
                Array.init chunkSize (fun i -> FSharp.Stats.Random.rndgen.NextInt(i) )
             )
    
        chunks
        |> Seq.map (fun chunk ->
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
        |> Seq.average