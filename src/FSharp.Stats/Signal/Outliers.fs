namespace FSharp.Stats.Signal 


module Outliers =
    open FSharp.Stats
    open Matrix

    /// <summary>Tukey's fences based on interquartile range. c defines the magnitude of interquartile range that is added/subtracted to Q3 and Q1 respectively.<br />Commonly c is 1.5 for outliers and 3 for points 'far out' (Tukey 1977).</summary>
    /// <remarks></remarks>
    /// <param name="k"></param>
    /// <param name="d"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let tukey (k:float) (d:float []) =
        let firstQ = Quantile.compute 0.25 d
        let thirdQ = Quantile.compute 0.75 d 
        let iqr = System.Math.Abs (thirdQ - firstQ)
        Interval.CreateClosed<float> ((firstQ - k * iqr),(thirdQ + k * iqr))
        
        
    /// <summary>Returns Z Score for an individual point. <br />x - raw score(raw data)<br />m - mean of the population<br />s - standard deviation of the population</summary>
    /// <remarks></remarks>
    /// <param name="x"></param>
    /// <param name="m"></param>
    /// <param name="s"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let zScore (x:float) (m:float) (s:float) =
        (x - m) / s

    /// <summary>Returns a list of Z scores of a population</summary>
    /// <remarks></remarks>
    /// <param name="ls"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let zScoresOfPopulation (ls:list<float>) =
        let m = List.mean ls
        let s = Seq.stDevPopulation(ls)
        [for x in ls -> zScore x m s]


    /// <summary>Returns a population interval according to desired max and min Z Score values    </summary>
    /// <remarks></remarks>
    /// <param name="minZ"></param>
    /// <param name="maxZ"></param>
    /// <param name="ls"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let populationIntervalByZScore (minZ:float) (maxZ:float) (ls:list<float>) =
        let m = List.mean ls
        let s = Seq.stDevPopulation(ls)
        Interval.CreateClosed<float> ((minZ * s + m),(maxZ * s + m))
    
    /// <summary>Returns a list of Z scores of a sample</summary>
    /// <remarks></remarks>
    /// <param name="ls"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let zScoresOfSample (ls:list<float>) =
        let m = List.mean ls
        let s = Seq.stDev(ls)
        [for x in ls -> zScore x m s]

    /// <summary>Returns a sample interval according to desired max and min Z Score values    </summary>
    /// <remarks></remarks>
    /// <param name="minZ"></param>
    /// <param name="maxZ"></param>
    /// <param name="ls"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let sampleIntervalByZscore (minZ:float) (maxZ:float) (ls:list<float>) =
        let m = List.mean ls
        let s = Seq.stDev(ls)
        Interval.CreateClosed<float> ((minZ * s + m),(maxZ * s + m))

    ///Returns Mahalanobi's distance for an individual observation in a matrix.
    ///dataSource - Sample or Population.
    /// <summary>orientation - RowWise or ColWise.</summary>
    /// <remarks></remarks>
    /// <param name="dataMatrix"></param>
    /// <param name="dataSource"></param>
    /// <param name="orientation"></param>
    /// <param name="observation"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mahalanobisDistanceOfEntry (dataMatrix:matrix) (dataSource:DataSource) (orientation:Orientation) (observation:seq<float>) :float =
        let sub (a:seq<float>) b = Seq.map2 (fun xa xb -> xb - xa) a b
        let invertedCovarianceMatrix = Algebra.LinearAlgebra.Inverse (covarianceMatrixOf dataSource orientation.Inverse dataMatrix)
        let meanVector = Matrix.meanAsSeq orientation.Inverse dataMatrix
        let subObsMean = Matrix.ofJaggedColSeq([sub observation meanVector])
        let multObsCov = Matrix.mul (subObsMean.Transpose) invertedCovarianceMatrix
        let distance = Matrix.toScalar(Matrix.mul multObsCov subObsMean)
        sqrt distance    

    ///Returns Mahalanobi's distance for for every observation in a matrix.
    ///dataSource - Sample or Population.
    /// <summary>orientation - RowWise or ColWise. (RowWise orientation means that each row is a Vector) </summary>
    /// <remarks></remarks>
    /// <param name="dataSource"></param>
    /// <param name="orientation"></param>
    /// <param name="dataMatrix"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mahalanobisDistances (dataSource:DataSource) (orientation:Orientation) (dataMatrix:matrix) =
        let getObsList = 
            match orientation with 
                |ColWise -> 
                    dataMatrix.Transpose
                    |> Matrix.toJaggedArray
                |RowWise -> 
                    dataMatrix |> Matrix.toJaggedArray
        getObsList |> Array.map (mahalanobisDistanceOfEntry dataMatrix dataSource orientation)