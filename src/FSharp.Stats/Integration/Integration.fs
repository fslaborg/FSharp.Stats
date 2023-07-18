namespace FSharp.Stats.Integration

open FSharp.Stats
open FSharpAux

/// Approximation methods for definitive integral estimation
type NumericalIntegrationMethod =
    /// Left Riemann sum or left endpoint rule - approximation via partition intervals using the function values of the left partition boundary
    | LeftEndpoint
    /// Right Riemann sum or right endpoint rule - approximation via partition intervals using the function values of the right partition boundary
    | RightEndpoint
    /// Midpoint rule - approximation via partition intervals using the function values of the mid points of the partition boundaries
    /// 
    /// Note: when estimating definite integrals for observations, the best guess for midpoints is at xMid = (x2-x1)/2 , yMid = (y2-y1)/2 which will lead to the same results as the trapezoidal rule.
    | Midpoint
    /// Trapezoidal rule - approximation via partition intervals using trapezoids in the partition boundaries
    | Trapezoidal
    ///Simpson's 1/3 rule or 'Kepplersche Faßregel' (barrel rule) - approximation via parabolas
    | Simpson

    /// returns a function that returns the estimated partition integrals of a function with the given method 
    static member getPartitionIntegrals = function 
        | LeftEndpoint -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.map (fun boundary -> f(boundary) * partitionSize)
        | RightEndpoint -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.map (fun boundary -> f(boundary + partitionSize) * partitionSize)
        | Midpoint -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.map (fun boundary -> f(boundary + (partitionSize / 2.)) * partitionSize)
        | Trapezoidal -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.map (fun boundary -> ((f(boundary) + f(boundary + partitionSize)) * partitionSize) / 2.)
        | Simpson -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.map (fun boundary -> ((f(boundary) + 4.0 * f(boundary + partitionSize / 2.0) + f(boundary + partitionSize)) * partitionSize) / 6.0)

    /// returns a function that estimates the definite integral of a function with the given method 
    static member calculateDefiniteFunctionIntegral = function 
        | LeftEndpoint -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.sumBy (fun boundary -> f(boundary) * partitionSize)
        | RightEndpoint -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.sumBy (fun boundary -> f(boundary + partitionSize) * partitionSize)
        | Midpoint -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.sumBy (fun boundary -> f(boundary + (partitionSize / 2.)) * partitionSize)
        | Trapezoidal -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.sumBy (fun boundary -> ((f(boundary) + f(boundary + partitionSize)) * partitionSize) / 2.)
        | Simpson -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.sumBy (fun boundary -> ((f(boundary) + 4.0 * f(boundary + partitionSize / 2.0) + f(boundary + partitionSize)) * partitionSize) / 6.0)

    /// returns a function that estimates the definite integral of a function with the given method in parallel
    static member calculateDefiniteFunctionIntegralParallel = function 
        | LeftEndpoint -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> PSeq.sumBy (fun boundary -> f(boundary) * partitionSize)
        | RightEndpoint -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> PSeq.sumBy (fun boundary -> f(boundary + partitionSize) * partitionSize)
        | Midpoint -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> PSeq.sumBy (fun boundary -> f(boundary + (partitionSize / 2.)) * partitionSize)
        | Trapezoidal -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> PSeq.sumBy (fun boundary -> ((f(boundary) + f(boundary + partitionSize)) * partitionSize) / 2.)
        | Simpson -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> PSeq.sumBy (fun boundary -> ((f(boundary) + 4.0 * f(boundary + partitionSize / 2.0) + f(boundary + partitionSize)) * partitionSize) / 6.0)

    /// returns a function that returns the estimated partition integrals of observations with the given method 
    static member getObservationPartitionIntegrals = function
        | LeftEndpoint -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.mapi (fun i x ->
                if i = xVals.Length - 1 then 
                    0.
                else 
                    let rectWidth = xVals[i+1] - x
                    (rectWidth*yVals[i])
            )
        | RightEndpoint -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.mapi (fun i x ->
                if i = 0 then 
                    0. 
                else 
                    let rectWidth = x - xVals[i-1]
                    (rectWidth*yVals[i])
            )
        | Midpoint -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.mapi (fun i x ->
                if i = xVals.Length - 1 then 
                    0.
                else 
                    let midValue = (yVals[i] + yVals[i+1]) / 2.
                    let rectWidth = xVals[i+1] - x
                    (rectWidth*midValue)
            )
        | Trapezoidal -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.mapi (fun i x ->
                if i = xVals.Length - 1 then 
                    0.
                else 
                    let trapezoid = (xVals[i+1] - x) * ((yVals[i] + yVals[i+1]) / 2.)
                    trapezoid
            )
        | Simpson -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.mapi (fun i x ->
                if i = xVals.Length - 1 then 
                    0.
                else 
                    let midValue = (yVals[i] + yVals[i+1]) / 2.
                    let parabola = ((xVals[i+1] - x) / 6.) * (yVals[i] + (4. * midValue) + yVals[i+1])
                    parabola
            )

    /// returns a function that estimates the definite integral of observations with the given method 
    static member calculateDefiniteObservationIntegral = function
        | LeftEndpoint -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.foldi (fun i acc x ->
                if i = xVals.Length - 1 then 
                    acc
                else 
                    let rectWidth = xVals[i+1] - x
                    acc + (rectWidth*yVals[i])
            ) 0.
        | RightEndpoint -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.foldi (fun i acc x ->
                if i = 0 then 
                    acc 
                else 
                    let rectWidth = x - xVals[i-1]
                    acc + (rectWidth*yVals[i])
            ) 0.
        | Midpoint -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.foldi (fun i acc x ->
                if i = xVals.Length - 1 then 
                    acc
                else 
                    let midValue = (yVals[i] + yVals[i+1]) / 2.
                    let rectWidth = xVals[i+1] - x
                    acc + (rectWidth*midValue)
            ) 0.
        | Trapezoidal -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.foldi (fun i acc x ->
                if i = xVals.Length - 1 then 
                    acc
                else 
                    let trapezoid = (xVals[i+1] - x) * ((yVals[i] + yVals[i+1]) / 2.)
                    acc + trapezoid
            ) 0.
        | Simpson -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.foldi (fun i acc x ->
                if i = xVals.Length - 1 then 
                    acc 
                else 
                    let midValue = (yVals[i] + yVals[i+1]) / 2.
                    let parabola = ((xVals[i+1] - x) / 6.) * (yVals[i] + (4. * midValue) + yVals[i+1])
                    acc + parabola
            ) 0.

/// Definite integral approximation
type NumericalIntegration() = 
    
    /// Returns a function that approximates the definite integral of the input function (float -&gt; float) with the given `method` for partitions of equal size `partitions` in an inclusive closed interval [`intervalStart`, `intervalEnd`]
    static member definiteIntegral(
        /// the numerical integration approximation method to use
        method: NumericalIntegrationMethod,
        /// leftmost x value, boundary of the integration interval
        intervalStart: float,
        /// rightmost x value, boundary of the integration interval
        intervalEnd: float,
        /// the amount of partitions in the integration interval
        partitions: int,
        /// wether to run in parallel
        ?Parallel: bool
    ) = 
        fun (f: float -> float) ->
            //size of the individual partitions
            let partitionSize = (intervalEnd - intervalStart) / (float partitions)
            //all left boundaries to use for integration methods
            let leftBoundaries = [0 .. 1 .. partitions - 1] |> Seq.map (fun i -> intervalStart + (float i) * partitionSize)

            let isParallel = defaultArg Parallel false

            if isParallel then
                NumericalIntegrationMethod.calculateDefiniteFunctionIntegralParallel method f leftBoundaries partitionSize
            else
                NumericalIntegrationMethod.calculateDefiniteFunctionIntegral method f leftBoundaries partitionSize

    
    /// returns a function that approximates the AUC of the observations ((x,y) data) with the given numerical integration method
    static member definiteIntegral(
        /// the numerical integration approximation method to use
        method: NumericalIntegrationMethod
    ) = 
        fun (data: (float*float) []) -> NumericalIntegrationMethod.calculateDefiniteObservationIntegral method data
        