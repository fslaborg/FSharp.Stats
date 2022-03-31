namespace FSharp.Stats.Integration

open FSharp.Stats
open FSharpAux

/// Approximation methods for definitive integral estimation
type NumericalIntegrationMethod =
    | LeftEndpoint
    | RightEndpoint
    | MidPoint
    | Trapezoidal
    | Simpson

    static member integrateFunction = function 
        | LeftEndpoint -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.sumBy (fun boundary -> f(boundary) * partitionSize)
        | RightEndpoint -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.sumBy (fun boundary -> f(boundary + partitionSize) * partitionSize)
        | MidPoint -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.sumBy (fun boundary -> f(boundary + (partitionSize / 2.)) * partitionSize)
        | Trapezoidal -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.sumBy (fun boundary -> ((f(boundary) + f(boundary + partitionSize)) * partitionSize) / 2.)
        | Simpson -> fun (f: (float -> float)) leftBoundaries partitionSize -> leftBoundaries |> Seq.sumBy (fun boundary -> ((f(boundary) + 4.0 * f(boundary + partitionSize / 2.0) + f(boundary + partitionSize)) * partitionSize) / 6.0)

    static member integrateObservations = function
        | LeftEndpoint -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.foldi (fun i auc x ->
                if i = xVals.Length - 1 then 
                    auc 
                else 
                    let rectWidth = xVals[i+1] - x
                    (auc + (rectWidth*yVals[i]))
            ) 0.
        | RightEndpoint -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.foldi (fun i auc x ->
                if i = 0 then 
                    auc 
                else 
                    let rectWidth = x - xVals[i-1]
                    (auc + (rectWidth*yVals[i]))
            ) 0.
        | MidPoint -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.foldi (fun i auc x ->
                if i = xVals.Length - 1 then 
                    auc 
                else 
                    let midValue = (yVals[i] + yVals[i+1]) / 2.
                    let rectWidth = xVals[i+1] - x
                    (auc + (rectWidth*midValue))
            ) 0.
        | Trapezoidal -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.foldi (fun i auc x ->
                if i = xVals.Length - 1 then 
                    auc 
                else 
                    let trapezoid = (xVals[i+1] - x) * ((yVals[i] + yVals[i+1]) / 2.)
                    (auc + trapezoid)
            ) 0.
        | Simpson -> fun (observations: (float*float) []) ->
            let xVals, yVals = observations |> Array.unzip
            xVals
            |> Array.foldi (fun i auc x ->
                if i = xVals.Length - 1 then 
                    auc 
                else 
                    let midValue = (yVals[i] + yVals[i+1]) / 2.
                    let parabola = ((xVals[i+1] - x) / 6.) * (yVals[i] + (4. * midValue) + yVals[i+1])
                    (auc + parabola)
            ) 0.

/// Definite integral approximation
type NumericalIntegration() = 
    
    /// Returns a function that performs numerical integration of the input with the given `method` for partitions of equal size `partitions` in an inclusive closed interval [`intervalStart`, `intervalEnd`]
    static member integrateFunction(
        /// the numerical integration approximation method to use
        method: NumericalIntegrationMethod,
        /// leftmost x value, boundary of the integration interval
        intervalStart: float,
        /// rightmost x value, boundary of the integration interval
        intervalEnd: float,
        /// the amount of partitions in the integration interval
        partitions: int
        ///// wether to run in parallel
        //?Parallel: bool,
        ///// how many threads to use when running in parallel
        //?NThreads: bool
    ) = 
        fun (f: float -> float) ->
            //size of the individual partitions
            let partitionSize = (intervalEnd - intervalStart) / (float partitions)
            //all left boundaries to use for integration methods
            let leftBoundaries = [0 .. 1 .. partitions - 1] |> Seq.map (fun i -> intervalStart + (float i) * partitionSize)

            NumericalIntegrationMethod.integrateFunction method f leftBoundaries partitionSize
    
    /// returns a function that approximates the AUC of the observations ((x,y) data) with the given numerical integration method
    static member integrateObservations(
        /// the numerical integration approximation method to use
        method: NumericalIntegrationMethod
    ) = 
        fun (data: (float*float) []) -> NumericalIntegrationMethod.integrateObservations method data
        

module DefiniteIntegral =

    type IntegrationMethod = (float->float) -> float -> float -> float

    let midRect f (x:float) (h:float) = f (x + h/2.)

    let trapezium f (x:float) (h:float) = ( (f x) + f (x+h)) / 2.0

    let simpson f (x:float) (h:float) = (f x + 4. * f (x + h/2.) + f(x+h))/6.0

    let integrate (methode:IntegrationMethod) f a b steps =
        let h = (b - a ) / steps
        let rec loop acc i =
            if i >= steps then
                acc
            else
                let sum = methode f (a+i*h) h 
                loop (acc+sum) (i+1.)
        
        h * loop 0. 0.

    let integratePSeq (methode:IntegrationMethod) f a b steps =
        let h = (b - a ) / steps
        [0. .. steps - 1.]
        |> FSharpAux.PSeq.sumBy (fun i ->
            methode f (a+i*h) h
            )
        |> fun x -> h * x

// let f' (x:float) = x * x * x 
// integrate simpson f' 0. 1. 100.