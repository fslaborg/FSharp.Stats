namespace FSharp.Stats.Interpolation

open FSharp.Stats
open FSharp.Stats.Algebra

module Polynomial =
    
    //In general a polynomial with degree=datapointNumber - 1 is flexible enough to interpolate all datapoints.
    //Bug polynomial regression with degree=datapointNumber - 1 cannot be used for polynomial interpolation 
    //because the least squares approach is not sufficient to converge interpolating.


    ///calculates the polynomial coefficients for interpolating the given unsorted data. No duplicates allowed!
    let coefficients (x_Data: Vector<float>) (y_Data: Vector<float>) =
        if x_Data.Length <> y_Data.Length then
            raise (System.ArgumentException("vector x and y have to be the same size!"))
        let order = x_Data.Length - 1
        let A =
            Matrix.init (order + 1) (order + 1) (fun i j  -> 
                pown x_Data.[i] j
                )
        let b = y_Data
        LinearAlgebra.SolveLinearSystem A b

    ///takes interpolating
    let fit (coef : Vector<float>) (x:float) =
        coef |> Vector.foldi (fun i acc c -> acc + (c * (pown x i))) 0.

