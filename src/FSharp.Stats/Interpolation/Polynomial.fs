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

    ///gets derivative at x with given polynomial coefficients. Level1 = fst derivative; Level2 = smd derivative ...
    let getDerivative (*(order: int)*) (coef: Vector<float>) (level: int) (x: float) =
        let order = coef.Length - 1
        Array.init (order + 1) (fun i -> 
            let factor = 
                //[for l = 0 to (level - 1) do yield i-l] 
                List.init level (fun l -> i-l)
                |> List.filter (fun v -> not (nan.Equals(v)))
                |> List.fold (fun acc c -> acc * (float c)) 1.
            factor * coef.[i] * (pown x (i-level))
            )
        |> Array.filter (fun v -> not (nan.Equals(v)))
        |> Array.sum

