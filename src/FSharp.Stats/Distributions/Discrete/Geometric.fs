namespace FSharp.Stats.Distributions.Discrete

open System
open System.Numerics
open FSharp.Stats
open FSharp.Stats.Distributions
open FsMath
open FsMath.GenericMath

//IFloatingPoint

/// <summary>
/// Geometric distribution (number of failures before the first success).
/// </summary>
type Geometric =

    /// <summary>Checks whether the parameter is valid (0.0 &lt; p &lt;= 1.0).</summary>
    static member inline CheckParam<'T when 'T :> Numerics.INumber<'T> 
                    and 'T : comparison
                    and 'T : (new : unit -> 'T)> 
                (p: 'T) =
        if p <= 'T.Zero || p > 'T.One then
            failwith "Geometric distribution requires 0.0 < p <= 1.0"

    /// <summary>Computes the mean: (1 - p) / p</summary>
    static member inline Mean<'T when 'T :> Numerics.INumber<'T> 
                    and 'T : comparison
                    and 'T : (new : unit -> 'T)>
                (p: 'T) =
        Geometric.CheckParam p
        ('T.One - p) / p

    /// <summary>Computes the variance: (1 - p) / p^2</summary>
    static member inline Variance<'T when 'T :> Numerics.INumber<'T> 
                    and 'T : comparison
                    and 'T : (new : unit -> 'T)>
                (p: 'T) =
        Geometric.CheckParam p
        ('T.One - p) / (p * p)

    /// <summary>Computes the standard deviation.</summary>
    static member inline StandardDeviation<'T when 'T :> Numerics.INumber<'T> 
                    and Numerics.IRootFunctions<'T>
                    and 'T : comparison
                    and 'T : (new : unit -> 'T)>
                (p: 'T) =
        Geometric.CheckParam p
        (( 'T.One - p ) / (p * p)) |> GenericMath.sqrt

    /// <summary>Computes the mode (always 0).</summary>
    static member inline Mode<'T when 'T :> Numerics.INumber<'T> 
                    and 'T : comparison
                    and 'T : (new : unit -> 'T)>
                (p: 'T) =
        Geometric.CheckParam p
        LanguagePrimitives.GenericZero<'T>

    /// <summary>Computes the PMF: P(X = k) = (1 - p)^k * p</summary>
    static member inline PMF<'T when 'T :> Numerics.INumber<'T> 
                    and Numerics.IPowerFunctions<'T>
                    and 'T : comparison
                    and 'T : (new : unit -> 'T)>
                (p: 'T) (k: int) : 'T =
        Geometric.CheckParam p
        if k < 0 then 'T.Zero
        else
            let q = 'T.One - p
            p * q ** (T k)

    /// <summary>Computes the CDF: P(X ≤ k) = 1 - (1 - p)^(k + 1)</summary>
    static member inline CDF<'T when 'T :> Numerics.INumber<'T> 
                    and Numerics.IPowerFunctions<'T>
                    and 'T : comparison
                    and 'T : (new : unit -> 'T)> 
                (p: 'T) (k: int) : 'T =
        Geometric.CheckParam p
        if k < 0 then 'T.Zero
        else
            let q = 'T.One - p
            let tmp = k + 1
            'T.One - (GenericMath.pow q (T tmp))

    /// <summary>Computes the inverse CDF (quantile function).</summary>
    static member inline InvCDF<'T when 'T :> Numerics.IFloatingPoint<'T>
                    and Numerics.ILogarithmicFunctions<'T>
                    and 'T : comparison
                    and 'T : (new : unit -> 'T)> 
                (p: 'T) (x: 'T) = // : int =
        Geometric.CheckParam p
        if x < 'T.Zero || x > 'T.One then failwith "x must be in [0.0, 1.0]"
        let oneMinusX = GenericMath.log ('T.One - x)
        let oneMinusP = GenericMath.log ('T.One - p)
        let raw = oneMinusX / oneMinusP
        GenericMath.floor raw
        |> Convert.ToInt32

    /// <summary>samples a value using the built-in random number generator.</summary>
    static member inline sample<'T when 'T :> Numerics.IFloatingPoint<'T>
                    and Numerics.ILogarithmicFunctions<'T>
                    and 'T : comparison
                    and 'T : (new : unit -> 'T)>
                (p: 'T) : int =
        Geometric.CheckParam p
        let u = 'T.CreateTruncating(Random.rndgen.NextFloat())
        let oneMinusX = GenericMath.log ('T.One - u)
        let oneMinusP = GenericMath.log ('T.One - p)
        let raw = oneMinusX / oneMinusP
        GenericMath.floor raw
        |> Convert.ToInt32

    /// <summary>Fits the distribution by estimating p = 1 / (mean + 1).</summary>
    static member inline Fit<'T when 'T :> Numerics.IFloatingPoint<'T>
                    and 'T : (static member DivideByInt : 'T * int -> 'T)
                    and Numerics.ILogarithmicFunctions<'T>
                    and 'T : comparison
                    and 'T : (new : unit -> 'T)
                    and 'T : struct
                    and 'T :> ValueType>
                (observations: 'T[], ?weights: 'T[]) : 'T =
        let mean =
            match weights with
            | None   -> Vector.mean observations
            | Some w -> Array.weightedMean w observations
        'T.One / (mean + 'T.One)

    ///// <summary>Estimates and returns a new initialized distribution instance.</summary>
    //static member inline Estimate(observations: 'T[], ?weights: 'T[]) =
    //    let p = Geometric.Fit(observations, ?weights = weights)
    //    Geometric.Init p

    ///// <summary>Returns the support of the geometric distribution: {0, 1, 2, ...}.</summary>
    //static member inline Support(p: 'T) =
    //    Geometric.CheckParam p
    //    Interval.CreateLowerBounded<int>(0)

    /// <summary>Returns a formatted string representation.</summary>
    static member inline ToString(p: 'T) =
        $"Geometric(p = {p})"

    ///// <summary>Initializes the generic geometric distribution.</summary>
    //static member inline Init(p: 'T) =
    //    { new DiscreteDistribution<'T, int> with
    //        member _.Mean              = Geometric.Mean p
    //        member _.Variance          = Geometric.Variance p
    //        member _.StandardDeviation = Geometric.StandardDeviation p
    //        member _.CDF x             = Geometric.CDF(p, x)
    //        member _.InvCDF x          = Geometric.InvCDF(p, x)
    //        member _.Mode              = Geometric.Mode p
    //        member _.Sample()          = Geometric.Sample p
    //        member _.PMF x             = Geometric.PMF(p, x)
    //        member _.Parameters        = DistributionParameters.Geometric {P = float p} // Assumes parameter storage as float
    //        override _.ToString()      = Geometric.ToString p
    //    }


