namespace FSharp.Stats.Distributions.Continuous

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops

// ######
// Multivariate normal distribution
// ######

 
/// multivariate normal distribution.
type MultivariateNormal =
    
     // multivariate normal distribution helper functions.
    static member CheckParam (mu:vector) (sigma:matrix) =
        // TODO Implement checkParam for MultivariateNormal 
        if false then 
            failwith "Multivariate normal distribution should be parametrized by "
      
    
    /// Computes the mean.
    static member Mean (mu:vector) (sigma:matrix) =
        MultivariateNormal.CheckParam mu sigma
        mu
    /// Computes the variance.
    static member Variance (mu:vector) (sigma:matrix) =
        MultivariateNormal.CheckParam mu sigma
        //sigma*sigma
        failwith "Not implemented yet."
    /// Computes the standard deviation.
    static member StandardDeviation (mu:vector) (sigma:matrix) =
        MultivariateNormal.CheckParam mu sigma
        //sigma 
        failwith "Not implemented yet."
    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample (mu:vector) (sigma:matrix) =
        if Vector.length mu = 2 then 
            let a = Algebra.LinearAlgebra.Cholesky sigma
            let z = Random.boxMullerTransform() |> fun (a,b) -> vector [a;b]
            mu + a*z
        else failwith "Not implemented yet."

    /// Computes the probability density function.
    static member PDF (mu:vector) (sigma:matrix) (x:vector) =
        MultivariateNormal.CheckParam mu sigma
        let k = Seq.length mu |> float
        let ex = Math.Exp(-0.5 * (x - mu).Transpose * (Algebra.LinearAlgebra.Inverse sigma) * (x-mu))
        (2.*Math.PI)**(-k/2.) * (Algebra.LinearAlgebra.Determinant sigma ** (-0.5)) * ex
    /// Computes the cumulative distribution function.
    static member CDF (mu:vector) (sigma:matrix) (x:vector) =
        failwith "Not implemented yet."
    /// Computes the inverse cumulative distribution function (quantile function).
    static member InvCDF (mu:vector) (sigma:matrix) (x:vector) =
        failwith "InvCDF not implemented yet."

    /// Initializes a multivariate normal distribution with mean mu and covariance matrix sigma       
    static member Init (mu:vector) (sigma:matrix) =
        { new ContinuousDistribution<vector,vector> with
            member d.Mode              = MultivariateNormal.Mean mu sigma
            member d.Mean              = MultivariateNormal.Mean mu sigma
            member d.StandardDeviation = MultivariateNormal.StandardDeviation mu sigma
            member d.Variance          = MultivariateNormal.Variance mu sigma
            member d.Sample ()         = MultivariateNormal.Sample mu sigma
            member d.PDF x             = MultivariateNormal.PDF mu sigma x      
            member d.CDF x             = MultivariateNormal.CDF mu sigma x         
            member d.InvCDF x             = MultivariateNormal.InvCDF mu sigma x         
            override d.ToString()      = d.ToString()
        }




