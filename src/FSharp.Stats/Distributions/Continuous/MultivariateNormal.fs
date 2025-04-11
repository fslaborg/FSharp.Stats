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
    static member CheckParam (mu:Vector<float>) (sigma:Matrix<float>) =
        // TODO Implement checkParam for MultivariateNormal 
        if false then 
            failwith "Multivariate normal distribution should be parametrized by "
      
    
    /// Computes the mean.
    static member Mean (mu:Vector<float>) (sigma:Matrix<float>) =
        MultivariateNormal.CheckParam mu sigma
        mu
    /// Computes the variance.
    static member Variance (mu:Vector<float>) (sigma:Matrix<float>) =
        MultivariateNormal.CheckParam mu sigma
        //sigma*sigma
        failwith "Not implemented yet."
    /// Computes the standard deviation.
    static member StandardDeviation (mu:Vector<float>) (sigma:Matrix<float>) =
        MultivariateNormal.CheckParam mu sigma
        //sigma 
        failwith "Not implemented yet."
    /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
    static member Sample (mu:Vector<float>) (sigma:Matrix<float>) =
        if Array.length mu = 2 then 
            let a = Algebra.LinearAlgebra.cholesky sigma
            let z = Random.boxMullerTransform() |> fun (a,b) -> [|a;b|]
            mu .+ a * z
        else failwith "Not implemented yet."

    /// Computes the probability density function.
    static member PDF (mu:Vector<float>) (sigma:Matrix<float>) (x:Vector<float>) =
        MultivariateNormal.CheckParam mu sigma
        let k = Seq.length mu |> float
        let tmp = x .- mu
        let tmp' = -0.5 .* tmp * (Algebra.LinearAlgebra.inverse sigma)
        let ex = Math.Exp(Vector.dot tmp' tmp)
        (2.*Math.PI)**(-k/2.) * (Algebra.LinearAlgebra.determinant sigma ** (-0.5)) * ex

    /// Computes the cumulative distribution function.
    static member CDF (mu:Vector<float>) (sigma:Matrix<float>) (x:Vector<float>) =
        failwith "Not implemented yet."
    /// Computes the inverse cumulative distribution function (quantile function).
    static member InvCDF (mu:Vector<float>) (sigma:Matrix<float>) (x:Vector<float>) =
        failwith "InvCDF not implemented yet."

    /// Initializes a multivariate normal distribution with mean mu and covariance matrix sigma       
    static member Init (mu:Vector<float>) (sigma:Matrix<float>) =
        { new ContinuousDistribution<Vector<float>,Vector<float>> with
            member d.Mode              = MultivariateNormal.Mean mu sigma
            member d.Mean              = MultivariateNormal.Mean mu sigma
            member d.StandardDeviation = MultivariateNormal.StandardDeviation mu sigma
            member d.Variance          = MultivariateNormal.Variance mu sigma
            member d.Sample ()         = MultivariateNormal.Sample mu sigma
            member d.PDF x             = MultivariateNormal.PDF mu sigma x      
            member d.CDF x             = MultivariateNormal.CDF mu sigma x         
            member d.InvCDF x             = MultivariateNormal.InvCDF mu sigma x         
            member d.Parameters        = DistributionParameters.MultivariateNormal {Mean=mu;StandardDeviation=sigma}
            override d.ToString()      = d.ToString()
        }




