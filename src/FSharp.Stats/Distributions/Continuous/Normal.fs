namespace FSharp.Stats.Distributions.Continuous

// Source: FSharp.MathTools
open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops

// ######
// (Gaussian)- Normal distribution
// ######


    
/// Normal distribution.
type Normal =

    // Normal distribution helper functions.
    static member CheckParam mu sigma = 
        if System.Double.IsNaN(mu) || sigma < 0.0 then 
            failwith "Normal distribution should be parametrized by sigma > 0.0."

    /// <summary>Computes the mode.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mode mu sigma =
        Normal.CheckParam mu sigma
        mu

    /// <summary>Computes the mean.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mean mu sigma =
        Normal.CheckParam mu sigma
        mu

    /// <summary>Computes the variance.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Variance mu sigma =
        Normal.CheckParam mu sigma
        sigma*sigma

    /// <summary>Computes the standard deviation.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member StandardDeviation mu sigma =
        Normal.CheckParam mu sigma
        sigma

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member SampleUnchecked mu sigma =
        // Source: fsmathtools
        let mutable v1 = 2.0 * Random.rndgen.NextFloat() - 1.0
        let mutable v2 = 2.0 * Random.rndgen.NextFloat() - 1.0
        let mutable r = v1 * v1 + v2 * v2
        while (r >= 1.0 || r = 0.0) do
            v1 <- 2.0 * Random.rndgen.NextFloat() - 1.0
            v2 <- 2.0 * Random.rndgen.NextFloat() - 1.0
            r <- v1 * v1 + v2 * v2
        let fac = sqrt(-2.0*(log r)/r)
        (sigma * v1 * fac + mu)
        //failwith "Not implemented yet."
    
    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Sample mu sigma =
        // Source: fsmathtools
        Normal.CheckParam mu sigma
        Normal.SampleUnchecked mu sigma
        
    /// <summary>Computes the probability density function.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member PDF mu sigma x =
        Normal.CheckParam mu sigma
        (exp (-0.5 * (x-mu)*(x-mu) / (sigma*sigma))) / (sqrt (2.0 * Ops.pi * (sigma*sigma)))
        
    /// <summary>Computes the cumulative distribution function.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member CDF mu sigma x =
        Normal.CheckParam mu sigma            
        0.5 * (1.0 + SpecialFunctions.Errorfunction.Erf((x - mu)/(sigma*(sqrt 2.0))))

    /// <summary>Computes the quantile function (inverse cumulative distribution).</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <param name="p"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member InvCDF mu sigma p =
        Normal.CheckParam mu sigma
        //ALGORITHM AS241 from "Wichura, Algorithm AS 241: The Percentage Points of the Normal Distribution., 1988"
        if p = 0. then 
            -infinity
        elif p = 1. then 
            infinity 
        else 
            let paramA = 
                [|3.3871328727963666080e0;1.3314166789178437745e2;1.9715909503065514427e3;1.3731693765509461125e4;4.5921953931549871457e4;6.7265770927008700853e4;3.3430575583588128105e4;2.5090809287301226727e3|]
            let paramB = 
                [|4.2313330701600911252e1;6.8718700749205790830e2;5.3941960214247511077e3;2.1213794301586595867e4;3.9307895800092710610e4;2.8729085735721942674e4;5.2264952788528545610e3|]
            let paramC =
                [|1.42343711074968357734e-0;4.63033784615654529590e-0;5.76949722146069140550e-0;3.64784832476320460504e-0;1.27045825245236838258e-0;2.41780725177450611770e-1;2.27238449892691845833e-2;7.74545014278341407640e-4|]
            let paramD =
                [|2.05319162663775882187e-0;1.67638483018380384940e-0;6.89767334985100004550e-1;1.48103976427480074590e-1;1.51986665636164571966e-2;5.47593808499534494600e-4;1.05075007164441684324e-9|]
            let paramE =
                [|6.65790464350110377720e-0;5.46378491116411436990e-0;1.78482653991729133580e-0;2.96560571828504891230e-1;2.65321895265761230930e-2;1.24266094738807843860e-3;2.71155556874348757815e-5;2.01033439929228813265e-7|]
            let paramF =
                [|5.99832206555887937690e-1;1.36929880922735805310e-1;1.48753612908506148525e-2;7.86869131145613259100e-4;1.84631831751005468180e-5;1.42151175831644588870e-7;2.04426310338993978564e-15|]
        
            let bcv param r =
                param
                |> Array.rev
                |> Array.fold (fun acc x -> acc * r + x) 0.0
            let q = p - 0.5
            let v =
                if abs q <= 0.425 then //.LE.
                    let r = 0.180625 - q * q
                    let value = 
                        q * (bcv paramA r) / (bcv paramB r * r + 1.)
                    value
                else
                    let r = 
                        if q <= 0 then 
                            sqrt (-(log p))
                        else 
                            sqrt (-(log (1. - p)))
                    let v =
                        if r <= 5. then 
                            let r = r - 1.6
                            let value = 
                                (bcv paramC r) / (bcv paramD r * r + 1.)
                            value
                        else
                            let r = r - 5.
                            let value =
                                (bcv paramE r) / (bcv paramF r * r + 1.)
                            value
                    if q < 0.0 then -v else v
        
            mu + sigma * v



    /// <summary>Returns the support of the exponential distribution: [0, Positive Infinity).</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Support mu sigma =
        Normal.CheckParam mu sigma
        Interval.CreateRightOpen<float>(Double.NegativeInfinity, Double.PositiveInfinity)


    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member ToString mu sigma =
        sprintf "Normal(μ = %f, σ = %f)" mu sigma

    /// <summary>Initializes a Normal distribution        </summary>
    /// <remarks></remarks>
    /// <param name="mu"></param>
    /// <param name="sigma"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Init mu sigma =
        { new ContinuousDistribution<float,float> with
            member d.Mean              = Normal.Mean mu sigma
            member d.StandardDeviation = Normal.StandardDeviation mu sigma
            member d.Variance          = Normal.Variance mu sigma
            member d.CDF x             = Normal.CDF mu sigma x
            member d.InvCDF x          = Normal.InvCDF mu sigma x

            member d.Mode              = Normal.Mode mu sigma         
            member d.Sample ()         = Normal.Sample mu sigma
            member d.PDF x             = Normal.PDF mu sigma x      
            member d.Parameters        = DistributionParameters.Normal {Mean=mu;StandardDeviation=sigma}
            override d.ToString()      = Normal.ToString mu sigma
        }

    /// <summary>Estimates the Normal distribution parameters from sample data with maximum-likelihood.</summary>
    /// <remarks></remarks>
    /// <param name="samples"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Estimate samples =
        let s   = Seq.stats samples
        let mu  = SummaryStats.mean s
        let sigma = SummaryStats.stDev s
            
        Normal.Init mu sigma
