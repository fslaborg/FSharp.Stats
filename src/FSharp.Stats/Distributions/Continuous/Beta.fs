namespace FSharp.Stats.Distributions.Continuous

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.Ops

// ######
// Beta distribution
// ######


/// Beta distribution
type Beta =
    // Beta distribution helper functions.
    static member CheckParam alpha beta = 
        if alpha <= 0.0 || beta <= 0.0 then 
            failwith "Beta distribution should be parametrized by alpha > 0.0, beta > 0.0."    

    /// <summary>Computes the mode.</summary>
    /// <remarks></remarks>
    /// <param name="alpha"></param>
    /// <param name="beta"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mode alpha beta =
        Beta.CheckParam alpha beta
        match alpha,beta with
        | 0.,0. -> 0.5
        | 0.,_ -> 0.    
        | _,0. -> 1.

        | a,b when Double.IsPositiveInfinity(a) && Double.IsPositiveInfinity(b) -> 0.5
        | a,_ when Double.IsPositiveInfinity(a) -> 1.
        | _,b when Double.IsPositiveInfinity(b) -> 0.

        | 1.,1. -> 0.
        | _ -> (alpha - 1.)/(alpha + beta - 2.)

    /// <summary>Computes the mean.</summary>
    /// <remarks></remarks>
    /// <param name="alpha"></param>
    /// <param name="beta"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mean alpha beta =
        Beta.CheckParam alpha beta
        alpha / (alpha + beta)

    /// <summary>Computes the variance.</summary>
    /// <remarks></remarks>
    /// <param name="alpha"></param>
    /// <param name="beta"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Variance alpha beta =
        Beta.CheckParam alpha beta
        (alpha * beta) / ((alpha + beta) * (alpha + beta) * (alpha + beta + 1.0))

    /// <summary>Computes the standard deviation.</summary>
    /// <remarks></remarks>
    /// <param name="alpha"></param>
    /// <param name="beta"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member StandardDeviation alpha beta =
        Beta.CheckParam alpha beta
        sqrt ((alpha * beta) / ((alpha + beta) * (alpha + beta) * (alpha + beta + 1.0)))

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="alpha"></param>
    /// <param name="beta"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Sample alpha beta = 
        // Source: fsmathtools
        Beta.CheckParam alpha beta
        let x = Gamma.Sample alpha 1.0
        let y = Gamma.Sample beta 1.0
        x / (x + y)

        
    /// <summary>Computes the log probability density function.</summary>
    /// <remarks></remarks>
    /// <param name="alpha"></param>
    /// <param name="beta"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member PDFLn alpha beta x = 
        Beta.CheckParam alpha beta
        if x >= 0.0 && x <= 1.0 then
            if x = 0. && alpha = 1. then  
                log beta
            elif x = 1. && beta = 1. then 
                log alpha
            else 
                (alpha - 1.) * log x + (beta - 1.) * log (1. - x) - SpecialFunctions.Beta._betaLn alpha beta
        else log 0.0
        
    /// <summary> Computes the probability density function.</summary> 
    /// <remarks> Calls exp(PDFLn) if alpha,beta &gt; 80</remarks> 
    static member PDF alpha beta x = 
        Beta.CheckParam alpha beta
        if x >= 0.0 && x <= 1.0 then
            if alpha > 80 || beta > 80 then 
                exp (Beta.PDFLn alpha beta x)
            else 
                (x ** (alpha - 1.0)) * ((1.0 - x) ** (beta - 1.0)) / (SpecialFunctions.Beta._beta alpha beta)
        else 0.0    
        
    /// <summary>Computes the cumulative distribution function.</summary>
    /// <remarks></remarks>
    /// <param name="alpha"></param>
    /// <param name="beta"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member CDF alpha beta x =
        Beta.CheckParam alpha beta
        if x < 0.0 then 0.0
        elif x > 1.0 then 1.0
        else 
            SpecialFunctions.Beta.lowerIncompleteRegularized alpha beta x

    /// <summary>Computes the inverse cumulative distribution function (quantile function).</summary>
    /// <remarks></remarks>
    /// <param name="alpha"></param>
    /// <param name="beta"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member InvCDF (alpha: float) (beta: float) (p: float) =
        Beta.CheckParam alpha beta
        if p < 0. || p > 1. then failwith "p must be in [0, 1]"
        if p = 0. then 0.
        elif p = 1. then 1.
        // Closed-form cases for boundary parameter values
        elif alpha = 1. && beta = 1. then p
        elif alpha = 1. then 1. - (1. - p) ** (1. / beta)
        elif beta  = 1. then p ** (1. / alpha)
        else
            // Newton–Raphson starting from the mean, clamped to (ε, 1−ε)
            let clamp x = max 1e-12 (min (1. - 1e-12) x)
            let x0 = clamp (alpha / (alpha + beta))

            let rec refine x iter =
                if iter >= 50 then x
                else
                    let fx  = Beta.CDF alpha beta x - p
                    let dfx = Beta.PDF alpha beta x
                    if abs dfx < 1e-300 then x
                    else
                        let x' = clamp (x - fx / dfx)
                        if abs (x' - x) < 1e-12 then x'
                        else refine x' (iter + 1)

            let xNR = refine x0 0
            // If Newton–Raphson left residual error, polish with Brent
            if abs (Beta.CDF alpha beta xNR - p) < 1e-8 then xNR
            else
                match Rootfinding.Brent.tryFindRootWith 1e-12 200
                          (fun x -> Beta.CDF alpha beta x - p) 1e-12 (1. - 1e-12) with
                | Some x -> x
                | None   -> xNR // best effort

    /// <summary>
    ///   Fits the underlying distribution to a given set of observations.
    /// </summary>
    static member Fit(observations:float[],?weights:float[]) =
        let mean, var = 
            match weights with
            | None   -> 
                let m = observations |> Array.average
                let v = observations |> Array.varOf m
                m,v
            | Some w -> 
                let m = observations |> Array.weightedMean w
                let v = observations |> Array.weightedVariance m w
                m, v

        if (var >= mean * (1.0 - mean)) then
                raise (NotSupportedException())

        let u = (mean * (1. - mean) / var) - 1.0
        let alpha = mean * u
        let beta = (1. - mean) * u
        (alpha, beta)


    /// <summary>
    ///   Estimates a new Beta distribution from a given set of observations.
    /// </summary>
    static member Estimate(observations:float[],?weights:float[]) =
        match weights with
        | None   -> Beta.Fit observations
        | Some w -> Beta.Fit (observations,w)
        |> fun (a,b) -> Beta.Init a b  

    /// <summary>Returns the support of the exponential distribution: [0.0, 1.0).</summary>
    /// <remarks></remarks>
    /// <param name="alpha"></param>
    /// <param name="beta"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Support alpha beta =
        Beta.CheckParam alpha beta
        Interval.CreateRightOpen<float>(0.0, 1.0)

    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="alpha"></param>
    /// <param name="beta"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member ToString alpha beta =
        sprintf "Beta(α = %f, β = %f)" alpha beta

    /// <summary>Initializes a Beta distribution</summary>
    /// <remarks></remarks>
    /// <param name="alpha"></param>
    /// <param name="beta"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Init alpha beta =
        { new ContinuousDistribution<float,float> with
            member d.Mean              = Beta.Mean alpha beta
            member d.StandardDeviation = Beta.StandardDeviation alpha beta   
            member d.Variance          = Beta.Variance alpha beta
            member d.CDF x             = Beta.CDF alpha beta x         
            member d.InvCDF x          = Beta.InvCDF alpha beta x         
            //member d.CoVariance        = Beta.CoVariance alpha beta 
            member d.Mode              = Normal.Mode alpha beta
            member d.Sample ()         = Beta.Sample alpha beta
            member d.Parameters        = DistributionParameters.Beta {Alpha=alpha; Beta=beta}
            member d.PDF x             = Beta.PDF alpha beta x
            //member d.PDFLn x             = Beta.PDFLn alpha beta x
            override d.ToString()      = Normal.ToString alpha beta 
        }
