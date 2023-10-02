namespace FSharp.Stats.Distributions.Discrete

open System
open FSharp.Stats
open FSharp.Stats.Distributions
open FSharp.Stats.SpecialFunctions

// ######
// Poisson-Distribution
// ----------------------------------------------
// wiki: "href="https://en.wikipedia.org/wiki/Poisson_distribution"
// ######

    
    // (lambda) is the expected number of occurrences.    


    
///Poisson distribution
type Poisson =

    // Poisson distribution helper functions.
    static member CheckParam lambda = if lambda <= 0. then failwith "Poisson distribution should be parametrized by lambda > 0."

    /// <summary>Computes the mode.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mode lambda =
        Poisson.CheckParam lambda
        floor lambda |> int

    /// <summary>Computes the mean.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Mean lambda =
        Poisson.CheckParam lambda
        lambda

    /// <summary>Computes the variance.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Variance lambda =
        Poisson.CheckParam lambda
        lambda

    /// <summary>Computes the standard deviation.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member StandardDeviation lambda =
        Poisson.CheckParam lambda
        lambda |> sqrt

    /// <summary>
    ///   Computes the entropy for this distribution.
    /// </summary>
    /// 
    /// <remarks>     
    ///   It's an approximation and better for large lambda.
    /// </remarks>
    static member Entropy lambda =
        0.5 * System.Math.Log(2.0 * System.Math.PI * lambda)
                        - 1.  / (12. * lambda)
                        - 1.  / (24. * lambda * lambda)
                        - 19. / (360. * lambda * lambda * lambda);        
            

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).<br />No parameter checking!</summary>
    /// <remarks></remarks>
    /// <param name="SampleUnchecked"></param>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member internal SampleUnchecked lambda =            
        let rec knuth p l k =
            // Knuth, 1969.            
            if p > l then
                knuth (p * Random.rndgen.NextFloat()) l (k+1)
            else
                k - 1
        // Rejection method PA" from "The Computer Generation of Poisson Random Variables" by A. C. Atkinson,
        // Journal of the Royal Statistical Society Series C (Applied Statistics) Vol. 28, No. 1. (1979)
        // The article is on pages 29-35. The algorithm given here is on page 32.
        let rec pa c alpha beta k =
            let u = Random.rndgen.NextFloat()
            let x = (alpha - Math.Log((1.0 - u)/u))/beta
            let n = floor(x + 0.5) |> int
            if n < 0 then
                pa c alpha beta k
            else
                let v = Random.rndgen.NextFloat()
                let y = alpha - (beta*x);
                let tmp = 1.0 + Math.Exp(y);
                let lhs = y + Math.Log(v/(tmp*tmp));
                let rhs = k + (float n * Math.Log(lambda)) - SpecialFunctions.Factorial.factorialLn(n)

                if (lhs <= rhs) then
                    n
                else
                    pa c alpha beta k

        if (lambda < 30.0) then
            knuth 1. (System.Math.Exp(-lambda)) 0
        else            
            
            // This block is recalulated every sampleing time
            let c = 0.767 - (3.36/lambda)
            let beta = Math.PI/Math.Sqrt(3.0*lambda)
            let alpha = beta*lambda
            let k = Math.Log(c) - lambda - Math.Log(beta)

            pa c alpha beta k 
             

    /// <summary>Produces a random sample using the current random number generator (from GetSampleGenerator()).</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Sample lambda =
        Poisson.CheckParam lambda
        Poisson.SampleUnchecked lambda

    /// Computes the probability density function at k, i.e. P(K = k)
    static member PMF lambda (k:int) =
        if k > 170 then 
            System.Math.E ** (System.Math.Log lambda * float k - SpecialFunctions.Factorial._factorialLn k) * System.Math.E**(-lambda)
        else
            (lambda**float k * System.Math.E**(-lambda)) / SpecialFunctions.Factorial.factorial k
        
    /// <summary>Computes the cumulative distribution function at x, i.e. P(X &lt;= x).</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <param name="k"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member CDF lambda k =
        Poisson.CheckParam lambda        
        Gamma.upperIncompleteRegularized (k + 1.) lambda

    /// <summary>Computes the inverse cumulative distribution function (quantile function).</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <param name="k"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member InvCDF lambda k =
        Poisson.CheckParam lambda        
        failwithf "InvCDF not implemented yet"

    /// <summary>
    ///   Fits the underlying distribution to a given set of observations.
    /// </summary>
    static member Fit(observations:float[],?weights:float[]) =
        match weights with
        | None   -> observations |> Array.average
        | Some w -> observations |> Array.weightedMean w

    /// <summary>
    ///   Estimates a new Poisson distribution from a given set of observations.
    /// </summary>
    static member Estimate(observations:float[],?weights:float[]) =
        match weights with
        | None   -> observations |> Array.average
        | Some w -> observations |> Array.weightedMean w
        |> Poisson.Init  

    /// <summary>Returns the support interval for this distribution.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Support lambda =
        Poisson.CheckParam lambda
        Interval.CreateClosed<int> (0,System.Int32.MaxValue)

    /// <summary>A string representation of the distribution.</summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member ToString lambda =
        sprintf "Poisson(λ = %f)" lambda
    

    /// <summary>Initializes a Binomial distribution </summary>
    /// <remarks></remarks>
    /// <param name="lambda"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    static member Init lambda =
        { new DiscreteDistribution<float,int> with
            member d.Mean              = Poisson.Mean lambda
            member d.StandardDeviation = Poisson.StandardDeviation lambda
            member d.Variance          = Poisson.Variance lambda
            //member d.CoVariance        = Binomial.CoVariance p n
            member d.CDF k             = Poisson.CDF lambda k
            member d.InvCDF k          = Poisson.InvCDF lambda k
            
            member d.Mode              = Poisson.Mode lambda
            member d.PMF k             = Poisson.PMF lambda k
            member d.Sample ()         = Poisson.Sample lambda
            member d.Parameters        = DistributionParameters.Poisson {Lambda=lambda}
            override d.ToString()      = Poisson.ToString lambda
        }


