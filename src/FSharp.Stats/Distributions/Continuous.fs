namespace FSharp.Stats.Distributions

// Source: FSharp.MathTools

open FSharp.Stats
open FSharp.Stats.Ops

// Continuous probability distributions
module Continuous = 
    
    open FSharp.Stats.SpecialFunctions

    
// ######
// ChiSquared distribution
// ######


    // ChiSquared distribution helper functions.
    let chiSquaredCheckParam dof = if System.Double.IsNaN(dof)  || dof < 0. then failwith "ChiSquared distribution should be parametrized by degrees of Freedom in [0,inf]."
    
    /// ChiSquared distribution.
    type ChiSquared =        
        /// Computes the mean.
        static member Mean dof =
            chiSquaredCheckParam dof
            dof
        /// Computes the variance.
        static member Variance dof =
            chiSquaredCheckParam dof
            dof * 2.
        /// Computes the standard deviation.
        static member StandardDeviation dof =
            chiSquaredCheckParam dof
            sqrt (dof * 2.)
        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample dof =
            chiSquaredCheckParam dof
            //rndgen.NextFloat() * (max - min) + min
            nan

        /// Computes the probability density function.
        static member PDF dof x =
            chiSquaredCheckParam dof
            if x < 0.0 || dof < 1. then
                0.0
            else
                let k = float dof * 0.5
                let x = x * 0.5
                if dof = 2. then
                    exp (-1. * x)
                else
                    let pValue = SpecialFunctions.Gamma.lowerIncomplete k x // incGamma -> gamma lower incomplete
                    if (isNan pValue) || (isInf pValue) ||  (pValue <= 1e-8) then
                        1e-14
                    else
                        1.- pValue / (SpecialFunctions.Gamma.gamma k)  
            

        /// Computes the cumulative distribution function.
        static member CDF dof x =
            chiSquaredCheckParam dof
            failwith "Not implemented yet."

        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support dof =
            chiSquaredCheckParam dof
            (0., System.Double.PositiveInfinity)


    /// Initializes a ChiSquared distribution        
    let chiSquared dof =
        { new Distribution<float,float> with
            member d.Mean              = ChiSquared.Mean dof
            member d.StandardDeviation = ChiSquared.StandardDeviation dof 
            member d.Variance          = ChiSquared.Variance dof
            //member d.CoVariance        = Uniform.CoVariance min max  
            member d.Sample ()         = ChiSquared.Sample dof
            member d.PDF x             = ChiSquared.PDF dof x           
            member d.CDF x             = ChiSquared.CDF dof  x         
        }



// ######
// Uniform distribution
// ######


    // Uniform distribution helper functions.
    let uniformCheckParam min max = if System.Double.IsNaN(min) || System.Double.IsNaN(max) || min > max then failwith "Uniform distribution should be parametrized by min < max in [-inf,inf]."
    
    /// Uniform distribution.
    type Uniform =        
        /// Computes the mean.
        static member Mean min max =
            uniformCheckParam min max
            min + (max - min) / 2.0

        /// Computes the variance.
        static member Variance min max =
            uniformCheckParam min max
            1.0/3.0 * (max*max + max * min + min*min)

        /// Computes the standard deviation.
        static member StandardDeviation min max =
            uniformCheckParam min max
            sqrt (1.0/3.0 * (max*max + max * min + min*min))

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample min max =
            uniformCheckParam min max
            //rndgen.NextFloat() * (max - min) + min
            nan

        /// Computes the probability density function.
        static member PDF min max x =
            uniformCheckParam min max
            if x <= max && x >= min then 1.0 / (max - min) else 0.0

        /// Computes the cumulative distribution function.
        static member CDF min max x =
            uniformCheckParam min max
            if x < min then 0.0
            elif x < max then (x - min) / (max - min)
            else 1.0

        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support min max =
            uniformCheckParam min max
            (0., System.Double.PositiveInfinity)

    /// Initializes a uniform distribution        
    let uniform min max =
        { new Distribution<float,float> with
            member d.Mean              = Uniform.Mean min max
            member d.StandardDeviation = Uniform.StandardDeviation min max   
            member d.Variance          = Uniform.Variance min max
            //member d.CoVariance        = Uniform.CoVariance min max  
            member d.Sample ()         = Uniform.Sample min max
            member d.PDF x             = Uniform.PDF min max x           
            member d.CDF x             = Uniform.CDF min max x         
        }   


// ######
// (Gaus)- Normal distribution
// ######


    // Normal distribution helper functions.
    let normalCheckParam mu tau = if System.Double.IsNaN(mu) || tau < 0.0 then failwith "Normal distribution should be parametrized by tau > 0.0."
    
    /// Normal distribution.
    type Normal =
        /// Computes the mean.
        static member Mean mu tau =
            normalCheckParam mu tau
            mu

        /// Computes the variance.
        static member Variance mu tau =
            normalCheckParam mu tau
            tau*tau

        /// Computes the standard deviation.
        static member StandardDeviation mu tau =
            normalCheckParam mu tau
            tau

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample mu tau =
            normalCheckParam mu tau
//            let mutable v1 = 2.0 * rndgen.NextFloat() - 1.0
//            let mutable v2 = 2.0 * rndgen.NextFloat() - 1.0
//            let mutable r = v1 * v1 + v2 * v2
//            while (r >= 1.0 || r = 0.0) do
//                v1 <- 2.0 * rndgen.NextFloat() - 1.0
//                v2 <- 2.0 * rndgen.NextFloat() - 1.0
//                r <- v1 * v1 + v2 * v2
//            let fac = sqrt(-2.0*(log r)/r)
//            (tau * v1 * fac + mu)
            failwith "Not implemented yet."

        /// Computes the probability density function.
        static member PDF mu tau x =
            normalCheckParam mu tau
            (exp (-0.5 * (x-mu)*(x-mu) / (tau*tau))) / (sqrt (2.0 * Ops.pi))

        /// Computes the cumulative distribution function.
        static member CDF mu tau x =
            normalCheckParam mu tau            
            0.5 * (1.0 + SpecialFunctions.Errorfunction.Erf((x - mu)/(tau*(sqrt 2.0))))

        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support mu tau =
            normalCheckParam mu tau
            (System.Double.NegativeInfinity, System.Double.PositiveInfinity)

    /// Initializes a Normal distribution        
    let normal mu tau =
        { new Distribution<float,float> with
            member d.Mean              = Normal.Mean mu tau
            member d.StandardDeviation = Normal.StandardDeviation mu tau
            member d.Variance          = Normal.Variance mu tau
            //member d.CoVariance        = Normal.CoVariance  mu tau
            member d.Sample ()         = Normal.Sample mu tau
            member d.PDF x             = Normal.PDF mu tau x      
            member d.CDF x             = Normal.CDF mu tau x         
        }   


// ######
// Exponential distribution
// ######


    // Exponential distribution helper functions.
    let expCheckParam lambda = if lambda <= 0.0 then failwith "Exponential distribution should be parametrized by lambda > 0.0."
    
    /// Exponential distribution.
    type Exponential =
        /// Computes the mean.
        static member Mean lambda =
            expCheckParam lambda
            1.0 / lambda

        /// Computes the variance.
        static member Variance lambda =
            expCheckParam lambda
            1.0 / (lambda * lambda)

        /// Computes the standard deviation.
        static member StandardDeviation lambda =
            expCheckParam lambda
            1.0 / lambda

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample lambda = 
            expCheckParam lambda
//            let mutable r = rndgen.NextFloat()
//            while (r = 0.0) do
//                r <- rndgen.NextFloat()
//            done;
//            (- log r)/lambda
            failwith "Not implemented yet."

        /// Computes the probability density function.
        static member PDF lambda x = 
            expCheckParam lambda
            if x >= 0.0 then
                - lambda * exp(-lambda * x)
            else 0.0

        /// Computes the cumulative distribution function.
        static member CDF lambda x =
            expCheckParam lambda
            if x < 0.0 then 0.0
            else 1.0 - exp(-lambda * x)

        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support lambda =
            expCheckParam lambda
            (0.0, System.Double.PositiveInfinity)

    /// Initializes a Exponential distribution        
    let exponential lambda =
        { new Distribution<float,float> with
            member d.Mean              = Exponential.Mean lambda
            member d.StandardDeviation = Exponential.StandardDeviation lambda   
            member d.Variance          = Exponential.Variance lambda
            //member d.CoVariance        = Uniform.CoVariance min max  
            member d.Sample ()         = Exponential.Sample lambda
            member d.PDF x             = Exponential.PDF lambda x           
            member d.CDF x             = Exponential.CDF lambda x         
        }   


// ######
// Gamma distribution
// ######


    // Gamma distribution helper functions.
    let gammaCheckParam alpha beta = if alpha <= 0.0 || beta <= 0.0 then failwith "Gamma distribution should be parametrized by alpha > 0.0, beta > 0.0."
    
    /// Gamma distribution
    /// Sampling implementation based on:
    ///     "A Simple Method for Generating Gamma Variables" - Marsaglia & Tsang
    ///     ACM Transactions on Mathematical Software, Vol. 26, No. 3, September 2000, Pages 363-372.
    type Gamma =
        /// Computes the mean.
        static member Mean alpha beta =
            gammaCheckParam alpha beta
            alpha / beta
        
        /// Computes the variance.
        static member Variance alpha beta =
            gammaCheckParam alpha beta
            alpha / (beta * beta)
        
        /// Computes the standard deviation.
        static member StandardDeviation alpha beta =
            gammaCheckParam alpha beta
            sqrt (alpha / (beta * beta))
        
        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample alpha beta = 
            gammaCheckParam alpha beta
//            let mutable a = alpha
//            // Fix when alpha is less than one.
//            let alphafix =
//                if alpha < 1.0 then
//                    a <- alpha + 1.0
//                    (rndgen.NextFloat() ** (1.0 / alpha))
//                else
//                    1.0
//            let d = a - 1.0 / 3.0
//            let c = 1.0 / sqrt(9.0 * d)
//            let rec gamma_sample () =
//                let mutable x = Normal.Sample 0.0 1.0
//                let mutable v = 1.0 + c * x
//                while v <= 0.0 do
//                    x <- Normal.Sample 0.0 1.0
//                    v <- 1.0 + c * x
//                v <- v * v * v
//                let u = rndgen.NextFloat()
//                x <- x * x
//                if u < 1.0 - 0.0331 * x * x then
//                    d * v
//                elif (log u) < 0.5 * x + d * (1.0 - v + (log v)) then
//                    d * v
//                else gamma_sample()
//            alphafix * gamma_sample() / beta
            failwith "Not implemented yet."
        
        /// Computes the probability density function.
        static member PDF alpha beta x = 
            gammaCheckParam alpha beta
            if x >= 0.0 then
                (beta**alpha) * (x ** (alpha - 1.0)) * (exp (-beta*x)) / SpecialFunctions.Gamma.gamma alpha
            else 0.0
        
        /// Computes the cumulative distribution function.
        static member CDF alpha beta x =
            gammaCheckParam alpha beta
            if x < 0.0 then 0.0
            else failwith "Not implemented yet."
        
        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support alpha beta =
            gammaCheckParam alpha beta
            (0.0, System.Double.PositiveInfinity)

    /// Initializes a Gamma distribution        
    let gamma alpha beta =
        { new Distribution<float,float> with
            member d.Mean              = Gamma.Mean alpha beta
            member d.StandardDeviation = Gamma.StandardDeviation alpha beta   
            member d.Variance          = Gamma.Variance alpha beta
            //member d.CoVariance        = Gamma.CoVariance alpha beta 
            member d.Sample ()         = Gamma.Sample alpha beta
            member d.PDF x             = Gamma.PDF alpha beta x           
            member d.CDF x             = Gamma.CDF alpha beta x         
        }   


// ######
// Beta distribution
// ######


    /// Beta distribution
    type Beta =
        /// Computes the mean.
        static member Mean alpha beta =
            gammaCheckParam alpha beta
            alpha / (alpha + beta)

        /// Computes the variance.
        static member Variance alpha beta =
            gammaCheckParam alpha beta
            (alpha * beta) / ((alpha + beta) * (alpha + beta) * (alpha + beta + 1.0))

        /// Computes the standard deviation.
        static member StandardDeviation alpha beta =
            gammaCheckParam alpha beta
            sqrt ((alpha * beta) / ((alpha + beta) * (alpha + beta) * (alpha + beta + 1.0)))

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample alpha beta = 
            gammaCheckParam alpha beta
            let x = Gamma.Sample alpha 1.0
            let y = Gamma.Sample beta 1.0
            x / (x + y)

        /// Computes the probability density function.
        static member PDF alpha beta x = 
            gammaCheckParam alpha beta
//            if x >= 0.0 && x <= 1.0 then
//                (x ** (alpha - 1.0)) * ((1.0 - x) ** (beta - 1.0)) / (Core.Beta alpha beta)
//            else 0.0
            failwith "Not implemented yet."

        /// Computes the cumulative distribution function.
        static member CDF alpha beta x =
            gammaCheckParam alpha beta
            if x < 0.0 then 0.0
            elif x > 1.0 then 1.0
            else failwith "Not implemented yet."

        /// Returns the support of the exponential distribution: [0.0, 1.0).
        static member Support alpha beta =
            gammaCheckParam alpha beta
            (0.0, 1.0)

    /// Initializes a Beta distribution        
    let beta alpha beta =
        { new Distribution<float,float> with
            member d.Mean              = Beta.Mean alpha beta
            member d.StandardDeviation = Beta.StandardDeviation alpha beta   
            member d.Variance          = Beta.Variance alpha beta
            //member d.CoVariance        = Beta.CoVariance alpha beta 
            member d.Sample ()         = Beta.Sample alpha beta
            member d.PDF x             = Beta.PDF alpha beta x           
            member d.CDF x             = Beta.CDF alpha beta x         
        }   


// ######
// Dirichlet distribution
// ######


//    // Dirichlet distribution helper functions.
//    let dirichletCheckParam (alpha: vector) =
//        let ok = Vector.fold (fun acc a -> if a < 0.0 then false else acc) true alpha
//        if (not ok) then failwith "Dirichlet distribution should be parametrized by a vector alpha > 0.0."
//    
//    /// Beta distribution
//    type Dirichlet =
//        static member Mean (alpha: vector) =
//            dirichletCheckParam alpha
//            let s = 1.0 / (Vector.sum alpha)
//            alpha * s
//        static member Covariance (alpha: vector) =
//            dirichletCheckParam alpha
//            let s = (Vector.sum alpha)
//            let n = s * s * (s + 1.0)
//            let p = Vector.length alpha
//            Matrix.init p p (fun i j ->
//                                if i = j then
//                                    alpha.[i] * (s - alpha.[i]) / n
//                                else
//                                    - alpha.[i] * alpha.[j] / n)
//        static member Sample (alpha: vector) =
//            dirichletCheckParam alpha
//            let p = Vector.length alpha
//            let gv = Vector.init p (fun i -> Gamma.Sample alpha.[i] 1.0)
//            let s = Vector.sum gv
//            Vector.init p (fun i -> gv.[i] / s)
//        static member PDF (alpha: vector) (x: vector) =
//            dirichletCheckParam alpha
//            if not (Vector.fold (fun acc a -> if a < 0.0 || a > 1.0 then false else acc) true alpha) then
//                0.0
//            else
//                let t = Vector.foldi (fun i acc a -> acc * (x.[i] ** (alpha.[i] - 1.0)) / (Core.Gamma alpha.[i])) 1.0 alpha
//                let s = (Vector.sum alpha)
//                t * (Core.Gamma s)
//        static member CDF (alpha: vector) (x: vector) =
//            dirichletCheckParam alpha
//            failwith "Not implemented yet."
//            0.0
//        static member Support (alpha: vector) =
//            dirichletCheckParam alpha
//            let p = Vector.length alpha
//            Vector.Generic.create p (0.0, 1.0)
//
//    /// Initializes a uniform distribution        
//    let uniform min max =
//        { new Distribution<float,float> with
//            member d.Mean              = Uniform.Mean min max
//            member d.StandardDeviation = Uniform.StandardDeviation min max   
//            member d.Variance          = Uniform.Variance min max
//            //member d.CoVariance        = Uniform.CoVariance min max  
//            member d.Sample ()         = Uniform.Sample min max
//            member d.PDF x             = Uniform.PDF min max x           
//            member d.CDF x             = Uniform.CDF min max x         
//        }   


// ######
// ... distribution
// ######
