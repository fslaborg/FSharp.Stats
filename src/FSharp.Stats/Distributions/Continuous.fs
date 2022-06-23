namespace FSharp.Stats.Distributions

// Source: FSharp.MathTools
open System
open FSharp.Stats
open FSharp.Stats.Ops

// Continuous probability distributions
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Continuous = 
    
    type Tails =
        | OneTailed
        | TwoTailed

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
                let gammaF = Gamma.gamma (dof/2.)
                let k = 2.**(dof/2.)
                let fraction = (1./((k)*gammaF))
                let ex1 = (x**((dof/2.)-1.))
                let ex2 = exp(-x/2.)
                let pdffunction = fraction*(ex1*ex2)
                pdffunction 

        /// Computes the logarithm of probability density function.
        static member PDFLn dof x = 
            if System.Double.IsPositiveInfinity(dof) || System.Double.IsPositiveInfinity(x) || x=0. then
                System.Double.NegativeInfinity
            else
                ((1.0 - (dof/2.0))*System.Math.Log(2.0)) + ((dof - 1.0)*System.Math.Log(x)) - (x*x/2.0) - Gamma.gammaLn(dof/2.0)

        /// Computes the cumulative distribution function.
        static member CDF dof x =
            chiSquaredCheckParam dof
            if dof = 0. then 
                if x > 0. then 1.
                else 0.
            else Gamma.lowerIncomplete (dof/2.) (x/2.)

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
    // ChiSquared distribution
    // ######


    // ChiSquared distribution helper functions.
    let chiCheckParam dof = if System.Double.IsNaN(dof)  || dof < 0. then failwith "Chi distribution should be parametrized by degrees of Freedom in [0,inf)."

    /// Chi distribution.
    type Chi =
        /// Computes the mean.
        static member Mean dof =
            chiCheckParam dof
            sqrt 2. * ((Gamma.gamma ((dof + 1.) / 2.))/(Gamma.gamma (dof / 2.)))
        /// Computes the variance.
        static member Variance dof =
            chiCheckParam dof
            let mean = sqrt 2. * ((Gamma.gamma ((dof + 1.) / 2.))/(Gamma.gamma (dof / 2.)))
            dof - pown mean 2
        /// Computes the standard deviation.
        static member StandardDeviation dof =
            chiCheckParam dof
            let mean = sqrt 2. * ((Gamma.gamma ((dof + 1.) / 2.))/(Gamma.gamma (dof / 2.)))
            let var = dof - pown mean 2
            sqrt var
        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample dof =
            chiCheckParam dof
            //rndgen.NextFloat() * (max - min) + min
            nan
        /// Computes the probability density function.
        static member PDF dof x =
            chiCheckParam dof
            if x < 0.0 || dof < 1. then
                0.0
            else
                let gammaF = Gamma.gamma (dof/2.)
                let k = 2.**(dof/2. - 1.)
                let fraction = 1./((k)*gammaF)
                let ex1 = x**(dof-1.)
                let ex2 = exp(-(x**2.)/2.)
                let pdffunction = fraction*(ex1*ex2)
                pdffunction 
        /// Computes the cumulative distribution function.
        static member CDF dof x =
            chiCheckParam dof
            if dof = 0. then 
                if x > 0. then 1.
                else 0.
            else Gamma.lowerIncomplete (dof / 2.) ((x**2.) /2.)
        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support dof =
            chiCheckParam dof
            (0., System.Double.PositiveInfinity)


    /// Initializes a Chi distribution        
    let chi dof =
        { new Distribution<float,float> with
            member d.Mean              = Chi.Mean dof
            member d.StandardDeviation = Chi.StandardDeviation dof 
            member d.Variance          = Chi.Variance dof
            member d.Sample ()         = Chi.Sample dof
            member d.PDF x             = Chi.PDF dof x           
            member d.CDF x             = Chi.CDF dof  x         
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
            // Source: fsmathtools
            uniformCheckParam min max            
            Random.rndgen.NextFloat() * (max - min) + min
            

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
// (Gaussian)- Normal distribution
// ######


    // Normal distribution helper functions.
    let normalCheckParam mu sigma = if System.Double.IsNaN(mu) || sigma < 0.0 then failwith "Normal distribution should be parametrized by sigma > 0.0."
    
    /// Normal distribution.
    type Normal =
        /// Computes the mean.
        static member Mean mu sigma =
            normalCheckParam mu sigma
            mu

        /// Computes the variance.
        static member Variance mu sigma =
            normalCheckParam mu sigma
            sigma*sigma

        /// Computes the standard deviation.
        static member StandardDeviation mu sigma =
            normalCheckParam mu sigma
            sigma

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample mu sigma =
            // Source: fsmathtools
            normalCheckParam mu sigma
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

        /// Computes the probability density function.
        static member PDF mu sigma x =
            normalCheckParam mu sigma
            (exp (-0.5 * (x-mu)*(x-mu) / (sigma*sigma))) / (sqrt (2.0 * Ops.pi * (sigma*sigma)))

        /// Computes the cumulative distribution function.
        static member CDF mu sigma x =
            normalCheckParam mu sigma            
            0.5 * (1.0 + SpecialFunctions.Errorfunction.Erf((x - mu)/(sigma*(sqrt 2.0))))

        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support mu sigma =
            normalCheckParam mu sigma
            (System.Double.NegativeInfinity, System.Double.PositiveInfinity)

        /// Initializes a Normal distribution        
        static member init mu sigma =
            { new Distribution<float,float> with
                member d.Mean              = Normal.Mean mu sigma
                member d.StandardDeviation = Normal.StandardDeviation mu sigma
                member d.Variance          = Normal.Variance mu sigma
                //member d.CoVariance        = Normal.CoVariance  mu sigma
                member d.Sample ()         = Normal.Sample mu sigma
                member d.PDF x             = Normal.PDF mu sigma x      
                member d.CDF x             = Normal.CDF mu sigma x         
            }

        /// Estimates the Normal distribution parameters from sample data with maximum-likelihood.
        static member Estimate samples =
            let s   = Seq.stats samples
            let mu  = SummaryStats.mean s
            let sigma = SummaryStats.stDev s
            
            Normal.init mu sigma

    /// Initializes a Normal distribution        
    let normal mu sigma = Normal.init mu sigma


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
            // Source: fsmathtools
            expCheckParam lambda
            let mutable r = Random.rndgen.NextFloat()
            while (r = 0.0) do
                r <- Random.rndgen.NextFloat()
            done;
            (- log r)/lambda
            

        /// Computes the probability density function.
        static member PDF lambda x = 
            expCheckParam lambda
            if x >= 0.0 then
                lambda * exp(-lambda * x)
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
            // Source: fsmathtools (same in MN)
            gammaCheckParam alpha beta
            let mutable a = alpha
            // Fix when alpha is less than one.
            let alphafix =
                if alpha < 1.0 then
                    a <- alpha + 1.0
                    (Random.rndgen.NextFloat() ** (1.0 / alpha))
                else
                    1.0
            let d = a - 1.0 / 3.0
            let c = 1.0 / sqrt(9.0 * d)
            let rec gammaSample () =
                let mutable x = Normal.Sample 0.0 1.0
                let mutable v = 1.0 + c * x
                while v <= 0.0 do
                    x <- Normal.Sample 0.0 1.0
                    v <- 1.0 + c * x
                v <- v * v * v
                let u = Random.rndgen.NextFloat()
                x <- x * x
                if u < 1.0 - 0.0331 * x * x then
                    d * v
                elif (log u) < 0.5 * x + d * (1.0 - v + (log v)) then
                    d * v
                else gammaSample()
            alphafix * gammaSample() / beta
            //failwith "Not implemented yet."
        
        /// Computes the probability density function.
        static member PDF alpha beta x = 
            gammaCheckParam alpha beta
            if x >= 0.0 then
                //(beta**alpha) * (x ** (alpha - 1.0)) * (exp (-beta*x)) / SpecialFunctions.Gamma.gamma alpha
                Math.Pow(beta, alpha) * Math.Pow(x, alpha - 1.0) * (exp (-beta * x)) / SpecialFunctions.Gamma.gamma alpha
            else 0.0
        
        /// Computes the cumulative distribution function.
        static member CDF alpha beta x =
            gammaCheckParam alpha beta
            if alpha = 0.0 && beta = 0.0 then 
                0.0
            else 
                SpecialFunctions.Gamma.lowerIncomplete alpha (x * beta)
        
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
            // Source: fsmathtools
            gammaCheckParam alpha beta
            let x = Gamma.Sample alpha 1.0
            let y = Gamma.Sample beta 1.0
            x / (x + y)

        /// Computes the probability density function.
        static member PDF alpha beta x = 
            gammaCheckParam alpha beta
            if x >= 0.0 && x <= 1.0 then
                (x ** (alpha - 1.0)) * ((1.0 - x) ** (beta - 1.0)) / (SpecialFunctions.Beta.beta alpha beta)
            else 0.0          

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
// Student's T-distribution
// ------------------------
// wiki: "http://en.wikipedia.org/wiki/Student%27s_t-distribution"
// ######


    // Student's T-distribution helper functions.
    //let studentTCheckParam mu tau dof = if System.Double.IsNaN(mu) || mu < 0.0 || tau < 0.0 || System.Double.IsNaN(dof)  || dof < 0. then failwith "Student's T-distribution should be parametrized by mu, tau and dof > 0.0."
    let studentTCheckParam mu tau dof = if System.Double.IsNaN(mu) || tau < 0.0 || System.Double.IsNaN(dof)  || dof < 0. then failwith "Student's T-distribution should be parametrized by mu, tau and dof > 0.0."
    
    /// Student's T-distribution
    type StudentT =
        /// Computes the mean.
        static member Mean mu tau dof =
            studentTCheckParam mu tau dof
            mu

        /// Computes the variance.
        static member Variance mu tau dof =
            studentTCheckParam mu tau dof
            match dof with
            | df when System.Double.IsPositiveInfinity(df) -> tau*tau
            | df when df > 2.0 -> dof*tau*tau/(dof-2.0)
            | _ -> System.Double.PositiveInfinity

        /// Computes the standard deviation.
        static member StandardDeviation mu tau dof =
            studentTCheckParam mu tau dof
            sqrt (StudentT.Variance mu tau dof)
            

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample mu tau dof =
            studentTCheckParam mu tau dof
            let gamma = Gamma.Sample (0.5*dof) 0.5
            Normal.Sample mu (tau*sqrt(dof/gamma))
            // let gamma = 1. / Gamma.Sample (0.5*dof) (0.5*dof)
            // Normal.Sample mu (tau*sqrt(gamma))

        /// Computes the probability density function.
        static member PDF mu tau dof x =
            studentTCheckParam mu tau dof
            let d = (x - mu) / tau
            exp (SpecialFunctions.Gamma.gammaLn((dof + 1.)/2.) - SpecialFunctions.Gamma.gammaLn(dof/2.)) * System.Math.Pow(1.0 + (d*d / dof), (-0.5 * (dof + 1.))) / sqrt (dof*pi) / tau

        /// Computes the cumulative distribution function.
        static member CDF mu tau dof x =
            studentTCheckParam mu tau dof            
            let k = (x - mu) / tau
            let h = dof / (dof + (k * k))
            let ib = 0.5 * SpecialFunctions.Beta.lowerIncomplete (dof/2.0) 0.5 h
            if x <= mu then ib else 1.0 - ib           

        /// Returns the support of the exponential distribution: (Negative Infinity, Positive Infinity).
        static member Support mu tau dof =
            studentTCheckParam mu tau dof
            (System.Double.NegativeInfinity, System.Double.PositiveInfinity)

    /// Initializes a Student's T-distribution        
    let studentT mu tau dof =
        { new Distribution<float,float> with
            member d.Mean              = StudentT.Mean mu tau dof
            member d.StandardDeviation = StudentT.StandardDeviation mu tau dof
            member d.Variance          = StudentT.Variance mu tau dof
            //member d.CoVariance        = StudentT.CoVariance  mu tau
            member d.Sample ()         = StudentT.Sample mu tau dof
            member d.PDF x             = StudentT.PDF mu tau dof x      
            member d.CDF x             = StudentT.CDF mu tau dof x         
        }   

    let getCriticalTValue df significanceLevel tailed =
        let cdf t = 
            let alpha =
                match tailed with
                | Tails.OneTailed -> significanceLevel
                | Tails.TwoTailed -> significanceLevel / 2.
            studentT 0. 1. df
            |> fun d -> alpha - d.CDF t
        Optimization.Bisection.tryFindRoot cdf 0.0000001 -1000. 0. 10000
        |> fun tValue -> 
            match tValue with
                | None -> failwithf "Critical t value could not be determined (increase maxIterations or decrease lower bound)."
                | Some t -> Math.Abs t

// ######
// Studentized range (q) distribution
// ------------------------------
// ######

    open FSharp.Stats.Integration

    /// Studentized range distribution helper functions.
    let studentizedRangeCheckParam q r v = 
        if  System.Double.IsNaN(q) || 
            System.Double.IsNaN(r) || 
            System.Double.IsNaN(v) ||
            r < 1.0 || 
            v < 1.0 
            then failwith "Studentized range distribution should be parametrized by r and v > 1.0."
    
    /// Studentized range (q) distribution. Used in Tukey's HSD post hoc test.
    /// method from: QUANTILES FROM THE MAXIMUM STUDENTIZED RANGE DISTRIBUTION, Ferreira, Rev. Mat. Estat., v.25, n.1, p.117-135, 2007
    /// table from: Tables of range and studentized range, Harter, 1960 and Lawal B, Applied Statistical Methods in Agriculture, Health and Life Sciences, DOI 10.1007/978-3-319-05555-8, 2014
    type StudentizedRange =
        /// Computes the mean.
        static member Mean =
            failwithf "Not implemented yet"

        /// Computes the variance.
        static member Variance =
            failwithf "Not implemented yet"

        /// Computes the standard deviation.
        static member StandardDeviation =
            failwithf "Not implemented yet"
            

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample() =
            failwithf "Not implemented yet"

        /// Computes the probability density function.
        static member PDF =
            failwithf "Not implemented yet"

        /// Computes the cumulative density function.
        /// Accuracy defines the number of steps within the integration (Recommended: 1k-10k, default: 2k). pValue accuracy is minimum 3 digits for v>3 at pValue=0.05.
        /// q:qValue r:number of treatments v:df (n-r) c:1.
        /// Integration can be performed in parallel using PSeq
        static member CDF q r v c accuracy computeParallel =
            // An alternative (not implemented) algorithm makes use of t statistic to approximate q quite accurate: 
            // An accurate, non-iterativeapproximation for studentizedrange quantiles John R. Gleason ,Computational Statistics & Data Analysis 31 (1999) 147           
            let accuracy   = defaultArg accuracy 2000
    
            studentizedRangeCheckParam q r v
            let normal = normal 0. 1.
    
            let h q r =
                let integrateInner y = 
                    let normalPDF = normal.PDF y
                    let normalCDF = (normal.CDF y - normal.CDF (y - q))**(r - 1.)
                    normalPDF * normalCDF
                if  not (Precision.almostEqualNorm (integrateInner -20.) (10.**(-20.))) || 
                    not (Precision.almostEqualNorm (integrateInner  20.) (10.**(-20.))) 
                    then printfn "Warning: Integral in q distribution H(q) does not start/end at y=0. Extend borders [-20,20]!"
                r * (NumericalIntegration.definiteIntegral(Midpoint, -20., 20., accuracy, Parallel=computeParallel) integrateInner)

    
            let f q r v c =
                let partH u = (h (q * sqrt u) r) ** c
                let gammapart = 2.**(v/2.)*SpecialFunctions.Gamma.gamma (v/2.)
                let sndQuotient u = 
                    let a = v**(v/2.)*Math.Exp((-u * v)/2.)*u**(v/2. - 1.)
                    a / gammapart
                let com u = partH u * sndQuotient u
                let check =
                    let bordercase = com 50.
                    if not (Precision.almostEqualNorm bordercase (10.**(-20.))) then 
                        printfn "Warning: Integral in q distribution F(q) does not end at y=0 but at y=%.12f. Extend border [0,50]!" bordercase
                NumericalIntegration.definiteIntegral(Midpoint, 0., 50., accuracy, Parallel=computeParallel) com

            f q r v c

            //Lawal B, Applied Statistical Methods in Agriculture, Health and Life Sciences, DOI 10.1007/978-3-319-05555-8, 2014
            //StudentizedRange.CDF 18.   2.  1. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9473 (2k accuracy) 0.9459 (1k accuracy)
            //StudentizedRange.CDF 59.6 20.  1. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9618 (2k accuracy) 0.9459 (1k accuracy)
            //StudentizedRange.CDF 6.08  2.  2. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9507 (2k accuracy) 0.9521 (1k accuracy)
            //StudentizedRange.CDF 16.8 20.  2. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9503 (2k accuracy) 0.9481 (1k accuracy)
            //StudentizedRange.CDF 4.5   2.  3. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9501 (2k accuracy) 0.9505 (1k accuracy)
            //StudentizedRange.CDF 11.2 20.  3. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9495 (2k accuracy) 0.9495 (1k accuracy)
            //StudentizedRange.CDF 3.93  2.  4. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9501 (2k accuracy) 0.9901 (1k accuracy)
            //StudentizedRange.CDF 9.23 20.  4. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9499 (2k accuracy) 0.9901 (1k accuracy)
            //StudentizedRange.CDF 3.64  2.  5. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9502 (2k accuracy)        (1k accuracy)
            //StudentizedRange.CDF 8.21 20.  5. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9500 (2k accuracy)        (1k accuracy)
            //StudentizedRange.CDF 3.46  2.  6. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9500 (2k accuracy)        (1k accuracy)
            //StudentizedRange.CDF 7.59 20.  6. 1. (Some 2000.) true //Lawal expected: 0.95   observed: 0.9501 (2k accuracy)        (1k accuracy)
    
    /// Initializes a studentized range distribution.     
    /// Accuracy defines the number of steps within the CDF integration (Recommended: 1k-10k, default: 2k). pValue accuracy is minimum 3 digits for v>3.
    /// q:qValue r:number of treatments v:df (n-r) c:1.   
    /// Integration can be performed in parallel using PSeq
    let studentizedRange r v c accuracy computeParallel =
        { new Distribution<float,float> with
            member d.Mean              = StudentizedRange.Mean
            member d.StandardDeviation = StudentizedRange.StandardDeviation
            member d.Variance          = StudentizedRange.Variance
            member d.Sample ()         = StudentizedRange.Sample()
            member d.PDF x             = StudentizedRange.PDF      
            member d.CDF q             = StudentizedRange.CDF q r v c accuracy computeParallel
        }   

    
// ######
// F-distribution or Fisher–Snedecor distribution
// ----------------------------------------------
// wiki: "https://en.wikipedia.org/wiki/F-distribution"
// ######


    // F-distribution helper functions.
    let fCheckParam dof1 dof2 = if dof1 < 0.0 || dof2 < 0.0 then failwith "F-distribution should be parametrized by dof1 and dof2 > 0.0."
    
    /// F-distribution
    type F =
        /// Computes the mean.
        static member Mean dof1 dof2 =
            fCheckParam dof1 dof2
            if dof2 <= 2. then
                nan
            else
                dof2 / (dof2 - 2.0)

        /// Computes the variance.
        static member Variance dof1 dof2 =
            fCheckParam dof1 dof2
            if dof2 <= 4. then
                nan
            else
                (2.0 * dof2 * dof2 * (dof1 + dof2 - 2.)) /
                            (dof1 * (dof2 - 2.) * (dof2 - 2.) * (dof2 - 4.))

        /// Computes the standard deviation.
        static member StandardDeviation dof1 dof2 =
            fCheckParam dof1 dof2
            sqrt (F.Variance dof1 dof2)
            

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample dof1 dof2 =
            fCheckParam dof1 dof2
            let gamma1 = Gamma.Sample (dof1 / 2.0) 2.0
            let gamma2 = Gamma.Sample (dof2 / 2.0) 2.0
            gamma1 / gamma2

        /// Computes the probability density function.
        static member PDF dof1 dof2 x =
            fCheckParam dof1 dof2
            if (x <= 0.) then
                0.
            else
                let u = Math.Pow(dof1 * x, dof1) * Math.Pow(dof2, dof2) / Math.Pow(dof1 * x + dof2, dof1 + dof2)
                let b = Beta.beta (dof1 * 0.5) (dof2 * 0.5)
                sqrt u / (x * b)

        /// Computes the cumulative distribution function.
        static member CDF dof1 dof2 x =
            fCheckParam dof1 dof2
            if (x <= 0.) then
                //1.
                0.
            else
                //equals 1 - cdf(x)
                //let u = dof2 / (dof2 + dof1 * x)
                //Beta.lowerIncomplete (dof2 * 0.5) (dof1 * 0.5) u
                //equals cdf(x)
                let u = (dof1 * x) / (dof2 + dof1 * x) 
                Beta.lowerIncomplete (dof1 * 0.5) (dof2 * 0.5) u

        // /// Computes the inverse of the cumulative distribution function.
        // static member InvCDF dof1 dof2 p =
        //     fTCheckParam dof1 dof2
        //     if (p <= 0.0 || p > 1.0) then
        //         invalidArg "P" "Input must be between zero and one"
        //     else
        //         let u = dof2 / (dof2 + dof1 * x)
        //         Beta.lowerIncomplete (dof2 * 0.5) (dof1 * 0.5) u

        /// Returns the support of the exponential distribution: (0., Positive Infinity).
        static member Support dof1 dof2 =
            fCheckParam dof1 dof2
            (0., System.Double.PositiveInfinity)

    /// Initializes a F-distribution         
    let f dof1 dof2 =
        { new Distribution<float,float> with
            member d.Mean              = F.Mean dof1 dof2
            member d.StandardDeviation = F.StandardDeviation dof1 dof2
            member d.Variance          = F.Variance dof1 dof2
            //member d.CoVariance        = F.CoVariance dof1 dof2
            member d.Sample ()         = F.Sample dof1 dof2
            member d.PDF x             = F.PDF dof1 dof2 x      
            member d.CDF x             = F.CDF dof1 dof2 x         
        }   


// ######
// Log-Normal distribution
// ######


    // Log-Normal distribution helper functions.
    let logNormalCheckParam mu tau = if System.Double.IsNaN(mu) || tau < 0.0 then failwith "Log-Normal distribution should be parametrized by tau > 0.0."
    
    /// Log-Normal distribution.
    type LogNormal =
        /// Computes the mean.
        static member Mean mu tau =
            logNormalCheckParam mu tau
            exp(mu + (tau*tau/2.0))
            

        /// Computes the variance.
        static member Variance mu tau =
            logNormalCheckParam mu tau
            tau*tau

        /// Computes the standard deviation.
        static member StandardDeviation mu tau =
            logNormalCheckParam mu tau
            let tau2 =  tau * tau
            sqrt (exp(tau2) - 1.0) * exp(mu + mu + tau2)

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample mu tau =
            // Source: fsmathtools
            logNormalCheckParam mu tau
            let mutable v1 = 2.0 * Random.rndgen.NextFloat() - 1.0
            let mutable v2 = 2.0 * Random.rndgen.NextFloat() - 1.0
            let mutable r = v1 * v1 + v2 * v2
            while (r >= 1.0 || r = 0.0) do
                v1 <- 2.0 * Random.rndgen.NextFloat() - 1.0
                v2 <- 2.0 * Random.rndgen.NextFloat() - 1.0
                r <- v1 * v1 + v2 * v2
            let fac = sqrt(-2.0*(log r)/r)
            exp (tau * v1 * fac + mu)
            //failwith "Not implemented yet."

        /// Computes the probability density function.
        static member PDF mu tau x =
            logNormalCheckParam mu tau
            let a = (log x - mu) / tau
            //exp(-0.5*a*a)/(x * tau * Ops.Sqrt2Pi)
            failwith "Not implemented yet."
            

        /// Computes the cumulative distribution function.
        static member CDF mu tau x =
            logNormalCheckParam mu tau            
            //0.5 * (1.0 + SpecialFunctions.Errorfunction.Erf((x - mu)/(tau*(sqrt 2.0))))
            failwith "Not implemented yet."

        /// Returns the support of the exponential distribution: [0, Positive Infinity).
        static member Support mu tau =
            logNormalCheckParam mu tau
            (0., System.Double.PositiveInfinity)

        static member init mu tau =
            { new Distribution<float,float> with
                member d.Mean              = LogNormal.Mean mu tau
                member d.StandardDeviation = LogNormal.StandardDeviation mu tau
                member d.Variance          = LogNormal.Variance mu tau
                //member d.CoVariance        = LogNormal.CoVariance  mu tau
                member d.Sample ()         = LogNormal.Sample mu tau
                member d.PDF x             = LogNormal.PDF mu tau x      
                member d.CDF x             = LogNormal.CDF mu tau x         
            }

        /// Estimates the log-normal distribution parameters from sample data with maximum-likelihood.
        static member Estimate samples =
            let s = 
                samples
                |> Seq.map log
                |> Seq.stats
            let mu  = SummaryStats.mean s
            let tau = SummaryStats.stDev s
            
            LogNormal.init mu tau


    /// Initializes a Normal distribution        
    let logNormal mu tau = LogNormal.init mu tau


// ######
// Multivariate normal distribution
// ######

    // multivariate normal distribution helper functions.
    let multivariateNormalCheckParam (mu:vector) (sigma:matrix) =
        if false then failwith "Multivariate normal distribution should be parametrized by "
    
    /// multivariate normal distribution.
    type MultivariateNormal =
        /// Computes the mean.
        static member Mean (mu:vector) (sigma:matrix) =
            multivariateNormalCheckParam mu sigma
            mu
        /// Computes the variance.
        static member Variance (mu:vector) (sigma:matrix) =
            multivariateNormalCheckParam mu sigma
            //sigma*sigma
            failwith "Not implemented yet."
        /// Computes the standard deviation.
        static member StandardDeviation (mu:vector) (sigma:matrix) =
            multivariateNormalCheckParam mu sigma
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
            multivariateNormalCheckParam mu sigma
            let k = Seq.length mu |> float
            let ex = Math.Exp(-0.5 * (x - mu).Transpose * (Algebra.LinearAlgebra.Inverse sigma) * (x-mu))
            (2.*Math.PI)**(-k/2.) * (Algebra.LinearAlgebra.Determinant sigma ** (-0.5)) * ex
        /// Computes the cumulative distribution function.
        static member CDF (mu:vector) (sigma:matrix) (x:vector) =
            failwith "Not implemented yet."

        /// Initializes a multivariate normal distribution with mean mu and covariance matrix sigma       
        static member init (mu:vector) (sigma:matrix) =
            { new Distribution<vector,vector> with
                member d.Mean              = MultivariateNormal.Mean mu sigma
                member d.StandardDeviation = MultivariateNormal.StandardDeviation mu sigma
                member d.Variance          = MultivariateNormal.Variance mu sigma
                member d.Sample ()         = MultivariateNormal.Sample mu sigma
                member d.PDF x             = MultivariateNormal.PDF mu sigma x      
                member d.CDF x             = MultivariateNormal.CDF mu sigma x         
            }

    /// Initializes a multivariate normal distribution with mean mu and covariance matrix sigma          
    let multivariateNormal mu sigma = MultivariateNormal.init mu sigma

// ######
// ... distribution 
// ######


