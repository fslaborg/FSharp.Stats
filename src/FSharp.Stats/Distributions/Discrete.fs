namespace FSharp.Stats.Distributions

open FSharp.Stats

/// Discrete probability distributions
module Discrete =

// ######
// Bernoulli distribution
// ######


    // Bernoulli distribution helper functions.
    let bernCheckParam p = if p < 0.0 || p > 1.0 then failwith "Bernoulli distribution should be parametrized by p in [0.0, 1.0]."
    
    /// Bernoulli distribution.
    type Bernoulli =

        // https://planetcalc.com/486/
        // > Mean, or expected value of a binomial distribution is equal to "np"(n=1 in bernoulli distribution),
        // > and the variance is equal to "np(1-p)"

        /// Computes the mean.
        static member Mean p =
            bernCheckParam p
            p

        /// Computes the variance.
        static member Variance p =
            bernCheckParam p
            p * (1.0 - p)

        /// Computes the standard deviation.
        static member StandardDeviation p =
            bernCheckParam p
            sqrt (p * (1.0 - p))

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample p = 
            bernCheckParam p
//            if rndgen.NextFloat() < p then 0.0 else 1.0
            failwith "Not implemented yet."

        // Rename PMF? https://en.wikipedia.org/wiki/Probability_mass_function
        // > A probability mass function differs from a probability density function (PDF) in that the latter is associated with continuous 
        // > rather than discrete random variables. A PDF must be integrated over an interval to yield a probability.

        /// Computes the probability density function.
        static member PDF p x =
            bernCheckParam p
            match x with
            | 0.0 -> 1.0 - p
            | 1.0 -> p
            | _ -> 0.0

        /// Computes the cumulative distribution function. P(X>=k)
        static member CDF p x =
            bernCheckParam p
            // Summary: This cdf calculates the probability, that value x is greater or equal to a random value (R) taken from the bernoulli distribution.
            // Reminder: A bernoulli distribution can only return 0 or 1 as result.
            //// If the value x is greater than 1.0, then the probability that x is greater than the random outcome (R) is 1.0, since R∈{0,1}.
            if x >= 1.0 then 1.0
            //// Example: p = 0.8. 80% of the time R=1 and 20% of the time R=0. The probability that x in the range of 0.0 ... 0.99 is greater than R is 20%. Therefore 1-p=q.
            elif x >= 0.0 then 1.0 - p
            // If the value x is less than 0, the probability that x is greater than the random outcome (R) of p is 0 since, R∈{0,1}.
            else 0.0

        /// Returns the support of the bernoulli distribution: {0, 1}.
        static member Support p =
            bernCheckParam p
            [0.0; 1.0]


    /// Initializes a uniform distribution        
    let bernoulli p =
        { new Distribution<float,float> with
            member d.Mean              = Bernoulli.Mean p
            member d.StandardDeviation = Bernoulli.StandardDeviation p 
            member d.Variance          = Bernoulli.Variance p
            //member d.CoVariance        = Uniform.CoVariance min max  
            member d.Sample ()         = Bernoulli.Sample p
            member d.PDF x             = Bernoulli.PDF p x           
            member d.CDF x             = Bernoulli.CDF p x         
        }   


//// ######
//// Multinomial distribution
//// ######
//
//    /// Computes the multinomial coefficient.
//    let Multinomial (n: int) (ni: int array) = floor (0.5 + exp ((FactorialLn n) - (Array.fold_left (fun acc a -> acc + (FactorialLn a)) 0.0 ni)))
//
//    // Multinomial distribution helper functions.
//    let multiCheckParam (p: vector) =
//        if not (Vector.fold (fun acc f -> acc && (f > 0.0)) true p) then
//            failwith "Multinomial distribution should be parametrized with p_i in [0.0, 1.0] and all entries summing to one."
//    
//    /// Multinomial distribution.
//    type Multinomial =
//        /// Computes the mean.
//        static member Mean (p:vector) (n:int) =
//            multiCheckParam p
//            (float n) $* p
//        /// Computes the variance.
//        static member Variance (p:vector) (n:int)  =
//            multiCheckParam p
//            (float n) $* (Vector.cptMul p (Vector.map (fun x -> 1.0 - x) p))
//        /// Computes the standard deviation.
//        static member StandardDeviation (p:vector) (n:int) =
//            multiCheckParam p
//            Vector.map (fun x -> sqrt x) ((float n) $* (Vector.cptMul p (Vector.map (fun x -> 1.0 - x) p)))
//        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
//        static member Sample (p:vector) (n:int) =
//            multiCheckParam p
//            let x = Vector.Generic.create (p.Length) 0
//            let cs = Vector.of_array (Core.CumulativeSum (Vector.to_array p))
//            for i=0 to n-1 do
//                let r = rndgen.NextFloat()
//                let t = int (Vector.sum (Vector.map (fun x -> if x < r then 1.0 else 0.0) cs))
//                printf "%A: %d\n" cs t
//                x.[t] <- x.[t] + 1
//            x
//        /// Computes the probability density function.
//        static member PDF (p:vector) (n:int) (x:Vector<int>) =
//            multiCheckParam p
//            (Core.Multinomial n (Vector.Generic.to_array x)) * (Vector.foldi ( fun i pi acc -> acc * (pi ** (float x.[i])) ) 1.0 p)
//
//
//    /// Initializes a uniform distribution        
//    let multinomial p =
//        { new Distribution<float,float> with
//            member d.Mean              = Multinomial.Mean p
//            member d.StandardDeviation = Multinomial.StandardDeviation p 
//            member d.Variance          = Multinomial.Variance p
//            //member d.CoVariance        = Uniform.CoVariance min max  
//            member d.Sample ()         = Multinomial.Sample p
//            member d.PDF x             = Multinomial.PDF p x           
//            member d.CDF x             = Multinomial.CDF p x         
//        }

   
// ######
// Hypergeometric distribution
// ----------------------------------------------
// wiki: "http://en.wikipedia.org/wiki/Hypergeometric_distribution"
// ######

    // In probability theory and statistics, the hypergeometric distribution is a discrete probability distribution 
    // that describes the probability of `k` successes (random draws for which the object 
    // drawn has a specified feature) in `n` draws, without replacement, from a finite 
    // population of size `N` that contains exactly `K` objects with that feature, 
    // wherein each draw is either a success or a failure. In contrast, the binomial distribution 
    // describes the probability of `k` successes in `n` draws with replacement.

    // N is the population size,
    // K is the number of success states in the population,
    // n is the number of draws,
    // k is the number of observed successes

    // N ∈ {0,1,2,...}
    // K ∈ {0,1,2,...,N}
    // n ∈ {0,1,2,...,N}
    // Hypergeometric distribution helper functions.
    let hypergeoCheckParam N K n = if N <= 0 || K <= 0 || n <= 0 || K > N || n > N then failwith "Hypergeometric distribution should be parametrized by N, K and n > 0.0. Further K and n must be <= N"
    
    // k ∈ {max(0,n+K-N),...,min(n,K)}
    let hypergeoCheckParam_k N K n k =
        if k < 0 then failwith "k must be non negative integer number."
        if k > N then failwith "k cannot exceed N."
        if k > K then failwith "k cannot exceed K."
        if k > n then failwith "k cannot exceed n."

    /// Hypergeometric distribution
    type Hypergeometric =
        /// Computes the mean.
        static member Mean N K n =
            hypergeoCheckParam N K n

            float (K * n) / float N

        /// Computes the variance.
        static member Variance N K n =
            hypergeoCheckParam N K n
            float (n * K * (N - n) * (N - K)) / float ((N * N * (N - 1)))

        /// Computes the standard deviation.
        static member StandardDeviation N K n =
            hypergeoCheckParam N K n
            sqrt (Hypergeometric.Variance N K n)
            

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        /// No parameter checking!
        static member internal SampleUnchecked N K n =            
            let rec loop N K n x =
                if 0 = n then
                    x
                else    
                    let p = float K / float N
                    let r = Random.rndgen.NextFloat()
                    if r < p then 
                        loop (N-1) (K-1) (n-1) (x+1)
                    else
                        loop (N-1) (K) (n-1) (x)
            
            loop N K n 0
            

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()) and returns the number of success states `k`.
        static member Sample N K n =
            hypergeoCheckParam N K n
            Hypergeometric.SampleUnchecked N K n


        // Rename PMF? https://en.wikipedia.org/wiki/Probability_mass_function
        // > A probability mass function differs from a probability density function (PDF) in that the latter is associated with continuous 
        // > rather than discrete random variables. A PDF must be integrated over an interval to yield a probability.

        /// Computes the probability density function at k for P(X = k).
        static member PDF N K n k =
            hypergeoCheckParam N K n
            hypergeoCheckParam_k N K n k
            //(SpecialFunctions.Binomial.coeffcient K k) * (SpecialFunctions.Binomial.coeffcient (N-K) (n-k)) / (SpecialFunctions.Binomial.coeffcient N n)
            if (N-K)<(n-k) then 0. 
            else
                exp ((SpecialFunctions.Binomial._coeffcientLn K k) + (SpecialFunctions.Binomial._coeffcientLn (N-K) (n-k)) - SpecialFunctions.Binomial._coeffcientLn N n)
        
        /// Computes the cumulative distribution function at x, i.e. P(X <= x).
        static member CDF N K n (k:int) =
            hypergeoCheckParam N K n
            hypergeoCheckParam_k N K n k
            if (k < (max 0 (n + K - N))) then 
                0.0
            elif (k >= (min K n)) then
                1.0
            elif N-K < n then
                1.0
            else
                let d = SpecialFunctions.Binomial.coeffcientLn N n
                let rec loop i acc =
                    if i <= k then
                        let tmp = exp ((SpecialFunctions.Binomial._coeffcientLn K i) + (SpecialFunctions.Binomial._coeffcientLn (N-K) (n-i)) - d)
                        loop (i+1) (acc+tmp)
                    else
                        acc
                loop 0 0.0

        // /// Computes the inverse of the cumulative distribution function.
        // static member InvCDF dof1 dof2 p =
        //     fTCheckParam dof1 dof2
        //     if (p <= 0.0 || p > 1.0) then
        //         invalidArg "P" "Input must be between zero and one"
        //     else
        //         let u = dof2 / (dof2 + dof1 * x)
        //         Beta.lowerIncomplete (dof2 * 0.5) (dof1 * 0.5) u

        /// Returns the support of the hypergeometric distribution: (0., Positive Infinity).
        static member Support N K n =
            hypergeoCheckParam N K n
            (0., System.Double.PositiveInfinity)

    /// <summary> Initializes a hypergeometric distribution.
    /// 
    /// The hypergeometric distribution is a discrete probability distribution
    /// that describes the probability of `k` successes (random draws for which the object
    /// drawn has a specified feature) in `n` draws, without replacement, from a finite
    /// population of size `N` that contains exactly `K` objects with that feature,
    /// wherein each draw is either a success (`1.0`) or a failure (`0.0`).</summary>
    /// <param name="N">The population size</param>
    /// <param name="K">The number of success states in the population</param>
    /// <param name="n">The number of draws</param>
    let hypergeometric N K n =
        { new Distribution<float,int> with
            member d.Mean               = Hypergeometric.Mean N K n
            member d.StandardDeviation  = Hypergeometric.StandardDeviation N K n
            member d.Variance           = Hypergeometric.Variance N K n
            //member d.CoVariance        = Hypergeometric.CoVariance N K n
            member d.Sample ()          = Hypergeometric.Sample N K n
            member d.PDF k              = Hypergeometric.PDF N K n k
            /// Computes the cumulative distribution function at k for P(X <= k).
            member d.CDF k              = Hypergeometric.CDF N K n (floor k |> int)         
        }



// ######
// Discrete Univariate Binomial distribution
// ----------------------------------------------
// wiki: "href="http://en.wikipedia.org/wiki/Binomial_distribution"
// ######


    
    // (p) is the success probability in each trial.    
    // (n) is the number of trails,
    //k is the number of observed successe



    // Binomial distribution helper functions.
    let binomialCheckParam p n = if n < 0 || p < 0. || p > 1. then failwith "Binomial distribution should be parametrized by n > 0.0 and 0 ≤ p ≤ 1."
    
    ///Binomial distribution
    type Binomial =
        /// Computes the mean.
        static member Mean p n =
            binomialCheckParam p n
            (float n) * p

        /// Computes the variance.
        static member Variance p n =
            binomialCheckParam p n
            p * (1.0 - p) * float n

        /// Computes the standard deviation.
        static member StandardDeviation p n =
            binomialCheckParam p n
            sqrt (Binomial.Variance p n)
            

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        /// No parameter checking!
        static member internal SampleUnchecked p n =          
            let rec loop n k =
                if n = 0 then k
                else 
                    let k' = if Random.rndgen.NextFloat() < p then k + 1 else k
                    loop (n-1) k'
            loop n 0
            //let rec loop p n k =
            //    if k < n then
            //        let k' = if Random.rndgen.NextFloat() < p then k + 1 else k
            //        loop p n k'
            //    else    
            //        k                                        
            
            //loop p n 0
            

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample p n =
            binomialCheckParam p n
            Binomial.SampleUnchecked p n


        /// Computes the probability density function at k, i.e. P(K = k)
        static member PDF p n k =
            binomialCheckParam  p n
            if k < 0 || k > n then
                0.0
            elif p = 0. then
                if k = 0 then 1. else 0.
            else
                exp ( (SpecialFunctions.Binomial._coeffcientLn n k) + (float k * log p + ( float (n - k)*log(1.-p) )) )


        /// Computes the cumulative distribution function at x, i.e. P(X <= x).
        static member CDF p n (x:float) =
            binomialCheckParam p n            
            if (x < 0.) then 
                0.0
            elif (x > float n) then
                1.0
            else
                let k = floor x |> int 
                SpecialFunctions.Beta.lowerIncomplete (float (n-k)) (float (k + 1)) (1. - p)




        /// Returns the support of the Binomial distribution: (0., n).
        static member Support p n =
            binomialCheckParam p n
            (0., float n)

    /// Initializes a Binomial distribution       
    let binomial p n =
        { new Distribution<float,int> with
            member d.Mean              = Binomial.Mean p n
            member d.StandardDeviation = Binomial.StandardDeviation p n
            member d.Variance          = Binomial.Variance p n
            //member d.CoVariance        = Binomial.CoVariance p n
            member d.Sample ()         = Binomial.Sample p n
            member d.PDF k             = Binomial.PDF p n k    
            member d.CDF x             = Binomial.CDF p n x         
        }   



// ######
// Poisson-Distribution
// ----------------------------------------------
// wiki: "href="https://en.wikipedia.org/wiki/Poisson_distribution"
// ######


    
    // (lambda) is the expected number of occurrences.    


    // Binomial distribution helper functions.
    let poissonCheckParam lambda = if lambda <= 0. then failwith "Binomial distribution should be parametrized by lambda > 0."
    
    ///Binomial distribution
    type Poisson =
        /// Computes the mean.
        static member Mean lambda =
            poissonCheckParam lambda
            failwith "Not implemented yet."

        /// Computes the variance.
        static member Variance lambda =
            poissonCheckParam lambda
            failwith "Not implemented yet."

        /// Computes the standard deviation.
        static member StandardDeviation lambda =
            poissonCheckParam lambda
            failwith "Not implemented yet."
            
        ///// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        ///// No parameter checking!
        //static member internal SampleUnchecked p n =            
        //    let rec loop p n k =
        //        if k < n then
        //            let k' = if Random.rndgen.NextFloat() < p then k + 1 else k
        //            loop p n k'
        //        else    
        //            k                                        
            
        //    loop p n 0

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        /// No parameter checking!
        static member internal SampleUnchecked lambda =            
            poissonCheckParam lambda
            failwith "Not implemented yet."
            

        /// Produces a random sample using the current random number generator (from GetSampleGenerator()).
        static member Sample lambda =
            poissonCheckParam lambda
            failwith "Not implemented yet."

        /// Computes the probability density function at k, i.e. P(K = k)
        static member PDF lambda k =
            if k > 170 then 
                System.Math.E ** (System.Math.Log lambda * float k - SpecialFunctions.Factorial._factorialLn k) * System.Math.E**(-lambda)
            else
                (lambda**float k * System.Math.E**(-lambda)) / SpecialFunctions.Factorial.factorial k


        /// Computes the cumulative distribution function at x, i.e. P(X <= x).
        static member CDF lambda =
            poissonCheckParam lambda
            failwith "Not implemented yet."




        /// Returns the support of the Binomial distribution: (0., n).
        static member Support lambda =
            poissonCheckParam lambda
            failwith "Not implemented yet."

    /// Initializes a Binomial distribution       
    let poisson lambda =
        { new Distribution<float,int> with
            member d.Mean              = Poisson.Mean lambda
            member d.StandardDeviation = Poisson.StandardDeviation lambda
            member d.Variance          = Poisson.Variance lambda
            //member d.CoVariance        = Binomial.CoVariance p n
            member d.Sample ()         = Poisson.Sample lambda
            member d.PDF k             = Poisson.PDF lambda k
            member d.CDF x             = Poisson.CDF lambda
        }

// ######
// ... distribution
// ######

