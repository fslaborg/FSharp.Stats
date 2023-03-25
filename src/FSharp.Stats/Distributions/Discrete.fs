namespace FSharp.Stats.Distributions

open FSharp.Stats

/// Discrete probability distributions
module DiscreteDistribution =

    /// Initializes a Binomial distribution      
    let poisson lambda = 
        Discrete.Poisson.Init lambda
    
    /// Initializes a uniform distribution        
    let bernoulli p =
        Discrete.Bernoulli.Init p

    /// Initializes a Binomial distribution       
    let binomial p n = 
        Discrete.Binomial.Init p n

    /// <summary> Initializes a hypergeometric distribution.
    /// The hypergeometric distribution is a discrete probability distribution
    /// that describes the probability of `k` successes (random draws for which the object
    /// drawn has a specified feature) in `n` draws, without replacement, from a finite
    /// population of size `N` that contains exactly `K` objects with that feature,
    /// wherein each draw is either a success (`1.0`) or a failure (`0.0`).
    /// </summary>
    /// <param name="N">The population size</param>
    /// <param name="K">The number of success states in the population</param>
    /// <param name="n">The number of draws</param>
    let hypergeometric N K n =
        Discrete.Hypergeometric.Init N K n 


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

