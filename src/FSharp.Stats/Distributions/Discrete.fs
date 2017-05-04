namespace FSharp.Stats.Distributions

/// Discrete probability distributions
module Discrete =

// ######
// Bernoulli distribution
// ######


    // Bernoulli distribution helper functions.
    let bernCheckParam p = if p < 0.0 || p > 1.0 then failwith "Bernoulli distribution should be parametrized by p in [0.0, 1.0]."
    
    /// Bernoulli distribution.
    type Bernoulli =
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

        /// Computes the probability density function.
        static member PDF p x =
            bernCheckParam p
            match x with
            | 0.0 -> p
            | 1.0 -> 1.0 - p
            | _ -> 0.0

        /// Computes the cumulative distribution function.
        static member CDF p x =
            bernCheckParam p
            if x < 0.0 then 0.0
            elif x < 1.0 then p
            else 1.0

        /// Returns the support of the exponential distribution: [0, Positive Infinity).
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
// ... distribution
// ######
