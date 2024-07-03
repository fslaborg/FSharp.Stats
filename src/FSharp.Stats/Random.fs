namespace FSharp.Stats

/// <summary>Uniform random number generators</summary>
module Random = 
    open System.Threading
    open System

    /// <summary>Interface that every uniform random number generator must implement.</summary>
    type IRandom = interface
        /// <summary>Returns a non-negative random integer.</summary>
        /// <returns>A 32-bit signed integer that is greater than or equal to 0 and less than <see cref="System.Int32.MaxValue"/>.</returns>
        abstract NextInt : unit -> int
        /// <summary>Returns a non-negative random integer that is less than the specified maximum.</summary>
        /// <param name="maxValue">The exclusive upper bound of the random number to be generated. <paramref name="maxValue"/> must be greater than or equal to 0.</param>
        /// <returns>A 32-bit signed integer that is greater than or equal to 0, and less than <paramref name="maxValue"/>; that is, the range of return values ordinarily includes 0 but not <paramref name="maxValue"/>. However, if <paramref name="maxValue"/> equals 0, <paramref name="maxValue"/> is returned.</returns>
        abstract NextInt : maxValue:int -> int
        /// <summary>Returns a random floating-point number that is greater than or equal to 0.0, and less than 1.0.</summary>
        /// <returns>A double-precision floating point number that is greater than or equal to 0.0, and less than 1.0.</returns>
        abstract NextFloat : unit -> float
    end

    /// <summary>A standard implementation of a uniform random source using <see cref="System.Random"/> that is thread-safe.</summary>
    type RandThreadSafe =
        val mutable rnd : ThreadLocal<Random>
        /// <summary>Constructs the default random number generator using a time dependent default seed value.</summary>
        new() = { rnd = new ThreadLocal<Random>(fun () -> Random()) }
        /// <summary>Constructs a random number generator with the specified seed value.</summary>
        /// <param name="n">If n is negative, the random number generator seed is based on system time, if it is zero or positive it will
        /// use n as the seed.</param>
        new(n) as this = { rnd = new ThreadLocal<Random>(fun () -> Random()) }
                         then
                            if n >= 0 then this.rnd <- new ThreadLocal<Random>(fun () -> Random(n))
        interface IRandom with
            member x.NextInt() = x.rnd.Value.Next()
            member x.NextInt maxValue = x.rnd.Value.Next(maxValue)
            member x.NextFloat() =x.rnd.Value.NextDouble()

    /// <summary>A basic implementation of a uniform random source using <see cref="System.Random"/> that is not thread-safe.</summary>
    type RandBasic =
        val mutable rnd : Random
        /// <summary>Constructs the default random number generator using a time dependent default seed value.</summary>
        new() = { rnd = Random() }
        /// <summary>Constructs a random number generator with the specified seed value.</summary>
        /// <param name="n">If n is negative, the random number generator seed is based on system time, if it is zero or positive it will
        /// use n as the seed.</param>
        new(n) as this = { rnd = Random() }
                         then
                            if n >= 0 then this.rnd <- new Random(n)
        interface IRandom with
            member x.NextInt() = x.rnd.Next()
            member x.NextInt maxValue = x.rnd.Next(maxValue)
            member x.NextFloat() =x.rnd.NextDouble()

        
    /// <summary>The uniform random source used for sampling functions.</summary>
    /// <remarks>Defaults to an instance of <see cref="RandThreadSafe"/>.</remarks>
    let mutable rndgen = RandThreadSafe() :> IRandom

    /// <summary>Sets the random number generator used for sampling.</summary>
    /// <param name="rg">The random number generator to use for sampling.</param>
    let SetSampleGenerator rg = rndgen <- rg
    
    /// <summary>Returns the random number generator used for sampling.</summary>
    /// <returns>The current random number generator used for sampling.</returns>
    let GetSampleGenerator () = rndgen

    /// <summary>Generates a pair of independent, standard, normally distributed random numbers using the Box-Muller transform.</summary>
    /// <returns>A tuple of two independent, standard, normally distributed random numbers.</returns>
    /// <remarks>
    /// This method uses the current random number generator to generate two uniform random numbers, and then applies the 
    /// Box-Muller transform to convert them into a pair of independent, standard, normally distributed random numbers.
    /// </remarks>
    let boxMullerTransform() =
        let (u1,u2) = rndgen.NextFloat(),rndgen.NextFloat()
        let z0 = sqrt(-2. * log u1) * cos (2. * Ops.pi * u2)
        let z1 = sqrt(-2. * log u1) * sin (2. * Ops.pi * u2)
        z0,z1
        