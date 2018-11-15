namespace FSharp.Stats

/// Uniform random number generators
module Random = 
    open System.Threading
    open System

    /// Interface that every uniform random number generator must implement.
    type IRandom = interface
        abstract NextInt : unit -> int
        abstract NextInt : int -> int
        abstract NextFloat : unit -> float
    end

    ///// A standard implementation of a uniform random source using System.Random()
    type RandThreadSafe =
        val mutable rnd : ThreadLocal<Random>
        /// Constructs the default random number generator with seed 17.
        new() = { rnd = new ThreadLocal<Random>(fun () -> new Random(17)) }
        /// If n is negative, the random number generator seed is based on system time, if it is zero or positive it will
        /// use n as the seed.
        new(n) as this = { rnd = new ThreadLocal<Random>(fun () -> new Random()) }
                         then
                            if n >= 0 then this.rnd <- new ThreadLocal<Random>(fun () -> new Random(n))
        interface IRandom with
            member x.NextInt() = x.rnd.Value.Next()
            member x.NextInt maxValue = x.rnd.Value.Next(maxValue)
            member x.NextFloat() =x.rnd.Value.NextDouble()
        end

    type RandBasic =
        val mutable rnd : Random
        /// Constructs the default random number generator with seed 17.
        new() = { rnd = new Random(17) }
        /// If n is negative, the random number generator seed is based on system time, if it is zero or positive it will
        /// use n as the seed.
        new(n) as this = { rnd = new Random() }
                         then
                            if n >= 0 then this.rnd <- new Random(n)
        interface IRandom with
            member x.NextInt() = x.rnd.Next()
            member x.NextInt maxValue = x.rnd.Next(maxValue)
            member x.NextFloat() =x.rnd.NextDouble()
        end

    /// The uniform random source used for sampling functions.
    let mutable rndgen = new RandThreadSafe() :> IRandom
    /// Sets the random number generator used for sampling.
    let SetSampleGenerator rg = rndgen <- rg
    /// Returns the random number generator used for sampling.
    let GetSampleGenerator () = rndgen