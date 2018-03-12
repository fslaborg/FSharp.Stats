namespace FSharp.Stats

/// Uniform random number generators
module Random = 

    /// Interface that every uniform random number generator must implement.
    type IRandom = interface
        abstract NextInt : unit -> int
        abstract NextFloat : unit -> float
    end

    /// A standard implementation of a uniform random source using System.Random()
    type RandBasic =
        val mutable rnd : System.Random
        /// Constructs the default random number generator with seed 17.
        new() = { rnd = new System.Random(17) }
        /// If n is negative, the random number generator seed is based on system time, if it is zero or positive it will
        /// use n as the seed.
        new(n) as this = { rnd = new System.Random() }
                         then
                            if n >= 0 then this.rnd <- new System.Random(n)
        interface IRandom with
            member x.NextInt() = x.rnd.Next()
            member x.NextFloat() =x.rnd.NextDouble()
        end



    /// The uniform random source used for sampling functions.
    let mutable rndgen = new RandBasic() :> IRandom
    /// Sets the random number generator used for sampling.
    let SetSampleGenerator rg = rndgen <- rg
    /// Returns the random number generator used for sampling.
    let GetSampleGenerator () = rndgen