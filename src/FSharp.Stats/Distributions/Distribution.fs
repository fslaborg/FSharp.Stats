namespace FSharp.Stats.Distributions


/// Interface which every probability distribution must implement.
type Distribution<'a, 'b> =
    abstract Mean : 'a
    abstract StandardDeviation : 'a
    abstract Variance : 'a
    //abstract CoVariance : 'b
    abstract Sample : unit -> 'a
    abstract PDF : 'a -> float
    abstract CDF : 'a -> float


