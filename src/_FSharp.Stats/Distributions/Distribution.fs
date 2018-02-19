namespace FSharp.Stats.Distributions


/// Interface which every probability distribution must implement.
type Distribution<'a, 'b> =
    abstract Mean : 'a
    abstract StandardDeviation : 'a
    abstract Variance : 'a
    //abstract CoVariance : 'b
    abstract Sample : unit -> 'b
    abstract PDF :  'b -> float
    abstract CDF :  'a -> float
    //abstract PDFLn : 'a -> float
    //abstract CDFLn : 'a -> float

