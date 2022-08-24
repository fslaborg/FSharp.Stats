namespace FSharp.Stats.Distributions


/// Interface which every probability distribution must implement.
type Distribution<'a> =
    abstract Mean              : 'a
    abstract StandardDeviation : 'a
    abstract Variance          : 'a
    abstract CDF               :  'a -> float
    //abstract PDFLn   : 'a -> float
    //abstract CDFLn   : 'a -> float
    abstract ToString          : unit -> string 
    


/// Interface for continuous probability distributions.
type ContinuousDistribution<'a,'b> =    
    inherit Distribution<'a> 
    abstract Mode   :   'b
    abstract PDF    :   'b -> float
    abstract Sample : unit -> 'b
    

/// Interface for discrete probability distributions.
type DiscreteDistribution<'a,'b> =    
    inherit Distribution<'a>
    abstract Mode   :   'b
    abstract PMF    :   'b -> float
    abstract Sample : unit -> 'b

