namespace FSharp.Stats.Signal 


module Outliers =
    open FSharp.Stats

    /// Tukey's fences based on interquartile range. c defines the magnitude of interquartile range that is added/subtracted to Q3 and Q1 respectively.
    /// Commonly c is 1.5 for outliers and 3 for points 'far out' (Tukey 1977).
    let tukey (k:float) (d:float []) =
        let firstQ = Quantile.compute 0.25 d
        let thirdQ = Quantile.compute 0.75 d 
        let iqr = System.Math.Abs (thirdQ - firstQ)
        Intervals.create (firstQ - k * iqr) (thirdQ + k * iqr) 
        
        
