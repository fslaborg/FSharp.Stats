namespace FSharp.Stats.SpecialFunctions

open System

/// Logistic (Sigmoid) Functions
module Logistic =
    
    /// Computes the standard logistic function value for x
    let standard x = 1.0 / (1.0 + exp -x)

    /// Computes the logistic function value for x where 
    ///
    /// x0 is the x-Value of the sigmoid midpoint,
    ///
    ///L is the curves maximum value and
    ///
    ///k is the steepness of the curve
    let generic x0 L k x = L / (1.0 + exp (-k * (x - x0)))
