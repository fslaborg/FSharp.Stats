namespace FSharp.Stats.SpecialFunctions

open System

/// Logistic (Sigmoid) Functions
module Logistic =
    
    /// <summary>Computes the standard logistic function value for x<br />where L=1, k=1, x0=0. It is sometimes simply called the sigmoid</summary>
    /// <remarks></remarks>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let standard x = 1.0 / (1.0 + exp -x)

    /// Computes the logistic function value for x where 
    ///
    /// x0 is the x-Value of the sigmoid midpoint,
    ///
    ///L is the curves maximum value and
    ///
    /// <summary>k is the steepness of the curve</summary>
    /// <remarks></remarks>
    /// <param name="x0"></param>
    /// <param name="L"></param>
    /// <param name="k"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let generic x0 L k x = L / (1.0 + exp (-k * (x - x0)))
