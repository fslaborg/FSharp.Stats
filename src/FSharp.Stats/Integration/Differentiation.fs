namespace FSharp.Stats.Integration

open FSharp.Stats
open FSharpAux

///In numerical analysis, numerical differentiation describes algorithms for estimating the derivative of a mathematical function using values of the function and perhaps other knowledge about the function.
module Differentiation =

    /// <summary>Three-Point Differentiation Helper</summary>
    /// <remarks></remarks>
    /// <param name="xValues">Sample Points t</param>
    /// <param name="yValues">Sample Values x(t)</param>
    /// <param name="idxT">Index of the point of the differentiation.</param>
    /// <param name="idx0">idx0 Index of the first sample.</param>
    /// <param name="idx1">Index of the second sample.</param>
    /// <param name="idx2">Index of the third sample.</param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let differentiateThreePoint (xValues:float []) (yValues:float []) idxT idx0 idx1 idx2 = 
        let x0 = yValues.[idx0]
        let x1 = yValues.[idx1]
        let x2 = yValues.[idx2]

        let t = xValues.[idxT]- xValues.[idx0]
        let t1 = xValues.[idx1]- xValues.[idx0]
        let t2 = xValues.[idx2]- xValues.[idx0]

        let a  = (x2 - x0 - (t2/t1*(x1 - x0)))/(t2*t2 - t1*t2)
        let b  = (x1 - x0 - a*t1*t1)/t1
        (2.*a*t) + b

    ///A two-point estimation is to compute the slope of a nearby secant line through two points.
    ///This gives an approximations of f'(x) at x respectively to two points "x and x+h"/"x-h and x+h"(depending on the used algorithm) of the function f.
    ///Choosing a small number h, h represents a small change in x, and it can be either positive or negative.
    module TwoPointDifferentiation =
    
  
        /// <summary>Iterates the data array beginning from the startIdx. The step size and direction are implied by magnitude and sign of stepSize.</summary>
        /// <remarks></remarks>
        /// <param name="predicate">takes the idx of the current value as an additional</param>
        /// <param name="stepSize"></param>
        /// <param name="startIdx"></param>
        /// <param name="arr"></param>
        /// <returns>the idx of the first value for which predicate returns true or the end/start of the collection is reached (returning None).</returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let private iterUntili (predicate: int -> 'T -> bool) stepSize startIdx (arr: 'T []) =
            let rec loop  (arr: 'T []) currentIdx =
                if currentIdx < 0 then None
                elif currentIdx > arr.Length-1 then None
                else                                              
                    match predicate currentIdx arr.[currentIdx] with 
                    | true -> Some currentIdx   
                    | _               -> loop arr (currentIdx+stepSize) 
            loop arr startIdx 

        /// <summary>Returns the approximation of f'(x) at x by calculating the two point differentiation.</summary>
        /// <remarks></remarks>
        /// <param name="h">window for the difference calculation</param>
        /// <param name="f">f is the function for which to calculate numerical differentiation.</param>
        /// <param name="x">x is the point at which the difference between "x and x+h"/"x-h and x+h" is calculated.</param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let differentiate h f x =
            //error is proportional to h
            let forwardDifference = 
                ( (f (x+h)) - (f x) ) / h     

            //error is proportional to h^2, so it decreases with this function for h<1
            let centralDifference = 
                ( (f (x+h)) - (f (x-h)) ) / (2.*h) 
            if h >= 1. then forwardDifference else centralDifference

        /// <summary>Returns the optimal size for h from all tested values in hArr. f is the function and x the point at which the numerical differentiation is calculated.</summary>
        /// <remarks>if something is wrong with the following function, try to implement the function shown next: TODO: https://www.johndcook.com/NumericalODEStepSize.pdf <br /> source for the function below: http://math.bd.psu.edu/faculty/stevens/Old-Courses/MTHBD423-Fall2003/Notes-Handouts/ndiff.pdf</remarks>
        /// <param name="hArr"></param>
        /// <param name="f"></param>
        /// <param name="x"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let private optimalStepSize hArr f x = 
            let doPadding arr = [|[|(Array.max arr)*2.|];arr;[|(Array.min arr)/2.|]|] |> Array.concat
            let paddedArr = doPadding hArr
            paddedArr
            |> Array.sortDescending
            |> iterUntili (fun i tryH ->  
                                i <> 0 && 
                                i <> (paddedArr.Length-1) && 
                                abs ((differentiate paddedArr.[i+1] f x) - (differentiate paddedArr.[i] f x)) >= abs ((differentiate paddedArr.[i] f x) - (differentiate paddedArr.[i-1] f x))
                          ) 1 0
            |> fun x -> if x.IsSome = true then x.Value else failwith "No value found, try choose smaller h."
            |> fun idx -> paddedArr.[idx]
        
        /// <summary>Finds optimal h from all values given in hArr and calculates "differentiate" -function.</summary>
        /// <remarks></remarks>
        /// <param name="hArr"></param>
        /// <param name="f">function for which numerical differentiation is calculated.</param>
        /// <param name="x">x is the point at which numerical differentiation is calculated.</param>
        /// <returns>Returns the approximation of f'(x) at x by calculating the two point differentiation.</returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let differentiateOptimalHBy hArr f x =
            differentiate (optimalStepSize hArr f x) f x
        
        let differentiateOptimalH f x =
            let small = [|for i in 0 .. 97 do yield 0.1**((float i)+2.);yield (0.1**((float i)+2.)/2.) |]
            differentiate (optimalStepSize small f x) f x


