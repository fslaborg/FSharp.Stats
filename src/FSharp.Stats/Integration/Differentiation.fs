namespace FSharp.Stats.Integration

open FSharp.Stats
open FSharpAux

///In numerical analysis, numerical differentiation describes algorithms for estimating the derivative of a mathematical function using values of the function and perhaps other knowledge about the function.
module Differentiation =

    /// Three-Point Differentiation Helper.
    /// xValues Sample Points t.
    /// yValues Sample Values x(t)
    /// idxT Index of the point of the differentiation.</param>
    /// idx0 Index of the first sample.</param>
    /// idx1 Index of the second sample.</param>
    /// idx2 Index of the third sample.</param>
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
    
        //correcture of the Array.iterUntili function; here no boundarys are set to the lower and upper end
        /// Iterates the data array beginning from the startIdx. 
        /// The step size and direction are implied by magnitude and sign of stepSize. The function returns
        /// the idx of the first value for which predicate returns true or the end/start of the collection
        /// is reached (returning None). The predicate function takes the idx of the current value as an additional
        /// parameter.
        let private iterUntiliWithoutCutOff (predicate: int -> 'T -> bool) stepSize startIdx (arr: 'T []) =
            let rec loop  (arr: 'T []) currentIdx =
                if currentIdx < 0 then None
                elif currentIdx > arr.Length-1 then None
                else                                              
                    match predicate currentIdx arr.[currentIdx] with 
                    | true -> Some currentIdx   
                    | _               -> loop arr (currentIdx+stepSize) 
            loop arr startIdx 

        ///Returns the approximation of f'(x) at x by calculating the two point differentiation.
        ///h is the window for the difference calculation. f is the function for which to calculate numerical differentiation. x is the point at which the difference between "x and x+h"/"x-h and x+h" is calculated.
        let differentiate h f x =
            //error is proportional to h
            let forwardDifference = 
                ( (f (x+h)) - (f x) ) / h     

            //error is proportional to h^2, so it decreases with this function for h<1
            let centralDifference = 
                ( (f (x+h)) - (f (x-h)) ) / (2.*h) 
            if h >= 1. then forwardDifference else centralDifference

        //if something is wrong with the following function, try to implement the function shown next: TODO: https://www.johndcook.com/NumericalODEStepSize.pdf
        //source for the function below: http://math.bd.psu.edu/faculty/stevens/Old-Courses/MTHBD423-Fall2003/Notes-Handouts/ndiff.pdf
        ///Returns the optimal size for h from all tested values in hArr. f is the function and x the point at which the numerical differentiation is calculated.
        let optimalStepSize hArr f x = 
            let doPadding arr = [|[|(Array.max arr)*2.|];arr;[|(Array.min arr)/2.|]|] |> Array.concat
            let paddedArr = doPadding hArr
            paddedArr
            |> Array.sortDescending
            |> iterUntiliWithoutCutOff (fun i tryH -> if i <> 0 && i <> (paddedArr.Length-1)
                                                            then (abs ((differentiate paddedArr.[i+1] f x) - (differentiate paddedArr.[i] f x)) >= abs ((differentiate paddedArr.[i] f x) - (differentiate paddedArr.[i-1] f x)))
                                                            else 0. = 1. //anything that gives false, iterUntili goes to the next index number if output is false.
                                       ) 1 0
            |> fun x -> if x.IsSome = true then x.Value else failwith "No value found, try choose smaller h."
            |> fun idx -> paddedArr.[idx]
        
        ///Returns the approximation of f'(x) at x by calculating the two point differentiation.
        ///Uses "optimalStepSize"-function to calculate optimal h for "differentiate" -function. 
        ///h is tested from all numbers in hArr. f is the function and x the point at which numerical differentiation is calculated.
        let differentiateTryFindH hArr f x =
            differentiate (optimalStepSize hArr f x) f x
        
        ///Returns the approximation of f'(x) at x by calculating the two point differentiation.
        ///Uses "optimalStepSize"-function to calculate optimal h for "differentiate"-function. 
        ///h is tested from h = 0.01 to 5e^-100 in [|0.01; 0.005; 0.001; 0.0005; 0.0001 ..|]-increments. f is the function and x the point at which numerical differentiation is calculated.
        let differentiateOptimalH f x =
            let small = Array.init 98 (fun i ->[|( 0.1 **((float i)+2.));(0.5*( 0.1 **((float i)+2.)))|]
                                          )|> Array.concat|> Array.sortDescending
            differentiate (optimalStepSize small f x) f x


