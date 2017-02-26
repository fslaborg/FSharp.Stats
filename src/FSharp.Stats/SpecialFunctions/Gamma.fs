namespace FSharp.Stats

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
/// Special mathematical functions
module SpecialFunctions =
    
    open System
    
    /// Computes the gamma function using the Lanczos Coefficients described in Numerical Recipes (Press et al) 
    let gamma z = 
        let lanczosCoefficients = [76.18009172947146;-86.50532032941677;24.01409824083091;-1.231739572450155;0.1208650973866179e-2;-0.5395239384953e-5]
        let rec sumCoefficients acc i coefficients =
            match coefficients with
            | []   -> acc
            | h::t -> sumCoefficients (acc + (h/i)) (i+1.0) t
        let gamma = 5.0
        let x = z - 1.0
        Math.Pow(x + gamma + 0.5, x + 0.5) * Math.Exp( -(x + gamma + 0.5) ) * Math.Sqrt( 2.0 * Math.PI ) * sumCoefficients 1.000000000190015 (x + 1.0) lanczosCoefficients


    /// Computes the log gamma function using the Lanczos Coefficients described in Numerical Recipes (Press et al)
    let gammaln z = 
        let lanczosCoefficients = [76.18009172947146;-86.50532032941677;24.01409824083091;-1.231739572450155;0.1208650973866179e-2;-0.5395239384953e-5]
        let rec sumCoefficients acc i coefficients =
            match coefficients with
            | []   -> acc
            | h::t -> sumCoefficients (acc + (h/i)) (i+1.0) t
        let gamma = 5.0
        let x = z - 1.0
        let tmp = x + gamma + 0.5 
        -(tmp - ((x + 0.5) * log(tmp))) + log(Math.Sqrt( 2.0 * Math.PI ) * sumCoefficients 1.000000000190015 (x + 1.0) lanczosCoefficients)
    