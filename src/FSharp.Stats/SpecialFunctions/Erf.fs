namespace FSharp.Stats

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
/// Special mathematical functions
module SpecialFunctions =

    open System

    // Computes the error function. Note that this implementation has only been verified to have a relative error of around 1e-5.
    let rec Erf x =
        // Reference: Abramowitz and Stegun - Handbook of Mathematical Functions, p299.
        if x < 0.0 then
            - (Erf (-x))
        else
            let p  =  0.3275911
            let a1 =  0.254829592
            let a2 = -0.284496736
            let a3 =  1.421413741
            let a4 = -1.453152027
            let a5 =  1.061405429
            let t = 1.0 / (1.0 + p*x)
            1.0 - (exp (-(x*x))) * t * (a1 + t*(a2 + t*(a3 + t*(a4 + t*a5))))

    /// Computes the complement of the error function. Note that this implementation has only been verified to have a relative error of around 1e-4.
    let rec Erfc x =
        // Reference: Abramowitz and Stegun - Handbook of Mathematical Functions, p299.
        if x < 0.0 then
            2.0 - (Erfc (-x))
        else
            let p  =  0.3275911
            let a1 =  0.254829592
            let a2 = -0.284496736
            let a3 =  1.421413741
            let a4 = -1.453152027
            let a5 =  1.061405429
            let t = 1.0 / (1.0 + p*x)
            (exp (-(x*x))) * t * (a1 + t*(a2 + t*(a3 + t*(a4 + t*a5))))
