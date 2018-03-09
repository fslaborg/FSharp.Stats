namespace FSharp.Stats


/// Special mathematical functions and it's simple approximations
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



    module SimpleApprox =

        /// Computes a fast approximation for the gamma function 
        let gamma z =
        
            let invE = 0.36787944117144232159552377016147 // (E^-1) = (1.0 / E) 
            let twoPI = 6.283185307179586476925286766559  // 2.0 * PI

            let d   = 1.0 / (10.0 * z)
            let d'  = 1.0 / ((12. * z) - d)
            let d'' = (d' + z) * invE
            d''**z * sqrt (twoPI / z)

        /// Computes a fast approximation for the log gamma function
        let gammaLn n =
            // LOGPIHALF = (log10(PI) / 2.0)
            let logPiHalf = 0.24857493634706692717563414414545

            let d   = (1.0 + (2.0 * n)) * 4. * n
            let d'  = (d + 1.) * n
            let d'' = (log10(d') * (1./6.)) + n + logPiHalf 
            (n * log(n)) - d''


        /// Computes the incomplete gamma function        
        let incGamma s z =
            let rec loop sum nom denom s =
                let s'     = s + 1.
                let nom'   = nom*z
                let denom' = denom*s' 
                let sum'   = sum + (nom' / denom')
                if (sum'-sum > System.Double.Epsilon) then
                    loop sum' nom' denom' s' 
                else
                    sum

            if z < 0.0 then 0.0
            else
                let sc = (1. / s) * z**s * exp -z
                let sum = loop 1. 1. 1. s
                sum * sc



