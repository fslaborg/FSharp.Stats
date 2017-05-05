namespace FSharp.Stats.SpecialFunctions

open System

/// Special mathematical functions
module Beta =

    let private EPS = 3.0e-8    // Precision.DoublePrecision;
    let private FPMIN = 1.0e-30 // 0.0.Increment()/eps

    /// Computes the natural logarithm of the beta function.
    let betaLn z w = (Gamma.gammaLn z) + (Gamma.gammaLn w) - (Gamma.gammaLn (z+w))

    /// Computes the beta function.
    let beta z w = exp (betaLn z w)


    //  incomplete beta function 
    let betaRegularized a b x =       
        if (a < 0.0) then invalidArg "a" "Argument must not be negative"
        if (b < 0.0) then invalidArg "b" "Argument must not be negative"
        if (x < 0.0 || x > 1.0) then invalidArg "x" "Argument XY interval is inclusive"
        let bt = 
            if (x = 0.0 || x = 1.0) then
                0.0
            else
                exp (Gamma.gammaLn (a + b) - Gamma.gammaLn a - Gamma.gammaLn b + (a*Math.Log(x)) + (b*Math.Log(1.0 - x)))

        let isSymmetryTransformation = ( x >= (a + 1.0)/(a + b + 2.0))

        let symmetryTransformation a b x =
            let qab = a + b
            let qap = a + 1.0
            let qam = a - 1.0
            let c = 1.0
            let d = 
                let tmp =  1.0 - (qab * x / qap)
                if (abs tmp < FPMIN) then 1. / FPMIN else 1. / tmp
            let h = d
            let rec loop m mm d h c =                
                let aa = float m * (b - float m)*x/((qam + mm)*(a + mm))
                let d' = 
                    let tmp = 1.0 + (aa*d)
                    if (abs tmp < FPMIN) then 1. / FPMIN else 1. / tmp
                let c' = 
                    let tmp = 1.0 + (aa/c)
                    if (abs tmp < FPMIN) then FPMIN else tmp
                let h' = h * d' * c'
                let aa' = -(a + float m)*(qab + float m)*x/((a + mm)*(qap + mm))
                let d'' = 
                    let tmp = 1.0 + (aa' * d')
                    if (abs tmp < FPMIN) then 1. / FPMIN else 1. / tmp
                let c'' = 
                    let tmp = 1.0 + (aa'/c')
                    if (abs tmp < FPMIN) then FPMIN else tmp
                
                let del = d*c
                let h'' = h' * del
                
                if abs (del - 1.0) <= EPS then
                    if isSymmetryTransformation then 1.0 - (bt*h/a) else bt*h/a
                else
                    if m < 140 then
                        loop (m+1) (mm+2.) d'' h'' c''
                    else 
                         if isSymmetryTransformation then 1.0 - (bt*h/a) else bt*h/a
                
            loop 1 2. d h c 
            

        if isSymmetryTransformation then
            symmetryTransformation b a (1.0-x)
        else
            symmetryTransformation a b x

