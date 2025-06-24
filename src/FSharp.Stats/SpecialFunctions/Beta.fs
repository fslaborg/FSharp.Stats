namespace FSharp.Stats.SpecialFunctions

open System
open FSharp.Stats
open FSharp.Stats.GenericMath

/// The beta function B(p,q), or the beta integral (also called the Eulerian integral of the first kind) is defined by
///
/// B(p, q) = (Γ(p) * Γ(q)) / Γ(p+q)
module Beta =

    let private EPS : 'T = T 3.0e-8    // Precision.DoublePrecision;
    let private FPMIN : 'T = T 1.0e-30 // 0.0.Increment()/eps

    ///<summary>
    /// Computes an approximation of the real value of the log beta function using approximations for the gamma function using Lanczos Coefficients described in Numerical Recipes (Press et al) 
    ///</summary>
    ///<remarks>
    /// The caller is responsible to handle edge cases such as nan, infinity, and -infinity in the input
    ///</remarks>
    /// <param name="z">The function input for approximating ln(B(z, w))</param>
    /// <param name="w">The function input for approximating ln(B(z, w))</param>
    let inline _betaLn<'T when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T : equality
        and 'T :> ValueType
        and 'T :> System.Numerics.IFloatingPoint<'T>
        and 'T :> System.Numerics.IExponentialFunctions<'T>
        and 'T :> System.Numerics.ILogarithmicFunctions<'T>
        and 'T :> System.Numerics.IRootFunctions<'T>
        and 'T :> System.Numerics.IPowerFunctions<'T>
        and 'T : comparison>  
        (z: 'T) (w: 'T) = 
        (Gamma._gammaLn z) + (Gamma._gammaLn w) - (Gamma._gammaLn (z+w))

    ///<summary>
    /// Computes an approximation of the real value of the beta function using approximations for the gamma function using Lanczos Coefficients described in Numerical Recipes (Press et al) 
    ///</summary>
    ///<remarks>
    /// The caller is responsible to handle edge cases such as nan, infinity, and -infinity in the input
    ///</remarks>
    /// <param name="z">The function input for approximating B(z, w)</param>
    /// <param name="w">The function input for approximating B(z, w)</param>
    let inline _beta<'T when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T : equality
        and 'T :> ValueType
        and 'T :> System.Numerics.IFloatingPoint<'T>
        and 'T :> System.Numerics.IExponentialFunctions<'T>
        and 'T :> System.Numerics.ILogarithmicFunctions<'T>
        and 'T :> System.Numerics.IRootFunctions<'T>
        and 'T :> System.Numerics.IPowerFunctions<'T>
        and 'T : comparison> 
        (z: 'T) (w: 'T) = 
        exp (_betaLn z w)

    ///<summary>
    /// Computes an approximation of the real value of the log beta function using approximations for the gamma function using Lanczos Coefficients described in Numerical Recipes (Press et al) 
    ///</summary>
    ///<remarks>
    /// Edge cases in the input (nan, infinity, and -infinity) are catched and handled. 
    /// This might be slower than the unchecked version `_betaLn` but does not require input sanitation to get expected results for these cases.
    ///</remarks>    
    /// <param name="z">The function input for approximating ln(B(z, w))</param>
    /// <param name="w">The function input for approximating ln(B(z, w))</param>
    let inline betaLn<'T when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T : equality
        and 'T :> ValueType
        and 'T :> System.Numerics.IFloatingPoint<'T>
        and 'T :> System.Numerics.IExponentialFunctions<'T>
        and 'T :> System.Numerics.ILogarithmicFunctions<'T>
        and 'T :> System.Numerics.IRootFunctions<'T>
        and 'T :> System.Numerics.IPowerFunctions<'T>
        and 'T : comparison>  
        (z: 'T) (w: 'T) = 
        (Gamma.gammaLn z) + (Gamma.gammaLn w) - (Gamma.gammaLn (z+w))

    ///<summary>
    /// Computes an approximation of the real value of the beta function using approximations for the gamma function using Lanczos Coefficients described in Numerical Recipes (Press et al) 
    ///</summary>
    ///<remarks>
    /// Edge cases in the input (nan, infinity, and -infinity) are catched and handled. 
    /// This might be slower than the unchecked version `_beta` but does not require input sanitation to get expected results for these cases.
    ///</remarks>
    /// <param name="z">The function input for approximating B(z, w)</param>
    /// <param name="w">The function input for approximating B(z, w)</param>
    let inline beta<'T when 'T :> Numerics.INumber<'T>
        and 'T : (new: unit -> 'T)
        and 'T : struct
        and 'T : equality
        and 'T :> ValueType
        and 'T :> System.Numerics.IFloatingPoint<'T>
        and 'T :> System.Numerics.IExponentialFunctions<'T>
        and 'T :> System.Numerics.ILogarithmicFunctions<'T>
        and 'T :> System.Numerics.IRootFunctions<'T>
        and 'T :> System.Numerics.IPowerFunctions<'T>
        and 'T : comparison>  
        (z: 'T) (w: 'T) = 
        exp (betaLn z w)

    // incomplete beta function 
    /// <summary>
    /// Returns the regularized lower incomplete beta function 
    /// </summary>
    /// <param name="a">The first Beta parameter, a positive real number.</param>
    /// <param name="b">The second Beta parameter, a positive real number.</param>
    /// <param name="x">The upper limit of the integral.</param>
    let inline lowerIncompleteRegularized
        (a: ^T when ^T :> Numerics.INumber<^T>
              and ^T : (new: unit -> ^T)
              and ^T : struct
              and ^T : equality
              and ^T :> ValueType
              and ^T :> System.Numerics.IFloatingPoint<^T>
              and ^T :> System.Numerics.IExponentialFunctions<^T>
              and ^T :> System.Numerics.ILogarithmicFunctions<^T>
              and ^T :> System.Numerics.IRootFunctions<^T>
              and ^T :> System.Numerics.IPowerFunctions<^T>
              and ^T : comparison)
        (b: ^T when ^T :> Numerics.INumber<^T>
              and ^T : (new: unit -> ^T)
              and ^T : struct
              and ^T : equality
              and ^T :> ValueType
              and ^T :> System.Numerics.IFloatingPoint<^T>
              and ^T :> System.Numerics.IExponentialFunctions<^T>
              and ^T :> System.Numerics.ILogarithmicFunctions<^T>
              and ^T :> System.Numerics.IRootFunctions<^T>
              and ^T :> System.Numerics.IPowerFunctions<^T>
              and ^T : comparison)
        (x: ^T when ^T :> Numerics.INumber<^T>
              and ^T : (new: unit -> ^T)
              and ^T : struct
              and ^T : equality
              and ^T :> ValueType
              and ^T :> System.Numerics.IFloatingPoint<^T>
              and ^T :> System.Numerics.IExponentialFunctions<^T>
              and ^T :> System.Numerics.ILogarithmicFunctions<^T>
              and ^T :> System.Numerics.IRootFunctions<^T>
              and ^T :> System.Numerics.IPowerFunctions<^T>
              and ^T : comparison) =       
        if (a < T 0.0) then invalidArg "a" "Argument must not be negative"
        if (b < T 0.0) then invalidArg "b" "Argument must not be negative"
        if (x < T 0.0 || x > T 1.0) then invalidArg "x" "Argument XY interval is inclusive"
        let bt: 'T = 
            if (x = T 0.0 || x = T 1.0) then
                T 0.0
            else
                exp (Gamma._gammaLn (a + b) - Gamma._gammaLn a - Gamma._gammaLn b + (a*log(x)) + (b*log(T 1.0 - x)))

        let isSymmetryTransformation = ( x >= (a + T 1.0)/(a + b + T 2.0))

        let inline symmetryTransformation 
            (a: 'T) (b: 'T) (x: 'T) =
            let qab = a + b
            let qap = a + T 1.0
            let qam = a - T 1.0
            let c = T 1.0
            let d: 'T = 
                let tmp =  T 1.0 - (qab * x / qap)
                if (abs tmp < FPMIN) then T 1. / FPMIN else T 1. / tmp
            let h = d
            let rec loop (m: 'T) (mm: 'T) (d: 'T) (h: 'T) (c: 'T) =                
                let aa = m * (b - m)*x/((qam + mm)*(a + mm))
                let d' = 
                    let tmp = T 1.0 + (aa*d)
                    if (abs tmp < FPMIN) then T 1. / FPMIN else T 1. / tmp
                let c' = 
                    let tmp = T 1.0 + (aa/c)
                    if (abs tmp < FPMIN) then FPMIN else tmp
                let h': 'T = h * d' * c'
                let aa': 'T = -(a + m)*(qab + m)*x/((a + mm)*(qap + mm))
                let d'': 'T = 
                    let tmp = T 1.0 + (aa' * d')
                    if (abs tmp < FPMIN) then T 1. / FPMIN else T 1. / tmp
                let c'': 'T = 
                    let tmp = T 1.0 + (aa'/c')
                    if (abs tmp < FPMIN) then FPMIN else tmp
                
                let del: 'T = d''*c''
                let h'': 'T = h' * del
                
                let tepmI: 'T = 
                    bt*h''/a

                if abs (del - T 1.0) <= EPS then
                    
                    if isSymmetryTransformation then 
                        T 1.0 - tepmI 
                    else 
                        tepmI
                else
                    if m < T 140 then
                        loop (m+T 1) (mm+T 2.) d'' h'' c''
                    else 
                            if isSymmetryTransformation then T 1.0 - tepmI else tepmI
                
            loop (T 1) (T 2.) d h c             

        if isSymmetryTransformation then
            symmetryTransformation b a (T 1.0-x)
        else
            symmetryTransformation a b x


    /// <summary>
    /// Returns the lower incomplete (unregularized) beta function
    /// </summary>
    /// <param name="a">The first Beta parameter, a positive real number.</param>
    /// <param name="b">The second Beta parameter, a positive real number.</param>
    /// <param name="x">The upper limit of the integral.</param>
    let inline lowerIncomplete(a, b, x) : 'T=
        (lowerIncompleteRegularized a b x) * (beta a b)
 
 
    /// <summary>
    ///   Power series for incomplete beta integral. Use when b*x
    ///   is small and x not too close to 1.
    /// </summary>
    let inline powerSeries a b x =
        let ai = T 1.0 / a
        let ui = (T 1.0 - b) * x 
        let t1 = ui / (a + T 1.0)
        let z  = Ops.epsilon * ai |> T

        let rec loop u t v s n =
            if (abs v > z) then 
                let u' = (n - b) * x / n
                let t' = t * u'
                let v' = t' / (a + n)
                loop u' t' (t' / (a + n)) (s + v') (n + 1.)
            else
                s + t1 + ai
        
        let s = loop ui ui t1 0. 2.  
        let u = a * log x
        
        if ((a + b) < Gamma.maximum && abs u < Ops.logMax) then
            let t = Gamma.gamma (a + b) / (Gamma.gamma a * Gamma.gamma b)
            s * t * System.Math.Pow(x, a)
        else
            let t = Gamma.gammaLn (a + b) - Gamma.gammaLn a - Gamma.gammaLn b + u + log s
            if (t < Ops.logMin) then
                0.0
            else
                exp t


    //   Inverse of incomplete beta integral.
    //let incompleteInverse aa bb yy0 =
    //    aa


    //TODO: Beta into class to allow [<ParamArray>]

    //   Multinomial Beta function.
    //let multinomial ([<ParamArray>] x:float[]) =
    //    let mutable sum = 0.
    //    let mutable prd = 1.

    //    for i = 0 to x.Length-1 do  
    //        sum <- sum + x[i];
    //        prd <- prd * Gamma.gamma(x[i]);

    //    prd / Gamma.gamma sum
  