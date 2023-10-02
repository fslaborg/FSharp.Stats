namespace FSharp.Stats.SpecialFunctions

open System

/// Approximations for the gamma function and related functions.
///
/// The gamma function (represented by Γ, the capital letter gamma from the Greek alphabet) is one commonly used extension 
/// of the factorial function to complex numbers:
///
/// Γ(x) = (x-1)!
///
///The gamma function is defined for all complex numbers except the non-positive integers.
module Gamma =
    open FSharp.Stats
    
    /// <summary>Maximum gamma</summary>
    let maximum = 171.624376956302725

    ///<summary>
    /// Computes an approximation of the real value of the gamma function using the Lanczos Coefficients described in Numerical Recipes (Press et al) 
    ///</summary>
    ///<remarks> 
    /// The caller is responsible to handle edge cases such as nan, infinity, and -infinity in the input
    ///</remarks> 
    /// <param name="z">The function input for approximating Γ(z)</param>
    let _gamma z = 
        let lanczosCoefficients = [76.18009172947146;-86.50532032941677;24.01409824083091;-1.231739572450155;0.1208650973866179e-2;-0.5395239384953e-5]
        let rec sumCoefficients acc i coefficients =
            match coefficients with
            | []   -> acc
            | h::t -> sumCoefficients (acc + (h/i)) (i+1.0) t
        let gamma = 5.0
        let x = z - 1.0
        Math.Pow(x + gamma + 0.5, x + 0.5) * Math.Exp( -(x + gamma + 0.5) ) * Math.Sqrt( 2.0 * Math.PI ) * sumCoefficients 1.000000000190015 (x + 1.0) lanczosCoefficients

    ///<summary>
    /// Computes an approximation of the real value of the log gamma function using the Lanczos Coefficients described in Numerical Recipes (Press et al)
    ///</summary>
    ///<remarks>
    /// The caller is responsible to handle edge cases such as nan, infinity, and -infinity in the input
    ///</remarks>
    /// <param name="z">The function input for approximating ln(Γ(z))</param>
    let _gammaLn z =
        let lanczosCoefficients = [76.18009172947146;-86.50532032941677;24.01409824083091;-1.231739572450155;0.1208650973866179e-2;-0.5395239384953e-5]
        let rec sumCoefficients acc i coefficients =
            match coefficients with
            | []   -> acc
            | h::t -> sumCoefficients (acc + (h/i)) (i+1.0) t
        let gamma = 5.0
        let x = z - 1.0
        let tmp = x + gamma + 0.5 
        -(tmp - ((x + 0.5) * log(tmp))) + log(Math.Sqrt( 2.0 * Math.PI ) * sumCoefficients 1.000000000190015 (x + 1.0) lanczosCoefficients)

    ///<summary>
    /// Computes an approximation of the real value of the gamma function using the Lanczos Coefficients described in Numerical Recipes (Press et al) 
    ///</summary>
    ///<remarks>
    /// Edge cases in the input (nan, infinity, and -infinity) are catched and handled. 
    /// This might be slower than the unchecked version `_gamma` but does not require input sanitation to get expected results for these cases.
    ///</remarks>
    /// <param name="z">The function input for approximating Γ(z)</param>
    let gamma z = 
        match z with
        | z when (infinity.Equals(z)) -> infinity
        | z when ((-infinity).Equals(z)) -> nan
        | _ -> _gamma z

    ///<summary>
    /// Computes an approximation of the real value of the log gamma function using the Lanczos Coefficients described in Numerical Recipes (Press et al)
    ///</summary>
    ///<remarks>
    /// Edge cases in the input (nan, infinity, and -infinity) are catched and handled. 
    /// This might be slower than the unchecked version `_gamma` but does not require input sanitation to get expected results for these cases.
    ///</remarks>
    /// <param name="z">The function input for approximating ln(Γ(z))</param>
    let gammaLn z = 
        match z with
        | z when (infinity.Equals(z)) -> infinity
        | z when ((-infinity).Equals(z)) -> nan
        | _ -> _gammaLn z
            
    let private FPMIN = 1.0e-30 //(System.Double.MinValue + System.Double.Epsilon)
    let private EPS = 3.0e-8    //System.Double.Epsilon


    let private gser a x =    
        let gln = gammaLn a
        let rec loop sum del ap =
            let ap' = ap + 1.
            let del' = del * x / ap'
            if (abs del < (abs sum) * EPS) then 
                sum * exp(-x + a * log(x) - gln)
            else 
                loop (sum+del') (del') (ap')
 
        loop (1. / a) (1. / a) a

    // Page 286

    // Returns the incomplete gamma function Q(a,x) evaluated by its continued fraction representation. Also sets ln
    // .a/ as gln. User should not call directly.
    let private gcf a x =
    
        // let eps = 3.0e-7//System.Double.Epsilon
        let gln = gammaLn a    
        let b = x + 1.0 - a
        let c = 1. / FPMIN
        let d = 1. / b
        let h = d

        let rec loop i b c d  h =
            let an = -i * (i - a)
            let b = b + 2.
            let d = 
                let tmp = an * d + b
                if abs tmp < FPMIN then FPMIN else tmp
            let c = 
                let tmp = b + an / c
                if abs tmp < FPMIN then FPMIN else tmp
            let d = 1. / d
            let del = d * c
            let h = h * del

            if (abs (del - 1.) <= EPS) then 
                exp(-x + a * log(x) - gln) * h
            else
                loop (i+1.) b c d h
    
        loop (1.) b c d h




    // Incomplete gamma by quadrature. Returns P(a,x) or Q(a,x), when psig is 1 or 0, respectively. 
    // User should not call directly.
    let private gammpapprox a x (psig:bool) =

        // Abscissas for Gauss-Legendre quadrature
        let gaussLegQuadY =
            [|  
                0.0021695375159141994; 0.011413521097787704; 0.027972308950302116; 0.051727015600492421; 0.082502225484340941; 0.12007019910960293;
                0.16415283300752470; 0.21442376986779355; 0.27051082840644336; 0.33199876341447887; 0.39843234186401943; 0.46931971407375483;
                0.54413605556657973; 0.62232745288031077; 0.70331500465597174; 0.78649910768313447; 0.87126389619061517; 0.95698180152629142;
            |]
        // Weights for Gauss-Legendre quadrature
        let gaussLegQuadWeights =
            [|  
                0.0055657196642445571; 0.012915947284065419; 0.020181515297735382; 0.027298621498568734; 0.034213810770299537; 0.040875750923643261;
                0.047235083490265582; 0.053244713977759692; 0.058860144245324798; 0.064039797355015485; 0.068745323835736408; 0.072941885005653087;
                0.076598410645870640; 0.079687828912071670; 0.082187266704339706; 0.084078218979661945; 0.085346685739338721; 0.085983275670394821;
            |]
    
        let ngau = gaussLegQuadWeights.Length //18

        let a1 = a - 1.0
        let lna1 = log a1
        let sqrta1 = sqrt a1
        let gln = gammaLn a
        let xu =
            if (x > a1) then    
                max (a1 + 11.5*sqrta1) (x + 6.0*sqrta1)
            else 
                max 0. ( min (a1 - 7.5*sqrta1) (x - 5.0*sqrta1))

        let rec gaussLegendre j sum =
            if j < ngau then
                let t =  x + (xu - x) * gaussLegQuadY.[j]
                let sum' = sum + gaussLegQuadWeights.[j] * exp(-(t - a1) + a1 * (log(t) - lna1))
                gaussLegendre (j+1) sum'
            else
                let ans = sum * (xu - x) * exp(a1 * (lna1 - 1.) - gln)
                if psig then 
                    if (ans > 0.0) then 1.0 - ans else -ans
                else
                    if (ans>=0.0) then ans else 1.0 + ans

        gaussLegendre 0 0.0



    /// Returns the incomplete gamma function P(a,X) (regularized gamma) 
    // gammp -> GammaLowerIncomplete
    let lowerIncompleteRegularized a x =
        let ASWITCH=100.
        //if (x < 0.0 || a <= 0.0) then failwith ("bad args in gammp")
        match x with
        | x when x = 0. -> 0.
        | x when (x < 0.0 || a <= 0.0) -> nan
        | x when (isPosInf x) -> 1.
        | _ -> 
            if (x= 0.0) then 0.0
            elif (a >= ASWITCH) then
                gammpapprox a x true

            elif (x < a+1.0) then gser a x
            else 1.0 - gcf a x



    /// Returns the incomplete gamma function Q(a,X) = 1 - P(a,X) (regularized gamma) 
    // gammq -> GammaUpperIncomplete
    let upperIncompleteRegularized a x =
        let ASWITCH=100.
        //if (x < 0.0 || a <= 0.0) then failwith ("bad args in gammp")
        match x with
        | x when x = 0. -> 1.
        | x when (x < 0.0 || a <= 0.0) -> nan
        | x when (isPosInf x) -> 0.
        | _ -> 
            if (x= 0.0) then 1.0
            elif (a >= ASWITCH) then
                1.0 - gammpapprox a x true

            elif (x < a+1.0) then 1.0 - gser a x
            else gcf a x
    
    /// <summary>Returns the lower incomplete gamma function<br />gamma(a,x) = int(exp(-t)t^(a-1),t=0..x) for real a &gt; 0, x &gt; 0.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let lowerIncomplete a x =
        (lowerIncompleteRegularized a x) * gamma(a)

    /// <summary>Returns the upper incomplete gamma function<br />Gamma(a,x) = int(exp(-t)t^(a-1),t=0..x) for real a &gt; 0, x &gt; 0.</summary>
    /// <remarks></remarks>
    /// <param name="a"></param>
    /// <param name="x"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let upperIncomplete a x =
        (upperIncompleteRegularized a x) * gamma(a)

    /// <summary>
    ///   Digamma function.
    /// </summary>
    let rec digamma (x:float) = 
        match x with
        | x when (x = 0.) ->         
            Double.NegativeInfinity
        | x when (x < 0.) ->
            digamma(1. - x) + Math.PI / Math.Tan(-Math.PI * x)
        | _ ->
            let nz =

                let q = x
                let p = floor q

                if (p = q) then
                    raise (OverflowException("Function computation resulted in arithmetic overflow."))

                let nz = q - p
                if (nz <> 0.5) then
                    if (nz > 0.5) then
                        let p = p + 1.0
                        Math.PI / Math.Tan(System.Math.PI * (q - p))
                    else
                        Math.PI / Math.Tan(System.Math.PI * nz)
                else
                    0.0

            let x' = 
                if x <= 0 then
                    1.0 - x
                else
                    x

            let y =

                if (x' <= 10.0 && x' = floor x') then
                    let n = floor x'
                    [1. .. (n-1.)]
                    |> Seq.fold (fun w y -> y + 1.0 / w) 0.
                    |> fun y -> y - 0.57721566490153286061
                else

                    let rec loop s w =
                        if (s < 10.0) then
                            let w = w + 1.0 / s
                            let s = s + 1.0
                            loop s w
                        
                        else
                            if (s < 1.0E17) then
                                let z = 1.0 / (s * s)
                                let polv = 
                                    ((((((8.33333333333333333333E-2
                                        * z - 2.10927960927960927961E-2)
                                        * z + 7.57575757575757575758E-3)
                                        * z - 4.16666666666666666667E-3)
                                        * z + 3.96825396825396825397E-3)
                                        * z - 8.33333333333333333333E-3)
                                        * z + 8.33333333333333333333E-2)
                                Math.Log(s) - 0.5 / s - (z * polv) - w
                            
                            else
                                Math.Log(s) - 0.5 / s - w

                    loop x' 0.

            if (x < 0.) then
                y - nz
            else
                y
                    

    /// <summary>
    ///   Trigamma function.
    /// </summary>
    /// 
    /// <remarks>
    ///   This code has been adapted from the FORTRAN77 and subsequent
    ///   C code by B. E. Schneider and John Burkardt. The code had been
    ///   made public under the GNU LGPL license.
    /// </remarks>
    let rec trigamma (x:float) =
    
        match x with
        | x when  (x < 0.) ->
            let v = Math.PI / Math.Sin(-Math.PI * x)
            -trigamma(1. - x) + v * v
        | x when  (x <= 0.) ->
            raise (ArgumentException("The input parameter x must be positive.", "x"))
        | x when  (x <= 0.0001) ->
            // small value approximation        
            1.0 / x / x   
        | _ ->       
        
            let b  = 5.0
            let b2 = 0.1666666667
            let b4 = -0.03333333333
            let b6 = 0.02380952381
            let b8 = -0.03333333333
        
            let rec loop v z =
                if (z < b) then
                    let v' = v + 1. / z / z 
                    let z' = z + 1.
                    loop v' z' 
                else
                    // Apply asymptotic formula if argument is B or greater.
                    let y = 1.0 / z / z
                    v + 0.5 * y + (1.0 + y * (b2 + y * (b4 + y * (b6 + y * b8)))) / z
                
            loop 0. x

    