namespace FSharp.Stats.SpecialFunctions

open System

/// Special mathematical functions
module Gamma =
    

    
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
    let gammaLn z = 
        let lanczosCoefficients = [76.18009172947146;-86.50532032941677;24.01409824083091;-1.231739572450155;0.1208650973866179e-2;-0.5395239384953e-5]
        let rec sumCoefficients acc i coefficients =
            match coefficients with
            | []   -> acc
            | h::t -> sumCoefficients (acc + (h/i)) (i+1.0) t
        let gamma = 5.0
        let x = z - 1.0
        let tmp = x + gamma + 0.5 
        -(tmp - ((x + 0.5) * log(tmp))) + log(Math.Sqrt( 2.0 * Math.PI ) * sumCoefficients 1.000000000190015 (x + 1.0) lanczosCoefficients)



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
    let lowerIncomplete a x =
        let ASWITCH=100.
        if (x < 0.0 || a <= 0.0) then failwith ("bad args in gammp")
        if (x= 0.0) then 0.0
        elif (a >= ASWITCH) then
            gammpapprox a x true

        elif (x < a+1.0) then gser a x
        else 1.0 - gcf a x



    /// Returns the incomplete gamma function Q(a,X) = 1 - P(a,X) (regularized gamma) 
    // gammq -> GammaUpperIncomplete
    let upperIncomplete a x =
        let ASWITCH=100.
        if (x < 0.0 || a <= 0.0) then failwith ("bad args in gammp")
        if (x= 0.0) then 0.0
        elif (a >= ASWITCH) then
            gammpapprox a x true

        elif (x < a+1.0) then 1.0 - gser a x
        else gcf a x





    