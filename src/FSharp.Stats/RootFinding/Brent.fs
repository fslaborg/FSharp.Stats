namespace FSharp.Stats.Rootfinding

open System

module Brent =
    
    ///
    let private adjustBounds (lowerBound:float) (upperBound:float) (root:float) (fMin:float) (fMax:float) (fRoot:float) (d,e) =
        let isSignEqualFrootFMax = (Math.Sign(fRoot) = Math.Sign(fMax)) 
        let (upperBound',fMax',(d',e')) =
            if isSignEqualFrootFMax then
                let tmp = root - lowerBound
                lowerBound, fMin, (tmp,tmp)
            else 
                upperBound, fMax, (d,e)            
        let isAbsFMaxSmallerThanFRoot = ((fMax' |> abs) < (fRoot |> abs))
        let lowerBound',root',upperBound'',fMin',fRoot',fMax'' = 
            if isAbsFMaxSmallerThanFRoot then
                root, upperBound', root, fRoot, fMax', fRoot
            else 
                lowerBound, root, upperBound', fMin, fRoot, fMax'

        lowerBound', upperBound'', root', fMin', fMax'', fRoot', (d',e')    
               
    ///
    let private checkConvergence accuracy upperBound root fRoot xMid = 
        let xAccuracy = Double.Epsilon*(root |> abs) + 0.5 * accuracy
        let xMidOld = xMid
        let xMid' = (upperBound - root) / 2.0 
        match (Math.Abs(xMid') <= xAccuracy || FSharp.Stats.Precision.almostEqualNormRelative 0. fRoot accuracy) with 
        | true -> 
            true, Some (xAccuracy, root, xMid')  
        | false when xMid' = xMidOld -> 
            false, None 
        | false -> 
            false, Some (xAccuracy, root, xMid')   

    ///
    let private acceptInterPolation d p q =
        let e' = d
        let d' = p/q
        d', e'         
        
    ///    
    let private acceptBisection d xMid =
        let d' = xMid
        let e' = d 
        d', e'

    ///
    let private attemptInverseQuadraticInterpolation xAccuracy (lowerBound:float) (upperBound:float) (root:float) (fMin:float) (fMax:float) (fRoot:float) (d,e) xMid = 
        let s = fRoot/ fMin
        let isLowerBoundEqualToUpperBound =  FSharp.Stats.Precision.almostEqualNorm lowerBound upperBound 
        let (p,q) = 
            if isLowerBoundEqualToUpperBound then 
                let p = 2.0*xMid*s
                let q = 1.-s
                p, q 
            else
                let q = fMin/fMax 
                let r = fRoot/fMax
                let p = s*(2.0*xMid*q*(q-r) - (root - lowerBound) * (r-1.0))
                let q' = (q - 1.) * (r-1.) * (s-1.)
                p,q'
        let isPGreaterZero = p > 0.  
        let q' = if isPGreaterZero = true then -q else q 
        let p' = p |> abs
        let isInterpolationValid = 2. * p' < Math.Min(3. * xMid * q' - ((xAccuracy*q') |> abs), (e*q') |> abs)  
        let (d',e') = 
            if isInterpolationValid then 
                acceptInterPolation d p' q' 
            else 
                acceptBisection d xMid
        (d',e')

    ///
    let private updateBounds func xAccuracy root fRoot d xMid = 
        let lowerBound = root
        let fMin = fRoot
        let root' = 
            if (d |> abs) > xAccuracy then 
                root + d 
            else 
                root + FSharp.Stats.Ops.signum xAccuracy xMid
        let fRoot' = func root'
        lowerBound, fMin, root', fRoot'
    ///
    let tryFindRoot accuracy maxInterations (func: float -> float) (lowerBound:float) (upperBound:float) = 
        let fMin: float  = func lowerBound
        let fMax: float  = func upperBound
        if Math.Sign(fMin) = Math.Sign(fMax) then None 
        else
        let rec loop accuracy maxInterations counter (lowerBound:float) (upperBound:float) (root:float) (fMin:float) (fMax:float) (fRoot:float) (d,e) xMid = 
            if counter = maxInterations then 
                failwith "Root was not reached within the given amount of iterations" 
            else
            let (lowerBound', upperBound', root', fMin', fMax', fRoot', (d',e')) = adjustBounds lowerBound upperBound root fMin fMax fRoot (d,e)         
            match checkConvergence accuracy upperBound' root' fRoot' xMid with 
            | false, (Some (xAccuracy, root'', xMid')) -> 
                /// Convergence is not reached restart loop with parameters updated using bisection or inverse quadratic interpolation 
                let tryInverseQuadraticInterpolation =  (e' |> abs) >= xAccuracy && ((fMin' |> abs) > (fRoot' |> abs))  
                let (d'',e'') = 
                    if tryInverseQuadraticInterpolation then 
                        attemptInverseQuadraticInterpolation xAccuracy lowerBound' upperBound' root'' fMin' fMax' fRoot' (d',e') xMid'
                    else 
                        acceptBisection d' xMid'
                let (lowerBound'',fMin'',root''',fRoot'') = 
                    updateBounds func xAccuracy root'' fRoot' d'' xMid' 
                loop accuracy maxInterations (counter+1) lowerBound'' upperBound' root''' fMin'' fMax' fRoot'' (d'',e'') xMid'
            | true,  Some (xAccuracy, root'', xMid) -> 
                /// Convergence is reached, return root
                Some (root'')
            | _, _   -> 
                /// accuracy is not sufficient to reach a root 
                None 
        loop accuracy maxInterations 0 lowerBound upperBound upperBound fMin fMax fMax  (0.,0.) nan