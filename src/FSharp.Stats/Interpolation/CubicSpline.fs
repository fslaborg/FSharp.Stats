namespace FSharp.Stats.Interpolation

open System

module CubicSpline = 
    
    type Spline = {
        /// sample points (N+1), sorted ascending
        XValues : float []
        /// Zero order spline coefficients (N)
        C0 : float []
        /// First order spline coefficients (N)
        C1 : float []
        /// Second order spline coefficients (N)
        C2 : float []
        /// Third order spline coefficients (N)
        C3 : float []
        }

    ///
    let createSpline xValues c0 c1 c2 c3 = {
        XValues=xValues;C0=c0;C1=c1;C2=c2;C3=c3 
        }

    // For reference see: http://www.dorn.org/uni/sls/kap06/f08_01.htm
    ///
    let interpolateHermiteSorted (xValues:float []) (yValues:float []) (firstDerivatives:float []) = 
        if xValues.Length <> yValues.Length || xValues.Length <> firstDerivatives.Length then
            failwith "input arrays differ in length"
        elif
            xValues.Length < 2 then
            failwith "input arrays are too small to perform a spline interpolation" 
    
        let c0 = Array.zeroCreate (xValues.Length-1)
        let c1 = Array.zeroCreate (xValues.Length-1)
        let c2 = Array.zeroCreate (xValues.Length-1)
        let c3 = Array.zeroCreate (xValues.Length-1)
        for i = 0 to (c0.Length-1) do
            let w = xValues.[i+1] - xValues.[i]
            let ww = w*w 
            c0.[i] <- yValues.[i] 
            c1.[i] <- firstDerivatives.[i]
            c2.[i] <- (3.*(yValues.[i+1] - yValues.[i])/w - 2. * firstDerivatives.[i] - firstDerivatives.[i+1])/w 
            c3.[i] <- (2.*(yValues.[i] - yValues.[i+1])/w +      firstDerivatives.[i] + firstDerivatives.[i+1])/ww 
        let spline = createSpline xValues c0 c1 c2 c3
        (fun (splineOperation: Spline -> float -> float) x -> splineOperation spline x) 

    ///
    let private leftSegmentIdx arr value = 
        let idx = 
            let tmp = Array.BinarySearch(arr, value)
            let idx = if tmp < 0 then ~~~tmp-1 else tmp
            idx
        Math.Min(arr.Length-2,Math.Max(idx, 0))

    //For reference see: http://www.dorn.org/uni/sls/kap06/f08_0204.htm
    ///
    let initAkimaInterpolation (xValues:float []) (yValues:float []) =
        if xValues.Length <> yValues.Length then
            failwith "input arrays differ in length"
        elif
            xValues.Length < 5 then
            failwith "input arrays are too small to perform a spline interpolation" 
        // prepare divided differences (diff) and weights (we)
        let diff = 
            let tmp = Array.zeroCreate (xValues.Length-1)
            for i = 0 to tmp.Length-1 do
                tmp.[i] <- (yValues.[i+1] - yValues.[i])/(xValues.[i+1] - xValues.[i])
            tmp
        let we  = 
            let tmp = Array.zeroCreate (xValues.Length-1)
            for i = 1 to tmp.Length-1 do
                tmp.[i] <- (diff.[i]-diff.[i-1]) |> abs 
            tmp
        // prepare Hermite interpolation scheme   
        let dd = 
            let tmp = Array.zeroCreate xValues.Length
            for i = 2 to tmp.Length-3 do 
                let ddi = 
                    if FSharp.Stats.Precision.almostEqualNorm we.[i-1] 0.0 && FSharp.Stats.Precision.almostEqualNorm we.[i+1] 0.0 then
                        (((xValues.[i + 1] - xValues.[i])*diff.[i - 1]) + ((xValues.[i] - xValues.[i - 1])*diff.[i]))/(xValues.[i + 1] - xValues.[i - 1])
                    else
                        ((we.[i + 1]*diff.[i - 1]) + (we.[i - 1]*diff.[i]))/(we.[i + 1] + we.[i - 1])
                tmp.[i] <- ddi 
            tmp.[0]          <- FSharp.Stats.Integration.Differentiation.differentiateThreePoint xValues yValues 0 0 1 2
            tmp.[1]          <- FSharp.Stats.Integration.Differentiation.differentiateThreePoint xValues yValues 1 0 1 2 
            tmp.[xValues.Length-2] <- FSharp.Stats.Integration.Differentiation.differentiateThreePoint xValues yValues (xValues.Length-2) (xValues.Length-3) (xValues.Length-2) (xValues.Length-1)
            tmp.[xValues.Length-1] <- FSharp.Stats.Integration.Differentiation.differentiateThreePoint xValues yValues (xValues.Length-2) (xValues.Length-3) (xValues.Length-2) (xValues.Length-1)
            tmp
        interpolateHermiteSorted xValues yValues dd
    
    ///
    let interpolateAtX (spline:Spline) xVal =
        let k = leftSegmentIdx spline.XValues xVal 
        let x = xVal - spline.XValues.[k]
        spline.C0.[k] + x*(spline.C1.[k] + x*(spline.C2.[k] + x*spline.C3.[k]))


    ///
    let firstDerivative (spline:Spline) xVal =
        let k = leftSegmentIdx spline.XValues xVal 
        let x = xVal - spline.XValues.[k]
        spline.C1.[k] + x*(2.*spline.C2.[k] + x*3.*spline.C3.[k])


    ///
    let secondDerivative (spline:Spline) xVal =
        let k = leftSegmentIdx spline.XValues xVal 
        let x = xVal - spline.XValues.[k]
        2.*spline.C2.[k] + x*6.*spline.C3.[k]


    ///
    let computeIndefiniteIntegral (spline:Spline) = 
        let integral = 
            let tmp = Array.zeroCreate spline.C0.Length
            for i = 0 to tmp.Length-2 do
                let w = spline.XValues.[i+1] - spline.XValues.[i] 
                tmp.[i+1] <- tmp.[i] + w*(spline.C0.[i] + w*(spline.C1.[i]/2. + w*(spline.C2.[i]/3. + w*spline.C3.[i]/4.)))
            tmp
        integral

    ///
    let integrate (spline:Spline) xVal = 
        let integral = computeIndefiniteIntegral spline 
        let k = leftSegmentIdx spline.XValues xVal 
        let x = xVal - spline.XValues.[k]
        integral.[k] + x*(spline.C0.[k] + x*(spline.C1.[k]/2. + x*(spline.C2.[k]/3. + x*spline.C3.[k]/4.)))

    ///
    let definiteIntegral (integrateF: float -> float) xVal1 xVal2 =
        integrateF xVal2 - integrateF xVal1 