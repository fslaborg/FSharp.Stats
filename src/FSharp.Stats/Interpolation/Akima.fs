namespace FSharp.Stats.Interpolation

open System
open FSharp.Stats

module Akima =

    type SplineCoef = {
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
        } with 
            static member Create xValues c0 c1 c2 c3 = {
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
        SplineCoef.Create xValues c0 c1 c2 c3
        
    let internal leftSegmentIdx arr value = 
        LinearSpline.leftSegmentIdx arr value

    //For reference see: http://www.dorn.org/uni/sls/kap06/f08_0204.htm
    ///
    let interpolate (xValues:float []) (yValues:float []) =
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
                    if Precision.almostEqualNorm we.[i-1] 0.0 && Precision.almostEqualNorm we.[i+1] 0.0 then
                        (((xValues.[i + 1] - xValues.[i])*diff.[i - 1]) + ((xValues.[i] - xValues.[i - 1])*diff.[i]))/(xValues.[i + 1] - xValues.[i - 1])
                    else
                        ((we.[i + 1]*diff.[i - 1]) + (we.[i - 1]*diff.[i]))/(we.[i + 1] + we.[i - 1])
                tmp.[i] <- ddi 
            tmp.[0]          <- Integration.Differentiation.differentiateThreePoint xValues yValues 0 0 1 2
            tmp.[1]          <- Integration.Differentiation.differentiateThreePoint xValues yValues 1 0 1 2 
            tmp.[xValues.Length-2] <- Integration.Differentiation.differentiateThreePoint xValues yValues (xValues.Length-2) (xValues.Length-3) (xValues.Length-2) (xValues.Length-1)
            tmp.[xValues.Length-1] <- Integration.Differentiation.differentiateThreePoint xValues yValues (xValues.Length-2) (xValues.Length-3) (xValues.Length-2) (xValues.Length-1)
            tmp
        interpolateHermiteSorted xValues yValues dd
    
    ///
    let predict (splineCoeffs: SplineCoef) xVal =
        let k = leftSegmentIdx splineCoeffs.XValues xVal 
        let x = xVal - splineCoeffs.XValues.[k]
        splineCoeffs.C0.[k] + x*(splineCoeffs.C1.[k] + x*(splineCoeffs.C2.[k] + x*splineCoeffs.C3.[k]))


    ///
    let getFirstDerivative (splineCoeffs: SplineCoef) xVal =
        let k = leftSegmentIdx splineCoeffs.XValues xVal 
        let x = xVal - splineCoeffs.XValues.[k]
        splineCoeffs.C1.[k] + x*(2.*splineCoeffs.C2.[k] + x*3.*splineCoeffs.C3.[k])


    ///
    let getSecondDerivative (splineCoeffs:SplineCoef) xVal =
        let k = leftSegmentIdx splineCoeffs.XValues xVal 
        let x = xVal - splineCoeffs.XValues.[k]
        2.*splineCoeffs.C2.[k] + x*6.*splineCoeffs.C3.[k]


    ///
    let computeIndefiniteIntegral (splineCoeffs:SplineCoef) = 
        let integral = 
            let tmp = Array.zeroCreate splineCoeffs.C0.Length
            for i = 0 to tmp.Length-2 do
                let w = splineCoeffs.XValues.[i+1] - splineCoeffs.XValues.[i] 
                tmp.[i+1] <- tmp.[i] + w*(splineCoeffs.C0.[i] + w*(splineCoeffs.C1.[i]/2. + w*(splineCoeffs.C2.[i]/3. + w*splineCoeffs.C3.[i]/4.)))
            tmp
        integral

    ///
    let integrate (splineCoeffs:SplineCoef) xVal = 
        let integral = computeIndefiniteIntegral splineCoeffs 
        let k = leftSegmentIdx splineCoeffs.XValues xVal 
        let x = xVal - splineCoeffs.XValues.[k]
        integral.[k] + x*(splineCoeffs.C0.[k] + x*(splineCoeffs.C1.[k]/2. + x*(splineCoeffs.C2.[k]/3. + x*splineCoeffs.C3.[k]/4.)))

    ///
    let definiteIntegral (integrateF: float -> float) xVal1 xVal2 =
        integrateF xVal2 - integrateF xVal1 
