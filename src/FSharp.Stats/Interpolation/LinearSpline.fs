namespace FSharp.Stats.Interpolation

open System

module LinearSpline = 
    
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
        }

    ///
    let createSpline xValues c0 c1 c2 c3 = {
        XValues=xValues;C0=c0;C1=c1;C2=c2;C3=c3 
        }

