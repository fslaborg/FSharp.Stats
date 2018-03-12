namespace FSharp.Stats.Integration

module Differentiation =

    /// Three-Point Differentiation Helper.
    /// xValues Sample Points t.
    /// yValues Sample Values x(t)
    /// idxT Index of the point of the differentiation.</param>
    /// idx0 Index of the first sample.</param>
    /// idx1 Index of the second sample.</param>
    /// idx2 Index of the third sample.</param>
    let differentiateThreePoint (xValues:float []) (yValues:float []) idxT idx0 idx1 idx2 = 
        let x0 = yValues.[idx0]
        let x1 = yValues.[idx1]
        let x2 = yValues.[idx2]

        let t = xValues.[idxT]- xValues.[idx0]
        let t1 = xValues.[idx1]- xValues.[idx0]
        let t2 = xValues.[idx2]- xValues.[idx0]

        let a  = (x2 - x0 - (t2/t1*(x1 - x0)))/(t2*t2 - t1*t2)
        let b  = (x1 - x0 - a*t1*t1)/t1
        (2.*a*t) + b

