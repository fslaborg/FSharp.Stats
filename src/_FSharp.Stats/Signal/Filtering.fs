namespace FSharp.Stats.Signal

open FSharp.Stats


module Filtering =
    
//    open FSharp.Care
//    open FSharp.Care.Collections
//    open MathNet.Numerics
//    open MathNet.Numerics.LinearAlgebra

//    //http://www.centerspace.net/blog/nmath/iir-filtering-with-butterworth-filters/
//    let butterworthFilter  (sampleFrequency : float) (order : int) (f0 : float) (dcGain : float) (signal:seq<float>) =
//        
//        let signalFFT = Math.Complex.toComplexFloatArray signal
//        MathNet.Numerics.IntegralTransforms.Fourier.Forward signalFFT
//  
//        let n       = signalFFT.Length       
//        let numBins = float n / 2.  // Half the length of the FFT by symmetry
//        let binWidth = sampleFrequency / numBins // Hz
//        // Filter
//        for i = 1 to n / 2 do
//            let binFreq = binWidth * float i
//            let gain = dcGain / ( sqrt( ( 1. + System.Math.Pow ( binFreq / f0, 2.0 * float order ) ) ) ) |> Math.Complex.toComplexFromReal
//            signalFFT.[i] <- signalFFT.[i] * gain
//            signalFFT.[n - i] <- signalFFT.[n - i] * gain
//
//        // Reverse filtered signal
//        MathNet.Numerics.IntegralTransforms.Fourier.Inverse signalFFT
//        signalFFT |> Math.Complex.fromComplexFloatArray



 
//    ///http://www.centerspace.net/blog/nmath/chebyshev-filters-with-nmath/
//    let chebeshevFilter = 
//        0




    /// Smooth (and optionally differentiate) data with a Savitzky-Golay filter.
    /// The Savitzky-Golay filter is a type of low-pass filter and removes high frequency noise from data.
    //  Parameters
    //  ----------
    //  data : array_like, shape (N,)
    //     the values of the time history of the signal.
    //  window_size : int
    //     the length of the window. Must be an odd integer number.
    //  order : int
    //     the order of the polynomial used in the filtering.
    //     Must be less then `window_size` - 1.
    //  deriv: int
    //     the order of the derivative to compute (default = 0 means only smoothing)
    //
    //  The Savitzky-Golay is a type of low-pass filter, particularly suited for smoothing noisy data. 
    //  The main idea behind this approach is to make for each point a least-square fit with a
    //  polynomial of high order over a odd-sized window centered at the point.
    let savitzky_golay (window_size:int) (order:int) deriv rate (data:float[]) =
        ///             
        let correlate_valid (x:Vector<float>) (y:Vector<float>) =
            if x.Length >= y.Length then 
                vector [Vector.dot x y]
            else
                let n = x.Length
                vector [ for i=1 to y.Length-n do
                            yield Vector.dot x y.[i..i+n-1] ]


        if window_size % 2 <> 1 || window_size < 1 then
            failwith "window_size size must be a positive odd number"
        if window_size < order + 2 then
            failwith "window_size is too small for the polynomials order"
        //let order_range = [0..order]
        let half_window = (window_size - 1) / 2
        // precompute coefficients
        let b = Matrix.init (half_window*2 + 1) (order+1) (fun k coli -> float(k-half_window)**float(coli))   
  
        let m = (Algebra.LinearAlgebraManaged.pseudoInvers b).Row(deriv) * ((float(rate)**float(deriv)) * SpecialFunctions.Factorial.factorial(deriv))
        //pad the signal at the extremes with values taken from the signal itself
    
        let firstvals = 
            let length = half_window + 1    
            Array.init length (fun i -> 
                data.[0] - (abs data.[length-i] - data.[0]))
    
        let lastvals = 
            Array.init half_window (fun i -> 
                data.[data.Length-1] - (abs data.[data.Length-(2+i)] - data.[data.Length-1]) ) 
           
        let y = 
            Array.concat [firstvals; data; lastvals;] |> vector
    
        correlate_valid m.Transpose y

        

