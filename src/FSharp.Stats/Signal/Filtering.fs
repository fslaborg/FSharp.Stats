namespace FSharp.Stats.Signal

open System
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
    //  windowSize : int
    //     the length of the window. Must be an odd integer number.
    //  order : int
    //     the order of the polynomial used in the filtering.
    //     Must be less then `windowSize` - 1.
    //  deriv: int
    //     the order of the derivative to compute (default = 0 means only smoothing)
    //
    //  The Savitzky-Golay is a type of low-pass filter, particularly suited for smoothing noisy data. 
    //  The main idea behind this approach is to make for each point a least-square fit with a
    //  polynomial of high order over a odd-sized window centered at the point.
    let savitzkyGolay (windowSize:int) (order:int) deriv rate (data:float[]) =
        ///             
        let correlateValid (x:Vector<float>) (y:Vector<float>) =
            if x.Length >= y.Length then 
                [|Vector.dot x y|]
            else
                let n = x.Length
                [|for i=1 to y.Length-n do
                        yield Vector.dot x y.[i..i+n-1] |]


        if windowSize % 2 <> 1 || windowSize < 1 then
            failwith "windowSize size must be a positive odd number"
        if order < deriv then
            failwith "order must be greater or equal to the used derivative"
        if windowSize < order + 2 then
            failwith "windowSize is too small for the polynomials order"
        //let orderRange = [0..order]
        let halfWindow = (windowSize - 1) / 2
        // precompute coefficients
        let b = Matrix.init (halfWindow*2 + 1) (order+1) (fun k coli -> float(k-halfWindow)**float(coli))   
  
        let m = (Algebra.LinearAlgebraManaged.pseudoInvers b).Row(deriv) * ((float(rate)**float(deriv)) * SpecialFunctions.Factorial.factorial(deriv))
        //pad the signal at the extremes with values taken from the signal itself
    
        let firstvals = 
            let length = halfWindow + 1    
            Array.init length (fun i -> 
                data.[0] - (abs data.[length-i] - data.[0]))
    
        let lastvals = 
            Array.init halfWindow (fun i -> 
                data.[data.Length-1] - (abs data.[data.Length-(2+i)] - data.[data.Length-1]) ) 
           
        let y = 
            Array.concat [firstvals; data; lastvals;] |> vector
    
        correlateValid m.Transpose y

    [<Obsolete("Use savitzkyGolay instead.")>]
    let savitzky_golay (windowSize:int) (order:int) deriv rate (data:float[]) =
        savitzkyGolay windowSize order deriv rate data

    // Method is based on: https://doi.org/10.1021/ac0600196
    /// Estimates the autocorrelation at lag 1 of a blank signal (containing only noise). Subsequently, the signal of interest is smoothed
    /// several times by a savitzky golay filter using constant polynomial order and variing windowWidth. For each iteration, the deviation
    /// of the smoothed to the original signal is computed and the autocorrelation at lag 1 of this residual noise is computed. The function returns the optimized
    /// window width yielding a autocorrelation at lag 1 closest to the value computed for the blank signal.
    let optimizeWindowWidth polOrder (windowWidthToTest:int[]) (blankSignal:float[]) (signalOfInterest:float[]) =
        let signalOfInterest' = signalOfInterest |> vector
        let noiseAutoCorr = Correlation.Vector.autoCorrelation 1 (blankSignal |> vector)
        let filterF w yData = savitzkyGolay w polOrder 0 0 yData
        let windowWidthToTest' = windowWidthToTest |> Array.filter (fun x -> x%2 <> 0)
        let optimizedWindowWidth = 
            windowWidthToTest'
            |> Array.map (fun w ->
                          let smoothedY = filterF w signalOfInterest
                          let noise = (vector smoothedY) - (signalOfInterest')
                          w, Correlation.Vector.autoCorrelation 1 noise
                         )
            |> Array.minBy (fun (w,ac) -> (ac - noiseAutoCorr) |> abs ) 
            |> fst
        optimizedWindowWidth          

    let kk = savitzky_golay

        