namespace FSharp.Stats.Signal

open System
open FSharp.Stats
open FSharp.Stats.Algebra

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

    /// <summary>
    /// "Valid" cross-correlation of a kernel <paramref name="kernel"/> with a signal <paramref name="signal"/>.
    /// Returns an array of length (signal.Length - kernel.Length + 1) if signal.Length >= kernel.Length,
    /// otherwise a single dot product if kernel.Length > signal.Length.
    /// </summary>
    /// <param name="kernel">Vector of length K (FIR filter, etc.).</param>
    /// <param name="signal">Vector of length N.</param>
    /// <returns>
    /// An array of length max(1, N-K+1) with the "valid" correlation result. Typically used for FIR filtering.
    /// </returns>
    let correlateValid (kernel: Vector<float>) (signal: Vector<float>) : float[] =
        let kLen = kernel.Length
        let sLen = signal.Length

        if sLen < kLen then
            // The original code returned a single dot if kernel longer than signal
            [| Vector.dotProduct kernel signal |]
        else
            // We produce sLen - kLen + 1 outputs
            let outLen = sLen - kLen + 1
            Array.init outLen (fun i ->
                // dot kernel with signal[i.. i+kLen-1]
                Vector.dotProduct kernel signal.[i .. i + kLen - 1]
            )


    /// <summary>
    /// Smooth (or differentiate) data with a Savitzky–Golay filter of given <paramref name="windowSize"/>
    /// and polynomial <paramref name="order"/>. The <paramref name="deriv"/> parameter specifies the
    /// derivative order to compute (0 = smoothing only). The <paramref name="rate"/> helps scale the 
    /// derivative if deriv > 0.
    /// </summary>
    /// <param name="windowSize">Must be odd and at least (order+2) if deriving.</param>
    /// <param name="order">Polynomial order (must be >= deriv). A higher order can better fit curvature.</param>
    /// <param name="deriv">Order of derivative to compute (0 => smoothing).</param>
    /// <param name="rate">Scaling factor for derivative, typically sampling rate = 1.0 if no special scale needed.</param>
    /// <param name="data">The data vector to filter (length N).</param>
    /// <returns>
    /// A float[] array with the filtered (or derived) signal, of length = data.Length.
    /// </returns>
    let savitzkyGolay (windowSize:int) (order:int) (deriv:int) (rate:float) (data: Vector<float>) : float[] =
        // 0) Validate parameters
        if windowSize % 2 <> 1 || windowSize < 1 then
            failwith "windowSize must be a positive odd integer."
        if order < deriv then
            failwith "Polynomial order must be >= derivative order."
        if windowSize < order + 2 then
            failwithf "windowSize (%d) is too small for polynomial order (%d)." windowSize order

        let n = data.Length
        if n < windowSize then
            failwithf "Data length (%d) smaller than windowSize (%d)." n windowSize

        // 1) Precompute "b" matrix of shape (windowSize × (order+1)).
        //    b[k, col] = (float(k - halfWindow))^(col)
        let halfWindow = (windowSize - 1) / 2
        let b =
            Matrix.init windowSize (order + 1) (fun k col ->
                // k goes 0..(windowSize-1), so "center" is k - halfWindow
                let x = float (k - halfWindow)
                x ** float col
            )

        // 2) Pseudoinverse of b => shape = ((order+1) × windowSize).
        //    Then we select row(deriv), which is shape (1 × windowSize).
        //    We'll scale it by (rate^deriv * factorial(deriv)) if derivative is requested.
        let bInv = LinearAlgebra.pseudoInvers b
        // The row we want is bInv.Row(deriv), which is a 1×windowSize slice -> treat as Vector
        let rowDeriv = Matrix.getRow deriv bInv // shape (1, windowSize), but we can interpret as Vector

        // Scale row by factor = rate^deriv * factorial(deriv)
        let factor = (rate ** float(deriv)) * float (FSharp.Stats.SpecialFunctions.Factorial.factorial deriv)
        let m = factor .* rowDeriv  // final FIR coefficients (length = windowSize)

        // 3) Build the padded signal array "y" at both ends to handle "valid" portion
        //    Pad with a reflection scheme: for the left side, reflect the first part of data;
        //    for the right side, reflect the last part.
        let firstvals =
            // halfWindow+1 elements for the left pad 
            Array.init (halfWindow) (fun i ->
                let diff = data.[(halfWindow - i)] - data.[0]
                data.[0] - diff
            )
            |> Array.rev  // ensure the left-most index is for i=0

        let lastvals =
            // halfWindow elements for the right pad
            Array.init halfWindow (fun i ->
                let diff = data.[n-1] - data.[(n-2 - i)]
                data.[n-1] + diff
            )

        let yVec = Array.concat [ firstvals; data; lastvals ]

        // 4) Correlate "m" with padded "y" in "valid" mode, i.e. output length = data.Length
        let result = correlateValid m yVec

        result


    /// <summary>Estimates the autocorrelation at lag 1 of a blank signal (containing only noise). Subsequently, the signal of interest is smoothed<br />several times by a savitzky golay filter using constant polynomial order and variing windowWidth. For each iteration, the deviation<br />of the smoothed to the original signal is computed and the autocorrelation at lag 1 of this residual noise is computed. The function returns the optimized<br />window width yielding a autocorrelation at lag 1 closest to the value computed for the blank signal.</summary>
    /// <remarks>Method is based on: https://doi.org/10.1021/ac0600196</remarks>
    /// <param name="polOrder"></param>
    /// <param name="windowWidthToTest"></param>
    /// <param name="blankSignal"></param>
    /// <param name="signalOfInterest"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let optimizeWindowWidth polOrder (windowWidthToTest:int[]) (blankSignal:float[]) (signalOfInterest:float[]) =
        let signalOfInterest' = signalOfInterest
        let noiseAutoCorr = Correlation.Vector.autoCorrelation 1 (blankSignal)
        let filterF w yData = savitzkyGolay w polOrder 0 0 yData
        let windowWidthToTest' = windowWidthToTest |> Array.filter (fun x -> x%2 <> 0)
        let optimizedWindowWidth = 
            windowWidthToTest'
            |> Array.map (fun w ->
                          let smoothedY = filterF w signalOfInterest
                          let noise = (smoothedY) .- (signalOfInterest')
                          w, Correlation.Vector.autoCorrelation 1 noise
                         )
            |> Array.minBy (fun (w,ac) -> (ac - noiseAutoCorr) |> abs ) 
            |> fst
        optimizedWindowWidth          


        