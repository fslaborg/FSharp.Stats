namespace FSharp.Stats


/// <summary>
/// Module to compute summary statistics for a collection of numeric values,
/// including the count, mean, M2 (sum of squared deviations),
/// minimum, and maximum.
/// </summary>
module SummaryStats =


    /// <summary>
    /// Represents summary statistics for a collection of numeric values,
    /// including the count, mean, M2 (sum of squared deviations),
    /// minimum, and maximum.
    /// </summary>
    type SummaryStats<'T> =
        {
            /// <summary>The number of data points observed.</summary>
            N : 'T     
            /// <summary>The mean (average) of all observed data points.</summary>
            Mean  : 'T       
            /// <summary>The sum of squared deviations fom the mean </summary>
            SumSqrdDevations    : 'T
            /// <summary>The minimum observed value.</summary>
            Min   : 'T
            /// <summary>The maximum observed value.</summary>
            Max   : 'T
        }

    /// <summary>
    /// Creates a <c>SummaryStats</c> record from the given fields.
    /// </summary>
    /// <param name="count">The number of observed data points.</param>
    /// <param name="mean">The running mean of the data points.</param>
    /// <param name="sumOfSquares">The sum of squared deviations fom the mean</param>
    /// <param name="minVal">The minimum observed value.</param>
    /// <param name="maxVal">The maximum observed value.</param>
    /// <returns>A new <c>SummaryStats</c> record.</returns>
    let createSummaryStats n mean sos min max =
        {N=n;Mean=mean;SumSqrdDevations=sos;Min=min;Max=max}

     /// <summary>
    /// Returns the mean (average) value from the specified summary statistics.
    /// </summary>
    /// <param name="sStats">A summary statistics record.</param>
    /// <returns>The mean of the observed data.</returns>
    let inline mean sStats = 
        sStats.Mean

    /// <summary>
    /// Returns the population variance from the specified summary statistics.
    /// </summary>
    /// <remarks>
    /// The population variance is defined as <c>SumOfSquares / N</c>,
    /// where <c>SumOfSquares</c> is the sum of squared deviations (M2),
    /// and <c>N</c> is the total count.
    /// </remarks>
    /// <param name="sStats">A summary statistics record.</param>
    /// <returns>The population variance of the observed data.</returns>
    let inline varPopulation sStats = 
        sStats.SumSqrdDevations / sStats.N

    /// <summary>
    /// Returns the sample variance from the specified summary statistics,
    /// using <c>N - 1</c> in the denominator.
    /// </summary>
    /// <remarks>
    /// The sample variance is computed as <c>SumOfSquares / (N - 1)</c>,
    /// which is the unbiased estimator when <c>N &gt; 1</c>.
    /// </remarks>
    /// <param name="sStats">A summary statistics record.</param>
    /// <returns>The sample variance of the observed data.</returns>
    let inline var (sStats: SummaryStats<'T>) = 
        let one = LanguagePrimitives.GenericOne<'T>
        sStats.SumSqrdDevations / (sStats.N - one)

    /// <summary>
    /// Returns the sample standard deviation from the specified summary statistics.
    /// </summary>
    /// <remarks>
    /// This is the square root of the sample variance.
    /// </remarks>
    /// <param name="sStats">A summary statistics record.</param>
    /// <returns>The sample standard deviation of the observed data.</returns>
    let inline stDev (sStats: SummaryStats<'T>) = 
        sqrt (var sStats)

    /// <summary>
    /// Returns the population standard deviation from the specified summary statistics.
    /// </summary>
    /// <remarks>
    /// This is the square root of the population variance.
    /// </remarks>
    /// <param name="sStats">A summary statistics record.</param>
    /// <returns>The population standard deviation of the observed data.</returns>
    let inline stDevPopulation (sStats: SummaryStats<'T>) = 
        sqrt (varPopulation sStats)


    /// <summary>
    /// Computes Welford-based summary statistics (count, mean, sum of squares, min, max)
    /// in a single pass for a given sequence of numeric data.
    /// </summary>
    /// <remarks>
    /// This function reads the sequence one item at a time (via its enumerator),
    /// applying Welford's online update formula. It is generic over any numeric
    /// type <c>'T</c> that supports F# inlined arithmetic.
    /// </remarks>
    /// <param name="items">A sequence of numeric data.</param>
    /// <returns>
    /// A <c>SummaryStats</c> record containing the final count, mean, sum of squares, min, and max.
    let inline ofSeq (items: seq<'T>) =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero<'T>
        let one  = LanguagePrimitives.GenericOne<'T>
    
        // Recursive loop for Welford’s algorithm
        let rec loop n currentMin currentMax mean m2 =
            match e.MoveNext() with
            | true ->
                let x = e.Current
                // "delta" is how far the new value is from the old mean
                let delta = x - mean
                // n+1 is the new total number of data points
                let n' = n + one
                let mean' = mean + delta / n'
                // Welford’s M2 update -> (M2) is the sum of squares of the differences from the mean
                let m2' = m2 + delta * (x - mean')
                loop n' (min x currentMin) (max x currentMax) mean' m2'
            | false ->
                // At the end, n is the total count of items,
                // mean is the final mean, M2 is the sum of squared deviations, etc.
                createSummaryStats n mean m2 currentMin currentMax

        // Pull the first item out before starting the loop.
        match e.MoveNext() with
        | true ->
            let firstVal = e.Current
            // n = 1, mean = firstVal, M2 = 0, min = max = firstVal
            loop one firstVal firstVal firstVal zero
        | false ->
            // No data --> return "empty" stats
            let nan = zero / zero
            createSummaryStats zero nan nan nan nan    


    /// <summary>
    /// Computes Welford-based summary statistics (count, mean, sum of squares, min, max)
    /// in a single pass for a given array of numeric data.
    /// </summary>
    /// <remarks>
    /// This function iterates over the array exactly once. It is generic over any
    /// numeric type <c>'T</c> that supports F# inlined arithmetic.
    /// </remarks>
    /// <param name="arr">An array of numeric data.</param>
    /// <returns>
    /// A <c>SummaryStats</c> record containing the final count, mean, sum of squares, min, and max.
    /// </returns>
    let inline ofArray (arr: 'T[]) =
        // We'll need zero, one, etc. from generic operators:
        let zero = LanguagePrimitives.GenericZero<'T>
        let one  = LanguagePrimitives.GenericOne<'T>

        // Define a "generic NaN" to handle empty arrays:
        let uNan = zero / zero

        match arr.Length with
        | 0 ->
            // No elements, produce an all-NaN stats
            createSummaryStats zero uNan uNan uNan uNan

        | _ ->
            // Initialize with the first value
            let mutable count  = one
            let mutable m1     = arr.[0]  // mean
            let mutable m2     = zero     // sum of squared deviations
            let mutable minVal = arr.[0]
            let mutable maxVal = arr.[0]

            // Process the rest of the array
            for i in 1 .. arr.Length - 1 do

                let x = arr.[i]
                // "delta" is how far the new value is from the old mean
                let delta = x - m1
                // n+1 is the new total number of data points
                let n' = count + one
                let mean' = m1 + delta / n'
                // Welford’s M2 update -> (M2) is the sum of squares of the differences from the mean
                let m2' = m2 + delta * (x - mean')

                // Update rolling state
                count  <- n'
                m1     <- mean'
                m2     <- m2'
            
                // Track min/max
                if  x < minVal then minVal <-  x
                if  x > maxVal then maxVal <-  x

            createSummaryStats count m1 m2 minVal maxVal





//module RunningStats =

//    type RunningStats<'T> = {
//        N : int
//        M1 : 'T
//        M2 : 'T
//        M3 : 'T
//        M4 : 'T
//    }

//    let createRunningStats n m1 m2 m3 m4 =
//        {N=n;M1=m1;M2=m2;M3=m3;M4=m4}

//    //let inline combine (a:RunningStats<'T>) (b:RunningStats<'T>) = 
        
//    //    let (..*) n a  = Ops.multByInt32 a n

//    //    let cn = a.N + b.N
//    //    let delta = b.M1 - a.M1
//    //    let delta2 = delta * delta
//    //    let delta3 = delta * delta2
//    //    let delta4 = delta2 * delta2

//    //    let cM1 = LanguagePrimitives.DivideByInt<'T> ( (Ops.multByInt32 a.M1 a.N ) + (Ops.multByInt32 b.M1 b.N))  cn
//    //    let cM2 = LanguagePrimitives.DivideByInt<'T> (Ops.multByInt32 (a.M2 + b.M2 +  delta2) (a.N * b.N))  cn
//    //    let cM3 = 
//    //        let tmp = LanguagePrimitives.DivideByInt<'T> (Ops.multByInt32 (a.M3 + b.M3 + delta3) (a.N * b.N * (a.N - b.N))) (cn * cn)
//    //        tmp + LanguagePrimitives.DivideByInt<'T> ((Ops.multByInt32 delta 3) * (Ops.multByInt32 b.M2 a.N) - (Ops.multByInt32 a.M2 b.N))  cn
//    //    let cM4 =
//    //        let tmp  = LanguagePrimitives.DivideByInt<'T> (Ops.multByInt32 (Ops.multByInt32 (a.M4 + b.M4 + delta4) (a.N*b.N)) (a.N*a.N - a.N*b.N + b.N*b.N)) (cn * cn * cn)
//    //        //let tmp2 = LanguagePrimitives.DivideByInt<'T> ((Ops.multByInt32 delta2 6) * ((Ops.multByInt32 b.M2 (a.N * a.N)) + (Ops.multByInt32 a.M2 (b.N * b.N)))) (cn*cn)
//    //        tmp + (LanguagePrimitives.DivideByInt<'T> (6 ..* delta2 * ( (a.N * a.N) ..* b.M2 +  (b.N * b.N) ..* a.M2)) (cn*cn)) + LanguagePrimitives.DivideByInt<'T>  (4 ..* delta * (a.N ..* b.M3 - b.N ..* a.M3)) cn
            
//    //    createRunningStats cn cM1 cM2 cM3 cM4




//    let inline mean rStats = 
//        rStats.M1
//    ///
//    let inline varPopulation rStats = 
//        LanguagePrimitives.DivideByInt rStats.M2 rStats.N
//    ///
//    let inline var (rStats:RunningStats<'T>) = 
//        LanguagePrimitives.DivideByInt rStats.M2 (rStats.N-1)
//    ///
//    let inline stDev (rStats:RunningStats<'T>) = 
//        sqrt (var rStats)
//    ///
//    let inline stDevPopulation (rStats:RunningStats<'T>) = 
//        sqrt (varPopulation rStats)

////    ///Skewness
////    let inline skewness (rStats:RunningStats<'T>) = 
////        sqrt(double(n)) * M3/ pown(M2, 1.5)
////        sqrt (varPopulation rStats)
    
//    /// Kurtosis
////    let inline kurtosis (rStats:RunningStats<'T>) = 
////        let one   = LanguagePrimitives.GenericOne< 'T >
////        let tmp = Ops.multByInt32 rStats.M4 rStats.N         
////        tmp / (rStats.M2 * rStats.M2) - (one + one + one)


    
////    let inline ofSeq (items:seq<'T>) : RunningStats< 'U >  =
////        use e = items.GetEnumerator()
////        let zero  = LanguagePrimitives.GenericZero< 'U > 
////        //let one   = LanguagePrimitives.GenericOne< 'U > 
                
////        let rec loop n (m1:'U) (m2:'U) (m3:'U) (m4:'U) =
////            match e.MoveNext() with
////            | true  -> 
////                let n'       = n + 1
////                let delta    = e.Current - m1
////                let delta_n  = LanguagePrimitives.DivideByInt< 'U > delta n
////                let delta_n2 = delta_n * delta_n
////                let term1    = Ops.multByInt32 (delta * delta_n) n'
////                let m1' = m1 + delta_n
////                let m4' = m4 + (Ops.multByInt32 (term1 * delta_n2) (n'*n' - 3*n' + 3)) + (Ops.multByInt32 (delta_n2 * m2) 6) - (Ops.multByInt32 (delta_n * m3) 4)
////                let m3' = m3 + (Ops.multByInt32 (term1 * m2) (n' - 2)) - (Ops.multByInt32 (delta_n * m2) 3 )
//////                let m4' = m4 + (term1 * delta_n2 * (n'*n' - 3*n' + 3) + 6 * delta_n2 * m2 - 4 * delta_n * m3)
//////                let m3' = m3 + (term1 * delta_n * (n' - 2) - 3 * delta_n * m2)
////                let m2' = term1

////                loop (n + 1) m1' m2' m3' m4' 
////            | false -> 
////                if (n > 1) then 
////                    createRunningStats n m1 m2 m3 m4 
////                else
////                    let nanU = zero / zero
////                    createRunningStats n nanU nanU nanU nanU
////        loop 0 zero zero zero zero
    

