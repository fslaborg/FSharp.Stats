namespace FSharp.Stats


/// Module to compute common statistical measure
[<AutoOpen>]
module Seq = 

    let range (items:seq<_>) =
        use e = items.GetEnumerator()
        let rec loop (minimum) (maximum) =
            match e.MoveNext() with
            | true  -> loop (min e.Current minimum) (max e.Current maximum)
            | false -> Intervals.create minimum maximum          
        //Init by fist value
        match e.MoveNext() with
        | true  -> loop e.Current e.Current
        | false -> Intervals.Interval.Empty


    let rangeBy f (items:seq<_>) =
        use e = items.GetEnumerator()
        let rec loop minimum maximum minimumV maximumV =
            match e.MoveNext() with
            | true  -> 
                let current = f e.Current
                let mmin,mminV = if current < minimum then current,e.Current else minimum,minimumV
                let mmax,mmaxV = if current > maximum then current,e.Current else maximum,maximumV
                loop mmin mmax mminV mmaxV
            | false -> Intervals.create minimumV maximumV          
        //Init by fist value
        match e.MoveNext() with
        | true  -> 
            let current = f e.Current
            loop current current e.Current e.Current
        | false -> Intervals.Interval.Empty


    // #region means

    /// <summary>
    ///   Computes the population mean (Normalized by N)
    /// </summary>
    ///
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns default value if data is empty or if any entry is NaN.</remarks>
    /// <returns>population mean (Normalized by N)</returns>   
    let inline mean (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let rec loop n (acc) =
            match e.MoveNext() with
            | true  -> loop (n + 1 ) (acc + e.Current)
            | false -> if (n > 0) then LanguagePrimitives.DivideByInt< 'U > acc n else (zero / zero)      
        loop 0 LanguagePrimitives.GenericZero< 'U >


    /// <summary>
    ///   Computes the population mean (Normalized by N)s
    /// </summary>
    ///
    /// <param name="f">A function applied to transform each element of the sequence.</param>
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>population mean (Normalized by N)</returns>   
    let inline meanBy (f : 'T -> ^U) (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let rec loop n (acc) =
            match e.MoveNext() with
            | true  -> loop (n + 1 ) (acc + f e.Current)
            | false -> if (n > 0) then LanguagePrimitives.DivideByInt< 'U > acc n else (zero / zero)     
        loop 0 zero


    /// <summary>
    ///   Computes harmonic mean
    /// </summary>
    ///
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>harmonic mean</returns>   
    let inline meanHarmonic (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let one = LanguagePrimitives.GenericOne< 'U >
        let rec loop n (acc) =
            match e.MoveNext() with
            | true  -> loop (n + one ) (acc + (one / e.Current))
            | false -> if (LanguagePrimitives.GenericGreaterThan n zero) then (n / acc) else (zero / zero)
        loop zero zero 


    /// <summary>
    ///   Computes harmonic mean
    /// </summary>
    ///
    /// <param name="f">A function applied to transform each element of the sequence.</param>
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>harmonic mean</returns>   
    let inline meanHarmonicBy (f : 'T -> ^U) (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let one = LanguagePrimitives.GenericOne< 'U >
        let rec loop n (acc) =
            match e.MoveNext() with
            | true  -> loop (n + one ) (acc + (one / f e.Current))
            | false -> if (LanguagePrimitives.GenericGreaterThan n zero) then (n / acc) else (zero / zero)
        loop zero zero                 


    /// <summary>
    ///   Computes gemetric mean
    /// </summary>
    ///
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>gemetric mean</returns>   
    let inline meanGeometric (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let rec loop n (acc) =
            match e.MoveNext() with
            | true  -> loop (n + 1) (acc + log e.Current)
            | false -> 
                if (n > 0) then exp (LanguagePrimitives.DivideByInt< 'U > acc n) else (zero / zero)            
        loop 0 zero          
        

    /// <summary>
    ///   Computes gemetric mean
    /// </summary>
    ///
    /// <param name="f">A function applied to transform each element of the sequence.</param>
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>gemetric mean</returns>   
    let inline meanGeometricBy f (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let rec loop n (acc) =
            match e.MoveNext() with
            | true  -> loop (n + 1) (acc + log ( f e.Current ))
            | false -> 
                if (n > 0) then exp (LanguagePrimitives.DivideByInt< 'U > acc n) else (zero / zero)            
        loop 0 zero          

    


//    //GrandMean
//    // Computes the mean of the means of several subsample



    /// <summary>
    ///   Computes the truncated (trimmed) mean
    /// </summary>
    ///
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>truncated (trimmed) mean</returns>  
    let inline meanTruncated  (percent:float) (data:seq<'T>) : 'U  =
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let n = Seq.length(data)
        if (n > 0) then    
            let k = int (floor (float n * percent))
            data
            |> Seq.sort
            |> Seq.skip k
            |> Seq.take (n - k)
            |> mean

        else
            (zero / zero)  
                 
    
    /// <summary>
    ///   Computes the truncated (trimmed) mean
    /// </summary>
    ///
    /// <param name="items">The input sequence.</param>
    /// <param name="f">A function applied to transform each element of the sequence.</param>    
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>truncated (trimmed) mean</returns>  
    let inline meanTruncatedBy  (f : 'T -> ^U) (percent:float) (data:seq<'T>) : 'U  =
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let n = Seq.length(data)
        if (n > 0) then    
            let k = int (floor (float n * percent))
            data
            |> Seq.sort
            |> Seq.skip k
            |> Seq.take (n - k)
            |> meanBy f

        else
            (zero / zero)    



    // #endregion means

    // ##### ##### ##### ##### #####
    // Median 
    /// Sample Median
    let inline median (items:seq<'T>) : 'U  =
        raise (new System.NotImplementedException())

        


    // #region standard deviation, variance and coefficient of variation      

    /// <summary>
    ///   Computes the sample variance (Bessel's correction by N-1)
    /// </summary>
    ///
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>variance of a sample (Bessel's correction by N-1)</returns> 
    let inline var (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let rec loop n m1 m2 =
            match e.MoveNext() with
            | true  ->                         
                let n' = n + 1
                let delta  = e.Current - m1                                   
                let m1'    = m1 + (LanguagePrimitives.DivideByInt< 'U > delta n')
                let delta2   = e.Current - m1'
                let m2' = m2 + delta * delta2
                loop n' m1' m2'
            | false -> 
                if n > 1 then 
                    LanguagePrimitives.DivideByInt< 'U > m2 (n-1)
                else (zero / zero)
        loop 0 zero zero 


    /// <summary>
    ///   Computes the sample variance (Bessel's correction by N-1)
    /// </summary>
    ///
    /// <param name="f">A function applied to transform each element of the sequence.</param>
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>variance of a sample (Bessel's correction by N-1)</returns> 
    let inline varBy f (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let rec loop n m1 m2 =
            match e.MoveNext() with
            | true  ->                         
                let n' = n + 1
                let c = f e.Current
                let delta  = c - m1                                   
                let m1'    = m1 + (LanguagePrimitives.DivideByInt< 'U > delta n')
                let delta2   = c - m1'
                let m2' = m2 + delta * delta2
                loop n' m1' m2'
            | false -> 
                if n > 1 then 
                    LanguagePrimitives.DivideByInt< 'U > m2 (n-1)
                else (zero / zero)
        loop 0 zero zero 


    /// <summary>
    ///   Computes variance of the given values (denominator N)
    /// </summary>
    ///    
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>population variance estimator (denominator N)</returns> 
    let inline varPopulation (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let rec loop n m1 m2 =
            match e.MoveNext() with
            | true  ->                         
                let n' = n + 1
                let delta  = e.Current - m1                                   
                let m1'    = m1 + (LanguagePrimitives.DivideByInt< 'U > delta n')
                let delta2   = e.Current - m1'
                let m2' = m2 + delta * delta2
                loop n' m1' m2'
            | false -> 
                if n > 1 then 
                    LanguagePrimitives.DivideByInt< 'U > m2 n
                else (zero / zero)
        loop 0 zero zero 


    /// <summary>
    ///   Computes variance of the given values (denominator N)
    /// </summary>
    ///    
    /// <param name="f">A function applied to transform each element of the sequence.</param>
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>population variance estimator (denominator N)</returns> 
    let inline varPopulationBy f (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let rec loop n m1 m2 =
            match e.MoveNext() with
            | true  ->                         
                let n' = n + 1
                let c = f e.Current
                let delta  = c - m1                                   
                let m1'    = m1 + (LanguagePrimitives.DivideByInt< 'U > delta n')
                let delta2   = c - m1'
                let m2' = m2 + delta * delta2
                loop n' m1' m2'
            | false -> 
                if n > 1 then 
                    LanguagePrimitives.DivideByInt< 'U > m2 n 
                else (zero / zero)
        loop 0 zero zero 



    /// <summary>
    ///   Computes the sample standard deviation
    /// </summary>
    ///
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>standard deviation of a sample (Bessel's correction by N-1)</returns> 
    let inline stDev (items:seq<'T>) : 'U  =
        sqrt ( var items )


    /// <summary>
    ///   Computes the sample standard deviation
    /// </summary>
    ///
    /// <param name="f">A function applied to transform each element of the sequence.</param>
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>standard deviation of a sample (Bessel's correction by N-1)</returns> 
    let inline stDevBy f (items:seq<'T>) : 'U  =
        sqrt ( varBy f items )    


    /// <summary>
    ///   Computes the population standard deviation (denominator = N)
    /// </summary>
    ///
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>population standard deviation (denominator = N)</returns>     
    let inline stDevPopulation (items:seq<'T>) : 'U  =
        sqrt (varPopulation items)


    /// <summary>
    ///   Computes the population standard deviation (denominator = N)
    /// </summary>
    ///
    /// <param name="f">A function applied to transform each element of the sequence.</param>
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>population standard deviation (denominator = N)</returns>     
    let inline stDevPopulationBy f (items:seq<'T>) : 'U  =
        sqrt (varPopulationBy f items)
   














    /// <summary>
    ///   Computes the Coefficient of Variation of a sample (Bessel's correction by N-1)
    /// </summary>
    ///
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>Coefficient of Variation of a sample (Bessel's correction by N-1)</returns> 
    let inline cv (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let rec loop n m1 m2 =
            match e.MoveNext() with
            | true  ->                         
                let n' = n + 1
                let delta  = e.Current - m1                                   
                let m1'    = m1 + (LanguagePrimitives.DivideByInt< 'U > delta n')
                let delta2   = e.Current - m1'
                let m2' = m2 + delta * delta2
                loop n' m1' m2'
            | false -> 
                if n > 1 then 
                    let sd = sqrt ( LanguagePrimitives.DivideByInt< 'U > m2 (n-1) )
                    sd / m1
                else (zero / zero)
        loop 0 zero zero         


    /// <summary>
    ///   Computes the Coefficient of Variation of a sample (Bessel's correction by N-1)
    /// </summary>
    ///
    /// <param name="f">A function applied to transform each element of the sequence.</param>
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>Coefficient of Variation of a sample (Bessel's correction by N-1)</returns> 
    let inline cvBy f (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let rec loop n m1 m2 =
            match e.MoveNext() with
            | true  ->                         
                let n' = n + 1
                let current = f e.Current
                let delta  = current - m1                                   
                let m1'    = m1 + (LanguagePrimitives.DivideByInt< 'U > delta n')
                let delta2   = current - m1'
                let m2' = m2 + delta * delta2
                loop n' m1' m2'
            | false -> 
                if n > 1 then 
                    let sd = sqrt ( LanguagePrimitives.DivideByInt< 'U > m2 (n-1) )
                    sd / m1
                else (zero / zero)
        loop 0 zero zero   
        


    /// <summary>
    ///   Computes the Coefficient of Variation of the population (population standard deviation)
    /// </summary>
    ///
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>Coefficient of Variation of the population</returns> 
    let inline cvPopulation (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let rec loop n m1 m2 =
            match e.MoveNext() with
            | true  ->                         
                let n' = n + 1
                let delta  = e.Current - m1                                   
                let m1'    = m1 + (LanguagePrimitives.DivideByInt< 'U > delta n')
                let delta2   = e.Current - m1'
                let m2' = m2 + delta * delta2
                loop n' m1' m2'
            | false -> 
                if n > 1 then 
                    let sd = sqrt ( LanguagePrimitives.DivideByInt< 'U > m2 n )
                    sd / m1
                else (zero / zero)
        loop 0 zero zero   


    /// <summary>
    ///   Computes the Coefficient of Variation of the population (population standard deviation)
    /// </summary>
    ///
    /// <param name="f">A function applied to transform each element of the sequence.</param>
    /// <param name="items">The input sequence.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>Coefficient of Variation of the population</returns> 
    let inline cvPopulationBy f (items:seq<'T>) : 'U  =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'U > 
        let rec loop n m1 m2 =
            match e.MoveNext() with
            | true  ->                         
                let n' = n + 1
                let current = f e.Current
                let delta  = current - m1                                   
                let m1'    = m1 + (LanguagePrimitives.DivideByInt< 'U > delta n')
                let delta2   = current - m1'
                let m2' = m2 + delta * delta2
                loop n' m1' m2'
            | false -> 
                if n > 1 then 
                    let sd = sqrt ( LanguagePrimitives.DivideByInt< 'U > m2 n )
                    sd / m1
                else (zero / zero)
        loop 0 zero zero 


//    // #endregion standard deviation, variance and coefficient of variation
//    
//
//    /// <summary>
//    ///   Computes the covariance between two sequences of values    
//    /// </summary>
//    ///
//    /// <param name="mean1">mean of sequence 1</param>
//    /// <param name="mean2">mean of sequence 2</param>
//    /// <param name="items1">sequence 1 of float  </param>    
//    /// <param name="items2">sequence 2 of float  </param>
//    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>    
//    /// <returns>Covariance between two sequences of values</returns> 
//    let covarianceOfMeans mean1 mean2 (items1:seq<float>) (items2:seq<float>) = 
//        use e1 = items1.GetEnumerator()
//        use e2 = items2.GetEnumerator()
//        let rec loop n (acc:float) =
//            match e1.MoveNext(),e2.MoveNext() with
//            | true,true   -> loop (n + 1)     (acc + ((e1.Current - mean1) * (e2.Current - mean2)))
//            | false,false -> if (n > 1) then (acc / float n) else nan          
//            | _           -> raise (System.ArgumentException("Vectors need to have the same length."))    
//        loop 0 0.0         
//        
//
//    /// <summary>
//    ///   Computes the covariance between two sequences of values    
//    /// </summary>
//    ///
//    /// <param name="items1">sequence 1 of float  </param>    
//    /// <param name="items2">sequence 2 of float  </param>
//    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>    
//    /// <returns>Covariance between two sequences of values</returns> 
//    let covariance (items1:seq<float>) (items2:seq<float>) = 
//        let mean1 = mean items1
//        let mean2 = mean items2        
//        covarianceOfMeans mean1 mean2 items1 items2
//
//
//
//    /// <summary>
//    ///   Computes the unbiased covariance between two sequences of values (Bessel's correction by N-1)   
//    /// </summary>
//    ///
//    /// <param name="mean1">mean of sequence 1</param>
//    /// <param name="mean2">mean of sequence 2</param>
//    /// <param name="items1">sequence 1 of float  </param>    
//    /// <param name="items2">sequence 2 of float  </param>
//    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>    
//    /// <returns>Unbiased covariance between two sequences of values (Bessel's correction by N-1)</returns> 
//    let covarianceUnbaisedOfMeans mean1 mean2 (items1:seq<float>) (items2:seq<float>) = 
//        use e1 = items1.GetEnumerator()
//        use e2 = items2.GetEnumerator()
//        let rec loop n (acc:float) =
//            match e1.MoveNext(),e2.MoveNext() with
//            | true,true   -> loop (n + 1)     (acc + ((e1.Current - mean1) * (e2.Current - mean2)))
//            | false,false -> if (n > 1) then (acc / float (n - 1)) else nan          
//            | _           -> raise (System.ArgumentException("Vectors need to have the same length."))    
//        loop 0 0.0         
//        
//
//    /// <summary>
//    ///   Computes the unbiased covariance between two sequences of values (Bessel's correction by N-1)   
//    /// </summary>
//    ///
//    /// <param name="items1">sequence 1 of float  </param>    
//    /// <param name="items2">sequence 2 of float  </param>
//    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>    
//    /// <returns>Unbiased covariance between two sequences of values (Bessel's correction by N-1)</returns> 
//    let covarianceUnbaised (items1:seq<float>) (items2:seq<float>) = 
//        let mean1 = mean items1
//        let mean2 = mean items2        
//        covarianceUnbaisedOfMeans mean1 mean2 items1 items2
//
//
//    
//
//    /// <summary>
//    ///   Computes the Skewness for the given values.
//    /// </summary>
//    /// 
//    /// <param name="mean">mean of sequence of values</param> 
//    /// <param name="items">sequence of float</param> 
//    /// <remarks>
//    ///   Skewness characterizes the degree of asymmetry of a distribution
//    ///   around its mean. Positive skewness indicates a distribution with
//    ///   an asymmetric tail extending towards more positive values. Negative
//    ///   skewness indicates a distribution with an asymmetric tail extending
//    ///   towards more negative values.
//    /// </remarks>
//    let skewnessFromMean mean (items:seq<float>) =
//        use e = items.GetEnumerator()
//        let rec loop n (s2:float) (s3:float) =
//            match e.MoveNext() with
//            | true  -> let dev = e.Current - mean
//                       loop (n + 1) (s2 + dev * dev) (s3 + s2  * dev)
//            | false -> if (n > 1) then 
//                        let n = float n
//                        let m2 = s2 / n
//                        let m3 = s3 / n
//                        let g = m3 / (System.Math.Pow(m2, 3. / 2.0))
//                        let a = System.Math.Sqrt(n * (n - 1.));
//                        let b = n - 2.;
//                        (a / b) * g                       
//                       else
//                        nan            
//        loop 0 0.0 0.0
//    
//
//    /// <summary>
//    ///   Computes the Skewness for the given values.
//    /// </summary>
//    /// 
//    /// <param name="mean">mean of sequence of values</param> 
//    /// <param name="items">sequence of float</param> 
//    /// <remarks>
//    ///   Skewness characterizes the degree of asymmetry of a distribution
//    ///   around its mean. Positive skewness indicates a distribution with
//    ///   an asymmetric tail extending towards more positive values. Negative
//    ///   skewness indicates a distribution with an asymmetric tail extending
//    ///   towards more negative values.
//    /// </remarks>    
//    let skewness (items:seq<float>) =
//        let mean = mean items                  
//        skewnessFromMean mean items
//        
//
//
//
//    /// <summary>
//    ///   Computes the Skewness for the given population. (baised)
//    /// </summary>
//    /// 
//    /// <remarks>
//    ///   Skewness characterizes the degree of asymmetry of a distribution
//    ///   around its mean. Positive skewness indicates a distribution with
//    ///   an asymmetric tail extending towards more positive values. Negative
//    ///   skewness indicates a distribution with an asymmetric tail extending
//    ///   towards more negative values.
//    /// </remarks>
//    let skewnessPopulation (data:seq<float>) =
//        let n = float ( Seq.length data )
//        let mean = data |> mean
//        let (s2,s3) = Seq.fold (fun (s2,s3) v -> let dev = v - mean
//                                                 ((s2 + dev * dev),(s3 + s2  * dev))) (0.,0.) data
//        let m2 = s2 / n
//        let m3 = s3 / n
//        let g = m3 / (System.Math.Pow(m2, 3. / 2.0))
//        g
//
//
//    /// <summary>
//    ///   Computes the Kurtosis for the given values.
//    /// </summary>
//    /// 
//    /// <remarks>
//    ///   The framework uses the same definition used by default in SAS and SPSS.
//    /// </remarks>
//    // http://www.ats.ucla.edu/stat/mult_pkg/faq/general/kurtosis.htm
//    let kurtosis (data:seq<float>) =
//        let n = float ( Seq.length data )
//        let mean = data |> mean
//        let (s2,s4) = Seq.fold (fun (s2,s4) v -> let dev = v - mean
//                                                 ((s2 + dev * dev),(s4 + s2  * s2))) (0.,0.) data
//        let m2 = s2 / n
//        let m4 = s4 / n
//        
//        let v = s2 / (n - 1.)
//        let a = (n * (n + 1.)) / ((n - 1.) * (n - 2.) * (n - 3.))
//        let b = s4 / (v * v);
//        let c = ((n - 1.) * (n - 1.)) / ((n - 2.) * (n - 3.));
//
//        a * b - 3. * c
//
//
//    /// <summary>
//    ///   Computes the Kurtosis for the given population. (baised)
//    /// </summary>
//    /// 
//    /// <remarks>
//    ///   The framework uses the same definition used by default in SAS and SPSS.
//    /// </remarks>
//    // http://www.ats.ucla.edu/stat/mult_pkg/faq/general/kurtosis.htm
//    let kurtosisPopulation (data:seq<float>) =
//        let n = float ( Seq.length data )
//        let mean = data |> mean
//        let (s2,s4) = Seq.fold (fun (s2,s4) v -> let dev = v - mean
//                                                 ((s2 + dev * dev),(s4 + s2  * s2))) (0.,0.) data
//        let m2 = s2 / n
//        let m4 = s4 / n
//        
//        m4 / (m2 * m2) - 3.




//    /// Median absolute deviation
//    /// MAD
//    let medianAbsoluteDev (data:seq<float>) =
//        let median = MathNet.Numerics.Statistics.Statistics.Median(data)
//        let dev = data |> Seq.map (fun x -> abs(x-median))
//        MathNet.Numerics.Statistics.Statistics.Median(dev)
//
//
//    /// Average absolute deviation (Normalized by N)
//    let populationAverageDev (data) =        
//        let filterSeq =
//            data |> Seq.filter (fun x -> not (System.Double.IsNaN x))
//        if (Seq.length(filterSeq) > 0) then
//            let median = MathNet.Numerics.Statistics.Statistics.Median(filterSeq)
//            let dev = filterSeq |> Seq.map (fun x -> abs(x-median))
//            MathNet.Numerics.Statistics.Statistics.Mean(dev)
//        else nan
//
//    /// Average absolute deviation (Normalized by N-1)
//    let averageDev (data) =
//        let filterSeq =
//            data |> Seq.filter (fun x -> not (System.Double.IsNaN x))
//        if (Seq.length(filterSeq) > 0) then 
//            let median = MathNet.Numerics.Statistics.Statistics.Median(filterSeq)
//            let dev = filterSeq |> Seq.map (fun x -> abs(x-median))
//            let sumDev = dev |> Seq.sum
//            sumDev/(float(Seq.length(filterSeq)))
//        else nan    
//
//    

    /// Returns SummeryStats of deq with N, mean, sum-of-squares, minimum and maximum
    let inline stats (items:seq<'T>) =
        use e = items.GetEnumerator()
        let zero = LanguagePrimitives.GenericZero< 'T > 
        let one = LanguagePrimitives.GenericOne< 'T >        
        
        let rec loop n (minimum) (maximum) m1 m2 =
            match e.MoveNext() with
            | true  -> 
                let current  = e.Current
                let delta    = current - m1               
                let delta_n  = (delta / n)
                //let delta_n2 = delta_n * delta_n
                let m1'    = m1 + delta_n            
                let m2' = m2 + delta * delta_n * (n-one)
                loop (n + one) (min current minimum) (max current maximum) m1' m2'

            | false -> SummeryStats.createSummeryStats (n-one) m1 m2 minimum maximum

        //Init by fist value        
        match e.MoveNext() with
        | true -> loop one e.Current e.Current zero zero 
        | false ->
            let uNan = zero / zero 
            SummeryStats.createSummeryStats zero uNan uNan uNan uNan
































    // ########################################################################
    /// A module which implements helper functions to provide special statistical measures
    module UtilityFunctions =
        
        /// <summary>
        ///   Computes sum of squares
        /// </summary>
        ///
        /// <param name="items">seq of float</param>
        /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
        /// <returns>sum of squares</returns> 
        let sumOfSquares (xData:seq<float>) (exData:seq<float>) =
            let xX = Seq.zip xData exData 
            Seq.fold (fun acc (x,ex) -> acc + square (x - ex)) 0. xX


        /// <summary>
        ///   Computes the pooled variance of the given values
        /// </summary>
        /// 
        /// <param name="sizes">The number of samples</param>
        /// <param name="variances">The population variances for each samples.</param>
        let pooledVarOf (sizes:seq<int>) (variances:seq<float>) =            
            
            let var,n =
                Seq.zip variances sizes
                |> Seq.fold (fun (varAcc,nAcc) (variance,size) -> let n   = float size
                                                                  let var = variance * (n - 1.)
                                                                  (varAcc + var,nAcc + n)) (0., 0.)  
            var / n 


        /// <summary>
        ///   Computes the pooled variance of the given values
        /// </summary>
        let pooledVar (data:seq<#seq<float>>) =
            let sizes = data |> Seq.map Seq.length            
            
            let var,n =
                Seq.zip data sizes
                |> Seq.fold (fun (varAcc,nAcc) (sample,size) -> let n   = float size
                                                                let var = (varPopulation sample) * (n - 1.)
                                                                (varAcc + var,nAcc + n)) (0., 0.)  
            var / n 

        /// <summary>
        ///   Computes the pooled population variance of the given values (Bessel's correction by N-1)
        /// </summary>
        /// 
        /// <param name="sizes">The number of samples</param>
        /// <param name="variances">The population variances for each samples.</param>
        let pooledVarPopulationOf (sizes:seq<int>) (variances:seq<float>) =            
            
            let var,n =
                Seq.zip variances sizes
                |> Seq.fold (fun (varAcc,nAcc) (variance,size) -> let n   = float (size - 1)
                                                                  let var = variance * n
                                                                  (varAcc + var,nAcc + n)) (0., 0.)  
            var / n 


        /// <summary>
        ///   Computes the pooled population variance of the given values (Bessel's correction by N-1)
        /// </summary>
        let pooledVarPopulation (data:seq<#seq<float>>) =
            let sizes = data |> Seq.map Seq.length            
            
            let var,n =
                Seq.zip data sizes
                |> Seq.fold (fun (varAcc,nAcc) (sample,size) -> let n   = float (size - 1)
                                                                let var = (varPopulation sample) * n
                                                                (varAcc + var,nAcc + n)) (0., 0.)  
            var / n 


        /// <summary>
        ///   Computes the pooled standard deviation of the given values
        /// </summary>
        ///
        /// <param name="sizes">The number of samples</param>
        /// <param name="variances">The population variances for each samples.</param>       
        let pooledStDevOf (sizes:seq<int>) (variances:seq<float>) =  
            sqrt (pooledVarOf sizes variances)


        /// <summary>
        ///   Computes the pooled standard deviation of the given values.
        /// </summary>       
        let pooledStDev (data:seq<#seq<float>>) = 
            sqrt (pooledVar data)


        /// <summary>
        ///   Computes the pooled population standard deviation of the given values (Bessel's correction by N-1)
        /// </summary>
        ///
        /// <param name="sizes">The number of samples</param>
        /// <param name="variances">The population variances for each samples.</param>       
        let pooledStDevPopulationOf (sizes:seq<int>) (variances:seq<float>) =  
            sqrt (pooledVarPopulationOf sizes variances)


        /// <summary>
        ///   Computes the pooled population standard deviation of the given values (Bessel's correction by N-1)
        /// </summary>       
        let pooledStDevPopulation (data:seq<#seq<float>>) = 
            sqrt (pooledVarPopulation data)


//    // ########################################################################
//    /// A module which implements functional matrix operations.
//    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//    module Matrix =
//        
//        open MathNet.Numerics
//        open MathNet.Numerics.LinearAlgebra
//        open MathNet.Numerics.LinearAlgebra.Double
//
//        /// Returns the covariance matrix for the columns
//        let inline columnCovariance (A: #Matrix<float>) =
//            let rM = DenseMatrix(A.ColumnCount,A.ColumnCount)    
//            for (i,coli) in A.EnumerateColumnsIndexed() do
//                for (j,colj) in A.EnumerateColumnsIndexed() do 
//                    let cov = covariance coli colj
//                    rM.[i,j] <- cov
//                    rM.[j,i] <- cov
//            rM
//
//
//        /// Returns the covariance matrix for the rows
//        let inline rowCovariance (A: #Matrix<float>) =
//            let rM = DenseMatrix(A.RowCount,A.RowCount)    
//            for (i,rowi) in A.EnumerateRowsIndexed() do
//                for (j,rowj) in A.EnumerateRowsIndexed() do 
//                    let cov = covariance rowi rowj
//                    rM.[i,j] <- cov
//                    rM.[j,i] <- cov
//            rM
//
//
//        /// Returns the covariance matrix for the columns (Bessel's correction by N-1)
//        let inline columnCovarianceUnbaised (A: #Matrix<float>) =
//            let rM = DenseMatrix(A.ColumnCount,A.ColumnCount)    
//            for (i,coli) in A.EnumerateColumnsIndexed() do
//                for (j,colj) in A.EnumerateColumnsIndexed() do 
//                    let cov = covarianceUnbaised coli colj
//                    rM.[i,j] <- cov
//                    rM.[j,i] <- cov
//            rM
//
//
//        /// Returns the covariance matrix for the rows (Bessel's correction by N-1)
//        let inline rowCovarianceUnbaised (A: #Matrix<float>) =
//            let rM = DenseMatrix(A.RowCount,A.RowCount)    
//            for (i,rowi) in A.EnumerateRowsIndexed() do
//                for (j,rowj) in A.EnumerateRowsIndexed() do 
//                    let cov = covarianceUnbaised rowi rowj
//                    rM.[i,j] <- cov
//                    rM.[j,i] <- cov
//            rM
//
//
//        /// Returns mean over column
//        let inline columnMean (A: #Matrix<float>) =  
//            seq { for coli in A.EnumerateColumns() do
//                    yield mean coli }                
//
//
//        /// Returns mean over row
//        let inline rowMean (A: #Matrix<float>) =
//            seq { for rowi in A.EnumerateRows() do 
//                    yield mean rowi }
//
//        /// Returns range over row
//        let inline rowRange (A: #Matrix<float>) =
//            seq { for rowi in A.EnumerateRows() do 
//                    yield range rowi }
//
//
//        /// Returns range over column
//        let inline columnRange (A: #Matrix<float>) =
//            seq { for coli in A.EnumerateColumns() do 
//                    yield range coli }
//
//
//        
//    //  ##### #####
//    /// All descriptice stats function filters NaN and +/- inf values
//    module NaN =
//        
//        /// Computes the population mean (Normalized by N)
//        /// Removes NaN before calculation
//        let mean (data:seq<float>) =
//            let fdata = data |> Seq.Double.filterNanAndInfinity
//            mean fdata
//
//        /// Computes the median
//        /// Removes NaN before calculation
//        let median (data:seq<float>) =
//            let fdata = data |> Seq.Double.filterNanAndInfinity
//            median fdata            
//
//
//        /// Computes the population standard deviation (Normalized by N)
//        /// Removes NaN before calculation
//        let stDevPopulation data =
//            let fdata = data |> Seq.Double.filterNanAndInfinity
//            stDevPopulation fdata
//
//
//        /// Computes the baised population variance estimator (Normalized by N)
//        /// Removes NaN before calculation
//        let varPopulation data =
//            let fdata = data |> Seq.Double.filterNanAndInfinity
//            varPopulation fdata
//
