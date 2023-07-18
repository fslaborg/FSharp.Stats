namespace FSharp.Stats.Testing

open FSharp.Stats

module PostHoc =    

    open TestStatistics
        
    type Contrast = { Index            : int    
                      /// group mean difference
                      L                : float
                      DegreesOfFreedom : float
                      MeanSquares      : float
                      Significance     : float                      
                      Statistic        : float
                      SumOfSquares     : float
                      }
    
    let createContrast index l degreesOfFreedom meanSquares significance statistic sumOfSquares =
        {Index = index; L = l; DegreesOfFreedom = degreesOfFreedom; MeanSquares = meanSquares; Significance = significance; Statistic = statistic; SumOfSquares = sumOfSquares;}  


    // Ref.: Hays, William L. (1988). Statistics (4th ed.). New York: Holt, Rinehart, & Winston. (pp. 313–315)                                      
    let hays (contrastMatrix:float[][]) (data:float[][]) =

        let calcStats (sampleSizes:int[]) (sampleMeans:float[]) (contrast:float[]) =        
            let l           =  Array.fold2 (fun state mi ai -> state + (mi * ai)) 0.0 sampleMeans contrast 
            let denominator = (Array.map2 (fun a n -> a * a / (float n)) contrast sampleSizes) |> Array.sum
            (l * l / denominator,l)
        
        // Sample sizes
        let sizes = data |> Array.map (fun x -> x.Length)
        let totalSize = sizes |> Array.sum
        let groupCount = data.Length
        // Degrees of freedom
        let Db = float(groupCount - 1)
        let Dw = float(totalSize - groupCount)
        let Dt = groupCount * totalSize - 1
        

        // Step 1. Calculate the mean within each group
        let sampleMeans = data |> Array.map (fun x ->Array.average x )        
        // Step 2. Calculate the sum of squares contrast associated
        let ssc_l = contrastMatrix |> Array.map (fun ar -> calcStats sizes sampleMeans ar)
        let ssc = ssc_l |> Array.map (fun (ssc,l) -> ssc)
        let l = ssc_l |> Array.map (fun (ssc,l) -> l)
        // Step 3. Calculate the "within-group" sum of squares
        let Sw = data|> Array.mapi (fun i ar -> ar |> Array.fold (fun acc elem -> acc + ((elem-sampleMeans.[i])**2.0)) 0.0) |> Array.sum
        let MSw = Sw / Dw // within-group mean square or MSerror
        // Step 5. Calculate the F statistic per contrast
        Array.mapi2 (fun i sscV l' -> let fValue = sscV / MSw
                                      //printfn  "%f %b " fValue (nan.Equals(fValue))
                                      if isNan fValue then
                                        createContrast i l' Db MSw nan nan sscV 
                                      else
                                        let FTest = createFTest fValue 1. Dw                                         
                                        createContrast i l' Db MSw FTest.PValue FTest.Statistic sscV                                      
                                         
                    ) ssc l


    // https://web.mst.edu/~psyworld/tukeyssteps.htm
    // https://www.uvm.edu/~dhowell/gradstat/psych341/labs/Lab1/Multcomp.html
    // https://brownmath.com/stat/anova1.htm
    /// Tukey-Kramer approach
    let tukeyHSD (contrastMatrix:float[][]) (data:float[][]) =
        
        let calcStats (msw:float) (sampleSizes:int[]) (sampleMeans:float[]) (contrast:float[]) =   
            // Sum of means, that are taken into account by the current contrast (scaled by contrast values)
            // if -1 and 1 than l is the difference of the sample means, that should be compared
            let l =  
                Array.fold2 (fun state mi ai -> state + (mi * ai)) 0.0 sampleMeans contrast 
                |> System.Math.Abs
            // MS_error/n_i + MS_error/n_j
            let denominator = (Array.map2 (fun a n -> (abs a) * (msw / (float n))) contrast sampleSizes) |> Array.sum
            //returns the q statistic for studentized range table
            ((l / (sqrt (denominator/2.))),l)
            
        // Sample sizes
        let sizes = data |> Array.map (fun x -> x.Length)
        let totalSize = sizes |> Array.sum
        let groupCount = data.Length //k or r in studentized range distribution
        
        // Degrees of freedom
        let Db = float(groupCount - 1)
        let Dw = float(totalSize - groupCount) //v in studentized range distribution
        let Dt = groupCount * totalSize - 1

        // Step 1. Calculate the mean within each group
        let sampleMeans = data |> Array.map Seq.mean        

        // Step 2. Calculate the "within-group" sum of squares
        let Sw = data|> Array.mapi (fun i ar -> ar |> Array.fold (fun acc elem -> acc + ((elem-sampleMeans.[i])**2.0)) 0.0) |> Array.sum
        let MSw = Sw / Dw // within-group mean square or MSerror
    
        // Step 3. 
        let stats = contrastMatrix |> Array.map (fun ar -> calcStats MSw sizes sampleMeans ar)
    
        // Step 4. Calculate the q statistic per contrast
        Array.mapi  (fun i (qValue,l)  ->
                            if isNan qValue then
                                createContrast i l Dw MSw nan nan Sw  
                            else
                                let significance = 
                                    1. - Distributions.Continuous.StudentizedRange.CDF qValue (float groupCount) Dw 1. None true
                                createContrast i l Db MSw significance qValue Sw
                                      
                    ) stats

    /// Fisher's LSD. Sequential t tests with the variance estimated from all samples instead of the individual groups.
    /// FishersLSD requires ANOVA protection (apply ANOVA first). Not multiple testing corrected! Apply e.g. Benjamini-Hochberg method afterwards.
    let fishersLSD (contrastMatrix:float[][]) (data:float[][]) =

        let calcStats (msw:float) (sampleSizes:int[]) (sampleMeans:float[]) (contrast:float[]) =        
            // Sum of means, that are taken into account by the current contrast (scaled by contrast values)
            // if -1 and 1 than l is the difference of the sample means, that should be compared
            let meanDiff    =  Array.fold2 (fun state mi ai -> state + (mi * ai)) 0.0 sampleMeans contrast 
            // MS_errorWithin * (1/n_i + 1/n_j)
            let stError = (Array.map2 (fun a n -> (abs a) * (msw / (float n))) contrast sampleSizes) |> Array.sum
            // returns the t statistic
            (meanDiff / (sqrt stError)),meanDiff
            
        // Sample sizes
        let sizes = data |> Array.map (fun x -> x.Length)
        let totalSize = sizes |> Array.sum
        let groupCount = data.Length

        // Degrees of freedom
        let db = float(groupCount - 1)
        let dw = float(totalSize - groupCount)
        let dt = groupCount * totalSize - 1

        // Step 1. Calculate the mean within each group
        let sampleMeans = data |> Array.map Seq.mean        

        // Step 2. Calculate the "within-group" sum of squares
        let ssw = 
            data
            |> Array.mapi (fun i group -> 
                group 
                |> Array.sumBy (fun elem -> 
                    pown (elem - sampleMeans.[i]) 2
                    )
                ) 
            |> Array.sum
        // within-group mean square or MSerror
        let msw = ssw / dw 

        // Step 3. 
        let stats = contrastMatrix |> Array.map (fun ar -> calcStats msw sizes sampleMeans ar)

        // Step 4. Calculate the F statistic per contrast
        stats
        |> Array.mapi (fun i (tValue,meanDiff)  ->
            if isNan tValue then
                createContrast i meanDiff db msw nan nan ssw  
            else
                let tTest = createTTest tValue dw
                createContrast i meanDiff db msw tTest.PValue tTest.Statistic ssw 
            ) 

    type CriticalValueContrast = { 
        Index            : int    
        /// group mean difference
        L                : float
        DegreesOfFreedom : float
        MeanSquares      : float
        Significance     : bool
        Statistic        : float
        SumOfSquares     : float
        CriticalValue    : float
        } with static member create index l degreesOfFreedom meanSquares significance statistic sumOfSquares criticalValue=
                {Index = index; L = l; DegreesOfFreedom = degreesOfFreedom; MeanSquares = meanSquares; Significance = significance; Statistic = statistic; SumOfSquares = sumOfSquares; CriticalValue = criticalValue}  

    
    /// Dunnetts post hoc test compares groups to one control group (multiple-to-one comparison). 
    /// Returns if the groups defined in the contrast differ significantly (already multi comparison corrected for FWER &lt; a) 
    /// Critical value tables can be found in Testing.Tables
    let dunnetts (contrastMatrix:float[][]) (data:float[][]) (criticalTable:Matrix<float>) =
        // Sample sizes
        let sizes = data |> Array.map (fun x -> x.Length)
        let totalSize = sizes |> Array.sum
        let groupCount = data.Length
        if groupCount > 40 then failwithf "Not implemented for group count > 40"
        
        // Degrees of freedom
        let db = float(groupCount - 1)
        let dw = totalSize - groupCount
        
        // Step 1. Calculate the mean within each group
        let sampleMeans = data |> Array.map Seq.mean        
        
        // Step 2. Calculate the "within-group" sum of squares
        let ssw = 
            data
            |> Array.mapi (fun i group -> 
                group 
                |> Array.sumBy (fun elem -> 
                    pown (elem - sampleMeans.[i]) 2
                    )
                ) 
            |> Array.sum
        // within-group mean square or MSerror
        let msw = ssw / (float dw) 

        let calcStats i (sampleSizes:int[]) (sampleMeans:float[]) (contrast:float[]) =        
            let meanDifference =  Array.fold2 (fun state mi ai -> state + (mi * ai)) 0.0 sampleMeans contrast
            let denominator = (Array.map2 (fun a n -> (abs a) * (msw / (float n))) contrast sampleSizes) |> Array.sum                
            //n=df Error from ANOVA table (all measurements - groupcount), k=number of groups (excl. control)
            let t_d = criticalTable.[dw - 1,groupCount - 2]
            let critical = t_d * (sqrt denominator)
            CriticalValueContrast.create i meanDifference db msw ((abs meanDifference) > critical) (meanDifference / (sqrt denominator)) ssw t_d

        let stats = contrastMatrix |> Array.mapi (fun i ar -> calcStats i sizes sampleMeans ar)
        stats

    //let dunnetts095 contrastMatrix data criticalTable = 
    //    dunnetts contrastMatrix data dunnetts095
