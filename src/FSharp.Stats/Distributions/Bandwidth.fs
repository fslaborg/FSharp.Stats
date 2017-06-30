namespace FSharp.Stats.Distributions


/// Bandwidth selectors 
module Bandwidth =

    open FSharp.Stats    

    /// Compute the number of bins for a histogram
    module BinNumber =
        
        /// Compute the number of bins from bandwidth
        let fromBandwidth (min,max) bw =            
            ceil ((max - min) / bw)
            
        /// Square-root choice
        let sqrt ndataLength =
            sqrt ndataLength


        /// Sturges' formula is derived from a binomial distribution and implicitly assumes an approximately normal distribution.
        let sturges ndataLength =
            ceil (1. + log2 ndataLength)


        ///The Rice Rule is presented as a simple alternative to Sturges's rule.
        let riceRule ndataLength =            
            ceil (2. * (ndataLength ** (1./3.)))


    
    
    /// Calculates the bandwidth from min max and number of bins
    let inline fromBinNumber min max nBins =
        let tmp = (float nBins)
        if tmp < 0. then raise (System.ArgumentException("need at least 2 data points")) 
        else
            (max - min ) / tmp

    
    /// Simple bandwidth for histogram
    let forHistogram data =
        let data' =
            data
            |> Seq.filter (fun v -> not (isNan v))
            |> Seq.filter (fun v -> not (isInf v))

        let interval = Seq.range data' 
        let dmin,dmax = Intervals.values interval
        
        fromBinNumber dmin dmax (float(Seq.length(data)))
        

    /// Calcultes bandwidth according to Scott's normal reference rule
    let scottNormal (data:float[]) =
        let xLength = float data.Length
        if (xLength < 2. ) then raise (System.ArgumentException("need at least 2 data points"))
        let ssd = Seq.stDev data
        (3.5 * ssd) / (xLength ** (1./3.))


//    /// Calcultes bandwidth based on the Freedman–Diaconis rule
//    let freedmanDiaconis (data:float[]) =
//        let xLength = float data.Length
//        if (xLength < 2. ) then raise (System.ArgumentException("need at least 2 data points"))
//        let iqr = (Percentiles.IQR nist data) |> float
//        (2. * iqr ) / (1./3.)
//
//
//    //  It defaults to 0.9 times the minimum of the standard deviation and the interquartile range divided by 1.34 times
//    //  the sample size to the negative one-fifth power (= Silverman's ‘rule of thumb’, Silverman (1986, page 48, eqn (3.31)) unless the quartiles coincide when a positive result will be guaranteed.
//    /// Implements Silverman's ‘rule of thumb’ for choosing the bandwidth of a Gaussian kernel density estimator.
//    let nrd0 (x:float[]) =
//        let xLength = x.Length
//        if (xLength < 2 ) then raise (System.ArgumentException("need at least 2 data points"))
//        let hi = Seq.stDev x        
//        let iqrX = (Percentiles.IQR nist x |> float) / 1.34 // # qnorm(.75) - qnorm(.25) = 1.34898
//        let lo = let m = Array.min([|hi;iqrX|])
//                 if (m = 0.0)  then
//                    if hi <> 0. then
//                        hi
//                    elif abs(x.[0]) <> 0. then
//                        abs(x.[0])
//                    else
//                        1.
//                 else
//                    m
//        0.9 * lo * float(xLength)**(-0.2)


    //  Scott and Terrell, 1987 || Silverman, 1986
    //  implementaion according to R! bw.ucv 
    /// Least squares cross-validation of bandwidth (unbiased)
    let uLSCV (data:float[]) (numberOfBins:option<int>) =
        // <- bandwidth counts 
        let band_ucv_bin (h:float) (n:int) (nbin:int) (d:float) (x:int[]) =
            let nn = float n
            let sum = [0..nbin-1] |> Seq.sumBy (fun i -> let delta = (float(i) * d / h)**2.
                                                         let term  = exp(-delta / 4.) - sqrt(8.0) * exp(-delta / 2.)                                                 
                                                         term * float(x .[i]) )
            1. / (2. * nn * h * sqrt(System.Math.PI)) + sum / (nn * nn * h * sqrt(System.Math.PI))

        // <- 
        let n     = float(data.Length)
        let nb    = if numberOfBins.IsNone then (if n < 1000. then int n else 1000) else numberOfBins.Value
        let hmax  = 1.144 * sqrt (Seq.varPopulation(data)) * n**(-1./5.)    
        let lower = 0.1 * hmax
        let upper = hmax
        let tol   = 0.1 * lower
        //band_den_bin
        let d = let xmin = data |> Array.min
                let xmax = data |> Array.max
                let rang = (xmax - xmin) * 1.01
                rang / float(nb)
            
    
        let cnt = let tmpCnt :int[] = Array.zeroCreate data.Length
                  for i=1 to data.Length - 1 do
                    let ii = int(data.[i] / d)
                    for j=0 to i - 1 do
                        let jj = int(data.[j] / d)
                        let iij = (abs((ii - jj)))
                        tmpCnt.[iij] <- tmpCnt.[iij]  + 1
                  tmpCnt  

        let bandUCV (value:float) = band_ucv_bin (value) (data.Length) (nb) (d) (cnt)
        let optFunction = new System.Func<float,float>(bandUCV)
        
        MathNet.Numerics.RootFinding.Brent.FindRoot(optFunction, lower, upper)
        
