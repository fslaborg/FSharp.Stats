namespace FSharp.Stats.Fitting


(*

we estimate the relationship of one variable with another by expressing one in terms of a linear function of the other.
*)
module GoodnessOfFit =    
    open FSharp.Stats
  
    /// Three sum of squares 
    type SumOfSquares = {
        /// Regression sum of squares (SSR - explained) 
        Regression : float
        /// Error sum of squares (SSE - unexplained)
        Error      : float        
        /// Total sum of squares (SST - total)
        Total      : float        
        SSxx       : float
        SSxy       : float
        MeanX      : float
        MeanY      : float
        /// Count N
        Count      : float
        
        }
    
    let createSumOfSquares ssr sse sst ssxx ssxy meanX meanY count =
        {Regression=ssr; Error=sse; Total=sst; SSxx=ssxx; SSxy=ssxy; MeanX = meanX;MeanY =meanY; Count=count}
              
    ///  
    let calulcateSumOfSquares (fitFunc:float -> float)  (x_data : seq<float>) (y_data : seq<float>) = 
        let meanX = Seq.mean x_data
        let meanY = Seq.mean y_data
        let count,sst,sse,ssxx,ssxy =
            Seq.zip x_data y_data
            |> Seq.fold (fun (counter,stateSST,stateSSE,stateSSxx,stateSSxy) (x,y) -> 
                                           let exY   = fitFunc x
                                           let dSSe  = exY - y
                                           let dSSt  = y - meanY
                                           let dssxx = x - meanX 
                                           let ssxy  = dssxx * dSSt 
                                           (counter+1., stateSST + dSSt*dSSt, stateSSE + dSSe*dSSe, stateSSxx + dssxx*dssxx, stateSSxy + ssxy)
                        ) (0.,0.,0.,0.,0.)       
        createSumOfSquares (sst - sse) sse sst ssxx ssxy meanX meanY count

    /// Standard deviation of y(x) 
    // Square root of variance s2y,x
    let stDevY (sumOfSquares:SumOfSquares) =
        sumOfSquares.Error / (sumOfSquares.Count - 2.) |> sqrt

    /// Standard deviation of slope (beta)    
    let stDevSlope (sumOfSquares:SumOfSquares) =
        ( sumOfSquares.Error / (sumOfSquares.Count - 2.) ) / sumOfSquares.SSxx

    /// Standard deviation of intercept (alpha)
    let stDevIntercept (sumOfSquares:SumOfSquares) =
         let s2yx = sumOfSquares.Error / (sumOfSquares.Count - 2.) 
         let mx2  = sumOfSquares.MeanX*sumOfSquares.MeanX
         s2yx * ((1./sumOfSquares.Count) + (mx2/sumOfSquares.SSxx))
        

    let ttestSlope slope (sumOfSquares:SumOfSquares) =
        let sb = stDevSlope sumOfSquares |> sqrt
        let statistic =  slope / sb 
        let TTest = Testing.TestStatistics.createTTest statistic (sumOfSquares.Count - 2.)
        TTest

    let ttestIntercept intercept (sumOfSquares:SumOfSquares) =
        let si = stDevIntercept sumOfSquares |> sqrt
        let statistic =  intercept / si 
        let TTest = Testing.TestStatistics.createTTest statistic (sumOfSquares.Count - 2.)
        TTest

    let calulcateDetermination (sumOfSquares:SumOfSquares) =
        sumOfSquares.Regression / sumOfSquares.Total
        

    /// Gets the coefficient of determination, as known as the R-Squared (R²)
    (*
        ///    The coefficient of determination is used in the context of statistical models
        ///    whose main purpose is the prediction of future outcomes on the basis of other
        ///    related information. It is the proportion of variability in a data set that
        ///    is accounted for by the statistical model. It provides a measure of how well
        ///    future outcomes are likely to be predicted by the model.
        ///    
        ///    The R^2 coefficient of determination is a statistical measure of how well the
        ///    regression approximates the real data points. An R^2 of 1.0 indicates that the
        ///    regression perfectly fits the data.
    *)
    let calulcateDeterminationFromValue (actual:seq<float>) (expected:seq<float>) = 
        let meanY = Seq.mean actual
        let SSE,SST =
            Seq.zip actual expected
            |> Seq.fold (fun (stateEx,stateA) (ex,a) -> 
                                           let dSSe = ex - a
                                           let dSSt = ex - meanY
                                           (stateEx + dSSe*dSSe,stateA + dSSt*dSSt)
                        ) (0.,0.)
        ((SST - SSE) / SST)
 

    /// Calculates Akaike information criterion (AIC) which is a measure of the relative quality of a regression model for a given set of data    
    // ! Formula used for regression only (because number of model parameter are missing)
    // k = 2 for usual AIC
    let calcAIC k n sse = 
        n * (log (sse / n)) + (2. * k)


    /// Calculates Bayesian information criterion (BIC) which is a measure of the relative quality of a regression model for a given set of data
    // ! Formula used for regression only (because number of model parameter are missing)
    let calcBIC (k:float) n sse = 
        n * log (sse/n) + k * log (n) 
    
    /// Calculates the residuals
    let getResiduals (fitFunc:float -> float)  (x_data : Vector<float>) (y_data : Vector<float>) = 
        Seq.map2 (fun x y -> let y_estimated = fitFunc x
                             (y - y_estimated) ) x_data y_data

    /// Calculates SSE: sum of squares of errors
    /// also: unexplained sum of squares    
    let calculateSSE (fitFunc:float -> float)  (x_data : Vector<float>) (y_data : Vector<float>) = 
        Seq.map2 (fun x y -> let y_estimated = fitFunc x
                             (y - y_estimated) * (y - y_estimated) ) x_data y_data
        |> Seq.sum


    /// Calculates SST: sum of squares total
    /// also: total sum of squares
    let calculateSST (fitFunc:float -> float)  (x_data : Vector<float>) (y_data : Vector<float>) = 
        let meanY = Seq.mean y_data
        x_data |> Vector.map (fun x -> let y_estimated = fitFunc x
                                       (y_estimated - meanY) * (y_estimated - meanY) )
        |> Seq.sum


    /// explained = total - unexplained

    let calculateANOVA (order:int) (fitFunc:float -> float)  (x_data : Vector<float>) (y_data : Vector<float>) = 
        let meanY = Seq.mean y_data
        let sst,sse =
            Seq.zip x_data y_data
            |> Seq.fold (fun (stateSST,stateSSE) (x,a) -> 
                                           let ex    = fitFunc x
                                           let dSSe = ex - a
                                           let dSSt = a - meanY
                                           (stateSST + dSSt*dSSt,stateSSE + dSSe*dSSe)
                        ) (0.,0.)
//        let sst,sse =                                     
//            Seq.zip x_data y_data
//            |> Seq.fold (fun (accT,accE) (x,y) -> let y_estimated = fitFunc x
//                                                  let ssTotal = (y_estimated - meanY) * (y_estimated - meanY) 
//                                                  let ssError = (y - y_estimated) * (y - y_estimated)
//                                                  (accT + ssTotal,accE + ssError)
//                                                  ) (0.,0.)
        
        let dfR = float order
        let MSR = (sst-sse) / dfR
        
        let dfE = float (x_data.Length - order - 1)
        let MSE = sse / dfE

        let dfT = float (x_data.Length - 1)
        let MST = sst / dfT
        // MS regression / MS Residual
        let FTest = 
            try 
                Testing.TestStatistics.createFTest (MSR / MSE) dfR dfE                
            with 
                | _ as e -> 
                    Testing.TestStatistics.createFTest 0. dfR dfE
                    
        
        [| Testing.Anova.createAnovaVariationSource dfR MSR FTest.PValue Testing.Anova.VariationSource.Regression FTest.Statistic (sst-sse); 
           Testing.Anova.createAnovaVariationSource dfE MSE nan          Testing.Anova.VariationSource.Residual   nan sse;
           Testing.Anova.createAnovaVariationSource dfT MST nan          Testing.Anova.VariationSource.Total      nan sst;|]
       
    module OrdinaryLeastSquares =

        module Linear =

            module RTO = 
                /// 
                let calculateANOVA (coef : float) x y =                
                    calculateANOVA 1 (LinearRegression.OrdinaryLeastSquares.Linear.RTO.fitFunc coef) x y 

            module Univariable = 

                let calculateANOVA (coef : Vector<float>) (x_data) (y_data) =
                    if coef.Length <> 2 then
                        raise (System.ArgumentException("Coefficient has to be [a;b]!"))
                    let fitFunction x = coef.[0] + coef.[1] * x 
                    calculateANOVA 1 fitFunction x_data y_data 

        module Polynomial = 

            //http://www.wolframalpha.com/input/?i=Vandermonde%20matrix&lk=1&a=ClashPrefs_%2aMathWorld.VandermondeMatrix-
            let private vandermondeRow (order) (x:float) = 
                //DenseVector.OfEnumerable (seq { for i = 0 to order do yield x**(float i) })
                Vector.init (order+1) (fun i -> pown x i)        

            let calculateANOVA (order) (coef : Vector<float>) (x_data) (y_data) = 
                let fitFunction x = Vector.dot coef (vandermondeRow order x)
                calculateANOVA order fitFunction x_data y_data 
            
            
