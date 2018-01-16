namespace FSharp.Stats.Fitting


(*

we estimate the relationship of one variable with another by expressing one in terms of a linear function of the other.
*)
module Regression =    

    open FSharp.Stats
    
//    ///Least Squares Linear Regressio
//    module General =
//        // define our target functions
//        let f1 x = Math.Sqrt(Math.Exp(x))
//        let f2 x = SpecialFunctions.DiGamma(x*x)
//
//        // create data samples, with chosen parameters and with gaussian noise added
//        let fy (noise:IContinuousDistribution) x = 2.5*f1(x) - 4.0*f2(x) + noise.Sample()
//        let xdata = [ 1.0 .. 1.0 .. 10.0 ]
//        let ydata = xdata |> List.map (fy (Normal.WithMeanVariance(0.0,2.0)))
//
//        // build matrix form
//        let X =
//            [|
//                xdata |> List.map f1 |> vector
//                xdata |> List.map f2 |> vector
//            |] |> DenseMatrix.CreateFromColumns
//        let y = vector ydata
//
//        // solve
//        let p = X.QR().Solve(y)
//        let (a,b) = (p.[0], p.[1])
 
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
    let stDevY (sumOfSqures:SumOfSquares) =
        sumOfSqures.Error / (sumOfSqures.Count - 2.) |> sqrt

    /// Standard deviation of slope (beta)    
    let stDevSlope (sumOfSqures:SumOfSquares) =
        ( sumOfSqures.Error / (sumOfSqures.Count - 2.) ) / sumOfSqures.SSxx

    /// Standard deviation of intercept (alpha)
    let stDevIntercept (sumOfSqures:SumOfSquares) =
         let s2yx = sumOfSqures.Error / (sumOfSqures.Count - 2.) 
         let mx2  = sumOfSqures.MeanX*sumOfSqures.MeanX
         s2yx * ((1./sumOfSqures.Count) + (mx2/sumOfSqures.SSxx))
        

    let ttestSlope slope (sumOfSqures:SumOfSquares) =
        let sb = stDevSlope sumOfSqures |> sqrt
        let statistic =  slope / sb 
        let TTest = Testing.TestStatistics.createTTest statistic (sumOfSqures.Count - 2.)
        TTest

    let ttestIntercept intercept (sumOfSqures:SumOfSquares) =
        let si = stDevIntercept sumOfSqures |> sqrt
        let statistic =  intercept / si 
        let TTest = Testing.TestStatistics.createTTest statistic (sumOfSqures.Count - 2.)
        TTest

    let calulcateDetermination (sumOfSqures:SumOfSquares) =
        sumOfSqures.Regression / sumOfSqures.Total
        

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

    let private calculateANOVA (order:int) (fitFunc:float -> float)  (x_data : Vector<float>) (y_data : Vector<float>) = 
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
          
        /// Simple linear regression y : x -> a + bx
        module Linear = 
        
            /// Regression through the origin (y : x -> bx)
            module RTO =
            
                /// Caclualtes the coefficients for linear regression through the origin 
                let coefficientOfVector (x : Vector<float>) (y : Vector<float>) =
                    if x.Length <> y.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let numerator   = Seq.zip x y |> Seq.sumBy (fun (x,y) -> x * y)
                    let denominator = x |> Seq.sumBy (fun x -> x * x)
                    numerator / denominator

                /// Caclualtes the coefficients for linear regression through the origin 
                let coefficient (x : float list) (y : float list) =
                    coefficientOfVector (vector x) (vector y)
                
                /// Returns the regression function
                /// coefficient is beta only
                let fitFunc (coef: float) =            
                    fun x -> coef * x
                

                /// Fit to x
                /// coefficient is beta only
                let fit (coef: float) (x:float) =            
                    coef * x

                /// 
                let calculateANOVA (coef : float) x y =                
                    calculateANOVA 1 (fitFunc coef) x y 
      
            /// Caclualtes the coefficients for linear regression
            /// in the form of [|intercept; slope;|]
            let coefficient (x_data : Vector<float>) (y_data : Vector<float>) =
                if x_data.Length <> y_data.Length then
                    raise (System.ArgumentException("vector x and y have to be the same size!"))
                let N = x_data.Length
                let X = Matrix.init N 2 (fun m x ->  if x = 0 then 1. else x_data.[m] )
                Algebra.LinearAlgebra.LeastSquares X y_data
                    
            /// Fit to x
            let fit (coef : Vector<float>) (x:float) =
                if coef.Length <> 2 then
                    raise (System.ArgumentException("Coefficient has to be [a;b]!"))
                coef.[0] + coef.[1] * x

        
            let calculateANOVA (coef : Vector<float>) (x_data) (y_data) =
                if coef.Length <> 2 then
                    raise (System.ArgumentException("Coefficient has to be [a;b]!"))
                let fitFunction x = coef.[0] + coef.[1] * x 
                calculateANOVA 1 fitFunction x_data y_data 
    
            /// Fits a model (y(x) = b + m * x) to the data and returns the cooks distance for every data pair present in the
            /// input collections as an estimator for the influence of each data point in coefficient estimation.  
            let cooksDistance (x_data : Vector<float>) (y_data : Vector<float>) =
                if x_data.Length <> y_data.Length then
                    raise (System.ArgumentException("vector x and y have to be the same size!"))
                let N = x_data.Length
                let X = Matrix.init N 2 (fun m x ->  if x = 0 then 1. else x_data.[m] )
                let coeffs = Algebra.LinearAlgebra.LeastSquares X y_data
                let leverages = Algebra.LinearAlgebra.leverage X
                let yPred = Vector.map (fit coeffs) x_data
                let squaredDeviations = Vector.map2 (fun y yPr -> (y - yPr) ** 2.)  yPred y_data 
                let MSE = squaredDeviations |> Vector.sum |> fun sumOfSquares -> sumOfSquares / (float x_data.Length)         
                // compute cooksDistance for every Point in the dataSet
                squaredDeviations 
                |> FSharp.Stats.Vector.mapi (fun i squaredDev -> 
                                                let fstFactor = squaredDev / (MSE * float coeffs.Length)
                                                let sndFactor = leverages.[i] / ((1. - leverages.[i]) ** 2.)
                                                fstFactor * sndFactor
                                            )
                                   

        /// Simple polynomial regression
        module Polynomial =
            //http://www.wolframalpha.com/input/?i=Vandermonde%20matrix&lk=1&a=ClashPrefs_%2aMathWorld.VandermondeMatrix-
            let private vandermondeRow (order) (x:float) = 
                //DenseVector.OfEnumerable (seq { for i = 0 to order do yield x**(float i) })
                Vector.init (order+1) (fun i -> pown x i)        

            let private vandermondeMatrix (order) (vec : Vector<float>) =        
                Matrix.init vec.Length (order+1) (fun m order -> pown vec.[m] order) 
                //Matrix. ofRowVector (vector [ for i = 0 to (vec.Count - 1) do yield (vandermondeRow order vec.[i]) ])
            
            /// Caclualtes the coefficients for polynomial regression
            let coefficient order (x_data : Vector<float>) (y_data : Vector<float>) =
                if x_data.Length <> y_data.Length then
                    raise (System.ArgumentException("vector x and y have to be the same size!"))
                let N = x_data.Length
                let A = vandermondeMatrix order x_data
                // Least Squares of |y=A(x)*c| 
                //  tr(A)*y = tr(A)*A*c
                //  inv(tr(A)*A)*tr(A)*y = c        
                let AtA = A.Transpose * A
                let Aty = A.Transpose * y_data
                Algebra.LinearAlgebra.LeastSquares AtA Aty        
    
            /// Fit to x
            let fit (order) (coef : Vector<float>) (x:float) =            
                Vector.dot coef (vandermondeRow order x)

            let calculateANOVA (order) (coef : Vector<float>) (x_data) (y_data) = 
                let fitFunction x = Vector.dot coef (vandermondeRow order x)
                calculateANOVA order fitFunction x_data y_data 

            /// Fits a polynomial model of user defined order to the data and returns the cooks distance for every data pair present in the
            /// input collections as an estimator for the influence of each data point in coefficient estimation.  
            let cooksDistance order (x_data : Vector<float>) (y_data : Vector<float>) =
                if x_data.Length <> y_data.Length then
                    raise (System.ArgumentException("vector x and y have to be the same size!"))
                let N = x_data.Length
                let A = vandermondeMatrix order x_data
                let coeffs = Algebra.LinearAlgebra.LeastSquares A y_data
                let leverages = Algebra.LinearAlgebra.leverage A
                let yPred = Vector.map (fit order coeffs) x_data
                let squaredDeviations = Vector.map2 (fun y yPr -> (y - yPr) ** 2.)  yPred y_data 
                let MSE = squaredDeviations |> Vector.sum |> fun sumOfSquares -> sumOfSquares / (float x_data.Length)         
                // compute cooksDistance for every Point in the dataSet
                squaredDeviations 
                |> FSharp.Stats.Vector.mapi (fun i squaredDev -> 
                                                let fstFactor = squaredDev / (MSE * float coeffs.Length)
                                                let sndFactor = leverages.[i] / ((1. - leverages.[i]) ** 2.)
                                                fstFactor * sndFactor
                                            )
                                   
            // <summary>
            // Find the model parameters ? such that X*? with predictor X becomes as close to response Y as possible, with least squares residuals.
            // Uses a singular value decomposition and is therefore more numerically stable (especially if ill-conditioned) than the normal equations or QR but also slower.
            // </summary>            
                  