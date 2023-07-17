namespace FSharp.Stats.Fitting


open System
open FSharp.Stats

/// <summary>
///   Linear regression is used to estimate the relationship of one variable (y) with another (x) by expressing y in terms of a linear function of x.
/// </summary>
module LinearRegression =    
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
        
    /// <summary>
    ///   Ordinary Least Squares (OLS) regression aims to minimise the sum of squared y intercepts between the original and predicted points at each x value.
    /// </summary>
    module OrdinaryLeastSquares = 
          
        /// <summary>
        ///   Simple linear regression using straight lines:  f(x) =  a + bx.
        /// </summary>
        module Linear = 

            /// <summary>
            /// Fits straight regression lines through the origin f(x) = bx.
            /// </summary>
            module RTO =
            
                /// <summary>
                ///   Calculates the slope of a regression line through the origin  
                /// </summary>
                /// <param name="xData">vector of x values</param>
                /// <param name="yData">vector of y values</param>
                /// <returns>Slope of the regression line through the origin</returns>
                /// <example> 
                /// <code> 
                /// // e.g. days since a certain event
                /// let xData = vector [|0.;1.;2.;3.;4.;5.;|]
                /// // some measured feature, that theoretically is zero at day 0
                /// let yData = vector [|1.;5.;9.;13.;17.;18.;|]
                /// 
                /// // Estimate the slope of the regression line.
                /// let coefficients = 
                ///     LinearRegression.OrdinaryLeastSquares.Linear.fit xData yData 
                /// </code> 
                /// </example>
                let fitOfVector (xData : Vector<float>) (yData : Vector<float>) =
                    if xData.Length <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let numerator   = Seq.zip xData yData |> Seq.sumBy (fun (x,y) -> x * y)
                    let denominator = xData |> Seq.sumBy (fun x -> x * x)
                    let slope = numerator / denominator
                    slope
                    //Coefficient.Create (vector [0.;slope])

                [<Obsolete("Use RTO.fitOfVector instead.")>]
                let coefficientOfVector (xData : Vector<float>) (yData : Vector<float>) = 
                    fitOfVector xData yData

                /// <summary>
                ///   Calculates the slope of a regression line through the origin  
                /// </summary>
                /// <param name="xData">vector of x values</param>
                /// <param name="yData">vector of y values</param>
                /// <returns>Slope of the regression line through the origin</returns>
                /// <example> 
                /// <code> 
                /// // e.g. days since a certain event
                /// let xData = vector [|0.;1.;2.;3.;4.;5.;|]
                /// // some measured feature, that theoretically is zero at day 0
                /// let yData = vector [|1.;5.;9.;13.;17.;18.;|]
                /// 
                /// // Estimate the slope of the regression line.
                /// let coefficients = 
                ///     LinearRegression.OrdinaryLeastSquares.Linear.RTO.fit xData yData 
                /// </code> 
                /// </example>
                let fit (xData : seq<float>) (yData : seq<float>) =
                    fitOfVector (vector xData) (vector yData)

                [<Obsolete("Use RTO.fit instead.")>]
                let coefficient (xData : Vector<float>) (yData : Vector<float>) = 
                    fitOfVector xData yData
                
                /// <summary>
                ///   Returns the regression function of a line through the origin
                /// </summary>
                /// <param name="coef">The functions slope</param>
                /// <returns>Function that takes a x value and returns the predicted y value</returns>
                /// <example> 
                /// <code> 
                /// let mySlope = 17.8
                ///
                /// // get the fítting function that fits through the origin
                /// let myF = 
                ///     LinearRegression.OrdinaryLeastSquares.Linear.RTO.predictFunc mySlope
                ///
                /// // Returns predicted y value at x=6 using a line with intercept=0 and slope=17.8
                /// myF 6.
                /// </code> 
                /// </example>
                let predictFunc (coef: float) =
                    fun x -> coef * x

                /// <summary>
                ///   Predicts the y value for a given slope and x value (intercept=0)
                /// </summary>
                /// <param name="coef">The functions slope</param>
                /// <param name="x">x value of which the corresponding y value should be predicted</param>
                /// <returns>predicted y value</returns>
                /// <example> 
                /// <code> 
                /// let mySlope = 17.8
                ///
                /// // Returns predicted y value at x=6 using a line with intercept=0 and slope=17.8
                /// LinearRegression.OrdinaryLeastSquares.Linear.RTO.predict mySlope 6.
                /// </code> 
                /// </example>
                let predict (coef: float) (x:float) =            
                    coef * x

            /// <summary>
            ///   Univariable handles two dimensional x,y data.
            /// </summary>
            module Univariable =

                /// <summary>
                ///   Calculates the intercept and slope for a straight line fitting the data. Linear regression minimizes the sum of squared residuals.
                /// </summary>
                /// <param name="xData">vector of x values</param>
                /// <param name="yData">vector of y values</param>
                /// <returns>vector of [intercept; slope]</returns>
                /// <example> 
                /// <code> 
                /// // e.g. days since a certain event
                /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
                /// // e.g. some measured feature 
                /// let yData = vector [|4.;7.;9.;10.;11.;15.|]
                /// 
                /// // Estimate the coefficients of a straight line fitting the given data
                /// let coefficients = 
                ///     Univariable.fit xData yData 
                /// </code> 
                /// </example>
                let fit (xData : Vector<float>) (yData : Vector<float>) =
                    if xData.Length <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let N = xData.Length
                    let X = Matrix.init N 2 (fun m x ->  if x = 0 then 1. else xData.[m] )
                    Algebra.LinearAlgebra.LeastSquares X yData
                    
                [<Obsolete("Use Univariable.fit instead.")>]
                let coefficient (xData : Vector<float>) (yData : Vector<float>) = 
                    fit xData yData

                /// <summary>
                ///   Calculates the intercept and slope for a straight line fitting the data using Cholesky Decomposition. Linear regression minimizes the sum of squared residuals.
                /// </summary>
                /// <param name="xData">vector of x values</param>
                /// <param name="yData">vector of y values</param>
                /// <returns>vector of [intercept; slope]</returns>
                /// <example> 
                /// <code> 
                /// // e.g. days since a certain event
                /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
                /// // e.g. some measured feature 
                /// let yData = vector [|4.;7.;9.;10.;11.;15.|]
                /// 
                /// // Estimate the coefficients of a straight line fitting the given data
                /// let coefficients = 
                ///     Univariable.fitCholesky xData yData 
                /// </code> 
                /// </example>
                let fitCholesky (xData: Vector<float>) (yData: Vector<float>) =
                    if xData.NumRows <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))

                    let X =
                        Matrix.init (xData.Length) 2 (fun i j -> if j = 0 then 1.0 else xData.[i])

                    Algebra.LinearAlgebra.LeastSquaresCholesky X yData

                
                [<Obsolete("Use Univariable.fitCholesky instead.")>]
                let coefficientCholesky (xData : Vector<float>) (yData : Vector<float>) = 
                    fitCholesky xData yData

                /// <summary>
                ///   Calculates the intercept and slope for a straight line fitting the data and passing through a specified point (xC,yC) . Linear regression minimizes the sum of squared residuals.
                /// </summary>
                /// <param name="xData">vector of x values</param>
                /// <param name="yData">vector of y values</param>
                /// <returns>vector of [intercept; slope]</returns>
                /// <example> 
                /// <code> 
                /// // e.g. days since a certain event
                /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
                /// // e.g. some measured feature 
                /// let yData = vector [|4.;7.;9.;10.;11.;15.|]
                /// 
                /// // Estimate the coefficients of a straight line fitting the given data
                /// let coefficients = 
                ///     Univariable.fitConstrained xData yData (6.,15.)
                /// </code> 
                /// </example>
                let fitConstrained (xData : Vector<float>) (yData : Vector<float>) ((xC,yC): float*float) =
                    let xTransformed = xData |> Vector.map (fun x -> x - xC)
                    let yTransformed = yData |> Vector.map (fun y -> y - yC)
                    let slope = RTO.fitOfVector xTransformed yTransformed
                    [|- xC * slope - yC;slope|]

                
                [<Obsolete("Use Univariable.fitConstrained instead.")>]
                let coefficientConstrained (xData : Vector<float>) (yData : Vector<float>) = 
                    fitConstrained xData yData

                /// <summary>
                ///   Takes intercept and slope of simple linear regression to predict the corresponding y value.
                /// </summary>
                /// <param name="coef">vector of [intercept;slope] (e.g. determined by Univariable.coefficient)</param>
                /// <param name="x">x value of which the corresponding y value should be predicted</param>
                /// <returns>predicted y value with given coefficients at X=x</returns>
                /// <example> 
                /// <code> 
                /// // e.g. days since a certain event
                /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
                /// // e.g. some measured feature 
                /// let yData = vector [|4.;7.;9.;10.;11.;15.|]
                /// 
                /// // Estimate the coefficients of a straight line fitting the given data
                /// let coefficients = 
                ///     Univariable.fit xData yData 
                ///
                /// // Predict the feature at midnight between day 1 and 2. 
                /// Univariable.predict coefficients 1.5
                /// </code> 
                /// </example>
                let predict (coef : Vector<float>) (x:float) =
                    if coef.Length <> 2 then
                        raise (System.ArgumentException("Coefficient has to be [a;b]!"))
                    coef.[0] + coef.[1] * x
        
                /// <summary>
                ///   Fits a model f(x) = b + m * x) to the data and returns the cooks distance for every data pair present in the
                ///   input collections as an estimator for the influence of each data point in coefficient estimation.
                /// </summary>
                /// <param name="xData">vector of x values</param>
                /// <param name="yData">vector of y values</param>
                /// <returns>Collection of cooks distances for every input coordinate.</returns>
                /// <example> 
                /// <code> 
                /// // e.g. days since a certain event
                /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
                /// // e.g. some measured feature 
                /// let yData = vector [|4.;7.;9.;10.;11.;15.|]
                /// 
                /// let distances = 
                ///     Univariable.cooksDistance xData yData 
                /// </code> 
                /// </example>
                let cooksDistance (xData : Vector<float>) (yData : Vector<float>) =
                    if xData.Length <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let N = xData.Length
                    let X = Matrix.init N 2 (fun m x ->  if x = 0 then 1. else xData.[m] )
                    let coeffs = Algebra.LinearAlgebra.LeastSquares X yData
                    let leverages = Algebra.LinearAlgebra.leverage X
                    let yPred = Vector.map (predict coeffs) xData
                    let squaredDeviations = Vector.map2 (fun y yPr -> (y - yPr) ** 2.)  yPred yData 
                    let MSE = squaredDeviations |> Vector.sum |> fun sumOfSquares -> sumOfSquares / (float xData.Length)         
                    // compute cooksDistance for every Point in the dataSet
                    squaredDeviations 
                    |> Vector.mapi (fun i squaredDev -> 
                        let fstFactor = squaredDev / (MSE * float coeffs.Length)
                        let sndFactor = leverages.[i] / ((1. - leverages.[i]) ** 2.)
                        fstFactor * sndFactor
                    )

            /// <summary>
            ///   Multivariable handles multi dimensional data where a vector of independent x values should be used to predict a single y value.
            /// </summary>
            module Multivariable =           

                /// <summary>
                ///   Calculates the coefficients for a straight line fitting the data. Linear regression minimizes the sum of squared residuals.
                /// </summary>
                /// <param name="xData">matrix of x vectors</param>
                /// <param name="yData">vector of y values</param>
                /// <returns>vector of linear coefficients</returns>
                /// <example> 
                /// <code> 
                ///   let xVectorMulti =
                ///       [
                ///       [1.; 1. ;2.  ]
                ///       [2.; 0.5;6.  ]
                ///       [3.; 0.8;10. ]
                ///       [4.; 2. ;14. ]
                ///       [5.; 4. ;18. ]
                ///       [6.; 3. ;22. ]
                ///       ]
                ///       |> Matrix.ofJaggedSeq
                ///   
                ///   // Here the x values are transformed. In most cases the y values are just provided.
                ///   let yVectorMulti = 
                ///       let transformX (x:Matrix<float>) =
                ///           x
                ///           |> Matrix.mapiRows (fun _ v -> 100. + (v.[0] * 2.5) + (v.[1] * 4.) + (v.[2] * 0.5))
                ///       xVectorMulti
                ///       |> transformX
                ///       |> vector
                ///   
                ///   let coefficientsMV = 
                ///       Multivariable.fit xVectorMulti yVectorMulti
                /// </code> 
                /// </example>
                let fit (xData : Matrix<float>) (yData : Vector<float>) =
                    if xData.NumRows <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let m = xData.NumRows
                    let n = xData.NumCols
                    let X = Matrix.init m (n+1) (fun m n ->  if n = 0 then 1. else xData.[m,n-1] )
                    Algebra.LinearAlgebra.LeastSquares X yData
                    
                [<Obsolete("Use Multivariable.fit instead.")>]
                let coefficients (xData : Matrix<float>) (yData : Vector<float>) = 
                    fit xData yData

                /// <summary>
                /// <summary>
                ///   Calculates the coefficients for a straight line fitting the data using Cholesky Decomposition. Linear regression minimizes the sum of squared residuals.
                /// </summary>
                /// <param name="xData">matrix of x vectors</param>
                /// <param name="yData">vector of y values</param>
                /// <returns>vector of linear coefficients</returns>
                /// <example> 
                /// <code> 
                ///   let xVectorMulti =
                ///       [
                ///       [1.; 1. ;2.  ]
                ///       [2.; 0.5;6.  ]
                ///       [3.; 0.8;10. ]
                ///       [4.; 2. ;14. ]
                ///       [5.; 4. ;18. ]
                ///       [6.; 3. ;22. ]
                ///       ]
                ///       |> Matrix.ofJaggedSeq
                ///   
                ///   // Here the x values are transformed. In most cases the y values are just provided.
                ///   let yVectorMulti = 
                ///       let transformX (x:Matrix<float>) =
                ///           x
                ///           |> Matrix.mapiRows (fun _ v -> 100. + (v.[0] * 2.5) + (v.[1] * 4.) + (v.[2] * 0.5))
                ///       xVectorMulti
                ///       |> transformX
                ///       |> vector
                ///   
                ///   let coefficientsMV = 
                ///       Multivariable.fitCholesky xVectorMulti yVectorMulti
                /// </code> 
                /// </example>
                let fitCholesky (xData: Matrix<float>) (yData: Vector<float>) =
                    if xData.NumRows <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))

                    let X =
                        Matrix.init (xData.NumRows) (xData.NumCols + 1) 
                            (fun i j -> if j = 0 then 1.0 else xData.[i, j - 1])

                    Algebra.LinearAlgebra.LeastSquaresCholesky X yData
                    
                [<Obsolete("Use Multivariable.fitCholesky instead.")>]
                let coefficientsCholesky (xData : Matrix<float>) (yData : Vector<float>) = 
                    fitCholesky xData yData

                /// <summary>
                ///   Takes linear coefficients and x vector to predict the corresponding y value.
                /// </summary>
                /// <param name="coef">Coefficients from linear regression.</param>
                /// <param name="c">x vector for which the y value should be predicted</param>
                /// <returns>predicted y value with given coefficients at X=x</returns>
                /// <example> 
                /// <code> 
                ///   let xVectorMulti =
                ///       [
                ///       [1.; 1. ;2.  ]
                ///       [2.; 0.5;6.  ]
                ///       [3.; 0.8;10. ]
                ///       [4.; 2. ;14. ]
                ///       [5.; 4. ;18. ]
                ///       [6.; 3. ;22. ]
                ///       ]
                ///       |> Matrix.ofJaggedSeq
                ///   
                ///   // Here the x values are transformed. In most cases the y values are just provided.
                ///   let yVectorMulti = 
                ///       let transformX (x:Matrix<float>) =
                ///           x
                ///           |> Matrix.mapiRows (fun _ v -> 100. + (v.[0] * 2.5) + (v.[1] * 4.) + (v.[2] * 0.5))
                ///       xVectorMulti
                ///       |> transformX
                ///       |> vector
                ///   
                ///   let coefficientsMV = 
                ///       Multivariable.fit xVectorMulti yVectorMulti
                ///
                ///   let fittingFunctionMV x = 
                ///       Multivariable.predict coefficientsMV x
                /// </code> 
                /// </example>
                let predict (coef: Vector<float>) (x: Vector<float>) =
                    let tmp: Vector<float> = Vector.init (x.Length+1) (fun i -> if i = 0 then 1. else x.[i-1])
                    Vector.dot tmp coef 
            
            module RidgeRegression =           
                

                let fit lambda (xData : Matrix<float>) (yData : Vector<float>) =
                    if xData.NumRows <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let m = xData.NumRows
                    let n = xData.NumCols
                    let X = Matrix.init m (n+1) (fun m n ->  if n = 0 then 1. else xData.[m,n-1] )
                    
                    let lambdaIdentity = lambda .* Matrix.identity n
                    let sumDot = X.Transpose * X + lambdaIdentity
                    let theInverse = Algebra.LinearAlgebra.Inverse sumDot
                    let inverseXt = theInverse * X.Transpose
                    let w = inverseXt * yData
 
                    w
                    
                [<Obsolete("Use RidgeRegression.fit instead.")>]
                let coefficients lambda (xData : Matrix<float>) (yData : Vector<float>) = 
                    fit lambda xData yData

                /// Fit to x
                let predict (coef : Vector<float>) (x:Vector<float>) =
                    let tmp :Vector<float> = Vector.init (x.Length+1) (fun i -> if i = 0 then 1. else x.[i-1])
                    Vector.dot tmp coef 

        /// <summary>
        ///   Linear regression using polynomials as regression function:  f(x) =  a + bx + cx^2 + ....
        /// </summary>
        module Polynomial =

            //http://www.wolframalpha.com/input/?i=Vandermonde%20matrix&lk=1&a=ClashPrefs_%2aMathWorld.VandermondeMatrix-
            let private vandermondeRow (order) (x:float) = 
                //DenseVector.OfEnumerable (seq { for i = 0 to order do yield x**(float i) })
                Vector.init (order+1) (fun i -> pown x i)        

            let private vandermondeMatrix (order) (vec : Vector<float>) =        
                Matrix.init vec.Length (order+1) (fun m order -> pown vec.[m] order) 
                //Matrix. ofRowVector (vector [ for i = 0 to (vec.Count - 1) do yield (vandermondeRow order vec.[i]) ])
            
            /// <summary>
            ///   Calculates the polynomial coefficients for polynomial regression. 
            /// </summary>
            /// <param name="order">order of the polynomial (1 = linear, 2 = quadratic, ... )</param>
            /// <param name="xData">vector of x values</param>
            /// <param name="yData">vector of y values</param>
            /// <returns>vector of polynomial coefficients sorted as [intercept;constant;quadratic;...]</returns>
            /// <example> 
            /// <code> 
            /// // e.g. days since a certain event
            /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
            /// // e.g. temperature measured at noon of the days specified in xData 
            /// let yData = vector [|4.;7.;9.;8.;7.;9.;|]
            /// 
            /// // Estimate the three coefficients of a quadratic polynomial.
            /// let coefficients = 
            ///     LinearRegression.OrdinaryLeastSquares.Polynomial.fit 2 xData yData 
            /// </code> 
            /// </example>
            let fit order (xData : Vector<float>) (yData : Vector<float>) =
                if xData.Length <> yData.Length then
                    raise (System.ArgumentException("vector x and y have to be the same size!"))
                let N = xData.Length
                let A = vandermondeMatrix order xData
                // Least Squares of |y=A(x)*c| 
                //  tr(A)*y = tr(A)*A*c
                //  inv(tr(A)*A)*tr(A)*y = c        
                let AtA = A.Transpose * A
                let Aty = A.Transpose * yData
                Algebra.LinearAlgebra.LeastSquares AtA Aty        

            [<Obsolete("Use Polynomial.fit instead.")>]
            let coefficient order (xData : Vector<float>) (yData : Vector<float>) = 
                fit order xData yData

            /// <summary>
            ///   Calculates the polynomial coefficients for polynomial regression with a given point weighting. 
            /// </summary>
            /// <param name="order">order of the polynomial (1 = linear, 2 = quadratic, ... )</param>
            /// <param name="weighting">Vector of weightings that define the releveance of each point for fitting.</param>
            /// <param name="xData">vector of x values</param>
            /// <param name="yData">vector of y values</param>
            /// <returns>vector of polynomial coefficients sorted as [intercept;constant;quadratic;...]</returns>
            /// <example> 
            /// <code> 
            /// // e.g. days since a certain event
            /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
            /// // e.g. temperature measured at noon of the days specified in xData 
            /// let yData = vector [|4.;7.;9.;8.;7.;9.;|]
            /// 
            /// // Estimate the three coefficients of a quadratic polynomial.
            /// let coefficients = 
            ///     LinearRegression.OrdinaryLeastSquares.Polynomial.fit 2 xData yData 
            /// </code> 
            /// </example>
            let fitWithWeighting order (weighting : Vector<float>) (xData : Vector<float>) (yData : Vector<float>) = 
                if xData.Length <> yData.Length || xData.Length <> weighting.Length then
                    raise (System.ArgumentException("vector x,y and weighting have to be the same size!"))
                let A = 
                    Matrix.init 
                        (order + 1) 
                        (order + 1) 
                        (fun i j -> 
                            Vector.map2 (fun x w -> w * (pown x (i + j))) xData weighting 
                            |> Vector.sum
                        )
                let b = 
                    Vector.init 
                        (order + 1) 
                        (fun i -> 
                            Vector.map3 (fun x y w -> w * (pown x i) * y) xData yData weighting 
                            |> Vector.sum
                        )
                Algebra.LinearAlgebra.SolveLinearSystem A b   
                
            [<Obsolete("Use Polynomial.fitWithWeighting instead.")>]
            let coefficientsWithWeighting order (weighting : Vector<float>) (xData : Vector<float>) (yData : Vector<float>) = 
                fitWithWeighting order weighting xData yData

            /////takes vector of data with n>1 replicates and gives a vector of weightings based on the variance in measurements ( 1/var(i..j) )
            /////only apply if y > 0 !
            //let getWeightingOfVariance numberOfReplicates (yData:Vector<float>) =
            //    let var =
            //        if yData.Length % numberOfReplicates = 0 then
            //            let length = yData.Length / numberOfReplicates
            //            let variance = vector [for i = 0 to length-1 do yield yData.[i * numberOfReplicates .. (i + 1) * numberOfReplicates - 1] |> Seq.var]
            //            variance
            //        else raise (System.ArgumentException("data length no multiple of replicate number!")) 
            //    Vector.init (yData.Length / numberOfReplicates) (fun i -> 1. / var.[i])
                        
            /// <summary>
            ///   Takes polynomial coefficients and x value to predict the corresponding y value.
            /// </summary>
            /// <param name="order">order of the polynomial (1 = linear, 2 = quadratic, ... )</param>
            /// <param name="coef">vector of polynomial coefficients (e.g. determined by Polynomial.coefficients), sorted as [intercept;constant;quadratic;...]</param>
            /// <param name="x">x value of which the corresponding y value should be predicted</param>
            /// <returns>predicted y value with given polynomial coefficients at X=x</returns>
            /// <example> 
            /// <code> 
            /// // e.g. days since a certain event
            /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
            /// // e.g. temperature measured at noon of the days specified in xData 
            /// let yData = vector [|4.;7.;9.;8.;7.;9.;|]
            /// 
            /// // Define the polynomial coefficients.
            /// let coefficients = 
            ///     LinearRegression.OrdinaryLeastSquares.Polynomial.fit xData yData 
            /// 
            /// // Predict the temperature value at midnight between day 1 and 2. 
            /// LinearRegression.OrdinaryLeastSquares.Polynomial.predict coefficients 1.5
            /// </code> 
            /// </example>
            /// <remarks>If all coefficients are nonzero, the order is equal to the length of the coefficient vector!</remarks>
            let predict (order) (coef : Vector<float>) (x:float) =            
                Vector.dot coef (vandermondeRow order x)

            /// <summary>
            ///   calculates derivative values at X=x with given polynomial coefficients. Level 1 = fst derivative; Level2 = snd derivative ...
            /// </summary>
            /// <param name="coef">vector of polynomial coefficients (e.g. determined by Polynomial.coefficients), sorted as [intercept;constant;quadratic;...]</param>
            /// <param name="level">depth of derivative: 1 = slope, 2 = curvature, ... </param>
            /// <param name="x">x value of which the corresponding y value should be predicted</param>
            /// <returns>predicted derivative with given polynomial coefficients at X=x</returns>
            /// <example> 
            /// <code> 
            /// // e.g. days since a certain event
            /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
            /// // e.g. temperature measured at noon of the days specified in xData 
            /// let yData = vector [|4.;7.;9.;8.;7.;9.;|]
            /// 
            /// // Estimate the polynomial coefficients
            /// let coefficients = 
            ///     LinearRegression.OrdinaryLeastSquares.Polynomial.fit xData yData 
            /// 
            /// // Predict the curvature of the regression function at midnight between day 1 and 2. 
            /// LinearRegression.OrdinaryLeastSquares.Polynomial.getDerivative coefficients 2 1.5
            /// </code> 
            /// </example>
            let getDerivative (*(order: int)*) (coef: Vector<float>) (level: int) (x: float) =
                let order = coef.Length - 1
                Array.init (order + 1) (fun i -> 
                    let factor = 
                        //[for l = 0 to (level - 1) do yield i-l] 
                        List.init level (fun l -> i-l)
                        |> List.filter (not << isNan)
                        |> List.fold (fun acc c -> acc * (float c)) 1.
                    factor * coef.[i] * (pown x (i-level))
                    )
                |> Array.filter (not << isNan)
                |> Array.sum

            /// <summary>
            ///   Fits a polynomial model of user defined order to the data and returns the cooks distance for every data pair present in the
            ///   input collections as an estimator for the influence of each data point in coefficient estimation.  
            /// </summary>
            /// <param name="order">order of the polynomial (1 = linear, 2 = quadratic, ... )</param>
            /// <param name="xData">vector of x values</param>
            /// <param name="yData">vector of y values</param>
            /// <returns>returns collection of cooks distances for each data point</returns>
            /// <example> 
            /// <code> 
            /// // e.g. days since a certain event
            /// let xData = vector [|1.;2.;3.;4.;5.;6.|]
            /// // e.g. temperature measured at noon of the days specified in xData 
            /// let yData = vector [|4.;7.;9.;8.;7.;9.;|]
            ///
            /// // Returns distances for a quadratic polynomial
            /// let distances = 
            ///     LinearRegression.OrdinaryLeastSquares.Polynomial.cooksDistance 2 xData yData 
            /// </code> 
            /// </example>
            let cooksDistance order (xData : Vector<float>) (yData : Vector<float>) =
                if xData.Length <> yData.Length then
                    raise (System.ArgumentException("vector x and y have to be the same size!"))
                let N = xData.Length
                let A = vandermondeMatrix order xData
                let coeffs = Algebra.LinearAlgebra.LeastSquares A yData
                let leverages = Algebra.LinearAlgebra.leverage A
                let yPred = Vector.map (predict order coeffs) xData
                let squaredDeviations = Vector.map2 (fun y yPr -> (y - yPr) ** 2.)  yPred yData 
                let MSE = squaredDeviations |> Vector.sum |> fun sumOfSquares -> sumOfSquares / (float xData.Length)         
                // compute cooksDistance for every Point in the dataSet
                squaredDeviations 
                |> Vector.mapi (fun i squaredDev -> 
                    let fstFactor = squaredDev / (MSE * float coeffs.Length)
                    let sndFactor = leverages.[i] / ((1. - leverages.[i]) ** 2.)
                    fstFactor * sndFactor
                )



    /// <summary>
    ///   Robust regression does not necessarily minimize the distance of the fitting function to the input data points (least squares), but has alternative aims (non-parametric).
    /// </summary>
    module RobustRegression =
        
        /// <summary>
        ///   Simple linear regression using straight lines:  f(x) =  a + bx.
        /// </summary>
        module Linear =

            /// <summary>
            ///   Calculates simple linear regression coefficients using theil's incomplete method in the form of [|intercept; slope;|]. Performs well if outlier corrupt the regression line.
            /// </summary>
            /// <param name="xData">vector of x values</param>
            /// <param name="yData">vector of y values</param>
            /// <returns>vector of polynomial coefficients sorted as [intercept;constant;quadratic;...]</returns>
            /// <example> 
            /// <code> 
            /// // e.g. days since experiment start
            /// let xData = vector [|1. .. 100.|]
            /// // e.g. plant size in cm
            /// let yData = vector [|4.;7.;8.;9.;7.;11.; ...|]
            /// 
            /// // Estimate the intercept and slope of a line, that fits the data.
            /// let coefficients = 
            ///     LinearRegression.RobustRegression.Linear.theilEstimator xData yData 
            /// </code> 
            /// </example>
            /// <remarks>Not robust if data count is low! http://195.134.76.37/applets/AppletTheil/Appl_Theil2.html</remarks>
            let theilEstimator (xData: Vector<float>) (yData: Vector<float>)= 
                //sort data in ascending order (xData)
                let data =
                    Array.zip (Vector.toArray xData) (Vector.toArray yData)
                    |> Array.sortBy fst
                
                //low/high group. (If n is odd, the middle value is ignored)
                let (low,high) =
                    let length = data.Length

                    if length <= 1 then 
                        raise (System.ArgumentException("input vector is too small"))

                    match length % 2 with
                    | 1 -> data.[..(length / 2 - 1)],data.[(length / 2 + 1)..]
                    | _ -> data.[..(length / 2 - 1)],data.[(length / 2)..]

                let slope =
                    low
                    |> Array.mapi (fun i (xL,yL) -> 
                        let (xH,yH) = high.[i]
                        //calculate slope
                        (yH - yL) / (xH - xL)
                                )
                    |> FSharp.Stats.Array.median

                let intercept =
                    data
                    |> Array.map (fun (xV,yV) -> yV - (slope * xV))
                    |> FSharp.Stats.Array.median

                vector [|intercept;slope|]

            /// <summary>
            ///   Calculates simple linear regression coefficients using the Theil-Sen estimator in the form of [|intercept; slope;|]. Performs well if outlier corrupt the regression line.
            /// </summary>
            /// <param name="xData">vector of x values</param>
            /// <param name="yData">vector of y values</param>
            /// <returns>vector of polynomial coefficients sorted as [intercept;constant;quadratic;...]</returns>
            /// <example> 
            /// <code> 
            /// // e.g. days since experiment start
            /// let xData = vector [|1. .. 100.|]
            /// // e.g. plant size in cm
            /// let yData = vector [|4.;7.;8.;9.;7.;11.; ...|]
            /// 
            /// // Estimate the intercept and slope of a line, that fits the data.
            /// let coefficients = 
            ///     LinearRegression.RobustRegression.Linear.theilSenEstimator xData yData 
            /// </code> 
            /// </example>
            /// <remarks>Not robust if data count is low!</remarks>
            let theilSenEstimator (xData: Vector<float>) (yData: Vector<float>) =
                let xLength = xData.Length

                let indicesOfUniqueOccurences =
                    let rec loop acc i =
                        if i < xLength then 
                            let tmp = xData.[i]
                            let occurences =
                                xData
                                |> Seq.filter (fun xT -> tmp = xT)
                            if Seq.length occurences > 1 
                                then loop acc (i+1)
                            else loop (i::acc) (i+1)
                        else acc
                    loop [] 0

                let isolateUnique (data: Vector<float>) =
                    indicesOfUniqueOccurences
                    |> List.map (fun i -> data.[i])
                    |> vector

                let filteredXData = isolateUnique xData
                let filteredYData = isolateUnique yData
                theilEstimator filteredXData filteredYData


            /// <summary>
            ///   Takes vector of [intercept; slope] and x value to predict the corresponding y value
            /// </summary>
            /// <param name="coef">vector of coefficients, sorted as [intercept;slope]</param>
            /// <param name="x">x value of which the corresponding y value should be predicted</param>
            /// <returns>predicted y value with given coefficients at X=x</returns>
            /// <example> 
            /// <code> 
            /// // e.g. days since experiment start
            /// let xData = vector [|1. .. 100.|]
            /// // e.g. plant size in cm
            /// let yData = vector [|4.;7.;8.;9.;7.;11.; ...|]
            /// 
            /// // Estimate the intercept and slope of a line, that fits the data.
            /// let coefficients = 
            ///     LinearRegression.RobustRegression.Linear.theilEstimator xData yData
            ///
            /// // Predict the size on day 10.5
            /// LinearRegression.RobustRegression.Linear.predict coefficients 10.5 
            /// </code> 
            /// </example>
            /// <remarks>Equal to OrdinaryLeastSquares.Linear.Univariable.predict!</remarks>
            let predict coef x = OrdinaryLeastSquares.Linear.Univariable.predict coef x


            [<Obsolete("Use Linear.predict instead.")>]
            let fit coef x = 
                predict coef x
