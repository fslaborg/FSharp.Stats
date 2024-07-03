namespace FSharp.Stats.Fitting


open System
open FSharp.Stats

/// <summary>
///   Linear regression is used to estimate the relationship of one variable (y) with another (x) by expressing y in terms of a linear function of x.
/// </summary>
module LinearRegression =

    //http://www.wolframalpha.com/input/?i=Vandermonde%20matrix&lk=1&a=ClashPrefs_%2aMathWorld.VandermondeMatrix-
    let internal vandermondeRow (order) (x:float) = 
        //DenseVector.OfEnumerable (seq { for i = 0 to order do yield x**(float i) })
        Vector.init (order+1) (fun i -> pown x i)

    let internal vandermondeMatrix (order) (vec : Vector<float>) =
        Matrix.init vec.Length (order+1) (fun m order -> pown vec.[m] order) 
        //Matrix. ofRowVector (vector [ for i = 0 to (vec.Count - 1) do yield (vandermondeRow order vec.[i]) ])
            
    /// <summary>
    ///   Polynomial coefficients with various properties are stored within this type.
    /// </summary>
    type Coefficients(coefficients: vector) =
        let n = coefficients.Length
        
        /// <summary>Contains polynomial coefficients as vector in the form of [constant; linear; quadratic; cubic].</summary>
        member this.Coefficients            = coefficients
        
        /// <summary>Number of polynomial coefficients (degree + 1).</summary>
        member this.Count                   = n
        
        /// <summary>Polynomial degree of the polynomial (coefficient count - 1)</summary>
        member this.Degree                  = n - 1
        
        /// <summary>Constant (first-degree) coefficient of the polynomial a + bx + cx^2 + dx^3 .. --&gt; a</summary>
        member this.Constant                = if n = 0 then 0. else coefficients.[0]
        
        /// <summary>Linear (second-degree) coefficient of the polynomial a + bx + cx^2 + dx^3 .. --&gt; b</summary>
        member this.Linear                  = if n < 2 then 0. else coefficients.[1]
        
        /// <summary>Quadratic (third-degree) coefficient of the polynomial a + bx + cx^2 + dx^3 .. --&gt; c</summary>
        member this.Quadratic               = if n < 3 then 0. else coefficients.[2]
        
        /// <summary>Cubic (fourth-degree) coefficient of the polynomial a + bx + cx^2 + dx^3 .. --&gt; d</summary>
        member this.Cubic                   = if n < 4 then 0. else coefficients.[3]
        
        /// <summary>Highest degree coefficient</summary>
        member this.Leading                 = if n = 0 then 0. else Seq.last coefficients
        
        /// <summary>Get coefficient of specified degree term.</summary>
        member this.getCoefficient degree   = coefficients.[degree]
        
        /// <summary>Get coefficient of specified degree term.</summary>
        member this.Item degree             = coefficients.[degree]
        
        /// <summary>Gets a x value and predicts the corresponding y value for the given polynomial coefficients.</summary>
        member this.Predict (x: float)      = 
            Vector.dot coefficients (vandermondeRow this.Degree x)
        
        /// <summary>Gets a x value vector and predicts the corresponding y value for the given polynomial coefficients.</summary>
        member this.Predict (x: vector)     = 
            let tmp = Vector.init (x.Length + 1) (fun i -> if i = 0 then 1. else x.[i-1])
            Vector.dot tmp coefficients
        
        /// <summary>Prints the polynomial function in a human readable form.</summary>
        override this.ToString()            = 
            let body = 
                coefficients 
                |> Seq.mapi (fun degree coefficient -> 
                    if degree = 0 then sprintf "%.3f" coefficient
                    elif degree = 1 then sprintf "%.3fx" coefficient
                    else sprintf "%.3fx^%i" coefficient degree
                    ) 
                |> String.concat " + " 
            "f(x) = " + body
        
        static member Init(coefficients) = Coefficients(coefficients)
        
        /// <summary>Initializes Coefficients type with an empty vector.</summary>
        static member Empty() = Coefficients(vector [])

    /// <summary>
    ///   Ordinary Least Squares (OLS) regression aims to minimise the sum of squared y intercepts between the original and predicted points at each x value.
    /// </summary>
    module OLS = 
          
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
                ///   // e.g. days since a certain event
                ///   let xData = vector [|0.;1.;2.;3.;4.;5.;|]
                ///   // some measured feature, that theoretically is zero at day 0
                ///   let yData = vector [|1.;5.;9.;13.;17.;18.;|]
                ///   
                ///   // Estimate the slope of the regression line.
                ///   let coefficients = 
                ///       LinearRegression.OLS.Linear.fit xData yData 
                /// </code> 
                /// </example>
                let fitOfVector (xData : Vector<float>) (yData : Vector<float>) =
                    if xData.Length <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let numerator   = Seq.zip xData yData |> Seq.sumBy (fun (x,y) -> x * y)
                    let denominator = xData |> Seq.sumBy (fun x -> x * x)
                    let slope = numerator / denominator
                    Coefficients(vector [0.;slope])

                [<Obsolete("Use RTO.fitOfVector instead.")>]
                let coefficientOfVector (xData : Vector<float>) (yData : Vector<float>) = 
                    (fitOfVector xData yData).Linear

                /// <summary>
                ///   Calculates the slope of a regression line through the origin  
                /// </summary>
                /// <param name="xData">vector of x values</param>
                /// <param name="yData">vector of y values</param>
                /// <returns>Slope of the regression line through the origin</returns>
                /// <example> 
                /// <code> 
                ///   // e.g. days since a certain event
                ///   let xData = vector [|0.;1.;2.;3.;4.;5.;|]
                ///   // some measured feature, that theoretically is zero at day 0
                ///   let yData = vector [|1.;5.;9.;13.;17.;18.;|]
                ///   
                ///   // Estimate the slope of the regression line.
                ///   let coefficients = 
                ///       LinearRegression.OLS.Linear.RTO.fit xData yData 
                ///   </code> 
                /// </example>
                let fit (xData : seq<float>) (yData : seq<float>) =
                    fitOfVector (vector xData) (vector yData)

                [<Obsolete("Use RTO.fit instead.")>]
                let coefficient (xData : Vector<float>) (yData : Vector<float>) = 
                    (fitOfVector xData yData).Linear
                
                /// <summary>
                ///   Returns the regression function of a line through the origin
                /// </summary>
                /// <param name="coef">The functions slope</param>
                /// <returns>Function that takes a x value and returns the predicted y value</returns>
                /// <example> 
                /// <code> 
                ///   let mySlope = 17.8
                ///   
                ///   // get the f�tting function that fits through the origin
                ///   let myF = 
                ///       LinearRegression.OLS.Linear.RTO.predictFunc mySlope
                ///   
                ///   // Returns predicted y value at x=6 using a line with intercept=0 and slope=17.8
                ///   myF 6.
                /// </code> 
                /// </example>
                let predictFunc (coef: Coefficients) =
                    fun x -> coef.Linear * x

                /// <summary>
                ///   Predicts the y value for a given slope and x value (intercept=0)
                /// </summary>
                /// <param name="coef">The functions slope</param>
                /// <param name="x">x value of which the corresponding y value should be predicted</param>
                /// <returns>predicted y value</returns>
                /// <example> 
                /// <code> 
                ///   let mySlope = 17.8
                ///   
                ///   // Returns predicted y value at x=6 using a line with intercept=0 and slope=17.8
                ///   LinearRegression.OLS.Linear.RTO.predict mySlope 6.
                /// </code> 
                /// </example>
                let predict (coef: Coefficients) (x: float) =            
                    coef.Linear * x

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
                ///   // e.g. days since a certain event
                ///   let xData = vector [|1.;2.;3.;4.;5.;6.|]
                ///   // e.g. some measured feature 
                ///   let yData = vector [|4.;7.;9.;10.;11.;15.|]
                ///   
                ///   // Estimate the coefficients of a straight line fitting the given data
                ///   let coefficients = 
                ///       Univariable.fit xData yData 
                /// </code> 
                /// </example>
                let fit (xData: Vector<float>) (yData: Vector<float>) =
                    if xData.Length <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let N = xData.Length
                    let X = Matrix.init N 2 (fun m x ->  if x = 0 then 1. else xData.[m] )
                    let coef = Algebra.LinearAlgebra.LeastSquares X yData
                    Coefficients(coef)
                    
                [<Obsolete("Use Univariable.fit instead.")>]
                let coefficient (xData : Vector<float>) (yData : Vector<float>) = 
                    (fit xData yData).Coefficients

                /// <summary>
                ///   Calculates the intercept and slope for a straight line fitting the data using Cholesky Decomposition. Linear regression minimizes the sum of squared residuals.
                /// </summary>
                /// <param name="xData">vector of x values</param>
                /// <param name="yData">vector of y values</param>
                /// <returns>vector of [intercept; slope]</returns>
                /// <example> 
                /// <code> 
                ///   // e.g. days since a certain event
                ///   let xData = vector [|1.;2.;3.;4.;5.;6.|]
                ///   // e.g. some measured feature 
                ///   let yData = vector [|4.;7.;9.;10.;11.;15.|]
                ///   
                ///   // Estimate the coefficients of a straight line fitting the given data
                ///   let coefficients = 
                ///       Univariable.fitCholesky xData yData 
                /// </code> 
                /// </example>
                let fitCholesky (xData: Vector<float>) (yData: Vector<float>) =
                    if xData.NumRows <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))

                    let X =
                        Matrix.init (xData.Length) 2 (fun i j -> if j = 0 then 1.0 else xData.[i])

                    let coef = Algebra.LinearAlgebra.LeastSquaresCholesky X yData
                    Coefficients(coef)

                
                [<Obsolete("Use Univariable.fitCholesky instead.")>]
                let coefficientCholesky (xData : Vector<float>) (yData : Vector<float>) = 
                    (fitCholesky xData yData).Coefficients

                /// <summary>
                ///   Calculates the intercept and slope for a straight line fitting the data and passing through a specified point (xC,yC) . Linear regression minimizes the sum of squared residuals.
                /// </summary>
                /// <param name="xData">vector of x values</param>
                /// <param name="yData">vector of y values</param>
                /// <returns>vector of [intercept; slope]</returns>
                /// <example> 
                /// <code> 
                ///   // e.g. days since a certain event
                ///   let xData = vector [|1.;2.;3.;4.;5.;6.|]
                ///   // e.g. some measured feature 
                ///   let yData = vector [|4.;7.;9.;10.;11.;15.|]
                ///   
                ///   // Estimate the coefficients of a straight line fitting the given data
                ///   let coefficients = 
                ///       Univariable.fitConstrained xData yData (6.,15.)
                /// </code> 
                /// </example>
                let fitConstrained (xData : Vector<float>) (yData : Vector<float>) ((xC,yC): float*float) =
                    let xTransformed = xData |> Vector.map (fun x -> x - xC)
                    let yTransformed = yData |> Vector.map (fun y -> y - yC)
                    let slope = (RTO.fitOfVector xTransformed yTransformed).Linear
                    let intercept = yC - xC * slope
                    Coefficients(vector [|intercept;slope|])

                
                [<Obsolete("Use Univariable.fitConstrained instead.")>]
                let coefficientConstrained (xData : Vector<float>) (yData : Vector<float>) ((xC,yC): float*float) = 
                    (fitConstrained xData yData (xC,yC)).Coefficients

                /// <summary>
                ///   Takes intercept and slope of simple linear regression to predict the corresponding y value.
                /// </summary>
                /// <param name="coef">vector of [intercept;slope] (e.g. determined by Univariable.coefficient)</param>
                /// <param name="x">x value of which the corresponding y value should be predicted</param>
                /// <returns>predicted y value with given coefficients at X=x</returns>
                /// <example> 
                /// <code> 
                ///   // e.g. days since a certain event
                ///   let xData = vector [|1.;2.;3.;4.;5.;6.|]
                ///   // e.g. some measured feature 
                ///   let yData = vector [|4.;7.;9.;10.;11.;15.|]
                ///   
                ///   // Estimate the coefficients of a straight line fitting the given data
                ///   let coefficients = 
                ///       Univariable.fit xData yData 
                ///   
                ///   // Predict the feature at midnight between day 1 and 2. 
                ///   Univariable.predict coefficients 1.5
                /// </code> 
                /// </example>
                let predict (coef : Coefficients) (x:float) =
                    if coef.Count <> 2 then
                        raise (System.ArgumentException("Coefficient has to be [a;b]!"))
                    coef.Constant + coef.Linear * x
        
                /// <summary>
                ///   Fits a model f(x) = b + m * x) to the data and returns the cooks distance for every data pair present in the
                ///   input collections as an estimator for the influence of each data point in coefficient estimation.
                /// </summary>
                /// <param name="xData">vector of x values</param>
                /// <param name="yData">vector of y values</param>
                /// <returns>Collection of cooks distances for every input coordinate.</returns>
                /// <example> 
                /// <code> 
                ///   // e.g. days since a certain event
                ///   let xData = vector [|1.;2.;3.;4.;5.;6.|]
                ///   // e.g. some measured feature 
                ///   let yData = vector [|4.;7.;9.;10.;11.;15.|]
                ///   
                ///   let distances = 
                ///       Univariable.cooksDistance xData yData 
                /// </code> 
                /// </example>
                let cooksDistance (xData : Vector<float>) (yData : Vector<float>) =
                    if xData.Length <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let N = xData.Length
                    let X = Matrix.init N 2 (fun m x ->  if x = 0 then 1. else xData.[m] )
                    let coeffs = Coefficients(Algebra.LinearAlgebra.LeastSquares X yData)
                    let leverages = Algebra.LinearAlgebra.leverage X
                    let yPred = Vector.map (predict coeffs) xData
                    let squaredDeviations = Vector.map2 (fun y yPr -> (y - yPr) ** 2.)  yPred yData 
                    let MSE = squaredDeviations |> Vector.sum |> fun sumOfSquares -> sumOfSquares / (float xData.Length)         
                    // compute cooksDistance for every Point in the dataSet
                    squaredDeviations 
                    |> Vector.mapi (fun i squaredDev -> 
                        let fstFactor = squaredDev / (MSE * float coeffs.Count)
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
                ///       let transformX (x) =
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
                    Coefficients(Algebra.LinearAlgebra.LeastSquares X yData)
                    
                [<Obsolete("Use Multivariable.fit instead.")>]
                let coefficients (xData : Matrix<float>) (yData : Vector<float>) = 
                    (fit xData yData).Coefficients

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
                ///       let transformX (x) =
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

                    Coefficients(Algebra.LinearAlgebra.LeastSquaresCholesky X yData)
                    
                [<Obsolete("Use Multivariable.fitCholesky instead.")>]
                let coefficientsCholesky (xData : Matrix<float>) (yData : Vector<float>) = 
                    (fitCholesky xData yData).Coefficients

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
                ///       let transformX (x) =
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
                let predict (coef: Coefficients) (x: Vector<float>) =
                    let tmp: Vector<float> = Vector.init (x.Length+1) (fun i -> if i = 0 then 1. else x.[i-1])
                    Vector.dot tmp coef.Coefficients 
            
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
 
                    Coefficients(w)
                    
                [<Obsolete("Use RidgeRegression.fit instead.")>]
                let coefficients lambda (xData : Matrix<float>) (yData : Vector<float>) = 
                    (fit lambda xData yData).Coefficients

                /// <summary>Fit to x</summary>
                /// <remarks></remarks>
                /// <param name="coef"></param>
                /// <param name="x"></param>
                /// <returns></returns>
                /// <example>
                /// <code>
                /// </code>
                /// </example>
                let predict (coef : Coefficients) (x:Vector<float>) =
                    let tmp :Vector<float> = Vector.init (x.Length+1) (fun i -> if i = 0 then 1. else x.[i-1])
                    Vector.dot tmp coef.Coefficients 

        /// <summary>
        ///   Linear regression using polynomials as regression function:  f(x) =  a + bx + cx^2 + ....
        /// </summary>
        module Polynomial =

            /// <summary>
            ///   Calculates the polynomial coefficients for polynomial regression. 
            /// </summary>
            /// <param name="order">order of the polynomial (1 = linear, 2 = quadratic, ... )</param>
            /// <param name="xData">vector of x values</param>
            /// <param name="yData">vector of y values</param>
            /// <returns>vector of polynomial coefficients sorted as [intercept;constant;quadratic;...]</returns>
            /// <example> 
            /// <code> 
            ///   // e.g. days since a certain event
            ///   let xData = vector [|1.;2.;3.;4.;5.;6.|]
            ///   // e.g. temperature measured at noon of the days specified in xData 
            ///   let yData = vector [|4.;7.;9.;8.;7.;9.;|]
            ///   
            ///   // Estimate the three coefficients of a quadratic polynomial.
            ///   let coefficients = 
            ///       LinearRegression.OLS.Polynomial.fit 2 xData yData 
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
                Coefficients(Algebra.LinearAlgebra.LeastSquares AtA Aty        )

            [<Obsolete("Use Polynomial.fit instead.")>]
            let coefficient order (xData : Vector<float>) (yData : Vector<float>) = 
                (fit order xData yData).Coefficients

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
            ///   // e.g. days since a certain event
            ///   let xData = vector [|1.;2.;3.;4.;5.;6.|]
            ///   // e.g. temperature measured at noon of the days specified in xData 
            ///   let yData = vector [|4.;7.;9.;8.;7.;9.;|]
            ///   
            ///   // Estimate the three coefficients of a quadratic polynomial.
            ///   let coefficients = 
            ///       LinearRegression.OLS.Polynomial.fit 2 xData yData 
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
                Coefficients(Algebra.LinearAlgebra.SolveLinearSystem A b)
                
            [<Obsolete("Use Polynomial.fitWithWeighting instead.")>]
            let coefficientsWithWeighting order (weighting : Vector<float>) (xData : Vector<float>) (yData : Vector<float>) = 
                (fitWithWeighting order weighting xData yData).Coefficients

            //takes vector of data with n>1 replicates and gives a vector of weightings based on the variance in measurements ( 1/var(i..j) )
            //only apply if y > 0 !
            //let getWeightingOfVariance numberOfReplicates (yData:Vector<float>) =
            //    let var =
            //        if yData.Length % numberOfReplicates = 0 then
            //            let length = yData.Length / numberOfReplicates
            //            let variance = vector [for i = 0 to length-1 do yield yData.[i * numberOfReplicates .. (i + 1) * numberOfReplicates - 1] |> Seq.var]
            //            variance
            //        else raise (System.ArgumentException("data length no multiple of replicate number!")) 
            //    Vector.init (yData.Length / numberOfReplicates) (fun i - 1. / var.[i])
                        
            /// <summary>
            ///   Takes polynomial coefficients and x value to predict the corresponding y value.
            /// </summary>
            /// <param name="order">order of the polynomial (1 = linear, 2 = quadratic, ... )</param>
            /// <param name="coef">vector of polynomial coefficients (e.g. determined by Polynomial.coefficients), sorted as [intercept;constant;quadratic;...]</param>
            /// <param name="x">x value of which the corresponding y value should be predicted</param>
            /// <returns>predicted y value with given polynomial coefficients at X=x</returns>
            /// <example> 
            /// <code> 
            ///   // e.g. days since a certain event
            ///   let xData = vector [|1.;2.;3.;4.;5.;6.|]
            ///   // e.g. temperature measured at noon of the days specified in xData 
            ///   let yData = vector [|4.;7.;9.;8.;7.;9.;|]
            ///   
            ///   // Define the polynomial coefficients.
            ///   let coefficients = 
            ///       LinearRegression.OLS.Polynomial.fit xData yData 
            ///   
            ///   // Predict the temperature value at midnight between day 1 and 2. 
            ///   LinearRegression.OLS.Polynomial.predict coefficients 1.5
            /// </code> 
            /// </example>
            /// <remarks>If all coefficients are nonzero, the order is equal to the length of the coefficient vector!</remarks>
            let predict (coef: Coefficients) (x: float) =
                Vector.dot coef.Coefficients (vandermondeRow coef.Degree x)

            /// <summary>
            ///   calculates derivative values at X=x with given polynomial coefficients. Level 1 = fst derivative; Level2 = snd derivative ...
            /// </summary>
            /// <param name="coef">vector of polynomial coefficients (e.g. determined by Polynomial.coefficients), sorted as [intercept;constant;quadratic;...]</param>
            /// <param name="level">depth of derivative: 1 = slope, 2 = curvature, ... </param>
            /// <param name="x">x value of which the corresponding y value should be predicted</param>
            /// <returns>predicted derivative with given polynomial coefficients at X=x</returns>
            /// <example> 
            /// <code> 
            ///   // e.g. days since a certain event
            ///   let xData = vector [|1.;2.;3.;4.;5.;6.|]
            ///   // e.g. temperature measured at noon of the days specified in xData 
            ///   let yData = vector [|4.;7.;9.;8.;7.;9.;|]
            ///   
            ///   // Estimate the polynomial coefficients
            ///   let coefficients = 
            ///       LinearRegression.OLS.Polynomial.fit xData yData 
            ///   
            ///   // Predict the curvature of the regression function at midnight between day 1 and 2. 
            ///   LinearRegression.OLS.Polynomial.getDerivative coefficients 2 1.5
            /// </code> 
            /// </example>
            let getDerivative (*(order: int)*) (coef: Coefficients) (level: int) (x: float) =
                let order = coef.Degree
                Array.init (order + 1) (fun i -> 
                    let factor = 
                        //[for l = 0 to (level - 1) do yield i-l] 
                        List.init level (fun l -> i-l)
                        |> List.filter (not << Ops.isNan)
                        |> List.fold (fun acc c -> acc * (float c)) 1.
                    factor * coef.Coefficients.[i] * (pown x (i-level))
                    )
                |> Array.filter (not << Ops.isNan)
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
            ///   // e.g. days since a certain event
            ///   let xData = vector [|1.;2.;3.;4.;5.;6.|]
            ///   // e.g. temperature measured at noon of the days specified in xData 
            ///   let yData = vector [|4.;7.;9.;8.;7.;9.;|]
            ///   
            ///   // Returns distances for a quadratic polynomial
            ///   let distances = 
            ///       LinearRegression.OLS.Polynomial.cooksDistance 2 xData yData 
            /// </code> 
            /// </example>
            let cooksDistance order (xData : Vector<float>) (yData : Vector<float>) =
                if xData.Length <> yData.Length then
                    raise (System.ArgumentException("vector x and y have to be the same size!"))
                let N = xData.Length
                let A = vandermondeMatrix order xData
                let coeffs = Coefficients(Algebra.LinearAlgebra.LeastSquares A yData)
                let leverages = Algebra.LinearAlgebra.leverage A
                let yPred = Vector.map (predict coeffs) xData
                let squaredDeviations = Vector.map2 (fun y yPr -> (y - yPr) ** 2.)  yPred yData 
                let MSE = squaredDeviations |> Vector.sum |> fun sumOfSquares -> sumOfSquares / (float xData.Length)         
                // compute cooksDistance for every Point in the dataSet
                squaredDeviations 
                |> Vector.mapi (fun i squaredDev -> 
                    let fstFactor = squaredDev / (MSE * float coeffs.Count)
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
            ///   // e.g. days since experiment start
            ///   let xData = vector [|1. .. 100.|]
            ///   // e.g. plant size in cm
            ///   let yData = vector [|4.;7.;8.;9.;7.;11.; ...|]
            ///   
            ///   // Estimate the intercept and slope of a line, that fits the data.
            ///   let coefficients = 
            ///       LinearRegression.RobustRegression.Linear.theilEstimator xData yData 
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

                Coefficients(vector [|intercept;slope|])

            /// <summary>
            ///   Calculates simple linear regression coefficients using the Theil-Sen estimator in the form of [|intercept; slope;|]. Performs well if outlier corrupt the regression line.
            /// </summary>
            /// <param name="xData">vector of x values</param>
            /// <param name="yData">vector of y values</param>
            /// <returns>vector of polynomial coefficients sorted as [intercept;constant;quadratic;...]</returns>
            /// <example> 
            /// <code> 
            ///   // e.g. days since experiment start
            ///   let xData = vector [|1. .. 100.|]
            ///   // e.g. plant size in cm
            ///   let yData = vector [|4.;7.;8.;9.;7.;11.; ...|]
            ///   
            ///   // Estimate the intercept and slope of a line, that fits the data.
            ///   let coefficients = 
            ///       LinearRegression.RobustRegression.Linear.theilSenEstimator xData yData 
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
            ///   // e.g. days since experiment start
            ///   let xData = vector [|1. .. 100.|]
            ///   // e.g. plant size in cm
            ///   let yData = vector [|4.;7.;8.;9.;7.;11.; ...|]
            ///   
            ///   // Estimate the intercept and slope of a line, that fits the data.
            ///   let coefficients = 
            ///       LinearRegression.RobustRegression.Linear.theilEstimator xData yData
            ///   
            ///   // Predict the size on day 10.5
            ///   LinearRegression.RobustRegression.Linear.predict coefficients 10.5 
            /// </code> 
            /// </example>
            /// <remarks>Equal to OLS.Linear.Univariable.predict!</remarks>
            let predict (coef: Coefficients) (x: float) = OLS.Linear.Univariable.predict coef x

            [<Obsolete("Use Linear.predict instead.")>]
            let fit coef x = 
                predict (Coefficients(coef)) x


    
/// <summary>
///   Defines if regression function should pass any specific point.
/// </summary>
/// <param name="'a">float*float coordinate</param>
type Constraint<'a> =
    /// <summary>No constraints are given.</summary>
    | Unconstrained
    /// <summary>The regression line must go through the origin (0,0)</summary>
    | RegressionThroughOrigin
    /// <summary>The regression line must go through a specified point, defined as float*float tuple ('xCorrdinate*'yCoordinate)</summary>
    /// <param name="'a">float*float coordinate</param>
    | RegressionThroughXY of 'a
    
/// <summary>
///   Defines method of slope estimation for robust line regression.
/// </summary>
type RobustEstimator = 
    /// <summary>Theils incomplete method</summary>
    | Theil
    /// <summary>Theil Sen estimator</summary>
    | TheilSen
    
/// <summary>
///   Defines regression method.
/// </summary>
type Method = 
    /// <summary>Fits a straight line through two-, or multidimensional data (OLS).</summary>
    | SimpleLinear 
    /// <summary>Fits a polynomial of the specified order (degree) to twodimensional data (OLS).</summary>
    | Polynomial of int
    /// <summary>Fits an outlier-insensitive straight line through twodimensional data (NOT OLS).</summary>
    | Robust of RobustEstimator

/// <summary>
///   This LinearRegression type summarized the most common fitting procedures.
/// </summary>
/// <returns>Either linear regression coefficients or a prediction function to map x values/vectors to its corresponding y value.</returns>
/// <example> 
/// <code> 
///   // e.g. days since experiment start
///   let xData = vector [|1. .. 100.|]
///   // e.g. plant size in cm
///   let yData = vector [|4.;7.;8.;9.;7.;11.; ...|]
///   
///   // Estimate the intercept and slope of a line, that fits the data.
///   let coefficientsSimpleLinear = 
///       LinearRegression.fit(xData,yData,FittingMethod=Fitting.Method.SimpleLinear,Constraint=Fitting.Constraint.RegressionThroughOrigin)
///   
///   // Predict the size on day 10.5
///   LinearRegression.predict(coefficientsSimpleLinear) 10.5
/// </code> 
/// </example>
type LinearRegression() = 

    /// <summary>
    ///   Determines coefficients for univariate linear regression.
    /// </summary>
    /// <param name="xData">vector of x values</param>
    /// <param name="yData">vector of y values</param>
    /// <param name="FittingMethod">Either Fitting.SimpleLinear (straight line), Fitting.Polynomial (polynomial), or Fitting.Robust (outlier insensitive straight line).</param>
    /// <param name="Constraint">Either Fitting.Unconstrained, Fitting.RegressionThroughOrigin, or Fitting.RegressionThroughXY (x,y) with x,y beeing coordinates that the line must pass.</param>
    /// <param name="Weighting">If a pointwise weight should be attached, a weight vector can be given, that is in the same order as x/y values.</param>
    /// <returns>Linear regression coefficients.</returns>
    /// <example> 
    /// <code> 
    ///   // e.g. days since experiment start
    ///   let xData = vector [|1.; 2.; 3.; 4.; 5.; 6. |]
    ///   // e.g. plant size in cm
    ///   let yData = vector [|4.; 7.; 8.; 9.; 7.; 11.|]
    ///   
    ///   // Estimate the intercept and slope of a line, that fits the data.
    ///   let coefficientsSimpleLinear = 
    ///       LinearRegression.fit(xData,yData,FittingMethod=Fitting.Method.SimpleLinear,Constraint=Fitting.Constraint.RegressionThroughXY (1.,2.))
    ///   
    ///   // Estimate the coefficients of a cubic polynomial that fits the data with the given point weighting.
    ///   let coefficientsPolynomial = 
    ///       LinearRegression.fit(xData, yData, FittingMethod=Fitting.Method.Polynomial 3, Weighting = vector [2.; 1.; 0.5.; 1.; 1.; 1.])
    ///   
    ///   // Estimate the intercept and slope of a line, that fits the data and ignore outliers.
    ///   let coefficientsRobust = 
    ///       LinearRegression.fit(xData,yData,FittingMethod=Fitting.Method.Robust Fitting.RobustEstimator.Theil)
    /// </code> 
    /// </example>
    /// <remarks>Default is simple linear regression fitting without constraints.</remarks>
    static member fit(xData: vector, yData, ?FittingMethod: Method, ?Constraint: Constraint<float*float>, ?Weighting: vector) = 

        let _constraint = defaultArg Constraint Unconstrained
        
        let _fittingMethod = defaultArg FittingMethod Method.SimpleLinear

        match _fittingMethod with 
        | Method.SimpleLinear ->
            match Weighting with 
            | None -> 
                match _constraint with
                | Constraint.Unconstrained ->
                    LinearRegression.OLS.Linear.Univariable.fit xData yData
                | Constraint.RegressionThroughOrigin -> 
                    LinearRegression.OLS.Linear.RTO.fit (vector xData) (vector yData)
                | Constraint.RegressionThroughXY coordinate -> 
                    LinearRegression.OLS.Linear.Univariable.fitConstrained (vector xData) (vector yData) coordinate
            | _ -> failwithf "Weighted simple linear regression is not yet implemented! Use polynomial weighted regression with degree 1 instead."

        | Method.Polynomial o -> 
            match _constraint with 
            | Constraint.Unconstrained -> 
                match Weighting with 
                | Some w -> LinearRegression.OLS.Polynomial.fitWithWeighting o w xData yData
                | None -> LinearRegression.OLS.Polynomial.fit o xData yData
            | _ -> failwithf "Constrained polynomial regression is not yet implemented!"

        | Method.Robust r -> 
            match _constraint with 
            | Constraint.Unconstrained -> 
                match Weighting with 
                | None -> 
                    match r with
                    | RobustEstimator.Theil -> LinearRegression.RobustRegression.Linear.theilEstimator xData yData
                    | RobustEstimator.TheilSen -> LinearRegression.RobustRegression.Linear.theilSenEstimator xData yData
                | Some w -> failwithf "Weighting for robust regression is not yet implemented!"
            | _ -> failwithf "Constrained robust regression is not yet implemented!"

    /// <summary>
    ///   Determines coefficients for multivariate linear regression.
    /// </summary>
    /// <param name="xData">matrix of x vectors</param>
    /// <param name="yData">vector of y values</param>
    /// <param name="FittingMethod">Multivariate regression currently just supports Fitting.SimpleLinear (straight line).</param>
    /// <returns>Linear regression coefficients for multivariate regression.</returns>
    /// <example> 
    /// <code> 
    ///   // x vectors
    ///   let xData = 
    ///       matrix [
    ///           [1.; 1. ;2.  ]
    ///           [2.; 0.5;6.  ]
    ///           [3.; 0.8;10. ]
    ///           [4.; 2. ;14. ]
    ///           [5.; 4. ;18. ]
    ///           [6.; 3. ;22. ]
    ///       ]
    ///   // measured feature
    ///   let yData = vector [107.5; 110.0; 115.7; 125.0; 137.5; 138.0]
    ///   
    ///   // Estimate linear coefficients for a straight line that fits the data.
    ///   let coefficientsSimpleLinear = 
    ///       LinearRegression.fit(xData, yData)
    /// </code> 
    /// </example>
    /// <remarks>Default is simple linear regression fitting without constraints.</remarks>
    static member fit(xData: matrix, yData, ?FittingMethod: Method) = 

        let _fittingMethod = defaultArg FittingMethod Method.SimpleLinear
        
        match _fittingMethod with 
        | Method.SimpleLinear ->
            LinearRegression.OLS.Linear.Multivariable.fit xData yData
        | _ -> failwithf "NYI"
        
    //static member predict(coeff: LinearRegression.Coefficients, ?FittingMethod: Method) =
    //    (fun (x: float) - 
    //        LinearRegression.OLS.Polynomial.predict coeff x)

    /// <summary>
    ///   Creates prediction function for linear regression.
    /// </summary>
    /// <param name="coeff">Linear regression coefficients (e.g. from LinearRegression.fit())</param>
    /// <param name="x">x value of which the corresponding y value should be predicted</param>
    /// <returns>Prediction function that takes an x value and predicts its corresponding y value.</returns>
    /// <example> 
    /// <code> 
    ///   // e.g. days since experiment start
    ///   let xData = vector [|1.; 2.; 3.; 4.; 5.; 6. |]
    ///   // e.g. plant size in cm
    ///   let yData = vector [|4.; 7.; 8.; 9.; 7.; 11.|]
    ///   
    ///   // Estimate the intercept and slope of a line, that fits the data.
    ///   let coefficientsSimpleLinear = 
    ///       LinearRegression.fit(xData,yData,FittingMethod=Fitting.Method.SimpleLinear,Constraint=Fitting.Constraint.RegressionThroughOrigin)
    ///   
    ///   // Predict the size on day 10.5
    ///   LinearRegression.predict(coefficientsSimpleLinear) 10.5
    /// </code> 
    /// </example>
    static member predict(coeff: LinearRegression.Coefficients) (xValue: float) =
        coeff.Predict xValue

    /// <summary>
    ///   Creates prediction function for multivariate linear regression.
    /// </summary>
    /// <param name="coeff">Multivariate linear regression coefficients (e.g. from LinearRegression.fit())</param>
    /// <param name="x">x value of which the corresponding y value should be predicted</param>
    /// <returns>Prediction function that takes an x vector and predicts its corresponding y value.</returns>
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
    ///       let transformX (x) =
    ///           x
    ///           |> Matrix.mapiRows (fun _ v -> 100. + (v.[0] * 2.5) + (v.[1] * 4.) + (v.[2] * 0.5))
    ///       xVectorMulti
    ///       |> transformX
    ///       |> vector
    ///   
    ///   // Estimate the intercept and slope of a line, that fits the data.
    ///   let coefficientsSimpleLinear = 
    ///       LinearRegression.fit(xVectorMulti,yVectorMulti,FittingMethod=Fitting.Method.SimpleLinear)
    ///   
    ///   // Predict the size on day 10.5
    ///   LinearRegression.predictMultivariate(coefficientsSimpleLinear) (vector [1.;2.;3.;])
    /// </code> 
    /// </example>
    static member predictMultivariate(coeff: LinearRegression.Coefficients) (xVector: vector) =
        coeff.Predict xVector
        