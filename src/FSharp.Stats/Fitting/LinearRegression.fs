namespace FSharp.Stats.Fitting


(*

we estimate the relationship of one variable with another by expressing one in terms of a linear function of the other.
*)
module LinearRegression =    

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

            module Univariable =
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

            module Multivariable =           
                /// Caclualtes the coefficients for linear regression
                /// in the form of [|intercept; slope;|]
                let coefficients (x_data : Matrix<float>) (y_data : Vector<float>) =
                    if x_data.NumRows <> y_data.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let m = x_data.NumRows
                    let n = x_data.NumCols
                    let X = Matrix.init m (n+1) (fun m n ->  if n = 0 then 1. else x_data.[m,n-1] )
                    Algebra.LinearAlgebra.LeastSquares X y_data
                    
                /// Fit to x
                let fit (coef : Vector<float>) (x:Vector<float>) =
                    let tmp :Vector<float> = Vector.init (x.Length+1) (fun i -> if i = 0 then 1. else x.[i-1])
                    Vector.dot tmp coef 

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
                  