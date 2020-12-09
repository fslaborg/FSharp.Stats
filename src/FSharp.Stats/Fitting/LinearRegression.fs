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
            
                /// Calculates the coefficients for linear regression through the origin 
                let coefficientOfVector (x : Vector<float>) (y : Vector<float>) =
                    if x.Length <> y.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let numerator   = Seq.zip x y |> Seq.sumBy (fun (x,y) -> x * y)
                    let denominator = x |> Seq.sumBy (fun x -> x * x)
                    numerator / denominator

                /// Calculates the coefficients for linear regression through the origin 
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
                /// Calculates the coefficients for linear regression
                /// in the form of [|intercept; slope;|]
                let coefficient (xData : Vector<float>) (yData : Vector<float>) =
                    if xData.Length <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let N = xData.Length
                    let X = Matrix.init N 2 (fun m x ->  if x = 0 then 1. else xData.[m] )
                    Algebra.LinearAlgebra.LeastSquares X yData

                /// Calculates the coefficients for linear regression through a specified point (xC,yC) 
                let coefficientConstrained (xData : Vector<float>) (yData : Vector<float>) ((xC,yC): float*float) =
                    let xTransformed = xData |> Vector.map (fun x -> x - xC)
                    let yTransformed = yData |> Vector.map (fun y -> y - yC)
                    let slope = RTO.coefficientOfVector xTransformed yTransformed
                    [|- xC * slope - yC;slope|]

                /// Fit to x
                let fit (coef : Vector<float>) (x:float) =
                    if coef.Length <> 2 then
                        raise (System.ArgumentException("Coefficient has to be [a;b]!"))
                    coef.[0] + coef.[1] * x
        
                /// Fits a model (y(x) = b + m * x) to the data and returns the cooks distance for every data pair present in the
                /// input collections as an estimator for the influence of each data point in coefficient estimation.  
                let cooksDistance (xData : Vector<float>) (yData : Vector<float>) =
                    if xData.Length <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let N = xData.Length
                    let X = Matrix.init N 2 (fun m x ->  if x = 0 then 1. else xData.[m] )
                    let coeffs = Algebra.LinearAlgebra.LeastSquares X yData
                    let leverages = Algebra.LinearAlgebra.leverage X
                    let yPred = Vector.map (fit coeffs) xData
                    let squaredDeviations = Vector.map2 (fun y yPr -> (y - yPr) ** 2.)  yPred yData 
                    let MSE = squaredDeviations |> Vector.sum |> fun sumOfSquares -> sumOfSquares / (float xData.Length)         
                    // compute cooksDistance for every Point in the dataSet
                    squaredDeviations 
                    |> FSharp.Stats.Vector.mapi (fun i squaredDev -> 
                                                    let fstFactor = squaredDev / (MSE * float coeffs.Length)
                                                    let sndFactor = leverages.[i] / ((1. - leverages.[i]) ** 2.)
                                                    fstFactor * sndFactor
                                                )

            module Multivariable =           
                /// Calculates the coefficients for linear regression
                /// in the form of [|intercept; slope;|]
                let coefficients (xData : Matrix<float>) (yData : Vector<float>) =
                    if xData.NumRows <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let m = xData.NumRows
                    let n = xData.NumCols
                    let X = Matrix.init m (n+1) (fun m n ->  if n = 0 then 1. else xData.[m,n-1] )
                    Algebra.LinearAlgebra.LeastSquares X yData
                    
                /// Fit to x
                let fit (coef : Vector<float>) (x:Vector<float>) =
                    let tmp :Vector<float> = Vector.init (x.Length+1) (fun i -> if i = 0 then 1. else x.[i-1])
                    Vector.dot tmp coef 
            
            module RidgeRegression =           

                let coefficients lambda (xData : Matrix<float>) (yData : Vector<float>) =
                    if xData.NumRows <> yData.Length then
                        raise (System.ArgumentException("vector x and y have to be the same size!"))
                    let m = xData.NumRows
                    let n = xData.NumCols
                    let X = Matrix.init m (n+1) (fun m n ->  if n = 0 then 1. else xData.[m,n-1] )
                    
                    let lambdaIdentity = lambda * Matrix.identity n
                    let sumDot = X.Transpose * X + lambdaIdentity
                    let theInverse = Algebra.LinearAlgebra.Inverse sumDot
                    let inverseXt = theInverse * X.Transpose
                    let w = inverseXt * yData
 
                    w

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
            
            /// Calculates the coefficients for polynomial regression
            let coefficient order (xData : Vector<float>) (yData : Vector<float>) =
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

            /// Calculates the coefficients for polynomial regression with given weighting
            let coefficientsWithWeighting order (weighting : Vector<float>) (xData : Vector<float>) (yData : Vector<float>) = 
                if xData.Length <> yData.Length || xData.Length <> weighting.Length then
                    raise (System.ArgumentException("vector x,y and weighting have to be the same size!"))
                let A = 
                    let includeWeighting weighting order =
                        Matrix.init (order + 1) (order + 1) (fun i j -> 
                                                                Vector.map2 (fun x w -> w * (pown x (i + j))) xData weighting 
                                                                |> Vector.sum
                                                            )
                    includeWeighting weighting order
                let b = 
                    Vector.init (order + 1) (fun i -> 
                                                Vector.map3 (fun x y w -> w * (pown x i) * y) xData yData weighting 
                                                |> Vector.sum
                                            )
                Algebra.LinearAlgebra.SolveLinearSystem A b   

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
                        
            /// Fit to x
            let fit (order) (coef : Vector<float>) (x:float) =            
                Vector.dot coef (vandermondeRow order x)

            ///gets derivative at x with given polynomial coefficients. Level1 = fst derivative; Level2 = smd derivative ...
            let getDerivative (*(order: int)*) (coef: Vector<float>) (level: int) (x: float) =
                let order = coef.Length - 1
                Array.init (order + 1) (fun i -> 
                    let factor = 
                        //[for l = 0 to (level - 1) do yield i-l] 
                        List.init level (fun l -> i-l)
                        |> List.filter (fun v -> not (nan.Equals(v)))
                        |> List.fold (fun acc c -> acc * (float c)) 1.
                    factor * coef.[i] * (pown x (i-level))
                    )
                |> Array.filter (fun v -> not (nan.Equals(v)))
                |> Array.sum
                
            /// Fits a polynomial model of user defined order to the data and returns the cooks distance for every data pair present in the
            /// input collections as an estimator for the influence of each data point in coefficient estimation.  
            let cooksDistance order (xData : Vector<float>) (yData : Vector<float>) =
                if xData.Length <> yData.Length then
                    raise (System.ArgumentException("vector x and y have to be the same size!"))
                let N = xData.Length
                let A = vandermondeMatrix order xData
                let coeffs = Algebra.LinearAlgebra.LeastSquares A yData
                let leverages = Algebra.LinearAlgebra.leverage A
                let yPred = Vector.map (fit order coeffs) xData
                let squaredDeviations = Vector.map2 (fun y yPr -> (y - yPr) ** 2.)  yPred yData 
                let MSE = squaredDeviations |> Vector.sum |> fun sumOfSquares -> sumOfSquares / (float xData.Length)         
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
    
    module RobustRegression =
        
        /// Simple linear regression y : x -> a + bx
        module Linear =

            //(http://195.134.76.37/applets/AppletTheil/Appl_Theil2.html)
            ///Calculates theil's incomplete method in the form of [|intercept; slope;|]
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


            ///Calculates the robust Theil-Sen estimator for linear regression in the form of [|intercept; slope;|]
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


            let fit = OrdinaryLeastSquares.Linear.Univariable.fit
