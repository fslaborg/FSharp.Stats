namespace FSharp.Stats.Fitting

open System
(*

we estimate the relationship of one variable with another by expressing one in terms of a linear function of the other.
*)
module NonLinearRegression =    
    open FSharp.Stats
    open FSharp.Stats.Algebra

    ///
    let standardErrorOfPrediction dOF (predicted:float []) (actual:float [])  =
        let n = actual.Length-1 |> float 
        match n with
        | x when x > dOF -> 
            let sumOfResSq = Array.fold2 (fun acc yReal yPred  -> acc + ((yPred-yReal)**2.) ) 0.0  actual predicted
            sqrt( (sumOfResSq / (n - dOF)))
        | _             -> -1.

    ///
    type Model = 
        {
        ParameterNames  : string []
        ///originally GetValue; contains function body
        GetFunctionValue    : (Vector<float> -> float -> float)
        ///Gradient: Vector of partial derivations of function body
        GetGradientValue        : (Vector<float> -> Vector<float> -> float -> Vector<float> )
        }
        
    ///
    let createModel parameterNames getFunctionValue getGradientValue = {
        ParameterNames = parameterNames; GetFunctionValue = getFunctionValue; GetGradientValue=getGradientValue }

    ///
    type SolverOptions = {
        MinimumDeltaValue: float
        MinimumDeltaParameters: float
        MaximumIterations: int
        InitialParamGuess: float []
        }

    ///
    let createSolverOption minimumDeltaValue minimumDeltaParameters maximumIterations initialParamGuess = {
        MinimumDeltaValue = minimumDeltaValue; MinimumDeltaParameters = minimumDeltaParameters; MaximumIterations = maximumIterations; InitialParamGuess=initialParamGuess}

    /// Returns the residual sum of squares (RSS) as a measure of discrepancy between the data and the used estimation model.
    let getRSS (model: Model) (xData: float[]) (yData: float []) (paramVector: Vector<float>) =
        let sumOfSquaredResiduals =
            Array.fold2 (fun acc xValue yValue ->  
                            let yValueEst = model.GetFunctionValue paramVector xValue
                            acc + ((yValueEst - yValue) ** 2.)
                        ) 0.0 xData yData
        
        sumOfSquaredResiduals

    let updateJacobianInplace (model: Model) (xData: float[]) (paramVector: Vector<float>) (jacobian: Matrix<float> ) =
        // Nr. of Parameters
        let paramCount = paramVector.Length
        // populate Jacobian Matrix
        for i = 0 to xData.Length-1 do 
            let gradient = Vector.zeroCreate paramCount
            model.GetGradientValue paramVector gradient xData.[i] |> ignore
            Matrix.setRow jacobian i gradient            
        jacobian

    /// Returns the residual vector, each row i contains the difference between the yEst_i and the yData_i. 
    let updateResidualVectorInPlace (model: Model) (xData: float[]) (yData: float []) (paramVector: Vector<float>) (residualVector: Vector<float>) = 
        for i = 0 to xData.Length-1 do 
            let yValueEst = model.GetFunctionValue paramVector xData.[i]
            residualVector.[i] <- (yValueEst - yData.[i])
        residualVector

    /// Returns true if convergence criteria are met or a user defined number of iiterations has been carried out
    let shouldTerminate (currentValueRSS: float) (newValueRSS: float) (iterationCount:int) (currentParamGuess:Vector<float>) 
            (newParamGuess:Vector<float>) (solverOptions: SolverOptions)  = 
        //abs (newValueRSS-currentValueRSS) <= solverOptions.MinimumDeltaValue ||
            Vector.sub newParamGuess currentParamGuess |> Vector.norm <= solverOptions.MinimumDeltaParameters ||
                iterationCount >= solverOptions.MaximumIterations 


    
    ///
    let solverConverged (solverOptions: SolverOptions) (estParams:ResizeArray<vector>) =
        solverOptions.MaximumIterations = estParams.Count 
        |> not     
        
    module GaussNewton = 
        /// Returns an collection of parameter vectors as a possible solution for linear least square based nonlinear fitting of a given dataset (xData, yData) with a given 
        /// model function. 
        let estimatedParamsVerbose (model: Model) (solverOptions: SolverOptions) (xData: float[]) (yData: float []) = 
            let paramsAtIteration = new ResizeArray<vector>()
            let initialParamGuess = Vector.ofArray solverOptions.InitialParamGuess
            let residualVector = Vector.zeroCreate xData.Length
            let jacobian = Matrix.zero xData.Length solverOptions.InitialParamGuess.Length
            let initialValueRSS = getRSS model xData yData initialParamGuess  
            let rec loop jacobian residualVector currentParamGuess currentValueRSS (paramsAtIteration:ResizeArray<vector>) = 
                let jacobian' = updateJacobianInplace model xData currentParamGuess jacobian 
                let residualVector' = updateResidualVectorInPlace model xData yData currentParamGuess residualVector
                let hessian = jacobian'.Transpose * jacobian' 
                let step = LinearAlgebra.LeastSquares hessian (Matrix.mulV (jacobian'.Transpose) residualVector')
                let newParamGuess = currentParamGuess - step
                let newValueRSS = getRSS model xData yData newParamGuess
                paramsAtIteration.Add(newParamGuess)     
                if shouldTerminate currentValueRSS newValueRSS paramsAtIteration.Count currentParamGuess newParamGuess solverOptions then 
                    paramsAtIteration
                else 
                    let currentParamGuess' = Vector.mapi (fun i _ -> newParamGuess.[i]) currentParamGuess
                    let currentValueRSS' = newValueRSS
                    loop jacobian' residualVector' currentParamGuess' currentValueRSS' paramsAtIteration
            loop jacobian residualVector initialParamGuess initialValueRSS paramsAtIteration
        
        /// Returns a parameter vector as a possible solution for linear least square based nonlinear fitting of a given dataset (xData, yData) with a given 
        /// model function. 
        let estimatedParams (model: Model) (solverOptions: SolverOptions) (xData: float[]) (yData: float []) = 
            let estParams = estimatedParamsVerbose model solverOptions xData yData
            estParams.[estParams.Count-1]

    module LevenbergMarquardt = 

        /// Returns an collection of parameter vectors as a possible solution for linear least square based nonlinear fitting of a given dataset (xData, yData) with a given 
        /// model function. 
        let estimatedParamsVerbose (model: Model) (solverOptions: SolverOptions) lambdaInitial lambdaFactor (xData: float[]) (yData: float []) = 
            let paramsAtIteration = new ResizeArray<vector>()
            let initialParamGuess = Vector.ofArray solverOptions.InitialParamGuess
            let residualVector = Vector.zeroCreate xData.Length
            let jacobian = Matrix.zero xData.Length solverOptions.InitialParamGuess.Length
            let initialValueRSS = getRSS model xData yData initialParamGuess  
            let rec loop lambda jacobian residualVector currentParamGuess currentValueRSS (paramsAtIteration:ResizeArray<vector>) = 
                let jacobian' = updateJacobianInplace model xData currentParamGuess jacobian 
                let residualVector' = updateResidualVectorInPlace model xData yData currentParamGuess residualVector
                let hessian = jacobian'.Transpose * jacobian' 
                let diagonal = Matrix.initDiagonal (Vector.map (fun x -> ((lambda)*x)) hessian.Diagonal)
                let modHessian = (hessian + diagonal) 
                let step = FSharp.Stats.Algebra.LinearAlgebra.SolveLinearSystem modHessian (Matrix.mulV (jacobian'.Transpose) residualVector')
                let newParamGuess = currentParamGuess - step
                let newValueRSS = getRSS model xData yData newParamGuess
                paramsAtIteration.Add(newParamGuess)     
                if shouldTerminate currentValueRSS newValueRSS paramsAtIteration.Count currentParamGuess newParamGuess solverOptions then 
                    paramsAtIteration
                elif newValueRSS < currentValueRSS then
                    let lambda' = lambda / lambdaFactor
                    let currentParamGuess' = Vector.mapi (fun i _ -> newParamGuess.[i]) currentParamGuess
                    let currentValueRSS' = newValueRSS
                    loop lambda' jacobian' residualVector' currentParamGuess' currentValueRSS' paramsAtIteration
                else
                    let lambda' = lambda * lambdaFactor
                    loop lambda' jacobian' residualVector' currentParamGuess currentValueRSS paramsAtIteration
            loop lambdaInitial jacobian residualVector initialParamGuess initialValueRSS paramsAtIteration

        /// Returns a parameter vector as a possible solution for least square based nonlinear fitting of a given dataset (xData, yData) with a given 
        /// model function. 
        let estimatedParams (model: Model) (solverOptions: SolverOptions) lambdaInitial lambdaFactor (xData: float[]) (yData: float []) = 
            let estParams = estimatedParamsVerbose model solverOptions  lambdaInitial lambdaFactor xData yData
            estParams.[estParams.Count-1]
    

    /// This LevenbergMarquardt implementation supports the usage of box constrains. 
    // To achieve this the parameters are mapped between internal and external parameters. 
    // The mapping rules can be found at: https://lmfit.github.io/lmfit-py/bounds.html
    module LevenbergMarquardtConstrained = 
        
        ///
        let private validateBounds (lowerBound: vector) (upperBound: vector) (parameters: vector) =
            try
                if Vector.map3 (fun l u x -> if l <= x && u >= x then x else nan) lowerBound upperBound parameters |> Vector.exists isNan then 
                    failwith "initial parameters are not within Bounds"
                else 
                    ()
            with 
            | _ -> failwith "vector lengths differ"
        
        ///
        let private toInternalParameters (lowerBound: vector) (upperBound: vector) (extParameters: vector) =
            Vector.map3 (fun l u x -> 
                Math.Asin((2.0 * (x - l) / (u - l)) - 1.0)
            ) lowerBound upperBound extParameters
        
        ///
        let private toExternalParameters (lowerBound: vector) (upperBound: vector) (intParameters: vector) =
            Vector.map3 (fun l u x -> 
                l + (u / 2.0 - l / 2.0) * (Math.Sin(x) + 1.0)
            ) lowerBound upperBound intParameters
            
        ///
        let private calculateJacScaleFactors (lowerBound: vector) (upperBound: vector) (intParameters: vector) = 
            Vector.map3 (fun l u x -> 
                (u - l) / 2.0 * Math.Cos(x)
            ) lowerBound upperBound intParameters
        
        ///
        let private scaleJacobian (scaleFactors: vector) (jacobian:matrix) =
            jacobian
            |> Matrix.mapi (fun m n x -> x * scaleFactors.[m] * scaleFactors.[n])
        
        ///
        let private scaleGradient (scaleFactors: vector) (gradient:vector) =
            Vector.cptMul scaleFactors gradient
        
        /// Returns an collection of parameter vectors as a possible solution for least square based nonlinear fitting of a given dataset (xData, yData) with a given 
        /// model function. 
        let estimatedParamsVerbose (model: Model) (solverOptions: SolverOptions) lambdaInitial lambdaFactor (lowerBound: vector) (upperBound: vector) (xData: float[]) (yData: float []) = 
            let paramsAtIteration = new ResizeArray<vector>()
            let initialParamGuess = Vector.ofArray solverOptions.InitialParamGuess
            validateBounds lowerBound upperBound initialParamGuess
            let internalParamsGuess = toInternalParameters lowerBound upperBound initialParamGuess
            let residualVector = Vector.zeroCreate xData.Length
            let jacobian = Matrix.zero xData.Length solverOptions.InitialParamGuess.Length
            let initialValueRSS = getRSS model xData yData initialParamGuess  
            let rec loop lambda jacobian residualVector currentParamGuessExt currentParamGuessInt currentValueRSS (paramsAtIteration:ResizeArray<vector>) = 
                let scaleFactors = calculateJacScaleFactors lowerBound upperBound currentParamGuessInt
                let jacobian' = updateJacobianInplace model xData currentParamGuessExt jacobian 
                let residualVector' = updateResidualVectorInPlace model xData yData currentParamGuessExt residualVector
                let gradient = Matrix.mulV (jacobian'.Transpose) residualVector' |> scaleGradient scaleFactors
                let hessian = jacobian'.Transpose * jacobian' |> scaleJacobian scaleFactors
                let diagonal = Matrix.initDiagonal (Vector.map (fun x -> ((lambda)*x)) hessian.Diagonal)
                let modHessian = (hessian + diagonal) 
                let step = FSharp.Stats.Algebra.LinearAlgebra.SolveLinearSystem modHessian gradient
                let newParamGuessInt = currentParamGuessInt - step
                let newParamGuessExt = toExternalParameters lowerBound upperBound newParamGuessInt        
                let newValueRSS = getRSS model xData yData newParamGuessExt
                paramsAtIteration.Add(newParamGuessExt)     
                if shouldTerminate currentValueRSS newValueRSS paramsAtIteration.Count currentParamGuessExt newParamGuessExt solverOptions then 
                    paramsAtIteration
                elif newValueRSS < currentValueRSS then
                    let lambda' = lambda / lambdaFactor
                    loop lambda' jacobian' residualVector' newParamGuessExt newParamGuessInt newValueRSS paramsAtIteration
                else
                    let lambda' = lambda * lambdaFactor
                    loop lambda' jacobian' residualVector' currentParamGuessExt currentParamGuessInt currentValueRSS paramsAtIteration
            loop lambdaInitial jacobian residualVector initialParamGuess internalParamsGuess initialValueRSS paramsAtIteration
        
        /// Returns a parameter vector as a possible solution for linear least square based nonlinear fitting of a given dataset (xData, yData) with a given 
        /// model function. 
        let estimatedParams (model: Model) (solverOptions: SolverOptions) lambdaInitial lambdaFactor (lowerBound: vector) (upperBound: vector) (xData: float[]) (yData: float []) = 
            let estParams = estimatedParamsVerbose model solverOptions  lambdaInitial lambdaFactor (lowerBound: vector) (upperBound: vector) xData yData
            estParams.[estParams.Count-1]
        
        /// Returns a parameter vector tupled with its residual sum of squares (RSS) as a possible solution for linear least square based nonlinear fitting of a given dataset (xData, yData) with a given
        /// model function.
        let estimatedParamsWithRSS (model: Model) (solverOptions: SolverOptions) lambdaInitial lambdaFactor (lowerBound: vector) (upperBound: vector) (xData: float[]) (yData: float []) =
            let estParams = estimatedParamsVerbose model solverOptions lambdaInitial lambdaFactor lowerBound upperBound xData yData
            estParams
            |> fun estParams ->
                let paramGuess = Seq.last estParams
                let rss = getRSS model xData yData paramGuess
                estParams.[estParams.Count-1], rss

        // Looks for the real point in a dataset that is closest to the given point
        let private findClosestPoint (point: float) (data: float []) =
            let distance =
                data
                |> Array.map (fun x ->
                    abs (point - x)
                )
            let indexSmallest =
                distance
                |> Array.findIndex (fun x ->
                    x = (distance |> Array.min)
                )
            data.[indexSmallest]

        /// Returns an estimate for an initial parameter for the linear least square estimator for a given dataset (xData, yData).
        /// The initial estimation is intended for a logistic function.
        /// The returned parameters are the max y value, the steepness of the curve and the x value in the middle of the slope.
        let initialParam (xData: float[]) (yData: float[]) (cutoffPercentage: float)=
            let xRange = ((xData |> Array.max) - (xData |> Array.min))
            let yRange = ((yData |> Array.max) - (yData |> Array.min))
            let descending =
                let toTake = 
                    yData.Length
                    |> float
                    |> (*) 0.05
                    |> ceil
                    |> int
                let beginnign = 
                    yData
                    |> Array.take toTake
                    |> Array.average
                let ending =
                    yData
                    |> Array.rev
                    |> Array.take toTake
                    |> Array.average
                beginnign > ending
            let maxY = yData |> Array.max
            let combined = Array.map2 (fun x y -> x,y) xData yData
            // finds the point which is closest to the middle of the range on the y axis
            let midX,midY =
                let point = maxY - yRange / 2.
                let middleYData = findClosestPoint point yData
                Array.filter (fun (x,y) -> y = middleYData) combined
                |> Array.averageBy fst, middleYData
            // looks for the point where the descending functions slope begins to flatten
            // for that the first point which is in the lowest percent of the y values is taken
            let minSlopeX,minSlopeY =
                combined
                |> Array.filter (fun (x, y) -> y < cutoffPercentage * yRange)
                |> Array.sortByDescending snd
                |> Array.head
            // mirrors the x value of the right slope point through the x value of the middle point
            // takes max y for y
            let maxSlopeX, maxSlopeY =
                if descending then
                    let leftX = midX - abs (minSlopeX - abs midX)
                    leftX, maxY
                else
                    let rightX = midX + abs (minSlopeX - abs midX)
                    rightX, maxY
            // slope = (y2 - y1)/(x2 - x1)
            let slope =
                if descending then
                    ((minSlopeY - maxSlopeY)/yRange) / ((minSlopeX - maxSlopeX)/xRange)
                else
                    ((maxSlopeY - minSlopeY)/yRange) / ((maxSlopeX - minSlopeX)/xRange)
            let steepness = abs slope
            [|maxY; steepness; midX|]

        /// Returns an estimate for an initial parameter for the linear least square estimator for a given dataset (xData, yData).
        /// The steepness is given as an array and not estimated. An initial estimate is returned for every given steepness.
        /// The initial estimation is intended for a logistic function.
        let initialParamsOverRange (xData: float[]) (yData: float[]) (steepnessRange: float []) =
            // works the same as initialParam for mid point estimation
            let yRange = abs ((yData |> Array.max) - (yData |> Array.min))
            let maxY = yData |> Array.max
            let combined = Array.map2 (fun x y -> x,y) xData yData
            let midX,midY =
                let point = maxY - yRange / 2.
                let middleYData = findClosestPoint point yData
                Array.filter (fun (x,y) -> y = middleYData) combined
                |> Array.averageBy fst, middleYData
            steepnessRange
            |> Array.map (fun steepness -> [|maxY; steepness; midX|])
        
    module Table = 
        
        /////////////////////////
        /// Line  
        /// Line model of the form "y = a * x + b"
        let lineModel = {
            ParameterNames= [|"a";"b"|]
            GetFunctionValue = (fun (parameterVector:Vector<float>) xValue -> (parameterVector.[0] * xValue) + parameterVector.[1])
            GetGradientValue = (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue -> 
                                gradientVector.[0] <- xValue  
                                gradientVector.[1] <- 1.0 
                                gradientVector)
            }


        let lineSolverOptions initialParamGuess = {
            MinimumDeltaValue       = 0.00001
            MinimumDeltaParameters  = 0.00001  
            MaximumIterations       = 1000
            InitialParamGuess       = initialParamGuess
            }
                
        /////////////////////////
        /// Parabola
        
        /// paraboola model of the form "y = a * x^2 + b * x + c"
        let parabolaModel = {
            ParameterNames= [|"a";"b";"c"|]
            GetFunctionValue = (fun (parameterVector:Vector<float>) xValue -> (parameterVector.[0] * xValue**2.) + parameterVector.[1] * xValue + parameterVector.[2])
            GetGradientValue = (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue -> 
                                gradientVector.[0] <- xValue**2.  
                                gradientVector.[1] <- xValue 
                                gradientVector.[2] <- 1.0
                                gradientVector)
            }

        let parabolaSolverOptions initialParamGuess = {
            MinimumDeltaValue       = 0.00001
            MinimumDeltaParameters  = 0.00001  
            MaximumIterations       = 1000
            InitialParamGuess       = initialParamGuess
            }

        /////////////////////////
        /// Gaussian function of the form "y = amp * exp( -1. * ( ( ( (x-meanX)**2. ) / (2.*std**2.)) ) )"
        let gaussModel = {
            ParameterNames= [|"amp";"meanX";"std"|]
            GetFunctionValue = (fun (parameterVector:Vector<float>) xValue -> parameterVector.[0] * exp( (-1.) * ( ( ( (xValue-parameterVector.[1])**2. ) / (2.*parameterVector.[2]**2.)) ) ))
            GetGradientValue = (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue -> 
                                gradientVector.[0] <- exp( (-1.) * ( ( ( (xValue-parameterVector.[1])**2. ) / (2.*parameterVector.[2]**2.)) ) )
                                gradientVector.[1] <- ( (parameterVector.[0] * (xValue-parameterVector.[1]) * exp( (-1.) * ( ( ( (xValue-parameterVector.[1])**2. ) / (2.*parameterVector.[2]**2.)) ) ) ) ) / (parameterVector.[2]**2.)
                                gradientVector.[2] <- ( (parameterVector.[0] * ((xValue-parameterVector.[1])**2.) * exp( (-1.) * ( ( ( (xValue-parameterVector.[1])**2. ) / (2.*parameterVector.[2]**2.)) ) ) ) ) / (parameterVector.[2]**3.)
                                gradientVector)
            }

        let gaussSolverOptions initialParamGuess = {
            MinimumDeltaValue       = 0.01
            MinimumDeltaParameters  = 0.01  
            MaximumIterations       = 10000
            InitialParamGuess       = initialParamGuess
            }
            
        /////////////////////////
        /// Exponential function of the form "y = a * exp(b * x)"
        let expModel = 
            let parameterNames = [|"a";"b"|]
            let getFunctionValues = (fun (parameters:Vector<float>) x -> 
                parameters.[0] * Math.Exp(parameters.[1] * x))
            let getGradientValues =
                (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue -> 
                    gradientVector.[0] <- Math.Exp(parameterVector.[1] * xValue)  
                    gradientVector.[1] <- parameterVector.[0] * xValue * Math.Exp(parameterVector.[1] * xValue)  
                    gradientVector)    
            createModel parameterNames getFunctionValues getGradientValues

        ///Takes the result of the linearization as initialGuessParams
        let expSolverOptions (xData:float []) (yData:float [])= 
            //gets the linear representation of the problem and solves it by simple linear regression
            let initialParamGuess =
                let yLn = yData |> Array.map (fun x -> Math.Log(x)) |> vector
                let linearReg = LinearRegression.OLS.Linear.Univariable.fit (vector xData) yLn
                let a = exp linearReg.Constant
                let b = linearReg.Linear
                [|a;b|]

            {
            MinimumDeltaValue       = 0.0001
            MinimumDeltaParameters  = 0.0001  
            MaximumIterations       = 10000
            InitialParamGuess       = initialParamGuess
            }

        /////////////////////////
        /// log normal distribution
        let logNormalModel = 
            let parameterNames = [|"mu";"sigma"|]
            let getFunctionValues = 
                (fun (parameters:Vector<float>) x -> 
                    let mu = parameters.[0] 
                    let sigma = parameters.[1]
                    let a = 1. / (x * sigma * sqrt (2. * Math.PI))
                    let b = - ((log x - mu)**2.) / (2. * sigma * sigma)
                    a * exp b)

            let getGradientValues =
                (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) x -> 
                    
                    let mu = parameterVector.[0] 
                    let sigma = parameterVector.[1]
                    let partialDerivMu =
                        ((log (x) - mu) * exp(-(log (x) - mu)**2. / (2. * sigma**2.))) / (sqrt(2.) * sqrt(Math.PI) * sigma**3. * x)
                        
                    let partialDerivSigma =
                        -((sigma**2. - log (x)**2. + 2. * mu * log (x) - mu**2.) * exp(-(log (x) - mu)**2. / (2. * sigma**2.))) / (sqrt (2.) * sqrt (Math.PI) * x * sigma**4.)

                    gradientVector.[0] <- partialDerivMu
                    gradientVector.[1] <- partialDerivSigma
                    gradientVector)    
            createModel parameterNames getFunctionValues getGradientValues

        
        ///Takes the result of the linearization as initialGuessParams
        let logNormalOptions (sample:float []) = 
            //gets the linear representation of the problem and solves it by simple linear regression
            let initialParamGuess =

                let xBar = Seq.mean sample 
                let varHat = Seq.varPopulation sample
                let mu' = log (xBar / sqrt (1. + (varHat / xBar**2.)))

                let sigma' = 
                    log(1. + (varHat) / (xBar**2.))
                    |> sqrt

                let s = 
                    sample
                    |> Seq.filter (fun x -> x <> 0.)
                    |> Seq.map log
                    |> Seq.stats
                let mu  = SummaryStats.mean s
                let sigma = SummaryStats.stDev s
                [|mu;sigma|]


            {
            MinimumDeltaValue       = 0.0001
            MinimumDeltaParameters  = 0.0001  
            MaximumIterations       = 10000
            InitialParamGuess       = initialParamGuess
            }

    
        /////////////////////////
        /// Exponentially modified Gaussian (EMG) of the form "y =  ((amp*std)/tau) * sqrt(PI/2.) * exp(1./2. * ((std/tau)**2.) - ((x-meanX)/tau)) * Erfc((1./sqrt(2.)) * ((std/tau)-((x-meanX)/std)))"


        let private findZ initMeanX initStdev initTau x =
            1./sqrt(2.) * ((initStdev/initTau) - ((x - initMeanX) / initStdev))

        let emgModel = {

            ParameterNames= [|"amp";"meanX";"std";"tau"|]
            GetFunctionValue = (fun (parameterVector:Vector<float>) xValue -> 
                            
                                let a,m,s,t = parameterVector.[0], parameterVector.[1], parameterVector.[2],parameterVector.[3]

                                let standardEMG a m s t xValue = 
                                    ((a*s)/t) * sqrt(System.Math.PI/2.) * exp(1./2. * ((s/t)**2.) - ((xValue-m)/t)) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc((1./sqrt(2.)) * ((s/t)-((xValue-m)/s))) 
                                let delleyEMG a m s t xValue = 
                                    a * exp((-0.5)*((xValue-m)/s)**2.) * (s/t) * sqrt(Math.PI/2.) * FSharp.Stats.SpecialFunctions.Errorfunction._erfcx ((1./(sqrt 2.)) * ((s/t) - ((xValue-m)/s)))
                                let asymptoticEMG a m s t xValue = 
                                    let numerator = 
                                        let exp = exp(-(1./2.) * (((xValue - m)/s)**2.))
                                        a * exp
                                    let denominator = 1. - (((xValue - m)*t) / (s**2.) )
                                    numerator / denominator
                                let z = findZ m s t xValue 
                                if z < 0. then 
                                    standardEMG a m s t xValue 
                                elif z >= 0. && z <= (67100000.) then 
                                    delleyEMG a m s t xValue 
                                else 
                                    asymptoticEMG a m s t xValue 
                              )

            GetGradientValue = (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue -> 

                                let a,m,s,t = parameterVector.[0], parameterVector.[1], parameterVector.[2],parameterVector.[3]
                                let standardGradient a m s t xValue  = 
                                    gradientVector.[0] <- (1./t) * 1.25331 * s * exp( (0.5*(s**2.) / (t**2.) ) - ( (xValue-m) / t) ) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc(0.707107 * ( (s / t ) - ( (xValue-m) / s) ) )
                                    gradientVector.[1] <-  ( (1./t**2.) *  1.25331 * a * s * exp( (0.5*(s**2.) / (t**2.) ) - ( (xValue-m) / t) ) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc(0.707107 * ( (s / t ) - ( (xValue-m) / s) ) ) )  
                                                            - ( (1./t) * a * exp( ( (0.5 * s**2.) / t**2. ) - (0.5 * (((s/t) - ( (xValue-m) / s ) )**2.) ) - ( (xValue-m) / t ) ) )
                                    gradientVector.[2] <-     ( (1./ (t))                                 * 1.25331 * a * exp( (0.5*(s**2.) / (t**2.) ) - ( (xValue-m) / t) ) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc(0.707107 * ( (s / t ) - ( (xValue-m) / s) ) )) 
                                                            + ( (1./ (t**3.)) * (s**2.) * 1.25331 * a * exp( (0.5*(s**2.) / (t**2.) ) - ( (xValue-m) / t) ) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc(0.707107 * ( (s / t ) - ( (xValue-m) / s) ) ))
                                                            - ( (1./ (t))  )  * (s)     * 1.00000 * a * exp( (-0.5*( (s / t)  - ( (xValue-m) / s) )**2. ) - ((xValue-m) / t) + (0.5*(s**2.) / (t**2.) ) ) * ( ((xValue-m) / (s**2.) ) + 1./t )    

                                    gradientVector.[3] <-  (a * exp((-0.5 * (-xValue + m + s**2./t)**2.)/s**2. + 0.5 * (s/t)**2. - xValue/t + m/t) * s * (1. * s + ( (exp( (0.5 * (-xValue + m + s**2. / t)**2.)/s**2.)) * (-1.25331 * s**2. + (1.25331 * xValue - 1.25331 * m - 1.25331 * t) * t) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc((-0.707107 * (xValue - m))/s + (0.707107 * s)/t))/t))/t**3.              

                                    gradientVector 
                                /// TODO: Derivation is causing arithmetic overflows, do again with erfcx.
                                let delleyGradient a m s t xValue =
                                    gradientVector.[0] <- (1.25331 * s * exp(0.5 * (((s/t) - ((xValue - m)/s))**2.) - ((0.5 * ((xValue - m)**2.))/(s**2.))) * SpecialFunctions.Errorfunction.Erfc(0.707107 * ((s/t) - ((xValue - m)/s)))) / t
                            
                                    gradientVector.[1] <- (1.25331 * a * (exp((0.5 * ((s**2.) + t * ((m - xValue))**2.)) / ((s**2.)*(t**2.))) * ((s**2.) + (4.44089*(10.** -16.)) * t * xValue - (4.44089*(10.** -16.)) * t * m) * 
                                                            SpecialFunctions.Errorfunction.Erfc(((0.707107 * s)/t) - ((0.707107 * (xValue - m))/s)) - 0.797885 * s * t) *
                                                            exp(-((0.5 * ((s**2.) + t * ((m - xValue))**2.))/((s**2.) * (t**2.))) + ((0.5 * ((((s**2.)/t) - xValue + m)**2.))/(s**2.)) - ((0.5 * ((xValue - m)**2.))/(s**2.)))) 
                                                            / (s * (t**2.))
                            
                                    gradientVector.[2] <- (a * exp( ((0.5 * ((m + (s**2.)/t - xValue)**2.))/s**2.) - ((0.5 * ((xValue - m)**2.))/(s**2.)) ) *
                                                            ( ( (((2.78292*(10.** -16.)) * (m**2.))/s**2.) - (((5.5658*(10.** -16.)) * m * xValue)/(s**2.)) + ((1.25331 * s**2.)/(t**2.)) + (((2.78292*(10.** -16.)) * (xValue**2.))/(s**2.)) + 1.25331) * 
                                                                SpecialFunctions.Errorfunction.Erfc( ((0.707107 * s)/t) - ((0.707107 * (xValue - m))/s) ) + 
                                                                (exp( -(0.5 * ((m * t - t * xValue + (s**2.))**2.))/((t**2.) * (s**2.)) ) * 
                                                                //shows 1. in wolfram alpha, doesnt show dot for anything else
                                                                    (1.*m * t - 1.*t * xValue - 1.*(s**2.)) ) / (t * s))) /t
                            
                                    gradientVector.[3] <- -(1.25331 * a * s * 
                                                            (exp((0.5 * ((t * (m - xValue) + (s**2.))**2.))/((s**2.) * (t**2.))) * (t * (m - xValue + 1. * t) + s**2.) *
                                                                SpecialFunctions.Errorfunction.Erfc( ((0.707107 * s)/t) - ((0.707107 * (xValue - m))/s) ) - 0.797885 * s * t
                                                            ) *
                                                                exp(-((0.5 * ((t * (m - xValue) + (s**2.))**2.))/((s**2.) * (t**2.))) + ((0.5 * ((m + ((s**2.)/t) - xValue)**2.))/(s**2.)) - ((0.5 * ((xValue - m)**2.))/(s**2.))))
                                                                /(t**4.)
                                    gradientVector 
                        
                                let asymGradient a m s t xValue =
                                    gradientVector.[0] <- exp (-(0.5 * (xValue - m)**2.)/s**2.)/(1. - (t * (xValue - m))/s**2.)
                                    gradientVector.[1] <- -(a * exp(-(0.5 * (xValue - m)**2.)/(s**2.)) * ((s**2.)*t + (s**2.)*(m - xValue) + t*((xValue**2.) - 2.*xValue*m + (m**2.))))/(((s**2.) - t*xValue + t*m)**2.)
                                    gradientVector.[2] <- -(a * (m - xValue) * exp(-(0.5*((xValue - m)**2.))/(s**2.)) * (-m*t*(s**2.)*(m - xValue) - t*xValue*(s**2.)*(xValue - m) - (s**4.)*(m - xValue) - 2.*t*s**4.)) / ((s**3.)*(m*t - t*xValue + (s**2.))**2.)
                                    gradientVector.[3] <- (a * (xValue - m) * exp(-(0.5 * ((xValue - m)**2.))/s**2.)) / ((s**2.) * ((((t * (m - xValue))/(s**2.)) + 1.)**2.))
                                    gradientVector 
                                let z = findZ m s t xValue 
                                if z < 0. then 
                                    standardGradient a m s t xValue 
                                elif z >= 0. && z <= (67100000.) then 
                                    standardGradient a m s t xValue 
                                else 
                                    asymGradient a m s t xValue
                              ) 
            }


        let emgSolverOptions initialParamGuess = {
            MinimumDeltaValue       = 0.001
            MinimumDeltaParameters  = 0.001  
            MaximumIterations       = 10000
            //[|"amp";"meanX";"std";"tau"|]
            InitialParamGuess       = initialParamGuess
            }

        /////////////////////////
        /// Hill equation "y = Vm * x^n / (k^n+x^n)"
        let hillModel = 
            let parameterNames = [|"Vm";"n";"k"|]
            let getFunctionValues = (fun (parameters:Vector<float>) x -> 
                parameters.[0] * x**parameters.[1] / (parameters.[2]**parameters.[1] + x**parameters.[1])
                )
            let getGradientValues =
                (fun (parameters:Vector<float>) (gradientVector: Vector<float>) x -> 
                    gradientVector.[0] <- x**parameters.[1] / (parameters.[2]**parameters.[1] + x**parameters.[1])
                    gradientVector.[1] <- parameters.[0] * x**parameters.[1] * Math.Log(x) * (- 1. / ((parameters.[2]**parameters.[1] + x**parameters.[1])**2.)) * (parameters.[2]**parameters.[1] * Math.Log(parameters.[2]) + x**parameters.[1] * Math.Log(x))
                    gradientVector.[2] <- parameters.[0] * x**parameters.[1] * (- 1. / ((parameters.[2]**parameters.[1] + x**parameters.[1])**2.)) * parameters.[1] * parameters.[2]**(parameters.[1] - 1.)
                    gradientVector)    
            createModel parameterNames getFunctionValues getGradientValues
            
        let hillSolverOptions Vm n k = 
            if Vm <= 0. || n <= 0. || k <= 0. then 
                failwithf "Vm, n, and k cannot be negative!"

            {
            MinimumDeltaValue       = 0.0001
            MinimumDeltaParameters  = 0.0001  
            MaximumIterations       = 10000
            InitialParamGuess       = [|Vm;n;k|]
            }

        /////////////////////////
        /// Logistic function of the form "y = L/(1+e^(k(t-x)))"
        [<Obsolete("Use the ascending and descending versions instead.")>]
        let LogisticFunction = {
            ParameterNames= [|"L - curve maximum";"k - Steepness"; "x0 xValue of midpoint"|]
            GetFunctionValue = (fun (parameterVector:Vector<float>) xValue -> parameterVector.[0] / (1. + exp(parameterVector.[1]*(xValue-parameterVector.[2]))))
            GetGradientValue = (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue ->
                                gradientVector.[0] <- 1. / (1. + exp(parameterVector.[1]*(xValue-parameterVector.[2])))
                                gradientVector.[1] <- (parameterVector.[0] * (xValue-parameterVector.[2]) * exp(parameterVector.[1]*(xValue-parameterVector.[2])) ) / (exp(parameterVector.[1]*(xValue-parameterVector.[2])) + 1.)**2.
                                gradientVector.[2] <- (parameterVector.[0] * parameterVector.[1] * exp(parameterVector.[1]*(xValue-parameterVector.[2])) ) / (exp(parameterVector.[1]*(xValue-parameterVector.[2])) + 1.)**2.
                                gradientVector)
            }

        /// Logistic function of the form "y = L/(1+e^(k(t-x)))"
        let LogisticFunctionDescending = {
            ParameterNames= [|"L - curve maximum";"k - Steepness"; "x0 xValue of midpoint"|]
            GetFunctionValue = (fun (parameterVector:Vector<float>) xValue -> parameterVector.[0] / (1. + exp(parameterVector.[1]*(xValue-parameterVector.[2]))))
            GetGradientValue = (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue ->
                                gradientVector.[0] <- 1. / (1. + exp(parameterVector.[1]*(xValue-parameterVector.[2])))
                                gradientVector.[1] <- (parameterVector.[0] * (xValue-parameterVector.[2]) * exp(parameterVector.[1]*(xValue-parameterVector.[2])) ) / (exp(parameterVector.[1]*(xValue-parameterVector.[2])) + 1.)**2.
                                gradientVector.[2] <- (parameterVector.[0] * parameterVector.[1] * exp(parameterVector.[1]*(xValue-parameterVector.[2])) ) / (exp(parameterVector.[1]*(xValue-parameterVector.[2])) + 1.)**2.
                                gradientVector)
            }

        /// Logistic function of the form "y = L/(1+e^(-k(t-x)))"
        let LogisticFunctionAscending = {
            ParameterNames= [|"L - curve maximum";"k - Steepness"; "x0 xValue of midpoint"|]
            GetFunctionValue = (fun (parameterVector:Vector<float>) xValue -> parameterVector.[0] / (1. + exp(-parameterVector.[1]*(xValue-parameterVector.[2]))))
            GetGradientValue = (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue ->
                                gradientVector.[0] <- 1. / (1. + exp(parameterVector.[1]*(xValue-parameterVector.[2])))
                                gradientVector.[1] <- (parameterVector.[0] * (xValue-parameterVector.[2]) * exp(parameterVector.[1]*(xValue-parameterVector.[2])) ) / (exp(parameterVector.[1]*(xValue-parameterVector.[2])) + 1.)**2.
                                gradientVector.[2] <- (parameterVector.[0] * parameterVector.[1] * exp(parameterVector.[1]*(xValue-parameterVector.[2])) ) / (exp(parameterVector.[1]*(xValue-parameterVector.[2])) + 1.)**2.
                                gradientVector)
            }

        /// Logistic function of the form "y = L/(1+e^(k(t-x)))+N"
        /// Modified version of the Logistic function model with a variable curve minimum.
        let LogisticFunctionVarYDescending = {
            ParameterNames= [|"L - curve maximum";"k - Steepness"; "x0 xValue of midpoint"; "N - curve minimum"|]
            GetFunctionValue = (fun (parameterVector:Vector<float>) xValue -> 
                                parameterVector.[0] / (1. + exp(parameterVector.[1]*(xValue-parameterVector.[2]))) + parameterVector.[3])
            GetGradientValue = (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue ->
                                gradientVector.[0] <- 1. / (1. + exp(parameterVector.[1]*(xValue-parameterVector.[2])))
                                gradientVector.[1] <- (parameterVector.[0] * (xValue-parameterVector.[2]) * exp(parameterVector.[1]*(xValue-parameterVector.[2])) ) / (exp(parameterVector.[1]*(xValue-parameterVector.[2])) + 1.)**2.
                                gradientVector.[2] <- (parameterVector.[0] * parameterVector.[1] * exp(parameterVector.[1]*(xValue-parameterVector.[2])) ) / (exp(parameterVector.[1]*(xValue-parameterVector.[2])) + 1.)**2.
                                gradientVector.[3] <- 1.
                                gradientVector)
            }

        /// Logistic function of the form "y = L/(1+e^(-k(t-x)))+N"
        /// Modified version of the Logistic function model with a variable curve minimum.
        let LogisticFunctionVarYAscending = {
            ParameterNames= [|"L - curve maximum";"k - Steepness"; "x0 xValue of midpoint"; "N - curve minimum"|]
            GetFunctionValue = (fun (parameterVector:Vector<float>) xValue -> 
                                parameterVector.[0] / (1. + exp(-parameterVector.[1]*(xValue-parameterVector.[2]))) + parameterVector.[3])
            GetGradientValue = (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue ->
                                gradientVector.[0] <- 1. / (1. + exp(parameterVector.[1]*(xValue-parameterVector.[2])))
                                gradientVector.[1] <- (parameterVector.[0] * (xValue-parameterVector.[2]) * exp(parameterVector.[1]*(xValue-parameterVector.[2])) ) / (exp(parameterVector.[1]*(xValue-parameterVector.[2])) + 1.)**2.
                                gradientVector.[2] <- (parameterVector.[0] * parameterVector.[1] * exp(parameterVector.[1]*(xValue-parameterVector.[2])) ) / (exp(parameterVector.[1]*(xValue-parameterVector.[2])) + 1.)**2.
                                gradientVector.[3] <- 1.
                                gradientVector)
            }
        /// Descending version of the generalized logistic function or curve, also known as Richards' curve with 7 parameters.
        /// Logistic function of the form "Y(t) = A + (K - A) / (C + Q * e^(B * (t - M)))**(1. / v)"
        let richardsGenericDescending =
            {
            ParameterNames= [|"A - lower asymptote"; "K - upper asymptote"; "B - growth rate"; "v > 0 - affects near which asymptote maximum growth occurs"; "Q - related to Y(0)"; "C - typically 1"; "M - starting time"|]
            GetFunctionValue = 
                (fun (parameterVector:Vector<float>) t -> 
                    let a = parameterVector.[0]
                    let k = parameterVector.[1]
                    let b = parameterVector.[2]
                    let v = parameterVector.[3]
                    let q = parameterVector.[4]
                    let c = parameterVector.[5]
                    let m = parameterVector.[6]
                    a + (k - a) / (c + q * Math.Exp(b * (t - m)))**(1. / v)
                )
            GetGradientValue = 
                (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) t ->
                    let a = parameterVector.[0]
                    let k = parameterVector.[1]
                    let b = parameterVector.[2]
                    let v = parameterVector.[3]
                    let q = parameterVector.[4]
                    let c = parameterVector.[5]
                    let m = parameterVector.[6]
                    gradientVector.[0] <- 1.-1./(q*Math.Exp(b*(t-m))+c)**(1./v)
                    gradientVector.[1] <- 1./(q*Math.Exp(b*(t-m))+c)**(1./v)
                    gradientVector.[2] <- -((k-a)*q*(t-m)*Math.Exp((t-m)*b)*(q*Math.Exp((t-m)*b)+c)**(-1./v-1.))/v
                    gradientVector.[3] <- ((k-a)*Math.Log(q*Math.Exp(b*(t-m))+c))/((q*Math.Exp(b*(t-m))+c)**(1./v)*v**2.)
                    gradientVector.[4] <- -((k-a)*Math.Exp(b*(t-m))*(Math.Exp(b*(t-m))*q+c)**(-1./v-1.))/v
                    gradientVector.[5] <- -((k-a)*(c+q*Math.Exp(b*(t-m)))**(-1./v-1.))/v
                    gradientVector.[6] <- (b*(k-a)*q*Math.Exp(b*(t-m))*(q*Math.Exp(b*(t-m))+c)**(-1./v-1.))/v
                    gradientVector
                )
            }
        
        module GrowthModels =

            //Gibson et al., Predicting microbial growth [...], Int. Journal of Food Microbiology, 1988
            /// The gompertz function describes the log cell count at time point t.
            //inflection always at y = 0.36 * (upper asymptote - lower asymptote)
            let gompertz =
                {
                ParameterNames= [|"A: lower asymptote";"B: relative growth rate";"C: upper - lower asypmtote (yRange)";"M: x value of inflection point"|]
                GetFunctionValue = 
                    (fun (parameterVector:Vector<float>) t -> 
                        parameterVector.[0] + parameterVector.[2] * Math.Exp(-Math.Exp(-parameterVector.[1] * (t-parameterVector.[3]))))
                GetGradientValue = 
                    (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) t ->
                        let a = parameterVector.[0]
                        let b = parameterVector.[1]
                        let c = parameterVector.[2]
                        let m = parameterVector.[3]
                        gradientVector.[0] <- 1.
                        gradientVector.[1] <- c * (t - m) * Math.Exp(- Math.Exp(- b * (t - m)) - b * (t - m))
                        gradientVector.[2] <- Math.Exp(-Math.Exp(- b * (t - m)))
                        gradientVector.[3] <- -b * c * Math.Exp(-Math.Exp(- b * (t - m)) - b * (t - m))
                        gradientVector)
                }

            /// determines the solver options for cell count growth data (must be in log space). For untransformed data use 'id' as transform.
            let getSolverOptionsGompertz (xData :float []) (yDataLog :float []) expectedGenerationTime (usedLogTransform: float -> float) =
                // lower asymptote
                let a = Seq.min yDataLog
                // upper asymptote - lower asymptote (y range)
                let c = (Seq.max yDataLog) - a
                // relative growth rate
                let b = usedLogTransform 2. * Math.E / (expectedGenerationTime * c)
                // time point of inflection (in gompertz model at f(x)=36% of the y range)
                let m = 
                    let yAtInflection = a + c * 0.36
                    Seq.zip xData yDataLog
                    |> Seq.minBy (fun (xValue,yValue) ->
                        Math.Abs (yValue - yAtInflection)
                    )
                    |> fst
                createSolverOption 0.001 0.001 10000 [|a;b;c;m|]
            
            /// 4 parameter richards curve with minimum at 0; d &lt;&gt; 1
            let richards =
                {
                ParameterNames= [|"upper asymptote";"growth rate";"inflection point x";"d (influences inflection y)"|]
                GetFunctionValue = 
                    (fun (parameterVector:Vector<float>) t -> 
                        let l = parameterVector.[0]
                        let k = parameterVector.[1]
                        let y = parameterVector.[2]
                        let d = parameterVector.[3]
                        l * (1. + (d - 1.) * Math.Exp(- k * (t - y)))**(1. / (1. - d)))
                GetGradientValue = 
                    (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) t ->
                        let l = parameterVector.[0]
                        let k = parameterVector.[1]
                        let y = parameterVector.[2]
                        let d = parameterVector.[3]
                        gradientVector.[0] <- (1. + (d - 1.) * Math.Exp(- k * (t - y)))**(1. / (1. - d))
                        gradientVector.[1] <- 
                            -(l*(y - t)*(((d-1.)*Math.Exp(-(t-y)*k)+1.)**(1./(1.-d))))/(Math.Exp((t-y)*k)+d-1.)
                        gradientVector.[2] <- 
                            -(k*l*((d-1.)*Math.Exp(-k*(t-y))+1.)**(1./(1.-d)))/(Math.Exp((t-y)*k)+d-1.)
                        gradientVector.[3] <- 
                            l*(Math.Exp(-k*(t-y))/((Math.Exp(-k*(t-y))*(d-1.)+1.)*(1.-d)) + (log(Math.Exp(-k*(t-y))*(d-1.)+1.))/(pown (1. - d) 2))*(Math.Exp(-k*(t-y))*(d-1.)+1.)**(1./(1.-d))
                        gradientVector)
                }
            /// Generalized logistic function or curve, also known as Richards' curve with 7 parameters.
            /// Logistic function of the form "Y(t) = A + (K - A) / (C + Q * e^(-B * (t - M)))**(1. / v)"
            let richardsGeneric =
                {
                ParameterNames= [|"A - lower asymptote"; "K - upper asymptote"; "B - growth rate"; "v > 0 - affects near which asymptote maximum growth occurs"; "Q - related to Y(0)"; "C - typically 1"; "M - starting time"|]
                GetFunctionValue = 
                    (fun (parameterVector:Vector<float>) t -> 
                        let a = parameterVector.[0]
                        let k = parameterVector.[1]
                        let b = parameterVector.[2]
                        let v = parameterVector.[3]
                        let q = parameterVector.[4]
                        let c = parameterVector.[5]
                        let m = parameterVector.[6]
                        a + (k - a) / (c + q * Math.Exp(-b * (t - m)))**(1. / v)
                    )
                GetGradientValue = 
                    (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) t ->
                        let a = parameterVector.[0]
                        let k = parameterVector.[1]
                        let b = parameterVector.[2]
                        let v = parameterVector.[3]
                        let q = parameterVector.[4]
                        let c = parameterVector.[5]
                        let m = parameterVector.[6]
                        gradientVector.[0] <- 1.-1./(q*Math.Exp(-b*(t-m))+c)**(1./v)
                        gradientVector.[1] <- 1./(q*Math.Exp(-b*(t-m))+c)**(1./v)
                        gradientVector.[2] <- ((k-a)*q*(t-m))/(v*(q*Math.Exp(-(t-m)*b)+c)**(1./v)*(c*Math.Exp((t-m)*b)+q))
                        gradientVector.[3] <- ((k-a)*log(q*Math.Exp(-b*(t-m))+c))/((q*Math.Exp(-b*(t-m))+c)**(1./v)*v**2.)
                        gradientVector.[4] <- -((k-a)*Math.Exp(-b*(t-m))*(Math.Exp(-b*(t-m))*q+c)**(-1./v-1.))/v
                        gradientVector.[5] <- -((k-a)*(c+q*Math.Exp(-b*(t-m)))**(-1./v-1.))/v
                        gradientVector.[6] <- -(b*(k-a)*q*Math.Exp(-b*(t-m))*(q*Math.Exp(-b*(t-m))+c)**(-1./v-1.))/v
                        gradientVector
                    )
                }

            /// weibull growth model; if d=1 then it is a simple exponential growth model
            let weibull =
                {
                ParameterNames= [|"lower asymptote";"upper asymptote";"growth rate";"d (influences inflection x)"|]
                GetFunctionValue = 
                    (fun (parameterVector:Vector<float>) t -> 
                        let b = parameterVector.[0]
                        let l = parameterVector.[1]
                        let k = parameterVector.[2]
                        let d = parameterVector.[3]
                        l - (l-b)*Math.Exp(-((k*t)**d)))
                GetGradientValue = 
                    (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) t ->
                        let b = parameterVector.[0]
                        let l = parameterVector.[1]
                        let k = parameterVector.[2]
                        let d = parameterVector.[3]
                        gradientVector.[0] <- Math.Exp(-((k*t)**d))
                        gradientVector.[1] <- 1. - Math.Exp(-((k*t)**d))
                        gradientVector.[2] <- 
                            (d*(l-b)*(t*k)**d*Math.Exp(-((t*k)**d))) / k
                        gradientVector.[3] <- 
                            (l-b)*(t*k)**d*log(k*t)*Math.Exp(-((k*t)**d))
                        gradientVector)
                }

            /// stable growth model similar to weibull model
            let janoschek =
                {
                ParameterNames= [|"lower asymptote";"upper asymptote";"growth rate";"d (influences inflection x)"|]
                GetFunctionValue = 
                    (fun (parameterVector:Vector<float>) t -> 
                        let b = parameterVector.[0]
                        let l = parameterVector.[1]
                        let k = parameterVector.[2]
                        let d = parameterVector.[3]
                        l - (l-b)*Math.Exp(-(k*t**d)))
                GetGradientValue = 
                    (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) t ->
                        let b = parameterVector.[0]
                        let l = parameterVector.[1]
                        let k = parameterVector.[2]
                        let d = parameterVector.[3]
                        gradientVector.[0] <- Math.Exp(-(k*t**d))
                        gradientVector.[1] <- 1. - Math.Exp(-(k*t**d))
                        gradientVector.[2] <- 
                            (l-b)*t**d*Math.Exp(-(t**d*k))
                        gradientVector.[3] <- 
                            k*(l-b)*t**d*log(t)*Math.Exp(-(k*t**d))
                        gradientVector)
                }

            /// exponential growth model
            let exponential =
                {
                ParameterNames= [|"lower asymptote";"upper asymptote";"growth rate"|]
                GetFunctionValue = 
                    (fun (parameterVector:Vector<float>) t -> 
                        let b = parameterVector.[0]
                        let l = parameterVector.[1]
                        let k = parameterVector.[2]
                        l - (l-b)*Math.Exp(-k*t))
                GetGradientValue = 
                    (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) t ->
                        let b = parameterVector.[0]
                        let l = parameterVector.[1]
                        let k = parameterVector.[2]
                        gradientVector.[0] <- Math.Exp(-k*t)
                        gradientVector.[1] <- 1. - Math.Exp(-k*t)
                        gradientVector.[2] <- 
                            (l-b)*t*Math.Exp(-t*k)
                        gradientVector)
                }

            /// 4 parameter Morgan-Mercer-Flodin growth model 
            let morganMercerFlodin =
                {
                ParameterNames= [|"size at t=0";"upper asymptote";"growth rate";"d (influences inflection point)" |]
                GetFunctionValue = 
                    (fun (parameterVector:Vector<float>) t -> 
                        let b = parameterVector.[0]
                        let l = parameterVector.[1]
                        let k = parameterVector.[2]
                        let d = parameterVector.[3]
                        l - (l-b)/(1.+(k*t)**d))
                GetGradientValue = 
                    (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) t ->
                        let b = parameterVector.[0]
                        let l = parameterVector.[1]
                        let k = parameterVector.[2]
                        let d = parameterVector.[3]
                        gradientVector.[0] <- 1./((k*t)**d+1.)
                        gradientVector.[1] <- 1. - 1./((k*t)**d+1.)
                        gradientVector.[2] <- 
                            (d*(l-b)*(t*k)**d)/(k*((t*k)**d+1.)**2.)
                        gradientVector.[3] <- 
                            ((l-b)*(k*t)**d*log(k*t))/((k*t)**d+1.)**2.
                        gradientVector)
                                }

            /// 3 parameter verhulst logistic model with lower asymptote=0
            let verhulst = LogisticFunctionAscending

            /// 4 parameter verhulst model with variably lowers asymptote
            let verhulst4Param =
                {
                ParameterNames= [|"upper asymptote";"inflection point value (x)";"steepness";"lower asymptote"|]
                GetFunctionValue = 
                    (fun (parameterVector:Vector<float>) t -> 
                        let lmax = parameterVector.[0]
                        let k    = parameterVector.[1]
                        let d    = parameterVector.[2]
                        let lmin = parameterVector.[3]
                        lmin + (lmax-lmin)/(1. + Math.Exp((k-t)/d)))
                GetGradientValue = 
                    (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) t ->
                        let lmax = parameterVector.[0]
                        let k    = parameterVector.[1]
                        let d    = parameterVector.[2]
                        let lmin = parameterVector.[3]
                        let exp = Math.Exp((k-t)/d)
                        gradientVector.[0] <- 1./(exp+1.)
                        gradientVector.[1] <- 
                            -((lmax-lmin)+exp)/(d*(exp + 1.)**2.)
                        gradientVector.[2] <- 
                            ((k-t)*(lmax-lmin)*exp)/(d**2.*(exp + 1.)**2.)
                        gradientVector.[3] <- 
                            1. - 1./(exp + 1.)
                        gradientVector)
                }

            /// 3 parameter von Bertalanffy growth model
            let vonBertalanffy =
                {
                ParameterNames= [|"upper asymtote";"growth rate";"t0" |]
                GetFunctionValue = 
                    (fun (parameterVector:Vector<float>) t -> 
                        let l = parameterVector.[0]
                        let k = parameterVector.[1]
                        let t0 = parameterVector.[2]
                        l*(1. - Math.Exp(-k*(t-t0))))
                GetGradientValue = 
                    (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) t ->
                        let l = parameterVector.[0]
                        let k = parameterVector.[1]
                        let t0 = parameterVector.[2]
                        gradientVector.[0] <- 1. - Math.Exp(-k*(t-t0))
                        gradientVector.[1] <- -l*(t0-t)*Math.Exp(-(t-t0)*k)
                        gradientVector.[2] <- -k*l*Math.Exp(-k*(t-t0))
                        gradientVector)
                }

        //fails because n and k become negative during the optimization iterations
        //add borders to GaussNewton (default -Infinity - Infinity)
        //let hillModelWithFixedVm Vm = 
        //    let parameterNames = [|"n";"k"|]
        //    let getFunctionValues = (fun (parameters:Vector<float>) x -> 
        //        Vm * x**parameters.[0] / (parameters.[1]**parameters.[0] + x**parameters.[0])
        //        )
        //    let getGradientValues =
        //        (fun (parameters:Vector<float>) (gradientVector: Vector<float>) x -> 
        //            gradientVector.[0] <- Vm * x**parameters.[0] * Math.Log(x) * (- 1. / ((parameters.[1]**parameters.[0] + x**parameters.[0])**2.)) * (parameters.[1]**parameters.[0] * Math.Log(parameters.[1]) + x**parameters.[0] * Math.Log(x))
        //            gradientVector.[1] <- Vm * x**parameters.[0] * (- 1. / ((parameters.[1]**parameters.[0] + x**parameters.[0])**2.)) * parameters.[0] * parameters.[1]**(parameters.[0] - 1.)
        //            gradientVector)    
        //    createModel parameterNames getFunctionValues getGradientValues
            
        //let hillSolverOptionsWithFixedVm n k = 
        //    if n <= 0. || k <= 0. then 
        //        failwithf "n and k cannot be negative!"

        //    {
        //    MinimumDeltaValue       = 0.0001
        //    MinimumDeltaParameters  = 0.0001  
        //    MaximumIterations       = 10000
        //    InitialParamGuess       = [|n;k|]
        //    }

        module Finances = 
            // https://en.wikipedia.org/wiki/Fixed-income_attribution

            /// Nelson-Siegel is a curve fitting model that is frequently applied in finance to fit smoothed yield curves. 
            /// xData: The periods of available yield rates (time).
            /// yData: The observed yields.
            // model modified from: Forecasting the term structure of government bond yields, Diebolda and Li, 2006
            // initialGuess: b0 (long term factor), governs the yield curve y axis level and defines the level which is approached asymptotically.
            // b1 (short term factor, >0), governs the slope of the early yield curve.
            // b2 (medium term factor), governs the curvature of the yield curve and thereby affecting the medium-term yields.
            // t (>0), is the decay factor with high values leading to a fast decay
            // default: [0.01;0.01;0.01;1.0]
            let nelsonSiegel = 

                let parameterNames = [|"b0";"b1";"b2";"t"|]
                //function defining the model
                let getFunctionValue =                 
                            fun (parameterVector: Vector<float>) m -> 
                            let b0 = parameterVector.[0]
                            let b1 = parameterVector.[1]
                            let b2 = parameterVector.[2]
                            let t  = parameterVector.[3]
                            b0+b1*((1.-exp(-m/t))/(m/t)) + b2*((1.-exp(-m/t))/(m/t)-exp(-m/t))

                //partial derivatives of the function         
                let getGradientValues =
                    fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) m -> 
                        let b0 = parameterVector.[0]
                        let b1 = parameterVector.[1]
                        let b2 = parameterVector.[2]
                        let t = parameterVector.[3]
                        gradientVector.[0] <- 1.
                        gradientVector.[1] <- (t*(1.-exp(-m/t)))/m
                        gradientVector.[2] <- (t*(1.-exp(-m/t)))/m-exp(-m/t)
                        gradientVector.[3] <- (exp(-m/t)*((b2+b1)*t**2.*exp(m/t)+(-b2-b1)*t**2.+(-b2-b1)*m*t-b2*m**2.))/(m*t**2.)
                        gradientVector
                createModel parameterNames getFunctionValue getGradientValues
                   

            (*
            Example Nelson Siegel model
            let actual = [|0.0928; 0.106; 0.1174; 0.1246; 0.1395; 0.1489; 0.1686; 0.1806|]
            let time = [|1.0; 2.0; 3.0; 5.0; 7.0; 10.0; 15.0; 25.0|]
            let solverOptionsNS = createSolverOption 0.0001 0.0001 5000 [|0.01;0.01;0.01;1.0|]
            let coefficientsNS = LevenbergMarquardt.estimatedParams nelsonSiegel solverOptionsNS 0.001 10. time actual
            let fittingFunctionNS = nelsonSiegel.GetFunctionValue coefficientsNS
            let expected = time |> Array.map fittingFunctionNS

            let actual2 = [|12.71;13.13;13.34;13.78;13.77;13.75;13.74;13.62|]
            let time2 = [|3./12.;0.5;1.;2.;3.;5.;7.;10.|] 
            let initialParamGuess = [|13.;-1.;3.;1.|]
            *) 
            
                
            
            /// Nelson-Siegel-Svensson is a curve fitting model that is frequently applied in finance to fit smoothed yield curves and extends the Nelson Siegel model by a term introducing a second hump. 
            /// xData: The periods of available yield rates (time).
            /// yData: The observed yields.
            // model modified from: Forecasting the term structure of government bond yields, Diebolda and Li, 2006 
            // and Estimating and interpreting forward interest rates, Svensson, 1994
            // initialGuess: b0 (long term factor), governs the yield curve y axis level and defines the level which is approached asymptotically.
            // b1 (short term factor, >0), governs the slope of the early yield curve.
            // b2 (medium term factor), governs the curvature of the yield curve and thereby affecting the medium-term yields.
            // t1 (>0), is the decay factor with high values leading to a fast decay
            // default: [0.01;0.01;0.01;1.0]
            let nelsonSiegelSvensson = 
                let parameterNamesNSS = [|"b0";"b1";"b2";"b3";"t1";"t2"|]
                //b0 and t1 must be positive
                            
                let getFunctionValueNSS =                 
                    fun (parameterVector: Vector<float>) m -> 
                        let b0 = parameterVector.[0]
                        let b1 = parameterVector.[1]
                        let b2 = parameterVector.[2]
                        let b3 = parameterVector.[3]
                        let t1 = parameterVector.[4]
                        let t2 = parameterVector.[5]
                        b0+b1*((1.-exp(-m/t1))/(m/t1)) + b2*((1.-exp(-m/t1))/(m/t1)-exp(-m/t1)) + b3*((1.-exp(-m/t2))/(m/t2)-exp(-m/t2))                       

                let getGradientValuesNSS =
                    fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) m -> 
                        let b0 = parameterVector.[0]
                        let b1 = parameterVector.[1]
                        let b2 = parameterVector.[2]
                        let b3 = parameterVector.[3]
                        let t1 = parameterVector.[4]
                        let t2 = parameterVector.[5]
                        gradientVector.[0] <- 1.
                        gradientVector.[1] <- (t1*(1.-exp(-m/t1)))/m
                        gradientVector.[2] <- (t1*(1.-exp(-m/t1)))/m-exp(-m/t1)
                        gradientVector.[3] <- (t2*(1.-exp(-m/t2)))/m-exp(-m/t2)
                        gradientVector.[4] <- (exp(-m/t1)*((b2+b1)*t1**2.*exp(m/t1)+(-b2-b1)*t1**2.+(-b2-b1)*m*t1-b2*m**2.))/(m*t1**2.)
                        gradientVector.[5] <- b3*(-exp(-m/t2)/t2-(m*exp(-m/t2))/t2**2.+(1.-exp(-m/t2))/m)
                        gradientVector
                   
                createModel parameterNamesNSS getFunctionValueNSS getGradientValuesNSS
                        
            (*
            Example Nelson Siegel Svensson model
            let actual = [|0.0928; 0.106; 0.1174; 0.1246; 0.1395; 0.1489; 0.1686; 0.1806|]
            let time = [|1.0; 2.0; 3.0; 5.0; 7.0; 10.0; 15.0; 25.0|]
            let solverOptionsNSS = createSolverOption 0.0001 0.0001 5000 [|0.01;0.01;0.01;0.01;1.0;1.0|]
            let coefficientsNSS = LevenbergMarquardt.estimatedParams nelsonSiegelSvensson solverOptionsNSS 0.001 10. time actual
            let fittingFunctionNSS = nelsonSiegelSvensson.GetFunctionValue coefficientsNSS
            let expected = time |> Array.map fittingFunctionNSS

            
            let actual2 = [|12.71;13.13;13.34;13.78;13.77;13.75;13.74;13.62|]
            let time2 = [|3./12.;0.5;1.;2.;3.;5.;7.;10.|] 
            let initialParamGuess = [|13.;-1.;3.;1.;3.;1.|]
            *) 

