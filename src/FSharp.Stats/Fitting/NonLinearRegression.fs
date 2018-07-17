namespace FSharp.Stats.Fitting


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
                            acc + ((yValueEst - yValue) **2.)
                        ) 0.0 xData yData
        
        sumOfSquaredResiduals

    let updateJacobianInplace (model: Model) (xData: float[]) (paramVector: Vector<float>) (jacobian: Matrix<float> ) =
        // Nr. of Parameters
        let paramCount = paramVector.Length
        // populate Jacobian Matrix
        for i = 0 to xData.Length-1 do 
            let gradient = Vector.zero paramCount
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
        if abs (newValueRSS-currentValueRSS) <= solverOptions.MinimumDeltaValue ||
            Vector.sub newParamGuess currentParamGuess |> Vector.norm <= solverOptions.MinimumDeltaParameters ||
            iterationCount >= solverOptions.MaximumIterations then
            false
        else 
            true
    
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
            let residualVector = Vector.zero xData.Length
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
            let residualVector = Vector.zero xData.Length
            let jacobian = Matrix.zero xData.Length solverOptions.InitialParamGuess.Length
            let initialValueRSS = getRSS model xData yData initialParamGuess  
            let rec loop lambda jacobian residualVector currentParamGuess currentValueRSS (paramsAtIteration:ResizeArray<vector>) = 
                let jacobian' = updateJacobianInplace model xData currentParamGuess jacobian 
                let residualVector' = updateResidualVectorInPlace model xData yData currentParamGuess residualVector
                let hessian = jacobian'.Transpose * jacobian' 
                let diagonal = Matrix.initDiagonal hessian.Diagonal
                let modHessian = (hessian + Matrix.scale lambda diagonal) 
                let step = LinearAlgebra.LeastSquares modHessian (Matrix.mulV (jacobian'.Transpose) residualVector')
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
            //paramsAtIteration   

        /// Returns a parameter vector as a possible solution for linear least square based nonlinear fitting of a given dataset (xData, yData) with a given 
        /// model function. 
        let estimatedParams (model: Model) (solverOptions: SolverOptions) lambdaInitial lambdaFactor (xData: float[]) (yData: float []) = 
            let estParams = estimatedParamsVerbose model solverOptions  lambdaInitial lambdaFactor xData yData
            estParams.[estParams.Count-1]

            
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
    /// Exponentially modified Gaussian (EMG) of the form "y =  ((amp*std)/tau) * sqrt(PI/2.) * exp(1./2. * ((std/tau)**2.) - ((x-meanX)/tau)) * Erfc((1./sqrt(2.)) * ((std/tau)-((x-meanX)/std)))"

        let emgModel = {
            ParameterNames= [|"amp";"meanX";"std";"tau"|]
            GetFunctionValue = (fun (parameterVector:Vector<float>) xValue ->  ((parameterVector.[0]*parameterVector.[2])/parameterVector.[3]) * sqrt(System.Math.PI/2.) * exp(1./2. * ((parameterVector.[2]/parameterVector.[3])**2.) - ((xValue-parameterVector.[1])/parameterVector.[3])) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc((1./sqrt(2.)) * ((parameterVector.[2]/parameterVector.[3])-((xValue-parameterVector.[1])/parameterVector.[2]))) )
            GetGradientValue = (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue -> 
                                gradientVector.[0] <- (1./parameterVector.[3]) * 1.25331 * parameterVector.[2] * exp( (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3]) ) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc(0.707107 * ( (parameterVector.[2] / parameterVector.[3] ) - ( (xValue-parameterVector.[1]) / parameterVector.[2]) ) )
                            //0 passt
                                gradientVector.[1] <-  ( (1./parameterVector.[3]**2.) *  1.25331 * parameterVector.[0] * parameterVector.[2] * exp( (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3]) ) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc(0.707107 * ( (parameterVector.[2] / parameterVector.[3] ) - ( (xValue-parameterVector.[1]) / parameterVector.[2]) ) ) )  
                                                        - ( (1./parameterVector.[3]) * parameterVector.[0] * exp( ( (0.5 * parameterVector.[2]**2.) / parameterVector.[3]**2. ) - (0.5 * (((parameterVector.[2]/parameterVector.[3]) - ( (xValue-parameterVector.[1]) / parameterVector.[2] ) )**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3] ) ) )
                            //1 passt
                                gradientVector.[2] <-     ( (1./ (parameterVector.[3]))                                 * 1.25331 * parameterVector.[0] * exp( (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3]) ) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc(0.707107 * ( (parameterVector.[2] / parameterVector.[3] ) - ( (xValue-parameterVector.[1]) / parameterVector.[2]) ) )) 
                                                        + ( (1./ (parameterVector.[3]**3.)) * (parameterVector.[2]**2.) * 1.25331 * parameterVector.[0] * exp( (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3]) ) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc(0.707107 * ( (parameterVector.[2] / parameterVector.[3] ) - ( (xValue-parameterVector.[1]) / parameterVector.[2]) ) ))
                                                        - ( (1./ (parameterVector.[3]))  )  * (parameterVector.[2])     * 1.00000 * parameterVector.[0] * exp( (-0.5*( (parameterVector.[2] / parameterVector.[3])  - ( (xValue-parameterVector.[1]) / parameterVector.[2]) )**2. ) - ((xValue-parameterVector.[1]) / parameterVector.[3]) + (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) ) * ( ((xValue-parameterVector.[1]) / (parameterVector.[2]**2.) ) + 1./parameterVector.[3] )    
                        
                                gradientVector.[3] <-  - ( (1./ (parameterVector.[3]**2.))  * (parameterVector.[2])     * 1.25331 * parameterVector.[0] * exp( (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3]) ) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc(0.707107 * ( (parameterVector.[2] / parameterVector.[3] ) - ( (xValue-parameterVector.[1]) / parameterVector.[2]) ) )) 
                                               
                                                        + ( (1./ (parameterVector.[3]))      * (parameterVector.[2])     * 1.25331 * parameterVector.[0] * exp( (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) - ( (xValue-parameterVector.[1]) / parameterVector.[3]) ) * FSharp.Stats.SpecialFunctions.Errorfunction.Erfc(0.707107 * ( (parameterVector.[2] / parameterVector.[3] ) - ( (xValue-parameterVector.[1]) / parameterVector.[2]) ) )) 
                                                        * ( ((xValue-parameterVector.[1]) / parameterVector.[3]**2. ) - ((parameterVector.[2]**2.) / (parameterVector.[3]**3.)) )
                                               
                                                        + ( (1./ (parameterVector.[3]**3.))  )   * (parameterVector.[2]**2.)     * 1.00000 * parameterVector.[0] * exp( (-0.5*( (parameterVector.[2] / parameterVector.[3])  - ( (xValue-parameterVector.[1]) / parameterVector.[2])**2. ) ) - ((xValue-parameterVector.[1]) / parameterVector.[3]) + (0.5*(parameterVector.[2]**2.) / (parameterVector.[3]**2.) ) ) 
                                gradientVector ) 
            }


        let emgSolverOptions initialParamGuess = {
            MinimumDeltaValue       = 0.001
            MinimumDeltaParameters  = 0.001  
            MaximumIterations       = 10000
            //[|"amp";"meanX";"std";"tau"|]
            InitialParamGuess       = initialParamGuess
            }
        