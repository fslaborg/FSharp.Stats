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

    let updateJacobianInplace (model: Model) dataPointCount (xData: float[]) (paramVector: Vector<float>) (jacobian: Matrix<float> ) =
        // Nr. of Parameters
        let paramCount = paramVector.Length
        // populate Jacobian Matrix
        for i = 0 to dataPointCount-1 do 
            let gradient = Vector.zero paramCount
            model.GetGradientValue paramVector gradient xData.[i] |> ignore
            Matrix.setRow jacobian i gradient            
        jacobian

    /// Returns the residual vector, each row i contains the difference between the yEst_i and the yData_i. 
    let updateResidualVectorInPlace (model: Model) dataPointCount (xData: float[]) (yData: float []) (paramVector: Vector<float>) (residualVector: Vector<float>) = 
        for i = 0 to dataPointCount-1 do 
            let yValueEst = model.GetFunctionValue paramVector xData.[i]
            residualVector.[i] <- (yValueEst - yData.[i])
        residualVector

    /// Returns true if convergence criteria are met or a user defined number of iiterations has been carried out
    let private shouldTerminate (currentValueRSS: float) (newValueRSS: float) (iterationCount:int) (currentParamGuess:Vector<float>) 
            (newParamGuess:Vector<float>) (solverOptions: SolverOptions)  = 
        if abs (newValueRSS-currentValueRSS) <= solverOptions.MinimumDeltaValue ||
            Vector.sub newParamGuess currentParamGuess |> Vector.norm <= solverOptions.MinimumDeltaParameters ||
            iterationCount >= solverOptions.MaximumIterations then
            false
        else 
            true
            
    module GaussNewton = 

        /// Returns a parameter vector as a possible solution for linear least square based nonlinear fitting of a given dataset (xData, yData) with a given 
        /// model function. 
        let estimatedParamsVerbose (model: Model) (solverOptions: SolverOptions) (xData: float[]) (yData: float []) = 
            let paramsAtIteration = new ResizeArray<vector>()
            let mutable anotherIteration = true
            let mutable currentValueRSS = 0.0
            let mutable newValueRSS = 0.0
            let paramCount = solverOptions.InitialParamGuess.Length
            let dataPointCount = xData.Length
            let currentParamGuess = Vector.ofArray solverOptions.InitialParamGuess
            let jacobian       = Matrix.create dataPointCount paramCount 0.
            let residualVector = Vector.create dataPointCount 0.
            ///
            currentValueRSS <- getRSS model xData yData currentParamGuess
            while (anotherIteration = true) do 
                /// 
                let jacobian' = updateJacobianInplace model dataPointCount xData currentParamGuess jacobian            
                ///
                let residualVector' = updateResidualVectorInPlace model dataPointCount xData yData currentParamGuess residualVector 
                ///
                let hessian = jacobian'.Transpose * jacobian' 
                /// 
                let step = 
                     LinearAlgebra.SolveLinearSystem (LinearAlgebra.Cholesky hessian) (Matrix.mulV (Matrix.transpose jacobian') residualVector')     
                ///
                let newParamGuess = currentParamGuess - step
                /// 
                newValueRSS <- getRSS model xData yData newParamGuess
                ///
                paramsAtIteration.Add(newParamGuess) |> ignore       
                /// 
                anotherIteration <- shouldTerminate currentValueRSS newValueRSS paramsAtIteration.Count currentParamGuess newParamGuess solverOptions
                /// 
                Vector.inplace_mapi (fun i _ -> newParamGuess.[i]) currentParamGuess
                /// 
                currentValueRSS <- newValueRSS
            paramsAtIteration

        let estimatedParams (model: Model) (solverOptions: SolverOptions) (xData: float[]) (yData: float []) = 
            let estParams = estimatedParamsVerbose model solverOptions xData yData
            estParams.[estParams.Count-1]

    module LevenbergMarquardt = 

        /// Returns a parameter vector as a possible solution for linear least square based nonlinear fitting of a given dataset (xData, yData) with a given 
        /// model function. 
        let estimatedParamsVerbose (model: Model) (solverOptions: SolverOptions) lambdaInitial lambdaFactor (xData: float[]) (yData: float []) = 
            let paramsAtIteration = new ResizeArray<vector>()
            let mutable anotherIteration = true
            let mutable lambda = lambdaInitial
            let mutable currentValueRSS = 0.0
            let mutable newValueRSS = 0.0
            let paramCount = solverOptions.InitialParamGuess.Length
            let dataPointCount = xData.Length
            let currentParamGuess = Vector.ofArray solverOptions.InitialParamGuess
            let jacobian = Matrix.zero dataPointCount paramCount
            let residualVector = Vector.zero dataPointCount
            currentValueRSS <- getRSS model xData yData currentParamGuess      
            while (anotherIteration = true) do 
                /// 
                let jacobian' = updateJacobianInplace model dataPointCount xData currentParamGuess jacobian 
                ///
                let residualVector' = updateResidualVectorInPlace model dataPointCount xData yData currentParamGuess residualVector 
                ///
                let hessian = jacobian'.Transpose * jacobian' 
                ///
                let diagonal = Matrix.initDiagonal hessian.Diagonal
                ///
                let step = LinearAlgebra.SolveLinearSystem ((hessian + Matrix.scale lambda diagonal) |> LinearAlgebra.Cholesky) (Matrix.mulV (Matrix.transpose jacobian') residualVector')
                ///
                let newParamGuess = currentParamGuess - step
                /// 
                newValueRSS <- getRSS model xData yData newParamGuess
                //
                paramsAtIteration.Add(newParamGuess)     
                /// 
                anotherIteration <- shouldTerminate currentValueRSS newValueRSS paramsAtIteration.Count currentParamGuess newParamGuess solverOptions
                ///
                if newValueRSS < currentValueRSS then
                    Vector.inplace_mapi (fun i _ -> newParamGuess.[i]) currentParamGuess
                    currentValueRSS <- newValueRSS
                else
                    lambda <- lambda * lambdaFactor
            paramsAtIteration

        let estimatedParams (model: Model) (solverOptions: SolverOptions) lambdaInitial lambdaFactor (xData: float[]) (yData: float []) = 
            let estParams = estimatedParamsVerbose model solverOptions  lambdaInitial lambdaFactor xData yData
            estParams.[estParams.Count-1]

        ///
        let solverConverged (solverOptions: SolverOptions) (estParams:ResizeArray<vector>) =
            solverOptions.MaximumIterations = estParams.Count 
            |> not 
            
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
        