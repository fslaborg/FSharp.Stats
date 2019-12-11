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
                if Vector.map3 (fun l u x -> if l <= x && u >= x then x else nan) lowerBound upperBound parameters |> Vector.exists nan.Equals then 
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
        let expSolverOptions (x_data:float []) (y_data:float [])= 
            //gets the linear representation of the problem and solves it by simple linear regression
            let initialParamGuess =
                let y_ln = y_data |> Array.map (fun x -> Math.Log(x)) |> vector
                let linearReg = LinearRegression.OrdinaryLeastSquares.Linear.Univariable.coefficient (vector x_data) y_ln
                let a = exp linearReg.[0]
                let b = linearReg.[1]
                [|a;b|]

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
                                    a * exp((-0.5)*((xValue-m)/s)**2.) * (s/t) * sqrt(Math.PI/2.) * FSharp.Stats.SpecialFunctions.Errorfunction.erfcx ((1./(sqrt 2.)) * ((s/t) - ((xValue-m)/s)))
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
    let LogisticFunction = {
        ParameterNames= [|"L - curve maximum";"k - Steepness"; "x0 xValue of midpoint"|]
        GetFunctionValue = (fun (parameterVector:Vector<float>) xValue -> parameterVector.[0] / (1. + exp(parameterVector.[1]*(xValue-parameterVector.[2]))))
        GetGradientValue = (fun (parameterVector:Vector<float>) (gradientVector: Vector<float>) xValue ->
                            gradientVector.[0] <- 1. / (1. + exp(parameterVector.[1]*(xValue-parameterVector.[2])))
                            gradientVector.[1] <- (parameterVector.[0] * (xValue-parameterVector.[2]) * exp(parameterVector.[1]*(xValue-parameterVector.[2])) ) / (exp(parameterVector.[1]*(xValue-parameterVector.[2])) + 1.)**2.
                            gradientVector.[2] <- (parameterVector.[0] * parameterVector.[1] * exp(parameterVector.[1]*(xValue-parameterVector.[2])) ) / (exp(parameterVector.[1]*(xValue-parameterVector.[2])) + 1.)**2.
                            gradientVector)
        }

    /// Logistic function of the form "y = L/(1+e^(k(t-x)))+N"
    /// Modified version of the Logistic function model with a variable curve minimum.
    let LogisticFunctionVarY = {
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