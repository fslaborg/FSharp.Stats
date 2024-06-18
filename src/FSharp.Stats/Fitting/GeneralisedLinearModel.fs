namespace FSharp.Stats.Fitting.GLM


open System
open FSharp.Stats
open Algebra.LinearAlgebra 

/// 
/// Represents the distribution families for Generalized Linear Models (GLMs).
type GlmDistributionFamily =
    /// Normal distribution family.
    | Normal
    /// Exponential distribution family.
    | Exponential
    /// Gamma distribution family.
    | Gamma
    /// Inverse Gaussian distribution family.
    | InverseGaussian
    /// Poisson distribution family.
    | Poisson
    /// Bernoulli distribution family.
    | Bernouli
    /// Binomial distribution family.
    | Binomial
    /// Categorical distribution family.
    | Categorical
    /// Multinomial distribution family.
    | Multinomial

// /// <summary>
// ///   Linear regression is used to estimate the relationship of one variable (y) with another (x) by expressing y in terms of a linear function of x.
// /// </summary>
/// Represents a collection of link functions used in a generalized linear model.
type LinkFunctions =
    | GetLink of (float -> float)
    | GetInvLink of (float -> float)
    | GetInvLinkDerivative of (float -> float)

/// Represents a link function used in a generalized linear model.
type LinkFunction =
    {
        /// Gets the link function.
        getLink: float -> float
        /// Gets the inverse link function.
        getInvLink: float -> float
        /// Gets the derivative of the link function.
        getDeriv: float -> float
        /// Gets the derivative of the inverse link function.
        getInvLinkDerivative: float -> float
    }
/// Represents the return type of a Generalised Linear Model (GLM).
type GLMReturn = 
    {
        /// The coefficients used in the GLM.
        mX: Vector<float>
        /// The predicted mean values of the GLM.
        mu: Vector<float>
    }

/// Represents the statistics of a Generalised Linear Model (GLM).
type GLMStatisticsModel = 
    {
        /// The log-likelihood of the GLM.
        LogLikelihood: float
        /// The deviance of the GLM.
        Deviance: float
        /// The Pearson chi-squared statistic of the GLM.
        PearsonChi2: float
        //PseudoR2:float
    }

/// Represents the parameters of a Generalised Linear Model (GLM).
type GLMStatisticsPrameter = 
    {
        //Name:string
        /// The coefficient of the parameter.
        Coefficient: float
        /// The standard error of the parameter.
        StandardError: float
        /// The Z-score of the parameter.
        ZScore: float
        /// The person of Z of the parameter.
        PersonOfZ: float
    }

/// This module contains various link functions used in generalized linear models.
module LinkFunctions =
    /// Clips the logistic values to avoid numerical instability.
    let internal clipLogisticValues (p : float)  = 
        let floatEps = 2.220446049250313e-16

        max floatEps (min (1.0-floatEps) p)

    /// Clips the logistic values to avoid numerical instability.
    let internal clipLogisticValues2 (p : float)  = 
        let floatEps = 2.220446049250313e-16

        max floatEps p

    /// The logit link function used in logistic regression.
    let LogitLinkFunction : LinkFunction =
        {
            // Computes the link function value for a given parameter.
            getLink = fun b -> 
                let p = clipLogisticValues b
                System.Math.Log(p / (1.0 - p))
            // Computes the inverse link function value for a given parameter.
            getInvLink = fun a -> 
                1.0 / (1.0 + System.Math.Exp(-a))
            // Computes the derivative of the link function for a given parameter.
            getDeriv = fun a ->
                let p = clipLogisticValues a
                1./(p*(1.-p))
            // Computes the derivative of the inverse link function for a given parameter.
            getInvLinkDerivative = fun a ->
                let t = System.Math.Exp(a)
                t / ((1.0 + t) * (1.0 + t))
        }

    /// The log link function used in Poisson regression.
    let LogLinkFunction : LinkFunction =
        {
            // Computes the link function value for a given parameter.
            getLink = fun b -> System.Math.Log((clipLogisticValues2 b))
            // Computes the inverse link function value for a given parameter.
            getInvLink = fun a -> System.Math.Exp(a)
            // Computes the derivative of the link function for a given parameter.
            getDeriv = fun a -> 1./(clipLogisticValues2 a) 
            // Computes the derivative of the inverse link function for a given parameter.
            getInvLinkDerivative = fun a -> System.Math.Exp(a)
        }

    /// The inverse squared link function used in gamma regression.
    let InverseSquaredLinkFunction: LinkFunction =
        {
            // Computes the link function value for a given parameter.
            getLink = fun b -> Math.Pow(b,-2.)//1.0 / b
            // Computes the inverse link function value for a given parameter.
            getInvLink = fun a -> Math.Pow(a,(1./ -2.))//1.0 / a
            // Computes the derivative of the link function for a given parameter.
            getDeriv = fun a -> -2. * (Math.Pow(a,(-2.-1.)))
            // Computes the derivative of the inverse link function for a given parameter.
            getInvLinkDerivative = fun a -> 
                let inv1 = 1. - -2.
                let inv2 = inv1 / -2.
                let inv3 = Math.Pow(a,inv2)
                inv3 / -2.
        }

    /// The inverse link function used in inverse Gaussian regression.
    let InverseLinkFunction: LinkFunction =
        {
            // Computes the link function value for a given parameter.
            getLink = fun b -> Math.Pow(b,-1.)//1.0 / b
            // Computes the inverse link function value for a given parameter.
            getInvLink = fun a -> Math.Pow(a,-1.)//1.0 / a
            // Computes the derivative of the link function for a given parameter.
            getDeriv = fun a -> -1. * (Math.Pow(a,(-1.-1.)))
            // Computes the derivative of the inverse link function for a given parameter.
            getInvLinkDerivative = fun a -> 
                let inv1 = 1. - -1.
                let inv2 = inv1 / -1.
                let inv3 = Math.Pow(a,inv2)
                inv3 / -1.
        }

    /// The identity link function used in linear regression.
    let IdentityLinkFunction: LinkFunction =
        {
            // Computes the link function value for a given parameter.
            getLink = fun b -> b
            // Computes the inverse link function value for a given parameter.
            getInvLink = fun a -> a
            // Computes the derivative of the link function for a given parameter.
            getDeriv = fun a -> 1.
            // Computes the derivative of the inverse link function for a given parameter.
            getInvLinkDerivative = fun a -> 1.
        }


module GlmDistributionFamily =
    /// Cleans a floating-point value by replacing it with a minimum threshold value.
    /// Returns the original value if it is greater than the threshold.
    /// Otherwise, returns the threshold value.
    let internal clean (p: float) = 
        let floatEps = 2.220446049250313e-16

        max floatEps p

    /// Returns the sign of a floating-point value.
    /// Returns 1.0 if the value is positive, 0.0 if it is zero, and -1.0 if it is negative.
    let internal signFunction x =
        if x > 0. then 1.
        elif x = 0. then 0.
        else -1.

    /// Calculates the variance for a given distribution family and value.
    /// 
    /// Parameters:
    ///   - mDistributionFamily: The distribution family.
    ///   - g: The value for which to calculate the variance.
    /// 
    /// Returns:
    ///   The variance for the given distribution family and value.
    let getVariance (mDistributionFamily: GlmDistributionFamily) (g: float)  =

        match mDistributionFamily with
        | GlmDistributionFamily.Multinomial ->
            g * (1.0 - g)
        | GlmDistributionFamily.Gamma ->
            (abs(g)) ** 2.
        | GlmDistributionFamily.InverseGaussian ->
            g * g * g
        | GlmDistributionFamily.Normal ->
            1.0
        | GlmDistributionFamily.Poisson ->
            (g)
        | GlmDistributionFamily.Bernouli   -> 
            g * (1.0 - g)
        | GlmDistributionFamily.Binomial    -> 
            let cleanG = max 1e-8  (min (1.0-1e-8) g)
            cleanG * (1.0 - cleanG)
        | GlmDistributionFamily.Categorical -> 
            g * (1.0 - g)
        | GlmDistributionFamily.Exponential ->
            g * (1.0 - g)
        | _ ->
            raise (System.NotImplementedException())

    /// Returns the link function associated with a distribution family.
    let getLinkFunction (mDistributionFamily: GlmDistributionFamily) =
        match mDistributionFamily with
        | GlmDistributionFamily.Multinomial ->
            LinkFunctions.LogitLinkFunction
        | GlmDistributionFamily.Gamma ->
            LinkFunctions.InverseLinkFunction
        | GlmDistributionFamily.InverseGaussian ->
            LinkFunctions.InverseSquaredLinkFunction
        | GlmDistributionFamily.Normal ->
            LinkFunctions.IdentityLinkFunction
        | GlmDistributionFamily.Poisson ->
            LinkFunctions.LogLinkFunction
        | GlmDistributionFamily.Exponential ->
            LinkFunctions.LogitLinkFunction
        | GlmDistributionFamily.Bernouli ->
            LinkFunctions.LogitLinkFunction
        | GlmDistributionFamily.Binomial ->
            LinkFunctions.LogitLinkFunction
        | GlmDistributionFamily.Categorical ->
            LinkFunctions.LogitLinkFunction
        | _ ->
            raise (System.NotImplementedException())
    
    /// Returns the weights associated with a distribution family given the mean.
    let getFamilyWeights (family: GlmDistributionFamily) (mu: Vector<float>) =
        let link = getLinkFunction family
        let deriv = link.getDeriv
        let variance = getVariance family
    
        mu
        |> Vector.map(fun m -> 
            1. / (((deriv m) ** 2) * (variance m))
        )

    /// Returns the residual deviance associated with a distribution family given the endogenous variable and the mean.
    let getFamilyResidualDeviance (family: GlmDistributionFamily) (endog: Vector<float>) (mu: Vector<float>) =
        match family with 
            | GlmDistributionFamily.Poisson ->
                Vector.map2(fun endV muV -> 
                    let a = clean(endV / muV)
                    let b = System.Math.Log(a)
                    let c = endV - muV
                    let d = endV * b - c
                    2. * d 
                ) endog mu
               |> Vector.sum          
            | GlmDistributionFamily.Normal ->
                Vector.map2(fun endV muV -> 
                    let a = endV - muV
                    a ** 2.
                ) endog mu
                |> Vector.sum
            | GlmDistributionFamily.Gamma ->
                Vector.map2(fun endV muV -> 
                    let a = clean(endV / muV)
                    let b = System.Math.Log(a)
                    let c = endV - muV
                    let d = c / muV
                    let e = -b + d
                    2. * d 
                ) endog mu
                |> Vector.sum
            // | GlmDistributionFamily.Binomial ->
            //     Vector.map2(fun endV muV -> 
            //         let endogmu = clean(endV / (muV + 1e-20))
            //         let nendogmu = clean((1. - endV) / (1. - muV + 1e-20))
            //         endV * System.Math.Log(endogmu) + (1. - endV) * System.Math.Log(nendogmu)
            //         |> fun x -> 2. * x * tries
            //     ) endog mu
            //     |> Vector.sum 
            | GlmDistributionFamily.InverseGaussian ->
                Vector.map2(fun endV muV -> 
                    1. / (endV * muV ** 2.) * (endV - muV) ** 2.
                ) endog mu
                |> Vector.sum
            | _ -> 
                raise (System.NotImplementedException())


module GLMStatistics =

    /// Calculates the log-likelihood of a generalised linear model.
    /// Parameters:
    ///   - b: The coefficient vector.
    ///   - mu: The mean vector.
    /// Returns: The log-likelihood value.
    let getLogLikelihood (b: Vector<float>) (mu: Vector<float>) = 
        Vector.mapi(fun i v -> 
            let y =  b.[i]
            let meanDist =  v 
            y * System.Math.Log(meanDist) - meanDist - (SpecialFunctions.Gamma.gammaLn(y+1.0))
        ) mu
        |> Vector.sum

    /// Calculates the chi-square statistic for a generalised linear model.
    /// Parameters:
    ///   - b: The coefficient vector.
    ///   - mu: The mean vector.
    ///   - family: The distribution family.
    /// Returns: The chi-square statistic value.
    let getChi2 (b: Vector<float>) (mu: Vector<float>) (family: GlmDistributionFamily) =
        Vector.map2(fun y yi -> 
            let a = y - yi
            let nominator = a**2.
            nominator / (GlmDistributionFamily.getVariance family yi)
        ) b mu
        |> Vector.sum

    // let internal testR2 (b:Vector<float>) (linpred:Vector<float>) = 
    //     let yMean = Vector.mean b
    //     let tss = 
    //         Vector.map(fun y -> (y-yMean)**2.) b  
    //         |> Vector.sum
    //     let rss = 
    //         Vector.map2(fun y yhat -> (y-yhat)**2.) b linpred 
    //         |> Vector.sum
    //     let r2 = 1. - (rss / tss)
    //     r2

    let getGLMStatisticsModel (b:Vector<float>) (glmResult:GLMReturn) (family: GlmDistributionFamily) = 
        let logLikelihood = getLogLikelihood b glmResult.mu
        let deviance = GlmDistributionFamily.getFamilyResidualDeviance family b glmResult.mu 
        let chi2 = getChi2 b glmResult.mu family
        //let r2 = testR2 b (glmResult.mX * A)

        {
            LogLikelihood=logLikelihood
            Deviance=deviance
            PearsonChi2=chi2
            //PseudoR2=0.
        }

    /// Calculates the standard errors for the coefficients in a generalized linear model.
    /// The standard errors are calculated using the formula: sqrt(diagonal elements of (A^T * W * A)^-1)
    /// where A is the design matrix, b is the response vector, and W is the weight vector.
    let getStandardError (A: Matrix<float>) (b: Vector<float>) (W: Vector<float>) =
        let At :Matrix<float> = Matrix.transpose A
        let WMatrix = Matrix.diag W
        let AtW = At * WMatrix
        let AtWA :Matrix<float> = AtW*A
        let AtWAInv = Algebra.LinearAlgebra.Inverse AtWA

        let n = AtWAInv.NumRows
        let m = Vector.length b
        let stndErrors: Vector<float> = 
            Vector.init n (fun v -> 
                Matrix.get AtWAInv v v
                |> fun x -> System.Math.Sqrt(x)
            )
        stndErrors

    /// Calculates the Z-statistic for the coefficients in a generalized linear model.
    /// The Z-statistic is calculated as the ratio of the coefficient estimate to its standard error.
    let getZStatistic (mx: Vector<float>) (stndError: Vector<float>) = 
        Vector.map2 (fun x y -> 
            x/y
        ) mx stndError

    /// Calculates the p-value using the z-statistic.
    /// The p-value is calculated as 2 * (1 - phi), where phi is the cumulative distribution function (CDF) of the standard normal distribution.
    /// The z-statistic is a vector of values for which the p-value is calculated.
    let getPearsonOfZ (zStatistic: Vector<float>) = 
        Vector.map(fun x ->
            let phi = Distributions.Continuous.Normal.CDF 0. 1. (abs(x))
            let pValue = 2. * (1. - phi)
            pValue
        )zStatistic

    let getGLMParameterStatistics (A:Matrix<float>) (b:Vector<float> ) (solved:GLMReturn) (names:string seq) =

        let stndErrors = getStandardError A b solved.mu
        let zStatistic = getZStatistic solved.mX stndErrors
        let pValue = getPearsonOfZ zStatistic
        Seq.init (Vector.length solved.mX) (fun i -> 
            Seq.item i names,
            {
                Coefficient=solved.mX.[i]
                StandardError=stndErrors.[i]
                ZScore=zStatistic.[i]
                PersonOfZ=pValue.[i]
            }
        )

module internal QRSolver =


    /// Performs a stepwise gain QR calculation for a generalised linear model.
    /// This function calculates the cost, updated mean values, updated linear predictions,
    /// weighted least squares results, and weighted least squares endogenous values for a given
    /// matrix A, vector b, distribution family, vector t, vector mu, vector linPred, and old result.
    let stepwiseGainQR 
        (A: Matrix<float>) 
        (b: Vector<float>) 
        (mDistributionFamily: GlmDistributionFamily) 
        (t:Vector<float>) 
        (mu:Vector<float>)
        (linPred:Vector<float>)
        (oldResult:Vector<float>)
        = 

        let m = A.NumRows
        let n = A.NumCols

        // Get the link function in accordance to the distribution type
        let linkFunction= GlmDistributionFamily.getLinkFunction mDistributionFamily

        // Calculate the family weights for each observation
        let famWeight = GlmDistributionFamily.getFamilyWeights mDistributionFamily mu
        
        // Calculate the self-weights for each observation
        let selfWeights = 
            Vector.init m (fun i -> t[i] * (float 1.) * famWeight[i])
        
        // Calculate the derivatives of the link function at each observation
        let derivs = Vector.map(fun x -> linkFunction.getDeriv x) mu
        
        // Calculate the endogenous values for the weighted least squares
        let wlsendog: Vector<float> = Vector.init m (fun i -> linPred[i] + derivs[i] * (b[i]-mu[i]))        

        // Calculate the weighted endogenous values and the weighted exogenous matrix
        let wlsendog2,wlsexdog: Vector<float>*Matrix<float> = 
            let whalf = Vector.map(fun x -> System.Math.Sqrt(x)) selfWeights
            let en = Vector.init m (fun i -> whalf[i] * wlsendog[i])
            let ex = 
                A
                |> Matrix.toJaggedArray
                |> Array.mapi(fun i x ->
                    x
                    |> Array.map(fun v -> v*whalf[i])
                )
                |> Matrix.ofJaggedArray
            en,ex

        // Solve the linear system using QR decomposition
        let (wlsResults: Vector<float>),R = solveLinearQR wlsexdog wlsendog2

        // Calculate the new linear predictions
        let linPred_new: Vector<float> = A * wlsResults

        // Calculate the new mean values
        let mu_new = Vector.init m (fun i -> linkFunction.getInvLink(linPred_new[i]))

        // Calculate the cost of this step
        let cost:float = 
            oldResult - wlsResults 
            |> Vector.norm

        cost,mu_new,linPred_new,wlsResults,wlsendog

    /// This function performs a loop until the maximum number of iterations or until the cost for the gain is smaller than a given tolerance.
    /// It uses a cost function to calculate the cost, update the parameters, and check the termination condition.
    /// The loop stops when the maximum number of iterations is reached or when the cost is smaller than the tolerance.
    /// Returns the final values of the parameters and intermediate results.
    let internal loopTilIterQR 
        (A: Matrix<float>) 
        (b: Vector<float>) 
        (mDistributionFamily: GlmDistributionFamily) 
        (maxIter: int) 
        (mTol: float) 
        (costFunction: 
            Matrix<float> -> 
            Vector<float> -> 
            GlmDistributionFamily -> 
            Vector<float> -> 
            Vector<float> -> 
            Vector<float> -> 
            Vector<float> -> 
            float * Vector<float> * Vector<float> * Vector<float> * Vector<float>
        ) = 

        let m = A.NumRows
        let n = A.NumCols

        // Initialize an empty vector x
        let t_original: Vector<float>   = Vector.init m (fun i -> 1.)
        let bMean: float                = Vector.mean b
        let muStart:Vector<float>       = Vector.map(fun x -> ((x+bMean)/2.)) b
        let linPredStart: Vector<float> = Vector.init m (fun k -> GlmDistributionFamily.getLinkFunction(mDistributionFamily).getLink(muStart[k]))

        // Run the costFunction until maxIter has been reached or the cost for the gain is smaller than mTol
        let rec loopTilMaxIter (t: Vector<float>) (loopCount: int) (mu:Vector<float>) (linPred:Vector<float>) (wlsResult: Vector<float>) (wlsendog: Vector<float>) =
            if loopCount = maxIter then
                t_original,mu,linPred,wlsResult,wlsendog
            else
                let cost,mu_new,linPred_new,wlsResult_new,wlsendogNew = 
                    costFunction 
                        A 
                        b 
                        mDistributionFamily 
                        t_original  
                        mu 
                        linPred 
                        wlsResult

                if loopCount%10 = 0 then
                    printfn $"Iteration {loopCount}, Cost {cost}"

                if cost < mTol then
                    t_original,mu,linPred,wlsResult,wlsendog
                else
                    loopTilMaxIter t_original (loopCount+1) mu_new linPred_new wlsResult_new wlsendogNew


        loopTilMaxIter t_original 0 muStart linPredStart (Vector.zeroCreate n) (Vector.zeroCreate m)
    /// Solves a generalized linear model using the QR decomposition and Newton's method.
    ///
    /// Parameters:
    ///   - A: The design matrix.
    ///   - b: The response vector.
    ///   - maxIter: The maximum number of iterations.
    ///   - mDistributionFamily: The distribution family of the model.
    ///   - mTol: The tolerance for convergence.
    ///
    /// Returns: The solved generalized linear model.
    let solveQrNewton
        (A: Matrix<float>) 
        (b: Vector<float>) 
        (maxIter: int) 
        (mDistributionFamily: GlmDistributionFamily) 
        (mTol: float) =
        let m = A.NumRows
        let n = A.NumCols

        System.Diagnostics.Debug.Assert(m >= n) 

        let t,mu,linPred,wlsResult,wlsendog = 
            loopTilIterQR A b mDistributionFamily maxIter mTol stepwiseGainQR 

        let mX,R = wlsResult,wlsendog

        {mX=mX;mu=mu}

module SolveGLM = 

    /// Solves a generalized linear model using the QR decomposition and Newton's method.
    ///
    /// Parameters:
    ///   - A: The design matrix.
    ///   - b: The response vector.
    ///   - maxIter: The maximum number of iterations.
    ///   - mDistributionFamily: The distribution family of the model.
    ///   - mTol: The tolerance for convergence.
    ///
    /// Returns: The solved generalized linear model.
    let solveQR (A: Matrix<float>) (b: Vector<float>) (maxIter: int) (mDistributionFamily: GlmDistributionFamily) (mTol: float) = 
        QRSolver.solveQrNewton (A: Matrix<float>) (b: Vector<float>) (maxIter: int) (mDistributionFamily: GlmDistributionFamily) (mTol: float)
    
