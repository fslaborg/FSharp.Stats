namespace FSharp.Stats.Fitting.GLM


open System
open FSharp.Stats

// /// <summary>
// ///   Linear regression is used to estimate the relationship of one variable (y) with another (x) by expressing y in terms of a linear function of x.
// /// </summary>
type LinkFunctions =
    | GetLink of (float -> float)
    | GetInvLink of (float -> float)
    | GetInvLinkDerivative of (float -> float)

type LinkFunction =
    {
        getLink: float -> float
        getInvLink: float -> float
        getDeriv: float -> float
        getInvLinkDerivative: float -> float
    }

module LinkFunctions =
    let internal clipLogisticValues (p : float)  = 
        let floatEps = 2.220446049250313e-16

        max floatEps (min (1.0-floatEps) p)
    let internal clipLogisticValues2 (p : float)  = 
        let floatEps = 2.220446049250313e-16

        max floatEps p

    let LogitLinkFunction : LinkFunction =
        {
            getLink = fun b -> 
                let p = clipLogisticValues b
                System.Math.Log(p / (1.0 - p))
            getInvLink = fun a -> 
                1.0 / (1.0 + System.Math.Exp(-a))
            getDeriv = fun a ->
                let p = clipLogisticValues a
                1./(p*(1.-p))
            getInvLinkDerivative = fun a ->
                let t = System.Math.Exp(a)
                t / ((1.0 + t) * (1.0 + t))
        }

    let LogLinkFunction : LinkFunction =
        {
            getLink                 = fun b -> System.Math.Log((clipLogisticValues2 b))
            getInvLink              = fun a -> System.Math.Exp(a)
            getDeriv                = fun a -> 1./(clipLogisticValues2 a) 
            getInvLinkDerivative    = fun a -> System.Math.Exp(a)
        }

    let InverseSquaredLinkFunction: LinkFunction =
        {
            getLink                 = fun b -> Math.Pow(b,-2.)//1.0 / b
            getInvLink              = fun a -> Math.Pow(a,(1./ -2.))//1.0 / a
            getDeriv                = fun a -> -2. * (Math.Pow(a,(-2.-1.)))
            getInvLinkDerivative    = fun a -> 
                let inv1 = 1. - -2.
                let inv2 = inv1 / -2.
                let inv3 = Math.Pow(a,inv2)
                inv3 / -2.
        }

    let InverseLinkFunction: LinkFunction =

               // linkfun <- function(mu) 1/mu
               //linkinv <- function(eta) 1/eta
               //mu.eta <- function(eta) -1/(eta^2)
        {
            getLink                 = fun b -> Math.Pow(b,-1.)//1.0 / b
            getInvLink              = fun a -> Math.Pow(a,-1.)//1.0 / a
            getDeriv                = fun a -> -1. * (Math.Pow(a,(-1.-1.)))
            getInvLinkDerivative    = fun a -> 
                let inv1 = 1. - -1.
                let inv2 = inv1 / -1.
                let inv3 = Math.Pow(a,inv2)
                inv3 / -1.
                
                //-1.0 / (a * a)
        }

    let IdentityLinkFunction: LinkFunction =
        {
            getLink                 = fun b -> b
            getInvLink              = fun a -> a
            getDeriv                = fun a -> 1.
            getInvLinkDerivative    = fun a -> 1.
        }


type GlmDistributionFamily =
    |Normal
    |Exponential
    |Gamma
    |InverseGaussian
    |Poisson
    |Bernouli
    |Binomial
    |Categorical
    |Multinomial

module GlmDistributionFamily =
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
    
    let getFamilyWeights (family:GlmDistributionFamily) (mu:Vector<float>) =
        let link = getLinkFunction family
        let deriv = link.getDeriv
        let variance = getVariance family
    
        mu
        |> Vector.map(fun m -> 
            1./(((deriv m)**2) * (variance m))
        )

    let internal clean (p: float) = 
        let floatEps = 2.220446049250313e-16

        max floatEps p

    let internal signFunction x =
        if x>0. then 1.
        elif x=0. then 0.
        else -1.

    let getFamilyReisualDeviance (family:GlmDistributionFamily) (endog: Vector<float>) (mu:Vector<float>) =
        match family with 
            |GlmDistributionFamily.Poisson ->
                Vector.map2(fun endV muV -> 
                    let a = clean(endV/muV)
                    let b = System.Math.Log(a)
                    let c = endV-muV
                    let d = endV * b - c
                    2.*d 
                ) endog mu
               |> Vector.sum          

            | _ -> 
                raise (System.NotImplementedException())

type GLMStatistics = 
    {
        StandardErrors:Vector<float>
        ResidualStandardDeviation:float
        ResponseMean:float
        ResponseVariance:float
        R2:float
        AdjustedR2:float
    }

type GLMStatisticsPython = 
    {
        LogLikelihood:float
        Deviance:float
        PearsonChi2:float
        PseudoR2:float
    }

module GLMStatistics =
    let internal scalarMultiply (matrix:Matrix<float>) (vector:Vector<float>) =
        let m = matrix.NumRows
        let n = matrix.NumCols

        let results = Matrix.zero m n 
        for i=0 to m-1 do
            let scalar = Vector.get vector i
            let row = Matrix.getRow matrix i
            let scalarRow = 
                row*scalar
                |> RowVector.toArray|>Vector.ofArray
            Matrix.setRow results i scalarRow
        results
    

    let getStandardError (A: Matrix<float>) (b: Vector<float>) (W: Vector<float>) (mX:Vector<float>) (mDistributionFamily: GlmDistributionFamily) =
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




    let getStatisticsQR (A: Matrix<float>) (b: Vector<float>) (W: Vector<float>) (mX:Vector<float>) (mDistributionFamily: GlmDistributionFamily) =
        let At :Matrix<float> = Matrix.transpose A
        let AtW = scalarMultiply At W
        let AtWA :Matrix<float> = AtW*A
        let AtWAInv = Algebra.LinearAlgebra.Inverse AtWA

        let n = AtWAInv.NumRows
        let m = Vector.length b

        let rec crossProdLoop crossProd i j =
            if j=n then
                crossProd
            else
                let elementA: float = (Matrix.get A i j)
                let elementmX: float = mX[j]
                let crossProdNew = crossProd + (elementA*elementmX)
                crossProdLoop (crossProdNew) i (j+1)

        let linkFunction = GlmDistributionFamily.getLinkFunction mDistributionFamily

        let stndErrors: Vector<float> = Vector.init n (fun v -> Matrix.get AtWAInv v v)

        let outcomes: Vector<float> = m |> Vector.zeroCreate
        let residuals: Vector<float> = m |> Vector.zeroCreate
            
        for count=0 to m-1 do
            let crossProd       = crossProdLoop 0. count 0
            let elementB        = b[count]
            let link            = linkFunction.getInvLink crossProd
            
            residuals[count]    <-  (elementB-link)
            outcomes[count]     <-  (elementB) 
        
        let getStdDev (vec:Vector<float>) (mean:float) =
            Vector.fold (fun folder v -> 
                let a     = v - mean
                let valNew  = System.Math.Pow(a,2)
                folder + valNew 
            ) 0. vec
            |> fun x -> (System.Math.Sqrt((x)/float vec.Length))

        let residualStdDev = getStdDev residuals 0.
        let responseMean =   Vector.mean(outcomes)
        let responseVariance = 
            let v = getStdDev outcomes responseMean
            System.Math.Pow(v, 2)
        let r2 = 1. - residualStdDev * residualStdDev / responseVariance
        let adjustedR2 = 1. - (residualStdDev * residualStdDev) / responseVariance * (float n) / ((float n) - (float mX.Length) - 1.)
        
        {
            StandardErrors=stndErrors
            ResidualStandardDeviation=residualStdDev
            ResponseMean=responseMean
            ResponseVariance=responseVariance
            R2=r2
            AdjustedR2=adjustedR2
        }

    let getLogLikelihood (b:Vector<float>) (mu: vector) = 
        Vector.mapi(fun i v -> 
            let y =  b.[i]
            let meanDist =  v 
            y * System.Math.Log(meanDist) - meanDist - (SpecialFunctions.Gamma.gammaLn(y+1.0))
        ) mu
        |> Vector.sum

    let getSumOfSquares (b:Vector<float>) (linPred: vector) = 
        Vector.mapi(fun i v -> 
            let y =  b.[i]
            let yi =  v 
            let a = y - yi
            a*a
        ) linPred
        
    let getchi2 (b:Vector<float>) (linPred: vector) = 
        Vector.map2(fun y yi -> 
            let a = y - yi
            let nominator = a*a
            nominator / yi
        ) b linPred
        |> Vector.sum

    let getStatisticsIRLS (A: Matrix<float>) (b: Vector<float>) (mDistributionFamily: GlmDistributionFamily) (vcovmat: Matrix<float>) (mX: Vector<float>)  =
        let n = vcovmat.NumRows
        let m = Vector.length b

        let rec crossProdLoop crossProd i j =
            if j=n then
                crossProd
            else
                let elementA: float = (Matrix.get A i j)
                let elementmX: float = mX[j]
                let crossProdNew = crossProd + (elementA*elementmX)
                crossProdLoop (crossProdNew) i (j+1)

        let linkFunction = GlmDistributionFamily.getLinkFunction mDistributionFamily

        let stndErrors: Vector<float> = Vector.init n (fun v -> Matrix.get vcovmat v v)

        let outcomes: Vector<float> = m |> Vector.zeroCreate
        let residuals: Vector<float> = m |> Vector.zeroCreate

        for count=0 to m-1 do
            let crossProd       = crossProdLoop 0. count 0
            let elementB        = b[count]
            let link            = linkFunction.getInvLink crossProd
            
            residuals[count]    <-  (elementB-link)
            outcomes[count]     <-  (elementB) 
        
        let getStdDev (vec:Vector<float>) (mean:float) =
            Vector.fold (fun folder v -> 
                let a     = v - mean
                let valNew  = System.Math.Pow(a,2)
                folder + valNew 
            ) 0. vec
            |> fun x -> (System.Math.Sqrt((x)/float vec.Length))

        let residualStdDev = getStdDev residuals 0.
        let responseMean =   Vector.mean(outcomes)
        let responseVariance = 
            let v = getStdDev outcomes responseMean
            System.Math.Pow(v, 2)

        let r2 = 1. - residualStdDev * residualStdDev / responseVariance
        let adjustedR2 = 1. - (residualStdDev * residualStdDev) / responseVariance * (float n) / ((float n) - (float mX.Length) - 1.)
        
        {
            StandardErrors=stndErrors
            ResidualStandardDeviation=residualStdDev
            ResponseMean=responseMean
            ResponseVariance=responseVariance
            R2=r2
            AdjustedR2=adjustedR2
        }




module IrLS = 

    open LinkFunctions
    open GlmDistributionFamily

        
    let stepwiseGainIrls (A: Matrix<float>) (At: Matrix<float>) (b: Vector<float>) (mDistributionFamily: GlmDistributionFamily) (x: Vector<float>) =
        //Calculate the dimensions of the Matrix
        let m: int = A.NumRows
        let n: int = A.NumCols

        //Get the link function in accordance to the distribution type
        let linkFunction = getLinkFunction mDistributionFamily

        //Get the variance function in accordance to the distribution type
        let varianceFunction = getVariance mDistributionFamily
        
        let eta: Vector<float> = A * x
            
        let etaLength = eta.Length

        let g: Vector<float> = Vector.init etaLength (fun k -> linkFunction.getInvLink(eta[k]))

        let gprime: Vector<float> = Vector.init etaLength (fun k -> linkFunction.getInvLinkDerivative(eta[k]))

        let z: Vector<float> = Vector.init etaLength (fun k -> eta[k] + (b[k] - g[k]) / gprime[k])

        let W = Matrix.identity m

        //Update Variance and update the identity Matrix W 
        for k = 0 to m - 1 do
            let gVariance = varianceFunction (g.[k])
            if gVariance = 0.0 then
                printfn "Variance= 0.0 -> Exit"
                System.Environment.Exit(0)
            W.[k, k] <- gprime.[k] * gprime.[k] / gVariance

        let x_old: Vector<float>    = x

        let AtW: Matrix<float>      = At * W
        let AtWA: Matrix<float>     = AtW * A
        let AtWAInv: Matrix<float>  = Algebra.LinearAlgebra.Inverse AtWA

        let x_new = (AtWAInv * AtW) * z

        //Calculate the cost of this step
        let cost:float = 
            x_new - x_old 
            |> Vector.norm

        cost,x_old,x_new,AtWAInv

    let solveIrls (A: Matrix<float>) (b: Vector<float>) (maxIter: int) (mDistributionFamily: GlmDistributionFamily) (mTol: float) =
        let loopTilIter (A: Matrix<float>) (b: Vector<float>) (mDistributionFamily: GlmDistributionFamily) (maxIter: int) (mTol: float) (costFunction: Matrix<float> -> Matrix<float> -> Vector<float> -> GlmDistributionFamily -> Vector<float> -> float * Vector<float> * Vector<float> * Matrix<float>) = 
            //Calculate the dimensions of the Matrix
            let m: int = A.NumRows
            let n: int = A.NumCols

            //Transpose the Matrix
            let At = Matrix.transpose(A)

            //Init a empty vector x
            let x_original: Vector<float> = Vector.zeroCreate n 

            //Run the costFunction until maxIter has been reached or the cost for the gain is smaller than mTol
            let rec loopTilMaxIter (x: Vector<float>) (x_old: Vector<float>) (AtWAInv: Matrix<float>) (loopCount: int)  =
                if loopCount = maxIter then
                    x_old,x,AtWAInv
                else
                    
                    let (cost: float),(x_old: Vector<float>),(x: Vector<float>),(AtWAInv:Matrix<float>) = costFunction A At b mDistributionFamily x

                    if loopCount%10 = 0 then
                        printfn $"Iteration {loopCount}, Cost {cost}"
                    
                    if cost < mTol then
                        x_old,x,AtWAInv
                    else
                        loopTilMaxIter x x_old AtWAInv (loopCount+1)
            
            
            loopTilMaxIter x_original x_original (Matrix.zero 0 0) 0

        let x_old,x,AtWAInv =   loopTilIter A b mDistributionFamily maxIter mTol stepwiseGainIrls

        let mX: Vector<float> = Vector.init (A.NumCols) (fun i -> x[i])

        let statistics = GLMStatistics.getStatisticsIRLS A b mDistributionFamily AtWAInv mX
        
        mX,statistics


module QR =

    let internal qrAlternative (A:Matrix<float>) =
        let m: int = A.NumRows
        let n: int = A.NumCols

        let q: Matrix<float> = Matrix.zero m n
        let r: Matrix<float> = Matrix.zero n n
        let qLengths: Vector<float> = Vector.zeroCreate n

        let getVectorLength (v: Vector<float>) = Vector.fold (fun folder i -> folder+(i*i)) 0. v

        let setqOfA (n: int) =
            let aN: Vector<float> =  Matrix.getCol A n
            let qN = 
                if n = 0 then 
                    aN 
                else 
                    Array.init (n) (fun i -> 
                        let denominator = qLengths[i]
                        let forNominator: Vector<float> = Matrix.getCol q i 
                        let nominator: float = Vector.dot aN forNominator
                        r.[i, n] <- nominator
                        (nominator/denominator) * forNominator
                    )
                    |> Array.fold (fun folder  e -> folder-e ) aN
            Matrix.setCol q n qN
            qN  

        for i=0 to n-1 do
            let qN = setqOfA i 
            let qLength = getVectorLength qN
            let rValue = sqrt(qLength)
            r[i,i] <- rValue
            qLengths[i] <- qLength

        for i=0 to n-1 do
            let qN: Vector<float> = Matrix.getCol q i
            let updateQ = (1./sqrt( qLengths[i]  )) * qN 
            Matrix.setCol q i updateQ
            for j=i+1 to n-1 do
                let denominator = r[i, i]
                let nominator = r[i, j]
                r[i, j] <- (nominator/denominator)
    
        q,r

    let internal solveLinearQR (A: Matrix<float>) (t: Vector<float>) =
        let m = A.NumRows
        let n = A.NumCols

        System.Diagnostics.Debug.Assert(m >= n) 

        let q,r = qrAlternative A 

        let QT = q.Transpose

        let mX = Vector.zeroCreate n

        let c: Vector<float> = QT * t

        let rec build_mX_inner cross_prod i j =
            if j=n then 
                cross_prod
            else
                let newCrossprod = cross_prod + (r[i, j] * mX[j])
                build_mX_inner newCrossprod i (j+1)
    
        let rec build_mX_outer i =
            if i<0 then 
                ()
            else
                let crossProd = build_mX_inner 0. i (i+1)
                mX[i] <- (c[i] - crossProd) / r[i, i]
                build_mX_outer (i-1)
    
        build_mX_outer (n-1)
    
        mX,r
    
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
        //printfn $"m {m}"
        //Get the link function in accordance to the distribution type
        let linkFunction= GlmDistributionFamily.getLinkFunction mDistributionFamily

        let famWeight = GlmDistributionFamily.getFamilyWeights mDistributionFamily mu
        //printfn $"famWeight {famWeight}\n"
        let selfWeights = 
            Vector.init m (fun i -> t[i] * (float 1.) * famWeight[i])
        //printfn $"selfWeights {selfWeights}\n"
        
        let derivs = Vector.map(fun x -> linkFunction.getDeriv x) mu
        
        //printfn $"derivs {derivs}\n"

        let wlsendog: Vector<float> = Vector.init m (fun i -> linPred[i] + derivs[i] * (b[i]-mu[i]))
        //printfn $"wlsendog {wlsendog}\n"
        

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
            
        //printfn $"wlsendog2 {wlsendog2} \n"
        //printfn $"wlsexdog {wlsexdog} \n"

        let (wlsResults: Vector<float>),R = solveLinearQR wlsexdog wlsendog2

        let linPred_new: Vector<float> = A * wlsResults

        let mu_new = Vector.init m (fun i -> linkFunction.getInvLink(linPred_new[i]))

        //printfn $"wlsResults {wlsResults} \n"
        //printfn $"linPred_new {linPred_new}\n"
        //printfn $"mu_new {mu_new}\n\n\n\n\n"

        //let deviance = GlmDistributionFamily.resid_dev wlsendog mu_new (GlmDistributionFamily.getFamilyReisualDeviance mDistributionFamily)
        //printfn $"deviance {deviance}\n\n\n\n\n"

        //Calculate the cost of this step
        let cost:float = 
            oldResult - wlsResults 
            |> Vector.norm

        cost,mu_new,linPred_new,wlsResults,wlsendog

    let loopTilIterQR 
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

            //Init a empty vector x
            let t_original: Vector<float>   = Vector.init m (fun i -> 1.)
            let bMean: float                = Vector.mean b
            let muStart:Vector<float>       = Vector.map(fun x -> ((x+bMean)/2.)) b
            let linPredStart: Vector<float> = Vector.init m (fun k -> GlmDistributionFamily.getLinkFunction(mDistributionFamily).getLink(muStart[k]))

            //printfn $"muStart: {muStart}"
            //printfn $"linPredStart: {linPredStart}"

            //Run the costFunction until maxIter has been reached or the cost for the gain is smaller than mTol
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
                            

                    //if loopCount%10 = 0 then
                    printfn $"Iteration {loopCount}, Cost {cost}"
                    ////printfn $" {loopCount}"
                    
                    if cost < mTol then
           
                        t_original,mu,linPred,wlsResult,wlsendog

                    else
                        //let mxTest = solveLinearQR A wlsendog |> fst
                        //printfn $"mxTest {mxTest}"
                        loopTilMaxIter t_original (loopCount+1) mu_new linPred_new wlsResult_new wlsendogNew
            
            
            loopTilMaxIter t_original 0 muStart linPredStart (Vector.zeroCreate n) (Vector.zeroCreate m)

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

        let deviance = GlmDistributionFamily.getFamilyReisualDeviance mDistributionFamily b mu 

        let stndError = GLMStatistics.getStandardError A b mu mX mDistributionFamily

        let zStatistic = Vector.map2 (fun x y -> x/y) mX stndError

        printfn $"LogLikely: {(GLMStatistics.getLogLikelihood b mu)} \n Dev: {deviance} \n chi2: {GLMStatistics.getchi2 b linPred} \n stndError: {stndError} \n zStatistic: {zStatistic}"
        //Update Stats
        let statistics = GLMStatistics.getStatisticsQR A b mu mX mDistributionFamily
        mX,statistics 
