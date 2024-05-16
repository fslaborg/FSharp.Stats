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
        getInvLinkDerivative: float -> float
    }

module LinkFunctions =
    let internal clipLogisticValues (p : float)  = 
        let floatEps = 1e-8 
        max floatEps (min (1.0-floatEps) p)

    let LogitLinkFunction : LinkFunction =
        {
            getLink = fun b -> 
                let p = clipLogisticValues b
                System.Math.Log(p / (1.0 - p))
            getInvLink = fun a -> 
                1.0 / (1.0 + System.Math.Exp(-a))
            getInvLinkDerivative = fun a ->
                let t = System.Math.Exp(a)
                t / ((1.0 + t) * (1.0 + t))
        }

    let LogLinkFunction : LinkFunction =
        {
            getLink                 = fun b -> System.Math.Log(b)
            getInvLink              = fun a -> System.Math.Exp(a)
            getInvLinkDerivative    = fun a -> System.Math.Exp(a)
        }

    let InverseSquaredLinkFunction: LinkFunction =
        {
            getLink                 = fun b -> Math.Pow(b,-2.)//1.0 / b
            getInvLink              = fun a -> Math.Pow(a,(1./ -2.))//1.0 / a
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
            abs(g) ** -2.
        | GlmDistributionFamily.InverseGaussian ->
            g * g * g
        | GlmDistributionFamily.Normal ->
            1.0
        | GlmDistributionFamily.Poisson ->
            g
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

type GLMStatistics = 
    {
        StandardErrors:Vector<float>
        ResidualStandardDeviation:float
        ResponseMean:float
        ResponseVariance:float
        R2:float
        AdjustedR2:float
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

    let stepwiseGainQR (A: Matrix<float>) (b: Vector<float>) (mDistributionFamily: GlmDistributionFamily) (t:Vector<float>) (q:Matrix<float>) (Qt:Matrix<float>) (s_old:Vector<float>) = 
        let m = A.NumRows
        let n = A.NumCols

        //Get the link function in accordance to the distribution type
        let linkFunction= GlmDistributionFamily.getLinkFunction mDistributionFamily

        //Get the variance function in accordance to the distribution type
        let varianceFunction = GlmDistributionFamily.getVariance mDistributionFamily

        let g: Vector<float> = Vector.init m (fun k -> linkFunction.getInvLink(t[k]))

        let gprime: Vector<float> = Vector.init m (fun k -> linkFunction.getInvLinkDerivative(t[k]))

        let z: Vector<float> = Vector.init m (fun k -> t[k] + (b[k] - g[k]) / gprime[k])
        
        let W_og = Vector.zeroCreate m

        let rec buildW w_kk_min k =
            if k=m then
                W_og,w_kk_min

            else
                let gVariance = varianceFunction (g.[k])
                let w_kk = gprime[k] * gprime[k] / (gVariance)
                W_og[k] <- w_kk
                let w_kk_min_new = System.Math.Min(w_kk, w_kk_min)
                buildW w_kk_min_new (k+1)

        let W,w_kk_min = 
            buildW System.Double.MaxValue 0

        if w_kk_min < System.Math.Sqrt(System.Double.Epsilon) then
            System.Console.WriteLine("Warning: Tiny weights encountered, min(diag(W)) is too small")
        
        let WQ: Matrix<float>       = Matrix.zero m n
        let Wz: Vector<float>       = Vector.zeroCreate m
        for k=0 to m-1 do 
            Wz[k] <- z[k] * W[k]
            for k2 = 0 to n-1 do
                WQ[k, k2] <- q[k, k2] * W[k]


        let QtWQ: Matrix<float>         = Qt * WQ
        let QtWz: Vector<float>         = Qt * Wz

        let s,sM = solveLinearQR QtWQ QtWz

        let t_new: Vector<float> = q * s
        
        //Calculate the cost of this step
        let cost:float = 
            s_old - s 
            |> Vector.norm

        cost,t_new,s,s,W

    let loopTilIterQR (A: Matrix<float>) (b: Vector<float>) (mDistributionFamily: GlmDistributionFamily) (maxIter: int) (mTol: float) (q:Matrix<float>) (QT:Matrix<float>) (costFunction: Matrix<float> -> Vector<float> -> GlmDistributionFamily -> Vector<float> -> Matrix<float> -> Matrix<float> -> Vector<float> -> float * Vector<float> * Vector<float> * Vector<float> * Vector<float>) = 
        let m = A.NumRows
        let n = A.NumCols

        //Init a empty vector x
        let s_original: Vector<float>   = Vector.init n (fun i -> 1.)//Vector.zeroCreate n 
        let sy_original: Vector<float>  = Vector.init n (fun i -> 1.)//Vector.zeroCreate n 
        let t_original: Vector<float>   = Vector.init m (fun i -> 1.)
        let W_original: Vector<float>   = Vector.init m (fun i -> 1.)//Vector.zeroCreate m

        //Run the costFunction until maxIter has been reached or the cost for the gain is smaller than mTol
        let rec loopTilMaxIter (t: Vector<float>) (s: Vector<float>) (sy: Vector<float>) (W: Vector<float>) (loopCount: int)  =
            if loopCount = maxIter then
                t,s,sy,W
            else
                let (cost: float),(t: Vector<float>),(s: Vector<float>),(sy: Vector<float>),(W: Vector<float>) = costFunction A b mDistributionFamily t q QT s

                //if loopCount%10 = 0 then
                printfn $"Iteration {loopCount}, Cost {cost}"
                //printfn $" {loopCount}"
                    
                if cost < mTol then
                    t,s,sy,W

                else
                    let mxTest = solveLinearQR A t |> fst
                    printfn $"mxTest: {mxTest}"
                    loopTilMaxIter t s sy W (loopCount+1)
            
            
        loopTilMaxIter t_original s_original sy_original W_original 0

    let solveQrNewton(A: Matrix<float>) (b: Vector<float>) (maxIter: int) (mDistributionFamily: GlmDistributionFamily) (mTol: float) =
        let m = A.NumRows
        let n = A.NumCols

        System.Diagnostics.Debug.Assert(m >= n) 

        //let q,r = Algebra.LinearAlgebra.QR A 
        let q,r = qrAlternative A 

        let QT = q.Transpose
        // printfn $"Q {q.Dimensions} | r {r.Dimensions} | QT {QT.Dimensions} | A {A.Dimensions}"
        let (t: Vector<float>),(s: Vector<float>),(sy: Vector<float>),(W: Vector<float>) = loopTilIterQR A b mDistributionFamily maxIter mTol q QT stepwiseGainQR 

        let mX,R = solveLinearQR A t
        
        //Update Stats
        let statistics = GLMStatistics.getStatisticsQR A b W mX mDistributionFamily
        mX,statistics 
