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

    let LogitLinkFunction : LinkFunction =
        {
            getLink = fun b -> System.Math.Log(b / (1.0 - b))
            getInvLink = fun a -> 1.0 / (1.0 + System.Math.Exp(-a))
            getInvLinkDerivative = fun a ->
                let t = System.Math.Exp(-a)
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
            getLink                 = fun b -> -1.0 / (b * b)
            getInvLink              = fun a -> System.Math.Sqrt(-a)
            getInvLinkDerivative    = fun a -> -1.0 / System.Math.Sqrt(-a)
        }

    let InverseLinkFunction: LinkFunction =
        {
            getLink                 = fun b -> -1.0 / b
            getInvLink              = fun a -> -1.0 / a
            getInvLinkDerivative    = fun a -> -1.0 / (a * a)
        }

    let IdentityLinkFunction: LinkFunction =
        {
            getLink                 = fun b -> b
            getInvLink              = fun a -> a
            getInvLinkDerivative    = fun a -> 1.
        }

    let BinomialLinkFunction: LinkFunction = 
        {
            getLink = fun b -> System.Math.Log(b / (1.0 - b))
            getInvLink = fun a -> 1.0 / (1.0 + System.Math.Exp(-a))
            getInvLinkDerivative = fun a ->
                let t = System.Math.Exp(-a)
                t / ((1.0 + t) * (1.0 + t))
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
            g * g
        | GlmDistributionFamily.InverseGaussian ->
            g * g * g
        | GlmDistributionFamily.Normal ->
            1.0
        | GlmDistributionFamily.Poisson ->
            g
        | GlmDistributionFamily.Bernouli   -> 
            g * (1.0 - g)
        | GlmDistributionFamily.Binomial    -> 
            g * (1.0 - g)
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
    let getStatistics (A: Matrix<float>) (b: Vector<float>) (mDistributionFamily: GlmDistributionFamily) (vcovmat: Matrix<float>) (mX: Vector<float>)  =
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
                System.Environment.Exit(0)
            W.[k, k] <- gprime.[k] * gprime.[k] / gVariance

        let x_old: Vector<float>    = x

        let AtW: Matrix<float>      = At * W
        let AtWA: Matrix<float>     = AtW * A
        let AtWAInv: Matrix<float>  = Algebra.LinearAlgebra.Inverse AtWA

        let x = (AtWAInv * AtW) * z

        //Calculate the cost of this step
        let cost:float = 
            x - x_old 
            |> Vector.norm

        cost,x_old,x,AtWAInv

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

        let statistics = GLMStatistics.getStatistics A b mDistributionFamily AtWAInv mX
        
        mX,statistics
