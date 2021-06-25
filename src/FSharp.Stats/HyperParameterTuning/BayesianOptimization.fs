namespace FSharp.Stats.HyperParameterTuning

open FSharp.Stats


module BayesianOptimization =
 
    type SurrogateFunction<'T> = HyperParameterTuningResult<'T> list -> (HyperParameterValue list -> float*float)
    
    type SelectionFunction<'T> = HyperParameter list -> HyperParameterTuningResult<'T> list -> (HyperParameterValue list -> float*float) -> HyperParameterValue list

    module Surrogates = 

        let corr (x1 : HyperParameterValue list list) (x2  : HyperParameterValue list list) (theta : float list) = 
            x1
            |> List.map (fun r1 -> 
                x2
                |> List.map (fun r2 ->
                    (r1,r2,theta)
                    |||> List.map3 (fun v1 v2 t ->
                        let distance = (HyperParameterValue.GetAsFloat v1 - HyperParameterValue.GetAsFloat v2) ** 2.
                        distance * t
                    )
                    |> List.sum
                    |> (*) -1.
                    |> System.Math.Exp
                )
            )
            |> Matrix.ofJaggedList

        let negLikelyHood (x : HyperParameterValue list list) (y : float list) (theta : float list) =

            let y = RowVector.ofList y |>  Matrix.ofRowVector |> Matrix.transpose
            let theta = List.map (fun v -> 10. ** v) theta
            let n = x.Length
            let one = RowVector.create n 1. |> Matrix.ofRowVector
            let oneColumn = Matrix.transpose one

            let nugget = Matrix.init n n (fun i j -> if i = j then 0.0000000001 else 0.)
            let K = (corr x x theta) + nugget
            let inv_K = Algebra.LinearAlgebra.Inverse K
            
            let mu = (one * inv_K * y).[0,0] / (one * inv_K * oneColumn).[0,0]

            let sigmaSqr = 
                let h = (Matrix.map (fun v -> v - mu) y) * one
                (Matrix.transpose h * inv_K * h).[0,0] / (float n)

            let detK = Algebra.LinearAlgebra.Determinant K
            let lnLike = -(float n / 2.) * log(sigmaSqr) - 0.5*log(detK)

            mu, sigmaSqr, inv_K, -lnLike

        let minimize (bounds : HyperParameter list) (initialPoints : HyperParameterValue list) (f : float list -> float) =
            //let xs = HyperParameterValue.ToVector initialPoints
            //let derivative = Optimization.GradientDescent.grad f
            //Optimization.GradientDescent.minimize f derivative xs
            GridSearch.createSearchGrid 100 bounds
            |> List.map (fun x -> x, f (x |> List.map HyperParameterValue.GetAsFloat))
            |> List.minBy snd
            


        let fit nRestarts (x : HyperParameterValue list list) (y : float list) =

            let lb, ub = -3., 2.

            let lhd = 
                List.init nRestarts (fun _ ->
                    List.init x.[0].Length (fun _ -> Random.rndgen.NextFloat())
                )

            let initialPoints = lhd |> List.map (List.map (fun v -> HyperParameterValue.Float ((ub-lb) * v + lb)))

            let bounds = x.[0] |> List.map (fun _ -> HyperParameter.FloatBetween (Intervals.Interval.ClosedInterval (lb,ub)))

            let f = (negLikelyHood x y >> (fun (a,b,c,d) -> d))

            let minTheta, minlogLikelyHood = 
                initialPoints
                |> List.map (fun theta ->
                    minimize bounds theta f
                )
                |> List.minBy snd

            negLikelyHood x y (minTheta |> List.map HyperParameterValue.GetAsFloat)
            |> fun (a,b,c,d) ->

            minTheta, a, b, c, d 
            

        let predict theta mu sigmaSqr (inv_K : matrix) trainX testX y =

            let y = RowVector.ofList y |>  Matrix.ofRowVector |> Matrix.transpose
            let theta = List.map (fun v -> 10. ** v) theta
            let n = trainX |> List.length
            let oneColumn = RowVector.create n 1. |> Matrix.ofRowVector |> Matrix.transpose

            let k = (corr trainX testX theta)
            
            let f = mu + (Matrix.transpose k * inv_K * (y - Matrix.map (fun v -> v * mu) oneColumn)).[0,0]

            let SSqr = sigmaSqr * (1. - Matrix.transpose k * inv_K * k).[0,0]
            f, SSqr

        //let likelyhood 

        let gaussianProcessSurrogate (priors : HyperParameterTuningResult<'T> list) : HyperParameterValue list -> float*float =
            let x,y = 
                priors
                |> List.map (fun r -> r.Parameters, r.Score)
                |> List.unzip
            
            let (theta,mu,sqr,invK,logLike) = fit 5 x y
            fun testX -> predict (theta |> List.map HyperParameterValue.GetAsFloat) mu sqr invK x [testX] y

        let treeStructuredParsenEstimator x = 1


    
    module Selectors = 

        let expectedImprovementAt (xi : float) (prior : HyperParameterTuningResult<'T> list) (surrogate : HyperParameterValue list -> float*float) (x : HyperParameterValue list) =
            let mu,sigma = surrogate x

            if sigma = 0. then 0. 
            else

                let priorMax = prior |> List.maxBy (fun r -> r.Score)

                let imp = mu - priorMax.Score - xi
                let Z = imp / sigma

                let cdf = FSharp.Stats.Distributions.Continuous.Normal.CDF 0. 1. Z
                let pdf = FSharp.Stats.Distributions.Continuous.Normal.CDF 0. 1. Z

                imp * cdf + sigma * pdf
      
        let expectedImprovementSelectorWith (xi : float) (xs : HyperParameterValue list list) (hyperParams : HyperParameter list) (prior : HyperParameterTuningResult<'T> list) (surrogate : HyperParameterValue list -> float*float) = 
            xs
            |> List.maxBy (expectedImprovementAt xi prior surrogate)

        let expectedImprovementSelector (hyperParams : HyperParameter list) (prior : HyperParameterTuningResult<'T> list) (surrogate : HyperParameterValue list -> float*float) = 
            //Surrogates.minimize hyperParams 
            //expectedImprovementAt 0.01 prior surrogate
            let xs = GridSearch.createSearchGrid 1000 hyperParams
            expectedImprovementSelectorWith 0.01 xs hyperParams prior surrogate


    /// Perform a random search on n random hyper parameter value sets, returning the hyper parameters for which the model performance was maximized
    let bayesianOptimizationMaximize (maxIterations : int) (surrogateF : SurrogateFunction<'T>) (selectorF : SelectionFunction<'T>) (scoringFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (hyperParams : HyperParameter list) : HyperParameterTuningResult<'T> =
        let rec optimize nIterations (prior : HyperParameterTuningResult<'T> list) =
            if nIterations = maxIterations then
                prior 
                |> List.maxBy (fun r -> r.Score)
            else
                let surrogate = surrogateF prior
                let nextX = selectorF hyperParams prior surrogate
                let metaInfo,nextY = scoringFunction data nextX
                let posterior = List.append prior [HyperParameterTuningResult<'T>.create nextX nextY metaInfo]
                         
                optimize (nIterations + 1) posterior
        let randomPriors = RandomSearch.randomSearch 5 scoringFunction data hyperParams
        optimize 0 randomPriors