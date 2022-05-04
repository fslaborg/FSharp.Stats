namespace FSharp.Stats.StatisticalModels
open FSharp.Stats

///
type UnivariableOLS(
    independentVariable:Vector<float>,
    dependentVariable:Vector<float>,
    ?UseCholesky: bool
) =
    let useCholesky = defaultArg UseCholesky false

    let coeffs = 
        if useCholesky then
            Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.coefficient independentVariable dependentVariable 
        else
            Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.coefficientCholesky independentVariable dependentVariable 

    let fitFunc = 
        fun (coef : Vector<float>) (x:float) ->
            if coef.Length <> 2 then
                raise (System.ArgumentException("Coefficient has to be [a;b]!"))
            coef.[0] + coef.[1] * x

    let meanX = Seq.mean independentVariable
    let meanY = Seq.mean dependentVariable
    let sst,sse,ssxx,ssxy =
        Seq.zip independentVariable dependentVariable
        |> Seq.fold (fun (stateSST,stateSSE,stateSSxx,stateSSxy) (x,y) -> 
            let exY   = fitFunc coeffs x
            let dSSe  = exY - y
            let dSSt  = y - meanY
            let dssxx = x - meanX 
            let ssxy  = dssxx * dSSt 
            (stateSST + dSSt*dSSt, stateSSE + dSSe*dSSe, stateSSxx + dssxx*dssxx, stateSSxy + ssxy)
        ) (0.,0.,0.,0.)       

    member model.Coefficients = coeffs
    member model.Intercept = coeffs[0]
    member model.Slope = coeffs[1]
    member model.FitFunction = fitFunc

    member model.SST  = sst 
    member model.SSR  = (sst - sse)
    member model.SSE  = sse
    member model.SSxx = ssxx
    member model.SSxy = ssxy

    member model.SumOfSquares = Fitting.GoodnessOfFit.calculateSumOfSquares (model.FitFunction model.Coefficients) independentVariable dependentVariable
    member model.RSquared = Fitting.GoodnessOfFit.calculateDetermination model.SumOfSquares

    interface IStatisticalModel<Vector<float>,Vector<float>,float,float> with
        
        member model.Design = independentVariable
        member model.Response = dependentVariable
        member model.ModelName = StatisticalModelName.UnivariableOrdinaryLeastSquares
        member model.Predict(x: float) = model.FitFunction model.Coefficients x
        member model.FormatSummary() = ""
        member model.PrintSummary() = printfn "" 
        member model.InteractiveSummary() = "" 


type MultivariableOLS(
    independentVariables:Matrix<float>,
    dependentVariable:Vector<float>,
    ?UseCholesky: bool
) =
    let useCholesky = defaultArg UseCholesky false

    let coeffs = 
        if useCholesky then
            Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Multivariable.coefficients independentVariables dependentVariable 
        else
            Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Multivariable.coefficientsCholesky independentVariables dependentVariable 

    let fitFunc = 
        fun (coef : Vector<float>) (x:Vector<float>) ->
            let tmp :Vector<float> = Vector.init (x.Length+1) (fun i -> if i = 0 then 1. else x.[i-1])
            Vector.dot tmp coef 

    member model.Coefficients = coeffs
    member model.FitFunction = fitFunc

    interface IStatisticalModel<Matrix<float>,Vector<float>,Vector<float>,float> with
        
        member model.Design = independentVariables
        member model.Response = dependentVariable
        member model.ModelName = StatisticalModelName.MultivariableOrdinaryLeastSquares
        member model.Predict(variables: Vector<float>) = model.FitFunction model.Coefficients variables
        member model.FormatSummary() = ""
        member model.PrintSummary() = printfn ""
        member model.InteractiveSummary() = "" 