namespace FSharp.Stats.Optimization

module OptimizationStop =

  open System
  
  ///<summary>Possible criteria to end optimization</summary>
  ///<remarks>Optimization can end because the maximum number of iterations has been reached,
  ///a stationary point has been reached, or a stationary gradient has been reached</remarks>
  [<Flags>] 
  type StopCriteriaType =
    | None                       = 0 
    | MaximumIteration           = 1
    | MaximumFunctionEvaluation  = 2
    | MaximumGradientEvaluation  = 4
    | MaximumHessianEvaluation   = 8
    | StationaryPoint            = 16
    | StationaryGradient         = 32
    | StationaryHessian          = 64
    | FunctionEpsilon            = 128
    | GradientEpsilon            = 256 
    | HessianEpsilon             = 512

  type StopCounter(
      iterationCounter                    : int,
      functionEvaluationCounter           : int,
      gradientEvaluationCounter           : int,
      hessianEvaluationCounter            : int,
      stationaryPointIterationsCounter    : int,
      stationaryGradientIterationsCounter : int,
      stationaryHessianIterationsCounter  : int,
      endCriteria : StopCriteriaType
      ) =
    
    let mutable iterationCounter                    = iterationCounter
    let mutable functionEvaluationCounter           = functionEvaluationCounter
    let mutable gradientEvaluationCounter           = gradientEvaluationCounter
    let mutable hessianEvaluationCounter            = hessianEvaluationCounter
    let mutable stationaryPointIterationsCounter    = stationaryPointIterationsCounter
    let mutable stationaryGradientIterationsCounter = stationaryGradientIterationsCounter
    let mutable stationaryHessianIterationsCounter  = stationaryHessianIterationsCounter
    let mutable endCriteria                         = endCriteria

    new () = StopCounter(0,0,0,0,0,0,0,StopCriteriaType.None)

    with
      member __.IterationCounter 
        with get ()      = iterationCounter
        and  set (value) = iterationCounter <- value
      member __.IncIerationCounter() = 
        iterationCounter <- iterationCounter + 1 

      member __.FunctionEvaluationCounter
        with get ()      = functionEvaluationCounter
        and  set (value) = functionEvaluationCounter <- value
      member __.IncFunctionEvaluationCounter() = 
        functionEvaluationCounter <- functionEvaluationCounter + 1 

      member __.GradientEvaluationCounter
        with get ()      = gradientEvaluationCounter
        and  set (value) = gradientEvaluationCounter <- value
      member __.IncGradientEvaluationCounter() = 
        gradientEvaluationCounter <- gradientEvaluationCounter + 1 

      member __.HessianEvaluationCounter
        with get ()      = hessianEvaluationCounter
        and  set (value) = hessianEvaluationCounter <- value
      member __.IncHessianEvaluationCounter() = 
        hessianEvaluationCounter <- hessianEvaluationCounter + 1 

      member __.StationaryPointIterationsCounter
        with get ()      = stationaryPointIterationsCounter
        and  set (value) = stationaryPointIterationsCounter <- value
      member __.IncStationaryPointIterationsCounter() = 
        stationaryPointIterationsCounter <- stationaryPointIterationsCounter + 1 

      member __.StationaryGradientIterationsCounter
        with get ()      = stationaryGradientIterationsCounter
        and  set (value) = stationaryGradientIterationsCounter <- value
      member __.IncStationaryGradientIterationsCounter() = 
        stationaryGradientIterationsCounter <- stationaryGradientIterationsCounter + 1 

      member __.StationaryHessianIterationsCounter
        with get ()      = stationaryHessianIterationsCounter
        and  set (value) = stationaryHessianIterationsCounter <- value 
      member __.IncStationaryHessianIterationsCounter() = 
        stationaryHessianIterationsCounter <- stationaryHessianIterationsCounter + 1 

      member __.EndCriteria
        with get ()      = endCriteria
        and  set (value) = endCriteria <- value
      member __.AddEndCriteriaFlag(flag:StopCriteriaType) =
        endCriteria <- endCriteria ||| flag

      member __.Reset() =
        iterationCounter <- 0
        functionEvaluationCounter <- 0
        gradientEvaluationCounter <- 0
        hessianEvaluationCounter <- 0
        stationaryPointIterationsCounter <- 0
        stationaryGradientIterationsCounter <- 0
        stationaryHessianIterationsCounter <- 0
        endCriteria <- StopCriteriaType.None

  let initStopCounter() = new StopCounter()

  type StopCriteria = {
    MaxIteration                    : int
    MaxFunctionEvaluation           : int
    MaxGradientEvaluation           : int
    MaxHessianEvaluation            : int
    MaxStationaryPointIterations    : int
    MaxStationaryGradientIterations : int
    MaxStationaryHessianIterations  : int
    MinFunctionEpsilon              : float
    MinGradientEpsilon              : float
    MinHessianEpsilon               : float
    CancellationToken               : System.Threading.CancellationToken
  } 
  with
    
    static member InitWith (maxiteration, epsilon, maxfunctionevaluation, maxstationarypointiterations,cancellationToken) =
      {
        MaxIteration                    = maxiteration
        MaxFunctionEvaluation           = maxfunctionevaluation
        MaxGradientEvaluation           = maxfunctionevaluation
        MaxHessianEvaluation            = maxfunctionevaluation
        MaxStationaryPointIterations    = maxstationarypointiterations
        MaxStationaryGradientIterations = maxstationarypointiterations
        MaxStationaryHessianIterations  = maxstationarypointiterations
        MinFunctionEpsilon              = epsilon 
        MinGradientEpsilon              = epsilon 
        MinHessianEpsilon               = epsilon
        CancellationToken               = cancellationToken         
      }

    ///<summary>Check if the iteration number is less than the maximum iteration</summary>
    ///<remarks>
    ///If iteration count is equal to or greater than the maximum number of iterations then
    ///the ending criteria is set to <c>CriteriaType.MaximumIteration</c> and the function returns true.
    ///</remarks>    
    member __.CheckIteration (counter:StopCounter) =
      if counter.IterationCounter >= __.MaxIteration then
        StopCriteriaType.MaximumIteration
      else
        StopCriteriaType.None

    ///<summary>Check if the number of function evaluations is less than the maximum </summary>
    ///<remarks>
    ///If the number of function evaluations is equal to or greater than the maximum number of
    ///function evaluations then the ending criteria is set to <c>CriteriaType.MaximumFunctionEvaluation</c>
    /// and the function returns true.
    ///</remarks>
    member __.CheckFunctionEvaluations (counter:StopCounter) =
      if (counter.FunctionEvaluationCounter >= __.MaxFunctionEvaluation) then
        StopCriteriaType.MaximumFunctionEvaluation
      else
        StopCriteriaType.None

    ///<summary>Check if the number of gradient evaluations is less than the maximum </summary>
    ///<remarks>
    ///If the number of gradient evaluations is equal to or greater than the maximum number of
    ///gradient evaluations then the ending criteria is set to <c>CriteriaType.MaximumGradientEvaluation</c>
    /// and the function returns true.
    ///</remarks>
    member __.CheckGradientEvaluations (counter:StopCounter) =
      if (counter.GradientEvaluationCounter >= __.MaxGradientEvaluation) then
        StopCriteriaType.MaximumGradientEvaluation
      else
        StopCriteriaType.None

    ///<summary>Check if the number of hessian evaluations is less than the maximum </summary>
    ///<remarks>
    ///If the number of hessian evaluations is equal to or greater than the maximum number of
    ///hessian evaluations then the ending criteria is set to <c>CriteriaType.MaximumHessianEvaluation</c>
    /// and the function returns true.
    ///</remarks>
    member __.CheckHessianEvaluations (counter:StopCounter) =
      if (counter.HessianEvaluationCounter >= __.MaxHessianEvaluation) then
        StopCriteriaType.MaximumHessianEvaluation
      else
        StopCriteriaType.None

    ///<summary>Check if objective function changed by less than the function epsilon</summary>
    ///<remarks>
    /// If the change in objective function is less than the function epsilon then a possible stationary
    /// point has been found.  If the number of repeated iterations at this possible stationary point is
    /// greater than the maximum iterations at a station point then the ending criteria is set to
    /// <c>CriteriaType.StationaryPoint</c> and the function returns true;
    ///</remarks>
    member __.CheckStationaryPoint (stopCounter:StopCounter) (fold:float) (fnew:float) =
    
      let test = not ((System.Math.Abs(fold - fnew) >= __.MinFunctionEpsilon))
      if test then
        stopCounter.IncStationaryPointIterationsCounter()
      elif (stopCounter.StationaryPointIterationsCounter <> 0) then
        stopCounter.StationaryPointIterationsCounter <- 0
      
      if test && (stopCounter.StationaryPointIterationsCounter > __.MaxStationaryPointIterations) then
        StopCriteriaType.StationaryPoint
      else
        StopCriteriaType.None
      
    ///<summary>Check if gradient function changed by less than the gradient epsilon</summary>
    ///<remarks>
    /// If the change in gradient function is less than the gradient epsilon then a possible stationary
    /// point has been found.  If the number of repeated iterations at this possible stationary point is
    /// greater than the maximum iterations at a station point then the ending criteria is set to
    /// <c>CriteriaType.StationaryPoint</c> and the function returns true;
    ///</remarks>
    member __.CheckStationaryGradient (stopCounter:StopCounter) (gold:float) (gnew:float) =
      let test = (System.Math.Abs(gold - gnew) < __.MinGradientEpsilon)
      if (test) then
        stopCounter.IncStationaryGradientIterationsCounter()
      elif (stopCounter.StationaryGradientIterationsCounter <> 0) then
        stopCounter.StationaryPointIterationsCounter <- 0
      
      if (test && (stopCounter.StationaryGradientIterationsCounter > __.MaxStationaryGradientIterations)) then
        StopCriteriaType.StationaryGradient
      else
        StopCriteriaType.None

    ///<summary>Check if hessian function changed by less than the hessian epsilon</summary>
    ///<remarks>
    /// If the change in hessian function is less than the hessian epsilon then a possible stationary
    /// point has been found.  If the number of repeated iterations at this possible stationary point is
    /// greater than the maximum iterations at a station point then the ending criteria is set to
    /// <c>CriteriaType.StationaryPoint</c> and the function returns true;
    ///</remarks>
    member __.CheckStationaryHessian (stopCounter:StopCounter) (gold:float) (gnew:float) =
      let test = (System.Math.Abs(gold - gnew) < __.MinHessianEpsilon)
      if test then
        stopCounter.IncStationaryHessianIterationsCounter()
      elif (stopCounter.StationaryHessianIterationsCounter <> 0) then
        stopCounter.StationaryPointIterationsCounter <- 0
      if (test && (stopCounter.StationaryHessianIterationsCounter > __.MaxStationaryHessianIterations)) then
        StopCriteriaType.StationaryHessian
      else
        StopCriteriaType.None

    ///<summary>Check if objective function value is less than the function epsilon</summary>
    ///<remarks>
    /// If the objective function value is less than the function epsilon and only positive optimization
    /// is allowed then the ending criteria is set to <c>CriteriaType.FunctionEpsilon</c> and the
    /// function returns true;
    ///</remarks>
    member __.CheckFunctionEpsilon (v) =
      if (v < __.MinFunctionEpsilon) then
        StopCriteriaType.FunctionEpsilon
      else 
        StopCriteriaType.None

    ///<summary>Check if the norm of the gradient is less than the gradient epsilon</summary>
    ///<remarks>
    /// If the norm of the gradient is less than the gradient epsilon then the ending criteria is set
    /// to <c>CriteriaType.GradientEpsilon</c> and the function returns true;
    ///</remarks>
    member __.CheckGradientEpsilon (normDiff) =
    
      if (normDiff < __.MinGradientEpsilon) then
        StopCriteriaType.GradientEpsilon
      else
        StopCriteriaType.None
    

    ///<summary>Check if the norm of the hessian is less than the hessian epsilon</summary>
    ///<remarks>
    /// If the norm of the hessian is less than the gradient epsilon then the ending criteria is set
    /// to <c>CriteriaType.HessianEpsilon</c> and the function returns true;
    ///</remarks>
    member __.CheckHessianEpsilon (normDiff) =
      if (normDiff < __.MinHessianEpsilon) then
        StopCriteriaType.HessianEpsilon
      else
        StopCriteriaType.None


    ///<summary>Check if ending criteria are met</summary>
    ///<remarks>Returns true if one of the ending criteria is met, otherwise it returns false</remarks>
    member __.IsCriteria (stopCounter:StopCounter) (fold : float) (fnew:float) =
      let testFlags =
        __.CheckIteration stopCounter |||
        __.CheckFunctionEvaluations stopCounter |||
        __.CheckStationaryPoint stopCounter fold fnew |||
        __.CheckFunctionEpsilon fnew |||
        __.CheckFunctionEpsilon fold
      stopCounter.EndCriteria <- testFlags
      testFlags = StopCriteriaType.None   

    ///<summary>Check if gradient criteria are met</summary>
    ///<remarks>Returns true if one of the gradient criteria is not met, otherwise it returns false</remarks>
    member __.IsGradientCriteria (stopCounter:StopCounter) (normgold:float) (normgnew:float) =
      let testFlags =
        __.CheckGradientEvaluations stopCounter |||
        __.CheckStationaryGradient stopCounter normgnew normgold |||
        __.CheckGradientEpsilon normgnew |||
        __.CheckGradientEpsilon normgold
      stopCounter.EndCriteria <- testFlags
      testFlags = StopCriteriaType.None

  // 1e-8
  let defaultStopCriteria = StopCriteria.InitWith (1000, 1e-8, 10000, 100, System.Threading.CancellationToken.None)
