namespace FSharp.Stats.Optimization

module NelderMead =
    
    open FSharp.Stats
    open FSharp.Stats.Algebra
    open FSharp.Stats.Algebra.LinearAlgebra

    type NmConfig = {
        ///<summary> Delta used to generate initial simplex for non-zero value elements </summary>
        Delta  : float
        ZDelta : float
        ///<summary> Coefficient of reflection (Rho) </summary>  
        Rho    : float
        ///<summary> Coefficient of expansion (Chi) </summary>
        Chi    : float 
        ///<summary> Coefficient of contraction (Psi) </summary>
        Psi    : float
        ///<summary> Coefficient of shrinkage (Sigma) </summary>
        Sigma  : float
    }
      with 
        static member defaultInit () =
            {
                Delta = 0.05
                ZDelta = 0.00025
                Rho   = 1.
                Chi   = 2.
                Psi   = 0.5
                Sigma = 0.5
            }


    type StepType = 
      | Initialization
      | Reflection
      | Expansion
      | OutsideContraction
      | InsideContraction
      | Shrink



    type NmInterationResult = {
      Simplexes      : vector []
      Fx             : float array
      Vectors        : vector []
      Values         : float array
      GradientNorms  : float array
      SolutionVector : vector
      Solution       : float
      LastStep       : StepType
    }



    ///<summary> Create an initial simplex </summary>
    let createSimplex (nmc : NmConfig) (x : vector) =    
          let n = x.Length
          let simplex =
            Array.init (n+1) 
                (fun _ -> Vector.copy x )      
          for i = 1 to n do                      
            if (x[i - 1] <> 0) then        
              simplex[i][i - 1] <- (1. + nmc.Delta) * x[i - 1]
            else        
              simplex[i][i - 1] <- nmc.ZDelta        
      
          simplex


    // determine the values of each vertice in the initial simplex
    let rankVertices (simplexes : vector[]) (fn : vector -> float) = 
      let x  = Array.copy simplexes  
      let fx = simplexes |> Array.map fn
      System.Array.Sort(fx,x)
      (x,fx)
      


    let update (nmc : NmConfig) (stopCounter:OptimizationStop.StopCounter) (fn : vector -> float) (nmInterResult : NmInterationResult) =   
      let simplexes = Array.copy  nmInterResult.Simplexes // Copy necessary ???
      let fx =  nmInterResult.Fx 
      let mutable laststep_ = nmInterResult.LastStep
  
      // Calculate centroid of n best points (ie excluding worst point) 
      let xbar = 
        let mutable tmp = Vector.zeroCreate simplexes[0].Length 
        for i = 0 to (simplexes.Length - 2) do
          tmp <- tmp + simplexes[i]
    
        let d = float (simplexes.Length - 1)    
        tmp |> Vector.map (fun v -> v / d)

      // Calculate reflection point
      let xr = (1. + nmc.Rho) * xbar - nmc.Rho * simplexes[simplexes.Length - 1]   
      let fxr = fn xr
      stopCounter.IncFunctionEvaluationCounter()

      if (fxr < fx[simplexes.Length - 2]) then  
        // reflection point is better than worst point
        if (fxr < fx[0]) then    
          //reflection point is better than best point - > expand
          let xe = (1. + nmc.Rho * nmc.Chi) * xbar - nmc.Rho * nmc.Chi * simplexes[simplexes.Length - 1]
          let fxe = fn xe
          stopCounter.IncFunctionEvaluationCounter()
          if (fxe < fxr) then
            // accept expansion point
            simplexes[simplexes.Length - 1] <- xe
            fx[simplexes.Length - 1] <- fxe
            laststep_ <- StepType.Expansion
      
          else      
            // accept reflection point
            simplexes[simplexes.Length - 1] <- xr
            fx[simplexes.Length - 1] <- fxr
            laststep_ <- StepType.Reflection
          
        else
          // accept reflection point
          simplexes[simplexes.Length - 1] <- xr
          fx[simplexes.Length - 1] <- fxr
          laststep_ <- StepType.Reflection

      else
        // Try a contraction
        if ((fx[simplexes.Length - 2] <= fxr) && (fxr < fx[simplexes.Length - 1])) then
    
          // perform an outside contraction
          let xc = (1. + nmc.Psi * nmc.Rho) * xbar - nmc.Psi * nmc.Rho * simplexes[simplexes.Length - 1]
          let fxc = fn xc
          stopCounter.IncFunctionEvaluationCounter()
          if (fxc < fxr) then
            // accept the outside contraction
            simplexes[simplexes.Length - 1] <- xc
            fx[simplexes.Length - 1] <- fxc
            laststep_ <- StepType.OutsideContraction      
          else      
            // perform a shrink step
            for i = 1 to (simplexes.Length - 1) do        
              simplexes[i] <- simplexes[0] + nmc.Sigma * (simplexes[i] - simplexes[0])
              fx[i] <- fn simplexes[i]
              stopCounter.IncFunctionEvaluationCounter()
      
            laststep_ <- StepType.Shrink 

        else    
          // perform an inside contraction
          let xcc = (1. - nmc.Psi) * xbar + nmc.Psi * simplexes[simplexes.Length - 1];
          let fxcc = fn xcc
          stopCounter.IncFunctionEvaluationCounter()

          if (fxcc < fx[simplexes.Length - 1]) then      
            // accept inside contraction
            simplexes[simplexes.Length - 1] <- xcc
            fx[simplexes.Length - 1] <- fxcc
            laststep_ <- StepType.InsideContraction

          else
            // perform a shrink step
            for i = 1 to (simplexes.Length - 1) do

              simplexes[i] <- simplexes[0] + nmc.Sigma * (simplexes[i] - simplexes[0])
              fx[i] <- fn simplexes[i]
              stopCounter.IncFunctionEvaluationCounter()

            laststep_ <- StepType.Shrink
  
      let x,fx = rankVertices simplexes fn    
      let evalFn = fx[0] //fn simplexes[0]

      {
        Simplexes = x
        Fx        = fx
        Vectors   = 
            let tmp = nmInterResult.Vectors
            tmp[stopCounter.IterationCounter] <- x[0] 
            tmp
        Values   = 
            let tmp = nmInterResult.Values
            tmp[stopCounter.IterationCounter] <- evalFn //fn simplexes[0] 
            tmp
        GradientNorms =
            let tmp = nmInterResult.GradientNorms
            tmp[stopCounter.IterationCounter] <- 
                      2.0 * System.Math.Abs(fx[x.Length - 1] - fx[0]) /
                      (System.Math.Abs(fx[x.Length - 1]) + System.Math.Abs(fx[0]) + System.Double.Epsilon)
            tmp
        //Counter = nmIterResult.Counter + 1 
        SolutionVector = x[0]
        Solution = evalFn
        LastStep = laststep_
      }



    ///<summary> Initialize the optimization method </summary>
    ///<remarks> The use of this function is intended for testing/debugging purposes only </remarks>
    let initInteration (maxIteration:int) (initialsimplex :vector[]) (fn : vector -> float) = 
  
        let x,fx   = rankVertices initialsimplex fn      
        let evalFn = fn initialsimplex[0]

        {
          Simplexes = x
          Fx        = fx
          Vectors   = 
              let tmp = Array.zeroCreate (maxIteration+1) 
              tmp[0] <- initialsimplex[0] 
              tmp
          Values   = 
              let tmp = Array.zeroCreate (maxIteration+1) 
              tmp[0] <- evalFn //fn initialsimplex[0] 
              tmp
          GradientNorms =
              let tmp = Array.zeroCreate (maxIteration+1)
              tmp[0] <- 2.0 * System.Math.Abs(fx[x.Length - 1] - fx[0]) /
                        (System.Math.Abs(fx[x.Length - 1]) + System.Math.Abs(fx[0]) + System.Double.Epsilon)
              tmp
          //Counter  = 0 
          SolutionVector = x[0]
          Solution = evalFn
          LastStep = StepType.Initialization
        }

 

    ///<summary> Minimize the given cost function and stop criteria </summary>
    let minimizeWithStopCriteria (nmc : NmConfig) (x :vector) (fn : vector -> float) (stopCriteria:OptimizationStop.StopCriteria) = 
      // Action<double> newMinimalValueFound)

      let initialsimplex = createSimplex nmc x
      //let stopCriteria = Stop.defaultStopCriteria
      let stopCounter = OptimizationStop.StopCounter() 

      //let minCostSoFar = iteration.Fx[0]

      // Iterate the optimization method
      let rec loop nmInterResult =
        stopCounter.IncIerationCounter()
        let newNmInterResult = update nmc stopCounter fn nmInterResult

        printfn "Counter: %i" stopCounter.IterationCounter

        // if (fx[0] < minCostSoFar) then
        //   minCostSoFar = fx[0];
        //   newMinimalValueFound?.Invoke(minCostSoFar);
        let test = 
          (not <| stopCriteria.CancellationToken.IsCancellationRequested && not <| System.Double.IsNaN(nmInterResult.Fx[0])) 
          && stopCriteria.IsCriteria stopCounter  (newNmInterResult.Values[stopCounter.IterationCounter - 1]) (newNmInterResult.Values[stopCounter.IterationCounter])

        //iterationValues_[endCriteria_.iterationCounter]
        if test then
          loop newNmInterResult      
        else
          { newNmInterResult with Solution = newNmInterResult.Values[stopCounter.IterationCounter] } 

      loop (initInteration stopCriteria.MaxIteration initialsimplex fn)


    ///<summary> Minimize the given cost function and stop criteria </summary>
    let minimize (nmc : NmConfig) (x :vector) (fn : vector -> float) =
      minimizeWithStopCriteria nmc x fn OptimizationStop.defaultStopCriteria
