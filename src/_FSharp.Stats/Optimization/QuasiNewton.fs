namespace FSharp.Stats.Optimization


module QuasiNewton =

    open Microsoft.SolverFoundation.Common
    open Microsoft.SolverFoundation.Services
    open Microsoft.SolverFoundation.Solvers


    let minimize (f: float[] -> float[] -> float) (init:float[]) =
        
        let solverParams = CompactQuasiNewtonSolverParams() 
        let solver = CompactQuasiNewtonSolver() 
        //add variables for every starting point
        let variables = 
            init 
            |> Array.map (fun sp -> 
                let (_,variable) = solver.AddVariable(null)
                solver.SetValue(variable, Rational.op_Implicit sp)
                variable
                )

        //add a row and set it as the goal 
        let _, vidRow = solver.AddRow(null) 
        solver.AddGoal(vidRow, 0, true)  |> ignore
        
        let mutable cValue : float = nan
        let cVarValues : float[] = Array.zeroCreate variables.Length
        let cGradient  : float[] = Array.zeroCreate variables.Length

        let compute (values:ValuesByIndex) =
            for i=0 to variables.Length-1 do  
                cVarValues.[i] <- values.[variables.[i]]
            f cVarValues cGradient

        let diffFunc =
            let eval (model:INonlinearModel) (rowVid:int) (values:ValuesByIndex) (newValues:bool) =
                if newValues then
                    cValue <- compute values
                    cValue
                else
                    cValue
                
            new System.Func<INonlinearModel, int, ValuesByIndex, bool, float> (eval) 

        let gradFunc =
            let evalGrad (model:INonlinearModel) (rowVid:int) (values:ValuesByIndex) (newValues:bool) (gradient:ValuesByIndex) =
                if newValues then
                    cValue <- compute values
                for i=0 to variables.Length-1 do 
                    gradient.[variables.[i]] <- cGradient.[i]
                
            new System.Action<INonlinearModel, int, ValuesByIndex, bool, ValuesByIndex> (evalGrad) 

        solver.FunctionEvaluator <- diffFunc 
        solver.GradientEvaluator <- gradFunc 
        solver.Solve(solverParams) |> ignore
        
        variables |> Array.map (fun v -> solver.GetValue(v).ToDouble() )
    

//let rosenbrockFunction (values:float []) (gradient:float []) = 
//    gradient.[0] <- -2. * (1. - values.[0]) - 400. * values.[0] * (values.[1] - (values.[0] * values.[0])) 
//    gradient.[1] <- 200. * (values.[1] - (values.[0] * values.[0])) 
//    System.Math.Pow(1. - values.[0], 2.) + 100. * (System.Math.Pow(values.[1] - (values.[0] * values.[0]), 2.))
//
//
//
//QuasiNewton.minimize rosenbrockFunction ([|0.;0.|])
   




