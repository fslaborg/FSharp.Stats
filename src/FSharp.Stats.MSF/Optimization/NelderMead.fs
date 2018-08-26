namespace FSharp.Stats.Optimization


module NelderMead =

    open Microsoft.SolverFoundation.Common
    open Microsoft.SolverFoundation.Services
    open Microsoft.SolverFoundation.Solvers


    let minimize (f: float[] -> float) (init:float[]) = //xLower =
        
        let solverParams = NelderMeadSolverParams()
        let solver = NelderMeadSolver() 
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
        //let cGradient  : float[] = Array.zeroCreate variables.Length

        let compute (values:ValuesByIndex) =
            for i=0 to variables.Length-1 do  
                cVarValues.[i] <- values.[variables.[i]]
            f cVarValues

        let diffFunc =
            let eval (model:INonlinearModel) (rowVid:int) (values:ValuesByIndex) (newValues:bool) =
                if newValues then
                    cValue <- compute values
                    cValue
                else
                    cValue
                
            new System.Func<INonlinearModel, int, ValuesByIndex, bool, float> (eval) 


        solver.FunctionEvaluator <- diffFunc
        
        solver.Solve(solverParams) |> ignore
        
        variables |> Array.map (fun v -> solver.GetValue(v).ToDouble() )
    

    let minimizeWith (f: float[] -> float) (init:float[]) (lower:float[]) (upper:float[])  =
        
        let solverParams = NelderMeadSolverParams()
        let solver = NelderMeadSolver()
        
        //add variables for every starting point
        let variables = 
            init 
            |> Array.mapi (fun i sp -> 
                let (_,variable) = solver.AddVariable(null)
                solver.SetValue(variable, Rational.op_Implicit sp)
                solver.SetBounds(variable, Rational.op_Implicit lower.[i], Rational.op_Implicit upper.[i])
                variable
                )

        //add a row and set it as the goal 
        let _, vidRow = solver.AddRow(null) 
        solver.AddGoal(vidRow, 0, true)  |> ignore
        
        let mutable cValue : float = nan
        let cVarValues : float[] = Array.zeroCreate variables.Length
        //let cGradient  : float[] = Array.zeroCreate variables.Length

        let compute (values:ValuesByIndex) =
            for i=0 to variables.Length-1 do  
                cVarValues.[i] <- values.[variables.[i]]
            f cVarValues

        let diffFunc =
            let eval (model:INonlinearModel) (rowVid:int) (values:ValuesByIndex) (newValues:bool) =
                if newValues then
                    cValue <- compute values
                    cValue
                else
                    cValue
                
            new System.Func<INonlinearModel, int, ValuesByIndex, bool, float> (eval) 


        solver.FunctionEvaluator <- diffFunc
        
        solver.Solve(solverParams) |> ignore
        
        variables |> Array.map (fun v -> solver.GetValue(v).ToDouble() )
        


    let minimizeSingleWith (f: float -> float) (init:float) (lower:float) (upper:float)  =
        
        let solverParams = NelderMeadSolverParams()
        let solver = NelderMeadSolver()
        //add variables for every starting point
        let variableIndex= 
            let (_,variable) = solver.AddVariable(null)
            solver.SetValue(variable, Rational.op_Implicit init)
            variable
            
        solver.SetBounds(variableIndex, Rational.op_Implicit lower, Rational.op_Implicit upper)
        solver.AddGoal(variableIndex,0, true)  |> ignore
        
        let mutable cValue : float = nan

        let diffFunc =
            let eval (model:INonlinearModel) (rowVid:int) (values:ValuesByIndex) (newValues:bool) =
                if newValues then
                    cValue <- f values.[variableIndex]
                    cValue
                else
                    cValue
                
            new System.Func<INonlinearModel, int, ValuesByIndex, bool, float> (eval) 


        solver.FunctionEvaluator <- diffFunc
        
        solver.Solve(solverParams) |> ignore
        
        solver.GetValue(variableIndex).ToDouble()




//let rosenbrockFunction (values:float []) (gradient:float []) = 
//    gradient.[0] <- -2. * (1. - values.[0]) - 400. * values.[0] * (values.[1] - (values.[0] * values.[0])) 
//    gradient.[1] <- 200. * (values.[1] - (values.[0] * values.[0])) 
//    System.Math.Pow(1. - values.[0], 2.) + 100. * (System.Math.Pow(values.[1] - (values.[0] * values.[0]), 2.))
//
//
//
//QuasiNewton.minimize rosenbrockFunction ([|0.;0.|])
   




