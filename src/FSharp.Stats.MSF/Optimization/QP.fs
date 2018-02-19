namespace FSharp.Stats.Optimization


module QP =

    open Microsoft.SolverFoundation.Common
    open Microsoft.SolverFoundation.Services
    open Microsoft.SolverFoundation.Solvers

    let private addVariableWithBounds (solver:InteriorPointSolver) (index:int) (xlo:float) (xhi:float) =
        let (_,variable) = solver.AddVariable(sprintf "x%i" index)
        solver.SetBounds(variable, Rational.op_Implicit xlo, Rational.op_Implicit xhi)
        variable




    let private addGoal (solver:InteriorPointSolver) (minimize:bool) (coefficients:int[]) (c:float[]) (Q:float[,]) =
        let (_,goal) = solver.AddRow("Goal")
        solver.AddGoal(goal, 0, minimize) |> ignore
        c
        |> Array.iteri (fun i coeff -> solver.SetCoefficient(goal, coefficients.[i], Rational.op_Implicit coeff))
        
        let dim = Array2D.length1 Q
        for i=0 to dim-1 do 
            for ii=i to dim-1 do
                solver.SetCoefficient(goal, Rational.op_Implicit(Q.[ii,i]), coefficients.[i], coefficients.[ii])
                //printfn "%f" Q.[ii,i]

        goal


    let private addconstraintsWithBounds (solver:InteriorPointSolver) (coefficients:int[]) (A:float[,]) (b:array<float*float>) =
        let constraints =
            let dim = Array2D.length2 A // must be length decision
            b
            |> Array.mapi (fun i (blo,bhi) -> 
                let (_,constraint') = solver.AddRow(sprintf "c%i" i)
                solver.SetBounds(constraint', Rational.op_Implicit blo, Rational.op_Implicit bhi)
                // let blo' = blo |> Term.op_Implicit
                // let bhi' = bhi |> Term.op_Implicit
                for ii=0 to dim-1 do
                    solver.SetCoefficient(constraint', coefficients.[ii], Rational.op_Implicit A.[i,ii])                            
                constraint'
            )
        
        constraints

    // x^T Q X^T + c^T x
    let minimizeWith (A:float[,]) (b:array<float*float>) (Q:float[,]) (c:float[]) (x:array<float*float>)  =
        let solver = InteriorPointSolver()
        
        let variables =
            x |> Array.mapi (fun i (xlo,xhi) -> addVariableWithBounds solver i xlo xhi)
        
        let goal = addGoal solver true variables c Q
        
        let _ = addconstraintsWithBounds solver variables A b

        let param = InteriorPointSolverParams()
        let solution = solver.Solve(param)
        
        variables |> Array.map (fun v -> solution.GetValue(v).ToDouble())


    let minimize (A:float[,]) (b:array<float*float>) (Q:float[,]) (c:float[]) =
        let x = Array.init c.Length (fun _ -> (-infinity,infinity) )
        minimizeWith A b Q c x

    let maximizeWith (A:float[,]) (b:array<float*float>) (Q:float[,]) (c:float[]) (x:array<float*float>)  =
        let solver = InteriorPointSolver()
        
        let variables =
            x |> Array.mapi (fun i (xlo,xhi) -> addVariableWithBounds solver i xlo xhi)
        
        let goal = addGoal solver false variables c Q
        
        let _ = addconstraintsWithBounds solver variables A b

        let param = InteriorPointSolverParams()
        let solution = solver.Solve(param)
        
        variables |> Array.map (fun v -> solution.GetValue(v).ToDouble())


    let maximize (A:float[,]) (b:array<float*float>) (Q:float[,]) (c:float[]) =
        let x = Array.init c.Length (fun _ -> (-infinity,infinity) )
        maximizeWith A b Q c x



//// Q = [1  0       Term: x^2
////     -2  1]      Term: -2xy + y^2
//// let Q = array2D [[1.;0.];[-2.;1.];]
//// let A' = array2D [[ 1.; 1.;]; [ -1.; 2.;]]
//// let b' = [|0.,7.; 0.,4.;|]
//// let c' = [|2.; -3.;|]
//
//
//
//// example from: http://www.di.fc.ul.pt/~jpn/r/optimization/optimization.html
//
//let Q = array2D [[2.;-1.;0.];[-1.;2.;-1.];[0.;-1.;2.]]
//let A' = array2D [[-4.;-3.;0.];[2.;1.;0.];[0.;-2.;1.]]
//let b' = [|-8.,-8.; 2.,2.; 0.,infinity;|]
//let c' = [|0.;-5.; 3.;|]
//
//QP.minimize A' b' Q c'
   




