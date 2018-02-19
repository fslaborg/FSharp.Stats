namespace FSharp.Stats.Optimization


// Minimize/maximize the objective function
//   x^T X^T + c^T x
// subject to 
//   blo <= A x <= bhi
//   xlo <= x <= xhi
module LP = 

    open Microsoft.SolverFoundation.Common
    open Microsoft.SolverFoundation.Services
    open Microsoft.SolverFoundation.Solvers


    let private createDecisions (x:array<float*float>) =
        x 
        |> Array.mapi (fun i (xlo,xhi) -> 
            let xlo' = xlo |> Rational.op_Implicit
            let xhi' = xhi |> Rational.op_Implicit
            Decision(Domain.RealRange(xlo', xhi'), sprintf "x%i" i))

    let private createGoalTerm (decisions:array<Decision>) (c:array<float>) =
        // c.length must equal x.length
        let goalTerm = 
            c
            |> Array.mapi (fun i v -> Term.op_Implicit(v) * decisions.[i] )
            |> Model.Sum
        
        goalTerm

    let private createConstrains (decisions:array<Decision>) (A:float[,]) (b:array<float*float>) =
        let constraints =
            let dim = Array2D.length2 A // must be length decision
            b
            |> Array.mapi (fun i (blo,bhi) -> 
                let blo' = blo |> Term.op_Implicit
                let bhi' = bhi |> Term.op_Implicit
                //let tmp = [|for ii=0 to dim-1 do yield Term.op_Implicit(A.[i,ii]) * decisions.[ii] |] |> Model.Sum
                let tmp = [|for ii=0 to dim-1 do yield (Model.Product(Term.op_Implicit(A.[i,ii]),decisions.[ii])) |] |> Model.Sum
                Model.And(Model.GreaterEqual(tmp,blo'),Model.LessEqual(tmp,bhi'))
            )
        constraints

    let minimizeWith (A:float[,]) (b:array<float*float>) (c:float[]) (x:array<float*float>) =
        let context = SolverContext.GetContext()
        context.ClearModel() |> ignore
        let model = context.CreateModel()

        let decisions = createDecisions x
        model.AddDecisions(decisions)

        let goalTerm = createGoalTerm decisions c
        model.AddGoal("goal", GoalKind.Minimize, goalTerm ) |> ignore
        

        let constraints = createConstrains decisions A b 
        model.AddConstraints("constraints",constraints) |> ignore
        
        let solution = context.Solve(SimplexDirective())     
        
        // let sw = System.IO.StringWriter()
        // context.SaveModel(FileFormat.OML, sw)
        // sw.ToString()
        
        //solution.GetReport()

        solution.Decisions
        |> Seq.map (fun d -> d.ToDouble())
        |> Seq.toArray


    let minimize (A:float[,]) (b:array<float*float>) (c:float[]) =
        let x = Array.init c.Length (fun _ -> (0.0,infinity) ) 
        minimizeWith A b c x


    let maximizeWith (A:float[,]) (b:array<float*float>) (c:float[]) (x:array<float*float>) =
        let context = SolverContext.GetContext()
        context.ClearModel() |> ignore
        let model = context.CreateModel()

        let decisions = createDecisions x
        model.AddDecisions(decisions)

        let goalTerm = createGoalTerm decisions c
        model.AddGoal("goal", GoalKind.Maximize, goalTerm ) |> ignore
        
        let constraints = createConstrains decisions A b 
        model.AddConstraints("constraints",constraints) |> ignore
        
        let solution = context.Solve(SimplexDirective())     
        
        // let sw = System.IO.StringWriter()
        // context.SaveModel(FileFormat.OML, sw)
        // sw.ToString()
        
        //solution.GetReport()

        solution.Decisions
        |> Seq.map (fun d -> d.ToDouble())
        |> Seq.toArray


    let maximize (A:float[,]) (b:array<float*float>) (c:float[]) =
        let x = Array.init c.Length (fun _ -> (0.0,infinity) ) 
        maximizeWith A b c x




//let c = [|20.0; 15.0|]
//let x = [|0.,9000.; 0.,6000.|]
//let Ax = [|1900.,100000.; 1500.,100000.; 500.,100000.;|]
//
//let A = array2D [[0.3;0.4];[0.4;0.2];[0.2;0.3];]
//
//
//LP.minimizeWith A Ax c x 
//LP.maximizeWith A Ax c x 
//
//
//
//// Goals:
//// goal: 90500
//
//// Decisions:
//// x0: 2200
//// x1: 3100



