namespace FSharp.Stats.Fitting
open System
open System.Collections.Generic

(*
1. Do not use PSeq for parallelization (use Async instead)
2. add:
    FSharp.Stats.ServiceLocator.setEnvironmentPathVariable @"..\FSharp.Stats\lib"
    FSharp.Stats.Algebra.LinearAlgebra.Service()
*)

module Hermite =    

    open FSharp.Stats
    open FSharp.Stats.Optimization

    let initEvalAt (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =

        let n = x.Length

        //define interval stepwidths
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )
        // helper functions f(i,1-4)(t) for smoothing spline
        let calcF1 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            (x.[i+1] - t) / h.[i]

        let calcF2 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            (t - x.[i]) / h.[i]

        let calcF3 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            ((calcF1 h x i t)**3.0 - (calcF1 h x i t) ) * (h.[i]**2.0) / 6.0

        let calcF4 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            ((calcF2 h x i t)**3.0 - (calcF2 h x i t) ) * (h.[i]**2.0) / 6.0

        // helper function s(i) for smoothing spline 
        let calcS (h:Vector<float>) (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) (i:int) (t:float) =
            a.[i] * (calcF1 h x i t) + a.[i+1] * (calcF2 h x i t) + c.[i] * (calcF3 h x i t) + c.[i+1] * (calcF4 h x i t)

        let evalAt =  calcS h x a c 
    
        (fun t ->
            let i = 
                match Array.tryFindIndexBack (fun xs -> xs <= t) (x.InternalValues) with 
                | Some x -> x 
                | None   -> failwith "The x-value is not part of the section used to estimate the spline coefficients, thus a monotone function progression can not be guaranteed"
            evalAt i t
            )

    //calculates extrema with (type,x_value) where type is +1 for maxima and -1 for minima
    let getExtrema (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =
        let n = x.Length
        //define interval stepwidths
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )

        let searchExtrema i (a:Vector<float>) (c:Vector<float>) =
            //checks, if a polynomial coefficient vanishes 
            let isZero (coeff: float) =
                let tolerance = 0.00001
                if (Math.Abs coeff) <= tolerance then 0. else coeff
            let (lower,upper) = 0.,1.
            let aCoeff = -1./6. * c.[0] + 1./6. * c.[1]                 |> isZero
            let bCoeff = 0.5 * c.[0]                                    |> isZero
            let cCoeff = -1./3. * c.[0] - 1./6. * c.[1] - a.[0] + a.[1] |> isZero

            let aCoeff' = 3. * aCoeff
            let bCoeff' = 2. * bCoeff
            let cCoeff' = cCoeff

            let aCoeff'' = 2. * aCoeff'
            let bCoeff'' = bCoeff'

            let findFstDeriv_Zero =
                let root = (pown bCoeff' 2) - 4. * aCoeff' * cCoeff'
                if root < 0. then [] 
                else 
                    let result1 = (- bCoeff' + sqrt(root)) / (2. * aCoeff')
                    let result2 = (- bCoeff' - sqrt(root)) / (2. * aCoeff')
                    if result1 = result2 then
                        [result1]
                    else [result1;result2]

            let slope0 =    
                //check wether the extremum is inside the interval [0,1] (hermite basis interval)
                findFstDeriv_Zero |> List.filter (fun x -> x >= lower && x <= upper)

            slope0
            |> List.map (fun xValNorm -> 
                //recalculate the true xValue outside the normed interval [0,1]
                let xVal = h.[i] * xValNorm + x.[i]
                let getSndDerivative =
                    aCoeff'' * xValNorm + bCoeff''
                match getSndDerivative with
                | getSndDerivative when getSndDerivative = 0. -> (0.,0.)
                | getSndDerivative when getSndDerivative < 0. -> (1.,xVal)
                | _                                           -> (-1.,xVal)
                )
            |> List.filter (fun v -> v <> (0.,0.))

        x.[ .. x.Length-2]
        |> Array.ofSeq
        |> Array.map (fun intervalStart -> 
            let i =
                if intervalStart = Seq.last x then 
                    Seq.length x - 2
                else
                    x
                    |> Seq.findIndex(fun x_Knot -> (x_Knot - intervalStart) > 0.)
                    |> fun nextInterval -> nextInterval - 1
            searchExtrema i a.[i .. i + 1] c.[i .. i + 1]
            )
        |> Array.filter (fun x -> x <> [])
        |> List.concat
        |> List.distinctBy (fun (indicator,xVal) -> round 2 xVal )

    type Condition =
        | In0 //monotonically increasing
        | In1 //1 maximum
        | In2 //2 maxima
        | De0 //monotonically decreasing
        | De1 //1 minimum
        | De2 //2 minima
        | Complex //more complex


    //check the spline for the predefined condition
    let checkshape (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) (con:Condition)=
        let extrema = getExtrema x a c
        let extremaCount = extrema.Length
        let n = a.Length
        match con with
        | In0       -> extremaCount = 0 && a.[0] <= a.[n-1]
        | In1       -> extremaCount = 1 && (fst extrema.[0]) = 1.
        | In2       -> extremaCount = 2 && (fst extrema.[0]) = 1. && (fst extrema.[1]) = -1.
        | De0       -> extremaCount = 0 && a.[0] >= a.[n-1]
        | De1       -> extremaCount = 1 && (fst extrema.[0]) = -1.
        | De2       -> extremaCount = 2 && (fst extrema.[0]) = -1. && (fst extrema.[1]) = 1.
        | Complex   -> true

    let mapCondition (operator:Matrix<float> -> matrix) con =
        let mat = matrix [[1.]]
        match con with
        | x when con = 0 -> if mat = operator mat then Condition.De0 else Condition.In0
        | x when con = 1 -> if mat = operator mat then Condition.De1 else Condition.In1
        | x when con = 2 -> if mat = operator mat then Condition.De2 else Condition.In2

    type TempClassResult = {
        //spline function values at knots
        TraceA   : vector
        //spline second derivative values at knots
        TraceC   : vector
        //weighted error of a and y
        Error    : float
        //mGCV
        GCV      : float
        //used smoothing factor lambda
        Lambda   : float
        //used constraint matrix for shape determination
        Ctemp    : float [,]
        //AIC determined by SSE and the number of used extrema
        AICc     : float
        //function, that returns the splines' value at a given x_value
        SplineFunction  : (float -> float)}
    
    let createTempClassResult a c e gcv lambda ctemp aic splinefunction = {
        TraceA          = a
        TraceC          = c
        Error           = e
        GCV             = gcv
        Lambda          = lambda
        Ctemp           = ctemp
        AICc            = aic
        SplineFunction  = splinefunction
        } 
    
    let private unzip4 li =
        let length = Seq.length li
        let la = Array.zeroCreate length
        let lb = Array.zeroCreate length
        let lc = Array.zeroCreate length
        let ld = Array.zeroCreate length
        li |> Seq.iteri (fun i (a,b,c,d) ->   (la.[i] <- a)
                                              (lb.[i] <- b)
                                              (lc.[i] <- c)
                                              (ld.[i] <- d)
                                                )
        (la,lb,lc,ld)
    
    let calcMeanOfRep (arr:Vector<float>) rep =
        if arr.Length%rep = 0 then
            let length = arr.Length / rep 
            vector [for i = 0 to length-1 do yield arr.[i*rep..i*rep+rep-1] |> Seq.average]      
        else failwithf "arrLength no multiple of replicate number"
             
    let normValues (yVal:Vector<float>) =
        let yMean = yVal |> Seq.mean
        let std   = yVal |> Seq.stDev
        yVal |> Vector.map (fun x -> (x - yMean) / std) 
       
    type WeightingMethod =
        //weight: 1
        | Equal
        //weight: minmax((1/stDev),Wmin)
        | VarRobust//Variance
        //max 0. (log(1./|Seq.cvPopulation g|),2.))
        | CV
        //weight: (1/stDev)
        | StandardDeviation
        //weight: sqrt(1/stDev)
        | StandardDeviationSqrt
        //weight: sqrt(1/(stDev/mean))
        | StandardDeviationAdj
        //weight: sqrt(sqrt(1/(stDev/mean)))
        | StandardDeviationAdjSqrt
        
    ///creates a weighting matrix out of the x-, and y-Values, the given weighting method and the number of measured replicates.
    let getWeighting (xVal:Vector<float>) (yVal:Vector<float>) (w_method:WeightingMethod) (numRep:int) =
        match w_method with
        | Equal ->
            Matrix.diag(Vector.ones xVal.Length)
        | VarRobust -> //Variance
            let Wtemp = Matrix.create xVal.Length xVal.Length 0.            
            let diagVec = 
                if yVal.Length%numRep = 0 then
                    vector [for i = 0 to xVal.Length - 1 do yield 1. / (yVal.[(i*numRep)..(i*numRep + numRep - 1)] |> Seq.stDev)]
                else failwithf "arrLength no multiple of replicate number"
            //improves robustness of standard weighting
            let (maxWtemp,minWtemp) = 
                let mean = diagVec |> Seq.mean
                let std  = diagVec |> Seq.stDev
                (mean + std),(mean - std)
            
            let diagVecNew = 
                diagVec |> Vector.map (fun x -> match x with
                                                | x when x > maxWtemp -> maxWtemp
                                                | x when x < minWtemp -> minWtemp
                                                | _ -> x)
            let finalDiag = 
                let trace = (diagVecNew |> Vector.sum)
                let length = (float Wtemp.NumCols)
                diagVecNew |> Vector.map (fun x -> x / trace * length)
            for i = 0 to xVal.Length - 1 do Wtemp.[i,i] <- finalDiag.[i]
            Wtemp
        | CV -> 
            let Wtemp = Matrix.create xVal.Length xVal.Length 0.
            let cvOfVec =
                if yVal.Length%numRep = 0 then
                    let length = yVal.Length / numRep 
                    let cv = vector [for i = 0 to length-1 do yield yVal.[i*numRep..i*numRep+numRep-1] |> fun g -> max 0. (System.Math.Log((1./(Math.Abs(Seq.cvPopulation g))),2.))(*(1./((Seq.cvPopulation g) + 0.25))*)] //0.25???
                    cv
                else failwithf "arrLength no multiple of replicate number"

            for i = 0 to xVal.Length - 1 do Wtemp.[i,i] <- cvOfVec.[i]
            Wtemp
        | StandardDeviation     ->
            let Wtemp = Matrix.create xVal.Length xVal.Length 0.            
            let diagVec = 
                if yVal.Length%numRep = 0 then
                    vector [for i = 0 to xVal.Length - 1 do yield 1. / (yVal.[(i*numRep)..(i*numRep + numRep - 1)] |> Seq.stDev)]
                else failwithf "arrLength no multiple of replicate number"
            let avg = Seq.average diagVec
            for i = 0 to xVal.Length - 1 do Wtemp.[i,i] <- diagVec.[i] / avg
            Wtemp
        | StandardDeviationSqrt ->
            let Wtemp = Matrix.create xVal.Length xVal.Length 0.            
            let diagVec = 
                if yVal.Length%numRep = 0 then
                    vector [for i = 0 to xVal.Length - 1 do yield Math.Sqrt (1. / (yVal.[(i*numRep)..(i*numRep + numRep - 1)] |> Seq.stDev))]
                else failwithf "arrLength no multiple of replicate number"
            let avg = Seq.average diagVec
            for i = 0 to xVal.Length - 1 do Wtemp.[i,i] <- diagVec.[i] / avg
            Wtemp
        | StandardDeviationAdj -> //CV_Norm          ->
            let Wtemp = Matrix.create xVal.Length xVal.Length 0.
            let cvOfVec =
                if yVal.Length%numRep = 0 then
                    let length = yVal.Length / numRep 
                    let cv = vector [for i = 0 to length-1 do yield yVal.[i*numRep..i*numRep+numRep-1] |> fun x -> sqrt ( 1. / (Seq.stDev x / Seq.average x))]
                    cv
                else failwithf "arrLength no multiple of replicate number"
            let cvAvg = Seq.average cvOfVec
            for i = 0 to xVal.Length - 1 do Wtemp.[i,i] <- cvOfVec.[i] / cvAvg
            Wtemp
        | StandardDeviationAdjSqrt ->  //CV_NormSqrt      ->
            let Wtemp = Matrix.create xVal.Length xVal.Length 0.
            let cvOfVec =
                if yVal.Length%numRep = 0 then
                    let length = yVal.Length / numRep 
                    let cv = vector [for i = 0 to length-1 do yield yVal.[i*numRep..i*numRep+numRep-1] |> fun x -> sqrt(sqrt ( 1. / (Seq.stDev x / Seq.average x)))]
                    cv
                else failwithf "arrLength no multiple of replicate number"
            let cvAvg = Seq.average cvOfVec
            for i = 0 to xVal.Length - 1 do Wtemp.[i,i] <- cvOfVec.[i] / cvAvg
            Wtemp
               
    ///d: complexity (count of extrema)
    let getAIC d (a:Vector<float>) (y:Vector<float>) (W:Matrix<float>) =
        let sse =
            let weightedResiduals =
                W * (y - a)
            let nSSE =
                pown (weightedResiduals |> Vector.norm) 2
            nSSE
        let n = a.Length |> float
        n * Math.Log(sse/n) + 2. * (float d) +  ((2. * float d * (float d + 1.))/(float n - d - 1.))

    ///calculates an unconstrained smoothing spline
    let getInitialEstimate (D:Matrix<float>) (H:Matrix<float>) (W:Matrix<float>) n y xVal = 
        let I = Matrix.identity n
        let Z = Matrix.identity n
        let calcGLambda lambd = 
            let dInverse = Algebra.LinearAlgebra.Inverse D
            (2.0 * ( H.Transpose * dInverse * H + (lambd / float n) * (W.Transpose * W)))
            
        let A_tild (lambd:float)    = 
            let zproInverse = Algebra.LinearAlgebra.Inverse (Z.Transpose * (calcGLambda lambd) * Z)
            2. * (lambd / (float n)) * Z * zproInverse * Z.Transpose * W.Transpose * W
    
        let a_star (lambd:float)= 
            (A_tild lambd) * y
            
        let V_tild (lambd:float) =     
            //robustGCV for small sample size
            let rho = 1.3

            let no = 
                (W * ((a_star lambd) - y))
                |> Vector.fold (fun acc x -> acc + (pown x 2)) 0.   
            
            let tr = 
                I - (rho * (A_tild lambd)) 
                |> Matrix.trace
            (float n) * no / (tr * tr)
    
        let Var_tild lambd =
            let aMat = (a_star lambd) |> Matrix.ofVector
            let yMat = y |> Matrix.ofVector
            let no = 
                (W * (aMat - yMat))
                |> fun m -> Matrix.getCol m 0
                |> Vector.norm
                |> fun x -> x*x
            let rho = 1.3
            let tr = I - (rho * (A_tild lambd)) |> Matrix.getDiag |> Vector.sum //
            (float n) * no / (tr)  
    
        //NelderMeadSolver for global minimum
        let lambda_hat = 
            //let lamlist = [for i = 0 to 64 do yield 0.01*(1.2**(float i))]
            //lamlist
            //|> List.map V_tild
            //|> List.min
            NelderMead.minimizeSingleWith V_tild 3. 0.0001 2000.    
        
        let variance_unconstrained = Var_tild lambda_hat
        let GCV_unconstrained = V_tild lambda_hat
        let a_unconstrained = a_star lambda_hat
        let c =         
            let tmpH = H
            let tmpD = D
            let tmp = 
                let tmp' = Algebra.LinearAlgebra.SolveLinearSystems tmpD tmpH
                tmp' * a_unconstrained
            Vector.init (tmp.Length+2) (fun i -> if i>0 && i<=tmp.Length then tmp.[i-1] else 0.0)

        let getShape = getExtrema xVal a_unconstrained c

        let aic = getAIC (float getShape.Length) a_unconstrained y W

        ///function that calculates the y_Value of the spline corresponding to the given x_Value
        let splineFunction = initEvalAt xVal a_unconstrained c

        createTempClassResult a_unconstrained c 0. GCV_unconstrained 0. (Array2D.zeroCreate 0 0) aic splineFunction
    
    ///calculates an unconstrained smoothing spline for a given weightingmatrix, x-, and y-Values
    let getInitialEstimateOfWXY W (y:Vector<float>) (xVal:Vector<float>) =
        let n = y |> Seq.length
    
        let h = Array.init (n-1) (fun i -> xVal.[i+1] - xVal.[i] )
        let H = Array2D.zeroCreate (n-2) n
        let D = Array2D.zeroCreate (n-2) (n-2)
        for i = 1 to (n-2) do
            H.[i-1,i-1] <-  1.0/h.[i-1]
            H.[i-1,i]   <- -( (1.0/h.[i-1]) + (1.0/h.[i]) )
            H.[i-1,i+1] <-  1.0/h.[i]
            D.[i-1,i-1] <-  (h.[i-1] + h.[i]) / 3.0
        for i = 1 to (n-3) do
            D.[i-1,i]   <- h.[i]/6.0
            D.[i,i-1] <- h.[i]/6.0        
    
        getInitialEstimate (Matrix.ofArray2D D) (Matrix.ofArray2D H) W n y xVal
    

    ///calculates the generalized cross validation(GCV), and the modified GCV for small sample sizes: (mGCV,GCV)
    let getGCV (C:Matrix<float>) (D:Matrix<float>) (H:Matrix<float>) (W:Matrix<float>) (n:int) (y:vector) (a:vector) (lambda:float) =
        //identify inequality constraints where C*a<=[0]
        let constr = C * (a |> Matrix.ofVector) 
        let tol = 0.001 
        //identify 'Active set' where C*a=[0]
        let Ca =    
            let rec loop i caPrev =
                if i < C.NumRows then
                    if System.Math.Abs(constr.[i,0]) <= tol then 
                        loop (i+1) (((C.Row i) |> Seq.toArray)::caPrev)
                    else loop (i+1) caPrev
                else 
                    if caPrev |> List.isEmpty then
                        Matrix.create 0 0 0.
                    else 
                    matrix (caPrev |> List.rev)
            loop 0 []
        ///calculate null space of Ca (based on SVD)
        let Z =
            if Ca.NumRows * Ca.NumCols = 0 then 
                Matrix.identity n            
            else
                let nullspace = 
                    let (eps,U,V) =
                        Algebra.LinearAlgebra.SVD Ca
                    let rank = eps |> Seq.filter (fun x -> x >= 1e-5) |> Seq.length //Threshold if a singular value is considered as 0. //mat.NumRows - eps.Length
                    let count = V.NumRows - rank
                    Matrix.getRows V (rank) (count)
                    |> Matrix.transpose
                nullspace
       
        let I = Matrix.identity n

        let G_lambda (lambd:float)  = 
            let dInverse = Algebra.LinearAlgebra.Inverse D
            2. * ( H.Transpose * dInverse * H + lambd / (float n)  * W.Transpose * W)
    
        let A_tild (lambd:float)    = 
            let zproInverse = Algebra.LinearAlgebra.Inverse (Z.Transpose * (G_lambda lambd) * Z)
            2. * (lambd / (float n)) * Z * zproInverse * Z.Transpose * W.Transpose * W

        let V_tild rho (lambd:float) =     
            let no =
                W * (a - y)
                |> Vector.fold (fun acc x -> acc + (pown x 2)) 0.   
  
            let tr = 
                I - rho * (A_tild lambd)
                |> Matrix.trace

            (float n) * no / (tr * tr)

        let var_ny (lambd:float) =
            let a = ((W * (a-y)) |>Vector.norm ) ** 2.
            let b = I - (A_tild lambd) |> Matrix.getDiag |> Vector.sum
            a/b

        //standard gcv
        let gcv  = V_tild 1.0 lambda
        //modified gcv
        let mGcv = V_tild 1.3 lambda
        //robust gcv
        let rGcv() =
            let A = A_tild lambda
            let gamma = 0.2
            let mu = ((A * A) |> Matrix.trace) / (float n)
            (gamma + (1. - gamma) * mu) * gcv


        let variance = var_ny lambda
        //(gcv,variance)
        (mGcv,gcv)

    //record type for easy handling inside the shape classification process
    type InnerResult = {
        TraceA  : Vector<float>
        GCV     : float
        Var     : float
        Error   : float
        CTemp   : float [,]
        Lambda  : float}

    let createInnerResult a g c e v l = {
        TraceA  = a
        GCV     = g
        Var     = v
        Error   = e
        CTemp   = c
        Lambda  = l}

    ///calculates a spline with the shape defined by the operator(increasing or decreasing) and the condition(0,1,2,or 3 extrema)
    let private spline operator (x:Vector<float>) (y:Vector<float>) (W:Matrix<float>) con =

        /// Sets the diagonal to value inplace
        let setDiagonalInplace value (m:Matrix<_>) =
            let min = min m.NumRows m.NumCols
            for i=0 to min-1 do
                m.[i,i] <- value
            m

        let calcGlambda  (D:Matrix<float>) (H:Matrix<float>) (W:Matrix<float>) (n:int) lambd = 
            let dInverse = Algebra.LinearAlgebra.Inverse D
            2.0 * ((H.Transpose * dInverse * H)   +  (lambd / float n) * W.Transpose * W)         

        let calcclambda (W:Matrix<float>) (n:int) (y:Vector<float>) lambd =
            //-2.0 * ((float lambd) / (float n)) * (W.Transpose * W) * y |> Vector.toArray//(rowvec * matrix) <> (matrix * vec)
            -2.0 * (float lambd) / (float n) * y.Transpose * W.Transpose * W
            
        ///calculates the inequality constraint matrix C for the quadratic programming approach
        let getCtemP n (B:Matrix<float>) (h:Vector<float>) =
            let mutable list :Matrix<float> list= []
            let Ctemp = Matrix.zero (4*(n-1)+1) n

            if con=0 then
                for i =1 to (n-1) do    
                    let temp = 
                        (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                        |> vector
                    Matrix.setRow Ctemp (4*i-4) temp
                    Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                    Matrix.setRow Ctemp (4*i-2) (temp - (B.Row (i-1) |> vector))
                    Matrix.setRow Ctemp (4*i-1) (temp - (B.Row (i) |> vector))
                Matrix.setRow Ctemp (4*(n-1)) (B.Row (n - 1) |> vector)
                [|Ctemp|]

            elif (con=1) then
                for j = 3 to (n - 1) do 
                    for i = 1 to (j-1) do
                        let temp = 
                            (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                            |> vector
                        Matrix.setRow Ctemp (4*i-4) temp
                        Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                        Matrix.setRow Ctemp (4*i-2) (temp - (B.Row (i-1) |> vector))
                        Matrix.setRow Ctemp (4*i-1) (temp - (B.Row (i) |> vector))
                        if i = j - 1 then
                            Matrix.setRow Ctemp (4*i-4) (Vector.zero Ctemp.NumCols)
                            Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector) //0
                            Matrix.setRow Ctemp (4*i-2) (Vector.zero Ctemp.NumCols)
                            Matrix.setRow Ctemp (4*i-1) (Vector.zero Ctemp.NumCols)

                    for i = j to (n-1) do 
                        let temp = 
                            (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                            |> vector
                        Matrix.setRow Ctemp (4*i-4) (- temp)
                        Matrix.setRow Ctemp (4*i-3) (- (B.Row (i-1) |> vector))
                        Matrix.setRow Ctemp (4*i-2) (- (temp - (B.Row (i-1) |> vector)))
                        Matrix.setRow Ctemp (4*i-1) (- (temp - (B.Row (i) |> vector)))
                    Matrix.setRow Ctemp (4*(n-1)) (- (B.Row (n-1) |> vector))
                    list <- (Matrix.copy Ctemp)::list
                list
                |> Array.ofList 
                
            elif con = 2 then
                for m = 3 to (n-1) do 
                    for j = (m+1) to (n-1) do //from 7 to 6 do
                        for i=1 to (m-1) do 
                            let temp = 
                                (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                |> vector
                            Matrix.setRow Ctemp (4*i-4) temp
                            Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                            Matrix.setRow Ctemp (4*i-2) (temp - (B.Row (i-1) |> vector))
                            Matrix.setRow Ctemp (4*i-1) (temp - (B.Row (i) |> vector))
                            if i = m - 1 then
                                Matrix.setRow Ctemp (4*i-4) (Vector.zero Ctemp.NumCols)
                                Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                                Matrix.setRow Ctemp (4*i-2) (Vector.zero Ctemp.NumCols)
                                Matrix.setRow Ctemp (4*i-1) (Vector.zero Ctemp.NumCols)
                        for i = m to j-1 do
                            let temp =                
                                (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                |> vector
                            Matrix.setRow Ctemp (4*i-4) (- temp)
                            Matrix.setRow Ctemp (4*i-3) (- (B.Row (i-1) |> vector))
                            Matrix.setRow Ctemp (4*i-2) (- (temp - (B.Row (i-1) |> vector)))
                            Matrix.setRow Ctemp (4*i-1) (- (temp - (B.Row (i) |> vector)))
                            if i = j - 1 then
                                Matrix.setRow Ctemp (4*i-4) (Vector.zero Ctemp.NumCols)
                                Matrix.setRow Ctemp (4*i-3) -(B.Row (i-1) |> vector)
                                Matrix.setRow Ctemp (4*i-2) (Vector.zero Ctemp.NumCols)
                                Matrix.setRow Ctemp (4*i-1) (Vector.zero Ctemp.NumCols)
                        for i = j to n-1 do
                            let temp = 
                                (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                |> vector
                            Matrix.setRow Ctemp (4*i-4) (temp)
                            Matrix.setRow Ctemp (4*i-3) ((B.Row (i-1) |> vector))
                            Matrix.setRow Ctemp (4*i-2) ((temp - (B.Row (i-1) |> vector)))
                            Matrix.setRow Ctemp (4*i-1) ((temp - (B.Row (i) |> vector)))
                        Matrix.setRow Ctemp (4*(n-1)) (B.Row (n-1) |> vector)
                        list <- (Matrix.copy Ctemp)::list
                list
                |> Array.ofList 

            
            else failwith "Condition max 3"

        ///calculates the weighted squared error of the spline functions and the observation values at the knots
        let getError (y:Vector<float>) (a:Vector<float>) (W:Matrix<float>) =
            let tmp =  W * (y - a)
            tmp |> Seq.averageBy (fun x -> x**2.) |> sqrt
        
        //datalength
        let n = x.Length

        //Define intervals stepwidth
        let h = Array.init (n-1) (fun i -> x.[i+1] - x.[i] )
        
        //generation of matrices D and H (Wood94)
        let H = Array2D.zeroCreate (n-2) n
        let D = Array2D.zeroCreate (n-2) (n-2)

        for i = 1 to (n-2) do
            H.[i-1,i-1] <-  1.0/h.[i-1]
            H.[i-1,i]   <- -( (1.0/h.[i-1]) + (1.0/h.[i]) )
            H.[i-1,i+1] <-  1.0/h.[i]
            D.[i-1,i-1] <-  (h.[i-1] + h.[i]) / 3.0

        for i = 1 to (n-3) do
            D.[i-1,i]   <- h.[i]/6.0
            D.[i,i-1] <- h.[i]/6.0

        //generation of matrices P, U and B (Wood94) --- Matrix P corrected!
        let P = Array2D.zeroCreate n n
        let U = Array2D.zeroCreate n n

        for i = 1 to n do
            P.[i-1,i-1] <- 2.0

        for i = 2 to (n-1) do
            P.[i-1,i-2] <- h.[i-1] / (h.[i-1] + h.[i-2])//uncorrected: h.[i-1] / (h.[i-1] + h.[i]) --> h.[i] not existent when i = n-1
            P.[i-1,i] <- 1.0 - P.[i-1,i-2]
            U.[i-1,i-2] <- -3.0 * (P.[i-1,i-2] / h.[i-2])
            U.[i-1,i] <- 3.0 * (P.[i-1,i] / h.[i-1])
            U.[i-1,i-1] <- -(U.[i-1,i] + U.[i-1,i-2])

        P.[0,1] <- 1.0
        U.[0,0] <- -3.0 / h.[0]
        U.[0,1] <- - U.[0,0]
        P.[n-1,n-2] <- 1.0
        U.[n-1,n-2] <- -3.0 / h.[n-2]
        U.[n-1,n-1] <- - U.[n-1,n-2]

        ///Calculation of B by P*B = U
        let PMat = Matrix.ofArray2D P
        let UMat = Matrix.ofArray2D U
        let B = Algebra.LinearAlgebra.SolveLinearSystems PMat UMat

        ///generation of the constraint matrix
        let ctList = getCtemP n B (Vector.ofArray h)
        
        ///the constraint-matrices for a given shape are transformed into 2D-Arrays (solver compatibility) and the operator defines the monotonicity direction
        let A = 
            ctList 
            |> Array.map (fun x -> 
                operator(x) 
                |> Matrix.toArray2D)
        
        let lamlist = [for i = 0 to 64 do yield 0.01*(1.2**(float i))]
        //let lamlist = [6.]//here attention

        A
        |> Array.map (fun aMat ->

            lamlist
            |> List.mapi (fun z lambd ->
                let Q = calcGlambda (Matrix.ofArray2D D) (Matrix.ofArray2D H) W n lambd //outsource (performance)
                let c = (calcclambda W n y lambd) |> RowVector.toArray                  //outsource (performance)
                //borders to solve the inequality constraints 
                let b = Array.create ((A.[0] |> Array2D.length1)) (-infinity,0.)
                //solver specific transformation (because symmetry of the matrix)
                let a' = 
                    let tmp = Matrix.create Q.NumRows Q.NumCols 1. |> setDiagonalInplace 0.5
                    let Q' = (Q .* tmp) |> Matrix.toArray2D
                    QP.minimize aMat b Q' c |> Vector.ofArray
                let e' = getError y a' W
                //let c' =         
                //    let tmpH = Matrix.ofArray2D H
                //    let tmpD = Matrix.ofArray2D D
                //    let tmp = 
                //        let tmp' = Algebra.LinearAlgebra.SolveLinearSystems tmpD tmpH //tested
                //        tmp' * a'                                                     //tested
                //    Vector.init (tmp.Length+2) (fun i -> if i>0 && i<=tmp.Length then tmp.[i-1] else 0.0)

                let (gcv,aic) = getGCV (aMat |> Matrix.ofArray2D) (Matrix.ofArray2D D) (Matrix.ofArray2D H) W y.Length y a' lambd
                createInnerResult a' gcv aMat e' aic lambd
                    )
            //minimize the gcv in respect to the used smoothing factor lambda
            |> List.minBy (fun innerResult -> innerResult.GCV)
            )
            //|> fun (a,e,gcvvar,c) -> 
            //    let gcv = gcvvar |> Array.map fst

            //    gcv 
            //    |> Array.indexed 
            //    |> Array.map (fun (i,x) -> sprintf "%i\t%f" i x)
            //    |> fun asd -> System.IO.File.WriteAllLines(@"C:\Users\bvenn\Documents\Projects\EIT\June\TestGCV\" + (string aind) + ".txt",asd)

            //    let var = gcvvar |> Array.map snd
            //    let gcvminIndex = gcv |> Array.findIndex (fun x -> x = (gcv |> Array.min))
            //    let varmin = var.[gcvminIndex]
            //    let gcvmin = gcv.[gcvminIndex]
            //    let efin   = e.[gcvminIndex]
            //    let afin   = a.[gcvminIndex]
            //    let cfin   = c.[gcvminIndex]
            //    let lambdafin = 0.01*(1.2**(float gcvminIndex))
            //    ((efin,gcvmin),(afin,varmin),lambdafin,cfin)
            //        )
        |> fun xt -> 
            //additional case, when no spline satisfies the given conditions (with additional constraints that checks extrema count)
            if xt = [||] then
                //for shape restriction if coefficient shrinkage is to intense (uncomment above)
                A,createTempClassResult (Vector.init n (fun _ -> 0.)) (Vector.init n (fun _ -> 0.)) infinity infinity infinity (Array2D.zeroCreate 1 1) infinity id
            else 
                xt
                //among all shape possibilities under a given parent shape ((min, then max);(max);...) minimize the mGCV to obtain the most promising spline candidate
                |> Array.minBy (fun innerResult -> innerResult.GCV)
                |> fun result -> 
                    let traceC = 
                        let tmpH = Matrix.ofArray2D H
                        let tmpD = Matrix.ofArray2D D
                        let tmp = 
                            let tmp' = Algebra.LinearAlgebra.SolveLinearSystems tmpD tmpH 
                            tmp' * result.TraceA                                          
                        Vector.init (tmp.Length+2) (fun i -> if i>0 && i<=tmp.Length then tmp.[i-1] else 0.0)
            
                    let aic = getAIC (float con) result.TraceA y W
                    let splineFunction = initEvalAt x result.TraceA traceC
                    A,createTempClassResult result.TraceA traceC result.Error result.GCV result.Lambda result.CTemp aic splineFunction
        //|> fun (x,afinvar,lambdafin,c) -> ((x |> Array.unzip),afinvar,lambdafin,c)
        //|> fun tmp -> 
        //    let e =         tmp |> fun (a,b,c,d) -> fst a
        //    let gcvmin =    tmp |> fun (a,b,c,d) -> snd a
        //    let afin =      tmp |> fun (a,b,c,d) -> b |> Array.map fst
        //    let varfin =    tmp |> fun (a,b,c,d) -> b |> Array.map snd
        //    let lambdafin = tmp |> fun (a,b,c,d) -> c
        //    let cfin =      tmp |> fun (a,b,c,d) -> d
        
        //    let eminIndex = e |> Array.findIndex (fun x -> x = (e |> Array.min))
        //    let efinal = e.[eminIndex]
        //    let afinal = afin.[eminIndex]
        //    let gcvfinal = gcvmin.[eminIndex]
        //    let lambdafinal = lambdafin.[eminIndex]
        //    let cfinal = cfin.[eminIndex]
        //    let varfinal = varfin.[eminIndex]
        //    createTempClassResultSimple afinal cfinal efinal gcvfinal lambdafinal (A.[eminIndex]) varfinal

    //calculate the constrained smoothing spline with given constraintmatrix and given lambda
    let splineManual ctemp (x:Vector<float>) (y:Vector<float>) (W:Matrix<float>) lambd =

        /// Sets the diagonal to value inplace
        let setDiagonalInplace value (m:Matrix<_>) =
            let min = min m.NumRows m.NumCols
            for i=0 to min-1 do
                m.[i,i] <- value
            m

        let calcGlambda  (D:Matrix<float>) (H:Matrix<float>) (W:Matrix<float>) (n:int) lambd = 
            let dInverse = Algebra.LinearAlgebra.Inverse D
            (2.0 * ( (H.Transpose * dInverse * H)   +  ((lambd / float n)) * (W.Transpose * W)    ) )           

        let calcclambda (W:Matrix<float>) (n:int) (y:Vector<float>) lambd =
            //-2.0 * ((float lambd) / (float n)) * (W.Transpose * W) * y
            -2.0 * ((float lambd) / (float n)) * y.Transpose * (W.Transpose * W) 


        let getError (y:Vector<float>) (a:Vector<float>) (W:Matrix<float>) =
            let tmp =  W * (y - a)
            tmp |> Seq.averageBy (fun x -> x**2.) |> sqrt

    
        let n = x.Length

        //define intervals stepwidth
        let h = Array.init (n-1) (fun i -> x.[i+1] - x.[i] )
        
        //generation of matrices D and H (Wood)
        let H = Array2D.zeroCreate (n-2) n
        let D = Array2D.zeroCreate (n-2) (n-2)

        for i = 1 to (n-2) do
            H.[i-1,i-1] <-  1.0/h.[i-1]
            H.[i-1,i]   <- -( (1.0/h.[i-1]) + (1.0/h.[i]) )
            H.[i-1,i+1] <-  1.0/h.[i]
            D.[i-1,i-1] <-  (h.[i-1] + h.[i]) / 3.0

        for i = 1 to (n-3) do
            D.[i-1,i]   <- h.[i]/6.0
            D.[i,i-1] <- h.[i]/6.0

        let A = [|ctemp|]
            
        A
        |> Array.map (fun aMat ->
            [lambd]
            |> List.mapi (fun z lambd ->
                let Q = calcGlambda (Matrix.ofArray2D D) (Matrix.ofArray2D H) W n lambd 
                let c = calcclambda W n y lambd |> RowVector.toArray
                let b = Array.create ((A.[0] |> Array2D.length1)) (-infinity,0.)
                let a' = 
                    let tmp = Matrix.create Q.NumRows Q.NumCols 1. |> setDiagonalInplace 0.5
                    let Q' = (Q .* tmp) |> Matrix.toArray2D
                    QP.minimize aMat b Q' c |> Vector.ofArray
                let e' = getError y a' W
                let c' =         
                    let tmpH = Matrix.ofArray2D H
                    let tmpD = Matrix.ofArray2D D
                    let tmp = 
                        let tmp' = Algebra.LinearAlgebra.SolveLinearSystems tmpD tmpH
                        tmp' * a'
                    Vector.init (tmp.Length+2) (fun i -> if i>0 && i<=tmp.Length then tmp.[i-1] else 0.0)
                let (mgcv,gcv) = getGCV (aMat |> Matrix.ofArray2D) (Matrix.ofArray2D D) (Matrix.ofArray2D H) W y.Length y a' lambd
                a',e',mgcv,c' 
                        )
            |> unzip4
            |> fun (a,e,gcv,c) -> 
                let gcvminIndex = gcv |> Array.findIndex (fun x -> x = (gcv |> Array.min))
                let gcvmin = gcv.[gcvminIndex]
                let efin = e.[gcvminIndex]
                let afin = a.[gcvminIndex]
                let cfin = c.[gcvminIndex]
                let lambdafin = lambd
                ((efin,gcvmin),afin,lambdafin,cfin)
                    )
        |> unzip4
        |> fun (x,afin,lambdafin,c) -> ((x |> Array.unzip),afin,lambdafin,c)
        |> fun tmp -> 
            let e = tmp |> fun (a,b,c,d) -> fst a
            let gcvmin = tmp |> fun (a,b,c,d) -> snd a
            let afin = tmp |> fun (a,b,c,d) -> b
            let lambdafin = tmp |> fun (a,b,c,d) -> c
            let cfin = tmp |> fun (a,b,c,d) -> d
        
            let eminIndex = e |> Array.findIndex (fun x -> x = (e |> Array.min))
            let efinal = e.[eminIndex]
            let afinal = afin.[eminIndex]
            let gcvfinal = gcvmin.[eminIndex]
            let lambdafinal = lambdafin.[eminIndex]
            let cfinal = cfin.[eminIndex]
            let splineFunction = initEvalAt x afinal cfinal
            createTempClassResult afinal cfinal gcvfinal efinal lambdafinal ctemp infinity splineFunction



    ///calculates a constrained initially increasing spline with given y-,and y-Values, a weighting matrix and the number of allowed extrema
    let splineIncreasing (x:Vector<float>) (y:Vector<float>) (W:Matrix<float>) con = 
        spline (~-) x y W con
    
    ///calculates a constrained initially decreasing spline with given y-,and y-Values, a weighting matrix and the number of allowed extrema
    let splineDecreasing (x:Vector<float>) (y:Vector<float>) (W:Matrix<float>) con = 
        spline (~+) x y W con

    ///returns the best fit of the observation x,-and y-values and a given weightingMatrix
    let getBestFitOfWeighting x_values y_values wMat= 
        let getinitialestimate = getInitialEstimateOfWXY wMat (vector y_values) x_values
        let initialSelectionCriterion = getinitialestimate.AICc
        
        //monotone splines
        let fst1 =(In0,splineIncreasing x_values (vector y_values) wMat 0) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result //(cl, createTempClassResultSimple x.TraceA x.TraceC x.Error (x.GCV*1.04) x.Lambda x.Ctemp x.AICc x.SplineFunction)
        let fst2 =(De0,splineDecreasing x_values (vector y_values) wMat 0) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result //(cl, createTempClassResultSimple x.TraceA x.TraceC x.Error (x.GCV*1.04) x.Lambda x.Ctemp x.AICc x.SplineFunction)
        let bestfst = [fst1;fst2] |> List.minBy (fun (cl,x) -> x.AICc)                                                                
        //splines with one extremum
        let snd1 =(In1,splineIncreasing x_values (vector y_values) wMat 1) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result //(cl, createTempClassResultSimple x.TraceA x.TraceC x.Error (x.GCV*1.06) x.Lambda x.Ctemp x.AICc x.SplineFunction)
        let snd2 =(De1,splineDecreasing x_values (vector y_values) wMat 1) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result //(cl, createTempClassResultSimple x.TraceA x.TraceC x.Error (x.GCV*1.06) x.Lambda x.Ctemp x.AICc x.SplineFunction)
        let bestsnd = [snd1;snd2] |> List.minBy (fun (cl,x) -> x.AICc)                                                               
        //splines with two extrema                                                                                                                                             
        let trd1 =(In2,splineIncreasing x_values (vector y_values) wMat 2) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result //(cl, createTempClassResultSimple x.TraceA x.TraceC x.Error (x.GCV*1.08) x.Lambda x.Ctemp x.AICc x.SplineFunction)
        let trd2 =(De2,splineDecreasing x_values (vector y_values) wMat 2) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result //(cl, createTempClassResultSimple x.TraceA x.TraceC x.Error (x.GCV*1.08) x.Lambda x.Ctemp x.AICc x.SplineFunction)
        let besttrd = [trd1;trd2] |> List.minBy (fun (cl,x) -> x.AICc)

        //selection of optimal shape possibility by model selection via AICc 
        [bestfst;bestsnd;besttrd]
        |> List.minBy (fun (cl,result) -> result.AICc)
        |> fun (cl,result) -> if result.AICc < initialSelectionCriterion then (cl,result) else Complex,result        

        

    ///gets indices of minima and maxima by brute force search
    let private investigateTrace pct (arr:float[]) =
        let almostEqual (pct:float) f1 f2 =
            let (min,max) = f2 - Math.Abs(f2 * pct),f2 + Math.Abs(f2 * pct)
            f1 >= min && f1 <= max
   
        let defGreaterThan (pct:float) f1 f2 =
            if f1 > 0. then 
                let max = f2 * (1.+pct)
                f1 > max
            else 
                let max = f2 * (1.-pct) 
                f1 > max

        let defLowerThan (pct:float) f1 f2 =
            if f1 > 0. then 
                let min = f2 * (1.-pct)  //2. * GradientDescent.eps
                f1 < min
            else 
                let min = f2 * (1.+pct) 
                f1 < min
        let length = arr.Length - 1
        let rec loop x accMin accMax=
            if x < length && x > 0 then
                let tmp = arr.[x]
                let prev = arr.[x-1] 
                let next = arr.[x+1]
                if not (almostEqual pct tmp prev) && not (almostEqual pct tmp prev) then 
                    let globalprevMax = arr.[0..x-1]|> Array.max |> defLowerThan pct tmp
                    let globalprevMin = arr.[0..x-1]|> Array.min |> defGreaterThan pct tmp 
                    let globalnextMax = arr.[x+1..] |> Array.max |> defLowerThan pct tmp 
                    let globalnextMin = arr.[x+1..] |> Array.min |> defGreaterThan pct tmp 
                    ////simple max
                    if defGreaterThan pct tmp prev && defGreaterThan pct tmp next then 
                        loop (x+1) accMin (x::accMax)
                    else
                        //simple min
                        if defLowerThan pct tmp prev && defLowerThan pct tmp next then 
                            loop (x+1) (x::accMin) accMax
                        else
                            //comp min
                            if defLowerThan pct tmp prev && almostEqual pct tmp next && globalnextMax && globalprevMax && not globalnextMin  then 
                                loop (x+1) (x::accMin) accMax
                            else
                            //comp max
                                if defGreaterThan pct tmp prev && almostEqual pct tmp next && globalnextMin && globalprevMin && not globalnextMax then 
                                    loop (x+1) accMin (x::accMax) 
                                else loop (x+1) accMin accMax
                else loop (x+1) accMin accMax
            else 
                accMin,accMax
        loop 1 [] []





    ///helper function for initWithLinearInterpol
    let private leftSegmentIdx arr value = 
        let idx = 
            let tmp = Array.BinarySearch(arr, value)
            let idx = if tmp < 0 then ~~~tmp-1 else tmp
            idx
        idx 

    ///geturns a function, that gives the corresponding spline function and approximates the areas outside of x by linear interpolation of the border knots and the next value of stepwidth difference
    let initEvalAtWithLinearInterpol (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) (stepwidth:float)=
        //stepwidth defines range in that interpolation is calculated (xVal.[0] + stepwidth) and (xVal.last - stepwidth)
        let n = x.Length

        //Definiere Intervalle
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )

        // Hilfsfunktionen F{i,1-4}(t) für SmoothSpline
        let calcF1 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            (x.[i+1] - t) / h.[i]

        let calcF2 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            (t - x.[i]) / h.[i]

        let calcF3 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            ((calcF1 h x i t)**3.0 - (calcF1 h x i t) ) * (h.[i]**2.0) / 6.0

        let calcF4 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            ((calcF2 h x i t)**3.0 - (calcF2 h x i t) ) * (h.[i]**2.0) / 6.0

        // Hilfsfunktion S{i} für SmoothSpline 
        let calcS (h:Vector<float>) (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) (i:int) (t:float) =
            a.[i] * (calcF1 h x i t) + a.[i+1] * (calcF2 h x i t) + c.[i] * (calcF3 h x i t) + c.[i+1] * (calcF4 h x i t)

        let evalAt =  calcS h x a c 

        let linearInterPolLowerBorder t =
            let halfDeltaLeftMostKnotAdjacentKnot = (x.InternalValues.[1] - x.InternalValues.[0]) / 2.
            let halfDeltaLeftMostKnotAdjacentKnot = stepwidth
            //let halfDeltaLeftMostKnotAdjacentKnot = x.InternalValues.[0] + stepwidth
            let yValHalfSegment = evalAt 0 (halfDeltaLeftMostKnotAdjacentKnot + x.InternalValues.[0])
            let yAtLeftMostKnot = evalAt 0 x.InternalValues.[0]
            let m = (yValHalfSegment - yAtLeftMostKnot ) / halfDeltaLeftMostKnotAdjacentKnot
            let b = yAtLeftMostKnot - m * x.InternalValues.[0]
            m * t + b

        let linearInterPolUpperBorder t =
            //let halfDeltaRightMostKnotAdjacentKnot = (x.InternalValues.[x.InternalValues.Length-1] - x.InternalValues.[x.InternalValues.Length-2]) / 2.
            let halfDeltaRightMostKnotAdjacentKnot = stepwidth
            let yValHalfSegment = evalAt (x.InternalValues.Length-2) (x.InternalValues.[x.InternalValues.Length-1] - halfDeltaRightMostKnotAdjacentKnot)
            let yAtRightMostKnot = evalAt (x.InternalValues.Length-2) x.InternalValues.[x.InternalValues.Length-1]
            let m = (yAtRightMostKnot - yValHalfSegment) / halfDeltaRightMostKnotAdjacentKnot
            let b = yAtRightMostKnot - (m * x.InternalValues.[x.InternalValues.Length-1])
            m * t + b
        
        (fun t ->
                match leftSegmentIdx x.InternalValues t with 
                | idx when idx < 0 -> 
                    linearInterPolLowerBorder t 
                | idx when idx > (x.InternalValues.Length-2) -> 
                    linearInterPolUpperBorder t
                | idx -> evalAt idx t 
           
            )    
