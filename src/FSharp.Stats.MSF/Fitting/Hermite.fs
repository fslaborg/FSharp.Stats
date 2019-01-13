namespace FSharp.Stats.Fitting
open System
open System.Collections.Generic

module Hermite =    

    open FSharp.Stats
    open FSharp.Stats.Optimization

    type HermiteResultSimple = {
        TraceA   : vector
        TraceC   : vector
        Error    : float
        GCV      : float
        Lambda   : float
        }
    
    let createHermiteResultSimple a c e gcv lambda= {
        TraceA   = a
        TraceC   = c
        Error    = e
        GCV      = gcv
        Lambda   = lambda
        }
    
    type HermiteResult = {
        TraceA   : vector
        TraceC   : vector
        Error    : float
        GCV      : float
        Lambda   : float
        Minimum  : int list
        Maximum  : int list
        TClass   : int
        }
    
    let createHermiteResult a c e gcv lambda mins maxs tclass= {
        TraceA   = a
        TraceC   = c
        Error    = e
        GCV      = gcv
        Lambda   = lambda
        Minimum  = mins
        Maximum  = maxs
        TClass   = tclass
        }
    
    let private unzip4Arr arr  =
        Array.foldBack (fun (a,b,c,d) (accA,accB,accC,accD) -> a::accA, b::accB, c::accC, d::accD) arr ([],[],[],[])
        |> fun (ra,rb,rc,rd) -> Array.ofList ra,Array.ofList rb,Array.ofList rc,Array.ofList rd
    
    let private unzip4List li =
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
    
    let private getDiag (m:Matrix<'a>)=
        let range = min m.NumRows m.NumCols
        vector [for i = 0 to range - 1 do yield m.[i,i]]
    
    let calcMeanOfRep (arr:Vector<float>) rep =
        if arr.Length%rep = 0 then
            let length = arr.Length / rep 
            vector [for i = 0 to length-1 do yield arr.[i*rep..i*rep+rep-1] |> Seq.average]
        
        else failwithf "arrLength no multiple of replicate number"
    
    let private addMatrix (a:Matrix<float>) (b:Matrix<float>) =   
        let rowNa = a.NumRows
        let rowNb = b.NumRows 
        let colNa = a.NumCols
        let colNb = b.NumCols
        let rowN = rowNa + rowNb
    
        if colNa <> colNb then 
            failwithf "Matrices differ in column length"
        else
            let conMatrix = Matrix.create rowN colNa 0.
    
            for i = 0 to rowNa - 1 do 
                for j = 0 to colNa - 1 do
                    conMatrix.[i,j] <- a.[i,j]
    
            for i = 0 to rowNb - 1 do 
                for j = 0 to colNb - 1 do
                    conMatrix.[i + rowNa,j] <- b.[i,j]
            conMatrix
    
    let private addRowVector (a:Matrix<float>) (b:RowVector<float>) =
    
        let rowNa = a.NumRows
        let colNa = a.NumCols
        let colNb = b.Length
        let rowN = rowNa + 1
    
        if colNa <> colNb then 
            failwithf "Matrix and vector differ in column length"
        else
            let conMatrix = Matrix.create rowN colNa 0.
    
            for i = 0 to rowNa - 1 do 
                for j = 0 to colNa - 1 do
                    conMatrix.[i,j] <- a.[i,j]
    
            for j = 0 to colNb - 1 do
                conMatrix.[rowN - 1,j] <- b.[j]
            conMatrix
                
    let normValues (yVal:Vector<float>) =
        let yMean = yVal |> Seq.mean
        let std   = yVal |> Seq.stDev
        yVal |> Vector.map (fun x -> (x - yMean) / std) 
    
            
    ///W_method: 0=identical weighting | 1=Variance //identical y_replicates lead to 1./0.
    let getWeighting (xVal:Vector<float>) (yVal:Vector<float>) (w_method:int) (numRep:int) =
        if w_method = 0 || numRep = 1 then 
            Matrix.diag(Vector.ones xVal.Length)
        else
            let Wtemp = Matrix.create xVal.Length xVal.Length 0.
            let diagVec = vector [for i = 0 to xVal.Length - 1 do yield 1. / (yVal.[(i*numRep)..(i*numRep + numRep - 1)] |> Seq.stDev)]
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
                
    let getInitialEstimate (D:Matrix<float>) (H:Matrix<float>) (W:Matrix<float>) (n) (y)= 
        let I = Matrix.identity n
        let Z = Matrix.identity n
        let calcGLambda lambd = 
            let dInverse = Algebra.LinearAlgebra.Inverse D
            (2.0 * ( (H.Transpose * dInverse * H)   +  ((lambd / float n)) * (W.Transpose * W)    ) )
            
        let A_tild (lambd:float)    = 
            let zproInverse = Algebra.LinearAlgebra.Inverse (Z.Transpose * (calcGLambda lambd) * Z)
            2. * (lambd / (float n)) * ( Z * zproInverse) * Z.Transpose * (W.Transpose * W)
    
        let a_star (lambd:float)= 
            (A_tild lambd) * y
            
        let V_tild (lambd:float) =     
            let aMat = (a_star lambd) |> Matrix.ofVector
            let yMat = y |> Matrix.ofVector
            let no = 
                (W * (aMat - yMat))
                |> fun m -> Matrix.getCol m 0
                |> Vector.norm
                |> fun x -> x*x
    
            let rho = 1.0
            let tr = I - (rho * (A_tild lambd)) |> getDiag |> Vector.sum
            (float n) * no / (tr * tr)
    
        let Var_tild lambd =
            let aMat = (a_star lambd) |> Matrix.ofVector
            let yMat = y |> Matrix.ofVector
            let no = 
                (W * (aMat - yMat))
                |> fun m -> Matrix.getCol m 0
                |> Vector.norm
                |> fun x -> x*x
            let rho = 1.0
            let tr = I - (rho * (A_tild lambd)) |> getDiag |> Vector.sum
            (float n) * no / (tr)  
    
        //NelderMeadSolver for global minimum
        let lambda_hat = NelderMead.minimizeSingleWith V_tild 3. 0.0001 2000.    
        
        let variance_unconstrained = Var_tild lambda_hat
        let GCV_unconstrained = V_tild lambda_hat
        let a_unconstrained = a_star lambda_hat
        let c' =         
            let tmpH = H
            let tmpD = D
            let tmp = 
                let tmp' = Algebra.LinearAlgebra.SolveLinearSystems tmpD tmpH
                tmp' * a_unconstrained
            Vector.init (tmp.Length+2) (fun i -> if i>0 && i<=tmp.Length then tmp.[i-1] else 0.0)
        variance_unconstrained,GCV_unconstrained,a_unconstrained,c'
    
    let getInitialEstimateOfWXY W (y:Vector<float>) (x:Vector<float>) =
        let n = y |> Seq.length
    
        let h = Array.init (n-1) (fun i -> x.[i+1] - x.[i] )
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
    
        getInitialEstimate (Matrix.ofArray2D D) (Matrix.ofArray2D H) W n y 
    

    ///works if MKL service is available
    let getGCV (C:Matrix<float>) (D:Matrix<float>) (H:Matrix<float>) (W:Matrix<float>) (n:int) (y:vector) (a:vector) (lambda:float) =
        let constr = C*(a |> Matrix.ofVector) 
        let tol = 0.001 
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

        let Z =
            if Ca.NumRows * Ca.NumCols = 0 then 
                Matrix.identity n            
            else
                let k = 
                    let (eps,U,V) =
                        Algebra.LinearAlgebra.SVD Ca
                    let rank = eps |> Seq.filter (fun x -> x >= 1e-5) |> Seq.length //Threshold if a singular value is considered as 0. //mat.NumRows - eps.Length
                    let count = V.NumRows - rank
                    Matrix.getRows V (rank) (count)
                    |> Matrix.transpose
                k
       
        let I = Matrix.identity n

        let G_lambda (lambd:float)  = 
            let dInverse = Algebra.LinearAlgebra.Inverse D
            2. * ( H.Transpose * dInverse * H + lambd / (float n)  * (W.Transpose * W))
    
        let A_tild (lambd:float)    = 
            let zproInverse = Algebra.LinearAlgebra.Inverse (Z.Transpose * (G_lambda lambd) * Z)
            2. * (lambd / (float n)) * ( Z * zproInverse) * Z.Transpose * (W.Transpose * W)

        let V_tild (lambd:float) =     
            let aMat = a |> Matrix.ofVector
            let yMat = y |> Matrix.ofVector
            let no = 
                (W * (aMat - yMat))
                |> fun m -> Matrix.getCol m  0
                |> Vector.norm
                |> fun x -> x*x
  
            let tr = I - (A_tild lambd) |> getDiag |> Vector.sum

            (float n) * no / (tr * tr)

        let var_ny (lambd:float) =
            let a = ((W * (a-y)) |>Vector.norm ) ** 2.
            let b = I - (A_tild lambd) |> getDiag |> Vector.sum
            a/b

        let gcv = V_tild lambda

        let variance = var_ny lambda
        (gcv,variance)

    let private spline operator (x:Vector<float>) (y:Vector<float>) (W:Matrix<float>) lambd con=

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
            -2.0 * ((float lambd) / (float n)) * (W.Transpose * W) * y


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
                    for j = (m+1) to (n-1) do
                        //printfn "con2:m=%i; j=%i" m j
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
                                Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector) //oder 0??
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
                                Matrix.setRow Ctemp (4*i-3) -(B.Row (i-1) |> vector) //0??
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
                //printfn "ctemp fertig"
                list
                |> Array.ofList 

            else failwith "Condition max 2"


        let getError (y:Vector<float>) (a:Vector<float>) (W:Matrix<float>) =
            let tmp =  W * (y - a)
            tmp |> Seq.averageBy (fun x -> x**2.) |> sqrt

    
        let n = x.Length

        //Define intervals    (4*(n-1)+1) n
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


        //generation of matrices P, U and B (Wood) --- Matrix P corrected!
        let P = Array2D.zeroCreate n n
        let U = Array2D.zeroCreate n n

        for i = 1 to n do
            P.[i-1,i-1] <- 2.0

        for i = 2 to (n-1) do
            P.[i-1,i-2] <- h.[i-1] / (h.[i-1] + h.[i-2])
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

        //Berechnung von B über P * B = U
        let P' = Matrix.ofArray2D P
        let U' = Matrix.ofArray2D U
        let B = Algebra.LinearAlgebra.SolveLinearSystems P' U' 
        
        let ctList = getCtemP n B (Vector.ofArray h)        
        let A = ctList |> Array.map (fun x -> operator(x) |> Matrix.toArray2D)
        let lamlist = [for i = 0 to 64 do yield 0.01*(1.2**(float i))]

        A
        |> Array.mapi (fun aind aMat ->
            lamlist
            |> List.mapi (fun z lambd ->
                let Q = calcGlambda (Matrix.ofArray2D D) (Matrix.ofArray2D H) W n lambd 
                let c = calcclambda W n y lambd |> Vector.toArray
                let b = Array.create ((A.[0] |> Array2D.length1)) (-infinity,0.)
                let a' = 
                    let tmp = Matrix.create Q.NumRows Q.NumCols 1. |> setDiagonalInplace 0.5
                    let Q' = (Q .* tmp) |> Matrix.toArray2D
                    QP.minimize aMat b Q' c |> Vector.ofArray
                let b' = B*a'
                let e' = getError y a' W
                let c' =         
                    let tmpH = Matrix.ofArray2D H
                    let tmpD = Matrix.ofArray2D D
                    let tmp = 
                        let tmp' = Algebra.LinearAlgebra.SolveLinearSystems tmpD tmpH
                        tmp' * a'
                    Vector.init (tmp.Length+2) (fun i -> if i>0 && i<=tmp.Length then tmp.[i-1] else 0.0)
                let (gcv,var) = getGCV (aMat |> Matrix.ofArray2D) (Matrix.ofArray2D D) (Matrix.ofArray2D H) W y.Length y a' lambd
                a',e',gcv,c' 
                        )
            |> unzip4List
            |> fun (a,e,gcv,c) -> 
                let gcvminIndex = gcv |> Array.findIndex (fun x -> x = (gcv |> Array.min))
                let gcvmin = gcv.[gcvminIndex]
                let efin = e.[gcvminIndex]
                let afin = a.[gcvminIndex]
                let cfin = c.[gcvminIndex]
                let lambdafin = 0.01*(1.2**(float gcvminIndex))
                ((efin,gcvmin),afin,lambdafin,cfin)
                    )
        |> unzip4List
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
            createHermiteResultSimple afinal cfinal gcvfinal efinal lambdafinal



    let splineIncreasing (x:Vector<float>) (y:Vector<float>) (W:Matrix<float>) lambd con= 
        spline (~-) x y W lambd con

    let splineDecreasing (x:Vector<float>) (y:Vector<float>) (W:Matrix<float>) lambd con= 
        spline (~+) x y W lambd con
        

    let getBestFitOfWeighting arr wMat xVal=
        let getinitialestimate = getInitialEstimateOfWXY wMat (vector arr) xVal 
        let initial = getinitialestimate |> fun (a,b,c,d) -> b
        
        let fst1 =(1,splineIncreasing xVal (vector arr) wMat 1. 0 )  |> fun (cl,x) -> (cl, createHermiteResultSimple x.TraceA x.TraceC x.Error (x.GCV*1.04) x.Lambda)
        let fst2 =(-1,splineDecreasing xVal (vector arr) wMat 1. 0)  |> fun (cl,x) -> (cl, createHermiteResultSimple x.TraceA x.TraceC x.Error (x.GCV*1.04) x.Lambda)
        let bestfst = [fst1;fst2] |> List.minBy (fun (cl,x) -> x.Error)
        
        let snd1 =(2,splineIncreasing xVal (vector arr) wMat 1. 1; ) |> fun (cl,x) -> (cl, createHermiteResultSimple x.TraceA x.TraceC x.Error (x.GCV*1.06) x.Lambda)
        let snd2 =( 2,splineDecreasing xVal (vector arr) wMat 1. 1;) |> fun (cl,x) -> (cl, createHermiteResultSimple x.TraceA x.TraceC x.Error (x.GCV*1.06) x.Lambda)
        let bestsnd = [snd1;snd2] |> List.minBy (fun (cl,x) -> x.Error)
        
        let trd1 =(3,splineIncreasing xVal (vector arr) wMat 1. 2; ) |> fun (cl,x) -> (cl, createHermiteResultSimple x.TraceA x.TraceC x.Error (x.GCV*1.08) x.Lambda)
        let trd2 =(-3,splineDecreasing xVal (vector arr) wMat 1. 2;) |> fun (cl,x) -> (cl, createHermiteResultSimple x.TraceA x.TraceC x.Error (x.GCV*1.08) x.Lambda)
        let besttrd = [trd1;trd2] |> List.minBy (fun (cl,x) -> x.Error)
        
        [bestfst;bestsnd;besttrd]
        |> List.minBy (fun (cl,x) -> x.GCV)
        |> fun (cl,x) -> if x.GCV < initial then (cl,x) else 0,x

    ///gets indices of minima and maxima
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


    ///weightinmethod 0:identical; 1:variance
    let getBestFit xVal yVal repNumber weightingMethod = 
        let xValMeans = calcMeanOfRep yVal repNumber
        let weightingMatrix = (getWeighting xVal (yVal |> vector) weightingMethod repNumber)
        let (cl,fit) = getBestFitOfWeighting xValMeans weightingMatrix xVal
        let (mins,maxs) = investigateTrace 0.001 (fit.TraceA |> Seq.toArray)
        let clNew = 
            if mins = [] then
                if maxs = [] then 
                    if fit.TraceA.[0] < fit.TraceA.[fit.TraceA.Length - 1] then 1
                    else -1
                else 2
            else  
                if maxs = [] then -2
                else 
                    if maxs.[0] < mins.[0] then 3
                    else -3

        createHermiteResult fit.TraceA fit.TraceC fit.Error fit.GCV fit.Lambda mins maxs clNew

        

    let initEvalAt (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =

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
    
        (fun t ->
            let i = 
                match Array.tryFindIndexBack (fun xs -> xs <= t) (x.InternalValues) with 
                | Some x -> x 
                | None   -> failwith "The x-value is not part of the section used to estimate the spline coefficients, thus a monotone function progression can not be guaranteed"
            evalAt i t
            )
    ///
    let private leftSegmentIdx arr value = 
        let idx = 
            let tmp = Array.BinarySearch(arr, value)
            let idx = if tmp < 0 then ~~~tmp-1 else tmp
            idx
        idx 

    ///
    let initEvalAtWithLinearInterpol (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =

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
            let yValHalfSegment = evalAt 0 (halfDeltaLeftMostKnotAdjacentKnot + x.InternalValues.[0])
            let yAtLeftMostKnot = evalAt 0 x.InternalValues.[0]
            let m = (yValHalfSegment - yAtLeftMostKnot ) / halfDeltaLeftMostKnotAdjacentKnot
            let b = yAtLeftMostKnot - m * x.InternalValues.[0]
            m * t + b

        let linearInterPolUpperBorder t =
            let halfDeltaRightMostKnotAdjacentKnot = (x.InternalValues.[x.InternalValues.Length-1] - x.InternalValues.[x.InternalValues.Length-2]) / 2.
            let yValHalfSegment = evalAt (x.InternalValues.Length-2) (x.InternalValues.[x.InternalValues.Length-2] + halfDeltaRightMostKnotAdjacentKnot)
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
