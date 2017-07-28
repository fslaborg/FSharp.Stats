namespace FSharp.Stats.Fitting
open System
open System.Collections.Generic

module Hermite =    

    open FSharp.Stats
    open FSharp.Stats.Optimization


    let private spline operator (x:Vector<float>) (y:Vector<float>) (W:Matrix<float>) lambd =

        /// Sets the diagonal to value inplace
        let setDiagonalInplace value (m:Matrix<_>) =
            let min = min m.NumRows m.NumCols
            for i=0 to min-1 do
                m.[i,i] <- value
            m

        let calcGlambda  (D:Matrix<float>) (H:Matrix<float>) (W:Matrix<float>) (n:int) lambd = 
            let dInverse = Algebra.LinearAlgebra.Inverse D
            (2.0 * ( (H.Transpose * dInverse * H)   +  ((lambd / float n)) * (W.Transpose * W)    ) )
            //2. * ( H.TransposeThisAndMultiply(D.Inverse()).TransposeThisAndMultiply(H) + (lambd / float n ) * W.TransposeThisAndMultiply(W) )


        let calcclambda (W:Matrix<float>) (n:int) (y:Vector<float>) lambd =
            -2.0 * ((float lambd) / (float n)) * (W.Transpose * W) * y


        let getCtemP n (B:Matrix<float>) (h:Vector<float>) =
            let Ctemp = Matrix.zero (4*(n-1)+1) n
            //Constraint für monoton steigend
            for i=1 to (n-1) do
                for j=1 to n do
                    if (j<i) then
                        Ctemp.[(4*i - 3) - 1 , j - 1] <- 0.0
                        Ctemp.[(4*i - 1) - 1 , j - 1] <- 0.0 - B.[i-1 , j-1]
                        Ctemp.[(4*i - 0) - 1 , j - 1] <- 0.0 - B.[i , j-1]
                    elif (j=i) then
                        Ctemp.[(4*i - 3) - 1 , j - 1] <- (-3.0 / h.[i-1])
                        Ctemp.[(4*i - 1) - 1 , j - 1] <- (-3.0 / h.[i-1]) - B.[i-1 , j-1]
                        Ctemp.[(4*i - 0) - 1 , j - 1] <- (-3.0 / h.[i-1]) - B.[i , j-1]
                    elif (j=i+1) then
                        Ctemp.[(4*i - 3) - 1 , j - 1] <- (+3.0 / h.[i-1])
                        Ctemp.[(4*i - 1) - 1 , j - 1] <- (+3.0 / h.[i-1]) - B.[i-1 , j-1]
                        Ctemp.[(4*i - 0) - 1 , j - 1] <- (+3.0 / h.[i-1]) - B.[i , j-1]
                    else
                        Ctemp.[(4*i - 3) - 1 , j - 1] <- 0.0
                        Ctemp.[(4*i - 1) - 1 , j - 1] <- 0.0 - B.[i-1 , j-1]
                        Ctemp.[(4*i - 0) - 1 , j - 1] <- 0.0 - B.[i , j-1]
                    Ctemp.[(4*i - 2) - 1 , j - 1] <- B.[i-1 , j-1]
            for j in 1 .. n do
                 Ctemp.[(4*(n-1)) + 1 - 1 , j - 1] <- B.[n-1 , j-1]
        
            Ctemp


        let getError (y:Vector<float>) (a:Vector<float>) (W:Matrix<float>) =
            let tmp =  W * (y - a)
            tmp |> Seq.averageBy (fun x -> x**2.) |> sqrt



        //++++++++++++
    
        let n = x.Length

        //Definiere Intervalle    (4*(n-1)+1) n
        let h = Array.init (n-1) (fun i -> x.[i+1] - x.[i] )

        //Matrizen D, H
        //Bestimmung der Matrizen D und H nach Wood
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


        //Matrizen D, H
        //Bestimmung der Matrizen P, U und B nach Wood --- Matrix P korrigiert!
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
  
        //A = Ctemp (+Ctemp für aufsteigend / -Ctem für absteigend)
        //let A = -(getCtemP n B (Vector.ofArray h)) |> Matrix.toArray2D
        let A = operator(getCtemP n B (Vector.ofArray h)) |> Matrix.toArray2D
        let Q = calcGlambda (Matrix.ofArray2D D) (Matrix.ofArray2D H) W n lambd 
        let c = calcclambda W n y lambd |> Vector.toArray

        let b = Array.create n (-infinity,0.) // Reconsider

        let a' = 
            let tmp = Matrix.create Q.NumRows Q.NumCols 1. |> setDiagonalInplace 0.5
            let Q' = (Q .* tmp) |> Matrix.toArray2D
            QP.minimize A b Q' c |> Vector.ofArray
        
        let b' = B * a'
        let e' = getError y a' W
        let c' =         
            let tmpH = Matrix.ofArray2D H
            let tmpD = Matrix.ofArray2D D
            let tmp = (Algebra.LinearAlgebra.SolveLinearSystems tmpD tmpH) * a'
            
            // padding zero
            Vector.init (tmp.Length+2) (fun i -> if i>0 && i<=tmp.Length then tmp.[i-1] else 0.0) 

        (a',e',b',c')



    let splineIncreasing (x:Vector<float>) (y:Vector<float>) (W:Matrix<float>) lambd = 
        spline (~-) x y W lambd


    let splineDecreasing (x:Vector<float>) (y:Vector<float>) (W:Matrix<float>) lambd = 
        spline (~+) x y W lambd


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
