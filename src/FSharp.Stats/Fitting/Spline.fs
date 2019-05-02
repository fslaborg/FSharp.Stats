namespace FSharp.Stats.Fitting


module Spline =

    open FSharp.Stats
    open FSharp.Stats.Algebra
    open FSharp.Stats.Algebra.LinearAlgebra

    /// Some preprocessing of the input data
    let private preprocess (data : (float*float) []) =
        if Array.length data < 3 then failwith "Too little input points"
        data 
        |> Seq.sortBy fst
        |> Seq.distinctBy fst
        |> Array.ofSeq
          
    let private preprocessBasis (data : float []) =
       if Array.length data < 3 then failwith "Too little input points"
       data |> Seq.sort |> Seq.distinct |> Array.ofSeq
 
    let private checkSmoothingParameter l =
        if l < 0. then failwith "smoothing parameter should be positive"
    /// Creates a smoothing spline through some data. Takes as spline points the x-values given by basispts
    let smoothingSpline (data: (float*float) []) (basispts : float [])=
 
        // Some preprocessing
        let xdata,ydata = data |> preprocess |> Array.unzip
        let ydata = vector ydata
        let n = Array.length xdata
        let n' = Array.length basispts
        let xm = basispts.[n'-2]
        let xn = basispts.[n'-1]
     
        // Construct the basis functions
        let basis : (float -> float) [] =
            let f x y =
                max 0. (pown (x-y) 3)
            [|  yield fun _ -> 1.
                yield id;
                for i in 0 .. n' - 3 do
                    let xi = basispts.[i]
                    yield fun x -> (f x xi - f x xn)/(xn-xi) - (f x xm - f x xn)/(xn-xm)
                    |]
 
        // Construct the matrices we need
        let Bt = Matrix.init n' n (fun c r -> basis.[c] xdata.[r])
        let BtB = Bt * Bt.Transpose
        let penaltyFunc r c =
            let xi = xdata.[-2+min r c]
            let xj = xdata.[-2+max r c]
            -((6.*(xj - xm)*(xj*(xj + xm) - 2.*xm*xn + xi*(-3.*xj + xm + 2.*xn)))/((xi - xn)*(-xj + xn)))
        let det = LinearAlgebra.Determinant BtB
        if det = 0. then failwith "Can't deal with input data for some reason"
        let Omega = Matrix.init n' n' (fun r c ->
            match min r c <= 1 with
            | true -> 0.
            | false -> penaltyFunc r c)
 
        // Create a function that creates a smoothing spline as a function of a smoothing factor. Avoids having to recompute all of the above when varying the smoothing parameter
        let n' = float n'
        fun (lambda: float) ->
            do checkSmoothingParameter lambda
            let theta = LinearAlgebra.Inverse (BtB + n'*lambda*Omega)
            let theta = theta * Bt * ydata
            let helper = Array.zip basis (theta.ToArray())
            /// Our actualy smoothing spline
            fun x -> helper |> Array.sumBy  (fun (f,w) -> w * f x)
    
    module Interpolation =
        
        type BoundaryCondition =
            | Natural
            | Periodic
            | Parabolic
            | NotAKnot
            //| Quadratic
            //| Clamped
              
        ///computes all coefficients for piecewise interpolating splines. In the form of [a0;b0;c0;d0;a1;b1;...;d(n-2)]. 
        ///fn(x) = (an)x³+(bn)x²+(cn)x+(dn)
        let coefficients (boundaryCondition: BoundaryCondition) (x_Values: Vector<float>) (y_Values: Vector<float>) =
            //f(x)   = ax³+bx²+cx+d
            //f'(x)  = 3ax²+2bx+c
            //f''(x) = 6ax+2b

            let intervalNumber = x_Values.Length - 1

            let interpolatingConstraints intervalIndex (x:float) (y:float) =
                let tmp = Array.init (4 * intervalNumber) (fun x -> 0.)
                tmp.[4 * intervalIndex + 0] <- pown x 3  
                tmp.[4 * intervalIndex + 1] <- pown x 2 
                tmp.[4 * intervalIndex + 2] <- x
                tmp.[4 * intervalIndex + 3] <- 1.
                (tmp,y)

            let firstDerivativeConstraints intervalIndex x =
                let tmp = Array.init (4 * intervalNumber) (fun x -> 0.)
                let f'a = 3. * (pown x 2)
                let f'b = 2. * x
                let f'c = 1.
                let f'd = 0.
                tmp.[4 * intervalIndex + 0] <- f'a
                tmp.[4 * intervalIndex + 1] <- f'b
                tmp.[4 * intervalIndex + 2] <- f'c
                //tmp.[4 * intervalIndex + 3] <- f'd
                tmp.[4 * intervalIndex + 4] <- - f'a
                tmp.[4 * intervalIndex + 5] <- - f'b
                tmp.[4 * intervalIndex + 6] <- - f'c
                //tmp.[4 * intervalIndex + 7] <- - f'd
                (tmp,0.)

            let secondDerivativeConstraints intervalIndex x =
                let tmp = Array.init (4 * intervalNumber) (fun x -> 0.)
                let f''a = 6. * x
                let f''b = 2.
                let f''c = 0.
                let f''d = 0.
                tmp.[4 * intervalIndex + 0] <- f''a
                tmp.[4 * intervalIndex + 1] <- f''b
                //tmp.[4 * intervalIndex + 2] <- f''c
                //tmp.[4 * intervalIndex + 3] <- f''d
                tmp.[4 * intervalIndex + 4] <- - f''a
                tmp.[4 * intervalIndex + 5] <- - f''b
                //tmp.[4 * intervalIndex + 6] <- - f''c
                //tmp.[4 * intervalIndex + 7] <- - f''d
                (tmp,0.)
            
            let boundaryCondition = 
                let firstX = x_Values.[0]
                let secondX = x_Values.[1]
                let lastX = x_Values.[intervalNumber]
                let penultimate = x_Values.[intervalNumber - 1]
                let tmp1 = Array.init (4 * intervalNumber) (fun x -> 0.)
                let tmp2 = Array.init (4 * intervalNumber) (fun x -> 0.)
                match boundaryCondition with
                | Natural ->
                    //first condition: f''0(x0) = 0
                    tmp1.[0] <- 6. * firstX
                    tmp1.[1] <- 2.
                    //tmp.[2] <- 0.
                    //tmp.[3] <- 0.

                    //second condition: f''n-1(xn) = 0
                    tmp2.[4 * (intervalNumber - 1) + 0] <- 6. * lastX
                    tmp2.[4 * (intervalNumber - 1) + 1] <- 2.
                    //tmp2.[4 * (intervalNumber - 1) + 2] <- 0.
                    //tmp2.[4 * (intervalNumber - 1) + 3] <- 0.

                    [|(tmp1,0.);(tmp2,0.)|]

                | Periodic ->
                    //first conditionf'0(x0)-f'n-1(xn) = 0
                    tmp1.[0] <- 3. * (pown firstX 2)
                    tmp1.[1] <- 2. * firstX
                    tmp1.[2] <- 1.
                    tmp1.[4 * (intervalNumber - 1) + 0] <- -3. * (pown lastX 2)
                    tmp1.[4 * (intervalNumber - 1) + 1] <- -2. * lastX
                    tmp1.[4 * (intervalNumber - 1) + 2] <- -1.

                    //second condition: f''0(x0)-f''n-1(xn) = 0
                    tmp2.[0] <- 6. * firstX
                    tmp2.[1] <- 2. 
                    tmp2.[4 * (intervalNumber - 1) + 0] <- -6. * lastX
                    tmp2.[4 * (intervalNumber - 1) + 1] <- -2. 

                    [|(tmp1,0.);(tmp2,0.)|]

                | Parabolic -> 
                    //first condition: f''0(x0) - f''0(x1) = 0
                    tmp1.[0] <- 6. * firstX
                    tmp1.[1] <- 2.
                    tmp1.[4] <- -6. * secondX
                    tmp1.[5] <- -2.
                
                    //second condition: f''n-1(x(n-1)) - f''n-1(xn) = 0
                    tmp2.[4 * (intervalNumber - 1) + 0] <- 6. * lastX
                    tmp2.[4 * (intervalNumber - 1) + 1] <- 2. 
                    tmp2.[4 * (intervalNumber - 2) + 0] <- -6. * penultimate
                    tmp2.[4 * (intervalNumber - 2) + 1] <- -2. 
                
                    [|(tmp1,0.);(tmp2,0.)|]
                
                | NotAKnot ->
                    //first condition: f'''0(x1) - f'''1(x1) = 0
                    tmp1.[0] <- 1.
                    tmp1.[4] <- -1.
                
                    //second condition: f'''n-1(x(n-1)) - f'''n-2(x(n-1)) = 0
                    tmp2.[4 * (intervalNumber - 1) + 0] <- 1.
                    tmp2.[4 * (intervalNumber - 2) + 0] <- -1.
                
                    [|(tmp1,0.);(tmp2,0.)|]



            let (equations,solutions) =
                let interPolConstraints =
                    [|0 .. intervalNumber - 1|]
                    |> Array.map (fun i -> 
                        [|
                        interpolatingConstraints i x_Values.[i] y_Values.[i]
                        interpolatingConstraints i x_Values.[i+1] y_Values.[i+1]
                        |])
                        |> Array.concat

                let derivativeConstraints =
                    [|0 .. intervalNumber - 2|]
                    |> Array.map (fun i -> 
                        [|
                        firstDerivativeConstraints  i x_Values.[i+1]
                        secondDerivativeConstraints i x_Values.[i+1]
                        |])
                    |> Array.concat
                
                [|interPolConstraints;derivativeConstraints;boundaryCondition|]
                |> Array.concat
                |> Array.unzip
                
            let A = Matrix.ofArray equations
            let b = Vector.ofArray solutions

            Algebra.LinearAlgebra.SolveLinearSystem A b 

        let fit (coefficients: Vector<float>) (x_Values: Vector<float>) x =
            let intervalNumber =
                
                if x > Seq.last x_Values || x < x_Values.[0] then 
                    failwith "Spline is not defined outside of the interval [x_Values.[0],x_Values.[n-1]]"
                
                if x = Seq.last x_Values then 
                    Vector.length x_Values - 2
                else
                    x_Values
                    |> Seq.findIndex(fun x_Knot -> (x_Knot - x) > 0.)
                    |> fun nextInterval -> nextInterval - 1
            
            let y_Value = 
                coefficients.[4 * intervalNumber + 0] * (pown x 3) +    //a*x³
                coefficients.[4 * intervalNumber + 1] * (pown x 2) +    //b*x²
                coefficients.[4 * intervalNumber + 2] * x          +    //c*x
                coefficients.[4 * intervalNumber + 3]                   //d
            
            y_Value

            


                    
                

            