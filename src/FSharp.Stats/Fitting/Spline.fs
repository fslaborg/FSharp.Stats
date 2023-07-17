namespace FSharp.Stats.Fitting


module Spline =

    open FSharp.Stats
    open FSharp.Stats.Algebra

    /// Some preprocessing of the input data
    let internal preprocess (data : (float*float) []) =
        if Array.length data < 3 then failwith "Too little input points"
        data 
        |> Seq.sortBy fst
        |> Array.ofSeq
          
    let internal preprocessBasis (data : float []) =
       if Array.length data < 3 then failwith "Too little input points"
       data |> Array.distinct |> Array.sort 
 
    let internal checkSmoothingParameter l =
        if l < 0. then failwith "smoothing parameter should be positive"
    
    /// Creates a smoothing spline through some data. Takes as spline points the x-values given by basispts.
    /// The resulting function takes lambda (regularization parameter) and a x_Value as input. 
    let smoothingSpline (data: (float*float) []) (basispts: float []) =
        //https://robjhyndman.com/etc5410/splines.pdf
        // Some preprocessing
        let basistmp = preprocessBasis basispts
        let xdata,ydata = data |> preprocess |> Array.unzip
        let ydata = vector ydata
        let n = Array.length xdata
        let n' = Array.length basistmp
        let xm = basistmp.[n'-2]
        let xn = basistmp.[n'-1]
     
        // Construct the basis functions
        let basis : (float -> float) [] =
            let f x y =
                max 0. (pown (x-y) 3)
            [|  yield fun _ -> 1.
                yield id;
                for i in 0 .. n' - 3 do
                    let xi = basistmp.[i]
                    yield fun x -> (f x xi - f x xn)/(xn-xi) - (f x xm - f x xn)/(xn-xm)
                    |]
 
        // Construct the matrices we need
        let Bt = Matrix.init n' n (fun c r -> basis.[c] xdata.[r])
        let BtB = Bt * Bt.Transpose
        let penaltyFunc r c =
            let xi = xdata.[-2+min r c]
            let xj = xdata.[-2+max r c]
            -((6.*(xj - xm)*(xj*(xj + xm) - 2.*xm*xn + xi*(-3.*xj + xm + 2.*xn)))/((xi - xn)*(-xj + xn)))
        let Omega = Matrix.init n' n' (fun r c ->
            match min r c <= 1 with
            | true -> 0.
            | false -> penaltyFunc r c)
 
        // Create a function that creates a smoothing spline as a function of a smoothing factor. Avoids having to recompute all of the above when varying the smoothing parameter
        let n' = float n'
        fun (lambda: float) ->
            do checkSmoothingParameter lambda
            let beta = FSharp.Stats.Algebra.LinearAlgebra.LeastSquares (BtB + n'*lambda*Omega) (Bt * ydata)
            let helper = Array.zip basis (beta.ToArray())
            /// Our actualy smoothing spline
            fun x -> helper |> Array.sumBy  (fun (f,w) -> w * f x)
       