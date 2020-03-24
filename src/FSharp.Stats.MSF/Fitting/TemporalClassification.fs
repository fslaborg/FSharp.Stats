namespace FSharp.Stats.Fitting
open System
open System.Collections.Generic

(*
1. Do not use PSeq for parallelization (use Async instead)
2. add:
    FSharp.Stats.ServiceLocator.setEnvironmentPathVariable @"..\FSharp.Stats\lib"
    FSharp.Stats.Algebra.LinearAlgebra.Service()
*)

module TemporalClassification =    
//module Hermite =    

    open FSharp.Stats
    open FSharp.Stats.Optimization

    let roundToNext (xVal:float) (xVals:seq<float>) =
        xVals 
        |> Seq.minBy (fun xI -> Math.Abs (xI - xVal))

    /// gives smoothing spline function
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

        //function: a * ((y - t) / h) + b * (t - x) / h + c * ((((y - t) / h)^3.0 - ((y - t) / h) ) * (h^2.0) / 6.0  )+ d * ((((t - x) / h)^3.0 - ((t - x) / h) ) * (h^2.0) / 6.0) where b = a.[i+1] ...
        let evalAt =  calcS h x a c 
        
        (fun t ->
            let i = 
                match Array.tryFindIndexBack (fun xs -> xs <= t) (x.InternalValues) with 
                | Some x -> x 
                | None   -> failwith "The x-value is not part of the section used to estimate the spline coefficients, thus a monotone function progression can not be guaranteed"
            evalAt i t
            )

    /// fst derivative at amplitudes a and curvatures c with x values x
    let initEvalFstDerivativeAt (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =

        let n = x.Length

        //define interval stepwidths
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )

        let calcF1 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            (pown h.[i] 2) * (3. * (pown (t-x.[i]) 2) / (pown h.[i] 3) - (1. / h.[i]))
            |> fun x -> x / 6.

        let calcF2 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            (pown h.[i] 2) * ((1./h.[i]) - ((3. * (pown (x.[i+1] - t) 2)/pown h.[i] 3))) 
            |> fun x -> x / 6.

        let calcF3 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            1. / h.[i]

        let calcF4 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            1. / h.[i]

        // helper function for fst derivative
        let calcS (h:Vector<float>) (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) (i:int) (t:float) =
            c.[i+1] * (calcF1 h x i t) + c.[i] * (calcF2 h x i t) + a.[i+1] * (calcF3 h x i t) - a.[i] * (calcF4 h x i t)

        let evalAt =  calcS h x a c 
    
        (fun t ->
            let i = 
                match Array.tryFindIndexBack (fun xs -> xs <= t) (x.InternalValues) with 
                | Some x -> x 
                | None   -> failwith "The x-value is not part of the section used to estimate the spline coefficients, thus a monotone function progression can not be guaranteed"
            evalAt i t
            )
    
    type Extremum = {
        ///1:Maximum; -1:Minimum
        Indicator : int
        ///xValue of extremum
        Xvalue    : float
        }
    let createExtremum indicator xValue = {Indicator = indicator; Xvalue = xValue}

    /// calculates extrema present in the given smoothing spline
    let getExtrema (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =
        let n = x.Length 
        //define interval stepwidths
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )  
        // helper function for identification of roots in first derivative
        let calcS (h:Vector<float>) (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) (i:int) (t:float) =      
            let f1 = (c.[i] * x.[i+1] - c.[i+1] * x.[i]) / (c.[i] - c.[i+1])
            let f2 = sqrt 2. / (c.[i] - c.[i+1])
            let root = 
                let s1 = 0.5*c.[i]**2.*x.[i+1]**2.
                let s2 = -c.[i]*c.[i+1]*x.[i+1]*x.[i]
                let s3 = -1./6. * c.[i] * (6.*a.[i]-6.*a.[i+1]-c.[i]*h.[i]**2. + 3. *c.[i]*x.[i+1]**2. + c.[i+1]*h.[i]**2. - 3.* c.[i+1]*x.[i]**2.)
                let s4 = 0.5 * c.[i+1]**2. * x.[i]**2.
                let s5 = 1./6. * c.[i+1] * (6.*a.[i]-6.*a.[i+1]-c.[i]*h.[i]**2. + 3. *c.[i]*x.[i+1]**2. + c.[i+1]*h.[i]**2. - 3.* c.[i+1]*x.[i]**2.)
                (s1 + s2 + s3 + s4 + s5)
            if root < 0. then 
                []
            else 
                [
                    f1 - f2 * sqrt root
                    f1 + f2 * sqrt root
                ]
    
        let evalAt = calcS h x a c 
        
        // calculate amplitude of second derivative in interval i at xValue t
        let calcSndDeriv (i:int) (t:float) =
            c.[i+1] * (t - x.[i]) / h.[i] + c.[i] * (x.[i+1] - t) / h.[i]
        
        let sigBigger a b = a > (b+0.0000001)
        
        let splineFunction = initEvalAt x a c

        x.[0.. Seq.length x - 2]
        |> Seq.map 
                (fun t ->
                    let i = 
                        match Array.tryFindIndexBack (fun xs -> xs <= t) (x.InternalValues) with 
                        | Some x -> x 
                        | None   -> failwith "The x-value is not part of the section used to estimate the spline coefficients, thus a monotone function progression can not be guaranteed"
                    //get an open interval of the current signal interval
                    let interval = Intervals.create (x.[i]-0.0000001) (x.[i+1]+0.0000001)
                    let secondDerivative t = calcSndDeriv i t
                    //calculate roots of fst derivative and check snd derivative
                    let extrema = 
                        evalAt i t
                        |> List.filter (fun x -> Intervals.liesInInterval x interval)
                        |> List.choose (fun xAtS0 -> 
                            let s1 = secondDerivative xAtS0
                            match s1 with
                                | w when round 5 s1 = 0. -> 
                                    if round 1 (xAtS0-0.05) <= Seq.head x  || round 1 (xAtS0+0.05) >= Seq.last x then 
                                        None
                                    else 
                                        let yAtX = splineFunction xAtS0
                                        let yAtpX = splineFunction (xAtS0 - 0.05)
                                        let yAtaX = splineFunction (xAtS0 + 0.05)
                                        if   sigBigger yAtX yAtpX && sigBigger yAtX yAtaX then Some (createExtremum  1 xAtS0) //(1.,xAtS0)
                                        elif sigBigger yAtpX yAtX && sigBigger yAtaX yAtX then Some (createExtremum -1 xAtS0) //(-1.,xAtS0)
                                        else None//real saddle point
                                | w when s1 < 0. -> Some (createExtremum 1 xAtS0) //(1.,xAtS0)
                                | _ -> Some (createExtremum -1 xAtS0)//(-1.,xAtS0)
                            )
                    extrema
                    //|> List.filter (fun (i,xv) -> i <> 0.)
                    )
                |> List.concat
                |> List.sortBy (fun ex -> ex.Xvalue)
                |> List.map (fun ex -> {ex with Xvalue = round 2 (roundToNext ex.Xvalue x)})
                |> List.distinct

    /// second derivative at amplitudes a and curvatures c with x values x
    let initEvalSndDerivativeAt (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =

        let n = x.Length

        //define interval stepwidths
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )

        let calcF1 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            (t - x.[i]) / h.[i]

        let calcF2 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
            (x.[i+1] - t) / h.[i]

        // helper function s(i) for smoothing spline 
        let calcS (h:Vector<float>) (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) (i:int) (t:float) =
            c.[i+1] * (calcF1 h x i t) + c.[i] * (calcF2 h x i t)

        let evalAt =  calcS h x a c 

        (fun t ->
            let i = 
                match Array.tryFindIndexBack (fun xs -> xs <= t) (x.InternalValues) with 
                | Some x -> x 
                | None   -> failwith "The x-value is not part of the section used to estimate the spline coefficients, thus a monotone function progression can not be guaranteed"
            evalAt i t
            )
    /// trd derivative at amplitudes a and curvatures c with x values x  
    let initEvalTrdDerivativeAt (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =

        let n = x.Length

        //define interval stepwidths
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )

        // helper function s(i) for smoothing spline 
        let calcS (h:Vector<float>) (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) (i:int) (t:float) =
            (c.[i+1] - c.[i]) / h.[i]

        let evalAt =  calcS h x a c 
        
        (fun t ->
            let i = 
                match Array.tryFindIndexBack (fun xs -> xs <= t) (x.InternalValues) with 
                | Some x -> x 
                | None   -> failwith "The x-value is not part of the section used to estimate the spline coefficients, thus a monotone function progression can not be guaranteed"
            evalAt i t
            )

    //let initEvalAtFstDerivativ (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =
    //    let n = x.Length
    //    //define interval stepwidths
    //    let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )
    //    // helper functions f(i,1-4)(t) for smoothing spline
    //    let calcF1 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) = (x.[i+1] - t) / h.[i]
    //    let calcF2 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) = (t - x.[i]) / h.[i]
    //    let calcF3 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
    //        (3. * (calcF1 h x i t) ** 2.0 * (calcF1 h x i t) - (calcF1 h x i t) ) * (h.[i]**2.0) / 6.0
    //    let calcF4 (h:Vector<float>) (x:Vector<float>) (i:int) (t:float) =
    //        ((calcF2 h x i t)**3.0 - (calcF2 h x i t) ) * (h.[i]**2.0) / 6.0 
    //    let calcS (h:Vector<float>) (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) (i:int) (t:float) =
    //        a.[i] * (calcF1 h x i t) + a.[i+1] * (calcF2 h x i t) + c.[i] * (calcF3 h x i t) + c.[i+1] * (calcF4 h x i t)
    //    let evalAt =  calcS h x a c 
    //    (fun t ->
    //        let i = 
    //            match Array.tryFindIndexBack (fun xs -> xs <= t) (x.InternalValues) with 
    //            | Some x -> x 
    //            | None   -> failwith "The x-value is not part of the section used to estimate the spline coefficients, thus a monotone function progression can not be guaranteed"
    //        evalAt i t
    //        )
    
    [<Obsolete("Only applicable at equal x spacing. Use getExtrema instead")>]
    //calculates extrema with (type,x_value) where type is +1 for maxima and -1 for minima
    let getExtrema_equalSpacing (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =
        let n = x.Length
        //define interval stepwidths
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )
        let splineFunction = initEvalAt x a c
        let sigBigger a b = a > (b+0.0000001)
        let searchExtrema i (a:Vector<float>) (c:Vector<float>) =
            //checks, if a polynomial coefficient vanishes 
            let isZero (coeff: float) =
                let tolerance = 0.0000001
                if (Math.Abs coeff) <= tolerance then 0. else coeff
            //search space in which extrema belong to the actual interval spline (if outside the bounds, the extremum is not present in global spline)
            let (lower,upper) = 0.,1.
            //coefficients for spline as: ax^3+bx^2+cx+d (d not used in extremum calculation)
            let aCoeff = -1./6. * c.[0] + 1./6. * c.[1]                 |> isZero
            let bCoeff = 0.5 * c.[0]                                    |> isZero
            let cCoeff = -1./3. * c.[0] - 1./6. * c.[1] - a.[0] + a.[1] |> isZero
            
            //coefficients for first derivative 3ax^2+2bx+c
            let aCoeff' = 3. * aCoeff
            let bCoeff' = 2. * bCoeff
            let cCoeff' = cCoeff

            //coefficients for snd derivative 6ax+2b
            let aCoeff'' = 2. * aCoeff'
            let bCoeff'' = bCoeff'

            let findFstDeriv_Zero =
                //apply abc formula to calculate extreme points (but the term within the root should not be negative) --> test
                //if inflection point, root is sometimes -0.0000000000001 because term in root is zero! [0..4] [|3.54049285; 8.761635241; 9.805863719; 9.685956812; 9.086422279|] [|0.0; -6.265370869; -2.428723889e-11; -0.7194414397; 0.0|]
                let root = 
                    (pown bCoeff' 2) - 4. * aCoeff' * cCoeff'
                    |> round 6
                
                if root < 0. then [] 
                else
                    //https://people.csail.mit.edu/bkph/articles/Quadratics.pdf
                    if bCoeff' >= 0. then
                        let x1 = (- bCoeff' - sqrt(root)) / (2. * aCoeff')
                        let x2 = (2. * cCoeff') / (- bCoeff' - sqrt(root))
                        [x1;x2]
                    else
                        let x1 = (2. * cCoeff') / (- bCoeff' + sqrt(root))
                        let x2 = (- bCoeff' + sqrt(root)) / (2. * aCoeff')
                        [x1;x2]

            let slope0 =    
                //check wether the extremum is inside the interval [0,1] (hermite basis interval)
                findFstDeriv_Zero |> List.filter (fun x -> round 1 (x+0.05) >= lower && round 1 (x-0.05) <= upper)

            slope0
            |> List.choose (fun xValNorm -> 
                //recalculate the true xValue outside the normed interval [0,1]
                let xVal = h.[i] * xValNorm + x.[i]
                let roundXVal = round 2 xVal
                if roundXVal = Seq.last x || roundXVal = Seq.head x then 
                    None//(0.,0.)
                else 
                    let sndDerivative = aCoeff'' * xValNorm + bCoeff''
                    match sndDerivative with
                    | s when (round 3 sndDerivative) = 0. -> 
                        if round 1 (xVal-0.05) <= Seq.head x  || round 1 (xVal+0.05) >= Seq.last x then 
                            None//(0.,0.)
                        else 
                            let yAtX = splineFunction xVal
                            //printfn "yAtX: %f" yAtX
                            let yAtpX = splineFunction (xVal - 0.1)
                            let yAtaX = splineFunction (xVal + 0.1)
                            //printfn "yPx: %f yx: %f yAx: %f" yAtpX yAtX yAtaX
                            if sigBigger yAtX yAtpX && sigBigger yAtX yAtaX then Some (1.,xVal)
                            elif sigBigger yAtpX yAtX && sigBigger yAtaX yAtX then Some (-1.,xVal)
                            else None//(0.,0.) //real saddle point
                    | s when sndDerivative < 0. -> Some (1.,xVal)
                    | _                         -> Some (-1.,xVal)
                    )
            //filter items, where not only the first, but also the second derivative is 0 at the specified x Value

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
        |> List.distinctBy (fun (indicator,xVal) -> roundToNext xVal x) //round necessary when root is small and f'(x)=0 at 1.02 and 1.04 because of floating point issues in the root (must be 0)
        //possibility to ignore exrema if max and min are present within 0.2 x Values

    type Condition =
        | In0 //monotonically increasing
        | In1 //1 maximum
        | In2 //2 maxima
        | In3 //3 maxima
        | In4 //4 maxima
        | De0 //monotonically decreasing
        | De1 //1 minimum
        | De2 //2 minima
        | De3 //3 minima
        | De4 //4 minima
        | Complex //more complex

    let getParentShape (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =
        let extrema = getExtrema x a c
        let extremaCount = extrema.Length
        let n = a.Length
        match extremaCount with
        | extremaCount when extremaCount = 0 -> 
            if a.[0] < a.[n-1] then In0
            else De0
        | extremaCount when extremaCount = 1 -> 
            if extrema.[0].Indicator = 1 then In1
            else De1
        | extremaCount when extremaCount = 2 -> 
            if extrema.[0].Indicator = 1 && extrema.[1].Indicator = -1 then In2
            else De2
        | extremaCount when extremaCount = 3 ->
            if extrema.[0].Indicator = 1 && extrema.[1].Indicator = -1 then In3
            else De3
        | extremaCount when extremaCount = 4 ->
            if extrema.[0].Indicator = 1 && extrema.[1].Indicator = -1 then In4
            else De4
        | _ -> Complex
        //if extremaCount = 0 then
        //    if a.[0] < a.[n-1] then In0
        //    else De0
        //elif extremaCount = 1 then
        //    if fst extrema.[0] = 1. then In1
        //    else De1
        //elif extremaCount = 2 then
        //    if fst extrema.[0] = 1. && fst extrema.[1] = -1. then In2
        //    else De2
        //elif extremaCount = 3 then
        //    if fst extrema.[0] = 1. && fst extrema.[1] = -1. then In3
        //    else De3
        //elif extremaCount = 4 then
        //    if fst extrema.[0] = 1. && fst extrema.[1] = -1. then In4
        //    else De4
        //else Complex

    //check the spline for the predefined condition
    let checkshape (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) (con:Condition)=
        let extrema = getExtrema x a c
        let extremaCount = extrema.Length
        let n = a.Length
        match con with
        | In0       -> extremaCount = 0 && a.[0] <= a.[n-1]
        | In1       -> extremaCount = 1 && extrema.[0].Indicator = 1
        | In2       -> extremaCount = 2 && extrema.[0].Indicator = 1 && extrema.[1].Indicator = -1
        | In3       -> extremaCount = 3 && extrema.[0].Indicator = 1
        | In4       -> extremaCount = 4 && extrema.[0].Indicator = 1 && extrema.[1].Indicator = -1
        | De0       -> extremaCount = 0 && a.[0] >= a.[n-1]
        | De1       -> extremaCount = 1 && extrema.[0].Indicator = -1
        | De2       -> extremaCount = 2 && extrema.[0].Indicator = -1 && extrema.[1].Indicator = 1
        | De3       -> extremaCount = 3 && extrema.[0].Indicator = -1
        | De4       -> extremaCount = 4 && extrema.[0].Indicator = -1 && extrema.[1].Indicator = 1
        | Complex   -> true

    let mapCondition (operator:Matrix<float> -> matrix) con =
        let mat = matrix [[1.]]
        match con with
        | x when con = 0 -> if mat = operator mat then Condition.De0 else Condition.In0
        | x when con = 1 -> if mat = operator mat then Condition.De1 else Condition.In1
        | x when con = 2 -> if mat = operator mat then Condition.De2 else Condition.In2
        | x when con = 3 -> if mat = operator mat then Condition.De3 else Condition.In3
        | x when con = 4 -> if mat = operator mat then Condition.De4 else Condition.In4

    type TempClassResult = {
        ///spline function values at knots
        TraceA   : vector
        ///spline second derivative values at knots
        TraceC   : vector
        ///weighted error of a and y
        Error    : float
        ///mGCV
        GCV      : float
        ///used smoothing factor lambda
        Lambda   : float
        ///used constraint matrix for shape determination
        Ctemp    : float [,]
        ///AIC determined by SSE and the number of used extrema
        AICc     : float
        ///function, that returns the splines' value at a given x_value
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
             
    let calcStDevOfRep (arr:Vector<float>) rep =
        if arr.Length%rep = 0 then
            let length = arr.Length / rep 
            vector [for i = 0 to length-1 do yield arr.[i*rep..i*rep+rep-1] |> Seq.stDev]      
        else failwithf "arrLength no multiple of replicate number"

    let normValues (yVal:Vector<float>) =
        let yMean = yVal |> Seq.mean
        let std   = yVal |> Seq.stDevPopulation
        yVal |> Vector.map (fun x -> (x - yMean) / std) 
       
    type WeightingMethod =
        /// weight: 1
        | Equal
        /// weight: minmax((1/stDev),Wmin)
        | VarRobust//Variance
        /// max 0. (log(1./|Seq.cvPopulation g|),2.))
        | CV
        /// weight: (1/stDev)
        | StandardDeviation
        /// weight: sqrt(1/stDev)
        | StandardDeviationSqrt
        /// weight: sqrt(1/(stDev/mean))
        | StandardDeviationAdj
        /// weight: sqrt(sqrt(1/(stDev/mean)))
        | StandardDeviationAdjSqrt
        
    ///creates a weighting matrix out of the x-, and y-Values, the given weighting method and the number of measured replicates.
    let getWeighting (xVal:Vector<float>) (yVal:Vector<float>) (w_method:WeightingMethod) (numRep:int) =
        match w_method with
        | Equal ->
            Matrix.diag(Vector.oneCreate xVal.Length)
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
                //for j = 3 to (n - 1) do 
                for j = 2 to (n - 1) do 
                    for i = 1 to (j-1) do
                        let temp = 
                            (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                            |> vector
                        Matrix.setRow Ctemp (4*i-4) temp
                        Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                        Matrix.setRow Ctemp (4*i-2) (temp - (B.Row (i-1) |> vector))
                        Matrix.setRow Ctemp (4*i-1) (temp - (B.Row (i) |> vector))
                        if i = j - 1 then
                            Matrix.setRow Ctemp (4*i-4) (Vector.zeroCreate Ctemp.NumCols)
                            Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector) //0
                            Matrix.setRow Ctemp (4*i-2) (Vector.zeroCreate Ctemp.NumCols)
                            Matrix.setRow Ctemp (4*i-1) (Vector.zeroCreate Ctemp.NumCols)

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
                //for m = 3 to (n-1) do 
                for m = 2 to (n-1) do 
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
                                Matrix.setRow Ctemp (4*i-4) (Vector.zeroCreate Ctemp.NumCols)
                                Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                                Matrix.setRow Ctemp (4*i-2) (Vector.zeroCreate Ctemp.NumCols)
                                Matrix.setRow Ctemp (4*i-1) (Vector.zeroCreate Ctemp.NumCols)
                        for i = m to j-1 do
                            let temp =                
                                (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                |> vector
                            Matrix.setRow Ctemp (4*i-4) (- temp)
                            Matrix.setRow Ctemp (4*i-3) (- (B.Row (i-1) |> vector))
                            Matrix.setRow Ctemp (4*i-2) (- (temp - (B.Row (i-1) |> vector)))
                            Matrix.setRow Ctemp (4*i-1) (- (temp - (B.Row (i) |> vector)))
                            if i = j - 1 then
                                Matrix.setRow Ctemp (4*i-4) (Vector.zeroCreate Ctemp.NumCols)
                                Matrix.setRow Ctemp (4*i-3) -(B.Row (i-1) |> vector)
                                Matrix.setRow Ctemp (4*i-2) (Vector.zeroCreate Ctemp.NumCols)
                                Matrix.setRow Ctemp (4*i-1) (Vector.zeroCreate Ctemp.NumCols)
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

            elif con = 3 then
                //for m = 3 to (n-1) do 
                for m = 2 to (n-1) do 
                    for j = (m+1) to (n-1) do
                        for k = (j+1) to (n-1) do
                            for i=1 to (m-1) do 
                                let temp = 
                                    (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                    |> vector
                                Matrix.setRow Ctemp (4*i-4) temp
                                Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                                Matrix.setRow Ctemp (4*i-2) (temp - (B.Row (i-1) |> vector))
                                Matrix.setRow Ctemp (4*i-1) (temp - (B.Row (i) |> vector))
                                if i = m - 1 then
                                    Matrix.setRow Ctemp (4*i-4) (Vector.zeroCreate Ctemp.NumCols)
                                    Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                                    Matrix.setRow Ctemp (4*i-2) (Vector.zeroCreate Ctemp.NumCols)
                                    Matrix.setRow Ctemp (4*i-1) (Vector.zeroCreate Ctemp.NumCols)
                            for i = m to j-1 do
                                let temp =                
                                    (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                    |> vector
                                Matrix.setRow Ctemp (4*i-4) (- temp)
                                Matrix.setRow Ctemp (4*i-3) (- (B.Row (i-1) |> vector))
                                Matrix.setRow Ctemp (4*i-2) (- (temp - (B.Row (i-1) |> vector)))
                                Matrix.setRow Ctemp (4*i-1) (- (temp - (B.Row (i) |> vector)))
                                if i = j - 1 then
                                    Matrix.setRow Ctemp (4*i-4) (Vector.zeroCreate Ctemp.NumCols)
                                    Matrix.setRow Ctemp (4*i-3) -(B.Row (i-1) |> vector)
                                    Matrix.setRow Ctemp (4*i-2) (Vector.zeroCreate Ctemp.NumCols)
                                    Matrix.setRow Ctemp (4*i-1) (Vector.zeroCreate Ctemp.NumCols)
                            for i = j to k-1 do
                                let temp =                
                                    (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                    |> vector
                                Matrix.setRow Ctemp (4*i-4) (temp)
                                Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                                Matrix.setRow Ctemp (4*i-2) (temp - (B.Row (i-1) |> vector))
                                Matrix.setRow Ctemp (4*i-1) (temp - (B.Row (i) |> vector))
                                if i = k - 1 then //if i = j - 1 then
                                    Matrix.setRow Ctemp (4*i-4) (Vector.zeroCreate Ctemp.NumCols)
                                    Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector) 
                                    Matrix.setRow Ctemp (4*i-2) (Vector.zeroCreate Ctemp.NumCols)
                                    Matrix.setRow Ctemp (4*i-1) (Vector.zeroCreate Ctemp.NumCols)
                            for i = k to n-1 do
                                let temp = 
                                    (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                    |> vector
                                Matrix.setRow Ctemp (4*i-4) (- temp)
                                Matrix.setRow Ctemp (4*i-3) (-(B.Row (i-1) |> vector))
                                Matrix.setRow Ctemp (4*i-2) (-(temp - (B.Row (i-1) |> vector)))
                                Matrix.setRow Ctemp (4*i-1) (-(temp - (B.Row (i) |> vector)))
                            Matrix.setRow Ctemp (4*(n-1)) (- B.Row (n-1) |> vector)
                            list <- (Matrix.copy Ctemp)::list
                list
                |> Array.ofList 
                
            elif con = 4 then
                //for m = 3 to (n-1) do 
                for m = 2 to (n-1) do 
                    for j = (m+1) to (n-1) do
                        for k = (j+1) to (n-1) do
                            for l = (k+1) to (n-1) do //
                                for i=1 to (m-1) do 
                                    let temp = 
                                        (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                        |> vector
                                    Matrix.setRow Ctemp (4*i-4) temp
                                    Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                                    Matrix.setRow Ctemp (4*i-2) (temp - (B.Row (i-1) |> vector))
                                    Matrix.setRow Ctemp (4*i-1) (temp - (B.Row (i) |> vector))
                                    if i = m - 1 then
                                        Matrix.setRow Ctemp (4*i-4) (Vector.zeroCreate Ctemp.NumCols)
                                        Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                                        Matrix.setRow Ctemp (4*i-2) (Vector.zeroCreate Ctemp.NumCols)
                                        Matrix.setRow Ctemp (4*i-1) (Vector.zeroCreate Ctemp.NumCols)
                                for i = m to j-1 do
                                    let temp =                
                                        (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                        |> vector
                                    Matrix.setRow Ctemp (4*i-4) (- temp)
                                    Matrix.setRow Ctemp (4*i-3) (- (B.Row (i-1) |> vector))
                                    Matrix.setRow Ctemp (4*i-2) (- (temp - (B.Row (i-1) |> vector)))
                                    Matrix.setRow Ctemp (4*i-1) (- (temp - (B.Row (i) |> vector)))
                                    if i = j - 1 then
                                        Matrix.setRow Ctemp (4*i-4) (Vector.zeroCreate Ctemp.NumCols)
                                        Matrix.setRow Ctemp (4*i-3) -(B.Row (i-1) |> vector)
                                        Matrix.setRow Ctemp (4*i-2) (Vector.zeroCreate Ctemp.NumCols)
                                        Matrix.setRow Ctemp (4*i-1) (Vector.zeroCreate Ctemp.NumCols)
                                for i = j to k-1 do
                                    let temp =                
                                        (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                        |> vector
                                    Matrix.setRow Ctemp (4*i-4) (temp)
                                    Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                                    Matrix.setRow Ctemp (4*i-2) (temp - (B.Row (i-1) |> vector))
                                    Matrix.setRow Ctemp (4*i-1) (temp - (B.Row (i) |> vector))
                                    if i = k - 1 then //if i = j - 1 then
                                        Matrix.setRow Ctemp (4*i-4) (Vector.zeroCreate Ctemp.NumCols)
                                        Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector) 
                                        Matrix.setRow Ctemp (4*i-2) (Vector.zeroCreate Ctemp.NumCols)
                                        Matrix.setRow Ctemp (4*i-1) (Vector.zeroCreate Ctemp.NumCols)
                                for i = k to l-1 do
                                    let temp =                
                                        (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                        |> vector
                                    Matrix.setRow Ctemp (4*i-4) (temp)
                                    Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector)
                                    Matrix.setRow Ctemp (4*i-2) (temp - (B.Row (i-1) |> vector))
                                    Matrix.setRow Ctemp (4*i-1) (temp - (B.Row (i) |> vector))
                                    if i = l - 1 then //if i = j - 1 then
                                        Matrix.setRow Ctemp (4*i-4) (Vector.zeroCreate Ctemp.NumCols)
                                        Matrix.setRow Ctemp (4*i-3) (B.Row (i-1) |> vector) 
                                        Matrix.setRow Ctemp (4*i-2) (Vector.zeroCreate Ctemp.NumCols)
                                        Matrix.setRow Ctemp (4*i-1) (Vector.zeroCreate Ctemp.NumCols)
                                for i = l to n-1 do
                                    let temp = 
                                        (List.init (i-1) (fun x -> 0.))@((-3./h.[i-1])::(3./h.[i-1])::(List.init (n-i-1) (fun x -> 0.)))
                                        |> vector
                                    Matrix.setRow Ctemp (4*i-4) (- temp)
                                    Matrix.setRow Ctemp (4*i-3) (-(B.Row (i-1) |> vector))
                                    Matrix.setRow Ctemp (4*i-2) (-(temp - (B.Row (i-1) |> vector)))
                                    Matrix.setRow Ctemp (4*i-1) (-(temp - (B.Row (i) |> vector)))
                                Matrix.setRow Ctemp (4*(n-1)) (- B.Row (n-1) |> vector)
                                list <- (Matrix.copy Ctemp)::list
                list
                |> Array.ofList  
            else failwith "Condition max 4"

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

    type Minimizer = 
        | GCV
        | AICc

    ///returns the best fit of the observation x,-and y-values and a given weightingMatrix
    let getBestFitOfWeighting x_values y_values wMat minimizer = 
        let getinitialestimate = getInitialEstimateOfWXY wMat (vector y_values) x_values
        //monotone splines
        let fst1 =(In0,splineIncreasing x_values (vector y_values) wMat 0) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result 
        let fst2 =(De0,splineDecreasing x_values (vector y_values) wMat 0) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result 

        //splines with one extremum
        let snd1 =(In1,splineIncreasing x_values (vector y_values) wMat 1) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result 
        let snd2 =(De1,splineDecreasing x_values (vector y_values) wMat 1) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result 

        //splines with two extrema                                                                                                              
        let trd1 =(In2,splineIncreasing x_values (vector y_values) wMat 2) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result 
        let trd2 =(De2,splineDecreasing x_values (vector y_values) wMat 2) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result 

        //splines with three extremum
        let qua1 =(In3,splineIncreasing x_values (vector y_values) wMat 3) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result 
        let qua2 =(De3,splineDecreasing x_values (vector y_values) wMat 3) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result 

        //splines with four extrema                                                                                                             
        let qui1 =(In4,splineIncreasing x_values (vector y_values) wMat 4) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result 
        let qui2 =(De4,splineDecreasing x_values (vector y_values) wMat 4) |> fun (shapeClass,(constraintMatrices,result)) -> shapeClass,result 

        match minimizer with
        | Minimizer.AICc -> 
            let initialSelectionCriterion = getinitialestimate.AICc
        
            let bestfst = [fst1;fst2] |> List.minBy (fun (cl,x) -> x.AICc)                                                                
            let bestsnd = [snd1;snd2] |> List.minBy (fun (cl,x) -> x.AICc)                                                               
            let besttrd = [trd1;trd2] |> List.minBy (fun (cl,x) -> x.AICc)
            let bestqua = [qua1;qua2] |> List.minBy (fun (cl,x) -> x.AICc)                                                               
            let bestqui = [qui1;qui2] |> List.minBy (fun (cl,x) -> x.AICc)

            //selection of optimal shape possibility by model selection via AICc 
            [bestfst;bestsnd;besttrd;bestqua;bestqui]
            |> List.indexed 
            |> List.minBy (fun (i,(cl,result)) -> 1.05**(float i) * result.AICc)
            |> fun (i,(cl,result)) -> if result.AICc < initialSelectionCriterion then (cl,result) else Complex,result        
        | Minimizer.GCV -> 
            let initialSelectionCriterion = getinitialestimate.GCV             
            let bestfst = [fst1;fst2] |> List.minBy (fun (cl,x) -> x.GCV)                                                                
            let bestsnd = [snd1;snd2] |> List.minBy (fun (cl,x) -> x.GCV)                                                               
            let besttrd = [trd1;trd2] |> List.minBy (fun (cl,x) -> x.GCV)
            let bestqua = [qua1;qua2] |> List.minBy (fun (cl,x) -> x.GCV)                                                               
            let bestqui = [qui1;qui2] |> List.minBy (fun (cl,x) -> x.GCV)

            //selection of optimal shape possibility by model selection via AICc 
            [bestfst;bestsnd;besttrd;bestqua;bestqui]
            |> List.indexed 
            |> List.minBy (fun (i,(cl,result)) -> 1.05**(float i) * result.GCV)
            |> fun (i,(cl,result)) -> if result.GCV < initialSelectionCriterion then (cl,result) else Complex,result        
             

    [<Obsolete("Only applicable at equal x spacing. Use initEvalAt instead")>]
    //same as initEvalAt, but with recalculated polynomial coefficients
    let initFunctionWithCoefficients (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =  
        let n = x.Length
        //define interval stepwidth
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )

        let deriv i (a:Vector<float>) (c:Vector<float>) xV=
            let tmpT = (xV - x.[i]) / h.[i]
            let pa = -1./6. * c.[0] + 1./6. * c.[1]
            let pb = 0.5 * c.[0] 
            let pc = -1./3. * c.[0] - 1./6. * c.[1] - a.[0] + a.[1]
            let pd = a.[0]
            pa * tmpT**3. + pb * tmpT**2. + pc * tmpT + pd

        (fun t ->
            let i =
                if t = Seq.last x then 
                    Seq.length x - 2
                else
                    x
                    |> Seq.findIndex(fun x_Knot -> (x_Knot - t) > 0.)
                    |> fun nextInterval -> nextInterval - 1
            deriv i a.[i .. i+1] c.[i .. i+1] t
            )
    
    (*
    How to determine coefficients?
    f(x)    =ax^3+bx^2+cx+d //traceA
    f'(x)   =3ax^2+2bx+c
    f''(x)  =6ax+2b         //traceC
    - at knots x is 0 or 1 (begin and end of interval)
    - c0 = 2b
    - c1 = 6a+2b
    - a0 = d
    - a1 = a+b+c+d
    solve system by substitution and you get the coefficients for this interval
    *)


    ///recalculates the polynomial coefficients of the given spline f(knot) and f''(knot)
    ///[a1,b1,c1,d1,a2,b2...] (ax^3+bx^2...)
    [<Obsolete("Only applicable at equal x spacing!")>]
    let getCoefficients (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =
        let n = x.Length
        //Definiere Intervalle
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )
        let deriv (a:Vector<float>) (c:Vector<float>) =
            let pa = -1./6. * c.[0] + 1./6. * c.[1]
            let pb = 0.5 * c.[0] 
            let pc = -1./3. * c.[0] - 1./6. * c.[1] - a.[0] + a.[1]
            let pd = a.[0]
            [pa;pb;pc;pd]
        [0 .. n-1]
        |> List.map (fun i ->
            let t = x.[i]
            let i =
                if t = Seq.last x then 
                    Seq.length x - 2
                else
                    x
                    |> Seq.findIndex(fun x_Knot -> (x_Knot - t) > 0.)
                    |> fun nextInterval -> nextInterval - 1
            deriv a.[i .. i+1] c.[i .. i+1]
            )
        |> List.concat

    [<Obsolete("Only applicable at equal x spacing. Use initEvalSndDerivativeAt instead")>]
    ///returns a function that calculates the second derivative for a given xValue
    let getSndDerivative (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =
        let n = x.Length
        //define interval stepwidth
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )
        let deriv i (a:Vector<float>) (c:Vector<float>) xV=
            let tmpT = (xV - x.[i]) / h.[i] 
            let pa = -1./6. * c.[0] + 1./6. * c.[1]
            let pb = 0.5 * c.[0] 
            //let pc = -1./3. * c.[0] - 1./6. * c.[1] - a.[0] + a.[1]
            //let pd = a.[0]
            6. * pa * tmpT + 2. * pb
        (fun t ->
            let i =
                if t = Seq.last x then 
                    Seq.length x - 2
                else
                    x
                    |> Seq.findIndex(fun x_Knot -> (x_Knot - t) > 0.)
                    |> fun nextInterval -> nextInterval - 1
            deriv i a.[i .. i+1] c.[i .. i+1] t
            )


    [<Obsolete("Only applicable at equal x spacing. Use initEvalAt instead")>]
    ///returns a function that calculates the first derivative for a given xValue
    let getSignalFunction (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =
        let n = x.Length
        //define interval stepwidth
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )
        let deriv i (a:Vector<float>) (c:Vector<float>) xV=
            let tmpT = (xV - x.[i]) / h.[i]
            let pa = -1./6. * c.[0] + 1./6. * c.[1]
            let pb = 0.5 * c.[0] 
            let pc = -1./3. * c.[0] - 1./6. * c.[1] - a.[0] + a.[1]
            let pd = a.[0]
            pa * tmpT**3. + pb * tmpT**2. + pc * tmpT + pd
        (fun t ->
            let i =
                if t = Seq.last x then 
                    Seq.length x - 2
                else
                    x
                    |> Seq.findIndex(fun x_Knot -> (x_Knot - t) > 0.) 
                    |> fun nextInterval -> nextInterval - 1
            deriv i a.[i .. i+1] c.[i .. i+1] t
            )
    
    [<Obsolete("Only applicable at equal x spacing. Use initEvalFstDerivativeAt instead")>]
    ///returns a function that calculates the first derivative for a given xValue
    let getFstDerivative (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =
        let n = x.Length
        //define interval stepwidth
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )
        let deriv i (a:Vector<float>) (c:Vector<float>) xV=
            let tmpT = (xV - x.[i]) / h.[i]
            let pa = -1./6. * c.[0] + 1./6. * c.[1]
            let pb = 0.5 * c.[0] 
            let pc = -1./3. * c.[0] - 1./6. * c.[1] - a.[0] + a.[1]
            //let pd = a.[0]
            3. * pa * tmpT**2. + 2. * pb * tmpT + pc
        (fun t ->
            let i =
                if t = Seq.last x then 
                    Seq.length x - 2
                else
                    x
                    |> Seq.findIndex(fun x_Knot -> (x_Knot - t) > 0.)
                    |> fun nextInterval -> nextInterval - 1
            deriv i a.[i .. i+1] c.[i .. i+1] t
            )

    [<Obsolete("Only applicable at equal x spacing. Use initEvalTrdDerivativeAt instead")>]    
    ///returns a function that calculates the third derivative for a given xValue
    let getTrdDerivative (x:Vector<float>) (a:Vector<float>) (c:Vector<float>) =
        let n = x.Length
        //define interval stepwidth
        let h = Vector.init (n-1) (fun i -> x.[i+1] - x.[i] )
        let deriv i (a:Vector<float>) (c:Vector<float>) xV=
            let pa = -1./6. * c.[0] + 1./6. * c.[1]
            6. * pa
        (fun t ->
            let i =
                if t = Seq.last x then 
                    Seq.length x - 2
                else
                    x
                    |> Seq.findIndex(fun x_Knot -> (x_Knot - t) > 0.)
                    |> fun nextInterval -> nextInterval - 1
            deriv i a.[i .. i+1] c.[i .. i+1] t
            )

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

    ///gets the observation values (x,y), the number of measured replicates and the weightingmethod and returns the spline result of the best fit
    let getBestFit xVal yVal repNumber weightingMethod minimizer = 
        let yValMeans = calcMeanOfRep yVal repNumber //|> normValues
        let weightingMatrix = (getWeighting xVal (yVal |> vector) weightingMethod repNumber)
        let (cl,fit) = getBestFitOfWeighting xVal yValMeans weightingMatrix minimizer
        fit





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
