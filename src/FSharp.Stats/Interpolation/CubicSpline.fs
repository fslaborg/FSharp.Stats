namespace FSharp.Stats.Interpolation

open System

module CubicSpline = 
    open FSharp.Stats

    type BoundaryCondition =
        ///most used spline variant: f'' at borders is set to 0
        | Natural
        ///f' at first point is the same as f' at the last point
        | Periodic
        ///f'' at first/second and last/penultimate knot are equal
        | Parabolic
        ///f''' at second and penultimate knot are continuous
        | NotAKnot
        ///first and last polynomial are quadratic, not cubic
        | Quadratic
        ///f' at first and last knot are set by user
        | Clamped
              
    /// computes all coefficients for piecewise interpolating splines. In the form of [a0;b0;c0;d0;a1;b1;...;d(n-2)]. 
    /// where: fn(x) = (an)x^3+(bn)x^2+(cn)x+(dn)
    let fit (boundaryCondition: BoundaryCondition) (xValues: Vector<float>) (yValues: Vector<float>) =
        //f(x)   = ax³+bx²+cx+d
        //f'(x)  = 3ax²+2bx+c
        //f''(x) = 6ax+2b

        let (xVal,yVal) =
            let indices =
                xValues
                |> Seq.indexed
                |> Seq.sortBy snd
                |> Seq.map fst
            let sortedXValues = indices |> Seq.map (fun i -> xValues.[i]) |> vector
            let sortedYValues = indices |> Seq.map (fun i -> yValues.[i]) |> vector
            sortedXValues,sortedYValues

        let intervalNumber = xVal.Length - 1

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
            let firstX = xVal.[0]
            let secondX = xVal.[1]
            let lastX = xVal.[intervalNumber]
            let penultimate = xVal.[intervalNumber - 1]
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

            | Quadratic ->
                //first condition: a1 = 0
                tmp1.[0] <- 1.
                
                //second condition: an = 0.
                tmp2.[4 * (intervalNumber - 1) + 0] <- 1.
                
                [|(tmp1,0.);(tmp2,0.)|]
                
            | Clamped -> //user defined border f''
                failwith "Not implemented yet. Slopes m1 and m2 have to be set by user"
                ////first condition: f''0(x0) = m1
                //tmp1.[0] <- 6. * firstX
                //tmp1.[1] <- 2.
                ////second condition: f''n-1(xn) = m2
                //tmp2.[4 * (intervalNumber - 1) + 0] <- 6. * lastX
                //tmp2.[4 * (intervalNumber - 1) + 1] <- 2.
                //[|(tmp1,m1);(tmp2,m2)|]

        let (equations,solutions) =
            let interPolConstraints =
                [|0 .. intervalNumber - 1|]
                |> Array.map (fun i -> 
                    [|
                    interpolatingConstraints i xVal.[i] yVal.[i]
                    interpolatingConstraints i xVal.[i+1] yVal.[i+1]
                    |])
                    |> Array.concat

            let derivativeConstraints =
                [|0 .. intervalNumber - 2|]
                |> Array.map (fun i -> 
                    [|
                    firstDerivativeConstraints  i xVal.[i+1]
                    secondDerivativeConstraints i xVal.[i+1]
                    |])
                |> Array.concat
                
            [|interPolConstraints;derivativeConstraints;boundaryCondition|]
            |> Array.concat
            |> Array.unzip
                
        let A = Matrix.ofJaggedArray equations
        let b = Vector.ofArray solutions

        Algebra.LinearAlgebra.SolveLinearSystem A b 
        
    ///Fits a cubic spline with given coefficients. Only defined within the range of the given xValues
    let predictWithinRange (coefficients: Vector<float>) (xValues: Vector<float>) x =
        let sortedX = xValues |> Seq.sort
        let intervalNumber =
                
            if x > Seq.last sortedX || x < Seq.head sortedX then 
                failwith "Spline is not defined outside of the interval of the xValues"
                
            if x = Seq.last sortedX then 
                Seq.length sortedX - 2
            else
                sortedX
                |> Seq.findIndex(fun xKnot -> (xKnot - x) > 0.)
                |> fun nextInterval -> nextInterval - 1
            
        let yValue = 
            coefficients.[4 * intervalNumber + 0] * (pown x 3) +    //a*x³
            coefficients.[4 * intervalNumber + 1] * (pown x 2) +    //b*x²
            coefficients.[4 * intervalNumber + 2] * x          +    //c*x
            coefficients.[4 * intervalNumber + 3]                   //d
            
        yValue

    ///fits a cubic spline fit even outside of the interval defined in xValues by linear interpolation of slope given by border knots
    let predict (coefficients: Vector<float>) (xValues: Vector<float>) x =
        let sortedX = xValues |> Seq.sort
        let xHead = xValues |> Seq.head
        let xLast = xValues |> Seq.last
            
        let intercept intervalNumber x = 
            coefficients.[4 * intervalNumber + 0] * (pown x 3) +    //a*x³
            coefficients.[4 * intervalNumber + 1] * (pown x 2) +    //b*x²
            coefficients.[4 * intervalNumber + 2] * x          +    //c*x
            coefficients.[4 * intervalNumber + 3]                   //d           
            
        let slope intervalNumber x= 
            3. * coefficients.[4 * intervalNumber + 0] * (pown x 2) +   //3a*x²
            2. * coefficients.[4 * intervalNumber + 1] * x +            //2b*x
            coefficients.[4 * intervalNumber + 2]                       //c

        if x >= Seq.last sortedX then 
            let intervalNr = Seq.length sortedX - 2
            let diffX = x - xLast
            let y = intercept intervalNr xLast + diffX * (slope intervalNr xLast)
            y
        elif x < Seq.head sortedX then 
            let intervalNr = 0
            let diffX = x - xHead
            let y = intercept intervalNr xHead + diffX * (slope intervalNr xHead)
            y
        else
            predictWithinRange coefficients xValues x

    let private getDerivative order (coefficients: Vector<float>) (xValues: Vector<float>) x =
        let sortedX = xValues |> Seq.sort
        let intervalNumber =
            if x >= Seq.last sortedX then 
                Seq.length sortedX - 2
            elif x < Seq.head sortedX then 
                0
            else
                sortedX
                |> Seq.findIndex(fun xKnot -> (xKnot - x) > 0.)
                |> fun nextInterval -> nextInterval - 1            
        match order with
        | d when d = 1 ->
            let firstDerivative = 
                3. * coefficients.[4 * intervalNumber + 0] * (pown x 2) +   //3a*x²
                2. * coefficients.[4 * intervalNumber + 1] * x +            //2b*x
                coefficients.[4 * intervalNumber + 2]                       //c          
            firstDerivative
        | d when d = 2 ->
            let secondDerivative = 
                6. * coefficients.[4 * intervalNumber + 0] * x +   //6ax
                2. * coefficients.[4 * intervalNumber + 1]         //2b      
            secondDerivative
        | d when d = 3 ->
            let thirdDerivative = 
                6. * coefficients.[4 * intervalNumber + 0]  //6a     
            thirdDerivative
        | _ -> failwithf "for cubic splines no derivative > 3 is defined"

    let getFirstDerivative (coefficients: Vector<float>) (xValues: Vector<float>) x =
        getDerivative 1 coefficients xValues x

    let getSecondDerivative (coefficients: Vector<float>) (xValues: Vector<float>) x =
        getDerivative 2 coefficients xValues x    
        
    let getThirdDerivative (coefficients: Vector<float>) (xValues: Vector<float>) x =
        getDerivative 3 coefficients xValues x

    module Hermite =

        ///calculates a function to interpolate between the datapoints with given slopes (yData').
        ///the data has to be sorted ascending
        let cubicHermite (xData: Vector<float>) (yData: Vector<float>) (yData': Vector<float>) =
            let n = xData.Length

            let phi0 t tAdd1 x =
                let tmp = (x - t) / (tAdd1 - t)
                2. * (pown tmp 3) - 3. * (pown tmp 2) + 1.
                           
            let phi1 t tAdd1 x =
                let tmp = (x - t) / (tAdd1 - t)
                - 2. * (pown tmp 3) + 3. * (pown tmp 2)    

            let psi0 t tAdd1 x =
                let tmp = (x - t) / (tAdd1 - t)
                let a = tAdd1 - t
                let b = (pown tmp 3) - 2. * (pown tmp 2) + tmp
                a * b
                
            let psi1 t tAdd1 x =
                let tmp = (x - t) / (tAdd1 - t)
                let a = tAdd1 - t
                let b = (pown tmp 3) - (pown tmp 2)
                a * b 

            let calculate index x =
                let ph0 = yData.[index]    * phi0 xData.[index] xData.[index+1] x
                let ph1 = yData.[index+1]  * phi1 xData.[index] xData.[index+1] x
                let ps0 = yData'.[index]   * psi0 xData.[index] xData.[index+1] x
                let ps1 = yData'.[index+1] * psi1 xData.[index] xData.[index+1] x
                ph0 + ph1 + ps0 + ps1


            (fun t ->
                if t = Seq.last xData then 
                    Seq.last yData
                else                 
                    let i = 
                        match Array.tryFindIndexBack (fun xs -> xs <= t) (xData |> Vector.toArray) with 
                        | Some x -> x 
                        | None   -> failwith "The given xValue is out of the range defined in xData"
                    calculate i t
                )              
        

        ///calculates the slopes by averaging the slopes of neighbouring tangents
        let getSimpleSlopes (xData: Vector<float>) (yData: Vector<float>) = 
            Vector.init xData.Length (fun i ->
                if i = 0 then
                    (yData.[i] - yData.[i+1]) / (xData.[i] - xData.[i+1])
                elif i = xData.Length - 1 then 
                    (yData.[i] - yData.[i-1]) / (xData.[i] - xData.[i-1])
                else 
                    let s1 = (yData.[i] - yData.[i+1]) / (xData.[i] - xData.[i+1])
                    let s2 = (yData.[i] - yData.[i-1]) / (xData.[i] - xData.[i-1])
                    (s1 + s2) / 2.
                                        )

        ///if the knots are monotone in/decreasing, the spline also is monotone (http://www.korf.co.uk/spline.pdf)
        let getSlopesTryMonotonicity (xData: Vector<float>) (yData: Vector<float>) =
            let calcSlope i =
                let s1 = (xData.[i+1] - xData.[i]) / (yData.[i+1] - yData.[i])
                let s2 = (xData.[i] - xData.[i-1]) / (yData.[i] - yData.[i-1])
                2. / (s1 + s2)

            let rec loop i acc =
                if i = xData.Length - 1 then
                    let s1 = (3. * (yData.[i] - yData.[i-1])) / (2. * (xData.[i] - xData.[i-1]))
                    let s2 = calcSlope (i-1) / 2.
                    let tmp = s1 - s2
                    (tmp::acc) |> List.rev
                else 
                    let tmp = calcSlope i
                    if ((yData.[i] - yData.[i-1]) * (yData.[i+1] - yData.[i])) <= 0. then 
                        loop (i+1) (0.::acc)
                    else 
                        loop (i+1) (tmp::acc)

            let slopeAtFstKnot = 
                let s1 = (3. * (yData.[1] - yData.[0])) / (2. * (xData.[1] - xData.[0]))
                let s2 = calcSlope 1 / 2.
                let slope = s1 - s2
                slope
 
            let slopes = loop 1 [slopeAtFstKnot] 
            slopes |> vector

    [<Obsolete("Use Interpolation.CubicSpline instead!")>]
    module Simple = 
    
        [<Obsolete("Use Interpolation.CubicSpline.coefficients instead!")>]
        let coefficients (boundaryCondition: BoundaryCondition) (xValues: Vector<float>) (yValues: Vector<float>) =
            fit boundaryCondition xValues yValues 
    
        [<Obsolete("Use Interpolation.CubicSpline.predictWithinRange instead!")>]
        let fit (coefficients: Vector<float>) (xValues: Vector<float>) x =
            predictWithinRange coefficients xValues
    
        [<Obsolete("Use Interpolation.CubicSpline.predict instead!")>]
        let fitWithLinearPrediction (coefficients: Vector<float>) (xValues: Vector<float>) x =
            predict coefficients xValues
    
        [<Obsolete("Use Interpolation.CubicSpline.getFirstDerivative instead!")>]
        let getFirstDerivative (coefficients: Vector<float>) (xValues: Vector<float>) x =
            getFirstDerivative coefficients xValues
    
        [<Obsolete("Use Interpolation.CubicSpline.getSecondDerivative instead!")>]
        let getSecondDerivative (coefficients: Vector<float>) (xValues: Vector<float>) x =
            getSecondDerivative coefficients xValues
    
        [<Obsolete("Use Interpolation.CubicSpline.getThirdDerivative instead!")>]
        let getThirdDerivative (coefficients: Vector<float>) (xValues: Vector<float>) x =
            getThirdDerivative coefficients xValues
    
    [<Obsolete("Use Interpolation.Akima instead!")>]
    module Differentiable = 
    
        [<Obsolete("Use Interpolation.Akima.fitHermiteSorted instead!")>]
        let interpolateHermiteSorted (xValues:float []) (yValues:float []) (firstDerivatives:float []) = 
            Akima.fitHermiteSorted xValues yValues firstDerivatives
    
        [<Obsolete("Use Interpolation.Akima.fit instead!")>]
        let coefficients (xValues:float []) (yValues:float []) =
            Akima.fit xValues yValues
    
        [<Obsolete("Use Interpolation.Akima.predict instead!")>]
        let interpolateAtX (splineCoeffs: Akima.SplineCoefficients) xVal =
            Akima.predict splineCoeffs xVal
    
        [<Obsolete("Use Interpolation.Akima.getFirstDerivative instead!")>]
        let firstDerivative (splineCoeffs: Akima.SplineCoefficients) xVal =
            Akima.getFirstDerivative splineCoeffs xVal
    
        [<Obsolete("Use Interpolation.Akima.secondDerivative instead!")>]
        let secondDerivative (splineCoeffs: Akima.SplineCoefficients) xVal =
            Akima.getSecondDerivative splineCoeffs xVal
    
        [<Obsolete("Use Interpolation.Akima.computeIndefiniteIntegral instead!")>]
        let computeIndefiniteIntegral (splineCoeffs: Akima.SplineCoefficients) = 
            Akima.computeIndefiniteIntegral splineCoeffs
    
        [<Obsolete("Use Interpolation.Akima.integrate instead!")>]
        let integrate (splineCoeffs: Akima.SplineCoefficients) xVal = 
            Akima.integrate splineCoeffs xVal
    
        [<Obsolete("Use Interpolation.Akima.definiteIntegral instead!")>]
        let definiteIntegral (integrateF: float -> float) xVal1 xVal2 =
            Akima.definiteIntegral xVal1 xVal2
