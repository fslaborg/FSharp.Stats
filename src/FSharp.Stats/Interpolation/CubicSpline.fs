namespace FSharp.Stats.Interpolation

open System

/// <summary>
///   Cubic splines interpolate two dimensional data by applying piecewise cubic polynomials that are continuous at the input coordinates (knots).
///   The function itself, the first- and second derivative are continuous at the knots.
/// </summary>
module CubicSpline = 
    open FSharp.Stats

    type BoundaryCondition =
        ///<summary>most used spline variant: f'' at borders is set to 0</summary>
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
    
    /// <summary>
    /// Contains x data, y data (c0), slopes (c1), curvatures (c2), and the third derivative at each knot
    /// </summary>
    type CubicSplineCoef = {
        XData : vector
        /// <summary>
        /// vector of [a0;b0;c0;d0;a1;b1;...;d(n-2)] where f_n(x) = (an)x^3 + (bn)x^2 + (cn)x + (dn)
        /// </summary>
        C0_3 : vector} with
            static member Create x c = {XData=x;C0_3=c}

    /// <summary>
    ///   Computes coefficients for piecewise interpolating splines. In the form of [a0;b0;c0;d0;a1;b1;...;d(n-2)]. 
    ///   where: fn(x) = (an)x^3+(bn)x^2+(cn)x+(dn)
    /// </summary>
    /// <param name="boundaryCondition">Condition that defines how slopes and curvatures are defined at the left- and rightmost knot</param>
    /// <param name="xValues">Note: Must not contain duplicate x values (use Approximation.regularizeValues to preprocess data!)</param>
    /// <param name="yValues">function value at x values</param>
    /// <returns>Coefficients that define the interpolating function.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    ///
    /// // get coefficients for piecewise interpolating cubic polynomials
    /// let coefficients = 
    ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
    /// </code> 
    /// </example>
    let interpolate (boundaryCondition: BoundaryCondition) (xValues: Vector<float>) (yValues: Vector<float>) =
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

        let coeffs = Algebra.LinearAlgebra.SolveLinearSystem A b 
        CubicSplineCoef.Create xValues coeffs

    /// <summary>
    ///   Returns function that takes x value (that lies within the range of input x values) and predicts the corresponding interpolating y value.
    /// </summary>
    /// <param name="coefficients">Interpolation functions coefficients.</param>
    /// <param name="x">X value of which the y value should be predicted.</param>
    /// <returns>Function that takes an x value and returns function value.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get coefficients for piecewise interpolating cubic polynomials
    /// let coefficients = 
    ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
    ///
    /// // get interpolating value
    /// let func = CubicSpline.predictWithinRange(coefLinSpl)
    ///
    /// // get function value at x=3.4
    /// func 3.4
    ///
    /// </code> 
    /// </example>
    /// <remarks>Only defined within the range of the given xValues!</remarks>
    let predictWithinRange (coefficients: CubicSplineCoef) x =
        let sortedX = coefficients.XData |> Seq.sort
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
            coefficients.C0_3.[4 * intervalNumber + 0] * (pown x 3) +    //a*x³
            coefficients.C0_3.[4 * intervalNumber + 1] * (pown x 2) +    //b*x²
            coefficients.C0_3.[4 * intervalNumber + 2] * x          +    //c*x
            coefficients.C0_3.[4 * intervalNumber + 3]                   //d
            
        yValue

    /// <summary>
    ///   Returns function that takes x value and predicts the corresponding interpolating y value.
    /// </summary>
    /// <param name="coefficients">Interpolation functions coefficients.</param>
    /// <param name="x">X value of which the y value should be predicted.</param>
    /// <returns>Function that takes an x value and returns function value.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get coefficients for piecewise interpolating cubic polynomials
    /// let coefficients = 
    ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
    ///
    /// // get interpolating function 
    /// let func = CubicSpline.predict(coefLinSpl)
    ///
    /// // get function value at x=3.4
    /// func 3.4
    ///
    /// </code> 
    /// </example>
    /// <remarks>x values outside of the xValue range are predicted by straight lines defined by the nearest knot!</remarks>
    let predict (coefficients: CubicSplineCoef) x =
        let sortedX = coefficients.XData |> Seq.sort
        let xHead = coefficients.XData |> Seq.head
        let xLast = coefficients.XData |> Seq.last
            
        let intercept intervalNumber x = 
            coefficients.C0_3.[4 * intervalNumber + 0] * (pown x 3) +    //a*x³
            coefficients.C0_3.[4 * intervalNumber + 1] * (pown x 2) +    //b*x²
            coefficients.C0_3.[4 * intervalNumber + 2] * x          +    //c*x
            coefficients.C0_3.[4 * intervalNumber + 3]                   //d           
            
        let slope intervalNumber x = 
            3. * coefficients.C0_3.[4 * intervalNumber + 0] * (pown x 2) +   //3a*x²
            2. * coefficients.C0_3.[4 * intervalNumber + 1] * x +            //2b*x
            coefficients.C0_3.[4 * intervalNumber + 2]                       //c

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
            predictWithinRange coefficients x

    let internal getDerivative order (coefficients: CubicSplineCoef) x =
        let sortedX = coefficients.XData |> Seq.sort
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
                3. * coefficients.C0_3.[4 * intervalNumber + 0] * (pown x 2) +   //3a*x²
                2. * coefficients.C0_3.[4 * intervalNumber + 1] * x +            //2b*x
                coefficients.C0_3.[4 * intervalNumber + 2]                       //c          
            firstDerivative
        | d when d = 2 ->
            let secondDerivative = 
                6. * coefficients.C0_3.[4 * intervalNumber + 0] * x +   //6ax
                2. * coefficients.C0_3.[4 * intervalNumber + 1]         //2b      
            secondDerivative
        | d when d = 3 ->
            let thirdDerivative = 
                6. * coefficients.C0_3.[4 * intervalNumber + 0]  //6a     
            thirdDerivative
        | _ -> failwithf "for cubic splines no derivative > 3 is defined"

    /// <summary>
    ///   Returns function that takes x value and predicts the corresponding slope.
    /// </summary>
    /// <param name="coefficients">Interpolation functions coefficients.</param>
    /// <param name="x">X value of which the slope should be predicted.</param>
    /// <returns>Function that takes an x value and returns the function slope.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get coefficients for piecewise interpolating cubic polynomials
    /// let coefficients = 
    ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
    ///
    /// // get slope function
    /// let func = CubicSpline.getFirstDerivative(coefLinSpl)
    ///
    /// // get slope at x=3.4
    /// func 3.4
    ///
    /// </code> 
    /// </example>
    /// <remarks>x values outside of the xValue range are predicted by straight lines defined by the nearest knot!</remarks>
    let getFirstDerivative (coefficients: CubicSplineCoef) x =
        getDerivative 1 coefficients x

    /// <summary>
    ///   Returns function that takes x value and predicts the corresponding curvature.
    /// </summary>
    /// <param name="coefficients">Interpolation functions coefficients.</param>
    /// <param name="x">X value of which the curvature should be predicted.</param>
    /// <returns>Function that takes an x value and returns the function curvature.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get coefficients for piecewise interpolating cubic polynomials
    /// let coefficients = 
    ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
    ///
    /// // get curvature function
    /// let func = CubicSpline.getSecondDerivative(coefLinSpl)
    ///
    /// // get curvature at x=3.4
    /// func 3.4
    ///
    /// </code> 
    /// </example>
    /// <remarks>x values outside of the xValue range are predicted by straight lines defined by the nearest knot!</remarks>
    let getSecondDerivative (coefficients: CubicSplineCoef) x =
        getDerivative 2 coefficients x    
        
    /// <summary>
    ///   Returns function that takes x value and predicts the corresponding third derivative.
    /// </summary>
    /// <param name="coefficients">Interpolation functions coefficients.</param>
    /// <param name="x">X value of which the y value should be predicted.</param>
    /// <returns>Function that takes an x value and returns the function third derivative.</returns>
    /// <example> 
    /// <code> 
    /// // e.g. days since a certain event
    /// let xData = vector [|0.;1.;5.;4.;3.;|]
    /// // some measured feature
    /// let yData = vector [|1.;5.;4.;13.;17.|]
    /// 
    /// // get coefficients for piecewise interpolating cubic polynomials
    /// let coefficients = 
    ///     CubicSpline.interpolate CubicSpline.BoundaryConditions.Natural xData yData
    ///
    /// // get third derivative function
    /// let func = CubicSpline.getThirdDerivative(coefLinSpl)
    ///
    /// // get third derivative at x=3.4
    /// func 3.4
    ///
    /// </code> 
    /// </example>
    /// <remarks>x values outside of the xValue range are predicted by straight lines defined by the nearest knot!</remarks>
    let getThirdDerivative (coefficients: CubicSplineCoef) x =
        getDerivative 3 coefficients x

    /// <summary>
    /// Hermite cubic splines are defined by the function values and their slopes (first derivatives). If the slopws are unknown, they must be estimated.
    /// </summary>
    module Hermite =

        type HermiteCoef = {
            /// sample points, sorted ascending
            XValues : vector
            /// Zero order spline coefficients, intersects, y values
            YValues : vector
            /// First order spline coefficients, slopes at knots
            Slopes : vector
            } with 
                static member Create xValues yValues slopes  = {
                    XValues=xValues;YValues=yValues;Slopes=slopes
                    }

        /// <summary>
        ///   Computes coefficients for piecewise interpolating splines.
        ///   The x data has to be sorted ascending.
        /// </summary>
        /// <param name="xData">Note: Must not contain duplicate x values (use Approximation.regularizeValues to preprocess data!)</param>
        /// <param name="yData">function value at x values</param>
        /// <returns>Coefficients that define the interpolating function.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        ///
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     CubicSpline.Hermite.interpolate xData yData
        ///
        /// </code> 
        /// </example>
        /// <remarks>Second derivative (curvature) is NOT continuous at knots to allow higher flexibility to reduce oscillations!</remarks>
        let interpolate (xData: Vector<float>) (yData: Vector<float>) = 
            let slopes = 
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
            HermiteCoef.Create xData yData slopes

        /// <summary>
        ///   Computes coefficients for piecewise interpolating splines. 
        ///   If the knots are monotone in/decreasing, the spline also is monotone (http://www.korf.co.uk/spline.pdf)        
        ///   The x data has to be sorted ascending
        /// </summary>
        /// <param name="yData">function value at x values</param>
        /// <returns>Coefficients that define the interpolating function.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;6.;13.;13.1|]
        ///
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     CubicSpline.Hermite.interpolateTryMonotonicity xData yData
        ///
        /// </code> 
        /// </example>
        /// <remarks>Second derivative (curvature) is NOT continuous at knots to allow higher flexibility to reduce oscillations!</remarks>
        let interpolateTryMonotonicity (xData: Vector<float>) (yData: Vector<float>) =
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
            HermiteCoef.Create xData yData (slopes |> vector)


        /// <summary>
        ///   Returns function that takes x value and predicts the corresponding interpolating y value. 
        /// </summary>
        /// <param name="coefficients">Interpolation functions coefficients.</param>
        /// <param name="x">X value of which the y value should be predicted.</param>
        /// <returns>Function that takes an x value and returns function value.</returns>
        /// <example> 
        /// <code> 
        /// // e.g. days since a certain event
        /// let xData = vector [|0.;1.;5.;4.;3.;|]
        /// // some measured feature
        /// let yData = vector [|1.;5.;4.;13.;17.|]
        /// 
        /// // get coefficients for piecewise interpolating cubic polynomials
        /// let coefficients = 
        ///     CubicSpline.Hermite.interpolate xData yData
        ///
        /// // get interpolating function 
        /// let func = CubicSpline.Hermite.predict coefLinSpl
        ///
        /// // get function value at x=3.4
        /// func 3.4
        /// </code> 
        /// </example>
        /// <remarks>x values outside of the xValue range are predicted by straight lines defined by the nearest knot!</remarks>
        let predict (coef: HermiteCoef) x =
            let n = coef.XValues.Length

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
                let ph0 = coef.YValues.[index]    * phi0 coef.XValues.[index] coef.XValues.[index+1] x
                let ph1 = coef.YValues.[index+1]  * phi1 coef.XValues.[index] coef.XValues.[index+1] x
                let ps0 = coef.Slopes.[index]   * psi0 coef.XValues.[index] coef.XValues.[index+1] x
                let ps1 = coef.Slopes.[index+1] * psi1 coef.XValues.[index] coef.XValues.[index+1] x
                ph0 + ph1 + ps0 + ps1

            if x = Seq.last coef.XValues then 
                Seq.last coef.YValues
            else                 
                let i = 
                    match Array.tryFindIndexBack (fun xs -> xs <= x) (coef.XValues |> Vector.toArray) with 
                    | Some x -> x 
                    | None   -> failwith "The given xValue is out of the range defined in xData"
                calculate i x

        ///calculates the slopes by averaging the slopes of neighbouring tangents
        [<Obsolete("Use interpolate instead to get slopes")>]
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
        [<Obsolete("Use interpolateTryMonotonicity instead")>]
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
            interpolate boundaryCondition xValues yValues 
    
        [<Obsolete("Use Interpolation.CubicSpline.predictWithinRange instead!")>]
        let fit (coefficients: Vector<float>) (xValues: Vector<float>) x =
            predictWithinRange (CubicSplineCoef.Create xValues coefficients) x
    
        [<Obsolete("Use Interpolation.CubicSpline.predict instead!")>]
        let fitWithLinearPrediction (coefficients: Vector<float>) (xValues: Vector<float>) x =
            predict (CubicSplineCoef.Create xValues coefficients) x
    
        [<Obsolete("Use Interpolation.CubicSpline.getFirstDerivative instead!")>]
        let getFirstDerivative (coefficients: Vector<float>) (xValues: Vector<float>) x =
            getFirstDerivative (CubicSplineCoef.Create xValues coefficients)
    
        [<Obsolete("Use Interpolation.CubicSpline.getSecondDerivative instead!")>]
        let getSecondDerivative (coefficients: Vector<float>) (xValues: Vector<float>) x =
            getSecondDerivative (CubicSplineCoef.Create xValues coefficients)
    
        [<Obsolete("Use Interpolation.CubicSpline.getThirdDerivative instead!")>]
        let getThirdDerivative (coefficients: Vector<float>) (xValues: Vector<float>) x =
            getThirdDerivative (CubicSplineCoef.Create xValues coefficients)
    
    [<Obsolete("Use Interpolation.Akima instead!")>]
    module Differentiable = 
    
        [<Obsolete("Use Interpolation.Akima.interpolateHermiteSorted instead!")>]
        let interpolateHermiteSorted (xValues:float []) (yValues:float []) (firstDerivatives:float []) = 
            Akima.interpolateHermiteSorted xValues yValues firstDerivatives
    
        [<Obsolete("Use Interpolation.Akima.interpolate instead!")>]
        let coefficients (xValues:float []) (yValues:float []) =
            Akima.interpolate xValues yValues
    
        [<Obsolete("Use Interpolation.Akima.predict instead!")>]
        let interpolateAtX (splineCoeffs: Akima.SubSplineCoef) xVal =
            Akima.predict splineCoeffs xVal
    
        [<Obsolete("Use Interpolation.Akima.getFirstDerivative instead!")>]
        let firstDerivative (splineCoeffs: Akima.SubSplineCoef) xVal =
            Akima.getFirstDerivative splineCoeffs xVal
    
        [<Obsolete("Use Interpolation.Akima.secondDerivative instead!")>]
        let secondDerivative (splineCoeffs: Akima.SubSplineCoef) xVal =
            Akima.getSecondDerivative splineCoeffs xVal
    
        [<Obsolete("Use Interpolation.Akima.computeIndefiniteIntegral instead!")>]
        let computeIndefiniteIntegral (splineCoeffs: Akima.SubSplineCoef) = 
            Akima.computeIndefiniteIntegral splineCoeffs
    
        [<Obsolete("Use Interpolation.Akima.integrate instead!")>]
        let integrate (splineCoeffs: Akima.SubSplineCoef) xVal = 
            Akima.integrate splineCoeffs xVal
    
        [<Obsolete("Use Interpolation.Akima.definiteIntegral instead!")>]
        let definiteIntegral (integrateF: float -> float) xVal1 xVal2 =
            Akima.definiteIntegral xVal1 xVal2



