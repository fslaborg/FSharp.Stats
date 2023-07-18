namespace FSharp.Stats.Interpolation

open System
open FSharp.Stats

/// <summary>
/// Approximation
/// </summary>
module Approximation =

    type Spacing = 
        | Equally
        | Chebyshev

    /// <summary>
    ///   Zips x and y values, sorts them by x values and applies a given function to y values of x duplicate (like R! regularize.values).
    ///   1. pairs x-y values 
    ///   2. filters non finite entries and sorts value pairs by x values
    ///   3. handles y values of x value ties by given function
    /// </summary> 
    /// <param name="xData">unsorted x values</param>
    /// <param name="yData">y values of corresponding x values</param>
    /// <param name="ties">function to transform a collection of y values, that have equal x values (e.g. Seq.average, Seq.max, Seq.min)</param>
    /// <returns>Collection of sorted x,y tuples with unique x values</returns>
    let regularizeValues (xData: seq<float>) (yData: seq<float>) (ties: seq<float> -> float) =
        if Seq.length xData <> Seq.length yData then
            raise (System.ArgumentException("x and y are of different length!"))
        let xy = 
            Seq.zip xData yData
            // Remove nan
            |> Seq.filter (fun (x,y) -> not (isNan x || isNan y || isInf x || isInf y))
            // sort by x
            |> Seq.sortBy fst
            |> Seq.groupBy fst
            |> Seq.map (fun (key,values) -> 
                let ny = values |> Seq.map snd |> ties  //|> Seq.averageBy ( fun (x,y) -> y) //
                (key,ny) 
                )
        xy

    /// <summary>
    ///   Return a sequence of points which interpolate the given data points by straight lines.
    /// </summary> 
    /// <param name="xData">unsorted x values</param>
    /// <param name="yData">y values of corresponding x values</param>
    /// <param name="v">Collection of x values of which the y values should be predicted using straight interpolating lines between the input knots.</param>
    /// <param name="ties">function to transform a collection of y values, that have equal x values (e.g. Seq.average, Seq.max, Seq.min)</param>
    /// <returns>Collection of predicted v,y tuples.</returns>
    let approx (xData: seq<float>) (yData: seq<float>) (v: seq<float>) (ties: seq<float> -> float) =
        let xy = regularizeValues xData yData ties
        let nx,ny = xy |> Seq.toArray |> Array.unzip 
        let interPol = LinearSpline.interpolate nx ny
        v |> Seq.map (LinearSpline.predict interPol)

    /// <summary>
    ///   Creates a collection of ordered x values within a given interval. The spacing is according to Chebyshev to remove runges phenomenon when approximating a given function.
    /// </summary> 
    /// <param name="interval">start and end value of interpolation range</param>
    /// <param name="n">number of points that should be placed within the interval (incl. start and end)</param>
    /// <returns>Collection of chebyshev spaced x values.</returns>
    /// <remarks>www.mscroggs.co.uk/blog/57</remarks>
    let chebyshevNodes (interval: Intervals.Interval<float>) n = 
        let center = 0.5 * (interval.TryStart.Value + interval.TryEnd.Value)
        let halfrange = 0.5 * (interval.TryEnd.Value - interval.TryStart.Value) 
        Array.init n (fun i -> 
            center + halfrange * cos(Math.PI * (2. * (float i + 1.) - 1.)/(2. * float n)) 
            )
        |> Array.sort
        |> vector

    /// <summary>
    ///   Creates a collection of ordered x values within a given interval. The spacing of the values is always the same.
    /// </summary> 
    /// <param name="interval">start and end value of interpolation range</param>
    /// <param name="n">number of points that should be placed within the interval (incl. start and end)</param>
    /// <returns>Collection of equally spaced x values.</returns>
    let equalNodes (interval: Intervals.Interval<float>) n = 
        let range = Intervals.getSize interval
        Vector.init n (fun k -> interval.TryStart.Value + float k * range / (float n - 1.))
    

type Approximation() =

    /// <summary>
    ///   Determines polynomial coefficients to approximate the given function with (i) n equally spaced nodes or (ii) n nodes spaced according to Chebyshev.
    ///   Use Polynomial.fit to get a polynomial function of the coefficients.
    /// </summary> 
    /// <param name="f">Function that should be approximated by a polynomial interpolating function.</param>
    /// <param name="i">Interval for which the function should be approximated</param>
    /// <param name="n">number of points that should be placed within the interval (incl. start and end)</param>
    /// <param name="spacing">X values can be spaced equally or according to chebyshev.</param>
    /// <returns>Coefficients for polynomial interpolation. Use Polynomial.fit to predict function values.</returns>
    static member approxWithPolynomial (f: float -> float,i: Intervals.Interval<float>,n: int,spacing: Approximation.Spacing) = 
        match spacing with
        | Approximation.Equally -> 
            let xVal = Approximation.equalNodes i n 
            let yVal = Vector.map f xVal
            Polynomial.interpolate xVal yVal
        | Approximation.Chebyshev ->    
            let xVal = Approximation.chebyshevNodes i n 
            let yVal = Vector.map f xVal
            Polynomial.interpolate xVal yVal

    /// <summary>
    ///   Determines polynomial coefficients to approximate the given data with (i) n equally spaced nodes or (ii) n nodes spaced according to Chebyshev.
    ///   Use Polynomial.fit to get a polynomial function of the coefficients.
    /// </summary> 
    /// <param name="xData">Note: Must not contain duplicate x values (use Approximation.regularizeValues to preprocess data!)</param>
    /// <param name="yData">vector of y values</param>
    /// <param name="n">number of points that should be placed within the interval (incl. start and end)</param>
    /// <param name="spacing">X values can be spaced equally or according to chebyshev.</param>
    /// <returns>Coefficients for polynomial interpolation. Use Polynomial.fit to predict function values.</returns>
    static member approxWithPolynomialFromValues (xData: seq<float>,yData: seq<float>,n: int,spacing: Approximation.Spacing) = 
        match spacing with
        | Approximation.Equally -> 
            let i = Intervals.ofSeq xData
            let linearSplineCoeff = LinearSpline.interpolate (Array.ofSeq xData) (Array.ofSeq yData)
            let f = LinearSpline.predict linearSplineCoeff
            let xVal = Approximation.equalNodes i n 
            let yVal = Vector.map f xVal
            Polynomial.interpolate xVal yVal
        | Approximation.Chebyshev ->    
            let i = Intervals.ofSeq xData
            let linearSplineCoeff = LinearSpline.interpolate (Array.ofSeq xData) (Array.ofSeq yData)
            let f = LinearSpline.predict linearSplineCoeff
            let xVal = Approximation.chebyshevNodes i n 
            let yVal = Vector.map f xVal
            Polynomial.interpolate xVal yVal

//open FSharp.Stats
//open FSharp.Stats.Fitting.LinearRegression
//open System
//open Plotly.NET

//let myFunction = fun x -> 1./(1. + 25.*x**2.)

//let interval = Intervals.Interval.Create (-1.,1.)

//let chart n =
//    let eqX = Interpolation.Approximation.equalNodes interval n
//    let chX = Interpolation.Approximation.chebyshevNodes interval n
//    let eqCoeffs = Interpolation.Approximation.approxPolynomial myFunction interval n
//    let chCoeffs = Interpolation.Approximation.approxChebyshevPolynomial myFunction interval n

//    let getCh f = 
//        [interval.TryStart.Value .. 0.01 .. interval.TryEnd.Value]
//        |> List.map (fun x -> 
//            x,f x)
//        |> Chart.Line

//    let x,y = 
//        [interval.TryStart.Value .. 0.001 .. interval.TryEnd.Value]
//        |> List.map (fun x -> 
//            x,myFunction x)
//        |> List.unzip
        
//    let olscoef = OrdinaryLeastSquares.Polynomial.coefficient n (vector x) (vector y)
//    let olsfit = OrdinaryLeastSquares.Polynomial.fit n olscoef

//    [
//        getCh myFunction |> Chart.withLineStyle(Width=4.,Color=Color.fromHex "2ca02c") |> Chart.withTraceName "original"
//        eqX |> Seq.map (fun x -> x,0) |> Chart.Point |> Chart.withMarkerStyle(Color=Color.fromHex "#1f77b4")|> Chart.withTraceName "eqX"
//        chX |> Seq.map (fun x -> x,0) |> Chart.Point |> Chart.withMarkerStyle(Color=Color.fromHex "#ff7f0e")|> Chart.withTraceName "chX"
//        getCh (Interpolation.Polynomial.fit eqCoeffs) |> Chart.withLineStyle(Color=Color.fromHex "#1f77b4") |> Chart.withTraceName "equally"
//        getCh (Interpolation.Polynomial.fit chCoeffs) |> Chart.withLineStyle(Color=Color.fromHex "#ff7f0e") |> Chart.withTraceName "chebyshev"
//        getCh olsfit |> Chart.withLineStyle(Color=Color.fromHex "#9467bd")|> Chart.withTraceName "OLS"
//    ]
//    |> Chart.combine
//    |> Chart.withTemplate ChartTemplates.lightMirrored

//chart 9
//|> Chart.show
