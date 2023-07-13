namespace FSharp.Stats.Interpolation

open System
open FSharp.Stats
open FSharp.Stats.Interpolation

module Approximation =
    
    /// Regularize (like R! regularize.values) used in approx
    /// 1. pairs x -y values 
    /// 2. filters nan on both sides and sortby x
    /// 3. handels ties by given function
    let regularizeValues (x:seq<float>) (y:seq<float>) (ties:seq<float> -> float) =
        let xLength = Seq.length(x) 
        if (xLength <> Seq.length(y)) then
            raise (System.ArgumentException("x and y are of different length!"))
        // Remove nan on both sides
        let xy = (Seq.map2 ( fun x y -> (x,y) ) x y )
                 |> Seq.filter  ( fun (x,y) -> not(isNan x && isNan y))
                 // sort byx
                 |> Seq.sortBy  ( fun (x,y) -> x)
                 |> Seq.groupBy ( fun (x,y) -> x)
                 |> Seq.map     ( fun (key,values) -> let ny = values |> Seq.map ( fun (x,y) -> y) |>  ties  //|> Seq.averageBy ( fun (x,y) -> y) //
                                                      (key,ny) )
        xy


    /// Return a sequence of points which linearly interpolate given data points, or a function performing the linear interpolation.
    let approx (x:seq<float>) (y:seq<float>) (v:seq<float>) (ties:seq<float> -> float) =
        let xy = regularizeValues x y ties
        let nx = xy |> Seq.map ( fun (x,y) -> x) |> Seq.toArray
        let ny = xy |> Seq.map ( fun (x,y) -> y) |> Seq.toArray

        
        let interPol = 
            LinearSpline.interpolate nx ny
        v |> Seq.map (fun x -> LinearSpline.predict interPol x )

    /// Creates ordered x values in chebyshev spacing to remove runges phenomenon when approximating a given function
    // www.mscroggs.co.uk/blog/57
    let chebyshevNodes (interval: Intervals.Interval<float>) n = 
        let center = 0.5 * (interval.TryStart.Value + interval.TryEnd.Value)
        let halfrange = 0.5 * (interval.TryEnd.Value - interval.TryStart.Value) 
        Array.init n (fun i -> 
            center + halfrange * cos(Math.PI * (2. * (float i + 1.) - 1.)/(2. * float n)) 
            )
        |> Array.sort
        |> vector

    /// Creates ordered and equally spaced values to approximate a given function with polynomials.
    let equalNodes (interval: Intervals.Interval<float>) n = 
        let range = Intervals.getSize interval
        Vector.init n (fun k -> interval.TryStart.Value + float k * range / (float n - 1.))
    
    /// Determines polynomial coefficients to approximate the given function with n equally spaced nodes.
    /// Use Polynomial.fit to get a polynomial function of the coefficients.
    let approxPolynomial (f: float -> float) (i: Intervals.Interval<float>) (n: int) = 
        let xVal = equalNodes i n 
        let yVal = Vector.map f xVal
        Polynomial.interpolate xVal yVal

    /// Determines polynomial coefficients to approximate the given function with n equally spaced nodes.
    /// Use Polynomial.fit to get a polynomial function of the coefficients.
    let approxPolynomialFromValues xs ys (n: int) = 
        let i = Intervals.ofSeq xs
        let linearSplineCoeff = LinearSpline.interpolate (Array.ofSeq xs) (Array.ofSeq ys)
        let f = LinearSpline.predict linearSplineCoeff
        let xVal = equalNodes i n 
        let yVal = Vector.map f xVal
        Polynomial.interpolate xVal yVal

    /// Determines polynomial coefficients to approximate the given function with n nodes, spaced according to chebyshev
    /// Use Polynomial.fit to get a polynomial function of the coefficients.
    // www.mscroggs.co.uk/blog/57
    let approxChebyshevPolynomial (f: float -> float) (i: Intervals.Interval<float>) (n: int) = 
        let xVal = chebyshevNodes i n 
        let yVal = Vector.map f xVal
        Polynomial.interpolate xVal yVal

    /// Determines polynomial coefficients to approximate the function defined by the given values with n nodes, spaced according to chebyshev
    /// Use Polynomial.fit to get a polynomial function of the coefficients.
    // www.mscroggs.co.uk/blog/57
    let approxChebyshevPolynomialFromValues xs ys (n: int) = 
        let i = Intervals.ofSeq xs
        let linearSplineCoeff = LinearSpline.interpolate (Array.ofSeq xs) (Array.ofSeq ys)
        let f = LinearSpline.predict linearSplineCoeff
        let xVal = chebyshevNodes i n 
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
