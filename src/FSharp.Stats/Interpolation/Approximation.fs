namespace FSharp.Stats.Interpolation

open System

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
                 |> Seq.filter  ( fun (x,y) -> not(nan.Equals(x) && nan.Equals(y)) )
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


        v
        //let interPol = MathNet.Numerics.Interpolation.LinearSpline.Interpolate(nx,ny)
        //v |> Seq.map (fun x ->  interPol.Interpolate(x) )



 