namespace FSharp.Stats.Signal


open FSharp.Stats
open System


///padds data points to the beginning, the end and on internal intervals of the data
module Padding =

    module HelperFunctions =

        ///average spacing of the data points
        let inline getAvgSpacing (data : ('a * float) []) (getDiff: 'a -> 'a -> float) = 
            let n = data.Length
            ///minimal x_Value
            let minX = data |> Array.head |> fst
            ///minimal y_Value
            let maxX = data |> Array.last |> fst
            (getDiff maxX minX) / (float n)

        module Time =

            ///getDiff: calculates the time span between the two events as total minutes (float)
            let getDiffMinutes (a: DateTime) (b: DateTime) =
                a - b
                |> fun x -> x.TotalMinutes 
            ///addToXValue: adds minutes to the date
            let addToXValueMinutes (dt: DateTime) (minutes: float) =
                dt.AddMinutes(minutes)
            ///addToXValue: adds minutes to the date
            let addToXValueHours (dt: DateTime) (hours: float) =
                dt.AddHours(hours)    

        module Float =

            ///getDiff: calculates the difference of the two events (-)
            let getDiffFloat (a: float) (b: float) =
                a - b
            ///addToXValue: adds toAdd to a (+)
            let addToXValueFloat (a: float) (toAdd: float) =
                a + toAdd

        module Int =
            ///getDiff: calculates the difference of the two events
            let getDiffInt (a: int) (b: int) =
                (float a) - (float b)
            ///addToXValue: adds toAdd to a
            let addToXValueInt (a: int) (toAdd: float) =
                a + (int toAdd)

    //padds data point in small gaps (e.g. a missing data point or small ranges with no data)
    type InternalPaddingMethod =
        //inserts random data points taken from the original data set
        | Random 
        //inserts nan values
        | NaN
        //does not insert any point internally
        | Delete
        //inserts 0.0 as y_Value
        | Zero
        //inserts points lying on the linear interpolation of the two adjacent knots
        | LinearInterpolation
        
    //padds data point in huge gaps (e.g. big ranges with no data)
    type HugeGapPaddingMethod =
        //inserts random data points taken from the original data set in a huge data gap
        | Random 
        //inserts nan values in a huge data gap
        | NaN
        //does not insert any point internally
        | Delete
        //inserts 0.0 as y_Value
        | Zero
        //inserts points lying on the linear interpolation of the two adjacent knots
        | LinearInterpolation

    ///Adds additional data points to the beginning and end of data set (number: borderpadding; x_Value distance: minDistance; y_Value: random).
    ///Between every pair of data point where the difference in x_Values is greater than minDistance, additional datapoints are generated as defined in internalPaddingMethod.
    ///If huge data chunks are missing (missing gap < maxDistance), data points are added as defined in hugeGapPaddingMethod.
    ///default: internalPaddingMethod=LinearInterpolation; hugeGapPaddingMethod=Random (like in border cases)
    ///getDiff: get the difference in x_Values as float representation (if 'a is float then (-))
    ///addToXValue: function that adds a float to the x_Value (if 'a is float then (+))
    let inline padd (data : ('a * float) []) (minDistance: float) (maxDistance : float) (getDiff: 'a -> 'a -> float) (addToXValue : 'a -> float -> 'a) (borderpadding : int) (internalPaddingMethod: InternalPaddingMethod) (hugeGapPaddingMethod: HugeGapPaddingMethod) =
        let rnd = System.Random()
        let n = data.Length
        ///minimal x_Value
        let minX = data |> Array.head |> fst
        ///minimal y_Value
        let maxX = data |> Array.last |> fst
        ///adds 'borderpadding' number of random data points to the left
        let leftPadding     = 
            Array.init borderpadding (fun i -> 
                let paddX = addToXValue minX (- (float i + 1.) * minDistance)
                let paddY = snd data.[rnd.Next(0,n)] //n+1
                paddX,paddY)
                |> Array.rev
        ///adds 'borderpadding' number of random data points to the rigth
        let rightPadding    = 
            Array.init borderpadding (fun i -> 
                let paddX = addToXValue maxX ((float i + 1.) * minDistance)
                let paddY = snd data.[rnd.Next(0,n)] //n+1
                paddX,paddY)

        let fillSpaceInBetween = 
            //interpolate the space between the two adjacent knots and add aditional points (number = (getDiff p1 p2 / minDistance) - 1)
            let linearInterpol current next numberOfPointsToAdd xSpacing =
                let m =
                    let deltaX = getDiff (fst next) (fst current)
                    let deltaY = snd next - snd current
                    deltaY/deltaX
                //number of points to add between the adjacent points, that the distance is < minDistance
                let pointsToAdd =
                    [1 .. int numberOfPointsToAdd]
                    |> List.map (fun interval -> 
                        let x = addToXValue (fst current) (float interval * xSpacing)
                        let y = snd current + (m * float interval * xSpacing)
                        x,y)
                pointsToAdd

            let rec loop i acc = 
                //abort condition
                if i = n-1 then
                    acc 
                    |> List.rev
                    |> List.concat
                else
                    //current data point
                    let current = data.[i]
                    //right next data point
                    let next = data.[i+1]
                    //difference of these two points
                    let diff = Math.Abs (getDiff (fst current) (fst next))

                    //if the difference is greater than the minimal distance, padding must happen
                    if diff > minDistance then
                        //how many points should be added that no gap is greater than minDistance
                        let numberOfPointsToAdd = (diff / minDistance) - 1. |> floor
                        //how must the spacing be to insert the number of data points in a uniformly manner
                        let xSpacing = diff / (numberOfPointsToAdd + 1.)

                        //if there is a huge gap, then do not use the internalPaddingMethod, but use the specified hugeGapPaddingMethod
                        if diff > maxDistance then
                            match hugeGapPaddingMethod with
                            | HugeGapPaddingMethod.Random -> 
                                let pointsToAdd =
                                    [1 .. int numberOfPointsToAdd]
                                    |> List.map (fun interval -> 
                                        let x = addToXValue (fst current) (float interval * xSpacing)
                                        let y = data.[rnd.Next(0,n)] |> snd
                                        x,y)
                                loop (i+1) (pointsToAdd::[current]::acc)    //add random             
                            | HugeGapPaddingMethod.NaN -> 
                                let pointsToAdd =
                                    [1 .. int numberOfPointsToAdd]
                                    |> List.map (fun interval -> 
                                        let x = addToXValue (fst current) (float interval * xSpacing)
                                        let y = nan
                                        x,y)
                                loop (i+1) (pointsToAdd::[current]::acc)    //add nan
                            | HugeGapPaddingMethod.Delete ->
                                loop (i+1) ([current]::acc)                 //no values are added
                            | HugeGapPaddingMethod.Zero -> 
                                let pointsToAdd =
                                    [1 .. int numberOfPointsToAdd]
                                    |> List.map (fun interval -> 
                                        let x = addToXValue (fst current) (float interval * xSpacing)
                                        let y = 0.0
                                        x,y)
                                loop (i+1) (pointsToAdd::[current]::acc)    //zeros are added
                            | _ ->
                                let pointsToAdd = linearInterpol current next numberOfPointsToAdd xSpacing                              
                                loop (i+1) (pointsToAdd::[current]::acc)    //linear interpolation values are added
                        //if the gap is greater than minDistance but smaller than maxDistance, padd the gap as defined in internalPaddingMethod
                        else
                            match internalPaddingMethod with
                            | InternalPaddingMethod.Random -> 
                                let pointsToAdd =
                                    [1 .. int numberOfPointsToAdd]
                                    |> List.map (fun interval -> 
                                        let x = addToXValue (fst current) (float interval * xSpacing)
                                        let y = data.[rnd.Next(0,n)] |> snd
                                        x,y
                                        )
                                loop (i+1) (pointsToAdd::[current]::acc)    //add random             
                            | InternalPaddingMethod.NaN -> 
                                let pointsToAdd =
                                    [1 .. int numberOfPointsToAdd]
                                    |> List.map (fun interval -> 
                                        let x = addToXValue (fst current) (float interval * xSpacing)
                                        let y = nan
                                        x,y
                                        )
                                loop (i+1) (pointsToAdd::[current]::acc)    //add nan
                            | InternalPaddingMethod.Delete -> 
                                loop (i+1) ([current]::acc)                 //no values are added
                            | InternalPaddingMethod.Zero -> 
                                let pointsToAdd =
                                    [1 .. int numberOfPointsToAdd]
                                    |> List.map (fun interval -> 
                                        let x = addToXValue (fst current) (float interval * xSpacing)
                                        let y = 0.0
                                        x,y
                                        )
                                loop (i+1) (pointsToAdd::[current]::acc)    //zeros are added
                            | _ ->
                                let pointsToAdd = linearInterpol current next numberOfPointsToAdd xSpacing                              
                                loop (i+1) (pointsToAdd::[current]::acc)    //linear interpolation values are added


                    else
                        loop (i+1) ([current]::acc)
            loop 0 []
            |> Array.ofSeq
        [leftPadding;fillSpaceInBetween;rightPadding] |> Array.concat

    
    let padSignalStartAndEnd x = raise (System.NotImplementedException())

    let padSignalStart x = raise (System.NotImplementedException())

    let padSignalEnd x = raise (System.NotImplementedException())

    let padSparseSignal x = raise (System.NotImplementedException())
