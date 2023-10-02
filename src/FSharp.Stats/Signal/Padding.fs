namespace FSharp.Stats.Signal


open FSharp.Stats
open System


///padds data points to the beginning, the end and on internal intervals of the data
module Padding =

    module HelperFunctions =

        /// <summary>average spacing of the data points</summary>
        /// <remarks></remarks>
        /// <param name="data"></param>
        /// <param name="getDiff"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline getAvgSpacing (data : ('a * float) []) (getDiff: 'a -> 'a -> float) = 
            let n = data.Length
            ///minimal x_Value
            let minX = data |> Array.head |> fst
            ///maximal x_Value
            let maxX = data |> Array.last |> fst
            (getDiff maxX minX) / (float n)

        /// <summary>median spacing of the data points</summary>
        /// <remarks></remarks>
        /// <param name="data"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline getMedianSpacing (data : ('a * float) []) (getDiff: 'a -> 'a -> float) = 
            let n = data.Length
            ///get median spacing
            let median =
                [|1 .. n - 1|]
                |> Array.map (fun idx -> getDiff (fst data.[idx]) (fst data.[idx - 1]))
                |> Array.sort
                |> fun intervals -> intervals.[int (n / 2) - 1]
            median
        
        /// <summary>minimum spacing of the data points</summary>
        /// <remarks></remarks>
        /// <param name="data"></param>
        /// <param name="getDiff"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline getMinimumSpacing (data : ('a * float) []) (getDiff: 'a -> 'a -> float) = 
            let n = data.Length
            [|1 .. n - 1|]
            |> Array.map (fun idx -> getDiff (fst data.[idx]) (fst data.[idx - 1]))
            |> Array.min

        module Time =

            /// <summary>getDiff: calculates the time span between the two events as total minutes (float)</summary>
            /// <remarks></remarks>
            /// <param name="a"></param>
            /// <param name="b"></param>
            /// <returns></returns>
            /// <example>
            /// <code>
            /// </code>
            /// </example>
            let getDiffMinutes (a: DateTime) (b: DateTime) =
                a - b
                |> fun x -> x.TotalMinutes 
            /// <summary>addToXValue: adds minutes to the date</summary>
            /// <remarks></remarks>
            /// <param name="dt"></param>
            /// <param name="minutes"></param>
            /// <returns></returns>
            /// <example>
            /// <code>
            /// </code>
            /// </example>
            let addToXValueMinutes (dt: DateTime) (minutes: float) =
                dt.AddMinutes(minutes)
            /// <summary>addToXValue: adds minutes to the date</summary>
            /// <remarks></remarks>
            /// <param name="dt"></param>
            /// <param name="hours"></param>
            /// <returns></returns>
            /// <example>
            /// <code>
            /// </code>
            /// </example>
            let addToXValueHours (dt: DateTime) (hours: float) =
                dt.AddHours(hours)    

        module Float =

            /// <summary>getDiff: calculates the difference of the two events (-)</summary>
            /// <remarks></remarks>
            /// <param name="a"></param>
            /// <param name="b"></param>
            /// <returns></returns>
            /// <example>
            /// <code>
            /// </code>
            /// </example>
            let getDiffFloat (a: float) (b: float) =
                a - b
            /// <summary>addToXValue: adds toAdd to a (+)</summary>
            /// <remarks></remarks>
            /// <param name="a"></param>
            /// <param name="toAdd"></param>
            /// <returns></returns>
            /// <example>
            /// <code>
            /// </code>
            /// </example>
            let addToXValueFloat (a: float) (toAdd: float) =
                a + toAdd

        module Int =
            /// <summary>getDiff: calculates the difference of the two events</summary>
            /// <remarks></remarks>
            /// <param name="a"></param>
            /// <param name="b"></param>
            /// <returns></returns>
            /// <example>
            /// <code>
            /// </code>
            /// </example>
            let getDiffInt (a: int) (b: int) =
                (float a) - (float b)
            /// <summary>addToXValue: adds toAdd to a</summary>
            /// <remarks></remarks>
            /// <param name="a"></param>
            /// <param name="toAdd"></param>
            /// <returns></returns>
            /// <example>
            /// <code>
            /// </code>
            /// </example>
            let addToXValueInt (a: int) (toAdd: float) =
                a + (int toAdd)

    ///padds data point at signals start and end
    type BorderPaddingMethod =
        ///inserts random data points taken from the original data set
        | Random
        ///inserts 0.0 as y_Value
        | Zero

    ///padds data point in small gaps (e.g. a missing data point or small ranges with no data)
    type InternalPaddingMethod =
        //(inserts random data points taken from the original data set
        | Random 
        ///inserts nan values
        | NaN
        ///does not insert any point internally
        | Delete
        //inserts 0.0 as y_Value
        | Zero
        ///inserts points lying on the linear interpolation of the two adjacent knots
        | LinearInterpolation
        
    ///padds data point in huge gaps (e.g. big ranges with no data)
    type HugeGapPaddingMethod =
        ///inserts random data points taken from the original data set in a huge data gap
        | Random 
        ///inserts nan values in a huge data gap
        | NaN
        ///does not insert any point internally
        | Delete
        ///inserts 0.0 as y_Value
        | Zero
        ///inserts points lying on the linear interpolation of the two adjacent knots
        | LinearInterpolation

    /// <summary>Adds additional data points to the beginning and end of data set (number: borderpadding; x_Value distance: minDistance; y_Value: random).<br/>Between every pair of data point where the difference in x_Values is greater than minDistance, additional datapoints are generated as defined in internalPaddingMethod.<br/>If huge data chunks are missing (missing gap &lt; maxDistance), data points are added as defined in hugeGapPaddingMethod.</summary>
    /// <remarks>default: internalPaddingMethod=LinearInterpolation; hugeGapPaddingMethod=Random (like in border cases)</remarks>
    /// <param name="data"></param>
    /// <param name="minDistance"></param>
    /// <param name="maxDistance"></param>
    /// <param name="getDiff">get the difference in x_Values as float representation (if 'a is float then (-))</param>
    /// <param name="addToXValue">function that adds a float to the x_Value (if 'a is float then (+))</param>
    /// <param name="borderpadding"></param>
    /// <param name="borderPaddingMethod"></param>
    /// <param name="internalPaddingMethod"></param>
    /// <param name="hugeGapPaddingMethod"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline pad (data : ('a * float) []) (minDistance: float) (maxDistance : float) (getDiff: 'a -> 'a -> float) (addToXValue : 'a -> float -> 'a) (borderpadding : int) (borderPaddingMethod: BorderPaddingMethod) (internalPaddingMethod: InternalPaddingMethod) (hugeGapPaddingMethod: HugeGapPaddingMethod) =
        let rnd = System.Random()
        let n = data.Length
        ///minimal x_Value
        let minX = data |> Array.head |> fst
        ///minimal y_Value
        let maxX = data |> Array.last |> fst
        ///adds 'borderpadding' number of random data points to the left
        let leftPadding     = 
            match borderPaddingMethod with
            | BorderPaddingMethod.Random -> 
                Array.init borderpadding (fun i -> 
                    let paddX = addToXValue minX (- (float i + 1.) * minDistance)
                    let paddY = snd data.[rnd.Next(0,n)]
                    paddX,paddY)
                    |> Array.rev
            | BorderPaddingMethod.Zero -> 
                Array.init borderpadding (fun i -> 
                    let paddX = addToXValue minX (- (float i + 1.) * minDistance)
                    let paddY = 0.
                    paddX,paddY)
                    |> Array.rev
        ///adds 'borderpadding' number of random data points to the rigth
        let rightPadding    = 
            match borderPaddingMethod with
            | BorderPaddingMethod.Random -> 
                Array.init borderpadding (fun i -> 
                    let paddX = addToXValue maxX ((float i + 1.) * minDistance)
                    let paddY = snd data.[rnd.Next(0,n)]
                    paddX,paddY)
            | BorderPaddingMethod.Zero -> 
                Array.init borderpadding (fun i -> 
                    let paddX = addToXValue maxX ((float i + 1.) * minDistance)
                    let paddY = 0.
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
                    [data.[i]]::acc 
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
    
    module Discrete =

        
        /// <summary>Adds additional data points to the beginning and end of data set (number: borderpadding; y_Value: random).</summary>
        /// <remarks></remarks>
        /// <param name="data"></param>
        /// <param name="borderpadding"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let inline padRnd (data : 'a []) (borderpadding : int) =
            let rnd = System.Random()
            let n = data.Length
            ///adds 'borderpadding' number of random data points to the left
            let leftPadding     = 
                Array.init borderpadding (fun i -> 
                    let paddY = data.[rnd.Next(0,n)] //n+1
                    paddY)
                    |> Array.rev
            ///adds 'borderpadding' number of random data points to the rigth
            let rightPadding    = 
                Array.init borderpadding (fun i -> 
                    let paddY = data.[rnd.Next(0,n)] //n+1
                    paddY)

            [leftPadding;data;rightPadding] |> Array.concat

        /// <summary>Adds additional data points with value zero to the beginning and end of the given data.</summary>
        /// <remarks></remarks>
        /// <param name="data">An array of values</param>
        /// <param name="borderpadding">The number of points to add to each end</param>
        /// <returns></returns>
        /// <example>
        /// <code> 
        ///  let data = [|0.1; 0.2; 0.3; 0.4|]
        ///            
        ///  let padding = 2
        ///       
        ///  // padding the data points with 2 artificial zero-valued points on each side
        ///  let paddedData =Padding.Discrete.padZero data padding
        ///  
        /// </code> 
        /// </example>        
        let inline padZero (data : 'a []) (borderpadding : int) =
            let padding = 
                Array.zeroCreate borderpadding 
            [padding;data;padding] |> Array.concat


        module ThreeDimensional =
            
            type Padding3DMethod =
                /// <summary>
                /// Adds zeros to the Array2D borders 
                /// </summary>
                | Zero
                /// <summary>
                /// Adds random data points taken from the original data to the Array2D borders
                /// </summary>
                | Random
            
            /// <summary>
            /// Pads artificial data points to the borders of the given two dimensional array.
            /// </summary>
            /// <param name="data">A two dimensional array</param>
            /// <param name="borderpadding">The number of points to add to each side</param>
            /// <param name="paddingMethod">The padding method to use</param>
            /// <returns>A two dimensional array, containing the data of the original array padded with artificial data points on each side.</returns>
            /// <example> 
            /// <code> 
            ///  let data2D =
            ///            let rnd = System.Random()
            ///            Array2D.init 50 50 (fun i j -> 
            ///            if (i,j) = (15,15) then 5.
            ///            elif (i,j) = (35,35) then -5.
            ///            else rnd.NextDouble())
            /// 
            ///  let padding = 11
            ///       
            ///  // padding the data points with 11 artificial random points on each side
            ///  let paddedData2D =Padding.Discrete.ThreeDimensional.pad data2D padding Padding.Discrete.ThreeDimensional.Random
            ///  
            /// </code> 
            /// </example>
            let inline pad (data: 'a [,]) (borderpadding: int) (paddingMethod : Padding3DMethod) : float [,]=
                //TODO: change data input from float to 'a
                let rnd = System.Random()    

                let padArray2D =
                    let rowCount = Array2D.length1 data 
                    let colCount = Array2D.length2 data
                    let rowPadding = rowCount + borderpadding
                    let colPadding = colCount + borderpadding
                    Array2D.init (rowCount + borderpadding * 2) (colCount + borderpadding * 2)
                        (fun rowI colI -> 
                            if (rowI < borderpadding || colI < borderpadding) || (rowI >= rowPadding  || colI >= colPadding) then
                                match paddingMethod with
                                | Zero -> 0.
                                | _ -> float data.[rnd.Next(0,rowCount),rnd.Next(0,colCount)] 
                            else
                                float data.[rowI-borderpadding,colI-borderpadding]              
                        )
        
                padArray2D

////Example
//let rnd = System.Random()
//let data = 
//    Array.init 1000 (fun x -> 1000.* rnd.NextDouble(),rnd.NextDouble()) 
//    |> Array.sortBy fst

////let avgSpacing = HelperFunctions.getAvgSpacing data (-)
//let avgSpacing = HelperFunctions.getAvgSpacing data HelperFunctions.Float.getDiffFloat
////interpolate data point y-values when small gaps are present
//let innerPadMethod = Padding.InternalPaddingMethod.LinearInterpolation
////take random data point y-values when huge gaps are between data points
//let hugeGapPadMethod = Padding.HugeGapPaddingMethod.Random
////maximal allowed gap between datapoints where internalPaddingMethod can be applied.
////if internalPaddingMethod = hugeGapPaddingMethod, then it does not matter which value is chosen
//let maxSpacing = 0.05
////since were dealing with floats the functions are (-) and (+)
//let getDiffFu = HelperFunctions.Float.getDiffFloat      //(-)
//let addXValue = HelperFunctions.Float.addToXValueFloat  //(+)
////number of datapoints the dataset gets expanded to the left and to the rigth
//let borderpadding = 4000
////get the paddedDataSet
//let paddedData =
//    //if a gap is greater than 1000. the InternalPaddingMethod is applied
//    Padding.pad data avgSpacing maxSpacing getDiffFu addXValue borderpadding innerPadMethod hugeGapPadMethod