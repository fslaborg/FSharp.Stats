namespace FSharp.Stats

module Intervals =

    /// Closed interval [Start,End]
    type Interval<'a> = 
        | ClosedInterval of 'a * 'a    
        | Empty

    /// Creates closed interval [min,max] by given min and max
    let create min max =
        ClosedInterval (min, max)
        
    /// Creates closed interval [min,max] by given start and size
    let ofSize min size =
        ClosedInterval (min, min + size)
        
    //(duplicated from Seq module due to circular dependency)
    /// Creates closed interval of the given data based on its minimum and maximum 
    let ofSeq (source:seq<_>) = 
        use e = source.GetEnumerator()
        let rec loop minimum maximum =
            match e.MoveNext() with
            | true  -> loop (min e.Current minimum) (max e.Current maximum)
            | false -> create minimum maximum          
        //Init by fist value
        match e.MoveNext() with
        | true  -> loop e.Current e.Current
        | false -> Interval.Empty

    //(duplicated from Seq module due to circular dependency)
    /// Creates closed interval [min,max] of the given data based on the extreme values obtained by applying the projection function
    let ofSeqBy projection (source:seq<_>) =
        use e = source.GetEnumerator()
        let rec loop minimum maximum minimumV maximumV =
            match e.MoveNext() with
            | true  -> 
                let current = projection e.Current
                let mmin,mminV = if current < minimum then current,e.Current else minimum,minimumV
                let mmax,mmaxV = if current > maximum then current,e.Current else maximum,maximumV
                loop mmin mmax mminV mmaxV
            | false -> create minimumV maximumV          
        //Init by fist value
        match e.MoveNext() with
        | true  -> 
            let current = projection e.Current
            loop current current e.Current e.Current
        | false -> Interval.Empty

    /// Returns min and max value of Interval [min,max]
    let inline values (interval:Interval<'a>) =
        let zero = LanguagePrimitives.GenericZero< 'a >
        match interval with
        | ClosedInterval (min,max) -> min,max
        | Empty -> (zero / zero,zero / zero)
    
    /// Returns min/start value of Interval [min,max]
    let inline getStart (interval:Interval<'a>) =
        let zero = LanguagePrimitives.GenericZero< 'a >
        match interval with
        | ClosedInterval (min,_) -> min
        | Empty -> zero / zero

    /// Returns max/end value of Interval [min,max]
    let inline getEnd (interval:Interval<'a>) =
        let zero = LanguagePrimitives.GenericZero< 'a >
        match interval with
        | ClosedInterval (_,max) -> max
        | Empty -> zero / zero

    /// Returns range of of Interval [min,max] (max - min)
    let inline getRange (interval:Interval<'a>) =
        let zero = LanguagePrimitives.GenericZero< 'a >
        match interval with
        | ClosedInterval (min,max) -> max - min
        | Empty -> zero / zero

    /// Returns the size of an closed interval
    let inline trySize interval =
        match interval with
        | ClosedInterval (min,max) -> Some (max - min)
        | Empty -> None


    /// Returns the interval as a string
    let toString interval =
        match interval with
        | ClosedInterval (min,max) -> sprintf "[%A,%A]" min max
        | Empty -> "[empty]"
        

    /// Add two given intervals.
    let add a b =
        match a,b with
        | ClosedInterval (minA,maxA), ClosedInterval (minB,maxB) 
            -> ClosedInterval (minA + minB, maxA + maxB)
        | ClosedInterval (min,max), Empty -> a
        | Empty, ClosedInterval (min,max) -> b
        | Empty,Empty -> Empty
                
        

    /// Subtract a given interval from the other interval.
    let subtract a b =
        match a,b with
        | ClosedInterval (minA,maxA), ClosedInterval (minB,maxB) 
            -> ClosedInterval (minA - maxB, maxA - minB)
        | ClosedInterval (min,max), Empty -> a
        | Empty, ClosedInterval (min,max) -> b
        | Empty,Empty -> Empty
        
        
    // a0----a1
    //     b0-----b1
    /// Checking for intersection of both intervals
    let isIntersection a b =
        match a,b with
        | ClosedInterval (minA,maxA), ClosedInterval (minB,maxB) 
            -> minA <= maxB && minB <= maxA
        | ClosedInterval (min,max), Empty -> false
        | Empty, ClosedInterval (min,max) -> false
        | Empty,Empty -> true
        

    /// Returns the intersection of this interval with another.
    let intersect a b =
        if not (isIntersection a b) then
            None
        else
            match a,b with
            | ClosedInterval (minA,maxA), ClosedInterval (minB,maxB) 
                -> if not (minA <= maxB && minB <= maxA) then
                        None
                   else
                        let min' = max minA minB
                        let max' = min maxA maxB
                        ClosedInterval (min',max') |> Some
            | ClosedInterval (min,max), Empty -> None
            | Empty, ClosedInterval (min,max) -> None
            | Empty,Empty -> Some (Empty)

            

    /// Get the value at a given percentage within (0.0 - 1.0) or outside (< 0.0, > 1.0) of the interval. Rounding to nearest neighbour occurs when needed.
    let inline getValueAt percentage interval =        
        match trySize interval with
        | Some size -> percentage * (float size)
        | None      -> nan
       

    ///   Does the given value lie in the interval or not.
    let liesInInterval value interval =
        match interval with
        | ClosedInterval (min,max) -> value >= min && value <= max                                      
        | Empty -> false        
        
        

// ####################################################

// interval tree
//http://www.geeksforgeeks.org/interval-tree/
//  https://fgiesen.wordpress.com/2011/10/16/checking-for-interval-overlap/
// https://github.com/Whathecode/Framework-Class-Library-Extension/blob/master/Whathecode.System/Arithmetic/Range/Interval.cs


///// <summary> 
/////   Get a percentage how far inside (0.0 - 1.0) or outside (< 0.0, > 1.0) the interval a certain value lies. 
/////   For single intervals, '1.0' is returned when inside the interval, '-1.0' otherwise. 
///// </summary> 
///// <param name = "position">The position value to get the percentage for.</param> 
///// <returns>The percentage indicating how far inside (or outside) the interval the given value lies.</returns> 
//let getPercentageFor position r =
//    let inside = liesInInterval position r
//    let sizeR  = size r
//    if (sizeR = 0.0) then
//        if inside then 1.0 else -1.0
//    else
//        let rangeP = create r.Start position
//        size rangeP / sizeR 
//
///// Map a value from the source range, to a value in another range (target) linearly.        
//let map source target value =
//    let tmp = getPercentageFor value source
//    getValueAt tmp target
//
///// Limit a given value to the range of the intertval. When the value is smaller/bigger than the range, snap it to the range border.
//let clampSingelton value r =
//    if value < r.Start then r.Start
//    elif value > r.End then r.End 
//    else value 
//
//
// 
///// Limit the target range to the source range. 
///// When part of the given range lies outside of this range, it isn't included in the resulting range. 
//let clamp source target =
//    failwith "not implemented"
//    
//
///// Split the interval into two intervals at the given point, or nearest valid point.
//let split atPoint interval =
//    failwith "not implemented"
//
//
//
//
//
///// Get values for each step within the interval.
//let getValues stepSize interval =
//    let rec gen c =
//        seq { 
//            if c <= interval.End then
//                let uc = c + stepSize
//                yield uc
//                yield! gen uc 
//                                 }
//    gen interval.Start
//
//
///// Returns a reversed version of the current interval, swapping the start position with the end position.
//let reverse interval =
//    create interval.End interval.Start
//
///// Checks if interval is reversed
//let isReversed interval =
//    interval.End < interval.Start
//
///// Returns an interval offsetted from the current interval by a specified amount.
//let move amount interval =
//    create (interval.Start + amount) (interval.End + amount)
//
///// <summary> 
/////   Returns a scaled version of the current interval, but prevents the interval from exceeding the values specified in a passed limit. 
/////   This is useful to prevent <see cref="ArgumentOutOfRangeException" /> during calculations for certain types. 
///// </summary> 
///// <param name="scale"> 
/////   Percentage to scale the interval up or down. 
/////   Smaller than 1.0 to scale down, larger to scale up. 
///// </param> 
///// <param name="limit">The limit which the interval snaps to when scaling exceeds it.</param> 
///// <param name="aroundPercentage">The percentage inside the interval around which to scale.</param> 
//let scale a =
//    failwith "not implemented"
//
/////<summary> 
/////   Returns an expanded interval of the current interval up to the given value. 
/////   When the value lies within the interval the returned interval is the same. 
///// </summary> 
///// <param name = "value">The value up to which to expand the interval.</param> 
///// <param name = "include">Include the value to which is expanded in the interval.</param> 
//let expandTo a =
//    failwith "not implemented"
//
//let r1 = create 1.5 2.5
//let r2 = create 2.0 4.5
//    
//let r3 = create 5.0 6.0
//
//
//getPercentageFor 2.6 r1


