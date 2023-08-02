namespace FSharp.Stats

open System

/// Closed interval [Start,End]
[<RequireQualifiedAccess>]
type Interval<'a when 'a : comparison> = 

    /// <summary>[start,end] includes endpoints</summary>
    | Closed    of 'a * 'a
    /// <summary>(start,end] includes right endpoints</summary>
    | LeftOpen  of 'a * 'a
    /// <summary>[start,end) includes leftendpoints</summary>
    | RightOpen of 'a * 'a
    /// <summary>(start,end) endpoints are excluded</summary>
    | Open      of 'a * 'a
    | Empty

        ///   Does the given value lie in the interval or not.
    member inline this.liesInInterval value =
        match this with
        | Interval.Closed    (min,max) -> value >= min && value <= max
        | Interval.Open      (min,max) -> value >  min && value <  max
        | Interval.LeftOpen  (min,max) -> value >  min && value <= max
        | Interval.RightOpen (min,max) -> value >= min && value <  max
        | Empty -> false
    

    member inline this.TryStart = 
        match this with
        | Closed    (min,_) -> Some min
        | LeftOpen  (min,_) -> Some min
        | RightOpen (min,_) -> Some min
        | Open      (min,_) -> Some min
        | Empty -> None
        
    member inline this.TryEnd = 
        match this with
        | Closed     (_,max) -> Some max
        | LeftOpen   (_,max) -> Some max
        | RightOpen  (_,max) -> Some max
        | Open       (_,max) -> Some max
        | Empty -> None

    member inline this.TryToTuple = 
        match this with 
        | Closed     (min,max) -> Some (min,max)
        | LeftOpen   (min,max) -> Some (min,max)
        | RightOpen  (min,max) -> Some (min,max)
        | Open       (min,max) -> Some (min,max)
        | Empty -> None


    member inline this.ToTuple() = 
        match this with 
        | Closed     (min,max) -> (min,max)
        | LeftOpen   (min,max) -> (min,max)
        | RightOpen  (min,max) -> (min,max)
        | Open       (min,max) -> (min,max)
        | Empty -> failwithf "Interval was empty!"
        
    member inline this.GetStart() = 
        match this with
        | Closed     (min,_) -> min
        | LeftOpen   (min,_) -> min
        | RightOpen  (min,_) -> min
        | Open       (min,_) -> min
        | Empty -> failwithf "Interval was empty!"
        
    member inline this.GetEnd() = 
        match this with
        | Closed     (_,max) -> max
        | LeftOpen   (_,max) -> max
        | RightOpen  (_,max) -> max
        | Open       (_,max) -> max
        | Empty -> failwithf "Interval was empty!"

    static member inline CreateClosed (min,max) =     
        //if min > max then failwithf "Interval start must be lower or equal to interval end!" //[1,2,3] < [2,1,4] returns true but is invalid!
        Closed (min,max)

    static member inline CreateLeftOpen (min,max) =     
        //if min >= max then failwithf "Interval start must be lower than interval end!" //[1,2,3] < [2,1,4] returns true but is invalid!
        LeftOpen (min,max)

    static member inline CreateRightOpen (min,max) =     
        //if min >= max then failwithf "Interval start must be lower than interval end!" //[1,2,3] < [2,1,4] returns true but is invalid!
        RightOpen (min,max)
        
    static member inline CreateOpen (min,max) =     
        //if min >= max then failwithf "Interval start must be lower than interval end!" //[1,2,3] < [2,1,4] returns true but is invalid!
        Open (min,max)

    static member inline ofSeqBy (projection:'a -> 'b) (source:seq<'a>) =
        use e = source.GetEnumerator()
        //Init by fist value
        match e.MoveNext() with
        | true  -> 
            let current = projection e.Current
            let  isfloat = box current :? float
            //inner loop 
            let rec loop minimum maximum minimumV maximumV =
                match e.MoveNext() with
                | true  -> 
                    let current = projection e.Current
                    // fail if collection contains nan
                    if isfloat && isNan current then 
                        //Interval.Empty 
                        raise (System.Exception("Interval cannot be determined if collection contains nan"))
                    else
                        let mmin,mminV = if current <  minimum then current,e.Current else minimum,minimumV
                        let mmax,mmaxV = if current >= maximum then current,e.Current else maximum,maximumV
                        loop mmin mmax mminV mmaxV
                | false -> Interval.Closed (minimumV,maximumV)
            loop current current e.Current e.Current
        | false -> Interval.Empty

    static member inline ofSeq (source:seq<'a>) = 
        Interval.ofSeqBy id source

    /// Returns the interval as a string
    override this.ToString() =
        match this with
        | Interval.Closed    (min,max) -> sprintf "[%A,%A]" min max
        | Interval.Open      (min,max) -> sprintf "(%A,%A)" min max
        | Interval.LeftOpen  (min,max) -> sprintf "(%A,%A]" min max
        | Interval.RightOpen (min,max) -> sprintf "[%A,%A)" min max
        | Empty -> "[empty]"




module Interval =

    [<Obsolete("Use Interval.CreateClosed instead")>]
    let inline create min max = 
        Interval.Closed (min,max)
        
    let inline values (interval: Interval<'a>) = 
        interval.ToTuple()
        
    let inline getStart (interval: Interval<'a>) =
        interval.GetStart()

    let inline getEnd (interval: Interval<'a>) =
        interval.GetEnd()

    /// Creates closed interval [min,max] by given start and size
    let inline createClosedOfSize min size =
        Interval.Closed (min, min + size)

    /// Creates open interval (min,max) by given start and size
    let inline createOpenOfSize (min: 'a) (size: 'a): Interval<'a>=
        let z = LanguagePrimitives.GenericZero< 'a >
        if size = z then 
            Interval.Empty 
        else Interval.Open (min, min + size)

    /// Creates closed interval [min,max] by given start and size
    let inline createLeftOpenOfSize min size =
        let z = LanguagePrimitives.GenericZero< 'a >
        if size = z then 
            Interval.Empty 
        else Interval.LeftOpen (min, min + size)

    /// Creates closed interval [min,max] by given start and size
    let inline createRightOpenOfSize min size =
        let z = LanguagePrimitives.GenericZero< 'a >
        if size = z then 
            Interval.Empty 
        else Interval.RightOpen (min, min + size)

    /// Returns the size of an Interval [min,max] (max - min)
    let inline getSize interval =
        let z = LanguagePrimitives.GenericZero< 'a >
        match interval with
        | Interval.Closed (min,max) -> max - min
        | Interval.Open (min,max) -> max - min
        | Interval.LeftOpen (min,max) -> max - min
        | Interval.RightOpen (min,max) -> max - min
        | Interval.Empty -> z / z
    
    /// Returns the range of an Interval [min,max] (projection max - projection min)
    let inline getSizeBy (projection:'a -> 'b) (interval: Interval<'a>) =
        let zero = LanguagePrimitives.GenericZero< 'b >
        match interval with
        | Interval.Closed    (min,max) -> projection max - projection min
        | Interval.Open      (min,max) -> projection max - projection min
        | Interval.LeftOpen  (min,max) -> projection max - projection min
        | Interval.RightOpen (min,max) -> projection max - projection min
        | Interval.Empty -> zero / zero
        
    /// Returns the size of an closed interval
    let inline trySize interval =
        match interval with
        | Interval.Closed    (min,max) -> Some(max - min)
        | Interval.Open      (min,max) -> Some(max - min)
        | Interval.LeftOpen  (min,max) -> Some(max - min)
        | Interval.RightOpen (min,max) -> Some(max - min)
        | Interval.Empty -> None

    /// Add two given intervals. 
    let inline add (a: Interval<'a>) b =
        match a,b with
        | Interval.Closed (minA,maxA), Interval.Closed (minB,maxB) 
            -> Interval.Closed (minA + minB, maxA + maxB)
        | Interval.Closed (min,max), Interval.Empty -> a
        | Interval.Empty, Interval.Closed (min,max) -> b
        | Interval.Empty,Interval.Empty -> Interval.Empty
        | _ -> failwithf "Addition of (half) open intervals is not supported!"
                
    /// Subtract a given interval from the other interval.
    let inline subtract (a: Interval<'a>) b =
        match a,b with
        | Interval.Closed (minA,maxA), Interval.Closed (minB,maxB) 
            -> Interval.Closed (minA - maxB, maxA - minB)
        | Interval.Closed (min,max), Interval.Empty -> a
        | Interval.Empty, Interval.Closed (min,max) -> b
        | Interval.Empty,Interval.Empty -> Interval.Empty
        | _ -> failwithf "Subtraction of (half) open intervals is not supported!"
        
    // a0----a1
    //     b0-----b1
    /// Checking for intersection of both intervals
    let inline isIntersection (a: Interval<'a>) b =
        match a,b with
        | Interval.Closed (minA,maxA), Interval.Closed (minB,maxB) -> minA <= maxB && minB <= maxA
        | Interval.Closed (minA,maxA), Interval.Open (minB,maxB) -> minA < maxB && minB < maxA
        | Interval.Closed (minA,maxA), Interval.RightOpen (minB,maxB) -> minA < maxB && minB <= maxA
        | Interval.Closed (minA,maxA), Interval.LeftOpen (minB,maxB) -> minA <= maxB && minB < maxA
        | Interval.Closed (_,_), Interval.Empty -> false
        | Interval.Open (minA,maxA), Interval.Closed (minB,maxB) -> minA < maxB && minB < maxA
        | Interval.Open (minA,maxA), Interval.Open (minB,maxB) -> minA < maxB && minB < maxA
        | Interval.Open (minA,maxA), Interval.RightOpen (minB,maxB) -> minA < maxB && minB < maxA
        | Interval.Open (minA,maxA), Interval.LeftOpen (minB,maxB) -> minA < maxB && minB < maxA
        | Interval.Open (_,_), Interval.Empty -> false
        | Interval.LeftOpen (minA,maxA), Interval.Closed (minB,maxB) -> minA < maxB && minB <= maxA
        | Interval.LeftOpen (minA,maxA), Interval.Open (minB,maxB) -> minA < maxB && minB < maxA
        | Interval.LeftOpen (minA,maxA), Interval.RightOpen (minB,maxB) -> minA <= maxB && minB <= maxA
        | Interval.LeftOpen (minA,maxA), Interval.LeftOpen (minB,maxB) -> minA < maxB && minB < maxA
        | Interval.LeftOpen (_,_), Interval.Empty -> false
        | Interval.RightOpen (minA,maxA), Interval.Closed (minB,maxB) -> minA <= maxB && minB < maxA
        | Interval.RightOpen (minA,maxA), Interval.Open (minB,maxB) -> minA < maxB && minB < maxA
        | Interval.RightOpen (minA,maxA), Interval.RightOpen (minB,maxB) -> minA < maxB && minB < maxA
        | Interval.RightOpen (minA,maxA), Interval.LeftOpen (minB,maxB) -> minA <= maxB && minB <= maxA
        | Interval.RightOpen (_,_), Interval.Empty -> false
        | Interval.Empty, Interval.Closed (_,_) -> false
        | Interval.Empty, Interval.Open (_,_) -> false
        | Interval.Empty, Interval.LeftOpen (_,_) -> false
        | Interval.Empty, Interval.RightOpen (_,_) -> false
        | Interval.Empty,Interval.Empty -> true
        

    /// Returns the intersection of this interval with another.
    let inline intersect (a: Interval<'a>) b =
        if not (isIntersection a b) then
            Interval.Empty
        else
            match a,b with
            | Interval.Closed (minA,maxA), Interval.Closed (minB,maxB) -> 
                if not (minA <= maxB && minB <= maxA) then
                        Interval.Empty
                else
                    let min' = max minA minB
                    let max' = min maxA maxB
                    Interval.Closed (min',max') 
            | Interval.Closed (min,max), Interval.Empty -> Interval.Empty
            | Interval.Empty, Interval.Closed (min,max) -> Interval.Empty
            | Interval.Empty,Interval.Empty -> Interval.Empty
            | Interval.LeftOpen (minA,maxA), Interval.LeftOpen (minB,maxB) -> 
                if not (minA < maxB && minB < maxA) then
                        Interval.Empty
                else
                    let min' = max minA minB
                    let max' = min maxA maxB
                    Interval.LeftOpen (min',max')
            | Interval.RightOpen (minA,maxA), Interval.RightOpen (minB,maxB) ->
                if not (minA < maxB && minB < maxA) then
                        Interval.Empty
                else
                    let min' = max minA minB
                    let max' = min maxA maxB
                    Interval.RightOpen (min',max')
            | Interval.Open (minA,maxA), Interval.Open (minB,maxB) ->
                if not (minA < maxB && minB < maxA) then
                        Interval.Empty
                else
                    let min' = max minA minB
                    let max' = min maxA maxB
                    Interval.Open (min',max')
            | _ -> failwithf "Intersection of mixed interval types is not supported!"

    /// Get the value at a given percentage within (0.0 - 1.0) or outside (&lt; 0.0, &gt; 1.0) of the interval. Rounding to nearest neighbour occurs when needed.
    let inline getValueAt percentage (interval: Interval<'a>) =        
        match trySize interval with
        | Some size -> float (interval.GetStart()) + percentage * float size
        | None      -> nan

// ####################################################

// interval tree
//http://www.geeksforgeeks.org/interval-tree/
//  https://fgiesen.wordpress.com/2011/10/16/checking-for-interval-overlap/
// https://github.com/Whathecode/Framework-Class-Library-Extension/blob/master/Whathecode.System/Arithmetic/Range/Interval.cs


///// <summary> 
/////   Get a percentage how far inside (0.0 - 1.0) or outside (&lt; 0.0, &gt; 1.0) the interval a certain value lies. 
/////   For single intervals, '1.0' is returned when inside the interval, '-1.0' otherwise. 
///// </summary> 
///// <param name="position">The position value to get the percentage for.</param> 
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
/////   This is useful to prevent ArgumentOutOfRangeException during calculations for certain types. 
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
///// <param name="value">The value up to which to expand the interval.</param> 
///// <param name="include">Include the value to which is expanded in the interval.</param> 
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


