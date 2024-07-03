namespace FSharp.Stats

open System

/// <summary>Represents an interval between two values of type 'a, which can be either inclusive or exclusive.</summary>
/// <typeparam name="'a">The type of the interval values, must support comparison.</typeparam>
[<RequireQualifiedAccess>]
type Interval<'a when 'a : comparison> = 
    /// <summary>Represents a closed interval [start,end] that includes endpoints</summary>
    /// <param name="intervalStart">The start value of the interval</param>
    /// <param name="intervalEnd">The end value of the interval</param>
    | Closed    of intervalStart:'a * intervalEnd:'a
    /// <summary>Represents an interval (start,end] that includes the right endpoint.</summary>
    /// <param name="intervalStart">The start value of the interval</param>
    /// <param name="intervalEnd">The end value of the interval</param>
    | LeftOpen  of intervalStart:'a * intervalEnd:'a
    /// <summary>Represents an interval [start,end) that includes the left endpoint.</summary>
    /// <param name="intervalStart">The start value of the interval</param>
    /// <param name="intervalEnd">The end value of the interval</param>
    | RightOpen of intervalStart:'a * intervalEnd:'a
    /// <summary>Represents the open interval (start,end).</summary>
    /// <param name="intervalStart">The start value of the interval</param>
    /// <param name="intervalEnd">The end value of the interval</param>
    | Open      of intervalStart:'a * intervalEnd:'a
    /// <summary>Represents an empty interval</summary>
    | Empty

    /// <summary>Checks if the given value lies in the interval or not.</summary>
    /// <param name="value">The value to check for containment in the interval</param>
    /// <returns>True if the value is contained in the interval, false otherwise</returns>
    /// <example>
    /// <code>
    /// // Create a closed interval [2.0, 7.0]
    /// let interval = Interval.CreateClosed(2.0, 7.0)
    /// // Check if the value 5.0 lies in the interval (true)
    /// interval.liesInInterval 5.0
    /// </code>
    /// </example>
    member inline this.liesInInterval value =
        match this with
        | Interval.Closed    (min,max) -> value >= min && value <= max
        | Interval.Open      (min,max) -> value >  min && value <  max
        | Interval.LeftOpen  (min,max) -> value >  min && value <= max
        | Interval.RightOpen (min,max) -> value >= min && value <  max
        | Empty -> false
    
    /// <summary>Returns an option containing the start value of the interval, if it exists</summary>
    /// <example>
    /// <code>
    /// // Create a closed interval [2.0, 7.0]
    /// let interval = Interval.CreateClosed(2.0, 7.0)
    /// // Get the start value of the interval Some(2.0)
    /// interval.TryStart
    /// </code>
    /// </example>
    member inline this.TryStart = 
        match this with
        | Closed    (min,_) -> Some min
        | LeftOpen  (min,_) -> Some min
        | RightOpen (min,_) -> Some min
        | Open      (min,_) -> Some min
        | Empty -> None

    /// <summary>Returns an option containing the end value of the interval, if it exists</summary>
    /// <example>
    /// <code>
    /// // Create a closed interval [2.0, 7.0]
    /// let interval = Interval.CreateClosed(2.0, 7.0)
    /// // Get the end value of the interval Some(7.0)
    /// interval.TryEnd
    /// </code>
    /// </example>
    member inline this.TryEnd = 
        match this with
        | Closed     (_,max) -> Some max
        | LeftOpen   (_,max) -> Some max
        | RightOpen  (_,max) -> Some max
        | Open       (_,max) -> Some max
        | Empty -> None

    /// <summary>Returns an option containing a tuple of the start and end values of the interval, if they exist</summary>
    /// <example>
    /// <code>
    /// // Create a closed interval [2.0, 7.0]
    /// let interval = Interval.CreateClosed(2.0, 7.0)
    /// // Get the start and end values of the interval Some((2.0, 7.0))
    /// interval.TryToTuple()
    /// </code>
    /// </example>
    member inline this.TryToTuple = 
        match this with 
        | Closed     (min,max) -> Some (min,max)
        | LeftOpen   (min,max) -> Some (min,max)
        | RightOpen  (min,max) -> Some (min,max)
        | Open       (min,max) -> Some (min,max)
        | Empty -> None

    /// <summary>Returns a tuple of the start and end values of the interval</summary>
    /// <exception cref="System.InvalidOperationException">Thrown when the interval is empty</exception>
    /// <example>
    /// <code>
    /// // Create a closed interval [2.0, 7.0]
    /// let interval = Interval.CreateClosed(2.0, 7.0)
    /// // Get the start and end values of the interval (2.0, 7.0)
    /// interval.ToTuple()
    /// </code>
    /// </example>
    member inline this.ToTuple() = 
        match this with 
        | Closed     (min,max) -> (min,max)
        | LeftOpen   (min,max) -> (min,max)
        | RightOpen  (min,max) -> (min,max)
        | Open       (min,max) -> (min,max)
        | Empty -> invalidOp "Cannot convert Interval.Empty to a tuple."
        
    /// <summary>Returns the start value of the interval</summary>
    /// <exception cref="System.InvalidOperationException">Thrown when the interval is empty</exception>
    /// <example>
    /// <code>
    /// // Create a closed interval [2.0, 7.0]
    /// let interval = Interval.CreateClosed(2.0, 7.0)
    /// // Get the start value of the interval (2.0)
    /// interval.GetStart()
    /// </code>
    /// </example>
    member inline this.GetStart() = 
        match this with
        | Closed     (min,_) -> min
        | LeftOpen   (min,_) -> min
        | RightOpen  (min,_) -> min
        | Open       (min,_) -> min
        | Empty -> invalidOp "Cannot GetStart of Interval.Empty"
        
    /// <summary>Returns the end value of the interval</summary>
    /// <exception cref="System.InvalidOperationException">Thrown when the interval is empty</exception>
    /// <example>
    /// <code>
    /// // Create a closed interval [2.0, 7.0]
    /// let interval = Interval.CreateClosed(2.0, 7.0)
    /// // Get the end value of the interval (7.0)  
    /// interval.GetEnd()
    /// </code>
    /// </example>
    member inline this.GetEnd() = 
        match this with
        | Closed     (_,max) -> max
        | LeftOpen   (_,max) -> max
        | RightOpen  (_,max) -> max
        | Open       (_,max) -> max
        | Empty -> invalidOp "Cannot GetEnd of Interval.Empty"

    /// <summary>Creates a closed interval [min,max]</summary>
    /// <param name="min">The start value of the interval</param>
    /// <param name="max">The end value of the interval</param>
    /// <returns>A closed interval</returns>
    static member inline CreateClosed (min,max) =     
        //if min > max then failwithf "Interval start must be lower or equal to interval end!" //[1,2,3] < [2,1,4] returns true but is invalid!
        Closed (min,max)

    /// <summary>Creates a left-open interval (min,max]</summary>
    /// <param name="min">The start value of the interval</param>
    /// <param name="max">The end value of the interval</param>
    /// <returns>A left-open interval</returns>
    static member inline CreateLeftOpen (min,max) =     
        //if min >= max then failwithf "Interval start must be lower than interval end!" //[1,2,3] < [2,1,4] returns true but is invalid!
        LeftOpen (min,max)

    /// <summary>Creates a right-open interval [min,max)</summary>
    /// <param name="min">The start value of the interval</param>
    /// <param name="max">The end value of the interval</param>
    /// <returns>A right-open interval</returns>
    static member inline CreateRightOpen (min,max) =     
        //if min >= max then failwithf "Interval start must be lower than interval end!" //[1,2,3] < [2,1,4] returns true but is invalid!
        RightOpen (min,max)
        
    /// <summary>Creates an open interval (min,max)</summary>
    /// <param name="min">The start value of the interval</param>
    /// <param name="max">The end value of the interval</param>
    /// <returns>An open interval</returns>
    static member inline CreateOpen (min,max) =     
        //if min >= max then failwithf "Interval start must be lower than interval end!" //[1,2,3] < [2,1,4] returns true but is invalid!
        Open (min,max)

    /// <summary>Creates an interval from a sequence of values using a projection function</summary>
    /// <param name="projection">A function to project each element of the sequence to the desired type</param>
    /// <param name="source">The input sequence of values</param>
    /// <returns>A closed interval containing the minimum and maximum projected values</returns>
    /// <exception cref="System.InvalidOperationException">Thrown when the input sequence contains NaN values</exception>
    /// <example>
    /// <code>
    /// // "hello" is a char seq whos interval is [e,o]
    /// Interval.ofSeqBy int "hello" // Closed('e','o')
    /// </code>
    /// </example>
    static member inline ofSeqBy (projection:'a -> 'b) (source:seq<'a>) =
        use e = source.GetEnumerator()
        //Init by fist value
        match e.MoveNext() with
        | true  -> 
            let current = projection e.Current
            let isfloat = box current :? float
            //inner loop 
            let rec loop minimum maximum minimumV maximumV =
                match e.MoveNext() with
                | true  -> 
                    let current = projection e.Current
                    // fail if collection contains nan
                    if isfloat && Ops.isNan current then 
                        //Interval.Empty 
                        invalidOp "Interval cannot be determined if collection contains nan"
                    else
                        let mmin,mminV = if current <  minimum then current,e.Current else minimum,minimumV
                        let mmax,mmaxV = if current >= maximum then current,e.Current else maximum,maximumV
                        loop mmin mmax mminV mmaxV
                | false -> Interval.Closed (minimumV,maximumV)
            loop current current e.Current e.Current
        | false -> Interval.Empty

    /// <summary>Creates an interval from a sequence of values</summary>
    /// <param name="source">The input sequence of values</param>
    /// <returns>A closed interval containing the minimum and maximum values</returns>
    /// <example>
    /// <code>
    /// // Get the interval of [5.0; 10.0; 7.0]
    /// Interval.ofSeq [5.0; 10.0; 7.0] // Closed(5.0,10.0)
    /// </code>
    /// </example>
    static member inline ofSeq (source:seq<'a>) = 
        Interval.ofSeqBy id source

    /// <summary>Returns a string representation of the interval</summary>
    /// <returns>A string representing the interval</returns>
    override this.ToString() =
        match this with
        | Interval.Closed    (min,max) -> sprintf "[%A,%A]" min max
        | Interval.Open      (min,max) -> sprintf "(%A,%A)" min max
        | Interval.LeftOpen  (min,max) -> sprintf "(%A,%A]" min max
        | Interval.RightOpen (min,max) -> sprintf "[%A,%A)" min max
        | Empty -> "[empty]"




module Interval =

    /// <summary>[Obsolete] Creates a closed interval [min,max]. Use Interval.CreateClosed instead.</summary>
    /// <param name="min">The start value of the interval</param>
    /// <param name="max">The end value of the interval</param>
    /// <returns>A closed interval</returns>
    [<Obsolete("Use Interval.CreateClosed instead")>]
    let inline create min max = 
        Interval.Closed (min,max)
        
    /// <summary>Returns a tuple of the start and end values of the interval</summary>
    /// <param name="interval">The input interval</param>
    /// <returns>A tuple of the start and end values</returns>
    /// <example>
    /// <code>
    /// // Create a closed interval [2.0, 7.0]
    /// let interval = Interval.CreateClosed(2.0, 7.0)
    /// // Get the start and end values of the interval (2.0, 7.0)
    /// Interval.values interval
    /// </code>
    /// </example>
    let inline values (interval: Interval<'a>) = 
        interval.ToTuple()
        
    /// <summary>Returns the start value of the interval</summary>
    /// <param name="interval">The input interval</param>
    /// <returns>The start value of the interval</returns>
    /// <example>
    /// <code>
    /// // Create a closed interval [2.0, 7.0]
    /// let interval = Interval.CreateClosed(2.0, 7.0)
    /// // Get the start value of the interval (2.0)
    /// Interval.getStart interval
    /// </code>
    /// </example>
    let inline getStart (interval: Interval<'a>) =
        interval.GetStart()

    /// <summary>Returns the end value of the interval</summary>
    /// <param name="interval">The input interval</param>
    /// <returns>The end value of the interval</returns>
    /// <example>
    /// <code>
    /// // Create a closed interval [2.0, 7.0]
    /// let interval = Interval.CreateClosed(2.0, 7.0)
    /// // Get the end value of the interval (7.0)
    /// Interval.getEnd interval
    /// </code>
    /// </example>
    let inline getEnd (interval: Interval<'a>) =
        interval.GetEnd()

    /// <summary>Creates a closed interval [min, min + size]</summary>
    /// <param name="min">The start value of the interval</param>
    /// <param name="size">The size of the interval</param>
    /// <returns>A closed interval</returns>
    /// <example>
    /// <code>
    /// // Create a closed interval of size 5.0 starting at 2.0: [2.0, 7.0]
    /// Interval.createClosedOfSize 2.0 5.0
    /// </code>
    /// </example>
    let inline createClosedOfSize min size =
        Interval.Closed (min, min + size)

    /// <summary>Creates an open interval (min, min + size)</summary>
    /// <param name="min">The start value of the interval</param>
    /// <param name="size">The size of the interval</param>
    /// <returns>An open interval if size is non-zero, otherwise an empty interval</returns>
    /// <example>
    /// <code>
    /// // Create an open interval of size 5.0 starting at 2.0: (2.0, 7.0)
    /// Interval.createOpenOfSize 2.0 5.0
    /// </code>
    /// </example>
    let inline createOpenOfSize min size =
        if size = LanguagePrimitives.GenericZero then 
            Interval.Empty 
        else Interval.Open (min, min + size)

    /// <summary>Creates a left-open interval (min, min + size]</summary>
    /// <param name="min">The start value of the interval</param>
    /// <param name="size">The size of the interval</param>
    /// <returns>A left-open interval if size is non-zero, otherwise an empty interval</returns>
    /// <example>
    /// <code>
    /// // Create a left-open interval of size 5.0 starting at 2.0: (2.0, 7.0]
    /// Interval.createLeftOpenOfSize 2.0 5.0
    /// </code>
    /// </example>
    let inline createLeftOpenOfSize min size =
        if size = LanguagePrimitives.GenericZero then 
            Interval.Empty 
        else Interval.LeftOpen (min, min + size)

    /// <summary>Creates a right-open interval [min, min + size)</summary>
    /// <param name="min">The start value of the interval</param>
    /// <param name="size">The size of the interval</param>
    /// <returns>A right-open interval if size is non-zero, otherwise an empty interval</returns>
    /// <example>
    /// <code>
    /// // Create a right-open interval of size 5.0 starting at 2.0: [2.0, 7.0)
    /// Interval.createRightOpenOfSize 2.0 5.0
    /// </code>
    /// </example>
    let inline createRightOpenOfSize min size =
        if size = LanguagePrimitives.GenericZero then 
            Interval.Empty 
        else Interval.RightOpen (min, min + size)

    /// <summary>Returns the size of the interval (max - min)</summary>
    /// <param name="interval">The input interval</param>
    /// <returns>The size of the interval</returns>
    /// <exception cref="System.DivideByZeroException">Thrown when the interval is empty and a NaN value does not exist for the type</exception>
    /// <example>
    /// <code>
    /// // Create an open interval (2.0, 10.0)
    /// let interval = Interval.CreateOpen(2.0, 10.0)
    /// // Get the size of the interval (8.0)
    /// Interval.getSize interval
    /// </code>
    /// </example>
    let inline getSize interval =
        let z = LanguagePrimitives.GenericZero
        match interval with
        | Interval.Closed (min,max)
        | Interval.Open (min,max)
        | Interval.LeftOpen (min,max)
        | Interval.RightOpen (min,max) -> max - min
        | Interval.Empty -> z / z
    
    /// <summary>Returns the size of the interval after applying a projection function (projection max - projection min)</summary>
    /// <param name="projection">A function to project the interval values to the desired type</param>
    /// <param name="interval">The input interval</param>
    /// <returns>The size of the projected interval</returns>
    /// <exception cref="System.DivideByZeroException">Thrown when the interval is empty and a NaN value does not exist for the type</exception>
    /// <example>
    /// <code>
    /// // Create a closed interval ['a', 'c']
    /// let interval = Interval.CreateClosed('a', 'c')
    /// // Get the size of the interval using the ASCII values of the characters (3)
    /// Interval.getSizeBy int interval
    /// </code>
    /// </example>
    let inline getSizeBy (projection:'a -> 'b) (interval: Interval<'a>) =
        let zero = LanguagePrimitives.GenericZero< 'b >
        match interval with
        | Interval.Closed (min,max)
        | Interval.Open (min,max)
        | Interval.LeftOpen (min,max)
        | Interval.RightOpen (min,max) -> projection max - projection min
        | Interval.Empty -> zero / zero
        
    /// <summary>Returns an option containing the size of the interval, if it exists</summary>
    /// <param name="interval">The input interval</param>
    /// <returns>An option containing the size of the interval, or None if the interval is empty</returns>
    /// <example>
    /// <code>
    /// // Create an open interval (2.0, 10.0)
    /// let interval = Interval.CreateOpen(2.0, 10.0)
    /// // Get the size of the interval Some(8.0)
    /// Interval.trySize interval
    /// </code>
    /// </example>
    let inline trySize interval =
        match interval with
        | Interval.Closed (min,max)
        | Interval.Open (min,max)
        | Interval.LeftOpen (min,max)
        | Interval.RightOpen (min,max) -> Some(max - min)
        | Interval.Empty -> None

    /// <summary>Adds two intervals</summary>
    /// <param name="a">The first interval</param>
    /// <param name="b">The second interval</param>
    /// <returns>The sum of the two intervals</returns>
    /// <exception cref="System.InvalidOperationException ">Thrown when trying to add (half) open intervals</exception>
    let inline add a b =
        match a,b with
        | Interval.Closed (minA,maxA), Interval.Closed (minB,maxB) 
            -> Interval.Closed (minA + minB, maxA + maxB)
        | Interval.Closed (min,max), Interval.Empty -> a
        | Interval.Empty, Interval.Closed (min,max) -> b
        | Interval.Empty,Interval.Empty -> Interval.Empty
        | _ -> invalidOp "Addition of (half) open intervals is not supported!"
                
    /// <summary>Subtracts one interval from another</summary>
    /// <param name="a">The first interval</param>
    /// <param name="b">The interval to subtract</param>
    /// <returns>The difference of the two intervals</returns>
    /// <exception cref="System.InvalidOperationException">Thrown when trying to subtract (half) open intervals</exception>
    let inline subtract a b =
        match a,b with
        | Interval.Closed (minA,maxA), Interval.Closed (minB,maxB) 
            -> Interval.Closed (minA - maxB, maxA - minB)
        | Interval.Closed (min,max), Interval.Empty -> a
        | Interval.Empty, Interval.Closed (min,max) -> b
        | Interval.Empty,Interval.Empty -> Interval.Empty
        | _ -> invalidOp "Subtraction of (half) open intervals is not supported!"
        
    /// <summary>Checks if two intervals intersect</summary>
    /// <param name="a">The first interval</param>
    /// <param name="b">The second interval</param>
    /// <returns>True if the intervals intersect, false otherwise</returns>
    /// <example>
    /// <code>
    /// // Create an open interval (0.0, 10.0)
    /// let interval1 = Interval.CreateOpen(0.0, 10.0)
    /// // Create a closed interval [5.0, 15.0]
    /// let interval2 = Interval.CreateClosed(5.0, 15.0)
    /// // Check if the two intervals intersect (true)
    /// Interval.isIntersection interval1 interval2
    /// </code>
    /// </example>
    let inline isIntersection a b =
        match a,b with
        | Interval.Empty, Interval.Empty  -> true
        | Interval.Empty, _ | _, Interval.Empty -> false
        | Interval.Closed (minA,maxA), Interval.Closed (minB,maxB) -> minA <= maxB && minB <= maxA
        | Interval.Open (minA,maxA), Interval.Open (minB,maxB) 
        | Interval.LeftOpen (minA,maxA), Interval.LeftOpen (minB,maxB)
        | Interval.RightOpen (minA,maxA), Interval.RightOpen (minB,maxB) -> minA < maxB && minB < maxA && max minA minB < min maxA maxB
        | Interval.Open (minB,maxB), Interval.Closed (minA,maxA)
        | Interval.Closed (minA,maxA), Interval.Open (minB,maxB) -> (minB < minA && maxB > minA) || (minB >= minA && ((maxB <= maxA && minB <> maxB) || (minB < maxA && maxB > maxA)))
        | Interval.RightOpen (minB,maxB), Interval.Closed (minA,maxA)
        | Interval.Closed (minA,maxA), Interval.RightOpen (minB,maxB) -> minA < maxB && minB <= maxA && not (maxB <= maxA && max minA minB = maxB)
        | Interval.LeftOpen (minB,maxB), Interval.Closed (minA,maxA)
        | Interval.Closed (minA,maxA), Interval.LeftOpen (minB,maxB) -> minA <= maxB && minB < maxA && not (minB >= minA && min maxA maxB = minB)
        | Interval.RightOpen (minB,maxB), Interval.Open (minA,maxA)
        | Interval.Open (minA,maxA), Interval.RightOpen (minB,maxB) -> minA < maxB && minB < maxA && not ((minB > minA && min maxA maxB = minB) || min maxA maxB = minA)
        | Interval.LeftOpen (minB,maxB), Interval.Open (minA,maxA)
        | Interval.Open (minA,maxA), Interval.LeftOpen (minB,maxB) -> minA < maxB && minB < maxA && not((maxB < maxA && max minA minB = maxB) || max minA minB = maxA)
        | Interval.RightOpen (minB,maxB), Interval.LeftOpen (minA,maxA)
        | Interval.LeftOpen (minA,maxA), Interval.RightOpen (minB,maxB) -> minA <= maxB && minB <= maxA && not((maxB <= maxA && (minA = maxB || (minA < minB && minB = maxB)) || minA = maxA))
        
    /// <summary>Returns the intersection of two intervals</summary>
    /// <param name="a">The first interval</param>
    /// <param name="b">The second interval</param>
    /// <returns>The intersection of the two intervals</returns>
    /// <example>
    /// <code>
    /// // Create an open interval (0.0, 10.0)
    /// let interval1 = Interval.CreateOpen(0.0, 10.0)
    /// // Create a closed interval [5.0, 15.0]
    /// let interval2 = Interval.CreateClosed(5.0, 15.0)
    /// // Get the intersection of the two intervals [5.0, 10.0)
    /// Interval.intersect interval1 interval2
    /// </code>
    /// </example>
    let inline intersect a b =
        match a,b with
        | Interval.Empty, _ | _, Interval.Empty -> Interval.Empty
        | Interval.Closed (minA,maxA), Interval.Closed (minB,maxB) -> 
            if minA <= maxB && minB <= maxA then 
                Interval.Closed(max minA minB, min maxA maxB)
            else 
                Interval.Empty
        | Interval.Open (minA,maxA), Interval.Open (minB,maxB) -> 
            let min' = max minA minB
            let max' = min maxA maxB
            if min' < max' then 
                Interval.Open(min',max')
            else 
                Interval.Empty
        | Interval.LeftOpen (minA,maxA), Interval.LeftOpen (minB,maxB) -> 
            let min' = max minA minB
            let max' = min maxA maxB
            if min' < max' then 
                Interval.LeftOpen(min',max')
            else 
                Interval.Empty
        | Interval.RightOpen (minA,maxA), Interval.RightOpen (minB,maxB) -> 
            let min' = max minA minB
            let max' = min maxA maxB
            if min' < max' then 
                Interval.RightOpen(min',max')
            else 
                Interval.Empty
        | Interval.Open (minB,maxB), Interval.Closed (minA,maxA)
        | Interval.Closed (minA,maxA), Interval.Open (minB,maxB) -> 
            if minB >= minA then 
                if maxB <= maxA then 
                    if minB = maxB then 
                        Interval.Empty
                    else
                        Interval.Open(minB,maxB)
                elif minB >= maxA then 
                    Interval.Empty
                else
                    Interval.LeftOpen(minB,maxA)
            else
                if maxB <= maxA then 
                    if maxB <= minA then 
                        Interval.Empty
                    else
                        Interval.RightOpen(minA,maxB)
                else Interval.Closed(minA,maxA)
        | Interval.RightOpen (minB,maxB), Interval.Closed (minA,maxA)
        | Interval.Closed (minA,maxA), Interval.RightOpen (minB,maxB) ->
            let min' = max minA minB
            if maxB <= maxA then 
                if min' >= maxB then 
                    Interval.Empty
                else
                    Interval.RightOpen(min',maxB)
            elif min' <= maxA then
                Interval.Closed(min',maxA)
            else 
                Interval.Empty
        | Interval.LeftOpen (minB,maxB), Interval.Closed (minA,maxA)
        | Interval.Closed (minA,maxA), Interval.LeftOpen (minB,maxB) ->
            let max' = min maxA maxB
            if minB >= minA then 
                if max' <= minB then 
                    Interval.Empty
                else
                    Interval.LeftOpen(minB,max')
            elif minA <= max' then
                Interval.Closed(minA,max')
            else 
                Interval.Empty
        | Interval.RightOpen (minB,maxB), Interval.Open (minA,maxA)
        | Interval.Open (minA,maxA), Interval.RightOpen (minB,maxB) ->
            let max' = min maxA maxB
            if minB > minA then 
                if max' <= minB then 
                    Interval.Empty
                else
                    Interval.RightOpen(minB,max')
            elif minA >= max' then 
                Interval.Empty
            else
                Interval.Open(minA,max')
        | Interval.LeftOpen (minB,maxB), Interval.Open (minA,maxA)
        | Interval.Open (minA,maxA), Interval.LeftOpen (minB,maxB) -> 
            let min' = max minA minB
            if maxB < maxA then 
                if min' >= maxB then 
                    Interval.Empty
                else
                    Interval.LeftOpen(min',maxB)
            elif min' >= maxA then
                Interval.Empty
            else 
                Interval.Open(min',maxA)
        | Interval.RightOpen (minB,maxB), Interval.LeftOpen (minA,maxA)
        | Interval.LeftOpen (minA,maxA), Interval.RightOpen (minB,maxB) -> 
            if maxB <= maxA then
                if minA >= minB then 
                    if minA >= maxB then 
                        Interval.Empty
                    else
                        Interval.Open(minA,maxB)
                elif minB >= maxB then 
                    Interval.Empty
                else
                    Interval.RightOpen(minB,maxB)
            else    
                if minA >= minB then 
                    if minA >= maxA then 
                        Interval.Empty
                    else
                        Interval.LeftOpen(minA,maxA)
                elif minB <= maxA then
                    Interval.Closed(minB,maxA)
                else 
                    Interval.Empty    

    /// <summary>Get the value at a given proportion within (0.0 - 1.0) or outside (&lt; 0.0, &gt; 1.0) of the interval. Rounding to nearest neighbour occurs when needed.</summary>
    /// <remarks></remarks>
    /// <param name="proportion">The proportion (0.0 - 1.0 inside, &lt; 0.0 or &gt; 1.0 outside)</param>
    /// <param name="interval">The input interval</param>
    /// <returns>The value at the given proportion, or NaN if the interval is empty</returns>
    /// <example>
    /// <code>
    /// // Create a closed interval [0.0, 10.0]
    /// let interval = Interval.CreateClosed(0.0, 10.0)
    /// // Get the value at 0.5 within the interval (5.0)
    /// Interval.getValueAt 0.5 interval
    /// </code>
    /// </example>
    /// <exception cref="System.DivideByZeroException">Thrown when the interval is empty and a NaN value does not exist for the type</exception>
    let inline getValueAt proportion interval =        
        match trySize interval with
        | Some size -> interval.GetStart() + proportion * size
        | None -> LanguagePrimitives.GenericZero/LanguagePrimitives.GenericZero

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


