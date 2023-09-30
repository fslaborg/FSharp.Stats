namespace FSharp.Stats


/// Module to compute common statistical measure on list
[<AutoOpen>]
module List =
    let range (items:list<_>) =        
        let rec loop l (minimum) (maximum) =
            match l with
            | h::t -> loop t (min h minimum) (max h maximum)
            | [] -> Interval.CreateClosed<'a> (minimum,maximum)
        //Init by fist value
        match items with
        | h::t  -> loop t h h
        | [] -> Interval.Empty

    /// <summary>computes the population mean (normalized by n)</summary>
    /// <remarks></remarks>
    /// <param name="items"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline mean (items: 'T list) =
        let zero = LanguagePrimitives.GenericZero<'T>
        let one = LanguagePrimitives.GenericOne<'T>
        items
        |> List.fold (fun (n,sum) x -> one + n,sum + x) (zero,zero)
        |> fun (n,sum) -> sum / n

    /// <summary>Calculate the median of a list of items.<br />The result is a tuple of two items whose mean is the median.</summary>
    /// <remarks></remarks>
    /// <param name="xs"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let inline median (xs: 'T list) =
        let one = LanguagePrimitives.GenericOne<'T>
        /// <summary>Partition list into three piles; less-than, equal and greater-than<br />x:    Current pivot<br />xs:   Sublist to partition<br />cont: Continuation function</summary>
        /// <remarks></remarks>
        /// <param name="partition"></param>
        /// <param name="x"></param>
        /// <param name="xs"></param>
        /// <param name="cont"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let rec partition x xs cont =
            match xs with
            | [] ->
                // place pivot in equal pile
                cont [] 0 [x] 1 [] 0
            | y::ys when isNan y -> y
            | y::ys ->
                if y < x then
                    // place item in less-than pile
                    partition x ys (fun lts n1 eqs n2 gts n3 ->
                        cont (y::lts) (n1+1) eqs n2 gts n3)
                elif y = x then
                    // place pivot in equal pile, and use item as new pivot,
                    // so that the order is preserved
                    partition y ys (fun lts n1 eqs n2 gts n3 ->
                        cont lts n1 (x::eqs) (n2+1) gts n3)
                else // y > x
                    // place item in greater-than pile
                    partition x ys (fun lts n1 eqs n2 gts n3 ->
                        cont lts n1 eqs n2 (y::gts) (n3+1))
        /// <summary>Partition input and recurse into the part than contains the median<br />before: Number of elements before this sublist.<br />xs:     Current sublist.<br />after:  Number of elements after this sublist.</summary>
        /// <remarks></remarks>
        /// <param name="loop"></param>
        /// <param name="before"></param>
        /// <param name="xs"></param>
        /// <param name="after"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let rec loop before xs after =
            match xs with
            | [] -> failwith "Median of empty list"
            | x::xs when isNan x -> x
            | x::xs ->
                partition x xs (fun lts numlt eqs numeq gts numgt ->
                    if before + numlt > numeq + numgt + after then
                        // Recurse into less pile
                        loop before lts (after + numeq + numgt)
                    elif before + numlt = numeq + numgt + after then
                        // Median is split between less and equal pile
                        (List.max lts + x) / (one + one)
                    elif before + numlt + numeq > numgt + after then
                        // Median is completely inside equal pile
                        x
                    elif before + numlt + numeq = numgt + after then
                        // Median is split between equal and greater pile
                        (x + List.min gts) / (one + one)
                    else
                        // Recurse into greater pile
                        loop (before + numlt + numeq) gts after)
        loop 0 xs 0

    /// <summary>
    ///   Computes the population covariance of two random variables
    /// </summary>
    ///    
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>population covariance estimator (denominator N)</returns> 
    let inline covPopulation (list1:list<'T>) (list2:list<'T>) : 'U =
        Seq.covPopulation list1 list2

    /// <summary>
    ///   Computes the population covariance of two random variables.
    ///   The covariance will be calculated between the paired observations.
    /// </summary>
    /// <param name="list">The input list.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>population covariance estimator (denominator N)</returns> 
    /// <example> 
    /// <code> 
    /// // Consider a list of paired x and y values:
    /// // [(x1, y1); (x2, y2); (x3, y3); (x4, y4); ... ]
    /// let xy = [(5., 2.); (12., 8.); (18., 18.); (-23., -20.); (45., 28.)]
    /// 
    /// // To get the population covariance between x and y:
    /// xy |> List.covPopulationOfPairs // evaluates to 347.92
    /// </code>
    /// </example>
    let inline covPopulationOfPairs (list:list<'T * 'T>) : 'U =
        list
        |> List.unzip
        ||> covPopulation

    /// <summary>
    ///   Computes the population covariance of two random variables generated by applying a function to the input list.
    /// </summary>
    /// <param name="f">A function applied to transform each element of the input list into a tuple of paired observations.</param>
    /// <param name="list">The input list.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>population covariance estimator (denominator N)</returns> 
    /// <example> 
    /// <code> 
    /// // To get the population covariance between x and y observations:
    /// let xy = [ {| x = 5.; y = 2. |}
    ///            {| x = 12.; y = 8. |}
    ///            {| x = 18.; y = 18. |}
    ///            {| x = -23.; y = -20. |} 
    ///            {| x = 45.; y = 28. |} ]
    /// 
    /// xy |> List.covPopulationBy (fun x -> x.x, x.y) // evaluates to 347.92
    /// </code>
    /// </example>
    let inline covPopulationBy f (list: 'T list) : 'U =
        list
        |> List.map f
        |> covPopulationOfPairs

    /// <summary>
    ///   Computes the sample covariance of two random variables
    /// </summary>
    ///    
    /// <param name="list1">The first input list.</param>
    /// <param name="list2">The second input list.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>sample covariance estimator (Bessel's correction by N-1)</returns> 
    let inline cov (list1:list<'T>) (list2:list<'T>) : 'U =
        Seq.cov list1 list2

    /// <summary>
    ///   Computes the sample covariance of two random variables.
    ///   The covariance will be calculated between the paired observations.    
    /// </summary>
    /// <param name="list">The input list.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>sample covariance estimator (Bessel's correction by N-1)</returns>
    /// <example> 
    /// <code> 
    /// // Consider a list of paired x and y values:
    /// // [(x1, y1); (x2, y2); (x3, y3); (x4, y4); ... ]
    /// let xy = [(5., 2.); (12., 8.); (18., 18.); (-23., -20.); (45., 28.)]
    /// 
    /// // To get the sample covariance between x and y:
    /// xy |> List.covOfPairs // evaluates to 434.90
    /// </code>
    /// </example>
    let inline covOfPairs (list:list<'T * 'T>) : 'U =
        list
        |> List.unzip
        ||> cov

    /// <summary>
    ///   Computes the sample covariance of two random variables generated by applying a function to the input list.
    /// </summary>
    /// <param name="f">A function applied to transform each element of the input list into a tuple of paired observations.</param>
    /// <param name="list">The input list.</param>
    /// <remarks>Returns NaN if data is empty or if any entry is NaN.</remarks>
    /// <returns>sample covariance estimator (Bessel's correction by N-1)</returns>
    /// <example> 
    /// <code> 
    /// // To get the sample covariance between x and y observations:
    /// let xy = [ {| x = 5.; y = 2. |}
    ///            {| x = 12.; y = 8. |}
    ///            {| x = 18.; y = 18. |}
    ///            {| x = -23.; y = -20. |} 
    ///            {| x = 45.; y = 28. |} ]
    /// 
    /// xy |> List.covBy (fun x -> x.x, x.y) // evaluates to 434.90
    /// </code>
    /// </example>
    let inline covBy f (list: 'T list) : 'U =
        list
        |> List.map f
        |> covOfPairs

[<AutoOpen>]
module ListExtension =

    type List() =

        /// <summary>
        /// Creates an float list with values between a given interval
        /// </summary>
        /// <param name="start">start value (is included)</param>
        /// <param name="stop">end value (by default is included )</param>
        /// <param name="Num">sets the number of elements in the list. If not set, stepsize = 1.</param>
        /// <param name="IncludeEndpoint">If false, the list does not contain the stop value</param>
        static member linspace(start:float,stop:float,num:int,?IncludeEndpoint:bool) : float list = 
        
            let includeEndpoint = defaultArg IncludeEndpoint true

            Seq.linspace(start,stop,num,includeEndpoint) |> List.ofSeq

        /// <summary>
        /// Creates a geometric list of floats with values between a given interval
        /// </summary>
        /// <param name="start">start value (is included)</param>
        /// <param name="stop">end value (by default is included)</param>
        /// <param name="Num">sets the number of elements in the list. Defaults to 50.</param>
        /// <param name="IncludeEndpoint">If false, the list does not contain the stop value. Defaults to true.</param>
        static member geomspace(start:float,stop:float,num:int,?IncludeEndpoint:bool) : float list = 
            let includeEndpoint = defaultArg IncludeEndpoint true

            Seq.geomspace (start, stop ,num, includeEndpoint)
            |> List.ofSeq

