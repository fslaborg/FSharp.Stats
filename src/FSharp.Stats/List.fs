namespace FSharp.Stats


/// Module to compute common statistical measure on list
[<AutoOpen>]
module List =


    let range (items:list<_>) =        
        let rec loop l (minimum) (maximum) =
            match l with
            | h::t -> loop t (min h minimum) (max h maximum)
            | [] -> Intervals.create minimum maximum          
        //Init by fist value
        match items with
        | h::t  -> loop t h h
        | [] -> Intervals.Interval.Empty

    /// computes the population mean (normalized by n)
    let mean items =
        items
        |> List.fold (fun (n,sum) x -> 1 + n,sum + x) (0,0.)
        |> fun (n,sum) -> sum / float n

    /// Calculate the median of a list of items.
    /// The result is a tuple of two items whose mean is the median.
    let median xs =
        /// Partition list into three piles; less-than, equal and greater-than
        /// x:    Current pivot
        /// xs:   Sublist to partition
        /// cont: Continuation function
        let rec partition x xs cont =
            match xs with
            | [] ->
                // place pivot in equal pile
                cont [] 0 [x] 1 [] 0
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
        /// Partition input and recurse into the part than contains the median
        /// before: Number of elements before this sublist.
        /// xs:     Current sublist.
        /// after:  Number of elements after this sublist.
        let rec loop before xs after =
            match xs with
            | [] -> failwith "Median of empty list"
            | x::xs ->
                partition x xs (fun lts numlt eqs numeq gts numgt ->
                    if before + numlt > numeq + numgt + after then
                        // Recurse into less pile
                        loop before lts (after + numeq + numgt)
                    elif before + numlt = numeq + numgt + after then
                        // Median is split between less and equal pile
                        (List.max lts, x)
                    elif before + numlt + numeq > numgt + after then
                        // Median is completely inside equal pile
                        (x, x)
                    elif before + numlt + numeq = numgt + after then
                        // Median is split between equal and greater pile
                        (x, List.min gts)
                    else
                        // Recurse into greater pile
                        loop (before + numlt + numeq) gts after)
        loop 0 xs 0

