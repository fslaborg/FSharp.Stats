namespace FSharp.Stats


/// Module to compute common statistical measure on array
[<AutoOpen>]
module Array =


    let range (a:array<_>) =        
        let rec loop index (minimum) (maximum) =
            if index < a.Length then
                let current = a.[index]
                loop (index+1) (min current minimum) (max current maximum)
            else
                Intervals.create minimum maximum          
        //Init by fist value
        if a.Length > 1 then
            loop 1 a.[0] a.[0] 
        else
            Intervals.Interval.Empty


    // Swaps items of left and right index
    let swapInPlace left right (items:array<'T>) =
        let tmp = items.[left]
        items.[left]  <- items.[right]
        items.[right] <- tmp

    /// Arranges the items between the left and right border, that all items left of the pivot element are smaller and bigger on the right.
    /// Function works in place and returns the index of the pivote element (using Lomuto's partitioning algorithm)
    let inline partitionSortInPlace left right (items:array<'T>) =   
        let random = Random.rndgen
        let pivot_index = left + random.NextInt() % (right - left + 1)
        let pivot = items.[pivot_index]
        if isNan pivot then
            ~~~pivot_index
        else
            swapInPlace pivot_index right items // swap random pivot to right.
            let pivot_index' = right;
            // https://stackoverflow.com/questions/22080055/quickselect-algorithm-fails-with-duplicated-elements
            //let mutable i = left - 1

            //for j = left to right - 1 do    
            //    if (arr.[j] <= pivot) then    
            //        i <- i + 1
            //        swapInPlace i j arr
            let rec loop i j =
                if j <  right then 
                    let v = items.[j]
                    if isNan v then   // true if nan
                        loop (~~~j) right // break beacause nan                    
                    else
                        if (v <= pivot) then        
                            let i' = i + 1
                            swapInPlace i' j items            
                            loop i' (j+1)
                        else
                            loop i (j+1)
                else
                    i

            let i = loop (left - 1) left
            if i < -1 then
                i
            else
                swapInPlace (i + 1) pivot_index' items // swap back the pivot
                i + 1

    /// Finds the kth smallest element in an unordered array (note that k is ONE-based)
    /// Works in place and can change the order of the elements in the input array
    let rec quickSelectInPlaceWith left right k (arr:array<'T>) : 'T =  

        if ( left = right ) then
            arr.[left]
        else
            let pivotIndex = partitionSortInPlace left right arr
            if pivotIndex < 0 then
                arr.[~~~pivotIndex]             
            else
                let length = pivotIndex - left + 1
                if ( length = k) then
                    arr.[pivotIndex]
                else 
                    if ( k < length ) then
                        quickSelectInPlaceWith left (pivotIndex - 1) k arr
                    else 
                        quickSelectInPlaceWith (pivotIndex + 1) right (k - length) arr
   

    /// Finds the kth smallest element in an unordered array (note that k is ONE-based)
    /// Works in place and can change the order of the elements in the input array
    let inline quickSelectInPlace k (items:array<'T>) : 'T =
        if k <= 0 then
            Array.min items
        elif k > items.Length-1 then
            Array.max items
        else
            quickSelectInPlaceWith 0 (items.Length - 1) k items


    /// Finds the kth smallest element in an unordered array (note that k is ONE-based)
    let inline quickSelect k (items:array<'T>) =
        if k <= 0 then
            Array.min items
        elif k > items.Length-1 then
            Array.max items
        else
            let items' = Array.copy items
            quickSelectInPlaceWith 0 (items'.Length - 1) k items'



    ///Old partitionSortInPlace fails with dublicates

    ///// Arranges the items between the left and right border, that all items left of the pivot element are smaller and bigger on the right.
    ///// Function works in place and returns the index of the pivote element
    //let partitionSortInPlace left right (items:array<'T>) =   

    //    // http://blog.mischel.com/2011/10/25/when-theory-meets-practice/
    //    // Median of three optimization improves performance in general,
    //    // and eliminates worst-case behavior for sorted or reverse-sorted data.    
    //    let center = (right + left) / 2
    //    if (items.[left] > items.[right]) then
    //        swapInPlace left right items
    //    if (items.[center] < items.[left]) then
    //        swapInPlace center  left items
    //    if (items.[center] > items.[right]) then
    //        swapInPlace center right  items
    //    // // pick the pivot point and save it
    //    let pivot = items.[center]    

    //    let rec moveRightIndex i j =
    //        if (pivot < items.[j]) && (i < j) then
    //            moveRightIndex i (j-1)
    //        else
    //            j
    
    //    let rec moveLeftIndex i j =
    //        if (items.[i] <= pivot) && (i < j) then
    //            moveLeftIndex (i+1) j 
    //        else
    //            i

    //    let rec loop i j =
    //        if (i < j) then
    //            let j' = moveRightIndex i j
    //            let i' = moveLeftIndex i j'            
    //            if i'=i && j'=j then // experimental to avoid: nan compare always false
    //                -1
    //            else               
    //                if i' < j' then swapInPlace i' j' items
    //                loop i' j'
    //        else
    //            i

    //    let i = loop left right        
    //    i


    ///// Finds the kth smallest element in an unordered array
    //let inline quickSelect k (items:array<'T>) =
    //    let zero = LanguagePrimitives.GenericZero< 'T > 

    //    let rec quickSelect' (items:array<'T>) left right k =

    //        // get pivot position  
    //        let pivot = partitionSortInPlace left right items 
    //        if pivot >= 0 then
    //            // if pivot is less than k, select from the right part  
    //            if (pivot < k) then             
    //                quickSelect' items (pivot + 1) right k
    //            // if pivot is greater than k, select from the left side  
    //            elif (pivot > k) then
    //                quickSelect' items left (pivot - 1) k
    //            // if equal, return the value
    //            else 
    //                items.[pivot]
    //        else
    //            zero / zero

    //    let k' = k - 1
    //    if k' <= 0 then
    //        Array.min items
    //    elif k' > items.Length-1 then
    //        Array.max items
    //    else        
    //        let items' = Array.copy items
    //        quickSelect' items' 0 (items'.Length - 1)  k' 


    ///// Finds the kth smallest element in an unordered array
    ///// Works in place and can change the order of the elements in the input array
    //let inline quickSelectInPlace k (items:array<'T>) : 'T =
    //    let zero = LanguagePrimitives.GenericZero< 'T > 
        
    //    let rec quickSelect' (items:array<'T>) left right k =
    //        // get pivot position  
    //        let pivot = partitionSortInPlace left right items 
    //        if pivot >= 0 then
    //            // if pivot is less than k, select from the right part  
    //            if (pivot < k) then             
    //                quickSelect' items (pivot + 1) right k
    //            // if pivot is greater than k, select from the left side  
    //            elif (pivot > k) then
    //                quickSelect' items left (pivot - 1) k
    //            // if equal, return the value
    //            else 
    //                items.[pivot]
    //        else
    //            zero / zero

    //    let k' = k - 1
    //    if k' <= 0 then
    //        Array.min items
    //    elif k' > items.Length-1 then
    //        Array.max items
    //    else
    //        quickSelect' items 0 (items.Length - 1) k'


    /// Computes the sample median
    let inline median (items:array<'T>) =

        let zero = LanguagePrimitives.GenericZero< 'T > 
        let one = LanguagePrimitives.GenericOne< 'T > 

        // returns max element of array from index to right index
        let rec max cMax index rigthIndex (input:array<'T>) =
            if index <= rigthIndex then
                let current = input.[index]
                if cMax < current then
                    max current (index+1) rigthIndex input
                else
                    max cMax (index+1) rigthIndex input
            else
                cMax


        // Returns a tuple of two items whose mean is the median by quickSelect algorithm
        let rec medianQuickSelect (items:array<'T>) left right k =

            // get pivot position  
            let pivotIndex = partitionSortInPlace left right items 
            if pivotIndex >= 0 then
                // if pivot is less than k, select from the right part  
                if (pivotIndex < k) then             
                    medianQuickSelect items (pivotIndex + 1) right k
                // if pivot is greater than k, select from the left side  
                elif (pivotIndex > k) then
                    medianQuickSelect items left (pivotIndex - 1) k
                // if equal, return the value
                else 
                    let n = items.Length
                    if n % 2 = 0 then
                        (max (items.[pivotIndex-1]) 0 (pivotIndex-1) items,items.[pivotIndex])
                    else
                        (items.[pivotIndex],items.[pivotIndex])
            else
                (zero/zero,zero/zero)

        
        if items.Length > 0 then
            let items' = Array.copy items
            let n = items'.Length
            let mid = n / 2    
            let m1,m2 = medianQuickSelect items' 0 (items'.Length - 1) (mid)
            (m1 + m2) / (one + one)

        else
            zero / zero    

    // When we sample with replacement, the two sample values are independent.
    // Practically, this means that what we get on the first one doesn't affect what we get on the second.
    // Mathematically, this means that the covariance between the two is zero
    /// Samples from an array of obj wit replacement (with putting back)
    let sampleWithReplacement (rnd:System.Random) (source:array<_>) (k:int) =
        if source.Length < 1 then failwithf "Source must not be empty."     
        Array.init k (fun _ -> source.[rnd.Next(0,source.Length)]) // Error fixed: previously source.Length-1



    // Implementation according to: http://krkadev.blogspot.de/2010/08/random-numbers-without-repetition.html
    /// Samples from an array of obj without replacement (without putting back)
    let sampleWithOutReplacement (rnd:System.Random) (source:array<_>) (k:int) =
        let n = source.Length
        let used = new System.Collections.Generic.Dictionary<int,int>()
        // recursive do-while implementation
        let rec loop (off:int) (n:int) (i:int) =    
            let value = n - i - 1 
            if used.ContainsKey(off) then
               loop (used.[off]) (n) (i)
            else
               used.Add(off,value)
               off

        Array.init k (fun i -> source.[(loop (rnd.Next(n - i)) n i)] )



    /// Shuffels the input array (method: Fisher-Yates)
    let shuffleFisherYates (arr : _[]) =
        let random = Random.rndgen //new System.Random()
        for i = arr.Length downto 1 do
            // Pick random element to swap.
            let j = random.NextInt i // 0 <= j <= i-1
            // Swap.
            let tmp = arr.[j]
            arr.[j] <- arr.[i - 1]
            arr.[i - 1] <- tmp
        arr  


    /// Generates array sequence (like R! seq.int)
    let seqInit (from:float) (tto:float) (length:int) =
        let stepWidth = (tto - from) / (float length - 1.)
        Array.init length ( fun x -> (float x * stepWidth) + from)  


    let sort2InPlaceByKeys (from:int) (count:int) (keys:array<'T>) (items:array<'T>) =
        System.Array.Sort(keys, items, from, count)
