namespace FSharp.Stats


/// Module to compute common statistical measure on array
[<AutoOpen>]
module Array =


    let range (items:list<_>) =        
        let rec loop l (minimum) (maximum) =
            match l with
            | h::t -> loop t (min h minimum) (max h maximum)
            | [] -> Intervals.create minimum maximum          
        //Init by fist value
        match items with
        | h::t  -> loop t h h
        | [] -> Intervals.Interval.Empty


    /// Arranges the items between the left and right border, that all items left of the pivot element are smaller and bigger on the right.
    /// Function works in place and returns the index of the pivote element
    let partionSortInPlace left right (items:array<'T>) =   
        
        // Swaps items of left and right index
        let swapInPlace left right (items:array<'T>) =
            let tmp = items.[left]
            items.[left]  <- items.[right]
            items.[right] <- tmp

        // Median of three optimization improves performance in general,
        // and eliminates worst-case behavior for sorted or reverse-sorted data.    
        let center = (right + left) / 2
        if (items.[left] > items.[right]) then
            swapInPlace left right items
        if (items.[center] < items.[left]) then
            swapInPlace center  left items
        if (items.[center] > items.[right]) then
            swapInPlace center right  items
        // // pick the pivot point and save it
        let pivot = items.[center]    

        let rec moveRightIndex j =
            if (pivot < items.[j]) then
                moveRightIndex (j-1)
            else
                j
    
        let rec moveLeftIndex i =
            if (items.[i] < pivot) then
                moveLeftIndex (i+1) 
            else
                i

        let rec loop i j =
            if (i < j) then
                let j' = moveRightIndex j
                let i' = moveLeftIndex i
                if i' < j' then swapInPlace i' j' items
            
                loop i' j'
            else
                i

        let i = loop left right        
        i


    /// Finds the kth smallest element in an unordered array
    let quickSelect k (items:array<'T>) =
        
        let rec quickSelect' (items:array<'T>) left right k =

            // get pivot position  
            let pivot = partionSortInPlace left right items 

            // if pivot is less than k, select from the right part  
            if (pivot < k) then             
                quickSelect' items (pivot + 1) right k
            // if pivot is greater than k, select from the left side  
            elif (pivot > k) then
                quickSelect' items left (pivot - 1) k
            // if equal, return the value
            else 
                items.[pivot]
    
        
        let items' = Array.copy items
        quickSelect' items' 0 (items'.Length - 1) (k - 1)


    /// Computes the sample median
    let inline median (items:array<'T>) =

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
            let pivotIndex = partionSortInPlace left right items 

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

        
        let zero = LanguagePrimitives.GenericZero< 'T > 
        let one = LanguagePrimitives.GenericOne< 'T > 
        
        if items.Length > 0 then
            let items' = Array.copy items
            let n = items'.Length
            let mid = n / 2    
            let m1,m2 = medianQuickSelect items' 0 (items'.Length - 1) (mid)
            (m1 + m2) / (one + one)

        else
            zero / zero    
