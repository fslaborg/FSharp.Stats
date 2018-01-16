namespace FSharp.Stats

[<AutoOpen>]
module JaggedArray =

    /// Copies the jagged array
    let copy (arr : _[][]) = 
        Array.init arr.Length (fun i ->
            Array.copy arr.[i]
            )    

    /// Transposes a jagged array
    let transpose (arr: 'T [][]) =
        if arr.Length > 0 then 
            let colSize = arr.[0].Length
            Array.init (colSize) (fun rowI ->  Array.init (arr.Length) (fun colI -> (arr.[colI].[rowI])))
        else
            arr

    /// Converts a jagged list into a jagged array
    let ofJaggedList (data: 'T list list) =
        data
        |> List.map (fun l -> l |> Array.ofList)
        |> Array.ofList

    /// Converts a jagged array into a jagged list
    let toJaggedList (arr: 'T [][]) =
        arr
        |> Array.map (fun a -> a |> List.ofArray)
        |> List.ofArray

    /// Converts a jagged Seq into a jagged array
    let ofJaggedSeq (data: seq<#seq<'T>>) =
        data
        |> Seq.map (fun s -> s |> Array.ofSeq)
        |> Array.ofSeq

    /// Converts a jagged array into a jagged seq
    let toJaggedSeq (arr: 'T [][]) =
        arr
        |> Seq.map (fun s -> s |> Array.toSeq) 

    /// Builds a new jagged array whose inner arrays are the results of applying the given function to each of their elements.
    let map (mapping: 'T -> 'U) (jArray : 'T[][]) =
        jArray
        |> Array.map (fun x -> x |> Array.map mapping)

    /// Builds a new jagged array whose inner arrays are the results of applying the given function to the corresponding elements of the inner arrays of the two jagged arrays pairwise. 
    /// All corresponding inner arrays must be of the same length, otherwise ArgumentException is raised.
    let map2 (mapping: 'T1 -> 'T2 -> 'U) (jArray1 : 'T1[][]) (jArray2 : 'T2[][]) = 
        jArray1
        |> Array.mapi (fun index x -> (Array.map2 mapping x jArray2.[index]))

    /// Builds a new jagged array whose inner arrays are the results of applying the given function to the corresponding elements of the inner arrays of the tree jagged arrays triplewise. 
    /// All corresponding inner arrays must be of the same length, otherwise ArgumentException is raised.
    let map3 (mapping : 'T1 -> 'T2 -> 'T3 -> 'U) (jArray1 : 'T1[][]) (jArray2 : 'T2[][]) (jArray3 : 'T3[][]) =
        jArray1 |> Array.mapi (fun index x -> (Array.map3 mapping x jArray2.[index] jArray3.[index]))

    ///Builds a new jagged array whose inner arrays are the results of applying the given function to each of their elements. The integer index passed to the function indicates the index of element in the inner array being transformed.
    let mapi (mapping: int -> 'T -> 'U) (jArray : 'T[][]) =
        jArray
        |> Array.map (fun x -> x |> Array.mapi mapping)

    ///Applies a function to each element of the inner arrays of the jagged array, threading an accumulator argument through the computation.
    let innerFold (folder: 'State -> 'T -> 'State) (state: 'State) (jArray : 'T[][]) =
        jArray
        |> Array.map (fun x -> x |> Array.fold folder state )

    ///Applies a function to each element of the inner arrays of the jagged array, threading an accumulator argument through the computation. 
    ///A second function is the applied to each result of the predeceding computation, again passing an accumulater through the computation 
    let fold (innerFolder : 'State1 -> 'T -> 'State1) (outerFolder : 'State2 -> 'State1 -> 'State2) (innerState : 'State1) (outerState : 'State2) ((jArray : 'T[][])) =
        jArray
        |> innerFold innerFolder innerState
        |> Array.fold outerFolder outerState

    ///Returns a new jagged array whose inner arrays only contain the elements for which the given predicate returns true
    let innerFilter (predicate: 'T -> bool) (jArray: 'T[][]) =
        jArray
        |> Array.map (fun x -> x |> Array.filter predicate)

    ///Applies the given function to each element in the inner arrays of the jagged array. Returns the jagged array whose inner arrays are comprised of the results x for each element where the function returns Some(x)
    let innerChoose (chooser: 'T -> 'U option) (jArray: 'T[][]) =
        jArray
        |> Array.map (fun x -> x |> Array.choose chooser)


    /// Shuffles each column of a jagged array separately  (method: Fisher-Yates)
    let shuffleColumnWiseInPlace (arr: 'T [][]) =
        if arr.Length > 0 then 
            let random      = new System.Random()
            let rowCount    = arr.Length
            let columnCount = arr.[0].Length
            
            for ci = columnCount - 1 downto 0 do 
                for ri = rowCount downto  1 do
                    // Pick random element to swap.
                    let rj = random.Next(ri) // 0 <= j <= i-1
                    // Swap.
                    let tmp         =  arr.[rj].[ci]
                    arr.[rj].[ci]     <- arr.[ri - 1].[ci]
                    arr.[ri - 1].[ci] <- tmp
            arr            

        else
            arr


    /// Shuffles each row of a jagged array separately  (method: Fisher-Yates)
    let shuffleRowWiseInPlace (arr: 'T [][]) =
        if arr.Length > 0 then 
            let random      = new System.Random()
            let rowCount    = arr.Length
            let columnCount = arr.[0].Length
            
            for ri = rowCount - 1 downto  0 do
                for ci = columnCount downto 1 do 
                    // Pick random element to swap.
                    let cj = random.Next(ci) // 0 <= j <= i-1
                    // Swap.
                    let tmp           =  arr.[ri].[cj]
                    arr.[ri].[cj]     <- arr.[ri].[ci - 1]
                    arr.[ri].[ci - 1] <- tmp
            arr            

        else
            arr


    /// Shuffels a jagged array (method: Fisher-Yates)
    let shuffleInPlace (arr: 'T [][]) =
        if arr.Length > 0 then 
            let random      = new System.Random()
            let rowCount    = arr.Length
            let columnCount = arr.[0].Length
            for ri = rowCount downto 1 do
                for ci = columnCount downto 1 do 
                    // Pick random element to swap.
                    let rj = random.Next(ri) // 0 <= j <= i-1
                    let cj = random.Next(ci)
                    // Swap.
                    let tmp               =  arr.[rj].[cj]
                    arr.[rj].[cj]         <- arr.[ri - 1].[ci - 1]
                    arr.[ri - 1].[ci - 1] <- tmp
            arr            

        else
            arr

[<AutoOpen>]
module JaggedList =
    

    // Transposes a jagged array
    let transpose (data: 'T list list) =
        let rec transpose = function
            | (_::_)::_ as M -> List.map List.head M :: transpose (List.map List.tail M)
            | _ -> []
        transpose data


    // Converts a jagged list into a jagged array     
    let toJaggedList (data: 'T list list) =
        data
        |> List.map (fun l -> l |> Array.ofList)
        |> Array.ofList


    // Converts a jagged array into a jagged list
    let ofJaggedArray (arr: 'T [][]) =
        arr
        |> Array.map (fun a -> a |> List.ofArray)
        |> List.ofArray

    // Converts a jagged Seq into a jagged list
    let ofJaggedSeq (data: seq<#seq<'T>>) =
        data
        |> Seq.map (fun s -> s |> List.ofSeq)
        |> List.ofSeq

    // Converts a jagged list into a jagged seq
    let toJaggedSeq (data: 'T list list) =
        data
        |> Seq.map (fun s -> s |> List.toSeq) 

    /// Builds a new jagged list whose inner lists are the results of applying the given function to each of their elements.
    let map (mapping: 'T -> 'U) (jlist : 'T list list) =
        jlist
        |> List.map (fun x -> x |> List.map mapping)

    /// Builds a new jagged list whose inner lists are the results of applying the given function to the corresponding elements of the inner lists of the two jagged lists pairwise. 
    /// All corresponding inner lists must be of the same length, otherwise ArgumentException is raised.
    let map2 (mapping: 'T1 -> 'T2 -> 'U) (jlist1 : 'T1 list list) (jlist2 : 'T2 list list) = 
        jlist1
        |> List.mapi (fun index x -> (List.map2 mapping x jlist2.[index]))

    /// Builds a new jagged list whose inner lists are the results of applying the given function to the corresponding elements of the inner lists of the tree jagged lists triplewise. 
    /// All corresponding inner lists must be of the same length, otherwise ArgumentException is raised.
    let map3 (mapping : 'T1 -> 'T2 -> 'T3 -> 'U) (jlist1 : 'T1 list list) (jlist2 : 'T2 list list) (jlist3 : 'T3 list list) =
        jlist1 |> List.mapi (fun index x -> (List.map3 mapping x jlist2.[index] jlist3.[index]))

    ///Builds a new jagged list whose inner lists are the results of applying the given function to each of their elements. The integer index passed to the function indicates the index of element in the inner list being transformed.
    let mapi (mapping: int -> 'T -> 'U) (jlist : 'T list list) =
        jlist
        |> List.map (fun x -> x |> List.mapi mapping)

    ///Applies a function to each element of the inner lists of the jagged list, threading an accumulator argument through the computation.
    let innerFold (folder: 'State -> 'T -> 'State) (state: 'State) (jlist : 'T list list) =
        jlist
        |> List.map (fun x -> x |> List.fold folder state )

    ///Applies a function to each element of the inner lists of the jagged list, threading an accumulator argument through the computation. 
    ///A second function is the applied to each result of the predeceding computation, again passing an accumulater through the computation 
    let fold (innerFolder : 'State1 -> 'T -> 'State1) (outerFolder : 'State2 -> 'State1 -> 'State2) (innerState : 'State1) (outerState : 'State2) ((jlist : 'T list list)) =
        jlist
        |> innerFold innerFolder innerState
        |> List.fold outerFolder outerState

    ///Returns a new jagged list whose inner lists only contain the elements for which the given predicate returns true
    let innerFilter (predicate: 'T -> bool) (jlist: 'T list list) =
        jlist
        |> List.map (fun x -> x |> List.filter predicate)

    ///Applies the given function to each element in the inner lists of the jagged List. Returns the jagged list whose inner lists are comprised of the results x for each element where the function returns Some(x)
    let innerChoose (chooser: 'T -> 'U option) (jlist: 'T list list) =
        jlist
        |> List.map (fun x -> x |> List.choose chooser)


