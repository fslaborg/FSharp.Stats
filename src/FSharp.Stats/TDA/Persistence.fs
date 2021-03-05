namespace FSharp.Stats.TDA

open System

module Persistence =

    type Direction =
        | Split
        | Join

    // adds zeroes at front and end of data to ensure local minima at both ends
    // and therefore "real" maxima at the original borders
    let paddData rawData = 
        Array.append (Array.append [|0.|] rawData) [|0.|]

    // union function for naive union-find data structure based on an array
    let union a b (uf:int []) = uf.[int b] <- a

    // find function for naive union-find data structure based on an array
    let rec find a (uf:int[]) =
        let atmp = if a<0 then uf.Length + int a else a
        if uf.[int atmp] = atmp then
            atmp
        else 
            find uf.[int atmp] uf 

    // function that computes for data array:
    //   persistence pairs as list of index pairs
    //   merge tree as list of edges represented by index pairs
    //   segmentation array assigning to each point its corresponding maximum
    let computePPMT (data:float[]) (direction: Direction) =
        let reverse = direction = Direction.Split
        
        // comparison functions for data points with simulation of simplicity
        let compare i j =
            let a = if i<0 || i>(data.Length-1) then nan else data.[i]
            let b = if j<0 || j>(data.Length-1) then nan else data.[j]
            if a=b then
                let res = i-j
                if reverse then res*(-1) else res
            else
                let res = if a<b then -1 else 1
                if reverse then res*(-1) else res
        let smaller i j=
            compare i j < 0

        // order points depending on direction (ascending for join tree, descending for split tree)
        let orderedPoints = 
            data
            |> Array.indexed
            |> Array.map fst
            |> Array.sortWith compare

        // initiate union-find data structures for merge tree and persistence pair segments
        // (no initial values, they are defined on creation of segments)
        let ufpP = Array.create data.Length -1
        let ufmT = Array.create data.Length -1

        let mutable persistencePairs    = [] 
        let mutable mergeTreePairs      = []
        let mutable segmentation_pp :int []= Array.zeroCreate data.Length
        let mutable segmentation_mt :int []= Array.zeroCreate data.Length

        // sweep over ordered points and
        // for each point determine type and track segments
        orderedPoints
        |> Array.iter (fun i ->
            let va = data.[i]
            let leftVal = if i = 0 then nan else data.[i-1]
            let rightVal = if i = data.Length - 1 then nan else data.[i+1]
            let leftIdx = i-1
            let rightIdx = i+1
        
            // if both neighbors larger, i is local minimum
            if (smaller i leftIdx && smaller i rightIdx) ||
               (nan.Equals leftVal && smaller i rightIdx) ||
               (smaller i leftIdx && nan.Equals rightVal) then
                // a new segment is created, atm it only consists of i
                ufpP.[i] <- i
                ufmT.[i] <- i
                segmentation_pp.[i] <- ufpP.[i]
                segmentation_mt.[i] <- ufmT.[i]
            // if i not at border, right neighbor larger and left one smaller, i is a regular point
            elif not (nan.Equals leftVal) && not (nan.Equals rightVal) && smaller i rightIdx && smaller leftIdx i then
                // add i to segment of left neighbor (smaller one)
                ufpP.[i] <- find (i-1) ufpP
                ufmT.[i] <- find (i-1) ufmT
                segmentation_pp.[i] <- ufpP.[i]
                segmentation_mt.[i] <- ufmT.[i]
            // if i not at border, left neighbor larger and right one smaller, i is a regular point
            elif not (nan.Equals leftVal) && not (nan.Equals rightVal) && smaller rightIdx i && smaller i leftIdx then
                // add i to segment of right neighbor (smaller one)
                ufpP.[i] <- find (i+1) ufpP
                ufmT.[i] <- find (i+1) ufmT
                segmentation_pp.[i] <- ufpP.[i]
                segmentation_mt.[i] <- ufmT.[i]
            // otherwise i is a local maximum
            else
                // case for i at the left border
                if nan.Equals leftVal then
                    // i belongs to segment of right neighbor and this segment "disappears"
                    ufpP.[i] <- find (i+1) ufpP
                    ufmT.[i] <- find (i+1) ufmT
                    segmentation_pp.[i] <- ufpP.[i]
                    segmentation_mt.[i] <- ufmT.[i]
                // case for i at the right border
                elif nan.Equals rightVal then
                    // i belongs to segment of left neighbor and
                    // nothing else happens this segment "disappears"
                    ufpP.[i] <- find (i-1) ufpP
                    ufmT.[i] <- find (i-1) ufmT
                    segmentation_pp.[i] <- ufpP.[i]
                    segmentation_mt.[i] <- ufmT.[i]
                // general case, the maximum corresponds to a merge point of two segments
                else
                    // for the merge tree:
                    //   add pair from i to the starting points of both old segments (representing inner edge)
                    //   start a new segment (representing inner edge)
                    ufmT.[i] <- i
                    mergeTreePairs <- (int(find(i+1) ufmT),i)::mergeTreePairs
                    mergeTreePairs <- (int(find(i-1) ufmT),i)::mergeTreePairs
                    segmentation_mt.[i] <- ufmT.[i]
                    union i (find (i-1) ufmT) ufmT
                    union i (find (i+1) ufmT) ufmT
                    // for persistence pairs:
                    //   add pair from i to starting point of "newer" segment
                    //   add i to segmentation of "older" segment
                    let le = find (i-1) ufpP
                    let re = find (i+1) ufpP
                    if smaller (int le) (int re) then 
                        persistencePairs <- (int(find (i+1) ufpP),i)::persistencePairs
                        segmentation_pp.[i] <- int (find (i-1) ufpP)
                        union le re ufpP
                    else 
                        persistencePairs <- (int(find(i-1) ufpP),i)::persistencePairs
                        segmentation_pp.[i] <- int (find (i+1) ufpP)
                        union re le ufpP 
            )

        // add last persistence pair from global maximum to -infinity
        persistencePairs <- (int(find orderedPoints.[orderedPoints.Length - 1] ufpP),-1)::persistencePairs

        // sort persistence pairs by persistence
        let pP_sorted = 
            persistencePairs
            |> List.sortBy (fun p -> 
                if snd p > 0 then 
                    data.[snd p]-data.[fst p]
                    |> Math.Abs
                else infinity
                )
        persistencePairs <- pP_sorted

        // add last edge from global minimum to -infinity to merge tree (edge to root)
        mergeTreePairs <- ((int(find (orderedPoints.[orderedPoints.Length - 1]) ufmT),-1))::mergeTreePairs     
    
        persistencePairs,mergeTreePairs |> List.rev,segmentation_pp,segmentation_mt

    // function that simplifies data array based on persistence threshold
    let simplifyData (data:float[]) (persistencePairs:(int*int)list) threshold (direction: Direction) =
        let dataSimpl = Array.copy data
        let reverse = direction = Direction.Split
        
        // comparison functions for data points with simulation of simplicity
        let compare i j =
            let a = if i<0 || i>(data.Length-1) then nan else data.[i]
            let b = if j<0 || j>(data.Length-1) then nan else data.[j]
            if a=b then
                let res = i-j
                if reverse then res*(-1) else res
            else
                let res = if a<b then -1 else 1
                if reverse then res*(-1) else res
        let smaller i j=
            compare i j < 0
    
        // iterate over sorted persistence pairs
        let rec loopList ppList =
            match ppList with
            | [] -> ()
            | pair::tail -> 
                // if root pair, ignore
                if snd pair < 0 then 
                    loopList tail
                else 
                    let v1 = data.[fst pair]
                    let v2 = data.[snd pair]
                    // if persistence over threshold, stop iteration (following pairs are also larger due to sorting)
                    if (Math.Abs (v2-v1))>threshold then ()
                    // otherwise, cut peak to level of corresponding saddle/merge point
                    else
                        // from maximum (second entry), move to the right and flatten until value lower than saddle
                        let rec innerloopA i =
                            if fst pair + i >= data.Length then
                                ()
                            elif smaller (fst pair + i) (snd pair) then
                                dataSimpl.[fst pair + i] <- data.[snd pair]
                                innerloopA (i+1)
                            else
                                ()
                        innerloopA 0
                        // from maximum (second entry), move to the left and flatten until value lower than saddle
                        let rec innerloopB i =
                            if fst pair - i < 0 then
                                ()
                            elif smaller (fst pair - i) (snd pair) then
                                dataSimpl.[fst pair - i] <- data.[snd pair]
                                innerloopB (i+1)
                            else
                                ()
                        innerloopB 0
                        // continue with next pair
                        loopList tail

        loopList persistencePairs

        dataSimpl   

    // function that simplifies merge tree and segmentation based on persistence threshold
    let simplifyMergeTreeAndSeg (data:float []) (mergeTreePairs:(int*int)list) (persistencePairs:(int*int)list) ppSegmentation mtSegmentation threshold (direction: Direction) =
    
        let split = direction = Direction.Split
        
        // comparison functions for data points with simulation of simplicity
        let compare i j =
            let a = if i<0 || i>(data.Length-1) then nan else data.[i]
            let b = if j<0 || j>(data.Length-1) then nan else data.[j]
            if a=b then
                let res = i-j
                if split then res*(-1) else res
            else
                let res = if a<b then -1 else 1
                if split then res*(-1) else res
        let smaller i j=
            compare i j < 0

        let mutable ppSegmentationSimpl = Array.copy ppSegmentation
        let mutable mtSegmentationSimpl = Array.copy mtSegmentation

        // build neighbor map that contains the list of neighbors for each vertex in merge tree
        let mutable neighborMap = [] |> Map.ofSeq
        mergeTreePairs
        |> List.iter (fun edge ->
            let tmpA = if Map.containsKey (fst edge) neighborMap then neighborMap.[fst edge] else []
            let tmpB = if Map.containsKey (snd edge) neighborMap then neighborMap.[snd edge] else []
            neighborMap <- neighborMap |> Map.add (fst edge) ((snd edge)::tmpA)
            neighborMap <- neighborMap |> Map.add (snd edge) ((fst edge)::tmpB)
            )

        // function for pruning nodes of degree 2
        let prune vID = 
            // get both remaining neighbors of vID
            let n1 = neighborMap.[vID].[0]
            let n2 = neighborMap.[vID].[1]
            // remove vID from neighbor lists of n1 and n2 and add new neighbor respectively
            let tmpR1 = List.append (List.filter (fun x -> x <> vID) neighborMap.[n1]) [n2]
            let tmpR2 = List.append (List.filter (fun x -> x <> vID) neighborMap.[n2]) [n1]
            // update neighborMap
            neighborMap <- neighborMap |> Map.remove n1 |> Map.add n1 tmpR1
            neighborMap <- neighborMap |> Map.remove n2 |> Map.add n2 tmpR2
            neighborMap <- neighborMap |> Map.add vID []

        neighborMap <- neighborMap |> Map.map (fun k v -> v |> List.rev)

        // iterate over sorted persistence pairs
        let rec loopList ppList =
            match ppList with
            | [] -> ()
            | pair::tail ->
            // if root, ignore pair
            if fst pair < 0 || snd pair < 0 then 
                loopList tail
            else 
            let v1 = data.[fst pair]
            let v2 = data.[snd pair]
            // if persistence over threshold, stop iteration (following pairs are also larger due to sorting)
            if Math.Abs(v1-v2) > threshold then 
                ()
            else
                // determine which vertex is inner node (kept) and which leave (simplified)
                let simplifiedExtremum = 
                    if smaller (fst pair) (snd pair) then fst pair else snd pair
                let keptExtremum = 
                    if simplifiedExtremum = snd pair then fst pair else snd pair

                // change segmentation value of for simplified vertex (rest of region follows later)
                ppSegmentationSimpl.[simplifiedExtremum] <- ppSegmentationSimpl.[keptExtremum]
                mtSegmentationSimpl.[simplifiedExtremum] <- mtSegmentationSimpl.[keptExtremum]
                // remove simplified node from neighbors of kept node
                let tmpR1 = (List.filter (fun x -> x <> simplifiedExtremum) neighborMap.[keptExtremum])
                neighborMap <- neighborMap |> Map.add keptExtremum tmpR1     
                neighborMap <- neighborMap |> Map.add simplifiedExtremum []
                // prune kept node if necessary (i.e. if degree 2)
                if neighborMap.[keptExtremum].Length = 2 then
                    let smallerNeighbor =
                        if smaller (neighborMap.[keptExtremum]).[0] (neighborMap.[keptExtremum]).[1]
                        then (neighborMap.[keptExtremum]).[0] else (neighborMap.[keptExtremum]).[1]
                    mtSegmentationSimpl.[keptExtremum] <- mtSegmentationSimpl.[smallerNeighbor]
                    prune (int keptExtremum)
                loopList tail
        loopList persistencePairs

        // propagate segmentation changes to whole segments (by flattening tree-like reference structure in array)
        let rec recurseSegSimplification (seg:int array) i =
            match seg.[i] with 
            | segi when segi=i -> i
            | _ -> 
                let segi = recurseSegSimplification seg (seg.[i])
                seg.[i] <- segi
                segi
        ppSegmentationSimpl <- Array.map (recurseSegSimplification ppSegmentationSimpl) ppSegmentationSimpl
        mtSegmentationSimpl <- Array.map (recurseSegSimplification mtSegmentationSimpl) mtSegmentationSimpl
    
        // set of edges in simplified merge tree
        let mutable mergeTreePairsSimplSet :Set<(int*int)> = Set.ofList []
        // for all vertex-neighbor- pairs, add pair to edge set 
        let mapOfDicList = neighborMap |> Map.toList |> List.rev 
        mapOfDicList
        // for each vertex v
        |> List.iter (fun (v,neighbors) -> 
            // for each neigbor n of v
            neighbors
            |> List.iter (fun n ->
                // if edge already in set, ignore
                if mergeTreePairsSimplSet |> Set.contains (v,n) || mergeTreePairsSimplSet |> Set.contains (n,v) then 
                    ()
                else 
                    let newPair =
                        // determine direction of pair based on tree type/direction
                        if v=(-1) then (n,v)
                        elif n=(-1) then (v,n)
                        elif split then
                            if data.[v]>data.[n] then (v,n) else (n,v)
                        else
                            if data.[v]>data.[n] then (n,v) else (v,n)
                    // add pair to edge set
                    mergeTreePairsSimplSet <- Set.add newPair mergeTreePairsSimplSet
                )
            )
        // convert edge set to edge list
        
        let mergeTreePairsSimpl = Set.toList mergeTreePairsSimplSet

        mergeTreePairsSimpl,ppSegmentationSimpl,mtSegmentationSimpl
 

    //let plotPersistenceDiagram' (data:float []) persistencePairs = 
    //    let mutable maxVal = -infinity
    //    let mutable minVal = infinity
    //    let mutable charts :GenericChart.GenericChart list = []
    //    persistencePairs
    //    |> Array.ofSeq
    //    |> Array.iteri (fun i pair -> 
    //        if snd pair >= 0 then
    //            let v1 = data.[indexsingle data (fst pair)]
    //            let v2 = data.[indexsingle data (snd pair)]
    //            let p1 =
    //                if v1<v2 then 
    //                    indexsingle data (fst pair)
    //                else indexsingle data (snd pair)
    //            let p2 = 
    //                if v1<v2 then 
    //                    indexsingle data (snd pair)
    //                else indexsingle data (fst pair) 
    //            charts <- Chart.Line([data.[p1],data.[p1];data.[p1],data.[p2]],Color="blue")::charts
    //            if data.[indexsingle data (fst pair)] > maxVal then maxVal <- data.[indexsingle data (fst pair)]
    //            if data.[indexsingle data (fst pair)] < minVal then minVal <- data.[indexsingle data (fst pair)]
    //            if data.[indexsingle data (snd pair)] > maxVal then maxVal <- data.[indexsingle data (snd pair)]
    //            if data.[indexsingle data (snd pair)] < minVal then minVal <- data.[indexsingle data (snd pair)]
    //        )
    //    charts <- (Chart.Line([(minVal,minVal);(maxVal,maxVal)],Color="grey",Dash=DrawingStyle.Dash))::charts
    //    charts <- (Chart.Line([(minVal,minVal+threshold);(maxVal,maxVal+threshold)],Color="grey",Dash=DrawingStyle.Dash))::charts
    //    charts |> Chart.Combine 
    //    |> Chart.withX_Axis (myAxis "")// (-0.5,3.))
    //    |> Chart.withY_Axis (myAxis "")


    //let plotMergeTree' (data:float []) mergeTreePairs direction = 
    //    let mutable charts = []
    //    let mutable maxVal = -infinity
    //    let mutable minVal = infinity

    //    mergeTreePairs
    //    |> Array.ofSeq
    //    |> Array.iter (fun pair -> 
    //        if data.[indexsingle data (fst pair)] > maxVal then maxVal <- data.[indexsingle data (fst pair)]
    //        if data.[indexsingle data (fst pair)] < minVal then minVal <- data.[indexsingle data (fst pair)]
    //        if data.[indexsingle data (snd pair)] > maxVal then maxVal <- data.[indexsingle data (snd pair)]
    //        if data.[indexsingle data (snd pair)] < minVal then minVal <- data.[indexsingle data (snd pair)]
    //        )

    //    mergeTreePairs
    //    |> Array.ofSeq 
    //    |> Array.iter (fun pair -> 
    //        let a = fst pair
    //        let b = snd pair
    //        if snd pair < 0 then    
    //            if direction = Direction.Split then
    //                charts <- (Chart.Line([(a,data.[indexsingle data a]);(a,minVal-0.1*(maxVal-minVal)) ],Color="blue",Width=1.))::charts
    //            else 
    //                charts <- (Chart.Line([(a,data.[indexsingle data a]);(a,maxVal+0.1*(maxVal-minVal)) ],Color="blue",Width=1.))::charts
    //        else
    //            charts <- (Chart.Line([(a,data.[indexsingle data a]);(b,data.[indexsingle data b]) ],Color="blue",Width=1.))::charts
    //        )
    //    charts |> Chart.Combine
    //    |> styleChart "" ""

    //let plotSegmentation' data segmentation =
    //    let mutable chartsA = []

    //    let dic = 
    //        segmentation 
    //        |> Array.indexed 
    //        |> Array.map (fun (a,b) -> b,a)
    //        |> Map.ofArray

    //    let nSeg = segmentation.Length

    //    data
    //    |> Array.iteri (fun i _ -> 
    //        if i>0 then 
    //            let col = segmentation.[i]%23
    //            chartsA <- Chart.Line( [ (float i - 0.5,data.[i] * 0.5 + data.[i-1] * 0.5);(float i,data.[i]) ],Color=colors.[col])::chartsA
    //        if i < data.Length-1 then     
    //            let col = segmentation.[i]%23
    //            chartsA <- Chart.Line( [ (float i + 0.5,data.[i] * 0.5 + data.[i+1] * 0.5);(float i,data.[i]) ],Color=colors.[col])::chartsA
    //        )
    //    chartsA |> Chart.Combine |> styleChart "" ""


    //let kk =
    //    [|0.023872;0.025756;0.115488;0.284967;0.508611;1.000000;0.647759;0.340945;0.187369;0.173648;0.500369;0.412591;0.473907;0.565233;
    //    0.357760;0.306837;0.484967;0.318439;0.288924;0.219217;0.188139;0.165209;0.089615;0.064551;0.045101;0.045462;0.061143;0.135332;0.054941;0.048056;0.047446;0.033650;0.025662;0.029593;0.030523;0.028509;0.040460;
    //    0.018154;0.045913;0.006574;0.015010;0.004261;0.015020;0.010787;0.005175;0.017486;0.016645;0.011349;0.009728;0.005602;0.006621;0.012780;0.013236;0.008958;0.010630;0.009805;0.004887;0.003469;0.004000;0.000000|]

    //let rawChart rawData = 
    //    rawData |> Seq.indexed |> Chart.Line |> styleChart "" ""

    //let plotPeristenceDiagram rawData =
    //    let data = paddData rawData
    //    //let data = rawData
    //    let (persistencePairsSplit, mergeTreePairsSplit, segmentationSplit) = computePPMT data Direction.Split
    //    plotPersistenceDiagram' data persistencePairsSplit 
    //    |> Chart.withTitle "Persistence Diagram for Split Tree" //|> Chart.withSize (1200.,700.) |> Chart.Show

    //let plotMergeTree rawData = 
    //    let data = paddData rawData
    //    //let data = rawData
    //    let raw = Chart.Line(data|> Seq.indexed,"raw",Color="grey",Dash=DrawingStyle.Dash) 
    //    let (persistencePairsSplit, mergeTreePairsSplit, segmentationSplit) = computePPMT data Direction.Split
    //    [raw;plotMergeTree' data mergeTreePairsSplit Direction.Split]
    //    |> Chart.Combine |> styleChart "" "" |> Chart.withTitle "Split Tree"

    //let plotMergeTreeSimp rawData = 
    //    let data = paddData rawData
    //    //let data = rawData
    //    let raw = Chart.Line(data|> Seq.indexed,"raw",Color="grey",Dash=DrawingStyle.Dash) 
    //    let (persistencePairsSplit, mergeTreePairsSplit, segmentationSplit) = computePPMT data Direction.Split
    //    let (mergeTreePairsSimplSplit,segmentationSimplSplit) = simplifyMergeTreeAndSeg data (Array.ofSeq mergeTreePairsSplit) (Array.ofSeq persistencePairsSplit) segmentationSplit threshold Direction.Split
    //    [raw;plotMergeTree' data mergeTreePairsSimplSplit Direction.Split] 
    //    |> Chart.Combine |> Chart.withTitle "Simplified Split Tree"
    
    //let plotSegmentation rawData = 
    //    let data = paddData rawData
    //    //let data = rawData
    //    let (persistencePairsSplit, mergeTreePairsSplit, segmentationSplit) = computePPMT data Direction.Split
    //    plotSegmentation' data segmentationSplit 
    //    |> Chart.withTitle "Segmentation for Split Tree" 

    //let plotSegmentationSimp rawData = 
    //    let data = paddData rawData
    //    //let data = rawData
    //    let (persistencePairsSplit, mergeTreePairsSplit, segmentationSplit) = computePPMT data Direction.Split
    //    let (mergeTreePairsSimplSplit,segmentationSimplSplit) = simplifyMergeTreeAndSeg data (Array.ofSeq mergeTreePairsSplit) (Array.ofSeq persistencePairsSplit) segmentationSplit threshold Direction.Split
    //    plotSegmentation' data segmentationSimplSplit 
    //    |> Chart.withTitle "Segmentation for Split Tree after Simplification" 
