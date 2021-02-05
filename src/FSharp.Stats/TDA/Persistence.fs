namespace FSharp.Stats.TDA

open System

module Persistence =

    let paddData rawData = 
        let rnd = System.Random(5)
        let tmp = rawData |> Array.map (fun x -> if x = 0. then rnd.NextDouble()/100000. else x)
        Array.append (Array.append [|0.|] tmp) [|0.|]

    let union a b (uf:int []) = uf.[int b] <- a

    let rec find a (uf:int[]) =
        let atmp = if a<0 then uf.Length + int a else a
        if uf.[int atmp] = atmp then
            atmp
        else 
            find uf.[int atmp] uf

    let indexsingle (da:'a[]) k = if k<0 then da.Length + k else k 

    let computePPMT data direction =
        let reverse = direction = "split"
        let smaller a b = 
            if reverse then 
                a>b 
            else 
                a < b
        let orderedPoints = 
            data
            |> Array.indexed
            |> Array.sortBy snd
            |> Array.map fst
            |> fun x -> 
                if reverse then 
                    x |> Array.toList |> List.rev |> Array.ofList
                else x

        let ufpP = Array.create data.Length -1
        let ufmT = Array.create data.Length -1

        let mutable persistencePairs    = [] 
        let mutable mergeTreePairs      = []
        let mutable segmentation :int []= Array.zeroCreate data.Length

        orderedPoints
        |> Array.iter (fun i -> 
            let va = data.[i]
            let ln = if i = 0 then nan else data.[i-1]
            let rn = if i = data.Length - 1 then nan else data.[i+1]
         
            if (smaller va ln && smaller va rn) || (nan.Equals ln && smaller va rn) || (smaller va ln && nan.Equals rn) then    
                ufpP.[i] <- i
                ufmT.[i] <- i
                segmentation.[i] <- ufpP.[i]
            elif not (nan.Equals ln) && not (nan.Equals rn) && smaller va rn && smaller ln va then
                ufpP.[i] <- find (i-1) ufpP
                ufmT.[i] <- find (i-1) ufmT
                segmentation.[i] <- ufpP.[i]
            elif not (nan.Equals ln) && not (nan.Equals rn) && smaller rn va && smaller va ln then
                ufpP.[i] <- find (i+1) ufpP
                ufmT.[i] <- find (i+1) ufmT
                segmentation.[i] <- ufpP.[i]
            else
                if nan.Equals ln then   
                    ufpP.[i] <- find (i+1) ufpP
                    ufmT.[i] <- find (i+1) ufmT
                    segmentation.[i] <- ufpP.[i]
                elif nan.Equals rn then
                    ufpP.[i] <- find (i-1) ufpP
                    ufmT.[i] <- find (i-1) ufmT
                    segmentation.[i] <- ufpP.[i]        
                else
                    ufmT.[i] <- i
                    mergeTreePairs <- (int(find(i+1) ufmT),i)::mergeTreePairs
                    mergeTreePairs <- (int(find(i-1) ufmT),i)::mergeTreePairs
                    let le = find (i-1) ufpP
                    let re = find (i+1) ufpP
                    union i (find (i-1) ufmT) ufmT
                    union i (find (i+1) ufmT) ufmT
                    if smaller data.[int le] data.[int re] then 
                        persistencePairs <- (int(find (i+1) ufpP),i)::persistencePairs
                        segmentation.[i] <- int (find (i-1) ufpP)
                        union le re ufpP
                    else 
                        persistencePairs <- (int(find(i-1) ufpP),i)::persistencePairs
                        segmentation.[i] <- int (find (i+1) ufpP)
                        union re le ufpP 
            )

        persistencePairs <- (int(find orderedPoints.[orderedPoints.Length - 1] ufpP),-1)::persistencePairs

        let ss = 
            persistencePairs
            |> List.sortBy (fun p -> 
                if snd p > 0 then 
                    data.[snd p]-data.[fst p]
                    |> Math.Abs
                else infinity
                )

        persistencePairs <- ss
        mergeTreePairs <- ((int(find (orderedPoints.[orderedPoints.Length - 1]) ufmT),-1))::mergeTreePairs     
    
        persistencePairs,mergeTreePairs |> List.rev,segmentation


    let simplifyData (data:float[]) (persistencePairs:(int*int)list) threshold =
        let sysFloatMin = 2.2250738585072014e-308
        let dataSimpl = Array.copy data
    
        let rec loop i =
        
            let pair = persistencePairs.[i]
            if snd pair < 0 then 
                loop (i+1)
            else 
                let v1 = data.[indexsingle data (fst pair)]
                let v2 = data.[indexsingle data (snd pair)]
                if v2-v1>threshold then ()
                else
                    let rec innerloopA i =
                        if data.[indexsingle data (fst pair)] < data.[indexsingle data (snd pair + i)] then 
                            let offset = float (Math.Abs(fst pair - (snd pair + 1))) * sysFloatMin
                            dataSimpl.[indexsingle data (snd pair + i)] <- data.[indexsingle data (fst pair)] - offset
                            innerloopA (i+1)
                        else ()
                    innerloopA 0
                    let rec innerloopB i =
                        if data.[indexsingle data (fst pair)] < data.[indexsingle data (snd pair - i)] then 
                            let offset = float (Math.Abs(fst pair - (snd pair - 1))) * sysFloatMin
                            dataSimpl.[indexsingle data (snd pair - i)] <- data.[indexsingle data (fst pair)] - offset
                            innerloopA (i+1)
                        else ()
                    innerloopB 0
        loop 0
        dataSimpl   


    let simplifyMergeTreeAndSeg (data:float []) mergeTreePairs (persistencePairs:(int*int)[]) segmentation threshold direction =
    
        let split = direction = "split"

        let mutable segmentationSimpl = Array.copy segmentation
        let mutable mergeTreePairsSimpl :(int*int)list= []
    
        let mutable mapOfDic = [] |> Map.ofSeq

        mergeTreePairs
        |> Array.iter (fun edge ->
            let tmpA = if Map.containsKey (fst edge) mapOfDic then mapOfDic.[fst edge] else []
            let tmpB = if Map.containsKey (snd edge) mapOfDic then mapOfDic.[snd edge] else []
            mapOfDic <- mapOfDic |> Map.add (fst edge) ((snd edge)::tmpA)
            mapOfDic <- mapOfDic |> Map.add (snd edge) ((fst edge)::tmpB)
            )

        let prune vID = 
            let n1 = mapOfDic.[vID].[0]
            let n2 = mapOfDic.[vID].[1]
            let tmpR1 = List.append (List.filter (fun x -> x <> vID) mapOfDic.[n1]) [n2]
            let tmpR2 = List.append (List.filter (fun x -> x <> vID) mapOfDic.[n2]) [n1]
            mapOfDic <- mapOfDic |> Map.remove n1 |> Map.add n1 tmpR1
            mapOfDic <- mapOfDic |> Map.remove n2 |> Map.add n2 tmpR2
            mapOfDic <- mapOfDic |> Map.add vID []

        mapOfDic <- mapOfDic |> Map.map (fun k v -> v |> List.rev)

        let rec loop i =
            if i = persistencePairs.Length - 1 then 
                () 
            else
            let pair = persistencePairs.[i]
            if fst pair < 0 || snd pair < 0 then 
                loop (i+1)
            else 
            
                let v1 = data.[indexsingle data (fst pair)]
                let v2 = data.[indexsingle data (snd pair)]
                if Math.Abs(v1-v2) > threshold then 
                    ()
                else
                    let simplifiedExtremum = 
                        if v1<v2 then 
                            if not split then 
                                fst pair
                            else snd pair
                        else    
                            if split then 
                                fst pair 
                            else snd pair
                    let keptExtremum = 
                        if simplifiedExtremum = snd pair then fst pair else snd pair

                    segmentation
                    |> Array.iteri (fun i _ -> 
                        if segmentationSimpl.[indexsingle segmentationSimpl i] = simplifiedExtremum then 
                            segmentationSimpl.[indexsingle segmentationSimpl i] <- segmentationSimpl.[indexsingle segmentationSimpl keptExtremum]
                        )
                    let tmpR1 = (List.filter (fun x -> x <> simplifiedExtremum) mapOfDic.[keptExtremum])
                    mapOfDic <- mapOfDic |> Map.add keptExtremum tmpR1     
                    mapOfDic <- mapOfDic |> Map.add simplifiedExtremum []
                    if mapOfDic.[keptExtremum].Length = 2 then prune (int keptExtremum)
                    loop (i+1)
        loop 0
    
        let mutable mergeTreePirsSimplSet :( int*int) list = []
    
        let mapOfDicList = mapOfDic |> Map.toList |> List.rev 
        mapOfDicList
        |> List.iter (fun (v,neighbors) -> 
            neighbors
            |> List.iter (fun n -> 
                if mergeTreePirsSimplSet |> List.contains (v,n) || mergeTreePirsSimplSet |> List.contains (n,v) then 
                    ()
                else 
                    mergeTreePirsSimplSet <- (v,n)::mergeTreePirsSimplSet
                )
            )
        mergeTreePirsSimplSet
        |> List.iteri (fun i p -> 
            let p = if fst p = -1 then (snd p,fst p) else p
            mergeTreePairsSimpl <- p::mergeTreePairsSimpl
            )

        mergeTreePairsSimpl,segmentationSimpl
 

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
    //            if direction = "split" then
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
    //    let (persistencePairsSplit, mergeTreePairsSplit, segmentationSplit) = computePPMT data "split"
    //    plotPersistenceDiagram' data persistencePairsSplit 
    //    |> Chart.withTitle "Persistence Diagram for Split Tree" //|> Chart.withSize (1200.,700.) |> Chart.Show

    //let plotMergeTree rawData = 
    //    let data = paddData rawData
    //    //let data = rawData
    //    let raw = Chart.Line(data|> Seq.indexed,"raw",Color="grey",Dash=DrawingStyle.Dash) 
    //    let (persistencePairsSplit, mergeTreePairsSplit, segmentationSplit) = computePPMT data "split"
    //    [raw;plotMergeTree' data mergeTreePairsSplit "split"]
    //    |> Chart.Combine |> styleChart "" "" |> Chart.withTitle "Split Tree"

    //let plotMergeTreeSimp rawData = 
    //    let data = paddData rawData
    //    //let data = rawData
    //    let raw = Chart.Line(data|> Seq.indexed,"raw",Color="grey",Dash=DrawingStyle.Dash) 
    //    let (persistencePairsSplit, mergeTreePairsSplit, segmentationSplit) = computePPMT data "split"
    //    let (mergeTreePairsSimplSplit,segmentationSimplSplit) = simplifyMergeTreeAndSeg data (Array.ofSeq mergeTreePairsSplit) (Array.ofSeq persistencePairsSplit) segmentationSplit threshold "split"
    //    [raw;plotMergeTree' data mergeTreePairsSimplSplit "split"] 
    //    |> Chart.Combine |> Chart.withTitle "Simplified Split Tree"
    
    //let plotSegmentation rawData = 
    //    let data = paddData rawData
    //    //let data = rawData
    //    let (persistencePairsSplit, mergeTreePairsSplit, segmentationSplit) = computePPMT data "split"
    //    plotSegmentation' data segmentationSplit 
    //    |> Chart.withTitle "Segmentation for Split Tree" 

    //let plotSegmentationSimp rawData = 
    //    let data = paddData rawData
    //    //let data = rawData
    //    let (persistencePairsSplit, mergeTreePairsSplit, segmentationSplit) = computePPMT data "split"
    //    let (mergeTreePairsSimplSplit,segmentationSimplSplit) = simplifyMergeTreeAndSeg data (Array.ofSeq mergeTreePairsSplit) (Array.ofSeq persistencePairsSplit) segmentationSplit threshold "split"
    //    plotSegmentation' data segmentationSimplSplit 
    //    |> Chart.withTitle "Segmentation for Split Tree after Simplification" 
