namespace FSharp.Stats.ML.Unsupervised

open System
open System.Collections.Generic
open Priority_Queue
open FSharp.Stats
open FSharp.Stats.ML

module HierarchicalClustering = 
    let fromFileWithSep (separator:char) (filePath) =     
        // The function is implemented using a sequence expression
        seq {   let sr = System.IO.File.OpenText(filePath)
                while not sr.EndOfStream do 
                    let line = sr.ReadLine() 
                    let words = line.Split separator//[|',';' ';'\t'|] 
                    yield words }

    

    /// Euclidean distance of two coordinate arrays
    type Distance<'a> = 'a -> 'a -> float
    let inline euclidean (a1:array<'a>) (a2:array<'a>) = 
        let dim = min a1.Length a2.Length
        let mutable dist = LanguagePrimitives.GenericZero< 'a > 
        for i in 0 .. (dim - 1) do
            let x = a1.[i] - a2.[i]
            dist <- dist + (x * x)
        float dist
        |> sqrt




    
    /// The linkage criterion determines the distance between sets of observations as a function of the pairwise distances between observations
    module Linker =
        
        //Lance and Williams Formula
        /// Signiture type for Lance and Williams Linker functions
        /// D(A u B,C) = alpa1 d(A,C) +  alpa2 d(B,C) + beta d(A,B) + gamma |d(A,C) - d(BC))|
        type LancWilliamsLinker = int*int*int -> float -> float-> float-> float

        /// <summary>Single linkage criterion<br />Calculates the minimal distance between all elements of a cluster<br />d(A u B, C)</summary>
        /// <remarks></remarks>
        /// <param name="mcABC"></param>
        /// <param name="dAB"></param>
        /// <param name="dAC"></param>
        /// <param name="dBC"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let singleLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
            //0.5 * dAC + 0.5 * dBC + 0.0 * dAB + (- 0.5 * abs(dAC - dBC))
            0.5 * dAC + 0.5 * dBC + (- 0.5 * abs(dAC - dBC))

        /// <summary>Complete linkage criterion<br />Calculates the <br />d(A u B, C)</summary>
        /// <remarks></remarks>
        /// <param name="mcABC"></param>
        /// <param name="dAB"></param>
        /// <param name="dAC"></param>
        /// <param name="dBC"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let completeLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
            0.5 * dAC + 0.5 * dBC + (0.5 * abs(dAC - dBC))
    
        /// <summary>Median linkage criterion<br />Calculates the <br />d(A u B, C)</summary>
        /// <remarks></remarks>
        /// <param name="mcABC"></param>
        /// <param name="dAB"></param>
        /// <param name="dAC"></param>
        /// <param name="dBC"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let medianLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
            0.5 * dAC + 0.5 * dBC + (- 1./4.) * dAB

        /// <summary>Weighted Group Average linkage criterion<br />Calculates the <br />d(A u B, C)</summary>
        /// <remarks></remarks>
        /// <param name="mcABC"></param>
        /// <param name="dAB"></param>
        /// <param name="dAC"></param>
        /// <param name="dBC"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let weightedGroupAverageLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
            0.5 * dAC + 0.5 * dBC
    
        /// <summary>Unweighted Group Average linkage criterion (UPGMA)<br />Calculates the <br />d(A u B, C)</summary>
        /// <remarks></remarks>
        /// <param name="mcABC"></param>
        /// <param name="dAB"></param>
        /// <param name="dAC"></param>
        /// <param name="dBC"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let upgmaLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
            let mA,mB,mC = mcABC
            let alpa1 = float mA / float (mA + mB)
            let alpa2 = float mB / float (mA + mB)
            alpa1 * dAC + alpa2 * dBC

        /// <summary>Centroid linkage criterion (UPGMA)<br />Calculates the <br />d(A u B, C)</summary>
        /// <remarks></remarks>
        /// <param name="mcABC"></param>
        /// <param name="dAB"></param>
        /// <param name="dAC"></param>
        /// <param name="dBC"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let centroidLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
            let mA,mB,mC = mcABC
            let alpa1 = float mA / float (mA + mB)
            let alpa2 = float mB / float (mA + mB)
            let beta  = - (float (mA * mB) / float ((mA + mB) * (mA + mB)))
            alpa1 * dAC + alpa2 * dBC + beta * dAB

        /// <summary>Ward linkage criterion (UPGMA)<br />Calculates the <br />d(A u B, C)</summary>
        /// <remarks></remarks>
        /// <param name="mcABC"></param>
        /// <param name="dAB"></param>
        /// <param name="dAC"></param>
        /// <param name="dBC"></param>
        /// <returns></returns>
        /// <example>
        /// <code>
        /// </code>
        /// </example>
        let wardLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
            let mA,mB,mC = 
                let mA,mB,mC =mcABC
                (float mA,float mB,float mC)
            let mABC = mA + mB + mC
            let alpa1 =  (mA + mC) / mABC
            let alpa2 =  (mB + mC) / mABC
            let beta  = - (mC / mABC)
            alpa1 * dAC + alpa2 * dBC + beta * dAB


    // ######################        
    /// Binary distance tree
    type Cluster<'T> = 
        ///ID * distance * leafCount * cluster left * cluster right
        | Node of int * float * int * Cluster<'T> * Cluster<'T>
        ///ID * leafCount * Tag
        | Leaf of int * int * 'T

    /// <summary>Returns cluster Id</summary>
    /// <remarks></remarks>
    /// <param name="c"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getClusterId (c:Cluster<'T>) =
        match c with
        | Cluster.Node(id,_,_,_,_) -> id   
        | Cluster.Leaf(id,_,_) -> id

    /// <summary>Returns cleaf value</summary>
    /// <remarks></remarks>
    /// <param name="c"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let tryGetLeafValue (c:Cluster<'T>) =
        match c with
        | Cluster.Node(_) -> None
        | Cluster.Leaf(_,_,value) -> Some value

    /// <summary>Returns cluster member count</summary>
    /// <remarks></remarks>
    /// <param name="c"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getClusterMemberCount (c:Cluster<'T>) =
        match c with        
        | Cluster.Node(id,_,mc,_,_) -> mc   
        | Cluster.Leaf(id,mc,_)     -> mc
    
    /// Create a cluster Node containing the two given subbranches
    let createCluster<'T> (id:int) (dist:float) (left:Cluster<'T>) (right:Cluster<'T>) =        
        let leaveCount = getClusterMemberCount left + getClusterMemberCount right
        Cluster.Node(id, dist, leaveCount, left, right)

    /// <summary>Create a cluster Leaf</summary>
    /// <remarks></remarks>
    /// <param name="id"></param>
    /// <param name="value"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let createClusterValue (id:int) (value:'T) =
        Cluster.Leaf(id,1,value)

    /// <summary>Returns a list of the distances between the subclusters</summary>
    /// <remarks></remarks>
    /// <param name="cluster"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getDistancesOfCluster (cluster: Cluster<'T>) =
        let rec loop l cluster=
            match cluster with
            | Node (_,dist,_,c1,c2) ->
                dist :: l
                |> List.append (loop [] c1)
                |> List.append (loop [] c2)
            | _ -> l
        loop [] cluster

    /// <summary>Returns a list of the leaf names</summary>
    /// <remarks></remarks>
    /// <param name="cluster"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getLeafsOfCluster (cluster: Cluster<'T>) =
        let rec loop l cluster=
            match cluster with
            | Node (_,dist,_,c1,c2) ->
                l
                |> List.append (loop [] c1)
                |> List.append (loop [] c2)
            | Leaf (_,_,l) -> [l]
        loop [] cluster

    /// <summary>Returns a list of the leaf names</summary>
    /// <remarks></remarks>
    /// <param name="cluster"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getLeafNamesOfCluster (cluster: Cluster<'T>) =
        let rec loop l cluster=
            match cluster with
            | Node (_,dist,_,c1,c2) ->
                l
                |> List.append (loop [] c1)
                |> List.append (loop [] c2)
            | Leaf (id,_,l) -> [id]
        loop [] cluster
        
    /// <summary>Aggregates the subbranches of a node to leafs, if the distance between them is smaller than the given distanceCutoff</summary>
    /// <remarks></remarks>
    /// <param name="distanceCutoff"></param>
    /// <param name="cluster"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let aggregateClusterByDistance distanceCutoff (cluster: Cluster<'T>) =
        let rec aggregate cluster =
            match cluster with
            | Node (id,dist,num,c1,c2) ->
                List.append (aggregate c1) (aggregate c2)
            | Leaf (id,x,name) -> [name]
        let rec loop cluster =
            match cluster with
            | Node (id,dist,num,c1,c2) when dist < distanceCutoff ->
                Leaf (id,num, List.append (aggregate c1) (aggregate c2))
            | Node (id,dist,num,c1,c2) ->
                Node (id,dist,num,loop c1,loop c2)
            | Leaf (id,x,name) -> Leaf (id,x,[name])
        loop cluster
    

    /// <summary>Aggregates the subbranches of a node to leafs, if the predicate function taking the distance and the number of subLeafs returns true</summary>
    /// <remarks></remarks>
    /// <param name="predicate"></param>
    /// <param name="cluster"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let aggregateClusterBy (predicate : float -> int -> bool) (cluster: Cluster<'T>) =
        let rec aggregate cluster =
            match cluster with
            | Node (id,dist,num,c1,c2) ->
                List.append (aggregate c1) (aggregate c2)
            | Leaf (id,x,name) -> [name]
        let rec loop cluster =
            match cluster with
            | Node (id,dist,num,c1,c2) when predicate dist num ->
                Leaf (id,num, List.append (aggregate c1) (aggregate c2))
            | Node (id,dist,num,c1,c2) ->
                Node (id,dist,num,loop c1,loop c2)
            | Leaf (id,x,name) -> Leaf (id,x,[name])
        loop cluster

    /// <summary>Maps the tags of the leafs of the cluster by applying a given mapping function</summary>
    /// <remarks></remarks>
    /// <param name="mapF"></param>
    /// <param name="cluster"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let mapClusterLeaftags (mapF : 'T -> 'U) (cluster: Cluster<'T>) =
        let rec loop cluster =
            match cluster with
            | Node (id,dist,num,c1,c2) ->
                Node (id,dist,num,loop c1,loop c2)
            | Leaf (id,x,name) -> Leaf (id,x, mapF name)
        loop cluster

    /// Class for chaching already calculated distances to speed up cluster build
    type DistanceCaching<'T>
        ( distance:Distance<'T>,
        linker:Linker.LancWilliamsLinker
        ) =
    
        // Dictionary chache    
        let dChache  = new System.Collections.Generic.Dictionary<(int*int), float>()                 
    
        
        let calculateSinglePairDistance (c1Id:int) (c1Values:'T) (c2Id:int) (c2Values:'T) =
            let id = if c1Id < c2Id then (c1Id,c2Id) else (c2Id,c1Id)
            if dChache.ContainsKey(id) then
                dChache.Item(id)
            else
                let dist = distance c1Values c2Values                
                dChache.Add(id,dist) |> ignore
                dist


        let calculateDistanceFromABC c1Id c2Id cA cB cC =
            let get (c1Id:int) (c2Id:int) =
                let id = if c1Id < c2Id then (c1Id,c2Id) else (c2Id,c1Id)
                dChache.Item(id)        
            let caId,cbId,ccId = (getClusterId cA),(getClusterId cB),(getClusterId cC)
            let ab,ac,bc = (get caId cbId),(get caId ccId),(get cbId ccId)
            let mA,mB,mC = (getClusterMemberCount cA),(getClusterMemberCount cB),(getClusterMemberCount cC)
            let dist = linker (mA,mB,mC)ab ac bc   
            let id = if c1Id < c2Id then (c1Id,c2Id) else (c2Id,c1Id)
            if not (dChache.ContainsKey(id)) then dChache.Add(id,dist)
            dist


        member this.calcDistance (c1:Cluster<'T>) (c2:Cluster<'T>) =    
            match c1,c2 with
            | Cluster.Leaf(c1Id,mc1,c1Values)   ,Cluster.Leaf(c2Id,mc2,c2Values)     -> calculateSinglePairDistance c1Id c1Values c2Id c2Values
            | Cluster.Node(c1Id,dist,mc1,cA,cB) ,Cluster.Leaf(c2Id,mc2,c2Value)      -> calculateDistanceFromABC c1Id c2Id cA cB c2                                                                                                                                         
            | Cluster.Leaf(c2Id,mc2,c2Value)    ,Cluster.Node(c1Id,dist,mc1,cA,cB)   -> calculateDistanceFromABC c1Id c2Id cA cB c1
            | Cluster.Node(c2Id,mc2,dist2,cA,cB),Cluster.Node(c1Id,mc1,dist1,_,_)    -> calculateDistanceFromABC c1Id c2Id cA cB c2





    //####------------------------###








    type ClusterIndex<'T>(c:Cluster<'T>) =
        inherit FastPriorityQueueNode()
        member this.Cluster = c

        
    let initPairsNR (cachedDist:DistanceCaching<'T>) (arr :array<'T>) =
        let dict = new Dictionary<Cluster<'T>,FastPriorityQueue<ClusterIndex<_>>>()

        for i=0 to arr.Length-1 do        
        //todo
            let tmpQueue = FastPriorityQueue<ClusterIndex<'T>>(10000)
            let leftCluster : 'T Cluster = createClusterValue (i) arr.[i]
            for ii=i+1 to arr.Length-1 do
                let rightCluster : 'T Cluster = createClusterValue (ii) arr.[ii]
                let dist = cachedDist.calcDistance leftCluster rightCluster
                tmpQueue.Enqueue(ClusterIndex(rightCluster), float32 dist)
            dict.Add(leftCluster,tmpQueue)

        dict



    let minDequeue (source: Dictionary<Cluster<'T>,FastPriorityQueue<ClusterIndex<_>>>) =    

        use mutable e = source.GetEnumerator()
        if not (e.MoveNext()) then
            invalidArg "source" "The input sequence was empty."

        let first = e.Current
        let mutable accv = first
        let accq = first.Value
        while not (source.ContainsKey(accq.First.Cluster)) do
            accq.Dequeue() |> ignore
        let mutable acc = accq.First.Priority


        while e.MoveNext() do
            let currv = e.Current
            let currq = currv.Value
            if currq.Count > 0 then
                if source.ContainsKey(currq.First.Cluster) then
                    let curr = currq.First.Priority

                    if curr < acc then
                        acc <- curr
                        accv <- currv
                else
                    currv.Value.Dequeue() 
                    |> ignore            
        
        accv


            



    // Loops while all clusters are merged
    let rec whileLoop (cachedDist:DistanceCaching<'T>)  (source: Dictionary<Cluster<'T>,FastPriorityQueue<ClusterIndex<_>>>)  (countIndex:int) =
        if source.Count > 1 then
            // find closest 
            let minKv = minDequeue source 
            let leftCluster = minKv.Key
            let rightCluster = minKv.Value.First.Cluster
            let dist = minKv.Value.First.Priority |> float //cachedDist.calcDistance leftCluster rightCluster
            
            // remove both from input dict
            source.Remove(leftCluster) |> ignore
            source.Remove(rightCluster) |> ignore

            // Add new cluster to input
            let cluster = createCluster (countIndex) dist leftCluster rightCluster
            
            //todo
            let tmpQueue = FastPriorityQueue<ClusterIndex<_>>(10000)
            for kv in source do

                let dist' = cachedDist.calcDistance cluster kv.Key 
                // Add new cluster to queues
                kv.Value.Enqueue(ClusterIndex(cluster), float32 dist')

                tmpQueue.Enqueue(ClusterIndex(kv.Key), float32 dist')
            source.Add(cluster,tmpQueue)

            whileLoop cachedDist source (countIndex+1)
        else
            source

    /// Builds a hierarchy of clusters of data containing cluster labels
    let generate<'T when 'T:equality> (distance:Distance<'T>) (linker:Linker.LancWilliamsLinker) (data:'T[]) = 
    
        //#region Distance Caching
        let cachedDist = DistanceCaching<'T> (distance,linker)
        
        let input = initPairsNR cachedDist data
        whileLoop cachedDist input data.Length


    let getClusterMemberLabels (cluster:Cluster<'a>) =
        let rec loop cluster (acc:int list) =
            match cluster with
            | Leaf (x,_,v) -> [(x::acc) |> List.rev]
            | Node (x, dist,_, left, right) -> (loop left (x::acc)) @ (loop right (x::acc))
        loop cluster []




    let flattenHClust (clusterTree:Cluster<'T>) = 
        let rec loop (c:Cluster<'T>) = [    
            match c with
            | Node (id,dist,cM,lc,rc)  -> 
                yield! loop lc
                yield! loop rc
            | Leaf (id,_,_)            -> yield c
            ]

        loop clusterTree






    /// <summary>cuts tree in chosen amount of clusters</summary>
    /// <remarks></remarks>
    /// <param name="desiredNumber"></param>
    /// <param name="clusterTree"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let cutHClust (desiredNumber:int) (clusterTree:Cluster<'T>) =    
        let getInversDistance (c:Cluster<'T>) =
            match c with
            | Node (id,dist,cM,lc,rc)  -> - dist                                                            
            | Leaf (id,_,_)            -> 0.0
        
        let toClusterList (clist: Cluster<'T> list) =
            match clist with
            | c::tail -> 
                match c with
                | Node (id,dist,cM,lc,rc)  -> lc::rc::tail                                                                    
                | Leaf (id,_,_)            -> c::tail
            | []      -> []
        
        let rec loop cN (clist: Cluster<'T> list) =                                             
            if clist.Length >= cN then
                clist
            else
                let sortedCList = 
                    toClusterList clist
                    |> List.sortBy getInversDistance 
                loop cN sortedCList
        
        loop desiredNumber [clusterTree]
        |> List.map flattenHClust

        
    /// <summary>Converts clusters into string seq</summary>
    /// <remarks></remarks>
    /// <param name="clusterTree"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let printHClust (clusterTree:Cluster<'T>) =    
            let rec printLoop (c) = seq {    
                match c with
                | Node (id,dist,cM,lc,rc)  -> 
                    yield  sprintf "{ \"name\" : %i,\n  \"children\": [" id
                    yield! printLoop lc
                    yield  sprintf ","
                    yield! printLoop rc
                    yield  sprintf "] }" 
                | Leaf (id,_,_)            -> yield  sprintf "{ \"id\" : %i,\n  \"children\": [] }" id
            }
            printLoop clusterTree

    /// <summary>Converts clusters into string seq</summary>
    /// <remarks></remarks>
    /// <param name="clusterTree"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let get (clusterTree:Cluster<'T>) =    
            let rec loop (c) = seq {    
                match c with
                | Node (id,dist,cM,lc,rc)  -> 
                    yield  dist,getClusterId lc, getClusterId rc
                    yield! loop lc
                    yield! loop rc
                | Leaf (id,_,_)            -> ()
            }
            loop clusterTree
    let getWithClusterName (clusterTree:Cluster<'T>) =    
            let rec loop (c) = seq {    
                match c with
                | Node (id,dist,cM,lc,rc)  -> 
                    yield  id,dist,getClusterId lc, getClusterId rc
                    yield! loop lc
                    yield! loop rc
                | Leaf (id,_,_)            -> ()
            }
            loop clusterTree




    /// <summary>Returns a list of the distances between the subclusters</summary>
    /// <remarks></remarks>
    /// <param name="cluster"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let getDistancesAndLabels (cluster: Cluster<'T>) =
        let rec loop l cluster=
            match cluster with
            | Node (id,dist,_,c1,c2) ->
                (id,dist) :: l
                |> List.append (loop [] c1)
                |> List.append (loop [] c2)
            | _ -> l
        loop [] cluster


    let usedDistancesAndLabels cluster = 
        getDistancesAndLabels cluster 
        |> List.groupBy snd 
        |> List.map (fun (x,y) -> (x, (List.map fst y )))


    let getLeftChild cluster = 
        match cluster with 
        | Node (id,dist,mc,c1,c2) -> c1
        | Leaf (id,mc,value) -> createClusterValue id value

    let getRightChild cluster =
        match cluster with 
        | Node (id,dist,mc,c1,c2) -> c2
        | Leaf (id,mc,value) -> createClusterValue id value

    let getDistance cluster = 
        match cluster with 
        | Node (id,dist,mc,c1,c2) -> dist
        | Leaf (id,mc,value) -> -1.


