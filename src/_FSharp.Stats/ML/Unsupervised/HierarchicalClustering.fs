namespace FSharp.Stats.ML.Unsupervised

open System
open System.Collections.Generic

open FSharp.Stats.ML

/// Agglomerative hierarchical clustering
module HierarchicalClustering =
    
    //open FSharpAux
    /// The linkage criterion determines the distance between sets of observations as a function of the pairwise distances between observations
    module Linker =
        
        //Lance and Williams Formula
        /// Signiture type for Lance and Williams Linker functions
        /// D(A u B,C) = alpa1 d(A,C) +  alpa2 d(B,C) + beta d(A,B) + gamma |d(A,C) - d(BC))|
        type LancWilliamsLinker = int*int*int -> float -> float-> float-> float

        /// Single linkage criterion
        /// Calculates the minimal distance between all elements of a cluster
        /// d(A u B, C)
        let singleLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
            //0.5 * dAC + 0.5 * dBC + 0.0 * dAB + (- 0.5 * abs(dAC - dBC))
            0.5 * dAC + 0.5 * dBC + (- 0.5 * abs(dAC - dBC))

        /// Complete linkage criterion
        /// Calculates the 
        /// d(A u B, C)
        let completeLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
            0.5 * dAC + 0.5 * dBC + (0.5 * abs(dAC - dBC))
    
        /// Median linkage criterion
        /// Calculates the 
        /// d(A u B, C)
        let medianLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
            0.5 * dAC + 0.5 * dBC + (- 1./4.) * dAB

        /// Weighted Group Average linkage criterion
        /// Calculates the 
        /// d(A u B, C)
        let weightedGroupAverageLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
            0.5 * dAC + 0.5 * dBC
    
        /// Unweighted Group Average linkage criterion (UPGMA)
        /// Calculates the 
        /// d(A u B, C)
        let upgmaLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
             let mA,mB,mC = mcABC
             let alpa1 = float mA / float (mA + mB)
             let alpa2 = float mB / float (mA + mB)
             alpa1 * dAC + alpa2 * dBC

        /// Centroid linkage criterion (UPGMA)
        /// Calculates the 
        /// d(A u B, C)
        let centroidLwLinker (mcABC:int*int*int) (dAB:float) (dAC:float) (dBC:float) =
             let mA,mB,mC = mcABC
             let alpa1 = float mA / float (mA + mB)
             let alpa2 = float mB / float (mA + mB)
             let beta  = - (float (mA * mB) / float ((mA + mB) * (mA + mB)))
             alpa1 * dAC + alpa2 * dBC + beta * dAB

        /// Ward linkage criterion (UPGMA)
        /// Calculates the 
        /// d(A u B, C)
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
    // Cluster
    type Cluster<'T> = 
        //------- ID * distance * leafCount * cluster left * cluster right
        | Node of int * float * int * Cluster<'T> * Cluster<'T>
        | Leaf of int * int * 'T

    // Returns cluster Id
    let getClusterId (c:Cluster<'T>) =
        match c with
        | Cluster.Node(id,_,_,_,_) -> id   
        | Cluster.Leaf(id,_,_) -> id

    let tryGetLeafValue (c:Cluster<'T>) =
        match c with
        | Cluster.Node(_) -> None
        | Cluster.Leaf(_,_,value) -> Some value

    // Returns cluster member count
    let private getClusterMemberCount (c:Cluster<'T>) =
        match c with        
        | Cluster.Node(id,_,mc,_,_) -> mc   
        | Cluster.Leaf(id,mc,_)     -> mc
    

    let createCluster<'T> (id:int) (dist:float) (left:Cluster<'T>) (right:Cluster<'T>) =        
        let leaveCount = getClusterMemberCount left + getClusterMemberCount right
        Cluster.Node(id, dist, leaveCount, left, right)

    let createClusterValue (id:int) (value:'T) =
        Cluster.Leaf(id,1,value)











    /// Class for chaching already calculated distances to speed up cluster build
    type DistanceCaching<'T>
        ( distance:DistanceMetrics.Distance<'T>,
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




 


    /// Builds a hierarchy of clusters of data containing cluster labels
    let generate<'T> (distance:DistanceMetrics.Distance<'T>) (linker:Linker.LancWilliamsLinker) (data:seq<'T>) = 
 
        //#region Distance Caching
        let cachedDist = DistanceCaching<'T> (distance,linker)

        //#endregion Distance Caching
        
        //#region Cluster Helper       
        // Removes cluster from list
        let removeCluster (inputList:Cluster<'T> list) (c1:Cluster<'T>) (c2:Cluster<'T>) = 
            let idC1 = getClusterId c1
            let idC2 = getClusterId c2
            let rec remove (inputL:Cluster<'T> list) acc =
                match inputL with
                | head::tail -> let idH = getClusterId head
                                if idH = idC1 || idH = idC2 then
                                    remove tail acc
                                else
                                    remove tail (head::acc)
                | []        -> acc |> List.rev
            remove inputList []
             
              
        // Finds cluster pair with min distance
        let findMinDinstancePair (cachedDist:DistanceCaching<'T>) (inputList:Cluster<'T> list) =    
            // Inner loop calculates the distance
            let rec innerLoop oHead inputList acc =               
                    match inputList with
                    | head::tail -> let dist = cachedDist.calcDistance oHead head
                                    let _,_,cmin = acc
                                    if dist < cmin then
                                        innerLoop oHead tail (oHead,head,dist)
                                    else
                                        innerLoop oHead tail acc 
                    | []         -> acc
            // Outer loop 
            let rec outerLoop inputList acc=
                match inputList with
                | head::tail -> outerLoop tail (innerLoop head tail acc)
                | [] -> acc    
        
            match inputList with
            | h1::h2::tail -> Some(outerLoop (inputList) (h1,h2,(cachedDist.calcDistance h1 h2)))
            | _            -> None                


        // Loops while all clusters are merged
        let rec whileLoop input (count:int) =
            match (findMinDinstancePair cachedDist input) with
            | Some(c1,c2,dist) -> //remove
                                    let rInput = removeCluster input c1 c2
                                    //let mCount = getClusterMemberCount c1 + getClusterMemberCount c2
                                    let nInput = ((createCluster (count) dist c1 c2)::rInput)
                                    whileLoop nInput (count + 1 )
            | None             -> input
    
        // Create a new cluster for each data point                        
        let clusterList = data |> Seq.mapi (fun i dp -> createClusterValue i dp) |> Seq.toList
        // Iterate
        (whileLoop clusterList (clusterList.Length))|> List.head





    ///// Builds a hierarchy of clusters of data containing cluster labels
    //let generate'<'T> (distance:DistanceMetrics.Distance<'T>) (linker:Linker.LancWilliamsLinker) (data:seq<'T>) = 
 
    //    //#region Distance Caching
    //    let cachedDist = DistanceCaching<'T> (distance,linker)

    //    //#endregion Distance Caching
        
    //    //#region Cluster Helper       
    //    // Removes cluster from list
    //    let removeCluster (inputList:List<Cluster<'T>>) (c1:Cluster<'T>) (c2:Cluster<'T>) = 
    //        let idC1 = getClusterId c1
    //        let idC2 = getClusterId c2
    //        inputList.RemoveAll(fun c -> 
    //            let cId = getClusterId c
    //            cId = idC1 || cId = idC2 ) |> ignore
    //        inputList
             

    //    // Finds cluster pair with min distance
    //    let tryFindMinDistanceCluster (cachedDist:DistanceCaching<'T>) (inputList:List<Cluster<'T>>) = 
    //        let len = inputList.Count
            
    //        let rec innerLoop ii c1 acc =
    //            printfn "H ii %i" ii
    //            if ii < len then
    //                let c2 = inputList.[ii]
    //                let dist = cachedDist.calcDistance c1 c2
    //                let _,_,cmin = acc
    //                if dist < cmin then
    //                    innerLoop (ii+1) c1 (c1,c2,dist)
    //                else
    //                    innerLoop (ii+1) c1 acc
    //            else
    //                acc

    //        let rec outerLoop i acc =
    //            printfn "Outer i %i len %i" i len
    //            if i < len-1 then
    //                let acc' = innerLoop (i+1) (inputList.[i]) acc
    //                outerLoop (i+1) acc'                    
    //            else
    //                acc

    //        if len > 1 then
    //            let h1 = inputList.[0]
    //            let h2 = inputList.[1]
    //            let d  = cachedDist.calcDistance h1 h2
    //            Some (outerLoop 0 (h1,h2,d))
    //        else
    //            None
            
    //    // Loops while all clusters are merged
    //    let rec whileLoop (input:List<Cluster<'T>>) (count:int) =
    //        match (tryFindMinDistanceCluster cachedDist input) with
    //        | Some(c1,c2,dist) -> //remove
    //                                let tmp = removeCluster input c1 c2
    //                                tmp.Add( createCluster (count) dist c1 c2 )
    //                                //let mCount = getClusterMemberCount c1 + getClusterMemberCount c2
    //                                whileLoop tmp (count + 1 )
    //        | None             -> input

    
    //    // Create a new cluster for each data point                        
    //    let clusterList = data |> Seq.mapi (fun i dp -> createClusterValue i dp) |> List
    //    // Iterate
    //    let tmp = whileLoop clusterList (clusterList.Count)
    //    if tmp.Count > 0 then tmp.[0] else failwith "List can not be empty"









    /// Returns 
    let getClusterMemberLabels (cluster:Cluster<'a>) =
        let rec loop cluster (acc:int list) =
            match cluster with
            | Leaf (x,_,v) -> [(x::acc) |> List.rev]
            | Node (x, dist,_, left, right) -> (loop left (x::acc)) @ (loop right (x::acc))
        loop cluster []


    /// Returns a flatten list containing Leafs
    let flattenHClust (clusterTree:Cluster<'T>) = 
        let rec loop (c:Cluster<'T>) = [    
            match c with
            | Node (id,dist,cM,lc,rc)  -> yield! loop lc
                                          yield! loop rc
            | Leaf (id,_,_)            -> yield c
            ]
    
        loop clusterTree


    /// Cuts a tree, as resulting from hclust, into several groups by specifying the desired number(s).
    /// If the desired number is odd the function cut the cluster with maximal distance
    let cutHClust (desiredNumber:int) (clusterTree:Cluster<'T>) =    
        let getInversDistance (c:Cluster<'T>) =
            match c with
            | Node (id,dist,cM,lc,rc)  -> - dist                                                            
            | Leaf (id,_,_)            -> 0.0
        
        let toClusterList (clist: Cluster<'T> list) =
            match clist with
            | c::tail -> match c with
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

       
    /// Converts clusters into string seq
    let printHClust (clusterTree:Cluster<'T>) =    
            let rec printLoop (c) = seq {    
                match c with
                | Node (id,dist,cM,lc,rc)  -> yield  sprintf "{ \"name\" : %i,\n  \"children\": [" id
                                              yield! printLoop lc
                                              yield  sprintf ","
                                              yield! printLoop rc
                                              yield  sprintf "] }" 
                | Leaf (id,_,_)            -> yield  sprintf "{ \"id\" : %i,\n  \"children\": [] }" id
            }
            printLoop clusterTree



