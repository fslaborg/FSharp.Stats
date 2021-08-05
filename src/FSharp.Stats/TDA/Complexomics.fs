namespace FSharp.Stats.TDA

// This module contains types and functions for topological data analysis on complexomics data
module Complexomics = 

    open Persistence
    open FSharp.Stats
    
    /// type of node in a merge tree
    type NodeType = 
        | Root
        | Node
        | Leaf 

    /// explicit label type for binary merge trees
    type Node = {
        XVal : int
        YVal : float
        Integral : float
        NodeType : NodeType
        XValMean : float
        } with static member Create x y i n xm = {XVal=x;YVal=y;Integral=i;NodeType=n;XValMean=xm}

    /// Computes binary tree data structure with label type 'Node' from edge list representation and segmentation of a given merge tree.
    let getTree (edgelist: (int*int) list) (data: float[]) (seg: int []) =
        let mutable id = 1
        let getID() = 
            let tmp = id
            id <- id + 1
            tmp
        let root = List.find (fun (l,o) -> o = -1) edgelist
        let rec loop current = 
            let xValue = fst current
            let yValue = data.[xValue]
            let origin = snd current
            let relatedNodes = 
                edgelist 
                |> List.filter (fun (l,o) -> o = xValue)
                |> fun x ->            
                    if x.Length = 1 then 
                        let branchA = loop x.[0]
                        let mergedIntegral = 0. //getIntegral branchA
                        let nodeType = Node
                        let newNode = BinaryTree.BinaryNode(Node.Create xValue yValue mergedIntegral nodeType (float(xValue)),getID(),branchA,BinaryTree.Empty)
                        if origin = -1
                            then BinaryTree.BinaryNode(Node.Create -1 0. 0. Root (float(xValue)),0,newNode,BinaryTree.Empty)
                            else newNode
                    elif x.Length = 2 then 
                        let branchA = loop x.[0]
                        let branchB = loop x.[1]
                        let mergedIntegral = 0. //getIntegral branchA + getIntegral branchB
                        let nodeType = Node
                        let newNode = BinaryTree.BinaryNode(Node.Create xValue yValue mergedIntegral nodeType (float(xValue)),getID(),branchA,branchB)
                        if origin = -1
                            then BinaryTree.BinaryNode(Node.Create -1 0. 0. Root (float(xValue)),0,newNode,BinaryTree.Empty)
                            else newNode
                    else 
                        let integralOfSegment =    
                            let segOfA = seg.[xValue]
                            Seq.zip seg data 
                            |> Seq.filter (fun (s,d) -> s = segOfA)
                            |> Seq.sumBy snd
                        let xmeanOfSegment =    
                            let segOfA = seg.[xValue]
                            seg 
                            |> Seq.mapi (fun i s -> (float(i),s))
                            |> Seq.filter (fun (i,s) -> s = segOfA)
                            |> Seq.map fst
                            |> Seq.mean
                        //printfn "%f" integralOfSegment
                        let nodeType = Leaf
                        let mergedIntegral = integralOfSegment
                        let newNode = BinaryTree.BinaryNode(Node.Create xValue yValue mergedIntegral nodeType xmeanOfSegment,getID(),BinaryTree.Empty,BinaryTree.Empty)
                        if origin = -1
                            then BinaryTree.BinaryNode(Node.Create -1 0. 0. Root (float(xValue)),0,newNode,BinaryTree.Empty)
                            else newNode
            relatedNodes
        loop root

    /// Computes merge tree of a given data array as binary tree. Merge tree is simplified by persistence with the given threshold value.
    let getTreeOfSeq a simplificationThreshold = 
        let (persistencePairsSplit, mergeTreePairsSplit, segmentationSplitPP, segmentationSplitMT) = computePPMT a Direction.Split
        let (mergeTreePairsSimplSplit,segmentationSimplSplitPP, segmentationSimplSplitMT) = simplifyMergeTreeAndSeg a mergeTreePairsSplit persistencePairsSplit segmentationSplitPP segmentationSplitMT simplificationThreshold Direction.Split
        getTree mergeTreePairsSimplSplit a segmentationSimplSplitMT

    /// Computes peaks of a given data array as a list of leaf nodes. Merge tree is simplified by persistence with the given threshold value.
    let getLeafStringofSeq a simplificationThreshold =
        let (persistencePairsSplit, mergeTreePairsSplit, segmentationSplitPP, segmentationSplitMT) = computePPMT a Direction.Split
        let (mergeTreePairsSimplSplit,segmentationSimplSplitPP, segmentationSimplSplitMT) = simplifyMergeTreeAndSeg a mergeTreePairsSplit persistencePairsSplit segmentationSplitPP segmentationSplitMT simplificationThreshold Direction.Split
        let tree = getTree mergeTreePairsSimplSplit a segmentationSimplSplitMT
        BinaryTree.getLeafStringAsList tree