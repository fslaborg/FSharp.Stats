module TreeEdit.AlignmentDistance

open TreeEdit.BinaryTree

// computes alignment distance between two ordered forests F1 and F2 and the corresponding matching
// writes result to memoization tables MT (for subtrees) and MF (for subforests)
// relabel costs and deletion/insertion costs are given as functions editCost and delCost
let rec alignmentDist_forest (F1:BinaryTree<'lt>) (F2:BinaryTree<'lt>)
                             (editCost:'lt->'lt->float) (delCost:'lt->float)
                             (MT:System.Collections.Generic.Dictionary<BinaryTree<'lt>*BinaryTree<'lt>,float*List<int*int>>)
                             (MF:System.Collections.Generic.Dictionary<BinaryTree<'lt>*BinaryTree<'lt>,float*List<int*int>>) = 
    let succ,res = MF.TryGetValue((F1,F2))
    if succ then res else
    match (F1,F2) with
    // base cases
    | (Empty,Empty) -> 
        MF.Add((F1,F2),(0.,[]))
        (0.,[])
    // deletion/insertion cases
    | (Empty,BinaryNode(l,id,c1,c2)) ->
        let res1 = alignmentDist_tree Empty c1 editCost delCost MT MF
        let res2 = alignmentDist_tree Empty c2 editCost delCost MT MF
        let nres = ((fst res1)+(fst res2),List.append (snd res1) (snd res2))
        MF.Add((F1,F2),nres)
        nres 
    | (BinaryNode(l,id,c1,c2),Empty) ->
        let res1 = alignmentDist_tree c1 Empty editCost delCost MT MF
        let res2 = alignmentDist_tree c2 Empty editCost delCost MT MF
        let nres = ((fst res1)+(fst res2),List.append (snd res1) (snd res2))
        MF.Add((F1,F2),nres)
        nres 
    // cases for matching subtrees
    | (BinaryNode(l1,id1,c11,c12),BinaryNode(l2,id2,c21,c22)) -> 
        let r11 = alignmentDist_tree c11 c21 editCost delCost MT MF
        let r12 = alignmentDist_tree c12 c22 editCost delCost MT MF
        let r21 = alignmentDist_tree c11 Empty editCost delCost MT MF
        let r22 = alignmentDist_forest c12 F2 editCost delCost MT MF
        let e2 = match c12 with
                 | Empty -> 0.
                 | BinaryNode(l,id,_,_) -> delCost l
        let r31 = alignmentDist_tree c12 Empty editCost delCost MT MF
        let r32 = alignmentDist_forest c11 F2 editCost delCost MT MF
        let e3 = match c11 with
                 | Empty -> 0.
                 | BinaryNode(l,id,_,_) -> delCost l
        let r41 = alignmentDist_tree Empty c21 editCost delCost MT MF
        let r42 = alignmentDist_forest F1 c22 editCost delCost MT MF
        let e4 = match c22 with
                 | Empty -> 0.
                 | BinaryNode(l,id,_,_) -> delCost l
        let r51 = alignmentDist_tree Empty c22 editCost delCost MT MF
        let r52 = alignmentDist_forest F1 c21 editCost delCost MT MF
        let e5 = match c21 with
                 | Empty -> 0.
                 | BinaryNode(l,id,_,_) -> delCost l
        let v1 = (fst r11) + (fst r12)
        let v2 = e2 + (fst r21) + (fst r22)
        let v3 = e3 + (fst r31) + (fst r32)
        let v4 = e4 + (fst r41) + (fst r42)
        let v5 = e5 + (fst r51) + (fst r52)
        let v = min v1 (min v2 (min v3 (min v4 v5)))
        let res =
            if v=v1 then (v,List.append (snd r11) (snd r12))
            elif v=v2 then (v,snd r22)
            elif v=v3 then (v,snd r32)
            elif v=v4 then (v,snd r42)
            else (v,snd r52)
        MF.Add((F1,F2),res)
        res

// computes alignment distance between two binary trees T1 and T2 and the corresponding matching
// writes result to memoization tables MT (for subtrees) and MF (for subforests)
// relabel costs and deletion/insertion costs are given as functions editCost and delCost
and alignmentDist_tree (T1:BinaryTree<'lt>) (T2:BinaryTree<'lt>)
                       (editCost:'lt->'lt->float) (delCost:'lt->float)
                       (MT:System.Collections.Generic.Dictionary<BinaryTree<'lt>*BinaryTree<'lt>,float*List<int*int>>)
                       (MF:System.Collections.Generic.Dictionary<BinaryTree<'lt>*BinaryTree<'lt>,float*List<int*int>>) = 
    let succ,res = MT.TryGetValue((T1,T2))
    if succ then res else
    match (T1,T2) with
    // base cases
    | (Empty,Empty) -> 
        MT.Add((T1,T2),(0.,[]))
        (0.,[])
    // deletion/insertion cases
    | (Empty,BinaryNode(l,id,c1,c2)) ->
        let res = alignmentDist_forest Empty (BinaryNode(l,id,c1,c2)) editCost delCost MT MF
        let nres = ((delCost l)+(fst res),snd res)
        MT.Add((T1,T2),nres)
        nres 
    | (BinaryNode(l,id,c1,c2),Empty) ->
        let res = alignmentDist_forest (BinaryNode(l,id,c1,c2)) Empty editCost delCost MT MF
        let nres = ((delCost l)+(fst res),snd res)
        MT.Add((T1,T2),nres)
        nres
    // cases for matching subtrees
    | (BinaryNode(l1,id1,c11,c12),BinaryNode(l2,id2,c21,c22)) -> 
        let r1 = alignmentDist_forest T1 T2 editCost delCost MT MF
        let r21 = alignmentDist_tree Empty (BinaryNode(l2,id2,c21,c22)) editCost delCost MT MF
        let r22 = alignmentDist_tree (BinaryNode(l1,id1,c11,c12)) c21 editCost delCost MT MF
        let r23 = alignmentDist_tree Empty c21 editCost delCost MT MF
        let r31 = alignmentDist_tree Empty (BinaryNode(l2,id2,c21,c22)) editCost delCost MT MF
        let r32 = alignmentDist_tree (BinaryNode(l1,id1,c11,c12)) c22 editCost delCost MT MF
        let r33 = alignmentDist_tree Empty c22 editCost delCost MT MF
        let r41 = alignmentDist_tree (BinaryNode(l1,id1,c11,c12)) Empty editCost delCost MT MF
        let r42 = alignmentDist_tree c11 (BinaryNode(l2,id2,c21,c22)) editCost delCost MT MF
        let r43 = alignmentDist_tree c11 Empty editCost delCost MT MF
        let r51 = alignmentDist_tree (BinaryNode(l1,id1,c11,c12)) Empty editCost delCost MT MF
        let r52 = alignmentDist_tree c12 (BinaryNode(l2,id2,c21,c22)) editCost delCost MT MF
        let r53 = alignmentDist_tree c12 Empty editCost delCost MT MF
        let v1 = (editCost l1 l2) + (fst r1)
        let v2 = (fst r21) + (fst r22) - (fst r23)
        let v3 = (fst r31) + (fst r32) - (fst r33)
        let v4 = (fst r41) + (fst r42) - (fst r43)
        let v5 = (fst r51) + (fst r52) - (fst r53)
        let v = min v1 (min v2 (min v3 (min v4 v5)))
        let res =
            if v=v1 then (v,(id1,id2)::(snd r1))
            elif v=v2 then (v,snd r22)
            elif v=v3 then (v,snd r32)
            elif v=v4 then (v,snd r42)
            else (v,snd r52)
        MT.Add((T1,T2),res)
        res

// wrapper function that returns alignment distance and corresponding matching of two binary trees T1 and T2 as a pair
// relabel costs and deletion/insertion costs are given as functions editCost and delCost
let alignmentDist (T1:BinaryTree<'lt>) (T2:BinaryTree<'lt>) (editCost:'lt->'lt->float) (delCost:'lt->float) =
    let memoizationTable_tree =
        new System.Collections.Generic.Dictionary<BinaryTree<'lt>*BinaryTree<'lt>,float*List<int*int>>()
    let memoizationTable_forest =
        new System.Collections.Generic.Dictionary<BinaryTree<'lt>*BinaryTree<'lt>,float*List<int*int>>()
    alignmentDist_tree T1 T2 editCost delCost memoizationTable_tree memoizationTable_forest