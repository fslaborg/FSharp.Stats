namespace FSharp.Stats.TDA

module TreeAlignmentDistance =

    open BinaryTree

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



module TreeEditDistance =

    open BinaryTree

    // computes edit distance between two ordered forests F1 and F2 and the corresponding matching
    // writes result to memoization table M
    // relabel costs and deletion/insertion costs are given as functions editCost and delCost
    let rec editDist_forest (F1:List<BinaryTree<'lt>>) (F2:List<BinaryTree<'lt>>)
                            (editCost:'lt->'lt->float) (delCost:'lt->float)
                            (M:System.Collections.Generic.Dictionary<List<BinaryTree<'lt>>*List<BinaryTree<'lt>>,float*List<int*int>>) =
        // if result already in memoization table, return immediately
        let succ,res = M.TryGetValue((F1,F2))
        if succ then res else
        // otherwise recurse and compute result
        match (F1,F2) with
        // base cases
        | ([],[]) -> 
            M.Add((F1,F2),(0.,[]))
            (0.,[])
        | (Empty::tail,F2) ->
            let res = editDist_forest tail F2 editCost delCost M
            M.Add((F1,F2),res)
            res
        | (F1,Empty::tail) ->
            let res = editDist_forest F1 tail editCost delCost M
            M.Add((F1,F2),res)
            res
        // deletion/insertion cases
        | ([],BinaryNode(l,id,c1,c2)::tail) ->
            let res = editDist_forest [] (c1::c2::tail) editCost delCost M
            let nres = ((delCost l)+(fst res),snd res)
            M.Add((F1,F2),nres)
            nres 
        | (BinaryNode(l,id,c1,c2)::tail,[]) ->
            let res = editDist_forest (c1::c2::tail) [] editCost delCost M
            let nres = ((delCost l)+(fst res),snd res)
            M.Add((F1,F2),nres)
            nres
        // cases for matching subtrees
        | (BinaryNode(l1,id1,c11,c12)::tail1,BinaryNode(l2,id2,c21,c22)::tail2) -> 
            let r1 = editDist_forest (c11::c12::tail1) (BinaryNode(l2,id2,c21,c22)::tail2) editCost delCost M
            let r2 = editDist_forest (BinaryNode(l1,id1,c11,c12)::tail1) (c21::c22::tail2) editCost delCost M
            let r31 = editDist_forest [c11;c12] [c21;c22] editCost delCost M
            let r32 = editDist_forest tail1 tail2 editCost delCost M
            let v1 = (delCost l1) + (fst r1)
            let v2 = (delCost l2) + (fst r2)
            let v3 = (editCost l1 l2) + (fst r31) + (fst r32)
            let v = min (min v1 v2) v3
            let res = if v=v1 then (v1,snd r1) elif v=v2 then (v2,snd r2) else (v3,(id1,id2)::(List.append (snd r31) (snd r32)))
            M.Add((F1,F2),res)
            res

    // wrapper function that returns edit distance and corresponding matching of two binary trees T1 and T2 as a pair
    // relabel costs and deletion/insertion costs are given as functions editCost and delCost
    let editDist (T1:BinaryTree<'lt>) (T2:BinaryTree<'lt>)
                 (editCost:'lt->'lt->float) (delCost:'lt->float) =
        let memoizationTable =
            new System.Collections.Generic.Dictionary<List<BinaryTree<'lt>>*List<BinaryTree<'lt>>,float*List<int*int>>()
        editDist_forest [T1] [T2] editCost delCost memoizationTable

module StringEditDistance =

    open System

    // computes edit distance between two sequences s1 and s2 and writes result to M
    // relabel costs and deletion/insertion costs are given as functions editCost and delCost
    let rec editDist_memoization (s1:List<'lt>) (s2:List<'lt>)
                                 (editCost:'lt->'lt->float) (delCost:'lt->float)
                                 (M:System.Collections.Generic.Dictionary<List<'lt>*List<'lt>,float>) =
        // if result already in memoization table, return immediately
        let succ,res = M.TryGetValue((s1,s2))
        if succ then res else
        // otherwise recurse and compute result
        match (s1,s2) with
        // base case for two emtpy strings
        | ([],[]) ->
            M.Add((s1,s2),0.)
            0.
        // base case for complete deletion/insertion of s1
        | (h::t,[]) ->
            let v = (delCost h) + (editDist_memoization t [] editCost delCost M)
            M.Add((s1,s2),v)
            v
        // base case for complete deletion/insertion of s2
        | ([],h::t) ->
            let v= (delCost h) + (editDist_memoization [] t editCost delCost M)
            M.Add((s1,s2),v)
            v
        // recursive case for non-empty s1 and s2
        | (h1::t1,h2::t2) -> 
            let v1 = (delCost h2) + (editDist_memoization (h1::t1) t2 editCost delCost M)
            let v2 = (delCost h1) + (editDist_memoization t1 (h2::t2) editCost delCost M)
            let v3 = (editCost h1 h2) + (editDist_memoization t1 t2 editCost delCost M)
            let v = min v1 (min v2 v3)
            M.Add((s1,s2),v)
            v

    // computes the matching as an array of id-pairs from memoization table
    // relabel costs and deletion/insertion costs are given as functions editCost and delCost
    let rec traceMatching (s1:List<'lt>) (p1:int) (s2:List<'lt>) (p2:int)
                          (editCost:'lt->'lt->float) (delCost:'lt->float)
                          (M:System.Collections.Generic.Dictionary<List<'lt>*List<'lt>,float>) =
        let succ,res = M.TryGetValue((s1,s2))
        match (s1,s2) with
        | ([],[]) -> []
        | (h::t,[]) -> []
        | ([],h::t) -> []
        | (h1::t1,h2::t2) -> 
            let suc1,r1 = M.TryGetValue((h1::t1,t2))
            let suc2,r2 = M.TryGetValue((t1,h2::t2))
            let suc3,r3 = M.TryGetValue((t1,t2))
            let v1 = (delCost h2) + r1
            let v2 = (delCost h1) + r2
            let v3 = (editCost h1 h2) + r3
            if res=v1 then traceMatching (h1::t1) p1 t2 (p2+1) editCost delCost M
            elif res=v2 then traceMatching (t1) (p1+1) (h2::t2) p2 editCost delCost M
            else (p1,p2)::(traceMatching t1 (p1+1) t2 (p2+1) editCost delCost M)

    // wrapper function that returns edit distance and corresponding matching of two sequences s1 and s2 as a pair
    // relabel costs and deletion/insertion costs are given as functions editCost and delCost
    let editDist (s1:List<'lt>) (s2:List<'lt>) (editCost:'lt->'lt->float) (delCost:'lt->float) = 
        let M = new System.Collections.Generic.Dictionary<List<'lt>*List<'lt>,float>()
        let dist = editDist_memoization s1 s2 editCost delCost M
        let matching = traceMatching s1 0 s2 0  editCost delCost M
        dist,matching
       