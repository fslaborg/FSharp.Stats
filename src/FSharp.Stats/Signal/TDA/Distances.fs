namespace FSharp.Stats.Signal.TDA

/// This module contains funtions to compute the alignment distance for rooted ordered trees with arbitrary labels
module TreeAlignmentDistance =

    open BinaryTree

    /// <summary>Helper function to compute alignment distance between two ordered binary forests and the corresponding matching.
    /// Writes result to memoization tables that are passed as parameters.
    /// Functions for relabel and deletion/insertion costs are passed as parameters.
    /// Only for internal use, has side effects for memoization.</summary>
    /// <param name="F1">First forest to align.</param>
    /// <param name="F2">Second forest to align.</param>
    /// <param name="editCost">Cost function for relabel operation.</param>
    /// <param name="delCost">Cost function for deleting/inserting a node.</param>
    /// <param name="MT">Memoization table for subtrees.</param>
    /// <param name="MF">memoization table for subforests.</param>
    /// <returns>A tuple (m,d) containing a list of matched ids m and the alignment costs d.</returns>
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

    /// <summary>Helper function to compute alignment distance between two ordered binary trees and the corresponding matching.
    /// Writes result to memoization tables that are passed as parameters.
    /// Functions for relabel and deletion/insertion costs are passed as parameters.
    /// Only for internal use, has side effects for memoization.</summary>
    /// <param name="T1">First tree to align.</param>
    /// <param name="T2">Second tree to align.</param>
    /// <param name="editCost">Cost function for relabel operation.</param>
    /// <param name="delCost">Cost function for deleting/inserting a node.</param>
    /// <param name="MT">Memoization table for subtrees.</param>
    /// <param name="MF">memoization table for subforests.</param>
    /// <returns>A tuple (m,d) containing a list of matched ids m and the alignment costs d.</returns>
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

    /// <summary>Function to compute alignment distance between two ordered binary trees and the corresponding matching.
    /// Functions for relabel and deletion/insertion costs are passed as parameters.
    /// (wrapper function for alignmentDist_tree() and alignmentDist_forest())</summary>
    /// <param name="T1">First tree to align.</param>
    /// <param name="T2">Second tree to align.</param>
    /// <param name="editCost">Cost function for relabel operation.</param>
    /// <param name="delCost">Cost function for deleting/inserting a node.</param>
    /// <returns>A tuple (m,d) containing a list of matched ids m and the alignment costs d.</returns>
    let alignmentDist (T1:BinaryTree<'lt>) (T2:BinaryTree<'lt>) (editCost:'lt->'lt->float) (delCost:'lt->float) =
        let memoizationTable_tree =
            new System.Collections.Generic.Dictionary<BinaryTree<'lt>*BinaryTree<'lt>,float*List<int*int>>()
        let memoizationTable_forest =
            new System.Collections.Generic.Dictionary<BinaryTree<'lt>*BinaryTree<'lt>,float*List<int*int>>()
        alignmentDist_tree T1 T2 editCost delCost memoizationTable_tree memoizationTable_forest

/// This module contains funtions to compute the general edit distance for rooted ordered trees with arbitrary labels
module TreeEditDistance =

    open BinaryTree

    /// <summary>Helper function to compute edit distance between two ordered binary forests and the corresponding matching.
    /// Writes result to memoization table that is passed as parameter.
    /// Functions for relabel and deletion/insertion costs are passed as parameters.
    /// Only for internal use, has side effects for memoization.</summary>
    /// <param name="F1">First forest.</param>
    /// <param name="F2">Second forest.</param>
    /// <param name="editCost">Cost function for relabel operation.</param>
    /// <param name="delCost">Cost function for deleting/inserting a node.</param>
    /// <param name="M">Memoization table for subforests.</param>
    /// <returns>A tuple (m,d) containing a list of matched ids m and the alignment costs d.</returns>
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

    /// <summary>Function to compute edit distance between two ordered binary trees and the corresponding matching.
    /// Functions for relabel and deletion/insertion costs are passed as parameters.
    /// (wrapper function for editDist_forest())</summary>
    /// <param name="T1">First tree.</param>
    /// <param name="T2">Second tree.</param>
    /// <param name="editCost">Cost function for relabel operation.</param>
    /// <param name="delCost">Cost function for deleting/inserting a node.</param>
    /// <returns>A tuple (m,d) containing a list of matched ids m and the edit distance d.</returns>
    let editDist (T1:BinaryTree<'lt>) (T2:BinaryTree<'lt>)
                 (editCost:'lt->'lt->float) (delCost:'lt->float) =
        let memoizationTable =
            new System.Collections.Generic.Dictionary<List<BinaryTree<'lt>>*List<BinaryTree<'lt>>,float*List<int*int>>()
        editDist_forest [T1] [T2] editCost delCost memoizationTable

/// This module contains funtions to compute the edit distance and alignments for strings or sequences of arbitrary labels
module StringEditDistance =

    open System

    /// <summary>Helper function to compute edit distance between two sequences s1 and s2.
    /// Writes result to memoization table passed as parameter.
    /// Relabel and deletion/insertion costs are passed as parameters.</summary>
    /// <param name="s1">First sequence to align.</param>
    /// <param name="s2">Second sequence to align.</param>
    /// <param name="editCost">Cost function for relabel operation.</param>
    /// <param name="delCost">Cost function for deleting/inserting a node.</param>
    /// <param name="M">Memoization table for subsequences.</param>
    /// <returns>The edit distance between s1 and s2.</returns>
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

    /// <summary>Helper function to compute alignment/matching between two sequences s1 and s2 from a given memoization table with edit distances.
    /// Relabel and deletion/insertion costs are passed as parameters.</summary>
    /// <param name="s1">First sequence to align.</param>
    /// <param name="s2">Second sequence to align.</param>
    /// <param name="p1">id of head of first sequence.</param>
    /// <param name="p2">id of head of second sequnce.</param>
    /// <param name="editCost">Cost function for relabel operation.</param>
    /// <param name="delCost">Cost function for deleting/inserting a node.</param>
    /// <param name="M">Memoization table for subsequences.</param>
    /// <returns>The matching between s1 and s2 as list of matched ids.</returns>
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
            if res=v3 then (p1,p2)::(traceMatching t1 (p1+1) t2 (p2+1) editCost delCost M)
            elif res=v1 then traceMatching (h1::t1) p1 t2 (p2+1) editCost delCost M
            else traceMatching (t1) (p1+1) (h2::t2) p2 editCost delCost M

    /// <summary>Function to compute edit distance and alignment/matching between two sequences s1 and s2.
    /// Relabel and deletion/insertion costs are passed as parameters.
    /// (wrapper function for editDist_memoization() and traceMatching())</summary>
    /// <param name="s1">First sequence to align.</param>
    /// <param name="s2">Second sequence to align.</param>
    /// <param name="editCost">Cost function for relabel operation.</param>
    /// <param name="delCost">Cost function for deleting/inserting a node.</param>
    /// <returns>A tuple (m,d) containing a list of matched ids m and the edit distance d.</returns>
    let editDist (s1:List<'lt>) (s2:List<'lt>) (editCost:'lt->'lt->float) (delCost:'lt->float) = 
        let M = new System.Collections.Generic.Dictionary<List<'lt>*List<'lt>,float>()
        let dist = editDist_memoization s1 s2 editCost delCost M
        let matching = traceMatching s1 0 s2 0  editCost delCost M
        dist,matching

/// This module contains funtions to compute a modified edit distance for strings or sequences of arbitrary labels
module StringEditDistanceWithMerging =

    open System.Collections.Generic
    open Persistence

    /// <summary>Helper function to compute edit distance between two sequences s1 and s2.
    /// Writes result to memoization table passed as parameter.
    /// Relabel and deletion/insertion costs are passed as parameters.</summary>
    /// <param name="s1">First sequence as array.</param>
    /// <param name="s2">Second sequence as array.</param>
    /// <param name="i1">Current index in first sequence.</param>
    /// <param name="i2">Current index in second sequence.</param>
    /// <param name="p1">Index of merging start in first sequence.</param>
    /// <param name="p2">Index of merging start in second sequence.</param>
    /// <param name="editCost">Cost function for relabel operation.</param>
    /// <param name="mergeAllowed">Function that defines whether subsequence of labels can be merged.</param>
    /// <param name="delCost">Cost function for deleting/inserting a node.</param>
    /// <param name="M">Memoization table for subsequences.</param>
    /// <returns>The edit distance between s1 and s2.</returns>
    let rec editDist_memoization (s1:'lt []) (s2:'lt [])
                                 (i1:int) (i2:int) (p1:int) (p2:int)
                                 (editCost:'lt[]->'lt[]->int->int->int->int->float) (delCost:'lt[]->int->int->float)
                                 (mergeAllowed:'lt[]->int->int->bool) (M:Dictionary<int*int*int*int,float>) =
        // if result already in memoization table, return immediately
        let succ,res = M.TryGetValue((i1,i2,p1,p2))
        if succ then res else
        // otherwise recurse and compute result
        match (i1,i2) with
        // base case for two emtpy strings
        | (x,y) when (i1>=s1.Length) && (i2>=s2.Length) ->
            M.Add((i1,i2,p1,p2),0.)
            0.
        // base case for complete deletion/insertion of s1
        | (x,y) when (i1<s1.Length) && (i2>=s2.Length) ->
            let v = (delCost s1 i1 p1) + (editDist_memoization s1 s2 (i1+1) i2 (i1+1) p2 editCost delCost mergeAllowed M)
            M.Add((i1,i2,p1,p2),v)
            v
        // base case for complete deletion/insertion of s2
        | (x,y) when (i1>=s1.Length) && (i2<s2.Length) ->
            let v = (delCost s2 i2 p2) + (editDist_memoization s1 s2 i1 (i2+1) p1 (i2+1) editCost delCost mergeAllowed M)
            M.Add((i1,i2,p1,p2),v)
            v
        // recursive case for non-empty s1 and s2
        | _ -> 
            // delete p2-i2
            let v1 = (delCost s2 i2 p2) + (editDist_memoization s1 s2 i1 (i2+1) p1 (i2+1) editCost delCost mergeAllowed M)
            // delete p1-i1
            let v2 = (delCost s1 i1 p1) + (editDist_memoization s1 s2 (i1+1) i2 (i1+1) p2 editCost delCost mergeAllowed M)
            // match p1-i1 to p2-i2
            let v3 = (editCost s1 s2 i1 i2 p1 p2) + (editDist_memoization s1 s2 (i1+1) (i2+1) (i1+1) (i2+1) editCost delCost mergeAllowed M)
            // merge i1 to i1+1
            let v4 = if i1<s1.Length-1 && (mergeAllowed s1 i1 p1)
                        then editDist_memoization s1 s2 (i1+1) i2 p1 p2 editCost delCost mergeAllowed M
                        else v3
            // merge i2 to i2+1
            let v5 = if i2<s2.Length-1 && (mergeAllowed s2 i2 p2)
                        then editDist_memoization s1 s2 i1 (i2+1) p1 p2 editCost delCost mergeAllowed M
                        else v3

            let v = List.min [v1;v2;v3;v4;v5]
            M.Add((i1,i2,p1,p2),v)
            v

    /// <summary>Helper function to compute matching between two sequences s1 and s2 from a given memoization table with edit distances.
    /// Relabel and deletion/insertion costs are passed as parameters.</summary>
    /// <param name="s1">First sequence as array.</param>
    /// <param name="s2">Second sequence as array.</param>
    /// <param name="i1">Current index in first sequence.</param>
    /// <param name="i2">Current index in second sequence.</param>
    /// <param name="p1">Index of merging start in first sequence.</param>
    /// <param name="p2">Index of merging start in second sequence.</param>
    /// <param name="editCost">Cost function for relabel operation.</param>
    /// <param name="delCost">Cost function for deleting/inserting a node.</param>
    /// <param name="mergeAllowed">Function that defines whether subsequence of labels can be merged.</param>
    /// <param name="M">Memoization table for subsequences.</param>
    /// <returns>The matching between s1 and s2 as list of matched ids.</returns>
    let rec traceMatching (s1:'lt []) (s2:'lt [])
                          (i1:int) (i2:int) (p1:int) (p2:int)
                          (editCost:'lt[]->'lt[]->int->int->int->int->float) (delCost:'lt[]->int->int->float)
                          (mergeAllowed:'lt[]->int->int->bool) (M:Dictionary<int*int*int*int,float>) =
        let succ,res = M.TryGetValue((i1,i2,p1,p2))
        match (s1,s2) with
        | (x,y) when (i1>=s1.Length) && (i2>=s2.Length) -> []
        | (x,y) when (i1<s1.Length) && (i2>=s2.Length) -> []
        | (x,y) when (i1>=s1.Length) && (i2<s2.Length) -> []
        | _ -> 
            let suc1,r1 = M.TryGetValue((i1,(i2+1),p1,(i2+1)))
            let suc2,r2 = M.TryGetValue(((i1+1),i2,(i1+1),p2))
            let suc3,r3 = M.TryGetValue(((i1+1),(i2+1),(i1+1),(i2+1)))
            let suc4,r4 = if i1<s1.Length-1 && (mergeAllowed s1 i1 p1) then M.TryGetValue(((i1+1),i2,p1,p2)) else false,-1.
            let suc5,r5 = if i2<s2.Length-1 && (mergeAllowed s2 i2 p2) then M.TryGetValue((i1,(i2+1),p1,p2)) else false,-1.
            let v1 = (delCost s2 i2 p2) + r1
            let v2 = (delCost s1 i1 p1) + r2
            let v3 = (editCost s1 s2 i1 i2 p1 p2) + r3
            let v4 = r4
            let v5 = r5
            if res=v1 then traceMatching s1 s2 i1 (i2+1) p1 (i2+1) editCost delCost mergeAllowed M
            elif res=v2 then traceMatching s1 s2 (i1+1) i2 (i1+1) p2 editCost delCost mergeAllowed M
            elif res=v3 then ((p1,i1),(p2,i2))::(traceMatching s1 s2 (i1+1) (i2+1) (i1+1) (i2+1) editCost delCost mergeAllowed M)
            elif res=v4 then traceMatching s1 s2 (i1+1) i2 p1 p2 editCost delCost mergeAllowed M
            else traceMatching s1 s2 i1 (i2+1) p1 p2 editCost delCost mergeAllowed M

    /// <summary>Function to compute edit distance and matching between two sequences s1 and s2.
    /// Relabel and deletion/insertion costs are passed as parameters.
    /// (wrapper function for editDist_memoization() and traceMatching())</summary>
    /// <param name="s1">First sequence to align.</param>
    /// <param name="s2">Second sequence to align.</param>
    /// <param name="editCost">Cost function for relabel operation.</param>
    /// <param name="delCost">Cost function for deleting/inserting a node.</param>
    /// <param name="mergeAllowed">Function that defines whether subsequence of labels can be merged.</param>
    /// <returns>A tuple (m,d) containing a list of matched ids m and the edit distance d.</returns>
    let editDist (s1:'lt list) (s2:'lt list)
                 (editCost:'lt[]->'lt[]->int->int->int->int->float) (delCost:'lt[]->int->int->float)
                 (mergeAllowed:'lt[]->int->int->bool) = 
        let M = new Dictionary<int*int*int*int,float>()
        let dist = editDist_memoization (s1|>Array.ofList) (s2|>Array.ofList) 0 0 0 0 editCost delCost mergeAllowed M
        let matching = traceMatching (s1|>Array.ofList) (s2|>Array.ofList) 0 0 0 0  editCost delCost mergeAllowed M
        dist,matching
       