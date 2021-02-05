module TreeEdit.GeneralEditDistance

open TreeEdit.BinaryTree

let rec editDist_forest (F1:List<BinaryTree<'lt>>) (F2:List<BinaryTree<'lt>>) (editCost:'lt->'lt->float) (delCost:'lt->float) (M:System.Collections.Generic.Dictionary<List<BinaryTree<'lt>>*List<BinaryTree<'lt>>,float*List<int*int>>) = 
    let succ,res = M.TryGetValue((F1,F2))
    if succ then res else
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

let editDist (T1:BinaryTree<'lt>) (T2:BinaryTree<'lt>) (editCost:'lt->'lt->float) (delCost:'lt->float) =
    let memoizationTable = new System.Collections.Generic.Dictionary<List<BinaryTree<'lt>>*List<BinaryTree<'lt>>,float*List<int*int>>()
    editDist_forest [T1] [T2] editCost delCost memoizationTable