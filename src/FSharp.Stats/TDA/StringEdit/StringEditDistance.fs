module StringEdit.StringEditDistance

open System

let rec editDist_memoization (s1:List<'lt>) (s2:List<'lt>) (editCost:'lt->'lt->float) (delCost:'lt->float) (M:System.Collections.Generic.Dictionary<List<'lt>*List<'lt>,float>) =
    let succ,res = M.TryGetValue((s1,s2))
    if succ then res else
    match (s1,s2) with
    | ([],[]) ->
        M.Add((s1,s2),0.)
        0.
    | (h::t,[]) ->
        let v = (delCost h) + (editDist_memoization t [] editCost delCost M)
        M.Add((s1,s2),v)
        v
    | ([],h::t) ->
        let v= (delCost h) + (editDist_memoization [] t editCost delCost M)
        M.Add((s1,s2),v)
        v
    | (h1::t1,h2::t2) -> 
        let v1 = (delCost h2) + (editDist_memoization (h1::t1) t2 editCost delCost M)
        let v2 = (delCost h1) + (editDist_memoization t1 (h2::t2) editCost delCost M)
        let v3 = (editCost h1 h2) + (editDist_memoization t1 t2 editCost delCost M)
        let v = min v1 (min v2 v3)
        M.Add((s1,s2),v)
        v

let rec traceMatching (s1:List<'lt>) (p1:int) (s2:List<'lt>) (p2:int) (editCost:'lt->'lt->float) (delCost:'lt->float) (M:System.Collections.Generic.Dictionary<List<'lt>*List<'lt>,float>) =
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

let editDist (s1:List<'lt>) (s2:List<'lt>) (editCost:'lt->'lt->float) (delCost:'lt->float) = 
    let M = new System.Collections.Generic.Dictionary<List<'lt>*List<'lt>,float>()
    let dist = editDist_memoization s1 s2 editCost delCost M
    let matching = traceMatching s1 0 s2 0  editCost delCost M
    dist,matching
