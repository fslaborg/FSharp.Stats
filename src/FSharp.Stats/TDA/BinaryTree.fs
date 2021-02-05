namespace FSharp.Stats.TDA

module BinaryTree =

    // generic binary tree type
    type BinaryTree<'LabelType> =
        | Empty
        | BinaryNode of label:'LabelType * id:int * child1:BinaryTree<'LabelType> * child2:BinaryTree<'LabelType>

    // returns int-labeled tree with labels in range [0,numLabels)
    let rec randomTree numNodes numLabels currId (randGen:System.Random) =
        if numNodes=0 then Empty else
        let label = randGen.Next(numLabels)
        let numLeft = randGen.Next(numNodes-1)
        let left = randomTree numLeft numLabels (currId+1) randGen
        let right = randomTree (numNodes-numLeft-1) numLabels (currId+numLeft+1) randGen
        BinaryNode(label,currId,left,right)

    // returns float-labeled tree with labels in range [0,maxLabel)
    let rec randomTree_floatLabels numNodes maxLabel currId (randGen:System.Random) =
        if numNodes=0 then Empty else
        let label = System.Math.Round(randGen.NextDouble() * maxLabel,2)
        let numLeft = randGen.Next(numNodes-1)
        let left = randomTree_floatLabels numLeft maxLabel (currId+1) randGen
        let right = randomTree_floatLabels (numNodes-numLeft-1) maxLabel (currId+numLeft+1) randGen
        BinaryNode(label,currId,left,right)

    // appends dotstring for nodes in subtree of binary tree n to string s.
    // nodes are names with "t<tree id>n<node id>" 
    let rec appendNodeString n s tId =
        match n with
        | Empty -> s
        | BinaryNode(l,id,c1,c2) ->
            let s1 = appendNodeString c1 s tId
            let s2 = appendNodeString c2 s1 tId
            let s3 =
                match c1 with
                | Empty -> ""
                | BinaryNode(_,cid,_,_) -> "    t" + tId.ToString() + "n" + id.ToString() +
                                           "->t" + tId.ToString() + "n" + cid.ToString() + ";\n"
            let s4 =
                match c2 with
                | Empty -> ""
                | BinaryNode(_,cid,_,_) -> "    t" + tId.ToString() + "n" + id.ToString() +
                                           "->t" + tId.ToString() + "n" + cid.ToString() + ";\n" 
            let s5 = "    t" + tId.ToString() + "n" + id.ToString() + "[label=" + l.ToString() + ",rank=\"min\"];\n"
            s5 + s3 + s4 + s2

    // wrapper funtion for appendNodeString that returns dotstring for binary tree t.
    // nodes are names with "t<tree id>n<node id>"
    let dotString (t:BinaryTree<'lt>) (tID:int) = "digraph{\n" + (appendNodeString t "" tID) + "}\n"

    // appends dotstring for matching of nodes of two tree with tree ids tId1 and tId2 to string s.
    // nodes are names with "t<tree id>n<node id>"
    let rec appendNodeMatchStrings (ml:List<int*int>) s tId1 tId2 =
        match ml with
        | [] -> s
        | head::tail -> "  t" + tId1.ToString() + "n" + (fst head).ToString() + 
                        "->t" + tId2.ToString() + "n" + (snd head).ToString() +
                        "[dir=none,ltail=cluster1,lhead=cluster2,constraint=false,color=blue,penwidth=2.0];\n" +
                        (appendNodeMatchStrings tail s tId1 tId2)

    // wrapper funtion for appendNodeString and appendNodeMatchStrings
    // that returns dotstring for two binary trees with a given matching.
    // nodes are names with "t<tree id>n<node id>"
    let dotString_matching (t1:BinaryTree<'lt>) (t2:BinaryTree<'lt>) (matchList:List<int*int>) =
        let g1 = (appendNodeString t1 "" 1)
        let g2 = (appendNodeString t2 "" 2)
        let m = (appendNodeMatchStrings matchList "" 1 2)
        "digraph{\n  subgraph cluster1 {\n" + g1 + "  }\n  subgraph cluster2 {\n" + g2 + "  }\n" + m + "}\n"