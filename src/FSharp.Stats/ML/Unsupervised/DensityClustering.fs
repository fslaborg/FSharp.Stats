namespace BioFSharp.Stats.ML.Unsupervised


module DensityClustering =
    
    open BioFSharp.Stats.ML
    
    
    type LabeledData<'T> =
        | UnVisited of 'T
        | Visited   of 'T
        | InCluster of 'T        
        | Noise     of 'T
    
    let private getData (lp:LabeledData<'T>) =
        match lp with
        | UnVisited (x) -> x
        | Visited   (x) -> x
        | InCluster (x) -> x
        | Noise     (x) -> x

    //let visited        
    
    let getDistance (distance:DistanceMetrics.Distance<'T>) (p1:LabeledData<'T>) (p2:LabeledData<'T>) =
        distance (getData p1) (getData p2)    
        
    
    let private getRegion (distance:DistanceMetrics.Distance<'T>) P points eps =
        let region = points |> List.filter (fun p -> (getDistance distance P p) <= eps)
        region


//    let private expandCluster points P (neighborPts:LabeledData<'T> list) (C:LabeledData<'T> list) eps minPts = 
//        
//        C.Add(P)
//        P.InCluster <- true
//
//        while neighborPts.Count > 0 do
//            let last = neighborPts.Count-1
//            let P' = neighborPts.[last]
//            neighborPts.RemoveAt(last)
//
//            if not P'.Visited then
//                P'.Visited <- true
//                let neighborPts' = getRegion P' points eps
//                if neighborPts'.Count >= minPts then
//                    neighborPts.AddRange(neighborPts')
//            if not P'.InCluster then
//                C.Add(P')
//                P'.InCluster <- true


module DensityClustering2 =
    
    open System.Collections.Generic


    type Point(lon,lat) = 
        [<DefaultValue>] val mutable InCluster : bool
        [<DefaultValue>] val mutable Visited : bool
        [<DefaultValue>] val mutable Noise : bool
        member this.Lon = lon
        member this.Lat = lat

        // or
//        member val InCluster = false with get, set
//        member val Visited = false with get, set
//        member val Noise = false with get, set
//        member val Lon = lon with get
//        member val Lat = lat with get

    let getDistance (p1:Point) (p2:Point) =
        let diffX = p2.Lon - p1.Lon
        let diffY = p2.Lat - p1.Lat
        let d = diffX * diffX + diffY * diffY
        d

    let private getRegion P points eps =
        let region = points |> List.filter (fun p -> (getDistance P p) <= eps)
        new System.Collections.Generic.List<Point>(region)
    
    let private expandCluster points P (neighborPts:List<Point>) (C:List<Point>) eps minPts = 
        
        C.Add(P)
        P.InCluster <- true

        while neighborPts.Count > 0 do
            let last = neighborPts.Count-1
            let P' = neighborPts.[last]
            neighborPts.RemoveAt(last)

            if not P'.Visited then
                P'.Visited <- true
                let neighborPts' = getRegion P' points eps
                if neighborPts'.Count >= minPts then
                    neighborPts.AddRange(neighborPts')
            if not P'.InCluster then
                C.Add(P')
                P'.InCluster <- true

    
    let DBSCAN points eps minPts = 
        let eps = eps*eps
        let clusters = new List<List<Point>>()
        for (p:Point) in points do
            if not p.Visited then
                p.Visited <- true            
                let neighborPts = getRegion p points eps
    
                if neighborPts.Count < minPts then
                    p.Noise <- true
                else
                    let C = new List<Point>()
                    expandCluster points p neighborPts C eps minPts
                    if C.Count > 0 then clusters.Add(C)
        clusters

    let test = 
        let points = [new Point(0, 100);
                      new Point(0, 200);
                      (new Point(0, 275));
                      (new Point(100, 150));
                      (new Point(200, 100));
                      (new Point(250, 200));        
                      (new Point(0, 300));        
                      (new Point(100, 200));        
                      (new Point(600, 700));        
                      (new Point(650, 700));        
                      (new Point(675, 700));        
                      (new Point(675, 710));        
                      (new Point(675, 720));        
                      (new Point(50, 400))]
        let clusters = DBSCAN points 100 3
        for point in points do
            if point.Noise && not point.InCluster then
                printfn "noise %d %d" point.Lon point.Lat
        let mutable counter = 0
        for cluster in clusters do
            counter <- counter + 1
            printfn "Cluster %d consists of the following %d point(s)" counter cluster.Count
            for point in cluster do
                printf "(%d, %d) " point.Lon point.Lat
            printfn ""



