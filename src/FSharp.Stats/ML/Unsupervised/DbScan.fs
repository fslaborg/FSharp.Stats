namespace FSharp.Stats.ML.Unsupervised


open System
open System.Collections.Generic

open FSharp.Stats

module DbScan =
 
    [<Flags>]
    type DbscanFlag =
        | None        = 0
        | IsVisited   = 1
        | IsCore      = 2
        | IsInCluster = 4


    type TaggedValue<'a> (value : 'a) = 
        let mutable tag = DbscanFlag.None
        member this.Value with get() = value

        member this.SetIsVisited () =  tag <- tag ||| DbscanFlag.IsVisited

        member this.IsVisited =  tag.HasFlag DbscanFlag.IsVisited
//not necessary if no information about core or border point is needed 
//        member this.SetIsCore () =  tag <- tag ||| DbscanFlag.IsCore
//
//        member this.IsCore =  tag.HasFlag DbscanFlag.IsCore

        member this.SetIsInCluster () =  tag <- tag ||| DbscanFlag.IsInCluster

        member this.IsInCluster =  tag.HasFlag DbscanFlag.IsInCluster

    type DbscanResult<'a> = {
        Clusterlist     : seq<seq<'a>>
        Noisepoints     : seq<'a>
        }

    let inline private createDbscanResult clusterList noisepoints = {
        Clusterlist  = clusterList
        Noisepoints  = noisepoints }


    //let inline compute (dfu:'a -> 'a -> float) (minPts:int) (eps:float) (input:seq<'a>) = 
    let inline compute (dfu:array<'a> -> array<'a> -> float) (minPts:int) (eps:float) (input:seq<#seq<'a>>) = 

        let convert (input:seq<'a>) =
            match input with
            | :? array<'a> as value -> TaggedValue(value)
            | _ -> TaggedValue( input |> Seq.toArray )

        let inline expandCluster dfu (point:TaggedValue<'a array>) (neighours:List<TaggedValue<'a array>>) (sourcelist:List<TaggedValue<'a array>>) (eps:float) (minPts:int) (cluster:List<TaggedValue<'a array>>)= 
            cluster.Add point                                                                        
            point.SetIsInCluster ()                                                                 
            let rec loop i =                                                                         
                if i < neighours.Count then                                                          
                    let p = neighours.[i]                                                            
                    if not p.IsVisited then                                                        
                        p.SetIsVisited()                                                         
                        let neiOfP = sourcelist.FindAll (fun x -> dfu x.Value p.Value <= eps)
                        if neiOfP.Count >= minPts then
//                            p.SetIsCore ()  // not necessary if no information about core or border point is needed                                               
                            neighours.AddRange neiOfP                                                
                    if not p.IsInCluster then                                                      
                        cluster.Add p                                                                
                        p.SetIsInCluster ()                                                          
                    loop (i+1)                                                                       
            loop 0 

        let noiseList= List<TaggedValue<'a array>>()
        let clusterList = List<List<TaggedValue<'a array>>>() 
    
        let sourcelist = List (input |> Seq.map convert)
                                                                                                            
        for i=0 to sourcelist.Count-1 do                                                                                               
            if not sourcelist.[i].IsVisited then                                        
                sourcelist.[i].SetIsVisited ()
                let neiOfI = sourcelist.FindAll (fun x -> dfu x.Value sourcelist.[i].Value <= eps)      
                if neiOfI.Count >= minPts then                                        
                    let c = List<TaggedValue<'a array>>()                                         
                    expandCluster dfu sourcelist.[i] neiOfI sourcelist eps minPts c      
                    clusterList.Add c                                                
        let noisepoints =                                                            
            sourcelist.FindAll (fun x -> not x.IsInCluster)
        noiseList.AddRange noisepoints
        let clusterListResult = clusterList |> Seq.map (fun l -> l |> Seq.map (fun x -> x.Value) )
        let noisePtsResult    = noiseList |> Seq.map (fun x -> x.Value) 
//        let corePtsResult     = 
//            clusterList 
//            |> Seq.map (fun l -> 
//                            l 
//                            |> Seq.filter (fun x -> x.IsCore = true) 
//                            |> Seq.map (fun x -> x.Value)
//                       )
        createDbscanResult clusterListResult noisePtsResult

