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

        member this.SetIsCore () =  tag <- tag ||| DbscanFlag.IsCore

        member this.IsCore =  tag.HasFlag DbscanFlag.IsCore

        member this.SetIsInCluster () =  tag <- tag ||| DbscanFlag.IsInCluster

        member this.IsInCluster =  tag.HasFlag DbscanFlag.IsInCluster





    let inline compute (dfu:array<'a> -> array<'a> -> float) (minPts:int) (eps:float) (input:seq<#seq<'a>>) =   

        let convert (input:seq<'a>) =
            match input with
            | :? array<'a> as value -> TaggedValue(value)
            | _ -> TaggedValue( input |> Seq.toArray )

        let inline expandCluster dfu (point:TaggedValue<'a array>) (neighours:List<TaggedValue<'a array>>) (sourcelist:List<TaggedValue<'a array>>) (eps:float) (minPts:int) (cluster:List<TaggedValue<'a array>>)= 
            cluster.Add point                                                                        
            point.SetIsInCluster () //InCluster <- true                                                                  
            let rec loop i =                                                                         
                if i < neighours.Count then                                                          
                    let p = neighours.[i]                                                            
                    if not p.IsVisited then                                                        
                        p.SetIsVisited()//p.Visited <- true                                                            
                        //let neiOfP = getRegion dfu p sourcelist eps
                        let neiOfP = sourcelist.FindAll (fun x -> dfu x.Value p.Value <= eps)
                        if neiOfP.Count >= minPts then                                               
                            neighours.AddRange neiOfP                                                
                    if not p.IsInCluster then                                                      
                        cluster.Add p                                                                
                        p.SetIsInCluster ()                                                          
                    loop (i+1)                                                                       
            loop 0 

        let noiseList= List<TaggedValue<'a array>>()
        let clusterList = List<List<TaggedValue<'a array>>>() 
    
        let sourcelist = List (input |> Seq.map convert)

        let newEPS = eps * eps                                                        
        let mutable counter = 0                                                       
        for i=0 to sourcelist.Count-1 do                                              
            //printfn "counter %i" counter                                              
            counter <- counter + 1                                                    
            if not sourcelist.[i].IsVisited then                                        
                sourcelist.[i].SetIsVisited ()
                let neiOfI = sourcelist.FindAll (fun x -> dfu x.Value sourcelist.[i].Value <= newEPS)
                //let neiOfI = getRegion dfu sourcelist.[i] sourcelist newEPS                 //here
                if neiOfI.Count >= minPts then                                        
                    let c = List<TaggedValue<'a array>>()                                         
                    expandCluster dfu sourcelist.[i] neiOfI sourcelist newEPS minPts c      //here
                    clusterList.Add c                                                
        let noisepoints =                                                            
            sourcelist.FindAll (fun x -> not x.IsInCluster)
        noiseList.AddRange noisepoints

        clusterList |> Seq.map (fun l -> l |> Seq.map (fun x -> x.Value) )






