namespace FSharp.Stats.Testing

open FSharp.Stats

module SAM = 

    /// Module with different estimation methods computing s0 
    module S0 =
        open System.Runtime.CompilerServices
        open PvalueAdjust.Qvalue
        open System.Reflection
        
        // <summary>
        /// Computes s0 using the nearest rank method
        // <summary>
        //
        /// <param name="percentile" >float</param>
        /// <param name="sis" >array of float</param>
        /// <param name="tt" >array of float</param>
        /// <param name="len" >int</param>
        /// <returns> s0 via nearest rank calulation </returns> 
        let nearestRank percentile (sis:array<float>) =
            if percentile < 0. || percentile > 1.0 then failwith "Percentile must be between 0.0 and 1.0"
            let sissorted= 
                sis
                |> Array.sort
            let index = System.Math.Round( percentile * float sis.Length, 0)
            
            sissorted.[(int index)]


        // <summary>
        /// Creates s0 by calulating the mean of the si values
        // <summary>
        //
        /// <param name="sis" >array of float</param>
        /// <param name="tt" >array of float</param>
        /// <param name="len" >int</param>
        /// <returns> s0 via median calulation </returns> 
        let meadian (sis:array<float>) = Array.median sis


        

        // <summary>
        /// Estimates s0. Chosen to minimize the coefficient of variation of the test statistics 
        // <summary>
        //
        /// <param name="sd" >combined standard deviation</param>
        /// <param name="tt" >test statistcs</param>
        /// <returns> s0 via estimation </returns> 
        let estimate (sd:array<float>) (tt:array<float>) =
            // Refactor to Care
            let partition predicate source =
                let map =
                    source
                        |> Seq.groupBy predicate
                        |> Map.ofSeq
                let get flag =
                    map
                        |> Map.tryFind flag
                        |> Option.defaultValue Seq.empty
                get true, get false            


            // Refactor to Care
            let bind (predicate:'a -> 'b -> bool) (binder:seq<'a>) (source:seq<'b>) =
                let en = binder.GetEnumerator()
                let rec loop rest =
                    seq {
                        match en.MoveNext() with
                        | false -> ()
                        | true -> 
                            let a,rest' = partition (fun x -> predicate en.Current x) rest
                            yield a
                            yield! loop rest'
                    }
                loop source


            let sd' = Array.copy sd
            let s0percentile= [|0. .. 0.05 .. 1.|] 
            let br = [|0. .. 0.01 .. 1.|] |> Array.map (fun p -> Quantile.InPlace.modeInPLace p sd')            

            let cvSd =
                let tmp = Seq.zip sd tt
                s0percentile
                |> Array.map (fun k -> 
                    let w  = Quantile.InPlace.modeInPLace k sd'
                    // Calc corrected test statistic
                    //let tt' = Array.map2 (fun t s -> s,t * s / (s + w)) tt sd
                    let mads = 
                        bind (fun a b -> fst b < a) br tmp
                        |> Seq.map (fun v ->
                            v 
                            |> Seq.map (fun (t,s) -> t * s / (s + w))
                            |> Seq.medianAbsoluteDev                            
                            )
                        |> Seq.toArray                                            
                    k, Seq.stDev mads / Array.median mads
                    )
                                
            let pMinCvSd = cvSd |> Array.minBy snd |> fst // kleinste oder größte quantile mit kleinster cv
            Quantile.InPlace.modeInPLace pMinCvSd sd'


    let a = 42