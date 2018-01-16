namespace FSharp.Stats.Testing

open FSharp.Stats

module SAM = 

    type SAM = {
        /// relative diffence of mean
        Ri : float
        /// pooled standard error 
        Si : float
        /// test statistics
        Statistics : float
        }

    let createSAM ri si stats =
        {Ri=ri; Si=si; Statistics=stats}


    /// Calculates SAM test statistics 
    let calculate s0 (dataA:float array array) (dataU:float array array) =
    
        let calcStats s0 (a:float[]) (u:float[]) =        
            let ma,mu = Array.average a,Array.average u
            let ri    = ma - mu
            let si    = 
                let n = ((1.0/ float a.Length) + (1.0/ float u.Length)) / float (a.Length+u.Length-2 )
                n * (a |> Array.sumBy (fun v -> v - ma * v - ma)) + (u |> Array.sumBy (fun v -> v - mu * v - mu))
                |> sqrt

            createSAM ri si (ri/(si+s0))

        Array.map2 (fun a u -> calcStats s0 a u) dataA dataU    


    /// 
    let permutationBalancedColWiseSAM iterations s0 (dataA:float array array) (dataU:float array array) =
        //let rnd = System.Random()
        let dataA' = JaggedArray.copy dataA
        let dataU' = JaggedArray.copy dataU
    
        [|0..iterations-1|]
        |> Array.map (fun _ -> 
            let rndDataA = JaggedArray.shuffleColumnWiseInPlace dataA'
            let rndDataU = JaggedArray.shuffleColumnWiseInPlace dataU'
            calculate s0 rndDataA rndDataU
            )
        |> JaggedArray.transpose
        |> Array.map (fun tt -> 
            let (ri',si',di') = tt |> Array.fold (fun (ri,si,di) t -> ri+t.Ri,si+t.Si,di + t.Statistics) (0.,0.,0.)  
            createSAM (ri' / float iterations) (si' / float iterations) (di' / float iterations)
            )

    /// 
    let permutationBalancedSAM iterations s0 (dataA:float array array) (dataU:float array array) =
        //let rnd = System.Random()
        let dataA' = JaggedArray.copy dataA
        let dataU' = JaggedArray.copy dataU
    
        [|0..iterations-1|]
        |> Array.map (fun _ -> 
            let rndDataA = JaggedArray.shuffleInPlace dataA'
            let rndDataU = JaggedArray.shuffleInPlace dataU'
            calculate s0 rndDataA rndDataU
            )
        |> JaggedArray.transpose
        |> Array.map (fun tt -> 
            let (ri',si',di') = tt |> Array.fold (fun (ri,si,di) t -> ri+t.Ri,si+t.Si,di + t.Statistics) (0.,0.,0.)  
            createSAM (ri' / float iterations) (si' / float iterations) (di' / float iterations)
            )

    ///
    let permutationCompleteSAM iterations s0 (dataA:float array array) (dataU:float array array) =
        let rnd = System.Random()
        let dataA' = JaggedArray.copy dataA
        let dataU' = JaggedArray.copy dataU
        let data = Array.zip dataA' dataU'


        // Case specific FisherYates shuffling 
        let shuffleInPlace' (random:System.Random) (arr : array<('a[]*'a[])>) = 
            let rowCount =  arr.Length
            let colCountFst,colCountSnd = 
                if rowCount < 1 then 0,0 else Array.length ( fst(arr.[0]) ), Array.length ( snd(arr.[0]) )
            let colCount = colCountFst + colCountSnd
    
            let getByIndex i (darr:'a[]*'a[]) =
                if i > colCountFst-1 then (snd(darr)).[i-colCountFst] else (fst(darr)).[i]

            let setByIndexInPlace i value (darr:'a[]*'a[]) =
                if i > colCountFst-1 then (snd(darr)).[i-colCountFst] <- value else (fst(darr)).[i] <- value


            for colI = colCount downto 1 do
                for rowI = rowCount downto 1 do
                    // Pick random element to swap.
                    let i = random.Next(rowI) // 0 <= i <= rowI-1
                    let j = random.Next(colI) // 0 <= j <= colI-1
                    // Swap.
                    let tmp = arr.[i] |> getByIndex j
                    arr.[i] |> setByIndexInPlace j (arr.[rowI - 1] |> getByIndex (colI - 1))
                    arr.[rowI - 1] |> setByIndexInPlace (colI - 1) tmp
            arr 
    
        [|0..iterations-1|]
        |> Array.map (fun _ -> 
            let rndData = shuffleInPlace' rnd data        
            let rndDataA,rndDataU = Array.unzip rndData
            calculate s0 rndDataA rndDataU
            )
        |> JaggedArray.transpose
        |> Array.map (fun tt -> 
            let (ri',si',di') = tt |> Array.fold (fun (ri,si,di) t -> ri+t.Ri,si+t.Si,di + t.Statistics) (0.,0.,0.)  
            createSAM (ri' / float iterations) (si' / float iterations) (di' / float iterations)
            )


    /// Module with different estimation methods computing s0 
    module S0 =
                

        /// Computes s0 using the nearest rank method
        let nearestRank percentile (tt:array<SAM>) =
            if percentile < 0. || percentile > 1.0 then failwith "Percentile must be between 0.0 and 1.0"
            let sortedTT= 
                tt
                |> Array.sortBy (fun t -> t.Si)
            let index = System.Math.Round( percentile * float tt.Length, 0)
            
            sortedTT.[(int index)].Si

        
        /// Creates s0 by calulating the mean of si (pooled standard error )
        let median (tt:array<SAM>) = 
            tt
            |> Array.map (fun t -> t.Si)
            |> Array.median


        /// Estimates s0. Chosen to minimize the coefficient of variation of the test statistics 
        let estimate (tt:array<SAM>) = //(sd:array<float>) (tt:array<float>) =
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


            let si    = tt |> Array.map (fun t -> t.Si)
            let stats = tt |> Array.map (fun t -> t.Statistics)
            let s0percentile= [|0. .. 0.05 .. 1.|] 
            let br = [|0. .. 0.01 .. 1.|] |> Array.map (fun p -> Quantile.InPlace.modeInPLace p si)            

            let cvSd =
                let tmp = Seq.zip si stats
                s0percentile
                |> Array.map (fun k -> 
                    let w  = Quantile.InPlace.modeInPLace k si
                    // Calc corrected test statistic
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
            Quantile.InPlace.modeInPLace pMinCvSd si


        /// Estimates s0 from given data set. Chosen to minimize the coefficient of variation of the test statistics 
        let estimateFrom (dataA:float array array) (dataU:float array array) =
            let tt = calculate 0.0 dataA dataU
            estimate tt


    