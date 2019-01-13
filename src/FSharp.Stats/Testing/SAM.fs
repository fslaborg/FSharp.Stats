namespace FSharp.Stats.Testing

(** The module SAM as well as the module s0 were implemented as part of the master thesis Controlling false discovery ratio for differential pull down experiments by Esther Wieczorek submitted in July 2018.
**)
open FSharp.Stats

module SAM = 

    /// Represents the relative difference, the pooled standard error and the test statistics
    type SAM = {
        /// relative diffence of mean
        Ri : float
        /// pooled standard error 
        Si : float
        /// test statistics
        Statistics : float
        }


    /// Creates the type SAM
    let createSAM ri si stats =
        {Ri=ri; Si=si; Statistics=stats}


    // <param name="s0" >small positive constant chosen to minimize the coefficient of variation of the test statistics  </param>
    // <param name="dataA" >samples with treatment A</param>
    // <param name="dataU" >samples with treatment U</param>

    /// Calculates SAM test statistics for two class unpaired data
    let calculateTwoClassUnpaired s0 (dataA:float array array) (dataU:float array array) =
    
        let calcStats s0 (a:float[]) (u:float[]) =        
            let ma,mu = Array.average a,Array.average u
            let ri    = mu - ma
            let si    = 
                let n = ((1.0/ float a.Length) + (1.0/ float u.Length)) / float (a.Length+u.Length-2 )
                n * ((a |> Array.sumBy (fun v -> (v - ma) * (v - ma))) + (u |> Array.sumBy (fun v -> (v - mu) * (v - mu))))
                |> sqrt

            createSAM ri si (ri/(si+s0))

        Array.map2 (fun a u -> calcStats s0 a u) dataA dataU    


    let expectedValue (dibs:SAM[][]) =
        let iterations = dibs.Length
        let dei =
            dibs
            |> JaggedArray.transpose
            |> Array.map (fun tt -> 
                            let (ri',si',di') = tt |> Array.fold (fun (ri,si,di) t -> ri+t.Ri,si+t.Si,di + t.Statistics) (0.,0.,0.)  
                            createSAM (ri' / float iterations) (si' / float iterations) (di' / float iterations)
                         )
        
        dei        

    ///Permutes all the samples. Different treatments are NOT mixed. Then the statistics for the permuatations is calculated.
    let permutationWithin calculate iterations s0 (dataA:float array array) (dataU:float array array) =
        let dataA' = JaggedArray.copy dataA
        let dataU' = JaggedArray.copy dataU
        let dibs= 
            [|0..iterations-1|]
            |> Array.map (fun _ -> 
                let rndDataA = JaggedArray.shuffleColumnWiseInPlace dataA'
                let rndDataU = JaggedArray.shuffleColumnWiseInPlace dataU'
                let tmp = calculate s0 rndDataA rndDataU                
                tmp |> Array.sortInPlaceBy (fun t -> t.Statistics) 
                tmp
                )
        dibs
        // for each di count dib > di


    // TODO
    /// Permutes all the samples. Different treatments are mixed.
    let permutationBalanced calculate iterations s0 (dataA':float array array) (dataU':float array array) =
        let half=int (float  dataA'.[0].Length/ 2.)
        let amount= dataA'.Length
        let reps= dataA'.[0].Length

        let shuffleFisherYates (random:FSharp.Stats.Random.IRandom) (arr : _[]) = 
            let arr' = Array.copy arr
            for i = arr.Length downto 1 do
                let j = random.NextInt(i) 
                let tmp = arr'.[j]
                arr'.[j] <- arr'.[i - 1]
                arr'.[i - 1] <- tmp
            arr'  

        let dib=
            let rec balanced index (dataA:float[][]) (dataU:float[][]) temp1=
                if index < iterations then
                    let rndDataA0,rndDataU0 = Array.unzip ([|0..amount-1|]|>Array.map (fun x -> ([|dataA.[x].[0..half-1];dataU.[x].[half..reps-1]|]|> Array.concat),([|dataU.[x].[0..half-1];dataA.[x].[half..reps-1]|]|> Array.concat)))
                    let rndDataA =  rndDataA0|> Array.map (fun x -> shuffleFisherYates Random.rndgen x)
                    let rndDataU =  rndDataU0|> Array.map (fun x -> shuffleFisherYates Random.rndgen x)
                    let temp= calculate s0 rndDataA rndDataU
                    temp|> Array.sortInPlaceBy ( fun x -> x.Statistics)
                    balanced (index+1) rndDataA rndDataU (temp::temp1)               
                else temp1|> List.toArray
            balanced 0 dataA' dataU' []
        dib


    /// Permutes all the samples. Different treatments are mixed.
    let permutationImbalanced calculate iterations s0 (dataA:float array array) (dataU:float array array) =
        let dataA' = JaggedArray.copy dataA
        let dataU' = JaggedArray.copy dataU
        let data = Array.zip dataA' dataU'


        // Case specific FisherYates shuffling 
        let shuffleInPlace' (random:FSharp.Stats.Random.IRandom) (arr : array<('a[]*'a[])>) = 
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
                    let i = random.NextInt(rowI) // 0 <= i <= rowI-1
                    let j = random.NextInt(colI) // 0 <= j <= colI-1
                    // Swap.
                    let tmp = arr.[i] |> getByIndex j
                    arr.[i] |> setByIndexInPlace j (arr.[rowI - 1] |> getByIndex (colI - 1))
                    arr.[rowI - 1] |> setByIndexInPlace (colI - 1) tmp
            arr 
    
        [|0..iterations-1|]
        |> Array.map (fun _ -> 
            let rndData = shuffleInPlace' Random.rndgen data        
            let rndDataA,rndDataU = Array.unzip rndData
            let tmp = calculate s0 rndDataA rndDataU                
            tmp |> Array.sortInPlaceBy (fun t -> t.Statistics) 
            tmp
            )


    // TODO: move to Array          private
    /// Iterates over elements of the input array and increase the counter if the function returens true
    let private countIf f (arr : _[]) =
        let mutable counter = 0
        for i=0 to arr.Length-1 do
            if f arr.[i] then
                counter <- counter + 1
        counter

    // TODO: move to Array            private
    let private binarySearchIndexBy compare (arr: 'a []) = 
        if arr.Length = 0 then ~~~(1) else

        let rec loop lower upper = 
            if lower > upper then ~~~ lower 
            else
                let middle = lower + ((upper - lower) / 2)
                let comparisonResult = compare arr.[middle]   
                if comparisonResult = 0 then
                    middle
                elif comparisonResult > 0 then
                    loop lower (middle - 1)
                else
                    loop (middle + 1) upper           
        loop 0 (arr.Length-1) 

    // TODO: move to Array            private
    let private bindBy (objArr:float[]) (arr:float[]) =
        let arr' = Array.copy arr
        let objArr' = Array.copy objArr
        let index = Array.init arr.Length id
        System.Array.Sort(objArr',index)
        for i=1 to arr'.Length-1 do
            if arr'.[index.[i]] < arr'.[index.[i-1]] then
                arr'.[index.[i]] <- arr'.[index.[i-1]]
        arr'

    /// Estimates pi0. Chosen to minimize the coefficient of variation of the test statistics 
    let estimatePi0 (dis:SAM []) (dibs:SAM[][]) =
        let perms =
            dibs
            |> Array.collect (fun x -> x |> Array.map (fun y -> y.Statistics))
        let q25 = Quantile.InPlace.modeInPLace 0.25 perms
        let q75 = Quantile.InPlace.modeInPLace 0.75 perms
        //let q0,q50=modeInPLace 0.0 perms , modeInPLace 0.5 perms
        let notSig =
            dis |> countIf (fun x -> q25 < x.Statistics && x.Statistics < q75)

        float notSig / (0.5 * float dis.Length)


    ///Finds all pairs of asymmetric cutoffs according to the differences between di and dEi
    let getAsymmetricCuts (di : SAM [])  (dei : SAM []) =
        let getDelta (_,_,delta) = delta
        let getDi (di,_,_) = di
        let replaceDeltaAbs (disA,deisA,deltaA) (disB,deisB,deltaB) = (disA,deisA,(abs deltaB))
        let di'  = di |> Array.sortBy (fun x -> x.Statistics)
        let dei' = dei |> Array.sortBy (fun x -> x.Statistics)
        let ups,los =
            Array.zip di' dei'
            |> Array.map (fun (di,dei) -> di.Statistics,dei.Statistics, (di.Statistics - dei.Statistics))
            |> Array.partition (fun (dis,deis,delta) -> deis >= 0.)
         //monoton increase ups (inplace)
        for i=1 to ups.Length-1 do
            if getDelta ups.[i] < getDelta ups.[i-1] then
                ups.[i] <- replaceDeltaAbs ups.[i] ups.[i-1]
                
        // monoton increase los (inplace)
        let los' = los |> Array.map (fun (a,b,c) -> a,b, abs c)
        for i=1 to los'.Length-1 do
            if getDelta los'.[i] > getDelta los'.[i-1] then
                los'.[i] <- replaceDeltaAbs los'.[i] los'.[i-1]
        // find the matching cutoffs
        let cuts set1 set2=
            set1
            |> Array.map (fun x -> 
                let cur =
                    set2
                    |> Array.filter (fun (dis,deis,delta) -> delta >= (getDelta x) )
                let y =
                    if Array.isEmpty cur then
                        let (dis,deis,delta) = x 
                        (dis,deis,(set1 |> Array.maxBy getDelta |> getDelta))
                    else
                        cur |> Array.minBy (fun (dis,deis,delta)-> delta )
                x,y)
        
        let cutsFromUp  = cuts ups los' |> Array.map (fun (a,b) -> getDi a, getDi b )
        let cutsFromLow = cuts los' ups |> Array.map (fun (a,b) -> getDi b, getDi a )
        
        [|cutsFromUp;cutsFromLow|]|> Array.concat |> Array.distinct
        

    ///Finds all pairs of symmetric cutoffs
    // an array conmtaining all the pairs of cuts, the higher cut is the first, the lower cut the second value
    let getSymmetricCuts (di : SAM [])  (dei : SAM [])=
        let ups,los =
            di
            |> Array.map (fun b -> b.Statistics)
            |> Array.sort
            |> Array.map (fun x -> (x, -x))
            |> Array.partition (fun (a,b) -> a >= 0.)
        [|ups;los|]|> Array.concat |> Array.distinct
        


    let qvalueByCut pi0 (dis: SAM []) (dibs: SAM[][]) (cut:(float*float)) = 
        // median amount of permutations above and below the cuts
        let cut1,cut2 = cut
        let gmed =
            dibs
            |> Array.map (fun arr -> arr |> countIf ( fun x -> x.Statistics >= cut1 || x.Statistics <= cut2) )
            |> Array.median

        
        let fdis = dis |> countIf (fun v -> v.Statistics > cut1 || v.Statistics < cut2)
        
        let medianfdr= (pi0 * float gmed)/( float fdis)
        //medianfdr*100.
        medianfdr


    let qvalues pi0 (dis: SAM []) (dibs: SAM[][]) (cuts:(float*float)[]) = 
        let convertIndex (arr: _ [])  (i:int) =
            if i < 0 then
                let i' = (~~~i)-1
                if i' < 0 then arr.[0] else arr.[i']
            else
                if i >= arr.Length then arr.[arr.Length-1] else arr.[i]

        let convertIndex' (arr: _ [])  (i:int) =
            if i < 0 then
                let i' = (~~~i)
                if i' < 0 then arr.[0] else
                    if i' >= arr.Length then arr.[arr.Length-1] else arr.[i']
            else
                if i >= arr.Length then arr.[arr.Length-1] else arr.[i]
                                
                

        let cuToQ,clToQ =
            cuts
            |> Array.map (fun cut ->
                let q = qvalueByCut pi0 dis dibs cut
                (fst cut,q),(snd cut,q) )
            |> Array.unzip
        // Ensure monotonicity within q values
        let cuToQ =
            let cu,q = cuToQ |> Array.unzip    
            let q' = bindBy (cu |> Array.map (fun x -> -x)) q
            Array.zip cu q'

        let clToQ =
            let cl,q = clToQ |> Array.unzip    
            let q' = bindBy cl q
            Array.zip cl q'

        cuToQ |> Array.sortInPlaceBy fst
        clToQ |> Array.sortInPlaceBy fst
        dis
        |> Array.map (fun s ->
            let cuQ = binarySearchIndexBy (fun (cu,v) -> compare cu s.Statistics) cuToQ |> convertIndex cuToQ |> snd
            let clQ = binarySearchIndexBy (fun (cl,v) -> compare cl s.Statistics) clToQ |> convertIndex' clToQ |> snd
            min cuQ clQ
            |> min 1.0
            )


    /// Module with different estimation methods computing s0 
    module S0 =
                

        /// Computes s0 using the nearest rank method
        let nearestRank percentile (tt:array<SAM>) =
            if percentile < 0. || percentile > 100.0 then failwith "Percentile must be between 0.0 and 100.0"
            let sortedTT= 
                tt
                |> Array.sortBy (fun t -> t.Si)
            let index = System.Math.Round( percentile / 100. * float tt.Length, 0)
            
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


            let si = tt |> Array.map (fun t -> t.Si)
            let di = tt |> Array.map (fun t -> t.Statistics)
            let si' = Array.copy si |> Array.sort
            let s0percentile= [|0. .. 0.05 .. 1.|] 
            let br = [|0. .. 0.01 .. 1.|] |> Array.map (fun p -> Quantile.mode p si')            
            let cvSd =
                let tmp = Seq.zip si di
                s0percentile
                |> Array.map (fun k -> 
                    let w  = if k = 0. then 0. else  Quantile.mode k si'
                    let mads = 
                        bind (fun a b -> fst b < a) br tmp
                        |> Seq.map (fun v ->
                            v 
                            |> Seq.map (fun (t,s) -> t * s / (s + w))
                            |> Seq.medianAbsoluteDev          
                            //|> fun x -> x / 0.64
                            )
                        |> Seq.toArray  
                        |> Array.map (fun (x) ->if nan.Equals(x) then 100. else x)
                    printfn "s0Perc = %f, cv = %f" k (Seq.cv mads) 
                    k, Seq.cv mads //Seq.stDev mads / Array.median mads

                    )
 
            let pMinCvSd = cvSd |> Array.minBy snd |> fst 
            Quantile.mode pMinCvSd si'


        /// Estimates s0 from given data set. Chosen to minimize the coefficient of variation of the test statistics 
        let estimateFrom calculate (dataA:float array array) (dataU:float array array) =
            let tt = calculate 0.0 dataA dataU
            estimate tt







//let n = 1000
//let nRep = 3

//let dataA =
//    [|0..n|]
//    |> Array.map (fun i ->
//        Array.init 3 (fun _ -> Distributions.Continuous.Normal.Sample 0. 1. ))


//let dataB =
//    [|0..n|]
//    |> Array.map (fun i ->
//        if i < 100 then 
//            Array.init 3 (fun _ -> Distributions.Continuous.Normal.Sample 5. 1. )
//        elif i > 100 && i < 200 then 
//            Array.init 3 (fun _ -> Distributions.Continuous.Normal.Sample -5. 1. )
//        else
//            Array.init 3 (fun _ -> Distributions.Continuous.Normal.Sample 0. 1. ))



//let priorStats = SAM.calculateTwoClassUnpaired 0. dataA dataB
//let s0 = SAM.S0.estimate priorStats

//let samStats  = SAM.calculateTwoClassUnpaired s0 dataA dataB
//let permStats = SAM.permutationWithin (SAM.calculateTwoClassUnpaired) 5 s0 dataA dataB 

//let pi0 = SAM.estimatePi0 samStats permStats

//let cuts = SAM.getSymmetricCuts samStats (SAM.expectedValue permStats)

//let qValues = SAM.qvalues pi0 samStats permStats cuts


//qValues |> Array.filter (fun q -> q <= 0.05) |> Array.length    
 

//Chart.Point(Seq.zip (samStats |> Seq.map (fun s -> s.Statistics)) qValues)
//|> Chart.Show


//Chart.Histogram (samStats |> Seq.map (fun x -> x.Statistics))
//|> Chart.Show



    