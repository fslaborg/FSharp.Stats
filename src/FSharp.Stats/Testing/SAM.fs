namespace FSharp.Stats.Testing


open FSharp.Stats


//#r "nuget: Deedle"
//#r "nuget: Plotly.NET, 2.0.0-preview.6"
//#r "nuget: Plotly.NET.Interactive, 2.0.0-preview.6"


//open Plotly.NET

//open Deedle

///// read Data via deedle and create 2 samples (untreated/treated)
//let df:Frame<string,string> = 
//    Frame.ReadCsv(@"C:\Users\Selly\Desktop\testx.txt",hasHeaders=true,separators = "\t")
//    |> Frame.indexRows "genes"


//df.Print()

//let rowheader :string[] = df.RowKeys |> Seq.toArray

//let (sample1,sample2) :float[][] * float [][]=  
//    df
//    |> Frame.getRows
//    |> Series.values
//    |> Seq.map (Series.values >> Seq.toArray >> Array.chunkBySize 4 >> fun x -> x.[0],x.[1])
//    |> Array.ofSeq
//    |> Array.unzip
    
//Array.map3 (fun id s1 s2 -> sprintf "%s\t%A\t%A" id s1 s2) rowheader sample1 sample2

//// create tupel from gene name and data for better identification later
//let tupel a b = (a,b)
//let data1 = Array.map2 tupel rowheader sample1 
//let data2 = Array.map2 tupel rowheader sample2


type SAM = {
    // identification of gene
    ID : string
    /// relative diffence of mean
    Ri : float
    /// pooled standard error 
    Si : float
    /// test statistics
    Statistics : float
    // local FDR of gene
    QValue : float
    } with static member Create id ri si stats qv= {ID=id; Ri=ri; Si=si; Statistics=stats; QValue = qv}


type SAMResult = {
    // small positive constant for independent variance of gene expression
    S0 : float
    // coefficient to avoid overestimation of FDR
    Pi0 : float
    // treshold, absolute difference between observed and expected statistics
    Delta : float
    // first Statistic where the difference of observed and expected is greater than delta
    UpperCut : float
    // first Statistic where the difference of observed and expected is greater than delta
    LowerCut : float
    // Array of all positively regulated genes at given FDR 
    PosSigGenes : SAM []
    // Array of all negatively regulated genes at given FDR 
    NegSigGenes : SAM []
    // Array of nonsignificant/unchanged  at given FDR 
    NonSigGenes : SAM []
    // Array of the expected values using the average of permutations
    AveragePermutations : SAM []
    // False Discovery Rate 
    FDR : float
    // Amount of genes called significant (positive and negative) in the dataset
    SigCalledGenesCount : int
    // Amount of "random" significant genes in the permutations (false positives) 
    MedianFalsePositivesCount : float
    } with static member Create s0 pi0 delta uc lc psg nesg nosg perms fdr scg mfp = {
                S0          = s0
                Pi0         = pi0
                Delta       = delta
                UpperCut    = uc
                LowerCut    = lc
                PosSigGenes = psg
                NegSigGenes = nesg
                NonSigGenes = nosg
                AveragePermutations = perms
                FDR         = fdr
                SigCalledGenesCount = scg
                MedianFalsePositivesCount = mfp
                }
module SAM = 
// calculates the statistic for the dataset (observed results) 
    let getObservedStats (s0:float) (dataa:(string*float[])[]) (datab:(string*float[])[])=

        let calcStats (a:string*float[]) (u:string*float[]) =  
            // test is arrays are same length
            if (fst a) <> (fst u) then failwithf "row identifier do not match"
            let idOfGene = fst a 
            let dataA = snd a
            let dataB = snd u
        
            // get average of one gene 
            let ma,mu = Array.average dataA,Array.average dataB
            let ri    = (fun x y -> x - y) mu ma 
            let si    = 
                // length of arrays
                let n1 = dataA |> Array.length |> float
                let n2 = dataB |> Array.length |> float
                // denominator of equation
                let denominator =  (fun x y -> ((x + y)-2.)) n1 n2
            
                let sums = 
                    ((dataA |> Array.sumBy (fun v -> (v - ma) * (v - ma))) + (dataB |> Array.sumBy (fun v -> (v - mu) * (v - mu))))
                //let stats =
                let firstpart = 
                    (fun x y -> 1./x + 1./y) n1 n2 
                let final = (((firstpart)*(sums/denominator))**0.5)
            
                //from r/ttest.func.R ((n2-1) * varr(x[, y==2], meanx=m2) + (n1-1) * varr(x[, y==1], meanx=m1) )
                //from r/ttest.func.R 
                //from r/ttest.func.R // first part / denominator
                //from r/ttest.func.R (1/n1+1/n2)/(n1+n2-2)
                final       
            // calculate statistic (ri/si+s0)

            let statistic = ri / (si + s0)
        
            SAM.Create idOfGene ri si statistic nan

        Array.map2 (fun a b -> calcStats a b) dataa datab



    let estimateS0 (priorStats:array<SAM>) = 
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

    
        let si = priorStats |> Array.map (fun t -> t.Si)
        let di = priorStats |> Array.map (fun t -> t.Statistics)
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
                    
                        )
                    |> Seq.toArray  
                    |> Array.map (fun (x) ->if nan.Equals(x) then 100. else x)
                //printfn "s0Perc = %f, cv = %f" k (Seq.cv mads) 
                k, Seq.cv mads 

                )
 
        let pMinCvSd = cvSd |> Array.minBy snd |> fst 
        Quantile.mode pMinCvSd si'


    // create permutations with Fisher Yates Shuffling 

    /// Anpassen von chunking (mit n = Array.map snd data.length |> ("gene", x.[.. n/2-1]) , (["gene",x.[n..])0
    /// default permutation scheme
    let (*private*) getPermutations iterations s0 (data1:(string*float[])[]) (data2:(string*float[])[])  =
        let dataset = 
            data1
            |> Array.mapi (fun i (id,data) -> 
                Array.append data (snd data2.[i])
                )

        Array.init iterations (fun iteration ->             
            let shuffletemplate = 
                Array.init 8 id
                |> FSharp.Stats.Array.shuffleFisherYates

            //printfn "%A" shuffletemplate
            let shuffledsample1,shuffledsample2 = 
                dataset
                |> Array.map (fun row -> 
                    shuffletemplate 
                    |> Array.map (fun si -> row.[si])
                    |> fun x -> 
                        ("gene",x.[..3]),("gene",x.[4..])
                    )
                |> Array.unzip

            let stats = getObservedStats s0 shuffledsample1 shuffledsample2 |> Array.sortBy (fun x -> x.Statistics) 
            stats 
            )
    ////// new permutation to test
    let getPermutations2 iterations s0 (data1:(string*float[])[]) (data2:(string*float[])[]) = 
        let dataset = 
               data1
               |> Array.mapi (fun i (id,data) -> 
                   Array.append data (snd data2.[i])
                   )
        Array.init iterations (fun iterations ->   
            let shuffleddata= (
                dataset |> Array.map (fun x -> 
                    x |> Array.shuffleFisherYates))
            let shuffled1,shuffled2 =  
                (shuffleddata 
                |> Array.map ( fun x -> ("gene",x.[..3]),("gene",x.[4..]) ))
                |> Array.unzip
        
            
            let stats = getObservedStats s0 shuffled1 shuffled2 |> Array.sortBy (fun x -> x.Statistics)
            stats)

    /// permutation from Esthers Master Thesis 
    let permutationBalanced (calculate: float -> (string*float [])[] -> (string*float [])[] -> SAM[]) iterations s0 (dataA:(string*float array) array) (dataU:(string * float array) array)  =
        let dataA' = Array.map snd dataA
        let dataB' = Array.map snd dataU
        let half=int (float  dataA'.[0].Length/ 2.)
        let amount= dataA'.Length
        let reps= dataA'.[0].Length

        let dib=
            let rec balanced index (dataA:(string*float[])[]) (dataB:(string*float[])[]) temp1 =
                if index < iterations then
                    let rndDataA0,rndDataU0 = 
                        let arrA =
                            //if random sampling from every row sequences:
                            //let tmp = [|0..reps-1|] |> Array.shuffleFisherYates
                            ////if first half of SeqA and second half of SeqB:
                            let tmp = [|0..reps-1|] 
                            let indicesRandomPart1 = tmp.[..half-1]
                            let indicesRandomPart2 = tmp.[half..]
                            let getValuesOfIndices indices (data:string*float[]) =
                                indices |> Array.map (fun x ->  (snd data).[x])

                            //global array with each entry representing a row in raw data
                            Array.init amount (fun i -> 
                                let fstTupel = fst dataA.[i],Array.append (getValuesOfIndices indicesRandomPart1 dataA.[i]) (getValuesOfIndices indicesRandomPart2 dataB.[i])
                                let sndTupel = fst dataA.[i],Array.append (getValuesOfIndices indicesRandomPart1 dataB.[i]) (getValuesOfIndices indicesRandomPart2 dataA.[i])
                                fstTupel,sndTupel
                                )
                        arrA |> Array.unzip

                    let rndDataA =  rndDataA0|> Array.map (fun (identifier,shuffledData) -> identifier,Array.shuffleFisherYates shuffledData)
                    let rndDataU =  rndDataU0|> Array.map (fun (identifier,shuffledData) -> identifier,Array.shuffleFisherYates shuffledData)
                    let temp= calculate s0 rndDataA rndDataU
                    temp|> Array.sortInPlaceBy ( fun x -> x.Statistics)
                    balanced (index+1) rndDataA rndDataU (temp::temp1)               
                else temp1|> List.toArray
            balanced 0 dataA dataU []
        dib


    // expected value from permutations
    // mean of all permutations per gene 
    let getExpectedStats (expStats:SAM[][]) =
       let iterations = expStats.Length
       let dei =
           expStats
           |> Array.map (Array.sortBy (fun x -> x.Statistics))
           |> JaggedArray.transpose
           |> Array.map (fun tt -> 
                           let (id',ri',si',di') = tt |> Array.fold (fun (id,ri,si,di) t -> t.ID,ri+t.Ri,si+t.Si,di + t.Statistics) ("",0.,0.,0.)  
                           SAM.Create id' (ri' / float iterations) (si' / float iterations) (di' / float iterations) nan
                        )

       dei       


    let (*private*) countIf f (arr : _[]) =
        let mutable counter = 0
        for i=0 to arr.Length-1 do
            if f arr.[i] then
                counter <- counter + 1
        counter

    // estimate pi0 using the quantile method. Helps to not overestimate the False Discovery Rate 
    //umbenennen: samStats=obsStats; permStats=expStats
    let estimatePi0 (obsStats:SAM []) (expStats:SAM[][]) =
        let perms =
            expStats
            |> Array.collect (fun x -> x |> Array.map (fun y -> y.Statistics)) 
        let q25 = Quantile.InPlace.modeInPLace 0.25 perms
        let q75 = Quantile.InPlace.modeInPLace 0.75 perms 
        //let q0,q50=modeInPLace 0.0 perms , modeInPLace 0.5 perms
        let notSig =
            obsStats |> countIf (fun x -> q25 < x.Statistics && x.Statistics < q75)

        float notSig / (0.5 * float obsStats.Length)




    let monotonizeIncreasing (accessionFun: 'a -> float) (replaceFun: 'a -> float -> 'a) (input : 'a []) =
        let copy = Array.copy input
        for i=1 to copy.Length-1 do
            if accessionFun copy.[i] < accessionFun copy.[i-1] then
                //copy.[i] <- copy.[i-1]
                copy.[i] <- replaceFun copy.[i] (accessionFun copy.[i-1])
        copy
   
   
    let monotonizeDecreasing (accessionFun: 'a -> float) (replaceFun: 'a -> float -> 'a) (input : 'a []) =
        let copy = Array.copy input
        for i=1 to copy.Length-1 do
            if accessionFun copy.[i] > accessionFun copy.[i-1] then
                //copy.[i] <- copy.[i-1]
                copy.[i] <- replaceFun copy.[i] (accessionFun copy.[i-1])
        copy


// Monotonisierung drin, aber dann wieder lowercut auf -infinity bei 0.05 FDR und s0 = 0.0 
    let getDeltaAndAsymmCuts (obsStats : SAM[]) (dei: SAM[]) =
        let getDelta (_,_,delta) = delta
        let getDi (di,_,_) = di
        let replaceDeltaAbs (disA,deisA,deltaA) (disB,deisB,deltaB) = (disA,deisA,(abs deltaB))
        let di' = obsStats |> Array.sortBy (fun x -> x.Statistics)
        let dei' = dei |> Array.sortBy (fun x -> x.Statistics)
        let ups,los = 
            Array.zip di' dei' 
            |> Array.map (fun (di,dei) -> di.Statistics , dei.Statistics , (di.Statistics - dei.Statistics))
            |> Array.partition (fun (dis,deis,delta) -> dis >= 0.)

        let getCuts set1 set2 =
            set1 
            |> Array.map (fun (currentDi, currentDei, currentDelta) ->
                if currentDi > 0. then
                    set2 
                    |> Array.tryFindBack (fun (di,dei,delta) -> delta >= currentDelta)
                    |> fun x -> 
                        match x with 
                        | Some (di,dei,delta) -> (di,currentDi),currentDelta
                        | None -> (-infinity,currentDi),currentDelta
                else
                    set2 
                    |> Array.tryFind (fun (di,dei,delta) -> delta >= currentDelta)
                    |> fun x ->
                        match x with 
                        | Some (di,dei,delta) -> (currentDi,di),currentDelta
                        | None -> (currentDi,infinity),currentDelta
                    )
        let los' = los |> Array.map (fun (a,b,c) -> a,b, abs c )

        let ups' = monotonizeIncreasing getDelta (fun (di,dei,delta) newDelta -> (di,dei,newDelta+1E-80)) ups
        let los' = monotonizeDecreasing getDelta (fun (di,dei,delta) newDelta -> (di,dei,newDelta-1E-80)) los'
        let resultingcuts = [|getCuts ups' los';getCuts los' ups'|] |> Array.concat |> Array.distinct

        //resultingcuts
        //|> Array.iter (
        //    fun values -> 
        //        let ((a,b),c) = values 
        //        printfn ("%f \t %f \t %f \t") a b c 
        //    )

        resultingcuts




//// output of significant genes divided into positive and negative changing genes 
//let significantGeneResults (a,b,c,(d,e)) (samStats:SAM []) = 
//    let sigs = 
//        samStats |> Array.filter (fun v -> v.Statistics > d || v.Statistics < e) 
//    let sigpos,signeg = 
//        sigs 
//        |> Array.partition (fun x -> x.Statistics > 0.)
//    sigpos,signeg

//FSharp.Stats.Testing.SAM.twoClassUnpaired fdr iterations (data1:string,float[])
//umbenennen: twoClassUnpaired
    let twoClassUnpaired iterations fdr data1 data2 = 
    // count genes that fall above or below cut -> < and > (SAM Manual)

        let getMedianFalsePositives cut expStats = 
            let cut1,cut2 = cut
            expStats
            |> Array.map (fun arr -> arr |> countIf ( fun x -> x.Statistics > cut2 || x.Statistics < cut1) ) 
            |> Array.map float
            |> Array.median

        let getSignificantGenes cut obsStats = 
            let cut1,cut2 = cut
            obsStats |> countIf (fun v -> v.Statistics > cut2 || v.Statistics < cut1) 

        let getMedianFdr pi0 medianFalsePos significantGenes = 
            //if no permutation statistic is significant and no observed statistic is significant the fdr is 0 (not nan)
            //if medianFalsePos = 0. && significantGenes = 0 then 
            //    0. 
            if significantGenes = 0. then 
                if medianFalsePos = 0. then 
                    0.
                else 1.
            else (pi0 * float medianFalsePos)/( float significantGenes)

        let priorStats =    getObservedStats 0.0 data1 data2
        let s0 =            estimateS0 priorStats
        let obsStats =      getObservedStats s0 data1 data2 |> Array.sortBy (fun x -> x.Statistics)
        let expStats =      getPermutations iterations s0 data1 data2
        let deis =          getExpectedStats(expStats)
        printfn "checkp 0"
        let cuts =          getDeltaAndAsymmCuts obsStats deis 
        printfn "checkp 1"
        let pi0 =           estimatePi0 obsStats expStats
    
        printfn "checkp 2"
        let smallestCutForFDR,delta = 
            cuts 
            |> Array.filter (fun (cut,delta) -> 
                let tmpfdr =
                    let medianFalsePos = getMedianFalsePositives cut expStats
                    let significantGenes = getSignificantGenes cut obsStats
                    printfn "%A \t %A \t" medianFalsePos significantGenes
                    getMedianFdr pi0 medianFalsePos ( significantGenes |> float)
                // "take the smallest delta such that FDR <= alpha "
                printfn "%A" tmpfdr
                tmpfdr <= fdr
            
                //significanceByCut pi0 samStats permStats
                )
            |> Array.minBy snd
        //printfn "smallest cuts & delta %A " smallestCutForFDR , delta 
            

    
        printfn "checkp 3"
        // Qval //

        let fdrsPerCut = 
            cuts
            |> Array.map (fun (cut,delta) -> 
                let medianFalsePos = getMedianFalsePositives cut expStats
                let significantGenes = getSignificantGenes cut obsStats
                let localFDR = 
                    getMedianFdr pi0 medianFalsePos ( significantGenes |> float)
                //printfn "%f\t%f\t%f\t%i\t%f" (fst cut) (snd cut) medianFalsePos significantGenes localFDR
                (cut,localFDR)
                )
        //printfn "fdrs are %A" fdrsPerCut
    
        printfn "checkp 4"
        let negCutToFDR = 
            fdrsPerCut
            |> Array.map (fun ((lower,upper),fdr) -> lower,fdr)
            |> Array.sortByDescending fst
            |> monotonizeDecreasing snd (fun (origCut,fdr) newFdr -> origCut,newFdr)
            //to elimiate same cuts with multiple fdrs, minimal fdr per cut are isolated
            |> Array.sortBy snd
            |> Array.distinctBy fst
            //Array must be sorted by cut (descending because of negative values)
            |> Array.sortByDescending fst

        printfn "checkp 5"
        let posCutToFDR = 
            fdrsPerCut
            |> Array.map (fun ((lower,upper),fdr) -> upper,fdr)
            |> Array.sortBy fst
            |> monotonizeDecreasing snd (fun (origCut,fdr) newFdr -> origCut,newFdr)
            //to elimiate same cuts with multiple fdrs, minimal fdr per cut are isolated
            |> Array.sortBy snd
            |> Array.distinctBy fst
            //Array must be sorted by cut
            |> Array.sortBy fst
        
        //posCutToFDR |> Chart.Point |> Chart.withX_AxisStyle "uppercut" |> Chart.withY_AxisStyle "FDr" |> Chart.Show
        //negCutToFDR |> Chart.Point |> Chart.withX_AxisStyle "lowercut" |> Chart.withY_AxisStyle "FDr" |> Chart.Show
        // >= and <= because of "The q-value of a gene is the FDR for the gene List that includes that gene and all genes that are more significant (SAM Manual)
        // Storey Chapter : d(j) - de >= delta & de - d(j) <= delta 
        let getQvalues obsStats = 
            obsStats 
            |> Array.map (fun currentGene -> 
                if currentGene.Statistics < 0. then
                    let firstLowerCut =
                        negCutToFDR
                        |> Array.tryFind (fun (lowerCut,fdr) -> 
                            lowerCut <= currentGene.Statistics
                            )
                    match firstLowerCut with 
                    | Some (cut,fdr) -> {currentGene with QValue = fdr}
                    | None -> {currentGene with QValue = 1.}
                else 
                    let firstGreaterCut = 
                        posCutToFDR 
                        |> Array.tryFind (fun (upperCut, fdr)-> 
                        upperCut >= currentGene.Statistics
                        )
                    match firstGreaterCut with
                    | Some (cut,fdr) -> {currentGene with QValue = fdr}
                    | None -> {currentGene with QValue = 1.}
                    )




    
        //let chosenCuts = smallestCutForFDR
        let chosenDelta =  delta

        let upperCut = snd smallestCutForFDR
        let lowerCut = fst smallestCutForFDR
        let getQvals = getQvalues obsStats


        //To do
        let posSigGenes = getQvals |> Array.filter (fun x -> x.Statistics > upperCut)
        let negSigGenes = getQvals |> Array.filter (fun x -> x.Statistics < lowerCut)

        let nonSigGenes = getQvals |> Array.filter (fun x -> x.Statistics >= lowerCut && x.Statistics <= upperCut)
        let medianFalsePos = getMedianFalsePositives smallestCutForFDR expStats
        let significantGenes = getSignificantGenes smallestCutForFDR getQvals
        let medianFDR = getMedianFdr pi0 medianFalsePos ( significantGenes |> float)
        let delta = chosenDelta 
    
        printfn "obs length is %A" getQvals.Length
        SAMResult.Create s0 pi0 delta upperCut lowerCut posSigGenes negSigGenes nonSigGenes deis medianFDR significantGenes medianFalsePos 


//let res = twoClassUnpaired 150 0.05 data1 data2