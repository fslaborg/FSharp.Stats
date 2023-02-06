namespace FSharp.Stats.Testing


open FSharp.Stats
open FSharpAux

module SAM =
////////////////////////////////////// Read in data /////////////////////////////////////////////////////////////////////////////////////////////////
/// read Data via deedle and create 2 samples (control/factor)

//let df:Frame<string,string> = 
//    Frame.ReadCsv(@"C:\..\TestDataSAM.txt",hasHeaders=true,separators = "\t")
//    |> Frame.indexRows "gene"


//df.Print()

//let rowheader :string[] = df.RowKeys |> Seq.toArray


////////////////////// CHUNKING ////////////////////
//let (sample1,sample2) :float[][] * float [][]=  
//    df
//    |> Frame.getRows
//    |> Series.values
//  ------------------------ Change the replicate amount ----------------
//    //|> Seq.map (Series.values >> Seq.toArray >> Array.chunkBySize 4 >> fun x -> x.[0],x.[1])
//    |> Seq.map (Series.values >> Seq.toArray >> Array.chunkBySize 3 >> fun x -> x.[0],x.[1])
//    |> Array.ofSeq
//    |> Array.unzip

//Array.map3 (fun id s1 s2 -> sprintf "%s\t%A\t%A" id s1 s2) rowheader sample1 sample2

// create tupel from sample name and data for better identification later

//let tupel a b = (a,b)
//let data1 = Array.zip rowheader sample1 
//let data2 = Array.zip rowheader sample2

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /// Type that contains all info regarding SAM for each sample
    type SAM = {
        /// identification of bioitem
        ID : string
        /// relative diffence of mean
        Ri : float
        /// pooled standard error 
        Si : float
        /// test statistics
        Statistics : float
        /// local FDR of bioitem
        QValue : float
        /// foldchange from condition to control
        Foldchange : float 
        /// mean condition A
        MeanA : float 
        /// standard deviation condition A 
        StDevA : float 
        /// mean condition B 
        MeanB : float 
        /// standard deviation condition B 
        StDevB : float 
        } with static member Create id ri si stats qv fc ma stdA mb stdB = {ID=id; Ri=ri; Si=si; Statistics=stats; QValue = qv;Foldchange = fc; MeanA = ma; StDevA = stdA;MeanB = mb; StDevB = stdB }


    // Result after calculating SAM
    /// SAM Result containing all information  
    type SAMResult = {
        /// small positive constant for independent variance of statistic
        S0 : float
        /// coefficient to avoid overestimation of FDR
        Pi0 : float
        /// treshold, absolute difference between observed and expected statistics
        Delta : float
        /// first Statistic where the difference of observed and expected is greater than delta
        UpperCut : float
        /// first Statistic where the difference of observed and expected is greater than delta
        LowerCut : float
        /// Array of all positively regulated bioitems at given FDR 
        PosSigBioitem : SAM []
        /// Array of all negatively regulated bioitems at given FDR 
        NegSigBioitem : SAM []
        /// Array of nonsignificant/unchanged bioitems at given FDR 
        NonSigBioitem : SAM []
        /// Array of the expected values using the average of permutations
        AveragePermutations : SAM []
        /// False Discovery Rate 
        FDR : float
        /// Amount of bioitems called significant (positive and negative) in the dataset at given FDR
        SigCalledCount : int
        /// Amount of "random" significant bioitems in the permutations (false positives) 
        MedianFalsePositivesCount : float
        } with static member Create s0 pi0 delta uc lc psg nesg nosg perms fdr scg mfp = {
                    S0          = s0
                    Pi0         = pi0
                    Delta       = delta
                    UpperCut    = uc
                    LowerCut    = lc
                    PosSigBioitem = psg
                    NegSigBioitem = nesg
                    NonSigBioitem = nosg
                    AveragePermutations = perms
                    FDR         = fdr
                    SigCalledCount = scg
                    MedianFalsePositivesCount = mfp
                    }


    /// median centering of the data
    let medianCentering data = 
        let calculateMedian:float[] = 
            data 
            |> Array.map snd 
            |> JaggedArray.transpose
            |> Array.map Array.median
        //printf "medians are %A" calculateMedian
        let medianCorrectedData = 
            data
            |> Array.map snd 
            |> JaggedArray.mapi (fun i x -> x - calculateMedian.[i])
        medianCorrectedData

    /// t-test statistic for observed statistics, used as default. 
    let getObservedStats (s0:float) (dataA:(string*float[])[]) (dataB:(string*float[])[])=

        let calcStats (a:string*float[]) (b:string*float[]) =  
            // test is arrays are same length
            if (fst a) <> (fst b) then failwithf "row identifier do not match"
            // get name of observed sample
            let idOfBioitem = fst a 
            // get datapoints 
            let dataA = snd a
            let dataB = snd b
        
            // get average of one sample 
            let ma,mb = Array.average dataA,Array.average dataB
            let fc = mb/ma
            let stDevA = Seq.stDev dataA
            let stDevB = Seq.stDev dataB
            let ri    = (fun x y -> x - y) mb ma 
            let si    = 
                // length of arrays
                let n1 = dataA |> Array.length |> float
                let n2 = dataB |> Array.length |> float
                // denominator of equation
                let denominator =  (fun x y -> ((x + y)-2.)) n1 n2
            
                let sums = 
                    ((dataA |> Array.sumBy (fun v -> (v - ma) * (v - ma))) + (dataB |> Array.sumBy (fun v -> (v - mb) * (v - mb))))
                 
                let firstPart = 
                    (fun x y -> 1./x + 1./y) n1 n2 
                let final = (((firstPart)*(sums/denominator))**0.5)

                final       

            let statistic = ri / (si + s0) 

        
            SAM.Create idOfBioitem ri si statistic nan fc ma stDevA mb stDevB

        Array.map2 (fun a b -> calcStats a b) dataA dataB





    module S0 = 
        /// S0 estimation 
        // the s0 percentiles used in R! are [|0. .. 0.05 .. 1.|] 
        // can be changed to users preference 
        let estS0 (values:SAM[]) (s0perc: float []) = 
            // tt is Statistics (Score) 
            // si is Si(Standard Deviation)
            let tt = values |> Array.map (fun x -> x.Statistics)
            let si = values |> Array.map (fun x -> x.Si)

            //separate the list of (tt,si) tuples into 100 percentiles
            //The very first value is assigned to the first percentile
            let percentilesOfSi =
                Array.init 100 (fun percentile -> 
                    let lower = Quantile.mode (float percentile / 100.) si
                    let upper = Quantile.mode ((float percentile + 1.) / 100.)  si
                    Array.zip tt si
                    |> Array.filter (fun (t,s) -> 
                        if percentile = 0 then 
                            lower <= s && s <= upper
                        else 
                            lower < s && s <= upper
                        )
                    )

            //for the given quantile, determine an estimate of s0 (as quantile of si) and calculate the statistics within the given percentiles
            let getCVFromQuantile quantile =
                //current s0 estimate based on sis
                let w = 
                    // if perc = 0 take correctionfactor s0 = 0, else calculate percentile 
                    if quantile = 0. then 
                        0. 
                    else Quantile.mode quantile si

                //With the given s0 calculate the median absolute deviation (mad) of every percentile.
                //Determine the cv of the mad.
                percentilesOfSi
                |> Array.map (fun percentiles -> 
                    let tt2 = 
                        percentiles 
                        |> Array.map (fun (tt,si) -> 
                            let tnew = tt * si / (si + w)
                            tnew
                            )
                    tt2
                    |> Array.filter (fun t -> not (nan.Equals t) && not (infinity.Equals t)) 
                    |> Seq.medianAbsoluteDev
                    )
                |> fun x -> 
                    let cv = Seq.cv x
                    // this filter is implemented in R!, not completely sure of function because we didn't have that case 
                    let siWOnull = si |> Array.filter (fun x -> x <> 0.)
                    let s0 = Quantile.mode quantile siWOnull
                    quantile,cv,s0

            s0perc
            |> Array.map getCVFromQuantile

        /// search s0 where coefficient of variation is minimal.
        /// input is the result from estS0 function 
        let s0ForMinCV s0List= s0List |> Array.minBy (fun (x,y,z) -> y) |>  fun (x,y,z) -> z


    module Permutations = 

        /// default permutation method. 
        /// create permutations using Fisher Yates Shuffling 
        let getPermutations iterations s0 (dataA:(string*float[])[]) (dataB:(string*float[])[]) rnd =
            let dataset = 
                dataA
                |> Array.mapi (fun i (id,data) -> 
                    id,Array.append data (snd dataB.[i])
                    )
            let replicateCount1 = (snd dataA.[0]).Length
            let replicateCount2 = (snd dataB.[0]).Length
    
            Array.init iterations (fun _ ->             
                let shuffleTemplate = 
                    Array.init (replicateCount1 + replicateCount2) id
                    |> FSharpAux.Array.shuffleFisherYates rnd

                let shuffledSampleA,shuffledSampleB = 
                    dataset
                    |> Array.map (fun (id,row) -> 
                        shuffleTemplate 
                        |> Array.map (fun si -> row.[si])
                        |> fun x -> 
                            (id,x.[..replicateCount1-1]),(id,x.[replicateCount1..])
                        )
                    |> Array.unzip
    
                let stats = getObservedStats s0 shuffledSampleA shuffledSampleB |> Array.sortBy (fun x -> x.Statistics) 
                stats 
                )

        /// expected Stats are the scores/values obtained from the permutations via averaging
        // ID stays unchanged
        let getExpectedStats (expStats:SAM[][]) =
           let iterations = expStats.Length
           let expAvgStats =
               expStats
               |> Array.map (Array.sortBy (fun x -> x.Statistics))
               |> JaggedArray.transpose
               |> Array.map (fun tt -> 
                               let (id',ri',si',di') = tt |> Array.fold (fun (id,ri,si,di) t -> t.ID,ri+t.Ri,si+t.Si,di + t.Statistics) ("",0.,0.,0.)  
                               SAM.Create id' (ri' / float iterations) (si' / float iterations) (di' / float iterations) nan nan nan nan nan nan 
                            )

           expAvgStats       

        // default for Microarray Analysis : 
        // let permStats = getPermutations 100 s0 dataA dataB
        // let expectedStats = getExpectedStats permStats 


    module SAMHelper = 

        /// function for counting 
        let private countIf f (arr : _[]) =
            let mutable counter = 0
            for i=0 to arr.Length-1 do
                if f arr.[i] then
                    counter <- counter + 1
            counter

        //let countIfCuts cutlow cuthigh (arr : SAM[]) =
        //    arr |> countIf (fun x -> x.Statistics > cuthigh || x.Statistics < cutlow) 
    
        /// estimate pi0 using the quantile method. Helps to not overestimate the False Discovery Rate 
        let estimatePi0 (obsStats:SAM []) (expStats:SAM[][]) =
            let perms =
                expStats
                |> Array.collect (fun x -> x |> Array.map (fun y -> y.Statistics)) 
            // obtain 25% and 75% 
            let q25 = Quantile.InPlace.modeInPLace 0.25 perms
            let q75 = Quantile.InPlace.modeInPLace 0.75 perms 
            // non significant data are between the 25 and 75 quantile
            let notSig =
                obsStats |> countIf (fun x -> q25 < x.Statistics && x.Statistics < q75)
    
            float notSig / (0.5 * float obsStats.Length)
    
        /// ensures continuous increase of data. 
        let monotonizeIncreasing (accessionFun: 'a -> float) (replaceFun: 'a -> float -> 'a) (input : 'a []) =
            let copy = Array.copy input
            for i=1 to copy.Length-1 do
                if accessionFun copy.[i] < accessionFun copy.[i-1] then
                    copy.[i] <- replaceFun copy.[i] (accessionFun copy.[i-1])
            copy
       
       /// ensures continuous decrease of data. 
        let monotonizeDecreasing (accessionFun: 'a -> float) (replaceFun: 'a -> float -> 'a) (input : 'a []) =
            let copy = Array.copy input
            for i=1 to copy.Length-1 do
                if accessionFun copy.[i] > accessionFun copy.[i-1] then
                    copy.[i] <- replaceFun copy.[i] (accessionFun copy.[i-1])
            copy
    
    
        /// default in R!. Calculates asymmetric cuts for each delta present. 
        // can be substituted with symmetric cuts (not implemented yet) 
        let getDeltaAndAsymmCuts (obsStats : SAM[]) (expAvgStats: SAM[]) =
            let getDelta (_,_,delta) = delta
            // di' are sorted Statistics from the observed (measured) values 
            let di' = obsStats |> Array.sortBy (fun x -> x.Statistics)
            // dei' are the sorted Statistics from the averaged values (permutations) 
            let dei' = expAvgStats |> Array.sortBy (fun x -> x.Statistics)
            // ups and los observed Statistics partitioned at 0. to sort them to positive and negative 
            let ups,los = 
                Array.zip di' dei' 
                |> Array.map (fun (di,dei) -> di.Statistics , dei.Statistics , (di.Statistics - dei.Statistics))
                |> Array.partition (fun (dis,deis,delta) -> dis >= 0.)
        
            // obtaining cuts based on delta (difference between observed and averaged statistics) 
            // finding matching positive or negative statistic as cutoff 
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
            // increase of cutoffs by +1E-80 to include the statistic itself that is used as cutoff 
            let ups' = monotonizeIncreasing getDelta (fun (di,dei,delta) newDelta -> (di,dei,newDelta+1E-80)) ups
            let los' = monotonizeDecreasing getDelta (fun (di,dei,delta) newDelta -> (di,dei,newDelta-1E-80)) los'
            let resultingcuts = [|getCuts ups' los';getCuts los' ups'|] |> Array.concat |> Array.distinct
        
            resultingcuts

        /// median false positives are obtained by counting how many statistics (from permuted data) fall outside of the cuts by chance.
        /// Permuted data should show no effect/change, therefor the ones that do are false positive 
        let getMedianFalsePositives cut expStats = 
            let cut1,cut2 = cut
            expStats
            |> Array.map (fun arr -> arr |> countIf ( fun x -> x.Statistics >= cut2 || x.Statistics <= cut1) ) 
            |> Array.map float
            |> Array.median
    
        /// amount of Bioitems that are more extreme than the cuts and therefor are called significant 
        let getSignificantBioitem cut obsStats = 
            let cut1,cut2 = cut
            obsStats |> countIf (fun v -> v.Statistics > cut2 || v.Statistics < cut1) 
    
        /// calculate the median False Discovery Rate
        let getMedianFdr pi0 medianFalsePos significantBioitems = 
            //if no permutation statistic is significant and no observed statistic is significant the fdr is 0 (not nan)
            if significantBioitems = 0. then 
                if medianFalsePos = 0. then 
                    0.
                else 1.
            else (pi0 * float medianFalsePos)/( float significantBioitems)
    

    
        /// get the smallest cut and corresponding delta for chosen FDR 
        // Result has to be smallestCutForFDR,delta                                               
        let smallestCutAndDeltaForFDR cuts expStats obsStats fdr pi0  = 
            cuts 
            |> Array.filter (fun (cut,delta)  -> 
                let tmpfdr =
                    let medianFalsePos = getMedianFalsePositives cut expStats
                    let significantBioitems = getSignificantBioitem cut obsStats
                
                    getMedianFdr pi0 medianFalsePos ( significantBioitems |> float)
                // "take the smallest delta such that FDR <= alpha "
                // alpha as in significance 
            
                tmpfdr <= fdr
            
                )
            |> Array.minBy snd

        
        /// Calculate the FDR for each cut 
        let fdrsPerCut cuts expStats obsStats pi0 = 
            cuts
            |> Array.map (fun (cut,delta) -> 
                let medianFalsePos = getMedianFalsePositives cut expStats
                let significantBioitems = getSignificantBioitem cut obsStats
                let localFDR = 
                    getMedianFdr pi0 medianFalsePos ( significantBioitems |> float)
               
                (cut,localFDR)
                )
        
        
        /// obtain negative cut for chosen FDR
        let negCutToFDR fdrsPerCut = 
            fdrsPerCut
            |> Array.map (fun ((lower,upper),fdr) -> lower,fdr)
            |> Array.sortByDescending fst
            |> monotonizeDecreasing snd (fun (origCut,fdr) newFdr -> origCut,newFdr)
            //to elimiate same cuts with multiple fdrs, minimal fdr per cut are isolated
            |> Array.sortBy snd
            |> Array.distinctBy fst
            //Array must be sorted by cut (descending because of negative values)
            |> Array.sortByDescending fst

        /// obtain positive cut for chosen FDR 
        let posCutToFDR fdrsPerCut = 
            fdrsPerCut
            |> Array.map (fun ((lower,upper),fdr) -> upper,fdr)
            |> Array.sortBy fst
            |> monotonizeDecreasing snd (fun (origCut,fdr) newFdr -> origCut,newFdr)
            //to elimiate same cuts with multiple fdrs, minimal fdr per cut are isolated
            |> Array.sortBy snd
            |> Array.distinctBy fst
            //Array must be sorted by cut
            |> Array.sortBy fst


    
        // >= and <= because of "The q-value of a gene is the FDR for the gene List that includes that gene and all genes that are more significant (SAM Manual)
        // Storey Chapter : d(j) - de >= delta & de - d(j) <= delta 
        // Qvalue calculation is done as in R!
        // Cuts corresponding to the chosen FDR are used. First data are parted at 0. for use of positive and negative cuts.
        // Array.tryFind searches the statistic in observed statistics that is greater/less or equal than the cut.
        // The nearest cut from observed statistics is matched with the FDR of that statistic (QValue = fdr) 


        // negative and positive cut are results from negCutToFDR and posCutToFDR
        /// returns the value for each sample where it is firstly called significantly different. 
        let getQvalues obsStats negativeCut positiveCut   = 
            obsStats 
            |> Array.map (fun currentBioitem -> 
                if currentBioitem.Statistics < 0. then
                    let firstLowerCut =
                        negativeCut
                        |> Array.tryFind (fun (lowerCut,fdr) -> 
                            lowerCut <= currentBioitem.Statistics
                            )
                    match firstLowerCut with 
                    | Some (cut,fdr) -> {currentBioitem with QValue = fdr}
                    | None -> {currentBioitem with QValue = 1.}
                else 
                    let firstGreaterCut = 
                        positiveCut 
                        |> Array.tryFind (fun (upperCut, fdr)-> 
                        upperCut >= currentBioitem.Statistics
                        )
                    match firstGreaterCut with
                    | Some (cut,fdr) -> {currentBioitem with QValue = fdr}
                    | None -> {currentBioitem with QValue = 1.}
                    )


    /// default version of SAM. Two class unpaired calculation. Rnd can be either System.Random() or System.Random(seed).
    let twoClassUnpaired iterations fdr data1 data2 rnd = 

        let getMedianFalsePositives cut expStats = 
            let cut1,cut2 = cut

            // function for counting 
            let countIf' (arr : SAM[]) =
                let mutable counter = 0
                for i=0 to arr.Length-1 do
                    if arr.[i].Statistics > cut2 || arr.[i].Statistics < cut1 then
                        counter <- counter + 1
                counter
            expStats
            |> Array.map (fun permutation -> 
                permutation 
                |> countIf'
                )
            |> Array.map float
            |> Array.median


        let getSignificantBioitem cut obsStats = 
            let cut1,cut2 = cut

            let countIf' (arr : SAM[]) =
                let mutable counter = 0
                for i=0 to arr.Length-1 do
                    if arr.[i].Statistics > cut2 || arr.[i].Statistics < cut1 then
                        counter <- counter + 1
                counter

            obsStats 
            |> countIf'

        let getMedianFdr pi0 medianFalsePos significantBioitems = 
            //if no permutation statistic is significant and no observed statistic is significant the fdr is 0 (not nan)
            if significantBioitems = 0. then 
                if medianFalsePos = 0. then 
                    0.
                else 1.
            else (pi0 * float medianFalsePos)/( float significantBioitems)

        let priorStats =    getObservedStats 0.0 data1 data2
        let s0prior =             S0.estS0 priorStats [|0. .. 0.05 .. 1.|]
        let s0 =                  s0prior |> Array.minBy (fun (x,y,z) -> y) |>  fun (x,y,z) -> z
        let obsStats =      getObservedStats s0 data1 data2 |> Array.sortBy (fun x -> x.Statistics)
        let expStats =     Permutations.getPermutations iterations s0 data1 data2 rnd 
        let expAvgStats =          Permutations.getExpectedStats(expStats)
        let cuts =          SAMHelper.getDeltaAndAsymmCuts obsStats expAvgStats 
        let pi0 =           SAMHelper.estimatePi0 obsStats expStats
        let fdrsPerCut = 
            cuts
            |> Array.map (fun (cut,delta) -> 
                let medianFalsePos = getMedianFalsePositives cut expStats
                let significantBioitem = getSignificantBioitem cut obsStats
                let localFDR = 
                    getMedianFdr pi0 medianFalsePos ( significantBioitem |> float)
                (cut,localFDR,delta)
                )

        let smallestCutForFDR,delta = 
                    let smallestCut =
                        fdrsPerCut
                        |> Array.filter (fun (cut,tmpfdr,delta) -> 
                            tmpfdr <= fdr
                            )
                    match smallestCut with 
                    | [||] ->  ((0.,0.),100000.)
                    | _ -> 
                        smallestCut
                        |> Array.minBy (fun (a,b,c) -> c)
                        |> (fun (a,b,c) -> a,c)

        match smallestCutForFDR,delta with 
        | ((0.,0.),100000.) ->
            let defaultIfNoSignificance:SAM[]= [||]
            printfn "No significant results were found"
            SAMResult.Create s0 pi0 delta 0. 0. defaultIfNoSignificance defaultIfNoSignificance obsStats expAvgStats 0. 0 0 
        | _ -> 
            let negCutToFDR = 
                fdrsPerCut
                |> Array.map (fun ((lower,upper),fdr,delta) -> lower,fdr)
                |> Array.sortByDescending fst
                |> SAMHelper.monotonizeDecreasing snd (fun (origCut,fdr) newFdr -> origCut,newFdr)
                //to elimiate same cuts with multiple fdrs, minimal fdr per cut are isolated
                |> Array.sortBy snd
                |> Array.distinctBy fst
                //Array must be sorted by cut (descending because of negative values)
                |> Array.sortByDescending fst

            let posCutToFDR = 
                fdrsPerCut
                |> Array.map (fun ((lower,upper),fdr,delta) -> upper,fdr)
                |> Array.sortBy fst
                |> SAMHelper.monotonizeDecreasing snd (fun (origCut,fdr) newFdr -> origCut,newFdr)
                //to elimiate same cuts with multiple fdrs, minimal fdr per cut are isolated
                |> Array.sortBy snd
                |> Array.distinctBy fst
                //Array must be sorted by cut
                |> Array.sortBy fst

            // >= and <= because of "The q-value of a gene is the FDR for the gene List that includes that gene and all genes that are more significant (SAM Manual)
            // Storey Chapter : d(j) - de >= delta & de - d(j) <= delta 
            let getQvalues obsStats = 
                obsStats 
                |> Array.map (fun currentBioitem -> 
                    if currentBioitem.Statistics < 0. then
                        let firstLowerCut =
                            negCutToFDR
                            |> Array.tryFind (fun (lowerCut,fdr) -> 
                                lowerCut <= currentBioitem.Statistics
                                )
                        match firstLowerCut with 
                        | Some (cut,fdr) -> {currentBioitem with QValue = fdr}
                        | None -> {currentBioitem with QValue = 1.}
                    else 
                        let firstGreaterCut = 
                            posCutToFDR 
                            |> Array.tryFind (fun (upperCut, fdr)-> 
                            upperCut >= currentBioitem.Statistics
                            )
                        match firstGreaterCut with
                        | Some (cut,fdr) -> {currentBioitem with QValue = fdr}
                        | None -> {currentBioitem with QValue = 1.}
                        )

            let chosenDelta =  delta
            let upperCut = snd smallestCutForFDR
            let lowerCut = fst smallestCutForFDR
            let getQvals = getQvalues obsStats
            let PosSigBioitem = getQvals |> Array.filter (fun x -> x.Statistics > upperCut)
            let NegSigBioitem = getQvals |> Array.filter (fun x -> x.Statistics < lowerCut)
            let NonSigBioitem = getQvals |> Array.filter (fun x -> x.Statistics >= lowerCut && x.Statistics <= upperCut)
            let medianFalsePos = getMedianFalsePositives smallestCutForFDR expStats 
            let significantBioitems = getSignificantBioitem smallestCutForFDR getQvals 
            let medianFDR = getMedianFdr pi0 medianFalsePos ( significantBioitems |> float)
            let delta = chosenDelta 


            SAMResult.Create s0 pi0 delta upperCut lowerCut PosSigBioitem NegSigBioitem NonSigBioitem expAvgStats medianFDR significantBioitems medianFalsePos 

// Workflow of SAM (default mode)  

//let priorStats = getObservedStats 0.0 data1 data2

//let s0est = S0.estS0 priorStats [|0. .. 0.05 .. 1.|]
//let s0 = S0.s0ForMinCV s0est

//let observedStats = teststatsSAM.getObservedStats s0 data1 data2

//let permStats = Permutations.getPermutations 100 s0 data1 data2 

//let expStats = Permutations.getExpectedStats permStats

//let pi0 = SAMHelper.estimatePi0 observedStats permStats

//let cutsAndDelta = SAMHelper.getDeltaAndAsymmCuts observedStats expStats

//let smallestCutForFDR = SAMHelper.smallestCutAndDeltaForFDR cutsAndDelta permStats observedStats 0.05 pi0

//let fdrsPerCut = SAMHelper.fdrsPerCut cutsAndDelta permStats observedStats pi0 

//let negCut = SAMHelper.negCutToFDR fdrsPerCut 

//let posCut = SAMHelper.posCutToFDR fdrsPerCut 

//let qValues = SAMHelper.getQvalues observedStats negCut posCut

//let delta = snd smallestCutForFDR

//let upperCut = snd (fst smallestCutForFDR)

//let lowerCut = fst (fst smallestCutForFDR)

//let posSigGenes = qValues |> Array.filter (fun x -> x.Statistics > upperCut) 

//let negSigGenes = qValues |> Array.filter (fun x -> x.Statistics < lowerCut) 

//let nonSigGenes = qValues |> Array.filter (fun x -> x.Statistics >= lowerCut && x.Statistics <= upperCut)

//let medianFalsePos = SAMHelper.getMedianFalsePositives (fst smallestCutForFDR) permStats

//let significantGenes = SAMHelper.getSignificantGenes (fst smallestCutForFDR) qValues

//let medianFDR = SAMHelper.getMedianFdr pi0 medianFalsePos ( significantGenes |> float)
