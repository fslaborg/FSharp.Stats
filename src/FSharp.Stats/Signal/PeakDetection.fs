namespace FSharp.Stats.Signal

open FSharp.Stats
open System



module PeakDetection =
    
    /// TODO: Add FSharpAux reference
    /// Iterates the data array beginning from the startIdx. 
    /// The step size and direction are implied by magnitude and sign of stepSize. The function returns
    /// the idx of the first value for which predicate returns true or the end/start of the collection
    /// is reached (returning None). 
    let iterUntil (predicate: 'T -> bool) stepSize startIdx (arr: 'T []) =
        let rec loop  (arr: 'T []) currentIdx =
            if currentIdx <= 0 then None
            elif currentIdx >= arr.Length-1 then None
            else                                              
                match predicate arr.[currentIdx] with 
                | true -> Some currentIdx   
                | _               -> loop arr (currentIdx+stepSize) 
        loop arr startIdx 

    /// TODO: Add FSharpAux reference
    /// Iterates the data array beginning from the startIdx. 
    /// The step size and direction are implied by magnitude and sign of stepSize. The function returns
    /// the idx of the first value for which predicate returns true or the end/start of the collection
    /// is reached (returning None). The predicate function takes the idx of the current value as an additional
    /// parameter.
    let iterUntili (predicate: int -> 'T -> bool) stepSize startIdx (arr: 'T []) =
        let rec loop  (arr: 'T []) currentIdx =
            if currentIdx <= 0 then None
            elif currentIdx >= arr.Length-1 then None
            else                                              
                match predicate currentIdx arr.[currentIdx] with 
                | true -> Some currentIdx   
                | _               -> loop arr (currentIdx+stepSize) 
        loop arr startIdx     


    ///        
    type Tag<'t,'v> = {
        Meta : 't
        Data : 'v
        }
    
    ///
    type Extrema =
        | None 
        | Positive 
        | Negative 

    ///
    type PeakFeature = {
        Index :int
        XVal  :float
        YVal  :float
        }

    ///
    let createPeakFeature index xVal yVal = {
        Index = index
        XVal  = xVal 
        YVal  = yVal 
        }    

    ///
    type IdentifiedPeak = {
        Apex                 : PeakFeature option
        LeftLiftOff          : PeakFeature option 
        LeftEnd              : PeakFeature option
        RightLiftOff         : PeakFeature option 
        RightEnd             : PeakFeature option 
        LeftSidedConvolved   : bool
        RightSidedConvolved  : bool
        XData                : float []
        YData                : float []
        }

    ///
    let createIdentifiedPeak apex leftLiftOff leftEnd rightLiftOff rightEnd leftSidedConvolved rightSidedConvolved xData yData = {
        Apex                = apex                
        LeftLiftOff         = leftLiftOff         
        LeftEnd             = leftEnd             
        RightLiftOff        = rightLiftOff        
        RightEnd            = rightEnd            
        LeftSidedConvolved  = leftSidedConvolved  
        RightSidedConvolved = rightSidedConvolved 
        XData               = xData               
        YData               = yData               
        }    

    /// Returns a collection local maxima. Attention: The algorithm is very sensitive to noise and behaves irregulary for negative Y-values.
    let localMaxima yThreshold (xData:float[]) (smoothYData:float[]) =
        if xData.Length <= 5 then [||]
        else       
        [|for i = 3 to xData.Length-3 do
            // Peak must be concave in the interval [i-2 .. i+2] and exheat a yThreshold
            if (smoothYData.[i] > yThreshold && smoothYData.[i] > smoothYData.[i - 1] && smoothYData.[i] > smoothYData.[i + 1] 
                                    && smoothYData.[i - 1] >= smoothYData.[i - 2] && smoothYData.[i + 1] >= smoothYData.[i + 2]) then

                // take the intensity at the apex of the profile peak
                yield (xData.[i], smoothYData.[i])
            |]  
    
    /// Returns a collection of indices corresponding to local maxima. Attention: The algorithm is very sensitive to noise and behaves irregulary for negative Y-values.
    let localMaximaIdx yThreshold (xData:float[]) (smoothYData:float[]) =
        if xData.Length <= 5 then [||]
        else       
        [|for i = 3 to xData.Length-3 do
            // Peak must be concave in the interval [i-2 .. i+2] and exheat a yThreshold
            if (smoothYData.[i] > yThreshold && smoothYData.[i] > smoothYData.[i - 1] && smoothYData.[i] > smoothYData.[i + 1] 
                                    && smoothYData.[i - 1] >= smoothYData.[i - 2] && smoothYData.[i + 1] >= smoothYData.[i + 2]) then

                // take the intensity at the apex of the profile peak
                yield i
            |]  

    /// Returns a collection of local minima. Attention: The algorithm is very sensitive to noise   
    let localMinima (xData:float[]) (smoothYData:float[]) =
        if xData.Length <= 5 then [||]
        else
        [|for i = 3 to xData.Length-3 do
            // Peak must be concave in the interval [i-2 .. i+2] and exheat a min hight (min_dh)
            if (smoothYData.[i] < smoothYData.[i - 1] && smoothYData.[i] < smoothYData.[i + 1]  //smoothYData.[i] > yThreshold
                && smoothYData.[i - 1] <= smoothYData.[i - 2] && smoothYData.[i + 1] <= smoothYData.[i + 2]) then

                // take the intensity at the apex of the profile peak
                yield (xData.[i], smoothYData.[i])
            |]    

    /// Returns a collection of indices corresponding to local minima. Attention: The algorithm is very sensitive to noise   
    let localMinimaIdx (xData:float[]) (smoothYData:float[]) =
        if xData.Length <= 5 then [||]
        else
        [|for i = 3 to xData.Length-3 do
            // Peak must be concave in the interval [i-2 .. i+2] and exheat a min hight (min_dh)
            if (smoothYData.[i] < smoothYData.[i - 1] && smoothYData.[i] < smoothYData.[i + 1]  //smoothYData.[i] > yThreshold
                && smoothYData.[i - 1] <= smoothYData.[i - 2] && smoothYData.[i + 1] <= smoothYData.[i + 2]) then

                // take the intensity at the apex of the profile peak
                yield i
            |]    

    /// Returns Index of the highestPeak flanking a given mzValue
    let idxOfHighestPeakBy (xData: float []) (yData: float []) x = 
        let idxHigh = 
            xData |> Array.tryFindIndex (fun x -> x > x) // faster as binary search
        let idxLow = 
            match idxHigh with 
            | Option.None   -> Some (xData.Length-1) 
            | Some value -> match value with 
                            | 0 -> Option.None
                            | _ -> Some (value-1)  
        if idxLow = Option.None then 
                idxHigh.Value
        elif idxHigh = Option.None then 
                idxLow.Value
        else
            if yData.[idxLow.Value] > yData.[idxHigh.Value] then 
                    idxLow.Value
            else idxHigh.Value
                
    /// Returns Index of the highestPeak flanking a given mzValue
    let idxOfClosestPeakBy (xData: float []) (yData: float []) x = 
        if xData |> Array.isEmpty then 0
        else
        xData 
        |> Array.mapi (fun i x -> abs (x - x), i) // faster as binary search
        |> Array.minBy (fun (value,idx) -> value)
        |> fun (value,idx) -> idx

    /// Returns a collection of local Maxima and Minima. Attention: The algorithm is very sensitive to noise   
    let labelPeaks negYThreshold posYThreshold (xData:float[]) (smoothYData:float[]) =
        if xData.Length <= 5 then [||]
        else
        [|for i = 0 to xData.Length-1 do 
            if i < 3 || i > xData.Length-3 then
                yield {Meta=Extrema.None; Data= xData.[i],smoothYData.[i]}
                                    
            elif (smoothYData.[i] > posYThreshold && smoothYData.[i] > smoothYData.[i - 1] && smoothYData.[i] > smoothYData.[i + 1] 
                && smoothYData.[i - 1] >= smoothYData.[i - 2] && smoothYData.[i + 1] >= smoothYData.[i + 2]) then
                yield {Meta=Extrema.Positive; Data= xData.[i],smoothYData.[i]} //TODO: Typ is tin Peak.fs definiert, creatorFunktion verwenden

            // Peak must be concave in the interval [i-2 .. i+2] and exheat a min hight (min_dh)
            elif (smoothYData.[i] < negYThreshold && smoothYData.[i] < smoothYData.[i - 1] && smoothYData.[i] < smoothYData.[i + 1]  //smoothYData.[i] > yThreshold
                && smoothYData.[i - 1] <= smoothYData.[i - 2] && smoothYData.[i + 1] <= smoothYData.[i + 2]) then
                yield {Meta=Extrema.Negative; Data= xData.[i],smoothYData.[i]}
            else
                yield {Meta=Extrema.None; Data= xData.[i],smoothYData.[i]}
            |]    

    // Returns the index of the peak with the highest intensity
    let idxOfHighestLabeledPeakBy (labeledData: Tag<Extrema,(float*float)>[]) (labelV:Extrema)  = 
        if labeledData |> Array.isEmpty then Option.None
        else
        labeledData  
        |> Array.mapi (fun i x -> i, x) 
        |> Array.filter (fun (i,x) -> x.Meta = labelV)
        |> fun farr -> 
            if farr |> Array.isEmpty then 
                Option.None     
            else  
                Array.maxBy (fun (idx,value) -> snd value.Data) farr
                |> Option.Some 
                
    // Returns the index of the peak with the highest intensity
    let idxOfClosestLabeledPeak (labeledData: Tag<Extrema,(float*float)>[]) (labelV:Extrema) x = 
        if labeledData |> Array.isEmpty then Option.None
        else
        labeledData  
        |> Array.mapi (fun i x -> i, x) 
        |> Array.filter (fun (i,x) -> x.Meta = labelV)
        |> fun farr -> 
            if farr |> Array.isEmpty then 
                Option.None     
            else
                farr 
                |> Array.minBy (fun (idx,value) -> abs (fst value.Data - x) ) 
                |> Option.Some

    module SecondDerivative = 
             
        // Step 5: find rightLiftOff
        let closestLiftOffIdx stepSize labeledSndDevData peakIdx   = 
            iterUntil (fun (x:Tag<Extrema,(float*float)>) -> x.Meta = Extrema.Negative) stepSize (peakIdx + stepSize)  labeledSndDevData 

        // Step 4I: find leftLiftOffIdx
        let closestLeftLiftOffIdx labeledSndDevData peakIdx =
            closestLiftOffIdx (-1) labeledSndDevData peakIdx

        // Step 5: find rightLiftOff
        let closestRightLiftOffIdx labeledSndDevData peakIdx = 
            closestLiftOffIdx (+1) labeledSndDevData peakIdx

        /// Given a noisy data set, the labled negative second derivative, the index of a putative peak and the index of the peak lift of position, the function iterates
        /// in the direction given by the step parameter and returns a tuple. The first value of the tuple indicates if the peak is isolated (true indicates yes) and the second value is the 
        /// index index of the determined peak end. 
        let tryFindPeakEnd step (xData: float []) (yData: float []) (smoothedYData: float []) (labeledSndDevData: Tag<Extrema,(float*float)> []) (closestPeakIdx: int) (closestLiftOffIdx: int option) =
            printfn "clostestPeakIdx = %i" closestPeakIdx
            ///
            let signalBorderBy (step:int) =
                if Math.Sign(step) = 1 then 
                    xData.Length-1
                else 
                    0
            /// Inspects the sourrounding of the peak. The function walks in the direction given by the step parameter. The function accumulates all
            /// lift offs till till the next peak or the end of the signal trace is reached. Returns the last index, the number of lift offs and a bool
            /// indicating if a flanking peak is present.
            let rec loopF step (labeledSndDevData: Tag<Extrema,(float*float)> []) (currentIdx: int) (kLiftOffs: int) (hasFlankingPeak:bool) = 
                if currentIdx = signalBorderBy step then 
                    currentIdx, kLiftOffs, hasFlankingPeak
                else
                    match kLiftOffs with 
                    | x when x >=2 -> currentIdx, kLiftOffs, hasFlankingPeak
                    | _ -> 
                        match labeledSndDevData.[currentIdx].Meta with
                        | Extrema.Positive ->  
                            currentIdx, kLiftOffs, true
                        | Extrema.Negative ->
                            loopF step labeledSndDevData (currentIdx+step) (kLiftOffs+1) hasFlankingPeak
                        |_ -> 
                            loopF step labeledSndDevData (currentIdx+step) kLiftOffs hasFlankingPeak
            match closestLiftOffIdx with 
            | Some liftOfIdx -> 
                let (_,kLiftOffs,hasFlankingPeak) = loopF step labeledSndDevData liftOfIdx 0 false 
                // Only one Liftoff and no flanking peak indicates a isolated peak.
                if kLiftOffs = 1 && hasFlankingPeak = false then
                    true, 
                    match iterUntili  (fun i (y:float) -> y > smoothedYData.[i-step] || y > smoothedYData.[closestPeakIdx]) step (closestPeakIdx+step) smoothedYData with 
                    | Option.None   -> signalBorderBy step
                    | Option.Some x -> x            
                // Only one Liftoff indicates a convoluted peak, iterate backwards.            
                elif kLiftOffs = 1 then
                    false, 
                    match  iterUntili (fun i (y:float) -> y > smoothedYData.[i-step] || y > smoothedYData.[closestPeakIdx]) step (closestPeakIdx+step) smoothedYData with
                    | Option.None   ->  (closestPeakIdx)+1
                    | Option.Some x -> x-step
                // If more than one Liftoff between two peaks is detected, the peaks are well separated
                elif kLiftOffs > 1 then
                    true,  
                    match iterUntili  (fun i (y:float) -> y > smoothedYData.[i-step] || y > smoothedYData.[closestPeakIdx]) step (liftOfIdx+step) smoothedYData with 
                    | Option.None   -> signalBorderBy step
                    | Option.Some x -> x        
                else
                    /// No Liftoffs detected
                    false,  
                    match iterUntili (fun i (y:float) ->  y > smoothedYData.[i-step] || y > smoothedYData.[closestPeakIdx]) step (closestPeakIdx+step) smoothedYData with 
                    | Option.None   -> signalBorderBy step
                    | Option.Some x -> x
            | Option.None   ->
                    false, 
                    match iterUntili (fun i (y:float) -> y > smoothedYData.[i-step] || y > smoothedYData.[closestPeakIdx]) step  (closestPeakIdx+step) smoothedYData with
                    | Option.None   -> signalBorderBy step 
                    | Option.Some x -> x
          
        /// Given a noisy data set, the labled negative second derivative, the index of a putative peak and the index of the peak lift of position, the function iterates
        /// in the positive direction returns a tuple. The first value of the tuple indicates if the peak is isolated (true indicates yes) and the second value is the 
        /// index index of the determined peak end. 
        let findLeftBorderOf (xData: float []) (yData: float []) smoothedYData (labeledSndDevData: Tag<Extrema,(float*float)> []) (closestPeakIdx: int) (closestLiftOffIdx: int option) =
            tryFindPeakEnd (-1) xData yData smoothedYData labeledSndDevData closestPeakIdx closestLiftOffIdx

        /// Given a noisy data set, the labled negative second derivative, the index of a putative peak and the index of the peak lift of position, the function iterates
        /// in the positive direction returns a tuple. The first value of the tuple indicates if the peak is isolated (true indicates yes) and the second value is the 
        /// index index of the determined peak end. 
        let findRightBorderOf (xData: float []) (yData: float []) smoothedYData (labeledSndDevData: Tag<Extrema,(float*float)> []) (closestPeakIdx: int) (closestLiftOffIdx: int option) =
            tryFindPeakEnd (1) xData yData smoothedYData labeledSndDevData closestPeakIdx closestLiftOffIdx


        ///
        let characterizePeak (xData: float []) (yData: float []) smoothedYData (labeledSndDevData: Tag<Extrema,(float*float)> []) (peakIdx: int) = 
            let apex = Option.Some (createPeakFeature peakIdx xData.[peakIdx] yData.[peakIdx])
            let leftLiftOffIdx = closestLeftLiftOffIdx labeledSndDevData peakIdx  
            let leftLiftOff = 
                match leftLiftOffIdx with 
                | Option.Some i -> Option.Some (createPeakFeature i xData.[i] yData.[i])
                | Option.None   -> Option.None
            let convL,leftPeakEnd  = 
                let conv,leftIdx = findLeftBorderOf xData yData smoothedYData labeledSndDevData peakIdx leftLiftOffIdx
                conv, Some (createPeakFeature leftIdx  xData.[leftIdx] yData.[leftIdx])
            let rightLiftOffIdx = closestRightLiftOffIdx labeledSndDevData peakIdx  
            let rightLiftOff = 
                match rightLiftOffIdx with 
                | Option.Some i -> Option.Some (createPeakFeature i xData.[i] yData.[i])
                | Option.None   -> Option.None
            let convR,rightPeakEnd = 
                let conv,rightIdx = findRightBorderOf xData yData smoothedYData labeledSndDevData peakIdx rightLiftOffIdx
                conv, Some (createPeakFeature rightIdx xData.[rightIdx] yData.[rightIdx])
            createIdentifiedPeak 
                apex 
                leftLiftOff
                leftPeakEnd
                rightLiftOff
                rightPeakEnd
                (not convL)
                (not convR)
                xData.[leftPeakEnd.Value.Index .. rightPeakEnd.Value.Index]
                yData.[leftPeakEnd.Value.Index .. rightPeakEnd.Value.Index]


        ///
        let filterpeaks noiseLevel (yData:float[]) (labeledDataTmp: _ []) = 
            [|
            for i = 0 to yData.Length-1 do 
                if yData.[i] > noiseLevel && labeledDataTmp.[i].Meta=Extrema.Positive then 
                    yield i,labeledDataTmp.[i]
            |] 
                                                                                                                                                 

        ///    
        let getPeaks snr ws xData yData = 
            ///
            let smoothedYData = Filtering.savitzky_golay ws 3 0 1 yData |> Array.ofSeq
            ///
            let negSndDev = Filtering.savitzky_golay ws 3 2 1 yData |> Array.ofSeq |> Array.map ((*) -1.)  
            ///
            let labeledDataTmp = labelPeaks 0. 0. (xData |> Array.ofSeq) negSndDev 
            ///
            let noiseLevel = Seq.map2 (fun x y -> abs(x-y)) smoothedYData yData |> Seq.mean |> (*) snr
            /// peaks above noiselevel
            let peaks = filterpeaks noiseLevel yData labeledDataTmp // |> Array.map (fun x -> (snd x).Data)
            peaks
            //|> Array.filter (fun (i,x) -> x.Data |> fst > 14.4 && x.Data |> fst < 22. )
            |> Array.map (fun (i,_) -> characterizePeak xData yData smoothedYData labeledDataTmp i)        

