module SignalTests 


open Expecto
open System
open FSharp.Stats
open FSharp.Stats.Signal
open FSharp.Stats.Signal.Padding.Discrete
open Signal.Outliers
open TestExtensions

[<Tests>]
let outlierTests =
    let ls = [-1.4; -1.4; -1.3; -7.9; 9.4; -1.5; 5.0; 7.0; 1.1; 1.6]
    let m = mean ls //1.06
    
    let dataRow =
        [
            [20.;  11.];
            [22.;  29.];
            [12.;  27.];
            [13.;  15.];
            [19.;  23.];
            [28.;  18.];
            [16.;  30.];
            [25.;  24.];
            [14.;  21.];
            [17.;  26.]
        ]
        |> matrix

    let dataColumn = 
        [
            [20.;22.;12.;13.;19.;28.;16.;25.;14.;17.];
            [11.;29.;27.;15.;23.;18.;30.;24.;21.;26.]
        ]
        |> matrix


    let compareIntervals a b (str:string) =
        Expect.floatClose Accuracy.high (Interval.getStart a) (Interval.getStart b) str
        Expect.floatClose Accuracy.high (Interval.getEnd a) (Interval.getEnd b) str
    
    testList "Signal.OutlierTests" [
        testList "Z-Score" [
            testCase "Z-Score in a population" <| fun() ->
                let s = stDevPopulation(ls) //4.745144887
                Expect.floatClose Accuracy.high (zScore -1.4 m s) -0.5184246337 "Z-Score in a population was calculated incorrectly"

            testCase "Z-Score in a sample" <| fun()->
                let sSample = stDev(ls)
                Expect.floatClose Accuracy.high (zScore -1.4 m sSample) -0.4918207913 "Z-Score in a sample was calculated incorrectly"
                
            testCase "Z-Scores of a population" <| fun()->
                let zLs = [-0.5184246337; -0.5184246337; -0.4973504616; -1.88824582; 1.757585953; -0.5394988058; 0.8303223808; 1.251805823; 0.008429668841; 0.1138005294]
                TestExtensions.sequenceEqual Accuracy.high (zScoresOfPopulation ls) zLs "Z-Score of a population was calculated incorrectly"
                
            testCase "Z-Scores of a sample" <| fun()->
                let zLsSample = [-0.4918207913; -0.4918207913; -0.4718280762; -1.791347272; 1.667392439; -0.5118135064; 0.7877129747; 1.187567277; 0.007997086037; 0.1079606615]
                TestExtensions.sequenceEqual Accuracy.high (zScoresOfSample ls) zLsSample "Z-Score of a sample was calculated incorrectly"
                
            testCase "Population interval by Z-Score" <| fun()->
                let populationInterval = Interval.CreateClosed (-0.3635434661,3.432572444)
                compareIntervals (populationIntervalByZScore -0.3 0.5 ls) populationInterval "Z-Score interval in a population was calculated incorrectly"

            testCase "Sample interval by Z-Score" <| fun()->
                let sampleInterval = Interval.CreateClosed (-0.4405465671,3.560910945)
                compareIntervals (sampleIntervalByZscore -0.3 0.5 ls) sampleInterval "Z-Score interval in a sample was calculated incorrectly"
        ]

        testList "Mahalanobi's Distance" [
            testCase "Mahalanobi's Distance for an observation in a matrix"<| fun() ->
                let obs = Vector.ofList [20.; 11.] 
                Expect.floatClose Accuracy.high (mahalanobisDistanceOfEntry dataRow    Matrix.Sample     Matrix.RowWise obs) 1.843936618 "Mahalanobi's Distance for an observation(Sample, RowWise) calculated incorrectly"
                Expect.floatClose Accuracy.high (mahalanobisDistanceOfEntry dataColumn Matrix.Sample     Matrix.ColWise obs) 1.843936618 "Mahalanobi's Distance for an observation calculated(Sample, ColWise) incorrectly"
                Expect.floatClose Accuracy.high (mahalanobisDistanceOfEntry dataRow    Matrix.Population Matrix.RowWise obs) 1.943679857 "Mahalanobi's Distance for an observation calculated(Population, RowWise) incorrectly"
                Expect.floatClose Accuracy.high (mahalanobisDistanceOfEntry dataColumn Matrix.Population Matrix.ColWise obs) 1.943679857 "Mahalanobi's Distance for an observation calculated(Population, ColWise) incorrectly"

            testCase "Mahalanobi's Distance for every observation in a matrix"<| fun() ->
                let mahalDistancesSample = [1.843936618; 1.315823162; 1.395764847; 1.698572419; 0.1305760401; 1.862248734; 1.280527036; 1.28097611; 0.934074348; 0.6301069471]
                let mahalDistancesPopulation = [1.943679857; 1.386999396; 1.471265332; 1.790452538; 0.1376392315; 1.962982523; 1.349794013; 1.350267379; 0.9846008145; 0.6641910408]
                TestExtensions.sequenceEqual Accuracy.high (mahalanobisDistances Matrix.Sample       Matrix.RowWise dataRow   ) mahalDistancesSample  "Mahalanobi's Distance for every observation in a matrix(Sample, RowWise) was calculated incorrectly"
                TestExtensions.sequenceEqual Accuracy.high (mahalanobisDistances Matrix.Population   Matrix.RowWise dataRow   ) mahalDistancesPopulation  "Mahalanobi's Distance for every observation in a matrix(Population, RowWise) was calculated incorrectly"
                TestExtensions.sequenceEqual Accuracy.high (mahalanobisDistances Matrix.Sample       Matrix.ColWise dataColumn) mahalDistancesSample "Mahalanobi's Distance for every observation in a matrix(Sample, ColWise) was calculated incorrectly"
                TestExtensions.sequenceEqual Accuracy.high (mahalanobisDistances Matrix.Population   Matrix.ColWise dataColumn) mahalDistancesPopulation  "Mahalanobi's Distance for every observation in a matrix(Population, ColWise) was calculated incorrectly"

        ]
    ]

[<Tests>]
let normalizationTests =
    
    let table = 
        [
            [4.5;4.2;3.6]
            [4.3;4.2;0.5]
            [2.5;4.1;0.6]
        ]
        |> matrix

    let tableB = 
        [|
        [|100.; 130.; 30.|]
        [| 80.; 200.; 30.|]
        [|  0.;  50.;  0.|]
        [| 40.;  50.; 20.|]
        [| 50.;  45.; 25.|]
        [| 40.;  50.; 15.|]
        |]
        |> matrix

    let tableWithNan = 
        [
            [4.5;nan;3.6]
            [4.3;4.2;nan]
            [2.5;4.1;0.6]
        ]
        |> matrix

    testList "Signal.NormalizationTests" [
        testCase "MedianOfRatios" <| fun() ->

            let expectedNormalizedTable = 
                [
                    [3.29784;2.08239;10.99283]
                    [3.15127;2.08239;1.52678]
                    [1.83213;2.03281;1.83213]
                    
                ]
                |> matrix

            let result = Normalization.medianOfRatios table

            TestExtensions.sequenceEqual 4 result.NormedData expectedNormalizedTable "Matrix was not normalized correctly"

        testCase "MedianOfRatiosIgnoreNans" <| fun() ->
           
            let result = Normalization.medianOfRatiosBy (fun x -> if System.Double.IsNaN x then 0.1 else x) tableWithNan

            Expect.hasCountOf result.NormedData 2u System.Double.IsNaN "Only initial nan values should be nans afterwards"

        testCase "MedianOfRatioWides" <| fun() ->
        
            let result = Normalization.medianOfRatiosWide table
            let expected = 
                table
                |> Matrix.transpose
                |> Normalization.medianOfRatios
                |> fun x -> x.NormedData
                |> Matrix.transpose
            TestExtensions.sequenceEqual 4 result.NormedData expected "Wide method should return the same result as the non wide method on a transposed matrix"

        testCase "quantile" <| fun() ->

            let expectedNormalizedTable = 
                [
                    [110. ; 80.  ; 80.  ]
                    [80.  ; 110. ; 110. ]
                    [15.  ; 35.  ; 15.  ]
                    [35.  ; 36.6666666667 ; 36.6666666667 ]
                    [41.6666666667 ; 15   ; 41.6666666667 ]
                    [36.6666666667 ; 41.6666666667 ; 35.  ]
                ]
                |> matrix

            let result = Normalization.quantile tableB

            TestExtensions.sequenceEqual 4 result expectedNormalizedTable "Matrix was not normalized correctly"
    ]

    
[<Tests>]
let binningTests =
    
    let testData =
        [
        "AT5G40650", 0.6142592186244475
        "AT5G36950", 0.02961887351477155
        "AT4G35320", 0.5711371856687455
        "AT1G52030", 0.13714132092557502
        "AT1G25480", 0.1777802253955505
        "AT1G13608", 0.1835805021082776
        "AT5G36950", 0.02961887351477155 //duplicate
        "AT5G06120", 0.5109225016759817
        "AT5G49150", 0.597941654040864
        "AT4G36770", 0.6812994122019935
        "AT5G10780", 0.003410975374229297
        ]

    let testData1 =
        [
        0.05;
        0.1;
        0.2;
        0.2;
        0.3;
        0.3;
        0.3;
        0.3;
        0.4;
        3.0;
        3.0;
        4.0;
        6.0;
        ]

    testList "Signal.BinningTests" [
        testCase "binBy" <| fun() ->

            let expected = 
                [|
                0.05, ["AT5G36950", 0.02961887351477155;"AT5G36950", 0.02961887351477155;"AT5G10780", 0.003410975374229297]
                0.15, ["AT1G52030", 0.13714132092557502; "AT1G25480", 0.1777802253955505;"AT1G13608", 0.1835805021082776]
                0.55, ["AT4G35320", 0.5711371856687455; "AT5G06120", 0.5109225016759817;  "AT5G49150", 0.597941654040864]
                0.65, ["AT5G40650", 0.6142592186244475;"AT4G36770", 0.6812994122019935]
                |]

            let expectedBins = expected |> Array.map fst
            let expectedIds  = expected |> Array.map (snd >> List.map fst)
            let expectedVals = expected |> Array.map (snd >> List.map snd)

            let actual = 
                Signal.Binning.binBy snd 0.1 testData  
                |> Map.map (fun a b -> List.ofSeq b)
                |> Map.toArray

            let actualBins = actual |> Array.map fst
            let actualIds  = actual |> Array.map (snd >> List.map fst)
            let actualVals = actual |> Array.map (snd >> List.map snd)
                
            TestExtensions.sequenceEqual 10 actualBins expectedBins "Binning was not performed correctly"

            expectedVals 
            |> Array.iteri (fun i e -> 
                TestExtensions.sequenceEqual 10 actualVals.[i] e "Binning was not performed correctly"
                )
                
            Expect.equal actualIds expectedIds "Binning was not performed correctly"

        testCase "zeroBindwith" <| fun() ->

            let zeroBandwidth() = 
                Signal.Binning.binBy snd 0.0 testData |> ignore

            Expect.throwsT<(System.DivideByZeroException) > zeroBandwidth "Binning was not performed correctly"

        testCase "bin0.1" <| fun() ->
            
            let actual = 
                Signal.Binning.bin 0.1 testData1
                |> Map.map (fun a b -> List.ofSeq b)
                |> Map.toArray

            let actualBins = actual |> Array.map fst
            let actualIds  = actual |> Array.map snd
            let actualVals = actual |> Array.map snd

            let expected = 
                [|
                0.05, [0.05]
                0.15, [0.1]
                0.25, [0.2;0.2]
                0.35, [0.3;0.3;0.3;0.3;]
                0.45, [0.4]
                3.05, [3.;3.]
                4.05, [4.]
                6.05, [6.]
                |]

            let expectedBins = expected |> Array.map fst
            let expectedIds  = expected |> Array.map snd
            let expectedVals = expected |> Array.map snd

            TestExtensions.sequenceEqual 10 actualBins expectedBins "Binning was not performed correctly"

            expectedVals 
            |> Array.iteri (fun i e -> 
                TestExtensions.sequenceEqual 10 actualVals.[i] e "Binning was not performed correctly"
                )
                
            Expect.equal actualIds expectedIds "Binning was not performed correctly"

        testCase "bin1.0" <| fun() ->
            
            let actual = 
                Signal.Binning.bin 1. testData1
                |> Map.map (fun a b -> List.ofSeq b)
                |> Map.toArray

            let actualBins = actual |> Array.map fst
            let actualIds  = actual |> Array.map snd
            let actualVals = actual |> Array.map snd

            let expected = 
                [|
                0.5, [0.05;0.1;0.2;0.2;0.3;0.3;0.3;0.3;0.4]
                3.5, [3.;3.]
                4.5, [4.]
                6.5, [6.]
                |]

            let expectedBins = expected |> Array.map fst
            let expectedIds  = expected |> Array.map snd
            let expectedVals = expected |> Array.map snd

            TestExtensions.sequenceEqual 10 actualBins expectedBins "Binning was not performed correctly"

            expectedVals 
            |> Array.iteri (fun i e -> 
                TestExtensions.sequenceEqual 10 actualVals.[i] e "Binning was not performed correctly"
                )
                
            Expect.equal actualIds expectedIds "Binning was not performed correctly"
    ]


[<Tests>]
let paddingTests =
    let rnd = System.Random()
    let dataLength = 20
    let padding = 10

    let data =
        Array.init dataLength (
            fun i ->
                (3.0 + float i, 7.0 - float i)
        )
    
    let randomTwoDimensionalArray dimension1Length dimension2Length  =
        Array2D.init dimension1Length dimension2Length (fun _ _ -> rnd.NextDouble())
    
    let randomArray length =
        Array.init length (fun _ -> rnd.NextDouble())
             
    testList "Signal.PaddingTests" [

        testCase "pad" <| fun() ->

            let expectLeadIn  = Array.init padding (fun i -> (3.0 - float (padding-i), 0.0))
            let expectLeadOut = Array.init padding (fun i -> (3.0 + float (dataLength + i), 0.0))
            let expectedPadded = Array.concat [expectLeadIn; data; expectLeadOut]

            let padded = Padding.pad data 1.0 Double.PositiveInfinity (-) (+) padding Padding.BorderPaddingMethod.Zero Padding.InternalPaddingMethod.NaN Padding.HugeGapPaddingMethod.NaN

            Expect.equal (Array.sub padded 0 padding) expectLeadIn "padding is incorrect" 
            Expect.equal (Array.sub padded (padded.Length - padding) padding) expectLeadOut "padding is incorrect"
            Expect.equal (Array.sub padded padding data.Length) data "All the original data should be contained in the padded data"
            Expect.equal padded.Length (data.Length + 2 * padding) "Length should be the original data length plus padding at each end"
            Expect.equal (padded |> Array.sortBy fst) expectedPadded "Result should be the lead-in, whole data, then lead-out (maybe not in order?)"
            Expect.equal padded expectedPadded "Result should be the lead-in, whole data, then lead-out"
            
        testCase "three dimensional pad with zeroes" <| fun() ->
            let originalDimension1 = 30
            let originalDimension2 = 40
            let originalData = randomTwoDimensionalArray originalDimension1 originalDimension2
            
            let newHeight = (originalDimension1 + 2 * padding)
            let newWidth = (originalDimension2 + 2 * padding)
            let isPointInOriginalData i j =
                (i >= padding && i < originalDimension1 + padding) &&
                (j >= padding && j < originalDimension2 + padding)
            
            let expected =
                        Array2D.init newHeight newWidth (fun i j -> 
                        if isPointInOriginalData i j 
                            then originalData[i-padding, j-padding]
                        else 0.)
            
            let paddedData2D = ThreeDimensional.pad originalData padding ThreeDimensional.Zero
            
            Expect.equal paddedData2D expected "padded data is incorrect" 
        
        
        testCase "three dimensional pad with random padding" <| fun() ->
            let originalHeight = 30
            let originalWidth = 40
            
            let originalData = randomTwoDimensionalArray originalHeight originalWidth
                        
            let newHeight = (originalHeight + 2 * padding)
            let newWidth = (originalWidth + 2 * padding)
            let flattenToArray (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toArray
            
            let paddedData2D = ThreeDimensional.pad originalData padding ThreeDimensional.Random
            
            Expect.equal paddedData2D.Length (newHeight * newWidth) "padded data length incorrect"
            // All the padded values should belong to the original data set
            Expect.containsAll (originalData |> flattenToArray) (paddedData2D |> flattenToArray) "padded data contains item not in original data"
        
        
        testCase "padZero to discrete data" <| fun() ->
            let originalData = randomArray dataLength
            let newLength = (dataLength + 2 * padding)
            let isPointInOriginalData i =
                (i >= padding && i < dataLength + padding)
                
            let expected = Array.init newLength (fun i -> if isPointInOriginalData i 
                                                          then originalData[i-padding]
                                                          else 0.)
            
            let paddedData = padZero originalData padding
            
            Expect.equal paddedData expected "padded data incorrect"
        
        testCase "padRnd to discrete data" <| fun() ->
            let originalData = randomArray dataLength
            let newLength = (dataLength + 2 * padding)
                
            let paddedData = padRnd originalData padding
            
            Expect.equal paddedData.Length newLength "padded data length incorrect"
            // All the padded values should belong to the original data set
            Expect.containsAll originalData paddedData "padded data contains item not in original data"
        ]
    