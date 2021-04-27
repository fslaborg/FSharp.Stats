namespace FSharp.Stats.Optimization.HyperParameterTuning

open FSharp.Stats

/// Functions for finding the optimal hyper parameters using grid search
module GridSearch = 

    /// Returns equidistant values for a given hyper parameter 
    let createGridForSingleParam (gridLineNumber : int) (hyperParam : HyperParameter) =
        match hyperParam with
        | Category categories -> categories |> List.map HyperParameterValue.String
        | IntBetween interval ->        
            let min,max = 
                if Intervals.getStart interval <= Intervals.getEnd interval then
                    Intervals.values interval
                else
                    Intervals.getEnd interval, Intervals.getStart interval
            if max - min <= gridLineNumber then 
                [min .. max] 
            else
                List.init gridLineNumber (fun i ->
                    (max - min) * (i + 1) / (gridLineNumber + 1) 
                )
            |> List.map HyperParameterValue.Int
        | FloatBetween interval ->
            let min,max = 
                if Intervals.getStart interval <= Intervals.getEnd interval then
                    Intervals.values interval
                else
                    Intervals.getEnd interval, Intervals.getStart interval
            List.init gridLineNumber (fun i ->
                (max - min) * (i + 1 |> float) / (gridLineNumber + 1 |> float) 
            )
            |> List.map HyperParameterValue.Float

    /// Returns equidistant values for a given set of hyper parameters
    let createSearchGrid (gridLineNumber : int) (hyperParams : HyperParameter list) : HyperParameterValue list list =
        let rec collectGridPoints (parameters : HyperParameter list) (combination : HyperParameterValue list) (combinations : HyperParameterValue list list) = 
            match parameters with
            | param :: parameters ->
                createGridForSingleParam gridLineNumber param
                |> List.map (fun value ->
                    collectGridPoints parameters (value :: combination) combinations                          
                )
                |> List.concat
            | [] -> (List.rev combination) :: combinations

        collectGridPoints hyperParams [] []

    /// Perform a grid search, returning all model performances
    let gridSearch (gridLineNumber : int) (scoringFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (hyperParams : HyperParameter list) : HyperParameterTuningResult<'T> list =
        createSearchGrid gridLineNumber hyperParams
        |> List.map (fun hps -> 
            let metaInfo,score = scoringFunction data hps
            HyperParameterTuningResult<'T>.create hps score metaInfo
        )

    /// Perform a grid search, returning the hyper parameters for which the model performance was maximized
    let gridSearchMaximize (gridLineNumber : int) (scoringFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (hyperParams : HyperParameter list) : HyperParameterTuningResult<'T> =
        gridSearch gridLineNumber scoringFunction data hyperParams
        |> List.maxBy (fun result -> result.Score)

    /// Perform a grid search, returning the hyper parameters for which the model performance was minimized
    let gridSearchMinimize (gridLineNumber : int) (scoringFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (hyperParams : HyperParameter list) : HyperParameterTuningResult<'T>  =
        gridSearch gridLineNumber scoringFunction data hyperParams
        |> List.minBy (fun result -> result.Score)
