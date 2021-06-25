namespace FSharp.Stats.HyperParameterTuning

open FSharp.Stats

/// Functions for finding the optimal hyper parameters using random search
module RandomSearch = 

    /// Create a random hyper parameter value for a given hyper parameter
    let getRandomHPValue (rnd : System.Random) (hyperParameter : HyperParameter) : HyperParameterValue =
        match hyperParameter with
        | Category categories -> 
            categories.[rnd.Next(0,categories.Length)]
            |> HyperParameterValue.String
        | FloatBetween interval ->
            let min,max = Intervals.valuesOrdered interval
            rnd.NextDouble() * (max - min) + min
            |> HyperParameterValue.Float
        | IntBetween interval ->
            let min,max = Intervals.valuesOrdered interval
            rnd.Next(min,max)
            |> HyperParameterValue.Int

    /// Returns n randomly distributed value sets for a given set of hyper parameters
    let createRandomHPValues (n : int) (hyperParams : HyperParameter list) : HyperParameterValue list list =
        let rnd = System.Random()
        List.init n (fun _ -> 
            hyperParams |> List.map (getRandomHPValue rnd)
        )

    /// Perform a random search on n random hyper parameter value sets, returning all model performances
    let randomSearch (n : int) (scoringFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (hyperParams : HyperParameter list) : HyperParameterTuningResult<'T> list =
        createRandomHPValues n hyperParams
        |> List.map (fun hps -> 
            let metaInfo,score = scoringFunction data hps
            HyperParameterTuningResult<'T>.create hps score metaInfo
        )

    /// Perform a random search on n random hyper parameter value sets, returning the hyper parameters for which the model performance was maximized
    let randomSearchMaximize (n : int) (scoringFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (hyperParams : HyperParameter list) : HyperParameterTuningResult<'T> =
        randomSearch n scoringFunction data hyperParams
        |> List.maxBy (fun result -> result.Score)

    /// Perform a random search on n random hyper parameter value sets, returning the hyper parameters for which the model performance was minimized
    let randomSearchMinimize (n : int) (scoringFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (hyperParams : HyperParameter list) : HyperParameterTuningResult<'T>  =
        randomSearch n scoringFunction data hyperParams
        |> List.minBy (fun result -> result.Score)
