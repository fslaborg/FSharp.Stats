namespace FSharp.Stats.HyperParameterTuning

open FSharp.Stats

/// Specificies the possible values a Hyper Parameter can take
type HyperParameter =
    | Category of string list
    | IntBetween of Intervals.Interval<int>
    | FloatBetween of Intervals.Interval<float>

    static member Create (categories : string list) =
        HyperParameter.Category categories

    static member Create (lower,upper) =
        HyperParameter.IntBetween (Intervals.Interval.ClosedInterval (lower,upper))

    static member Create (lower,upper) =
        HyperParameter.FloatBetween (Intervals.Interval.ClosedInterval (lower,upper))

/// Specificies a specific value of a Hyper Parameter
type HyperParameterValue =
    | String    of string
    | Int       of int
    | Float     of float

    static member TryGetInt (hpValue : HyperParameterValue) =
        match hpValue with
        | Int i -> Some i
        | _ -> None 

    static member TryGetFloat (hpValue : HyperParameterValue) =
        match hpValue with
        | Float f -> Some f
        | _ -> None

    static member TryGetString (hpValue : HyperParameterValue) =
        match hpValue with
        | String s -> Some s
        | _ -> None

    static member GetInt (hpValue : HyperParameterValue) =
        match hpValue with
        | Int i -> i
        | _ -> failwith "HyperParameterValue is not of type int"      

    static member GetFloat (hpValue : HyperParameterValue) =
        match hpValue with
        | Float f -> f
        | _ -> failwith "HyperParameterValue is not of type float"

    static member GetAsFloat (hpValue : HyperParameterValue) =
        match hpValue with
        | Float f -> f
        | Int i   -> float i
        | _ -> failwith "HyperParameterValue is not of type float"

    static member GetString (hpValue : HyperParameterValue) =
        match hpValue with
        | String s -> s
        | _ -> failwith "HyperParameterValue is not of type string"

    static member ToVector (hpValues : HyperParameterValue list) =
        hpValues
        |> List.map HyperParameterValue.GetAsFloat
        |> vector

    static member OfVector (vec : vector) =
        vec
        |> Seq.map HyperParameterValue.Float
        |> List.ofSeq

/// Contains the performance score a ML model achieved for a given set of hyper parameter values
type HyperParameterTuningResult<'T> =
    {
        Parameters  : HyperParameterValue list
        Score       : float
        MetaInfo    : 'T 
    }

    static member create parameters score metaInfo =
        {
            Parameters  = parameters
            Score       = score
            MetaInfo    = metaInfo
        }

/// A function for scoring the ML model performance for a given set of hyper parameter values on a dataset
type HPScoringFunction<'Data,'T> = 'Data -> HyperParameterValue list -> 'T * float