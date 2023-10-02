namespace FSharp.Stats

module ConfidenceInterval =

    open FSharp.Stats.Distributions

    /// <summary>The deviation from the sample mean is calculated by using a t distribution. Common ciLevel = 0.95</summary>
    /// <remarks></remarks>
    /// <param name="ciLevel"></param>
    /// <param name="sample"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let ciDeviation ciLevel (sample : seq<float>)=
        let n = float (Seq.length sample)
        let stDev = Seq.stDev sample
        let t = Testing.TTest.getCriticalTValue (float n - 1.) (1. - ciLevel) Testing.TTest.TwoTailed
        let delta = t * stDev / sqrt (float n)
        delta

    /// <summary>The confidence interval is calculated by using a t distribution. Common ciLevel = 0.95</summary>
    /// <remarks></remarks>
    /// <param name="ciLevel"></param>
    /// <param name="sample"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let ci ciLevel (sample : seq<float>)=
        let mean  = Seq.mean  sample
        let delta = ciDeviation ciLevel sample
        Interval.CreateClosed<float> ((mean - delta),(mean + delta))
