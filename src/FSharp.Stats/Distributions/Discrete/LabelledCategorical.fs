namespace FSharp.Stats.Distributions.Discrete

open FSharp.Stats
open FSharp.Stats.Distributions

/// <summary>
/// A wrapper around the Categorical distribution that associates each probability with a custom label of type <c>'Label</c>.
/// </summary>
/// <typeparam name="'Label">The type used to label the categories (e.g., string, union, enum).</typeparam>
type LabelledCategorical<'Label when 'Label: comparison> private 
    (
        labelToIndex: Map<'Label, int>, 
        indexToLabel: 'Label[], 
        probs: float[]
    ) =

    let dist = Categorical.Init probs

    /// <summary>
    ///   Initializes a new instance of the LabelledCategorical distribution using label-probability pairs.
    /// </summary>
    /// <param name="labels">An array of labels for each category (must be unique).</param>
    /// <param name="probs">An array of probabilities for each label (must sum to 1).</param>
    new (labels: 'Label[], probs: float[]) =
        if labels.Length <> probs.Length then
            failwith "Label and probability array must be the same length."
        let labelToIndex = labels |> Array.mapi (fun i l -> l, i) |> Map.ofArray
        LabelledCategorical(labelToIndex, labels, probs)

    /// <summary>
    /// Gets the array of category labels.
    /// </summary>
    member _.Labels : 'Label[] = indexToLabel

    /// <summary>
    /// Gets the array of probabilities associated with the labels.
    /// </summary>
    member _.Probabilities : float[] = probs

    /// <summary>
    /// Returns the probability of a given label.
    /// </summary>
    /// <param name="label">The label to query.</param>
    /// <returns>The probability associated with the label, or 0.0 if the label is not found.</returns>
    member _.PMF(label: 'Label) =
        match Map.tryFind label labelToIndex with
        | Some i -> dist.PMF i
        | None -> 0.0

    /// <summary>
    /// Returns the cumulative probability up to and including the given label.
    /// </summary>
    /// <param name="label">The label to evaluate the cumulative probability up to.</param>
    /// <returns>The cumulative probability.</returns>
    member _.CDF(label: 'Label) =
        match Map.tryFind label labelToIndex with
        | Some i -> probs |> Array.take (i + 1) |> Array.sum
        | None -> 0.0

    /// <summary>
    /// Returns the array of labels (support of the distribution).
    /// </summary>
    member _.Support = indexToLabel

    /// <summary>
    /// Draws a random sample from the distribution and returns the sampled label.
    /// </summary>
    /// <returns>A label corresponding to a sampled category.</returns>
    member _.Sample() =
        let i = dist.Sample()
        indexToLabel[i]

    /// <summary>
    /// Returns the label closest to the expected (mean) index of the distribution.
    /// </summary>
    /// <returns>The label whose index is closest to the mean.</returns>
    member _.MeanLabel() =
        let mi = dist.Mean |> round |> int
        indexToLabel[mi]

    /// <summary>
    /// Fits a LabelledCategorical distribution from a set of labeled observations.
    /// </summary>
    /// <param name="observations">An array of observed labels.</param>
    /// <returns>A new LabelledCategorical distribution with probabilities estimated from observed frequencies.</returns>
    static member Fit (observations: 'Label[]) =
        observations
        |> Array.countBy id
        |> Array.sortBy fst
        |> fun counts ->
            let total = float observations.Length
            let labelArray = counts |> Array.map fst
            let probs = counts |> Array.map (fun (_, c) -> float c / total)
            LabelledCategorical(labelArray, probs)

    /// <summary>
    /// Estimates a distribution from labels and counts (frequencies).
    /// </summary>
    /// <param name="labels">An array of labels.</param>
    /// <param name="counts">An array of counts corresponding to each label.</param>
    /// <returns>A new LabelledCategorical distribution.</returns>
    static member Estimate (labels: 'Label[]) (counts: int[]) =
        if labels.Length <> counts.Length then
            failwith "Labels and counts must be the same length."
        let total = float (Array.sum counts)
        let probs = counts |> Array.map (fun c -> float c / total)
        LabelledCategorical(labels, probs)


    /// <summary>
    /// Initializes a LabelledCategorical distribution from labels and a cumulative unnormalized weight vector.
    /// </summary>
    /// <param name="labels">Labels corresponding to cumulative weights.</param>
    /// <param name="cdf">Unnormalized cumulative weights (must be increasing).</param>
    /// <returns>A normalized LabelledCategorical distribution.</returns>
    static member FromCdfUnnormalized(labels: 'Label[], cdf: float[]) =
        if labels.Length <> cdf.Length then
            failwith "Labels and cumulative weights must be the same length."
        let deltas = 
            Array.init cdf.Length (fun i ->
                if i = 0 then cdf.[0]
                else cdf.[i] - cdf.[i - 1])
        let total = Array.sum deltas
        let probs = deltas |> Array.map (fun x -> x / total)
        LabelledCategorical(labels, probs)
