namespace FSharp.Stats.Testing
open FSharp.Stats

/// Confusion matrix for binary classification
type BinaryConfusionMatrix = {
    /// true positives: amount of labels correctly predicted as positive
    TP: int
    /// true negatives: amount of labels correctly predicted as negative
    TN: int
    /// false positives: amount of labels incorrectly predicted as positive
    FP: int
    /// false negatives: amount of labels incorrectly predicted as negative
    FN: int
} with
    static member create(tp, tn, fp, fn) =
        {
            TP = tp
            TN = tn
            FP = fp
            FN = fn
        }
    static member ofPredictions(
        positiveLabel: 'A,
        actual: seq<'A>,
        predictions: seq<'A>
    ) =

        let isPositiveLabel x = x = positiveLabel

        // get tp tn fp fn (and p/n as combinations) in one iteration. Using mutable accumulators might be more performant than re-creating new objects
        Seq.zip actual predictions
        |> Seq.fold (fun cm (truth,pred) ->
            match (isPositiveLabel truth, isPositiveLabel pred) with
            | (true,true)   -> {cm with TP = cm.TP + 1}
            | (false,false) -> {cm with TN = cm.TN + 1}
            | (false,true)  -> {cm with FP = cm.FP + 1}
            | (true,false)  -> {cm with FN = cm.FN + 1}
        ) (BinaryConfusionMatrix.create(0,0,0,0))

    static member ofPredictions(
        actual: seq<bool>,
        predictions: seq<bool>
    ) = BinaryConfusionMatrix.ofPredictions(true,actual,predictions)


    static member thresholdMap(
        actual: seq<bool>,
        predictions: seq<float>,
        thresholds:seq<float>
    ) =
    
        // get values that are outside of the prediction range for max threshold
        // for this threshold, the predictor can not report any positives.
        let largest = (Seq.max predictions) + 1.
        
        let distinctThresholds =
            thresholds
            |> fun x -> seq{largest; yield! x}

        distinctThresholds
        |> Array.ofSeq
        |> Array.map (fun thr ->
            let thresholded = predictions |> Seq.map (fun x -> x >= thr)
            thr, BinaryConfusionMatrix.ofPredictions(actual,thresholded)
        )

    static member thresholdMap(
        actual: seq<bool>,
        predictions: seq<float>
    ) =
    
        let zippedSorted =
            Seq.zip 
                actual
                predictions
            |> Array.ofSeq
            |> Array.sortByDescending snd

        // get values that are outside of the prediction range for max threshold
        // for this threshold, the predictor can not report any positives.
        let largest = snd (Array.head zippedSorted) + 1.
        
        let distinctThresholds =
            zippedSorted
            |> Array.distinctBy snd
            |> Array.map snd
            |> fun x -> [|largest; yield! x|]

        distinctThresholds
        |> Array.map (fun thr ->
            let thresholded = predictions |> Seq.map (fun x -> x >= thr)
            thr, BinaryConfusionMatrix.ofPredictions(actual,thresholded)
        )

open System
open FSharpAux

type MultiLabelConfusionMatrix = {
    Labels: string []
    Confusion: Matrix<int>
} with
    static member create(labels: string [], confusion:Matrix<int>) =
        if (labels.Length <> confusion.NumCols) || (labels.Length <> confusion.NumRows) then failwith "The confusion matrix must be square and the labels must contain as many values as the confusion dimensions."
        {
            Labels = labels
            Confusion = confusion
        }

    static member ofPredictions(
        labels: #IConvertible [],
        actual: #IConvertible [],
        predictions: #IConvertible []
    ) = 
        let zipped = Array.zip actual predictions

        let confusion =
            Matrix.Generic.init labels.Length labels.Length (fun m n ->
                let labelM = labels[m]
                let labelN = labels[n]
                zipped
                |> Array.filter (fun (truth,pred) ->
                    truth = labelM && pred = labelN
                )
                |> Array.length
            )

        MultiLabelConfusionMatrix.create(labels |> Array.map string, confusion)
    
    static member oneVsRest (label: string) (mlcm: MultiLabelConfusionMatrix) =

        let labelIndex = Array.findIndex (fun x -> x = label) mlcm.Labels

        mlcm.Confusion 
        |> Matrix.Generic.foldi (
            fun rI cI cm elem -> 
                match (rI, cI) with
                | (rI, cI) when rI = labelIndex && cI = labelIndex -> {cm with TP = cm.TP + elem}
                | (rI, cI) when rI <> labelIndex && cI = labelIndex -> {cm with FP = cm.FP + elem}
                | (rI, cI) when rI = labelIndex && cI <> labelIndex -> {cm with FN = cm.FN + elem}
                | _ ->   {cm with TN = cm.TN + elem}

        ) (BinaryConfusionMatrix.create(0,0,0,0))

    static member allVsAll (mlcm: MultiLabelConfusionMatrix) =
        mlcm.Labels |> Array.map (fun label -> label, MultiLabelConfusionMatrix.oneVsRest label mlcm)
