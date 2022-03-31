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
        // use mutable variables here so we do not have to iterate through the input multiple times
        let mutable tp = 0.
        let mutable tn = 0.
        let mutable fp = 0.
        let mutable fn = 0.

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

    static member oneVsRest (mlcm: MultiLabelConfusionMatrix) =

        mlcm.Labels
        |> Array.mapi (fun labelIndex label ->
            let tp = mlcm.Confusion[labelIndex,labelIndex]
            let tn = mlcm.Confusion |> Matrix.Generic.foldi (fun rI cI acc elem -> if rI = labelIndex || cI = labelIndex then acc else acc + elem) 0
            let fp = mlcm.Confusion.Column(labelIndex) |> Seq.foldi (fun rI acc elem -> if rI = labelIndex then acc else acc + elem) 0
            let fn = mlcm.Confusion.Row(labelIndex) |> Seq.foldi (fun cI acc elem -> if cI = labelIndex then acc else acc + elem) 0
            label, BinaryConfusionMatrix.create(tp, tn, fp, fn)
        )
