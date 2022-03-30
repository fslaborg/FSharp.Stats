namespace FSharp.Stats.Testing

/// Confusion matrix for binary classification
type BinaryConfusionMatrix<'A when 'A: equality>(
    isPositiveLabel: 'A -> bool, 
    actual: seq<'A>, 
    predictions: seq<'A>
) = 

    // use mutable variables here so we do not have to iterate through the input multiple times
    let mutable tp = 0.
    let mutable tn = 0.
    let mutable fp = 0.
    let mutable fn = 0.

    // get tp tn fp fn (and p/n as combinations) in one iteration
    let _ = 
        Seq.zip actual predictions
        |> Seq.iter (fun (truth,pred) ->
            match (isPositiveLabel truth, isPositiveLabel pred) with
            | (true,true)   -> tp <- tp + 1.; 
            | (false,true)  -> fp <- fp + 1.; 
            | (true,false)  -> fn <- fn + 1.; 
            | (false,false) -> tn <- tn + 1.; 
        )

    let p = (tp + fn)
    let n = (tn + fp)

    new (positiveLabel: 'A, actual: seq<'A>, predictions: seq<'A>) =
        BinaryConfusionMatrix(
            isPositiveLabel = (fun x -> x = positiveLabel), 
            actual = actual,
            predictions = predictions
        )

    /// a function that classifies a label as positive
    member val IsPositiveLabel = isPositiveLabel
    /// a function that classifies a label as negative
    member val IsNegativeLabel = (isPositiveLabel >> not)

    /// actual classifications (ground truth)
    member val Actual = actual

    /// predicted classifications
    member val Predictions = predictions

    /// the real positive cases in the data
    member cm.GetConditionPositives() = cm.Actual |> Seq.filter (fun x -> cm.IsPositiveLabel x)
    /// the real negative cases in the data
    member cm.GetConditionNegatives() = cm.Actual |> Seq.filter (fun x -> cm.IsNegativeLabel x)

    /// amount of condition positives (real positive) cases in the data
    member cm.P = int p
    /// amount of condition negatives (real negative) cases in the data
    member cm.N = int n
    /// total amount of comparisons
    member cm.SampleSize = cm.P + cm.N
    /// true positives: amount of labels correctly predicted as positive
    member cm.TP = int tp
    /// true negatives: amount of labels correctly predicted as negative
    member cm.TN = int tn
    /// false positives: amount of labels incorrectly predicted as positive
    member cm.FP = int fp
    /// false negatives: amount of labels incorrectly predicted as negative
    member cm.FN = int fn

    /// https://en.wikipedia.org/wiki/Confusion_matrix

    /// sensitivity, recall, hit rate, or true positive rate (TPR)
    member cm.Sensitivity = tp / p
    /// specificity, selectivity or true negative rate (TNR)
    member cm.Specificity = tn / n
    /// precision or positive predictive value (PPV)
    member cm.Precision = tp / (tp + fp)
    /// negative predictive value (NPV)
    member cm.NegativePredictiveValue = tn / (tn + fn)
    /// miss rate or false negative rate (FNR)
    member cm.Missrate = fn / p
    /// fall-out or false positive rate (FPR)
    member cm.FallOut = fp / n
    /// false discovery rate (FDR)
    member cm.FalseDiscoveryRate = fp / (fp + tp)
    /// false omission rate (FOR)
    member cm.FalseOmissionRate = fn / (fn + tn)
    /// Positive likelihood ratio (LR+)
    member cm.PositiveLikelihoodRatio = cm.Sensitivity / cm.FallOut
    /// Negative likelihood ratio (LR-)
    member cm.NegativeLikelihoodRatio = cm.Missrate / cm.Specificity
    /// prevalence threshold (PT)
    member cm.PrevalenceThreshold = (sqrt cm.FallOut)  / ((sqrt cm.Sensitivity) + (sqrt cm.FallOut))
    /// threat score (TS) or critical success index (CSI)
    member cm.ThreatScore = tp / (tp + fn + fp)

    /// Prevalence
    member cm.Prevalence = p / float cm.SampleSize
    /// accuracy (ACC)
    member cm.Accuracy = (tp + tn) / float cm.SampleSize
    /// balanced accuracy (BA)
    member cm.BalancedAccuracy = (cm.Sensitivity + cm.Specificity) / 2.
    /// F1 score (harmonic mean of precision and sensitivity)
    member cm.F1 = (2. * tp) / ((2. * tp) + fp + fn)
    /// phi coefficient (φ or rφ) or Matthews correlation coefficient (MCC)
    member cm.PhiCoefficient = ((tp*tn) - (fp * fn)) / (sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)))
    /// Fowlkes–Mallows index (FM)
    member cm.FowlkesMallowsIndex = sqrt(cm.Precision * cm.Sensitivity)
    /// informedness or bookmaker informedness (BM)
    member cm.Informedness = cm.Sensitivity + cm.Specificity - 1.
    /// markedness (MK) or deltaP (Δp)
    member cm.Markedness = cm.Precision + cm.NegativePredictiveValue - 1.
    /// Diagnostic odds ratio (DOR)
    member cm.DiagnosticOddsRatio = cm.PositiveLikelihoodRatio / cm.NegativeLikelihoodRatio