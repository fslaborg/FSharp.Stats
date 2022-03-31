namespace FSharp.Stats.Testing

type ComparisonMetrics(bcm: BinaryConfusionMatrix) = 

    let tp, tn, fp, fn, p, n = float bcm.TP, float bcm.TN, float bcm.FP, float bcm.FN, float (bcm.TP + bcm.FN), float (bcm.TN + bcm.FP)

    member cm.TP = bcm.TP
    member cm.TN = bcm.TN
    member cm.FP = bcm.FP
    member cm.FN = bcm.FN
    member cm.P = (cm.TP + cm.FN)
    member cm.N = (cm.TN + cm.FP)
    member cm.SampleSize = cm.P + cm.N

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