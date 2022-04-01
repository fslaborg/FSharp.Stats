namespace FSharp.Stats.Testing

/// Comparison metrics that can be derived from a binary confusion matrix
// https://en.wikipedia.org/wiki/Confusion_matrix
type ComparisonMetrics = {
    /// Condition-positives: all positive labels in the sample
    P: float
    /// Condition-negatives: all negative labels in the sample
    N: float
    /// all observations in the sample
    SampleSize: float
    /// true positives: correctly predicted condition-positive labels
    TP: float
    /// true negatives: correctly predicted condition-negative labels
    TN: float
    /// false positives: condition-negative labels incorrectly predicted as positives
    FP: float
    /// false negatives: condition-positive labels incorrectly predicted as negatives
    FN: float
    /// sensitivity, recall, hit rate, or true positive rate (TPR)
    Sensitivity: float
    /// specificity, selectivity or true negative rate (TNR)
    Specificity: float
    /// precision or positive predictive value (PPV)
    Precision: float
    /// negative predictive value (NPV)
    NegativePredictiveValue: float
    /// miss rate or false negative rate (FNR)
    Missrate: float
    /// fall-out or false positive rate (FPR)
    FallOut: float
    /// false discovery rate (FDR)
    FalseDiscoveryRate: float
    /// false omission rate (FOR)
    FalseOmissionRate: float
    /// Positive likelihood ratio (LR+)
    PositiveLikelihoodRatio: float
    /// Negative likelihood ratio (LR-)
    NegativeLikelihoodRatio: float
    /// prevalence threshold (PT)
    PrevalenceThreshold: float
    /// threat score (TS) or critical success index (CSI)
    ThreatScore: float
    /// Prevalence
    Prevalence: float
    /// accuracy (ACC)
    Accuracy: float
    /// balanced accuracy (BA)
    BalancedAccuracy: float
    /// F1 score (harmonic mean of precision and sensitivity)
    F1: float
    /// phi coefficient (φ or rφ) or Matthews correlation coefficient (MCC)
    PhiCoefficient: float
    /// Fowlkes–Mallows index (FM)
    FowlkesMallowsIndex: float
    /// informedness or bookmaker informedness (BM)
    Informedness: float
    /// markedness (MK) or deltaP (Δp)
    Markedness: float
    /// Diagnostic odds ratio (DOR)
    DiagnosticOddsRatio: float
} with
    static member create(
        p: float,
        n: float,
        samplesize: float,
        tp: float,
        tn: float,
        fp: float,
        fn: float,
        sensitivity: float,
        specificity: float,
        precision: float,
        negativepredictivevalue: float,
        missrate: float,
        fallout: float,
        falsediscoveryrate: float,
        falseomissionrate: float,
        positivelikelihoodratio: float,
        negativelikelihoodratio: float,
        prevalencethreshold: float,
        threatscore: float,
        prevalence: float,
        accuracy: float,
        balancedaccuracy: float,
        f1: float,
        phicoefficient: float,
        fowlkesmallowsindex: float,
        informedness: float,
        markedness: float,
        diagnosticoddsratio: float
    ) = 
        {
            P                       = p
            N                       = n
            SampleSize              = samplesize
            TP                      = tp 
            TN                      = tn 
            FP                      = fp 
            FN                      = fn 
            Sensitivity             = sensitivity            
            Specificity             = specificity            
            Precision               = precision              
            NegativePredictiveValue = negativepredictivevalue
            Missrate                = missrate               
            FallOut                 = fallout                
            FalseDiscoveryRate      = falsediscoveryrate     
            FalseOmissionRate       = falseomissionrate      
            PositiveLikelihoodRatio = positivelikelihoodratio
            NegativeLikelihoodRatio = negativelikelihoodratio
            PrevalenceThreshold     = prevalencethreshold    
            ThreatScore             = threatscore            
            Prevalence              = prevalence             
            Accuracy                = accuracy               
            BalancedAccuracy        = balancedaccuracy       
            F1                      = f1                     
            PhiCoefficient          = phicoefficient         
            FowlkesMallowsIndex     = fowlkesmallowsindex    
            Informedness            = informedness           
            Markedness              = markedness             
            DiagnosticOddsRatio     = diagnosticoddsratio    

        }

    static member create (tp: float, tn: float, fp: float, fn: float) =
        let p = tp + fn
        let n = tn + fp
        let samplesize = p + n

        let sensitivity = tp / p
        let specificity = tn / n
        let precision = tp / (tp + fp)
        let negativepredictivevalue = tn / (tn + fn)
        let missrate = fn / p
        let fallout = fp / n
        let falsediscoveryrate = fp / (fp + tp)
        let falseomissionrate = fn / (fn + tn)
        let positivelikelihoodratio = sensitivity / fallout
        let negativelikelihoodratio = missrate / specificity
        let prevalencethreshold = (sqrt fallout)  / ((sqrt sensitivity) + (sqrt fallout))
        let threatscore = tp / (tp + fn + fp)
        let prevalence = p / float samplesize
        let accuracy = (tp + tn) / float samplesize
        let balancedaccuracy = (sensitivity + specificity) / 2.
        let f1 = (2. * tp) / ((2. * tp) + fp + fn)
        let phicoefficient = ((tp*tn) - (fp * fn)) / (sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)))
        let fowlkesmallowsindex = sqrt(precision * sensitivity)
        let informedness = sensitivity + specificity - 1.
        let markedness = precision + negativepredictivevalue - 1.
        let diagnosticoddsratio = positivelikelihoodratio / negativelikelihoodratio

        ComparisonMetrics.create(
            p,
            n,
            samplesize,
            tp,
            tn,
            fp,
            fn,
            sensitivity,
            specificity,
            precision,
            negativepredictivevalue,
            missrate,
            fallout,
            falsediscoveryrate,
            falseomissionrate,
            positivelikelihoodratio,
            negativelikelihoodratio,
            prevalencethreshold,
            threatscore,
            prevalence,
            accuracy,
            balancedaccuracy,
            f1,
            phicoefficient,
            fowlkesmallowsindex,
            informedness,
            markedness,
            diagnosticoddsratio
        )

    static member create (bcm: BinaryConfusionMatrix) = ComparisonMetrics.create(bcm.TP, bcm.TN, bcm.FP, bcm.FN)

    /// calculates comparison metrics from multiple binary confusion matrices as micro-averages (all TP/TN/FP/FN are aggregated before calculating metrics)
    static member microAverage (cms: seq<BinaryConfusionMatrix>) = 
        ComparisonMetrics.create(
            cms
            |> Seq.reduce (fun acc bcm ->
                { acc with
                    TP = acc.TP + bcm.TP
                    TN = acc.TN + bcm.TN
                    FP = acc.FP + bcm.FP
                    FN = acc.FN + bcm.FN
                }
            )
        )

    /// calculates comparison metrics from the given MultiLabelConfusionMatrix as micro-averages (one-vs-rest binary confusion matrices (TP/TN/FP/FN) are calculated for each label and then aggregated before calculating metrics)
    static member microAverage (mlcm: MultiLabelConfusionMatrix) = 
        MultiLabelConfusionMatrix.allVsAll mlcm
        |> Array.map snd 
        |> ComparisonMetrics.microAverage

    /// calculates comparison metrics as macro average of the given sequence of comparison metrics (all metrics are calculated as the average of the respective metrics)
    static member macroAverage (metrics: seq<ComparisonMetrics>) = 

        let len = float (Seq.length metrics)

        metrics
        |> Seq.reduce (fun acc cm ->
            { acc with 
                P                      = acc.P                       + cm.P                      
                N                      = acc.N                       + cm.N                      
                SampleSize             = acc.SampleSize              + cm.SampleSize             
                TP                     = acc.TP                      + cm.TP                     
                TN                     = acc.TN                      + cm.TN                     
                FP                     = acc.FP                      + cm.FP                     
                FN                     = acc.FN                      + cm.FN                     
                Sensitivity            = acc.Sensitivity             + cm.Sensitivity            
                Specificity            = acc.Specificity             + cm.Specificity            
                Precision              = acc.Precision               + cm.Precision              
                NegativePredictiveValue= acc.NegativePredictiveValue + cm.NegativePredictiveValue
                Missrate               = acc.Missrate                + cm.Missrate               
                FallOut                = acc.FallOut                 + cm.FallOut                
                FalseDiscoveryRate     = acc.FalseDiscoveryRate      + cm.FalseDiscoveryRate     
                FalseOmissionRate      = acc.FalseOmissionRate       + cm.FalseOmissionRate      
                PositiveLikelihoodRatio= acc.PositiveLikelihoodRatio + cm.PositiveLikelihoodRatio
                NegativeLikelihoodRatio= acc.NegativeLikelihoodRatio + cm.NegativeLikelihoodRatio
                PrevalenceThreshold    = acc.PrevalenceThreshold     + cm.PrevalenceThreshold    
                ThreatScore            = acc.ThreatScore             + cm.ThreatScore            
                Prevalence             = acc.Prevalence              + cm.Prevalence             
                Accuracy               = acc.Accuracy                + cm.Accuracy               
                BalancedAccuracy       = acc.BalancedAccuracy        + cm.BalancedAccuracy       
                F1                     = acc.F1                      + cm.F1                     
                PhiCoefficient         = acc.PhiCoefficient          + cm.PhiCoefficient         
                FowlkesMallowsIndex    = acc.FowlkesMallowsIndex     + cm.FowlkesMallowsIndex    
                Informedness           = acc.Informedness            + cm.Informedness           
                Markedness             = acc.Markedness              + cm.Markedness             
                DiagnosticOddsRatio    = acc.DiagnosticOddsRatio     + cm.DiagnosticOddsRatio    
                
            }
        )
        |> fun sumCM ->
            { sumCM with 
                P                      = sumCM.P                       / len
                N                      = sumCM.N                       / len
                SampleSize             = sumCM.SampleSize              / len
                TP                     = sumCM.TP                      / len
                TN                     = sumCM.TN                      / len
                FP                     = sumCM.FP                      / len
                FN                     = sumCM.FN                      / len
                Sensitivity            = sumCM.Sensitivity             / len
                Specificity            = sumCM.Specificity             / len
                Precision              = sumCM.Precision               / len
                NegativePredictiveValue= sumCM.NegativePredictiveValue / len
                Missrate               = sumCM.Missrate                / len
                FallOut                = sumCM.FallOut                 / len
                FalseDiscoveryRate     = sumCM.FalseDiscoveryRate      / len
                FalseOmissionRate      = sumCM.FalseOmissionRate       / len
                PositiveLikelihoodRatio= sumCM.PositiveLikelihoodRatio / len
                NegativeLikelihoodRatio= sumCM.NegativeLikelihoodRatio / len
                PrevalenceThreshold    = sumCM.PrevalenceThreshold     / len
                ThreatScore            = sumCM.ThreatScore             / len
                Prevalence             = sumCM.Prevalence              / len
                Accuracy               = sumCM.Accuracy                / len
                BalancedAccuracy       = sumCM.BalancedAccuracy        / len
                F1                     = sumCM.F1                      / len
                PhiCoefficient         = sumCM.PhiCoefficient          / len
                FowlkesMallowsIndex    = sumCM.FowlkesMallowsIndex     / len
                Informedness           = sumCM.Informedness            / len
                Markedness             = sumCM.Markedness              / len
                DiagnosticOddsRatio    = sumCM.DiagnosticOddsRatio     / len
            
            }

    static member macroAverage (mlcm: MultiLabelConfusionMatrix) = 
        MultiLabelConfusionMatrix.allVsAll mlcm
        |> Array.map (snd >> ComparisonMetrics.create)
        |> ComparisonMetrics.macroAverage