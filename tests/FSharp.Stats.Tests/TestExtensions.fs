module TestExtensions

open Expecto
open FSharp.Stats
open FSharp.Stats.Testing
/// Expects the `actual` sequence to equal the `expected` one after rounding the floats in both to the given digit count.
let sequenceEqualRounded (digits : int) actual expected message =
    let round (v:float) = System.Math.Round(v,digits)
    Expect.sequenceEqual (actual |> Seq.map round) (expected |> Seq.map round) message

/// Expects the `actual` sequence to equal the `expected` one after rounding the floats in both to the given digit count (nans are checked)
let sequenceEqualRoundedNaN (digits : int) actual expected message =
    let round (v:float) = System.Math.Round(v,digits)
    Seq.iter2 (fun a b -> 
        if nan.Equals a then 
            Expect.isTrue (nan.Equals b) message
        else 
            Expect.equal (round a) (round b) message
        ) 
        actual 
        expected

let comparisonMetricsEqualRounded (digits : int) (actual: ComparisonMetrics) (expected: ComparisonMetrics) message =
    let round (v:float) = System.Math.Round(v,digits)
    let actual = 
        [
            actual.P                       
            actual.N                       
            actual.SampleSize              
            actual.TP                      
            actual.TN                      
            actual.FP                      
            actual.FN                      
            actual.Sensitivity             
            actual.Specificity             
            actual.Precision               
            actual.NegativePredictiveValue 
            actual.Missrate                
            actual.FallOut                 
            actual.FalseDiscoveryRate      
            actual.FalseOmissionRate       
            actual.PositiveLikelihoodRatio 
            actual.NegativeLikelihoodRatio 
            actual.PrevalenceThreshold     
            actual.ThreatScore             
            actual.Prevalence              
            actual.Accuracy                
            actual.BalancedAccuracy        
            actual.F1                      
            actual.PhiCoefficient          
            actual.FowlkesMallowsIndex     
            actual.Informedness            
            actual.Markedness              
            actual.DiagnosticOddsRatio     
        ]
        
    let expected = 
        [
            expected.P                       
            expected.N                       
            expected.SampleSize              
            expected.TP                      
            expected.TN                      
            expected.FP                      
            expected.FN                      
            expected.Sensitivity             
            expected.Specificity             
            expected.Precision               
            expected.NegativePredictiveValue 
            expected.Missrate                
            expected.FallOut                 
            expected.FalseDiscoveryRate      
            expected.FalseOmissionRate       
            expected.PositiveLikelihoodRatio 
            expected.NegativeLikelihoodRatio 
            expected.PrevalenceThreshold     
            expected.ThreatScore             
            expected.Prevalence              
            expected.Accuracy                
            expected.BalancedAccuracy        
            expected.F1                      
            expected.PhiCoefficient          
            expected.FowlkesMallowsIndex     
            expected.Informedness            
            expected.Markedness              
            expected.DiagnosticOddsRatio     
        ]
    sequenceEqualRoundedNaN digits actual expected message