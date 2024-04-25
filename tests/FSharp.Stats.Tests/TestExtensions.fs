module TestExtensions

    open Expecto
    open FSharp.Stats
    open FSharp.Stats.Testing
    open System
    open System.IO
    open System.Text
    open System.Reflection

    type TestExtensions() =
        static member sequenceEqual(digits: int) =
            let round (v:float) = System.Math.Round(v,digits)
            fun actual expected message -> 
                Expect.sequenceEqual (actual |> Seq.map round) (expected |> Seq.map round) message
            
        static member sequenceEqual(accuracy: Expecto.Accuracy) =
            fun actual expected message -> 
                if Seq.length actual <> Seq.length expected then Expect.isTrue false message
                Seq.iter2 (fun a b -> Expect.floatClose accuracy a b message) actual expected
            
        static member sequenceEqualRoundedNaN (digits: int) =
            let round (v:float) = System.Math.Round(v,digits)
            fun actual expected message -> 
                if Seq.length actual <> Seq.length expected then Expect.isTrue false message
                Seq.iter2 (fun a b -> 
                    if nan.Equals a then 
                        Expect.isTrue (nan.Equals b) message
                    else 
                        Expect.equal (round a) (round b) message
                    ) 
                    actual 
                    expected

    let assembly = Assembly.GetExecutingAssembly()
    let resnames = assembly.GetManifestResourceNames();
    let readEmbeddedRessource (name:string) = 
        match Array.tryFind (fun (r:string) -> r.Contains(name)) resnames with
        | Some path -> 
            use stream = assembly.GetManifestResourceStream(path)
            use reader = new StreamReader(stream, encoding=Text.Encoding.UTF8)
            reader.ReadToEnd()

        | _ -> failwithf "could not embedded ressources, check package integrity"

    let readCsv path =
        readEmbeddedRessource path
        |> fun s -> 
            s.Replace("\r\n","\n").Split("\n")
        |> Array.skip 1
        |> Array.map (fun x -> 
            match x.Split(", ") with
            | [|a;b|] -> a, float b
            | _ -> failwith "invalid csv format"
         )

 
    let comparisonMetricsEqualRounded (digits : int) (actual: ComparisonMetrics) (expected: ComparisonMetrics) message =
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
        TestExtensions.sequenceEqualRoundedNaN digits actual expected message