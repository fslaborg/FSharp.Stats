module DistributionsDiscreteTests

open Expecto
open System
open FSharp.Stats
open FSharp.Stats.Distributions


// Defining an accuracy appropriate for testing random sampling and inference
let fittingAccuracy : Accuracy = {absolute= 0.1 ;relative= 0.1}



[<Tests>]
let hypergeometricTests =   

    let hypergeoDistribution_basicCase = Distributions.Discrete.Hypergeometric.Init 50 40 5
    let hypergeoDistribution_K_equal_n = Distributions.Discrete.Hypergeometric.Init  50 20 20
    let hypergeoDistribution_max_K = Distributions.Discrete.Hypergeometric.Init  50 50 20
    let hypergeoDistribution_max_n = Distributions.Discrete.Hypergeometric.Init  50 20 50
    let hypergeoDistribution_max_K_n = Distributions.Discrete.Hypergeometric.Init  50 50 50
    // 2022-06-23
    // https://hypergeon.wikipedia.org/wiki/Hypergeometric_distribution
    // N is population size,
    // K is the number of success states in the population,
    // n is the number of draws,
    // k is the number of observed successes
    // N ∈ {0,1,2,...}
    // K ∈ {0,1,2,...,N}
    // n ∈ {0,1,2,...,N}
    testList "Distributions.Discrete.Hypergeometric" [
        test "hypergeoCheckParam" {
            // Low N edge cases are difficult to test separately, as K and n MUST be smaller than N, but MUST also be bigger than 0  
            let N_isZero = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 0 1 1
            let N_isNegative = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam -2 1 1
            let N_isPositive = Distributions.Discrete.Hypergeometric.CheckParam 2 1 1
            //
            let K_isZero = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 2 0 1
            let K_isNegative = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 2 -2 1
            let K_positiveBiggerN = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 2 3 1
            let K_positiveEqualN = Distributions.Discrete.Hypergeometric.CheckParam 2 2 1
            let K_positiveSmallerN = Distributions.Discrete.Hypergeometric.CheckParam 2 1 1
            //
            let n_isZero = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 2 1 0
            let n_isNegative = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 2 1 -2
            let n_positiveBiggerN = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam 2 1 3
            let n_positiveEqualN = Distributions.Discrete.Hypergeometric.CheckParam 2 1 2
            let n_positiveSmallerN = Distributions.Discrete.Hypergeometric.CheckParam 2 1 1
            Expect.throws N_isZero "N_isZero"
            Expect.throws N_isNegative "N_isNegative"
            Expect.equal N_isPositive () "N_isPositive"
            //
            Expect.throws K_isZero "K_isZero"
            Expect.throws K_isNegative "K_isNegative"
            Expect.throws K_positiveBiggerN "K_positiveBiggerN"
            Expect.equal K_positiveEqualN () "K_positiveEqualN"
            Expect.equal K_positiveSmallerN () "K_positiveSmallerN"
            //
            Expect.throws n_isZero "n_isZero"
            Expect.throws n_isNegative "n_isNegative"
            Expect.throws n_positiveBiggerN "n_positiveBiggerN"
            Expect.equal n_positiveEqualN () "n_positiveEqualN"
            Expect.equal n_positiveSmallerN () "n_positiveSmallerN"
        }
        test "hypergeoCheckParam_k" {
            let k_isNegative = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam_k 4 2 2 -2
            let k_isPositive = Distributions.Discrete.Hypergeometric.CheckParam_k 4 2 2 1
            let k_isPositive_allEqual = Distributions.Discrete.Hypergeometric.CheckParam_k 4 2 2 2
            let k_isPositiveBiggerN = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam_k 4 2 2 5
            let k_isPositiveBiggerK = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam_k 4 2 3 3
            let k_isPositiveBigger_n = fun (x:unit) -> Distributions.Discrete.Hypergeometric.CheckParam_k 4 3 2 3
            Expect.throws k_isNegative "k_isNegative"
            Expect.equal k_isPositive () "k_isPositive; should not throw."
            Expect.equal k_isPositive_allEqual () "k_isPositive_allEqual; should not throw."
            Expect.throws k_isPositiveBiggerN "k_isPositiveBiggerN"
            Expect.throws k_isPositiveBiggerK "k_isPositiveBiggerK"
            Expect.throws k_isPositiveBigger_n "k_isPositiveBigger_n"
        }
        // 2022-06-23
        // https://www.emathhelp.net/calculators/probability-statistics/hypergeometric-distribution-calculator/?pn=50&pk=40&sn=5&sk=5
        test "Mean" {
            Expect.floatClose Accuracy.high hypergeoDistribution_basicCase.Mean 4.0 "hyperDistribution_basicCase"
            Expect.floatClose Accuracy.high hypergeoDistribution_K_equal_n.Mean 8.0 "hyperDistribution_K_equal_n"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_K.Mean 20.0 "hyperDistribution_max_K"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_n.Mean 20.0 "hyperDistribution_max_n"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_K_n.Mean 50.0 "hyperDistribution_max_K_n"
        }
        // 2022-06-23
        // https://www.emathhelp.net/calculators/probability-statistics/hypergeometric-distribution-calculator/?pn=50&pk=40&sn=5&sk=5
        test "Variance" {
            Expect.floatClose Accuracy.high hypergeoDistribution_basicCase.Variance 0.73469387755102 "hyperDistribution_basicCase"
            Expect.floatClose Accuracy.high hypergeoDistribution_K_equal_n.Variance 2.938775510204082 "hyperDistribution_K_equal_n"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_K.Variance 0.0 "hyperDistribution_max_K"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_n.Variance 0.0 "hyperDistribution_max_n"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_K_n.Variance 0.0 "hyperDistribution_max_K_n"
        }
        // 2022-06-23
        // https://www.emathhelp.net/calculators/probability-statistics/hypergeometric-distribution-calculator/?pn=50&pk=40&sn=5&sk=5
        test "StandardDeviation" {
            Expect.floatClose Accuracy.high hypergeoDistribution_basicCase.StandardDeviation 0.857142857142857 "hyperDistribution_basicCase"
            Expect.floatClose Accuracy.high hypergeoDistribution_K_equal_n.StandardDeviation 1.714285714285714 "hyperDistribution_K_equal_n"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_K.StandardDeviation 0.0 "hyperDistribution_max_K"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_n.StandardDeviation 0.0 "hyperDistribution_max_n"
            Expect.floatClose Accuracy.high hypergeoDistribution_max_K_n.StandardDeviation 0.0 "hyperDistribution_max_K_n"
        }

        // 2022-06-23
        // https://www.omnicalculator.com/statistics/hypergeometric-distribution
        test "PMF" {
            // test k = 0; Accuracy.medium, because online calculator has not enough decimal places.
            Expect.floatClose Accuracy.medium (hypergeoDistribution_basicCase.PMF 0) 0.00011894 "hyperDistribution_basicCase k=0"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_K_equal_n.PMF 0) 0.0000006375 "hyperDistribution_K_equal_n k=0"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K.PMF 0) 0. "hyperDistribution_max_K k=0"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_n.PMF 0) 0. "hyperDistribution_max_n k=0"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K_n.PMF 0) 0. "hyperDistribution_max_K_n k=0"
            // test any k 
            Expect.floatClose Accuracy.medium (hypergeoDistribution_basicCase.PMF 3) 0.20984 "hyperDistribution_basicCase k=3"
            // Accuracy.low, because online calculator has not enough decimal places.
            Expect.floatClose Accuracy.low (hypergeoDistribution_K_equal_n.PMF 6) 0.1196 "hyperDistribution_K_equal_n k=6"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K.PMF 10) 0. "hyperDistribution_max_K k=10"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_n.PMF 13) 0. "hyperDistribution_max_n k=44"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K_n.PMF 50) 1.0 "hyperDistribution_max_K_n k=50"
        }
        // 2022-06-23
        // https://www.omnicalculator.com/statistics/hypergeometric-distribution
        test "CDF" {
            Expect.floatClose Accuracy.medium (hypergeoDistribution_basicCase.CDF 3)0.2581 "hyperDistribution_basicCase k=3"
            // Accuracy.low, because online calculator has not enough decimal places.
            Expect.floatClose Accuracy.low (hypergeoDistribution_K_equal_n.CDF 7) 0.3858 "hyperDistribution_K_equal_n k=7"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K.CDF 14) 0.0 "hyperDistribution_max_K k=14"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_n.CDF 3) 0.0 "hyperDistribution_max_n k=3"
            Expect.floatClose Accuracy.medium (hypergeoDistribution_max_K_n.CDF 3) 0.0 "hyperDistribution_max_K_n k=3"
        }
        //// No idea what this is meant for, but its Syntax differs from Bernoulli.Support
        //test "Support" {
        //    /// 40 20 5 do not matter as long as they don't fail "hypergeoCheckParam"
        //    let s = Distributions.Discrete.Hypergeometric.Support 40 20 5
        //    Expect.equal s (0., infinity) ""
        //}
        test "SampleUnchecked" {
            let generateALL = Distributions.Discrete.Hypergeometric.Sample 40 20 40
            let generate50 = Array.init 50 (fun x -> Distributions.Discrete.Hypergeometric.Sample 40 20 10)
            let numbersAreBetween_1_K = generate50 |> Array.forall (fun x -> x >= 0 && x < 20)
            // If N = n then k = K
            Expect.equal generateALL 20 "generateALL"
            Expect.isTrue numbersAreBetween_1_K "numbersAreBetween_1_K"
            }
        ]


[<Tests>]
let poissonDistributionTests =
     
    let lambda = 4.2
    
    let d      = Discrete.Poisson.Init lambda

    let mean   = d.Mean      
    let var    = d.Variance  
    let cdf1   = d.CDF 2 // 0.21023798702309743
    let cdf2   = d.CDF 4 // 0.589827021310577643
    let cdf3   = d.CDF 7 // 0.936056660272578944
    let pmf1   = d.PMF 4 // 0.19442365170822165
    let pmf2   = d.PMF 5 // 0.1633158674349062
    let pmf3   = d.PMF 6 // 0.11432110720443435

    
    testList "Distributions.Discrete.Poisson" [

        testCase "Mean" <| fun () ->
            Expect.floatClose Accuracy.high mean lambda "Mean should be equal"

        testCase "Variance" <| fun () ->
            Expect.floatClose Accuracy.high var lambda "Variance should be equal"
                
        testCase "Cdf1" <| fun () ->
            Expect.floatClose Accuracy.high cdf1 0.21023798702309743 "Cdf should be equal"
                
        testCase "Cdf2" <| fun () ->
            Expect.floatClose Accuracy.high cdf2 0.589827021310577643 "Cdf should be equal"
                
        testCase "Cdf3" <| fun () ->
            Expect.floatClose Accuracy.high cdf3 0.93605666027257894 "Cdf should be equal"

        testCase "Pmf1" <| fun () ->
            Expect.floatClose Accuracy.high pmf1 0.19442365170822165 "Pdf should be equal"

        testCase "Pmf2" <| fun () ->
            Expect.floatClose Accuracy.high pmf2 0.1633158674349062 "Pdf should be equal"

        testCase "Pmf3" <| fun () ->
            Expect.floatClose Accuracy.high pmf3 0.11432110720443435 "Pdf should be equal"
        
        
        testCase "FitTest<30" <| fun () ->
            let lambda = 11.5
            let observations = Array.init 9999 (fun _ -> float (Discrete.Poisson.Sample lambda))
            let lambda' = Discrete.Poisson.Fit observations
            
            Expect.floatClose fittingAccuracy lambda lambda' 
                "Poisson Distribution Fit lambda < 30 (knuth) " 
        
        testCase "FitTest>30" <| fun () ->
            let lambda = 125.5
            let observations = Array.init 9999 (fun _ -> float (Discrete.Poisson.Sample lambda))
            let lambda' = Discrete.Poisson.Fit observations
            
            Expect.floatClose fittingAccuracy lambda lambda' 
                "Poisson Distribution Fit lambda > 30 (pma)" 
    ]



[<Tests>]
let negBinomDistribution_failuresTests =
     
    let negb01 = Distributions.Discrete.NegativeBinomial_failures.Init 3 0.09
    let negb02 = Distributions.Discrete.NegativeBinomial_failures.Init 1 0.1
    let negb03 = Distributions.Discrete.NegativeBinomial_failures.Init 10 0.1 
    let negb04 = Distributions.Discrete.NegativeBinomial_failures.Init 10 0.0 
    let negb05 = Distributions.Discrete.NegativeBinomial_failures.Init 1 0.1
    let negb06 = Distributions.Discrete.NegativeBinomial_failures.Init 6 0.1

    testList "Distributions.Discrete.NegBinom_failures" [
        
        //tested against Mathnet/scipy.stats.nbinom.Pmf/r dnbinom
        testCase "PMF" <| fun () ->
            let pmf1 = 0.01873636711 
            let pmf2 = 0.03486784401 
            let pmf3 = 9e-10         
            let pmf4 = 0.0           
            let pmf5 = 0.1           
            let pmf6 = 0.0181098507  
            
            Expect.floatClose Accuracy.high (negb01.PMF 10) pmf1 "PMF should be equal"
            Expect.floatClose Accuracy.high (negb02.PMF 10) pmf2 "PMF should be equal"
            Expect.floatClose Accuracy.high (negb03.PMF  1) pmf3 "PMF should be equal"
            Expect.floatClose Accuracy.high (negb04.PMF  2) pmf4 "PMF should be equal"
            Expect.floatClose Accuracy.high (negb05.PMF  0) pmf5 "PMF should be equal"
            Expect.floatClose Accuracy.high (negb06.PMF 49) pmf6 "PMF should be equal"
            
        //tested against Mathnet/scipy.stats.nbinom.Cdf/r pnbinom
        testCase "CDF" <| fun () ->
            let cdf1 = 0.1053608621
            let cdf2 = 0.6861894039
            let cdf3 = 9.999999717e-10
            let cdf4 = 0.
            let cdf5 = 0.1
            let cdf6 = 0.4755642039
            
            Expect.floatClose Accuracy.high (negb01.CDF 10) cdf1 "CDF should be equal"
            Expect.floatClose Accuracy.high (negb02.CDF 10) cdf2 "CDF should be equal"
            Expect.floatClose Accuracy.high (negb03.CDF  1) cdf3 "CDF should be equal"
            Expect.floatClose Accuracy.high (negb04.CDF  2) cdf4 "CDF should be equal"
            Expect.floatClose Accuracy.high (negb05.CDF  0) cdf5 "CDF should be equal"
            Expect.floatClose Accuracy.high (negb06.CDF 49) cdf6 "CDF should be equal"
                

        //tested against Mathnet and https://homepage.divms.uiowa.edu/~mbognar/applets/nb1.html
        testCase "Mode" <| fun () ->
            let mode1 = 20         
            let mode2 = 0          
            let mode3 = 80         
            let mode6 = 45         
            
            let mode4() = negb04.Mode |> ignore
            Expect.floatClose Accuracy.high (negb01.Mode) mode1 "Mode should be equal"
            Expect.floatClose Accuracy.high (negb02.Mode) mode2 "Mode should be equal"
            Expect.floatClose Accuracy.high (negb03.Mode) mode3 "Mode should be equal"
            Expect.throws mode4 "Mode cannot be determined"
            Expect.floatClose Accuracy.high (negb06.Mode) mode6 "Mode should be equal"
                
        //tested against Mathnet and https://homepage.divms.uiowa.edu/~mbognar/applets/nb1.html
        testCase "Mean" <| fun () ->
            let mean1 = 30.33333333
            let mean2 = 9.0        
            let mean3 = 90.0       
            let mean4 = nan   
            let mean6 = 54.0       
            
            Expect.floatClose Accuracy.high (negb01.Mean) mean1 "Mean should be equal"
            Expect.floatClose Accuracy.high (negb02.Mean) mean2 "Mean should be equal"
            Expect.floatClose Accuracy.high (negb03.Mean) mean3 "Mean should be equal"
            Expect.isTrue (nan.Equals(negb04.Mean)) "Mean should be equal"
            Expect.floatClose Accuracy.high (negb06.Mean) mean6 "Mean should be equal"
        
        //tested against Mathnet and https://homepage.divms.uiowa.edu/~mbognar/applets/nb1.html
        testCase "Variance" <| fun () ->
            let var1 = 337.037037
            let var2 = 90.0      
            let var3 = 900.0     
            let var4 = nan  
            let var6 = 540       
            
            Expect.floatClose Accuracy.high (negb01.Variance) var1 "Variance should be equal"
            Expect.floatClose Accuracy.high (negb02.Variance) var2 "Variance should be equal"
            Expect.floatClose Accuracy.high (negb03.Variance) var3 "Variance should be equal"
            Expect.isTrue (nan.Equals(negb04.Variance)) "Variance should be equal"
            Expect.floatClose Accuracy.high (negb06.Variance) var6 "Variance should be equal"
                
        //tested against Mathnet and https://homepage.divms.uiowa.edu/~mbognar/applets/nb1.html
        testCase "StandardDeviation" <| fun () ->
            let stdev = sqrt 337.037037
            Expect.floatClose Accuracy.high negb01.StandardDeviation stdev "Standard deviation should be equal"
    ]


[<Tests>]
let negBinomDistribution_trialsTests =
     
    let negb01 = Distributions.Discrete.NegativeBinomial_trials.Init 3 0.09
    let negb02 = Distributions.Discrete.NegativeBinomial_trials.Init 1 0.1
    let negb03 = Distributions.Discrete.NegativeBinomial_trials.Init 10 0.1 
    let negb04 = Distributions.Discrete.NegativeBinomial_trials.Init 10 0.0 
    let negb05 = Distributions.Discrete.NegativeBinomial_trials.Init 1 0.1
    let negb06 = Distributions.Discrete.NegativeBinomial_trials.Init 6 0.1

    testList "Distributions.Discrete.NegBinom_trials" [
        
        //tested against Mathnet/scipy.stats.nbinom.Pmf/r dnbinom
        //tested against scipy.nbinom.pmf (10, 3,  0.09, loc=3)
        testCase "PMF" <| fun () ->
            let pmf1 = 0.01873636711 
            let pmf2 = 0.03486784401 
            let pmf3 = 9e-10         
            let pmf4 = 0.           
            let pmf5 = 0.1           
            let pmf6 = 0.0181098507  
            
            Expect.floatClose Accuracy.high (negb01.PMF (10 + 3)) pmf1 "PMF should be equal"
            Expect.floatClose Accuracy.high (negb02.PMF (10 + 1)) pmf2 "PMF should be equal"
            Expect.floatClose Accuracy.high (negb03.PMF ( 1 + 10)) pmf3 "PMF should be equal"
            Expect.floatClose Accuracy.high (negb04.PMF ( 2 + 1)) pmf4 "PMF should be equal"
            Expect.floatClose Accuracy.high (negb05.PMF ( 0 + 1)) pmf5 "PMF should be equal"
            Expect.floatClose Accuracy.high (negb06.PMF (49 + 6)) pmf6 "PMF should be equal"
            
        //tested against Mathnet/scipy.stats.nbinom.Cdf/r pnbinom
        testCase "CDF" <| fun () ->
            let cdf1 = 0.1053608621
            let cdf2 = 0.6861894039
            let cdf3 = 9.999999717e-10
            let cdf4 = 0.
            let cdf5 = 0.1
            let cdf6 = 0.4755642039
            
            Expect.floatClose Accuracy.high (negb01.CDF (10. + 3.)) cdf1 "CDF should be equal"
            Expect.floatClose Accuracy.high (negb02.CDF (10. + 1.)) cdf2 "CDF should be equal"
            Expect.floatClose Accuracy.high (negb03.CDF ( 1. + 10.)) cdf3 "CDF should be equal"
            Expect.floatClose Accuracy.high (negb04.CDF ( 2. + 10.)) cdf4 "CDF should be equal"
            Expect.floatClose Accuracy.high (negb05.CDF ( 0. + 1.)) cdf5 "CDF should be equal"
            Expect.floatClose Accuracy.high (negb06.CDF (49. + 6.)) cdf6 "CDF should be equal"
                

        //tested against Mathnet and https://homepage.divms.uiowa.edu/~mbognar/applets/nb1.html
        testCase "Mode" <| fun () ->
            let mode1 = 23         
            let mode2 = 1          
            let mode3 = 90         
            let mode6 = 51         
            
            let mode4() = negb04.Mode |> ignore
            Expect.floatClose Accuracy.high (negb01.Mode) mode1 "Mode should be equal"
            Expect.floatClose Accuracy.high (negb02.Mode) mode2 "Mode should be equal"
            Expect.floatClose Accuracy.high (negb03.Mode) mode3 "Mode should be equal"
            Expect.throws mode4 "Mode cannot be determined"
            Expect.floatClose Accuracy.high (negb06.Mode) mode6 "Mode should be equal"
                
        //tested against Mathnet and https://homepage.divms.uiowa.edu/~mbognar/applets/nb1.html
        testCase "Mean" <| fun () ->
            let mean1 = 33.33333333
            let mean2 = 10.0        
            let mean3 = 100.0       
            let mean4 = nan   
            let mean6 = 60.0       
            
            Expect.floatClose Accuracy.high (negb01.Mean) mean1 "Mean should be equal"
            Expect.floatClose Accuracy.high (negb02.Mean) mean2 "Mean should be equal"
            Expect.floatClose Accuracy.high (negb03.Mean) mean3 "Mean should be equal"
            Expect.isTrue (nan.Equals(negb04.Mean)) "Mean should be equal"
            Expect.floatClose Accuracy.high (negb06.Mean) mean6 "Mean should be equal"
        
        //tested against Mathnet and https://homepage.divms.uiowa.edu/~mbognar/applets/nb1.html
        testCase "Variance" <| fun () ->
            let var1 = 337.037037
            let var2 = 90.0      
            let var3 = 900.0     
            let var4 = nan  
            let var6 = 540       
            
            Expect.floatClose Accuracy.high (negb01.Variance) var1 "Variance should be equal"
            Expect.floatClose Accuracy.high (negb02.Variance) var2 "Variance should be equal"
            Expect.floatClose Accuracy.high (negb03.Variance) var3 "Variance should be equal"
            Expect.isTrue (nan.Equals(negb04.Variance)) "Variance should be equal"
            Expect.floatClose Accuracy.high (negb06.Variance) var6 "Variance should be equal"
                
        //tested against Mathnet and https://homepage.divms.uiowa.edu/~mbognar/applets/nb1.html
        testCase "StandardDeviation" <| fun () ->
            let stdev = sqrt 337.037037
            Expect.floatClose Accuracy.high negb01.StandardDeviation stdev "Standard deviation should be equal"
    ]