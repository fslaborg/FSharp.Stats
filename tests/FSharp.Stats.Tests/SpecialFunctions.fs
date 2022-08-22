module SpecialFunctionsTests
open Expecto
    
open FSharp.Stats.SpecialFunctions
open FSharp.Stats

[<Tests>]
let gammaFunctionsTests =

    testList "SpecialFunctions.Gamma" [
        //expected values taken from filling function values in wolfram alpha https://www.wolframalpha.com/
        //_gamma (unchecked)
        testCase "_gamma(5)" <| fun () ->
            let gam = Gamma._gamma 5.
            Expect.floatClose Accuracy.high gam 24. "Should be equal (double precision)"
        
        testCase "_gamma(-1)" <| fun () ->
            let gam = Gamma._gamma -1.
            Expect.isTrue ((-infinity).Equals(gam)) "Expected gamma of negative number to return -infinity"

        testCase "_gamma(420) returns infinity (although incorrect)" <| fun () ->
            let gam = Gamma._gamma 420.
            Expect.isTrue (infinity.Equals(gam)) "Expected gamma of large number to return infinity"
        
        testCase "_gamma(1) = gamma(1)" <| fun () ->
            let _gam = Gamma._gamma 1.
            let gam = Gamma.gamma 1.
            Expect.equal _gam gam "expected equal result for checked and unchecked version"
        
        //gamma (checked)
        testCase "gamma(5)" <| fun () ->
            let gam = Gamma.gamma 5.
            Expect.floatClose Accuracy.high gam 24. "Should be equal (double precision)"
        
        testCase "gamma(-1)" <| fun () ->
            let gam = Gamma.gamma -1.
            Expect.isTrue ((-infinity).Equals(gam)) "Expected gamma of negative number to return -infinity"

        testCase "gamma(420) returns infinity (although incorrect)" <| fun () ->
            let gam = Gamma.gamma 420.
            Expect.isTrue (infinity.Equals(gam)) "Expected gamma of large number to return infinity"
        testCase "gamma(nan) = nan" <| fun () ->
            let gam = Gamma.gamma nan
            Expect.isTrue (nan.Equals(gam)) "Expected gamma(nan) to be nan"

        testCase "gamma(infinity) = infinity" <| fun () ->
            let gam = Gamma.gamma infinity
            Expect.isTrue (infinity.Equals(gam)) "Expected gamma(infinity) to be infinity"

        testCase "gamma(-infinity) = nan" <| fun () ->
            let gam = Gamma.gamma (-infinity)
            Expect.isTrue (nan.Equals(gam)) "Expected gamma(-infinity) to be nan"

        //_gammaLn(unchecked)
        testCase "_gammaLn(5)" <| fun () ->
            let gam = Gamma._gammaLn 5.
            Expect.floatClose Accuracy.high gam 3.1780538303479456196469416012970554088739909609035152140967343621 "Should be equal (double precision)"
        
        testCase "_gammaLn(-1)" <| fun () ->
            let gam = Gamma._gammaLn -1.
            Expect.isTrue (nan.Equals(gam)) "Expected _gammaLn of negative number to return nan"

        testCase "_gammaLn(420)" <| fun () ->
            let gam = Gamma._gammaLn 420.
            Expect.floatClose Accuracy.high gam 2114.8059883267407613276719264808503756320291823875025922347978642 "Should be equal (double precision)"
                
        testCase "_gammaLn(420) = gammaLn(420)" <| fun () ->
            let _gam = Gamma._gamma 420.
            let gam = Gamma.gamma 420.
            Expect.equal _gam gam "expected equal result for checked and unchecked version"

        //gammaLn(checked)
        testCase "gammaLn(5)" <| fun () ->
            let gam = Gamma.gammaLn 5.
            Expect.floatClose Accuracy.high gam 3.1780538303479456196469416012970554088739909609035152140967343621 "Should be equal (double precision)"
        
        testCase "gammaLn(-1)" <| fun () ->
            let gam = Gamma.gammaLn -1.
            Expect.isTrue (nan.Equals(gam)) "Expected gammaLn of negative number to return nan"

        testCase "gammaLn(420) returns infinity (although incorrect)" <| fun () ->
            let gam = Gamma.gammaLn 420.
            Expect.floatClose Accuracy.high gam 2114.8059883267407613276719264808503756320291823875025922347978642 "Should be equal (double precision)"

        testCase "gammaLn(nan) = nan" <| fun () ->
            let gam = Gamma.gammaLn nan
            Expect.isTrue (nan.Equals(gam)) "Expected gammaLn(nan) to be nan"

        testCase "gammaLn(infinity) = infinity" <| fun () ->
            let gam = Gamma.gammaLn infinity
            Expect.isTrue (infinity.Equals(gam)) "Expected gammaLn(infinity) to be infinity"

        testCase "gammaLn(-infinity) = nan" <| fun () ->
            let gam = Gamma.gammaLn (-infinity)
            Expect.isTrue (nan.Equals(gam)) "Expected gammaLn(-infinity) to be nan"
 
        //lowerIncomplete
        testCase "lowerIncomplete(0.5,0.5)" <| fun () ->
            let gam = Gamma.lowerIncomplete 0.5 0.5
            Expect.floatClose Accuracy.low gam 0.682689 "Should be equal (low precision)"

        testCase "lowerIncomplete(-1,1) = nan" <| fun () ->
            let gam = Gamma.lowerIncomplete -1. 1.
            Expect.isTrue (nan.Equals(gam)) "Expected lowerIncomplete(-1,1) to be nan"

        testCase "lowerIncomplete(-1,0) = 0" <| fun () ->
            let gam = Gamma.lowerIncomplete -1. 0.
            Expect.floatClose Accuracy.high gam 0.0 "Expected lowerIncomplete(-1,0) = 0 to be 0"

        testCase "lowerIncomplete(0.5,infinity) = 1" <| fun () ->
            let gam = Gamma.lowerIncomplete 0.5 Ops.inf
            Expect.equal gam 1.0 "lowerIncomplete(0.5,infinity) = 1"

        //upperIncomplete
        testCase "upperIncomplete(0.5,0.5)" <| fun () ->
            let gamu = Gamma.upperIncomplete 0.5 0.5
            let gam  = 1. - Gamma.lowerIncomplete 0.5 0.5
            Expect.floatClose Accuracy.medium gamu gam "Should be equal (medium precision)"

        testCase "upperIncomplete(-1,1)" <| fun () ->
            let gam = Gamma.upperIncomplete -1. 1.
            Expect.isTrue (nan.Equals(gam)) "Expected upperIncomplete(-1,1) to be nan"

        testCase "upperIncomplete(0.5, infinity)" <| fun () ->
            let gam = Gamma.upperIncomplete 0.5 Ops.inf
            Expect.equal gam 0.0 "expected upperIncomplete(0.5, infinity) to be 0"
        
        testCase "digamma(0.17) positive" <| fun () ->
            let diGam = Gamma.digamma 0.17
            Expect.equal diGam -6.2100942259248626 "expected upperIncomplete(0.5, infinity) to be 0"    
        
        testCase "digamma(-1.8209678549077879) negative" <| fun () ->
            let diGam = Gamma.digamma -1.8209678549077879
            Expect.equal diGam -4.1343001655848468 "expected upperIncomplete(0.5, infinity) to be 0"       
        
        testCase "trigamma(0.17) positive" <| fun () ->
            let triGam = Gamma.trigamma 0.17
            Expect.equal triGam 35.915302055854525 "expected upperIncomplete(0.5, infinity) to be 0"    
        
        testCase "trigamma(-1.8209678549077879) negative" <| fun () ->
            let triGam = Gamma.trigamma -1.8209678549077879
            Expect.equal triGam 34.283184056369407 "expected upperIncomplete(0.5, infinity) to be 0"      
    ]    


[<Tests>]
let betaFunctionsTests =
    //expected values taken from filling function values in wolfram alpha https://www.wolframalpha.com/
    testList "SpecialFunctions.Beta" [
        testCase "betaLn equality1" <| fun () ->
            let result = Beta.betaLn 1. 1. 
            Expect.floatClose Accuracy.veryHigh result 0.0 "Should be equal (double precision)" //rtol=1e-14, atol=0
        testCase "betaLn equality3" <| fun () ->
            let result = Beta.betaLn 0.0342 170.
            Expect.floatClose Accuracy.veryHigh result 3.1811881124242447 "Should be equal (double precision)" //rtol=1e-14, atol=0
        testCase "_betaLn(1,1) = betaLn(1,1)" <| fun () ->
            let _bet = Beta._betaLn 1. 1. 
            let bet = Beta.betaLn 1. 1. 
            Expect.equal _bet bet "expected equal result for checked and unchecked version"
        
        //_beta
        testCase "_beta(1.,1.)" <| fun () ->
            let bet = Beta._beta 1. 1.
            Expect.floatClose Accuracy.high bet 1. "Should be equal (double precision)"
        // these are incorrect due to approximation issues, see for example https://www.wolframalpha.com/input?i=beta%28-1%2C1%29
        testCase "_beta(-1.,1.)" <| fun () ->
            let bet = Beta._beta -1. 1.
            Expect.isTrue (nan.Equals(bet)) "Expected beta(-1.,1.) to return nan"

        testCase "_beta(1.,-1.)" <| fun () ->
            let bet = Beta._beta 1. -1.
            Expect.isTrue (nan.Equals(bet)) "Expected beta(1.,-1.) to return nan"

        testCase "_beta(-1.,-1.)" <| fun () ->
            let bet = Beta._beta -1. -1.
            Expect.isTrue (nan.Equals(bet)) "Expected beta(-1.,-1.) to return nan"
            
        testCase "_beta(420,420)" <| fun () ->
            let bet = Beta._beta 420. 420.
            Expect.floatClose Accuracy.high bet 2.360006414298225624664636431560387583108464693985603322036e-254 "Should be equal (double precision)"//beta
        
        testCase "_beta(1,1) = beta(1,1)" <| fun () ->
            let _bet = Beta._beta 420. 420.
            let bet = Beta.beta 420. 420.
            Expect.equal _bet bet "expected equal result for checked and unchecked version"

        //beta
        testCase "beta(1.,1.)" <| fun () ->
            let bet = Beta.beta 1. 1.
            Expect.floatClose Accuracy.high bet 1. "Should be equal (double precision)"
        // these are incorrect due to approximation issues, see for example https://www.wolframalpha.com/input?i=beta%28-1%2C1%29
        testCase "beta(-1.,1.)" <| fun () ->
            let bet = Beta.beta -1. 1.
            Expect.isTrue (nan.Equals(bet)) "Expected beta(-1.,1.) to return nan"

        testCase "beta(1.,-1.)" <| fun () ->
            let bet = Beta.beta 1. -1.
            Expect.isTrue (nan.Equals(bet)) "Expected beta(1.,-1.) to return nan"

        testCase "beta(-1.,-1.)" <| fun () ->
            let bet = Beta.beta -1. -1.
            Expect.isTrue (nan.Equals(bet)) "Expected beta(-1.,-1.) to return nan"
            
        testCase "beta(420,420)" <| fun () ->
            let bet = Beta.beta 420. 420.
            Expect.floatClose Accuracy.high bet 2.360006414298225624664636431560387583108464693985603322036e-254 "Should be equal (double precision)"

        testCase "beta(nan,1.)" <| fun () ->
            let bet = Beta.beta nan 1.
            Expect.isTrue (nan.Equals(bet)) "Expected beta(nan,1.) to return nan"

        testCase "beta(1.,nan)" <| fun () ->
            let bet = Beta.beta 1. nan
            Expect.isTrue (nan.Equals(bet)) "Expected beta(1,nan) to return nan"

        testCase "beta(nan,nan)" <| fun () ->
            let bet = Beta.beta nan nan
            Expect.isTrue (nan.Equals(bet)) "Expected beta(nan,nan) to return nan"

        testCase "beta(infinity,1.)" <| fun () ->
            let bet = Beta.beta infinity 1.
            Expect.isTrue (nan.Equals(bet)) "Expected beta(infinity,1.) to return nan"

        testCase "beta(1.,infinity)" <| fun () ->
            let bet = Beta.beta 1. infinity
            Expect.isTrue (nan.Equals(bet)) "Expected beta(1,infinity) to return nan"

        testCase "beta(infinity,infinity)" <| fun () ->
            let bet = Beta.beta infinity infinity
            Expect.isTrue (nan.Equals(bet)) "Expected beta(infinity,infinity) to return nan"

        testCase "beta(-infinity,1.)" <| fun () ->
            let bet = Beta.beta -infinity 1.
            Expect.isTrue (nan.Equals(bet)) "Expected beta(-infinity,1.) to return nan"

        testCase "beta(1.,-infinity)" <| fun () ->
            let bet = Beta.beta 1. -infinity
            Expect.isTrue (nan.Equals(bet)) "Expected beta(1,-infinity) to return nan"

        testCase "beta(-infinity,-infinity)" <| fun () ->
            let bet = Beta.beta -infinity -infinity
            Expect.isTrue (nan.Equals(bet)) "Expected beta(-infinity,-infinity) to return nan"

        testCase "test_incbcf function" <| fun () ->
            let result  = nan //(4.,2.,4.2); nan
            let disired = -0.23046874999999992
            Expect.floatClose Accuracy.veryHigh result disired "Should be equal (double precision)"
        
        testCase "test_incbcd function" <| fun () ->
            let result  = nan //(4.,2.,4.2); nan
            let disired = 0.7375
            Expect.floatClose Accuracy.veryHigh result disired "Should be equal (double precision)"

        testCase "test_incompleteInverse" <| fun () ->
            let result  = nan //(0.5,0.6,0.1); nan
            let disired = 0.019145979066925722
            Expect.floatClose Accuracy.veryHigh result disired "Should be equal (double precision)"

        testCase "test_powerSeries" <| fun () ->
            let result  = Beta.powerSeries 4. 2. 4.2
            let disired = -3671.801280000001
            Expect.floatClose Accuracy.veryHigh result disired "Should be equal (double precision)"

        //TODO: Beta into class to allow [<ParamArray>]
        
        //testCase "test_multinomial" <| fun () ->
        //    let result  = Beta.multinomial (0.42, 0.5, 5.2 )
        //    let disired = 0.82641912952987062
        //    Expect.floatClose Accuracy.veryHigh result disired "Should be equal (double precision)"

    ]

[<Tests>]
let factorialTests =
    //expected values taken from filling function values in wolfram alpha https://www.wolframalpha.com/
    testList "SpecialFunctions.Factorial" [
        //factorial
        testCase "Prevents Double overflow for 171! as infinity" (fun _ -> 
            Expect.equal (Factorial.factorial 171) infinity "Expected factorial of a number larger than 170 to result in infinity (171! is larger than max double)"
        )
        testCase "0! equals 1" (fun _ -> 
            Expect.equal (Factorial.factorial 0) 1. "Expected factorial of 0 to be 1."
        )
        testCase "69!" (fun _ -> 
            Expect.floatClose Accuracy.high (Factorial.factorial 69) 1.7112245e+98 "Expected factorial of 69 to be 1.7112245e+98"
        )
        testCase "factorial not defined for negative numbers" (fun _ -> 
            Expect.throws (fun _ -> Factorial.factorial -69421337 |> ignore) "Expected factorial to fail for negative values"
        )
        //_factorialLn
        testCase "_ln(6942!)" (fun _ -> 
            Expect.floatClose Accuracy.high (Factorial._factorialLn 6942) 54467.727976695301612523565124699078303834231913072759124392135342 "factorialLn of large number failed"
        )
        testCase "_ln(0!) = 0" (fun _ -> 
            Expect.equal (Factorial._factorialLn 0) 0. "Expected factorialLn of 0 to be 1."
        )
        testCase "_ln(69!)" (fun _ -> 
            Expect.floatClose Accuracy.high 226.19054832372759333227016852232261788323276357495863628461257077 (Factorial._factorialLn 69) "Expected factorialLn of 69 to be 226.19054832372759333227016852232261788323276357495863628461257077"
        )
        testCase "_factorialLn not defined for negative numbers" (fun _ -> 
            Expect.throws (fun _ -> Factorial._factorialLn -69421337 |> ignore) "Expected factorialLn to fail for negative values"
        )        
        testCase "_ln(6942!) = ln(6942!)" (fun _ ->
            let _ln = Factorial._factorialLn 6942
            let ln = Factorial.factorialLn 6942
            Expect.equal _ln ln "expected equal result for checked and unchecked version"
        )
        //factorialLn
        testCase "ln(6942!)" (fun _ -> 
            Expect.floatClose Accuracy.high (Factorial.factorialLn 6942) 54467.727976695301612523565124699078303834231913072759124392135342 "factorialLn of large number failed"
        )
        testCase "ln(0!) = 0" (fun _ -> 
            Expect.equal (Factorial.factorialLn 0) 0. "Expected factorialLn of 0 to be 1."
        )
        testCase "ln(69!)" (fun _ -> 
            Expect.floatClose Accuracy.high 226.19054832372759333227016852232261788323276357495863628461257077 (Factorial.factorialLn 69) "Expected factorialLn of 69 to be 226.19054832372759333227016852232261788323276357495863628461257077"
        )
        testCase "factorialLn not defined for negative numbers" (fun _ -> 
            Expect.throws (fun _ -> Factorial.factorialLn -69421337 |> ignore) "Expected factorialLn to fail for negative values"
        )
    ]

[<Tests>]
let logisticTests =
    //expected values taken from filling function values in wolfram alpha https://www.wolframalpha.com/
    testList "SpecialFunctions.Logistic" [
        testCase "standard x=69" (fun _ -> 
            Expect.floatClose Accuracy.low (Logistic.standard 2.) 0.8807970779778824440597291413023967952063842986289682757984052500 ""
        )
        testCase "standard nan" (fun _ -> 
            Expect.isTrue (nan.Equals(Logistic.standard nan)) "Expected nan"
        )
        testCase "standard inf" (fun _ -> 
            Expect.floatClose Accuracy.high (Logistic.standard infinity) 1. "Expected 1"
        )
        testCase "standard -inf" (fun _ -> 
            Expect.floatClose Accuracy.high (Logistic.standard (-infinity)) 0. "Expected 0"
        )
        testCase "generic x0=4 L=2 k=4 x=5 " (fun _ -> 
            Expect.floatClose Accuracy.high (Logistic.generic 4. 2. 4. 5. ) 1.9640275800758168839464137241009231502550299762409347760482632174 ""
        )
        //nan
        testCase "generic x=nan L=2 k=4 x0=4" (fun _ -> 
            Expect.isTrue (nan.Equals(Logistic.generic nan 2. 4. 5.)) "Expected nan"
        )
        testCase "generic x=4 L=nan k=4 x0=4" (fun _ -> 
            Expect.isTrue (nan.Equals(Logistic.generic 4. nan 4. 5.)) "Expected nan"
        )
        testCase "generic x=4 L=2 k=nan x0=4" (fun _ -> 
            Expect.isTrue (nan.Equals(Logistic.generic 4. 2. nan 5.)) "Expected nan"
        )
        testCase "generic x=4 L=2 k=4 x0=nan" (fun _ -> 
            Expect.isTrue (nan.Equals(Logistic.generic 4. 2. 4. nan)) "Expected nan"
        )
        //infinity
        testCase "generic x=infinity L=2 k=4 x0=4" (fun _ -> 
            Expect.floatClose Accuracy.high  (Logistic.generic infinity 2. 4. 5.) 0. "Expected 0"
        )
        testCase "generic x=4 L=infinity k=4 x0=4" (fun _ -> 
            Expect.isTrue (infinity.Equals(Logistic.generic 4. infinity 4. 5.)) "Expected infinity"
        )
        testCase "generic x=4 L=2 k=infinity x0=4" (fun _ -> 
            Expect.floatClose Accuracy.high (Logistic.generic 4. 2. infinity 5.) 2. "Expected 2"
        )
        testCase "generic x=4 L=2 k=4 x0=infinity" (fun _ -> 
            Expect.floatClose Accuracy.high (Logistic.generic 4. 2. 4. infinity) 2. "Expected 2"
        )
        //-infinity
        testCase "generic x=-infinity L=2 k=4 x0=4" (fun _ -> 
            Expect.floatClose Accuracy.low 2. (Logistic.generic (-infinity) 2. 4. 5.) "Expected 2"
        )
        testCase "generic x=4 L=-infinity k=4 x0=4" (fun _ -> 
            Expect.isTrue ((-infinity).Equals(Logistic.generic 4. (-infinity) 4. 5.)) "Expected -infinity"
        )
        testCase "generic x=4 L=2 k=-infinity x0=4" (fun _ -> 
            Expect.floatClose Accuracy.high 0. (Logistic.generic 4. 2. (-infinity) 5.)  "Expected 0"
        )
        testCase "generic x=4 L=2 k=4 x0=-infinity" (fun _ -> 
            Expect.floatClose Accuracy.high 0. (Logistic.generic 4. 2. 4. (-infinity))  "Expected 0"
        )
    ]


[<Tests>]
let erfTests =
    //expected values taken from filling function values in wolfram alpha https://www.wolframalpha.com/
    testList "SpecialFunctions.ErrorFunction(erf)" [
        // erf
        testCase "erf(0) = 0" (fun _ -> 
            Expect.floatClose Accuracy.medium (Errorfunction.Erf 0.) 0. "expected erf(0) to be 0"
        )
        testCase "erf(-3)" (fun _ -> 
            Expect.floatClose Accuracy.medium (Errorfunction.Erf -3.) -0.999977909503001414558627223870417679620152292912600750342761045 "erf returned insufficient approximation of the result"
        )
        testCase "erf(3)" (fun _ -> 
            Expect.floatClose Accuracy.medium (Errorfunction.Erf 3.) 0.9999779095030014145586272238704176796201522929126007503427610451 "erf returned insufficient approximation of the result"
        )
        testCase "erf(nan)" (fun _ -> 
            Expect.isTrue (nan.Equals(Errorfunction.Erf nan)) "Expected nan"
        )
        testCase "erf(infinity)" (fun _ -> 
            Expect.equal  (Errorfunction.Erf infinity) 1. "expected erf(infinity) to be 1"
        )
        testCase "erf(-infinity)" (fun _ -> 
            Expect.equal  (Errorfunction.Erf -infinity) -1. "expected erf(-infinity) to be -1"
        )
        //erfc
        testCase "erfc(0) = 1" (fun _ -> 
            Expect.floatClose Accuracy.low (Errorfunction.Erfc 0.) 1. "expected erfc(0) to be 1"
        )
        testCase "erfc(-3)" (fun _ -> 
            Expect.floatClose Accuracy.low (Errorfunction.Erfc -3.) 1.9999779095030014145586272238704176796201522929126007503427610451 "erfc returned insufficient approximation of the result"
        )
        testCase "erfc(3)" (fun _ -> 
            Expect.floatClose Accuracy.low (Errorfunction.Erfc 3.) 0.0000220904969985854413727761295823203798477070873992496572389548 "erfc returned insufficient approximation of the result"
        )
        testCase "erfc(nan)" (fun _ -> 
            Expect.isTrue (nan.Equals(Errorfunction.Erfc nan)) "Expected nan"
        )
        testCase "erfc(infinity)" (fun _ -> 
            Expect.equal  (Errorfunction.Erfc infinity) 0. "expected erfc(infinity) to be 0"
        )
        testCase "erfc(-infinity)" (fun _ -> 
            Expect.equal  (Errorfunction.Erfc -infinity) 2. "expected erfc(-infinity) to be 2"
        )
        //_erfcx (unchecked)
        testCase "_erfcx(0) = 1" (fun _ -> 
            Expect.floatClose Accuracy.low (Errorfunction._erfcx 0.) 1. "expected _erfcx(0) to be 1"
        )
        testCase "_erfcx(-3)" (fun _ -> 
            Expect.floatClose Accuracy.low (Errorfunction._erfcx -3.) 16205.988853999586625469574084050206309035724190299120070784655345 "_erfcx returned insufficient approximation of the result"
        )
        testCase "_erfcx(3)" (fun _ -> 
            Expect.floatClose Accuracy.low (Errorfunction._erfcx 3.) 0.1790011511813899504192948153136209872279853641068542156627588395 "_erfcx returned insufficient approximation of the result"
        )
        testCase "_erfcx(0) = _erfcx(0)" (fun _ -> 
            let _res = Errorfunction._erfcx 0
            let res = Errorfunction.erfcx 0
            Expect.equal _res res "expected equal result for checked and unchecked version"
        )
        //erfcx (checked)
        testCase "erfcx(0) = 1" (fun _ -> 
            Expect.floatClose Accuracy.low (Errorfunction.erfcx 0.) 1. "expected erfcx(0) to be 1"
        )
        testCase "erfcx(-3)" (fun _ -> 
            Expect.floatClose Accuracy.low (Errorfunction.erfcx -3.) 16205.988853999586625469574084050206309035724190299120070784655345 "erfcx returned insufficient approximation of the result"
        )
        testCase "erfcx(3)" (fun _ -> 
            Expect.floatClose Accuracy.low (Errorfunction.erfcx 3.) 0.1790011511813899504192948153136209872279853641068542156627588395 "erfcx returned insufficient approximation of the result"
        )
        testCase "erfcx(nan)" (fun _ -> 
            Expect.isTrue (nan.Equals(Errorfunction.erfcx nan)) "Expected nan"
        )
        testCase "erfcx(infinity)" (fun _ -> 
            Expect.isTrue (nan.Equals(Errorfunction.erfcx infinity)) "expected erfcx(infinity) to be nan"
        )
        testCase "erfcx(-infinity)" (fun _ -> 
            Expect.isTrue (infinity.Equals(Errorfunction.erfcx -infinity)) "expected erfcx(-infinity) to be infinity"
        )
    ]

[<Tests>]
let binomialCoefficientTests =
    //expected values taken from filling function values in wolfram alpha https://www.wolframalpha.com/
    testList "SpecialFunctions.Binomial" [
        // _coefficient (unchecked)
        testCase "_(0 | 0) = 1" (fun _ -> 
            Expect.floatClose Accuracy.high (Binomial._coeffcient 0 0) 1. "Expected (0 | 0) to be 1"
        )
        testCase "_(-1 | 0) should throw (negative value)" (fun _ -> 
            Expect.throws (fun _ -> (Binomial._coeffcient -1 0) |> ignore) "Expected (-1 | 0) to throw"
        )
        testCase "_(0 | -1) should throw (negative value)" (fun _ -> 
            Expect.throws (fun _ -> (Binomial._coeffcient 0 -1) |> ignore) "Expected (0 | -1) to throw"
        )
        testCase "_(1 | 2) should throw (n<k)" (fun _ -> 
            Expect.throws (fun _ -> (Binomial._coeffcient 1 2) |> ignore) "Expected (1 | 2) to throw"
        )
        testCase "_(69 | 42)" (fun _ -> 
            Expect.floatClose Accuracy.high (Binomial._coeffcient 69 42) 11185257572725865552. "Binomail coefficient returned wrong result"
        )        
        testCase "_(69 | 42) = (69 | 42)" (fun _ -> 
            let _res = Binomial._coeffcient 69 42
            let res = Binomial.coeffcient 69 42
            Expect.equal _res res "expected equal result for checked and unchecked version"
        )
        // coefficient (checked)
        testCase "(0 | 0) = 1" (fun _ -> 
            Expect.floatClose Accuracy.high (Binomial.coeffcient 0 0) 1. "Expected (0 | 0) to be 1"
        )
        testCase "(-1 | 0) should throw (negative value)" (fun _ -> 
            Expect.throws (fun _ -> (Binomial.coeffcient -1 0) |> ignore) "Expected (-1 | 0) to throw"
        )
        testCase "(0 | -1) should throw (negative value)" (fun _ -> 
            Expect.throws (fun _ -> (Binomial.coeffcient 0 -1) |> ignore) "Expected (0 | -1) to throw"
        )
        testCase "(1 | 2) should throw (n<k)" (fun _ -> 
            Expect.throws (fun _ -> (Binomial.coeffcient 1 2) |> ignore) "Expected (1 | 2) to throw"
        )
        testCase "(69 | 42)" (fun _ -> 
            Expect.floatClose Accuracy.high (Binomial.coeffcient 69 42) 11185257572725865552. "Binomail coefficient returned wrong result"
        )
        // _coefficientLn (unchecked)
        testCase "_ln(0 | 0) = 1" (fun _ -> 
            Expect.floatClose Accuracy.high (Binomial._coeffcientLn 0 0) 0. "Expected ln(0 | 0) to be 0"
        )
        testCase "_ln(-1 | 0) should throw (negative value)" (fun _ -> 
            Expect.throws (fun _ -> (Binomial._coeffcientLn -1 0) |> ignore) "Expected ln(-1 | 0) to throw"
        )
        testCase "_ln(0 | -1) should throw (negative value)" (fun _ -> 
            Expect.throws (fun _ -> (Binomial._coeffcientLn 0 -1) |> ignore) "Expected ln(0 | -1) to throw"
        )
        testCase "_ln(1 | 2) should throw (n<k)" (fun _ -> 
            Expect.throws (fun _ -> (Binomial._coeffcientLn 1 2) |> ignore) "Expected ln(1 | 2) to throw"
        )
        testCase "_ln(69 | 42)" (fun _ -> 
            Expect.floatClose Accuracy.high (Binomial._coeffcientLn 69 42) 43.861128296976190734480722409484720407496953168236423941162466992 "Binomail coefficientln returned wrong result"
        )
        testCase "_ln(69000 | 4200)" (fun _ -> 
            Expect.floatClose Accuracy.high (Binomial._coeffcientLn 69000 4200) 15820.331735478233070945558688627562355591359160763289683149612673 "Binomail coefficientln returned wrong result"
        )        
        testCase "_ln(69 | 42) = ln(69 | 42)" (fun _ -> 
            let _res = Binomial._coeffcientLn 69 42
            let res = Binomial.coeffcientLn 69 42
            Expect.equal _res res "expected equal result for checked and unchecked version"
        )
        // coefficientLn (checked)
        testCase "ln(0 | 0) = 1" (fun _ -> 
            Expect.floatClose Accuracy.high (Binomial.coeffcientLn 0 0) 0. "Expected ln(0 | 0) to be 0"
        )
        testCase "ln(-1 | 0) should throw (negative value)" (fun _ -> 
            Expect.throws (fun _ -> (Binomial.coeffcientLn -1 0) |> ignore) "Expected ln(-1 | 0) to throw"
        )
        testCase "ln(0 | -1) should throw (negative value)" (fun _ -> 
            Expect.throws (fun _ -> (Binomial.coeffcientLn 0 -1) |> ignore) "Expected ln(0 | -1) to throw"
        )
        testCase "ln(1 | 2) should throw (n<k)" (fun _ -> 
            Expect.throws (fun _ -> (Binomial.coeffcientLn 1 2) |> ignore) "Expected ln(1 | 2) to throw"
        )
        testCase "ln(69 | 42)" (fun _ -> 
            Expect.floatClose Accuracy.high (Binomial.coeffcientLn 69 42) 43.861128296976190734480722409484720407496953168236423941162466992 "Binomail coefficientln returned wrong result"
        )
        testCase "ln(69000 | 4200)" (fun _ -> 
            Expect.floatClose Accuracy.high (Binomial.coeffcientLn 69000 4200) 15820.331735478233070945558688627562355591359160763289683149612673 "Binomail coefficientln returned wrong result"
        )
    ]