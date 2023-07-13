module InterpolationTests 

open Expecto
open FSharp.Stats
open FSharp.Stats.Interpolation 

open TestExtensions

[<Tests>]
let cubicInterpolationTests =
    let t = vector [0.0; 1.0; 2.0; 3.0]
    let y = vector [187.6;185.7;193.7;197.0]
    let tt = vector [0.0;0.25;0.5;0.75;1.;1.25;1.5;1.75;2.;2.25;2.5;2.75;3.0]
    
    let u = vector [1.0 ;4.0; 9.0; 16.0]
    let t2 = vector [1.0; 2.0; 3.0; 4.0]

    testList "Interpolation.CubicSpline" [
        testCase "Natural Cubic Spline" <| fun () -> 
            //Verifies that the fitted point match the expectred fittied points
            //https://columbiaeconomics.com/2010/01/20/how-economists-convert-quarterly-data-into-monthly-cubic-spline-interpolation/comment-page-1/
            let coefficientsSpline = 
                CubicSpline.interpolate CubicSpline.Natural t y            
            let fitOutPut = tt |> Vector.map (CubicSpline.predict coefficientsSpline)
            let expectedValues = vector [187.6; 186.4328125; 185.5425; 185.2059375; 185.7; 187.179375;189.31; 191.635625; 193.7; 195.1528125; 196.0675; 196.6234375;197.0]
            TestExtensions.sequenceEqual Accuracy.low expectedValues fitOutPut "Fitted Values and Expected Output should be equal (double precision)"
    
        testCase "Quadratic Cubic Spline" <| fun () ->            
            let coefficientsQuadraticSpline = 
                CubicSpline.interpolate CubicSpline.Quadratic t2 u         
            let fittingFunc x = 
                CubicSpline.predictWithinRange coefficientsQuadraticSpline x           
            Expect.floatClose Accuracy.high (fittingFunc 1.5) 2.25  "Fitted Value should be equal (double precision)"
            Expect.floatClose Accuracy.high (fittingFunc 2.5) 6.25 "Fitted Value should be equal (double precision)"
            Expect.floatClose Accuracy.high (fittingFunc 3.5) 12.25 "Fitted Value should be equal (double precision)"  

        let seriesx = [|20.15;24.41;28.78|] |> Array.sort |> vector
        let seriesy = [|0.367;0.591;0.796|] |> Array.sort |> vector
        testCase "Parabolic Cubic Interpolation" <| fun () ->       
            //http://support.ptc.com/help/mathcad/en/index.html#page/PTC_Mathcad_Help%2Fexample_cubic_spline_interpolation.html%23     
            let coeffParabolic = CubicSpline.interpolate CubicSpline.Parabolic seriesx seriesy 
            let fittingFuncParabolic x = 
                CubicSpline.predict coeffParabolic x     

            let genrateX = vector [20.0..25.0]
            let interpParabolic = genrateX |> Vector.map fittingFuncParabolic
            let parabolicSndDeriv x = CubicSpline.getSecondDerivative coeffParabolic x 

            Expect.floatClose Accuracy.high (parabolicSndDeriv interpParabolic.[0])  (parabolicSndDeriv interpParabolic.[1]) "the second derivative at the first and second points should be equal (double precision)"
    ]

[<Tests>]
let akimaInterpolationTests =
    let t = vector [0.0; 1.0; 2.0; 3.0]
    let y = vector [187.6;185.7;193.7;197.0]
    let tt = vector [0.0;0.25;0.5;0.75;1.;1.25;1.5;1.75;2.;2.25;2.5;2.75;3.0]
    
    let u = vector [1.0 ;4.0; 9.0; 16.0]
    let t2 = vector [1.0; 2.0; 3.0; 4.0]

    testList "Interpolation.CubicSpline" [
        
        let values =  [|0.0; 2.0; 1.0; 3.0; 2.0; 6.0; 5.5; 5.5; 2.7; 5.1; 3.0|]
        let time = [|0.0..10.0|] 
        testCase "Akima Interpolation" <| fun () ->            
            let splineCoefsAkima = Akima.interpolate time values 
            let fittingFuncAkima x = 
                Akima.predict splineCoefsAkima  x          
            Expect.floatClose Accuracy.high (fittingFuncAkima 0.5) 1.375 "Fitted Value should be equal (double precision)"
            Expect.floatClose Accuracy.high (fittingFuncAkima 1.0) 2.0 "Fitted Value should be equal (double precision)"
            Expect.floatClose Accuracy.high (fittingFuncAkima 1.5) 1.5 "Fitted Value  should be equal (double precision)" 
            Expect.floatClose Accuracy.high (fittingFuncAkima 2.5) 1.953125 "Fitted Value should be equal (double precision)"  
            Expect.floatClose Accuracy.high (fittingFuncAkima 3.5) 2.484375 "Fitted Value should be equal (double precision)" 
            Expect.floatClose Accuracy.medium (fittingFuncAkima 4.5) 4.136363 "Fitted Value should be equal (double precision)" 

    ]

[<Tests>]
let polynomialInterpolationTests =
    testList "Interpolation.Polynomial" [
        let datax = vector [301.0;306.0;318.0;332.0;333.0]
        let datay = vector [0.02;0.2;-0.04;0.06;0.17]
    
        testCase "Polynomial Interpolation" <| fun() -> 
            //http://support.ptc.com/help/mathcad/en/index.html#page/PTC_Mathcad_Help%2Fexample_polynomial_interpolation.html%23wwID0E3LVS
            let coeffs = Polynomial.interpolate datax datay
            let expectedCoeffs = [18489.1150794045; -249.9950165; 1.2620688143; -0.0028205075; 0.0000023552]
            let polyInterpFit x = Polynomial.predict coeffs x 
            Expect.floatClose Accuracy.high (polyInterpFit 328.0) -0.1894337636 "Fitted Value should be equal (double precision)"
            TestExtensions.sequenceEqual(Accuracy.high) (datax |> Seq.map polyInterpFit) datay "Fitted Value should be equal (double precision)"
            TestExtensions.sequenceEqual(Accuracy.high) coeffs.C0_CX expectedCoeffs "Coefficients should be equal (double precision)"
        ]











