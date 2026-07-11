module InterpolationTests 

open Expecto
open FsMath
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
            let fitOutPut = tt |> Array.map (CubicSpline.predict coefficientsSpline)
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
            let interpParabolic = genrateX |> Array.map fittingFuncParabolic
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

[<Tests>]
let BezierInterpolationTests =
    testList "Interpolation.Bezier" [

        testCase "Bezier Interpolation of degree 1" <| fun() ->
            // Without control point, this is just linear interpolation
            let p0 = vector [|1.;1.;1.|] //point 0 that should be traversed
            let p1 = vector [|3.;2.;0.|] //point 1 that should be traversed
            let data = [|p0;p1|]
            let interpolate = Bezier.interpolate data
            let expectedMiddle = p0 .+ 0.5 .* (p1 .- p0)
            TestExtensions.sequenceEqual(Accuracy.high) (interpolate 0.) p0 "Initial point should be equal (double precision)"
            TestExtensions.sequenceEqual(Accuracy.high) (interpolate 0.5) expectedMiddle "Middle point should be equal (double precision)"
            TestExtensions.sequenceEqual(Accuracy.high) (interpolate 1.) p1 "Final point should be equal (double precision)"

        testCase "Bezier Interpolation of degree 2" <| fun() ->
            let p0 = vector [|1.;1.;1.|] //point 0 that should be traversed
            let c0 = vector [|1.1;2.1;2.|] //control point 0
            let p1 = vector [|3.;2.;0.|] //point 1 that should be traversed
            let data = [|p0;c0;p1|]
            let interpolate = Bezier.interpolate data
            let a = p0 .+ 0.5 .* (c0 .- p0)
            let b = c0 .+ 0.5 .* (p1 .- c0)
            let expectedMiddle = a .+ 0.5 .* (b .- a)
            TestExtensions.sequenceEqual(Accuracy.high) (interpolate 0.) p0 "Initial point should be equal (double precision)"
            TestExtensions.sequenceEqual(Accuracy.high) (interpolate 0.5) expectedMiddle "Middle point should be equal (double precision)"
            TestExtensions.sequenceEqual(Accuracy.high) (interpolate 1.) p1 "Final point should be equal (double precision)"

        testCase "Bezier Interpolation of degree 3" <| fun() ->
            let p0 = vector [|1.;1.;1.|] //point 0 that should be traversed
            let c0 = vector [|1.1;2.1;2.|] //control point 0
            let c1 = vector [|3.8;1.6;1.4|] //control point 1
            let p1 = vector [|3.;2.;0.|] //point 1 that should be traversed
            let data = [|p0;c0;c1;p1|]
            let interpolate = Bezier.interpolate data
            let a = p0 .+ 0.5 .* (c0 .- p0)
            let b = c0 .+ 0.5 .* (c1 .- c0)
            let c = c1 .+ 0.5 .* (p1 .- c1)
            let d = a .+ 0.5 .* (b .- a)
            let e = b .+ 0.5 .* (c .- b)
            let expectedMiddle = d .+ 0.5 .* (e .- d)
            TestExtensions.sequenceEqual(Accuracy.high) (interpolate 0.) p0 "Initial point should be equal (double precision)"
            TestExtensions.sequenceEqual(Accuracy.high) (interpolate 0.5) expectedMiddle "Middle point should be equal (double precision)"
            TestExtensions.sequenceEqual(Accuracy.high) (interpolate 1.) p1 "Final point should be equal (double precision)"

    ]




[<Tests>]
let integrationTests =
    testList "Interpolation.integrate" [

        testCase "LinearSpline.integrate positive linear function" <| fun () ->
            // y = 2x at {0,2,4} => integral [0,4] = [x^2]_0^4 = 16
            let coefs = LinearSpline.interpolate [|0.;2.;4.|] [|0.;4.;8.|]
            Expect.floatClose Accuracy.high (LinearSpline.integrate coefs 0. 4.) 16.0
                "integral of y=2x from 0 to 4 should be 16"
            // [1,3]: segments [0,2] and [2,4], so partial cross-segment
            // ∫[1,3] 2x dx = [x^2]_1^3 = 9 - 1 = 8
            Expect.floatClose Accuracy.high (LinearSpline.integrate coefs 1. 3.) 8.0
                "integral of y=2x from 1 to 3 should be 8"
            // ∫[0.5,1.5] 2x dx = 1.5^2 - 0.5^2 = 2.25 - 0.25 = 2 
            Expect.floatClose Accuracy.high (LinearSpline.integrate coefs 0.5 1.5) 2.0
                "integral of y=2x from 0.5 to 1.5 should be 2"
        
        testCase "LinearSpline.integrate reversed limits" <| fun () ->
            let coefs = LinearSpline.interpolate [|0.;2.;4.|] [|0.;4.;8.|]
            let forward = LinearSpline.integrate coefs 1. 3.
            let reverse = LinearSpline.integrate coefs 3. 1.

            Expect.floatClose Accuracy.high forward (-reverse)
                "integrating with reversed limits should negate the result"

        testCase "LinearSpline.integrate is additive" <| fun () ->
            let coefs = LinearSpline.interpolate [|0.;2.;4.|] [|0.;4.;8.|]
            let whole = LinearSpline.integrate coefs 1. 3.
            let partials = LinearSpline.integrate coefs 1. 2. + LinearSpline.integrate coefs 2. 3.
            Expect.floatClose Accuracy.high whole partials
                "integral should be additive across knot boundaries"

        testCase "LinearSpline.integrate returns zero for equal bounds" <| fun () ->
            let coefs = LinearSpline.interpolate [|0.;1.;2.|] [|1.;2.;3.|]
            Expect.floatClose Accuracy.high (LinearSpline.integrate coefs 1. 1.) 0.0
                "integral with equal bounds should be 0"
        
        testCase "LinearSpline.integrate negative linear function" <| fun () ->
            let coefs = LinearSpline.interpolate [|0.;1.|] [|-1.;0.|]
            // ∫[0,1] (x-1) dx = -1/2
            Expect.floatClose Accuracy.high (LinearSpline.integrate coefs 0. 1.) -0.5
                "integral of negative linear function should be negative"
        
        testCase "LinearSpline.integrate across zero crossing" <| fun () ->
            let coefs = LinearSpline.interpolate [|0.;1.;2.|] [|-1.;0.;1.|]
            // ∫[0,2] (x-1) dx = 0
            Expect.floatClose Accuracy.high (LinearSpline.integrate coefs 0. 2.) 0.0
                "equivalent positive and negative areas should cancel"

        testCase "Step.integrate constant positive segments" <| fun () ->
            // y = 2 on [0,1), y = 3 on [1,2), y = 4 on [2,3)
            let coefs = Step.interpolate [|0.;1.;2.;3.|] [|2.;3.;4.;5.|]
            // ∫[0,3] = 2*1 + 3*1 + 4*1 = 9
            Expect.floatClose Accuracy.high (Step.integrate coefs 0. 3.) 9.0
                "integral of step function from 0 to 3 should be 9"
            // ∫[0.5,2.5] = 2*0.5 + 3*1 + 4*0.5 = 1 + 3 + 2 = 6
            Expect.floatClose Accuracy.high (Step.integrate coefs 0.5 2.5) 6.0
                "partial integral of step function from 0.5 to 2.5 should be 6"
            // ∫[1,2] = 3 * 1 = 3
            Expect.floatClose Accuracy.high (Step.integrate coefs 1. 2.) 3.0
                "integral within exact segment boundaries should be 3"
            Expect.floatClose Accuracy.high (Step.integrate coefs 1.5 1.5) 0.0
                "integral over zero-width interval should be zero"
            // Uses first interval value for values below x = 0
            // ∫[-1,0] = 2 * 1 = 2
            Expect.floatClose Accuracy.high (Step.integrate coefs -1 0) 2.0
                "integral before the first breakpoint should use first segment value"
        
        testCase "Step.integrate is additive" <| fun () ->
            let coefs = Step.interpolate [|0.;1.;2.;3.|] [|2.;3.;4.;5.|]
            let whole = Step.integrate coefs 0.5 2.5
            let partials = (Step.integrate coefs 0.5 1.5) + (Step.integrate coefs 1.5 2.5)
            Expect.floatClose Accuracy.high whole partials
                "integral should be additive acreoss sub-intervals"
        
        testCase "Step.integrate reversed limits" <| fun () ->
            let coefs = Step.interpolate [|0.;1.;2.;3.|] [|2.;3.;4.;5.|]

            let forward = Step.integrate coefs 0.5 2.5
            let reverse = Step.integrate coefs 2.5 0.5

            Expect.floatClose Accuracy.high reverse (-forward)
                "integrating with reversed limits should negate the result"
        
        testCase "Step.integrate constant negative segments" <| fun () ->
            // y = -2 on [0,1), y = -3 on [1,2)
            let coefs = Step.interpolate [|0.;1.;2.|] [|-2.;-3.;-3.|]
            //∫[0,2] = (-2)*1 + (-3)*1 = -5
            Expect.floatClose Accuracy.high (Step.integrate coefs 0. 2.) -5.0
                "Integral of negative step function should be negative"

        testCase "Step.integrate across zero crossing" <| fun () ->
            // y= -2 on [0,1), y = 2 on [1,2)
            let coefs = Step.interpolate [|0.;1.;2.|] [|-2.; 2.; 2.|]
            // ∫[0,2] = (-2)*1 + 2*1 = 0
            Expect.floatClose Accuracy.high (Step.integrate coefs 0. 2.) 0.0
                "equivalent positive and negative areas should cancel"
        
        testCase "CubicSpline.integrate positive quadratic function" <| fun () ->
            // Quadratic boundary condition reproduces y=x^2 exactly
            let t = vector [| 1.; 2.; 3.; 4. |]
            let u = vector [| 1.; 4.; 9.; 16. |]  // y = x^2
            let coefs = CubicSpline.interpolate CubicSpline.Quadratic t u
            // ∫[1,4] x^2 dx = [x^3/3]_1^4 = 64/3 - 1/3 = 21
            Expect.floatClose Accuracy.high (CubicSpline.integrate coefs 1. 4.) 21.0
                "integral of y=x^2 from 1 to 4 should be 21"
            // ∫[1,2] x^2 dx = 8/3 - 1/3 = 7/3
            Expect.floatClose Accuracy.high (CubicSpline.integrate coefs 1. 2.) (7. / 3.)
                "integral of y=x^2 from 1 to 2 should be 7/3"
            // ∫[1.5,3.5] x² dx = (3.5³ - 1.5³)/3
            Expect.floatClose Accuracy.high (CubicSpline.integrate coefs 1.5 3.5) ((3.5 ** 3. - 1.5 ** 3.) / 3.)
                "integral across several spline intervals should be correct"

        testCase "CubicSpline.integrate is additive" <| fun () ->
            let t = vector [| 1.; 2.; 3.; 4. |]
            let u = vector [| 1.; 4.; 9.; 16. |]  // y = x^2
            let coefs = CubicSpline.interpolate CubicSpline.Quadratic t u
            let whole= CubicSpline.integrate coefs 1. 4.
            let partials = CubicSpline.integrate coefs 1. 2 + CubicSpline.integrate coefs 2. 3. + CubicSpline.integrate coefs 3. 4.
            Expect.floatClose Accuracy.high whole partials
                "integral should be additive across sub-intervals"

        testCase "CubicSpline.integrate returns zero for equal bounds" <| fun () ->
            let t = vector [| 0.; 1.; 2. |]
            let u = vector [| 0.; 1.; 4. |]
            let coefs = CubicSpline.interpolate CubicSpline.Natural t u
            Expect.floatClose Accuracy.high (CubicSpline.integrate coefs 1. 1.) 0.0
                "integral with equal bounds should be 0"
        
        testCase "CubicSpline.integrate reversed limits" <| fun () ->
            let t = vector [| 1.; 2.; 3.; 4. |]
            let u = vector [| 1.; 4.; 9.; 16. |]
            let coefs = CubicSpline.interpolate CubicSpline.Quadratic t u

            let forward = CubicSpline.integrate coefs 1. 4.
            let reverse = CubicSpline.integrate coefs 4. 1.

            Expect.floatClose Accuracy.high forward (-reverse)
                "integrating with reversed limits should negate the result"

        testCase "CubicSpline.integrate negative values" <| fun () ->
            let t = vector [|1.;2.;3.;4.|]
            let u = vector [|-1.;-4.;-9.;-16.|]
            let coefs = CubicSpline.interpolate CubicSpline.Quadratic t u
            // ∫[1,4] -x² dx = -21
            Expect.floatClose Accuracy.high (CubicSpline.integrate coefs 1. 4.) -21.0
                "integral of negative quadratic should be negative"
        
        testCase "CubicSpline.integrate positive and negative values" <| fun () ->
            let t = vector [|-2.;-1.;0.;1.;2.|]
            let u = vector [|-2.;-1.;0.;1.;2.|]
            let coefs = CubicSpline.interpolate CubicSpline.Quadratic t u
            // ∫[-2,2] x dx = 0
            Expect.floatClose Accuracy.high (CubicSpline.integrate coefs -2. 2.) 0.0
                "positive and negative contributions should cancel"
    ]










