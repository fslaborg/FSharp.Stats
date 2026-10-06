module Optimization


open Expecto
open System
open FsMath
open FSharp.Stats
open FSharp.Stats.Optimization




[<Tests>]
let NelderMeadTests =   
    testList "Optimization.NelderMead" [
        
        let poly (xs: Vector<float>) =
            System.Math.Pow(xs[0], 2)        
 
        // Rosenbrock's valley or Rosenbrock's banana function
        let rosenbrock (xs: Vector<float>) =
            let x, y = xs.[0], xs.[1]
            pown (1.0 - x) 2 + 100.0 * pown (y - pown x 2) 2

        // Fletcher and Powell's helic valley
        let fphv (x : Vector<float>) =
            100. * (x[2] - 10. * (atan2 x[1] x[0]) / (2. * Ops.pi))**2. + 
                (sqrt(x[0]**2. + x[1]**2.) - 1.)**2. + x[2]**2.

        // Powell's Singular Function (PSF)
        let psf (x : Vector<float>) =
          (x[0] + 10. * x[1])**2. + 5. * (x[2] - x[3])**2. + 
            (x[1] - 2. * x[2])**4. + 10.*(x[0] - x[3])**4.
               
        testList "Test rosenbrock" [

            let x0 = vector [| 1.3; 0.7; 0.8; 1.9; 1.2 |] 
            let nmc = NelderMead.NmConfig.defaultInit()   

            let optim = NelderMead.minimize nmc x0 rosenbrock 
          
            test "rosenbrock: solution value" {
                let expected = 0.0
                let actual   = optim.Solution
                Expect.floatClose Accuracy.low actual expected "rosenbrock: solution did not match"
            }

            test "rosenbrock: x1" {
                let expected =  1.0
                let actual = optim.SolutionVector[0]
                Expect.floatClose Accuracy.low actual expected "rosenbrock: x1 did not match"
            }

            test "rosenbrock: x2" {
                let expected = 1.0
                let actual = optim.SolutionVector[1]
                Expect.floatClose Accuracy.low actual expected "rosenbrock: x2 did not match"
            }
        ]

        testList "Test Fletcher" [

            let x0 = vector [| -1.0; 0.0; 0.0; |] 
            let nmc = NelderMead.NmConfig.defaultInit() 
            let optim = 
                NelderMead.minimizeWithStopCriteria nmc x0 fphv 
                    { OptimizationStop.defaultStopCriteria with MinFunctionEpsilon = 1e-24 }
          
            //test "Fletcher: solution value" {
            //    let expected = 0.
            //    let actual   = optim.Solution
            //    Expect.floatClose Accuracy.low actual expected "fletcher: solution did not match"
            //}
            //seq [0.9999272578; -2.558463341e-05; -4.073735628e-05]; #  1 0 0
            testCase "Fletcher: solution vector" <| fun () ->
                Expect.floatClose Accuracy.low optim.SolutionVector[0] 1. "fletcher: x1 did not match"
                Expect.floatClose Accuracy.low optim.SolutionVector[1] 0. "fletcher: x2 did not match"
                Expect.floatClose Accuracy.low optim.SolutionVector[2] 0. "fletcher: x3 did not match"

        ]

        testList "Test Powell's Singular Function" [

            let x0 = vector [| 3.0; -1.0; 0.0; 1.0; |] 
            let nmc = NelderMead.NmConfig.defaultInit()   

            let optim = NelderMead.minimize nmc x0 psf 
          
            test "Psf: solution value near zero" {
                // PSF minimum is at (0,0,0,0) with value 0; verify convergence
                Expect.floatClose { absolute = 1e-12; relative = 1e-6 } optim.Solution 0.0 "psf: solution should converge near 0"
            }

            testCase "v: solution vector near zero" <| fun () ->
                // PSF minimum is at (0,0,0,0); verify all components converge close to 0
                let acc = { absolute = 1e-4; relative = 1e-3 }
                Expect.floatClose acc optim.SolutionVector[0] 0. "psf: x1 should be near 0"
                Expect.floatClose acc optim.SolutionVector[1] 0. "psf: x2 should be near 0"
                Expect.floatClose acc optim.SolutionVector[2] 0. "psf: x3 should be near 0"
                Expect.floatClose acc optim.SolutionVector[3] 0. "psf: x4 should be near 0"
        ]

        testList "Test negative-minimum quadratic (issue #260)" [
            // f(x) = x^2 - 0.32x - 0.13 has minimum at x=0.16, f(0.16) ≈ -0.1556
            // Before the fix, CheckFunctionEpsilon caused early termination for negative f values
            let myFunction (xs: Vector<float>) =
                let x = xs.[0]
                x**2. - 0.32*x - 0.13

            let x0 = vector [| -0.3 |]
            let nmc = NelderMead.NmConfig.defaultInit()
            let optim = NelderMead.minimize nmc x0 myFunction

            test "negative-minimum: solution x value" {
                Expect.floatClose Accuracy.medium optim.SolutionVector[0] 0.16 "quadratic: x* should be near 0.16"
            }

            test "negative-minimum: solution function value" {
                let expected = -0.1556  // 0.16^2 - 0.32*0.16 - 0.13
                Expect.floatClose Accuracy.medium optim.Solution expected "quadratic: f(x*) should be near -0.1556"
            }
        ]
        
    ]