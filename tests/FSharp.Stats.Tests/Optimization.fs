module Optimization


open Expecto
open System
open FSharp.Stats
open FSharp.Stats.Optimization




[<Tests>]
let NelderMeadTests =   
    testList "Optimization.NelderMead" [
        
        let poly (xs: vector) =
            System.Math.Pow(xs[0], 2)        
 
        // Rosenbrock's valley or Rosenbrock's banana function
        let rosenbrock (xs: vector) =
            let x, y = xs.[0], xs.[1]
            pown (1.0 - x) 2 + 100.0 * pown (y - pown x 2) 2

        // Fletcher and Powell's helic valley
        let fphv (x : vector) =
            100. * (x[2] - 10. * (atan2 x[1] x[0]) / (2. * pi))**2. + 
                (sqrt(x[0]**2. + x[1]**2.) - 1.)**2. + x[2]**2.

        // Powell's Singular Function (PSF)
        let psf (x : vector) =
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

        //testList "Test Fletcher" [

        //    let x0 = vector [| -1.0; 0.0; 0.0; |] 
        //    let nmc = 
        //        { 
        //           NelderMead.NmConfig.defaultInit() 
        //           with 
        //              Rho = 10.
        //              Psi = 1.5
        //        }

        //    let optim = NelderMead.minimize nmc x0 fphv 
          
        //    test "Fletcher: solution value" {
        //        let expected = 6.950929861e-09
        //        let actual   = optim.Solution
        //        Expect.floatClose Accuracy.low actual expected "fletcher: solution did not match"
        //    }
        //    //seq [0.9999272578; -2.558463341e-05; -4.073735628e-05]; #  1 0 0
        //    testCase "Fletcher: solution vector" <| fun () ->
        //        Expect.floatClose Accuracy.low optim.SolutionVector[0] 1. "fletcher: x1 did not match"
        //        Expect.floatClose Accuracy.low optim.SolutionVector[1] 0. "fletcher: x2 did not match"
        //        Expect.floatClose Accuracy.low optim.SolutionVector[2] 0. "fletcher: x3 did not match"

        //]

        testList "Test Powell's Singular Function" [

            let x0 = vector [| 3.0; -1.0; 0.0; 1.0; |] 
            let nmc = NelderMead.NmConfig.defaultInit()   

            let optim = NelderMead.minimize nmc x0 psf 
          
            test "Psf: solution value" {
                let expected = 5.675294665e-09
                let actual   = optim.Solution
                Expect.floatClose Accuracy.low actual expected "psf: solution did not match"
            }
            //
            testCase "v: solution vector" <| fun () ->
                let expected = [|-0.0005532762725; 5.500401575e-05; -0.002250883404;-0.002282958824|]
                
                Expect.floatClose Accuracy.low optim.SolutionVector[0] expected[0] "psf: x1 did not match"
                Expect.floatClose Accuracy.low optim.SolutionVector[1] expected[1] "psf: x2 did not match"
                Expect.floatClose Accuracy.low optim.SolutionVector[2] expected[2] "psf: x3 did not match"
                Expect.floatClose Accuracy.low optim.SolutionVector[3] expected[3] "psf: x4 did not match"
        ]
        
    ]