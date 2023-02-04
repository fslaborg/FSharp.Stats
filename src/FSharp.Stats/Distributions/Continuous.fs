namespace FSharp.Stats.Distributions

// Source: FSharp.MathTools
open System
open FSharp.Stats
open FSharp.Stats.Ops

// Continuous probability distributions
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module ContinuousDistribution = 

    /// Initializes a Normal distribution        
    let normal mu sigma = Continuous.Normal.Init mu sigma
 
    /// Initializes a Chi distribution        
    let chi dof = Continuous.Chi.Init dof

    /// Initializes a ChiSquared distribution        
    let chiSquared dof = Continuous.ChiSquared.Init dof
 
    /// Initializes a uniform distribution        
    let uniform min max = Continuous.Uniform.Init min max

    /// Initializes a Exponential distribution        
    let exponential lambda = Continuous.Exponential.Init lambda

    /// Initializes a Gamma distribution        
    let gamma alpha beta =
        Continuous.Gamma.Init alpha beta  

    /// Initializes a Normal distribution        
    let logNormal mu sigma = Continuous.LogNormal.Init mu sigma

    /// Initializes a Beta distribution        
    let beta alpha beta = Continuous.Beta.Init alpha beta

    /// Initializes a Student's T-distribution        
    let studentT mu tau dof = Continuous.StudentT.Init mu tau dof
  
    /// Initializes a F-distribution         
    let f dof1 dof2 = Continuous.F.Init dof1 dof2

    /// Initializes a studentized range distribution.     
    /// Accuracy defines the number of steps within the CDF integration (Recommended: 1k-10k, default: 2k). pValue accuracy is minimum 3 digits for v>3.
    /// q:qValue r:number of treatments v:df (n-r) c:1.   
    /// Integration can be performed in parallel using PSeq
    let studentizedRange r v c accuracy computeParallel = Continuous.StudentizedRange.Init r v c accuracy computeParallel
    

    /// Initializes a multivariate normal distribution with mean mu and covariance matrix sigma          
    let multivariateNormal mu sigma = Continuous.MultivariateNormal.Init mu sigma

    type Tails =
        | OneTailed
        | TwoTailed

    let getCriticalTValue df significanceLevel tailed =
        let cdf t = 
            let alpha =
                match tailed with
                | Tails.OneTailed -> significanceLevel
                | Tails.TwoTailed -> significanceLevel / 2.
            studentT 0. 1. df
            |> fun d -> alpha - d.CDF t
        Optimization.Bisection.tryFindRoot cdf 0.0000001 -1000. 0. 10000
        |> fun tValue -> 
            match tValue with
                | None -> failwithf "Critical t value could not be determined (increase maxIterations or decrease lower bound)."
                | Some t -> Math.Abs t












  


// ######
// Dirichlet distribution
// ######


//    // Dirichlet distribution helper functions.
//    let dirichletCheckParam (alpha: vector) =
//        let ok = Vector.fold (fun acc a -> if a < 0.0 then false else acc) true alpha
//        if (not ok) then failwith "Dirichlet distribution should be parametrized by a vector alpha > 0.0."
//    
//    /// Beta distribution
//    type Dirichlet =
//        static member Mean (alpha: vector) =
//            dirichletCheckParam alpha
//            let s = 1.0 / (Vector.sum alpha)
//            alpha * s
//        static member Covariance (alpha: vector) =
//            dirichletCheckParam alpha
//            let s = (Vector.sum alpha)
//            let n = s * s * (s + 1.0)
//            let p = Vector.length alpha
//            Matrix.init p p (fun i j ->
//                                if i = j then
//                                    alpha.[i] * (s - alpha.[i]) / n
//                                else
//                                    - alpha.[i] * alpha.[j] / n)
//        static member Sample (alpha: vector) =
//            dirichletCheckParam alpha
//            let p = Vector.length alpha
//            let gv = Vector.init p (fun i -> Gamma.Sample alpha.[i] 1.0)
//            let s = Vector.sum gv
//            Vector.init p (fun i -> gv.[i] / s)
//        static member PDF (alpha: vector) (x: vector) =
//            dirichletCheckParam alpha
//            if not (Vector.fold (fun acc a -> if a < 0.0 || a > 1.0 then false else acc) true alpha) then
//                0.0
//            else
//                let t = Vector.foldi (fun i acc a -> acc * (x.[i] ** (alpha.[i] - 1.0)) / (Core.Gamma alpha.[i])) 1.0 alpha
//                let s = (Vector.sum alpha)
//                t * (Core.Gamma s)
//        static member CDF (alpha: vector) (x: vector) =
//            dirichletCheckParam alpha
//            failwith "Not implemented yet."
//            0.0
//        static member Support (alpha: vector) =
//            dirichletCheckParam alpha
//            let p = Vector.length alpha
//            Vector.Generic.create p (0.0, 1.0)
//
//    /// Initializes a uniform distribution        
//    let uniform min max =
//        { new Distribution<float,float> with
//            member d.Mean              = Uniform.Mean min max
//            member d.StandardDeviation = Uniform.StandardDeviation min max   
//            member d.Variance          = Uniform.Variance min max
//            //member d.CoVariance        = Uniform.CoVariance min max  
//            member d.Sample ()         = Uniform.Sample min max
//            member d.PDF x             = Uniform.PDF min max x           
//            member d.CDF x             = Uniform.CDF min max x         
//        }   


