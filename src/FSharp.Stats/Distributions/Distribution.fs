namespace FSharp.Stats.Distributions

open FSharp.Stats

module Parameters = 

    type Normal = {
        Mean              : float
        StandardDeviation : float }

    type Gamma = {
        Alpha : float
        Beta  : float }

    type Beta = {
        Alpha : float
        Beta  : float }

    type F = {
        DOF1 : float
        DOF2 : float}

    type StudentT = {
        Mean              : float
        StandardDeviation : float
        DOF               : float}

    type StudentizedRange = {
        R               : float
        V               : float
        C               : float
        Accuracy        : int option
        ComputeParallel : bool}

    type LogNormal = {
        Mean              : float
        StandardDeviation : float}
    
    type MultivariateNormal = {
        Mean              : vector
        StandardDeviation : matrix}

    type Exponential = {
        Lambda : float}

    type Uniform = {
        Min : float
        Max : float}
    
    type Chi = {
        DOF : float}

    type ChiSquared = {
        DOF : float}

    type Bernoulli = {
        P : float}
    
    type Binomial = {
        P : float
        N : int}
    
    type NegativeBinomial = {
        R : int
        P : float}

    type Hypergeometric = {
        /// popuulation
        N : int
        /// successes
        K : int
        /// sample size
        n : int}

    type Poisson = {
        Lambda : float}

type DistributionParameters =
    // Continuous distributions
    | Normal of Parameters.Normal
    | Gamma of Parameters.Gamma
    | Beta of Parameters.Beta
    | F of Parameters.F
    | StudentT of Parameters.StudentT
    | LogNormal of Parameters.LogNormal
    | StudentizedRange of Parameters.StudentizedRange
    | MultivariateNormal of Parameters.MultivariateNormal
    | Exponential of Parameters.Exponential
    | Uniform of Parameters.Uniform
    | Chi of Parameters.Chi
    | ChiSquared of Parameters.ChiSquared
    // Discrete distributions
    | Bernoulli of Parameters.Bernoulli
    | Binomial of Parameters.Binomial
    | NegativeBinomial of Parameters.NegativeBinomial
    | Hypergeometric of Parameters.Hypergeometric
    | Poisson of Parameters.Poisson

/// Interface which every probability distribution must implement.
type Distribution<'a> =
    abstract Mean              : 'a
    abstract StandardDeviation : 'a
    abstract Variance          : 'a
    abstract CDF               : 'a -> float
    abstract InvCDF            : 'a -> float
    //abstract PDFLn   : 'a -> float
    //abstract CDFLn   : 'a -> float
    abstract ToString          : unit -> string 
    abstract Parameters        : DistributionParameters

/// Interface for continuous probability distributions.
type ContinuousDistribution<'a,'b> =    
    inherit Distribution<'a> 
    abstract Mode   :   'b
    abstract PDF    :   'b -> float
    abstract Sample : unit -> 'b

/// Interface for discrete probability distributions.
type DiscreteDistribution<'a,'b> =    
    inherit Distribution<'a>
    abstract Mode   :   'b
    abstract PMF    :   'b -> float
    abstract Sample : unit -> 'b
