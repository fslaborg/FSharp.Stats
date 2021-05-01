namespace FSharp.Stats.Optimization.HyperParameterTuning

open FSharp.Stats

/// Functions for finding the optimal hyper parameters using an evolutionary algorithm
module EvolutionaryAlgorihm = 

    type Agent = 
        {
            Id          : int
            Generation  : int
            Chromosome  : HyperParameterValue list
            Fitness     : float
        }

        static member create i generation chromosome fitness =
            {
                Id          = i
                Generation  = generation
                Chromosome  = chromosome
                Fitness     = fitness        
            }

        static member generate (i,hps : HyperParameter list, ?Rnd : System.Random) =
            let rnd = match Rnd with | Some rnd -> rnd | None -> System.Random()
            let chromosome = hps |> List.map (RandomSearch.getRandomHPValue rnd)

            Agent.create i 0 chromosome 0.


    type EvolutionaryAlgorithmOptions =
        {
            /// Probability with which a gene will be mutated after recombination
            ///
            /// 0 = no mutations
            ///
            /// 1 = all genes are randomly mutated
            MutationProbability     : float 
            /// Probability with which recombination happens between a gene will be mutated after recombination
            ///
            /// 0 = only alleles of fittest parent are passed on
            ///
            /// 0.5 = half of the alleles are from the fittest parent, half of the alleles are from the other parents 
            ///
            /// 1 = only alleles of other parents are passed on
            CrossoverProbability    : float
            /// Determines the size of the parent pool from which to sample
            ///
            /// 0 = Don't do this
            ///
            /// 1 = All agents of this generation are sampled from
            SurvivalRate            : float
            /// Used in combination with survival rate. Determines, how much fitness affects survival
            ///
            /// 0 = Surviving parents are randomly chosen from population
            ///
            /// 1 = Only the fittest survive
            SelectivePressure       : float
            /// Number of parents per child
            /// 
            /// 1 = Child will be parent plus mutations
            ///
            /// 2 = Most similar to real biology, if used with CrossoverProbability = 0.5
            /// 
            /// n = Chromosome of fittest parents gets selected and recombined by other parents based on crossover probability
            ParentCount             : int
        }

        static member create mtProbability coProbability survivalRate pressure parentCount =
            {
                MutationProbability     = mtProbability
                CrossoverProbability    = coProbability     
                SurvivalRate            = survivalRate
                SelectivePressure       = pressure
                ParentCount             = parentCount
            }


    /// Create population from random values
    let generatePopulation hps (count:int) : Agent [] = 
        let rnd = System.Random()
        Array.init count (fun i -> Agent.generate(i,hps,rnd))

    /// Score the fitness of each agent
    let evaluateAgents (scoringFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (population:Agent []) = 
        population
        |> Array.map(fun agent ->
            let metaInfo,score = scoringFunction data agent.Chromosome
            HyperParameterTuningResult<'T>.create agent.Chromosome score metaInfo, {agent with Fitness = score}  
        )


    /// Randomly choose parents for a child.
    ///
    /// The greater the selectivePressure, the higher the probability, that a parent is choosen from the fittestAgents group
    let selectParentAgents (rnd : System.Random) parentCount selectivePressure (fittestAgents : Agent []) (otherAgents : Agent []) = 

        let rec selectParents (parents : Agent []) fittestAgents otherAgents =
            if parents.Length = parentCount then
                parents
            else
                if rnd.NextDouble() < selectivePressure then
                    let add, rest =fittestAgents |> Array.shuffleFisherYates |> Array.splitAt 1
                    selectParents (Array.append parents add) rest otherAgents
                else
                    let add, rest = otherAgents |> Array.shuffleFisherYates |> Array.splitAt 1
                    selectParents (Array.append parents add) fittestAgents rest

        selectParents [||] fittestAgents otherAgents

    /// Pair parent agents to create an offspring agent.
    ///
    /// Chromosome of fittest parent is chosen, alleles of other parents are picked according to crossOverProbability
    let pairAgents (rnd: System.Random) coProbability id generation (parents : Agent []) =
        let fittest = parents |> Array.maxBy (fun p -> p.Fitness)
        let others = parents |> Array.filter (fun p -> p.Id <> fittest.Id)

        let recombinedChromosome =
            fittest.Chromosome
            |> List.mapi (fun i allele ->
                if rnd.NextDouble() < coProbability then 
                    let parentI = rnd.Next(0,others.Length - 1)
                    others.[parentI].Chromosome.[i]
                else
                    allele
            )

        Agent.create id generation recombinedChromosome 0.

    /// Alleles of agent seperately are changed to random value with a probability of mtProbability
    let mutateAgent (rnd : System.Random) mtProbability (hyperParameters : HyperParameter list) (agent : Agent) =
        let newChromosome = 
            agent.Chromosome 
            |> List.mapi (fun i allele ->
                if rnd.NextDouble() < mtProbability then 
                    hyperParameters.[i]
                    |> RandomSearch.getRandomHPValue rnd
                else
                    allele
            )
        {
            agent with Chromosome = newChromosome
        }


    /// Create a child generation based on the parent generation and given parameters
    let evolveAgents (rnd: System.Random) (eaOptions : EvolutionaryAlgorithmOptions) (hyperParams : HyperParameter list) generation (population : Agent []) : Agent [] = 

        let survivalCount = eaOptions.SurvivalRate * (float population.Length) |> int
        let fittestAgents,otherAgents = population |> Array.sortByDescending (fun a -> a.Fitness) |> Array.splitAt survivalCount

        Array.init population.Length (fun i ->
            selectParentAgents rnd eaOptions.ParentCount eaOptions.SelectivePressure fittestAgents otherAgents
            |> pairAgents rnd eaOptions.CrossoverProbability i generation
            |> mutateAgent rnd eaOptions.MutationProbability hyperParams
        )


    /// Perform an evolutionary optimization, returning the hyper parameters for which the model performance was maximized
    let evolutionaryAlgorithmMaximize (eaOptions : EvolutionaryAlgorithmOptions) (populationSize : int) (generations : int) (scoringFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (hyperParams : HyperParameter list) : HyperParameterTuningResult<'T> =
        let rnd = System.Random()

        let rec runEvolutionaryCycle i population = 
            if i <= generations then
                population
                |> evaluateAgents scoringFunction data
                |> Array.map snd
                |> evolveAgents rnd eaOptions hyperParams i
                |> runEvolutionaryCycle (i+1)
            else
                population
                
        generatePopulation hyperParams populationSize
        |> runEvolutionaryCycle generations
        |> Array.map (fun agent -> 
            let (metaInfo,score) = scoringFunction data agent.Chromosome
            HyperParameterTuningResult<'T>.create
                agent.Chromosome
                score
                metaInfo
        )
        |> Array.maxBy (fun result -> result.Score)

    /// Perform an evolutionary optimization, returning the hyper parameters for which the model performance was maximized
    let evolutionaryAlgorithmMinimize (eaOptions : EvolutionaryAlgorithmOptions) (populationSize : int) (generations : int) (scoringFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (hyperParams : HyperParameter list) : HyperParameterTuningResult<'T> =
        let rnd = System.Random()
        let minimizationFunction data hyperParams = 
            scoringFunction data hyperParams 
            |> fun (metaInfo,score) -> metaInfo, -score

        let rec runEvolutionaryCycle i population = 
            if i <= generations then
                population
                |> evaluateAgents minimizationFunction data
                |> Array.map snd
                |> evolveAgents rnd eaOptions hyperParams i
                |> runEvolutionaryCycle (i+1)
            else
                population
                
        generatePopulation hyperParams populationSize
        |> runEvolutionaryCycle generations
        |> Array.map (fun agent -> 
            let (metaInfo,score) = scoringFunction data agent.Chromosome
            HyperParameterTuningResult<'T>.create
                agent.Chromosome
                score
                metaInfo
        )
        |> Array.minBy (fun result -> result.Score)


    /// Perform an evolutionary optimization, returning all agents of all generations
    let evolutionaryAlgorithm (eaOptions : EvolutionaryAlgorithmOptions) (populationSize : int) (generations : int) (fitnessFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (hyperParams : HyperParameter list) : Agent [] =
        let rnd = System.Random()

        let rec runEvolutionaryCycle i allAgents population = 
            if i <= generations then
                population
                |> evaluateAgents fitnessFunction data
                |> Array.map snd
                |> evolveAgents rnd eaOptions hyperParams i
                |> fun newPopulation -> runEvolutionaryCycle (i+1) (Array.append allAgents newPopulation) newPopulation
            else
                allAgents
                
        generatePopulation hyperParams populationSize
        |> fun population -> runEvolutionaryCycle generations population population


