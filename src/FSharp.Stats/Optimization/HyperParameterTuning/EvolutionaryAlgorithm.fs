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

    /// Strategy applied when recombining a the main parent with another parent selected for cross over
    type CrossOverStrategy =
        /// Take allele from cross validation partner
        | Substitute
        /// Average value between the two parents
        | Merge
        /// Custom cross over
        | Custom of (HyperParameterValue -> HyperParameterValue -> HyperParameterValue)

        member this.Recombine(mainParent : HyperParameterValue,coParent : HyperParameterValue) =
            match this with
            | Substitute -> coParent
            | Merge -> 
                match mainParent,coParent with
                | String m, String c -> String c
                | Int m, Int c -> (m + c)/2 |> Int
                | Float m, Float c -> (m + c)/2. |> Float
                | _ -> failwith "Can't merge parent genes of differing types"
            | Custom f -> f mainParent coParent 

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
            /// Strategy applied when recombining a the main parent with another parent selected for cross over
            ///
            /// Subsitute = Take allele from cross validation partner
            ///
            /// Merge = Average value between the two parents
            CrossOverStrategy       : CrossOverStrategy
            /// Determines the size of the parent pool from which to sample for the child generation
            ///
            /// 0 = Don't do this
            ///
            /// 1 = All agents of this generation are sampled from
            SurvivalRate            : float
            /// If set to true, the survivors of one generation (mostly the fittest agents) will be kept in the next generation
            /// 
            /// If set to false, the new population will consist only of children of these survivors
            ///
            /// Setting this option to true might speed up the optimization
            KeepSurvivors           : bool
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

        static member create mtProbability coProbability coStrategy survivalRate keepSurvivors pressure parentCount =
            {
                MutationProbability     = mtProbability
                CrossoverProbability    = coProbability    
                CrossOverStrategy       = coStrategy
                SurvivalRate            = survivalRate
                KeepSurvivors           = keepSurvivors
                SelectivePressure       = pressure
                ParentCount             = parentCount
            }


    /// Create population from random values
    let generatePopulation hps (count:int) : Agent [] = 
        let rnd = System.Random()
        Array.init count (fun i -> Agent.generate(i,hps,rnd))

    /// Score the fitness of each agent, 
    ///
    /// If the agent is from a previous generation, it will not be rescored
    let evaluateAgents generation (scoringFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (population:Agent []) = 
        population
        |> Array.map(fun agent ->
            if agent.Generation < generation then
                agent
            else 
                let _,score = scoringFunction data agent.Chromosome
                {agent with Fitness = score}  
        )
    

    /// Select the agents that will survive one cycle and repopulate the next generation
    ///
    /// The greater the selectivePressure, the higher the chance for a fitter agent to survive
    let selectSurvivors (rnd : System.Random) survivalRate selectivePressure (population : Agent [])=
        let shuffled = population |> Array.shuffleFisherYates |> Array.mapi (fun i a -> a.Id,i) |> Map.ofArray
        let sorted = population |> Array.sortByDescending (fun agent -> agent.Fitness) |> Array.mapi (fun i a -> a.Id,i) |> Map.ofArray
        let survivalCount = survivalRate * (float population.Length) |> int
        population
        |> Array.map (fun a -> 
            let fitnessScore = selectivePressure * (float sorted.[a.Id])
            let randomScore = (1.-selectivePressure) * (float shuffled.[a.Id])
            a,
            fitnessScore + randomScore
        )
        |> Array.sortByDescending snd
        |> Array.map fst
        |> Array.take survivalCount

    /// Randomly sample parents form the survivors for a child.
    let selectParentAgents (rnd : System.Random) parentCount (population : Agent []) = 
        
        Array.sampleWithOutReplacement rnd population parentCount

    /// Pair parent agents to create an offspring agent.
    ///
    /// Chromosome of fittest parent is chosen, alleles of other parents are picked according to crossOverProbability
    let pairAgents (rnd: System.Random) coProbability (coStrategy : CrossOverStrategy) id generation (parents : Agent []) =
        let fittest = parents |> Array.maxBy (fun p -> p.Fitness)
        let others = parents |> Array.filter (fun p -> p.Id <> fittest.Id)

        let recombinedChromosome =
            fittest.Chromosome
            |> List.mapi (fun i allele ->
                if rnd.NextDouble() < coProbability then 
                    let parentI = rnd.Next(0,others.Length - 1)
                    coStrategy.Recombine(allele,others.[parentI].Chromosome.[i])
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

        let survivers = selectSurvivors rnd eaOptions.SurvivalRate eaOptions.SelectivePressure population

        if eaOptions.KeepSurvivors then

            Array.init (population.Length - survivers.Length) (fun i ->
                selectParentAgents rnd eaOptions.ParentCount survivers
                |> pairAgents rnd eaOptions.CrossoverProbability eaOptions.CrossOverStrategy i generation
                |> mutateAgent rnd eaOptions.MutationProbability hyperParams
            )
            |> Array.append survivers
        else
            Array.init population.Length (fun i ->
                selectParentAgents rnd eaOptions.ParentCount survivers
                |> pairAgents rnd eaOptions.CrossoverProbability eaOptions.CrossOverStrategy i generation
                |> mutateAgent rnd eaOptions.MutationProbability hyperParams
            )


    /// Perform an evolutionary optimization, returning the hyper parameters for which the model performance was maximized
    let evolutionaryAlgorithmMaximize (eaOptions : EvolutionaryAlgorithmOptions) (populationSize : int) (generations : int) (scoringFunction : HPScoringFunction<'Data,'T>) (data : 'Data) (hyperParams : HyperParameter list) : HyperParameterTuningResult<'T> =
        let rnd = System.Random()

        let rec runEvolutionaryCycle i population = 
            if i <= generations then
                population
                |> evaluateAgents i scoringFunction data
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
                |> evaluateAgents i minimizationFunction data
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
                |> evaluateAgents i fitnessFunction data
                |> evolveAgents rnd eaOptions hyperParams i
                |> fun newPopulation -> runEvolutionaryCycle (i+1) (Array.append allAgents newPopulation) newPopulation
            else
                allAgents
                
        generatePopulation hyperParams populationSize
        |> fun population -> runEvolutionaryCycle generations population population


