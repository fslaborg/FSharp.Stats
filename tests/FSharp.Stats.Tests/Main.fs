module FSharp.Stats.Tests

open Expecto

[<EntryPoint>]
let main argv =
    //================================ Matrix ===============================================================
    Tests.runTestsWithCLIArgs [] argv MatrixTests.floatImplementationDenseTests |> ignore
    
    //================================ Vector ===============================================================
    Tests.runTestsWithCLIArgs [] argv VectorTests.covarianceTests               |> ignore

    //================================ RowVector ============================================================
    Tests.runTestsWithCLIArgs [] argv RowVectorTests.floatImplementationTests   |> ignore
    //================================ DistanceMetrics ======================================================
    Tests.runTestsWithCLIArgs [] argv DistanceMetricsTests.euclidianseqfunctiontests   |> ignore
    Tests.runTestsWithCLIArgs [] argv DistanceMetricsTests.euclidianvecfunctiontests   |> ignore
    Tests.runTestsWithCLIArgs [] argv DistanceMetricsTests.euclidianarrayfunctiontests   |> ignore
    Tests.runTestsWithCLIArgs [] argv DistanceMetricsTests.cityblockseqfunctiontests   |> ignore
    Tests.runTestsWithCLIArgs [] argv DistanceMetricsTests.cityblockvectorfunctiontests   |> ignore
    Tests.runTestsWithCLIArgs [] argv DistanceMetricsTests.cityblockarrayfunctiontests   |> ignore
    Tests.runTestsWithCLIArgs [] argv DistanceMetricsTests.Levenshteindistancetest   |> ignore
    //=========================== Special Functions =========================================================    
    Tests.runTestsWithCLIArgs [] argv SpecialFunctionsTests.gammaFunctionsTests |> ignore
    Tests.runTestsWithCLIArgs [] argv SpecialFunctionsTests.betaFunctionsTests  |> ignore
    Tests.runTestsWithCLIArgs [] argv SpecialFunctionsTests.factorialTests  |> ignore
    //================================ Algebra ==============================================================
    Tests.runTestsWithCLIArgs [] argv LinearAlgebraTests.managedSVDTests   |> ignore
    Tests.runTestsWithCLIArgs [] argv LinearAlgebraTests.nullspace         |> ignore
    
    //================================== List ===============================================================
    Tests.runTestsWithCLIArgs [] argv ListTests.medianTests |> ignore
    Tests.runTestsWithCLIArgs [] argv ListTests.meanTests   |> ignore

    //================================== Array ==============================================================
    Tests.runTestsWithCLIArgs [] argv ArrayTests.medianTests   |> ignore
    Tests.runTestsWithCLIArgs [] argv ArrayTests.dropNanTests   |> ignore
    Tests.runTestsWithCLIArgs [] argv ArrayTests.linspaceTests   |> ignore

    //================================== Interval ===========================================================
    Tests.runTestsWithCLIArgs [] argv IntervalTests.intervalTests   |> ignore

    //================================= Seq ==============================================================
    Tests.runTestsWithCLIArgs [] argv SeqTests.medianTests |> ignore
    Tests.runTestsWithCLIArgs [] argv SeqTests.meanTests   |> ignore
    Tests.runTestsWithCLIArgs [] argv SeqTests.meanQuadraticTests   |> ignore
    Tests.runTestsWithCLIArgs [] argv SeqTests.geomspaceTests   |> ignore


    //============================= Distributions ===========================================================
    Tests.runTestsWithCLIArgs [] argv DistributionsTests.distanceFunctionsTests |> ignore
    Tests.runTestsWithCLIArgs [] argv DistributionsTests.chiTests               |> ignore
    Tests.runTestsWithCLIArgs [] argv DistributionsTests.chiSquaredTests        |> ignore
    //Tests.runTestsWithCLIArgs [] argv DistributionsTests.studentizedRangeTests  |> ignore //Test ommitted due to extremely long runtime of CodeCov.
    Tests.runTestsWithCLIArgs [] argv DistributionsTests.multivariateNormalTests|> ignore
    Tests.runTestsWithCLIArgs [] argv DistributionsTests.bandWithTests|> ignore

    Tests.runTestsWithCLIArgs [] argv DistributionsTests.exponentialTests       |> ignore
    Tests.runTestsWithCLIArgs [] argv DistributionsTests.bernoulliTests |> ignore
    Tests.runTestsWithCLIArgs [] argv DistributionsTests.binomialTests          |> ignore 
    //Tests.runTestsWithCLIArgs [] argv DistributionsTests.logNormal |> ignore
    Tests.runTestsWithCLIArgs [] argv DistributionsEmpiricalTests.empiricalTests |> ignore


    //============================= Distributions Continuous ================================================
    Tests.runTestsWithCLIArgs [] argv DistributionsContinuousTests.GammaDistributionTests |> ignore
    Tests.runTestsWithCLIArgs [] argv DistributionsContinuousTests.BetaDistributionTests  |> ignore

    //============================= Distributions Discrete ================================================
    Tests.runTestsWithCLIArgs [] argv DistributionsDiscreteTests.hypergeometricTests      |> ignore
    Tests.runTestsWithCLIArgs [] argv DistributionsDiscreteTests.poissonDistributionTests |> ignore
    Tests.runTestsWithCLIArgs [] argv DistributionsDiscreteTests.negBinomDistribution_failuresTests |> ignore
    Tests.runTestsWithCLIArgs [] argv DistributionsDiscreteTests.negBinomDistribution_trialsTests |> ignore


    //=============================== Correlation ===========================================================
    Tests.runTestsWithCLIArgs [] argv CorrelationTests.kendallCorrelationTests |> ignore
    Tests.runTestsWithCLIArgs [] argv CorrelationTests.pearsonCorrelationTests |> ignore
    
    //=============================== Covariance ============================================================
    Tests.runTestsWithCLIArgs [] argv CovarianceTests.sequenceTests |> ignore
    Tests.runTestsWithCLIArgs [] argv CovarianceTests.listTests     |> ignore
    Tests.runTestsWithCLIArgs [] argv CovarianceTests.arrayTests    |> ignore
    Tests.runTestsWithCLIArgs [] argv CovarianceTests.matrixTests   |> ignore
    
    //================================ Testing ==============================================================
    //Tests.runTestsWithCLIArgs [] argv TestingTests.testPostHocTests |> ignore 
    Tests.runTestsWithCLIArgs [] argv TestingTests.hTestTests       |> ignore
    Tests.runTestsWithCLIArgs [] argv TestingTests.chiSquaredTests  |> ignore
    Tests.runTestsWithCLIArgs [] argv TestingTests.pearsonTests     |> ignore
    Tests.runTestsWithCLIArgs [] argv TestingTests.tTestTests     |> ignore
    Tests.runTestsWithCLIArgs [] argv TestingTests.fTestTests           |> ignore
    Tests.runTestsWithCLIArgs [] argv TestingTests.friedmanTestTests    |> ignore
    Tests.runTestsWithCLIArgs [] argv TestingTests.wilcoxonTestTests    |> ignore
    Tests.runTestsWithCLIArgs [] argv TestingTests.tTestTests     |> ignore
    Tests.runTestsWithCLIArgs [] argv TestingTests.SAMTests |> ignore
    Tests.runTestsWithCLIArgs [] argv ConfidenceIntervalTests.ci |> ignore
    
    //================================== ML =================================================================
    //SimilarityMetrics
    Tests.runTestsWithCLIArgs [] argv MLTests.SimilarityMetrics.jaccardIndexTests           |> ignore
    Tests.runTestsWithCLIArgs [] argv MLTests.SimilarityMetrics.overlapIndexTests           |> ignore
    Tests.runTestsWithCLIArgs [] argv MLTests.SimilarityMetrics.sorensenDiceIndexTests      |> ignore
    Tests.runTestsWithCLIArgs [] argv MLTests.SimilarityMetrics.tverskyIndexTests           |> ignore
    Tests.runTestsWithCLIArgs [] argv MLTests.SimilarityMetrics.tverskySymmetricIndexTests  |> ignore
    Tests.runTestsWithCLIArgs [] argv MLTests.PCA.pcaTests |> ignore

    //================================== Fitting ============================================================
    Tests.runTestsWithCLIArgs [] argv FittingTests.nonLinearRegressionTests     |> ignore
    Tests.runTestsWithCLIArgs [] argv FittingTests.leastSquaresCholeskyTests    |> ignore
    Tests.runTestsWithCLIArgs [] argv FittingTests.splineTests                  |> ignore
    
    //================================== Interpolation ============================================================
    Tests.runTestsWithCLIArgs [] argv InterpolationTests.cubicInterpolationTests           |> ignore
    Tests.runTestsWithCLIArgs [] argv InterpolationTests.polynomialInterpolationTests      |> ignore
    
    
    //================================== Integration ============================================================
    Tests.runTestsWithCLIArgs [] argv IntegrationTests.numericalIntegrationTests      |> ignore

    //================================== Integration ============================================================
    Tests.runTestsWithCLIArgs [] argv RankTests.rankTests      |> ignore

    //================================== Quantile ============================================================
    Tests.runTestsWithCLIArgs [] argv QuantileTests.quantileDefaultTests  |> ignore
    Tests.runTestsWithCLIArgs [] argv QuantileTests.quantileTests         |> ignore
    Tests.runTestsWithCLIArgs [] argv QuantileTests.quantileOfSortedTests |> ignore

    //================================ Formatting ===============================================================
    Tests.runTestsWithCLIArgs [] argv FormattingTests.formatValueTests |> ignore
    Tests.runTestsWithCLIArgs [] argv FormattingTests.formatTableTests |> ignore
    Tests.runTestsWithCLIArgs [] argv FormattingTests.matrixFormattingtests |> ignore
    
    //================================ Signal ===============================================================
    Tests.runTestsWithCLIArgs [] argv SignalTests.outlierTests  |> ignore
    Tests.runTestsWithCLIArgs [] argv SignalTests.normalizationTests  |> ignore
    Tests.runTestsWithCLIArgs [] argv SignalTests.binningTests  |> ignore
    Tests.runTestsWithCLIArgs [] argv SignalTests.paddingTests  |> ignore

    0