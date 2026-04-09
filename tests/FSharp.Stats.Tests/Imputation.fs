module ImputationTests

open Expecto
open FSharp.Stats
open FSharp.Stats.ML

/// Tolerance for floating-point comparisons
let eps = 1e-9

[<Tests>]
let imputationTests =
    testList "Imputation" [

        testList "kNearestImpute" [

            test "imputes single missing value using nearest neighbour mean" {
                // Row 0: [1; 2; 3], Row 1: [2; 4; 6], Row 2: [4; 8; 12]
                // Query: [1; nan; 3] — missing at index 1
                // Euclidean (NaN-skipped) distances from [1;__;3] to rows:
                //   Row 0 [1;2;3]:   d² = (1-1)² + (3-3)² = 0
                //   Row 1 [2;4;6]:   d² = (1-2)² + (3-6)² = 10
                //   Row 2 [4;8;12]:  d² = (1-4)² + (3-12)² = 90
                // k=2 nearest: [1;2;3] and [2;4;6]  →  mean of index 1 = (2+4)/2 = 3
                let data = [| [|1.0; 2.0; 3.0|]; [|2.0; 4.0; 6.0|]; [|4.0; 8.0; 12.0|] |]
                let query = [| 1.0; nan; 3.0 |]
                let imputer = Imputation.kNearestImpute 2
                let result = imputer data query 1
                Expect.floatClose Accuracy.high result 3.0 "kNN-2 unweighted mean should be 3.0"
            }

            test "imputes correctly when k equals dataset size" {
                let data = [| [|0.0; 10.0|]; [|4.0; 20.0|] |]
                let query = [| 1.0; nan |]
                // k=2, mean of 10 and 20 = 15
                let imputer = Imputation.kNearestImpute 2
                let result = imputer data query 1
                Expect.floatClose Accuracy.high result 15.0 "mean of 10 and 20 should be 15"
            }
        ]

        testList "kNearestWeightedImpute" [

            test "with k=1 returns the nearest neighbour value unchanged" {
                // Only one neighbour → weight doesn't matter
                let data = [| [|0.0; 10.0|]; [|10.0; 99.0|] |]
                let query = [| 1.0; nan |]
                let invDist d = 1.0 / (d + System.Double.Epsilon)
                let imputer = Imputation.kNearestWeightedImpute FSharp.Stats.DistanceMetrics.Array.euclideanNaNSquared invDist 1
                let result = imputer data query 1
                // Nearest is [0; 10], its value at index 1 is 10.0
                Expect.floatClose Accuracy.high result 10.0 "nearest neighbour value should be 10.0"
            }

            test "inverse-distance weighting biases result toward closer neighbour" {
                // Data: [0; 10] and [4; 20]
                // Query: [1; nan]
                // Distances (euclideanNaNSquared, skip NaN):
                //   d²([0;10], [1;_]) = (1-0)² = 1.0
                //   d²([4;20], [1;_]) = (1-4)² = 9.0
                // Weights: w1=1/1=1,  w2=1/9
                // Weighted mean = (1*10 + (1/9)*20) / (1 + 1/9)
                //               = (10 + 20/9) / (10/9)
                //               = (110/9) / (10/9) = 11.0
                let data = [| [|0.0; 10.0|]; [|4.0; 20.0|] |]
                let query = [| 1.0; nan |]
                let invDist d = 1.0 / d
                let imputer = Imputation.kNearestWeightedImpute FSharp.Stats.DistanceMetrics.Array.euclideanNaNSquared invDist 2
                let result = imputer data query 1
                Expect.floatClose Accuracy.high result 11.0 "weighted average should be 11.0"
            }

            test "equal distances yield simple mean regardless of weight function" {
                // If all neighbours are equidistant the weighted mean equals the simple mean
                // [0;10] and [2;20] are both d²=1 from query [1;nan]
                let data = [| [|0.0; 10.0|]; [|2.0; 20.0|] |]
                let query = [| 1.0; nan |]
                let invDist d = 1.0 / d
                let imputer = Imputation.kNearestWeightedImpute FSharp.Stats.DistanceMetrics.Array.euclideanNaNSquared invDist 2
                let result = imputer data query 1
                // Both weights = 1/1 = 1 → equal weights → mean = (10+20)/2 = 15
                Expect.floatClose Accuracy.high result 15.0 "equal-distance weighted mean should be 15.0"
            }

            test "returns nan when dataset is empty" {
                let data : float[][] = [| |]
                let query = [| 1.0; nan |]
                let invDist d = 1.0 / (d + System.Double.Epsilon)
                let imputer = Imputation.kNearestWeightedImpute FSharp.Stats.DistanceMetrics.Array.euclideanNaNSquared invDist 3
                let result = imputer data query 1
                Expect.isTrue (System.Double.IsNaN result) "should return nan for empty dataset"
            }

            test "imputeBy with kNearestWeightedImpute replaces nans in matrix" {
                let isMissing = System.Double.IsNaN
                let rawData : seq<float[]> =
                    seq {
                        yield [| 0.0; 10.0; 100.0 |]
                        yield [| 4.0; 20.0; 200.0 |]
                        yield [| 1.0; nan;  150.0 |]
                    }
                let invDist d = 1.0 / (d + System.Double.Epsilon)
                let imputer = Imputation.kNearestWeightedImpute FSharp.Stats.DistanceMetrics.Array.euclideanNaNSquared invDist 2
                let result = Imputation.imputeBy imputer isMissing rawData
                // The third row's missing value at index 1 should be imputed
                let imputed = result.[2].[1]
                Expect.isFalse (System.Double.IsNaN imputed) "imputed value should not be nan"
                // Verify it lies in the plausible range [10, 20]
                Expect.isTrue (imputed >= 10.0 && imputed <= 20.0) "imputed value should be between 10 and 20"
            }
        ]
    ]
