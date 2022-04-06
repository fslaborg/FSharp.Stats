(**
---
title: Comparison metrics
index: 14
category: Documentation
categoryindex: 0
---
*)

(***hide***)

(*** condition: prepare ***)
#I "../src/FSharp.Stats/bin/Release/netstandard2.0/"
#r "FSharp.Stats.dll"
#r "nuget: Plotly.NET, 2.0.0-preview.16"

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 2.0.0-preview.16"
#r "nuget: Plotly.NET.Interactive, 2.0.0-preview.16"
#r "nuget: FSharp.Stats"
#endif // IPYNB

(**
# Evaluating predictions and tests

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/fslaborg/FSharp.Stats/gh-pages?filepath=Integration.ipynb)

FSharp.Stats contains a collection for assessing both binary and multi-label comparisons, for example the results of a binary/multi-label classification or the results of a statistical test.

Usually, using the functions provided by the `ComparisonMetrics` module should be enough, but for clarity this documentation also introduces the `BinaryConfusionMatrix` and `MultiLabelConfusionMatrix` types that are used to derive the `ComparisonMetrics.`

## Confusion matrices

See also: https://en.wikipedia.org/wiki/Confusion_matrix

Confusion matrices can be used to count and visualize the outcomes of a prediction against the actual 'true' values and therefore assess the prediction quality.

Each row of the matrix represents the instances in an actual class while each column represents the instances in a predicted class, or vice versa. The name stems from the fact that it makes it easy to see whether the system is confusing two classes (i.e. commonly mislabeling one as another).

### Binary confusion matrix

A binary confusion matrix is a special kind of contingency table, with two dimensions ("actual" and "predicted"), and identical sets of "classes" in both dimensions (each combination of dimension and class is a variable in the contingency table).

let for example the actual labels be the set 

$$actual = (1,1,1,1,0,0,0)$$ 

and the predictions 

$$predicted = (1,1,1,0,1,0,0)$$

a binary confusion matrix can be filled by comparing actual and predicted values at their respective indices:

|       |      | predicted | |
| ---   |  --- | ---  |  ---  |
|       |      | True | False |
|actual | True |   3  |   1   |
|       | False|   1  |   2   |

A whole array of prediction/test evaluation metrics can be derived from binary confusion matrices, which are all based on the 4 values of the confusion matrix: 

- TP (True Positives, the actual true labels predicted correctly as true)
- TN (True Negatives, the actual false labels predicted correctly as false)
- FP (False Positives, the actual false labels incorrectly predicted as true)
- TP (False Negatives, the actual true labels incorrectly predicted as false)

|       |      | Predicted | |
| ---   |  --- | ---  |  ---  |
|       |      | True | False |
|Actual | True |  TP  |   FN  |
|       | False|  FP  |   TN  |

These 4 base metrics are in principle what comprises the record type `BinaryConfusionMatrix`. 

A BinaryConfusionMatrix can be created in various ways :

- from predictions and actual labels of any type using `BinaryConfusionMatrix.fromPredictions`, additionally passing which label is the "positive" label
*)
let actual = [1;1;1;1;0;0;0]
let predicted = [1;1;1;0;1;0;0]

open FSharp.Stats.Testing

BinaryConfusionMatrix.ofPredictions(1,actual,predicted)
(***include-it***)

(**
- from boolean predictions and actual labels using `BinaryConfusionMatrix.fromPredictions`
*)
let actualBool = [true;true;true;true;false;false;false]
let predictedBool = [true;true;true;false;true;false;false]

BinaryConfusionMatrix.ofPredictions(actualBool,predictedBool)
(***include-it***)

(**
- directly from obtained TP/TN/FP/FN values using `BinaryConfusionMatrix.create`
*)

BinaryConfusionMatrix.create(tp=3,tn=2,fp=1,fn=1)
(***include-it***)

(**
There are more things you can do with `BinaryConfusionMatrix`, but most use cases are covered and abstracted by `ComparisonMetrics` (see [below](#Comparison-metrics)).

### Multi label confusion matrix

Confusion matrix is not limited to binary classification and can be used in multi-class classifiers as well, increasing both dimensions by the amount of additional labels.

let for example the actual labels be the set 

$$actual = (A,A,A,A,A,B,B,B,C,C,C,C,C,C)$$ 

and the predictions 

$$predicted = (A,A,A,B,C,B,B,A,C,C,C,C,A,A)$$

a multi-label confusion matrix can be filled by comparing actual and predicted values at their respective indices:

|        |         | Predicted |         |         |
| ---    | ---     | ---       | ---     | ---     |
|        |         |  Label A  | Label B | Label C |
| Actual | Label A |     3     |    1    |    1    |
|        | Label B |     1     |    2    |    0    |
|        | Label C |     2     |    0    |    4    |

A `MultiLabelConfusionMatrix` can be created either

- from the labels and a confusion matrix (`Matrix<int>`, note that the index in the label array will be assigned for the column/row indices of the matrix, and that the matrix must be square and of the same dimensions as the label array)
*)
open FSharp.Stats

let mlcm =
    MultiLabelConfusionMatrix.create(
        labels = [|"A"; "B"; "C"|],
        confusion =(
            [
                [3; 1; 1]
                [1; 2; 0]
                [2; 0; 4]
            ]
            |> array2D
            |> Matrix.Generic.ofArray2D
        )
    )
(**
- from predictions and actual labels of any type using `MultiLabelConfusionMatrix.ofPredictions`, additionally passing the labels
*)
MultiLabelConfusionMatrix.ofPredictions(
    labels = [|"A"; "B"; "C"|],
    actual = [|"A"; "A"; "A"; "A"; "A"; "B"; "B"; "B"; "C"; "C"; "C"; "C"; "C"; "C"|],
    predictions = [|"A"; "A"; "A"; "B"; "C"; "B"; "B"; "A"; "C"; "C"; "C"; "C"; "A"; "A"|]
)
(***include-it***)

(**

It is however not as easy to extract comparable metrics directly from this matrix. 

Therefore, multi-label classification are most often compared using `one/all-vs-rest` and `micro/macro averaging` of metrics.

It is possible to derive binary `one-vs-rest` confusion matrices to evaluate prediction metrics of individual labels from a multi-label confusion matrix.

This is done by taking all occurences of the label in the actual labels as positive values, and all other label occurences as negatives. The same is done for the prediction vector.

As an example, the derived binary confusion matrix for `Label A` in above example would be:

|       |           | Predicted | |
| ---   |---        | ---  | ---      |
|       |           | is A | is not A |
|Actual | is A      |  3   |    2     |
|       | is not A  |  3   |    6     |

Programmatically, this can be done via `MultiLabelConfusionMatrix.oneVsRest`
*)
mlcm
|> MultiLabelConfusionMatrix.oneVsRest "A"
(***include-it***)

(**
Binary confusion matrices for all labels can be obtained by `MultiLabelConfusionMatrix.allVsAll`

*)
mlcm
|> MultiLabelConfusionMatrix.allVsAll
|> Array.iter (fun (label, cm) -> printf $"{label}:\n{cm}\n")
(***include-output***)

(**
## Comparison Metrics

`Comparison Metrics` is a record type that contains (besides other things) the 21 metric shown in the table below.

It also provides static methods to perform calculation of individual metrics derived from a BinaryConfusionMatrix via the `ComparisonMetrics.calculate<Metric>` functions:

| Metric                        | Formula                                                           | API reference                                          |
| ---                           |---                                                                | ---                                                    |
|Sensitivity (TPR)              | $TPR = \frac{TP}{TP+TN}$                                          | [ComparisonMetrics.calculateSensitivity            ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateSensitivity            ) |
|Specificity (TNR)              | $TNR = \frac{TN}{TN+TP}$                                          | [ComparisonMetrics.calculateSpecificity            ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateSpecificity            ) |
|Precision (PPV)                | $PPV = \frac{TP}{TP+FP}$                                          | [ComparisonMetrics.calculatePrecision              ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculatePrecision              ) |
|NegativePredictiveValue (NPV)  | $NPV = \frac{TN}{TN+FN}$                                          | [ComparisonMetrics.calculateNegativePredictiveValue](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateNegativePredictiveValue) |
|Missrate (FNR)                 | $FNR = \frac{FN}{FN+TP}$                                          | [ComparisonMetrics.calculateMissrate               ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateMissrate               ) |
|FallOut (FPR)                  | $FPR = \frac{FP}{FP+TN}$                                          | [ComparisonMetrics.calculateFallOut                ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateFallOut                ) |
|FalseDiscoveryRate (FDR)       | $FDR = \frac{FP}{FP+TP}$                                          | [ComparisonMetrics.calculateFalseDiscoveryRate     ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateFalseDiscoveryRate     ) |
|FalseOmissionRate (FOR)        | $FOR = \frac{FN}{FN+TN}$                                          | [ComparisonMetrics.calculateFalseOmissionRate      ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateFalseOmissionRate      ) |
|PositiveLikelihoodRatio (LR+)  | $LR+ = \frac{TPR}{FPR}$                                           | [ComparisonMetrics.calculatePositiveLikelihoodRatio](/reference/fsharp-stats-testing-comparisonmetrics.html#calculatePositiveLikelihoodRatio) |
|NegativeLikelihoodRatio (LR-)  | $LR- = \frac{FNR}{TNR}$                                           | [ComparisonMetrics.calculateNegativeLikelihoodRatio](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateNegativeLikelihoodRatio) |
|PrevalenceThreshold (PT)       | $PT = \frac{\sqrt{FPR}}{\sqrt{TPR}+\sqrt{FPR}}$                   | [ComparisonMetrics.calculatePrevalenceThreshold    ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculatePrevalenceThreshold    ) |
|ThreatScore (TS)               | $TS = \frac{TP}{TP+FN+FP}$                                        | [ComparisonMetrics.calculateThreatScore            ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateThreatScore            ) |
|Prevalence                     | $Prevalence = \frac{P}{P+N}$                                      | [ComparisonMetrics.calculatePrevalence             ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculatePrevalence             ) |
|Accuracy (ACC)                 | $ACC = \frac{TP+TN}{TP+TN+FP+FN}$                                 | [ComparisonMetrics.calculateAccuracy               ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateAccuracy               ) |
|BalancedAccuracy (BA)          | $BA = \frac{TPR+TNR}{2}$                                          | [ComparisonMetrics.calculateBalancedAccuracy       ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateBalancedAccuracy       ) |
|F1 Score                       | $F1 = \frac{2TP}{2TP+FP+FN}$                                      | [ComparisonMetrics.calculateF1                     ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateF1                     ) |
|PhiCoefficient (MCC)           | $MCC = \frac{TP*TN-FP*FN}{\sqrt{(TP+FP)(TP+FN)(TN+FP)(TN+FN)}}$   | [ComparisonMetrics.calculatePhiCoefficient         ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculatePhiCoefficient         ) |
|FowlkesMallowsIndex (FM)       | $FM = \frac{}{}$   | [ComparisonMetrics.calculateFowlkesMallowsIndex    ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateFowlkesMallowsIndex    ) |
|Informedness (BM)              | $BM = \frac{}{}$   | [ComparisonMetrics.calculateInformedness           ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateInformedness           ) |
|Markedness (MK)                | $MK = \frac{}{}$   | [ComparisonMetrics.calculateMarkedness             ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateMarkedness             ) |
|DiagnosticOddsRatio (DOR)      | $DOR = \frac{}{}$   | [ComparisonMetrics.calculateDiagnosticOddsRatio    ](/reference/fsharp-stats-testing-comparisonmetrics.html#calculateDiagnosticOddsRatio    ) |

### ComparisonMetrics for binary comparisons

You can create the `ComparisonMetrics` record in various ways:

- directly from obtained TP/TN/FP/FN values using `ComparisonMetrics.create`
*)
ComparisonMetrics.create(3,2,1,1)
(**
- From a `BinaryConfusionMatrix` using `ComparisonMetrics.create`
*)
let bcm = BinaryConfusionMatrix.ofPredictions(1,actual,predicted)
ComparisonMetrics.create(bcm)
(**
- from predictions and actual labels of any type using `ComparisonMetrics.ofBinaryPredictions`, additionally passing which label is the "positive" label
*)
ComparisonMetrics.ofBinaryPredictions(1,actual,predicted)
(**
- from boolean predictions and actual labels using `BinaryConfusionMatrix.ofBinaryPredictions`
*)
ComparisonMetrics.ofBinaryPredictions(actualBool, predictedBool)
(***include-it***)

(**
### ComparisonMetrics for multi-label comparisons

see also: https://cran.r-project.org/web/packages/yardstick/vignettes/multiclass.html

To evaluate individual label prediction metrics, you can create comparison metrics for each individual label confusion matrix obtained by `MultiLabelConfusionMatrix.allVsAll`:
*)

mlcm
|> MultiLabelConfusionMatrix.allVsAll
|> Array.map (fun (label,cm) -> label, ComparisonMetrics.create(cm))
|> Array.iter(fun (label,metrics) -> printf $"Label {label}:\n\tSpecificity:%.3f{metrics.Specificity}\n\tAccuracy:%.3f{metrics.Accuracy}\n")
(***include-output***)

(**
#### Macro-averaging metrics

Macro averaging averages the metrics obtained by calculating the metric of interest for each `one-vs-rest` binary confusion matrix created from the multi-label confusion matrix..

So if you for example want to calculate the macro-average Sensitivity(TPR) $TPR_{macro}$ of a multi-label prediction, this is obtained by averaging the $TPR_i$ of each individual `one-vs-rest` label prediction for all $i = 1 .. k$ labels:

$$
TPR_{macro} = \frac1k\sum_{i=1}^{k}TPR_i
$$

macro average metrics can be obtained either from multiple metrics, a multi-label confusion matrix, or a sequence of binary confusion matrices

*)
ComparisonMetrics.macroAverage([ComparisonMetrics.create(3,6,3,2); ComparisonMetrics.create(2,10,1,1); ComparisonMetrics.create(4,7,1,2)] )
ComparisonMetrics.macroAverage(mlcm)
ComparisonMetrics.macroAverage(mlcm |> MultiLabelConfusionMatrix.allVsAll |> Array.map snd)

(**
or directly from predictions and actual labels of any type using `ComparisonMetrics.macroAverageOfMultiLabelPredictions`, additionally passing the labels
*)
ComparisonMetrics.macroAverageOfMultiLabelPredictions(
    labels = [|"A"; "B"; "C"|],
    actual = [|"A"; "A"; "A"; "A"; "A"; "B"; "B"; "B"; "C"; "C"; "C"; "C"; "C"; "C"|],
    predictions = [|"A"; "A"; "A"; "B"; "C"; "B"; "B"; "A"; "C"; "C"; "C"; "C"; "A"; "A"|]
)
(***include-it***)

(**

#### Micro-averaging metrics


Micro aggregates the `one-vs-rest` binary confusion matrices created from the multi-label confusion matrix, and then calculates the metric from the aggregated (TP/TN/FP/FN) values.

So if you for example want to calculate the micro-average Sensitivity(TPR) $TPR_{micro}$ of a multi-label prediction, this is obtained by summing each individual `one-vs-rest` label prediction's $TP$ and $TN$ and obtaining $TPR_{micro}$ by

$$
TPR_{micro} = \frac{TP_1 + TP_2 .. + TP_k}{(TP_1 + TP_2 .. + TP_k)+(TN_1 + TN_2 .. + TN_k)}
$$

micro average metrics can be obtained either from multiple binary confusion matrices or a multi-label confusion matrix

*)
ComparisonMetrics.microAverage([BinaryConfusionMatrix.create(3,6,3,2); BinaryConfusionMatrix.create(2,10,1,1); BinaryConfusionMatrix.create(4,7,1,2)] )
ComparisonMetrics.microAverage(mlcm)

(**
or directly from predictions and actual labels of any type using `ComparisonMetrics.macroAverageOfMultiLabelPredictions`, additionally passing the labels
*)
ComparisonMetrics.microAverageOfMultiLabelPredictions(
    labels = [|"A"; "B"; "C"|],
    actual = [|"A"; "A"; "A"; "A"; "A"; "B"; "B"; "B"; "C"; "C"; "C"; "C"; "C"; "C"|],
    predictions = [|"A"; "A"; "A"; "B"; "C"; "B"; "B"; "A"; "C"; "C"; "C"; "C"; "A"; "A"|]
)
(***include-it***)

(**
### Creating threshold-dependent metric maps

Predictions usually have a confidence or score attached, which indicates how "sure" the predictor is to report a label for a certain input. 

Predictors can be compared by comparing the relative frequency distributions of metrics of interest for each possible (or obtained) confidence value.

Two prominent examples are the **Reciever Operating Characteristic (ROC)** or the **Precision-Recall metric**

#### For binary predictions

*)
ComparisonMetrics.binaryThresholdMap(
    [true;true;true;true;false;false;false],
    [0.9 ;0.6 ;0.7 ; 0.2 ; 0.7; 0.3 ; 0.1]
)
|> Array.iter (fun (threshold,cm) -> printf $"Threshold {threshold}:\n\tSensitivity: %.2f{cm.Sensitivity}\n\tPrecision : %.2f{cm.Precision}\n\tFallout : %.2f{cm.FallOut}\n\tetc...\n")
(***include-output***)


(**
#### For multi-label predictions
*)

ComparisonMetrics.multiLabelThresholdMap(
    actual = 
             [|"A"; "A"; "A"; "A"; "A"; "B"; "B"; "B"; "C"; "C"; "C"; "C"; "C"; "C"|],
    predictions = [|
        "A", [|0.8; 0.7; 0.9; 0.4; 0.3; 0.1; 0.3; 0.5; 0.1; 0.1; 0.1; 0.3; 0.5; 0.4|]
        "B", [|0.0; 0.2; 0.0; 0.5; 0.1; 0.8; 0.7; 0.4; 0.0; 0.1; 0.1; 0.0; 0.1; 0.3|]
        "C", [|0.2; 0.3; 0.1; 0.1; 0.6; 0.1; 0.1; 0.1; 0.9; 0.8; 0.8; 0.7; 0.4; 0.3|]
    |]
)

(**
#### ROC curve example

##### Binary
*)
open Plotly.NET
open Plotly.NET.LayoutObjects
open FSharp.Stats.Integration

let binaryROC = 
    ComparisonMetrics.calculateROC(
        [true;true;true;true;false;false;false],
        [0.9 ;0.6 ;0.7 ; 0.2 ; 0.7; 0.3 ; 0.1]
    )

let auc = binaryROC |> NumericalIntegration.integrateObservations Midpoint

let binaryROCChart =
    [
        Chart.Line(binaryROC, Name= $"2 label ROC, AUC = %.2f{auc}")
        |> Chart.withLineStyle(Shape = StyleParam.Shape.Vh)
        Chart.Line([0.,0.; 1.,1.0], Name = "no skill", LineDash = StyleParam.DrawingStyle.Dash, LineColor = Color.fromKeyword Grey)
    ]
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withLegend(Legend.init(XAnchor=StyleParam.XAnchorPosition.Right, YAnchor=StyleParam.YAnchorPosition.Bottom, X = 0.5, Y = 0.1))
    |> Chart.withXAxisStyle("TPR", MinMax=(0.,1.))
    |> Chart.withYAxisStyle("FPR", MinMax=(0.,1.))
    |> Chart.withTitle "Binary receiver operating characteristic example"

(*** condition: ipynb ***)
#if IPYNB
binaryROCChart
#endif // IPYNB

(***hide***)
binaryROCChart |> GenericChart.toChartHTML
(***include-it-raw***)

(**

##### Multi-label

*)

let multiLabelROC = 
    ComparisonMetrics.calculateMultiLabelROC(
        actual = [|"A"; "A"; "A"; "A"; "A"; "B"; "B"; "B"; "C"; "C"; "C"; "C"; "C"; "C"|],
        predictions = [|
            "A", [|0.8; 0.7; 0.9; 0.4; 0.3; 0.1; 0.2; 0.5; 0.1; 0.1; 0.1; 0.3; 0.5; 0.4|]
            "B", [|0.0; 0.1; 0.0; 0.5; 0.1; 0.8; 0.7; 0.4; 0.0; 0.1; 0.1; 0.0; 0.1; 0.3|]
            "C", [|0.2; 0.2; 0.1; 0.1; 0.6; 0.1; 0.1; 0.1; 0.9; 0.8; 0.8; 0.7; 0.4; 0.3|]
        |]
    )

let aucMap = 
    multiLabelROC 
    |> Map.map (fun label roc -> roc |> NumericalIntegration.integrateObservations Midpoint)

let multiLabelROCChart =
    [
        yield!  
            multiLabelROC
            |> Map.toArray
            |> Array.map (fun (label,roc) -> 
                Chart.Line(roc, Name= $"{label} ROC, AUC = %.2f{aucMap[label]}")
                |> Chart.withLineStyle(Shape = StyleParam.Shape.Vh)
            )
        Chart.Line([0.,0.; 1.,1.0], Name = "no skill", LineDash = StyleParam.DrawingStyle.Dash, LineColor = Color.fromKeyword Grey)
    ]
    |> Chart.combine
    |> Chart.withTemplate ChartTemplates.lightMirrored
    |> Chart.withLegend(Legend.init(XAnchor=StyleParam.XAnchorPosition.Right, YAnchor=StyleParam.YAnchorPosition.Bottom, X = 0.5, Y = 0.1))
    |> Chart.withXAxisStyle("TPR", MinMax=(0.,1.))
    |> Chart.withYAxisStyle("FPR", MinMax=(0.,1.))
    |> Chart.withTitle "Binary receiver operating characteristic example"


(*** condition: ipynb ***)
#if IPYNB
multiLabelROCChart
#endif // IPYNB

(***hide***)
multiLabelROCChart |> GenericChart.toChartHTML
(***include-it-raw***)
