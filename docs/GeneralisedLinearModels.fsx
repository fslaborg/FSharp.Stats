
(** 
---
title: GLM Documentation
index: 24
category: Documentation
categoryindex: 0
---
*)

(*** hide ***)


(*** condition: prepare ***)
#I "../src/FSharp.Stats/bin/Release/netstandard2.0/"
#r "nuget: Deedle"
#r "FSharp.Stats.dll"
#r "nuget: Plotly.NET, 4.0.0"

Plotly.NET.Defaults.DefaultDisplayOptions <-
    Plotly.NET.DisplayOptions.init (PlotlyJSReference = Plotly.NET.PlotlyJSReference.NoReference)

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, 4.0.0"
#r "nuget: Plotly.NET.Interactive, 4.0.0"
#r "nuget: FSharp.Stats"

open Plotly.NET
open FSharp.Stats
open Deedle
#endif // IPYNB


(** 
# General linear models (GLMs)

_Summary:_ This document provides an overview of fitting a Generalized Linear Model (GLM) using FSharp.Stats.

General linear models (GLMs) are a broad class of statistical models that are used to analyze the relationship between a dependent variable and one or more independent variables. GLMs are a flexible framework that encompasses various statistical techniques, including ANOVA (Analysis of Variance).

Like ANOVA, GLMs are used to examine the effects of different factors or variables on an outcome of interest. They allow us to determine if there are significant differences between groups or if there is a relationship between the independent variables and the dependent variable.

GLMs extend the concept of ANOVA by allowing for more complex modeling scenarios. While ANOVA is primarily used for comparing the means of different groups, GLMs can handle a wider range of data types and relationships. For example, GLMs can handle continuous, categorical, and count data, as well as non-linear relationships between variables.

GLMs also provide a flexible framework for incorporating multiple independent variables, interactions between variables, and controlling for confounding factors. This allows for more nuanced relationships and better understand the factors that influence the outcome variable.

In terms of similarities, both ANOVA and GLMs involve partitioning the total variation in the data into different components. ANOVA partitions the variation into between-group and within-group components, while GLMs partition the variation into systematic (explained) and residual (unexplained) components. Both ANOVA and GLMs also calculate statistics (such as F-statistic in ANOVA and t-statistic in GLMs) to assess the significance of the relationships or differences.

Overall, GLMs provide a more flexible and powerful framework for analyzing data compared to ANOVA. They allow for more complex modeling scenarios and can handle a wider range of data types. However, ANOVA remains a useful and widely used technique, particularly when comparing the means of multiple groups.

In this notebook we will discuss how to design your GLMs and how to use them in F#.

# Designing a GLM
To design a General Linear Model (GLM), you need to consider the following components:

1. Dependent Variable: This is the variable you want to predict or explain. It should be continuous or categorical.

2. Independent Variables: These are the variables that you believe have an impact on the dependent variable. They can be continuous or categorical.

3. Link Function: The link function relates the linear predictor to the expected value of the dependent variable. It transforms the linear combination of the independent variables into the appropriate scale for the dependent variable. The choice of link function depends on the distribution of the dependent variable.

4. Distribution: The distribution of the dependent variable determines the appropriate probability distribution to model the data. The choice of distribution depends on the nature of the dependent variable (continuous, binary, count, etc.) and the assumptions about the data.

The formula for a GLM is typically written as:

```
Y = β₀ + β₁X₁ + β₂X₂ + ... + βₚXₚ
```
This model is used in statistics to predict the outcome of a dependent variable (Y) based on the values of multiple independent variables (X₁, X₂, ..., Xₚ).

Let's break down the equation:

- `Y` is the dependent variable, also known as the response or outcome variable. This is what we're trying to predict or estimate.
- `β₀` is the y-intercept of the model. It represents the predicted value of Y when all the independent variables (X's) are 0.
- `β₁, β₂, ..., βₚ` are the coefficients of the independent variables (X₁, X₂, ..., Xₚ). These values quantify the impact of each corresponding independent variable on the dependent variable. For example, `β₁` is the change in the predicted value of Y for a one-unit change in X₁, assuming all other variables are held constant.
- `X₁, X₂, ..., Xₚ` are the independent variables, also known as predictors or explanatory variables. These are the variables that we use to predict Y.

In the context of programming, this equation could be implemented in a variety of ways depending on the language and libraries used. For instance, in Python, you might use the `statsmodels` or `scikit-learn` libraries to create a GLM, but in F# we can utilise `FSharp.Stats`.


## Loading the Dataset
First, let's read some data to learn how to utilize Generalized Linear Models (GLMs). Below is the code to read the cheeseDataset, which is sourced from David S. Moore and George P. McCabe's "Introduction to the Practice of Statistics" (1993), second edition, published by W. H. Freeman and Company, available on the [Statlib database](https://dasl.datadescription.com). It contains information on the taste and concentration of various chemical components in 30 matured cheddar cheeses from the LaTrobe Valley in Victoria, Australia. The final Taste score is an aggregate of the scores given by several tasters.
*)

let cheeseDataset :Frame<int,string>= 
    Frame.ReadCsv "/Users/lux/Library/CloudStorage/OneDrive-ComputationalSystemsBiology/Projects/GeneralLinearModel/data/cheese.csv"
    |> Frame.indexRows "Column1"

(***include-value:cheeseDataset***)


(**
## Creating Histograms

Step two involves visualizing the data using histograms. Histograms are an effective way to understand the distribution and frequency of the data by dividing it into bins and displaying the count of data points in each bin. This visual representation can help identify patterns, trends, and potential outliers in the dataset<br>
*)

let histograms = 
    let histogramTaste = 
        Chart.Histogram(cheeseDataset?Taste |> Series.values)
        |> Chart.withXAxisStyle("Taste")
        |> Chart.withYAxisStyle("Frequency")
        |> Chart.withTitle "Histogram of Taste"
        |> Chart.withTraceInfo("Taste")
    let histogramAcetic = 
        Chart.Histogram(cheeseDataset?Acetic |> Series.values)
        |> Chart.withXAxisStyle("Acetic")
        |> Chart.withYAxisStyle("Frequency")
        |> Chart.withTitle "Histogram of Acetic"
        |> Chart.withTraceInfo("Acetic")
    let histogramH2S = 
        Chart.Histogram(cheeseDataset?H2S |> Series.values)
        |> Chart.withXAxisStyle("H2S")
        |> Chart.withYAxisStyle("Frequency")
        |> Chart.withTitle "Histogram of H2S"
        |> Chart.withTraceInfo("H2S")
    let histogramLactic =
        Chart.Histogram(cheeseDataset?Lactic |> Series.values)
        |> Chart.withXAxisStyle("Lactic")
        |> Chart.withYAxisStyle("Frequency")
        |> Chart.withTitle "Histogram of Lactic"
        |> Chart.withTraceInfo("Lactic")    
    Chart.Grid(2,2) [histogramTaste; histogramAcetic; histogramH2S; histogramLactic]

histograms

(**
## Preparing Data for GLM
Now we can try to predict the taste of a cheese by its Aciticity, its H2S content and its Lactic acid content: For this we utilise a GLM. To use this we need to get the dependent variable, the given taste from our dataframe, as a vector and the independent variables, Acetic, H2S and Lactic, into a Matrix.
*)

let dependentVector = 
    cheeseDataset?Taste
    |> Series.values
    |> Vector.ofSeq

let independentMatrix = 
    cheeseDataset
    |> Frame.dropCol "Taste" 
    |> Frame.toJaggedArray
    |> Matrix.ofJaggedArray

(**
To include the y-intercept (also known as the intercept term) in the GLM, we must add a column of ones to our matrix of independent variables. This column represents the constant term in the model and allows the estimation of the y-intercept when fitting the model.
*)

let updatedIndependentMatrix = 
    independentMatrix
    |> Matrix.toJaggedArray
    |> Array.map (fun row -> Array.append [|1.0|] row)
    |> Matrix.ofJaggedArray

(**
## Fitting the GLM
The next step we need to take is to determine which linker functions to use in our Model.
Generalized Linear Models extend linear models to allow for response variables that have error distribution models other than a normal distribution. The choice of distribution family in a GLM depends on the nature of the response variable (dependent variable). Here is a summary of when to use each GLM distribution family:

**Normal (Gaussian) Distribution**:
   - **Use when**: The response variable is continuous and normally distributed.
   - **Common applications**: Linear regression, ANOVA, ANCOVA.
   - **Examples**: Heights, weights, test scores.

**Binomial Distribution**:
   - **Use when**: The response variable is binary (0 or 1) or proportion data.
   - **Common applications**: Logistic regression, probit regression.
   - **Examples**: Yes/No outcomes, success/failure data.

**Poisson Distribution**:
   - **Use when**: The response variable represents count data, especially counts of rare events.
   - **Common applications**: Poisson regression.
   - **Examples**: Number of customer complaints, number of accidents.

**Negative Binomial Distribution**:
   - **Use when**: The response variable is count data with overdispersion (variance greater than the mean).
   - **Common applications**: Negative binomial regression.
   - **Examples**: Number of insurance claims, number of hospital visits.

**Gamma Distribution**:
   - **Use when**: The response variable is continuous and positive, often for skewed distributions.
   - **Common applications**: Gamma regression.
   - **Examples**: Insurance claims costs, time until an event occurs.

**Inverse Gaussian Distribution**:
   - **Use when**: The response variable is continuous and positive, and particularly when the data has a long right tail.
   - **Common applications**: Inverse Gaussian regression.
   - **Examples**: Reaction times, survival times.


**Multinomial Distribution**:
   - **Use when**: The response variable represents categorical data with more than two categories.
   - **Common applications**: Multinomial logistic regression.
   - **Examples**: Survey responses with multiple choices, type of disease diagnosis.

Each distribution family has a corresponding link function that relates the linear predictor to the mean of the distribution. The choice of link function can also be tailored to better fit the specific characteristics of the data. Common link functions include the identity link, log link, logit link, and inverse link, among others.

Understanding the characteristics of your data and the nature of the response variable is crucial in selecting the appropriate distribution family for a GLM.
*)

// Matrix of independent variables
let A = updatedIndependentMatrix

// Vector of dependent variable
let b = dependentVector

// Maximum number of iterations
let maxIter = 100

// Distribution family of the dependent variable
let distributionFamily = Fitting.GLM.GlmDistributionFamily.Poisson

// Tolerance for the convergence of the algorithm, usually 1e-11 or 1e-6
let mTol = 1e-6

// Fit the model
let glm = 
    Fitting.GLM.QR.solveQrNewton A b maxIter distributionFamily mTol

glm
(***include-value:glm***)

(**
## Getting GLM Predictions

The results of the GLM are in the GLMReturn format, containing the coefficient vector *mX* and the mean response vector *mu*. The coefficients in the *mx* vector are in the same order as the matrix of independent variables we gave the model. In our case this order is:
1. intercept term
2. Acetic
3. H2S
4. Lactic

This means we can build a predictor funtion using the result of the GLM that can predict Taste based on Acetic, H2S and Lactic.
Lets turn the predictions into a Map for easy access. For this we use the 'GLMParameterStatistics' for easy acess for each parameter of the predictions.
Using this map we can also access the zScore and Pearson scores of each of the predictors, which tell us how important they are to explain our model.
*)

let glmPredictions = 
    Fitting.GLM.QR.getGLMParameterStatistics A b glm ["Intercept"; "Acetic"; "H2S"; "Lactic"]
    |> Map.ofSeq

(***include-value:glmPredictions***)


(**
## Cheese Taste Predictor Function

This function returned a map of the name of the value we assigned to it and their coefficient, standard error, z score and pvalue.

### Coefficient
The estimated effect size of the predictor variable. It indicates the expected change in the dependent variable for a one-unit change in the predictor variable, holding all other variables constant.

### Standard Error
Measures the accuracy of the coefficient's estimate. It is the standard deviation of the sampling distribution of the coefficient. A smaller standard error indicates a more precise estimate.

### Z Score
Calculated as the coefficient divided by its standard error. It tests the null hypothesis that the coefficient is zero. A larger absolute value indicates stronger evidence against the null hypothesis.

### p-value
Indicates the probability of observing a test statistic as extreme as the observed value under the null hypothesis. A smaller p-value suggests stronger evidence against the null hypothesis. Typically, a p-value less than 0.05 is considered statistically significant.

Lets use these values to create a function to predict the taste based of the coefficients.

*)

/// Predicts the taste of cheese based on the given input variables.
///
/// Parameters:
///   acetic - The acetic acid level in the cheese.
///   h2s - The hydrogen sulfide level in the cheese.
///   lactic - The lactic acid level in the cheese.
///
/// Returns:
///   The predicted taste of the cheese.
let cheeseTastePredictor acetic h2s lactic =
    // Extract the intercept term from the GLM coefficients
    let intercept = glmPredictions.Item "Intercept" |> fun x -> x.Coefficient
    
    // Extract the coefficient for the acetic acid predictor from the GLM coefficients
    let aceticCoefficient = glmPredictions.Item "Acetic" |> fun x -> x.Coefficient
    
    // Extract the coefficient for the hydrogen sulfide (H2S) predictor from the GLM coefficients
    let H2SCoefficient = glmPredictions.Item "H2S" |> fun x -> x.Coefficient
    
    // Extract the coefficient for the lactic acid predictor from the GLM coefficients
    let LacticCoefficient = glmPredictions.Item "Lactic" |> fun x -> x.Coefficient
    
    // Calculate and return the predicted cheese taste
    // The prediction is the sum of the intercept and the products of each coefficient with its corresponding predictor value
    intercept + aceticCoefficient * acetic + H2SCoefficient * h2s + LacticCoefficient * lactic

(**
## Getting GLM Model Statistics

Lastly, let's examine how well our model fits the data overall. For this, we use the 'GLMModelStatistics', which provide key metrics such as LogLikelihood, Deviance, and PearsonChi2.

### LogLikelihood
LogLikelihood measures the goodness of fit of the model. It is the logarithm of the likelihood function, which evaluates how likely it is that the observed data would occur under the model parameters. Higher values indicate a better fit of the model to the data.

### Deviance
Deviance is a measure of the discrepancy between the observed data and the values predicted by the model. It compares the likelihood of the model to the likelihood of a perfect model that predicts the data exactly. Lower deviance indicates a better fit.

### Pearson Chi-Square (PearsonChi2)
Pearson Chi-Square is another measure of goodness of fit. It assesses how well the observed data match the expected data predicted by the model. Lower values suggest a better fit. It is particularly useful for identifying overdispersion or underdispersion in the model.

These statistics together give us a comprehensive view of the model's performance and its ability to explain the variability in the data.
*)

Fitting.GLM.QR.getGLMModelStatistics b glm distributionFamily
