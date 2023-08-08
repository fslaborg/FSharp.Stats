![](docs/img/logo_title.svg)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6337056.svg)](https://doi.org/10.5281/zenodo.6337056)
[![Discord](https://img.shields.io/discord/836161044501889064?color=purple&label=Join%20our%20Discord%21&logo=discord&logoColor=white)](https://discord.gg/y95XRJg23e)
[![Generic badge](https://img.shields.io/badge/Made%20with-FSharp-rgb(1,143,204).svg)](https://shields.io/)
![GitHub contributors](https://img.shields.io/github/contributors/CSBiology/FSharp.Stats)
[![Build status](https://ci.appveyor.com/api/projects/status/gjsjlqmrljtty780/branch/developer?svg=true)](https://ci.appveyor.com/project/kMutagene/fsharp-stats/branch/developer)
[![codecov](https://codecov.io/gh/fslaborg/FSharp.Stats/branch/developer/graph/badge.svg?token=LRBZPV6MH8)](https://codecov.io/gh/fslaborg/FSharp.Stats)

FSharp.Stats is a multipurpose project for statistical testing, linear algebra, machine learning, fitting and signal processing.

<br>

### Amongst others, following functionalities are covered:

#### Descriptive statistics
  - <a href="https://fslab.org/FSharp.Stats/BasicStats.html">Measures of central tendency</a>
  - <a href="https://fslab.org/FSharp.Stats/BasicStats.html">Measures of dispersion</a>
  - <a href="https://fslab.org/FSharp.Stats/Correlation.html">Correlation</a>
  - <a href="https://fslab.org/FSharp.Stats/Quantiles.html">Quantile/Rank</a>
  - <a href="https://fslab.org/FSharp.Stats/Distributions.html">Distribution</a>

#### Fitting
  - <a href="https://fslab.org/FSharp.Stats/Fitting.html#Linear-Regression">Linear regression</a>
    - <a href="https://fslab.org/FSharp.Stats/Fitting.html#Simple-Linear-Regression">Simple linear regression (weighted and constrained)</a>
    - <a href="https://fslab.org/FSharp.Stats/Fitting.html#Polynomial-Regression">Polynomial regression (weighted and constrained)</a>
  - <a href="https://fslab.org/FSharp.Stats/Fitting.html#Nonlinear-Regression">Nonlinear regression</a>
  - <a href="https://fslab.org/FSharp.Stats/Fitting.html#Smoothing-spline">Spline regression</a>
  - <a href="https://fslab.org/FSharp.Stats/GoodnessOfFit.html">Goodness of fit</a>

#### Interpolation
  - <a href="https://fslab.org/FSharp.Stats/Interpolation.html#Polynomial-Interpolation">Linear spline interpolation</a>
  - <a href="https://fslab.org/FSharp.Stats/Interpolation.html#Polynomial-Interpolation">Polynomial interpolation</a>
  - <a href="https://fslab.org/FSharp.Stats/Interpolation.html#Cubic-interpolating-Spline">Cubic spline interpolation</a>
  - <a href="https://fslab.org/FSharp.Stats/Interpolation.html">Akima subspline interpolation</a>
  - <a href="https://fslab.org/FSharp.Stats/Interpolation.html">Hermite subspline interpolation</a>

#### Signal processing
  - <a href="https://fslab.org/FSharp.Stats/Signal.html#Continuous-Wavelet">Continuous wavelet transform</a>
  - <a href="https://fslab.org/FSharp.Stats/Signal.html">Smoothing filters</a>
  - Peak detection

#### Linear Algebra
  - Singular value decomposition
  
#### Machine learning
  - <a href="https://fslab.org/FSharp.Stats/ML.html">PCA</a>
  - <a href="https://fslab.org/FSharp.Stats/Clustering.html">Clustering</a>
  - Surprisal analysis
  
#### Optimization
  - Brent minimization
  - Bisection
  - [Nelder Mead](https://fslab.org/FSharp.Stats/Optimization.html#Nelder-Mead)
  
#### Statistical testing
  - <a href="https://fslab.org/FSharp.Stats/Testing.html#T-Test">t test</a>, <a href="https://fslab.org/FSharp.Stats/Testing.html#H-Test">H test</a>, etc.<br>
  - <a href="https://fslab.org/FSharp.Stats/Testing.html#Anova">ANOVA</a><br>
  - <a href="https://fslab.org/FSharp.Stats/Testing.html#PostHoc">Post hoc tests</a><br>
  - <a href="https://fslab.org/FSharp.Stats/Testing.html#Q-Value">q values</a><br>
  - <a href="https://fslab.org/FSharp.Stats/Testing.html#SAM">SAM</a><br>
  - RMT


## Documentation

Indepth explanations, tutorials and general information about the project can be found [here](https://fslab.org/FSharp.Stats) or at [fslab](https://fslab.org/).
The documentation and tutorials for this library are automatically generated (using the F# Formatting) from *.fsx and *.md files in the docs folder. If you find a typo, please submit a pull request!


## Contributing

Please refer to the [Contribution guidelines](.github/CONTRIBUTING.md).

## Development

to build the project, run either `build.cmd` or `build.sh` depending on your OS.

build targets are defined in the modules of /build/build.fsproj. 

Some interesting targets may be:

  - `./build.cmd runtests` will build the project and run tests
  - `./build.cmd watchdocs` will build the project, run tests, and build and host a local version of the documentation.
  - `./build.cmd release` will start the full release pipeline.


## Library license

The library is available under Apache 2.0. For more information see the License file in the GitHub repository.

## Citation

FSharp.Stats can be cited using its [zenodo record](https://zenodo.org/record/7568568). 

> Benedikt Venn, Lukas Weil, Kevin Schneider, David Zimmer & Timo Mühlhaus. (2022). fslaborg/FSharp.Stats. Zenodo. https://doi.org/10.5281/zenodo.6337056 
