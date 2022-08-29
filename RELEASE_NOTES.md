### 0.4.8+be68a276 (Released 2022-8-29)
* Additions:
    * latest commit #be68a276
    * [[#0d26dd2b](https://github.com/fslaborg/FSharp.Stats/commit/0d26dd2b406f8c0243a89f20a677eecf342efaa3)] Add Gamma and Poisson fitting
    * [[#fa9dc6c1](https://github.com/fslaborg/FSharp.Stats/commit/fa9dc6c17ad12a2d1fd1c2d6a7fb698dff0f9cfc)] Refactoring distribution modules
    * [[#20eee2e7](https://github.com/fslaborg/FSharp.Stats/commit/20eee2e720762008b1445a7628610fa806ab1a64)] Add fitting and estimating a Gamma distribution from observations
    * [[#e9170b0e](https://github.com/fslaborg/FSharp.Stats/commit/e9170b0e4bbf486d4fab360eb6cc2f0d56302a34)] Add Trigamma function
    * [[#e8e46bff](https://github.com/fslaborg/FSharp.Stats/commit/e8e46bff8be9774133f391fe2125fa2495826d39)] Add Digamma function
    * [[#35b3fa36](https://github.com/fslaborg/FSharp.Stats/commit/35b3fa36ff82b6db5468699bcec24bf19cabeb7b)] Add unit test for Beta.powerSeries
    * [[#ae70774b](https://github.com/fslaborg/FSharp.Stats/commit/ae70774b879abd32433fb0be59a543462c457be0)] Add Beta.powerSeries for incomplete beta integral
    * [[#f64b4f1e](https://github.com/fslaborg/FSharp.Stats/commit/f64b4f1e89e4ca591d6cef090f090bf5924d0b3e)] Add Seq.meanQuadratic and Seq.meanQuadraticBy
    * [[#8b300c37](https://github.com/fslaborg/FSharp.Stats/commit/8b300c3783807781d5b0da400c5a7a1739b7a139)] add new version PCA
* Deletions:
    * [[#8ca9b900](https://github.com/fslaborg/FSharp.Stats/commit/8ca9b900bf8339ed2e90531d811c2a772bca37c5)] removed obsolete wilcoxon distribution #213
* Bugfixes:
    * [[#dcb044e6](https://github.com/fslaborg/FSharp.Stats/commit/dcb044e6d5881412c4ae2ac399572812f87ae698)] Fixed: Sampling from Gamma
    * [[#8fe33a6f](https://github.com/fslaborg/FSharp.Stats/commit/8fe33a6fa12610848f7814c6d81110b126eca0b6)] fixed Ftest tests
    * [[#b83b8a71](https://github.com/fslaborg/FSharp.Stats/commit/b83b8a718a44f1807499d2c8b56bd41638547cee)] :bug: Fix infinite loop bug in X² CDF
    * [[#526039cf](https://github.com/fslaborg/FSharp.Stats/commit/526039cf92a26b19cd8869868d4ea35d630239cd)] Fix Distributions.Discrete.Bernoulli.CDF :bug:
    * [[#a9b2b055](https://github.com/fslaborg/FSharp.Stats/commit/a9b2b055261e4caac804f05ae17bafa1cfb542f5)] Fix Distributions.Discrete.Bernoulli.PDF :bug:
    * [[#db447834](https://github.com/fslaborg/FSharp.Stats/commit/db447834a8de7415b1c3dca4f20b1e3530427676)] fix intervals #208
* Tests:
    * [[#bcc98700](https://github.com/fslaborg/FSharp.Stats/commit/bcc9870061a1baed5cdc11c3015d5d44ccf986d7)] add unit tests meanQuadratic #233
    * [[#d21cc8c0](https://github.com/fslaborg/FSharp.Stats/commit/d21cc8c0f71a8cda429219e70eed02796196ad09)] Added tests for Distributions.Continuous.F
    * [[#ab261eda](https://github.com/fslaborg/FSharp.Stats/commit/ab261edac3b7e2cfa28474ca42c9d6574516a117)] added Tests for FSharp.Stats.Distributions.Distinct.Binominal
    * [[#b0d6e540](https://github.com/fslaborg/FSharp.Stats/commit/b0d6e5400386a2989676c891be9faf8feb2ee882)] add twoSamplePaired ttest test
    * [[#f203428c](https://github.com/fslaborg/FSharp.Stats/commit/f203428cb54c7bc3cf642b0b2571859e001e0193)] add Intervals.intersect unit test
    * [[#a0ce2bbb](https://github.com/fslaborg/FSharp.Stats/commit/a0ce2bbbd9975d842f3191d73b485f260800c7fc)] add PCA tests
    * [[#b92244a4](https://github.com/fslaborg/FSharp.Stats/commit/b92244a4cce34131ffd95ba312c1600116ad1c05)] Added tests for Distributions.Continuous.F
    * [[#3ae95591](https://github.com/fslaborg/FSharp.Stats/commit/3ae955913748da31c25cdeb7757adf161ca82a79)] add interval tests #208
    * [[#4907e532](https://github.com/fslaborg/FSharp.Stats/commit/4907e532270e03296dcbedf83b86aad685b05aed)] Add exponential Distribution unit tests
    * [[#47e2595e](https://github.com/fslaborg/FSharp.Stats/commit/47e2595ebce59c6ad340cf83d310c26d02ec7164)] Add beta function tests and xml docs
    * [[#d9617f56](https://github.com/fslaborg/FSharp.Stats/commit/d9617f562baef89bc25f73b7020c128b24c8f137)] Add tests for SolveTriangularLinearSystem
    * [[#dd412093](https://github.com/fslaborg/FSharp.Stats/commit/dd41209316ea88174182b6f7ac9b784401323917)] Add gamma function tests, add edgecase matches, add xml docs
    * [[#68aec558](https://github.com/fslaborg/FSharp.Stats/commit/68aec5589fa925987ef8c1e06502433e3b742dd4)] add twoSamplePaired ttest test
    * [[#440a789c](https://github.com/fslaborg/FSharp.Stats/commit/440a789c87bb6d1b21fe3486d0430dc96de5d154)] add twosamplePaired ttest test
    * [[#e5d21a7e](https://github.com/fslaborg/FSharp.Stats/commit/e5d21a7ef5da2874b5b2c4ee25f9900293f40a4d)] Add Binomial coefficient tests
    * [[#142ae681](https://github.com/fslaborg/FSharp.Stats/commit/142ae6813c11e5da46b689aa55b7ddbff480f37c)] Add Unit test for F-Test
    * [[#542b7897](https://github.com/fslaborg/FSharp.Stats/commit/542b7897ea6a0be8dfb363d8761680f2fd7de4e3)] Add docs for Bernoulli distribution :books:
    * [[#312581f9](https://github.com/fslaborg/FSharp.Stats/commit/312581f99c3aa942597f4b382f4436ff4249d754)] add onesample ttest test
    * [[#8582dd72](https://github.com/fslaborg/FSharp.Stats/commit/8582dd72243b9b18b213fdce0a8a710256236315)] Add error function tests, fix edge case return values for erfcx
    * [[#cd964357](https://github.com/fslaborg/FSharp.Stats/commit/cd9643573f33b50b1e3e14eb49c228cd1b8182e8)] Add tests for SolveTriangularLinearSystems (Case: Lower)
    * [[#872ace38](https://github.com/fslaborg/FSharp.Stats/commit/872ace3832504a03490ff9d8075d928834662bc8)] add twosample ttest test
    * [[#c41cff63](https://github.com/fslaborg/FSharp.Stats/commit/c41cff63531a50b12072742b05a6ecc7ba9a4b2c)] Add logistic function tests
    * [[#60615948](https://github.com/fslaborg/FSharp.Stats/commit/60615948d06e22122c6263c6c4739bc36c0b98c6)] Add tests for SolveTriangularLinearSystems (Case: Upper)
    * [[#1673a013](https://github.com/fslaborg/FSharp.Stats/commit/1673a01318ed95f72267842f3e729f5f16288c14)] Add factorial and factorialLn tests, codebase cleanup
    * [[#261b0b80](https://github.com/fslaborg/FSharp.Stats/commit/261b0b80926d4a74d951fa529870510d8502b3d1)] add distance metrics tests
* Documentation
    * [[#8d03e0f3](https://github.com/fslaborg/FSharp.Stats/commit/8d03e0f3610db00db551e13e373784eefb2db209)] Update F Distribution dokumentation
    * [[#e159ab4d](https://github.com/fslaborg/FSharp.Stats/commit/e159ab4d3f6491ccdd97acc4726519cc7e847ca8)] update signal docu
    * [[#be68a276](https://github.com/fslaborg/FSharp.Stats/commit/be68a2761a4b0b247d0ec553ba3225c32f59e45b)] update distribution documentation
    * [[#454abfe8](https://github.com/fslaborg/FSharp.Stats/commit/454abfe8717137f3aaa5c919a6631fa56c825391)] update ML documentation
    * [[#b3e0960c](https://github.com/fslaborg/FSharp.Stats/commit/b3e0960c4a471b7d71e3f4e75a9764b1ca4a5de4)] add Intervals documentation
    * [[#f18cec6b](https://github.com/fslaborg/FSharp.Stats/commit/f18cec6b617cdb44766895ff930b21162a85c60d)] add PCA documentation

### 0.4.7+9386ed56 (Released 2022-5-30)
* Additions:
    * latest commit #9386ed56
    * [[#a28a119b](https://github.com/fslaborg/FSharp.Stats/commit/a28a119b52768a976040ade8a21dd655eb1bc835)] add release notes build target
* Bugfixes:
    * [[#8060b68a](https://github.com/fslaborg/FSharp.Stats/commit/8060b68a2dfa4135bd7aecc4f781b351dce526ba)] Fix index errors on ommitted rows + ommitted cols formatting
* Tests
    * [[#8a559f56](https://github.com/fslaborg/FSharp.Stats/commit/8a559f561516a869991362c2fc6be767984055fd)] Add int Matrix formatting tests

### 0.4.6+5133c86a (Released 2022-5-30)
* Additions:
    * latest commit #5133c86a
    * [[#194](https://github.com/fslaborg/FSharp.Stats/issues/194)] add nullspace accuracy parameter #194
    * [[#200](https://github.com/fslaborg/FSharp.Stats/issues/200)] move DistanceMetrics from ML to Core #200
    * [[#203](https://github.com/fslaborg/FSharp.Stats/pull/203)] Add FSharp.Stats.Interactive
* Bugfixes:
    * [[#195](https://github.com/fslaborg/FSharp.Stats/issues/195)] fix Gamma function edge cases
    * [[#183](https://github.com/fslaborg/FSharp.Stats/issues/183)] fix rank module #183
* Tests:
    * [[#72faba16](https://github.com/fslaborg/FSharp.Stats/commit/72faba1633269a02cdb9965f62efb2501ff569ba)] Add tests for formatting special float cases (infinity,nan)
    * [[#39ffb209](https://github.com/fslaborg/FSharp.Stats/commit/39ffb209f340f4cc59dac0b5ac8dd3fdd61c3fe4)] Add formatting tests
    * [[#9390d4b3](https://github.com/fslaborg/FSharp.Stats/commit/9390d4b39f2bf73c0412423ae8b4c62e465a5796)] add rank unit tests
    * [[#fb36a90d](https://github.com/fslaborg/FSharp.Stats/commit/fb36a90d2d17a1f788dbd2de4f139c7ee5954709)] add Polynomial interpolation test
    * [[#6de434f0](https://github.com/fslaborg/FSharp.Stats/commit/6de434f0e42a399fc27fd1fec938666fb5ede310)] add interpolation coefficients tests
    * [[#ca9ad465](https://github.com/fslaborg/FSharp.Stats/commit/ca9ad4653564438d98fe568886165bb1d83c6c55)] Add Natural Cubic,Quandratic and Parabolic Tests
* Documentation:
    * [[#5b17a5f6](https://github.com/fslaborg/FSharp.Stats/commit/5b17a5f69410bb2f796725dc02750c0d69fc3eae)] add rank documentation #183
    * [[#d1a46d61](https://github.com/fslaborg/FSharp.Stats/commit/d1a46d6162b034e0c7b2ee8f3da862d74f5696b3)] update q value variant description
    
### 0.4.5+dd76c80 (Released 2022-4-8)
* Additions:
    * latest commit #dd76c80
    * [[#188](https://github.com/fslaborg/FSharp.Stats/pull/188)] add Comparison metrics docs
    * [[#188](https://github.com/fslaborg/FSharp.Stats/pull/188)] add binary confusion matrix
    * [[#188](https://github.com/fslaborg/FSharp.Stats/pull/188)] add multi lable confusion matrix
    * [[#188](https://github.com/fslaborg/FSharp.Stats/pull/188)] add integration docs
* Improvements:
    * [[#188](https://github.com/fslaborg/FSharp.Stats/pull/188)] update Integration module
    * [[#2777594](https://github.com/fslaborg/FSharp.Stats/commit/277759476d1ceffe5486eb582119a6f72e808b70)] update to project-based build pipeline, use .net 6.0
    * [[#192](https://github.com/fslaborg/FSharp.Stats/issues/192)] improve Quantile speed

### 0.4.4+30d2f800 (Released 2022-3-8)
* Additions:
    * latest commit #30d2f800
    * [[#30d2f800](https://github.com/fslaborg/FSharp.Stats/commit/30d2f8003f5a1c8741657dae40c16f8d658788ef)] update FSharpAux version
    * [[#162](https://github.com/fslaborg/FSharp.Stats/issues/162)] add LeastSquaresCholesky
    * [[#165](https://github.com/fslaborg/FSharp.Stats/issues/165)] remove Complex module
    * [[#184](https://github.com/fslaborg/FSharp.Stats/issues/184)] update List.median output

### 0.4.3+d424857 (Released 2022-1-19)
* Additions:
    * latest commit #d424857
    * [[#a23fcd7](https://github.com/fslaborg/FSharp.Stats/commit/a23fcd72152dfd4561af11f60e0bd2f0d4197c7f)] update signal.fs namespace #168
    * [[#cc228f4](https://github.com/fslaborg/FSharp.Stats/commit/cc228f482d142a1f21426511c977195c4763cdf4)] improve svd
    * [[#09daa87](https://github.com/fslaborg/FSharp.Stats/commit/09daa87e1d2da7352e01c8fb17ae918d0a283875)] add Wilcoxon Test #117
    * [[#5c36566](https://github.com/fslaborg/FSharp.Stats/commit/5c36566c488a3942fc7b29170994edbd533142d3)] make benjaminiHochbergFDRBy tail-recursive
    * [[#79913a2](https://github.com/fslaborg/FSharp.Stats/commit/79913a22222a9b188e79d265bf0f474e572c444a)] update logo
    * [[#165b8e3](https://github.com/fslaborg/FSharp.Stats/commit/165b8e38c5f8740224e1d646dbbe9d16c35cd8bb)] add ascendingand descending versions of the 7 parameter richards curve
* Bugfixes:
    * [[#b62743d](https://github.com/fslaborg/FSharp.Stats/commit/b62743d5ad1102068ffd4ecfc5901db1f4efd852)] fix q value calculation #171
    * [[#832f50c](https://github.com/fslaborg/FSharp.Stats/commit/832f50ceb69a2bc98917d484238ed3526387d902)] fix svd #159
    * [[#ba7ac89](https://github.com/fslaborg/FSharp.Stats/commit/ba7ac8918c402fc1ac2cd30c3e34d9b3223fe68b)] fix Matrix.sumColumns output type #150
    * [[#c18863c](https://github.com/fslaborg/FSharp.Stats/commit/c18863cad3afd21ef518071b16ea078a020a647a)] fix spearman 

### 0.4.2+b91c80d (Released 2021-7-27)
* Additions:
    * latest commit #b91c80d
    * [[#050ead5](https://github.com/fslaborg/FSharp.Stats/commit/050ead5833ab83bfa6e394fe7253c0633e216e7b)] add dunnett multi comparison test #75
    * [[#2f9fcf7](https://github.com/fslaborg/FSharp.Stats/commit/2f9fcf736d22e3061c3bc717c96c282b4a9a3b37)] add RowVector.map #134
    * [[#7d89443](https://github.com/fslaborg/FSharp.Stats/commit/7d894434f43ec69d475d0223404f2dec618083e5)] add Matrix.map function #134
    * [[#352d196](https://github.com/fslaborg/FSharp.Stats/commit/352d1960a45aaf5644c68ffa4e3dd1d8c3b27551)] add nonlinear financial models
    * [[#d33d971](https://github.com/fslaborg/FSharp.Stats/commit/d33d9712ece1972f7409d46a2439e23ef8a739e0)] add Friedman-Test
* Bugfixes:
    * [[#6402636](https://github.com/fslaborg/FSharp.Stats/commit/6402636860aa145e7ac78ac3de8053550bd96bbc)] fix degrees of freedom calculations for t statistic
    * [[#3d4f3b8](https://github.com/fslaborg/FSharp.Stats/commit/3d4f3b8d2139fa95096cbef8a6544379ad4a3174)] fix Matrix.map references
    * [[#b91c80d](https://github.com/fslaborg/FSharp.Stats/commit/b91c80de5f61ff1484acd4cfc9ef44c70d0ce231)] fix LevenbergMarquardtConstrained parameter estimation

### 0.4.1+e699171 (Released 2021-4-9)
* Additions:
    * latest commit #e699171
    * [[#1138ef6](https://github.com/fslaborg/FSharp.Stats/commit/1138ef6346cb906d3919b5e7a33fa1bde51b8db9)] update docs generation workflow
    * [[#f8c2ed9](https://github.com/fslaborg/FSharp.Stats/commit/f8c2ed9d9996c4fade0f60cb5809b125afcd1916)] add binder links to all docs
    * [[#e5a98bb](https://github.com/fslaborg/FSharp.Stats/commit/e5a98bbc6835d57cc79acb8469c23a6bdc64ea2c)] update altcover and switch to lcov coverage format
    * [[#80c89d0](https://github.com/fslaborg/FSharp.Stats/commit/80c89d0af114935009bc608275bc871d333df61e)] add generic Seq.cov functions based on Vector.cov.
    * [[#8c82d85](https://github.com/fslaborg/FSharp.Stats/commit/8c82d857fb8cab3c0e5b19c20ff0b5b6fb99b8c6)] add deseq2 medianOfRatios normalization
    * [[#eba03f3](https://github.com/fslaborg/FSharp.Stats/commit/eba03f36b7e401f94833a84f3c9efc119417963a)] add ZScore calculations 
    * update clustering, basic stats, and distribution documentation
* Deletions:
    * [[#e855845](https://github.com/fslaborg/FSharp.Stats/commit/e8558451e3247e576611ade4021ab237ebc55adc)] remove unnecessary doc files
* Bugfixes:
    * [[#46d203e](https://github.com/fslaborg/FSharp.Stats/commit/46d203ead27711ca35116cd0cfdad20ae58fbcd4)] fix Matrix.meanRowWise #119

#### 0.4.0 - Tuesday. December 15, 2020
The documentation and unit tests have been extended.


* **FSharp.Stats** 
  * latest commit #c76af47
  * [Modernization of build and CI chains](https://github.com/fslaborg/FSharp.Stats/commit/413194017492aa6a3f60d055bbb4e2a34c739afe)
  * Additional functionalities:
    * [Chi Distribution](https://github.com/fslaborg/FSharp.Stats/commit/3ff9571375d03592fda3649a172a69e326664de9)
    * [Box Muller transform](https://github.com/fslaborg/FSharp.Stats/commit/50f5cc00fbc735a6adc4fd45df07ee0f72831a18)
    * [Multivariate normal distribution](https://github.com/fslaborg/FSharp.Stats/commit/eaa85712006c514e3484a7d1a793c548ef094ee4)
    * [zTransform](https://github.com/fslaborg/FSharp.Stats/commit/775112b918525bc58453873e7ee6ed6af9d4b723)
    * [Statistical correlation testing](https://github.com/fslaborg/FSharp.Stats/commit/1e9dc4a47c650cc41a45a0408a72dbd2458521b0)

* **FSharp.Stats.Lapack**
  * [detachment of FSharp.Stats.Lapack project from FSharp.Stats](https://github.com/fslaborg/FSharp.Stats/commit/90c9551b978f46270ecac374b424a28575227ae4)

* **FSharp.Stats.MSF**
  * [detachment of FSharp.Stats.MSF project from FSharp.Stats](https://github.com/fslaborg/FSharp.Stats/commit/90c9551b978f46270ecac374b424a28575227ae4)

#### 0.3.0-beta - Friday, October 9, 2020
Several bugfixes and additions to multiple namespaces. 
The documentation and unit tests have been extended.

Version bump to 0.3.0

* **FSharp.Stats** 
  * [Improvement of gap statistics performance](https://github.com/CSBiology/FSharp.Stats/commit/14471ea04e393386227f3f295657d8f69d636609)
  * Bug fixes:
    * [Random number generator bug in GapStatistics ](https://github.com/CSBiology/FSharp.Stats/commit/f74f0cfeec832a0c0524e79122c233fa28cf71e0)
    * [Slope calculation for monoton spline](https://github.com/CSBiology/FSharp.Stats/commit/1043d250a8156c3070698bea5df27e06092b1ee3)
    * [F distribution CDF](https://github.com/CSBiology/FSharp.Stats/commit/fbacbef509a7a31a116e4fda359a6565791ddec0)
    * [Chi square CDF](https://github.com/CSBiology/FSharp.Stats/commit/09e5fcf93f21e5d1a456ce536afe3e9d4fa5dae3)
    * [Covariance calculation](https://github.com/CSBiology/FSharp.Stats/commit/8b7b7305e87850bcd4cd9894549923663e31fbed)
    * [Tukey's HSD](https://github.com/CSBiology/FSharp.Stats/commit/2edac073b647f872734be44bc231c81a979ad109)
    * [Covariance calculation](https://github.com/CSBiology/FSharp.Stats/commit/8b7b7305e87850bcd4cd9894549923663e31fbed)
  * Additional functionalities:
    * [Silhouette index for cluster number determination](https://github.com/CSBiology/FSharp.Stats/commit/839297bd1bf97164717e1450867dfe72ee9a6fd9)
    * [Extend range support for hypergeometric distribution](https://github.com/CSBiology/FSharp.Stats/commit/cf369847677683a19caef559d9d0663cee73955d)
    * [getCriticalTValue function](https://github.com/CSBiology/FSharp.Stats/commit/d9eda45ba15d2af444ac915c5b096a22a3662d1d)
    * [Confidence and prediction bands for simple linear regression](https://github.com/CSBiology/FSharp.Stats/commit/e547a997b5fb5588b23a9276eb277b8c688ace86)
    * [Shuffle and split cross validation](https://github.com/CSBiology/FSharp.Stats/commit/9b173338b78820f4383b7d52af2d6c88dd9b0744)
    * [Generic version of leave one out cross validation](https://github.com/CSBiology/FSharp.Stats/commit/9366eff19d974bd37e9011ac8ae168eae7300ce3)
    * [Constrained simple linear regression](https://github.com/CSBiology/FSharp.Stats/commit/16b96283bd6b4915b98d958d0e2d63eb2ff1524c)
    * [Distribution distance metrices](https://github.com/CSBiology/FSharp.Stats/commit/abeedd0fba02fff27e60286c0632a2d96e9c2b18)
    * [Confidence intervals](https://github.com/CSBiology/FSharp.Stats/commit/b2017c0015aca533551fdfc9528db2bf20687f75)
    * [SEM (Standard error of the mean)](https://github.com/CSBiology/FSharp.Stats/commit/afb3352c4c13ef7fffa7b7a3cdfc7004b451d245)
    * [Growth curve fitting](https://github.com/CSBiology/FSharp.Stats/issues/63)
    * [One sample t test](https://github.com/CSBiology/FSharp.Stats/commit/18c80ec9006d2c4dc6bd2674d74e78aa8ffa9acc)
    * [Hierarchical clustering aggregation](https://github.com/CSBiology/FSharp.Stats/commit/ee3e485b0a3f3457123eccd917a28e1eb212547a)
    * [F test](https://github.com/CSBiology/FSharp.Stats/commit/02962581401c6139ddce07042596cd4b9e86b61e)
    * [Fishers LSD](https://github.com/CSBiology/FSharp.Stats/commit/ce00f676e7b07691f05c51e009b60ba76e5f39e7)
    * [H test](https://github.com/CSBiology/FSharp.Stats/commit/8fc3c5f407377dc7ef7c5c319f4e5fc86dd47818)
    * [Studentized range distribution](https://github.com/CSBiology/FSharp.Stats/commit/fecadc67b6cb1d64b2b269bd4b860ebe65505447)

* **FSharp.Stats.Lapack**
  * no changes

* **FSharp.Stats.MSF**
  * [Modularization of temporal classification module](https://github.com/CSBiology/FSharp.Stats/commit/5a479e3089c19dd7f4c4222675c5b0e8b4d4ff45)
  * Additional functionalities:
    * [xSpacing independent derivative determination with cubic splines](https://github.com/CSBiology/FSharp.Stats/commit/17ce2dc8c268a0b2d0e28272f6070acfccfa7226)

#### 0.2.1-beta - Monday, February 17, 2020
First nuget pre-release package.

Version bump to 0.2.1-beta.

* **FSharp.Stats** 
  * no changes

* **FSharp.Stats.Lapack**
  * no changes

* **FSharp.Stats.MSF**
  * no changes

#### 0.2.0 - Friday, February 14, 2020
Additions, improvements and bugfixes in multiple namespaces. 
The documentation has been extended.

* **FSharp.Stats.Fitting** 
    * Additional functionalities:
	  * [Nonlinear regression models for logistic functions](https://github.com/CSBiology/FSharp.Stats/commit/b117d27d16ae3344979736a9a482f117f96da019)	  
      * [Parameter estimation for constrained Levenberg Marquard solver](https://github.com/CSBiology/FSharp.Stats/commit/2a8c6d5b29ee8c9a86e8ebb755fed6e05421c251)

* **FSharp.Stats.ML**
    * [Namespace typo fix for GapStatistics](https://github.com/CSBiology/FSharp.Stats/commit/805b63014081c0b48aeed3ab1a0c3da877340ca9). Watch out if using older versions.
    * Additional functionalities:
      * [Imputation speed increase](https://github.com/CSBiology/FSharp.Stats/commit/43800844ef5c3e6798ece9a376c3f13c5f4f8804#diff-d0f16e69ed434add91a9c666cd09b556)

* **FSharp.Stats.MSF**
    * Additional functionalities:
      * [Third derivative spline calculation](https://github.com/CSBiology/FSharp.Stats/commit/805b63014081c0b48aeed3ab1a0c3da877340ca9)


#### 0.1.1 - Thursday, November 7, 2019
Several bugfixes and additions to multiple namespaces. 
The documentation and unit tests have been extended.

* **FSharp.Stats** (core)
    * Additional functionalities:
	  * [Sparse Matrix initialization](https://github.com/CSBiology/FSharp.Stats/commit/94d2d3030a6c390a2a0947730c5feeea77b937b0) 
	  * [Sparse Matrix multiplication](https://github.com/CSBiology/FSharp.Stats/commit/0f908c5d0af7efc7919b658d8dc7778cd7369e15) replaced implementation to gain performance
	  * [Sparse Matrix QR decomposition](https://github.com/CSBiology/FSharp.Stats/commit/b098fe21d49a91ec49159ef015768563fa9887cb)
      * [Spectral matrix norm](https://github.com/CSBiology/FSharp.Stats/commit/7d27b03457b746d054a1fe81f57db66ccdb1b903)
	  * [Generalized weighted pearson correlation](https://github.com/CSBiology/FSharp.Stats/commit/33b449881a1884371416c34dcdd9335202d2a758)
	  
* **FSharp.Stats.Fitting** 
    * Additional functionalities:
	  * [Crossvalidation kFold](https://github.com/CSBiology/FSharp.Stats/commit/4ef896b88636fe6161adfe76fbd0daebb5d54bf9)
	  * [RidgeRegression](https://github.com/CSBiology/FSharp.Stats/commit/7e66392bb126eaeaabe4453f3101648adeb531c4)
	  * [Levenberg Marquardt implementation supporting box constrains](https://github.com/CSBiology/FSharp.Stats/commit/5c1d95aa062bb14a9947cdffdd53f7d90a0f8e5a)
	  
* **FSharp.Stats.Signal** 
    * Additional functionalities:
	  * [Estimate optimal window width for savitzky golay filters](https://github.com/CSBiology/FSharp.Stats/commit/d1f7a7ef58ce3a82992a912e7c85a8f4b572f347)
	  * [BaselineALS'](https://github.com/CSBiology/FSharp.Stats/commit/30bfc3fcba531f7c834b708416915296ff4e3363) internal use of sparse matrices to increase performance
		
* **FSharp.Stats.ML** 
	* Additional functionalities:
	  * [Set similarity measures](https://github.com/CSBiology/FSharp.Stats/commit/fd1ec1e9d135750db63a2589093da5bb89505e94) 
	  * [Energy landscape plot to SA](https://github.com/CSBiology/FSharp.Stats/commit/46daf7f44a3683cd7d16b900ce67ffce70392101)
	  * [GapStatistics](https://github.com/CSBiology/FSharp.Stats/commit/e02a2dfa5291868e547f2be4813887e6342320c4) assists cluster number optimization


#### 0.1.0 - Wednesday, July 3, 2019
Several bugfixes and additions to multiple namespaces: 

* **FSharp.Stats** (core)
    * Additional functionalities:
      * [Matrix FSI printer](https://github.com/CSBiology/FSharp.Stats/commit/15b12b07278162fe1ddc5b5a2bc9975615c7a388) for increasing convenience when working with matrices
      * Biweight midcorrelation for [sequences](https://github.com/CSBiology/FSharp.Stats/commit/f85c362e121c72fdc501539f401b6bf8e89514a4), [vectors and matrices](https://github.com/CSBiology/FSharp.Stats/commit/84c4369e0b84eda8ac67d43362d93168427ed0f4)
    * Bug fixes:
      * Fix shuffle functions to not shuffle in place (commits [2816d81](https://github.com/CSBiology/FSharp.Stats/commit/2816d81eed86fdf3ceb4a754635cda698f96f3ab) and [475874d](https://github.com/CSBiology/FSharp.Stats/commit/475874d58a1c73d3a7bdcf6f3c49e3179cbf34d0))

* **FSharp.Stats.Fitting**
    * Additional functionalities:
      * [Weighted polynomial least squares fitting](https://github.com/CSBiology/FSharp.Stats/commit/723ae6514947c93e992cba17549646cd8db6beab) to cope with heteroscedacity
      * [Adjusted coefficient of determination](https://github.com/CSBiology/FSharp.Stats/commit/78ffe3d540478c1814eacaff5079a3ff43259fb4) to incorporate the number of required variables
      * [Leave-out-one cross validation](https://github.com/CSBiology/FSharp.Stats/commit/7b81f4198a6b48d948e2f30bef023e1205c1f788)
      * [Exponential fit](https://github.com/CSBiology/FSharp.Stats/commit/6050c6bb67d08bc1c06008601442af4c124b2ca7) model in non-linear regression
      * [Theil-Sen estimator](https://github.com/CSBiology/FSharp.Stats/commit/5c433181e0b7a1529a54bf64ec9fd16447187fdb) for robust linear regression
      * [Weighted pearson correlation](https://github.com/CSBiology/FSharp.Stats/commit/e03123933ee2bc3b3dd2e6da54892f2b9254ee27) to cope with heteroscedacity
      * Calculate derivatives for [polynomial regression](https://github.com/CSBiology/FSharp.Stats/commit/fa78afeedb9cda2563f6c7554588f1439782cfd9)

* **FSharp.Stats.Interpolation**
    * Additional functionalities:
      * [Polynomial interpolation](https://github.com/CSBiology/FSharp.Stats/commit/b8e72d3d22eabcdc1a68aba67a2139b7ea9d44a1)
      * [Cubic spline interpolating](https://github.com/CSBiology/FSharp.Stats/commit/5036d4ed666218ad9cb82a91b7db489e1742d424) with several boundary conditions
      * [Cubic Hermite spline](https://github.com/CSBiology/FSharp.Stats/commit/d9ac98d74904b19fa338dc35c3f1e676f3926b54) with [simple slope estimator](https://github.com/CSBiology/FSharp.Stats/commit/49ee64e5aea6a0dfb8504e0807260b31fd1148ed)
      * [Get monotonicity slopes](https://github.com/CSBiology/FSharp.Stats/commit/8de4caa2a31c5cbd5192597b375911d6a8ce96ec) that if possible fit an monotone interpolating cubic spline
      * Calculate derivatives for [interpolating polynomials](https://github.com/CSBiology/FSharp.Stats/commit/23682c65d8a6088a0cc3a919e2975729eda36687) and [interpolating cubic splines](https://github.com/CSBiology/FSharp.Stats/commit/98a74fe9222b39cfca2c5641879d99b689251aec)

* **FSharp.Stats.Integration**
    * Additional functionality:
      * [TwoPointDifferentiation](https://github.com/CSBiology/FSharp.Stats/commit/7bd502ca0c02de9cb9218ccbb7b1a7468881f6b9#diff-ea4073dc197d0496ca8c047f82974244)

* **FSharp.Stats.Algebra**
    * Additional functionality:
      * Calculation of the [nullpace of a matrix](https://github.com/CSBiology/FSharp.Stats/commit/723ae6514947c93e992cba17549646cd8db6beab) based on SVD

* **FSharp.Stats.Signal**
    * Additional functionalities:
      * [Ricker wavelet](https://github.com/CSBiology/FSharp.Stats/commit/60c6b16710d79bd0e87da8b6e0d1f6d8c33ecde4) for 2D continuous wavelet transform
      * [Marr wavelet](https://github.com/CSBiology/FSharp.Stats/commit/6f77aba16befaf22bb430900a25b5a59e204e0bc) for 3D continuous wavelet transform
      * [Signal padding](https://github.com/CSBiology/FSharp.Stats/commit/a575b7a7cd7260cffd28bb4bc903297e2ba11985)
      * [Continuous wavelet transform](https://github.com/CSBiology/FSharp.Stats/commit/90eb89eba11335c73ee4f6f181b421bf7bfb0029) with [defaultCWT](https://github.com/CSBiology/FSharp.Stats/commit/117e551a9c3d1c9798ce0aa2ff7b3f9027808322) and [discrete CWT](https://github.com/CSBiology/FSharp.Stats/commit/4feda5959b0024c7834e5d82c87cd1411f237d35)

* **FSharp.Stats.MSF**
    * Several improvements for Hermite (Temporal Classification):
      * commit [c0d98de](https://github.com/CSBiology/FSharp.Stats/commit/c0d98de77840a25a76712302dcca383646e52b95)  
        * add extrema calculation of constrained spline
        * add third extrema constraints
        * add various weighting methods
        * add corrected AIC model selection criterion



#### 0.0.14 - Friday, April 12, 2019
* PCA
#### 0.0.13 - Wednesday, December 12, 2018
* Fix pValueAdjust
#### 0.0.12 - Tuesday, December 11, 2018
* Fix median
#### 0.0.11 - Monday, December 10, 2018
* Bump version to 0.0.11
#### 0.0.1 - Tuesday, July 3, 2018
* Initial release
