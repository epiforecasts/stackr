stackr package
================

# Overview

The `stackr` package provides an easy way to combine individual models
to an ensemble. Models are stacked according to the Continuous Ranked
Probability Score (CRPS) based on predictive samples.

# Installation

install using

``` r
devtools::install_github("epiforecasts/stackr")
```

# CRPS Stacking

Given some training data with true observed values as well as
predictions generated from different models, `stackr` finds the optimal
weights to form an ensemble of these models. Using these weights, stackr
can then provide samples from the optimal model mixture by drawing from
the predictice samples of those models in the correct proportion. This
gives a mixture model solely based on predictive samples and is in this
regard superior to other ensembling techniques like Bayesian Model
Averaging. More information can be found in the package vignette.

Weights are generated using the `crps_weights` function. With these
weights and predictive samples, the `mixture_from_samples` function can
be used to obtain predictive samples from the optimal mixture model.

# Usage

## Load example data and split into train and test data

``` r
splitdate <- as.Date("2020-03-28")
data <- data.table::setDT(stackr::example_data)
traindata <- data[date <= splitdate]
testdata <- data[date > splitdate]
```

## Get weights and create mixture

``` r
weights <- stackr::crps_weights(traindata)
test_mixture <- stackr::mixture_from_samples(testdata, weights = weights)
```

## Score predictions

``` r
# combine data.frame with mixture with predictions from other models
score_df <- data.table::rbindlist(list(testdata, test_mixture), fill = TRUE)

# score all predictions using from github.com/epiforecasts/scoringutils
score_df[, crps := scoringutils::crps(unique(y_obs), t(y_pred)),
        by = .(geography, model, date)]

# summarise scores
score_df[, mean(crps), by = model][, setnames(.SD, "V1", "CRPS")]
```

# References

  - Using Stacking to Average Bayesian Predictive Distributions, Yuling
    Yao, Aki Vehtari, Daniel Simpson, and Andrew Gelman, 2018, Bayesian
    Analysis 13, Number 3, pp. 917–1003
  - Strictly Proper Scoring Rules, Prediction,and Estimation, Tilmann
    Gneiting and Adrian E. Raftery, 2007, Journal of the American
    Statistical Association, Volume 102, 2007 - Issue 477
  - Comparing Bayes Model Averaging and Stacking When Model
    Approximation Error Cannot be Ignored, Bertrand Clarke, 2003,
    Journal of Machine Learning Research 4
  - Bayesian Model Weighting: The Many Faces of Model Averaging, Marvin
    Höge, Anneli Guthke and Wolfgang Nowak, 2020, Water,
    <doi:10.3390/w12020309>
  - Bayesian Stacking and Pseudo-BMA weights using the loo package, Aki
    Vehtari and Jonah Gabry, 2019,
    <https://mc-stan.org/loo/articles/loo2-weights.html>
