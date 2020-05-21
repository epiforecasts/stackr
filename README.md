# stackr package

## Overview
The stackr package provides an easy way to stack models based on
the Continuous Ranked Probability Score (CRPS) based on predictive samples.
Given some training data with true observed values as well as predictions
generated from different models, stackr finds the optimal weights to form an
ensemble from these models. Using these weights, stackr can then provide
samples from the optimal model mixture by drawing from the predictice samples
of those models in the correct proportion. This gives a true mixture model
solely based on predictive samples and is in this regard superior to other
ensembling techniques like Bayesian Model Averaging.

Currently stacking using CRPS is supported. Weights are generated using
the `crps_weights` function. With these weights and predictive
samples, the `mixture_from_samples` function can be used to obtain predictive
samples from the optimal mixture model.

## Installation
install using
```
devtools::install_github("nikosbosse/stackr")
```

## Usage
```
library(data.table)
splitdate <- as.Date("2020-03-28")
data <- stackr::example_data

traindata <- data[date <= splitdate]
testdata <- data[date > splitdate]

weights <- stackr::crps_weights(traindata)

test_mixture <- stackr::mixture_from_samples(testdata,
                                     weights = weights)
score_df <- data.table::rbindlist(
               list(testdata, test_mixture), fill = TRUE
                   )
                   
score_df[, crps := scoringutils::crps(unique(y_obs), t(y_pred)),
        by = .(geography, model, date)]

score_df[, mean(crps), by = model]
```




