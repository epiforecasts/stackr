stackr package
================

## Overview

The stackr package provides an easy way to combine individual models to
an ensemble. Currently two methods are supported: The first one stacks
models according to the on the Continuous Ranked Probability Score
(CRPS) based on predictive samples. The second one combines quantile
forecasts using a quantile regression averaging approach.

## Installation

install using

``` r
devtools::install_github("nikosbosse/stackr")
```

Note: the package will eventually be moved to github.com/epiforecasts
under a different name

## CRPS Stacking

Given some training data with true observed values as well as
predictions generated from different models, stackr finds the optimal
weights to form an ensemble from these models. Using these weights,
stackr can then provide samples from the optimal model mixture by
drawing from the predictice samples of those models in the correct
proportion. This gives a true mixture model solely based on predictive
samples and is in this regard superior to other ensembling techniques
like Bayesian Model Averaging.

Currently stacking using CRPS is supported. Weights are generated using
the `crps_weights` function. With these weights and predictive samples,
the `mixture_from_samples` function can be used to obtain predictive
samples from the optimal mixture model.

### Load example data and split into train and test data

``` r
splitdate <- as.Date("2020-03-28")
data <- data.table::setDT(stackr::example_data)
traindata <- data[date <= splitdate]
testdata <- data[date > splitdate]
```

### get weights and make mixture

``` r
weights <- stackr::crps_weights(traindata)
test_mixture <- stackr::mixture_from_samples(testdata, weights = weights)
```

### score predictions

``` r
score_df <- data.table::rbindlist(list(testdata, test_mixture), fill = TRUE)
score_df[, crps := scoringutils::crps(unique(y_obs), t(y_pred)),
        by = .(geography, model, date)]
score_df[, mean(crps), by = model]
```

## QRA

### Create toy data

Generate observations over 3 weeks of daily toy data and forecasts,
produced at 3 different dates in May. There will be (toy) forecasts at a
regional and national level.

``` r
library("dplyr")
true_mean <- 25L
true_k <- 2

toy_data <- tidyr::expand_grid(
  value_type = c("cases", "deaths"),
  geography = c(paste("region", 1:3), "country"),
  creation_date = as.Date(c("2020-05-11",
                            "2020-05-18",
                            "2020-05-25")),
  horizon = 1:21) %>%
  dplyr::mutate(value_date = creation_date + horizon) %>%
  dplyr::select(-horizon) %>%
  dplyr::mutate(geography_scale =
                  dplyr::if_else(grepl("region", geography), "region", "nation"), 
                value = rnbinom(n(), true_mean, 1/true_k))
```

### create toy “forecasts” (just draws from negative binomials)

``` r
mean <- c(10L, 20L, 30L, 40L, 50L)
k <- c(0.5, 1, 1.5, 2, 3)
quantile_levels <- seq(0.05, 0.95, by = 0.05)
flist <- lapply(seq_along(mean), function(x) {
  toy_data %>%
    rowwise() %>%
    dplyr::mutate(model = paste("model", x),
                  quantiles = list(as_tibble(t(setNames(
                    qnbinom(quantile_levels, size = 1/k[x], mu = mean[x]),
                    paste0("percentile_",
                           sprintf("%.2f", quantile_levels))))))) %>%
    tidyr::unnest(quantiles) %>%
    dplyr::select(-value)
})
forecasts <- flist %>%
  dplyr::bind_rows()
```

### calculate QRA

Forecasts are pooled by forecast horizon and geography, use last \<14
days of forecasts for optimising the weights.

``` r
res <- stackr::qra(forecasts, toy_data, pool = c("horizon", "geography"),
                   min_date = max(forecasts$creation_date) - 13)
```
