##' Calculates a weighted interval score
##'
##' @param weights weights of the models
##' @param x input data frame
##' @param enforce_normalisation if TRUE, normalisation (weights sum to 1) is enforced
##' @param per_centile_weights if TRUE, separate weights are calculated for
##' different centiles
##' @importFrom dplyr rowwise summarise ungroup mutate group_by_at vars
##' @importFrom tidyr expand_grid
##' @importFrom scoringutils interval_score
##' @return average interval score
##' @keywords internal
##' @author Sebasian Funk
##' 

qra_weighted_average_interval_score <-
  function(weights, x, enforce_normalisation, per_centile_weights) {
    models <- unique(x$model)
    mw <- tidyr::expand_grid(model = models,
                             centile = unique(x$centile))
    
    if (per_centile_weights) {
      mw <- mw %>%
        dplyr::mutate(weight = weights)
    } else {
      mw <- mw %>%
        dplyr::mutate(weight = rep(weights, each = length(unique(x$centile))))
    }
    
    y <- x %>%
      dplyr::left_join(mw, by = c("model", "centile"))
    
    mean_score <- y %>%
      dplyr::group_by_at(dplyr::vars(-model, -value, -centile, -weight)) %>%
      dplyr::summarise(value = sum(value * weight)) %>%
      dplyr::ungroup() %>%
      tidyr::spread(boundary, value) %>%
      dplyr::rowwise() %>%
      dplyr::summarise(score = scoringutils::interval_score(data, lower, upper,
                                                            100 * interval, weigh = TRUE)) %>%
      dplyr::ungroup() %>%
      .$score %>%
      mean
    
    if (enforce_normalisation) {
      ## add penalty term to enforce normalisation
      penalty <- mw %>%
        dplyr::group_by(centile) %>%
        dplyr::summarise(score = abs(1 - sum(weight))^2 * 1e+10) %>%
        dplyr::ungroup() %>%
        .$score %>%
        mean
      
      mean_score <- mean_score + penalty
    }
    
    return(mean_score)
  }


##' Estimate weights for QRA for a data frame containing `model`, `centile`,
##' `boundary`,  `value`, `interval` columns
##'
##' @param x input data frame
##' @return data frame with weights
##' @inheritParams qra_weighted_average_interval_score
##' @importFrom nloptr sbplx
##' @importFrom dplyr mutate
##' @importFrom tidyr expand_grid
##' @keywords internal
##' @author Sebastian Funk

qra_estimate_weights <-
  function(x, per_centile_weights, enforce_normalisation) {
    
    nmodels <- length(unique(x$model))
    ncent <- length(unique(x$centile))
    
    if (per_centile_weights) {
      nweights <- nmodels * ncent
    } else {
      nweights <- nmodels
    }
    init_weights <- rep(1/nmodels, nweights)
    sbplx_opts <-
      list(x0 = init_weights, fn = qra_weighted_average_interval_score,
           x = x, per_centile_weights = per_centile_weights,
           enforce_normalisation = enforce_normalisation,
           control = list(xtol_rel = 10))
    if (enforce_normalisation) {
      sbplx_opts[["lower"]] <- rep(0, length(init_weights))
      sbplx_opts[["upper"]] <- rep(1, length(init_weights))
    }
    res <- do.call(nloptr::sbplx, sbplx_opts)
    
    if (per_centile_weights) {
      weights <- res$par
    } else {
      weights <- rep(res$par, each = ncent)
    }
    
    ret <- tidyr::expand_grid(model = unique(x$model), 
                              centile = unique(x$centile)) %>%
      dplyr::mutate(weight = weights)
    
    return(ret)
  }

##' @name qra
##' @title Quantile Regression Average
##' Calculates a quantile regression average for forecasts.
##' @param forecasts data frame with forecasts
##' @param data data frame with data
##' @param target_date the date for which to create a QRA; by default, will use
#' the latest \code{creation_date} in \code{forecasts} 
##' @param min_date the minimum creation date for past forecasts to be included
##' @param max_date the maximum creation date for past forecasts to be included
##' @param pool any columns to pool as a list of character vectors (e.g.,
##' horizon, geography_scale, etc.); by default, will not pool across anything
##' @param intervals Numeric - which central intervals to consider; by default will
##' consider the maximum spanning set. Options are determined by data but will be between
##' 0 and 1.
##' @importFrom dplyr filter arrange desc inner_join mutate rename select bind_rows group_by_at starts_with
##' @importFrom tidyr gather complete nest spread
##' @importFrom rlang `!!!` syms
##' @importFrom readr parse_number
##' @importFrom tidyselect all_of
##' @importFrom purrr map
##' @importFrom future.apply future_lapply
##' @inheritParams qra_weighted_average_interval_score
##' @return a list of `ensemble`, a data frame similar to the input forecast,
##' but with \code{model} set to "Quantile regression average" and the values
##' set to the weighted averages; and `weights`, a data frame giving the weights
##' @export
##' @author Sebastian Funk

qra <- function(forecasts, data, target_date, min_date, max_date,
                pool, intervals,
                per_centile_weights = FALSE, enforce_normalisation = TRUE) {
  
  if (missing(target_date)) {target_date <- max(forecasts$creation_date)}
  if (missing(pool)) {pool <- c()}
  
  latest_forecasts <- forecasts %>%
    dplyr::filter(creation_date == target_date)
  
  obs_and_pred <- forecasts %>%
    dplyr::filter(creation_date < target_date) %>%
    dplyr::arrange(dplyr::desc(creation_date))
  
  creation_dates <- unique(obs_and_pred$creation_date)
  
  if (!missing(min_date)) {
    creation_dates <- creation_dates[creation_dates >= min_date]
  }
  if (!missing(max_date)) {
    creation_dates <- creation_dates[creation_dates <= max_date]
  }
  
  obs_and_pred <- obs_and_pred %>%
    dplyr::filter(creation_date %in% creation_dates) %>%
    dplyr::filter(model %in% unique(latest_forecasts$model)) %>%
    dplyr::inner_join(data,
                      by = setdiff(colnames(data), c("value"))) %>%
    dplyr::rename(data = value) %>%
    dplyr::mutate(horizon = value_date - creation_date) %>%
    dplyr::select(-value_date) %>%
    tidyr::gather(percentile, value, dplyr::starts_with("percentile_")) %>%
    dplyr::mutate(value = round(value),
                  centile = readr::parse_number(percentile),
                  interval = round(2 * abs(centile - 0.5), 2),
                  boundary = dplyr::if_else(centile < 0.5, "lower", "upper")) %>%
    dplyr::select(-percentile)
  
  if (!missing(intervals)) {
    obs_and_pred <- obs_and_pred %>%
      dplyr::filter(interval %in% intervals)
  }
  
  alpha_lower <- obs_and_pred %>%
    dplyr::filter(centile == 0.5) %>%
    dplyr::mutate(boundary = "lower")
  
  obs_and_pred_double_alpha <- obs_and_pred %>%
    dplyr::bind_rows(alpha_lower)
  
  grouping_vars <-
    setdiff(colnames(obs_and_pred),
            c("creation_date", "value", "model", "data",
              "centile", "boundary", "interval", pool))
  
  pooling_vars <-
    c("creation_date", "model", pool)
  
  ## maximum horizon by grouping variables - the last date on which all models
  ## are available
  max_horizons <- obs_and_pred_double_alpha %>%
    dplyr::group_by_at(
      tidyselect::all_of(c(grouping_vars, "creation_date"))) %>%
    dplyr::mutate(max_horizon = max(horizon)) %>%
    dplyr::group_by_at(tidyselect::all_of(grouping_vars)) %>%
    dplyr::mutate(max_horizon = min(max_horizon)) %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyselect::all_of(c(grouping_vars, "max_horizon"))) %>%
    dplyr::distinct()
  
  ## require a complete set to be include in QRA
  complete_set <- obs_and_pred_double_alpha %>%
    dplyr::group_by_at(tidyselect::all_of(c(grouping_vars, "interval"))) %>%
    ## create complete tibble of all combinations of creation date, mmodel
    ## and pooling variables
    tidyr::complete(!!!syms(pooling_vars)) %>%
    dplyr::left_join(max_horizons, by = grouping_vars) %>%
    dplyr::filter(horizon <= max_horizon) %>%
    dplyr::select(-max_horizon) %>%
    ## check if anything is missing and filter out
    dplyr::group_by_at(tidyselect::all_of(c(grouping_vars, "model"))) %>%
    dplyr::mutate(any_na = any(is.na(value))) %>%
    dplyr::filter(!any_na) %>%
    dplyr::select(-any_na) %>%
    ## check that more than 1 model is available
    dplyr::group_by_at(tidyselect::all_of(grouping_vars)) %>%
    dplyr::mutate(n = length(unique(model))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(-n)
  
  ## perform QRA
  weights <- complete_set %>%
    tidyr::nest(test_data = c(-grouping_vars)) %>%
    dplyr::mutate(weights =
                    purrr::map(test_data, qra_estimate_weights,
                               per_centile_weights, enforce_normalisation)) %>%
    tidyr::unnest(weights) %>%
    dplyr::select(-test_data)
  
  ensemble <- latest_forecasts %>%
    ## only keep value dates which have all models present
    dplyr::group_by_at(tidyselect::all_of(c(grouping_vars, "value_date"))) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::group_by_at(tidyselect::all_of(grouping_vars)) %>%
    dplyr::filter(n == max(n)) %>%
    dplyr::select(-n) %>%
    ## join weights
    tidyr::gather(centile, value, starts_with("percentile_")) %>%
    dplyr::mutate(centile = readr::parse_number(centile),
                  horizon = value_date - creation_date) %>%
    dplyr::inner_join(weights, by = c(grouping_vars, "model", "centile")) %>%
    dplyr::select(-horizon) %>%
    ## weigh quantieles
    dplyr::group_by_at(dplyr::vars(-model, -weight, -value)) %>%
    dplyr::summarise(value = sum(weight * value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    ## format
    dplyr::mutate(centile = paste0("percentile_", sprintf("%.2f", centile))) %>%
    tidyr::spread(centile, value) %>%
    dplyr::mutate(model = "Quantile regression average")
  
  weights_ret <- weights %>%
    dplyr::arrange(centile) %>%
    dplyr::mutate(centile = paste0("percentile_", sprintf("%.2f", centile))) %>%
    tidyr::spread(centile, weight)
  
  return(list(ensemble = ensemble, weights = weights_ret))
}