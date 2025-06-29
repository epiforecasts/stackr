#' The 'lopensemble' package.
#'
#' @description
#' The lopensemble package provides an easy way to stack models based on the
#'   Continuous Ranked Probability Score (CRPS) based on predictive samples.
#'   Given some training data with true observed values as well as predictions
#'   generated from different models, lopensemble finds the optimal weights to
#'   form an ensemble from these models. Using these weights, lopensemble can
#'   then provide samples from the optimal model mixture by drawing from the
#'   predictice samples of those models in the correct proportion. This gives a
#'   true mixture model solely based on predictive samples and is in this regard
#'   superior to other ensembling techniques like Bayesian Model Averaging.
#'
#' Currently stacking using CRPS is supported. Weights are generated using the
#'   \link{crps_weights} function. With these weights and predictive samples,
#'   \link{mixture_from_samples} can be used to obtain predictive samples from
#'   the optimal mixture model.
#'
#' @docType package
#' @name lopensemble-package
#' @aliases lopensemble
#'
#' Using Stacking to Average Bayesian Predictive Distributions, Yuling Yao , Aki
#'   Vehtari, Daniel Simpson, and Andrew Gelman, 2018, Bayesian Analysis 13,
#'   Number 3, pp. 917–1003
#'
#' Strictly Proper Scoring Rules, Prediction,and Estimation, Tilmann Gneiting
#'   and Adrian E. Raftery, 2007, Journal of the American Statistical
#'   Association, Volume 102, 2007 - Issue 477
#'
#' @keywords internal
"_PACKAGE"
