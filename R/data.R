#' Example Data
#'
#' A dataset containing true values and predictions of an unknown
#' quantity in March an April 2020 on
#' the two planets Tatooine and Coruscant. Predictions were generated using
#' different models. The data on Coruscant is just a simple modulation
#' of the values observed on Tatooine.
#'
#' @format A data frame with 104000 rows and 6 variables:
#' \describe{
#'   \item{geography}{the location where values where observed and predicted}
#'   \item{model}{the model used to generate predictions}
#'   \item{sample_id}{the sample number that identifies a predictive sample
#'   for a specific geography and date}
#'   \item{date}{the date for which the prediction was made and the true
#'   value was observed}
#'   \item{predicted}{the value predicted for a given date and region}
#'   \item{observed}{the true value observed for a given date and region}
#' }
#' @keywords internal
"example_data"
