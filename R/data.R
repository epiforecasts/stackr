#' Example Data 
#'
#' A dataset containing predicted $R_t$ values in March an April 2020 on 
#' the two planets Tatooine and Coruscant. Predictions were generated using
#' different models. The data on Coruscant is just a simple modulation
#' of the values observed on Tatooine. 
#' 
#' @format A data frame with 104000 rows and 6 variables:
#' \describe{
#'   \item{geography}{the location where values where observed and predicted}
#'   \item{model}{the model used to generate predictions}
#'   \item{sample_nr}{the sample number that identifies a predictive sample
#'   for a specific geography and date}
#'   \item{date}{the date for which the prediction was made and the true
#'   value was observed}
#'   \item{y_pred}{the value predicted for a given date and region}
#'   \item{y_obs}{the true value observed for a given date and region}
#' }
"example_data"