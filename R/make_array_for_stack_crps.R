#' @title Make array for weighting by CRPS
#'
#' @description
#' Takes a data.frame as input and ouputs a correctly sized array for the 
#' predictions and true values to be scores with the stan model
#'
#' @param data a data.frame with the following entries: 
#' \itemize{
#'   \item model (the model used to generate the correspondig predictions)
#'   \item geography (the region for which predictions are generated)
#'   \item date (the date of corresponding prediction)
#'   \item y_obs
#'   \item y_pred
#'   \item sample_nr
#' }
#' @return list with two elements, an array for the predictions and an array
#' for the true_values. 
#' @examples
#'  
#' data <- stackr::sample_prepared_data
#' 
#' create_arrays(data) 
#' 
#' @export
#' @references Missing
#'


## turn predictive samples into array. 
## needs future work to be quicker
create_arrays <- function(data) {
  
  # number of models
  models <- data$model %>%
    unique()
  K <- length(models)
  
  regions <- data$geography %>%
    unique() 
  R <- length(regions)
  
  S <- data$sample_nr %>% max()
  
  dates <- data$date %>%
    unique()
  
  T <- length(dates)
  
  predict_sample_arr <- array(NA, c(T, R, S, K))
  for (r in 1:R) {
    for (t in 1:T) {
      predict_sample_arr[t, r, , ] <- data %>% 
        dplyr::select(c(model, sample_nr, date, y_pred, geography)) %>%
        dplyr::filter(date == dates[t], geography == regions[r]) %>%
        pivot_wider(names_from = model, values_from = y_pred) %>%
        dplyr::select(all_of(models)) %>%
        as.matrix()
    }
  }
  
  y_arr <- array(NA, c(R, T))
  for (r in 1:R) {
    y_arr[r, ] <- data %>%
      dplyr::filter(geography %in% regions[r], 
                    sample_nr == 1, 
                    model == models[1]) %>%
      .$y_obs # %>% unique()
  }
  
  return(list(prediction_array = predict_sample_arr, 
              true_value_array = y_arr, 
              K = K, 
              R = R, 
              T = T, 
              S = S))
}

