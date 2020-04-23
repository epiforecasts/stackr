#' @title Make mixture from samples
#'
#' @description
#' Takes a data.frame as input and ouputs a correctly sized array for the 
#' predictions and true values to be scores with the stan model
#'
#' @param data a data.frame with the following entries: 
#' \itemize{
#'   \item model (the model used to generate the correspondig predictions)
#'   \item geography (the region for which predictions are generated)
#'   \item date (the date of corresponding )
#' }
#' @param weights weights to give to the different models
#' @param type default is "crps"
#' @return tibble with samples from the mixture model
#' @examples
#'  
#' data <- stackr::sample_prepared_data
#' 
#' weights <- stackr::stack_crps(data)
#' 
#' create_sampled_mixture(data, weights = weights)
#' 
#' @export
#' @references Missing
#'

create_sampled_mixture <- function(data,
                                   weights = NULL,
                                   type = "crps") {
  
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
  
  draw_from_models <- function(data, 
                               models, 
                               weights, 
                               regions, 
                               S,
                               timepoint) {
    vec <- data %>%
      dplyr::filter(model %in% models, 
                    geography %in% regions, 
                    date %in% as.Date(timepoint)) %>%
      .$y_pred
    
    w <- rep(weights, each = S) / (S * length(weights))
    
    return(sample(vec, size = S, replace = T, prob = w))
    
  }
  
  # copy one model and fill data.frame
  mix <- data %>%
    dplyr::filter(model == models[1]) %>%
    dplyr::group_by(geography, date) %>%
    dplyr::mutate(model = "Mixture") %>%
    dplyr::mutate(y_pred = draw_from_models(data, 
                                            models, 
                                            weights, 
                                            unique(geography), 
                                            S,
                                            unique(date))) %>%
    dplyr::ungroup()
    
  return(mix)
}
