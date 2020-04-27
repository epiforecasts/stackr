#' @title Make simple mixture from samples
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

create_simple_mixture <- function(data,
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


#' @title Make simple mixture from samples
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

mixture_from_sample <- function(data,
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
  
  draw_from_models <-  function(data, 
                                models, 
                                weights, 
                                regions, 
                                S,
                                timepoint, 
                                permutation = TRUE)
  {
    # S <- nrow(data)
    K <- length(models)

    individual_draws <- data %>%
      dplyr::filter(model %in% models, 
                    geography %in% regions, 
                    date %in% as.Date(timepoint)) %>%
      tidyr::pivot_wider(names_from = model, values_from = y_pred, 
                         names_prefix = "y_pred_") %>%
      dplyr::select(starts_with("y_pred_")) %>%
      as.matrix()
    
    if (permutation == TRUE) {
      individual_draws <- individual_draws[sample(1:S), ]	 # random permutation of draws
    }
    
    round_with_preserved_sum <- function(x) {
      target_sum = sum(x)
      
      ints <- as.integer(x)
      int_sum <- sum(ints)
      remainder <- target_sum - int_sum
      
      decimals <- x - ints
      order <- order(decimals, decreasing = T)
      i <- 1
      while(remainder > 0) {
        ints[order[i]] <- ints[order[i]] + 1
        remainder <- remainder - 1
      }
      
      return(ints)
    }
    
    integer_part <- round_with_preserved_sum(S*weights)
    integer_part_index <- c(0,cumsum(integer_part))
    existing_draws <- integer_part_index[K+1]
    
    
    if (existing_draws < S){
      remaining_draws <- S - existing_draws
      remaining_assignment <- sample(1:K, 
                                     remaining_draws, 
                                     prob = weights, 
                                     replace = F)
      integer_part[remaining_assignment] <- integer_part[remaining_assignment] + 1
    }
    
    mixture_vector <- rep(NA, S)
    for(k in 1:K) {
      # skip if no draws to make
      if (integer_part[k] == 0) 
        next()
      mixture_vector[(1 + integer_part_index[k]):integer_part_index[k + 1]] <- 
        individual_draws[1:integer_part[k], k]
    }
    return(mixture_vector)
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

