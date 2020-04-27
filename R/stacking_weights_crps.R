#' @title Model weighting by CRPS
#'
#' @description
#' Model weights for stacking by CRPS. MISSING.
#'
#' @param data a data.frame with the following entries: 
#' \itemize{
#'   \item model (the model used to generate the correspondig predictions)
#'   \item geography (the region for which predictions are generated)
#'   \item date (the date of corresponding )
#' }
#' @param lambda weigh timepoints differently
#' @param gamma weigh countries differently
#' @param dirichlet_alpha prior for the weights
#' 
#' @return returns a vector with the model weights
#' 
#' @examples
#' 
#' 
#' 
#' splitdate <- as.Date("2020-04-01")
#' 
#' traindata <- stackr::sample_prepared_data %>%
#'   dplyr::filter(date <= splitdate)
#' 
#' testdata <- stackr::sample_prepared_data %>%
#'   dplyr::filter(date > splitdate)
#'
#' weights <- stackr::stack_crps(traindata)
#' 
#' test_mixture <- stackr::create_sampled_mixture(testdata,
#'                                                weights = weights)
#' 
#' rbind(testdata, 
#'       test_mixture) %>%
#'   group_by(model, forecast_date) %>%
#'   dplyr::mutate(crps = scoringutils::crps(unique(y_obs), 
#'                                           t(as.vector(y_pred)))) %>%
#'   group_by(model) %>%
#'   dplyr::summarise(crps = mean(crps))
#'
#'
#' #Missing
#' @export
#' @references Missing
#' 
#'

stack_crps <- function(data, 
                       lambda = NULL, 
                       gamma = NULL, 
                       dirichlet_alpha = 1.001) {
  
    arrays <- stackr::create_arrays(data)

    T <- arrays$T
    R <- arrays$R
    
    if (is.null(lambda))
      for (t in 1:T)
        lambda[t] <- 2 - (1 - t / T)^2
      
      if (is.null(gamma))
        gamma <- array(rep(1 / R, R))
      
      
    standata <- list(K = arrays$K,
                     R = arrays$R,
                     T = arrays$T,
                     S = arrays$S,
                     predict_sample_mat = arrays$prediction_array,
                     y = arrays$true_value_array,
                     lambda = lambda,
                     gamma = gamma,
                     dirichlet_alpha = dirichlet_alpha)
    
    model <- stanmodels$stacking_crps     # use inside package
    # model <- rstan::stan_model("stan/crps_test.stan")
    opt <- rstan::optimizing(model, data = standata)
    return(opt$par)
}


