#' @title Obtain CRPS stacking weights 
#'
#' @description
#' given true values and predictive samples from different models, 
#' `crps_weights` returns the stacking weights that produce the ensemble
#' that minimises the Continuos Ranked Probability Score (CRPS). 
#' 
#' @param data a data.frame with the following entries: 
#' \itemize{
#'   \item y_obs the true observed value
#'   \item y_pred a predicted value corresponding to the true value in y_obs
#'   \item model (the model used to generate the correspondig predictions)
#'   \item geography (the regions for which predictions are generated). If 
#'   geography is missing, it will be assumed there is only one region.
#'   \item date (the date of the corresponding prediction / true value)
#' }
#' @param lambda weights given to timepoints. If \code{lamba} is \code{NULL}, 
#' the default is a quadratic weight, were lambda[t] = 2 - (1 - t / T)^2. 
#' \code{lambda = "equal"} uses equal weights
#' 
#' @param gamma weights given to regions. If \code{gamma} is \code{NULL} the 
#' default is equal weights for the regions. Weights are mapped to regions
#' alphabetically, so make sure that the the weights correspond to the 
#' regions in alphabetical order. 
#' 
#' @param dirichlet_alpha prior for the weights
#' 
#' @importFrom magrittr "%>%"
#' 
#' @return returns a vector with the model weights
#' 
#' @examples
#' 
#' splitdate <- as.Date("2020-04-01") 
#' 
#' traindata <- stackr::sample_prepared_data %>%
#'   dplyr::filter(date <= splitdate)
#' 
#' testdata <- stackr::sample_prepared_data %>%
#'   dplyr::filter(date > splitdate)
#'
#' weights <- crps_weights(traindata)
#' 
#' test_mixture <- mixture_from_sample(testdata, 
#'                                     weights = weights)
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

crps_weights <- function(data, 
                         lambda = NULL, 
                         gamma = NULL, 
                         dirichlet_alpha = 1.001) {
  
  # check if geography exists. if not, create a region
  if (!("geography" %in% names(data)))
    data$geography <- "Atlantis"
  
  # number of models
  models <- unique(data$model) 
  K <- length(models)
  
  # number of regions
  regions <- unique(data$geography) 
  R <- length(regions)
  
  # number of predictive samples
  S <- max(data$sample_nr)
  
  # get number of timepoints 
  dates <- unique(data$date)
  T <- length(dates)

  # turn predictions into array that can be passed to the stan model
  pred_array <- array(dplyr::arrange(testdata, model, sample_nr, geography)$y_pred, 
              dim = c(T, R, S, K))
  
  # turn observations into array that can be passed to the stan model
  y_array <- dplyr::filter(data, 
                           sample_nr == 1, 
                           model == models[1]) %>%
    dplyr::arrange(date, geography) %>%
    dplyr::pull(y_obs) %>%
    array(c(R, T))

  # assign quadratic or equal weights if no lambda vector is provided
  if (is.null(lambda)) {
    for (t in 1:T)
      lambda[t] <- 2 - (1 - t / T)^2
  } else if (lambda == "equal") {
    lambda <- rep(1/T, T)
  }
  
  # assign equal weights to regions if no gamma is provided
  if (is.null(gamma))
    gamma <- array(rep(1 / R, R))
  
  
  standata <- list(K = K,
                   R = R,
                   T = T,
                   S = S,
                   predict_sample_mat = pred_array,
                   y = y_array,
                   lambda = lambda,
                   gamma = gamma,
                   dirichlet_alpha = dirichlet_alpha)
  
  model <- stanmodels$stacking_crps    
  opt <- rstan::optimizing(model, data = standata)
  return(opt$par)
}

# # turn data into a data.table
# # data.table::setDT(data)
# # pred_array <- array(data[order(model, sample_nr, geography)]$y_pred, 
# #                     dim = c(T, R, S, K))
# 
# arrays$prediction_array %>% dim()
# 
#   # turn data.frame into arrays for further processing
# arrays <- stackr::create_arrays(testdata)
# 
# T <- arrays$T
# R <- arrays$R
