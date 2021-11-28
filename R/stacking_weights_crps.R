#' @title Obtain CRPS Stacking Weights 
#'
#' @description
#' given true values and predictive samples from different models, 
#' `crps_weights` returns the stacking weights which produce the ensemble
#' that minimises the Continuos Ranked Probability Score (CRPS). 
#' 
#' @param data a data.frame with the following entries: 
#' \itemize{
#'   \item y_obs, the true observed values
#'   \item y_pred, predicted values corresponding to the true values in y_obs
#'   \item model, the name of the model used to generate the corresponding 
#'   predictions
#'   \item geography (optional), the regions for which predictions are 
#'   generated. If geography is missing, it will be assumed there are no 
#'   geographical differenes to take into account. Internally, regions will
#'   be ordered alphabetically 
#'   \item date (the date of the corresponding prediction / true value). Also
#'   works with numbers to indicate timesteps
#' }
#' @param lambda weights given to timepoints. If \code{lamba} is \code{NULL}, 
#' the default gives more weight to recent time points with 
#' lambda[t] = 2 - (1 - t / T)^2. Note that elemeents of lambda need not 
#' necessarily sum up to one as the stan model automatically constraints 
#' the final weights to sum to one irrespective of lambda. 
#' \code{lambda = "equal"} uses equal weights
#' 
#' @param gamma weights given to regions. If \code{gamma} is \code{NULL} the 
#' default is equal weights for the regions. Weights are mapped to regions
#' alphabetically, so make sure that the the weights correspond to the 
#' regions in alphabetical order. 
#' 
#' @param dirichlet_alpha prior for the weights. Default is 1.001
#' 
#' @importFrom data.table `:=` setDT 
#' @importFrom rstan optimizing
#' 
#' @return returns a vector with the model weights 
#' 
#' @examples
#' 
#' \dontrun{
#' library(data.table)
#' splitdate <- as.Date("2020-03-28")
#' data <- data.table::setDT(stackr::example_data)
#' 
#' traindata <- data[date <= splitdate]
#' testdata <- data[date > splitdate]
#'
#' weights <- stackr::crps_weights(traindata)
#' 
#' test_mixture <- stackr::mixture_from_samples(testdata, 
#'                                     weights = weights)
#'                                     
#' score_df <- data.table::rbindlist(list(testdata,test_mixture),
#'                                   fill = TRUE)
#' 
#' score_df[, crps := scoringutils::crps(unique(y_obs), t(y_pred)), 
#'          by = .(geography, model, date)]
#' 
#' score_df[, mean(crps), by = model][, setnames(.SD, "V1", "CRPS")]
#' }
#' 
#' @export
#' 
#' @references 
#' Strictly Proper Scoring Rules, Prediction,and Estimation,
#' Tilmann Gneiting and Adrian E. Raftery, 2007, Journal of the American
#' Statistical Association, Volume 102, 2007 - Issue 477
#' 
#' Using Stacking to Average Bayesian Predictive Distributions, 
#' Yuling Yao , Aki Vehtari, Daniel Simpson, and Andrew Gelman, 2018, 
#' Bayesian Analysis 13, Number 3, pp. 917–1003
#' 


crps_weights <- function(data, 
                         lambda = NULL, 
                         gamma = NULL, 
                         dirichlet_alpha = 1.001) {
  
  data.table::setDT(data)
  
  data.table::setorder(data, 
                       model, sample_nr, geography)
  
  # check if geography exists. if not, create a region
  if (!("geography" %in% names(data))) {
    data <- data[, geography := "Atlantis"]
  }
  
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
  pred_array <- array(data$y_pred, 
             dim = c(T, R, S, K))
  
  # turn observations into array that can be passed to the stan model
  y <- data[sample_nr == 1 & model == models[1]][order(date, geography)][, y_obs]
  y_array <- array(y, dim = c(R, T))
  
  # assign increasing or equal weights if no lambda vector is provided
  if (is.null(lambda)) {
    lambda <- 2 - (1 - (1:T / T))^2
  } else if (lambda == "equal") {
    lambda <- rep(1/T, T)
  }
  
  #quick hack to circumvent dimension mismatch error)
  if (length(lambda) == 1) {
    lambda <- as.array(lambda)
  }
  
  # assign equal weights to regions if no gamma is provided
  if (is.null(gamma)) {
    gamma <- array(rep(1 / R, R))
  }
  
  standata <- list(K = K,
                   R = R,
                   T = T,
                   S = S,
                   predict_sample_mat = pred_array,
                   y = y_array,
                   lambda = lambda,
                   gamma = gamma,
                   dirichlet_alpha = dirichlet_alpha)
  
  model <- stanmodels$stacking_weights_crps    
  opt <- rstan::optimizing(model, data = standata)
  
  weights <- opt$par
  names(weights) <- models
  
  return(weights)
}
