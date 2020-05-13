#' @title Make a simple bias correction based on CRPS
#'
#' @description
#' given a vector of true values and a matrix of predicted values, 
#' finddata a bias correction term to correct for a mismatch between predictions
#' and true values
#' 
#' Caveat: not sure whether this is strictly better than just comparing the 
#' mean of the predictive samples against the true values
#'
#' @param  a data.frame with the following entries: 
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
#' S <- 50
#' T <- 100
#' y_true <- rnorm(T)
#' y_pred <- replicate(T, rnorm(S, mean = 1))
#' 
#' bias_adjustment(y_true, y_pred)
#' 
#' @export
#' @references Missing
#'

bias_adjustment <- function(y_true,
                            y_pred, 
                            lambda = NULL) {
  
  
  T <- length(y_true)
  S <- nrow(y_pred)
  
  model <- stanmodels$bias_adjustment
  
  if (is.null(lambda))
    lambda <- rep(1, T)
    for (t in 1:T)
      lambda[t] <- 2 - (1 - t / T)^2

  standata <- list(y = y_true, 
                   T = T, 
                   S = S, 
                   predict_sample_mat = y_pred, 
                   # dirichlet_alpha = 1.001, 
                   lambda = lambda)
    
  model <- stanmodels$bias_adjustment     # use inside package
  
  correction_term <- rstan::optimizing(model, data = standata)
 
  return(correction_term$par) 
}


