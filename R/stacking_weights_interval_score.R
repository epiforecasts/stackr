#' @title Model weighting by Interval Score
#'
#' @description
#' Obtain weights for averaging a model by Interval Score. The Interval Score is
#' a Proper Scoring Rule to score quantile predictions, following Gneiting
#' and Raftery (2007). Smaller values are better.
#'
#' The score is computed as
#'
#' \deqn{
#' score = (upper - lower) + 2/alpha * (lower - true_value) *
#' 1(true_values < lower) + 2/alpha * (true_value - upper) *
#' 1(true_value > upper)
#' }
#' where $1()$ is the indicator function and alpha is the decimal value that
#' indicates how much is outside the prediction interval.
#' To improve usability, the user is asked to provide an interval range in
#' percentage terms, i.e. interval_range = 90 (percent) for a 90 percent
#' prediction interval. Correspondingly, the user would have to provide the
#' 5% and 95% quantiles (the corresponding alpha would then be 0.1).
#' No specific distribution is assumed,
#' but the range has to be symmetric (i.e you can't use the 0.1 quantile
#' as the lower bound and the 0.7 quantile as the upper).
#'
#'
#' @param true_values A vector with the true observed values of size n
#' @param lower vector of size n with the lower quantile of the given range
#' @param upper vector of size n with the upper quantile of the given range
#' @param interval_range the range of the prediction intervals. i.e. if you're
#' forecasting the 0.05 and 0.95 quantile, the interval_range would be 90.
#' Can be either a single number or a vector of size n, if the range changes
#' for different forecasrs to be scored.
#' @param verbose if TRUE, gives you feedback if your interval_range seems odd.
#' @return vector with the scoring values
#' @examples
#'
#' T = 10
#' y_true <- rnorm(T)
#' predictions_upper <- cbind(rnorm(T, 0.5),
#'                            rnorm(T, 0.1),
#'                            rnorm(T, 0.9))
#'
#' predictions_lower <- cbind(rnorm(T, - 0.5),
#'                            rnorm(T, - 0.1),
#'                            rnorm(T, - 0.9))
#'
#' pred <- array(c(predictions_lower, predictions_upper), c(2, T, 3))
#' lambda <- rep(1, T)
#' alpha <- 0.5
#'
#' weights_by_interval_score(y_true, pred, alpha, lambda)
#'
#' @export
#' @references Strictly Proper Scoring Rules, Prediction,and Estimation,
#' Tilmann Gneiting and Adrian E. Raftery, 2007, Journal of the American
#' Statistical Association, Volume 102, 2007 - Issue 477


weights_by_interval_score <- function(true_values,
                                      predictions_array,
                                      alpha,
                                      lambda) {
  T <- dim(predictions_array)[2]
  K <- dim(predictions_array)[3]

  standata <- list(y = true_values,
                   K = 3,
                   T = T,
                   alpha = 0.5,
                   forecasts = pred,
                   lambda = lambda,
                   dirichlet_alpha = 1.01)

  opt <- rstan::optimizing(stanmodels$model_average_quantiles, data = standata)

  # opt <- rstan::optimizing(m, data = standata)
  # opt_sampl <- rstan::sampling(m, data = standata)

  return(opt$par)
}



