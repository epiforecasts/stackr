#' @title Model weighting by CRPS
#'
#' @description
#' Model weights for stacking by CRPS. MISSING.
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
#' seed = 1
#' K = 3 # number of models
#' R = 1 # number of regions
#' T = 10 # number of timesteps
#' S = 20 # number of predictive samples
#' ## time point weights
#' lambda <- numeric(T)
#' for (t in 1:T) {
#'   # lambda[t] = 1.5 - (1 - (t + 0.0) /T)^2;
#'   lambda[t] = 1; # try equal weights for now
#' }
#' ## region weights
#' gamma <- array(0, R)
#' for (r in 1:R) {
#'   gamma[r] <- 1.0 / R;
#' }
#'
#'  y <- array(rnorm(T), c(R, T))
#'
#' # create an example array
#' predict_sample_mat <- array(NA, c(T, R, S,K))
#' for (r in 1:R) {
#'   for (t in 1:T) {
#'     predict_sample_mat[t, r, , ] <- cbind(rnorm(S, 2, 1),
#'                                           rgamma(S, shape = 1),
#'                                           rnorm(S, 0.5, 2.4))
#'   }
#' }
#'
#' stacking_weights_crps(predict_sample_mat, y, K, R, T, S)
#'
#' @export
#' @references Missing
#'

stacking_weights_crps <- function(predict_sample,
                                  y,
                                  K, R, T, S,
                                  lambda = NULL, gamma = NULL,
                                  dirichlet_alpha = 1.001) {

	if (dim(predict_sample) != c(T, R, S,K) || dim(y) != c(R, T))
		stop("Input dimensions do not match")

  if (K < 2)
		stop("At least two models are required model averaging")

  if (is.null(lambda))
		for (t in 1:T)
			lambda[t] <- 2 - (1 - t / T)^2

	if (is.null(gamma))
		gamma <- array(rep(1 / R, R))

	standata <- list(K = K,
									 R = R,
									 T = T,
									 S = S,
									 predict_sample_mat = predict_sample,
									 y = y,
									 lambda = lambda,
									 gamma = gamma,
									 dirichlet_alpha=dirichlet_alpha)

	opt <- rstan::optimizing(stanmodels$stacking_crps, data = standata)
	return(opt$par)
	# stacking_opt_model <- stan_model("stan/crps_test.stan")
}
