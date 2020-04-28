library(rstan)

seed = 1
model <- rstan::stan_model("stan/crps_test.stan")
K = 3# number of models
R = 1 # number of regions
T = 100 # number of timesteps
S = 200 # number of predictive samples
## time point weights
lambda <- numeric(T)
for (t in 1:T) {
	# lambda[t] = 1.5 - (1 - (t + 0.0) /T)^2;
	lambda[t] = 1; # try equal weights for now
}
## region weights
gamma <- array(0, R)
for (r in 1:R) {
	gamma[r] <- 1.0 / R; 
}

# create an example array
predict_sample_mat <- array(NA, c(T, R, S,K))
for (r in 1:R) {
  for (t in 1:T) {
      predict_sample_mat[t, r, , ] <- cbind(rnorm(S, 2, 1), 
                                        rnorm(S, 0.5, 1), 
                                        rnorm(S, mean = 4))
  }
}


# create observed true values
y_val <- sample(c(rnorm(S * R, mean = 2), 
                  rnorm(S * R, mean = 0.5)), 
                size = S * R)

y_mat <- array(y_val, c(R, T))


standata <- list(K = K,
                 R = R,
                 T = T,
                 S = S,
                 predict_sample_mat = predict_sample_mat, 
                 y = y_mat,
                 lambda = lambda,
                 gamma = gamma,
                 dirichlet_alpha = 1.01
)

model <- stackr:::stanmodels$stacking_crps
opt <- rstan::optimizing(model, data = standata,seed=20)
opt

