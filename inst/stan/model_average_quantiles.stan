functions {
  real pointwise_interval_score(real y, real lower, real upper, real alpha) {
    real res = (upper - lower) +
               2/alpha * (lower - y) * (y < lower) +
               2/alpha * (y - upper) * (y > upper);
    return(res);
  }
}


data {
  int<lower = 2> K; // number of models
  int<lower = 1> T; // number of time points to score
  // int<lower = 1> S; // number of predictive samples for every true value
  // int<lower = 1> R; // number of regions
  real<lower = 0, upper = 1> alpha;

  matrix[T, K] forecasts[2]; // one matrix for upper and one for lower
  vector[T] y;

  vector[T] lambda; // weight different time points differently
  real dirichlet_alpha;
}

parameters {
  simplex[K] weights;
}


model {
  vector[T] average_upper = rep_vector(0, T);
  vector[T] average_lower = rep_vector(0, T);

  for (k in 1:K) {
    average_lower += weights[k] * forecasts[1, , k];
    average_upper += weights[k] * forecasts[2, , k];
  }

  for(t in 1:T)
    target += - lambda[t] * pointwise_interval_score(y[t],
                                                     average_lower[t],
                                                     average_upper[t],
                                                     alpha);

      weights ~ dirichlet(rep_vector(dirichlet_alpha, K));

}

