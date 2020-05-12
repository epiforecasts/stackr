functions {
// CRPS for the model for time t
  real CRPS_pointwise(real y_true, vector obs, int S) { 
    real crps_one = 1.0 / S * sum(fabs(obs - y_true));
    
    real crps_two = 0;
    for (s in 1:S) {
      for (j in 1:S) {
        crps_two += fabs(obs[s] - obs[j]);
      }
    }
    
    return( crps_one + 1.0 / S^2 * crps_two );
  }
}


data {
  int<lower = 1> T; // number of time points to score
  int<lower = 1> S; // number of predictive samples for every true value
  
  matrix[S, T] predict_sample_mat; 
  vector[T] y;
  
  vector[T] lambda; // weight different time points differently
  // real dirichlet_alpha;
}

parameters {
  real bias_correction;
}


model {

	for(t in 1:T)
		 target += - lambda[t] * CRPS_pointwise(y[t], (col(predict_sample_mat, t) - bias_correction), S);

	// weights ~ dirichlet(rep_vector(dirichlet_alpha, K));
  
} 
