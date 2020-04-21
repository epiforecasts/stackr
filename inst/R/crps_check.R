## check that CRPS implementations are correct and give comparable results


## three CRPS implementations: 
## a) Yuling's implemenation currenly used in Stan
## b) implemenation in scoringRules package
## c) manual implementation that is slow but gives same result as scoringRules

## below: results

## a) Current Stan implementaion 
CRPS_pointwise=function(predict_sample, y, w){ 
  S = nrow(predict_sample)
  K = ncol(predict_sample)
  mean_bias= apply(abs(predict_sample-y), 2, mean)
  entropy= matrix(0,K,K)  ## it should be K not S
  for( k1 in 1:K){
    for(k2 in 1:k1)
      for(s1 in 1:S)
        for(s2 in 1:S)
          entropy[k1, k2] = entropy[k1, k2] + 1/(S^2)* abs( predict_sample[s1, k1] - predict_sample[s2, k2])
  }
  
  for( k1 in 1:(K-1) ){  ## change the boundary 
  for(k2 in (k1+1):K)	
    entropy[k1, k2]=entropy[k2, k1]
  }
  
  
  entropy_aggregrate=0
  for( k1 in 1:K)
    for(k2 in 1:K)
      entropy_aggregrate=entropy_aggregrate+entropy[k1, k2]*w[k1]*w[k2]
  return( mean_bias %*% w - 1/2* entropy_aggregrate )
}


## b) implementation in the scoringRules package
## (from scoringRules::crps_sample)
crps_scoringRules <- function (y, dat) 
{
  c_1n <- 1/length(dat)
  x <- sort(dat)
  a <- seq.int(0.5 * c_1n, 1 - 0.5 * c_1n, length.out = length(dat))
  f <- function(s) 2 * c_1n * sum(((s < x) - a) * (x - s))
  sapply(y, f)
}

## c) manual implemenation
## follows Yuling's summary but only for a single crps score
crps_single <- function(true_value, predictive_samples) {
  S <- length(predictive_samples)
  
  ## calculate all combinations to replace the double sum. 
  ## note that these combinations appear twice in the sum --> multiply
  ## by two --> write 1 / S^2 instead of 1 / 2*S^2
  combinations <- combn(predictive_samples, 2)
  crps <- 1/S * sum(abs(predictive_samples - true_value)) - 
    1 / S^2 * sum(abs(combinations[1,] - combinations[2,]))
  return(crps)
}



seed = 1

y = 3
predictive_sample = rnorm(1000)
predictive_sample2=rnorm(1000)
p <- cbind(predictive_sample, predictive_sample2) ##  generate two copies but not identical
CRPS_pointwise(y, predict_sample = p, c(0.5, 0.5))
CRPS_pointwise(y, predict_sample = p, c(1, 0))
crps_scoringRules(y, predictive_sample)
crps_single(y, predictive_sample)
crps_single(y, predictive_sample2)
 
