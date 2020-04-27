#' @title Raw version of the mixture model from sample function
#'
#' @description
#' Takes a data.frame as input and ouputs a correctly sized array for the 
#' predictions and true values to be scores with the stan model
#'
#' @param data a data.frame with the following entries: 
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
#' 
#' @export
#' @references Missing
#'
#'

mixture_ensembles <- function(individual_draws,  
                              weight, 
                              random_seed = 1, 
                              permutation = TRUE)
{
  set.seed(random_seed)
  S <- dim(individual_draws)[1]
  K <- dim(individual_draws)[2]
  
  if (permutation == TRUE) {
    individual_draws <- individual_draws[sample(1:S), ]	 # random permutation of draws
  }
  
  round_with_preserved_sum <- function(x) {
    target_sum = sum(x)
    
    ints <- as.integer(x)
    int_sum <- sum(ints)
    remainder <- target_sum - int_sum
    
    decimals <- x - ints
    order <- order(decimals, decreasing = T)
    i <- 1
    while(remainder > 0) {
      ints[order[i]] <- ints[order[i]] + 1
      remainder <- remainder - 1
    }
    
    return(ints)
  }
  
  integer_part <- round_with_preserved_sum(S*weights)
  
  integer_part_index <- c(0,cumsum(integer_part))
  existing_draws <- integer_part_index[K+1]
  
  
  if (existing_draws < S){
    remaining_draws <- S - existing_draws
    remaining_assignment <- sample(1:K, 
                                   remaining_draws, 
                                   prob = weight, 
                                   replace = F)
    integer_part[remaining_assignment] <- integer_part[remaining_assignment]+1
  }
  
  mixture_vector <- rep(NA, S)
  for(k in 1:K) {
    # skip if no draws to make
    if (integer_part[k] == 0) 
      next()
    mixture_vector[(1 + integer_part_index[k]):integer_part_index[k + 1]] <- 
      individual_draws[1:integer_part[k], k]
  }
  return(mixture_vector)
}