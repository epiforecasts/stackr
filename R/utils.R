#' @title Round vector elements while preserving the Sum
#'
#' @description
#' Round the real-valued elements of a vector in a way that the integers sum of
#'   the rounded elements equals the (rounded down) sum of the original
#'   elements.
#'
#' @details
#' By default, the function implements the Largest-Remainder mehtod, also known
#'   as Hare–Niemeyer method, Hamilton method or as Vinton's method. The
#'   procedure works by 1. Rounding everything down, 2. computing the difference
#'   between the current and the desired sum and 3. adding one to elements in
#'   decreasing order of their decimal parts.
#'
#' @param vector a vector with elements to be rounded to integers
#' @param type either 'LRM' or 'random' depending on whether the Largest-
#' Remainder method should be employed or whether there should be some
#' randomness
#'
#' @return returns the rounded version of the original input vector
round_with_preserved_sum <- function(vector, type = "LRM") {
  # round in case values don't sum up to an integer value
  target_sum <- round(sum(vector))

  # take integer part of elements
  ints <- as.integer(vector)

  # calculate difference to desired sum
  int_sum <- sum(ints)
  remainder <- target_sum - int_sum

  # check which of the elements has the highest decimal part
  decimals <- vector - ints
  order <- order(decimals, decreasing = TRUE)

  # add one as long as the target sum is not yet reached
  if (remainder > 0) {
    for (i in seq_len(remainder)) {
      ints[order[i]] <- ints[order[i]] + 1
    }
  }

  return(ints)
}


globalVariables(c(
  ".",
  "..",
  "..models",
  "CRPS_Mixture",
  "geography",
  "model",
  "sample_nr",
  "y_obs"
))
