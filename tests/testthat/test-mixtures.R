library("data.table")

test_that("Generating Mixtures works", {
  data <- setDT(example_data)
  weights <- c(0.2, 0.3, 0.4, 0.1)
  mix <- mixture_from_samples(data, weights = weights)
  first_sample_models <- data[
    sample_nr == 1 & date == min(date) & geography == "Tatooine"
  ]
  first_sample_mix <- mix[
    sample_nr == 1 & date == min(date) & geography == "Tatooine"
  ]
  expect_equal(nrow(data), nrow(mix) * 4)
})
