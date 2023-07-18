library("data.table")

test_that("Generating Mixtures works", {
  weights <- c(0.2, 0.3, 0.4, 0.1)
  mix <- mixture_from_samples(example_data, weights = weights)
  first_sample_models <- example_data[
    sample_nr == 1 & date == min(date) & geography == "Tatooine"
  ]
  first_sample_mix <- example_data[
    sample_nr == 1 & date == min(date) & geography == "Tatooine"
  ]
  expect_equal(nrow(example_data), nrow(mix) * 4)
})
