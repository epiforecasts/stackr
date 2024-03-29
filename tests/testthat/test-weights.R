library("data.table")

test_that("Generating CRPS weights works", {
  splitdate <- as.Date("2020-03-28")

  traindata <- example_data[date <= splitdate]
  testdata <- example_data[date > splitdate]

  weights <- crps_weights(traindata)
  expect_equal(unname(round(weights, 4)), c(0.0003, 0.9989, 0.0004, 0.0003))
})
