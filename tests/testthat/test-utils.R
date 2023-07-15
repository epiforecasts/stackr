test_that("Rounding with preserved sum works",  {
  expect_equal(
    round_with_preserved_sum(c(1.1, 2.2, 3.3, 4.4)),
    c(1, 2, 3, 5)
  )
})
