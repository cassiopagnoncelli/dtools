test_that("cap caps values to quantile bounds", {
  x <- c(1, 2, 3, 100)
  result <- dtools::cap(x, quantiles = c(0.25, 0.75))
  qs <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  expected <- x
  expected[expected < qs[1]] <- qs[1]
  expected[expected > qs[2]] <- qs[2]

  expect_equal(result, expected)
})

test_that("cap keeps missing values and validates inputs", {
  x <- c(1, NA, 10)

  expect_true(is.na(dtools::cap(x)[2]))
  expect_error(dtools::cap(x, quantiles = 0.5))
  expect_error(dtools::cap(x, quantiles = c(-0.1, 0.5)))
  expect_error(dtools::cap(x, quantiles = c(0.8, 0.2)))
})
