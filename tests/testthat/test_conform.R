test_that("conform replaces invalid entries with NA", {
  x <- c(1, NaN, Inf, -Inf, 1e6, -1e6)
  result <- dtools:::conform(x)

  expect_equal(result[1], 1)
  expect_true(all(is.na(result[2:6])))
})

test_that("conform respects custom limit and replacement", {
  x <- c(-10, 0, 5, 20)
  result <- dtools:::conform(x, limit = 10, na = 0)

  expect_equal(result, c(-10, 0, 5, 0))
})

test_that("conform leaves finite in-range values untouched", {
  x <- c(-50, 0, 50)
  expect_equal(dtools:::conform(x, limit = 1e3), x)
})
