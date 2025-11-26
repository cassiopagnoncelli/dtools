test_that("plot_distribution handles numeric vectors with custom bins", {
  set.seed(123)
  x <- rnorm(50)
  p <- dtools::plot_distribution(x, bins = 12, vline = 1, title = "Numeric Plot")

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Numeric Plot")
  expect_equal(p$layers[[1]]$stat_params$bins, 12)
})

test_that("plot_distribution computes bins when not provided", {
  set.seed(321)
  x <- rnorm(30)
  p <- dtools::plot_distribution(x, title = "Auto Bins")
  expected_bins <- ceiling(3.3 * log10(length(x)) + 1)

  expect_equal(p$layers[[1]]$stat_params$bins, expected_bins)
})

test_that("plot_distribution works with data frames and errors without numerics", {
  df <- data.frame(label = letters[1:5], value = rnorm(5), other = rnorm(5))
  p <- dtools::plot_distribution(df, bins = 5)

  expect_s3_class(p, "ggplot")
  expect_equal(p$layers[[1]]$stat_params$bins, 5)
  expect_error(dtools::plot_distribution(data.frame(a = letters[1:3], b = c("x", "y", "z"))))
})
