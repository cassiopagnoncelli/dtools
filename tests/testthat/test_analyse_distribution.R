test_that("analyse splits data into ordered groups", {
  x <- c(-2, -1, 0.5, 1.5)
  result <- dtools::analyse(x, groups = c(0))
  group_results <- result$group_results

  expect_equal(as.character(group_results$group), c("g1", "g2"))
  expect_equal(group_results$count, c(2, 2))
  expect_equal(group_results$prob, c(0.5, 0.5))
  expect_equal(result$overall_results$n, length(x))
  expect_equal(result$overall_results$NA_count, 0)
})

test_that("analyse handles missing and extreme values", {
  x <- c(-5, 0, 5, NA, NaN, 100)
  result <- dtools::analyse(x, groups = c(0, 2), extreme_threshold = 10)
  overall <- result$overall_results

  expect_equal(overall$n, length(x))
  expect_equal(overall$NA_count, 2)
  expect_equal(overall$NaN_count, 1)
  expect_equal(overall$Inf_count, 0)
  expect_equal(overall$extreme_count, 1)
  expect_equal(levels(result$group_results$group), c("g1", "g2", "g3"))
  expect_true(all(c(
    "count", "prob", "mean", "expected", "sd", "min", "q_0.05",
    "q_0.32", "median", "q_0.68", "q_0.95", "max", "group"
  ) %in% names(result$group_results)))
})

test_that("analyse uses the first numeric column from data frames", {
  df <- data.frame(
    label = letters[1:4],
    metric = c(-1, 0, 1, 2),
    extra = c(9, 8, 7, 6)
  )
  result <- dtools::analyse(df, groups = c(0))

  expect_equal(result$overall_results$n, nrow(df))
  expect_equal(result$group_results$count, c(1, 3))
})
