# Test suite for main describe_data function

test_that("describe_data works with basic input", {
  result <- describe_data(iris, verbose = FALSE)

  expect_s3_class(result, "descriptR")
  expect_true("statistics" %in% names(result))
  expect_true("variable_types" %in% names(result))
  expect_true("insights" %in% names(result))
  expect_true("missing_summary" %in% names(result))
})

test_that("describe_data detects variable types correctly", {
  result <- describe_data(iris, verbose = FALSE)

  expect_equal(length(result$variable_types), 5)
  expect_equal(sum(result$variable_types == "continuous"), 4)
  expect_equal(sum(result$variable_types == "nominal"), 1)
})

test_that("describe_data computes statistics correctly", {
  result <- describe_data(iris, vars = "Sepal.Length", verbose = FALSE)

  stats <- result$statistics

  expect_true("Mean" %in% names(stats))
  expect_true("SD" %in% names(stats))
  expect_true("Median" %in% names(stats))
  expect_true("N" %in% names(stats))

  # Check values are reasonable
  expect_gt(stats$Mean, 0)
  expect_gt(stats$SD, 0)
  expect_equal(stats$N, 150)
})

test_that("describe_data handles missing data", {
  df <- data.frame(
    x = c(1, 2, NA, 4, 5),
    y = c("a", "b", "c", NA, "e")
  )

  result <- describe_data(df, verbose = FALSE)

  expect_equal(nrow(result$missing_summary), 2)
  expect_true(any(result$missing_summary$N_Missing > 0))
})

test_that("describe_data with specific variables", {
  result <- describe_data(iris, vars = c("Sepal.Length", "Species"), verbose = FALSE)

  expect_equal(nrow(result$statistics), 2)
  expect_equal(result$statistics$Variable[1], "Sepal.Length")
  expect_equal(result$statistics$Variable[2], "Species")
})

test_that("describe_data generates insights", {
  result <- describe_data(iris, include_insights = TRUE, verbose = FALSE)

  expect_true(!is.null(result$insights))
  expect_true(is.character(result$insights))
})

test_that("describe_data can suppress insights", {
  result <- describe_data(iris, include_insights = FALSE, verbose = FALSE)

  expect_null(result$insights)
})

test_that("compute_skewness works correctly", {
  # Normal distribution (skewness ≈ 0)
  x_normal <- rnorm(1000)
  skew <- descriptR:::compute_skewness(x_normal)
  expect_lt(abs(skew), 0.2)

  # Right-skewed distribution
  x_right <- rexp(1000)
  skew_right <- descriptR:::compute_skewness(x_right)
  expect_gt(skew_right, 1)
})

test_that("compute_kurtosis works correctly", {
  # Normal distribution (excess kurtosis ≈ 0)
  x_normal <- rnorm(1000)
  kurt <- descriptR:::compute_kurtosis(x_normal)
  expect_lt(abs(kurt), 0.5)
})

test_that("print method works for descriptR", {
  result <- describe_data(iris, verbose = FALSE)

  expect_output(print(result), "Descriptive Statistics Summary")
  expect_output(print(result), "Variables analyzed")
})

test_that("describe_data handles different variable types", {
  df <- data.frame(
    continuous = rnorm(100),
    discrete = sample(1:5, 100, replace = TRUE),
    binary = sample(0:1, 100, replace = TRUE),
    categorical = sample(letters[1:3], 100, replace = TRUE),
    stringsAsFactors = FALSE
  )

  result <- describe_data(df, verbose = FALSE)

  expect_equal(nrow(result$statistics), 4)
  expect_true(any(result$variable_types == "continuous"))
  expect_true(any(result$variable_types == "discrete_numeric"))
  expect_true(any(result$variable_types == "binary"))
  expect_true(any(result$variable_types == "nominal"))
})

test_that("describe_data with confidence level parameter", {
  result <- describe_data(iris,
                         vars = "Sepal.Length",
                         conf.level = 0.99,
                         verbose = FALSE)

  expect_true("Mean_CI_Lower" %in% names(result$statistics))
  expect_true("Mean_CI_Upper" %in% names(result$statistics))

  # 99% CI should be wider than 95% CI
  result_95 <- describe_data(iris,
                            vars = "Sepal.Length",
                            conf.level = 0.95,
                            verbose = FALSE)

  ci_width_99 <- result$statistics$Mean_CI_Upper - result$statistics$Mean_CI_Lower
  ci_width_95 <- result_95$statistics$Mean_CI_Upper - result_95$statistics$Mean_CI_Lower

  expect_gt(ci_width_99, ci_width_95)
})
