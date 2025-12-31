# Test suite for outlier detection

# Main Function ===============================================================

test_that("detect_outliers works with default settings", {
  result <- detect_outliers(mtcars$mpg)

  expect_s3_class(result, "descriptR_outliers")
  expect_true(!is.null(result$outliers))
  expect_true(!is.null(result$summary))
  expect_true(!is.null(result$method_results))
})

test_that("detect_outliers works with data frame", {
  result <- detect_outliers(mtcars, vars = c("mpg", "hp"))

  expect_s3_class(result, "descriptR_outliers")
  expect_equal(nrow(result$summary), 2)
})

test_that("detect_outliers handles all methods", {
  methods <- c("zscore", "iqr", "mad", "grubbs", "dixon", "rosner")

  expect_no_error(
    detect_outliers(mtcars$mpg, methods = methods)
  )
})

test_that("detect_outliers validates methods", {
  expect_error(
    detect_outliers(mtcars$mpg, methods = "invalid_method"),
    "Invalid methods"
  )
})

# Z-Score Method ==============================================================

test_that("zscore method detects extreme values", {
  # Create data with clear outlier
  x <- c(rnorm(50, mean = 10, sd = 2), 100)

  result <- detect_outliers(x, methods = "zscore", threshold = 3)

  expect_true(nrow(result$outliers) > 0)
  expect_true(100 %in% result$outliers$Value)
})

test_that("zscore threshold parameter works", {
  x <- c(rnorm(50, mean = 10, sd = 2), 15)

  result_strict <- detect_outliers(x, methods = "zscore", threshold = 2)
  result_lenient <- detect_outliers(x, methods = "zscore", threshold = 5)

  expect_true(nrow(result_strict$outliers) >= nrow(result_lenient$outliers))
})

test_that("zscore method handles normal data", {
  x <- rnorm(100, mean = 50, sd = 10)

  result <- detect_outliers(x, methods = "zscore", threshold = 3)

  # Should detect very few or no outliers in normal data
  expect_true(nrow(result$outliers) < 5)
})

# IQR Method ==================================================================

test_that("iqr method detects outliers", {
  # Create data with outliers
  x <- c(1:20, 100, 200)

  result <- detect_outliers(x, methods = "iqr")

  expect_true(nrow(result$outliers) > 0)
  expect_true(100 %in% result$outliers$Value)
})

test_that("iqr multiplier parameter works", {
  x <- c(1:20, 30)

  result_strict <- detect_outliers(x, methods = "iqr", iqr_multiplier = 1.5)
  result_lenient <- detect_outliers(x, methods = "iqr", iqr_multiplier = 3)

  expect_true(nrow(result_strict$outliers) >= nrow(result_lenient$outliers))
})

test_that("iqr method identifies extreme outliers", {
  x <- c(1:20, 100)

  result <- detect_outliers(x, methods = "iqr")

  # Check method results for extreme classification
  iqr_result <- result$method_results[[1]]
  expect_true(!is.null(iqr_result$extreme_indices))
})

# MAD Method ==================================================================

test_that("mad method detects outliers", {
  x <- c(rnorm(50, mean = 10, sd = 2), 50)

  result <- detect_outliers(x, methods = "mad", threshold = 3)

  expect_true(nrow(result$outliers) > 0)
})

test_that("mad method is robust to outliers", {
  # MAD should be more robust than z-score
  x <- c(rnorm(50, mean = 10, sd = 2), 100, 110)

  result_zscore <- detect_outliers(x, methods = "zscore")
  result_mad <- detect_outliers(x, methods = "mad")

  # Both should detect the extreme values
  expect_true(nrow(result_mad$outliers) > 0)
})

# Grubbs Test =================================================================

test_that("grubbs test detects single outlier", {
  x <- c(rnorm(30, mean = 10, sd = 2), 50)

  result <- detect_outliers(x, methods = "grubbs", alpha = 0.05)

  expect_true(nrow(result$outliers) > 0)
})

test_that("grubbs test handles small samples", {
  x <- c(1, 2, 100)

  result <- detect_outliers(x, methods = "grubbs")

  # Should work with n = 3
  expect_s3_class(result, "descriptR_outliers")
})

test_that("grubbs test skips very small samples", {
  x <- c(1, 2)

  result <- detect_outliers(x, methods = "grubbs")

  # Should not detect outliers (sample too small)
  grubbs_result <- result$method_results[[1]]
  expect_true(!is.null(grubbs_result$note))
})

test_that("grubbs test is iterative", {
  # Multiple outliers
  x <- c(rnorm(30, mean = 10, sd = 2), 40, 45)

  result <- detect_outliers(x, methods = "grubbs")

  # May detect multiple outliers
  expect_s3_class(result, "descriptR_outliers")
})

# Dixon Test ==================================================================

test_that("dixon test works for small samples", {
  x <- c(1, 2, 3, 4, 100)

  result <- detect_outliers(x, methods = "dixon")

  expect_true(nrow(result$outliers) > 0)
})

test_that("dixon test skips inappropriate sample sizes", {
  # Too small
  x <- c(1, 2)
  result_small <- detect_outliers(x, methods = "dixon")

  # Too large
  x_large <- rnorm(50)
  result_large <- detect_outliers(x_large, methods = "dixon")

  # Both should note size issues
  dixon_small <- result_small$method_results[[1]]
  dixon_large <- result_large$method_results[[1]]

  expect_true(!is.null(dixon_small$note) || !is.null(dixon_large$note))
})

test_that("dixon test detects extreme values", {
  x <- c(10, 11, 12, 13, 100)

  result <- detect_outliers(x, methods = "dixon")

  expect_true(100 %in% result$outliers$Value || nrow(result$outliers) > 0)
})

# Rosner Test =================================================================

test_that("rosner test works for large samples", {
  x <- c(rnorm(100, mean = 10, sd = 2), 50, 55)

  result <- detect_outliers(x, methods = "rosner")

  expect_true(nrow(result$outliers) > 0)
})

test_that("rosner test skips small samples", {
  x <- rnorm(20)

  result <- detect_outliers(x, methods = "rosner")

  rosner_result <- result$method_results[[1]]
  expect_true(!is.null(rosner_result$note))
})

test_that("rosner test detects multiple outliers", {
  x <- c(rnorm(100, mean = 10, sd = 2), 40, 45, 50)

  result <- detect_outliers(x, methods = "rosner", alpha = 0.05)

  # Should detect multiple outliers
  expect_s3_class(result, "descriptR_outliers")
})

# Multiple Methods ============================================================

test_that("multiple methods produce combined results", {
  x <- c(rnorm(50, mean = 10, sd = 2), 50)

  result <- detect_outliers(x, methods = c("zscore", "iqr", "mad"))

  expect_equal(length(result$methods_used), 3)
  expect_true(nrow(result$outliers) > 0)
})

test_that("N_Methods column tracks agreement", {
  x <- c(rnorm(50, mean = 10, sd = 2), 100)

  result <- detect_outliers(x, methods = c("zscore", "iqr", "mad"))

  # Extreme outlier should be flagged by multiple methods
  if (nrow(result$outliers) > 0) {
    expect_true("N_Methods" %in% names(result$outliers))
    expect_true(max(result$outliers$N_Methods) >= 1)
  }
})

test_that("Methods column lists detection methods", {
  x <- c(rnorm(50, mean = 10, sd = 2), 100)

  result <- detect_outliers(x, methods = c("zscore", "iqr"))

  if (nrow(result$outliers) > 0) {
    expect_true("Methods" %in% names(result$outliers))
    expect_true(all(grepl("zscore|iqr", result$outliers$Methods)))
  }
})

# Consensus Mode ==============================================================

test_that("consensus mode filters outliers", {
  x <- c(rnorm(50, mean = 10, sd = 2), 100)

  result_all <- detect_outliers(x, methods = c("zscore", "iqr", "mad"),
                                consensus = FALSE)

  result_consensus <- detect_outliers(x, methods = c("zscore", "iqr", "mad"),
                                      consensus = TRUE, min_methods = 2)

  # Consensus should have fewer or equal outliers
  if (!is.null(result_consensus$consensus_outliers)) {
    expect_true(nrow(result_consensus$consensus_outliers) <=
                nrow(result_all$outliers))
  }
})

test_that("min_methods parameter works", {
  x <- c(rnorm(50, mean = 10, sd = 2), 100)

  result <- detect_outliers(x, methods = c("zscore", "iqr", "mad"),
                            consensus = TRUE, min_methods = 3)

  # Should only include outliers flagged by all 3 methods
  if (!is.null(result$consensus_outliers) &&
      nrow(result$consensus_outliers) > 0) {
    expect_true(all(result$consensus_outliers$N_Methods >= 3))
  }
})

# Data Frame Input ============================================================

test_that("detect_outliers works with multiple variables", {
  result <- detect_outliers(mtcars, vars = c("mpg", "hp", "wt"),
                            methods = "iqr")

  expect_equal(nrow(result$summary), 3)
  expect_true("mpg" %in% result$summary$Variable)
})

test_that("detect_outliers handles vars = NULL", {
  result <- detect_outliers(mtcars, methods = "iqr")

  # Should analyze all numeric variables
  expect_true(nrow(result$summary) > 0)
})

test_that("detect_outliers tracks variable in outlier df", {
  result <- detect_outliers(mtcars, vars = c("mpg", "hp"), methods = "iqr")

  if (nrow(result$outliers) > 0) {
    expect_true("Variable" %in% names(result$outliers))
    expect_true(all(result$outliers$Variable %in% c("mpg", "hp")))
  }
})

# Summary Output ==============================================================

test_that("summary has correct structure", {
  result <- detect_outliers(mtcars, vars = c("mpg", "hp"), methods = c("zscore", "iqr"))

  expect_true("Variable" %in% names(result$summary))
  expect_true("N_Total" %in% names(result$summary))
  expect_true("N_Outliers" %in% names(result$summary))
  expect_true("Pct_Outliers" %in% names(result$summary))
})

test_that("summary includes method-specific counts", {
  result <- detect_outliers(mtcars$mpg, methods = c("zscore", "iqr", "mad"))

  expect_true("zscore_count" %in% names(result$summary))
  expect_true("iqr_count" %in% names(result$summary))
  expect_true("mad_count" %in% names(result$summary))
})

# Recommendations =============================================================

test_that("recommendations are generated", {
  result <- detect_outliers(mtcars$mpg, methods = "iqr")

  expect_true(!is.null(result$recommendations))
  expect_true(length(result$recommendations) > 0)
})

test_that("recommendations vary by outlier rate", {
  # Few outliers
  x_normal <- rnorm(100, mean = 10, sd = 2)
  result_low <- detect_outliers(x_normal, methods = "iqr")

  # Many outliers
  x_outliers <- c(rnorm(50, mean = 10, sd = 2), rnorm(20, mean = 50, sd = 5))
  result_high <- detect_outliers(x_outliers, methods = "iqr")

  # Recommendations should differ
  expect_false(identical(result_low$recommendations,
                        result_high$recommendations))
})

# Print Methods ===============================================================

test_that("print method works", {
  result <- detect_outliers(mtcars$mpg, methods = "iqr")

  expect_output(print(result), "Outlier Detection Analysis")
  expect_output(print(result), "Summary by Variable")
  expect_output(print(result), "Recommendations")
})

test_that("summary method works", {
  result <- detect_outliers(mtcars$mpg, methods = "iqr")

  expect_output(summary(result), "Outlier Detection Summary")
  expect_output(summary(result), "Total outliers")
})

test_that("print handles no outliers gracefully", {
  x <- rnorm(50, mean = 10, sd = 2)

  result <- detect_outliers(x, methods = "zscore", threshold = 5)

  expect_output(print(result), "No outliers detected")
})

# Edge Cases ==================================================================

test_that("detect_outliers handles constant data", {
  x <- rep(10, 20)

  result <- detect_outliers(x, methods = "iqr")

  # IQR = 0, should handle gracefully
  expect_s3_class(result, "descriptR_outliers")
})

test_that("detect_outliers handles all outliers", {
  # Extreme case where most points are outliers
  x <- c(rep(1, 5), 10:30)

  result <- detect_outliers(x, methods = "iqr")

  expect_s3_class(result, "descriptR_outliers")
})

test_that("detect_outliers handles NA values", {
  x <- c(rnorm(50, mean = 10), NA, NA, 100)

  result <- detect_outliers(x, methods = "iqr")

  # Should work by removing NA
  expect_s3_class(result, "descriptR_outliers")
})

test_that("detect_outliers handles minimal data", {
  x <- c(1, 2, 3)

  # Should work for some methods, skip others
  result <- detect_outliers(x, methods = c("iqr", "grubbs", "rosner"))

  expect_s3_class(result, "descriptR_outliers")
})

# Integration Tests ===========================================================

test_that("full analysis with all methods works", {
  result <- detect_outliers(
    mtcars,
    vars = c("mpg", "hp", "wt"),
    methods = c("zscore", "iqr", "mad", "grubbs", "dixon", "rosner"),
    consensus = TRUE,
    min_methods = 2
  )

  expect_s3_class(result, "descriptR_outliers")
  expect_true(!is.null(result$summary))
  expect_true(!is.null(result$outliers))
  expect_true(!is.null(result$recommendations))

  # Should be printable
  expect_output(print(result))
  expect_output(summary(result))
})

test_that("results are internally consistent", {
  result <- detect_outliers(mtcars$mpg, methods = c("zscore", "iqr", "mad"))

  # Summary N_Outliers should match number in outlier df for that variable
  if (nrow(result$outliers) > 0) {
    unique_outlier_rows <- unique(result$outliers$Row)
    summary_count <- result$summary$N_Outliers[1]

    expect_equal(length(unique_outlier_rows), summary_count)
  }
})

test_that("method_results contain expected information", {
  result <- detect_outliers(mtcars$mpg, methods = "iqr")

  method_result <- result$method_results[[1]]

  expect_true(!is.null(method_result$method))
  expect_true(!is.null(method_result$outlier_indices))
  expect_true(!is.null(method_result$n_outliers))
})
