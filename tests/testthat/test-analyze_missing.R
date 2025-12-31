# Test suite for missing data analysis

# Main Function ===============================================================

test_that("analyze_missing works with complete data", {
  result <- analyze_missing(iris)

  expect_s3_class(result, "descriptR_missing")
  expect_equal(result$summary$missing_cells, 0)
  expect_equal(result$summary$n_complete_cases, nrow(iris))
})

test_that("analyze_missing detects missing data", {
  result <- analyze_missing(airquality)

  expect_s3_class(result, "descriptR_missing")
  expect_true(result$summary$missing_cells > 0)
  expect_true(result$summary$n_incomplete_cases > 0)
  expect_true(!is.null(result$variable_summary))
})

test_that("analyze_missing computes correct percentages", {
  result <- analyze_missing(airquality)

  # Check that percentages sum correctly
  pct_complete <- result$summary$pct_complete_cases
  pct_incomplete <- (result$summary$n_incomplete_cases /
                     result$summary$n_observations) * 100

  expect_equal(pct_complete + pct_incomplete, 100, tolerance = 0.01)
})

test_that("analyze_missing works with subset of variables", {
  result <- analyze_missing(airquality, vars = c("Ozone", "Solar.R"))

  expect_equal(result$summary$n_variables, 2)
  expect_equal(nrow(result$variable_summary), 2)
})

test_that("analyze_missing handles all parameters", {
  result <- analyze_missing(airquality,
                            test_mcar = TRUE,
                            pattern_analysis = TRUE,
                            correlation_analysis = TRUE,
                            include_plots = FALSE)

  expect_s3_class(result, "descriptR_missing")
  expect_true(!is.null(result$patterns))
  expect_true(!is.null(result$mcar_test))
  expect_true(!is.null(result$correlations))
})

# Variable Summary ============================================================

test_that("variable_summary is sorted by missing rate", {
  result <- analyze_missing(airquality)

  # Check that Pct_Missing is in descending order
  pct_missing <- result$variable_summary$Pct_Missing
  expect_equal(pct_missing, sort(pct_missing, decreasing = TRUE))
})

test_that("variable_summary has correct structure", {
  result <- analyze_missing(airquality)

  expect_true("Variable" %in% names(result$variable_summary))
  expect_true("N_Total" %in% names(result$variable_summary))
  expect_true("N_Missing" %in% names(result$variable_summary))
  expect_true("N_Present" %in% names(result$variable_summary))
  expect_true("Pct_Missing" %in% names(result$variable_summary))
})

test_that("variable_summary computes correct counts", {
  # Create test data with known missingness
  test_data <- data.frame(
    x = c(1, 2, NA, 4, 5),
    y = c(NA, NA, 3, 4, 5),
    z = c(1, 2, 3, 4, 5)
  )

  result <- analyze_missing(test_data)

  x_row <- result$variable_summary[result$variable_summary$Variable == "x", ]
  y_row <- result$variable_summary[result$variable_summary$Variable == "y", ]
  z_row <- result$variable_summary[result$variable_summary$Variable == "z", ]

  expect_equal(x_row$N_Missing, 1)
  expect_equal(y_row$N_Missing, 2)
  expect_equal(z_row$N_Missing, 0)

  expect_equal(x_row$Pct_Missing, 20)
  expect_equal(y_row$Pct_Missing, 40)
  expect_equal(z_row$Pct_Missing, 0)
})

# Pattern Analysis ============================================================

test_that("pattern analysis identifies patterns correctly", {
  result <- analyze_missing(airquality, pattern_analysis = TRUE)

  expect_true(!is.null(result$patterns))
  expect_true("Pattern_ID" %in% names(result$patterns))
  expect_true("N_Cases" %in% names(result$patterns))
  expect_true("Missing_Variables" %in% names(result$patterns))
})

test_that("pattern analysis respects max_patterns", {
  result <- analyze_missing(airquality, max_patterns = 5)

  expect_true(nrow(result$patterns) <= 5)
})

test_that("pattern analysis detects complete cases", {
  # Create data with some complete cases
  test_data <- data.frame(
    x = c(1, 2, 3, NA, 5),
    y = c(1, 2, 3, 4, NA)
  )

  result <- analyze_missing(test_data)

  # Should have pattern for complete cases
  complete_pattern <- result$patterns[
    grepl("Complete", result$patterns$Missing_Variables), ]

  expect_true(nrow(complete_pattern) > 0)
})

test_that("patterns sum to total observations", {
  result <- analyze_missing(airquality)

  total_in_patterns <- sum(result$patterns$N_Cases)
  expect_equal(total_in_patterns, nrow(airquality))
})

# MCAR Test ===================================================================

test_that("MCAR test runs on appropriate data", {
  result <- analyze_missing(airquality, test_mcar = TRUE)

  expect_true(!is.null(result$mcar_test))
  if (result$mcar_test$test_performed) {
    expect_true("test_statistic" %in% names(result$mcar_test))
    expect_true("p_value" %in% names(result$mcar_test))
    expect_true("interpretation" %in% names(result$mcar_test))
  }
})

test_that("MCAR test skips when no missing data", {
  result <- analyze_missing(iris, test_mcar = TRUE)

  expect_true(!is.null(result$mcar_test))
  # Should indicate no test performed or no missing data
  expect_true(is.null(result$mcar_test$p_value) ||
              !result$mcar_test$test_performed)
})

test_that("MCAR test can be disabled", {
  result1 <- analyze_missing(airquality, test_mcar = TRUE)
  result2 <- analyze_missing(airquality, test_mcar = FALSE)

  expect_true(!is.null(result1$mcar_test))
  expect_null(result2$mcar_test)
})

test_that("MCAR test handles insufficient numeric variables", {
  # Data with only one numeric variable
  test_data <- data.frame(
    x = c(1, 2, NA, 4, 5),
    y = c("a", "b", "c", "d", "e")
  )

  result <- analyze_missing(test_data, test_mcar = TRUE)

  # Should report error or not perform test
  if (!is.null(result$mcar_test)) {
    expect_false(result$mcar_test$test_performed)
  }
})

# Correlation Analysis ========================================================

test_that("correlation analysis identifies relationships", {
  result <- analyze_missing(airquality, correlation_analysis = TRUE)

  expect_true(!is.null(result$correlations))
  if (result$correlations$correlations_computed) {
    expect_true(!is.null(result$correlations$correlation_matrix))
  }
})

test_that("correlation analysis can be disabled", {
  result1 <- analyze_missing(airquality, correlation_analysis = TRUE)
  result2 <- analyze_missing(airquality, correlation_analysis = FALSE)

  expect_true(!is.null(result1$correlations))
  expect_null(result2$correlations)
})

test_that("correlation analysis handles insufficient variation", {
  # All values missing for one variable
  test_data <- data.frame(
    x = rep(NA, 10),
    y = 1:10,
    z = 11:20
  )

  result <- analyze_missing(test_data, correlation_analysis = TRUE)

  # Should handle gracefully
  expect_true(!is.null(result$correlations))
})

test_that("correlation analysis identifies significant pairs", {
  # Create data with correlated missingness
  set.seed(123)
  n <- 100
  missing_together <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7))

  test_data <- data.frame(
    x = ifelse(missing_together, NA, rnorm(n)),
    y = ifelse(missing_together, NA, rnorm(n)),
    z = rnorm(n)
  )

  result <- analyze_missing(test_data, correlation_analysis = TRUE)

  if (result$correlations$correlations_computed) {
    # Should find correlation between x and y missingness
    if (!is.null(result$correlations$significant_pairs)) {
      expect_true(nrow(result$correlations$significant_pairs) > 0)
    }
  }
})

# Recommendations =============================================================

test_that("recommendations are generated", {
  result <- analyze_missing(airquality)

  expect_true(!is.null(result$recommendations))
  expect_true(length(result$recommendations) > 0)
  expect_true(is.character(result$recommendations))
})

test_that("recommendations vary by missing rate", {
  # Low missing
  low_missing_data <- iris
  low_missing_data[1:2, 1] <- NA

  result_low <- analyze_missing(low_missing_data)

  # High missing
  high_missing_data <- airquality

  result_high <- analyze_missing(high_missing_data)

  # Recommendations should differ
  expect_false(identical(result_low$recommendations, result_high$recommendations))
})

test_that("recommendations identify high missing variables", {
  # Create variable with >50% missing
  test_data <- airquality
  test_data$HighMissing <- NA
  test_data$HighMissing[1:60] <- 1:60  # 60% present, 40% missing

  result <- analyze_missing(test_data)

  recommendations_text <- paste(result$recommendations, collapse = " ")
  # Should mention variables with high missingness
  expect_true(any(grepl(">20%", result$recommendations)) ||
              any(grepl(">50%", result$recommendations)))
})

# Print Methods ===============================================================

test_that("print method works", {
  result <- analyze_missing(airquality)

  expect_output(print(result), "Missing Data Analysis")
  expect_output(print(result), "Overall Summary")
  expect_output(print(result), "Recommendations")
})

test_that("summary method works", {
  result <- analyze_missing(airquality)

  expect_output(summary(result), "Missing Data Analysis Summary")
  expect_output(summary(result), "Missing data rate")
})

test_that("print handles complete data gracefully", {
  result <- analyze_missing(iris)

  expect_output(print(result), "No missing data detected")
})

# Edge Cases ==================================================================

test_that("analyze_missing handles single variable", {
  test_data <- data.frame(x = c(1, 2, NA, 4, 5))

  result <- analyze_missing(test_data)

  expect_s3_class(result, "descriptR_missing")
  expect_equal(result$summary$n_variables, 1)
})

test_that("analyze_missing handles all missing variable", {
  test_data <- data.frame(
    x = rep(NA, 10),
    y = 1:10
  )

  result <- analyze_missing(test_data)

  expect_s3_class(result, "descriptR_missing")
  expect_equal(result$variable_summary$N_Missing[
    result$variable_summary$Variable == "x"], 10)
})

test_that("analyze_missing handles completely missing data", {
  test_data <- data.frame(
    x = rep(NA, 10),
    y = rep(NA, 10)
  )

  result <- analyze_missing(test_data)

  expect_s3_class(result, "descriptR_missing")
  expect_equal(result$summary$n_complete_cases, 0)
  expect_equal(result$summary$pct_missing_overall, 100)
})

test_that("analyze_missing handles mixed data types", {
  test_data <- data.frame(
    numeric_var = c(1, 2, NA, 4, 5),
    char_var = c("a", NA, "c", "d", "e"),
    factor_var = factor(c("x", "y", NA, "z", "x")),
    date_var = as.Date(c("2020-01-01", "2020-01-02", NA,
                         "2020-01-04", "2020-01-05"))
  )

  expect_no_error(analyze_missing(test_data))
})

test_that("analyze_missing validates inputs", {
  expect_error(analyze_missing("not a data frame"), "must be a data frame")
  expect_error(analyze_missing(iris, vars = "NonexistentVar"),
               "not found in data")
})

# Integration Tests ===========================================================

test_that("full analysis runs end-to-end", {
  result <- analyze_missing(airquality,
                            vars = c("Ozone", "Solar.R", "Wind", "Temp"),
                            test_mcar = TRUE,
                            pattern_analysis = TRUE,
                            correlation_analysis = TRUE,
                            include_plots = FALSE,
                            max_patterns = 5)

  expect_s3_class(result, "descriptR_missing")
  expect_true(!is.null(result$summary))
  expect_true(!is.null(result$variable_summary))
  expect_true(!is.null(result$patterns))
  expect_true(!is.null(result$recommendations))

  # Should be printable
  expect_output(print(result))
  expect_output(summary(result))
})

test_that("analysis results are internally consistent", {
  result <- analyze_missing(airquality)

  # Total missing should equal sum across variables
  var_total_missing <- sum(result$variable_summary$N_Missing)
  expect_equal(var_total_missing, result$summary$missing_cells)

  # Percentages should be consistent
  for (i in 1:nrow(result$variable_summary)) {
    var_row <- result$variable_summary[i, ]
    expected_pct <- (var_row$N_Missing / var_row$N_Total) * 100
    expect_equal(var_row$Pct_Missing, expected_pct, tolerance = 0.01)
  }
})
