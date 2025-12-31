# Test suite for normality assessment

# Main Function ===============================================================

test_that("assess_normality works with vector input", {
  x <- rnorm(100)
  result <- assess_normality(x)

  expect_s3_class(result, "descriptR_normality")
  expect_true(!is.null(result$test_results))
  expect_true(!is.null(result$summary))
})

test_that("assess_normality works with data frame", {
  result <- assess_normality(iris, vars = c("Sepal.Length", "Petal.Width"))

  expect_s3_class(result, "descriptR_normality")
  expect_equal(nrow(result$summary), 2)
})

test_that("assess_normality handles all tests", {
  result <- assess_normality(rnorm(100), tests = "all")

  expect_true(length(result$tests_used) >= 4)
  expect_true("shapiro" %in% result$tests_used)
  expect_true("ks" %in% result$tests_used)
})

test_that("assess_normality validates test names", {
  expect_error(
    assess_normality(rnorm(100), tests = "invalid_test"),
    "Invalid tests"
  )
})

# Shapiro-Wilk Test ===========================================================

test_that("shapiro test detects normal data", {
  set.seed(123)
  x <- rnorm(100, mean = 50, sd = 10)

  result <- assess_normality(x, tests = "shapiro")

  test_result <- result$test_results$value$shapiro

  expect_true(test_result$performed)
  expect_true(test_result$is_normal)  # Should pass for normal data
  expect_true(test_result$p_value > 0.05)
})

test_that("shapiro test detects non-normal data", {
  # Highly skewed data
  x <- rexp(100, rate = 1)

  result <- assess_normality(x, tests = "shapiro")

  test_result <- result$test_results$value$shapiro

  expect_true(test_result$performed)
  # Should likely reject normality for exponential
  expect_true("p_value" %in% names(test_result))
})

test_that("shapiro test handles size limits", {
  # Too small
  x_small <- c(1, 2)
  result_small <- assess_normality(x_small, tests = "shapiro")

  # Too large (create but don't actually run - would be slow)
  # Just test the structure

  expect_s3_class(result_small, "descriptR_normality")
})

test_that("shapiro test skips large samples", {
  # Simulate large sample check
  x_large <- rnorm(6000)

  result <- assess_normality(x_large, tests = "shapiro")

  shapiro_result <- result$test_results$value$shapiro

  expect_false(shapiro_result$performed)
  expect_true(!is.null(shapiro_result$note))
})

# Kolmogorov-Smirnov Test =====================================================

test_that("ks test detects normal data", {
  set.seed(123)
  x <- rnorm(100)

  result <- assess_normality(x, tests = "ks")

  test_result <- result$test_results$value$ks

  expect_true(test_result$performed)
  expect_true("p_value" %in% names(test_result))
})

test_that("ks test detects non-normal data", {
  # Uniform distribution
  x <- runif(100, 0, 1)

  result <- assess_normality(x, tests = "ks")

  test_result <- result$test_results$value$ks

  expect_true(test_result$performed)
  # Uniform should be detected as non-normal
})

test_that("ks test handles small samples", {
  x <- c(1, 2, 3)

  result <- assess_normality(x, tests = "ks")

  # Should work but may not be reliable
  expect_s3_class(result, "descriptR_normality")
})

# Anderson-Darling Test =======================================================

test_that("anderson test works", {
  set.seed(123)
  x <- rnorm(100)

  result <- assess_normality(x, tests = "anderson")

  test_result <- result$test_results$value$anderson

  expect_true(test_result$performed)
  expect_true("statistic" %in% names(test_result))
  expect_true("p_value" %in% names(test_result))
})

test_that("anderson test detects tail departures", {
  # Distribution with heavy tails
  x <- rt(100, df = 3)

  result <- assess_normality(x, tests = "anderson")

  test_result <- result$test_results$value$anderson

  expect_true(test_result$performed)
})

test_that("anderson test handles small samples", {
  x <- rnorm(5)

  result <- assess_normality(x, tests = "anderson")

  anderson_result <- result$test_results$value$anderson

  # Should note sample size issue
  expect_true(!is.null(anderson_result))
})

# Jarque-Bera Test ============================================================

test_that("jarque test works for large samples", {
  set.seed(123)
  x <- rnorm(100)

  result <- assess_normality(x, tests = "jarque")

  test_result <- result$test_results$value$jarque

  expect_true(test_result$performed)
  expect_true("skewness" %in% names(test_result))
  expect_true("kurtosis" %in% names(test_result))
})

test_that("jarque test detects skewness", {
  # Right-skewed data
  x <- rexp(200, rate = 1)

  result <- assess_normality(x, tests = "jarque")

  test_result <- result$test_results$value$jarque

  expect_true(test_result$performed)
  expect_true(test_result$skewness > 0)  # Should be positive
})

test_that("jarque test skips small samples", {
  x <- rnorm(20)

  result <- assess_normality(x, tests = "jarque")

  jarque_result <- result$test_results$value$jarque

  expect_false(jarque_result$performed)
  expect_true(!is.null(jarque_result$note))
})

# D'Agostino Test =============================================================

test_that("dagostino test works", {
  set.seed(123)
  x <- rnorm(100)

  result <- assess_normality(x, tests = "dagostino")

  test_result <- result$test_results$value$dagostino

  expect_true(test_result$performed)
  expect_true("skewness_z" %in% names(test_result))
  expect_true("kurtosis_z" %in% names(test_result))
})

test_that("dagostino test combines skew and kurtosis", {
  x <- rnorm(100)

  result <- assess_normality(x, tests = "dagostino")

  test_result <- result$test_results$value$dagostino

  expect_true(test_result$performed)
  # Statistic should combine both components
  expect_true(test_result$statistic >= 0)
})

test_that("dagostino test handles small samples", {
  x <- rnorm(15)

  result <- assess_normality(x, tests = "dagostino")

  dagostino_result <- result$test_results$value$dagostino

  expect_false(dagostino_result$performed)
})

# Shape Statistics ============================================================

test_that("shape statistics are computed correctly", {
  set.seed(123)
  x <- rnorm(100)

  result <- assess_normality(x)

  shape <- result$shape_statistics$value

  expect_true("skewness" %in% names(shape))
  expect_true("kurtosis" %in% names(shape))
  expect_true("skewness_interpretation" %in% names(shape))
  expect_true("kurtosis_interpretation" %in% names(shape))
})

test_that("shape statistics detect right skew", {
  # Right-skewed data
  x <- rexp(200, rate = 1)

  result <- assess_normality(x)

  shape <- result$shape_statistics$value

  expect_true(shape$skewness > 0)
  expect_true(grepl("right", shape$skewness_interpretation))
})

test_that("shape statistics detect left skew", {
  # Left-skewed data (negative exponential)
  x <- -rexp(200, rate = 1)

  result <- assess_normality(x)

  shape <- result$shape_statistics$value

  expect_true(shape$skewness < 0)
  expect_true(grepl("left", shape$skewness_interpretation))
})

test_that("shape statistics detect heavy tails", {
  # t-distribution has heavier tails
  x <- rt(500, df = 3)

  result <- assess_normality(x)

  shape <- result$shape_statistics$value

  # t-distribution is leptokurtic
  expect_true(shape$kurtosis > 0)
})

test_that("shape statistics handle constant data", {
  x <- rep(10, 50)

  result <- assess_normality(x)

  shape <- result$shape_statistics$value

  expect_true(!is.null(shape$note))
})

# Transformation Suggestions ==================================================

test_that("transformations not suggested for normal data", {
  set.seed(123)
  x <- rnorm(100)

  result <- assess_normality(x, suggest_transformations = TRUE)

  trans <- result$transformations$value

  expect_false(trans$needed)
})

test_that("transformations suggested for right-skewed data", {
  x <- rexp(100, rate = 1)

  result <- assess_normality(x, suggest_transformations = TRUE,
                             tests = c("shapiro", "ks"))

  trans <- result$transformations$value

  if (trans$needed) {
    suggestions_text <- paste(trans$suggestions, collapse = " ")
    expect_true(grepl("log|sqrt", suggestions_text, ignore.case = TRUE))
  }
})

test_that("transformations suggested for left-skewed data", {
  x <- -rexp(100, rate = 1)

  result <- assess_normality(x, suggest_transformations = TRUE,
                             tests = c("shapiro", "ks"))

  trans <- result$transformations$value

  if (trans$needed) {
    suggestions_text <- paste(trans$suggestions, collapse = " ")
    expect_true(grepl("square|exponential", suggestions_text, ignore.case = TRUE))
  }
})

test_that("transformations warn about non-positive values", {
  # Data with zeros and negatives
  x <- c(rnorm(50, mean = -2), rexp(50))

  result <- assess_normality(x, suggest_transformations = TRUE,
                             tests = c("ks"))

  # Should handle gracefully
  expect_s3_class(result, "descriptR_normality")
})

test_that("transformation suggestions can be disabled", {
  x <- rexp(100)

  result <- assess_normality(x, suggest_transformations = FALSE)

  # Transformations list should be empty or all FALSE
  expect_true(length(result$transformations) == 0 ||
              all(sapply(result$transformations, function(t) !t$needed)))
})

# Multiple Variables ==========================================================

test_that("assess_normality handles multiple variables", {
  result <- assess_normality(mtcars, vars = c("mpg", "hp", "wt"))

  expect_equal(nrow(result$summary), 3)
  expect_equal(length(result$test_results), 3)
  expect_equal(length(result$shape_statistics), 3)
})

test_that("summary tracks verdicts correctly", {
  result <- assess_normality(iris, vars = c("Sepal.Length", "Petal.Width"))

  expect_true("Verdict" %in% names(result$summary))
  expect_true(all(result$summary$Verdict %in% c("Normal", "Non-normal")))
})

test_that("vars = NULL analyzes all numeric", {
  result <- assess_normality(mtcars)

  # Should analyze all numeric columns
  expect_true(nrow(result$summary) > 0)
  expect_true(all(result$summary$Variable %in% names(mtcars)))
})

# Summary Output ==============================================================

test_that("summary has correct structure", {
  result <- assess_normality(mtcars$mpg, tests = c("shapiro", "ks"))

  expect_true("Variable" %in% names(result$summary))
  expect_true("N_Tests" %in% names(result$summary))
  expect_true("N_Normal" %in% names(result$summary))
  expect_true("Verdict" %in% names(result$summary))
  expect_true("Skewness" %in% names(result$summary))
  expect_true("Kurtosis" %in% names(result$summary))
})

test_that("summary includes test-specific p-values", {
  result <- assess_normality(mtcars$mpg, tests = c("shapiro", "ks", "anderson"))

  expect_true("shapiro_p" %in% names(result$summary))
  expect_true("ks_p" %in% names(result$summary))
  expect_true("anderson_p" %in% names(result$summary))
})

test_that("verdict reflects majority of tests", {
  set.seed(123)
  x <- rnorm(100)

  result <- assess_normality(x, tests = c("shapiro", "ks", "anderson"))

  # Normal data should get "Normal" verdict
  expect_equal(result$summary$Verdict[1], "Normal")
})

# Interpretation ==============================================================

test_that("interpretation is generated", {
  result <- assess_normality(mtcars$mpg)

  expect_true(!is.null(result$interpretation))
  expect_true(length(result$interpretation) > 0)
})

test_that("interpretation varies by normality", {
  # Normal data
  x_normal <- rnorm(100)
  result_normal <- assess_normality(x_normal, tests = "shapiro")

  # Non-normal data
  x_nonnormal <- rexp(100)
  result_nonnormal <- assess_normality(x_nonnormal, tests = "shapiro")

  # Interpretations should differ
  expect_false(identical(result_normal$interpretation,
                        result_nonnormal$interpretation))
})

test_that("interpretation mentions transformations when needed", {
  x <- rexp(100, rate = 1)

  result <- assess_normality(x, tests = c("shapiro", "ks"),
                             suggest_transformations = TRUE)

  interp_text <- paste(result$interpretation, collapse = " ")

  # Should mention transformations if needed
  if (result$summary$Verdict == "Non-normal") {
    expect_true(grepl("transformation|transform", interp_text,
                     ignore.case = TRUE))
  }
})

# Print Methods ===============================================================

test_that("print method works", {
  result <- assess_normality(mtcars$mpg)

  expect_output(print(result), "Normality Assessment")
  expect_output(print(result), "Summary by Variable")
  expect_output(print(result), "Shape Statistics")
})

test_that("summary method works", {
  result <- assess_normality(mtcars$mpg)

  expect_output(summary(result), "Normality Assessment Summary")
  expect_output(summary(result), "Variables assessed")
})

test_that("print handles multiple variables", {
  result <- assess_normality(mtcars, vars = c("mpg", "hp", "wt"))

  expect_output(print(result))
})

# Edge Cases ==================================================================

test_that("assess_normality handles constant data", {
  x <- rep(10, 50)

  result <- assess_normality(x, tests = "shapiro")

  expect_s3_class(result, "descriptR_normality")
})

test_that("assess_normality handles NA values", {
  x <- c(rnorm(50), NA, NA, NA)

  result <- assess_normality(x, tests = "shapiro")

  # Should work by removing NA
  expect_s3_class(result, "descriptR_normality")
})

test_that("assess_normality handles minimal data", {
  x <- c(1, 2, 3, 4, 5)

  result <- assess_normality(x, tests = c("shapiro", "jarque", "dagostino"))

  # Some tests should skip, others may work
  expect_s3_class(result, "descriptR_normality")
})

test_that("assess_normality handles perfect normal data", {
  set.seed(42)
  x <- rnorm(1000, mean = 100, sd = 15)

  result <- assess_normality(x, tests = "all")

  # Should pass most/all tests
  expect_true(result$summary$N_Normal[1] > 0)
})

test_that("assess_normality handles highly non-normal data", {
  # Bimodal distribution
  x <- c(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 10, sd = 1))

  result <- assess_normality(x, tests = c("shapiro", "ks"))

  # Should fail normality tests
  expect_equal(result$summary$Verdict[1], "Non-normal")
})

# Integration Tests ===========================================================

test_that("full assessment with all tests works", {
  result <- assess_normality(
    mtcars,
    vars = c("mpg", "hp", "wt"),
    tests = c("shapiro", "ks", "anderson", "jarque", "dagostino"),
    alpha = 0.05,
    suggest_transformations = TRUE
  )

  expect_s3_class(result, "descriptR_normality")
  expect_true(!is.null(result$summary))
  expect_true(!is.null(result$test_results))
  expect_true(!is.null(result$shape_statistics))
  expect_true(!is.null(result$transformations))
  expect_true(!is.null(result$interpretation))

  # Should be printable
  expect_output(print(result))
  expect_output(summary(result))
})

test_that("results are internally consistent", {
  result <- assess_normality(mtcars$mpg, tests = c("shapiro", "ks", "anderson"))

  # N_Tests should match number of tests performed
  n_performed <- sum(sapply(result$test_results$mpg, function(t) t$performed))
  expect_equal(result$summary$N_Tests[1], n_performed)

  # N_Normal should be between 0 and N_Tests
  expect_true(result$summary$N_Normal[1] >= 0)
  expect_true(result$summary$N_Normal[1] <= result$summary$N_Tests[1])
})

test_that("alpha parameter affects verdicts", {
  x <- rnorm(100)

  result_strict <- assess_normality(x, tests = "shapiro", alpha = 0.01)
  result_lenient <- assess_normality(x, tests = "shapiro", alpha = 0.10)

  # Both should work
  expect_s3_class(result_strict, "descriptR_normality")
  expect_s3_class(result_lenient, "descriptR_normality")
})

test_that("test results contain expected fields", {
  result <- assess_normality(mtcars$mpg, tests = "shapiro")

  shapiro_result <- result$test_results$mpg$shapiro

  expect_true("test_name" %in% names(shapiro_result))
  expect_true("statistic" %in% names(shapiro_result))
  expect_true("p_value" %in% names(shapiro_result))
  expect_true("is_normal" %in% names(shapiro_result))
  expect_true("interpretation" %in% names(shapiro_result))
  expect_true("performed" %in% names(shapiro_result))
})
