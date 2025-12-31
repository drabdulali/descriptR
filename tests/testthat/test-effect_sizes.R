# Test suite for effect size calculations

# Cohen's d ===================================================================

test_that("compute_cohens_d works for independent samples", {
  x <- rnorm(50, mean = 10, sd = 2)
  y <- rnorm(50, mean = 12, sd = 2)

  result <- compute_cohens_d(x, y)

  expect_s3_class(result, "descriptR_cohens_d")
  expect_true("d" %in% names(result))
  expect_true("conf_int" %in% names(result))
  expect_true("interpretation" %in% names(result))
  expect_length(result$conf_int, 2)
})

test_that("compute_cohens_d with Hedges correction works", {
  x <- rnorm(20, mean = 10, sd = 2)
  y <- rnorm(20, mean = 11, sd = 2)

  d_regular <- compute_cohens_d(x, y, hedges = FALSE)
  d_hedges <- compute_cohens_d(x, y, hedges = TRUE)

  # Hedges' g should be slightly smaller than Cohen's d
  expect_true(abs(d_hedges$d) <= abs(d_regular$d))
  expect_true(grepl("Hedges", d_hedges$method))
})

test_that("compute_cohens_d works for paired samples", {
  before <- rnorm(30, mean = 100, sd = 10)
  after <- before + rnorm(30, mean = 5, sd = 5)

  result <- compute_cohens_d(before, after, paired = TRUE)

  expect_s3_class(result, "descriptR_cohens_d")
  expect_true(grepl("paired", result$method))
})

test_that("compute_cohens_d Glass's delta works", {
  x <- rnorm(30, mean = 10, sd = 2)
  y <- rnorm(30, mean = 12, sd = 3)

  result <- compute_cohens_d(x, y, pooled = FALSE)

  expect_s3_class(result, "descriptR_cohens_d")
  expect_true(grepl("Glass", result$method))
})

test_that("compute_cohens_d handles NA values", {
  x <- c(rnorm(20, mean = 10), NA, NA)
  y <- c(rnorm(20, mean = 12), NA)

  expect_no_error(compute_cohens_d(x, y, na.rm = TRUE))
})

test_that("compute_cohens_d validates paired input lengths", {
  x <- rnorm(20)
  y <- rnorm(25)

  expect_error(compute_cohens_d(x, y, paired = TRUE), "same length")
})

# Eta-Squared =================================================================

test_that("compute_eta_squared works for classical", {
  model <- aov(Sepal.Length ~ Species, data = iris)
  result <- compute_eta_squared(model, type = "classical")

  expect_s3_class(result, "descriptR_eta_squared")
  expect_true("eta_squared" %in% names(result))
  expect_true(result$eta_squared >= 0 && result$eta_squared <= 1)
  expect_equal(result$type, "classical")
})

test_that("compute_eta_squared works for partial", {
  model <- aov(Sepal.Length ~ Species, data = iris)
  result <- compute_eta_squared(model, type = "partial")

  expect_s3_class(result, "descriptR_eta_squared")
  expect_equal(result$type, "partial")
  expect_true(result$eta_squared >= 0 && result$eta_squared <= 1)
})

test_that("compute_eta_squared computes confidence intervals", {
  model <- aov(Sepal.Length ~ Species, data = iris)
  result <- compute_eta_squared(model)

  expect_length(result$conf_int, 2)
  expect_true(result$conf_int[1] <= result$eta_squared)
  expect_true(result$conf_int[2] >= result$eta_squared)
})

test_that("compute_eta_squared has correct interpretation", {
  model <- aov(Sepal.Length ~ Species, data = iris)
  result <- compute_eta_squared(model)

  expect_true(nchar(result$interpretation) > 0)
  expect_true(grepl("η²", result$interpretation))
})

# Omega-Squared ===============================================================

test_that("compute_omega_squared works", {
  model <- aov(Sepal.Length ~ Species, data = iris)
  result <- compute_omega_squared(model)

  expect_s3_class(result, "descriptR_omega_squared")
  expect_true("omega_squared" %in% names(result))
  expect_true(result$omega_squared >= 0)
})

test_that("omega_squared is less than or equal to eta_squared", {
  model <- aov(Sepal.Length ~ Species, data = iris)

  eta_result <- compute_eta_squared(model, type = "classical")
  omega_result <- compute_omega_squared(model)

  # Omega-squared should be less biased (typically smaller)
  expect_true(omega_result$omega_squared <= eta_result$eta_squared + 0.01)
})

test_that("compute_omega_squared handles small effects", {
  # Create data with minimal group differences
  set.seed(123)
  df <- data.frame(
    y = rnorm(60),
    group = rep(c("A", "B", "C"), each = 20)
  )

  model <- aov(y ~ group, data = df)
  result <- compute_omega_squared(model)

  # Should be close to 0
  expect_true(result$omega_squared >= 0)
  expect_true(result$omega_squared < 0.2)
})

# Cramér's V ==================================================================

test_that("compute_cramers_v works for independence test", {
  result <- compute_cramers_v(mtcars$vs, mtcars$am)

  expect_s3_class(result, "descriptR_cramers_v")
  expect_true("cramers_v" %in% names(result))
  expect_true(result$cramers_v >= 0 && result$cramers_v <= 1)
})

test_that("compute_cramers_v works with table input", {
  tab <- table(mtcars$vs, mtcars$am)
  result <- compute_cramers_v(tab)

  expect_s3_class(result, "descriptR_cramers_v")
})

test_that("compute_cramers_v bias correction works", {
  tab <- table(mtcars$vs, mtcars$am)

  v_corrected <- compute_cramers_v(tab, bias_correct = TRUE)
  v_uncorrected <- compute_cramers_v(tab, bias_correct = FALSE)

  # Bias correction typically reduces the estimate
  expect_true(v_corrected$cramers_v <= v_uncorrected$cramers_v + 0.01)
})

test_that("compute_cramers_v interpretation is correct", {
  result <- compute_cramers_v(mtcars$vs, mtcars$am)

  expect_true(nchar(result$interpretation) > 0)
  expect_true(grepl("Cramér's V", result$interpretation))
})

# Phi Coefficient =============================================================

test_that("compute_phi works for 2x2 tables", {
  result <- compute_phi(mtcars$vs, mtcars$am)

  expect_s3_class(result, "descriptR_phi")
  expect_true("phi" %in% names(result))
  expect_true(result$phi >= -1 && result$phi <= 1)
})

test_that("compute_phi equals Cramér's V for 2x2 tables", {
  phi_result <- compute_phi(mtcars$vs, mtcars$am)
  v_result <- compute_cramers_v(mtcars$vs, mtcars$am, bias_correct = FALSE)

  # Phi should equal Cramér's V for 2x2 tables
  expect_equal(abs(phi_result$phi), v_result$cramers_v, tolerance = 0.01)
})

test_that("compute_phi requires 2x2 table", {
  # Create 3x2 table
  x <- sample(1:3, 100, replace = TRUE)
  y <- sample(1:2, 100, replace = TRUE)

  expect_error(compute_phi(x, y), "2x2 table")
})

test_that("compute_phi works with table input", {
  tab <- matrix(c(10, 5, 3, 12), nrow = 2)
  result <- compute_phi(tab)

  expect_s3_class(result, "descriptR_phi")
})

# Effect Size Conversion ======================================================

test_that("convert_effect_size d to r works", {
  d <- 0.5
  r <- convert_effect_size(d, from = "d", to = "r")

  # Expected: r = d / sqrt(d^2 + 4)
  expected_r <- d / sqrt(d^2 + 4)
  expect_equal(r, expected_r, tolerance = 0.001)
})

test_that("convert_effect_size r to d works", {
  r <- 0.3
  d <- convert_effect_size(r, from = "r", to = "d")

  # Expected: d = 2r / sqrt(1 - r^2)
  expected_d <- 2 * r / sqrt(1 - r^2)
  expect_equal(d, expected_d, tolerance = 0.001)
})

test_that("convert_effect_size d to eta_sq works", {
  d <- 0.8
  eta_sq <- convert_effect_size(d, from = "d", to = "eta_sq")

  # Expected: eta_sq = d^2 / (d^2 + 4)
  expected_eta <- d^2 / (d^2 + 4)
  expect_equal(eta_sq, expected_eta, tolerance = 0.001)
})

test_that("convert_effect_size round-trip conversions work", {
  original_d <- 0.5

  # d -> r -> d
  r <- convert_effect_size(original_d, from = "d", to = "r")
  back_to_d <- convert_effect_size(r, from = "r", to = "d")
  expect_equal(back_to_d, original_d, tolerance = 0.001)

  # d -> eta_sq -> d
  eta <- convert_effect_size(original_d, from = "d", to = "eta_sq")
  back_to_d2 <- convert_effect_size(eta, from = "eta_sq", to = "d")
  expect_equal(back_to_d2, original_d, tolerance = 0.001)
})

test_that("convert_effect_size handles same metric", {
  d <- 0.5
  result <- convert_effect_size(d, from = "d", to = "d")
  expect_equal(result, d)
})

test_that("convert_effect_size validates metrics", {
  expect_error(convert_effect_size(0.5, from = "invalid", to = "d"),
               "Invalid metric")
  expect_error(convert_effect_size(0.5, from = "d", to = "invalid"),
               "Invalid metric")
})

# Comprehensive Effect Sizes ==================================================

test_that("compute_all_effect_sizes works for t.test", {
  # Two groups only
  df <- iris[iris$Species != "versicolor", ]

  result <- compute_all_effect_sizes(df, "Sepal.Length", "Species",
                                      type = "t.test")

  expect_s3_class(result, "descriptR_all_effect_sizes")
  expect_true("cohens_d" %in% names(result))
  expect_true("hedges_g" %in% names(result))
  expect_true("glass_delta" %in% names(result))
  expect_true("r_equivalent" %in% names(result))
})

test_that("compute_all_effect_sizes works for anova", {
  result <- compute_all_effect_sizes(iris, "Sepal.Length", "Species",
                                      type = "anova")

  expect_s3_class(result, "descriptR_all_effect_sizes")
  expect_true("eta_squared" %in% names(result))
  expect_true("partial_eta_squared" %in% names(result))
  expect_true("omega_squared" %in% names(result))
})

test_that("compute_all_effect_sizes works for chi.square", {
  result <- compute_all_effect_sizes(mtcars, "vs", "am",
                                      type = "chi.square")

  expect_s3_class(result, "descriptR_all_effect_sizes")
  expect_true("cramers_v" %in% names(result))
  expect_true("phi" %in% names(result))  # Both are binary
})

test_that("compute_all_effect_sizes validates input", {
  expect_error(compute_all_effect_sizes(iris, "Sepal.Length", "Species",
                                        type = "t.test"),
               "2 groups")
})

# Print Methods ===============================================================

test_that("print methods work without error", {
  # Cohen's d
  d_result <- compute_cohens_d(rnorm(30, 10), rnorm(30, 12))
  expect_output(print(d_result), "Cohen's d")

  # Eta-squared
  model <- aov(Sepal.Length ~ Species, data = iris)
  eta_result <- compute_eta_squared(model)
  expect_output(print(eta_result), "η²")

  # Omega-squared
  omega_result <- compute_omega_squared(model)
  expect_output(print(omega_result), "ω²")

  # Cramér's V
  v_result <- compute_cramers_v(mtcars$vs, mtcars$am)
  expect_output(print(v_result), "Cramér's V")

  # Phi
  phi_result <- compute_phi(mtcars$vs, mtcars$am)
  expect_output(print(phi_result), "Phi")

  # All effect sizes
  df <- iris[iris$Species != "versicolor", ]
  all_result <- compute_all_effect_sizes(df, "Sepal.Length", "Species",
                                          type = "t.test")
  expect_output(print(all_result), "Comprehensive")
})

# Edge Cases and Robustness ===================================================

test_that("effect sizes handle perfect separation", {
  # Groups with no overlap
  x <- rep(1, 20)
  y <- rep(2, 20)

  result <- compute_cohens_d(x, y)
  expect_true(is.finite(result$d))
  expect_true(abs(result$d) > 2)  # Should be very large
})

test_that("effect sizes handle identical groups", {
  # No difference between groups
  x <- rnorm(30, mean = 10, sd = 2)
  y <- x + rnorm(30, mean = 0, sd = 0.001)  # Essentially same

  result <- compute_cohens_d(x, y)
  expect_true(abs(result$d) < 0.1)  # Should be very small
})

test_that("cramers_v handles small expected frequencies", {
  # Small contingency table
  tab <- matrix(c(2, 1, 1, 2), nrow = 2)

  result <- suppressWarnings(compute_cramers_v(tab))
  expect_s3_class(result, "descriptR_cramers_v")
  expect_true(is.finite(result$cramers_v))
})

test_that("eta_squared handles one-way designs correctly", {
  # Simple one-way ANOVA
  df <- data.frame(
    y = c(rnorm(20, 5), rnorm(20, 6), rnorm(20, 7)),
    group = rep(c("A", "B", "C"), each = 20)
  )

  model <- aov(y ~ group, data = df)
  result <- compute_eta_squared(model)

  expect_true(result$eta_squared >= 0 && result$eta_squared <= 1)
})
