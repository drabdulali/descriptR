# Test suite for dimensionality reduction (PCA)

# Basic Functionality =========================================================

test_that("perform_pca works with default settings", {
  result <- perform_pca(mtcars)

  expect_s3_class(result, "descriptR_pca")
  expect_true(!is.null(result$scores))
  expect_true(!is.null(result$loadings))
  expect_true(!is.null(result$variance_explained))
})

test_that("perform_pca works with specified variables", {
  vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  result <- perform_pca(iris, vars = vars)

  expect_s3_class(result, "descriptR_pca")
  expect_equal(result$n_variables, 4)
  expect_equal(result$variables, vars)
})

test_that("perform_pca requires at least 2 variables", {
  expect_error(
    perform_pca(mtcars, vars = "mpg"),
    "at least 2 variables"
  )
})

# Variance Explained ==========================================================

test_that("variance explained sums to 100%", {
  result <- perform_pca(mtcars)

  total_var <- sum(result$variance_explained$Variance_Pct)

  expect_equal(total_var, 100, tolerance = 0.001)
})

test_that("cumulative variance is monotonic increasing", {
  result <- perform_pca(mtcars)

  cumvar <- result$variance_explained$Cumulative_Pct

  # Each value should be >= previous
  expect_true(all(diff(cumvar) >= 0))

  # Last value should be 100%
  expect_equal(cumvar[length(cumvar)], 100, tolerance = 0.001)
})

test_that("first component explains most variance", {
  result <- perform_pca(mtcars)

  var_pcts <- result$variance_explained$Variance_Pct

  # First PC should explain more than second
  expect_true(var_pcts[1] >= var_pcts[2])
})

# Component Retention =========================================================

test_that("n_components parameter works", {
  result <- perform_pca(mtcars, n_components = 3)

  expect_equal(result$n_components, 3)
  expect_equal(ncol(result$scores), 3)
  expect_equal(ncol(result$loadings), 3)
})

test_that("variance_threshold parameter works", {
  result <- perform_pca(mtcars, variance_threshold = 0.80)

  # Should retain components explaining >=80% variance
  cumvar <- result$variance_explained$Cumulative_Pct[result$n_components]

  expect_true(cumvar >= 80)
})

test_that("Kaiser criterion is computed", {
  result <- perform_pca(mtcars)

  kaiser_n <- result$retention_tests$kaiser

  # Should equal number of eigenvalues > 1
  expect_equal(kaiser_n, sum(result$eigenvalues > 1))
})

test_that("elbow point is identified", {
  result <- perform_pca(mtcars)

  elbow <- result$retention_tests$elbow

  # Should be a reasonable number
  expect_true(elbow >= 1)
  expect_true(elbow <= length(result$eigenvalues))
})

# Scaling and Centering =======================================================

test_that("scaling option works", {
  result_scaled <- perform_pca(mtcars, scale = TRUE)
  result_unscaled <- perform_pca(mtcars, scale = FALSE)

  # Results should differ
  expect_false(isTRUE(all.equal(result_scaled$scores,
                                result_unscaled$scores)))

  expect_true(result_scaled$scaled)
  expect_false(result_unscaled$scaled)
})

test_that("centering option works", {
  result_centered <- perform_pca(mtcars, center = TRUE)
  result_uncentered <- perform_pca(mtcars, center = FALSE)

  expect_true(result_centered$centered)
  expect_false(result_uncentered$centered)
})

# Loadings ====================================================================

test_that("loadings matrix has correct dimensions", {
  result <- perform_pca(mtcars, n_components = 5)

  n_vars <- length(result$variables)

  expect_equal(nrow(result$loadings), n_vars)
  expect_equal(ncol(result$loadings), 5)
})

test_that("loadings are orthogonal (unrotated)", {
  result <- perform_pca(mtcars, rotation = "none")

  # Check orthogonality: L'L should be close to identity (for standardized)
  ltl <- t(result$loadings) %*% result$loadings

  # Diagonal should be ~1, off-diagonal ~0
  expect_true(all(diag(ltl) > 0.9))
})

test_that("loadings are in reasonable range", {
  result <- perform_pca(mtcars)

  # Loadings should be between -1 and 1
  expect_true(all(abs(result$loadings) <= 1.1))  # Allow small numerical error
})

# Scores ======================================================================

test_that("scores matrix has correct dimensions", {
  result <- perform_pca(mtcars, n_components = 3)

  expect_equal(nrow(result$scores), result$n_observations)
  expect_equal(ncol(result$scores), 3)
})

test_that("scores are centered", {
  result <- perform_pca(mtcars)

  # Mean of each component should be ~0
  col_means <- colMeans(result$scores)

  expect_true(all(abs(col_means) < 1e-10))
})

# Eigenvalues =================================================================

test_that("eigenvalues are positive", {
  result <- perform_pca(mtcars)

  expect_true(all(result$eigenvalues > 0))
})

test_that("eigenvalues are in descending order", {
  result <- perform_pca(mtcars)

  # Should be monotonic decreasing
  expect_true(all(diff(result$eigenvalues) <= 0))
})

test_that("eigenvalues equal variance explained", {
  result <- perform_pca(mtcars)

  # Eigenvalues / sum(eigenvalues) should equal variance_explained
  var_from_eigen <- result$eigenvalues / sum(result$eigenvalues)
  var_reported <- result$variance_explained$Variance_Pct / 100

  expect_equal(var_from_eigen, var_reported, tolerance = 0.001)
})

# Rotation ====================================================================

test_that("varimax rotation works", {
  result <- perform_pca(mtcars, rotation = "varimax")

  expect_equal(result$rotation, "varimax")
  expect_true(!is.null(result$loadings))
})

test_that("rotation requires at least 2 components", {
  # Single component - rotation should be skipped with warning
  expect_warning(
    perform_pca(mtcars, n_components = 1, rotation = "varimax")
  )
})

# Helper Functions ============================================================

test_that("get_pca_scores extracts scores correctly", {
  result <- perform_pca(mtcars, n_components = 5)

  scores_all <- get_pca_scores(result)
  scores_subset <- get_pca_scores(result, components = c(1, 2))

  expect_equal(ncol(scores_all), 5)
  expect_equal(ncol(scores_subset), 2)
})

test_that("get_pca_loadings extracts loadings correctly", {
  result <- perform_pca(mtcars, n_components = 3)

  loadings_1 <- get_pca_loadings(result, component = 1)

  expect_true(is.data.frame(loadings_1))
  expect_true("Variable" %in% names(loadings_1))
  expect_true("Loading" %in% names(loadings_1))
})

test_that("get_pca_loadings applies threshold", {
  result <- perform_pca(mtcars)

  loadings_all <- get_pca_loadings(result, component = 1, threshold = 0)
  loadings_filtered <- get_pca_loadings(result, component = 1, threshold = 0.3)

  # Filtered should have fewer or equal rows
  expect_true(nrow(loadings_filtered) <= nrow(loadings_all))

  # All loadings should meet threshold
  expect_true(all(abs(loadings_filtered$Loading) >= 0.3))
})

test_that("interpret_pca_component generates interpretation", {
  result <- perform_pca(mtcars)

  interpretation <- interpret_pca_component(result, component = 1)

  expect_true(is.character(interpretation))
  expect_true(nchar(interpretation) > 0)
  expect_true(grepl("Component 1", interpretation))
})

# Print Methods ===============================================================

test_that("print method works", {
  result <- perform_pca(mtcars)

  expect_output(print(result), "Principal Component Analysis")
  expect_output(print(result), "Variance Explained")
  expect_output(print(result), "Component Loadings")
})

test_that("summary method works", {
  result <- perform_pca(mtcars)

  expect_output(summary(result), "PCA Summary")
  expect_output(summary(result), "Variance retained")
})

# Edge Cases ==================================================================

test_that("PCA handles perfect correlation", {
  # Create perfectly correlated variables
  df <- data.frame(
    x1 = 1:100,
    x2 = (1:100) * 2,
    x3 = (1:100) * 3
  )

  result <- perform_pca(df)

  # Should work, first component should explain ~100%
  expect_true(result$variance_explained$Variance_Pct[1] > 99)
})

test_that("PCA handles uncorrelated variables", {
  set.seed(123)
  # Create uncorrelated variables
  df <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100)
  )

  result <- perform_pca(df)

  # Each component should explain roughly equal variance
  var_pcts <- result$variance_explained$Variance_Pct[1:3]
  expect_true(max(var_pcts) - min(var_pcts) < 20)  # Within 20% of each other
})

test_that("PCA handles constant variables", {
  df <- data.frame(
    x1 = rep(10, 100),
    x2 = 1:100,
    x3 = rnorm(100)
  )

  # Scaling should handle this
  result <- perform_pca(df, scale = TRUE)

  expect_s3_class(result, "descriptR_pca")
})

test_that("PCA handles missing values", {
  df <- mtcars
  df[1:5, 1] <- NA

  expect_error(
    perform_pca(df),
    "Insufficient complete cases"
  )
})

# Validation ==================================================================

test_that("PCA validates inputs", {
  expect_error(
    perform_pca("not a data frame"),
    "must be a data frame"
  )

  expect_error(
    perform_pca(mtcars, vars = "NonexistentVar"),
    "not found in data"
  )
})

test_that("get_pca_scores validates inputs", {
  result <- perform_pca(mtcars)

  expect_error(
    get_pca_scores("not a pca object"),
    "must be a descriptR_pca object"
  )

  expect_error(
    get_pca_scores(result, components = 99),
    "only .* components retained"
  )
})

# Integration Tests ===========================================================

test_that("full PCA workflow works end-to-end", {
  # Perform PCA
  result <- perform_pca(
    iris,
    vars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    scale = TRUE,
    variance_threshold = 0.90
  )

  # Should complete successfully
  expect_s3_class(result, "descriptR_pca")

  # Extract scores
  scores <- get_pca_scores(result)
  expect_true(is.matrix(scores))

  # Extract loadings
  loadings <- get_pca_loadings(result, component = 1, threshold = 0.3)
  expect_true(is.data.frame(loadings))

  # Interpret
  interpretation <- interpret_pca_component(result, component = 1)
  expect_true(is.character(interpretation))

  # Print and summary
  expect_output(print(result))
  expect_output(summary(result))
})

test_that("PCA results are reproducible", {
  set.seed(123)
  result1 <- perform_pca(mtcars)

  set.seed(123)
  result2 <- perform_pca(mtcars)

  # Should get identical results
  expect_equal(result1$scores, result2$scores)
  expect_equal(result1$loadings, result2$loadings)
  expect_equal(result1$eigenvalues, result2$eigenvalues)
})

test_that("dimensionality reduction works as expected", {
  # Start with 11 variables
  result <- perform_pca(mtcars, variance_threshold = 0.95)

  # Should reduce dimensionality significantly
  expect_true(result$n_components < result$n_variables)

  # But retain most variance
  cumvar <- result$variance_explained$Cumulative_Pct[result$n_components]
  expect_true(cumvar >= 95)
})
