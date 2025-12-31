# Test suite for inferential statistical tests

# T-Tests ====================================================================

test_that("perform_t_test works for one-sample test", {
  x <- rnorm(50, mean = 0.5, sd = 1)
  result <- perform_t_test(x, mu = 0)

  expect_s3_class(result, "descriptR_ttest")
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("cohen_d" %in% names(result))
  expect_equal(result$test_type, "one_sample")
})

test_that("perform_t_test works for two-sample test", {
  x <- rnorm(30, mean = 10, sd = 2)
  y <- rnorm(30, mean = 12, sd = 2)
  result <- perform_t_test(x, y)

  expect_s3_class(result, "descriptR_ttest")
  expect_equal(result$test_type, "two_sample")
  expect_true(!is.na(result$cohen_d))
})

test_that("perform_t_test works for paired test", {
  before <- rnorm(25, mean = 100, sd = 10)
  after <- before + rnorm(25, mean = 5, sd = 5)
  result <- perform_t_test(before, after, paired = TRUE)

  expect_s3_class(result, "descriptR_ttest")
  expect_equal(result$test_type, "paired")
  expect_true("correlation" %in% names(result$descriptives))
})

test_that("perform_t_test handles NA values", {
  x <- c(rnorm(20), NA, NA)
  y <- c(rnorm(20), NA)

  expect_no_error(perform_t_test(x, y, na.rm = TRUE))
})

test_that("perform_t_test validates inputs", {
  expect_error(perform_t_test("not numeric"), "must be numeric")
  expect_error(perform_t_test(1:5, 1:3, paired = TRUE),
              "same length")
})

# ANOVA ======================================================================

test_that("perform_anova works with basic input", {
  result <- perform_anova(iris, "Sepal.Length", "Species")

  expect_s3_class(result, "descriptR_anova")
  expect_true("f_statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("eta_squared" %in% names(result))
})

test_that("perform_anova computes effect sizes", {
  result <- perform_anova(iris, "Sepal.Length", "Species",
                         check_assumptions = FALSE)

  expect_true(!is.na(result$eta_squared))
  expect_true(!is.na(result$omega_squared))
  expect_true(result$eta_squared >= 0 && result$eta_squared <= 1)
})

test_that("perform_anova performs post-hoc tests", {
  result <- perform_anova(iris, "Sepal.Length", "Species",
                         post_hoc = TRUE,
                         post_hoc_method = "tukey")

  if (result$p_value < 0.05) {
    expect_true(!is.null(result$post_hoc))
    expect_true(nrow(result$post_hoc) > 0)
  }
})

test_that("perform_anova checks assumptions", {
  result <- perform_anova(iris, "Sepal.Length", "Species",
                         check_assumptions = TRUE)

  expect_true(!is.null(result$assumptions))
  expect_true("normality" %in% names(result$assumptions))
  expect_true("homogeneity" %in% names(result$assumptions))
})

# Chi-Square =================================================================

test_that("perform_chi_square works for test of independence", {
  result <- perform_chi_square(mtcars$vs, mtcars$am)

  expect_s3_class(result, "descriptR_chisq")
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("cramers_v" %in% names(result))
})

test_that("perform_chi_square works with table input", {
  tab <- table(mtcars$vs, mtcars$am)
  result <- perform_chi_square(tab)

  expect_s3_class(result, "descriptR_chisq")
})

test_that("perform_chi_square computes effect sizes", {
  result <- perform_chi_square(mtcars$vs, mtcars$am)

  expect_true(!is.na(result$cramers_v))
  expect_true(result$cramers_v >= 0 && result$cramers_v <= 1)
})

test_that("perform_chi_square computes phi for 2x2 tables", {
  result <- perform_chi_square(mtcars$vs, mtcars$am)

  # vs and am are both binary
  expect_true(!is.na(result$phi))
})

test_that("perform_chi_square goodness of fit works", {
  x <- sample(1:4, 100, replace = TRUE)
  result <- perform_chi_square(x, p = rep(0.25, 4))

  expect_s3_class(result, "descriptR_chisq")
  expect_equal(result$test_type, "goodness_of_fit")
})

test_that("perform_fisher_test works for 2x2 tables", {
  tab <- matrix(c(5, 2, 3, 8), nrow = 2)
  result <- perform_fisher_test(tab)

  expect_s3_class(result, "descriptR_fisher")
  expect_true("odds_ratio" %in% names(result))
  expect_true("p_value" %in% names(result))
})

# Correlation ================================================================

test_that("perform_correlation_test works for Pearson", {
  result <- perform_correlation_test(iris$Sepal.Length,
                                     iris$Sepal.Width,
                                     method = "pearson")

  expect_s3_class(result, "descriptR_cor")
  expect_true("estimate" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(!any(is.na(result$conf_int)))  # CI for Pearson
})

test_that("perform_correlation_test works for Spearman", {
  result <- perform_correlation_test(iris$Sepal.Length,
                                     iris$Sepal.Width,
                                     method = "spearman")

  expect_s3_class(result, "descriptR_cor")
  expect_true(all(is.na(result$conf_int)))  # No CI for Spearman
})

test_that("perform_correlation_test works for Kendall", {
  result <- perform_correlation_test(iris$Sepal.Length,
                                     iris$Sepal.Width,
                                     method = "kendall")

  expect_s3_class(result, "descriptR_cor")
})

test_that("perform_correlation_test creates correlation matrix", {
  result <- perform_correlation_test(
    data = iris,
    vars = c("Sepal.Length", "Sepal.Width", "Petal.Length")
  )

  expect_s3_class(result, "descriptR_cor_matrix")
  expect_true("cor_matrix" %in% names(result))
  expect_true("p_matrix" %in% names(result))
  expect_equal(dim(result$cor_matrix), c(3, 3))
})

test_that("correlation matrix identifies significant correlations", {
  result <- perform_correlation_test(
    data = iris,
    vars = c("Sepal.Length", "Petal.Length", "Petal.Width")
  )

  # These variables are known to be highly correlated
  expect_true(result$cor_matrix["Petal.Length", "Petal.Width"] > 0.9)
  expect_true(result$p_matrix["Petal.Length", "Petal.Width"] < 0.001)
})

# Compare Groups =============================================================

test_that("compare_groups auto-selects t-test for 2 groups", {
  df <- data.frame(
    outcome = rnorm(60),
    group = rep(c("A", "B"), each = 30)
  )

  result <- compare_groups(df, "outcome", "group")

  expect_s3_class(result, "descriptR_comparison")
  expect_true(result$test_type %in% c("t.test", "mann.whitney"))
})

test_that("compare_groups auto-selects ANOVA for 3+ groups", {
  result <- compare_groups(iris, "Sepal.Length", "Species")

  expect_s3_class(result, "descriptR_comparison")
  expect_true(result$test_type %in% c("anova", "kruskal"))
})

test_that("compare_groups handles categorical outcomes", {
  result <- compare_groups(mtcars, "vs", "am")

  expect_s3_class(result, "descriptR_comparison")
  expect_equal(result$test_type, "chi.square")
})

test_that("compare_groups manual test specification works", {
  result <- compare_groups(iris, "Sepal.Length", "Species",
                          test_type = "anova",
                          check_assumptions = FALSE)

  expect_equal(result$test_type, "anova")
})

test_that("compare_groups includes descriptive statistics", {
  result <- compare_groups(iris, "Sepal.Length", "Species",
                          check_assumptions = FALSE)

  expect_true(!is.null(result$descriptives))
  expect_true(nrow(result$descriptives) == 3)  # 3 species
})

test_that("compare_groups provides interpretation", {
  result <- compare_groups(iris, "Sepal.Length", "Species",
                          check_assumptions = FALSE)

  expect_true(!is.null(result$interpretation))
  expect_true(nchar(result$interpretation) > 0)
})

# Compare Means ==============================================================

test_that("compare_means works for 2 groups", {
  df <- data.frame(
    outcome = rnorm(40),
    group = rep(c("A", "B"), each = 20)
  )

  result <- compare_means(df, "outcome", "group")

  # Should use t-test
  expect_true(!is.null(result))
})

test_that("compare_means works for 3+ groups", {
  result <- compare_means(iris, "Sepal.Length", "Species")

  # Should use ANOVA
  expect_s3_class(result, "descriptR_anova")
})

test_that("compare_means validates inputs", {
  expect_error(compare_means(iris, "Species", "Sepal.Length"),
              "must be numeric")
})

# Print Methods ==============================================================

test_that("print methods work without error", {
  # T-test
  t_result <- perform_t_test(rnorm(30), rnorm(30))
  expect_output(print(t_result), "t-test")

  # ANOVA
  anova_result <- perform_anova(iris, "Sepal.Length", "Species",
                                post_hoc = FALSE, check_assumptions = FALSE)
  expect_output(print(anova_result), "ANOVA")

  # Chi-square
  chi_result <- perform_chi_square(mtcars$vs, mtcars$am)
  expect_output(print(chi_result), "Chi-square")

  # Correlation
  cor_result <- perform_correlation_test(iris$Sepal.Length,
                                         iris$Sepal.Width)
  expect_output(print(cor_result), "correlation")

  # Comparison
  comp_result <- compare_groups(iris, "Sepal.Length", "Species",
                                check_assumptions = FALSE)
  expect_output(print(comp_result), "Comparison")
})
