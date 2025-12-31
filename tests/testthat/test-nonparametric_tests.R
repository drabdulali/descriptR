# Test suite for non-parametric tests

# Mann-Whitney U Test =========================================================

test_that("perform_mann_whitney works with vector input", {
  x <- rnorm(20, mean = 10)
  y <- rnorm(20, mean = 12)

  result <- perform_mann_whitney(x, y)

  expect_s3_class(result, "descriptR_mann_whitney")
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("effect_size" %in% names(result))
})

test_that("perform_mann_whitney works with formula", {
  result <- perform_mann_whitney(extra ~ group, data = sleep)

  expect_s3_class(result, "descriptR_mann_whitney")
  expect_equal(nrow(result$descriptives), 2)
})

test_that("perform_mann_whitney computes effect size", {
  x <- rnorm(30, mean = 10)
  y <- rnorm(30, mean = 15)

  result <- perform_mann_whitney(x, y)

  # Effect size should be between -1 and 1
  expect_true(result$effect_size >= -1 && result$effect_size <= 1)
})

test_that("perform_mann_whitney handles one-sided tests", {
  x <- rnorm(20, mean = 10)
  y <- rnorm(20, mean = 12)

  result_less <- perform_mann_whitney(x, y, alternative = "less")
  result_greater <- perform_mann_whitney(x, y, alternative = "greater")
  result_two <- perform_mann_whitney(x, y, alternative = "two.sided")

  expect_equal(result_less$alternative, "less")
  expect_equal(result_greater$alternative, "greater")
  expect_equal(result_two$alternative, "two.sided")
})

test_that("perform_mann_whitney handles NA values", {
  x <- c(rnorm(15), NA, NA)
  y <- c(rnorm(15), NA)

  expect_no_error(perform_mann_whitney(x, y))
})

test_that("perform_mann_whitney requires exactly 2 groups for formula", {
  # Create data with 3 groups
  df <- data.frame(
    value = rnorm(60),
    group = rep(c("A", "B", "C"), each = 20)
  )

  expect_error(
    perform_mann_whitney(value ~ group, data = df),
    "exactly 2 groups"
  )
})

# Kruskal-Wallis Test =========================================================

test_that("perform_kruskal_wallis works", {
  result <- perform_kruskal_wallis(Sepal.Length ~ Species, data = iris)

  expect_s3_class(result, "descriptR_kruskal")
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true("effect_size" %in% names(result))
})

test_that("perform_kruskal_wallis computes epsilon-squared", {
  result <- perform_kruskal_wallis(Sepal.Length ~ Species, data = iris)

  # Effect size should be between 0 and 1
  expect_true(result$effect_size >= 0 && result$effect_size <= 1)
})

test_that("perform_kruskal_wallis includes descriptives", {
  result <- perform_kruskal_wallis(Sepal.Length ~ Species, data = iris)

  expect_equal(nrow(result$descriptives), 3)  # 3 species
  expect_true("Median" %in% names(result$descriptives))
  expect_true("IQR" %in% names(result$descriptives))
})

test_that("perform_kruskal_wallis performs post-hoc tests", {
  result <- perform_kruskal_wallis(Sepal.Length ~ Species, data = iris,
                                   post_hoc = TRUE)

  expect_true(!is.null(result$post_hoc))
  expect_true("P_Adjusted" %in% names(result$post_hoc))
})

test_that("perform_kruskal_wallis can skip post-hoc", {
  result <- perform_kruskal_wallis(Sepal.Length ~ Species, data = iris,
                                   post_hoc = FALSE)

  expect_null(result$post_hoc)
})

test_that("perform_kruskal_wallis handles 2 groups", {
  df <- iris[iris$Species != "versicolor", ]

  result <- perform_kruskal_wallis(Sepal.Length ~ Species, data = df)

  expect_s3_class(result, "descriptR_kruskal")
})

test_that("perform_kruskal_wallis validates minimum groups", {
  df <- data.frame(
    y = rnorm(20),
    group = rep("A", 20)
  )

  expect_error(
    perform_kruskal_wallis(y ~ group, data = df),
    "at least 2 groups"
  )
})

# Wilcoxon Signed-Rank Test ===================================================

test_that("perform_wilcoxon_signed works with paired data", {
  before <- rnorm(20, mean = 100)
  after <- before + rnorm(20, mean = 5, sd = 3)

  result <- perform_wilcoxon_signed(before, after)

  expect_s3_class(result, "descriptR_wilcoxon_signed")
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
})

test_that("perform_wilcoxon_signed computes effect size", {
  x <- rnorm(30, mean = 10)
  y <- x + rnorm(30, mean = 2, sd = 1)

  result <- perform_wilcoxon_signed(x, y)

  # Effect size should be between -1 and 1
  expect_true(result$effect_size >= -1 && result$effect_size <= 1)
})

test_that("perform_wilcoxon_signed includes difference statistics", {
  x <- rnorm(20, mean = 100)
  y <- x + 5

  result <- perform_wilcoxon_signed(x, y)

  expect_equal(nrow(result$descriptives), 3)  # time1, time2, difference
  expect_true(any(grepl("Difference", result$descriptives$Measure)))
})

test_that("perform_wilcoxon_signed handles one-sided tests", {
  x <- rnorm(20)
  y <- x + 2

  result_less <- perform_wilcoxon_signed(x, y, alternative = "less")
  result_greater <- perform_wilcoxon_signed(x, y, alternative = "greater")

  expect_equal(result_less$alternative, "less")
  expect_equal(result_greater$alternative, "greater")
})

test_that("perform_wilcoxon_signed requires equal lengths", {
  x <- rnorm(20)
  y <- rnorm(25)

  expect_error(
    perform_wilcoxon_signed(x, y),
    "same length"
  )
})

test_that("perform_wilcoxon_signed handles NA in pairs", {
  x <- c(rnorm(15), NA, rnorm(4))
  y <- c(rnorm(15), rnorm(4), NA)

  result <- perform_wilcoxon_signed(x, y)

  # Should remove incomplete pairs
  expect_true(result$n < 20)
})

# Friedman Test ===============================================================

test_that("perform_friedman works with proper formula", {
  # Create repeated measures data
  set.seed(123)
  df <- data.frame(
    subject = rep(1:10, each = 3),
    condition = rep(c("A", "B", "C"), 10),
    score = rnorm(30)
  )

  result <- perform_friedman(score ~ condition | subject, data = df)

  expect_s3_class(result, "descriptR_friedman")
  expect_true("statistic" %in% names(result))
  expect_true("kendalls_w" %in% names(result))
})

test_that("perform_friedman computes Kendall's W", {
  set.seed(123)
  df <- data.frame(
    subject = rep(1:10, each = 3),
    condition = rep(c("A", "B", "C"), 10),
    score = rnorm(30)
  )

  result <- perform_friedman(score ~ condition | subject, data = df)

  # Kendall's W should be between 0 and 1
  expect_true(result$kendalls_w >= 0 && result$kendalls_w <= 1)
})

test_that("perform_friedman requires blocking variable", {
  df <- data.frame(
    condition = rep(c("A", "B", "C"), 10),
    score = rnorm(30)
  )

  expect_error(
    perform_friedman(score ~ condition, data = df),
    "blocking variable"
  )
})

test_that("perform_friedman includes descriptives", {
  set.seed(123)
  df <- data.frame(
    subject = rep(1:10, each = 3),
    condition = rep(c("A", "B", "C"), 10),
    score = rnorm(30)
  )

  result <- perform_friedman(score ~ condition | subject, data = df)

  expect_equal(nrow(result$descriptives), 3)  # 3 conditions
  expect_true("Median" %in% names(result$descriptives))
})

# Print Methods ===============================================================

test_that("print methods work without error", {
  # Mann-Whitney
  mw_result <- perform_mann_whitney(extra ~ group, data = sleep)
  expect_output(print(mw_result), "Mann-Whitney")

  # Kruskal-Wallis
  kw_result <- perform_kruskal_wallis(Sepal.Length ~ Species, data = iris)
  expect_output(print(kw_result), "Kruskal-Wallis")

  # Wilcoxon Signed-Rank
  ws_result <- perform_wilcoxon_signed(sleep$extra[sleep$group == 1],
                                       sleep$extra[sleep$group == 2])
  expect_output(print(ws_result), "Wilcoxon")

  # Friedman
  df <- data.frame(
    subject = rep(1:10, each = 3),
    condition = rep(c("A", "B", "C"), 10),
    score = rnorm(30)
  )
  fr_result <- perform_friedman(score ~ condition | subject, data = df)
  expect_output(print(fr_result), "Friedman")
})

# Effect Sizes ================================================================

test_that("rank-biserial correlation is correctly computed", {
  # Groups with no overlap should have large effect
  x <- rep(1, 10)
  y <- rep(10, 10)

  result <- perform_mann_whitney(x, y)

  # Should be close to -1 or 1
  expect_true(abs(result$effect_size) > 0.9)
})

test_that("epsilon-squared increases with group differences", {
  # Small difference
  df_small <- data.frame(
    y = c(rnorm(20, 10), rnorm(20, 10.5), rnorm(20, 11)),
    group = rep(c("A", "B", "C"), each = 20)
  )

  # Large difference
  df_large <- data.frame(
    y = c(rnorm(20, 10), rnorm(20, 20), rnorm(20, 30)),
    group = rep(c("A", "B", "C"), each = 20)
  )

  result_small <- perform_kruskal_wallis(y ~ group, data = df_small,
                                         post_hoc = FALSE)
  result_large <- perform_kruskal_wallis(y ~ group, data = df_large,
                                         post_hoc = FALSE)

  # Large effect should have larger epsilon-squared
  expect_true(result_large$effect_size > result_small$effect_size)
})

# Post-hoc Tests ==============================================================

test_that("post-hoc tests perform all pairwise comparisons", {
  result <- perform_kruskal_wallis(Sepal.Length ~ Species, data = iris,
                                   post_hoc = TRUE)

  # Should have 3 comparisons for 3 groups: C(3,2) = 3
  expect_equal(nrow(result$post_hoc), 3)
})

test_that("post-hoc p-values are adjusted", {
  result <- perform_kruskal_wallis(Sepal.Length ~ Species, data = iris,
                                   post_hoc = TRUE,
                                   p_adjust = "bonferroni")

  # Adjusted p-values should be >= raw p-values
  expect_true(all(result$post_hoc$P_Adjusted >= result$post_hoc$P_Raw))
})

test_that("post-hoc tests mark significance", {
  result <- perform_kruskal_wallis(Sepal.Length ~ Species, data = iris,
                                   post_hoc = TRUE)

  expect_true("Significant" %in% names(result$post_hoc))
  expect_true(is.logical(result$post_hoc$Significant))
})

# Edge Cases ==================================================================

test_that("tests handle tied ranks", {
  # Data with many ties
  x <- c(rep(1, 10), rep(2, 10))
  y <- c(rep(3, 10), rep(4, 10))

  expect_no_error(perform_mann_whitney(x, y))
})

test_that("tests handle constant data", {
  x <- rep(10, 20)
  y <- rep(10, 20)

  result <- perform_mann_whitney(x, y)

  # Should have very large p-value (no difference)
  expect_true(result$p_value > 0.5)
})

test_that("tests handle minimal data", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)

  expect_no_error(perform_mann_whitney(x, y))
})

test_that("wilcoxon handles no difference", {
  x <- rnorm(20)
  y <- x  # Identical

  result <- perform_wilcoxon_signed(x, y)

  # P-value should be very large
  expect_true(result$p_value > 0.5)
})

# Integration Tests ===========================================================

test_that("mann-whitney gives similar results to t-test for normal data", {
  set.seed(123)
  x <- rnorm(30, mean = 10, sd = 2)
  y <- rnorm(30, mean = 12, sd = 2)

  mw_result <- perform_mann_whitney(x, y)
  t_result <- perform_t_test(x, y)

  # Both should agree on significance direction
  both_sig <- mw_result$p_value < 0.05 && t_result$p_value < 0.05
  both_nonsig <- mw_result$p_value >= 0.05 && t_result$p_value >= 0.05

  expect_true(both_sig || both_nonsig)
})

test_that("kruskal gives similar results to anova for normal data", {
  set.seed(123)

  kw_result <- perform_kruskal_wallis(Sepal.Length ~ Species, data = iris,
                                      post_hoc = FALSE)
  aov_result <- perform_anova(iris, outcome = "Sepal.Length",
                              group = "Species", post_hoc = FALSE,
                              check_assumptions = FALSE)

  # Both should agree on significance
  both_sig <- kw_result$p_value < 0.05 && aov_result$p_value < 0.05

  expect_true(both_sig)
})

test_that("results are internally consistent", {
  result <- perform_kruskal_wallis(Sepal.Length ~ Species, data = iris)

  # Number of groups in descriptives should match n_groups
  expect_equal(nrow(result$descriptives), result$n_groups)

  # Post-hoc comparisons should be C(n_groups, 2)
  if (!is.null(result$post_hoc)) {
    expected_comparisons <- choose(result$n_groups, 2)
    expect_equal(nrow(result$post_hoc), expected_comparisons)
  }
})
