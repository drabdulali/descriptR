#' Perform T-Tests
#'
#' @description
#' Performs various types of t-tests including one-sample, two-sample (independent),
#' and paired t-tests. Automatically computes effect sizes (Cohen's d) and provides
#' interpretation.
#'
#' @param x Numeric vector for one-sample or first group for two-sample
#' @param y Numeric vector for second group (optional for one-sample)
#' @param mu Null hypothesis value for one-sample test (default 0)
#' @param paired Logical, perform paired t-test? (default FALSE)
#' @param var.equal Logical, assume equal variances? (default FALSE, uses Welch's t-test)
#' @param alternative Direction of alternative hypothesis: "two.sided", "less", or "greater"
#' @param conf.level Confidence level (default 0.95)
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return An object of class "descriptR_ttest" containing:
#' \itemize{
#'   \item \code{statistic}: t-statistic value
#'   \item \code{p_value}: P-value
#'   \item \code{df}: Degrees of freedom
#'   \item \code{conf_int}: Confidence interval for mean difference
#'   \item \code{estimate}: Estimated mean(s)
#'   \item \code{null_value}: Null hypothesis value
#'   \item \code{alternative}: Alternative hypothesis
#'   \item \code{method}: Description of test performed
#'   \item \code{cohen_d}: Cohen's d effect size
#'   \item \code{interpretation}: Plain-language interpretation
#'   \item \code{descriptives}: Descriptive statistics
#' }
#'
#' @details
#' ## Test Types
#'
#' **One-Sample T-Test** (y = NULL):
#' - Tests if mean of x differs from mu
#' - H0: μ = mu vs H1: μ ≠ mu (two.sided)
#'
#' **Independent Two-Sample T-Test** (y provided, paired = FALSE):
#' - Tests if means of two independent groups differ
#' - Default: Welch's t-test (unequal variances)
#' - var.equal = TRUE: Student's t-test (pooled variance)
#'
#' **Paired T-Test** (y provided, paired = TRUE):
#' - Tests if mean difference between paired observations differs from mu
#' - Requires x and y to have same length
#'
#' ## Effect Size (Cohen's d)
#'
#' Computed automatically:
#' - One-sample: d = (mean - mu) / sd
#' - Two-sample: d = (mean1 - mean2) / pooled_sd
#' - Paired: d = mean_diff / sd_diff
#'
#' Interpretation:
#' - |d| < 0.2: negligible
#' - |d| < 0.5: small
#' - |d| < 0.8: medium
#' - |d| < 1.3: large
#' - |d| ≥ 1.3: very large
#'
#' @examples
#' # One-sample t-test
#' perform_t_test(rnorm(100, mean = 0.5), mu = 0)
#'
#' # Two-sample t-test (independent)
#' x <- rnorm(50, mean = 10, sd = 2)
#' y <- rnorm(50, mean = 12, sd = 2)
#' perform_t_test(x, y)
#'
#' # Paired t-test
#' before <- rnorm(30, mean = 100, sd = 10)
#' after <- before + rnorm(30, mean = 5, sd = 5)
#' perform_t_test(before, after, paired = TRUE)
#'
#' # One-sided test
#' perform_t_test(x, y, alternative = "less")
#'
#' @export
perform_t_test <- function(x,
                           y = NULL,
                           mu = 0,
                           paired = FALSE,
                           var.equal = FALSE,
                           alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95,
                           na.rm = TRUE) {

  # Validate inputs
  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  alternative <- match.arg(alternative)
  validate_conf_level(conf.level)

  # Handle NA values
  if (na.rm) {
    x <- x[!is.na(x)]
    if (!is.null(y)) {
      y <- y[!is.na(y)]
    }
  }

  # Determine test type
  if (is.null(y)) {
    test_type <- "one_sample"
  } else if (paired) {
    test_type <- "paired"
  } else {
    test_type <- "two_sample"
  }

  # Validate based on test type
  if (test_type == "paired") {
    if (length(x) != length(y)) {
      stop("For paired t-test, x and y must have the same length", call. = FALSE)
    }
  }

  # Perform t-test
  if (test_type == "one_sample") {
    test_result <- stats::t.test(x,
                                 mu = mu,
                                 alternative = alternative,
                                 conf.level = conf.level)

    # Compute Cohen's d for one-sample
    cohen_d <- (mean(x) - mu) / sd(x)

    # Descriptives
    descriptives <- list(
      n = length(x),
      mean = mean(x),
      sd = sd(x),
      se = sd(x) / sqrt(length(x)),
      mu = mu
    )

  } else if (test_type == "paired") {
    test_result <- stats::t.test(x, y,
                                 paired = TRUE,
                                 mu = mu,
                                 alternative = alternative,
                                 conf.level = conf.level)

    # Compute Cohen's d for paired
    diff <- x - y
    cohen_d <- mean(diff) / sd(diff)

    # Descriptives
    descriptives <- list(
      n = length(x),
      mean_x = mean(x),
      mean_y = mean(y),
      mean_diff = mean(diff),
      sd_x = sd(x),
      sd_y = sd(y),
      sd_diff = sd(diff),
      correlation = cor(x, y)
    )

  } else {  # two_sample
    test_result <- stats::t.test(x, y,
                                 var.equal = var.equal,
                                 alternative = alternative,
                                 conf.level = conf.level)

    # Compute Cohen's d for two-sample
    if (var.equal) {
      # Pooled SD
      pooled_sd <- sqrt(((length(x) - 1) * var(x) + (length(y) - 1) * var(y)) /
                       (length(x) + length(y) - 2))
    } else {
      # Average SD for Welch's test
      pooled_sd <- sqrt((var(x) + var(y)) / 2)
    }
    cohen_d <- (mean(x) - mean(y)) / pooled_sd

    # Descriptives
    descriptives <- list(
      n_x = length(x),
      n_y = length(y),
      mean_x = mean(x),
      mean_y = mean(y),
      mean_diff = mean(x) - mean(y),
      sd_x = sd(x),
      sd_y = sd(y),
      se_x = sd(x) / sqrt(length(x)),
      se_y = sd(y) / sqrt(length(y))
    )
  }

  # Create interpretation
  interpretation <- create_ttest_interpretation(
    p_value = test_result$p.value,
    cohen_d = cohen_d,
    test_type = test_type,
    alternative = alternative,
    conf.level = conf.level
  )

  # Create result object
  result <- list(
    statistic = as.numeric(test_result$statistic),
    p_value = test_result$p.value,
    df = as.numeric(test_result$parameter),
    conf_int = as.numeric(test_result$conf.int),
    estimate = test_result$estimate,
    null_value = test_result$null.value,
    alternative = alternative,
    method = test_result$method,
    cohen_d = cohen_d,
    interpretation = interpretation,
    descriptives = descriptives,
    test_type = test_type
  )

  class(result) <- "descriptR_ttest"
  return(result)
}


#' Create T-Test Interpretation
#'
#' @param p_value P-value from test
#' @param cohen_d Cohen's d effect size
#' @param test_type Type of test
#' @param alternative Alternative hypothesis
#' @param conf.level Confidence level
#'
#' @return Character string with interpretation
#'
#' @keywords internal
#' @noRd
create_ttest_interpretation <- function(p_value, cohen_d, test_type,
                                       alternative, conf.level) {

  # Effect size interpretation
  abs_d <- abs(cohen_d)
  if (abs_d < 0.2) {
    effect_interp <- "negligible"
  } else if (abs_d < 0.5) {
    effect_interp <- "small"
  } else if (abs_d < 0.8) {
    effect_interp <- "medium"
  } else if (abs_d < 1.3) {
    effect_interp <- "large"
  } else {
    effect_interp <- "very large"
  }

  # Statistical significance
  alpha <- 1 - conf.level
  if (p_value < 0.001) {
    sig_interp <- "highly significant (p < 0.001)"
  } else if (p_value < 0.01) {
    sig_interp <- sprintf("very significant (p = %.3f)", p_value)
  } else if (p_value < alpha) {
    sig_interp <- sprintf("significant (p = %.3f)", p_value)
  } else if (p_value < 0.10) {
    sig_interp <- sprintf("marginally significant (p = %.3f)", p_value)
  } else {
    sig_interp <- sprintf("not significant (p = %.3f)", p_value)
  }

  # Direction
  if (cohen_d > 0) {
    direction <- "positive"
  } else if (cohen_d < 0) {
    direction <- "negative"
  } else {
    direction <- "no"
  }

  # Combine
  sprintf("The test is %s with a %s effect size (Cohen's d = %.2f, %s direction)",
         sig_interp, effect_interp, abs(cohen_d), direction)
}


#' Print Method for T-Test Results
#'
#' @param x An object of class "descriptR_ttest"
#' @param digits Number of digits to print (default 3)
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR_ttest <- function(x, digits = 3, ...) {

  cat("\n")
  cat(x$method, "\n")
  cat(rep("=", 70), "\n", sep = "")

  # Test results
  cat(sprintf("\nt = %.3f, df = %.1f, p-value = %s\n",
             x$statistic,
             x$df,
             if (x$p_value < 0.001) "< 0.001" else sprintf("%.4f", x$p_value)))

  # Alternative hypothesis
  alt_text <- switch(
    x$alternative,
    "two.sided" = "two-sided",
    "less" = "less than",
    "greater" = "greater than"
  )
  cat(sprintf("Alternative hypothesis: %s\n", alt_text))

  # Confidence interval
  cat(sprintf("%d%% confidence interval: [%.3f, %.3f]\n",
             round((1 - (1 - attr(x$conf_int, "conf.level"))) * 100),
             x$conf_int[1], x$conf_int[2]))

  # Estimates
  cat("\nEstimates:\n")
  if (x$test_type == "one_sample") {
    cat(sprintf("  Sample mean: %.3f\n", x$estimate))
    cat(sprintf("  Null value: %.3f\n", x$null_value))
  } else if (x$test_type == "paired") {
    cat(sprintf("  Mean of differences: %.3f\n", x$estimate))
  } else {
    cat(sprintf("  Mean of x: %.3f\n", x$estimate[1]))
    cat(sprintf("  Mean of y: %.3f\n", x$estimate[2]))
    cat(sprintf("  Difference: %.3f\n", x$estimate[1] - x$estimate[2]))
  }

  # Effect size
  cat(sprintf("\nEffect Size (Cohen's d): %.3f", x$cohen_d))
  cat(sprintf(" (%s)\n", interpret_effect_size(x$cohen_d, "cohen_d")))

  # Interpretation
  cat("\nInterpretation:\n")
  cat(strwrap(x$interpretation, width = 70, prefix = "  "), sep = "\n")

  # Descriptives
  cat("\n")
  cat(rep("-", 70), "\n", sep = "")
  cat("\nDescriptive Statistics:\n")

  if (x$test_type == "one_sample") {
    cat(sprintf("  n = %d\n", x$descriptives$n))
    cat(sprintf("  Mean = %.3f\n", x$descriptives$mean))
    cat(sprintf("  SD = %.3f\n", x$descriptives$sd))
    cat(sprintf("  SE = %.3f\n", x$descriptives$se))

  } else if (x$test_type == "paired") {
    cat(sprintf("  n = %d pairs\n", x$descriptives$n))
    cat(sprintf("  Mean (x) = %.3f (SD = %.3f)\n",
               x$descriptives$mean_x, x$descriptives$sd_x))
    cat(sprintf("  Mean (y) = %.3f (SD = %.3f)\n",
               x$descriptives$mean_y, x$descriptives$sd_y))
    cat(sprintf("  Mean difference = %.3f (SD = %.3f)\n",
               x$descriptives$mean_diff, x$descriptives$sd_diff))
    cat(sprintf("  Correlation: r = %.3f\n", x$descriptives$correlation))

  } else {
    cat(sprintf("  Group x: n = %d, Mean = %.3f, SD = %.3f, SE = %.3f\n",
               x$descriptives$n_x, x$descriptives$mean_x,
               x$descriptives$sd_x, x$descriptives$se_x))
    cat(sprintf("  Group y: n = %d, Mean = %.3f, SD = %.3f, SE = %.3f\n",
               x$descriptives$n_y, x$descriptives$mean_y,
               x$descriptives$sd_y, x$descriptives$se_y))
  }

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  invisible(x)
}


#' Compare Means Across Multiple Groups
#'
#' @description
#' Convenience wrapper for comparing means across groups using appropriate test.
#' Automatically selects between t-test (2 groups) and ANOVA (3+ groups).
#'
#' @param data Data frame
#' @param outcome Name of outcome variable (numeric)
#' @param group Name of grouping variable
#' @param ... Additional arguments passed to underlying test function
#'
#' @return Test result object
#'
#' @examples
#' # Two groups - performs t-test
#' compare_means(iris, "Sepal.Length", "Species")
#'
#' # Will automatically use appropriate test based on number of groups
#'
#' @export
compare_means <- function(data, outcome, group, ...) {

  validate_data_frame(data)
  validate_variables(data, c(outcome, group))

  # Extract variables
  y <- data[[outcome]]
  grp <- data[[group]]

  # Check numeric
  if (!is.numeric(y)) {
    stop(sprintf("`%s` must be numeric", outcome), call. = FALSE)
  }

  # Get unique groups
  groups <- unique(grp[!is.na(grp)])
  n_groups <- length(groups)

  if (n_groups < 2) {
    stop("Need at least 2 groups for comparison", call. = FALSE)
  }

  if (n_groups == 2) {
    # Two groups: use t-test
    x1 <- y[grp == groups[1]]
    x2 <- y[grp == groups[2]]
    result <- perform_t_test(x1, x2, ...)

  } else {
    # Three or more groups: use ANOVA
    result <- perform_anova(data, outcome, group, ...)
  }

  return(result)
}
