#' Comprehensive Normality Assessment
#'
#' @description
#' Performs comprehensive assessment of normality using multiple statistical tests
#' and provides transformation recommendations when needed.
#'
#' @param data Data frame or numeric vector
#' @param vars Character vector of variable names to analyze (NULL for all numeric)
#' @param tests Character vector of tests to perform:
#'   \itemize{
#'     \item "shapiro": Shapiro-Wilk test (n ≤ 5000, most powerful)
#'     \item "ks": Kolmogorov-Smirnov test
#'     \item "anderson": Anderson-Darling test (more sensitive to tails)
#'     \item "jarque": Jarque-Bera test (based on skewness/kurtosis)
#'     \item "dagostino": D'Agostino's K² test
#'     \item "all": All available tests
#'   }
#' @param alpha Significance level (default 0.05)
#' @param include_plots Logical, generate diagnostic plots? (default FALSE)
#' @param suggest_transformations Logical, suggest transformations? (default TRUE)
#'
#' @return An object of class "descriptR_normality" containing:
#' \itemize{
#'   \item \code{test_results}: Results from all normality tests
#'   \item \code{summary}: Summary by variable
#'   \item \code{shape_statistics}: Skewness and kurtosis
#'   \item \code{transformations}: Suggested transformations (if applicable)
#'   \item \code{interpretation}: Overall interpretation
#'   \item \code{plots}: Diagnostic plots (if requested)
#' }
#'
#' @details
#' ## Normality Tests
#'
#' **Shapiro-Wilk Test** (recommended for n ≤ 5000):
#' - Most powerful test for normality
#' - Sensitive to both tails and center
#' - H0: Data are normally distributed
#'
#' **Kolmogorov-Smirnov Test**:
#' - Tests goodness of fit to normal distribution
#' - Less powerful than Shapiro-Wilk
#' - Works for larger samples
#'
#' **Anderson-Darling Test**:
#' - More weight on tails than K-S test
#' - Good for detecting tail departures
#' - Recommended for finance/risk applications
#'
#' **Jarque-Bera Test**:
#' - Based on sample skewness and kurtosis
#' - Asymptotic (requires large n)
#' - Common in econometrics
#'
#' **D'Agostino's K² Test**:
#' - Combines skewness and kurtosis tests
#' - Good power for large samples
#' - Provides separate components
#'
#' ## Interpretation Guidelines
#'
#' **Skewness**:
#' - |skew| < 0.5: Approximately symmetric
#' - 0.5 ≤ |skew| < 1: Moderately skewed
#' - |skew| ≥ 1: Highly skewed
#'
#' **Kurtosis** (excess kurtosis):
#' - |kurt| < 0.5: Approximately mesokurtic
#' - 0.5 ≤ |kurt| < 1: Moderate departure
#' - |kurt| ≥ 1: Substantial departure
#'
#' ## Transformation Recommendations
#'
#' Based on distribution characteristics:
#' - Right skew: log, sqrt, inverse
#' - Left skew: square, exponential
#' - Heavy tails: Box-Cox transformation
#'
#' @examples
#' # Single variable
#' assess_normality(mtcars$mpg)
#'
#' # Multiple variables with all tests
#' assess_normality(iris, vars = c("Sepal.Length", "Petal.Width"),
#'                  tests = "all")
#'
#' # Specific tests only
#' assess_normality(mtcars, tests = c("shapiro", "anderson"))
#'
#' @export
assess_normality <- function(data,
                             vars = NULL,
                             tests = c("shapiro", "ks", "anderson"),
                             alpha = 0.05,
                             include_plots = FALSE,
                             suggest_transformations = TRUE) {

  # Handle vector input
  if (is.numeric(data) && is.null(vars)) {
    data <- data.frame(value = data)
    vars <- "value"
    vector_input <- TRUE
  } else {
    vector_input <- FALSE
  }

  # Validate
  validate_data_frame(data)

  # Select numeric variables
  if (is.null(vars)) {
    numeric_cols <- sapply(data, is.numeric)
    vars <- names(data)[numeric_cols]
  } else {
    validate_variables(data, vars)
  }

  # Handle "all" tests
  if ("all" %in% tests) {
    tests <- c("shapiro", "ks", "anderson", "jarque", "dagostino")
  }

  # Validate tests
  valid_tests <- c("shapiro", "ks", "anderson", "jarque", "dagostino")
  invalid <- setdiff(tests, valid_tests)
  if (length(invalid) > 0) {
    stop(sprintf("Invalid tests: %s. Choose from: %s",
                paste(invalid, collapse = ", "),
                paste(valid_tests, collapse = ", ")),
         call. = FALSE)
  }

  # Initialize results
  test_results <- list()
  shape_statistics <- list()
  transformations <- list()

  # Assess each variable
  for (var in vars) {
    x <- data[[var]]

    # Remove NA
    x_clean <- x[!is.na(x)]

    if (length(x_clean) < 3) {
      next  # Skip variables with too few observations
    }

    var_tests <- list()

    # Shapiro-Wilk test
    if ("shapiro" %in% tests) {
      var_tests$shapiro <- test_shapiro_wilk(x_clean, alpha)
    }

    # Kolmogorov-Smirnov test
    if ("ks" %in% tests) {
      var_tests$ks <- test_kolmogorov_smirnov(x_clean, alpha)
    }

    # Anderson-Darling test
    if ("anderson" %in% tests) {
      var_tests$anderson <- test_anderson_darling(x_clean, alpha)
    }

    # Jarque-Bera test
    if ("jarque" %in% tests) {
      var_tests$jarque <- test_jarque_bera(x_clean, alpha)
    }

    # D'Agostino test
    if ("dagostino" %in% tests) {
      var_tests$dagostino <- test_dagostino(x_clean, alpha)
    }

    test_results[[var]] <- var_tests

    # Compute shape statistics
    shape_statistics[[var]] <- compute_shape_statistics(x_clean)

    # Suggest transformations
    if (suggest_transformations) {
      transformations[[var]] <- suggest_transformation(x_clean, var_tests,
                                                       shape_statistics[[var]])
    }
  }

  # Create summary
  summary_df <- create_normality_summary(vars, test_results, shape_statistics,
                                         tests, alpha)

  # Overall interpretation
  interpretation <- generate_normality_interpretation(summary_df,
                                                      shape_statistics,
                                                      transformations)

  # Plots (placeholder for Phase 4)
  plots <- NULL
  if (include_plots) {
    plots <- list(message = "Plotting functionality coming in Phase 4")
  }

  # Create result object
  result <- list(
    test_results = test_results,
    summary = summary_df,
    shape_statistics = shape_statistics,
    transformations = transformations,
    interpretation = interpretation,
    tests_used = tests,
    alpha = alpha,
    plots = plots
  )

  class(result) <- "descriptR_normality"
  return(result)
}


# Normality Tests =============================================================

#' Shapiro-Wilk Test for Normality
#'
#' @param x Numeric vector
#' @param alpha Significance level
#'
#' @return List with test results
#'
#' @keywords internal
#' @noRd
test_shapiro_wilk <- function(x, alpha = 0.05) {
  n <- length(x)

  if (n < 3) {
    return(list(
      test_name = "Shapiro-Wilk",
      performed = FALSE,
      note = "Sample size too small (n < 3)"
    ))
  }

  if (n > 5000) {
    return(list(
      test_name = "Shapiro-Wilk",
      performed = FALSE,
      note = "Sample size too large for Shapiro-Wilk (n > 5000). Use K-S test instead."
    ))
  }

  # Perform test
  sw_result <- shapiro.test(x)

  # Interpretation
  is_normal <- sw_result$p.value >= alpha

  interpretation <- if (is_normal) {
    sprintf("Data are consistent with normality (W = %.4f, p = %.4f)",
           sw_result$statistic, sw_result$p.value)
  } else {
    sprintf("Data deviate from normality (W = %.4f, p = %.4f)",
           sw_result$statistic, sw_result$p.value)
  }

  list(
    test_name = "Shapiro-Wilk",
    statistic = as.numeric(sw_result$statistic),
    p_value = sw_result$p.value,
    is_normal = is_normal,
    interpretation = interpretation,
    performed = TRUE
  )
}


#' Kolmogorov-Smirnov Test for Normality
#'
#' @param x Numeric vector
#' @param alpha Significance level
#'
#' @return List with test results
#'
#' @keywords internal
#' @noRd
test_kolmogorov_smirnov <- function(x, alpha = 0.05) {
  n <- length(x)

  if (n < 3) {
    return(list(
      test_name = "Kolmogorov-Smirnov",
      performed = FALSE,
      note = "Sample size too small"
    ))
  }

  # Standardize data
  x_std <- (x - mean(x)) / sd(x)

  # Perform test against standard normal
  ks_result <- suppressWarnings(
    ks.test(x_std, "pnorm", mean = 0, sd = 1)
  )

  is_normal <- ks_result$p.value >= alpha

  interpretation <- if (is_normal) {
    sprintf("Data are consistent with normality (D = %.4f, p = %.4f)",
           ks_result$statistic, ks_result$p.value)
  } else {
    sprintf("Data deviate from normality (D = %.4f, p = %.4f)",
           ks_result$statistic, ks_result$p.value)
  }

  list(
    test_name = "Kolmogorov-Smirnov",
    statistic = as.numeric(ks_result$statistic),
    p_value = ks_result$p.value,
    is_normal = is_normal,
    interpretation = interpretation,
    performed = TRUE
  )
}


#' Anderson-Darling Test for Normality
#'
#' @param x Numeric vector
#' @param alpha Significance level
#'
#' @return List with test results
#'
#' @keywords internal
#' @noRd
test_anderson_darling <- function(x, alpha = 0.05) {
  n <- length(x)

  if (n < 8) {
    return(list(
      test_name = "Anderson-Darling",
      performed = FALSE,
      note = "Sample size too small (n < 8)"
    ))
  }

  # Standardize
  x_std <- (x - mean(x)) / sd(x)
  x_sorted <- sort(x_std)

  # Compute Anderson-Darling statistic
  n <- length(x_sorted)
  i <- 1:n
  z <- pnorm(x_sorted)

  # Anderson-Darling formula
  A2 <- -n - sum((2*i - 1) * (log(z) + log(1 - rev(z)))) / n

  # Adjust for estimated parameters (mean and SD)
  A2_adj <- A2 * (1 + 0.75/n + 2.25/n^2)

  # Critical values at different significance levels
  # Adjusted for estimated parameters
  critical_values <- c(
    "0.10" = 0.631,
    "0.05" = 0.752,
    "0.025" = 0.873,
    "0.01" = 1.035
  )

  # Determine p-value bracket
  if (A2_adj < critical_values["0.10"]) {
    p_value <- 0.15  # Approximate
    is_normal <- TRUE
  } else if (A2_adj < critical_values["0.05"]) {
    p_value <- 0.075  # Approximate
    is_normal <- alpha > 0.05
  } else if (A2_adj < critical_values["0.025"]) {
    p_value <- 0.0375  # Approximate
    is_normal <- alpha > 0.025
  } else if (A2_adj < critical_values["0.01"]) {
    p_value <- 0.0175  # Approximate
    is_normal <- alpha > 0.01
  } else {
    p_value <- 0.005  # Approximate
    is_normal <- FALSE
  }

  interpretation <- if (is_normal) {
    sprintf("Data are consistent with normality (A² = %.4f, p ≈ %.3f)",
           A2_adj, p_value)
  } else {
    sprintf("Data deviate from normality (A² = %.4f, p ≈ %.3f)",
           A2_adj, p_value)
  }

  list(
    test_name = "Anderson-Darling",
    statistic = A2_adj,
    p_value = p_value,
    is_normal = is_normal,
    interpretation = interpretation,
    performed = TRUE,
    note = "P-value is approximate"
  )
}


#' Jarque-Bera Test for Normality
#'
#' @param x Numeric vector
#' @param alpha Significance level
#'
#' @return List with test results
#'
#' @keywords internal
#' @noRd
test_jarque_bera <- function(x, alpha = 0.05) {
  n <- length(x)

  if (n < 30) {
    return(list(
      test_name = "Jarque-Bera",
      performed = FALSE,
      note = "Sample size too small (recommended n ≥ 30 for asymptotic test)"
    ))
  }

  # Compute skewness and kurtosis
  mean_x <- mean(x)
  sd_x <- sd(x)
  x_std <- (x - mean_x) / sd_x

  skew <- mean(x_std^3)
  kurt <- mean(x_std^4) - 3  # Excess kurtosis

  # Jarque-Bera statistic
  JB <- n * (skew^2 / 6 + kurt^2 / 24)

  # P-value from chi-square distribution with df = 2
  p_value <- pchisq(JB, df = 2, lower.tail = FALSE)

  is_normal <- p_value >= alpha

  interpretation <- if (is_normal) {
    sprintf("Data are consistent with normality (JB = %.4f, p = %.4f)",
           JB, p_value)
  } else {
    sprintf("Data deviate from normality (JB = %.4f, p = %.4f)",
           JB, p_value)
  }

  list(
    test_name = "Jarque-Bera",
    statistic = JB,
    p_value = p_value,
    is_normal = is_normal,
    skewness = skew,
    kurtosis = kurt,
    interpretation = interpretation,
    performed = TRUE
  )
}


#' D'Agostino's K² Test for Normality
#'
#' @param x Numeric vector
#' @param alpha Significance level
#'
#' @return List with test results
#'
#' @keywords internal
#' @noRd
test_dagostino <- function(x, alpha = 0.05) {
  n <- length(x)

  if (n < 20) {
    return(list(
      test_name = "D'Agostino K²",
      performed = FALSE,
      note = "Sample size too small (recommended n ≥ 20)"
    ))
  }

  # Compute skewness and kurtosis
  mean_x <- mean(x)
  sd_x <- sd(x)
  x_std <- (x - mean_x) / sd_x

  # Sample skewness
  b1 <- mean(x_std^3)

  # Sample excess kurtosis
  b2 <- mean(x_std^4) - 3

  # Transform to approximately normal Z-scores
  # Skewness component
  Y_skew <- b1 * sqrt((n + 1) * (n + 3) / (6 * (n - 2)))
  beta2_skew <- (3 * (n^2 + 27*n - 70) * (n + 1) * (n + 3)) /
                ((n - 2) * (n + 5) * (n + 7) * (n + 9))
  W2 <- sqrt(2 * (beta2_skew - 1)) - 1
  delta <- 1 / sqrt(log(sqrt(W2)))
  alpha_skew <- sqrt(2 / (W2 - 1))
  Z_skew <- delta * log(Y_skew / alpha_skew + sqrt((Y_skew / alpha_skew)^2 + 1))

  # Kurtosis component
  E_b2 <- 3 * (n - 1) / (n + 1)
  var_b2 <- 24 * n * (n - 2) * (n - 3) / ((n + 1)^2 * (n + 3) * (n + 5))
  Z_kurt <- (b2 - E_b2) / sqrt(var_b2)

  # Omnibus K² statistic
  K2 <- Z_skew^2 + Z_kurt^2

  # P-value from chi-square with df = 2
  p_value <- pchisq(K2, df = 2, lower.tail = FALSE)

  is_normal <- p_value >= alpha

  interpretation <- if (is_normal) {
    sprintf("Data are consistent with normality (K² = %.4f, p = %.4f)",
           K2, p_value)
  } else {
    sprintf("Data deviate from normality (K² = %.4f, p = %.4f)",
           K2, p_value)
  }

  list(
    test_name = "D'Agostino K²",
    statistic = K2,
    p_value = p_value,
    is_normal = is_normal,
    skewness_z = Z_skew,
    kurtosis_z = Z_kurt,
    interpretation = interpretation,
    performed = TRUE
  )
}


# Helper Functions ============================================================

#' Compute Shape Statistics
#'
#' @param x Numeric vector
#'
#' @return List with skewness and kurtosis
#'
#' @keywords internal
#' @noRd
compute_shape_statistics <- function(x) {
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)

  if (sd_x == 0) {
    return(list(
      skewness = 0,
      kurtosis = 0,
      note = "Zero variance - all values identical"
    ))
  }

  x_std <- (x - mean_x) / sd_x

  # Sample skewness (Fisher-Pearson coefficient)
  skew <- mean(x_std^3)

  # Adjust for bias (optional)
  skew_adj <- skew * sqrt(n * (n - 1)) / (n - 2)

  # Sample excess kurtosis
  kurt <- mean(x_std^4) - 3

  # Adjust for bias
  kurt_adj <- ((n - 1) * ((n + 1) * kurt + 6)) / ((n - 2) * (n - 3))

  # Interpretation
  skew_interp <- if (abs(skew_adj) < 0.5) {
    "approximately symmetric"
  } else if (abs(skew_adj) < 1) {
    paste(ifelse(skew_adj > 0, "moderately right", "moderately left"), "skewed")
  } else {
    paste(ifelse(skew_adj > 0, "highly right", "highly left"), "skewed")
  }

  kurt_interp <- if (abs(kurt_adj) < 0.5) {
    "approximately mesokurtic (normal tails)"
  } else if (kurt_adj > 0) {
    if (kurt_adj < 1) "moderately leptokurtic (heavy tails)"
    else "highly leptokurtic (very heavy tails)"
  } else {
    if (kurt_adj > -1) "moderately platykurtic (light tails)"
    else "highly platykurtic (very light tails)"
  }

  list(
    skewness = skew_adj,
    kurtosis = kurt_adj,
    skewness_interpretation = skew_interp,
    kurtosis_interpretation = kurt_interp
  )
}


#' Suggest Transformation for Non-Normal Data
#'
#' @param x Numeric vector
#' @param tests Test results
#' @param shape Shape statistics
#'
#' @return List with transformation suggestions
#'
#' @keywords internal
#' @noRd
suggest_transformation <- function(x, tests, shape) {

  # Check if data are normal
  n_tests <- length(tests)
  n_normal <- sum(sapply(tests, function(t) {
    if (t$performed) t$is_normal else FALSE
  }))

  if (n_normal / n_tests >= 0.5) {
    return(list(
      needed = FALSE,
      message = "Data appear normally distributed. No transformation needed."
    ))
  }

  # Check for non-positive values
  has_nonpositive <- any(x <= 0)

  suggestions <- character()

  # Based on skewness
  skew <- shape$skewness

  if (skew > 1) {
    # Right skewed
    suggestions <- c(suggestions,
      "• Right-skewed data detected. Consider:",
      "  - Log transformation: log(x) [requires x > 0]",
      "  - Square root: sqrt(x) [requires x ≥ 0]",
      "  - Inverse: 1/x [requires x ≠ 0]",
      "  - Box-Cox transformation (optimal power)")

    if (has_nonpositive) {
      suggestions <- c(suggestions,
        "  ⚠ Warning: Data contain non-positive values. Add constant before log/sqrt.")
    }

  } else if (skew < -1) {
    # Left skewed
    suggestions <- c(suggestions,
      "• Left-skewed data detected. Consider:",
      "  - Square transformation: x²",
      "  - Exponential: exp(x)",
      "  - Reflect and transform: apply right-skew transformations to -x")

  } else {
    # Mild skew or kurtosis issue
    suggestions <- c(suggestions,
      "• Mild departure from normality detected. Consider:",
      "  - Box-Cox transformation (data-driven power selection)",
      "  - Yeo-Johnson transformation (handles negative values)")
  }

  # Based on kurtosis
  kurt <- shape$kurtosis

  if (abs(kurt) > 1) {
    if (kurt > 0) {
      suggestions <- c(suggestions,
        "• Heavy tails detected (leptokurtic):",
        "  - Winsorization to reduce extreme values",
        "  - Robust statistical methods (median, MAD)",
        "  - Consider outlier removal if justified")
    } else {
      suggestions <- c(suggestions,
        "• Light tails detected (platykurtic):",
        "  - May indicate bounded distribution",
        "  - Consider beta or truncated normal distribution")
    }
  }

  list(
    needed = TRUE,
    skewness = skew,
    kurtosis = kurt,
    suggestions = suggestions
  )
}


#' Create Normality Summary
#'
#' @keywords internal
#' @noRd
create_normality_summary <- function(vars, test_results, shape_statistics,
                                     tests, alpha) {

  summary_list <- list()

  for (var in vars) {
    var_tests <- test_results[[var]]
    var_shape <- shape_statistics[[var]]

    # Count tests indicating normality
    n_tests_performed <- sum(sapply(var_tests, function(t) t$performed))
    n_normal <- sum(sapply(var_tests, function(t) {
      if (t$performed) t$is_normal else FALSE
    }))

    # Overall verdict
    verdict <- if (n_normal / n_tests_performed >= 0.5) {
      "Normal"
    } else {
      "Non-normal"
    }

    summary_row <- data.frame(
      Variable = var,
      N_Tests = n_tests_performed,
      N_Normal = n_normal,
      Verdict = verdict,
      Skewness = var_shape$skewness,
      Kurtosis = var_shape$kurtosis,
      stringsAsFactors = FALSE
    )

    # Add individual test results
    for (test_name in tests) {
      if (test_name %in% names(var_tests)) {
        test_result <- var_tests[[test_name]]
        if (test_result$performed) {
          summary_row[[paste0(test_name, "_p")]] <- test_result$p_value
        } else {
          summary_row[[paste0(test_name, "_p")]] <- NA
        }
      }
    }

    summary_list[[var]] <- summary_row
  }

  summary_df <- do.call(rbind, summary_list)
  rownames(summary_df) <- NULL

  return(summary_df)
}


#' Generate Normality Interpretation
#'
#' @keywords internal
#' @noRd
generate_normality_interpretation <- function(summary_df, shape_statistics,
                                              transformations) {

  interpretation <- character()

  # Overall assessment
  n_vars <- nrow(summary_df)
  n_normal <- sum(summary_df$Verdict == "Normal")
  pct_normal <- n_normal / n_vars * 100

  interpretation <- c(interpretation,
    sprintf("Normality Assessment Results (%d variable%s):",
           n_vars, ifelse(n_vars == 1, "", "s")),
    sprintf("  • %d (%.1f%%) appear normally distributed",
           n_normal, pct_normal),
    sprintf("  • %d (%.1f%%) show departures from normality",
           n_vars - n_normal, 100 - pct_normal),
    "")

  # Variables with non-normality
  non_normal_vars <- summary_df$Variable[summary_df$Verdict == "Non-normal"]

  if (length(non_normal_vars) > 0) {
    interpretation <- c(interpretation,
      "Non-normal variables:",
      paste0("  - ", non_normal_vars),
      "")

    # Transformation recommendations
    need_transform <- sapply(transformations, function(t) {
      if (!is.null(t$needed)) t$needed else FALSE
    })

    if (any(need_transform)) {
      interpretation <- c(interpretation,
        "Transformation recommended for improved normality.",
        "See detailed suggestions in the 'transformations' component.")
    }
  }

  # General recommendations
  interpretation <- c(interpretation,
    "",
    "General Recommendations:",
    "  • Normal data: Use parametric tests (t-test, ANOVA, Pearson correlation)",
    "  • Non-normal data: Consider non-parametric alternatives or transformations",
    "  • Large samples (n > 30): Parametric tests robust to moderate non-normality",
    "  • Always visualize: Q-Q plots and histograms provide additional insight")

  return(interpretation)
}


# Print Methods ===============================================================

#' Print Method for Normality Assessment
#'
#' @param x An object of class "descriptR_normality"
#' @param max_vars Maximum variables to display (default 10)
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR_normality <- function(x, max_vars = 10, ...) {

  cat("\n")
  cat("Normality Assessment\n")
  cat(rep("=", 80), "\n", sep = "")

  cat("\nTests used:", paste(x$tests_used, collapse = ", "), "\n")
  cat(sprintf("Significance level: α = %.2f\n", x$alpha))

  # Summary
  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nSummary by Variable:\n\n")

  summary_to_show <- head(x$summary, max_vars)
  print(summary_to_show, row.names = FALSE, digits = 3)

  if (nrow(x$summary) > max_vars) {
    cat(sprintf("\n... and %d more variables\n", nrow(x$summary) - max_vars))
  }

  # Detailed test results for first variable (if available)
  if (length(x$test_results) > 0) {
    first_var <- names(x$test_results)[1]
    cat("\n")
    cat(rep("-", 80), "\n", sep = "")
    cat(sprintf("\nDetailed Results for '%s':\n\n", first_var))

    for (test_result in x$test_results[[first_var]]) {
      if (test_result$performed) {
        cat(sprintf("%s: %s\n", test_result$test_name,
                   test_result$interpretation))
      }
    }

    # Shape statistics
    shape <- x$shape_statistics[[first_var]]
    cat(sprintf("\nShape Statistics:\n"))
    cat(sprintf("  Skewness: %.3f (%s)\n",
               shape$skewness, shape$skewness_interpretation))
    cat(sprintf("  Kurtosis: %.3f (%s)\n",
               shape$kurtosis, shape$kurtosis_interpretation))
  }

  # Transformations
  if (length(x$transformations) > 0) {
    needs_transform <- sapply(x$transformations, function(t) {
      if (!is.null(t$needed)) t$needed else FALSE
    })

    if (any(needs_transform)) {
      cat("\n")
      cat(rep("-", 80), "\n", sep = "")
      cat("\nTransformation Suggestions:\n\n")

      for (var in names(x$transformations)[needs_transform]) {
        trans <- x$transformations[[var]]
        if (trans$needed) {
          cat(sprintf("%s:\n", var))
          cat(paste(trans$suggestions, collapse = "\n"))
          cat("\n\n")
        }
      }
    }
  }

  # Interpretation
  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nInterpretation:\n\n")
  cat(paste(x$interpretation, collapse = "\n"))
  cat("\n")

  cat("\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("\n")

  invisible(x)
}


#' Summary Method for Normality Assessment
#'
#' @param object An object of class "descriptR_normality"
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.descriptR_normality <- function(object, ...) {

  cat("\n")
  cat("Normality Assessment Summary\n")
  cat(rep("=", 70), "\n", sep = "")

  n_vars <- nrow(object$summary)
  n_normal <- sum(object$summary$Verdict == "Normal")

  cat(sprintf("\nVariables assessed: %d\n", n_vars))
  cat(sprintf("Normal: %d (%.1f%%)\n", n_normal, n_normal / n_vars * 100))
  cat(sprintf("Non-normal: %d (%.1f%%)\n",
             n_vars - n_normal, (n_vars - n_normal) / n_vars * 100))

  cat(sprintf("\nTests used: %s\n", paste(object$tests_used, collapse = ", ")))

  # Variables by verdict
  if (n_normal > 0) {
    normal_vars <- object$summary$Variable[object$summary$Verdict == "Normal"]
    cat("\nNormal variables:\n")
    cat(paste("  -", normal_vars, collapse = "\n"))
    cat("\n")
  }

  if (n_normal < n_vars) {
    non_normal_vars <- object$summary$Variable[object$summary$Verdict == "Non-normal"]
    cat("\nNon-normal variables:\n")
    cat(paste("  -", non_normal_vars, collapse = "\n"))
    cat("\n")
  }

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  invisible(object)
}
