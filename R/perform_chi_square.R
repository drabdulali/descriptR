#' Perform Chi-Square Tests
#'
#' @description
#' Performs chi-square tests for categorical data including test of independence
#' and goodness of fit. Computes effect size (Cramér's V) and provides interpretation.
#'
#' @param x First categorical variable (vector or table)
#' @param y Second categorical variable (for test of independence, optional)
#' @param p Expected probabilities for goodness of fit (optional)
#' @param correct Logical, apply Yates' continuity correction for 2x2 tables? (default TRUE)
#' @param simulate.p.value Logical, use Monte Carlo simulation for p-value? (default FALSE)
#' @param B Number of replicates for Monte Carlo (default 2000)
#'
#' @return An object of class "descriptR_chisq" containing:
#' \itemize{
#'   \item \code{statistic}: Chi-square statistic
#'   \item \code{p_value}: P-value
#'   \item \code{df}: Degrees of freedom
#'   \item \code{method}: Description of test
#'   \item \code{observed}: Observed frequencies
#'   \item \code{expected}: Expected frequencies
#'   \item \code{residuals}: Pearson residuals
#'   \item \code{cramers_v}: Cramér's V effect size
#'   \item \code{phi}: Phi coefficient (for 2x2 tables)
#'   \item \code{interpretation}: Plain-language interpretation
#' }
#'
#' @details
#' ## Test Types
#'
#' **Test of Independence** (x and y provided):
#' - Tests if two categorical variables are independent
#' - H0: Variables are independent vs H1: Variables are associated
#'
#' **Goodness of Fit** (only x provided, with p):
#' - Tests if observed frequencies match expected distribution
#' - H0: Data follows expected distribution
#'
#' ## Effect Sizes
#'
#' **Cramér's V**: Measure of association strength (0 to 1)
#' - V = sqrt(χ² / (n * min(r-1, c-1)))
#' - Interpretation (df = 1):
#'   * Small: 0.10
#'   * Medium: 0.30
#'   * Large: 0.50
#'
#' **Phi coefficient** (φ): For 2x2 tables only
#' - φ = sqrt(χ² / n)
#' - Equivalent to Cramér's V for 2x2 tables
#'
#' ## Assumptions and Warnings
#'
#' - Expected frequencies should be ≥ 5 in each cell
#' - If violated, consider Fisher's exact test (for 2x2 tables)
#' - Yates' correction applied by default for 2x2 tables
#'
#' @examples
#' # Test of independence
#' perform_chi_square(mtcars$vs, mtcars$am)
#'
#' # Using table
#' tab <- table(mtcars$vs, mtcars$am)
#' perform_chi_square(tab)
#'
#' # Goodness of fit
#' x <- sample(1:4, 100, replace = TRUE)
#' perform_chi_square(x, p = rep(0.25, 4))
#'
#' @export
perform_chi_square <- function(x,
                               y = NULL,
                               p = NULL,
                               correct = TRUE,
                               simulate.p.value = FALSE,
                               B = 2000) {

  # Determine test type based on inputs
  if (is.table(x) || is.matrix(x)) {
    # x is already a table
    test_type <- "independence"
    obs_table <- as.table(x)

  } else if (!is.null(y)) {
    # Test of independence with two vectors
    test_type <- "independence"
    obs_table <- table(x, y)

  } else if (!is.null(p)) {
    # Goodness of fit test
    test_type <- "goodness_of_fit"
    obs_table <- table(x)

  } else {
    stop("For test of independence, provide both x and y (or a table). For goodness of fit, provide x and p.",
         call. = FALSE)
  }

  # Perform chi-square test
  if (test_type == "independence") {
    chi_result <- stats::chisq.test(obs_table,
                                    correct = correct,
                                    simulate.p.value = simulate.p.value,
                                    B = B)
  } else {
    chi_result <- stats::chisq.test(obs_table,
                                    p = p,
                                    simulate.p.value = simulate.p.value,
                                    B = B)
  }

  # Compute effect sizes
  n <- sum(obs_table)

  if (test_type == "independence") {
    # Cramér's V
    dims <- dim(obs_table)
    min_dim <- min(dims[1] - 1, dims[2] - 1)
    cramers_v <- sqrt(chi_result$statistic / (n * min_dim))

    # Phi coefficient for 2x2 tables
    if (all(dims == c(2, 2))) {
      phi <- sqrt(chi_result$statistic / n)
    } else {
      phi <- NA
    }

  } else {
    # For goodness of fit, use Cramér's V with k-1
    k <- length(obs_table)
    cramers_v <- sqrt(chi_result$statistic / (n * (k - 1)))
    phi <- NA
  }

  # Compute Pearson residuals
  pearson_residuals <- (chi_result$observed - chi_result$expected) /
                       sqrt(chi_result$expected)

  # Check assumptions
  min_expected <- min(chi_result$expected)
  cells_below_5 <- sum(chi_result$expected < 5)
  assumption_warning <- NULL

  if (min_expected < 1 || cells_below_5 > 0.2 * length(chi_result$expected)) {
    assumption_warning <- sprintf(
      "Warning: %d cells have expected frequency < 5. Consider Fisher's exact test or combining categories.",
      cells_below_5
    )
  }

  # Create interpretation
  interpretation <- create_chisq_interpretation(
    p_value = chi_result$p.value,
    cramers_v = cramers_v,
    test_type = test_type,
    assumption_warning = assumption_warning
  )

  # Create result object
  result <- list(
    statistic = as.numeric(chi_result$statistic),
    p_value = chi_result$p.value,
    df = chi_result$parameter,
    method = chi_result$method,
    observed = chi_result$observed,
    expected = chi_result$expected,
    residuals = pearson_residuals,
    cramers_v = as.numeric(cramers_v),
    phi = if (!is.na(phi)) as.numeric(phi) else NA,
    interpretation = interpretation,
    assumption_warning = assumption_warning,
    test_type = test_type
  )

  class(result) <- "descriptR_chisq"
  return(result)
}


#' Create Chi-Square Interpretation
#'
#' @param p_value P-value
#' @param cramers_v Effect size
#' @param test_type Type of test
#' @param assumption_warning Warning message if any
#'
#' @return Character string with interpretation
#'
#' @keywords internal
#' @noRd
create_chisq_interpretation <- function(p_value, cramers_v, test_type,
                                       assumption_warning = NULL) {

  # Significance
  if (p_value < 0.001) {
    sig_text <- "highly significant (p < 0.001)"
  } else if (p_value < 0.01) {
    sig_text <- sprintf("very significant (p = %.3f)", p_value)
  } else if (p_value < 0.05) {
    sig_text <- sprintf("significant (p = %.3f)", p_value)
  } else if (p_value < 0.10) {
    sig_text <- sprintf("marginally significant (p = %.3f)", p_value)
  } else {
    sig_text <- sprintf("not significant (p = %.3f)", p_value)
  }

  # Effect size (Cramér's V interpretation)
  if (cramers_v < 0.1) {
    effect_text <- "negligible"
  } else if (cramers_v < 0.3) {
    effect_text <- "small"
  } else if (cramers_v < 0.5) {
    effect_text <- "medium"
  } else {
    effect_text <- "large"
  }

  # Base interpretation
  if (test_type == "independence") {
    interp <- sprintf(
      "The test is %s with a %s effect size (Cramér's V = %.3f). ",
      sig_text, effect_text, cramers_v
    )

    if (p_value < 0.05) {
      interp <- paste0(interp, "The variables show significant association.")
    } else {
      interp <- paste0(interp, "No significant association detected.")
    }

  } else {
    interp <- sprintf(
      "The goodness of fit test is %s (Cramér's V = %.3f). ",
      sig_text, cramers_v
    )

    if (p_value < 0.05) {
      interp <- paste0(interp,
                      "The observed distribution significantly differs from expected.")
    } else {
      interp <- paste0(interp,
                      "The observed distribution matches the expected distribution.")
    }
  }

  # Add assumption warning if present
  if (!is.null(assumption_warning)) {
    interp <- paste(interp, assumption_warning)
  }

  return(interp)
}


#' Print Method for Chi-Square Results
#'
#' @param x An object of class "descriptR_chisq"
#' @param digits Number of digits to print (default 2)
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR_chisq <- function(x, digits = 2, ...) {

  cat("\n")
  cat(x$method, "\n")
  cat(rep("=", 70), "\n", sep = "")

  # Test results
  cat(sprintf("\nχ² = %.3f, df = %d, p-value = %s\n",
             x$statistic,
             x$df,
             if (x$p_value < 0.001) "< 0.001" else sprintf("%.4f", x$p_value)))

  # Effect size
  cat("\nEffect Size:\n")
  cat(sprintf("  Cramér's V = %.3f (%s)\n",
             x$cramers_v,
             interpret_effect_size(x$cramers_v, "cramers_v")))

  if (!is.na(x$phi)) {
    cat(sprintf("  Phi coefficient (φ) = %.3f (for 2×2 table)\n", x$phi))
  }

  # Observed and expected frequencies
  cat("\n")
  cat(rep("-", 70), "\n", sep = "")
  cat("\nObserved Frequencies:\n")
  print(x$observed)

  cat("\nExpected Frequencies:\n")
  print(round(x$expected, digits))

  # Pearson residuals
  cat("\nPearson Residuals:\n")
  cat("(Values > 2 or < -2 indicate significant contribution to χ²)\n")
  print(round(x$residuals, digits))

  # Highlight cells with large residuals
  large_residuals <- which(abs(x$residuals) > 2, arr.ind = TRUE)
  if (length(large_residuals) > 0) {
    cat("\nCells with large residuals (|r| > 2):\n")
    if (is.matrix(x$residuals)) {
      for (i in 1:nrow(large_residuals)) {
        row_idx <- large_residuals[i, 1]
        col_idx <- large_residuals[i, 2]
        cat(sprintf("  [%s, %s]: residual = %.2f\n",
                   rownames(x$residuals)[row_idx],
                   colnames(x$residuals)[col_idx],
                   x$residuals[row_idx, col_idx]))
      }
    }
  }

  # Interpretation
  cat("\n")
  cat(rep("-", 70), "\n", sep = "")
  cat("\nInterpretation:\n")
  cat(strwrap(x$interpretation, width = 70, prefix = "  "), sep = "\n")

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  invisible(x)
}


#' Fisher's Exact Test
#'
#' @description
#' Performs Fisher's exact test for 2x2 contingency tables. Useful when
#' chi-square assumptions are violated (expected frequencies < 5).
#'
#' @param x First categorical variable or 2x2 table
#' @param y Second categorical variable (optional if x is a table)
#' @param alternative Direction of alternative hypothesis: "two.sided", "less", or "greater"
#' @param conf.level Confidence level for odds ratio (default 0.95)
#'
#' @return List with test results including odds ratio and confidence interval
#'
#' @examples
#' # 2x2 table with small counts
#' tab <- matrix(c(5, 2, 3, 8), nrow = 2)
#' perform_fisher_test(tab)
#'
#' @export
perform_fisher_test <- function(x,
                                y = NULL,
                                alternative = c("two.sided", "less", "greater"),
                                conf.level = 0.95) {

  alternative <- match.arg(alternative)

  # Create table
  if (is.table(x) || is.matrix(x)) {
    tab <- as.table(x)
  } else if (!is.null(y)) {
    tab <- table(x, y)
  } else {
    stop("Provide either a 2x2 table or two categorical variables", call. = FALSE)
  }

  # Check dimensions
  if (!all(dim(tab) == c(2, 2))) {
    stop("Fisher's exact test requires a 2x2 table", call. = FALSE)
  }

  # Perform test
  fisher_result <- stats::fisher.test(tab,
                                     alternative = alternative,
                                     conf.level = conf.level)

  # Format results
  result <- list(
    p_value = fisher_result$p.value,
    odds_ratio = as.numeric(fisher_result$estimate),
    conf_int = as.numeric(fisher_result$conf.int),
    alternative = alternative,
    method = fisher_result$method,
    observed = tab
  )

  class(result) <- "descriptR_fisher"
  return(result)
}


#' Print Method for Fisher's Exact Test
#'
#' @param x An object of class "descriptR_fisher"
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR_fisher <- function(x, ...) {

  cat("\n")
  cat(x$method, "\n")
  cat(rep("=", 70), "\n", sep = "")

  cat(sprintf("\nOdds Ratio = %.3f\n", x$odds_ratio))
  cat(sprintf("95%% CI: [%.3f, %.3f]\n", x$conf_int[1], x$conf_int[2]))
  cat(sprintf("p-value = %s\n",
             if (x$p_value < 0.001) "< 0.001" else sprintf("%.4f", x$p_value)))

  cat("\nObserved Frequencies:\n")
  print(x$observed)

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  invisible(x)
}
