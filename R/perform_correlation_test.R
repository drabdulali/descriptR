#' Perform Correlation Tests
#'
#' @description
#' Computes correlation coefficients with significance tests. Supports Pearson,
#' Spearman, and Kendall correlations. Provides confidence intervals and
#' interpretation.
#'
#' @param x Numeric vector (first variable)
#' @param y Numeric vector (second variable, optional for correlation matrix)
#' @param data Data frame (if using formula or variable names)
#' @param vars Character vector of variable names for correlation matrix
#' @param method Correlation method: "pearson" (default), "spearman", or "kendall"
#' @param alternative Direction of alternative: "two.sided" (default), "less", or "greater"
#' @param conf.level Confidence level (default 0.95)
#' @param use Method for handling missing values: "everything", "complete.obs", "pairwise.complete.obs"
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return For single correlation: object of class "descriptR_cor"
#'         For correlation matrix: object of class "descriptR_cor_matrix"
#'
#' Single correlation contains:
#' \itemize{
#'   \item \code{estimate}: Correlation coefficient
#'   \item \code{statistic}: Test statistic
#'   \item \code{p_value}: P-value
#'   \item \code{conf_int}: Confidence interval (Pearson only)
#'   \item \code{method}: Correlation method used
#'   \item \code{n}: Sample size
#'   \item \code{interpretation}: Plain-language interpretation
#' }
#'
#' @details
#' ## Correlation Methods
#'
#' **Pearson (r)**: Linear correlation
#' - Assumes: Linear relationship, continuous variables, bivariate normality
#' - Range: -1 to +1
#' - Interpretation:
#'   * |r| < 0.1: negligible
#'   * |r| < 0.3: small
#'   * |r| < 0.5: medium
#'   * |r| < 0.7: large
#'   * |r| ≥ 0.7: very large
#'
#' **Spearman (ρ)**: Monotonic correlation (rank-based)
#' - Non-parametric alternative to Pearson
#' - Robust to outliers
#' - Use for: Ordinal data, non-linear monotonic relationships
#'
#' **Kendall (τ)**: Rank correlation
#' - More robust than Spearman for small samples
#' - Better for ties in data
#' - More computationally intensive
#'
#' ## Confidence Intervals
#'
#' - Available for Pearson correlation using Fisher's z-transformation
#' - Not available for Spearman/Kendall (use bootstrap for these)
#'
#' @examples
#' # Pearson correlation
#' perform_correlation_test(iris$Sepal.Length, iris$Sepal.Width)
#'
#' # Spearman correlation
#' perform_correlation_test(iris$Sepal.Length, iris$Sepal.Width,
#'                         method = "spearman")
#'
#' # Correlation matrix
#' perform_correlation_test(data = iris,
#'                         vars = c("Sepal.Length", "Sepal.Width",
#'                                 "Petal.Length", "Petal.Width"))
#'
#' @export
perform_correlation_test <- function(x = NULL,
                                    y = NULL,
                                    data = NULL,
                                    vars = NULL,
                                    method = c("pearson", "spearman", "kendall"),
                                    alternative = c("two.sided", "less", "greater"),
                                    conf.level = 0.95,
                                    use = "pairwise.complete.obs",
                                    na.rm = TRUE) {

  method <- match.arg(method)
  alternative <- match.arg(alternative)

  # Determine if single correlation or correlation matrix
  if (!is.null(vars) && !is.null(data)) {
    # Correlation matrix
    return(compute_correlation_matrix(data, vars, method, use))
  }

  # Single correlation
  if (is.null(x) || is.null(y)) {
    stop("Provide both x and y for single correlation, or data and vars for correlation matrix",
         call. = FALSE)
  }

  # Validate numeric
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric", call. = FALSE)
  }

  # Handle NA
  if (na.rm) {
    complete_cases <- complete.cases(x, y)
    x <- x[complete_cases]
    y <- y[complete_cases]
  }

  n <- length(x)

  if (n < 3) {
    stop("Need at least 3 observations for correlation test", call. = FALSE)
  }

  # Perform correlation test
  cor_result <- stats::cor.test(x, y,
                               method = method,
                               alternative = alternative,
                               conf.level = conf.level)

  # Extract confidence interval (only for Pearson)
  if (method == "pearson") {
    conf_int <- as.numeric(cor_result$conf.int)
  } else {
    conf_int <- c(NA, NA)
  }

  # Create interpretation
  interpretation <- create_cor_interpretation(
    estimate = as.numeric(cor_result$estimate),
    p_value = cor_result$p.value,
    method = method,
    n = n
  )

  # Create result object
  result <- list(
    estimate = as.numeric(cor_result$estimate),
    statistic = if (!is.null(cor_result$statistic)) as.numeric(cor_result$statistic) else NA,
    p_value = cor_result$p.value,
    conf_int = conf_int,
    df = if (!is.null(cor_result$parameter)) as.numeric(cor_result$parameter) else NA,
    method = cor_result$method,
    alternative = alternative,
    n = n,
    interpretation = interpretation
  )

  class(result) <- "descriptR_cor"
  return(result)
}


#' Compute Correlation Matrix
#'
#' @param data Data frame
#' @param vars Variable names
#' @param method Correlation method
#' @param use Handling of missing values
#'
#' @return Correlation matrix object
#'
#' @keywords internal
#' @noRd
compute_correlation_matrix <- function(data, vars, method, use) {

  validate_data_frame(data)
  validate_variables(data, vars)

  # Extract numeric variables
  data_numeric <- data[vars]

  # Check all numeric
  non_numeric <- vars[!sapply(data_numeric, is.numeric)]
  if (length(non_numeric) > 0) {
    stop(sprintf("Variables must be numeric: %s",
                paste(non_numeric, collapse = ", ")),
         call. = FALSE)
  }

  # Compute correlation matrix
  cor_matrix <- stats::cor(data_numeric, method = method, use = use)

  # Compute p-values
  n <- nrow(data_numeric)
  p_matrix <- matrix(NA, nrow = length(vars), ncol = length(vars))
  rownames(p_matrix) <- vars
  colnames(p_matrix) <- vars

  for (i in 1:length(vars)) {
    for (j in 1:length(vars)) {
      if (i != j) {
        test_result <- stats::cor.test(data_numeric[[i]],
                                      data_numeric[[j]],
                                      method = method)
        p_matrix[i, j] <- test_result$p.value
      }
    }
  }
  diag(p_matrix) <- 0  # Diagonal p-values are 0 (perfect correlation with self)

  # Create result object
  result <- list(
    cor_matrix = cor_matrix,
    p_matrix = p_matrix,
    method = method,
    n = n,
    vars = vars
  )

  class(result) <- "descriptR_cor_matrix"
  return(result)
}


#' Create Correlation Interpretation
#'
#' @param estimate Correlation coefficient
#' @param p_value P-value
#' @param method Correlation method
#' @param n Sample size
#'
#' @return Character string with interpretation
#'
#' @keywords internal
#' @noRd
create_cor_interpretation <- function(estimate, p_value, method, n) {

  # Absolute correlation
  abs_r <- abs(estimate)

  # Effect size interpretation
  if (abs_r < 0.1) {
    effect_text <- "negligible"
  } else if (abs_r < 0.3) {
    effect_text <- "small"
  } else if (abs_r < 0.5) {
    effect_text <- "medium"
  } else if (abs_r < 0.7) {
    effect_text <- "large"
  } else {
    effect_text <- "very large"
  }

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

  # Direction
  direction <- if (estimate > 0) "positive" else if (estimate < 0) "negative" else "no"

  # Coefficient name
  coef_name <- switch(method,
                     "pearson" = "r",
                     "spearman" = "ρ",
                     "kendall" = "τ")

  # Combine
  sprintf("The %s correlation is %s with a %s effect size (%s = %.3f, %s direction, n = %d)",
         method, sig_text, effect_text, coef_name, abs(estimate), direction, n)
}


#' Print Method for Correlation Test
#'
#' @param x An object of class "descriptR_cor"
#' @param digits Number of digits to print (default 3)
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR_cor <- function(x, digits = 3, ...) {

  cat("\n")
  cat(x$method, "\n")
  cat(rep("=", 70), "\n", sep = "")

  # Correlation coefficient
  coef_name <- switch(
    gsub("'s product-moment correlation|'s rank correlation rho|'s rank correlation tau", "",
         x$method),
    "Pearson" = "r",
    "Spearman" = "ρ",
    "Kendall" = "τ",
    "r"  # default
  )

  cat(sprintf("\nCorrelation coefficient: %s = %.3f\n", coef_name, x$estimate))

  # Test statistic and p-value
  if (!is.na(x$statistic)) {
    cat(sprintf("Test statistic: %.3f\n", x$statistic))
  }

  cat(sprintf("p-value = %s\n",
             if (x$p_value < 0.001) "< 0.001" else sprintf("%.4f", x$p_value)))

  # Confidence interval (Pearson only)
  if (!any(is.na(x$conf_int))) {
    cat(sprintf("95%% confidence interval: [%.3f, %.3f]\n",
               x$conf_int[1], x$conf_int[2]))
  }

  # Sample size
  cat(sprintf("Sample size: n = %d\n", x$n))

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


#' Print Method for Correlation Matrix
#'
#' @param x An object of class "descriptR_cor_matrix"
#' @param digits Number of digits to print (default 2)
#' @param sig.level Significance level for marking (default 0.05)
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR_cor_matrix <- function(x, digits = 2, sig.level = 0.05, ...) {

  cat("\n")
  cat(sprintf("%s Correlation Matrix\n", tools::toTitleCase(x$method)))
  cat(rep("=", 70), "\n", sep = "")
  cat(sprintf("Sample size: n = %d\n\n", x$n))

  # Print correlation matrix
  cat("Correlation Coefficients:\n")
  print(round(x$cor_matrix, digits))

  cat("\n")
  cat(rep("-", 70), "\n", sep = "")
  cat("\nP-values:\n")
  print(round(x$p_matrix, 4))

  # Identify significant correlations
  sig_cors <- which(x$p_matrix < sig.level & x$p_matrix > 0, arr.ind = TRUE)

  if (nrow(sig_cors) > 0) {
    # Remove duplicates (symmetric matrix)
    sig_cors <- sig_cors[sig_cors[, 1] < sig_cors[, 2], , drop = FALSE]

    cat("\n")
    cat(rep("-", 70), "\n", sep = "")
    cat(sprintf("\nSignificant Correlations (p < %.2f):\n", sig.level))

    for (i in 1:nrow(sig_cors)) {
      row_idx <- sig_cors[i, 1]
      col_idx <- sig_cors[i, 2]
      var1 <- x$vars[row_idx]
      var2 <- x$vars[col_idx]
      r <- x$cor_matrix[row_idx, col_idx]
      p <- x$p_matrix[row_idx, col_idx]

      cat(sprintf("  %s ↔ %s: r = %.3f, p = %.4f\n",
                 var1, var2, r, p))
    }
  } else {
    cat("\nNo significant correlations found.\n")
  }

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  invisible(x)
}
