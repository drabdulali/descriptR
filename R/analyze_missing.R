#' Comprehensive Missing Data Analysis
#'
#' @description
#' Performs comprehensive analysis of missing data patterns, tests for missing
#' data mechanisms, and provides recommendations for handling missingness.
#'
#' @param data Data frame
#' @param vars Character vector of variable names to analyze (NULL for all)
#' @param test_mcar Logical, test for MCAR (Missing Completely At Random)? (default TRUE)
#' @param pattern_analysis Logical, analyze missing patterns? (default TRUE)
#' @param correlation_analysis Logical, analyze correlations between missingness? (default TRUE)
#' @param include_plots Logical, generate visualization plots? (default FALSE)
#' @param max_patterns Maximum number of patterns to display (default 10)
#'
#' @return An object of class "descriptR_missing" containing:
#' \itemize{
#'   \item \code{summary}: Overall missing data summary
#'   \item \code{variable_summary}: Per-variable missing statistics
#'   \item \code{patterns}: Missing data patterns
#'   \item \code{mcar_test}: Little's MCAR test results (if requested)
#'   \item \code{correlations}: Missingness correlations (if requested)
#'   \item \code{recommendations}: Recommendations for handling missingness
#'   \item \code{plots}: Visualization plots (if requested)
#' }
#'
#' @details
#' ## Missing Data Mechanisms
#'
#' **MCAR (Missing Completely At Random)**:
#' - Missingness is unrelated to any variable (observed or unobserved)
#' - Can be tested with Little's MCAR test
#' - Safe to use complete case analysis
#'
#' **MAR (Missing At Random)**:
#' - Missingness related to observed variables only
#' - Can be addressed with proper imputation methods
#' - Multiple imputation recommended
#'
#' **MNAR (Missing Not At Random)**:
#' - Missingness related to unobserved values
#' - Most problematic; requires specialized methods
#' - Sensitivity analysis recommended
#'
#' ## Analysis Components
#'
#' 1. **Summary Statistics**: Overall and per-variable missing rates
#' 2. **Pattern Analysis**: Identifies common patterns of missingness
#' 3. **MCAR Testing**: Little's test for complete randomness
#' 4. **Correlation Analysis**: Relationships between missing indicators
#' 5. **Recommendations**: Suggested approaches based on findings
#'
#' @examples
#' # Complete missing data analysis
#' analyze_missing(airquality)
#'
#' # Specific variables only
#' analyze_missing(airquality, vars = c("Ozone", "Solar.R"))
#'
#' # Without MCAR test
#' analyze_missing(airquality, test_mcar = FALSE)
#'
#' @export
analyze_missing <- function(data,
                            vars = NULL,
                            test_mcar = TRUE,
                            pattern_analysis = TRUE,
                            correlation_analysis = TRUE,
                            include_plots = FALSE,
                            max_patterns = 10) {

  # Validate inputs
  validate_data_frame(data)

  # Select variables
  if (is.null(vars)) {
    vars <- names(data)
  } else {
    validate_variables(data, vars)
  }

  data_subset <- data[, vars, drop = FALSE]
  n_total <- nrow(data_subset)
  n_vars <- ncol(data_subset)

  # Overall summary
  n_complete <- sum(complete.cases(data_subset))
  n_incomplete <- n_total - n_complete
  pct_complete <- n_complete / n_total * 100
  total_cells <- n_total * n_vars
  missing_cells <- sum(is.na(data_subset))
  pct_missing_overall <- missing_cells / total_cells * 100

  overall_summary <- list(
    n_observations = n_total,
    n_variables = n_vars,
    n_complete_cases = n_complete,
    n_incomplete_cases = n_incomplete,
    pct_complete_cases = pct_complete,
    total_cells = total_cells,
    missing_cells = missing_cells,
    pct_missing_overall = pct_missing_overall
  )

  # Per-variable summary
  variable_summary <- data.frame(
    Variable = vars,
    N_Total = n_total,
    N_Missing = sapply(data_subset, function(x) sum(is.na(x))),
    N_Present = sapply(data_subset, function(x) sum(!is.na(x))),
    Pct_Missing = sapply(data_subset, function(x) sum(is.na(x)) / length(x) * 100),
    stringsAsFactors = FALSE
  )

  # Sort by missing percentage
  variable_summary <- variable_summary[order(-variable_summary$Pct_Missing), ]

  # Pattern analysis
  patterns <- NULL
  if (pattern_analysis) {
    patterns <- analyze_missing_patterns(data_subset, max_patterns)
  }

  # MCAR test
  mcar_test <- NULL
  if (test_mcar && missing_cells > 0 && n_incomplete > 0) {
    mcar_test <- test_little_mcar(data_subset)
  }

  # Correlation analysis
  correlations <- NULL
  if (correlation_analysis && n_vars > 1) {
    correlations <- analyze_missingness_correlations(data_subset)
  }

  # Generate recommendations
  recommendations <- generate_missing_recommendations(
    overall_summary,
    variable_summary,
    mcar_test,
    correlations
  )

  # Plots (placeholder for future implementation)
  plots <- NULL
  if (include_plots) {
    plots <- list(message = "Plotting functionality coming in Phase 4")
  }

  # Create result object
  result <- list(
    summary = overall_summary,
    variable_summary = variable_summary,
    patterns = patterns,
    mcar_test = mcar_test,
    correlations = correlations,
    recommendations = recommendations,
    plots = plots
  )

  class(result) <- "descriptR_missing"
  return(result)
}


#' Analyze Missing Data Patterns
#'
#' @param data Data frame
#' @param max_patterns Maximum patterns to report
#'
#' @return Data frame with pattern information
#'
#' @keywords internal
#' @noRd
analyze_missing_patterns <- function(data, max_patterns = 10) {

  # Create missing indicator matrix
  missing_matrix <- is.na(data)

  # Convert to pattern strings
  pattern_strings <- apply(missing_matrix, 1, function(row) {
    paste(as.integer(row), collapse = "")
  })

  # Count patterns
  pattern_counts <- table(pattern_strings)
  pattern_counts <- sort(pattern_counts, decreasing = TRUE)

  # Limit to max_patterns
  if (length(pattern_counts) > max_patterns) {
    pattern_counts <- pattern_counts[1:max_patterns]
  }

  # Create pattern descriptions
  patterns_df <- data.frame(
    Pattern_ID = 1:length(pattern_counts),
    N_Cases = as.numeric(pattern_counts),
    Pct_Cases = as.numeric(pattern_counts) / nrow(data) * 100,
    stringsAsFactors = FALSE
  )

  # Add variable names that are missing in each pattern
  pattern_vars <- lapply(names(pattern_counts), function(pattern_str) {
    pattern_bits <- as.integer(strsplit(pattern_str, "")[[1]])
    missing_vars <- names(data)[pattern_bits == 1]
    if (length(missing_vars) == 0) {
      return("Complete (no missing)")
    } else {
      return(paste(missing_vars, collapse = ", "))
    }
  })

  patterns_df$Missing_Variables <- unlist(pattern_vars)

  # Add pattern string for reference
  patterns_df$Pattern_Code <- names(pattern_counts)

  return(patterns_df)
}


#' Test for MCAR using Little's Test
#'
#' @description
#' Performs Little's test for Missing Completely At Random (MCAR).
#' Tests the null hypothesis that data are MCAR.
#'
#' @param data Data frame with numeric variables
#'
#' @return List with test results
#'
#' @details
#' Little's MCAR test compares observed and expected covariance matrices
#' under the assumption of multivariate normality.
#'
#' H0: Data are MCAR
#' H1: Data are not MCAR (MAR or MNAR)
#'
#' p-value < 0.05: Reject MCAR hypothesis
#' p-value ≥ 0.05: Cannot reject MCAR (consistent with MCAR)
#'
#' Note: Test assumes multivariate normality and requires numeric variables.
#'
#' @keywords internal
#' @noRd
test_little_mcar <- function(data) {

  # Select only numeric variables
  numeric_vars <- sapply(data, is.numeric)
  if (sum(numeric_vars) < 2) {
    return(list(
      error = "Little's MCAR test requires at least 2 numeric variables",
      test_performed = FALSE
    ))
  }

  data_numeric <- data[, numeric_vars, drop = FALSE]

  # Check if there's any missingness
  if (sum(is.na(data_numeric)) == 0) {
    return(list(
      message = "No missing data to test",
      test_performed = FALSE
    ))
  }

  # Simplified Little's MCAR test implementation
  # Full implementation would use naniar or MissMech packages
  # This is a basic approximation

  tryCatch({
    # Create missing indicator matrix
    missing_matrix <- is.na(data_numeric)
    n <- nrow(data_numeric)
    p <- ncol(data_numeric)

    # Pattern-based approach
    pattern_strings <- apply(missing_matrix, 1, function(row) {
      paste(as.integer(row), collapse = "")
    })

    patterns <- unique(pattern_strings)
    n_patterns <- length(patterns)

    # Compute test statistic (simplified version)
    # Full version would compute d² statistic comparing pattern means
    chi_sq_approx <- 0
    df <- 0

    for (pattern in patterns) {
      pattern_rows <- pattern_strings == pattern
      n_pattern <- sum(pattern_rows)

      if (n_pattern > 1 && pattern != paste(rep("0", p), collapse = "")) {
        # Variables observed in this pattern
        pattern_bits <- as.integer(strsplit(pattern, "")[[1]])
        obs_vars <- which(pattern_bits == 0)

        if (length(obs_vars) > 0) {
          # Compare pattern means to overall means
          pattern_data <- data_numeric[pattern_rows, obs_vars, drop = FALSE]
          overall_means <- colMeans(data_numeric[, obs_vars, drop = FALSE],
                                    na.rm = TRUE)

          if (length(obs_vars) > 1) {
            pattern_means <- colMeans(pattern_data, na.rm = TRUE)
            # Simplified contribution to chi-square
            diff <- pattern_means - overall_means
            chi_sq_approx <- chi_sq_approx + n_pattern * sum(diff^2)
            df <- df + length(obs_vars)
          }
        }
      }
    }

    # Adjust df
    df <- max(1, df - p)

    # Compute p-value
    p_value <- pchisq(chi_sq_approx, df, lower.tail = FALSE)

    # Interpretation
    if (p_value < 0.001) {
      interpretation <- "Strong evidence against MCAR (p < 0.001). Data likely MAR or MNAR."
    } else if (p_value < 0.05) {
      interpretation <- sprintf("Evidence against MCAR (p = %.3f). Data may be MAR or MNAR.", p_value)
    } else if (p_value < 0.10) {
      interpretation <- sprintf("Weak evidence against MCAR (p = %.3f). MCAR is plausible but not confirmed.", p_value)
    } else {
      interpretation <- sprintf("No evidence against MCAR (p = %.3f). Data consistent with MCAR.", p_value)
    }

    result <- list(
      test_statistic = chi_sq_approx,
      df = df,
      p_value = p_value,
      n_patterns = n_patterns,
      interpretation = interpretation,
      test_performed = TRUE,
      note = "Simplified MCAR test. For definitive results, consider using naniar::mcar_test() or MissMech::TestMCARNormality()"
    )

    return(result)

  }, error = function(e) {
    return(list(
      error = paste("MCAR test failed:", e$message),
      test_performed = FALSE
    ))
  })
}


#' Analyze Correlations Between Missingness Indicators
#'
#' @param data Data frame
#'
#' @return List with correlation results
#'
#' @keywords internal
#' @noRd
analyze_missingness_correlations <- function(data) {

  # Create missing indicator matrix
  missing_matrix <- is.na(data)
  colnames(missing_matrix) <- names(data)

  # Only analyze if there's variance in missingness
  vars_with_missing <- colSums(missing_matrix) > 0 &
                       colSums(missing_matrix) < nrow(data)

  if (sum(vars_with_missing) < 2) {
    return(list(
      message = "Insufficient variation in missingness for correlation analysis",
      correlations_computed = FALSE
    ))
  }

  # Select variables with missing data
  missing_subset <- missing_matrix[, vars_with_missing, drop = FALSE]

  # Compute correlations between missing indicators
  cor_matrix <- cor(missing_subset * 1, use = "pairwise.complete.obs")

  # Find significant correlations
  n <- nrow(missing_subset)
  p_matrix <- matrix(1, nrow = ncol(cor_matrix), ncol = ncol(cor_matrix))
  rownames(p_matrix) <- colnames(cor_matrix)
  colnames(p_matrix) <- colnames(cor_matrix)

  for (i in 1:(ncol(cor_matrix) - 1)) {
    for (j in (i + 1):ncol(cor_matrix)) {
      r <- cor_matrix[i, j]
      # Test for correlation
      if (!is.na(r)) {
        t_stat <- r * sqrt(n - 2) / sqrt(1 - r^2)
        p_val <- 2 * pt(abs(t_stat), df = n - 2, lower.tail = FALSE)
        p_matrix[i, j] <- p_val
        p_matrix[j, i] <- p_val
      }
    }
  }

  # Extract significant correlations
  sig_cors <- which(p_matrix < 0.05 & upper.tri(p_matrix), arr.ind = TRUE)

  if (nrow(sig_cors) > 0) {
    sig_pairs <- data.frame(
      Variable_1 = rownames(cor_matrix)[sig_cors[, 1]],
      Variable_2 = colnames(cor_matrix)[sig_cors[, 2]],
      Correlation = cor_matrix[sig_cors],
      P_Value = p_matrix[sig_cors],
      stringsAsFactors = FALSE
    )
    sig_pairs <- sig_pairs[order(-abs(sig_pairs$Correlation)), ]
  } else {
    sig_pairs <- NULL
  }

  result <- list(
    correlation_matrix = cor_matrix,
    p_value_matrix = p_matrix,
    significant_pairs = sig_pairs,
    correlations_computed = TRUE
  )

  return(result)
}


#' Generate Missing Data Recommendations
#'
#' @param summary Overall summary
#' @param variable_summary Variable-level summary
#' @param mcar_test MCAR test results
#' @param correlations Correlation results
#'
#' @return Character vector of recommendations
#'
#' @keywords internal
#' @noRd
generate_missing_recommendations <- function(summary, variable_summary,
                                            mcar_test, correlations) {

  recommendations <- character()

  # Overall missing rate
  if (summary$pct_missing_overall < 1) {
    recommendations <- c(recommendations,
      "• Very low missing data rate (<1%). Complete case analysis is acceptable.")

  } else if (summary$pct_missing_overall < 5) {
    recommendations <- c(recommendations,
      "• Low missing data rate (1-5%). Multiple imputation or complete case analysis recommended.")

  } else if (summary$pct_missing_overall < 10) {
    recommendations <- c(recommendations,
      "• Moderate missing data rate (5-10%). Multiple imputation strongly recommended.")

  } else {
    recommendations <- c(recommendations,
      sprintf("• High missing data rate (%.1f%%). Carefully consider missing data mechanism. Multiple imputation required.",
              summary$pct_missing_overall))
  }

  # Variables with high missingness
  high_missing <- variable_summary[variable_summary$Pct_Missing > 20, ]
  if (nrow(high_missing) > 0) {
    recommendations <- c(recommendations,
      sprintf("• Variables with >20%% missing: %s. Consider excluding or use specialized imputation.",
              paste(high_missing$Variable, collapse = ", ")))
  }

  # Variables with very high missingness
  very_high_missing <- variable_summary[variable_summary$Pct_Missing > 50, ]
  if (nrow(very_high_missing) > 0) {
    recommendations <- c(recommendations,
      sprintf("• Variables with >50%% missing: %s. Strongly consider exclusion from analysis.",
              paste(very_high_missing$Variable, collapse = ", ")))
  }

  # MCAR test results
  if (!is.null(mcar_test) && mcar_test$test_performed) {
    if (mcar_test$p_value < 0.05) {
      recommendations <- c(recommendations,
        "• MCAR test significant (p < 0.05). Data not missing completely at random. Use MAR-appropriate methods.")
    } else {
      recommendations <- c(recommendations,
        "• MCAR test non-significant. Data consistent with MCAR. Complete case analysis is valid.")
    }
  }

  # Correlation patterns
  if (!is.null(correlations) && correlations$correlations_computed) {
    if (!is.null(correlations$significant_pairs)) {
      n_sig <- nrow(correlations$significant_pairs)
      recommendations <- c(recommendations,
        sprintf("• Found %d significant correlation%s between missing indicators. Suggests MAR mechanism.",
                n_sig, ifelse(n_sig == 1, "", "s")))
    }
  }

  # Complete cases
  if (summary$pct_complete_cases < 50) {
    recommendations <- c(recommendations,
      sprintf("• Only %.1f%% complete cases. Complete case analysis would discard substantial data. Imputation essential.",
              summary$pct_complete_cases))
  }

  # General recommendations
  recommendations <- c(recommendations,
    "",
    "Recommended approaches:",
    "  1. Multiple imputation (MICE, missForest, Amelia)",
    "  2. Maximum likelihood methods (EM algorithm)",
    "  3. Sensitivity analysis to assess robustness",
    "",
    "Avoid:",
    "  × Mean imputation (biases standard errors)",
    "  × Last observation carried forward (LOCF)",
    "  × Single imputation without accounting for uncertainty"
  )

  return(recommendations)
}


#' Print Method for Missing Data Analysis
#'
#' @param x An object of class "descriptR_missing"
#' @param digits Number of digits to print (default 2)
#' @param max_patterns Maximum patterns to display (default 10)
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR_missing <- function(x, digits = 2, max_patterns = 10, ...) {

  cat("\n")
  cat("Missing Data Analysis\n")
  cat(rep("=", 80), "\n", sep = "")

  # Overall summary
  cat("\nOverall Summary:\n")
  cat(sprintf("  Total observations: %d\n", x$summary$n_observations))
  cat(sprintf("  Total variables: %d\n", x$summary$n_variables))
  cat(sprintf("  Complete cases: %d (%.1f%%)\n",
             x$summary$n_complete_cases, x$summary$pct_complete_cases))
  cat(sprintf("  Incomplete cases: %d (%.1f%%)\n",
             x$summary$n_incomplete_cases,
             100 - x$summary$pct_complete_cases))
  cat(sprintf("  Total cells: %s\n", format(x$summary$total_cells, big.mark = ",")))
  cat(sprintf("  Missing cells: %s (%.2f%%)\n",
             format(x$summary$missing_cells, big.mark = ","),
             x$summary$pct_missing_overall))

  # Variable summary
  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nMissing Data by Variable:\n\n")

  # Show only variables with missing data, or top N if all have missing
  vars_to_show <- x$variable_summary[x$variable_summary$N_Missing > 0, ]
  if (nrow(vars_to_show) == 0) {
    cat("  No missing data detected.\n")
  } else {
    if (nrow(vars_to_show) > 15) {
      vars_to_show <- vars_to_show[1:15, ]
      cat("  (Showing top 15 variables by missing rate)\n\n")
    }
    print(vars_to_show, row.names = FALSE, digits = digits)
  }

  # Missing patterns
  if (!is.null(x$patterns)) {
    cat("\n")
    cat(rep("-", 80), "\n", sep = "")
    cat(sprintf("\nMissing Data Patterns (top %d):\n\n", max_patterns))

    patterns_to_show <- head(x$patterns, max_patterns)
    patterns_display <- patterns_to_show[, c("Pattern_ID", "N_Cases",
                                             "Pct_Cases", "Missing_Variables")]
    print(patterns_display, row.names = FALSE, digits = digits)
  }

  # MCAR test
  if (!is.null(x$mcar_test) && x$mcar_test$test_performed) {
    cat("\n")
    cat(rep("-", 80), "\n", sep = "")
    cat("\nLittle's MCAR Test:\n\n")
    cat(sprintf("  χ² = %.3f, df = %d, p-value = %s\n",
               x$mcar_test$test_statistic,
               x$mcar_test$df,
               if (x$mcar_test$p_value < 0.001) "< 0.001"
               else sprintf("%.4f", x$mcar_test$p_value)))
    cat(sprintf("  %s\n", x$mcar_test$interpretation))
    if (!is.null(x$mcar_test$note)) {
      cat(sprintf("\n  Note: %s\n", x$mcar_test$note))
    }
  }

  # Correlations
  if (!is.null(x$correlations) && x$correlations$correlations_computed) {
    if (!is.null(x$correlations$significant_pairs)) {
      cat("\n")
      cat(rep("-", 80), "\n", sep = "")
      cat("\nSignificant Correlations Between Missing Indicators:\n\n")

      pairs_to_show <- head(x$correlations$significant_pairs, 10)
      print(pairs_to_show, row.names = FALSE, digits = 3)
    }
  }

  # Recommendations
  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nRecommendations:\n\n")
  cat(paste(x$recommendations, collapse = "\n"))
  cat("\n")

  cat("\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("\n")

  invisible(x)
}


#' Summary Method for Missing Data Analysis
#'
#' @param object An object of class "descriptR_missing"
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.descriptR_missing <- function(object, ...) {

  cat("\n")
  cat("Missing Data Analysis Summary\n")
  cat(rep("=", 70), "\n", sep = "")

  cat(sprintf("\nDataset: %d observations × %d variables\n",
             object$summary$n_observations, object$summary$n_variables))
  cat(sprintf("Missing data rate: %.2f%%\n", object$summary$pct_missing_overall))
  cat(sprintf("Complete cases: %.1f%%\n", object$summary$pct_complete_cases))

  # Count variables with missing
  n_vars_missing <- sum(object$variable_summary$N_Missing > 0)
  cat(sprintf("Variables with missing data: %d out of %d\n",
             n_vars_missing, object$summary$n_variables))

  # MCAR test quick result
  if (!is.null(object$mcar_test) && object$mcar_test$test_performed) {
    mcar_status <- if (object$mcar_test$p_value < 0.05) "NOT MCAR" else "Consistent with MCAR"
    cat(sprintf("MCAR test: %s (p = %.4f)\n", mcar_status, object$mcar_test$p_value))
  }

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  invisible(object)
}
