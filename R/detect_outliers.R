#' Comprehensive Outlier Detection
#'
#' @description
#' Detects outliers using multiple methods and provides recommendations for
#' handling them. Supports univariate and multivariate outlier detection.
#'
#' @param data Data frame or numeric vector
#' @param vars Character vector of variable names to analyze (NULL for all numeric)
#' @param methods Character vector of detection methods to use:
#'   \itemize{
#'     \item "zscore": Z-score method (|z| > threshold)
#'     \item "iqr": Interquartile range method (Tukey's fences)
#'     \item "mad": Median Absolute Deviation
#'     \item "grubbs": Grubbs' test for single outlier
#'     \item "dixon": Dixon's Q test
#'     \item "rosner": Rosner's test for multiple outliers
#'   }
#' @param threshold Threshold for z-score and MAD methods (default 3)
#' @param iqr_multiplier Multiplier for IQR method (default 1.5)
#' @param alpha Significance level for statistical tests (default 0.05)
#' @param consensus Require multiple methods to agree? (default FALSE)
#' @param min_methods Minimum methods flagging for consensus (default 2)
#'
#' @return An object of class "descriptR_outliers" containing:
#' \itemize{
#'   \item \code{outliers}: Data frame of detected outliers
#'   \item \code{summary}: Summary by variable
#'   \item \code{method_results}: Results from each method
#'   \item \code{consensus_outliers}: Outliers flagged by multiple methods
#'   \item \code{recommendations}: Recommendations for handling
#' }
#'
#' @details
#' ## Detection Methods
#'
#' **Z-Score Method**:
#' - Flags values where |z| > threshold (default 3)
#' - Assumes normal distribution
#' - Good for: Symmetric distributions
#'
#' **IQR Method (Tukey's Fences)**:
#' - Outliers: < Q1 - 1.5×IQR or > Q3 + 1.5×IQR
#' - Extreme outliers: < Q1 - 3×IQR or > Q3 + 3×IQR
#' - Non-parametric, robust to skewness
#' - Good for: Any distribution
#'
#' **MAD Method (Median Absolute Deviation)**:
#' - Modified z-score using median and MAD
#' - More robust than z-score
#' - Good for: Skewed distributions
#'
#' **Grubbs' Test**:
#' - Tests for single most extreme outlier
#' - Assumes normal distribution
#' - Iterative application detects multiple outliers
#'
#' **Dixon's Q Test**:
#' - Ratio-based test for outliers
#' - Good for small samples (3-30)
#' - Assumes normal distribution
#'
#' **Rosner's Test**:
#' - Generalized ESD test for multiple outliers
#' - Accounts for masking effect
#' - Assumes normal distribution
#'
#' @examples
#' # Single method
#' detect_outliers(mtcars$mpg, methods = "iqr")
#'
#' # Multiple methods
#' detect_outliers(iris, vars = c("Sepal.Length", "Sepal.Width"),
#'                methods = c("zscore", "iqr", "mad"))
#'
#' # Consensus approach
#' detect_outliers(mtcars, methods = c("zscore", "iqr", "mad"),
#'                consensus = TRUE, min_methods = 2)
#'
#' @export
detect_outliers <- function(data,
                           vars = NULL,
                           methods = c("zscore", "iqr", "mad"),
                           threshold = 3,
                           iqr_multiplier = 1.5,
                           alpha = 0.05,
                           consensus = FALSE,
                           min_methods = 2) {

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

  # Validate methods
  valid_methods <- c("zscore", "iqr", "mad", "grubbs", "dixon", "rosner")
  invalid <- setdiff(methods, valid_methods)
  if (length(invalid) > 0) {
    stop(sprintf("Invalid methods: %s. Choose from: %s",
                paste(invalid, collapse = ", "),
                paste(valid_methods, collapse = ", ")),
         call. = FALSE)
  }

  # Initialize results
  method_results <- list()
  all_outliers <- list()

  # Apply each method to each variable
  for (var in vars) {
    x <- data[[var]]

    # Remove NA
    x_clean <- x[!is.na(x)]
    original_indices <- which(!is.na(x))

    if (length(x_clean) < 3) {
      next  # Skip variables with too few observations
    }

    var_outliers <- list()

    # Z-score method
    if ("zscore" %in% methods) {
      zscore_result <- detect_outliers_zscore(x_clean, threshold)
      var_outliers$zscore <- original_indices[zscore_result$outlier_indices]
      method_results[[paste0(var, "_zscore")]] <- zscore_result
    }

    # IQR method
    if ("iqr" %in% methods) {
      iqr_result <- detect_outliers_iqr(x_clean, iqr_multiplier)
      var_outliers$iqr <- original_indices[iqr_result$outlier_indices]
      method_results[[paste0(var, "_iqr")]] <- iqr_result
    }

    # MAD method
    if ("mad" %in% methods) {
      mad_result <- detect_outliers_mad(x_clean, threshold)
      var_outliers$mad <- original_indices[mad_result$outlier_indices]
      method_results[[paste0(var, "_mad")]] <- mad_result
    }

    # Grubbs test
    if ("grubbs" %in% methods) {
      grubbs_result <- detect_outliers_grubbs(x_clean, alpha)
      var_outliers$grubbs <- original_indices[grubbs_result$outlier_indices]
      method_results[[paste0(var, "_grubbs")]] <- grubbs_result
    }

    # Dixon test
    if ("dixon" %in% methods) {
      dixon_result <- detect_outliers_dixon(x_clean, alpha)
      var_outliers$dixon <- original_indices[dixon_result$outlier_indices]
      method_results[[paste0(var, "_dixon")]] <- dixon_result
    }

    # Rosner test
    if ("rosner" %in% methods) {
      rosner_result <- detect_outliers_rosner(x_clean, alpha)
      var_outliers$rosner <- original_indices[rosner_result$outlier_indices]
      method_results[[paste0(var, "_rosner")]] <- rosner_result
    }

    all_outliers[[var]] <- var_outliers
  }

  # Create outlier summary
  outlier_df <- create_outlier_dataframe(data, vars, all_outliers, consensus,
                                         min_methods, methods)

  # Summary by variable
  summary_df <- create_outlier_summary(vars, all_outliers, nrow(data), methods)

  # Consensus outliers
  consensus_outliers <- NULL
  if (consensus) {
    consensus_outliers <- outlier_df[outlier_df$N_Methods >= min_methods, ]
  }

  # Recommendations
  recommendations <- generate_outlier_recommendations(
    summary_df, consensus_outliers, methods
  )

  # Create result object
  result <- list(
    outliers = outlier_df,
    summary = summary_df,
    method_results = method_results,
    consensus_outliers = consensus_outliers,
    methods_used = methods,
    consensus = consensus,
    min_methods = min_methods,
    recommendations = recommendations
  )

  class(result) <- "descriptR_outliers"
  return(result)
}


# Detection Methods ===========================================================

#' Detect Outliers Using Z-Score
#'
#' @param x Numeric vector
#' @param threshold Z-score threshold (default 3)
#'
#' @return List with outlier information
#'
#' @keywords internal
#' @noRd
detect_outliers_zscore <- function(x, threshold = 3) {
  z_scores <- (x - mean(x)) / sd(x)
  outlier_indices <- which(abs(z_scores) > threshold)

  list(
    method = "Z-Score",
    outlier_indices = outlier_indices,
    n_outliers = length(outlier_indices),
    threshold = threshold,
    scores = z_scores
  )
}


#' Detect Outliers Using IQR Method
#'
#' @param x Numeric vector
#' @param multiplier IQR multiplier (default 1.5)
#'
#' @return List with outlier information
#'
#' @keywords internal
#' @noRd
detect_outliers_iqr <- function(x, multiplier = 1.5) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1

  lower_fence <- q1 - multiplier * iqr
  upper_fence <- q3 + multiplier * iqr

  outlier_indices <- which(x < lower_fence | x > upper_fence)

  # Also identify extreme outliers (3*IQR)
  extreme_lower <- q1 - 3 * iqr
  extreme_upper <- q3 + 3 * iqr
  extreme_indices <- which(x < extreme_lower | x > extreme_upper)

  list(
    method = "IQR",
    outlier_indices = outlier_indices,
    extreme_indices = extreme_indices,
    n_outliers = length(outlier_indices),
    n_extreme = length(extreme_indices),
    lower_fence = as.numeric(lower_fence),
    upper_fence = as.numeric(upper_fence)
  )
}


#' Detect Outliers Using MAD
#'
#' @param x Numeric vector
#' @param threshold MAD threshold (default 3)
#'
#' @return List with outlier information
#'
#' @keywords internal
#' @noRd
detect_outliers_mad <- function(x, threshold = 3) {
  median_x <- median(x)
  mad_x <- mad(x, constant = 1.4826)  # Normalized for normal distribution

  # Modified z-score
  mod_z <- abs(0.6745 * (x - median_x) / mad_x)

  outlier_indices <- which(mod_z > threshold)

  list(
    method = "MAD",
    outlier_indices = outlier_indices,
    n_outliers = length(outlier_indices),
    threshold = threshold,
    scores = mod_z
  )
}


#' Detect Outliers Using Grubbs' Test
#'
#' @param x Numeric vector
#' @param alpha Significance level
#'
#' @return List with outlier information
#'
#' @keywords internal
#' @noRd
detect_outliers_grubbs <- function(x, alpha = 0.05) {
  n <- length(x)

  if (n < 3) {
    return(list(
      method = "Grubbs",
      outlier_indices = integer(0),
      n_outliers = 0,
      note = "Sample size too small for Grubbs test (n < 3)"
    ))
  }

  outlier_indices <- integer(0)
  x_work <- x
  max_iterations <- min(floor(n / 2), 10)  # Limit iterations

  for (iter in 1:max_iterations) {
    if (length(x_work) < 3) break

    # Compute Grubbs statistic
    mean_x <- mean(x_work)
    sd_x <- sd(x_work)

    # Find most extreme value
    deviations <- abs(x_work - mean_x)
    max_dev_idx <- which.max(deviations)
    G <- max(deviations) / sd_x

    # Critical value
    n_current <- length(x_work)
    t_crit <- qt(alpha / (2 * n_current), n_current - 2)
    G_crit <- ((n_current - 1) / sqrt(n_current)) *
              sqrt(t_crit^2 / (n_current - 2 + t_crit^2))

    # Test
    if (G > G_crit) {
      # Find original index
      outlier_value <- x_work[max_dev_idx]
      orig_idx <- which(x == outlier_value)[1]  # First match
      outlier_indices <- c(outlier_indices, orig_idx)

      # Remove and continue
      x_work <- x_work[-max_dev_idx]
    } else {
      break  # No more outliers
    }
  }

  list(
    method = "Grubbs",
    outlier_indices = outlier_indices,
    n_outliers = length(outlier_indices),
    alpha = alpha
  )
}


#' Detect Outliers Using Dixon's Q Test
#'
#' @param x Numeric vector
#' @param alpha Significance level
#'
#' @return List with outlier information
#'
#' @keywords internal
#' @noRd
detect_outliers_dixon <- function(x, alpha = 0.05) {
  n <- length(x)

  if (n < 3 || n > 30) {
    return(list(
      method = "Dixon",
      outlier_indices = integer(0),
      n_outliers = 0,
      note = "Dixon test requires 3 ≤ n ≤ 30"
    ))
  }

  # Sort data
  x_sorted <- sort(x)

  # Critical values for Dixon's Q (alpha = 0.05)
  # Simplified table for common sample sizes
  q_critical <- c(
    "3" = 0.941, "4" = 0.765, "5" = 0.642, "6" = 0.560,
    "7" = 0.507, "8" = 0.468, "9" = 0.437, "10" = 0.412,
    "11" = 0.392, "12" = 0.376, "13" = 0.361, "14" = 0.349,
    "15" = 0.338, "20" = 0.300, "25" = 0.277, "30" = 0.260
  )

  # Get critical value (approximate for sizes not in table)
  q_crit <- if (as.character(n) %in% names(q_critical)) {
    q_critical[as.character(n)]
  } else {
    0.3  # Conservative approximation
  }

  outlier_indices <- integer(0)

  # Test lowest value
  if (n >= 3) {
    Q_low <- (x_sorted[2] - x_sorted[1]) / (x_sorted[n] - x_sorted[1])
    if (Q_low > q_crit) {
      outlier_indices <- c(outlier_indices, which(x == x_sorted[1])[1])
    }
  }

  # Test highest value
  if (n >= 3) {
    Q_high <- (x_sorted[n] - x_sorted[n-1]) / (x_sorted[n] - x_sorted[1])
    if (Q_high > q_crit) {
      outlier_indices <- c(outlier_indices, which(x == x_sorted[n])[1])
    }
  }

  list(
    method = "Dixon",
    outlier_indices = outlier_indices,
    n_outliers = length(outlier_indices),
    alpha = alpha
  )
}


#' Detect Outliers Using Rosner's Test
#'
#' @param x Numeric vector
#' @param alpha Significance level
#' @param max_outliers Maximum number of outliers to detect
#'
#' @return List with outlier information
#'
#' @keywords internal
#' @noRd
detect_outliers_rosner <- function(x, alpha = 0.05, max_outliers = NULL) {
  n <- length(x)

  if (n < 25) {
    return(list(
      method = "Rosner",
      outlier_indices = integer(0),
      n_outliers = 0,
      note = "Rosner test requires n ≥ 25"
    ))
  }

  # Default max outliers: up to 10% of sample or 10, whichever is smaller
  if (is.null(max_outliers)) {
    max_outliers <- min(floor(n * 0.1), 10)
  }

  outlier_indices <- integer(0)
  x_work <- x
  work_indices <- 1:n

  for (k in 1:max_outliers) {
    if (length(x_work) < 3) break

    n_current <- length(x_work)
    mean_x <- mean(x_work)
    sd_x <- sd(x_work)

    # Compute test statistic for each observation
    R <- abs(x_work - mean_x) / sd_x
    max_R_idx <- which.max(R)
    R_max <- R[max_R_idx]

    # Critical value (generalized ESD)
    p <- 1 - alpha / (2 * (n_current - k + 1))
    t_val <- qt(p, n_current - k - 1)
    lambda <- (n_current - k) * t_val /
              sqrt((n_current - k - 1 + t_val^2) * (n_current - k + 1))

    # Test
    if (R_max > lambda) {
      # Outlier detected
      orig_idx <- work_indices[max_R_idx]
      outlier_indices <- c(outlier_indices, orig_idx)

      # Remove from working set
      x_work <- x_work[-max_R_idx]
      work_indices <- work_indices[-max_R_idx]
    } else {
      break  # No more outliers
    }
  }

  list(
    method = "Rosner",
    outlier_indices = outlier_indices,
    n_outliers = length(outlier_indices),
    alpha = alpha,
    max_outliers = max_outliers
  )
}


# Helper Functions ============================================================

#' Create Outlier Data Frame
#'
#' @keywords internal
#' @noRd
create_outlier_dataframe <- function(data, vars, all_outliers, consensus,
                                     min_methods, methods) {

  outlier_list <- list()

  for (var in vars) {
    var_outliers <- all_outliers[[var]]

    if (length(var_outliers) == 0) next

    # Get all unique outlier indices for this variable
    all_indices <- unique(unlist(var_outliers))

    if (length(all_indices) == 0) next

    for (idx in all_indices) {
      # Count methods flagging this observation
      n_methods <- sum(sapply(var_outliers, function(x) idx %in% x))

      # List methods
      methods_list <- names(var_outliers)[
        sapply(var_outliers, function(x) idx %in% x)
      ]

      outlier_list[[length(outlier_list) + 1]] <- data.frame(
        Row = idx,
        Variable = var,
        Value = data[[var]][idx],
        N_Methods = n_methods,
        Methods = paste(methods_list, collapse = ", "),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(outlier_list) == 0) {
    return(data.frame(
      Row = integer(0),
      Variable = character(0),
      Value = numeric(0),
      N_Methods = integer(0),
      Methods = character(0),
      stringsAsFactors = FALSE
    ))
  }

  outlier_df <- do.call(rbind, outlier_list)
  outlier_df <- outlier_df[order(-outlier_df$N_Methods, outlier_df$Variable), ]
  rownames(outlier_df) <- NULL

  return(outlier_df)
}


#' Create Outlier Summary by Variable
#'
#' @keywords internal
#' @noRd
create_outlier_summary <- function(vars, all_outliers, n_total, methods) {

  summary_list <- list()

  for (var in vars) {
    var_outliers <- all_outliers[[var]]

    # Count unique outliers across all methods
    all_indices <- unique(unlist(var_outliers))
    n_outliers_any <- length(all_indices)

    # Count by method
    method_counts <- sapply(methods, function(m) {
      if (m %in% names(var_outliers)) {
        length(var_outliers[[m]])
      } else {
        0
      }
    })

    summary_list[[var]] <- data.frame(
      Variable = var,
      N_Total = n_total,
      N_Outliers = n_outliers_any,
      Pct_Outliers = n_outliers_any / n_total * 100,
      stringsAsFactors = FALSE
    )

    # Add method-specific counts
    for (m in methods) {
      summary_list[[var]][[paste0(m, "_count")]] <- method_counts[m]
    }
  }

  summary_df <- do.call(rbind, summary_list)
  rownames(summary_df) <- NULL

  return(summary_df)
}


#' Generate Outlier Recommendations
#'
#' @keywords internal
#' @noRd
generate_outlier_recommendations <- function(summary_df, consensus_outliers,
                                            methods) {

  recommendations <- character()

  # Overall outlier rate
  total_outliers <- sum(summary_df$N_Outliers)
  total_obs <- sum(summary_df$N_Total)
  overall_pct <- total_outliers / total_obs * 100

  if (overall_pct < 1) {
    recommendations <- c(recommendations,
      sprintf("• Low outlier rate (%.2f%%). Outliers appear to be genuine extreme values.", overall_pct))
  } else if (overall_pct < 5) {
    recommendations <- c(recommendations,
      sprintf("• Moderate outlier rate (%.2f%%). Investigate outliers for data quality issues.", overall_pct))
  } else {
    recommendations <- c(recommendations,
      sprintf("• High outlier rate (%.2f%%). Suggests potential data quality problems or non-normal distribution.", overall_pct))
  }

  # Variables with many outliers
  high_outlier_vars <- summary_df[summary_df$Pct_Outliers > 10, ]
  if (nrow(high_outlier_vars) > 0) {
    recommendations <- c(recommendations,
      sprintf("• Variables with >10%% outliers: %s",
              paste(high_outlier_vars$Variable, collapse = ", ")))
  }

  # Consensus outliers
  if (!is.null(consensus_outliers) && nrow(consensus_outliers) > 0) {
    recommendations <- c(recommendations,
      sprintf("• %d observation(s) flagged by multiple methods (high confidence outliers)",
              nrow(consensus_outliers)))
  }

  # Recommendations for handling
  recommendations <- c(recommendations,
    "",
    "Recommended actions:",
    "  1. Investigate: Check for data entry errors or measurement problems",
    "  2. Visualize: Use boxplots, scatterplots, or Q-Q plots",
    "  3. Consider transformation: Log, sqrt, or Box-Cox for skewed data",
    "  4. Robust methods: Use median-based statistics if keeping outliers",
    "  5. Sensitivity analysis: Compare results with/without outliers",
    "",
    "Handling options:",
    "  • Winsorization: Replace with nearest non-outlier value",
    "  • Transformation: Reduce impact through data transformation",
    "  • Separate analysis: Analyze outliers separately",
    "  • Removal: Only if justified (document rationale)"
  )

  return(recommendations)
}


#' Print Method for Outlier Detection
#'
#' @param x An object of class "descriptR_outliers"
#' @param max_outliers Maximum outliers to display (default 20)
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR_outliers <- function(x, max_outliers = 20, ...) {

  cat("\n")
  cat("Outlier Detection Analysis\n")
  cat(rep("=", 80), "\n", sep = "")

  cat("\nMethods used:", paste(x$methods_used, collapse = ", "), "\n")

  if (x$consensus) {
    cat(sprintf("Consensus mode: Flagged by ≥%d methods\n", x$min_methods))
  }

  # Summary
  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nSummary by Variable:\n\n")
  print(x$summary, row.names = FALSE, digits = 2)

  # Outliers
  cat("\n")
  cat(rep("-", 80), "\n", sep = "")

  if (nrow(x$outliers) == 0) {
    cat("\nNo outliers detected.\n")
  } else {
    cat(sprintf("\nDetected Outliers (showing top %d):\n\n", max_outliers))

    outliers_to_show <- head(x$outliers, max_outliers)
    print(outliers_to_show, row.names = FALSE, digits = 3)

    if (nrow(x$outliers) > max_outliers) {
      cat(sprintf("\n... and %d more outliers\n",
                 nrow(x$outliers) - max_outliers))
    }
  }

  # Consensus outliers
  if (!is.null(x$consensus_outliers) && nrow(x$consensus_outliers) > 0) {
    cat("\n")
    cat(rep("-", 80), "\n", sep = "")
    cat("\nHigh-Confidence Outliers (flagged by multiple methods):\n\n")
    print(x$consensus_outliers, row.names = FALSE, digits = 3)
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


#' Summary Method for Outlier Detection
#'
#' @param object An object of class "descriptR_outliers"
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.descriptR_outliers <- function(object, ...) {

  cat("\n")
  cat("Outlier Detection Summary\n")
  cat(rep("=", 70), "\n", sep = "")

  total_outliers <- nrow(object$outliers)
  total_obs <- sum(object$summary$N_Total)

  cat(sprintf("\nTotal observations: %d\n", total_obs))
  cat(sprintf("Total outliers detected: %d (%.2f%%)\n",
             total_outliers, total_outliers / total_obs * 100))
  cat(sprintf("Methods used: %s\n", paste(object$methods_used, collapse = ", ")))

  if (object$consensus && !is.null(object$consensus_outliers)) {
    cat(sprintf("High-confidence outliers: %d\n",
               nrow(object$consensus_outliers)))
  }

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  invisible(object)
}
