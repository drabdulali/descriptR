#' Describe Data by Groups
#'
#' @description
#' Computes descriptive statistics separately for each group and optionally
#' performs statistical tests to compare groups. Combines describe_data()
#' with compare_groups() for comprehensive grouped analysis.
#'
#' @param data Data frame
#' @param vars Character vector of variable names to analyze. If NULL (default), all variables.
#' @param group_by Name of grouping variable
#' @param include_tests Logical, perform statistical tests comparing groups? (default TRUE)
#' @param include_effect_sizes Logical, compute effect sizes? (default TRUE)
#' @param post_hoc Logical, perform post-hoc tests when applicable? (default TRUE)
#' @param check_assumptions Logical, check statistical assumptions? (default TRUE)
#' @param conf.level Confidence level (default 0.95)
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return An object of class "descriptR_grouped" containing:
#' \itemize{
#'   \item \code{descriptives_by_group}: List of descriptive statistics for each group
#'   \item \code{combined_stats}: Data frame with all statistics combined
#'   \item \code{test_results}: List of test results (if include_tests = TRUE)
#'   \item \code{group_summary}: Summary information about groups
#'   \item \code{n_groups}: Number of groups
#'   \item \code{n_vars}: Number of variables analyzed
#' }
#'
#' @details
#' ## Analysis Workflow
#'
#' 1. **Split data by groups**
#' 2. **Compute descriptive statistics** for each group separately
#' 3. **Combine statistics** into comparison table
#' 4. **Perform statistical tests** (if requested):
#'    - Numeric variables: t-test (2 groups) or ANOVA (3+ groups)
#'    - Categorical variables: Chi-square test
#' 5. **Compute effect sizes** (if requested)
#' 6. **Check assumptions** (if requested)
#'
#' ## Statistics Computed
#'
#' For numeric variables (by group):
#' - N, Mean, SD, Median, IQR, Min, Max
#' - Skewness, Kurtosis (if sufficient data)
#'
#' For categorical variables (by group):
#' - Frequencies and percentages
#' - Mode and mode percentage
#'
#' ## Statistical Tests
#'
#' Automatically selected based on:
#' - Variable type (numeric vs categorical)
#' - Number of groups (2 vs 3+)
#' - Distribution characteristics
#'
#' @examples
#' # Basic grouped description
#' describe_grouped(iris, group_by = "Species")
#'
#' # Specific variables only
#' describe_grouped(iris,
#'                 vars = c("Sepal.Length", "Sepal.Width"),
#'                 group_by = "Species")
#'
#' # Without statistical tests
#' describe_grouped(mtcars, group_by = "cyl", include_tests = FALSE)
#'
#' # With assumption checking
#' describe_grouped(iris,
#'                 vars = "Sepal.Length",
#'                 group_by = "Species",
#'                 check_assumptions = TRUE)
#'
#' @export
describe_grouped <- function(data,
                            vars = NULL,
                            group_by,
                            include_tests = TRUE,
                            include_effect_sizes = TRUE,
                            post_hoc = TRUE,
                            check_assumptions = TRUE,
                            conf.level = 0.95,
                            na.rm = TRUE) {

  # Validate inputs
  validate_data_frame(data)
  validate_group_var(data, group_by, allow_null = FALSE)

  # Select variables
  if (is.null(vars)) {
    vars <- setdiff(names(data), group_by)
  } else {
    validate_variables(data, vars)
  }

  # Get group information
  grp <- data[[group_by]]
  groups <- unique(grp[!is.na(grp)])
  n_groups <- length(groups)

  # Compute descriptive statistics for each group
  descriptives_by_group <- lapply(groups, function(g) {
    group_data <- data[grp == g, vars, drop = FALSE]
    describe_data(group_data,
                 include_insights = FALSE,
                 include_plots = FALSE,
                 verbose = FALSE)
  })
  names(descriptives_by_group) <- groups

  # Combine statistics into comparison table
  combined_stats <- combine_group_statistics(descriptives_by_group, groups, vars)

  # Perform statistical tests if requested
  test_results <- NULL
  if (include_tests) {
    test_results <- lapply(vars, function(var) {
      tryCatch({
        compare_groups(data,
                      outcome = var,
                      group = group_by,
                      post_hoc = post_hoc,
                      check_assumptions = check_assumptions,
                      conf.level = conf.level,
                      na.rm = na.rm)
      }, error = function(e) {
        list(error = e$message, variable = var)
      })
    })
    names(test_results) <- vars
  }

  # Create group summary
  group_summary <- data.frame(
    Group = groups,
    N = sapply(groups, function(g) sum(grp == g, na.rm = TRUE)),
    Percentage = sapply(groups, function(g) {
      sum(grp == g, na.rm = TRUE) / sum(!is.na(grp)) * 100
    }),
    stringsAsFactors = FALSE
  )

  # Create result object
  result <- list(
    descriptives_by_group = descriptives_by_group,
    combined_stats = combined_stats,
    test_results = test_results,
    group_summary = group_summary,
    group_by = group_by,
    n_groups = n_groups,
    n_vars = length(vars),
    vars = vars
  )

  class(result) <- "descriptR_grouped"
  return(result)
}


#' Combine Group Statistics
#'
#' @param descriptives_by_group List of descriptR objects
#' @param groups Group names
#' @param vars Variable names
#'
#' @return Data frame with combined statistics
#'
#' @keywords internal
#' @noRd
combine_group_statistics <- function(descriptives_by_group, groups, vars) {

  # Initialize combined data frame
  combined_list <- list()

  for (var in vars) {
    # Get statistics for this variable across all groups
    var_stats_list <- list()

    for (g in groups) {
      desc_obj <- descriptives_by_group[[g]]
      var_row <- desc_obj$statistics[desc_obj$statistics$Variable == var, ]

      if (nrow(var_row) > 0) {
        var_stats <- as.list(var_row)
        var_stats$Group <- g
        var_stats_list[[g]] <- var_stats
      }
    }

    # Combine into data frame for this variable
    if (length(var_stats_list) > 0) {
      var_df <- do.call(rbind, lapply(var_stats_list, function(x) {
        data.frame(x, stringsAsFactors = FALSE)
      }))
      var_df$Variable <- var
      combined_list[[var]] <- var_df
    }
  }

  # Combine all variables
  if (length(combined_list) > 0) {
    combined_df <- do.call(rbind, combined_list)
    rownames(combined_df) <- NULL

    # Reorder columns: Variable, Group, then statistics
    col_order <- c("Variable", "Group",
                  setdiff(names(combined_df), c("Variable", "Group")))
    combined_df <- combined_df[, col_order[col_order %in% names(combined_df)]]

    return(combined_df)
  } else {
    return(data.frame())
  }
}


#' Print Method for Grouped Description
#'
#' @param x An object of class "descriptR_grouped"
#' @param digits Number of digits to print (default 2)
#' @param show_tests Logical, show test results? (default TRUE)
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR_grouped <- function(x, digits = 2, show_tests = TRUE, ...) {

  cat("\n")
  cat("Grouped Descriptive Statistics\n")
  cat(rep("=", 80), "\n", sep = "")

  # Summary information
  cat(sprintf("\nGrouping variable: %s (%d groups)\n", x$group_by, x$n_groups))
  cat(sprintf("Variables analyzed: %d\n\n", x$n_vars))

  # Group summary
  cat("Group Sizes:\n")
  print(x$group_summary, row.names = FALSE, digits = digits)

  # Combined statistics
  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nDescriptive Statistics by Group:\n\n")

  # Print statistics for each variable
  for (var in x$vars) {
    var_stats <- x$combined_stats[x$combined_stats$Variable == var, ]

    if (nrow(var_stats) > 0) {
      cat(sprintf("Variable: %s\n", var))

      # Select relevant columns based on variable type
      if ("Mean" %in% names(var_stats)) {
        # Numeric variable
        display_cols <- c("Group", "N", "Mean", "SD", "Median", "Min", "Max")
        display_cols <- display_cols[display_cols %in% names(var_stats)]
      } else {
        # Categorical variable
        display_cols <- c("Group", "N", "N_Unique", "Mode", "Mode_Freq", "Mode_Pct")
        display_cols <- display_cols[display_cols %in% names(var_stats)]
      }

      print_df <- var_stats[, display_cols, drop = FALSE]
      print(print_df, row.names = FALSE, digits = digits)
      cat("\n")
    }
  }

  # Test results
  if (show_tests && !is.null(x$test_results)) {
    cat(rep("-", 80), "\n", sep = "")
    cat("\nStatistical Tests:\n\n")

    for (var in names(x$test_results)) {
      test_result <- x$test_results[[var]]

      if ("error" %in% names(test_result)) {
        cat(sprintf("%s: Test failed - %s\n", var, test_result$error))
      } else {
        cat(sprintf("%s:\n", var))

        # Extract key information
        if ("p_value" %in% names(test_result$test_result)) {
          p_val <- test_result$test_result$p_value
          cat(sprintf("  Test: %s\n", test_result$test_type))
          cat(sprintf("  p-value: %s\n",
                     if (p_val < 0.001) "< 0.001" else sprintf("%.4f", p_val)))

          # Effect size
          if ("cohen_d" %in% names(test_result$test_result)) {
            cat(sprintf("  Cohen's d: %.3f (%s)\n",
                       abs(test_result$test_result$cohen_d),
                       interpret_effect_size(test_result$test_result$cohen_d, "cohen_d")))
          } else if ("eta_squared" %in% names(test_result$test_result)) {
            cat(sprintf("  Eta-squared: %.3f (%s)\n",
                       test_result$test_result$eta_squared,
                       interpret_effect_size(test_result$test_result$eta_squared,
                                           "eta_squared")))
          } else if ("cramers_v" %in% names(test_result$test_result)) {
            cat(sprintf("  Cramér's V: %.3f (%s)\n",
                       test_result$test_result$cramers_v,
                       interpret_effect_size(test_result$test_result$cramers_v,
                                           "cramers_v")))
          }

          # Significance
          if (p_val < 0.05) {
            cat("  Result: Significant difference between groups ✓\n")
          } else {
            cat("  Result: No significant difference\n")
          }
        }

        cat("\n")
      }
    }
  }

  cat(rep("=", 80), "\n", sep = "")
  cat("\n")

  invisible(x)
}


#' Summary Method for Grouped Description
#'
#' @param object An object of class "descriptR_grouped"
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.descriptR_grouped <- function(object, ...) {

  cat("\n")
  cat("Summary of Grouped Analysis\n")
  cat(rep("=", 70), "\n", sep = "")

  cat(sprintf("\nGrouping: %s (%d groups)\n", object$group_by, object$n_groups))
  cat(sprintf("Variables: %d analyzed\n", object$n_vars))

  # Count significant results
  if (!is.null(object$test_results)) {
    n_sig <- sum(sapply(object$test_results, function(test) {
      if ("test_result" %in% names(test) && "p_value" %in% names(test$test_result)) {
        test$test_result$p_value < 0.05
      } else {
        FALSE
      }
    }))

    cat(sprintf("\nSignificant differences: %d out of %d variables (%.1f%%)\n",
               n_sig, object$n_vars, n_sig / object$n_vars * 100))

    # List significant variables
    if (n_sig > 0) {
      sig_vars <- names(object$test_results)[sapply(object$test_results, function(test) {
        if ("test_result" %in% names(test) && "p_value" %in% names(test$test_result)) {
          test$test_result$p_value < 0.05
        } else {
          FALSE
        }
      })]

      cat("\nVariables with significant group differences:\n")
      for (var in sig_vars) {
        cat(sprintf("  - %s\n", var))
      }
    }
  }

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  invisible(object)
}
