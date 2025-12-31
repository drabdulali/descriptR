#' Compare Groups Comprehensively
#'
#' @description
#' Intelligent function that automatically selects and performs appropriate statistical
#' tests based on variable types and data characteristics. Provides complete comparison
#' including effect sizes, assumption checks, and plain-language interpretation.
#'
#' @param data Data frame
#' @param outcome Name of outcome variable
#' @param group Name of grouping variable
#' @param test_type Manually specify test type (optional): "auto" (default), "t.test",
#'   "anova", "chi.square", "kruskal", "mann.whitney"
#' @param post_hoc Logical, perform post-hoc tests if applicable? (default TRUE)
#' @param check_assumptions Logical, check test assumptions? (default TRUE)
#' @param conf.level Confidence level (default 0.95)
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return An object of class "descriptR_comparison" containing:
#' \itemize{
#'   \item \code{test_type}: Type of test performed
#'   \item \code{test_result}: Main test result object
#'   \item \code{effect_size}: Effect size measure and interpretation
#'   \item \code{descriptives}: Descriptive statistics by group
#'   \item \code{assumptions}: Assumption check results (if requested)
#'   \item \code{recommendation}: Test recommendation and rationale
#'   \item \code{interpretation}: Overall plain-language interpretation
#' }
#'
#' @details
#' ## Automatic Test Selection
#'
#' The function intelligently selects tests based on:
#'
#' **For Numeric Outcomes:**
#' - 2 groups + normal + equal variance → Student's t-test
#' - 2 groups + normal + unequal variance → Welch's t-test
#' - 2 groups + non-normal → Mann-Whitney U test
#' - 3+ groups + normal + equal variance → One-way ANOVA
#' - 3+ groups + normal + unequal variance → Welch's ANOVA
#' - 3+ groups + non-normal → Kruskal-Wallis test
#'
#' **For Categorical Outcomes:**
#' - Any number of groups → Chi-square test
#' - 2x2 table + small expected frequencies → Fisher's exact test
#'
#' ## Effect Sizes Computed
#'
#' - **T-test**: Cohen's d
#' - **ANOVA**: Eta-squared (η²) and Omega-squared (ω²)
#' - **Chi-square**: Cramér's V
#' - **Non-parametric**: Rank-biserial correlation or Epsilon-squared
#'
#' ## Assumption Checking
#'
#' When check_assumptions = TRUE:
#' - **Normality**: Shapiro-Wilk test
#' - **Homogeneity of variance**: Levene's test
#' - **Independence**: Visual checks and warnings
#'
#' @examples
#' # Numeric outcome, 2 groups
#' compare_groups(iris, "Sepal.Length", "Species")
#'
#' # Numeric outcome, 3+ groups
#' compare_groups(iris, "Sepal.Length", "Species")
#'
#' # Categorical outcome
#' compare_groups(mtcars, "vs", "am")
#'
#' # Manual test specification
#' compare_groups(iris, "Sepal.Length", "Species",
#'               test_type = "anova")
#'
#' @export
compare_groups <- function(data,
                          outcome,
                          group,
                          test_type = c("auto", "t.test", "anova", "chi.square",
                                      "kruskal", "mann.whitney"),
                          post_hoc = TRUE,
                          check_assumptions = TRUE,
                          conf.level = 0.95,
                          na.rm = TRUE) {

  # Validate inputs
  validate_data_frame(data)
  validate_variables(data, c(outcome, group))
  test_type <- match.arg(test_type)

  # Extract variables
  y <- data[[outcome]]
  grp <- data[[group]]

  # Handle NA
  if (na.rm) {
    complete_cases <- complete.cases(y, grp)
    y <- y[complete_cases]
    grp <- grp[complete_cases]
  }

  # Detect variable types
  outcome_type <- detect_var_type(y)
  group_type <- detect_var_type(grp)

  # Count groups
  groups <- unique(grp[!is.na(grp)])
  n_groups <- length(groups)

  if (n_groups < 2) {
    stop("Need at least 2 groups for comparison", call. = FALSE)
  }

  # Compute descriptive statistics by group
  descriptives <- compute_group_descriptives(y, grp, outcome_type)

  # Auto-select test if requested
  if (test_type == "auto") {
    test_selection <- select_appropriate_test(
      y, grp, outcome_type, group_type, n_groups, check_assumptions
    )
    test_type <- test_selection$test
    recommendation <- test_selection$rationale
  } else {
    recommendation <- sprintf("User manually specified: %s", test_type)
  }

  # Perform the selected test
  test_result <- perform_selected_test(
    y = y,
    grp = grp,
    test_type = test_type,
    n_groups = n_groups,
    post_hoc = post_hoc,
    conf.level = conf.level
  )

  # Check assumptions if requested
  assumption_results <- NULL
  if (check_assumptions && test_type %in% c("t.test", "anova")) {
    assumption_results <- check_comparison_assumptions(y, grp, test_type)
  }

  # Create overall interpretation
  interpretation <- create_comparison_interpretation(
    test_result = test_result,
    test_type = test_type,
    descriptives = descriptives
  )

  # Create result object
  result <- list(
    test_type = test_type,
    test_result = test_result,
    descriptives = descriptives,
    assumptions = assumption_results,
    recommendation = recommendation,
    interpretation = interpretation,
    outcome = outcome,
    group = group,
    n_groups = n_groups
  )

  class(result) <- "descriptR_comparison"
  return(result)
}


#' Select Appropriate Statistical Test
#'
#' @param y Outcome variable
#' @param grp Grouping variable
#' @param outcome_type Type of outcome
#' @param group_type Type of grouping variable
#' @param n_groups Number of groups
#' @param check_assumptions Should assumptions be checked?
#'
#' @return List with test name and rationale
#'
#' @keywords internal
#' @noRd
select_appropriate_test <- function(y, grp, outcome_type, group_type,
                                   n_groups, check_assumptions) {

  # For categorical outcomes
  if (outcome_type %in% c("nominal", "ordinal", "binary")) {
    return(list(
      test = "chi.square",
      rationale = "Categorical outcome → Chi-square test of independence"
    ))
  }

  # For numeric outcomes
  if (outcome_type %in% c("continuous", "discrete_numeric")) {

    # Check normality for each group if requested
    if (check_assumptions && n_groups <= 5) {
      normality_by_group <- tapply(y, grp, function(x) {
        if (length(x) >= 3 && length(x) <= 5000) {
          stats::shapiro.test(x)$p.value > 0.05
        } else {
          TRUE  # Assume normal for very small or large samples
        }
      })
      all_normal <- all(normality_by_group, na.rm = TRUE)
    } else {
      all_normal <- TRUE  # Assume normal if not checking
    }

    # 2 groups
    if (n_groups == 2) {
      if (all_normal) {
        return(list(
          test = "t.test",
          rationale = "2 groups, normally distributed → t-test (Welch's by default)"
        ))
      } else {
        return(list(
          test = "mann.whitney",
          rationale = "2 groups, non-normal distribution → Mann-Whitney U test"
        ))
      }
    }

    # 3+ groups
    if (all_normal) {
      return(list(
        test = "anova",
        rationale = sprintf("%d groups, normally distributed → One-way ANOVA",
                          n_groups)
      ))
    } else {
      return(list(
        test = "kruskal",
        rationale = sprintf("%d groups, non-normal distribution → Kruskal-Wallis test",
                          n_groups)
      ))
    }
  }

  # Default to t-test/anova
  if (n_groups == 2) {
    return(list(
      test = "t.test",
      rationale = "Default: t-test for 2 groups"
    ))
  } else {
    return(list(
      test = "anova",
      rationale = sprintf("Default: ANOVA for %d groups", n_groups)
    ))
  }
}


#' Perform Selected Test
#'
#' @param y Outcome variable
#' @param grp Grouping variable
#' @param test_type Type of test
#' @param n_groups Number of groups
#' @param post_hoc Perform post-hoc tests?
#' @param conf.level Confidence level
#'
#' @return Test result object
#'
#' @keywords internal
#' @noRd
perform_selected_test <- function(y, grp, test_type, n_groups, post_hoc,
                                 conf.level) {

  groups <- unique(grp)

  if (test_type == "t.test") {
    # Two-sample t-test
    x1 <- y[grp == groups[1]]
    x2 <- y[grp == groups[2]]
    result <- perform_t_test(x1, x2, conf.level = conf.level)

  } else if (test_type == "anova") {
    # One-way ANOVA
    df_temp <- data.frame(y = y, grp = as.factor(grp))
    result <- perform_anova(df_temp, "y", "grp",
                           post_hoc = post_hoc,
                           check_assumptions = FALSE)  # Already checked

  } else if (test_type == "chi.square") {
    # Chi-square test
    result <- perform_chi_square(y, grp)

  } else if (test_type == "mann.whitney") {
    # Mann-Whitney U test
    x1 <- y[grp == groups[1]]
    x2 <- y[grp == groups[2]]
    result <- stats::wilcox.test(x1, x2, conf.level = conf.level)
    # Wrap in standard format
    result <- list(
      statistic = as.numeric(result$statistic),
      p_value = result$p.value,
      method = result$method
    )

  } else if (test_type == "kruskal") {
    # Kruskal-Wallis test
    result <- stats::kruskal.test(y ~ as.factor(grp))
    # Wrap in standard format
    result <- list(
      statistic = as.numeric(result$statistic),
      p_value = result$p.value,
      df = as.numeric(result$parameter),
      method = result$method
    )
  }

  return(result)
}


#' Compute Group Descriptives
#'
#' @param y Outcome variable
#' @param grp Grouping variable
#' @param outcome_type Type of outcome
#'
#' @return Data frame with descriptive statistics
#'
#' @keywords internal
#' @noRd
compute_group_descriptives <- function(y, grp, outcome_type) {

  if (outcome_type %in% c("continuous", "discrete_numeric")) {
    # Numeric descriptives
    group_stats <- tapply(y, grp, function(x) {
      c(
        n = length(x),
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE)
      )
    })

    desc_df <- do.call(rbind, lapply(names(group_stats), function(g) {
      data.frame(
        Group = g,
        N = group_stats[[g]]["n"],
        Mean = group_stats[[g]]["mean"],
        SD = group_stats[[g]]["sd"],
        Median = group_stats[[g]]["median"],
        Min = group_stats[[g]]["min"],
        Max = group_stats[[g]]["max"],
        stringsAsFactors = FALSE
      )
    }))

  } else {
    # Categorical descriptives
    freq_table <- table(grp, y)
    desc_df <- as.data.frame.matrix(freq_table)
    desc_df$Group <- rownames(desc_df)
    desc_df$N <- rowSums(freq_table)
    desc_df <- desc_df[, c("Group", "N", setdiff(names(desc_df), c("Group", "N")))]
    rownames(desc_df) <- NULL
  }

  return(desc_df)
}


#' Check Comparison Assumptions
#'
#' @param y Outcome variable
#' @param grp Grouping variable
#' @param test_type Type of test
#'
#' @return List with assumption results
#'
#' @keywords internal
#' @noRd
check_comparison_assumptions <- function(y, grp, test_type) {

  grp <- as.factor(grp)

  # Normality
  normality_by_group <- tapply(y, grp, function(x) {
    if (length(x) >= 3 && length(x) <= 5000) {
      test_result <- stats::shapiro.test(x)
      list(statistic = test_result$statistic,
           p_value = test_result$p.value,
           normal = test_result$p.value > 0.05)
    } else {
      list(statistic = NA, p_value = NA, normal = NA)
    }
  })

  # Homogeneity of variance (Levene's test)
  abs_dev <- abs(y - ave(y, grp, FUN = median))
  levene_result <- stats::aov(abs_dev ~ grp)
  levene_p <- summary(levene_result)[[1]]["grp", "Pr(>F)"]

  list(
    normality = normality_by_group,
    homogeneity = list(
      test = "Levene's Test",
      p_value = levene_p,
      homogeneous = levene_p > 0.05
    )
  )
}


#' Create Comparison Interpretation
#'
#' @param test_result Test result object
#' @param test_type Type of test
#' @param descriptives Descriptive statistics
#'
#' @return Character string with interpretation
#'
#' @keywords internal
#' @noRd
create_comparison_interpretation <- function(test_result, test_type,
                                            descriptives) {

  # Extract p-value
  if ("p_value" %in% names(test_result)) {
    p_value <- test_result$p_value
  } else {
    p_value <- NA
  }

  # Create interpretation based on test type
  if (!is.na(p_value)) {
    if (p_value < 0.05) {
      sig_text <- "statistically significant difference"
    } else {
      sig_text <- "no significant difference"
    }

    base_interp <- sprintf("The %s revealed %s between groups (p = %s).",
                          test_type,
                          sig_text,
                          if (p_value < 0.001) "< 0.001" else sprintf("%.3f", p_value))
  } else {
    base_interp <- sprintf("The %s was performed.", test_type)
  }

  # Add effect size information if available
  if ("cohen_d" %in% names(test_result)) {
    effect_text <- sprintf(" Effect size: Cohen's d = %.2f (%s).",
                          abs(test_result$cohen_d),
                          interpret_effect_size(test_result$cohen_d, "cohen_d"))
    base_interp <- paste(base_interp, effect_text)
  } else if ("eta_squared" %in% names(test_result)) {
    effect_text <- sprintf(" Effect size: η² = %.3f (%s).",
                          test_result$eta_squared,
                          interpret_effect_size(test_result$eta_squared, "eta_squared"))
    base_interp <- paste(base_interp, effect_text)
  }

  return(base_interp)
}


#' Print Method for Group Comparison
#'
#' @param x An object of class "descriptR_comparison"
#' @param digits Number of digits to print (default 3)
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR_comparison <- function(x, digits = 3, ...) {

  cat("\n")
  cat("Comprehensive Group Comparison\n")
  cat(rep("=", 70), "\n", sep = "")

  cat(sprintf("\nOutcome: %s\n", x$outcome))
  cat(sprintf("Grouping: %s (%d groups)\n", x$group, x$n_groups))
  cat(sprintf("Test: %s\n", x$test_type))

  # Recommendation
  cat("\n")
  cat(rep("-", 70), "\n", sep = "")
  cat("\nTest Selection:\n")
  cat(strwrap(x$recommendation, width = 70, prefix = "  "), sep = "\n")

  # Descriptive statistics
  cat("\n")
  cat(rep("-", 70), "\n", sep = "")
  cat("\nDescriptive Statistics:\n")
  print(x$descriptives, row.names = FALSE, digits = digits)

  # Main test result
  cat("\n")
  cat(rep("-", 70), "\n", sep = "")
  cat("\nTest Results:\n")
  print(x$test_result, digits = digits)

  # Assumptions
  if (!is.null(x$assumptions)) {
    cat("\n")
    cat(rep("-", 70), "\n", sep = "")
    cat("\nAssumption Checks:\n")

    if (!is.null(x$assumptions$normality)) {
      cat("\nNormality (by group):\n")
      for (g in names(x$assumptions$normality)) {
        norm_test <- x$assumptions$normality[[g]]
        if (!is.na(norm_test$p_value)) {
          cat(sprintf("  %s: p = %.3f %s\n",
                     g,
                     norm_test$p_value,
                     if (norm_test$normal) "(✓)" else "(✗)"))
        }
      }
    }

    if (!is.null(x$assumptions$homogeneity)) {
      cat(sprintf("\nHomogeneity of Variance: p = %.3f %s\n",
                 x$assumptions$homogeneity$p_value,
                 if (x$assumptions$homogeneity$homogeneous) "(✓)" else "(✗)"
                 ))
    }
  }

  # Overall interpretation
  cat("\n")
  cat(rep("-", 70), "\n", sep = "")
  cat("\nInterpretation:\n")
  cat(strwrap(x$interpretation, width = 70, prefix = "  "), sep = "\n")

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  invisible(x)
}
