#' Perform One-Way Analysis of Variance (ANOVA)
#'
#' @description
#' Performs one-way ANOVA to compare means across three or more groups.
#' Includes effect size (eta-squared), post-hoc tests, and assumption checking.
#'
#' @param data Data frame
#' @param outcome Name of outcome variable (numeric)
#' @param group Name of grouping variable
#' @param post_hoc Logical, perform post-hoc tests? (default TRUE)
#' @param post_hoc_method Method for post-hoc comparisons: "tukey", "bonferroni", "none"
#' @param check_assumptions Logical, check ANOVA assumptions? (default TRUE)
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return An object of class "descriptR_anova" containing:
#' \itemize{
#'   \item \code{f_statistic}: F-statistic value
#'   \item \code{p_value}: P-value
#'   \item \code{df_between}: Between-groups degrees of freedom
#'   \item \code{df_within}: Within-groups degrees of freedom
#'   \item \code{eta_squared}: Effect size (proportion of variance explained)
#'   \item \code{omega_squared}: Unbiased effect size estimate
#'   \item \code{group_means}: Mean and SD for each group
#'   \item \code{post_hoc}: Post-hoc test results (if requested)
#'   \item \code{assumptions}: Assumption checking results (if requested)
#'   \item \code{interpretation}: Plain-language interpretation
#' }
#'
#' @details
#' ## ANOVA Assumptions
#'
#' 1. **Independence**: Observations are independent
#' 2. **Normality**: Residuals are normally distributed (checked with Shapiro-Wilk)
#' 3. **Homogeneity of variance**: Equal variances across groups (Levene's test)
#'
#' ## Effect Sizes
#'
#' **Eta-squared (η²)**: Proportion of total variance explained by group
#' - Small: 0.01
#' - Medium: 0.06
#' - Large: 0.14
#'
#' **Omega-squared (ω²)**: Less biased estimate (recommended for small samples)
#'
#' ## Post-Hoc Tests
#'
#' When ANOVA is significant, post-hoc tests identify which groups differ:
#' - **Tukey HSD**: Controls family-wise error rate, all pairwise comparisons
#' - **Bonferroni**: More conservative, recommended for planned comparisons
#'
#' @examples
#' # Basic ANOVA
#' perform_anova(iris, "Sepal.Length", "Species")
#'
#' # Without post-hoc tests
#' perform_anova(iris, "Sepal.Length", "Species", post_hoc = FALSE)
#'
#' # With Bonferroni correction
#' perform_anova(iris, "Sepal.Length", "Species", post_hoc_method = "bonferroni")
#'
#' @export
perform_anova <- function(data,
                         outcome,
                         group,
                         post_hoc = TRUE,
                         post_hoc_method = c("tukey", "bonferroni", "none"),
                         check_assumptions = TRUE,
                         na.rm = TRUE) {

  # Validate inputs
  validate_data_frame(data)
  validate_variables(data, c(outcome, group))
  post_hoc_method <- match.arg(post_hoc_method)

  # Extract variables
  y <- data[[outcome]]
  grp <- factor(data[[group]])

  # Check numeric
  if (!is.numeric(y)) {
    stop(sprintf("`%s` must be numeric", outcome), call. = FALSE)
  }

  # Handle NA
  if (na.rm) {
    complete_cases <- complete.cases(y, grp)
    y <- y[complete_cases]
    grp <- grp[complete_cases]
  }

  # Check minimum groups
  n_groups <- length(unique(grp))
  if (n_groups < 2) {
    stop("Need at least 2 groups for ANOVA", call. = FALSE)
  }

  # Perform ANOVA
  aov_model <- stats::aov(y ~ grp)
  aov_summary <- summary(aov_model)

  # Extract results
  f_stat <- aov_summary[[1]]["grp", "F value"]
  p_value <- aov_summary[[1]]["grp", "Pr(>F)"]
  df_between <- aov_summary[[1]]["grp", "Df"]
  df_within <- aov_summary[[1]]["Residuals", "Df"]

  # Compute effect sizes
  ss_between <- aov_summary[[1]]["grp", "Sum Sq"]
  ss_within <- aov_summary[[1]]["Residuals", "Sum Sq"]
  ss_total <- ss_between + ss_within
  n_total <- length(y)

  # Eta-squared
  eta_squared <- ss_between / ss_total

  # Omega-squared (less biased)
  ms_within <- aov_summary[[1]]["Residuals", "Mean Sq"]
  omega_squared <- (ss_between - df_between * ms_within) /
                  (ss_total + ms_within)

  # Group descriptives
  group_stats <- tapply(y, grp, function(x) {
    c(mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      n = length(x))
  })

  group_means <- do.call(rbind, lapply(names(group_stats), function(g) {
    data.frame(
      Group = g,
      N = group_stats[[g]]["n"],
      Mean = group_stats[[g]]["mean"],
      SD = group_stats[[g]]["sd"],
      stringsAsFactors = FALSE
    )
  }))

  # Post-hoc tests
  post_hoc_results <- NULL
  if (post_hoc && post_hoc_method != "none" && p_value < 0.05) {

    if (post_hoc_method == "tukey") {
      # Tukey HSD
      tukey_result <- stats::TukeyHSD(aov_model)
      post_hoc_results <- as.data.frame(tukey_result$grp)
      post_hoc_results$Comparison <- rownames(post_hoc_results)
      rownames(post_hoc_results) <- NULL
      post_hoc_results <- post_hoc_results[, c("Comparison", "diff", "lwr",
                                               "upr", "p adj")]
      names(post_hoc_results) <- c("Comparison", "Mean_Diff", "CI_Lower",
                                   "CI_Upper", "P_Value")

    } else if (post_hoc_method == "bonferroni") {
      # Pairwise t-tests with Bonferroni correction
      pairwise_result <- stats::pairwise.t.test(y, grp,
                                               p.adjust.method = "bonferroni")
      # Convert to data frame
      p_matrix <- pairwise_result$p.value
      comparisons <- which(!is.na(p_matrix), arr.ind = TRUE)

      post_hoc_results <- data.frame(
        Comparison = sprintf("%s - %s",
                           rownames(p_matrix)[comparisons[, 1]],
                           colnames(p_matrix)[comparisons[, 2]]),
        P_Value = p_matrix[comparisons],
        stringsAsFactors = FALSE
      )
    }
  }

  # Check assumptions
  assumption_results <- NULL
  if (check_assumptions) {
    assumption_results <- check_anova_assumptions(aov_model, y, grp)
  }

  # Create interpretation
  interpretation <- create_anova_interpretation(
    p_value = p_value,
    eta_squared = eta_squared,
    n_groups = n_groups,
    post_hoc_sig = if (!is.null(post_hoc_results)) {
      sum(post_hoc_results$P_Value < 0.05)
    } else NULL
  )

  # Create result object
  result <- list(
    f_statistic = f_stat,
    p_value = p_value,
    df_between = df_between,
    df_within = df_within,
    eta_squared = eta_squared,
    omega_squared = omega_squared,
    group_means = group_means,
    post_hoc = post_hoc_results,
    assumptions = assumption_results,
    interpretation = interpretation,
    outcome = outcome,
    group = group
  )

  class(result) <- "descriptR_anova"
  return(result)
}


#' Check ANOVA Assumptions
#'
#' @param aov_model AOV model object
#' @param y Outcome variable
#' @param grp Grouping variable
#'
#' @return List with assumption test results
#'
#' @keywords internal
#' @noRd
check_anova_assumptions <- function(aov_model, y, grp) {

  # 1. Normality of residuals (Shapiro-Wilk)
  residuals <- stats::residuals(aov_model)
  if (length(residuals) >= 3 && length(residuals) <= 5000) {
    shapiro_test <- stats::shapiro.test(residuals)
    normality_p <- shapiro_test$p.value
    normality_met <- normality_p > 0.05
  } else {
    normality_p <- NA
    normality_met <- NA
  }

  # 2. Homogeneity of variance (Levene's test using car::leveneTest or manual)
  # Manual Levene's test (median-based)
  group_levels <- unique(grp)
  abs_dev <- abs(y - ave(y, grp, FUN = median))
  levene_aov <- stats::aov(abs_dev ~ grp)
  levene_summary <- summary(levene_aov)
  levene_p <- levene_summary[[1]]["grp", "Pr(>F)"]
  variance_met <- levene_p > 0.05

  list(
    normality = list(
      test = "Shapiro-Wilk",
      statistic = if (!is.na(normality_p)) shapiro_test$statistic else NA,
      p_value = normality_p,
      assumption_met = normality_met,
      interpretation = if (is.na(normality_met)) {
        "Cannot test (sample size)"
      } else if (normality_met) {
        "Normality assumption met (p > 0.05)"
      } else {
        "Normality assumption violated (p < 0.05) - consider non-parametric test"
      }
    ),
    homogeneity = list(
      test = "Levene's Test (median)",
      p_value = levene_p,
      assumption_met = variance_met,
      interpretation = if (variance_met) {
        "Homogeneity of variance met (p > 0.05)"
      } else {
        "Homogeneity violated (p < 0.05) - consider Welch's ANOVA"
      }
    )
  )
}


#' Create ANOVA Interpretation
#'
#' @param p_value P-value
#' @param eta_squared Effect size
#' @param n_groups Number of groups
#' @param post_hoc_sig Number of significant post-hoc comparisons
#'
#' @return Character string with interpretation
#'
#' @keywords internal
#' @noRd
create_anova_interpretation <- function(p_value, eta_squared, n_groups,
                                       post_hoc_sig = NULL) {

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

  # Effect size
  if (eta_squared < 0.01) {
    effect_text <- "negligible"
  } else if (eta_squared < 0.06) {
    effect_text <- "small"
  } else if (eta_squared < 0.14) {
    effect_text <- "medium"
  } else {
    effect_text <- "large"
  }

  # Base interpretation
  interp <- sprintf("The ANOVA is %s with a %s effect size (η² = %.3f). ",
                   sig_text, effect_text, eta_squared)

  # Add post-hoc info
  if (!is.null(post_hoc_sig) && post_hoc_sig > 0) {
    interp <- paste0(interp,
                    sprintf("Post-hoc tests reveal %d significant pairwise difference%s.",
                           post_hoc_sig,
                           ifelse(post_hoc_sig == 1, "", "s")))
  } else if (p_value < 0.05) {
    interp <- paste0(interp, "Groups differ significantly overall.")
  }

  return(interp)
}


#' Print Method for ANOVA Results
#'
#' @param x An object of class "descriptR_anova"
#' @param digits Number of digits to print (default 3)
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR_anova <- function(x, digits = 3, ...) {

  cat("\n")
  cat("One-Way Analysis of Variance\n")
  cat(rep("=", 70), "\n", sep = "")

  # ANOVA results
  cat(sprintf("\nF(%d, %d) = %.3f, p-value = %s\n",
             x$df_between,
             x$df_within,
             x$f_statistic,
             if (x$p_value < 0.001) "< 0.001" else sprintf("%.4f", x$p_value)))

  # Effect sizes
  cat("\nEffect Sizes:\n")
  cat(sprintf("  Eta-squared (η²): %.3f (%s)\n",
             x$eta_squared,
             interpret_effect_size(x$eta_squared, "eta_squared")))
  cat(sprintf("  Omega-squared (ω²): %.3f (unbiased estimate)\n",
             x$omega_squared))

  # Group means
  cat("\n")
  cat(rep("-", 70), "\n", sep = "")
  cat("\nGroup Means:\n")
  print(x$group_means, row.names = FALSE, digits = digits)

  # Post-hoc results
  if (!is.null(x$post_hoc)) {
    cat("\n")
    cat(rep("-", 70), "\n", sep = "")
    cat("\nPost-Hoc Tests:\n")
    print(x$post_hoc, row.names = FALSE, digits = digits)

    # Highlight significant comparisons
    sig_comps <- x$post_hoc[x$post_hoc$P_Value < 0.05, ]
    if (nrow(sig_comps) > 0) {
      cat("\nSignificant comparisons (p < 0.05):\n")
      for (i in 1:nrow(sig_comps)) {
        cat(sprintf("  %s\n", sig_comps$Comparison[i]))
      }
    }
  }

  # Assumptions
  if (!is.null(x$assumptions)) {
    cat("\n")
    cat(rep("-", 70), "\n", sep = "")
    cat("\nAssumption Checks:\n")

    cat(sprintf("\n  Normality (%s):\n", x$assumptions$normality$test))
    cat(sprintf("    %s\n", x$assumptions$normality$interpretation))

    cat(sprintf("\n  Homogeneity of Variance (%s):\n",
               x$assumptions$homogeneity$test))
    cat(sprintf("    %s\n", x$assumptions$homogeneity$interpretation))
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
