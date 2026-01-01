#' ANOVA/MANOVA Analysis Module
#'
#' @description
#' Comprehensive ANOVA and MANOVA analysis including one-way, two-way,
#' repeated measures, and multivariate analysis with post-hoc tests.
#'
#' @name anova_analysis
NULL


#' Perform ANOVA Analysis
#'
#' @description
#' Performs comprehensive ANOVA with automatic post-hoc tests, effect sizes,
#' and assumption checking.
#'
#' @param data Data frame
#' @param outcome Character name of outcome variable(s). For MANOVA, provide vector
#' @param groups Character name(s) of grouping variable(s)
#' @param type ANOVA type: "one-way", "two-way", "repeated", "manova", "auto"
#' @param post_hoc Post-hoc test: "tukey", "bonferroni", "none", "auto" (default)
#' @param effect_size Calculate effect sizes? Default TRUE
#' @param assumptions_check Test assumptions? Default TRUE
#' @param ... Additional arguments
#'
#' @return descriptR_anova object with:
#' \itemize{
#'   \item anova_table - Main ANOVA results
#'   \item post_hoc - Post-hoc test results (if applicable)
#'   \item effect_sizes - Effect size measures (eta-squared, omega-squared)
#'   \item assumptions - Assumption test results
#'   \item descriptives - Group descriptive statistics
#'   \item insights - Automated interpretation
#' }
#'
#' @examples
#' # One-way ANOVA
#' result <- perform_anova(iris, outcome = "Sepal.Length", groups = "Species")
#'
#' # Two-way ANOVA
#' result <- perform_anova(mtcars, outcome = "mpg", groups = c("cyl", "am"))
#'
#' # MANOVA
#' result <- perform_anova(iris,
#'                        outcome = c("Sepal.Length", "Sepal.Width"),
#'                        groups = "Species",
#'                        type = "manova")
#'
#' @export
perform_anova <- function(data,
                         outcome,
                         groups,
                         type = c("auto", "one-way", "two-way", "repeated", "manova"),
                         post_hoc = c("auto", "tukey", "bonferroni", "none"),
                         effect_size = TRUE,
                         assumptions_check = TRUE,
                         ...) {

  type <- match.arg(type)
  post_hoc <- match.arg(post_hoc)

  # Auto-detect type
  if (type == "auto") {
    if (length(outcome) > 1) {
      type <- "manova"
    } else if (length(groups) == 1) {
      type <- "one-way"
    } else if (length(groups) == 2) {
      type <- "two-way"
    }
    message(sprintf("Auto-detected ANOVA type: %s", type))
  }

  # Validate inputs
  validate_anova_inputs(data, outcome, groups, type)

  # Prepare data
  prepared_data <- prepare_anova_data(data, outcome, groups, type)

  # Run ANOVA
  anova_result <- run_anova(prepared_data, outcome, groups, type)

  # Post-hoc tests
  posthoc_results <- NULL
  if (post_hoc != "none" && type != "manova") {
    if (post_hoc == "auto") {
      post_hoc <- "tukey"  # Default to Tukey HSD
    }
    posthoc_results <- run_posthoc_tests(prepared_data, outcome, groups, post_hoc, type)
  }

  # Effect sizes
  effect_sizes <- NULL
  if (effect_size) {
    effect_sizes <- calculate_anova_effect_sizes(anova_result, prepared_data, type)
  }

  # Assumptions
  assumption_results <- NULL
  if (assumptions_check && type != "manova") {
    assumption_results <- check_anova_assumptions(prepared_data, outcome, groups, type)
  }

  # Descriptive statistics
  descriptives <- calculate_group_descriptives(prepared_data, outcome, groups, type)

  # Insights
  insights <- generate_anova_insights(anova_result, posthoc_results,
                                     effect_sizes, assumption_results, type)

  # Create result object
  result <- structure(
    list(
      anova_table = anova_result,
      post_hoc = posthoc_results,
      effect_sizes = effect_sizes,
      assumptions = assumption_results,
      descriptives = descriptives,
      insights = insights,
      metadata = list(
        type = type,
        outcome = outcome,
        groups = groups,
        n_obs = nrow(prepared_data),
        post_hoc_method = post_hoc
      )
    ),
    class = c("descriptR_anova", "descriptR_result")
  )

  return(result)
}


#' Validate ANOVA Inputs
#' @keywords internal
#' @noRd
validate_anova_inputs <- function(data, outcome, groups, type) {

  # Check outcome variables exist
  if (!all(outcome %in% names(data))) {
    stop("Outcome variable(s) not found in data", call. = FALSE)
  }

  # Check group variables exist
  if (!all(groups %in% names(data))) {
    stop("Grouping variable(s) not found in data", call. = FALSE)
  }

  # Check outcome is numeric
  if (!all(sapply(data[outcome], is.numeric))) {
    stop("Outcome variable(s) must be numeric", call. = FALSE)
  }

  # MANOVA requires multiple outcomes
  if (type == "manova" && length(outcome) < 2) {
    stop("MANOVA requires at least 2 outcome variables", call. = FALSE)
  }

  return(TRUE)
}


#' Prepare Data for ANOVA
#' @keywords internal
#' @noRd
prepare_anova_data <- function(data, outcome, groups, type) {

  # Convert groups to factors
  for (g in groups) {
    if (!is.factor(data[[g]])) {
      data[[g]] <- factor(data[[g]])
    }
  }

  # Remove missing values
  all_vars <- c(outcome, groups)
  data <- data[complete.cases(data[all_vars]), ]

  return(data)
}


#' Run ANOVA
#' @keywords internal
#' @noRd
run_anova <- function(data, outcome, groups, type) {

  if (type == "manova") {
    # MANOVA
    outcome_matrix <- as.matrix(data[outcome])
    formula_str <- paste("outcome_matrix ~", paste(groups, collapse = " * "))
    model <- stats::manova(stats::as.formula(formula_str), data = data)
    anova_table <- summary(model)

  } else if (type == "one-way") {
    # One-way ANOVA
    formula_str <- paste(outcome, "~", groups[1])
    model <- stats::aov(stats::as.formula(formula_str), data = data)
    anova_table <- summary(model)

  } else if (type == "two-way") {
    # Two-way ANOVA
    formula_str <- paste(outcome, "~", paste(groups, collapse = " * "))
    model <- stats::aov(stats::as.formula(formula_str), data = data)
    anova_table <- summary(model)

  } else if (type == "repeated") {
    # Repeated measures ANOVA (simplified)
    formula_str <- paste(outcome, "~", paste(groups, collapse = " * "))
    model <- stats::aov(stats::as.formula(formula_str), data = data)
    anova_table <- summary(model)
  }

  # Convert to data frame for easier handling
  if (type != "manova") {
    anova_df <- as.data.frame(anova_table[[1]])
    anova_df$term <- rownames(anova_df)
    anova_df <- anova_df[, c("term", setdiff(names(anova_df), "term"))]
    rownames(anova_df) <- NULL
    return(anova_df)
  }

  return(anova_table)
}


#' Run Post-hoc Tests
#' @keywords internal
#' @noRd
run_posthoc_tests <- function(data, outcome, groups, method, type) {

  posthoc_results <- list()

  if (type == "one-way" && method == "tukey") {
    # Tukey HSD
    formula_str <- paste(outcome, "~", groups[1])
    model <- stats::aov(stats::as.formula(formula_str), data = data)
    tukey_result <- stats::TukeyHSD(model)

    posthoc_results$tukey <- as.data.frame(tukey_result[[1]])
    posthoc_results$tukey$comparison <- rownames(posthoc_results$tukey)
    rownames(posthoc_results$tukey) <- NULL

  } else if (method == "bonferroni") {
    # Pairwise t-tests with Bonferroni correction
    formula_str <- paste(outcome, "~", groups[1])
    pairwise_result <- stats::pairwise.t.test(
      data[[outcome]],
      data[[groups[1]]],
      p.adjust.method = "bonferroni"
    )

    # Convert to data frame
    p_mat <- pairwise_result$p.value
    comparisons <- expand.grid(
      group1 = rownames(p_mat),
      group2 = colnames(p_mat)
    )
    comparisons$p_value <- as.vector(p_mat)
    comparisons <- comparisons[!is.na(comparisons$p_value), ]

    posthoc_results$bonferroni <- comparisons
  }

  return(posthoc_results)
}


#' Calculate ANOVA Effect Sizes
#' @keywords internal
#' @noRd
calculate_anova_effect_sizes <- function(anova_result, data, type) {

  effect_sizes <- list()

  if (type %in% c("one-way", "two-way", "repeated")) {
    # Eta-squared
    ss_total <- sum(anova_result$`Sum Sq`, na.rm = TRUE)

    eta_squared <- anova_result$`Sum Sq` / ss_total
    names(eta_squared) <- anova_result$term

    # Omega-squared (less biased)
    ms_error <- anova_result$`Mean Sq`[nrow(anova_result)]
    df_error <- anova_result$Df[nrow(anova_result)]

    omega_squared <- (anova_result$`Sum Sq` - anova_result$Df * ms_error) /
                     (ss_total + ms_error)
    omega_squared[omega_squared < 0] <- 0
    names(omega_squared) <- anova_result$term

    effect_sizes$eta_squared <- eta_squared
    effect_sizes$omega_squared <- omega_squared

    # Interpret effect sizes
    effect_sizes$interpretation <- data.frame(
      term = anova_result$term,
      eta_squared = eta_squared,
      interpretation = sapply(eta_squared, function(x) {
        if (is.na(x)) return(NA)
        if (x < 0.01) return("negligible")
        if (x < 0.06) return("small")
        if (x < 0.14) return("medium")
        return("large")
      }),
      stringsAsFactors = FALSE
    )
  }

  return(effect_sizes)
}


#' Check ANOVA Assumptions
#' @keywords internal
#' @noRd
check_anova_assumptions <- function(data, outcome, groups, type) {

  assumptions <- list()

  # Normality (Shapiro-Wilk by group)
  normality_tests <- list()
  for (g in unique(data[[groups[1]]])) {
    group_data <- data[data[[groups[1]]] == g, outcome]
    if (length(group_data) >= 3 && length(group_data) <= 5000) {
      test_result <- stats::shapiro.test(group_data)
      normality_tests[[as.character(g)]] <- list(
        statistic = test_result$statistic,
        p_value = test_result$p.value,
        normal = test_result$p.value > 0.05
      )
    }
  }
  assumptions$normality <- normality_tests

  # Homogeneity of variance (Levene's test approximation using Bartlett)
  formula_str <- paste(outcome, "~", groups[1])
  bartlett_test <- stats::bartlett.test(stats::as.formula(formula_str), data = data)

  assumptions$homogeneity <- list(
    test = "Bartlett",
    statistic = bartlett_test$statistic,
    p_value = bartlett_test$p.value,
    homogeneous = bartlett_test$p.value > 0.05,
    conclusion = ifelse(bartlett_test$p.value > 0.05,
                       "Variances appear homogeneous",
                       "Variances may not be homogeneous")
  )

  return(assumptions)
}


#' Calculate Group Descriptive Statistics
#' @keywords internal
#' @noRd
calculate_group_descriptives <- function(data, outcome, groups, type) {

  if (type == "manova") {
    # Descriptives for multiple outcomes
    descriptives <- list()
    for (out in outcome) {
      descriptives[[out]] <- aggregate(
        data[[out]] ~ data[[groups[1]]],
        FUN = function(x) c(
          n = length(x),
          mean = mean(x),
          sd = stats::sd(x),
          min = min(x),
          max = max(x)
        )
      )
    }
    return(descriptives)
  }

  # Single outcome
  formula_str <- paste(outcome, "~", paste(groups, collapse = "+"))

  descriptives <- aggregate(
    stats::as.formula(formula_str),
    data = data,
    FUN = function(x) c(
      n = length(x),
      mean = mean(x),
      sd = stats::sd(x),
      min = min(x),
      max = max(x)
    )
  )

  # Flatten result
  desc_df <- data.frame(descriptives[groups])
  stats_df <- as.data.frame(descriptives[[outcome]])
  colnames(stats_df) <- c("n", "mean", "sd", "min", "max")
  descriptives <- cbind(desc_df, stats_df)

  return(descriptives)
}


#' Generate ANOVA Insights
#' @keywords internal
#' @noRd
generate_anova_insights <- function(anova_result, posthoc, effect_sizes,
                                   assumptions, type) {

  insights <- c()

  if (type != "manova") {
    # Main effects
    sig_effects <- anova_result$term[anova_result$`Pr(>F)` < 0.05 &
                                    anova_result$term != "Residuals"]

    if (length(sig_effects) > 0) {
      insights <- c(insights,
                   sprintf("Significant effects: %s", paste(sig_effects, collapse = ", ")))
    } else {
      insights <- c(insights, "No significant effects detected")
    }

    # Effect sizes
    if (!is.null(effect_sizes$interpretation)) {
      large_effects <- effect_sizes$interpretation$term[
        effect_sizes$interpretation$interpretation == "large" &
        !is.na(effect_sizes$interpretation$interpretation)
      ]
      if (length(large_effects) > 0) {
        insights <- c(insights,
                     sprintf("Large effect sizes for: %s",
                            paste(large_effects, collapse = ", ")))
      }
    }

    # Assumptions
    if (!is.null(assumptions$homogeneity)) {
      insights <- c(insights, assumptions$homogeneity$conclusion)
    }

    # Post-hoc significant pairs
    if (!is.null(posthoc$tukey)) {
      sig_pairs <- posthoc$tukey$comparison[posthoc$tukey$`p adj` < 0.05]
      if (length(sig_pairs) > 0) {
        insights <- c(insights,
                     sprintf("Significant pairwise differences: %s",
                            paste(sig_pairs, collapse = ", ")))
      }
    }
  }

  return(insights)
}


#' Print Method for ANOVA Results
#' @export
print.descriptR_anova <- function(x, ...) {
  cat("\nANOVA Analysis Results\n")
  cat(rep("=", 50), "\n", sep = "")
  cat(sprintf("Type: %s ANOVA\n", x$metadata$type))
  cat(sprintf("Outcome: %s\n", paste(x$metadata$outcome, collapse = ", ")))
  cat(sprintf("Groups: %s\n", paste(x$metadata$groups, collapse = ", ")))
  cat(sprintf("N = %d observations\n\n", x$metadata$n_obs))

  if (x$metadata$type != "manova") {
    cat("ANOVA Table:\n")
    print(x$anova_table, row.names = FALSE)
  }

  if (!is.null(x$effect_sizes$interpretation)) {
    cat("\nEffect Sizes:\n")
    print(x$effect_sizes$interpretation, row.names = FALSE)
  }

  cat("\nInsights:\n")
  for (insight in x$insights) {
    cat(sprintf("  - %s\n", insight))
  }

  cat(rep("=", 50), "\n", sep = "")
  invisible(x)
}
