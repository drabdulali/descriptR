#' Non-Parametric Tests
#'
#' @description
#' Comprehensive non-parametric statistical tests for situations where
#' parametric assumptions are violated or data are ordinal.
#'
#' @name nonparametric_tests
NULL


#' Mann-Whitney U Test (Wilcoxon Rank-Sum Test)
#'
#' @description
#' Non-parametric alternative to independent samples t-test. Tests whether
#' two independent samples come from the same distribution.
#'
#' @param x Numeric vector (group 1) or formula
#' @param y Numeric vector (group 2) or data frame
#' @param data Data frame (if using formula)
#' @param alternative Direction of test: "two.sided", "less", "greater"
#' @param conf.level Confidence level (default 0.95)
#' @param exact Compute exact p-value? (default NULL = auto)
#' @param correct Apply continuity correction? (default TRUE)
#'
#' @return Object of class "descriptR_mann_whitney" containing:
#' \itemize{
#'   \item \code{statistic}: U statistic
#'   \item \code{p_value}: P-value
#'   \item \code{effect_size}: Rank-biserial correlation
#'   \item \code{descriptives}: Median and IQR for each group
#'   \item \code{interpretation}: Plain-language interpretation
#' }
#'
#' @details
#' ## When to Use
#' - Alternative to independent t-test
#' - Ordinal data
#' - Non-normal continuous data
#' - Unequal variances
#' - Small samples
#'
#' ## Assumptions
#' - Independent samples
#' - Ordinal or continuous data
#' - Similar distribution shapes (for location comparison)
#'
#' ## Effect Size
#' Rank-biserial correlation: r = 1 - (2U)/(n1*n2)
#' - Range: -1 to 1
#' - Interpretation: Same as r (0.1 small, 0.3 medium, 0.5 large)
#'
#' @examples
#' # Two independent groups
#' perform_mann_whitney(extra ~ group, data = sleep)
#'
#' # Vector input
#' perform_mann_whitney(rnorm(20), rnorm(20, mean = 0.5))
#'
#' @export
perform_mann_whitney <- function(x, y = NULL, data = NULL,
                                  alternative = c("two.sided", "less", "greater"),
                                  conf.level = 0.95,
                                  exact = NULL,
                                  correct = TRUE) {

  alternative <- match.arg(alternative)

  # Handle formula interface
  if (inherits(x, "formula")) {
    formula <- x
    if (is.null(data)) {
      stop("data argument required when using formula", call. = FALSE)
    }

    vars <- all.vars(formula)
    outcome <- data[[vars[1]]]
    group <- data[[vars[2]]]

    groups <- unique(group[!is.na(group)])
    if (length(groups) != 2) {
      stop("Mann-Whitney test requires exactly 2 groups", call. = FALSE)
    }

    x <- outcome[group == groups[1]]
    y <- outcome[group == groups[2]]
    group_names <- as.character(groups)
  } else {
    group_names <- c("Group 1", "Group 2")
  }

  # Remove NA
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]

  n1 <- length(x)
  n2 <- length(y)

  if (n1 < 1 || n2 < 1) {
    stop("Both groups must have at least 1 observation", call. = FALSE)
  }

  # Perform test
  test_result <- wilcox.test(x, y,
                              alternative = alternative,
                              conf.level = conf.level,
                              exact = exact,
                              correct = correct)

  # Compute effect size (rank-biserial correlation)
  U <- as.numeric(test_result$statistic)
  r_rb <- 1 - (2 * U) / (n1 * n2)

  # Descriptive statistics
  descriptives <- data.frame(
    Group = group_names,
    N = c(n1, n2),
    Median = c(median(x), median(y)),
    IQR = c(IQR(x), IQR(y)),
    Min = c(min(x), min(y)),
    Max = c(max(x), max(y)),
    stringsAsFactors = FALSE
  )

  # Interpretation
  interpretation <- create_mann_whitney_interpretation(
    test_result$p.value, r_rb, group_names, alternative
  )

  result <- list(
    statistic = U,
    p_value = test_result$p.value,
    effect_size = r_rb,
    alternative = alternative,
    method = test_result$method,
    descriptives = descriptives,
    interpretation = interpretation,
    n1 = n1,
    n2 = n2
  )

  class(result) <- "descriptR_mann_whitney"
  return(result)
}


#' Kruskal-Wallis Test
#'
#' @description
#' Non-parametric alternative to one-way ANOVA. Tests whether multiple
#' independent samples come from the same distribution.
#'
#' @param formula Formula: outcome ~ group
#' @param data Data frame
#' @param post_hoc Perform post-hoc pairwise comparisons? (default TRUE)
#' @param p_adjust Method for p-value adjustment: "holm", "bonferroni", etc.
#'
#' @return Object of class "descriptR_kruskal" containing test results
#'
#' @details
#' ## When to Use
#' - Alternative to one-way ANOVA
#' - 3+ independent groups
#' - Ordinal data
#' - Non-normal continuous data
#' - Unequal variances
#'
#' ## Effect Size
#' Epsilon-squared (ε²): proportion of variance explained
#' - Analogous to eta-squared for ANOVA
#'
#' @examples
#' perform_kruskal_wallis(Sepal.Length ~ Species, data = iris)
#'
#' @export
perform_kruskal_wallis <- function(formula, data,
                                    post_hoc = TRUE,
                                    p_adjust = "holm") {

  # Extract variables
  vars <- all.vars(formula)
  outcome <- data[[vars[1]]]
  group <- data[[vars[2]]]

  # Remove NA
  complete_cases <- complete.cases(outcome, group)
  outcome <- outcome[complete_cases]
  group <- group[complete_cases]

  groups <- unique(group)
  n_groups <- length(groups)

  if (n_groups < 2) {
    stop("Kruskal-Wallis test requires at least 2 groups", call. = FALSE)
  }

  # Perform test
  test_result <- kruskal.test(outcome ~ group)

  # Compute effect size (epsilon-squared)
  n <- length(outcome)
  H <- as.numeric(test_result$statistic)
  epsilon_sq <- H / ((n^2 - 1) / (n + 1))

  # Descriptive statistics by group
  descriptives <- do.call(rbind, lapply(groups, function(g) {
    x <- outcome[group == g]
    data.frame(
      Group = as.character(g),
      N = length(x),
      Median = median(x),
      IQR = IQR(x),
      Mean_Rank = mean(rank(outcome)[group == g]),
      stringsAsFactors = FALSE
    )
  }))

  # Post-hoc pairwise comparisons
  post_hoc_results <- NULL
  if (post_hoc && n_groups > 2) {
    post_hoc_results <- pairwise_mann_whitney(outcome, group, p_adjust)
  }

  # Interpretation
  interpretation <- create_kruskal_interpretation(
    test_result$p.value, epsilon_sq, n_groups
  )

  result <- list(
    statistic = H,
    df = test_result$parameter,
    p_value = test_result$p.value,
    effect_size = epsilon_sq,
    method = test_result$method,
    descriptives = descriptives,
    post_hoc = post_hoc_results,
    interpretation = interpretation,
    n_groups = n_groups
  )

  class(result) <- "descriptR_kruskal"
  return(result)
}


#' Wilcoxon Signed-Rank Test
#'
#' @description
#' Non-parametric alternative to paired t-test. Tests whether paired
#' samples differ in their central tendency.
#'
#' @param x Numeric vector (time 1) or formula
#' @param y Numeric vector (time 2) or NULL if using formula
#' @param data Data frame (if using formula)
#' @param alternative Direction of test
#' @param conf.level Confidence level (default 0.95)
#' @param exact Compute exact p-value?
#' @param correct Apply continuity correction?
#'
#' @return Object of class "descriptR_wilcoxon_signed" with test results
#'
#' @details
#' ## When to Use
#' - Alternative to paired t-test
#' - Repeated measures (pre-post)
#' - Non-normal differences
#' - Ordinal data
#'
#' ## Effect Size
#' Matched-pairs rank-biserial correlation
#'
#' @examples
#' # Paired data
#' perform_wilcoxon_signed(extra ~ group, data = sleep[sleep$ID == 1, ])
#'
#' @export
perform_wilcoxon_signed <- function(x, y = NULL, data = NULL,
                                     alternative = c("two.sided", "less", "greater"),
                                     conf.level = 0.95,
                                     exact = NULL,
                                     correct = TRUE) {

  alternative <- match.arg(alternative)

  # Handle formula interface
  if (inherits(x, "formula")) {
    if (is.null(data)) {
      stop("data argument required when using formula", call. = FALSE)
    }

    vars <- all.vars(formula)
    x <- data[[vars[1]]]
    y <- data[[vars[2]]]
  }

  # Remove NA
  complete_cases <- complete.cases(x, y)
  x <- x[complete_cases]
  y <- y[complete_cases]

  if (length(x) != length(y)) {
    stop("x and y must have the same length", call. = FALSE)
  }

  n <- length(x)

  if (n < 1) {
    stop("Need at least 1 paired observation", call. = FALSE)
  }

  # Perform test
  test_result <- wilcox.test(x, y,
                              paired = TRUE,
                              alternative = alternative,
                              conf.level = conf.level,
                              exact = exact,
                              correct = correct)

  # Compute effect size (matched-pairs rank-biserial)
  differences <- x - y
  V <- as.numeric(test_result$statistic)
  r_rb <- 1 - (4 * V) / (n * (n + 1))

  # Descriptive statistics
  descriptives <- data.frame(
    Measure = c("Time 1", "Time 2", "Difference"),
    N = c(n, n, n),
    Median = c(median(x), median(y), median(differences)),
    IQR = c(IQR(x), IQR(y), IQR(differences)),
    stringsAsFactors = FALSE
  )

  # Interpretation
  interpretation <- create_wilcoxon_interpretation(
    test_result$p.value, r_rb, alternative
  )

  result <- list(
    statistic = V,
    p_value = test_result$p.value,
    effect_size = r_rb,
    alternative = alternative,
    method = test_result$method,
    descriptives = descriptives,
    interpretation = interpretation,
    n = n
  )

  class(result) <- "descriptR_wilcoxon_signed"
  return(result)
}


#' Friedman Test
#'
#' @description
#' Non-parametric alternative to repeated measures ANOVA. Tests whether
#' multiple related samples differ.
#'
#' @param formula Formula: outcome ~ condition | subject
#' @param data Data frame
#' @param post_hoc Perform post-hoc comparisons? (default TRUE)
#'
#' @return Object of class "descriptR_friedman" with test results
#'
#' @details
#' ## When to Use
#' - Alternative to repeated measures ANOVA
#' - 3+ related measurements
#' - Non-normal data
#' - Ordinal data
#'
#' ## Effect Size
#' Kendall's W (coefficient of concordance)
#'
#' @examples
#' # Requires data in long format with subject ID
#' # perform_friedman(score ~ time | subject, data = mydata)
#'
#' @export
perform_friedman <- function(formula, data, post_hoc = TRUE) {

  # Parse formula
  formula_parts <- as.character(formula)

  # Extract outcome and condition
  left_side <- formula_parts[2]
  right_side <- formula_parts[3]

  # Check for block variable (subject)
  if (!grepl("\\|", right_side)) {
    stop("Formula must include blocking variable: outcome ~ condition | subject",
         call. = FALSE)
  }

  parts <- strsplit(right_side, "\\|")[[1]]
  condition_var <- trimws(parts[1])
  block_var <- trimws(parts[2])

  outcome <- data[[left_side]]
  condition <- data[[condition_var]]
  block <- data[[block_var]]

  # Remove NA
  complete_cases <- complete.cases(outcome, condition, block)
  outcome <- outcome[complete_cases]
  condition <- condition[complete_cases]
  block <- block[complete_cases]

  # Check balance
  counts <- table(block, condition)
  if (!all(counts %in% c(0, 1))) {
    warning("Unbalanced design detected. Friedman test requires complete blocks.",
            call. = FALSE)
  }

  # Reshape to wide format for friedman.test
  wide_data <- reshape(data.frame(outcome, condition, block),
                      idvar = "block",
                      timevar = "condition",
                      direction = "wide")

  # Perform test
  test_result <- friedman.test(as.matrix(wide_data[, -1]))

  # Compute Kendall's W
  k <- ncol(wide_data) - 1  # Number of conditions
  n <- nrow(wide_data)       # Number of blocks
  chi_sq <- as.numeric(test_result$statistic)
  W <- chi_sq / (n * (k - 1))

  # Descriptive statistics
  conditions <- unique(condition)
  descriptives <- do.call(rbind, lapply(conditions, function(cond) {
    x <- outcome[condition == cond]
    data.frame(
      Condition = as.character(cond),
      N = length(x),
      Median = median(x),
      IQR = IQR(x),
      Mean_Rank = mean(rank(outcome)[condition == cond]),
      stringsAsFactors = FALSE
    )
  }))

  # Interpretation
  interpretation <- create_friedman_interpretation(
    test_result$p.value, W, k
  )

  result <- list(
    statistic = chi_sq,
    df = test_result$parameter,
    p_value = test_result$p.value,
    kendalls_w = W,
    method = test_result$method,
    descriptives = descriptives,
    interpretation = interpretation,
    n_conditions = k,
    n_blocks = n
  )

  class(result) <- "descriptR_friedman"
  return(result)
}


# Helper Functions ============================================================

#' Pairwise Mann-Whitney Tests
#'
#' @keywords internal
#' @noRd
pairwise_mann_whitney <- function(outcome, group, p_adjust = "holm") {

  groups <- unique(group)
  n_groups <- length(groups)

  comparisons <- combn(groups, 2, simplify = FALSE)

  results_list <- lapply(comparisons, function(pair) {
    x <- outcome[group == pair[1]]
    y <- outcome[group == pair[2]]

    test <- wilcox.test(x, y)

    # Effect size
    U <- as.numeric(test$statistic)
    n1 <- length(x)
    n2 <- length(y)
    r_rb <- 1 - (2 * U) / (n1 * n2)

    data.frame(
      Group1 = as.character(pair[1]),
      Group2 = as.character(pair[2]),
      U = U,
      P_Raw = test$p.value,
      Effect_Size = r_rb,
      stringsAsFactors = FALSE
    )
  })

  results_df <- do.call(rbind, results_list)

  # Adjust p-values
  results_df$P_Adjusted <- p.adjust(results_df$P_Raw, method = p_adjust)
  results_df$Significant <- results_df$P_Adjusted < 0.05

  return(results_df)
}


#' Create Mann-Whitney Interpretation
#'
#' @keywords internal
#' @noRd
create_mann_whitney_interpretation <- function(p_value, r_rb, group_names,
                                               alternative) {

  # Significance
  if (p_value < 0.001) {
    sig_text <- "highly significant (p < 0.001)"
  } else if (p_value < 0.05) {
    sig_text <- sprintf("significant (p = %.3f)", p_value)
  } else {
    sig_text <- sprintf("not significant (p = %.3f)", p_value)
  }

  # Effect size
  abs_r <- abs(r_rb)
  if (abs_r < 0.1) {
    effect_text <- "negligible"
  } else if (abs_r < 0.3) {
    effect_text <- "small"
  } else if (abs_r < 0.5) {
    effect_text <- "medium"
  } else {
    effect_text <- "large"
  }

  # Direction
  direction <- if (r_rb > 0) {
    sprintf("%s tends to have higher values than %s",
           group_names[1], group_names[2])
  } else {
    sprintf("%s tends to have lower values than %s",
           group_names[1], group_names[2])
  }

  sprintf("Mann-Whitney U test: %s. Effect size r = %.3f (%s). %s",
         sig_text, abs_r, effect_text, direction)
}


#' Create Kruskal-Wallis Interpretation
#'
#' @keywords internal
#' @noRd
create_kruskal_interpretation <- function(p_value, epsilon_sq, n_groups) {

  if (p_value < 0.001) {
    sig_text <- "highly significant (p < 0.001)"
  } else if (p_value < 0.05) {
    sig_text <- sprintf("significant (p = %.3f)", p_value)
  } else {
    sig_text <- sprintf("not significant (p = %.3f)", p_value)
  }

  if (epsilon_sq < 0.01) {
    effect_text <- "negligible"
  } else if (epsilon_sq < 0.06) {
    effect_text <- "small"
  } else if (epsilon_sq < 0.14) {
    effect_text <- "medium"
  } else {
    effect_text <- "large"
  }

  sprintf("Kruskal-Wallis test with %d groups: %s. Effect size ε² = %.3f (%s)",
         n_groups, sig_text, epsilon_sq, effect_text)
}


#' Create Wilcoxon Interpretation
#'
#' @keywords internal
#' @noRd
create_wilcoxon_interpretation <- function(p_value, r_rb, alternative) {

  if (p_value < 0.001) {
    sig_text <- "highly significant (p < 0.001)"
  } else if (p_value < 0.05) {
    sig_text <- sprintf("significant (p = %.3f)", p_value)
  } else {
    sig_text <- sprintf("not significant (p = %.3f)", p_value)
  }

  abs_r <- abs(r_rb)
  if (abs_r < 0.1) {
    effect_text <- "negligible"
  } else if (abs_r < 0.3) {
    effect_text <- "small"
  } else if (abs_r < 0.5) {
    effect_text <- "medium"
  } else {
    effect_text <- "large"
  }

  sprintf("Wilcoxon signed-rank test: %s. Effect size r = %.3f (%s)",
         sig_text, abs_r, effect_text)
}


#' Create Friedman Interpretation
#'
#' @keywords internal
#' @noRd
create_friedman_interpretation <- function(p_value, W, k) {

  if (p_value < 0.001) {
    sig_text <- "highly significant (p < 0.001)"
  } else if (p_value < 0.05) {
    sig_text <- sprintf("significant (p = %.3f)", p_value)
  } else {
    sig_text <- sprintf("not significant (p = %.3f)", p_value)
  }

  if (W < 0.1) {
    effect_text <- "weak concordance"
  } else if (W < 0.3) {
    effect_text <- "moderate concordance"
  } else if (W < 0.5) {
    effect_text <- "strong concordance"
  } else {
    effect_text <- "very strong concordance"
  }

  sprintf("Friedman test with %d conditions: %s. Kendall's W = %.3f (%s)",
         k, sig_text, W, effect_text)
}


# Print Methods ===============================================================

#' @export
print.descriptR_mann_whitney <- function(x, digits = 3, ...) {
  cat("\n")
  cat("Mann-Whitney U Test\n")
  cat(rep("=", 70), "\n", sep = "")

  cat(sprintf("\nU = %.2f, p = %s\n",
             x$statistic,
             if (x$p_value < 0.001) "< 0.001" else sprintf("%.4f", x$p_value)))
  cat(sprintf("Effect size (rank-biserial r) = %.3f\n", x$effect_size))

  cat("\nDescriptive Statistics:\n\n")
  print(x$descriptives, row.names = FALSE, digits = digits)

  cat(sprintf("\nInterpretation:\n%s\n", x$interpretation))

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")
  invisible(x)
}

#' @export
print.descriptR_kruskal <- function(x, digits = 3, ...) {
  cat("\n")
  cat("Kruskal-Wallis Test\n")
  cat(rep("=", 70), "\n", sep = "")

  cat(sprintf("\nχ² = %.2f, df = %d, p = %s\n",
             x$statistic, x$df,
             if (x$p_value < 0.001) "< 0.001" else sprintf("%.4f", x$p_value)))
  cat(sprintf("Effect size (ε²) = %.3f\n", x$effect_size))

  cat("\nDescriptive Statistics by Group:\n\n")
  print(x$descriptives, row.names = FALSE, digits = digits)

  if (!is.null(x$post_hoc)) {
    cat("\nPost-hoc Pairwise Comparisons:\n\n")
    print(x$post_hoc, row.names = FALSE, digits = digits)
  }

  cat(sprintf("\nInterpretation:\n%s\n", x$interpretation))

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")
  invisible(x)
}

#' @export
print.descriptR_wilcoxon_signed <- function(x, digits = 3, ...) {
  cat("\n")
  cat("Wilcoxon Signed-Rank Test\n")
  cat(rep("=", 70), "\n", sep = "")

  cat(sprintf("\nV = %.2f, p = %s\n",
             x$statistic,
             if (x$p_value < 0.001) "< 0.001" else sprintf("%.4f", x$p_value)))
  cat(sprintf("Effect size (rank-biserial r) = %.3f\n", x$effect_size))

  cat("\nDescriptive Statistics:\n\n")
  print(x$descriptives, row.names = FALSE, digits = digits)

  cat(sprintf("\nInterpretation:\n%s\n", x$interpretation))

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")
  invisible(x)
}

#' @export
print.descriptR_friedman <- function(x, digits = 3, ...) {
  cat("\n")
  cat("Friedman Test\n")
  cat(rep("=", 70), "\n", sep = "")

  cat(sprintf("\nχ² = %.2f, df = %d, p = %s\n",
             x$statistic, x$df,
             if (x$p_value < 0.001) "< 0.001" else sprintf("%.4f", x$p_value)))
  cat(sprintf("Kendall's W = %.3f\n", x$kendalls_w))

  cat("\nDescriptive Statistics by Condition:\n\n")
  print(x$descriptives, row.names = FALSE, digits = digits)

  cat(sprintf("\nInterpretation:\n%s\n", x$interpretation))

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")
  invisible(x)
}
