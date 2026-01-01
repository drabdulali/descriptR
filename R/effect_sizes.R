#' Comprehensive Effect Size Calculations
#'
#' @description
#' A comprehensive module for computing effect sizes across different statistical
#' contexts with confidence intervals and interpretation.
#'
#' @name effect_sizes
#' @keywords internal
NULL


# Cohen's d Family ============================================================

#' Compute Cohen's d
#'
#' @description
#' Computes Cohen's d effect size for comparing two groups. Provides multiple
#' variants and confidence intervals.
#'
#' @param x Numeric vector (group 1) or data frame
#' @param y Numeric vector (group 2) or grouping variable name
#' @param data Data frame (if using formula interface)
#' @param pooled Logical, use pooled SD? (default TRUE)
#' @param hedges Logical, apply Hedges' correction for small samples? (default FALSE)
#' @param paired Logical, paired samples? (default FALSE)
#' @param conf.level Confidence level (default 0.95)
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return List containing:
#' \itemize{
#'   \item \code{d}: Cohen's d estimate
#'   \item \code{conf_int}: Confidence interval
#'   \item \code{variance}: Variance of d
#'   \item \code{se}: Standard error
#'   \item \code{method}: Method used
#'   \item \code{interpretation}: Interpretation text
#' }
#'
#' @details
#' ## Variants
#'
#' **Pooled SD (default)**: d = (M1 - M2) / SD_pooled
#' - Most common variant
#' - Assumes equal variances
#'
#' **Separate SDs**: Available when pooled = FALSE
#' - Uses SD of control group (Glass's delta)
#' - Robust to variance differences
#'
#' **Hedges' g**: Small sample correction
#' - g = d * (1 - 3/(4N - 9))
#' - Unbiased estimator for small samples
#'
#' **Paired samples**: d = M_diff / SD_diff
#' - For within-subjects designs
#'
#' ## Interpretation (Cohen, 1988)
#' - Small: |d| = 0.20
#' - Medium: |d| = 0.50
#' - Large: |d| = 0.80
#'
#' @examples
#' x <- rnorm(50, mean = 10, sd = 2)
#' y <- rnorm(50, mean = 12, sd = 2)
#' compute_cohens_d(x, y)
#'
#' # Hedges' g for small samples
#' compute_cohens_d(x, y, hedges = TRUE)
#'
#' @export
compute_cohens_d <- function(x, y = NULL, data = NULL,
                              pooled = TRUE,
                              hedges = FALSE,
                              paired = FALSE,
                              conf.level = 0.95,
                              na.rm = TRUE) {

  # Handle data frame input
  if (!is.null(data) && is.character(x) && is.character(y)) {
    group_var <- data[[y]]
    outcome_var <- data[[x]]
    groups <- unique(group_var[!is.na(group_var)])
    if (length(groups) != 2) {
      stop("Cohen's d requires exactly 2 groups", call. = FALSE)
    }
    x <- outcome_var[group_var == groups[1]]
    y <- outcome_var[group_var == groups[2]]
  }

  # Validate inputs
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric", call. = FALSE)
  }

  # Handle NA
  if (na.rm) {
    if (paired) {
      complete_cases <- complete.cases(x, y)
      x <- x[complete_cases]
      y <- y[complete_cases]
    } else {
      x <- x[!is.na(x)]
      y <- y[!is.na(y)]
    }
  }

  n1 <- length(x)
  n2 <- length(y)

  if (paired && n1 != n2) {
    stop("For paired design, x and y must have the same length", call. = FALSE)
  }

  # Compute effect size
  if (paired) {
    # Paired Cohen's d
    diff <- x - y
    d <- mean(diff) / sd(diff)
    n <- n1
    method <- "Cohen's d (paired)"

    # Variance for paired d
    var_d <- (1/n + d^2/(2*n)) * 2*(1 - cor(x, y))

  } else {
    # Independent samples
    m1 <- mean(x)
    m2 <- mean(y)
    s1 <- sd(x)
    s2 <- sd(y)

    if (pooled) {
      # Pooled SD
      s_pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
      d <- (m1 - m2) / s_pooled
      method <- "Cohen's d (pooled SD)"

      # Variance of d (Hedges & Olkin, 1985)
      var_d <- (n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2))

    } else {
      # Glass's delta (uses control SD)
      d <- (m1 - m2) / s2
      method <- "Glass's delta"

      # Approximate variance
      var_d <- (n1 + n2) / (n1 * n2) + d^2 / (2 * n2)
    }

    n <- n1 + n2
  }

  # Apply Hedges' correction
  if (hedges) {
    correction_factor <- 1 - (3 / (4 * n - 9))
    d <- d * correction_factor
    var_d <- var_d * correction_factor^2
    method <- paste0(method, " with Hedges' correction")
  }

  # Standard error and confidence interval
  se <- sqrt(var_d)
  z_crit <- qnorm((1 + conf.level) / 2)
  ci_lower <- d - z_crit * se
  ci_upper <- d + z_crit * se

  # Interpretation
  interpretation <- interpret_cohens_d(d, n)

  result <- list(
    d = d,
    conf_int = c(ci_lower, ci_upper),
    variance = var_d,
    se = se,
    n = n,
    method = method,
    interpretation = interpretation
  )

  class(result) <- "descriptR_cohens_d"
  return(result)
}


#' Interpret Cohen's d
#'
#' @param d Cohen's d value
#' @param n Sample size
#'
#' @return Character string with interpretation
#'
#' @keywords internal
#' @noRd
interpret_cohens_d <- function(d, n) {
  abs_d <- abs(d)

  # Effect size magnitude (Cohen, 1988)
  if (abs_d < 0.2) {
    magnitude <- "negligible"
  } else if (abs_d < 0.5) {
    magnitude <- "small"
  } else if (abs_d < 0.8) {
    magnitude <- "medium"
  } else if (abs_d < 1.2) {
    magnitude <- "large"
  } else {
    magnitude <- "very large"
  }

  direction <- if (d > 0) "positive" else "negative"

  sprintf("Cohen's d = %.3f represents a %s %s effect (n = %d)",
          abs_d, magnitude, direction, n)
}


# ANOVA Effect Sizes ==========================================================

#' Compute Eta-Squared
#'
#' @description
#' Computes eta-squared (η²) effect size for ANOVA designs. Eta-squared
#' represents the proportion of total variance explained by the factor.
#'
#' @param aov_model ANOVA model object (from aov() or anova())
#' @param type Type of eta-squared: "partial" (default) or "classical"
#' @param conf.level Confidence level for CI (default 0.95)
#'
#' @return List with eta-squared value, confidence interval, and interpretation
#'
#' @details
#' ## Types
#'
#' **Classical η²**: SS_effect / SS_total
#' - Total variance explained
#' - Range: 0 to 1
#'
#' **Partial η²**: SS_effect / (SS_effect + SS_error)
#' - Variance explained removing other effects
#' - Preferred for multifactor designs
#'
#' ## Interpretation (Cohen, 1988)
#' - Small: η² = 0.01
#' - Medium: η² = 0.06
#' - Large: η² = 0.14
#'
#' @examples
#' model <- aov(Sepal.Length ~ Species, data = iris)
#' compute_eta_squared(model)
#'
#' @export
compute_eta_squared <- function(aov_model, type = c("partial", "classical"),
                                 conf.level = 0.95) {

  type <- match.arg(type)

  # Extract sums of squares
  aov_summary <- summary(aov_model)
  ss_table <- aov_summary[[1]]

  # Get SS values
  ss_effect <- ss_table[1, "Sum Sq"]
  ss_error <- ss_table[nrow(ss_table), "Sum Sq"]
  ss_total <- sum(ss_table[, "Sum Sq"])

  # Compute eta-squared
  if (type == "classical") {
    eta_sq <- ss_effect / ss_total
    method <- "Classical eta-squared"
  } else {
    eta_sq <- ss_effect / (ss_effect + ss_error)
    method <- "Partial eta-squared"
  }

  # Degrees of freedom
  df_effect <- ss_table[1, "Df"]
  df_error <- ss_table[nrow(ss_table), "Df"]
  df_total <- sum(ss_table[, "Df"])

  # Confidence interval using non-central F distribution
  f_stat <- ss_table[1, "F value"]
  n <- df_total + 1

  # Convert to confidence interval for eta-squared
  # Using method from Steiger (2004)
  if (!is.na(f_stat) && f_stat > 0) {
    # Non-centrality parameter limits
    lambda <- f_stat * df_effect
    ci <- conf_int_eta_squared(f_stat, df_effect, df_error, conf.level)
  } else {
    ci <- c(0, 0)
  }

  # Interpretation
  interpretation <- interpret_eta_squared(eta_sq)

  result <- list(
    eta_squared = eta_sq,
    conf_int = ci,
    type = type,
    df_effect = df_effect,
    df_error = df_error,
    method = method,
    interpretation = interpretation
  )

  class(result) <- "descriptR_eta_squared"
  return(result)
}


#' Confidence Interval for Eta-Squared
#'
#' @param f_stat F-statistic
#' @param df1 Effect degrees of freedom
#' @param df2 Error degrees of freedom
#' @param conf.level Confidence level
#'
#' @return Numeric vector with lower and upper CI bounds
#'
#' @keywords internal
#' @noRd
conf_int_eta_squared <- function(f_stat, df1, df2, conf.level) {

  # Find non-central parameters for CI
  alpha <- 1 - conf.level

  # Lower limit
  if (f_stat > 1) {
    ncp_lower <- tryCatch({
      uniroot(function(ncp) {
        pf(f_stat, df1, df2, ncp) - (1 - alpha/2)
      }, c(0, f_stat * df1 * 10))$root
    }, error = function(e) 0)

    eta_lower <- ncp_lower / (ncp_lower + df1 + df2 + 1)
  } else {
    eta_lower <- 0
  }

  # Upper limit
  ncp_upper <- tryCatch({
    uniroot(function(ncp) {
      pf(f_stat, df1, df2, ncp) - alpha/2
    }, c(0, f_stat * df1 * 100))$root
  }, error = function(e) {
    f_stat * df1
  })

  eta_upper <- ncp_upper / (ncp_upper + df1 + df2 + 1)

  # Bound between 0 and 1
  c(max(0, eta_lower), min(1, eta_upper))
}


#' Interpret Eta-Squared
#'
#' @param eta_sq Eta-squared value
#'
#' @return Character string with interpretation
#'
#' @keywords internal
#' @noRd
interpret_eta_squared <- function(eta_sq) {
  if (eta_sq < 0.01) {
    magnitude <- "negligible"
  } else if (eta_sq < 0.06) {
    magnitude <- "small"
  } else if (eta_sq < 0.14) {
    magnitude <- "medium"
  } else {
    magnitude <- "large"
  }

  sprintf("η² = %.3f represents a %s effect (%.1f%% of variance explained)",
          eta_sq, magnitude, eta_sq * 100)
}


#' Compute Omega-Squared
#'
#' @description
#' Computes omega-squared (ω²), an unbiased estimator of effect size for ANOVA.
#' Omega-squared is less biased than eta-squared, especially for small samples.
#'
#' @param aov_model ANOVA model object
#' @param conf.level Confidence level (default 0.95)
#'
#' @return List with omega-squared value and interpretation
#'
#' @details
#' Omega-squared corrects for bias in eta-squared:
#'
#' ω² = (SS_effect - df_effect * MS_error) / (SS_total + MS_error)
#'
#' Properties:
#' - Less biased than η² for small samples
#' - Can be negative (interpreted as 0)
#' - Better population parameter estimate
#'
#' Interpretation uses same benchmarks as eta-squared.
#'
#' @examples
#' model <- aov(Sepal.Length ~ Species, data = iris)
#' compute_omega_squared(model)
#'
#' @export
compute_omega_squared <- function(aov_model, conf.level = 0.95) {

  # Extract sums of squares
  aov_summary <- summary(aov_model)
  ss_table <- aov_summary[[1]]

  ss_effect <- ss_table[1, "Sum Sq"]
  ss_total <- sum(ss_table[, "Sum Sq"])
  df_effect <- ss_table[1, "Df"]
  ms_error <- ss_table[nrow(ss_table), "Mean Sq"]

  # Compute omega-squared
  omega_sq <- (ss_effect - df_effect * ms_error) / (ss_total + ms_error)

  # Omega-squared can be negative; interpret as 0
  omega_sq <- max(0, omega_sq)

  # Interpretation
  interpretation <- interpret_omega_squared(omega_sq)

  result <- list(
    omega_squared = omega_sq,
    df_effect = df_effect,
    interpretation = interpretation
  )

  class(result) <- "descriptR_omega_squared"
  return(result)
}


#' Interpret Omega-Squared
#'
#' @param omega_sq Omega-squared value
#'
#' @return Character string with interpretation
#'
#' @keywords internal
#' @noRd
interpret_omega_squared <- function(omega_sq) {
  # Uses same benchmarks as eta-squared
  if (omega_sq < 0.01) {
    magnitude <- "negligible"
  } else if (omega_sq < 0.06) {
    magnitude <- "small"
  } else if (omega_sq < 0.14) {
    magnitude <- "medium"
  } else {
    magnitude <- "large"
  }

  sprintf("ω² = %.3f represents a %s effect (%.1f%% of variance explained)",
          omega_sq, magnitude, omega_sq * 100)
}


# Categorical Effect Sizes ====================================================

#' Compute Cramér's V
#'
#' @description
#' Computes Cramér's V effect size for chi-square test of independence.
#' Measures strength of association between categorical variables.
#'
#' @param x First categorical variable or contingency table
#' @param y Second categorical variable (optional if x is table)
#' @param bias_correct Logical, apply bias correction? (default TRUE)
#' @param conf.level Confidence level (default 0.95)
#'
#' @return List with Cramér's V value and interpretation
#'
#' @details
#' Cramér's V is normalized chi-square:
#'
#' V = sqrt(χ² / (n * min(r-1, c-1)))
#'
#' Where:
#' - χ² is the chi-square statistic
#' - n is sample size
#' - r, c are number of rows and columns
#'
#' ## Bias Correction (Bergsma, 2013)
#'
#' V_corrected = sqrt(max(0, φ² - (r-1)(c-1)/(n-1)))
#'
#' ## Interpretation (based on df)
#'
#' For df = 1 (2x2 table):
#' - Small: 0.10
#' - Medium: 0.30
#' - Large: 0.50
#'
#' For df = 2:
#' - Small: 0.07
#' - Medium: 0.21
#' - Large: 0.35
#'
#' @examples
#' compute_cramers_v(mtcars$vs, mtcars$am)
#'
#' @export
compute_cramers_v <- function(x, y = NULL, bias_correct = TRUE,
                               conf.level = 0.95) {

  # Create contingency table
  if (is.table(x) || is.matrix(x)) {
    tab <- as.table(x)
  } else if (!is.null(y)) {
    tab <- table(x, y)
  } else {
    stop("Provide either a table or two categorical variables", call. = FALSE)
  }

  # Perform chi-square test
  chi_result <- suppressWarnings(chisq.test(tab))
  chi_sq <- as.numeric(chi_result$statistic)
  n <- sum(tab)

  # Dimensions
  dims <- dim(tab)
  r <- dims[1]
  c <- dims[2]
  min_dim <- min(r - 1, c - 1)

  # Compute Cramér's V
  v <- sqrt(chi_sq / (n * min_dim))

  # Bias correction (Bergsma, 2013)
  if (bias_correct && n > 1) {
    phi_sq <- chi_sq / n
    v_corrected_sq <- max(0, phi_sq - ((r - 1) * (c - 1)) / (n - 1))
    v <- sqrt(v_corrected_sq / min_dim)
  }

  # Interpretation
  interpretation <- interpret_cramers_v(v, min_dim)

  result <- list(
    cramers_v = v,
    chi_squared = chi_sq,
    n = n,
    df = min_dim,
    bias_corrected = bias_correct,
    interpretation = interpretation
  )

  class(result) <- "descriptR_cramers_v"
  return(result)
}


#' Interpret Cramér's V
#'
#' @param v Cramér's V value
#' @param df Degrees of freedom (min dimension - 1)
#'
#' @return Character string with interpretation
#'
#' @keywords internal
#' @noRd
interpret_cramers_v <- function(v, df) {

  # Benchmarks based on df (Cohen, 1988; adjusted)
  if (df == 1) {
    if (v < 0.10) {
      magnitude <- "negligible"
    } else if (v < 0.30) {
      magnitude <- "small"
    } else if (v < 0.50) {
      magnitude <- "medium"
    } else {
      magnitude <- "large"
    }
  } else if (df == 2) {
    if (v < 0.07) {
      magnitude <- "negligible"
    } else if (v < 0.21) {
      magnitude <- "small"
    } else if (v < 0.35) {
      magnitude <- "medium"
    } else {
      magnitude <- "large"
    }
  } else {
    # df >= 3
    if (v < 0.06) {
      magnitude <- "negligible"
    } else if (v < 0.17) {
      magnitude <- "small"
    } else if (v < 0.29) {
      magnitude <- "medium"
    } else {
      magnitude <- "large"
    }
  }

  sprintf("Cramér's V = %.3f represents a %s effect", v, magnitude)
}


#' Compute Phi Coefficient
#'
#' @description
#' Computes phi coefficient for 2x2 contingency tables. Phi is equivalent
#' to Pearson's r for dichotomous variables.
#'
#' @param x First binary variable or 2x2 table
#' @param y Second binary variable (optional if x is table)
#'
#' @return List with phi coefficient and interpretation
#'
#' @details
#' Phi coefficient for 2x2 table:
#'
#' φ = (ad - bc) / sqrt((a+b)(c+d)(a+c)(b+d))
#'
#' Where a, b, c, d are the four cell frequencies.
#'
#' Properties:
#' - Range: -1 to +1
#' - Equivalent to Pearson r for binary variables
#' - Same as Cramér's V for 2x2 tables
#'
#' Interpretation uses Cohen's d benchmarks (since φ ≈ r).
#'
#' @examples
#' compute_phi(mtcars$vs, mtcars$am)
#'
#' @export
compute_phi <- function(x, y = NULL) {

  # Create 2x2 table
  if (is.table(x) || is.matrix(x)) {
    tab <- as.table(x)
  } else if (!is.null(y)) {
    tab <- table(x, y)
  } else {
    stop("Provide either a 2x2 table or two binary variables", call. = FALSE)
  }

  # Check dimensions
  if (!all(dim(tab) == c(2, 2))) {
    stop("Phi coefficient requires a 2x2 table", call. = FALSE)
  }

  # Extract cell frequencies
  a <- tab[1, 1]
  b <- tab[1, 2]
  c <- tab[2, 1]
  d <- tab[2, 2]

  # Compute phi
  numerator <- a * d - b * c
  denominator <- sqrt((a + b) * (c + d) * (a + c) * (b + d))

  phi <- numerator / denominator

  # Interpretation
  interpretation <- interpret_phi(phi)

  result <- list(
    phi = phi,
    observed = tab,
    interpretation = interpretation
  )

  class(result) <- "descriptR_phi"
  return(result)
}


#' Interpret Phi Coefficient
#'
#' @param phi Phi coefficient value
#'
#' @return Character string with interpretation
#'
#' @keywords internal
#' @noRd
interpret_phi <- function(phi) {
  abs_phi <- abs(phi)

  # Use correlation benchmarks
  if (abs_phi < 0.10) {
    magnitude <- "negligible"
  } else if (abs_phi < 0.30) {
    magnitude <- "small"
  } else if (abs_phi < 0.50) {
    magnitude <- "medium"
  } else {
    magnitude <- "large"
  }

  direction <- if (phi > 0) "positive" else if (phi < 0) "negative" else "no"

  sprintf("φ = %.3f represents a %s %s association",
          abs_phi, magnitude, direction)
}


# Correlation-Based Effect Sizes ==============================================

#' Convert Between Effect Sizes
#'
#' @description
#' Converts between different effect size metrics using standard formulas.
#'
#' @param value Effect size value to convert
#' @param from Source metric: "d", "r", "eta_sq", "odds_ratio"
#' @param to Target metric: "d", "r", "eta_sq", "odds_ratio"
#' @param n Sample size (required for some conversions)
#'
#' @return Converted effect size value
#'
#' @details
#' ## Conversion Formulas
#'
#' **Cohen's d ↔ Correlation r**:
#' - r = d / sqrt(d² + 4)
#' - d = 2r / sqrt(1 - r²)
#'
#' **Cohen's d ↔ Eta-squared**:
#' - η² = d² / (d² + 4)
#' - d = 2 * sqrt(η² / (1 - η²))
#'
#' **Odds Ratio ↔ Cohen's d**:
#' - d = log(OR) * sqrt(3) / π
#' - OR = exp(d * π / sqrt(3))
#'
#' @examples
#' # Convert d to r
#' convert_effect_size(0.5, from = "d", to = "r")
#'
#' # Convert r to d
#' convert_effect_size(0.3, from = "r", to = "d")
#'
#' @export
convert_effect_size <- function(value, from, to, n = NULL) {

  # Validate inputs
  valid_metrics <- c("d", "r", "eta_sq", "odds_ratio")
  if (!from %in% valid_metrics || !to %in% valid_metrics) {
    stop("Invalid metric. Choose from: d, r, eta_sq, odds_ratio", call. = FALSE)
  }

  if (from == to) {
    return(value)
  }

  # Convert to d first (as intermediate)
  d_value <- switch(from,
    "d" = value,
    "r" = 2 * value / sqrt(1 - value^2),
    "eta_sq" = 2 * sqrt(value / (1 - value)),
    "odds_ratio" = log(value) * sqrt(3) / pi
  )

  # Convert from d to target
  result <- switch(to,
    "d" = d_value,
    "r" = d_value / sqrt(d_value^2 + 4),
    "eta_sq" = d_value^2 / (d_value^2 + 4),
    "odds_ratio" = exp(d_value * pi / sqrt(3))
  )

  return(result)
}


#' Compute Comprehensive Effect Sizes
#'
#' @description
#' Computes all applicable effect sizes for a given analysis and provides
#' a summary with interpretations.
#'
#' @param data Data frame
#' @param outcome Outcome variable name
#' @param group Grouping variable name
#' @param type Type of analysis: "t.test", "anova", "chi.square"
#' @param conf.level Confidence level (default 0.95)
#'
#' @return List with all computed effect sizes
#'
#' @examples
#' # For two-group comparison
#' compute_all_effect_sizes(iris[iris$Species != "versicolor", ],
#'                          outcome = "Sepal.Length",
#'                          group = "Species",
#'                          type = "t.test")
#'
#' @export
compute_all_effect_sizes <- function(data, outcome, group,
                                      type = c("t.test", "anova", "chi.square"),
                                      conf.level = 0.95) {

  type <- match.arg(type)

  # Extract variables
  y <- data[[outcome]]
  grp <- data[[group]]

  # Remove NA
  complete_cases <- complete.cases(y, grp)
  y <- y[complete_cases]
  grp <- grp[complete_cases]

  result <- list()

  if (type == "t.test") {
    # Two-group comparison
    groups <- unique(grp)
    if (length(groups) != 2) {
      stop("t.test type requires exactly 2 groups", call. = FALSE)
    }

    x1 <- y[grp == groups[1]]
    x2 <- y[grp == groups[2]]

    # Cohen's d variants
    result$cohens_d <- compute_cohens_d(x1, x2, pooled = TRUE,
                                        conf.level = conf.level)
    result$hedges_g <- compute_cohens_d(x1, x2, pooled = TRUE,
                                        hedges = TRUE, conf.level = conf.level)
    result$glass_delta <- compute_cohens_d(x1, x2, pooled = FALSE,
                                           conf.level = conf.level)

    # Correlation equivalent
    r_equiv <- convert_effect_size(result$cohens_d$d, from = "d", to = "r")
    result$r_equivalent <- r_equiv

  } else if (type == "anova") {
    # Multi-group comparison
    aov_model <- aov(y ~ grp)

    result$eta_squared <- compute_eta_squared(aov_model, type = "classical",
                                               conf.level = conf.level)
    result$partial_eta_squared <- compute_eta_squared(aov_model, type = "partial",
                                                       conf.level = conf.level)
    result$omega_squared <- compute_omega_squared(aov_model, conf.level = conf.level)

  } else if (type == "chi.square") {
    # Categorical association
    tab <- table(y, grp)

    result$cramers_v <- compute_cramers_v(tab, bias_correct = TRUE,
                                           conf.level = conf.level)

    # Phi if 2x2
    if (all(dim(tab) == c(2, 2))) {
      result$phi <- compute_phi(tab)
    }
  }

  class(result) <- "descriptR_all_effect_sizes"
  return(result)
}


# Print Methods ===============================================================

#' @export
print.descriptR_cohens_d <- function(x, ...) {
  cat("\n")
  cat(x$method, "\n")
  cat(rep("=", 70), "\n", sep = "")
  cat(sprintf("\nCohen's d = %.3f\n", x$d))
  cat(sprintf("95%% CI: [%.3f, %.3f]\n", x$conf_int[1], x$conf_int[2]))
  cat(sprintf("SE = %.3f\n", x$se))
  cat(sprintf("\n%s\n", x$interpretation))
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")
  invisible(x)
}

#' @export
print.descriptR_eta_squared <- function(x, ...) {
  cat("\n")
  cat(x$method, "\n")
  cat(rep("=", 70), "\n", sep = "")
  cat(sprintf("\nη² = %.3f\n", x$eta_squared))
  cat(sprintf("95%% CI: [%.3f, %.3f]\n", x$conf_int[1], x$conf_int[2]))
  cat(sprintf("\n%s\n", x$interpretation))
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")
  invisible(x)
}

#' @export
print.descriptR_omega_squared <- function(x, ...) {
  cat("\n")
  cat("Omega-Squared\n")
  cat(rep("=", 70), "\n", sep = "")
  cat(sprintf("\nω² = %.3f\n", x$omega_squared))
  cat(sprintf("\n%s\n", x$interpretation))
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")
  invisible(x)
}

#' @export
print.descriptR_cramers_v <- function(x, ...) {
  cat("\n")
  cat("Cramér's V\n")
  cat(rep("=", 70), "\n", sep = "")
  cat(sprintf("\nCramér's V = %.3f\n", x$cramers_v))
  if (x$bias_corrected) {
    cat("(Bias-corrected)\n")
  }
  cat(sprintf("\n%s\n", x$interpretation))
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")
  invisible(x)
}

#' @export
print.descriptR_phi <- function(x, ...) {
  cat("\n")
  cat("Phi Coefficient\n")
  cat(rep("=", 70), "\n", sep = "")
  cat(sprintf("\nφ = %.3f\n", x$phi))
  cat(sprintf("\n%s\n", x$interpretation))
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")
  invisible(x)
}

#' @export
print.descriptR_all_effect_sizes <- function(x, ...) {
  cat("\n")
  cat("Comprehensive Effect Size Analysis\n")
  cat(rep("=", 70), "\n", sep = "")

  for (name in names(x)) {
    cat(sprintf("\n%s:\n", gsub("_", " ", tools::toTitleCase(name))))
    print(x[[name]])
  }

  invisible(x)
}
