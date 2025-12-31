#' Advanced Statistical Models
#'
#' @description
#' Provides interfaces to advanced statistical models including factor analysis,
#' generalized linear models, and model diagnostics.
#'
#' @name advanced_models
NULL


#' Exploratory Factor Analysis
#'
#' @description
#' Performs exploratory factor analysis with automatic factor retention and rotation.
#'
#' @param data Data frame
#' @param vars Variables to include (NULL for all numeric)
#' @param n_factors Number of factors (NULL for auto-detection)
#' @param rotation Rotation method: "varimax", "promax", "oblimin", "none"
#' @param min_loading Minimum loading to display (default 0.3)
#' @param scores Compute factor scores? (default TRUE)
#'
#' @return Object of class "descriptR_efa" with factor loadings and diagnostics
#'
#' @details
#' ## Factor Retention Methods
#'
#' **Kaiser Criterion**: Eigenvalue > 1
#' **Scree Plot**: Elbow in eigenvalue plot
#' **Parallel Analysis**: Compare to random data
#' **Variance Explained**: Cumulative variance threshold
#'
#' ## Rotation Methods
#'
#' **Varimax**: Orthogonal, maximizes variance of loadings
#' **Promax**: Oblique, allows correlated factors
#' **Oblimin**: Oblique, minimizes cross-products
#'
#' @examples
#' # Auto-detect number of factors
#' perform_efa(mtcars)
#'
#' # Specify 3 factors with promax rotation
#' perform_efa(iris, vars = 1:4, n_factors = 3, rotation = "promax")
#'
#' @export
perform_efa <- function(data,
                        vars = NULL,
                        n_factors = NULL,
                        rotation = c("varimax", "promax", "oblimin", "none"),
                        min_loading = 0.3,
                        scores = TRUE) {

  rotation <- match.arg(rotation)

  # Select numeric variables
  if (is.null(vars)) {
    numeric_cols <- sapply(data, is.numeric)
    vars <- names(data)[numeric_cols]
  } else {
    if (is.numeric(vars)) {
      vars <- names(data)[vars]
    }
    validate_variables(data, vars)
  }

  if (length(vars) < 3) {
    stop("Factor analysis requires at least 3 variables", call. = FALSE)
  }

  # Extract data
  fa_data <- data[, vars, drop = FALSE]
  fa_data <- fa_data[complete.cases(fa_data), ]

  if (nrow(fa_data) < length(vars) * 2) {
    stop("Insufficient complete cases for factor analysis (need n > 2p)",
         call. = FALSE)
  }

  # Correlation matrix
  cor_matrix <- cor(fa_data, use = "complete.obs")

  # Determine number of factors
  if (is.null(n_factors)) {
    eigenvalues <- eigen(cor_matrix)$values
    n_factors <- sum(eigenvalues > 1)  # Kaiser criterion
    n_factors <- max(1, min(n_factors, length(vars) - 1))
  }

  # Perform factor analysis
  fa_result <- stats::factanal(
    fa_data,
    factors = n_factors,
    rotation = if (rotation == "none") "none" else rotation,
    scores = if (scores) "regression" else "none"
  )

  # Extract loadings
  loadings_matrix <- fa_result$loadings[]

  # Compute communalities
  communalities <- rowSums(loadings_matrix^2)

  # Variance explained
  var_explained <- colSums(loadings_matrix^2) / nrow(loadings_matrix)
  cumvar_explained <- cumsum(var_explained)

  # Filter loadings by threshold
  loadings_clean <- loadings_matrix
  loadings_clean[abs(loadings_clean) < min_loading] <- NA

  # Interpretation
  interpretation <- generate_efa_interpretation(
    n_factors, var_explained, communalities, fa_result
  )

  # Recommendations
  recommendations <- generate_efa_recommendations(
    n_factors, length(vars), var_explained, communalities
  )

  result <- list(
    loadings = loadings_clean,
    loadings_full = loadings_matrix,
    communalities = communalities,
    uniquenesses = fa_result$uniquenesses,
    variance_explained = var_explained,
    cumulative_variance = cumvar_explained,
    n_factors = n_factors,
    n_variables = length(vars),
    variables = vars,
    rotation = rotation,
    min_loading = min_loading,
    factor_scores = if (scores) fa_result$scores else NULL,
    test_statistic = fa_result$statistic,
    p_value = fa_result$PVAL,
    interpretation = interpretation,
    recommendations = recommendations
  )

  class(result) <- "descriptR_efa"
  return(result)
}


#' Generate EFA Interpretation
#'
#' @keywords internal
#' @noRd
generate_efa_interpretation <- function(n_factors, var_explained, communalities, fa_result) {

  interpretation <- character()

  interpretation <- c(interpretation,
    sprintf("Exploratory Factor Analysis:"),
    sprintf("  • %d factors extracted", n_factors),
    sprintf("  • Total variance explained: %.1f%%",
           sum(var_explained) * 100),
    sprintf("  • Mean communality: %.3f", mean(communalities)),
    "")

  # Factor-specific variance
  for (i in seq_len(n_factors)) {
    interpretation <- c(interpretation,
      sprintf("Factor %d: %.1f%% of variance",
             i, var_explained[i] * 100))
  }

  interpretation <- c(interpretation, "")

  # Model fit
  if (!is.null(fa_result$PVAL)) {
    if (fa_result$PVAL < 0.05) {
      interpretation <- c(interpretation,
        sprintf("Model fit: χ² = %.2f, p = %.4f (poor fit - consider more factors)",
               fa_result$statistic, fa_result$PVAL))
    } else {
      interpretation <- c(interpretation,
        sprintf("Model fit: χ² = %.2f, p = %.4f (adequate fit)",
               fa_result$statistic, fa_result$PVAL))
    }
  }

  return(interpretation)
}


#' Generate EFA Recommendations
#'
#' @keywords internal
#' @noRd
generate_efa_recommendations <- function(n_factors, n_vars, var_explained, communalities) {

  recommendations <- character()

  # Variance explained
  total_var <- sum(var_explained) * 100

  if (total_var >= 70) {
    recommendations <- c(recommendations,
      sprintf("• Good variance explained (%.1f%%). Factors capture most information.", total_var))
  } else if (total_var >= 50) {
    recommendations <- c(recommendations,
      sprintf("• Moderate variance explained (%.1f%%). Consider additional factors.", total_var))
  } else {
    recommendations <- c(recommendations,
      sprintf("• Low variance explained (%.1f%%). May need more factors or different approach.", total_var))
  }

  # Communalities
  low_comm <- sum(communalities < 0.3)
  if (low_comm > 0) {
    recommendations <- c(recommendations,
      sprintf("• %d variable(s) with low communalities (<0.3). Consider removing.", low_comm))
  }

  # General recommendations
  recommendations <- c(recommendations,
    "",
    "Next steps:",
    "  1. Examine loadings to interpret factor meaning",
    "  2. Name factors based on high-loading variables",
    "  3. Check communalities (should be >0.3)",
    "  4. Consider alternative rotations if interpretation unclear",
    "  5. Use factor scores in subsequent analyses")

  return(recommendations)
}


#' Generalized Linear Model
#'
#' @description
#' Fits generalized linear models with automatic family selection and diagnostics.
#'
#' @param formula Model formula
#' @param data Data frame
#' @param family Family/link function: "auto", "gaussian", "binomial", "poisson", "gamma"
#' @param diagnostics Run model diagnostics? (default TRUE)
#'
#' @return Object of class "descriptR_glm" with model results and diagnostics
#'
#' @details
#' ## Families
#'
#' **Gaussian**: Continuous outcome (identity link)
#' **Binomial**: Binary outcome (logit link)
#' **Poisson**: Count outcome (log link)
#' **Gamma**: Positive continuous with skew (log link)
#'
#' ## Auto-Selection
#' Based on outcome variable characteristics:
#' - Binary (0/1 or TRUE/FALSE) → Binomial
#' - Count (integers ≥ 0) → Poisson
#' - Continuous → Gaussian
#'
#' @examples
#' # Logistic regression (auto-detected)
#' perform_glm(vs ~ mpg + hp, data = mtcars)
#'
#' # Poisson regression for counts
#' perform_glm(carb ~ mpg + hp, data = mtcars, family = "poisson")
#'
#' @export
perform_glm <- function(formula,
                        data,
                        family = c("auto", "gaussian", "binomial", "poisson", "gamma"),
                        diagnostics = TRUE) {

  family_arg <- match.arg(family)

  # Extract outcome variable
  outcome_name <- all.vars(formula)[1]
  outcome <- data[[outcome_name]]

  # Auto-detect family
  if (family_arg == "auto") {
    family_arg <- detect_glm_family(outcome)
    message(sprintf("Auto-detected family: %s", family_arg))
  }

  # Convert to family object
  family_obj <- switch(family_arg,
    "gaussian" = stats::gaussian(),
    "binomial" = stats::binomial(),
    "poisson" = stats::poisson(),
    "gamma" = stats::Gamma()
  )

  # Fit model
  model <- stats::glm(formula, data = data, family = family_obj)

  # Summary
  model_summary <- summary(model)

  # Coefficients table
  coef_table <- as.data.frame(model_summary$coefficients)
  coef_table$Variable <- rownames(coef_table)
  coef_table <- coef_table[, c("Variable", names(coef_table)[-ncol(coef_table)])]
  rownames(coef_table) <- NULL

  # Model fit statistics
  fit_stats <- list(
    aic = model$aic,
    deviance = model$deviance,
    null_deviance = model$null.deviance,
    df_residual = model$df.residual,
    df_null = model$df.null
  )

  # Pseudo R-squared (McFadden's)
  pseudo_r2 <- 1 - (model$deviance / model$null.deviance)

  # Diagnostics
  diag_results <- NULL
  if (diagnostics) {
    diag_results <- run_glm_diagnostics(model, family_arg)
  }

  # Interpretation
  interpretation <- generate_glm_interpretation(
    family_arg, pseudo_r2, coef_table, fit_stats
  )

  result <- list(
    model = model,
    coefficients = coef_table,
    fit_statistics = fit_stats,
    pseudo_r2 = pseudo_r2,
    family = family_arg,
    formula = formula,
    diagnostics = diag_results,
    interpretation = interpretation
  )

  class(result) <- "descriptR_glm"
  return(result)
}


#' Detect GLM Family
#'
#' @keywords internal
#' @noRd
detect_glm_family <- function(outcome) {

  # Binary
  unique_vals <- unique(outcome[!is.na(outcome)])
  if (length(unique_vals) == 2 &&
      all(unique_vals %in% c(0, 1, TRUE, FALSE))) {
    return("binomial")
  }

  # Count data (non-negative integers)
  if (all(outcome >= 0, na.rm = TRUE) &&
      all(outcome == round(outcome), na.rm = TRUE)) {
    return("poisson")
  }

  # Default to gaussian
  return("gaussian")
}


#' Run GLM Diagnostics
#'
#' @keywords internal
#' @noRd
run_glm_diagnostics <- function(model, family) {

  # Residuals
  resid_deviance <- residuals(model, type = "deviance")
  resid_pearson <- residuals(model, type = "pearson")

  # Influence measures
  influence <- stats::influence.measures(model)

  # Overdispersion check (for Poisson/binomial)
  overdispersion <- NULL
  if (family %in% c("poisson", "binomial")) {
    deviance <- sum(resid_deviance^2)
    df <- model$df.residual
    overdispersion <- deviance / df

    if (overdispersion > 1.5) {
      overdispersion_note <- sprintf("WARNING: Overdispersion detected (%.2f). Consider negative binomial or quasi-poisson.",
                                    overdispersion)
    } else {
      overdispersion_note <- sprintf("No significant overdispersion (%.2f)", overdispersion)
    }
  }

  list(
    residuals_deviance = resid_deviance,
    residuals_pearson = resid_pearson,
    overdispersion = overdispersion,
    overdispersion_note = overdispersion_note,
    influence = influence
  )
}


#' Generate GLM Interpretation
#'
#' @keywords internal
#' @noRd
generate_glm_interpretation <- function(family, pseudo_r2, coef_table, fit_stats) {

  interpretation <- character()

  interpretation <- c(interpretation,
    sprintf("Generalized Linear Model (%s family):", family),
    sprintf("  • Pseudo R² (McFadden): %.3f", pseudo_r2),
    sprintf("  • AIC: %.2f", fit_stats$aic),
    sprintf("  • Deviance: %.2f (df = %d)", fit_stats$deviance, fit_stats$df_residual),
    "")

  # Significant predictors
  sig_predictors <- coef_table[coef_table[, ncol(coef_table)] < 0.05, "Variable"]
  sig_predictors <- sig_predictors[sig_predictors != "(Intercept)"]

  if (length(sig_predictors) > 0) {
    interpretation <- c(interpretation,
      sprintf("Significant predictors (p < 0.05): %s",
             paste(sig_predictors, collapse = ", ")))
  } else {
    interpretation <- c(interpretation,
      "No significant predictors at α = 0.05")
  }

  return(interpretation)
}


#' Model Comparison
#'
#' @description
#' Compares multiple models using AIC, BIC, and likelihood ratio tests.
#'
#' @param ... Model objects to compare
#' @param criterion Comparison criterion: "aic", "bic", "both"
#'
#' @return Data frame with model comparison statistics
#'
#' @examples
#' model1 <- lm(mpg ~ hp, data = mtcars)
#' model2 <- lm(mpg ~ hp + wt, data = mtcars)
#' compare_models(model1, model2)
#'
#' @export
compare_models <- function(..., criterion = c("aic", "bic", "both")) {

  criterion <- match.arg(criterion)
  models <- list(...)

  if (length(models) < 2) {
    stop("Need at least 2 models to compare", call. = FALSE)
  }

  # Extract model names
  model_names <- as.character(match.call(expand.dots = FALSE)$...)

  # Comparison table
  comparison <- data.frame(
    Model = model_names,
    stringsAsFactors = FALSE
  )

  # AIC
  if (criterion %in% c("aic", "both")) {
    comparison$AIC <- sapply(models, AIC)
    comparison$Delta_AIC <- comparison$AIC - min(comparison$AIC)
  }

  # BIC
  if (criterion %in% c("bic", "both")) {
    comparison$BIC <- sapply(models, BIC)
    comparison$Delta_BIC <- comparison$BIC - min(comparison$BIC)
  }

  # Degrees of freedom
  comparison$df <- sapply(models, function(m) {
    if (inherits(m, "glm")) {
      m$df.residual
    } else {
      m$df.residual
    }
  })

  # Best model
  if (criterion == "aic") {
    best_idx <- which.min(comparison$AIC)
  } else if (criterion == "bic") {
    best_idx <- which.min(comparison$BIC)
  } else {
    best_idx <- which.min(comparison$AIC)
  }

  comparison$Best <- ""
  comparison$Best[best_idx] <- "★"

  class(comparison) <- c("descriptR_model_comparison", "data.frame")
  return(comparison)
}


# Print Methods ===============================================================

#' @export
print.descriptR_efa <- function(x, digits = 3, ...) {

  cat("\n")
  cat("Exploratory Factor Analysis\n")
  cat(rep("=", 80), "\n", sep = "")

  cat(sprintf("\nFactors: %d\n", x$n_factors))
  cat(sprintf("Variables: %d\n", x$n_variables))
  cat(sprintf("Rotation: %s\n", x$rotation))

  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nFactor Loadings (loadings < %.2f suppressed):\n\n", x$min_loading)

  loadings_df <- as.data.frame(x$loadings)
  loadings_df$Variable <- rownames(x$loadings)
  loadings_df <- loadings_df[, c("Variable", names(loadings_df)[-ncol(loadings_df)])]

  print(loadings_df, row.names = FALSE, digits = digits, na.print = "")

  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nVariance Explained:\n\n")

  var_df <- data.frame(
    Factor = paste0("Factor", 1:x$n_factors),
    Variance = x$variance_explained,
    Cumulative = x$cumulative_variance
  )
  print(var_df, row.names = FALSE, digits = digits)

  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nInterpretation:\n\n")
  cat(paste(x$interpretation, collapse = "\n"))
  cat("\n")

  cat("\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("\n")

  invisible(x)
}


#' @export
print.descriptR_glm <- function(x, digits = 3, ...) {

  cat("\n")
  cat(sprintf("Generalized Linear Model (%s)\n", x$family))
  cat(rep("=", 80), "\n", sep = "")

  cat("\nFormula:", deparse(x$formula), "\n")

  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nCoefficients:\n\n")

  print(x$coefficients, row.names = FALSE, digits = digits)

  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nModel Fit:\n\n")

  cat(sprintf("  Pseudo R²: %.4f\n", x$pseudo_r2))
  cat(sprintf("  AIC: %.2f\n", x$fit_statistics$aic))
  cat(sprintf("  Deviance: %.2f (df = %d)\n",
             x$fit_statistics$deviance, x$fit_statistics$df_residual))

  if (!is.null(x$diagnostics$overdispersion_note)) {
    cat(sprintf("\n  %s\n", x$diagnostics$overdispersion_note))
  }

  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nInterpretation:\n\n")
  cat(paste(x$interpretation, collapse = "\n"))
  cat("\n")

  cat("\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("\n")

  invisible(x)
}


#' @export
print.descriptR_model_comparison <- function(x, ...) {

  cat("\n")
  cat("Model Comparison\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  print.data.frame(x, row.names = FALSE)

  cat("\n")
  cat("Note: ★ indicates best model\n")
  cat("      Lower AIC/BIC is better\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  invisible(x)
}
