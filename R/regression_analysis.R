#' Regression Analysis Module
#'
#' @description
#' Comprehensive regression analysis including linear, logistic, and multiple
#' regression with automatic diagnostics and assumption checking.
#'
#' @name regression_analysis
NULL


#' Perform Regression Analysis
#'
#' @description
#' Performs comprehensive regression analysis with automatic model selection,
#' diagnostics, assumption checking, and interpretation.
#'
#' @param data Data frame containing variables
#' @param outcome Character name of outcome/dependent variable
#' @param predictors Character vector of predictor/independent variable names.
#'   NULL = all numeric variables except outcome
#' @param type Regression type: "linear", "logistic", "poisson", "auto" (default)
#' @param interactions Include interaction terms? Default FALSE
#' @param polynomial Include polynomial terms? Default FALSE
#' @param step_wise Perform stepwise variable selection? Default FALSE
#' @param diagnostics Include diagnostic plots? Default TRUE
#' @param ... Additional arguments
#'
#' @return descriptR_regression object with:
#' \itemize{
#'   \item model - Fitted model object
#'   \item coefficients - Model coefficients with p-values
#'   \item model_fit - R-squared, AIC, BIC statistics
#'   \item diagnostics - Residual analysis, influence measures
#'   \item assumptions - Assumption test results
#'   \item predictions - Fitted values and residuals
#'   \item insights - Automated interpretation
#'   \item plots - Diagnostic plots (if requested)
#' }
#'
#' @examples
#' # Linear regression
#' result <- perform_regression_analysis(mtcars, outcome = "mpg",
#'                                       predictors = c("wt", "hp"))
#'
#' # Automatic type detection
#' result <- perform_regression_analysis(iris, outcome = "Sepal.Length")
#'
#' # Logistic regression
#' result <- perform_regression_analysis(mtcars, outcome = "am",
#'                                       type = "logistic")
#'
#' @export
perform_regression_analysis <- function(data,
                                        outcome,
                                        predictors = NULL,
                                        type = c("auto", "linear", "logistic", "poisson"),
                                        interactions = FALSE,
                                        polynomial = FALSE,
                                        step_wise = FALSE,
                                        diagnostics = TRUE,
                                        ...) {

  type <- match.arg(type)

  # Validate inputs
  if (!outcome %in% names(data)) {
    stop("Outcome variable not found in data", call. = FALSE)
  }

  # Auto-select predictors if NULL
  if (is.null(predictors)) {
    predictors <- setdiff(names(data)[sapply(data, is.numeric)], outcome)
  }

  # Auto-detect regression type
  if (type == "auto") {
    outcome_var <- data[[outcome]]
    if (is.factor(outcome_var) || length(unique(outcome_var)) == 2) {
      type <- "logistic"
    } else if (all(outcome_var >= 0) && all(outcome_var == floor(outcome_var))) {
      type <- "poisson"
    } else {
      type <- "linear"
    }
    message(sprintf("Auto-detected regression type: %s", type))
  }

  # Build formula
  formula_str <- build_regression_formula(outcome, predictors, interactions, polynomial)

  # Fit model
  model <- fit_regression_model(data, formula_str, type)

  # Extract coefficients
  coeffs <- extract_coefficients(model, type)

  # Model fit statistics
  fit_stats <- calculate_model_fit(model, data, type)

  # Diagnostics
  diag_results <- NULL
  diag_plots <- NULL
  if (diagnostics) {
    diag_results <- run_diagnostics(model, data, type)
    diag_plots <- create_diagnostic_plots(model, data, type)
  }

  # Assumption tests
  assumptions <- test_assumptions(model, data, type)

  # Predictions
  predictions <- list(
    fitted = stats::fitted(model),
    residuals = stats::residuals(model)
  )

  # Stepwise selection if requested
  if (step_wise) {
    model <- perform_stepwise(model, data, type)
  }

  # Generate insights
  insights <- generate_regression_insights(model, coeffs, fit_stats, assumptions, type)

  # Create result object
  result <- structure(
    list(
      model = model,
      coefficients = coeffs,
      model_fit = fit_stats,
      diagnostics = diag_results,
      assumptions = assumptions,
      predictions = predictions,
      insights = insights,
      plots = diag_plots,
      metadata = list(
        outcome = outcome,
        predictors = predictors,
        type = type,
        n_obs = nrow(data),
        formula = formula_str
      )
    ),
    class = c("descriptR_regression", "descriptR_result")
  )

  return(result)
}


#' Build Regression Formula
#' @keywords internal
#' @noRd
build_regression_formula <- function(outcome, predictors, interactions, polynomial) {

  if (length(predictors) == 0) {
    stop("At least one predictor variable required", call. = FALSE)
  }

  # Basic formula
  formula_str <- paste(outcome, "~", paste(predictors, collapse = " + "))

  # Add interactions
  if (interactions && length(predictors) >= 2) {
    formula_str <- paste(outcome, "~", paste(predictors, collapse = " * "))
  }

  # Add polynomial terms
  if (polynomial) {
    poly_terms <- paste0("I(", predictors, "^2)")
    formula_str <- paste(formula_str, "+", paste(poly_terms, collapse = " + "))
  }

  return(stats::as.formula(formula_str))
}


#' Fit Regression Model
#' @keywords internal
#' @noRd
fit_regression_model <- function(data, formula, type) {

  model <- switch(type,
    "linear" = stats::lm(formula, data = data),
    "logistic" = {
      # Convert outcome to binary if needed
      outcome_var <- all.vars(formula)[1]
      if (!is.factor(data[[outcome_var]])) {
        data[[outcome_var]] <- factor(data[[outcome_var]])
      }
      stats::glm(formula, data = data, family = stats::binomial())
    },
    "poisson" = stats::glm(formula, data = data, family = stats::poisson())
  )

  return(model)
}


#' Extract Model Coefficients
#' @keywords internal
#' @noRd
extract_coefficients <- function(model, type) {

  coef_summary <- summary(model)$coefficients

  coef_df <- data.frame(
    term = rownames(coef_summary),
    estimate = coef_summary[, 1],
    std_error = coef_summary[, 2],
    statistic = coef_summary[, 3],
    p_value = coef_summary[, 4],
    stringsAsFactors = FALSE
  )

  # Add confidence intervals
  ci <- tryCatch(
    stats::confint(model),
    error = function(e) NULL
  )

  if (!is.null(ci)) {
    coef_df$ci_lower <- ci[, 1]
    coef_df$ci_upper <- ci[, 2]
  }

  # Add significance stars
  coef_df$significance <- cut(coef_df$p_value,
                               breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                               labels = c("***", "**", "*", ".", ""),
                               include.lowest = TRUE)

  return(coef_df)
}


#' Calculate Model Fit Statistics
#' @keywords internal
#' @noRd
calculate_model_fit <- function(model, data, type) {

  n <- nrow(data)
  k <- length(stats::coef(model))

  fit_stats <- list(
    n_obs = n,
    n_predictors = k - 1,
    AIC = stats::AIC(model),
    BIC = stats::BIC(model),
    log_likelihood = stats::logLik(model)[1]
  )

  if (type == "linear") {
    summ <- summary(model)
    fit_stats$r_squared <- summ$r.squared
    fit_stats$adj_r_squared <- summ$adj.r.squared
    fit_stats$rmse <- sqrt(mean(stats::residuals(model)^2))
    fit_stats$f_statistic <- summ$fstatistic[1]
    fit_stats$f_p_value <- stats::pf(summ$fstatistic[1],
                                     summ$fstatistic[2],
                                     summ$fstatistic[3],
                                     lower.tail = FALSE)
  } else if (type %in% c("logistic", "poisson")) {
    # Pseudo R-squared for GLM
    null_deviance <- model$null.deviance
    residual_deviance <- model$deviance
    fit_stats$mcfadden_r2 <- 1 - (residual_deviance / null_deviance)
    fit_stats$deviance = residual_deviance
  }

  return(fit_stats)
}


#' Run Regression Diagnostics
#' @keywords internal
#' @noRd
run_diagnostics <- function(model, data, type) {

  diagnostics <- list()

  if (type == "linear") {
    # Influence measures
    influence <- stats::influence.measures(model)
    diagnostics$influential_points <- which(apply(influence$is.inf, 1, any))

    # Cook's distance
    cooks_d <- stats::cooks.distance(model)
    diagnostics$cooks_distance <- data.frame(
      observation = seq_along(cooks_d),
      cooks_d = cooks_d,
      influential = cooks_d > 4 / length(cooks_d)
    )

    # Leverage
    leverage <- stats::hatvalues(model)
    diagnostics$high_leverage <- which(leverage > 2 * length(stats::coef(model)) / nrow(data))
  }

  # Residual statistics
  residuals <- stats::residuals(model)
  diagnostics$residual_stats <- list(
    mean = mean(residuals),
    sd = stats::sd(residuals),
    min = min(residuals),
    max = max(residuals),
    outliers = which(abs(scale(residuals)) > 3)
  )

  return(diagnostics)
}


#' Test Regression Assumptions
#' @keywords internal
#' @noRd
test_assumptions <- function(model, data, type) {

  assumptions <- list()

  if (type == "linear") {
    # Normality of residuals (Shapiro-Wilk)
    residuals <- stats::residuals(model)
    if (length(residuals) <= 5000) {
      shapiro_test <- stats::shapiro.test(residuals)
      assumptions$normality <- list(
        test = "Shapiro-Wilk",
        statistic = shapiro_test$statistic,
        p_value = shapiro_test$p.value,
        conclusion = ifelse(shapiro_test$p.value > 0.05,
                           "Residuals appear normally distributed",
                           "Residuals may not be normally distributed")
      )
    }

    # Homoscedasticity (Breusch-Pagan test if available)
    # For now, simple variance check
    fitted_vals <- stats::fitted(model)
    groups <- cut(fitted_vals, breaks = 3)
    variance_by_group <- tapply(residuals, groups, stats::var)
    assumptions$homoscedasticity <- list(
      variance_range = range(variance_by_group, na.rm = TRUE),
      conclusion = ifelse(max(variance_by_group, na.rm = TRUE) / min(variance_by_group, na.rm = TRUE) < 3,
                         "Homoscedasticity assumption appears met",
                         "Possible heteroscedasticity detected")
    )

    # Multicollinearity (VIF)
    if (length(stats::coef(model)) > 2) {
      vif <- calculate_vif(model)
      assumptions$multicollinearity <- list(
        vif = vif,
        max_vif = max(vif, na.rm = TRUE),
        conclusion = ifelse(max(vif, na.rm = TRUE) < 5,
                           "No serious multicollinearity detected",
                           "Multicollinearity may be present (VIF > 5)")
      )
    }
  }

  return(assumptions)
}


#' Calculate Variance Inflation Factor
#' @keywords internal
#' @noRd
calculate_vif <- function(model) {

  # Extract design matrix (excluding intercept)
  X <- stats::model.matrix(model)[, -1, drop = FALSE]

  if (ncol(X) < 2) return(NULL)

  vif_values <- numeric(ncol(X))
  names(vif_values) <- colnames(X)

  for (i in seq_len(ncol(X))) {
    # Regress each predictor on all others
    y_i <- X[, i]
    X_i <- X[, -i, drop = FALSE]

    if (ncol(X_i) > 0) {
      r2 <- summary(stats::lm(y_i ~ X_i))$r.squared
      vif_values[i] <- 1 / (1 - r2)
    }
  }

  return(vif_values)
}


#' Create Diagnostic Plots
#' @keywords internal
#' @noRd
create_diagnostic_plots <- function(model, data, type) {

  plots <- list()

  if (type == "linear") {
    # 1. Residuals vs Fitted
    plots$residuals_vs_fitted <- create_residual_plot(model)

    # 2. QQ Plot
    plots$qq_plot <- create_qq_plot(model)

    # 3. Scale-Location
    plots$scale_location <- create_scale_location_plot(model)

    # 4. Cook's Distance
    plots$cooks_distance <- create_cooks_plot(model)
  }

  return(plots)
}


#' Generate Regression Insights
#' @keywords internal
#' @noRd
generate_regression_insights <- function(model, coeffs, fit_stats, assumptions, type) {

  insights <- c()

  # Model fit insight
  if (type == "linear") {
    if (!is.null(fit_stats$adj_r_squared)) {
      r2_pct <- round(fit_stats$adj_r_squared * 100, 1)
      insights <- c(insights,
                   sprintf("Model explains %s%% of variance in outcome (Adjusted R² = %.3f)",
                          r2_pct, fit_stats$adj_r_squared))
    }
  } else {
    insights <- c(insights,
                 sprintf("McFadden's pseudo R² = %.3f", fit_stats$mcfadden_r2))
  }

  # Significant predictors
  sig_predictors <- coeffs$term[coeffs$p_value < 0.05 & coeffs$term != "(Intercept)"]
  if (length(sig_predictors) > 0) {
    insights <- c(insights,
                 sprintf("Significant predictors: %s", paste(sig_predictors, collapse = ", ")))
  }

  # Strongest predictor
  coef_no_intercept <- coeffs[coeffs$term != "(Intercept)", ]
  if (nrow(coef_no_intercept) > 0) {
    strongest_idx <- which.max(abs(coef_no_intercept$estimate))
    strongest <- coef_no_intercept[strongest_idx, ]
    direction <- ifelse(strongest$estimate > 0, "positive", "negative")
    insights <- c(insights,
                 sprintf("Strongest predictor: %s (%s effect, β = %.3f)",
                        strongest$term, direction, strongest$estimate))
  }

  # Assumptions
  if (!is.null(assumptions$normality)) {
    insights <- c(insights, assumptions$normality$conclusion)
  }
  if (!is.null(assumptions$homoscedasticity)) {
    insights <- c(insights, assumptions$homoscedasticity$conclusion)
  }
  if (!is.null(assumptions$multicollinearity)) {
    insights <- c(insights, assumptions$multicollinearity$conclusion)
  }

  return(insights)
}


#' Perform Stepwise Regression
#' @keywords internal
#' @noRd
perform_stepwise <- function(model, data, type) {

  if (type == "linear") {
    # Backward stepwise using AIC
    model <- stats::step(model, direction = "backward", trace = 0)
  }

  return(model)
}


# Placeholder plot functions (would need ggplot2 integration)
create_residual_plot <- function(model) {
  return(NULL)  # Implement with ggplot2
}

create_qq_plot <- function(model) {
  return(NULL)  # Implement with ggplot2
}

create_scale_location_plot <- function(model) {
  return(NULL)  # Implement with ggplot2
}

create_cooks_plot <- function(model) {
  return(NULL)  # Implement with ggplot2
}


#' Print Method for Regression Results
#' @export
print.descriptR_regression <- function(x, ...) {
  cat("\nRegression Analysis Results\n")
  cat(rep("=", 50), "\n", sep = "")
  cat(sprintf("Type: %s regression\n", x$metadata$type))
  cat(sprintf("Outcome: %s\n", x$metadata$outcome))
  cat(sprintf("Predictors: %s\n", paste(x$metadata$predictors, collapse = ", ")))
  cat(sprintf("N = %d observations\n\n", x$metadata$n_obs))

  cat("Model Fit:\n")
  for (stat_name in names(x$model_fit)) {
    cat(sprintf("  %s: %.4f\n", stat_name, x$model_fit[[stat_name]]))
  }

  cat("\nCoefficients:\n")
  print(x$coefficients, row.names = FALSE)

  cat("\nInsights:\n")
  for (insight in x$insights) {
    cat(sprintf("  - %s\n", insight))
  }

  cat(rep("=", 50), "\n", sep = "")
  invisible(x)
}
