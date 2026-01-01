#' Generate Automated Insights from Descriptive Statistics
#'
#' @description
#' Analyzes descriptive statistics and generates plain-language insights
#' about the data, prioritizing high-impact and noteworthy findings.
#'
#' @param data Original data frame
#' @param statistics Statistics data frame from describe_data()
#' @param variable_types Named vector of variable types
#' @param max_insights Maximum number of insights to generate (default 10)
#'
#' @return Character vector of insights
#'
#' @keywords internal
#' @export
generate_insights <- function(data,
                             statistics,
                             variable_types,
                             max_insights = 10) {

  insights <- character(0)

  # 1. Missing data insights
  insights <- c(insights, generate_missing_insights(statistics))

  # 2. Distribution insights (skewness, normality)
  insights <- c(insights, generate_distribution_insights(statistics))

  # 3. Variability insights
  insights <- c(insights, generate_variability_insights(statistics))

  # 4. Categorical insights
  insights <- c(insights, generate_categorical_insights(statistics, variable_types))

  # 5. Range and outlier insights
  insights <- c(insights, generate_range_insights(statistics, data))

  # 6. General data quality insights
  insights <- c(insights, generate_quality_insights(statistics, variable_types))

  # Limit to max_insights
  if (length(insights) > max_insights) {
    insights <- insights[1:max_insights]
  }

  return(insights)
}


#' Generate Missing Data Insights
#'
#' @param statistics Statistics data frame
#'
#' @return Character vector of insights
#'
#' @keywords internal
#' @noRd
generate_missing_insights <- function(statistics) {

  insights <- character(0)

  # Check for high missingness
  high_missing <- statistics[statistics$Pct_Missing > 20, ]
  if (nrow(high_missing) > 0) {
    for (i in 1:min(3, nrow(high_missing))) {
      var <- high_missing$Variable[i]
      pct <- round(high_missing$Pct_Missing[i], 1)
      insights <- c(insights,
                   sprintf("Variable '%s' has %.1f%% missing data, which may impact analysis quality",
                          var, pct))
    }
  }

  # Check for complete data
  if (all(statistics$Pct_Missing == 0)) {
    insights <- c(insights, "All variables have complete data with no missing values")
  }

  # Check for moderate missingness
  moderate_missing <- statistics[statistics$Pct_Missing > 5 & statistics$Pct_Missing <= 20, ]
  if (nrow(moderate_missing) > 0 && nrow(high_missing) == 0) {
    insights <- c(insights,
                 sprintf("%d variable%s %s 5-20%% missing data",
                        nrow(moderate_missing),
                        ifelse(nrow(moderate_missing) == 1, "", "s"),
                        ifelse(nrow(moderate_missing) == 1, "has", "have")))
  }

  return(insights)
}


#' Generate Distribution Insights
#'
#' @param statistics Statistics data frame
#'
#' @return Character vector of insights
#'
#' @keywords internal
#' @noRd
generate_distribution_insights <- function(statistics) {

  insights <- character(0)

  # Check skewness for continuous variables
  if ("Skewness" %in% names(statistics)) {
    # Highly right-skewed
    right_skewed <- statistics[!is.na(statistics$Skewness) & statistics$Skewness > 1.5, ]
    if (nrow(right_skewed) > 0) {
      for (i in 1:min(2, nrow(right_skewed))) {
        var <- right_skewed$Variable[i]
        skew <- round(right_skewed$Skewness[i], 2)
        insights <- c(insights,
                     sprintf("'%s' is highly right-skewed (skewness = %.2f), indicating concentration of values below the mean",
                            var, skew))
      }
    }

    # Highly left-skewed
    left_skewed <- statistics[!is.na(statistics$Skewness) & statistics$Skewness < -1.5, ]
    if (nrow(left_skewed) > 0) {
      for (i in 1:min(2, nrow(left_skewed))) {
        var <- left_skewed$Variable[i]
        skew <- round(left_skewed$Skewness[i], 2)
        insights <- c(insights,
                     sprintf("'%s' is highly left-skewed (skewness = %.2f), with most values above the mean",
                            var, skew))
      }
    }

    # Approximately normal
    normal_dist <- statistics[!is.na(statistics$Skewness) &
                              abs(statistics$Skewness) < 0.5 &
                              !is.na(statistics$Kurtosis) &
                              abs(statistics$Kurtosis) < 0.5, ]
    if (nrow(normal_dist) > 0) {
      if (nrow(normal_dist) == 1) {
        insights <- c(insights,
                     sprintf("'%s' appears approximately normally distributed",
                            normal_dist$Variable[1]))
      } else if (nrow(normal_dist) <= 3) {
        insights <- c(insights,
                     sprintf("Variables %s appear approximately normally distributed",
                            paste(sprintf("'%s'", normal_dist$Variable), collapse = ", ")))
      }
    }
  }

  # Check kurtosis
  if ("Kurtosis" %in% names(statistics)) {
    # Heavy tails
    heavy_tails <- statistics[!is.na(statistics$Kurtosis) & statistics$Kurtosis > 2, ]
    if (nrow(heavy_tails) > 0) {
      for (i in 1:min(2, nrow(heavy_tails))) {
        var <- heavy_tails$Variable[i]
        kurt <- round(heavy_tails$Kurtosis[i], 2)
        insights <- c(insights,
                     sprintf("'%s' has heavy tails (kurtosis = %.2f), suggesting potential outliers",
                            var, kurt))
      }
    }
  }

  return(insights)
}


#' Generate Variability Insights
#'
#' @param statistics Statistics data frame
#'
#' @return Character vector of insights
#'
#' @keywords internal
#' @noRd
generate_variability_insights <- function(statistics) {

  insights <- character(0)

  # Check SD for continuous variables
  if ("SD" %in% names(statistics) && "Mean" %in% names(statistics)) {

    # Calculate CV where possible
    stats_with_cv <- statistics[!is.na(statistics$SD) &
                                !is.na(statistics$Mean) &
                                statistics$Mean != 0, ]

    if (nrow(stats_with_cv) > 0) {
      stats_with_cv$CV <- stats_with_cv$SD / abs(stats_with_cv$Mean) * 100

      # Low variability (CV < 10%)
      low_var <- stats_with_cv[stats_with_cv$CV < 10, ]
      if (nrow(low_var) > 0) {
        for (i in 1:min(2, nrow(low_var))) {
          var <- low_var$Variable[i]
          cv <- round(low_var$CV[i], 1)
          insights <- c(insights,
                       sprintf("'%s' shows low variability (CV = %.1f%%), suggesting homogeneity",
                              var, cv))
        }
      }

      # High variability (CV > 50%)
      high_var <- stats_with_cv[stats_with_cv$CV > 50, ]
      if (nrow(high_var) > 0) {
        for (i in 1:min(2, nrow(high_var))) {
          var <- high_var$Variable[i]
          cv <- round(high_var$CV[i], 1)
          insights <- c(insights,
                       sprintf("'%s' shows high variability (CV = %.1f%%), indicating heterogeneity",
                              var, cv))
        }
      }
    }
  }

  return(insights)
}


#' Generate Categorical Insights
#'
#' @param statistics Statistics data frame
#' @param variable_types Variable types
#'
#' @return Character vector of insights
#'
#' @keywords internal
#' @noRd
generate_categorical_insights <- function(statistics, variable_types) {

  insights <- character(0)

  # Filter categorical variables
  cat_vars <- names(variable_types)[variable_types %in% c("nominal", "ordinal", "binary")]
  cat_stats <- statistics[statistics$Variable %in% cat_vars, ]

  if (nrow(cat_stats) == 0) {
    return(insights)
  }

  # Check for highly concentrated distributions (low entropy/high Herfindahl)
  if ("Entropy" %in% names(cat_stats)) {
    # Estimate expected entropy assuming uniform distribution
    cat_stats$Max_Entropy <- log(cat_stats$N_Unique)
    cat_stats$Entropy_Ratio <- cat_stats$Entropy / cat_stats$Max_Entropy

    # Low diversity (entropy < 50% of maximum)
    low_diversity <- cat_stats[!is.na(cat_stats$Entropy_Ratio) &
                               cat_stats$Entropy_Ratio < 0.5, ]
    if (nrow(low_diversity) > 0) {
      for (i in 1:min(2, nrow(low_diversity))) {
        var <- low_diversity$Variable[i]
        mode_pct <- round(low_diversity$Mode_Pct[i], 1)
        mode_val <- low_diversity$Mode[i]
        insights <- c(insights,
                     sprintf("'%s' is highly concentrated: %.1f%% are '%s'",
                            var, mode_pct, mode_val))
      }
    }
  }

  # Check for high cardinality
  if ("N_Unique" %in% names(cat_stats)) {
    high_card <- cat_stats[!is.na(cat_stats$N_Unique) &
                           cat_stats$N_Unique > 10 &
                           cat_stats$N_Unique / cat_stats$N_Valid > 0.3, ]
    if (nrow(high_card) > 0) {
      for (i in 1:min(2, nrow(high_card))) {
        var <- high_card$Variable[i]
        n_cat <- high_card$N_Unique[i]
        insights <- c(insights,
                     sprintf("'%s' has high cardinality with %d unique categories",
                            var, n_cat))
      }
    }
  }

  # Binary variables - check for imbalance
  binary_stats <- cat_stats[variable_types[cat_stats$Variable] == "binary", ]
  if (nrow(binary_stats) > 0 && "Mode_Pct" %in% names(binary_stats)) {
    imbalanced <- binary_stats[!is.na(binary_stats$Mode_Pct) &
                               (binary_stats$Mode_Pct > 80 | binary_stats$Mode_Pct < 20), ]
    if (nrow(imbalanced) > 0) {
      for (i in 1:min(2, nrow(imbalanced))) {
        var <- imbalanced$Variable[i]
        pct <- round(imbalanced$Mode_Pct[i], 1)
        insights <- c(insights,
                     sprintf("'%s' is imbalanced with %.1f%% in one category",
                            var, pct))
      }
    }
  }

  return(insights)
}


#' Generate Range and Outlier Insights
#'
#' @param statistics Statistics data frame
#' @param data Original data frame
#'
#' @return Character vector of insights
#'
#' @keywords internal
#' @noRd
generate_range_insights <- function(statistics, data) {

  insights <- character(0)

  # Check for potential outliers using IQR method
  if (all(c("SD", "IQR") %in% names(statistics))) {

    numeric_stats <- statistics[!is.na(statistics$SD) & !is.na(statistics$IQR), ]

    for (i in 1:nrow(numeric_stats)) {
      var <- numeric_stats$Variable[i]
      x <- data[[var]]
      x <- x[!is.na(x)]

      if (length(x) < 4) next

      # IQR method for outliers
      q1 <- quantile(x, 0.25)
      q3 <- quantile(x, 0.75)
      iqr <- q3 - q1
      lower_fence <- q1 - 1.5 * iqr
      upper_fence <- q3 + 1.5 * iqr

      outliers <- x[x < lower_fence | x > upper_fence]
      n_outliers <- length(outliers)

      if (n_outliers > 0 && n_outliers / length(x) > 0.05) {
        pct <- round(n_outliers / length(x) * 100, 1)
        insights <- c(insights,
                     sprintf("'%s' has potential outliers: %d values (%.1f%%) beyond 1.5×IQR",
                            var, n_outliers, pct))

        # Only report first 2 outlier insights
        if (length(grep("potential outliers", insights)) >= 2) {
          break
        }
      }
    }
  }

  return(insights)
}


#' Generate Data Quality Insights
#'
#' @param statistics Statistics data frame
#' @param variable_types Variable types
#'
#' @return Character vector of insights
#'
#' @keywords internal
#' @noRd
generate_quality_insights <- function(statistics, variable_types) {

  insights <- character(0)

  # Sample size assessment
  n <- statistics$N[1]
  if (n < 30) {
    insights <- c(insights,
                 sprintf("Small sample size (n = %d) may limit statistical power and generalizability",
                        n))
  } else if (n > 1000) {
    insights <- c(insights,
                 sprintf("Large sample size (n = %d) provides good statistical power",
                        n))
  }

  # Variable type distribution
  type_counts <- table(variable_types)

  if (sum(type_counts[c("continuous", "discrete_numeric")]) == length(variable_types)) {
    insights <- c(insights, "Dataset consists entirely of numeric variables")
  } else if (sum(type_counts[c("nominal", "ordinal", "binary")]) == length(variable_types)) {
    insights <- c(insights, "Dataset consists entirely of categorical variables")
  } else {
    insights <- c(insights,
                 sprintf("Dataset contains mixed variable types: %d numeric, %d categorical",
                        sum(type_counts[c("continuous", "discrete_numeric")]),
                        sum(type_counts[c("nominal", "ordinal", "binary")])))
  }

  return(insights)
}


#' Interpret Effect Size
#'
#' @description
#' Provides plain-language interpretation of effect sizes
#'
#' @param effect_size Numeric effect size value
#' @param type Type of effect size (cohen_d, eta_squared, cramers_v, correlation)
#'
#' @return Character string with interpretation
#'
#' @examples
#' interpret_effect_size(0.2, "cohen_d")  # "small"
#' interpret_effect_size(0.5, "cohen_d")  # "medium"
#' interpret_effect_size(0.8, "cohen_d")  # "large"
#'
#' @export
interpret_effect_size <- function(effect_size, type = c("cohen_d", "eta_squared",
                                                        "cramers_v", "correlation")) {

  type <- match.arg(type)
  abs_effect <- abs(effect_size)

  interpretation <- switch(
    type,

    "cohen_d" = {
      if (abs_effect < 0.2) "negligible"
      else if (abs_effect < 0.5) "small"
      else if (abs_effect < 0.8) "medium"
      else if (abs_effect < 1.3) "large"
      else "very large"
    },

    "eta_squared" = {
      if (abs_effect < 0.01) "negligible"
      else if (abs_effect < 0.06) "small"
      else if (abs_effect < 0.14) "medium"
      else "large"
    },

    "cramers_v" = {
      if (abs_effect < 0.1) "negligible"
      else if (abs_effect < 0.3) "small"
      else if (abs_effect < 0.5) "medium"
      else "large"
    },

    "correlation" = {
      if (abs_effect < 0.1) "negligible"
      else if (abs_effect < 0.3) "small"
      else if (abs_effect < 0.5) "medium"
      else if (abs_effect < 0.7) "large"
      else "very large"
    }
  )

  return(interpretation)
}


#' Interpret P-Value
#'
#' @description
#' Provides plain-language interpretation of p-values
#'
#' @param p_value Numeric p-value
#' @param alpha Significance level (default 0.05)
#'
#' @return Character string with interpretation
#'
#' @examples
#' interpret_p_value(0.001)  # "highly significant"
#' interpret_p_value(0.03)   # "significant"
#' interpret_p_value(0.08)   # "not significant"
#'
#' @export
interpret_p_value <- function(p_value, alpha = 0.05) {

  if (is.na(p_value)) {
    return("not available")
  }

  if (p_value < 0.001) {
    "highly significant (p < 0.001)"
  } else if (p_value < 0.01) {
    sprintf("very significant (p = %.3f)", p_value)
  } else if (p_value < alpha) {
    sprintf("significant (p = %.3f)", p_value)
  } else if (p_value < 0.10) {
    sprintf("marginally significant (p = %.3f)", p_value)
  } else {
    sprintf("not significant (p = %.3f)", p_value)
  }
}
