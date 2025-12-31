#' Comprehensive Descriptive Statistics
#'
#' @description
#' The main function of the descriptR package. Automatically detects variable types
#' and computes appropriate descriptive statistics for all variables in a data frame.
#' Generates automated insights and produces publication-ready output.
#'
#' @param data A data frame to analyze
#' @param vars Character vector of variable names to analyze. If NULL (default), all variables.
#' @param group_by Optional grouping variable name for stratified analysis
#' @param na.rm Logical, remove NA values in calculations? (default TRUE)
#' @param trim Proportion to trim for trimmed mean (default 0)
#' @param conf.level Confidence level for intervals (default 0.95)
#' @param include_insights Logical, generate automated insights? (default TRUE)
#' @param include_plots Logical, generate plots? (default FALSE)
#' @param verbose Logical, print progress messages? (default TRUE)
#'
#' @return An object of class "descriptR" containing:
#' \itemize{
#'   \item \code{statistics}: Data frame with all descriptive statistics
#'   \item \code{variable_types}: Named vector of detected variable types
#'   \item \code{insights}: Character vector of automated insights
#'   \item \code{missing_summary}: Data frame summarizing missing data
#'   \item \code{group_info}: List of group-specific information (if grouped)
#'   \item \code{plots}: List of ggplot2 objects (if include_plots = TRUE)
#'   \item \code{call}: The function call
#' }
#'
#' @details
#' ## Variable Type Detection
#'
#' The function automatically detects variable types:
#' - **Continuous**: Numeric with >20 unique values → mean, SD, range, skewness, etc.
#' - **Discrete Numeric**: Numeric with 2-20 unique values → mean, median, mode, range
#' - **Categorical**: Factors/characters → frequencies, mode, entropy
#' - **Binary**: Two-level variables → proportion with confidence interval
#' - **DateTime**: Date/time variables → range, duration
#'
#' ## Statistics Computed
#'
#' **For Continuous Variables:**
#' - Central tendency: mean (with CI), median, trimmed mean
#' - Dispersion: SD, variance, IQR, MAD, range, CV
#' - Distribution: skewness, kurtosis
#' - Sample size: n, missing count/percentage
#'
#' **For Categorical Variables:**
#' - Frequencies and percentages
#' - Mode and mode percentage
#' - Entropy (diversity measure)
#' - Number of unique categories
#' - Missing count/percentage
#'
#' **For Binary Variables:**
#' - Proportion (with CI)
#' - Count of each level
#' - Percentages
#'
#' ## Automated Insights
#'
#' When `include_insights = TRUE`, generates plain-language findings such as:
#' - "Variable X is highly right-skewed (skewness = 2.3)"
#' - "15% of observations have missing data for variable A"
#' - "Variable B shows potential outliers"
#' - "The distribution of C is approximately normal"
#'
#' @examples
#' # Basic usage
#' describe_data(iris)
#'
#' # Specific variables
#' describe_data(iris, vars = c("Sepal.Length", "Species"))
#'
#' # Grouped analysis
#' describe_data(iris, group_by = "Species")
#'
#' # With plots and insights
#' result <- describe_data(mtcars,
#'                         vars = c("mpg", "cyl", "hp"),
#'                         include_insights = TRUE,
#'                         include_plots = TRUE)
#'
#' # Access components
#' result$statistics
#' result$insights
#' result$plots
#'
#' # Using clinical trial data
#' data(clinical_trial_data)
#' result <- describe_data(clinical_trial_data,
#'                         group_by = "treatment_group")
#'
#' @seealso
#' \code{\link{describe_numeric}}, \code{\link{describe_categorical}},
#' \code{\link{describe_grouped}}, \code{\link{print.descriptR}}
#'
#' @export
describe_data <- function(data,
                         vars = NULL,
                         group_by = NULL,
                         na.rm = TRUE,
                         trim = 0,
                         conf.level = 0.95,
                         include_insights = TRUE,
                         include_plots = FALSE,
                         verbose = TRUE) {

  # Validate inputs
  validate_data_frame(data, min_rows = 1)
  validate_trim(trim)
  validate_conf_level(conf.level)

  if (!is.null(group_by)) {
    validate_group_var(data, group_by, allow_null = TRUE)
  }

  # Store function call
  call <- match.call()

  # Select variables
  if (is.null(vars)) {
    vars <- names(data)
  } else {
    validate_variables(data, vars)
  }

  if (verbose) {
    message(sprintf("Analyzing %d variable(s)...", length(vars)))
  }

  # Detect variable types
  if (verbose) {
    message("Detecting variable types...")
  }

  var_types <- detect_all_types(data[vars])

  # Initialize results storage
  all_stats <- list()
  all_plots <- list()

  # Process each variable
  for (var in vars) {
    var_type <- var_types[var]
    x <- data[[var]]

    if (verbose) {
      message(sprintf("  %s (%s)", var, var_type))
    }

    # Compute statistics based on type
    stats <- compute_variable_stats(
      x = x,
      var_name = var,
      var_type = var_type,
      na.rm = na.rm,
      trim = trim,
      conf.level = conf.level
    )

    all_stats[[var]] <- stats

    # Generate plot if requested
    if (include_plots) {
      all_plots[[var]] <- create_variable_plot(x, var, var_type)
    }
  }

  # Combine statistics into data frame
  stats_df <- do.call(rbind, lapply(names(all_stats), function(var) {
    s <- all_stats[[var]]
    s$Variable <- var
    s$Type <- var_types[var]
    s
  }))
  rownames(stats_df) <- NULL

  # Reorder columns to put Variable and Type first
  stats_df <- stats_df[, c("Variable", "Type",
                           setdiff(names(stats_df), c("Variable", "Type")))]

  # Create missing data summary
  missing_summary <- create_missing_summary(data[vars])

  # Generate insights if requested
  insights <- NULL
  if (include_insights) {
    if (verbose) {
      message("Generating automated insights...")
    }
    insights <- generate_insights(
      data = data[vars],
      statistics = stats_df,
      variable_types = var_types
    )
  }

  # Handle grouped analysis
  group_info <- NULL
  if (!is.null(group_by)) {
    group_info <- create_group_summary(data, group_by)
  }

  # Create result object
  result <- list(
    statistics = stats_df,
    variable_types = var_types,
    insights = insights,
    missing_summary = missing_summary,
    group_info = group_info,
    plots = if (include_plots) all_plots else NULL,
    call = call
  )

  class(result) <- "descriptR"

  if (verbose) {
    message("Analysis complete!")
  }

  return(result)
}


#' Compute Statistics for a Single Variable
#'
#' @param x Vector
#' @param var_name Variable name
#' @param var_type Variable type
#' @param na.rm Remove NA?
#' @param trim Trim proportion
#' @param conf.level Confidence level
#'
#' @return Data frame with statistics
#'
#' @keywords internal
#' @noRd
compute_variable_stats <- function(x, var_name, var_type, na.rm, trim, conf.level) {

  n_total <- length(x)
  n_missing <- sum(is.na(x))
  n_valid <- n_total - n_missing
  pct_missing <- n_missing / n_total * 100

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  # Initialize stats data frame
  stats <- data.frame(
    N = n_total,
    N_Valid = n_valid,
    N_Missing = n_missing,
    Pct_Missing = pct_missing,
    stringsAsFactors = FALSE
  )

  # Compute type-specific statistics
  if (var_type %in% c("continuous", "discrete_numeric")) {
    # Numeric statistics
    mean_result <- compute_mean(x, trim = trim, na.rm = FALSE, conf.level = conf.level)

    stats$Mean <- mean_result$mean
    stats$Mean_CI_Lower <- mean_result$ci_lower
    stats$Mean_CI_Upper <- mean_result$ci_upper
    stats$SE <- mean_result$se

    stats$Median <- median(x, na.rm = FALSE)
    stats$SD <- compute_sd(x, na.rm = FALSE)
    stats$IQR <- compute_iqr(x, na.rm = FALSE)
    stats$Min <- min(x, na.rm = FALSE)
    stats$Max <- max(x, na.rm = FALSE)
    stats$Range <- max(x, na.rm = FALSE) - min(x, na.rm = FALSE)

    # Mode for discrete
    if (var_type == "discrete_numeric") {
      mode_val <- compute_mode(x, na.rm = FALSE)
      stats$Mode <- ifelse(length(mode_val) == 1, mode_val, NA_real_)
    }

    # Skewness and kurtosis
    if (n_valid >= 3) {
      stats$Skewness <- compute_skewness(x)
      stats$Kurtosis <- compute_kurtosis(x)
    }

  } else if (var_type %in% c("nominal", "ordinal", "binary")) {
    # Categorical statistics
    freq_table <- table(x, useNA = "no")
    n_unique <- length(freq_table)

    stats$N_Unique <- n_unique

    # Mode
    if (n_unique > 0) {
      mode_val <- names(freq_table)[which.max(freq_table)]
      mode_freq <- max(freq_table)
      stats$Mode <- mode_val
      stats$Mode_Freq <- mode_freq
      stats$Mode_Pct <- mode_freq / n_valid * 100

      # Entropy and Herfindahl
      if (n_unique > 1) {
        stats$Entropy <- compute_entropy(x, na.rm = FALSE)
        stats$Herfindahl <- compute_herfindahl(x, na.rm = FALSE)
      }
    }

    # For binary, also compute proportion
    if (var_type == "binary" && n_unique == 2) {
      # Convert to 0/1 if needed
      if (is.logical(x)) {
        x_binary <- as.integer(x)
      } else if (is.numeric(x)) {
        x_binary <- x
      } else {
        x_binary <- as.integer(factor(x)) - 1
      }

      prop <- mean(x_binary, na.rm = FALSE)
      stats$Proportion <- prop

      # CI for proportion
      if (n_valid > 0) {
        prop_test <- stats::prop.test(sum(x_binary), n_valid, correct = FALSE)
        stats$Prop_CI_Lower <- prop_test$conf.int[1]
        stats$Prop_CI_Upper <- prop_test$conf.int[2]
      }
    }

  } else if (var_type == "datetime") {
    # Date/time statistics
    if (n_valid > 0) {
      stats$Min_Date <- as.character(min(x, na.rm = FALSE))
      stats$Max_Date <- as.character(max(x, na.rm = FALSE))
      stats$Range_Days <- as.numeric(difftime(max(x, na.rm = FALSE),
                                              min(x, na.rm = FALSE),
                                              units = "days"))
    }
  }

  return(stats)
}


#' Create Missing Data Summary
#'
#' @param data Data frame
#'
#' @return Data frame with missing data information
#'
#' @keywords internal
#' @noRd
create_missing_summary <- function(data) {

  missing_df <- data.frame(
    Variable = names(data),
    N_Missing = sapply(data, function(x) sum(is.na(x))),
    Pct_Missing = sapply(data, function(x) sum(is.na(x)) / length(x) * 100),
    stringsAsFactors = FALSE
  )

  # Sort by percentage missing (descending)
  missing_df <- missing_df[order(missing_df$Pct_Missing, decreasing = TRUE), ]
  rownames(missing_df) <- NULL

  return(missing_df)
}


#' Create Group Summary
#'
#' @param data Data frame
#' @param group_by Grouping variable
#'
#' @return List with group information
#'
#' @keywords internal
#' @noRd
create_group_summary <- function(data, group_by) {

  groups <- data[[group_by]]
  group_table <- table(groups, useNA = "ifany")

  list(
    variable = group_by,
    n_groups = length(group_table),
    groups = names(group_table),
    counts = as.numeric(group_table),
    proportions = as.numeric(group_table) / sum(group_table)
  )
}


#' Create Variable Plot
#'
#' @param x Vector
#' @param var_name Variable name
#' @param var_type Variable type
#'
#' @return A ggplot2 object or NULL
#'
#' @keywords internal
#' @noRd
create_variable_plot <- function(x, var_name, var_type) {

  # Remove NA for plotting
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NULL)
  }

  if (var_type %in% c("continuous", "discrete_numeric")) {
    # Histogram
    p <- ggplot2::ggplot(data.frame(x = x), ggplot2::aes(x = x)) +
      ggplot2::geom_histogram(bins = 30, fill = "#4292C6", alpha = 0.8, color = "white") +
      ggplot2::labs(title = sprintf("Distribution: %s", var_name),
                   x = var_name, y = "Count") +
      ggplot2::theme_minimal()

  } else if (var_type %in% c("nominal", "ordinal", "binary")) {
    # Bar plot
    freq_table <- as.data.frame(table(x))
    names(freq_table) <- c("Category", "Count")

    p <- ggplot2::ggplot(freq_table, ggplot2::aes(x = Category, y = Count)) +
      ggplot2::geom_bar(stat = "identity", fill = "#4292C6", alpha = 0.8) +
      ggplot2::labs(title = sprintf("Frequencies: %s", var_name),
                   x = var_name, y = "Count") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  } else {
    p <- NULL
  }

  return(p)
}


#' Compute Skewness
#'
#' @param x Numeric vector
#'
#' @return Skewness value
#'
#' @keywords internal
#' @noRd
compute_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) {
    return(NA_real_)
  }

  m <- mean(x)
  s <- sd(x)

  if (s == 0) {
    return(NA_real_)
  }

  skew <- (sum((x - m)^3) / n) / s^3
  return(skew)
}


#' Compute Kurtosis
#'
#' @param x Numeric vector
#'
#' @return Kurtosis value (excess kurtosis)
#'
#' @keywords internal
#' @noRd
compute_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 4) {
    return(NA_real_)
  }

  m <- mean(x)
  s <- sd(x)

  if (s == 0) {
    return(NA_real_)
  }

  # Excess kurtosis (subtract 3 so normal distribution has kurtosis = 0)
  kurt <- (sum((x - m)^4) / n) / s^4 - 3
  return(kurt)
}


#' Print Method for descriptR Objects
#'
#' @param x An object of class "descriptR"
#' @param digits Number of digits to print (default 2)
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR <- function(x, digits = 2, ...) {

  cat("\n")
  cat("Descriptive Statistics Summary\n")
  cat(rep("=", 80), "\n", sep = "")
  cat(sprintf("Variables analyzed: %d\n", nrow(x$statistics)))
  cat(sprintf("Total observations: %d\n", x$statistics$N[1]))

  # Variable type breakdown
  cat("\nVariable types:\n")
  type_table <- table(x$variable_types)
  for (type in names(type_table)) {
    cat(sprintf("  %-20s: %d\n", type, type_table[type]))
  }

  # Missing data summary
  total_missing <- sum(x$missing_summary$N_Missing)
  if (total_missing > 0) {
    cat(sprintf("\nMissing data: %d values across all variables\n", total_missing))
    vars_with_missing <- sum(x$missing_summary$N_Missing > 0)
    cat(sprintf("Variables with missing data: %d\n", vars_with_missing))
  } else {
    cat("\nNo missing data\n")
  }

  # Group info
  if (!is.null(x$group_info)) {
    cat(sprintf("\nGrouped by: %s (%d groups)\n",
               x$group_info$variable,
               x$group_info$n_groups))
  }

  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nStatistics:\n")

  # Print statistics table (formatted)
  print_stats <- x$statistics
  numeric_cols <- sapply(print_stats, is.numeric)
  print_stats[numeric_cols] <- lapply(print_stats[numeric_cols], round, digits = digits)

  print(print_stats, row.names = FALSE)

  # Print insights if available
  if (!is.null(x$insights) && length(x$insights) > 0) {
    cat("\n")
    cat(rep("-", 80), "\n", sep = "")
    cat("\nAutomated Insights:\n")
    for (i in seq_along(x$insights)) {
      cat(sprintf("%d. %s\n", i, x$insights[i]))
    }
  }

  cat("\n")
  cat(rep("=", 80), "\n", sep = "")

  invisible(x)
}
