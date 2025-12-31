#' Describe Categorical Variables
#'
#' @description
#' Computes comprehensive descriptive statistics for categorical variables
#' including frequencies, mode, entropy, and concentration measures.
#'
#' @param data A data frame
#' @param vars Character vector of variable names. If NULL, all categorical variables are analyzed.
#' @param include_plots Logical, generate bar plots? (default FALSE)
#' @param max_categories Maximum number of categories to display (default 20)
#' @param sort_by How to sort categories: "frequency" (default), "alphabetical", or "none"
#'
#' @return An object of class "descriptR_categorical" containing:
#' \itemize{
#'   \item \code{frequencies}: List of frequency tables for each variable
#'   \item \code{summary}: Data frame with summary statistics
#'   \item \code{plots}: List of ggplot2 objects (if include_plots = TRUE)
#'   \item \code{n_vars}: Number of variables analyzed
#' }
#'
#' @details
#' For each categorical variable, computes:
#' - Frequency counts and percentages
#' - Mode (most common value) and its frequency
#' - Entropy (measure of diversity): H = -sum(p * log(p))
#' - Herfindahl index (concentration): H = sum(p^2)
#' - Number of unique categories
#' - Missing data count and percentage
#'
#' **Entropy** ranges from 0 (no diversity, one category) to log(k) where k is
#' the number of categories. Higher entropy indicates more even distribution.
#'
#' **Herfindahl index** ranges from 1/k (perfect equality) to 1 (complete concentration).
#' Lower values indicate more diversity.
#'
#' @examples
#' # Single variable
#' describe_categorical(iris, vars = "Species")
#'
#' # Multiple variables
#' describe_categorical(mtcars, vars = c("cyl", "gear", "carb"))
#'
#' # All categorical variables
#' data(clinical_trial_data)
#' describe_categorical(clinical_trial_data)
#'
#' # With plots
#' result <- describe_categorical(iris, vars = "Species", include_plots = TRUE)
#' result$plots$Species
#'
#' @export
describe_categorical <- function(data,
                                 vars = NULL,
                                 include_plots = FALSE,
                                 max_categories = 20,
                                 sort_by = c("frequency", "alphabetical", "none")) {

  # Validate inputs
  validate_data_frame(data)
  sort_by <- match.arg(sort_by)

  # If vars not specified, detect categorical variables
  if (is.null(vars)) {
    var_types <- get_variables_by_type(data)
    vars <- c(var_types$nominal, var_types$ordinal, var_types$binary)

    if (length(vars) == 0) {
      stop("No categorical variables found in data", call. = FALSE)
    }
  } else {
    validate_variables(data, vars)
  }

  # Initialize results
  frequencies <- list()
  summary_list <- list()
  plots <- list()

  # Analyze each variable
  for (var in vars) {
    x <- data[[var]]

    # Compute frequency table
    freq_result <- compute_frequencies(x,
                                      max_categories = max_categories,
                                      sort_by = sort_by)
    frequencies[[var]] <- freq_result

    # Compute summary statistics
    summary_stats <- compute_categorical_summary(x, var_name = var)
    summary_list[[var]] <- summary_stats

    # Generate plot if requested
    if (include_plots) {
      plots[[var]] <- plot_frequencies(freq_result, var_name = var)
    }
  }

  # Combine summaries into data frame
  summary_df <- do.call(rbind, summary_list)
  rownames(summary_df) <- NULL

  # Create result object
  result <- list(
    frequencies = frequencies,
    summary = summary_df,
    plots = if (include_plots) plots else NULL,
    n_vars = length(vars)
  )

  class(result) <- "descriptR_categorical"
  return(result)
}


#' Compute Frequency Table
#'
#' @param x A vector (categorical or numeric)
#' @param max_categories Maximum categories to show (default 20)
#' @param sort_by How to sort: "frequency", "alphabetical", or "none"
#'
#' @return Data frame with counts, percentages, and cumulative percentages
#'
#' @keywords internal
#' @noRd
compute_frequencies <- function(x, max_categories = 20, sort_by = "frequency") {

  # Count frequencies including NA
  freq_table <- table(x, useNA = "ifany")
  n_total <- length(x)
  n_missing <- sum(is.na(x))

  # Create data frame
  freq_df <- data.frame(
    Category = names(freq_table),
    Count = as.numeric(freq_table),
    Percentage = as.numeric(freq_table) / n_total * 100,
    stringsAsFactors = FALSE
  )

  # Handle NA category name
  freq_df$Category[is.na(freq_df$Category)] <- "<NA>"

  # Sort
  if (sort_by == "frequency") {
    freq_df <- freq_df[order(freq_df$Count, decreasing = TRUE), ]
  } else if (sort_by == "alphabetical") {
    freq_df <- freq_df[order(freq_df$Category), ]
  }

  # Truncate if too many categories
  if (nrow(freq_df) > max_categories) {
    other_count <- sum(freq_df$Count[(max_categories + 1):nrow(freq_df)])
    freq_df <- freq_df[1:max_categories, ]

    # Add "Other" category
    freq_df <- rbind(freq_df, data.frame(
      Category = sprintf("Other (%d categories)", nrow(freq_df) - max_categories),
      Count = other_count,
      Percentage = other_count / n_total * 100,
      stringsAsFactors = FALSE
    ))
  }

  # Add cumulative percentage
  freq_df$Cumulative_Pct <- cumsum(freq_df$Percentage)

  # Add attributes
  attr(freq_df, "n_total") <- n_total
  attr(freq_df, "n_missing") <- n_missing
  attr(freq_df, "n_valid") <- n_total - n_missing

  return(freq_df)
}


#' Compute Categorical Summary Statistics
#'
#' @param x A vector
#' @param var_name Variable name
#'
#' @return Data frame with summary statistics
#'
#' @keywords internal
#' @noRd
compute_categorical_summary <- function(x, var_name) {

  n_total <- length(x)
  n_missing <- sum(is.na(x))
  x_clean <- x[!is.na(x)]
  n_valid <- length(x_clean)

  # Basic counts
  if (n_valid == 0) {
    return(data.frame(
      Variable = var_name,
      N = n_total,
      N_Valid = 0,
      N_Missing = n_missing,
      Pct_Missing = 100,
      N_Unique = 0,
      Mode = NA_character_,
      Mode_Freq = NA_integer_,
      Mode_Pct = NA_real_,
      Entropy = NA_real_,
      Herfindahl = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  # Unique categories
  n_unique <- length(unique(x_clean))

  # Mode
  freq_table <- table(x_clean)
  mode_val <- names(freq_table)[which.max(freq_table)]
  mode_freq <- max(freq_table)
  mode_pct <- mode_freq / n_valid * 100

  # Entropy: H = -sum(p * log(p))
  proportions <- as.numeric(freq_table) / n_valid
  entropy <- -sum(proportions * log(proportions))

  # Herfindahl index: H = sum(p^2)
  herfindahl <- sum(proportions^2)

  # Create summary
  data.frame(
    Variable = var_name,
    N = n_total,
    N_Valid = n_valid,
    N_Missing = n_missing,
    Pct_Missing = n_missing / n_total * 100,
    N_Unique = n_unique,
    Mode = mode_val,
    Mode_Freq = mode_freq,
    Mode_Pct = mode_pct,
    Entropy = entropy,
    Herfindahl = herfindahl,
    stringsAsFactors = FALSE
  )
}


#' Plot Frequency Distribution
#'
#' @param freq_df Frequency data frame from compute_frequencies
#' @param var_name Variable name for title
#'
#' @return A ggplot2 object
#'
#' @keywords internal
#' @noRd
plot_frequencies <- function(freq_df, var_name) {

  # Remove NA row for plotting
  freq_df <- freq_df[freq_df$Category != "<NA>", ]

  if (nrow(freq_df) == 0) {
    return(NULL)
  }

  # Create plot
  p <- ggplot2::ggplot(freq_df, ggplot2::aes(x = reorder(Category, -Count), y = Count)) +
    ggplot2::geom_bar(stat = "identity", fill = "#4292C6", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", Percentage)),
                      vjust = -0.5, size = 3) +
    ggplot2::labs(
      title = sprintf("Frequency Distribution: %s", var_name),
      x = var_name,
      y = "Count"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(face = "bold")
    )

  return(p)
}


#' Print Method for descriptR_categorical
#'
#' @param x An object of class "descriptR_categorical"
#' @param ... Additional arguments (ignored)
#'
#' @export
print.descriptR_categorical <- function(x, ...) {

  cat("\n")
  cat("Categorical Variable Summary\n")
  cat(rep("=", 70), "\n", sep = "")
  cat(sprintf("Number of variables: %d\n\n", x$n_vars))

  # Print summary table
  print(x$summary, row.names = FALSE)

  cat("\n")
  cat(rep("-", 70), "\n", sep = "")
  cat("\nFrequency Tables\n")
  cat(rep("-", 70), "\n", sep = "")

  # Print frequency tables
  for (var_name in names(x$frequencies)) {
    cat(sprintf("\n%s:\n", var_name))
    freq_df <- x$frequencies[[var_name]]

    # Format for printing
    print_df <- freq_df
    print_df$Percentage <- sprintf("%.2f%%", print_df$Percentage)
    print_df$Cumulative_Pct <- sprintf("%.2f%%", print_df$Cumulative_Pct)

    print(print_df, row.names = FALSE)

    n_total <- attr(freq_df, "n_total")
    n_missing <- attr(freq_df, "n_missing")
    if (n_missing > 0) {
      cat(sprintf("  Missing: %d (%.2f%%)\n", n_missing, n_missing/n_total*100))
    }
  }

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")

  invisible(x)
}


#' Compute Entropy
#'
#' @description
#' Computes Shannon entropy, a measure of diversity/uncertainty.
#'
#' @param x A vector (categorical or numeric)
#' @param na.rm Logical, remove NA values? (default TRUE)
#' @param base Logarithm base (default e, use 2 for bits)
#'
#' @return Entropy value
#'
#' @details
#' Shannon entropy: H = -sum(p * log(p))
#'
#' Ranges from 0 (no diversity) to log(k) where k is the number of categories.
#' Higher values indicate more even distribution across categories.
#'
#' @examples
#' # Uniform distribution (high entropy)
#' compute_entropy(rep(1:4, each = 25))
#'
#' # Skewed distribution (low entropy)
#' compute_entropy(c(rep(1, 90), rep(2, 10)))
#'
#' @export
compute_entropy <- function(x, na.rm = TRUE, base = exp(1)) {

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(NA_real_)
  }

  # Compute proportions
  freq_table <- table(x)
  proportions <- as.numeric(freq_table) / length(x)

  # Remove zero proportions
  proportions <- proportions[proportions > 0]

  # Compute entropy
  -sum(proportions * log(proportions, base = base))
}


#' Compute Herfindahl Index
#'
#' @description
#' Computes Herfindahl-Hirschman Index, a measure of concentration.
#'
#' @param x A vector (categorical or numeric)
#' @param na.rm Logical, remove NA values? (default TRUE)
#' @param normalized Logical, return normalized version? (default FALSE)
#'
#' @return Herfindahl index value
#'
#' @details
#' Herfindahl index: H = sum(p^2)
#'
#' Ranges from 1/k (perfect equality) to 1 (complete concentration).
#' Lower values indicate more diversity.
#'
#' If normalized = TRUE, returns (H - 1/k) / (1 - 1/k), which ranges from 0 to 1.
#'
#' @examples
#' # Equal distribution (low concentration)
#' compute_herfindahl(rep(1:4, each = 25))
#'
#' # High concentration
#' compute_herfindahl(c(rep(1, 90), rep(2, 10)))
#'
#' @export
compute_herfindahl <- function(x, na.rm = TRUE, normalized = FALSE) {

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(NA_real_)
  }

  # Compute proportions
  freq_table <- table(x)
  proportions <- as.numeric(freq_table) / length(x)

  # Compute Herfindahl index
  h <- sum(proportions^2)

  # Normalize if requested
  if (normalized) {
    k <- length(freq_table)
    if (k > 1) {
      h <- (h - 1/k) / (1 - 1/k)
    }
  }

  return(h)
}


#' Chi-Square Goodness of Fit Test
#'
#' @description
#' Tests whether observed frequencies match expected frequencies.
#'
#' @param x A vector
#' @param p Vector of expected probabilities (must sum to 1)
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return List containing test results
#'
#' @examples
#' # Test for uniform distribution
#' x <- sample(1:4, 100, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1))
#' test_goodness_of_fit(x, p = c(0.25, 0.25, 0.25, 0.25))
#'
#' @export
test_goodness_of_fit <- function(x, p = NULL, na.rm = TRUE) {

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    stop("No data to test", call. = FALSE)
  }

  # If p not specified, assume uniform
  freq_table <- table(x)
  k <- length(freq_table)

  if (is.null(p)) {
    p <- rep(1/k, k)
  }

  # Validate p
  if (length(p) != k) {
    stop(sprintf("`p` must have length %d to match number of categories", k),
         call. = FALSE)
  }

  if (abs(sum(p) - 1) > 1e-6) {
    stop("`p` must sum to 1", call. = FALSE)
  }

  # Perform chi-square test
  test_result <- stats::chisq.test(freq_table, p = p)

  # Return formatted results
  list(
    statistic = test_result$statistic,
    p_value = test_result$p.value,
    df = test_result$parameter,
    method = test_result$method,
    observed = as.numeric(freq_table),
    expected = test_result$expected,
    categories = names(freq_table),
    significant = test_result$p.value < 0.05
  )
}
