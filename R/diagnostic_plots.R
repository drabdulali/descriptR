#' Diagnostic and Specialized Plots
#'
#' @description
#' Creates diagnostic plots for statistical analyses and specialized
#' visualizations for different data types.
#'
#' @name diagnostic_plots
NULL


#' Plot Scree Plot for PCA/EFA
#'
#' @description
#' Creates a scree plot showing eigenvalues for component/factor retention.
#'
#' @param result PCA or EFA result object
#' @param n_components Number of components to show (default: all)
#' @param add_kaiser Add Kaiser criterion line? (default TRUE)
#' @param color_scheme Color scheme to use
#' @param theme Plot theme
#'
#' @return ggplot2 object
#'
#' @examples
#' pca_result <- perform_pca(mtcars)
#' plot_scree(pca_result)
#'
#' @export
plot_scree <- function(result,
                       n_components = NULL,
                       add_kaiser = TRUE,
                       color_scheme = "medium",
                       theme = "publication") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required", call. = FALSE)
  }

  # Extract eigenvalues
  if (inherits(result, "descriptR_pca")) {
    eigenvalues <- result$eigenvalues
    title_text <- "Scree Plot - PCA"
  } else if (inherits(result, "descriptR_efa")) {
    # For EFA, compute eigenvalues from correlation matrix
    eigenvalues <- result$variance_explained * result$n_variables
    title_text <- "Scree Plot - EFA"
  } else {
    stop("Result must be from perform_pca() or perform_efa()", call. = FALSE)
  }

  # Limit components
  if (is.null(n_components)) {
    n_components <- length(eigenvalues)
  }
  n_components <- min(n_components, length(eigenvalues))

  eigenvalues <- eigenvalues[1:n_components]

  # Create data frame
  scree_data <- data.frame(
    Component = 1:n_components,
    Eigenvalue = eigenvalues
  )

  # Get color
  colors <- get_color_scheme(color_scheme, boldness = 2, n = 1)

  # Create plot
  p <- ggplot2::ggplot(scree_data, ggplot2::aes(x = Component, y = Eigenvalue)) +
    ggplot2::geom_line(color = colors[1], linewidth = 1.2) +
    ggplot2::geom_point(color = colors[1], size = 3, shape = 21, fill = "white") +
    ggplot2::scale_x_continuous(breaks = 1:n_components) +
    ggplot2::labs(
      title = title_text,
      subtitle = "Eigenvalues by Component",
      x = "Component Number",
      y = "Eigenvalue"
    )

  # Add Kaiser criterion line
  if (add_kaiser) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = 1,
        linetype = "dashed",
        color = "gray40",
        linewidth = 0.8
      ) +
      ggplot2::annotate(
        "text",
        x = n_components * 0.8,
        y = 1.1,
        label = "Kaiser Criterion (λ = 1)",
        hjust = 0,
        size = 3.5,
        color = "gray40"
      )
  }

  # Apply theme
  p <- p + apply_plot_theme(theme)

  return(p)
}


#' Plot Normality Diagnostics
#'
#' @description
#' Creates a panel of normality diagnostic plots (histogram, Q-Q, boxplot).
#'
#' @param data Data frame or numeric vector
#' @param variable Variable name (if data is data frame)
#' @param color_scheme Color scheme
#' @param theme Plot theme
#'
#' @return ggplot2 object (requires patchwork)
#'
#' @examples
#' plot_normality_diagnostics(mtcars$mpg)
#'
#' @export
plot_normality_diagnostics <- function(data,
                                        variable = NULL,
                                        color_scheme = "medium",
                                        theme = "publication") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required", call. = FALSE)
  }

  # Extract variable
  if (is.data.frame(data)) {
    if (is.null(variable)) {
      stop("Specify variable name when data is a data frame", call. = FALSE)
    }
    x <- data[[variable]]
    var_name <- variable
  } else {
    x <- data
    var_name <- deparse(substitute(data))
  }

  # Remove NA
  x <- x[!is.na(x)]

  # Get colors
  colors <- get_color_scheme(color_scheme, boldness = 2, n = 1)

  # Create data frame
  plot_data <- data.frame(value = x)

  # Histogram with density
  p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      fill = colors[1],
      alpha = 0.7,
      bins = 30
    ) +
    ggplot2::geom_density(color = "black", linewidth = 1) +
    ggplot2::stat_function(
      fun = dnorm,
      args = list(mean = mean(x), sd = sd(x)),
      color = "red",
      linetype = "dashed",
      linewidth = 1
    ) +
    ggplot2::labs(
      title = "Histogram with Normal Curve",
      x = var_name,
      y = "Density"
    ) +
    apply_plot_theme(theme)

  # Q-Q plot
  p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = value)) +
    ggplot2::stat_qq(color = colors[1], alpha = 0.7, size = 2) +
    ggplot2::stat_qq_line(color = "black", linewidth = 1) +
    ggplot2::labs(
      title = "Q-Q Plot",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    apply_plot_theme(theme)

  # Boxplot
  p3 <- ggplot2::ggplot(plot_data, ggplot2::aes(y = value)) +
    ggplot2::geom_boxplot(fill = colors[1], alpha = 0.7, width = 0.5) +
    ggplot2::labs(
      title = "Boxplot",
      y = var_name
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())

  # Combine plots if patchwork available
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined <- (p1 | p2 | p3) +
      patchwork::plot_annotation(
        title = sprintf("Normality Diagnostics: %s", var_name),
        theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold"))
      )
    return(combined)
  } else {
    message("Install 'patchwork' package for combined plot. Returning first plot only.")
    return(p1)
  }
}


#' Plot Missing Data Pattern
#'
#' @description
#' Visualizes missing data patterns across variables.
#'
#' @param data Data frame
#' @param vars Variables to include (NULL for all)
#' @param max_patterns Maximum patterns to show (default 20)
#' @param color_scheme Color scheme
#'
#' @return ggplot2 object
#'
#' @examples
#' plot_missing_pattern(airquality)
#'
#' @export
plot_missing_pattern <- function(data,
                                  vars = NULL,
                                  max_patterns = 20,
                                  color_scheme = "medium") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required", call. = FALSE)
  }

  # Select variables
  if (is.null(vars)) {
    vars <- names(data)
  }

  data_subset <- data[, vars, drop = FALSE]

  # Create missing indicator matrix
  missing_matrix <- is.na(data_subset)

  # Create pattern strings
  pattern_strings <- apply(missing_matrix, 1, function(row) {
    paste(as.integer(row), collapse = "")
  })

  # Count patterns
  pattern_counts <- table(pattern_strings)
  pattern_counts <- sort(pattern_counts, decreasing = TRUE)
  pattern_counts <- head(pattern_counts, max_patterns)

  # Create plot data
  plot_data <- data.frame()

  for (i in seq_along(pattern_counts)) {
    pattern <- names(pattern_counts)[i]
    pattern_bits <- as.integer(strsplit(pattern, "")[[1]])

    for (j in seq_along(vars)) {
      plot_data <- rbind(plot_data, data.frame(
        Pattern = i,
        Variable = vars[j],
        Missing = pattern_bits[j],
        Count = as.numeric(pattern_counts[i]),
        stringsAsFactors = FALSE
      ))
    }
  }

  # Get colors
  colors <- get_color_scheme(color_scheme, boldness = 2, n = 2)

  # Create plot
  p <- ggplot2::ggplot(plot_data,
                       ggplot2::aes(x = Variable, y = factor(Pattern))) +
    ggplot2::geom_tile(ggplot2::aes(fill = factor(Missing)),
                      color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_manual(
      values = c("0" = colors[1], "1" = colors[2]),
      labels = c("0" = "Observed", "1" = "Missing"),
      name = "Status"
    ) +
    ggplot2::labs(
      title = "Missing Data Patterns",
      subtitle = sprintf("Showing top %d patterns", min(max_patterns, length(pattern_counts))),
      x = "Variable",
      y = "Pattern"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )

  return(p)
}


#' Plot Outliers
#'
#' @description
#' Visualizes outliers detected by different methods.
#'
#' @param outlier_result Result from detect_outliers()
#' @param data Original data frame
#' @param variable Variable to plot
#' @param color_scheme Color scheme
#'
#' @return ggplot2 object
#'
#' @examples
#' outliers <- detect_outliers(mtcars$mpg, methods = c("zscore", "iqr"))
#' # plot_outliers(outliers, mtcars, "mpg")
#'
#' @export
plot_outliers <- function(outlier_result,
                          data,
                          variable,
                          color_scheme = "medium") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required", call. = FALSE)
  }

  if (!inherits(outlier_result, "descriptR_outliers")) {
    stop("First argument must be result from detect_outliers()", call. = FALSE)
  }

  # Extract variable data
  x <- data[[variable]]

  # Get outlier indices for this variable
  outlier_df <- outlier_result$outliers
  if (is.data.frame(data)) {
    outlier_indices <- outlier_df$Row[outlier_df$Variable == variable]
  } else {
    outlier_indices <- outlier_df$Row
  }

  # Create plot data
  plot_data <- data.frame(
    Index = seq_along(x),
    Value = x,
    IsOutlier = seq_along(x) %in% outlier_indices
  )

  # Get colors
  colors <- get_color_scheme(color_scheme, boldness = 3, n = 2)

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Index, y = Value)) +
    ggplot2::geom_point(
      ggplot2::aes(color = IsOutlier, size = IsOutlier),
      alpha = 0.7
    ) +
    ggplot2::scale_color_manual(
      values = c("FALSE" = colors[1], "TRUE" = colors[2]),
      labels = c("FALSE" = "Normal", "TRUE" = "Outlier")
    ) +
    ggplot2::scale_size_manual(
      values = c("FALSE" = 2, "TRUE" = 4),
      guide = "none"
    ) +
    ggplot2::labs(
      title = sprintf("Outlier Detection: %s", variable),
      subtitle = sprintf("%d outliers detected", length(outlier_indices)),
      x = "Observation Index",
      y = variable,
      color = "Status"
    ) +
    ggplot2::theme_minimal()

  return(p)
}


#' Plot Effect Sizes
#'
#' @description
#' Creates a forest plot of effect sizes with confidence intervals.
#'
#' @param effects Vector or list of effect sizes
#' @param ci_lower Lower confidence intervals
#' @param ci_upper Upper confidence intervals
#' @param labels Labels for each effect size
#' @param color_scheme Color scheme
#'
#' @return ggplot2 object
#'
#' @examples
#' effects <- c(0.3, 0.5, 0.8)
#' ci_low <- c(0.1, 0.3, 0.6)
#' ci_high <- c(0.5, 0.7, 1.0)
#' labels <- c("Effect 1", "Effect 2", "Effect 3")
#' plot_effect_sizes(effects, ci_low, ci_high, labels)
#'
#' @export
plot_effect_sizes <- function(effects,
                               ci_lower,
                               ci_upper,
                               labels,
                               color_scheme = "medium") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required", call. = FALSE)
  }

  # Validate inputs
  if (length(effects) != length(ci_lower) ||
      length(effects) != length(ci_upper) ||
      length(effects) != length(labels)) {
    stop("All inputs must have the same length", call. = FALSE)
  }

  # Create plot data
  plot_data <- data.frame(
    Label = factor(labels, levels = rev(labels)),
    Effect = effects,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper
  )

  # Get color
  colors <- get_color_scheme(color_scheme, boldness = 2, n = 1)

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Effect, y = Label)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = CI_Lower, xmax = CI_Upper),
      height = 0.2,
      linewidth = 1,
      color = colors[1]
    ) +
    ggplot2::geom_point(size = 4, color = colors[1]) +
    ggplot2::labs(
      title = "Effect Sizes with 95% Confidence Intervals",
      x = "Effect Size",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank()
    )

  return(p)
}


#' Plot Group Comparison
#'
#' @description
#' Creates a comprehensive visualization for group comparisons.
#'
#' @param data Data frame
#' @param outcome Outcome variable name
#' @param group Grouping variable name
#' @param plot_type Type: "violin", "boxplot", "both"
#' @param add_points Add individual points? (default TRUE)
#' @param color_scheme Color scheme
#' @param theme Plot theme
#'
#' @return ggplot2 object
#'
#' @examples
#' plot_group_comparison(iris, "Sepal.Length", "Species")
#'
#' @export
plot_group_comparison <- function(data,
                                   outcome,
                                   group,
                                   plot_type = c("violin", "boxplot", "both"),
                                   add_points = TRUE,
                                   color_scheme = "medium",
                                   theme = "publication") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required", call. = FALSE)
  }

  plot_type <- match.arg(plot_type)

  # Get colors
  n_groups <- length(unique(data[[group]]))
  colors <- get_color_scheme(color_scheme, boldness = 2, n = n_groups)

  # Base plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[group]], y = .data[[outcome]],
                                           fill = .data[[group]]))

  # Add layers based on type
  if (plot_type %in% c("violin", "both")) {
    p <- p + ggplot2::geom_violin(alpha = 0.6)
  }

  if (plot_type %in% c("boxplot", "both")) {
    p <- p + ggplot2::geom_boxplot(
      width = if (plot_type == "both") 0.2 else 0.5,
      alpha = if (plot_type == "both") 0.8 else 0.6
    )
  }

  # Add points
  if (add_points) {
    p <- p + ggplot2::geom_jitter(
      width = 0.1,
      alpha = 0.3,
      size = 1.5
    )
  }

  # Colors and labels
  p <- p +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = sprintf("%s by %s", outcome, group),
      x = group,
      y = outcome,
      fill = group
    ) +
    apply_plot_theme(theme)

  return(p)
}
