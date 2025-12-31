#' Automated Data Visualization
#'
#' @description
#' Creates publication-ready visualizations automatically based on variable types.
#' Integrates with descriptR color schemes for consistent, professional output.
#'
#' @param data Data frame
#' @param x X variable name
#' @param y Y variable name (optional for univariate plots)
#' @param group Grouping variable (optional)
#' @param plot_type Plot type: "auto", "histogram", "density", "boxplot", "violin",
#'   "scatter", "bar", "qq", "correlogram"
#' @param color_scheme Color scheme: "subtle", "medium", "bold", "vivid"
#' @param boldness Color boldness (1-4)
#' @param theme Theme: "publication", "presentation", "minimal"
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#'
#' @return ggplot2 object
#'
#' @details
#' ## Automatic Plot Selection
#'
#' Based on variable types:
#' - **Numeric**: Histogram with density overlay
#' - **Categorical**: Bar chart
#' - **Numeric × Numeric**: Scatter plot with trend
#' - **Categorical × Numeric**: Box plot or violin plot
#' - **Categorical × Categorical**: Grouped bar chart
#'
#' ## Themes
#'
#' **Publication**: Clean, minimal, grayscale-friendly
#' **Presentation**: Larger text, bold colors
#' **Minimal**: Bare essentials only
#'
#' @examples
#' # Automatic plot selection
#' plot_variable(mtcars, x = "mpg")
#'
#' # Scatter plot with grouping
#' plot_variable(iris, x = "Sepal.Length", y = "Sepal.Width", group = "Species")
#'
#' # Box plot
#' plot_variable(mtcars, x = "cyl", y = "mpg", plot_type = "boxplot")
#'
#' @export
plot_variable <- function(data,
                          x,
                          y = NULL,
                          group = NULL,
                          plot_type = "auto",
                          color_scheme = "medium",
                          boldness = 2,
                          theme = c("publication", "presentation", "minimal"),
                          title = NULL,
                          subtitle = NULL,
                          caption = NULL) {

  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Please install it.",
         call. = FALSE)
  }

  theme <- match.arg(theme)

  # Validate inputs
  validate_data_frame(data)
  validate_variables(data, x)
  if (!is.null(y)) validate_variables(data, y)
  if (!is.null(group)) validate_variables(data, group)

  # Detect variable types
  x_var <- data[[x]]
  x_type <- detect_var_type(x_var)

  y_var <- if (!is.null(y)) data[[y]] else NULL
  y_type <- if (!is.null(y)) detect_var_type(y_var) else NULL

  # Auto-select plot type
  if (plot_type == "auto") {
    plot_type <- select_plot_type(x_type, y_type)
  }

  # Get colors
  n_colors <- if (!is.null(group)) {
    length(unique(data[[group]]))
  } else {
    1
  }
  colors <- get_color_scheme(color_scheme, boldness, n = n_colors)

  # Create plot based on type
  p <- switch(plot_type,
    "histogram" = create_histogram(data, x, group, colors),
    "density" = create_density(data, x, group, colors),
    "boxplot" = create_boxplot(data, x, y, group, colors),
    "violin" = create_violin(data, x, y, group, colors),
    "scatter" = create_scatter(data, x, y, group, colors),
    "bar" = create_barplot(data, x, y, group, colors),
    "qq" = create_qqplot(data, x),
    stop(sprintf("Unknown plot type: %s", plot_type), call. = FALSE)
  )

  # Apply theme
  p <- p + apply_plot_theme(theme)

  # Add labels
  p <- p + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  )

  return(p)
}


#' Select Appropriate Plot Type
#'
#' @keywords internal
#' @noRd
select_plot_type <- function(x_type, y_type) {

  # Univariate
  if (is.null(y_type)) {
    if (x_type %in% c("continuous", "discrete_numeric")) {
      return("histogram")
    } else {
      return("bar")
    }
  }

  # Bivariate
  x_numeric <- x_type %in% c("continuous", "discrete_numeric")
  y_numeric <- y_type %in% c("continuous", "discrete_numeric")

  if (x_numeric && y_numeric) {
    return("scatter")
  } else if (x_numeric && !y_numeric) {
    return("boxplot")
  } else if (!x_numeric && y_numeric) {
    return("boxplot")
  } else {
    return("bar")
  }
}


#' Create Histogram
#'
#' @keywords internal
#' @noRd
create_histogram <- function(data, x, group, colors) {

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]]))

  if (!is.null(group)) {
    p <- p +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = .data[[group]]),
        alpha = 0.7,
        position = "identity",
        bins = 30
      ) +
      ggplot2::scale_fill_manual(values = colors)
  } else {
    p <- p +
      ggplot2::geom_histogram(fill = colors[1], alpha = 0.7, bins = 30) +
      ggplot2::geom_density(
        ggplot2::aes(y = ggplot2::after_stat(count)),
        color = "black",
        linewidth = 1
      )
  }

  p <- p +
    ggplot2::labs(x = x, y = "Count", fill = group)

  return(p)
}


#' Create Density Plot
#'
#' @keywords internal
#' @noRd
create_density <- function(data, x, group, colors) {

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]]))

  if (!is.null(group)) {
    p <- p +
      ggplot2::geom_density(
        ggplot2::aes(fill = .data[[group]], color = .data[[group]]),
        alpha = 0.5,
        linewidth = 1
      ) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::scale_color_manual(values = colors)
  } else {
    p <- p +
      ggplot2::geom_density(fill = colors[1], alpha = 0.5, linewidth = 1)
  }

  p <- p +
    ggplot2::labs(x = x, y = "Density", fill = group, color = group)

  return(p)
}


#' Create Box Plot
#'
#' @keywords internal
#' @noRd
create_boxplot <- function(data, x, y, group, colors) {

  # Determine which is categorical and which is numeric
  x_var <- data[[x]]
  y_var <- if (!is.null(y)) data[[y]] else NULL

  x_type <- detect_var_type(x_var)
  y_type <- if (!is.null(y)) detect_var_type(y_var) else NULL

  x_numeric <- x_type %in% c("continuous", "discrete_numeric")
  y_numeric <- y_type %in% c("continuous", "discrete_numeric")

  # Swap if needed
  if (x_numeric && !y_numeric) {
    temp <- x
    x <- y
    y <- temp
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]]))

  if (!is.null(group)) {
    p <- p +
      ggplot2::geom_boxplot(
        ggplot2::aes(fill = .data[[group]]),
        alpha = 0.7
      ) +
      ggplot2::scale_fill_manual(values = colors)
  } else {
    p <- p +
      ggplot2::geom_boxplot(fill = colors[1], alpha = 0.7)
  }

  p <- p +
    ggplot2::labs(x = x, y = y, fill = group)

  return(p)
}


#' Create Violin Plot
#'
#' @keywords internal
#' @noRd
create_violin <- function(data, x, y, group, colors) {

  # Same logic as boxplot for orientation
  x_var <- data[[x]]
  y_var <- if (!is.null(y)) data[[y]] else NULL

  x_type <- detect_var_type(x_var)
  y_type <- if (!is.null(y)) detect_var_type(y_var) else NULL

  x_numeric <- x_type %in% c("continuous", "discrete_numeric")
  y_numeric <- y_type %in% c("continuous", "discrete_numeric")

  if (x_numeric && !y_numeric) {
    temp <- x
    x <- y
    y <- temp
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]]))

  if (!is.null(group)) {
    p <- p +
      ggplot2::geom_violin(
        ggplot2::aes(fill = .data[[group]]),
        alpha = 0.7
      ) +
      ggplot2::geom_boxplot(
        ggplot2::aes(fill = .data[[group]]),
        width = 0.1,
        alpha = 0.5
      ) +
      ggplot2::scale_fill_manual(values = colors)
  } else {
    p <- p +
      ggplot2::geom_violin(fill = colors[1], alpha = 0.7) +
      ggplot2::geom_boxplot(width = 0.1, alpha = 0.5)
  }

  p <- p +
    ggplot2::labs(x = x, y = y, fill = group)

  return(p)
}


#' Create Scatter Plot
#'
#' @keywords internal
#' @noRd
create_scatter <- function(data, x, y, group, colors) {

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]]))

  if (!is.null(group)) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(color = .data[[group]]),
        alpha = 0.7,
        size = 2
      ) +
      ggplot2::geom_smooth(
        ggplot2::aes(color = .data[[group]], fill = .data[[group]]),
        method = "lm",
        alpha = 0.2
      ) +
      ggplot2::scale_color_manual(values = colors) +
      ggplot2::scale_fill_manual(values = colors)
  } else {
    p <- p +
      ggplot2::geom_point(color = colors[1], alpha = 0.7, size = 2) +
      ggplot2::geom_smooth(
        method = "lm",
        color = "black",
        fill = colors[1],
        alpha = 0.2
      )
  }

  p <- p +
    ggplot2::labs(x = x, y = y, color = group, fill = group)

  return(p)
}


#' Create Bar Plot
#'
#' @keywords internal
#' @noRd
create_barplot <- function(data, x, y, group, colors) {

  if (is.null(y)) {
    # Simple frequency bar chart
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]]))

    if (!is.null(group)) {
      p <- p +
        ggplot2::geom_bar(
          ggplot2::aes(fill = .data[[group]]),
          position = "dodge",
          alpha = 0.8
        ) +
        ggplot2::scale_fill_manual(values = colors)
    } else {
      p <- p +
        ggplot2::geom_bar(fill = colors[1], alpha = 0.8)
    }

    p <- p +
      ggplot2::labs(x = x, y = "Count", fill = group)

  } else {
    # Grouped bar chart
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]]))

    if (!is.null(group)) {
      p <- p +
        ggplot2::geom_bar(
          ggplot2::aes(fill = .data[[group]]),
          stat = "identity",
          position = "dodge",
          alpha = 0.8
        ) +
        ggplot2::scale_fill_manual(values = colors)
    } else {
      p <- p +
        ggplot2::geom_bar(stat = "identity", fill = colors[1], alpha = 0.8)
    }

    p <- p +
      ggplot2::labs(x = x, y = y, fill = group)
  }

  return(p)
}


#' Create Q-Q Plot
#'
#' @keywords internal
#' @noRd
create_qqplot <- function(data, x) {

  x_var <- data[[x]]

  p <- ggplot2::ggplot(data.frame(sample = x_var), ggplot2::aes(sample = sample)) +
    ggplot2::stat_qq(color = "#3498DB", alpha = 0.7) +
    ggplot2::stat_qq_line(color = "black", linewidth = 1) +
    ggplot2::labs(
      x = "Theoretical Quantiles",
      y = "Sample Quantiles",
      title = paste("Q-Q Plot:", x)
    )

  return(p)
}


#' Apply Plot Theme
#'
#' @keywords internal
#' @noRd
apply_plot_theme <- function(theme) {

  base_theme <- ggplot2::theme_minimal(base_size = 12)

  if (theme == "publication") {
    base_theme +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0),
        plot.subtitle = ggplot2::element_text(size = 11, hjust = 0),
        plot.caption = ggplot2::element_text(size = 9, hjust = 1, face = "italic"),
        axis.title = ggplot2::element_text(size = 11, face = "bold"),
        axis.text = ggplot2::element_text(size = 10),
        legend.position = "right",
        legend.title = ggplot2::element_text(size = 10, face = "bold"),
        legend.text = ggplot2::element_text(size = 9),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(fill = NA, color = "gray80")
      )

  } else if (theme == "presentation") {
    base_theme +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5),
        plot.caption = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.text = ggplot2::element_text(size = 12),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 13, face = "bold"),
        legend.text = ggplot2::element_text(size = 12),
        panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.5),
        panel.grid.minor = ggplot2::element_blank()
      )

  } else {  # minimal
    base_theme +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "gray95")
      )
  }
}


#' Save Plot with Multiple Resolutions
#'
#' @description
#' Saves plot at specified resolution with options for multiple formats.
#'
#' @param plot ggplot2 object
#' @param filename Output filename (without extension)
#' @param width Width in inches
#' @param height Height in inches
#' @param dpi Resolution: 72, 150, 300, 600, 1200, or "4K", "8K"
#' @param format Format: "png", "pdf", "svg", "jpg"
#' @param path Output directory (default: current)
#'
#' @return Invisibly returns filename
#'
#' @details
#' ## Resolution Presets
#' - 72 DPI: Screen/draft
#' - 150 DPI: PowerPoint
#' - 300 DPI: Standard print
#' - 600 DPI: High-quality print
#' - 1200 DPI: Professional publication
#' - 4K: 3840 × 2160 pixels (300 DPI at 12.8" × 7.2")
#' - 8K: 7680 × 4320 pixels (300 DPI at 25.6" × 14.4")
#'
#' @examples
#' p <- plot_variable(mtcars, x = "mpg")
#' \dontrun{
#' save_plot(p, "my_plot", dpi = 300)
#' save_plot(p, "my_plot_hires", dpi = "4K", format = "png")
#' }
#'
#' @export
save_plot <- function(plot,
                      filename,
                      width = 7,
                      height = 5,
                      dpi = 300,
                      format = c("png", "pdf", "svg", "jpg"),
                      path = ".") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Please install it.", call. = FALSE)
  }

  format <- match.arg(format)

  # Convert special DPI values
  if (is.character(dpi)) {
    dpi <- switch(dpi,
      "4K" = 300,  # 4K at standard viewing size
      "8K" = 600,  # 8K at standard viewing size
      as.numeric(dpi)
    )
  }

  # Full filepath
  filepath <- file.path(path, paste0(filename, ".", format))

  # Save
  ggplot2::ggsave(
    filename = filepath,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    device = format
  )

  message(sprintf("Plot saved: %s (%d DPI)", filepath, dpi))

  invisible(filepath)
}


#' Create Correlation Matrix Plot
#'
#' @description
#' Creates a publication-ready correlation matrix heatmap.
#'
#' @param data Data frame
#' @param vars Variables to include (NULL for all numeric)
#' @param method Correlation method: "pearson", "spearman", "kendall"
#' @param show_values Show correlation values? (default TRUE)
#' @param color_scheme Color scheme for heatmap
#'
#' @return ggplot2 object
#'
#' @examples
#' plot_correlation_matrix(mtcars, vars = c("mpg", "hp", "wt", "qsec"))
#'
#' @export
plot_correlation_matrix <- function(data,
                                     vars = NULL,
                                     method = "pearson",
                                     show_values = TRUE,
                                     color_scheme = "medium") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Please install it.", call. = FALSE)
  }

  # Select numeric variables
  if (is.null(vars)) {
    numeric_cols <- sapply(data, is.numeric)
    vars <- names(data)[numeric_cols]
  }

  # Compute correlation matrix
  cor_matrix <- cor(data[, vars], use = "pairwise.complete.obs", method = method)

  # Convert to long format
  cor_long <- reshape2::melt(cor_matrix)
  names(cor_long) <- c("Var1", "Var2", "Correlation")

  # Create diverging palette
  colors <- create_diverging_palette(
    low_color = "#3498DB",
    mid_color = "#FFFFFF",
    high_color = "#E74C3C",
    n = 11
  )

  # Create plot
  p <- ggplot2::ggplot(cor_long, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradientn(
      colors = colors,
      limits = c(-1, 1),
      breaks = seq(-1, 1, 0.5)
    )

  if (show_values) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.2f", Correlation)),
        size = 3
      )
  }

  p <- p +
    ggplot2::labs(
      title = paste(tools::toTitleCase(method), "Correlation Matrix"),
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )

  return(p)
}
