#' Analyze and Report in One Step
#'
#' Performs comprehensive analysis and generates a formatted report in a single
#' function call. This is a convenience wrapper that combines analysis functions
#' with report generation.
#'
#' @param data A data frame containing the data to analyze
#' @param output_file Path where the report should be saved (without extension
#'   for "all" format)
#' @param analysis_type Type of analysis to perform. One of:
#'   * "descriptive" - Basic descriptive statistics
#'   * "grouped" - Grouped comparison with effect sizes
#'   * "correlation" - Correlation analysis
#'   * "normality" - Normality assessment
#'   * "missing" - Missing data analysis
#'   * "outliers" - Outlier detection
#'   * "pca" - Principal component analysis
#'   * "comprehensive" - All applicable analyses
#' @param format Output format(s). See \code{\link{generate_report}}
#' @param template Report template style. See \code{\link{generate_report}}
#' @param vars Variables to include in analysis (NULL for all)
#' @param group Grouping variable name (for grouped analysis)
#' @param include_plots Whether to include plots in the report (default: TRUE)
#' @param title Report title (auto-generated if NULL)
#' @param author Report author name
#' @param ... Additional arguments passed to specific analysis functions
#'
#' @return Invisibly returns a list containing:
#'   * result: The analysis result object
#'   * report_path: Path(s) to generated report file(s)
#'
#' @examples
#' \dontrun{
#' # Quick descriptive report
#' analyze_and_report(
#'   mtcars,
#'   "mtcars_report",
#'   analysis_type = "descriptive",
#'   format = "html"
#' )
#'
#' # Grouped comparison report
#' analyze_and_report(
#'   iris,
#'   "iris_comparison",
#'   analysis_type = "grouped",
#'   group = "Species",
#'   format = "all"
#' )
#'
#' # Comprehensive analysis
#' analyze_and_report(
#'   my_data,
#'   "comprehensive_report",
#'   analysis_type = "comprehensive",
#'   template = "apa"
#' )
#' }
#'
#' @export
analyze_and_report <- function(data,
                               output_file,
                               analysis_type = "descriptive",
                               format = "html",
                               template = "default",
                               vars = NULL,
                               group = NULL,
                               include_plots = TRUE,
                               title = NULL,
                               author = NULL,
                               ...) {
  # Input validation
  if (is.null(data) || !is.data.frame(data)) {
    stop("data must be a data frame", call. = FALSE)
  }

  if (is.null(output_file) || !is.character(output_file)) {
    stop("output_file is required and must be a character string", call. = FALSE)
  }

  valid_types <- c(
    "descriptive", "grouped", "correlation", "normality",
    "missing", "outliers", "pca", "comprehensive"
  )

  if (!analysis_type %in% valid_types) {
    stop(
      "analysis_type must be one of: ",
      paste(valid_types, collapse = ", "),
      call. = FALSE
    )
  }

  # Auto-generate title if not provided
  if (is.null(title)) {
    title <- paste(
      tools::toTitleCase(gsub("_", " ", analysis_type)),
      "Analysis Report"
    )
  }

  # Perform analysis based on type
  result <- switch(
    analysis_type,
    descriptive = perform_descriptive_analysis(data, vars, ...),
    grouped = perform_grouped_analysis(data, group, vars, ...),
    correlation = perform_correlation_analysis(data, vars, ...),
    normality = perform_normality_analysis(data, vars, ...),
    missing = perform_missing_analysis(data, vars, ...),
    outliers = perform_outlier_analysis(data, vars, ...),
    pca = perform_pca_analysis(data, vars, ...),
    comprehensive = perform_comprehensive_analysis(data, vars, group, ...)
  )

  # Generate report
  report_paths <- generate_report(
    result,
    output_file,
    format = format,
    template = template,
    include_plots = include_plots,
    title = title,
    author = author
  )

  # Return result and paths invisibly
  invisible(list(
    result = result,
    report_paths = report_paths
  ))
}


#' Helper: Perform Descriptive Analysis
#' @keywords internal
perform_descriptive_analysis <- function(data, vars = NULL, ...) {
  # This would call the main descriptive statistics function
  # For now, creating a structured result
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric)]
  }

  stats_list <- lapply(vars, function(var) {
    x <- data[[var]]
    data.frame(
      variable = var,
      n = sum(!is.na(x)),
      missing = sum(is.na(x)),
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      q25 = quantile(x, 0.25, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      q75 = quantile(x, 0.75, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  statistics <- do.call(rbind, stats_list)

  # Generate insights
  insights <- character()
  for (i in seq_len(nrow(statistics))) {
    stat <- statistics[i, ]

    # Missing data insight
    if (stat$missing > 0) {
      pct_missing <- round(stat$missing / (stat$n + stat$missing) * 100, 1)
      insights <- c(
        insights,
        sprintf("%s has %.1f%% missing values", stat$variable, pct_missing)
      )
    }

    # Variability insight
    cv <- stat$sd / stat$mean
    if (!is.na(cv) && cv > 0.5) {
      insights <- c(
        insights,
        sprintf("%s shows high variability (CV = %.2f)", stat$variable, cv)
      )
    }
  }

  if (length(insights) == 0) {
    insights <- c("All variables show complete data with moderate variability")
  }

  structure(
    list(
      statistics = statistics,
      insights = insights,
      metadata = list(
        analysis_type = "descriptive",
        timestamp = Sys.time(),
        n_variables = length(vars),
        n_observations = nrow(data)
      )
    ),
    class = "descriptR_result"
  )
}


#' Helper: Perform Grouped Analysis
#' @keywords internal
perform_grouped_analysis <- function(data, group, vars = NULL, ...) {
  if (is.null(group)) {
    stop("group variable must be specified for grouped analysis", call. = FALSE)
  }

  if (!group %in% names(data)) {
    stop(sprintf("group variable '%s' not found in data", group), call. = FALSE)
  }

  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric)]
  }

  # Remove group from vars if present
  vars <- setdiff(vars, group)

  group_values <- unique(data[[group]])

  # Calculate statistics by group
  results_list <- lapply(vars, function(var) {
    by_group <- lapply(group_values, function(g) {
      x <- data[[var]][data[[group]] == g]
      data.frame(
        variable = var,
        group = as.character(g),
        n = sum(!is.na(x)),
        mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, by_group)
  })

  by_group <- do.call(rbind, results_list)

  # Generate insights
  insights <- character()

  # Check for group differences
  for (var in vars) {
    var_data <- by_group[by_group$variable == var, ]
    mean_diff <- max(var_data$mean) - min(var_data$mean)
    pooled_sd <- sqrt(mean(var_data$sd^2))

    if (mean_diff / pooled_sd > 0.5) {
      insights <- c(
        insights,
        sprintf(
          "%s shows notable differences between groups (difference = %.2f)",
          var, mean_diff
        )
      )
    }
  }

  if (length(insights) == 0) {
    insights <- c("Groups show similar distributions across variables")
  }

  structure(
    list(
      by_group = by_group,
      insights = insights,
      metadata = list(
        analysis_type = "grouped",
        grouping_variable = group,
        n_groups = length(group_values),
        timestamp = Sys.time()
      )
    ),
    class = "descriptR_grouped"
  )
}


#' Helper: Perform Correlation Analysis
#' @keywords internal
perform_correlation_analysis <- function(data, vars = NULL, method = "pearson", ...) {
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric)]
  }

  # Calculate correlation matrix
  cor_data <- data[, vars, drop = FALSE]
  cor_data <- cor_data[complete.cases(cor_data), ]

  if (nrow(cor_data) < 3) {
    stop("Insufficient complete cases for correlation analysis", call. = FALSE)
  }

  cor_matrix <- cor(cor_data, method = method)
  n_obs <- nrow(cor_data)

  # Convert to long format for reporting
  cor_long <- data.frame(
    var1 = rep(vars, each = length(vars)),
    var2 = rep(vars, times = length(vars)),
    correlation = as.vector(cor_matrix),
    stringsAsFactors = FALSE
  )

  # Remove diagonal and duplicates
  cor_long <- cor_long[cor_long$var1 < cor_long$var2, ]

  # Add significance testing
  cor_long$p_value <- apply(cor_long, 1, function(row) {
    r <- as.numeric(row["correlation"])
    t_stat <- r * sqrt((n_obs - 2) / (1 - r^2))
    2 * pt(abs(t_stat), df = n_obs - 2, lower.tail = FALSE)
  })

  # Generate insights
  insights <- character()

  strong_cors <- cor_long[abs(cor_long$correlation) > 0.7, ]
  if (nrow(strong_cors) > 0) {
    for (i in seq_len(min(3, nrow(strong_cors)))) {
      insights <- c(
        insights,
        sprintf(
          "Strong correlation between %s and %s (r = %.3f)",
          strong_cors$var1[i],
          strong_cors$var2[i],
          strong_cors$correlation[i]
        )
      )
    }
  }

  if (length(insights) == 0) {
    insights <- c("No strong correlations detected among variables")
  }

  structure(
    list(
      correlation_matrix = cor_matrix,
      correlations = cor_long,
      insights = insights,
      metadata = list(
        analysis_type = "correlation",
        method = method,
        n_variables = length(vars),
        n_observations = n_obs,
        timestamp = Sys.time()
      )
    ),
    class = "descriptR_correlation"
  )
}


#' Helper: Perform Normality Analysis
#' @keywords internal
perform_normality_analysis <- function(data, vars = NULL, ...) {
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric)]
  }

  # Use assess_normality if available, otherwise simplified version
  results_list <- lapply(vars, function(var) {
    x <- na.omit(data[[var]])

    if (length(x) < 3) {
      return(NULL)
    }

    # Shapiro-Wilk test
    sw_test <- tryCatch(
      shapiro.test(x),
      error = function(e) NULL
    )

    data.frame(
      variable = var,
      n = length(x),
      test = "Shapiro-Wilk",
      statistic = if (!is.null(sw_test)) sw_test$statistic else NA,
      p_value = if (!is.null(sw_test)) sw_test$p.value else NA,
      normal = if (!is.null(sw_test)) sw_test$p.value > 0.05 else NA,
      stringsAsFactors = FALSE
    )
  })

  results <- do.call(rbind, results_list[!sapply(results_list, is.null)])

  # Generate insights
  insights <- character()

  non_normal <- results[!is.na(results$normal) & !results$normal, ]
  if (nrow(non_normal) > 0) {
    insights <- c(
      insights,
      sprintf(
        "%d variable(s) show significant departure from normality",
        nrow(non_normal)
      )
    )
  } else {
    insights <- c(insights, "All variables appear normally distributed")
  }

  structure(
    list(
      normality_tests = results,
      insights = insights,
      metadata = list(
        analysis_type = "normality",
        timestamp = Sys.time()
      )
    ),
    class = "descriptR_normality"
  )
}


#' Helper: Perform Missing Data Analysis
#' @keywords internal
perform_missing_analysis <- function(data, vars = NULL, ...) {
  if (is.null(vars)) {
    vars <- names(data)
  }

  # Calculate missing statistics
  missing_stats <- lapply(vars, function(var) {
    x <- data[[var]]
    n_missing <- sum(is.na(x))
    pct_missing <- n_missing / length(x) * 100

    data.frame(
      variable = var,
      n_total = length(x),
      n_missing = n_missing,
      pct_missing = pct_missing,
      stringsAsFactors = FALSE
    )
  })

  missing_summary <- do.call(rbind, missing_stats)
  missing_summary <- missing_summary[order(-missing_summary$pct_missing), ]

  # Generate insights
  insights <- character()

  total_missing <- sum(missing_summary$pct_missing > 0)
  if (total_missing > 0) {
    insights <- c(
      insights,
      sprintf("%d variable(s) have missing data", total_missing)
    )

    high_missing <- missing_summary[missing_summary$pct_missing > 20, ]
    if (nrow(high_missing) > 0) {
      insights <- c(
        insights,
        sprintf(
          "%s has %.1f%% missing values - consider imputation",
          high_missing$variable[1],
          high_missing$pct_missing[1]
        )
      )
    }
  } else {
    insights <- c(insights, "No missing data detected")
  }

  structure(
    list(
      missing_summary = missing_summary,
      insights = insights,
      metadata = list(
        analysis_type = "missing",
        timestamp = Sys.time()
      )
    ),
    class = "descriptR_missing"
  )
}


#' Helper: Perform Outlier Analysis
#' @keywords internal
perform_outlier_analysis <- function(data, vars = NULL, method = "zscore", ...) {
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric)]
  }

  # Detect outliers using Z-score method
  outlier_summary <- lapply(vars, function(var) {
    x <- na.omit(data[[var]])

    if (length(x) < 3) {
      return(NULL)
    }

    z_scores <- abs((x - mean(x)) / sd(x))
    n_outliers <- sum(z_scores > 3)
    pct_outliers <- n_outliers / length(x) * 100

    data.frame(
      variable = var,
      n = length(x),
      n_outliers = n_outliers,
      pct_outliers = pct_outliers,
      method = method,
      stringsAsFactors = FALSE
    )
  })

  outlier_results <- do.call(rbind, outlier_summary[!sapply(outlier_summary, is.null)])

  # Generate insights
  insights <- character()

  vars_with_outliers <- outlier_results[outlier_results$n_outliers > 0, ]
  if (nrow(vars_with_outliers) > 0) {
    insights <- c(
      insights,
      sprintf(
        "%d variable(s) contain potential outliers",
        nrow(vars_with_outliers)
      )
    )
  } else {
    insights <- c(insights, "No extreme outliers detected")
  }

  structure(
    list(
      outlier_summary = outlier_results,
      insights = insights,
      metadata = list(
        analysis_type = "outliers",
        method = method,
        timestamp = Sys.time()
      )
    ),
    class = "descriptR_outliers"
  )
}


#' Helper: Perform PCA Analysis
#' @keywords internal
perform_pca_analysis <- function(data, vars = NULL, scale = TRUE, ...) {
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric)]
  }

  pca_data <- data[, vars, drop = FALSE]
  pca_data <- pca_data[complete.cases(pca_data), ]

  if (nrow(pca_data) < ncol(pca_data)) {
    stop("Insufficient observations for PCA", call. = FALSE)
  }

  # Perform PCA
  pca_result <- prcomp(pca_data, scale. = scale)

  # Variance explained
  var_exp <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
  cum_var <- cumsum(var_exp)

  variance_explained <- data.frame(
    component = paste0("PC", seq_along(var_exp)),
    variance = var_exp,
    cumulative = cum_var,
    stringsAsFactors = FALSE
  )

  # Generate insights
  insights <- character()

  n_components_80 <- which(cum_var >= 0.80)[1]
  insights <- c(
    insights,
    sprintf(
      "%d component(s) explain 80%% of total variance",
      n_components_80
    )
  )

  if (var_exp[1] > 0.5) {
    insights <- c(
      insights,
      sprintf(
        "First component dominates, explaining %.1f%% of variance",
        var_exp[1] * 100
      )
    )
  }

  structure(
    list(
      pca_object = pca_result,
      variance_explained = variance_explained,
      loadings = pca_result$rotation,
      scores = pca_result$x,
      insights = insights,
      metadata = list(
        analysis_type = "pca",
        n_components = length(var_exp),
        scaled = scale,
        timestamp = Sys.time()
      )
    ),
    class = "descriptR_pca"
  )
}


#' Helper: Perform Comprehensive Analysis
#' @keywords internal
perform_comprehensive_analysis <- function(data, vars = NULL, group = NULL, ...) {
  if (is.null(vars)) {
    vars <- names(data)
  }

  numeric_vars <- vars[sapply(data[vars], is.numeric)]

  # Run multiple analyses
  results <- list()

  # Descriptive
  results$descriptive <- tryCatch(
    perform_descriptive_analysis(data, numeric_vars),
    error = function(e) NULL
  )

  # Missing data
  results$missing <- tryCatch(
    perform_missing_analysis(data, vars),
    error = function(e) NULL
  )

  # Normality
  if (length(numeric_vars) > 0) {
    results$normality <- tryCatch(
      perform_normality_analysis(data, numeric_vars),
      error = function(e) NULL
    )
  }

  # Outliers
  if (length(numeric_vars) > 0) {
    results$outliers <- tryCatch(
      perform_outlier_analysis(data, numeric_vars),
      error = function(e) NULL
    )
  }

  # Correlation (if enough variables)
  if (length(numeric_vars) >= 2) {
    results$correlation <- tryCatch(
      perform_correlation_analysis(data, numeric_vars),
      error = function(e) NULL
    )
  }

  # Grouped (if group specified)
  if (!is.null(group) && group %in% names(data)) {
    results$grouped <- tryCatch(
      perform_grouped_analysis(data, group, numeric_vars),
      error = function(e) NULL
    )
  }

  # Compile insights from all analyses
  all_insights <- unlist(lapply(results, function(r) {
    if (!is.null(r) && !is.null(r$insights)) r$insights else character()
  }))

  structure(
    list(
      analyses = results,
      insights = all_insights,
      metadata = list(
        analysis_type = "comprehensive",
        n_analyses = sum(!sapply(results, is.null)),
        timestamp = Sys.time(),
        original_data = data,
        vars = vars,
        group = group
      )
    ),
    class = "descriptR_comprehensive"
  )
}


#' Export Multiple Plots
#'
#' Exports a list of plots to files with consistent formatting and naming.
#'
#' @param plots A list of ggplot objects
#' @param output_dir Directory where plots should be saved
#' @param prefix Prefix for output filenames (default: "plot")
#' @param width Plot width in inches (default: 7)
#' @param height Plot height in inches (default: 5)
#' @param dpi Resolution in dots per inch (default: 300)
#' @param formats Vector of output formats (default: "png")
#' @param device Graphics device to use (default: NULL, auto-detected)
#'
#' @return Invisibly returns a character vector of created file paths
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' plots <- list(
#'   ggplot(mtcars, aes(mpg)) + geom_histogram(),
#'   ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' )
#'
#' export_plots(
#'   plots,
#'   output_dir = "figures",
#'   prefix = "mtcars",
#'   formats = c("png", "pdf")
#' )
#' }
#'
#' @export
export_plots <- function(plots,
                         output_dir,
                         prefix = "plot",
                         width = 7,
                         height = 5,
                         dpi = 300,
                         formats = "png",
                         device = NULL) {
  # Input validation
  if (!is.list(plots)) {
    stop("plots must be a list", call. = FALSE)
  }

  if (length(plots) == 0) {
    warning("No plots to export")
    return(invisible(character()))
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  valid_formats <- c("png", "pdf", "svg", "jpeg", "tiff", "eps", "ps")
  if (!all(formats %in% valid_formats)) {
    stop(
      "Invalid format(s). Must be one of: ",
      paste(valid_formats, collapse = ", "),
      call. = FALSE
    )
  }

  # Export each plot
  created_files <- character()

  for (i in seq_along(plots)) {
    plot <- plots[[i]]

    # Get plot name if available
    plot_name <- names(plots)[i]
    if (is.null(plot_name) || plot_name == "") {
      plot_name <- sprintf("%s_%02d", prefix, i)
    } else {
      plot_name <- sprintf("%s_%s", prefix, plot_name)
    }

    # Export in each format
    for (fmt in formats) {
      filename <- file.path(output_dir, paste0(plot_name, ".", fmt))

      tryCatch({
        ggplot2::ggsave(
          filename,
          plot = plot,
          width = width,
          height = height,
          dpi = dpi,
          device = device
        )

        created_files <- c(created_files, filename)
      }, error = function(e) {
        warning(
          sprintf("Failed to save plot %d as %s: %s", i, fmt, e$message),
          call. = FALSE
        )
      })
    }
  }

  message(sprintf("Exported %d plot file(s) to %s", length(created_files), output_dir))

  invisible(created_files)
}


#' Batch Export Data Tables
#'
#' Exports multiple data frames to Excel workbook with separate sheets or
#' individual CSV files.
#'
#' @param data_list Named list of data frames to export
#' @param output_file Output file path (for Excel) or directory (for CSV)
#' @param format Export format: "excel" or "csv"
#' @param ... Additional arguments passed to export functions
#'
#' @return Invisibly returns the path(s) to created file(s)
#'
#' @examples
#' \dontrun{
#' tables <- list(
#'   descriptive = data.frame(var = "x", mean = 10),
#'   correlations = data.frame(var1 = "x", var2 = "y", r = 0.5)
#' )
#'
#' # Export to Excel
#' batch_export_tables(tables, "results.xlsx", format = "excel")
#'
#' # Export to CSV
#' batch_export_tables(tables, "results", format = "csv")
#' }
#'
#' @export
batch_export_tables <- function(data_list,
                                output_file,
                                format = "excel",
                                ...) {
  # Input validation
  if (!is.list(data_list)) {
    stop("data_list must be a list", call. = FALSE)
  }

  if (length(data_list) == 0) {
    warning("No data to export")
    return(invisible(character()))
  }

  if (!all(sapply(data_list, is.data.frame))) {
    stop("All elements of data_list must be data frames", call. = FALSE)
  }

  format <- match.arg(format, c("excel", "csv"))

  if (format == "excel") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' is required for Excel export", call. = FALSE)
    }

    # Create workbook
    wb <- openxlsx::createWorkbook()

    # Add each data frame as a sheet
    for (sheet_name in names(data_list)) {
      # Truncate sheet name if too long
      safe_name <- substr(sheet_name, 1, 31)

      openxlsx::addWorksheet(wb, safe_name)
      openxlsx::writeData(wb, safe_name, data_list[[sheet_name]])

      # Style header
      header_style <- openxlsx::createStyle(
        textDecoration = "bold",
        fgFill = "#4F81BD",
        fontColour = "#FFFFFF",
        border = "bottom"
      )

      openxlsx::addStyle(
        wb,
        safe_name,
        style = header_style,
        rows = 1,
        cols = 1:ncol(data_list[[sheet_name]]),
        gridExpand = TRUE
      )
    }

    # Save workbook
    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
    message(sprintf("Exported %d table(s) to %s", length(data_list), output_file))

    return(invisible(output_file))
  } else {
    # CSV export
    if (!dir.exists(output_file)) {
      dir.create(output_file, recursive = TRUE)
    }

    created_files <- character()

    for (table_name in names(data_list)) {
      filename <- file.path(output_file, paste0(table_name, ".csv"))

      tryCatch({
        write.csv(
          data_list[[table_name]],
          filename,
          row.names = FALSE
        )

        created_files <- c(created_files, filename)
      }, error = function(e) {
        warning(
          sprintf("Failed to save table '%s': %s", table_name, e$message),
          call. = FALSE
        )
      })
    }

    message(sprintf("Exported %d CSV file(s) to %s", length(created_files), output_file))

    return(invisible(created_files))
  }
}


#' Generate Comprehensive Plots
#'
#' @description
#' Generates all relevant plots for comprehensive analysis results.
#' Creates histograms, boxplots, correlation plots, QQ plots, etc.
#' Generate ALL Comprehensive Plots
#'
#' @description
#' Generates EVERY possible plot type for comprehensive analysis.
#' Creates 10+ plot types including histograms, density, boxplots, violin,
#' QQ plots, scatter, correlations, bar plots, outliers, and missing data.
#'
#' @param data Original data frame  
#' @param result Comprehensive analysis result
#' @param vars Variables to plot (NULL for all)
#' @param group Grouping variable (optional)
#'
#' @return List of ggplot objects
#'
#' @keywords internal
#' @noRd
generate_comprehensive_plots <- function(data, result = NULL, vars = NULL, group = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("ggplot2 required for plotting. Skipping plot generation.")
    return(list())
  }

  plots <- list()

  # Determine variables to plot
  if (is.null(vars)) {
    vars <- names(data)
  }

  numeric_vars <- names(data)[sapply(data, is.numeric)]
  categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]

  # Limit to avoid excessive plots but still comprehensive
  numeric_vars <- head(numeric_vars, 10)
  categorical_vars <- head(categorical_vars, 5)

  message(sprintf("Generating comprehensive visualizations: %d numeric, %d categorical variables", 
                  length(numeric_vars), length(categorical_vars)))

  # ==============================================================================
  # 1. HISTOGRAMS - Distribution of each variable
  # ==============================================================================
  for (var in numeric_vars) {
    tryCatch({
      p <- plot_variable(data, x = var, plot_type = "histogram", theme = "publication",
                        title = paste("Histogram:", var), subtitle = "Distribution with density overlay")
      plots[[paste0("01_histogram_", var)]] <- p
    }, error = function(e) NULL)
  }

  # ==============================================================================
  # 2. DENSITY PLOTS - Smooth distribution curves
  # ==============================================================================
  for (var in numeric_vars) {
    tryCatch({
      p <- plot_variable(data, x = var, plot_type = "density", theme = "publication",
                        title = paste("Density:", var), subtitle = "Kernel density estimation")
      plots[[paste0("02_density_", var)]] <- p
    }, error = function(e) NULL)
  }

  # ==============================================================================
  # 3. BOXPLOTS - Quartiles and outliers
  # ==============================================================================
  for (var in numeric_vars) {
    tryCatch({
      p <- ggplot2::ggplot(data, ggplot2::aes(y = !!ggplot2::sym(var))) +
        ggplot2::geom_boxplot(fill = "#3498db", alpha = 0.7) +
        ggplot2::labs(title = paste("Boxplot:", var), subtitle = "Median, quartiles, outliers", y = var) +
        ggplot2::theme_minimal()
      plots[[paste0("03_boxplot_", var)]] <- p
    }, error = function(e) NULL)
  }

  # ==============================================================================
  # 4. BOXPLOTS BY GROUP
  # ==============================================================================
  if (!is.null(group) && group %in% names(data)) {
    for (var in numeric_vars) {
      tryCatch({
        p <- plot_variable(data, x = group, y = var, plot_type = "boxplot", theme = "publication",
                          title = paste(var, "by", group), subtitle = "Group comparisons")
        plots[[paste0("04_boxplot_grouped_", var)]] <- p
      }, error = function(e) NULL)
    }
  }

  # ==============================================================================
  # 5. VIOLIN PLOTS BY GROUP
  # ==============================================================================
  if (!is.null(group) && group %in% names(data)) {
    for (var in head(numeric_vars, 5)) {
      tryCatch({
        p <- plot_variable(data, x = group, y = var, plot_type = "violin", theme = "publication",
                          title = paste("Violin:", var, "by", group), subtitle = "Distribution shapes")
        plots[[paste0("05_violin_", var)]] <- p
      }, error = function(e) NULL)
    }
  }

  # ==============================================================================
  # 6. QQ PLOTS - Normality assessment for ALL numeric variables
  # ==============================================================================
  for (var in numeric_vars) {
    tryCatch({
      p <- plot_variable(data, x = var, plot_type = "qq", theme = "publication",
                        title = paste("Q-Q Plot:", var), subtitle = "Normality assessment")
      plots[[paste0("06_qq_", var)]] <- p
    }, error = function(e) NULL)
  }

  # ==============================================================================
  # 7. SCATTER PLOTS - Pairwise relationships
  # ==============================================================================
  if (length(numeric_vars) >= 2) {
    pairs <- min(5, length(numeric_vars) - 1)
    for (i in 1:pairs) {
      tryCatch({
        p <- plot_variable(data, x = numeric_vars[i], y = numeric_vars[i + 1], group = group,
                          plot_type = "scatter", theme = "publication",
                          title = paste(numeric_vars[i], "vs", numeric_vars[i + 1]), 
                          subtitle = "Relationship with trend")
        plots[[paste0("07_scatter_", i)]] <- p
      }, error = function(e) NULL)
    }
  }

  # ==============================================================================
  # 8. CORRELATION HEATMAP
  # ==============================================================================
  if (length(numeric_vars) >= 2) {
    tryCatch({
      p <- plot_correlation_matrix(data[numeric_vars], method = "pearson",
                                   title = "Correlation Heatmap", theme = "publication")
      plots[[paste0("08_correlogram")]] <- p
    }, error = function(e) NULL)
  }

  # ==============================================================================
  # 9. BAR PLOTS - Categorical variables
  # ==============================================================================
  for (cat_var in categorical_vars) {
    tryCatch({
      p <- ggplot2::ggplot(data, ggplot2::aes(x = !!ggplot2::sym(cat_var))) +
        ggplot2::geom_bar(fill = "#3498db", alpha = 0.8) +
        ggplot2::labs(title = paste("Frequency:", cat_var), subtitle = "Count by category",
                     x = cat_var, y = "Count") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      plots[[paste0("09_barplot_", cat_var)]] <- p
    }, error = function(e) NULL)
  }

  # ==============================================================================
  # 10. MISSING DATA PATTERN
  # ==============================================================================
  if (any(is.na(data))) {
    tryCatch({
      p <- plot_missing_pattern(data)
      plots[[paste0("10_missing_pattern")]] <- p
    }, error = function(e) NULL)
  }

  # ==============================================================================
  # 11. OUTLIER PLOTS
  # ==============================================================================
  for (var in head(numeric_vars, 5)) {
    tryCatch({
      outliers <- detect_outliers(data, vars = var, method = "zscore")
      if (!is.null(outliers$outliers) && nrow(outliers$outliers) > 0) {
        p <- plot_outliers(outliers)
        plots[[paste0("11_outliers_", var)]] <- p
      }
    }, error = function(e) NULL)
  }

  message(sprintf("Generated %d visualizations", length(plots)))
  return(plots)
}

