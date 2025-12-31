# Test suite for integrated reporting

# Setup test data
test_data <- data.frame(
  age = c(25, 30, 35, 40, 45, 28, 32, 38, 42, 27),
  score = c(85, 90, 88, 92, 87, 86, 91, 89, 93, 84),
  height = c(165, 170, 175, 180, 185, 168, 172, 178, 182, 167),
  group = factor(rep(c("A", "B"), each = 5)),
  stringsAsFactors = FALSE
)

# Analyze and Report ==========================================================

test_that("analyze_and_report requires data", {
  expect_error(
    analyze_and_report(NULL, "test.html"),
    "data must be a data frame"
  )
})

test_that("analyze_and_report requires output_file", {
  expect_error(
    analyze_and_report(test_data, NULL),
    "output_file is required"
  )
})

test_that("analyze_and_report validates analysis_type", {
  expect_error(
    analyze_and_report(test_data, "test.html", analysis_type = "invalid"),
    "analysis_type must be one of"
  )
})

test_that("analyze_and_report works with descriptive analysis", {
  temp_file <- tempfile(fileext = ".md")

  result <- analyze_and_report(
    test_data,
    temp_file,
    analysis_type = "descriptive",
    format = "markdown",
    open_file = FALSE
  )

  expect_true(file.exists(temp_file))
  expect_true(is.list(result))
  expect_true("result" %in% names(result))

  unlink(temp_file)
})

test_that("analyze_and_report auto-generates title", {
  temp_file <- tempfile(fileext = ".md")

  result <- analyze_and_report(
    test_data,
    temp_file,
    analysis_type = "descriptive",
    format = "markdown",
    open_file = FALSE
  )

  content <- paste(readLines(temp_file), collapse = "\n")
  expect_true(grepl("Descriptive Analysis Report", content))

  unlink(temp_file)
})

test_that("analyze_and_report accepts custom title", {
  temp_file <- tempfile(fileext = ".md")

  result <- analyze_and_report(
    test_data,
    temp_file,
    analysis_type = "descriptive",
    title = "Custom Title",
    format = "markdown",
    open_file = FALSE
  )

  content <- paste(readLines(temp_file), collapse = "\n")
  expect_true(grepl("Custom Title", content))

  unlink(temp_file)
})

# Descriptive Analysis ========================================================

test_that("perform_descriptive_analysis works", {
  result <- perform_descriptive_analysis(test_data, vars = c("age", "score"))

  expect_s3_class(result, "descriptR_result")
  expect_true("statistics" %in% names(result))
  expect_true("insights" %in% names(result))
  expect_equal(nrow(result$statistics), 2)
})

test_that("perform_descriptive_analysis auto-selects numeric vars", {
  result <- perform_descriptive_analysis(test_data, vars = NULL)

  expect_true(nrow(result$statistics) >= 2)
  expect_true(all(result$statistics$variable %in% c("age", "score", "height")))
})

test_that("descriptive analysis includes expected statistics", {
  result <- perform_descriptive_analysis(test_data, vars = "age")

  stats <- result$statistics
  expect_true(all(c("n", "mean", "sd", "min", "median", "max") %in% names(stats)))
})

test_that("descriptive analysis generates insights", {
  result <- perform_descriptive_analysis(test_data)

  expect_true(length(result$insights) > 0)
  expect_true(is.character(result$insights))
})

# Grouped Analysis ============================================================

test_that("perform_grouped_analysis requires group", {
  expect_error(
    perform_grouped_analysis(test_data, group = NULL),
    "group variable must be specified"
  )
})

test_that("perform_grouped_analysis validates group exists", {
  expect_error(
    perform_grouped_analysis(test_data, group = "nonexistent"),
    "not found in data"
  )
})

test_that("perform_grouped_analysis works", {
  result <- perform_grouped_analysis(test_data, group = "group", vars = c("age", "score"))

  expect_s3_class(result, "descriptR_grouped")
  expect_true("by_group" %in% names(result))
  expect_true(all(c("variable", "group", "n", "mean", "sd") %in% names(result$by_group)))
})

test_that("grouped analysis separates by groups", {
  result <- perform_grouped_analysis(test_data, group = "group")

  groups <- unique(result$by_group$group)
  expect_equal(length(groups), 2)
  expect_true(all(c("A", "B") %in% groups))
})

test_that("grouped analysis generates insights", {
  result <- perform_grouped_analysis(test_data, group = "group")

  expect_true(length(result$insights) > 0)
})

# Correlation Analysis ========================================================

test_that("perform_correlation_analysis works", {
  result <- perform_correlation_analysis(test_data, vars = c("age", "score", "height"))

  expect_s3_class(result, "descriptR_correlation")
  expect_true("correlation_matrix" %in% names(result))
  expect_true("correlations" %in% names(result))
})

test_that("correlation analysis includes p-values", {
  result <- perform_correlation_analysis(test_data)

  expect_true("p_value" %in% names(result$correlations))
})

test_that("correlation analysis auto-selects numeric vars", {
  result <- perform_correlation_analysis(test_data, vars = NULL)

  expect_true(is.matrix(result$correlation_matrix))
  expect_true(ncol(result$correlation_matrix) >= 2)
})

test_that("correlation analysis handles different methods", {
  result_pearson <- perform_correlation_analysis(test_data, method = "pearson")
  result_spearman <- perform_correlation_analysis(test_data, method = "spearman")

  expect_s3_class(result_pearson, "descriptR_correlation")
  expect_s3_class(result_spearman, "descriptR_correlation")
})

test_that("correlation analysis requires sufficient data", {
  small_data <- data.frame(x = 1:2, y = 1:2)

  expect_error(
    perform_correlation_analysis(small_data),
    "Insufficient complete cases"
  )
})

# Normality Analysis ==========================================================

test_that("perform_normality_analysis works", {
  result <- perform_normality_analysis(test_data, vars = c("age", "score"))

  expect_s3_class(result, "descriptR_normality")
  expect_true("normality_tests" %in% names(result))
})

test_that("normality analysis includes test results", {
  result <- perform_normality_analysis(test_data, vars = "age")

  expect_true(all(c("variable", "test", "statistic", "p_value") %in%
    names(result$normality_tests)))
})

test_that("normality analysis generates insights", {
  result <- perform_normality_analysis(test_data)

  expect_true(length(result$insights) > 0)
  expect_true(any(grepl("normal", result$insights, ignore.case = TRUE)))
})

# Missing Data Analysis =======================================================

test_that("perform_missing_analysis works", {
  result <- perform_missing_analysis(test_data)

  expect_s3_class(result, "descriptR_missing")
  expect_true("missing_summary" %in% names(result))
})

test_that("missing analysis detects no missing data", {
  result <- perform_missing_analysis(test_data)

  expect_true(all(result$missing_summary$n_missing == 0))
  expect_true(any(grepl("No missing", result$insights)))
})

test_that("missing analysis detects missing data", {
  data_with_na <- test_data
  data_with_na$age[1:3] <- NA

  result <- perform_missing_analysis(data_with_na)

  age_row <- result$missing_summary[result$missing_summary$variable == "age", ]
  expect_true(age_row$n_missing == 3)
  expect_true(age_row$pct_missing == 30)
})

test_that("missing analysis includes all variables", {
  result <- perform_missing_analysis(test_data, vars = NULL)

  expect_true(all(names(test_data) %in% result$missing_summary$variable))
})

# Outlier Analysis ============================================================

test_that("perform_outlier_analysis works", {
  result <- perform_outlier_analysis(test_data, vars = c("age", "score"))

  expect_s3_class(result, "descriptR_outliers")
  expect_true("outlier_summary" %in% names(result))
})

test_that("outlier analysis auto-selects numeric vars", {
  result <- perform_outlier_analysis(test_data, vars = NULL)

  expect_true(nrow(result$outlier_summary) >= 2)
})

test_that("outlier analysis detects outliers", {
  data_with_outliers <- test_data
  data_with_outliers$age[1] <- 1000 # Extreme outlier

  result <- perform_outlier_analysis(data_with_outliers, vars = "age")

  age_row <- result$outlier_summary[result$outlier_summary$variable == "age", ]
  expect_true(age_row$n_outliers > 0)
})

# PCA Analysis ================================================================

test_that("perform_pca_analysis works", {
  result <- perform_pca_analysis(test_data, vars = c("age", "score", "height"))

  expect_s3_class(result, "descriptR_pca")
  expect_true("variance_explained" %in% names(result))
  expect_true("loadings" %in% names(result))
})

test_that("PCA analysis includes variance explained", {
  result <- perform_pca_analysis(test_data)

  expect_true(all(c("component", "variance", "cumulative") %in%
    names(result$variance_explained)))
})

test_that("PCA analysis requires sufficient observations", {
  small_data <- data.frame(
    x = 1:2,
    y = 1:2,
    z = 1:2
  )

  expect_error(
    perform_pca_analysis(small_data),
    "Insufficient observations"
  )
})

test_that("PCA analysis generates insights", {
  result <- perform_pca_analysis(test_data)

  expect_true(length(result$insights) > 0)
  expect_true(any(grepl("variance", result$insights)))
})

# Comprehensive Analysis ======================================================

test_that("perform_comprehensive_analysis runs multiple analyses", {
  result <- perform_comprehensive_analysis(test_data, group = "group")

  expect_s3_class(result, "descriptR_comprehensive")
  expect_true("analyses" %in% names(result))
  expect_true(length(result$analyses) > 1)
})

test_that("comprehensive analysis includes expected components", {
  result <- perform_comprehensive_analysis(test_data)

  expect_true("descriptive" %in% names(result$analyses))
  expect_true("missing" %in% names(result$analyses))
  expect_true("normality" %in% names(result$analyses))
})

test_that("comprehensive analysis includes grouped when specified", {
  result <- perform_comprehensive_analysis(test_data, group = "group")

  expect_true("grouped" %in% names(result$analyses))
})

test_that("comprehensive analysis compiles insights", {
  result <- perform_comprehensive_analysis(test_data)

  expect_true(length(result$insights) > 0)
  expect_true(is.character(result$insights))
})

test_that("comprehensive analysis handles errors gracefully", {
  bad_data <- data.frame(x = character(10))

  # Should not error, just skip analyses that fail
  expect_no_error({
    result <- perform_comprehensive_analysis(bad_data)
  })
})

# Export Plots ================================================================

test_that("export_plots requires list", {
  expect_error(
    export_plots("not a list", "output"),
    "plots must be a list"
  )
})

test_that("export_plots works with empty list", {
  temp_dir <- tempfile()

  expect_warning(
    export_plots(list(), temp_dir),
    "No plots to export"
  )
})

test_that("export_plots validates format", {
  skip_if_not_installed("ggplot2")

  plot <- ggplot2::ggplot(test_data, ggplot2::aes(x = age)) +
    ggplot2::geom_histogram()

  temp_dir <- tempfile()

  expect_error(
    export_plots(list(plot), temp_dir, formats = "invalid"),
    "Invalid format"
  )
})

test_that("export_plots creates output directory", {
  skip_if_not_installed("ggplot2")

  plot <- ggplot2::ggplot(test_data, ggplot2::aes(x = age)) +
    ggplot2::geom_histogram()

  temp_dir <- tempfile()

  export_plots(list(plot), temp_dir, prefix = "test")

  expect_true(dir.exists(temp_dir))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("export_plots exports files", {
  skip_if_not_installed("ggplot2")

  plot <- ggplot2::ggplot(test_data, ggplot2::aes(x = age)) +
    ggplot2::geom_histogram()

  temp_dir <- tempfile()

  files <- export_plots(
    list(test_plot = plot),
    temp_dir,
    prefix = "myplot",
    formats = "png"
  )

  expect_true(length(files) > 0)
  expect_true(all(file.exists(files)))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("export_plots handles multiple formats", {
  skip_if_not_installed("ggplot2")

  plot <- ggplot2::ggplot(test_data, ggplot2::aes(x = age)) +
    ggplot2::geom_histogram()

  temp_dir <- tempfile()

  files <- export_plots(
    list(plot),
    temp_dir,
    formats = c("png", "pdf")
  )

  expect_true(any(grepl("\\.png$", files)))
  expect_true(any(grepl("\\.pdf$", files)))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("export_plots uses plot names", {
  skip_if_not_installed("ggplot2")

  plots <- list(
    histogram = ggplot2::ggplot(test_data, ggplot2::aes(x = age)) +
      ggplot2::geom_histogram(),
    scatter = ggplot2::ggplot(test_data, ggplot2::aes(x = age, y = score)) +
      ggplot2::geom_point()
  )

  temp_dir <- tempfile()

  files <- export_plots(plots, temp_dir, prefix = "fig")

  expect_true(any(grepl("fig_histogram", files)))
  expect_true(any(grepl("fig_scatter", files)))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

# Batch Export Tables =========================================================

test_that("batch_export_tables requires list", {
  expect_error(
    batch_export_tables("not a list", "output.xlsx"),
    "data_list must be a list"
  )
})

test_that("batch_export_tables requires data frames", {
  expect_error(
    batch_export_tables(list(a = "not a df"), "output.xlsx"),
    "must be data frames"
  )
})

test_that("batch_export_tables validates format", {
  data_list <- list(table1 = test_data)

  expect_error(
    batch_export_tables(data_list, "output", format = "invalid"),
    "'arg' should be one of"
  )
})

test_that("batch_export_tables works with CSV", {
  data_list <- list(
    table1 = data.frame(x = 1:5),
    table2 = data.frame(y = 6:10)
  )

  temp_dir <- tempfile()

  files <- batch_export_tables(data_list, temp_dir, format = "csv")

  expect_true(file.exists(file.path(temp_dir, "table1.csv")))
  expect_true(file.exists(file.path(temp_dir, "table2.csv")))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("batch_export_tables works with Excel", {
  skip_if_not_installed("openxlsx")

  data_list <- list(
    table1 = data.frame(x = 1:5),
    table2 = data.frame(y = 6:10)
  )

  temp_file <- tempfile(fileext = ".xlsx")

  file <- batch_export_tables(data_list, temp_file, format = "excel")

  expect_true(file.exists(temp_file))

  # Cleanup
  unlink(temp_file)
})

test_that("batch_export_tables handles empty list", {
  expect_warning(
    batch_export_tables(list(), "output.csv", format = "csv"),
    "No data to export"
  )
})

test_that("batch_export_tables creates directory for CSV", {
  data_list <- list(table1 = data.frame(x = 1:5))

  temp_dir <- tempfile()

  batch_export_tables(data_list, temp_dir, format = "csv")

  expect_true(dir.exists(temp_dir))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

# Integration Tests ===========================================================

test_that("full workflow: analyze and report with plots", {
  skip_if_not_installed("ggplot2")

  temp_file <- tempfile(fileext = ".md")

  result <- analyze_and_report(
    test_data,
    temp_file,
    analysis_type = "descriptive",
    format = "markdown",
    title = "Full Workflow Test",
    author = "Test Suite",
    include_plots = TRUE,
    open_file = FALSE
  )

  expect_true(file.exists(temp_file))
  expect_true(is.list(result))
  expect_s3_class(result$result, "descriptR_result")

  content <- paste(readLines(temp_file), collapse = "\n")
  expect_true(grepl("Full Workflow Test", content))
  expect_true(grepl("Test Suite", content))

  unlink(temp_file)
})

test_that("full workflow: grouped analysis and export", {
  temp_file <- tempfile(fileext = ".html")

  result <- analyze_and_report(
    test_data,
    temp_file,
    analysis_type = "grouped",
    group = "group",
    format = "html",
    template = "apa",
    open_file = FALSE
  )

  expect_true(file.exists(temp_file))
  expect_s3_class(result$result, "descriptR_grouped")

  content <- paste(readLines(temp_file), collapse = "\n")
  expect_true(grepl("<html>", content, fixed = TRUE))

  unlink(temp_file)
})

test_that("export workflow: plots and tables together", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("openxlsx")

  # Create plots
  plots <- list(
    hist = ggplot2::ggplot(test_data, ggplot2::aes(x = age)) +
      ggplot2::geom_histogram()
  )

  # Create tables
  tables <- list(
    descriptive = data.frame(var = "age", mean = 35)
  )

  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Export plots
  plot_files <- export_plots(plots, file.path(temp_dir, "figures"))

  # Export tables
  table_file <- batch_export_tables(
    tables,
    file.path(temp_dir, "tables.xlsx"),
    format = "excel"
  )

  expect_true(length(plot_files) > 0)
  expect_true(file.exists(table_file))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})
