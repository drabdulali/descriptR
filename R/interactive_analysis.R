#' Interactive Variable Selection
#'
#' @description
#' Interactively select variables from a dataset for analysis. This function
#' presents a menu to the user and allows them to choose which variables to
#' include in the analysis.
#'
#' @param data A data frame
#' @param type Variable type to filter: "all", "numeric", "categorical", or "both"
#' @param multiple Allow multiple variable selection? (default: TRUE)
#' @param prompt Custom prompt message
#'
#' @return Character vector of selected variable names
#'
#' @examples
#' \dontrun{
#' # Select variables interactively
#' vars <- select_variables(mtcars)
#'
#' # Select only numeric variables
#' num_vars <- select_variables(iris, type = "numeric")
#'
#' # Select single variable
#' single_var <- select_variables(iris, multiple = FALSE)
#' }
#'
#' @export
select_variables <- function(data,
                             type = c("all", "numeric", "categorical", "both"),
                             multiple = TRUE,
                             prompt = NULL) {

  if (!interactive()) {
    stop("select_variables() requires an interactive R session", call. = FALSE)
  }

  type <- match.arg(type)

  # Get available variables based on type
  all_vars <- names(data)

  if (type == "numeric") {
    available_vars <- names(data)[sapply(data, is.numeric)]
  } else if (type == "categorical") {
    available_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  } else {
    available_vars <- all_vars
  }

  if (length(available_vars) == 0) {
    stop(sprintf("No %s variables found in dataset", type), call. = FALSE)
  }

  # Default prompt
  if (is.null(prompt)) {
    if (multiple) {
      prompt <- "Select variables (comma-separated numbers, or 0 for all):"
    } else {
      prompt <- "Select a variable (enter number):"
    }
  }

  # Display menu
  cat("\n")
  cat("Available variables:\n")
  cat("-------------------\n")
  for (i in seq_along(available_vars)) {
    var_name <- available_vars[i]
    var_type <- class(data[[var_name]])[1]
    cat(sprintf("%2d. %s (%s)\n", i, var_name, var_type))
  }
  if (multiple) {
    cat(" 0. All variables\n")
  }
  cat("\n")

  # Get user input
  input <- readline(prompt = paste0(prompt, " "))

  # Parse input
  if (multiple) {
    if (trimws(input) == "0") {
      selected_vars <- available_vars
    } else {
      indices <- as.numeric(unlist(strsplit(input, ",")))
      indices <- indices[!is.na(indices)]

      if (length(indices) == 0) {
        stop("No valid selection made", call. = FALSE)
      }

      if (any(indices < 1 | indices > length(available_vars))) {
        stop("Invalid selection: numbers must be between 1 and ",
             length(available_vars), call. = FALSE)
      }

      selected_vars <- available_vars[indices]
    }
  } else {
    index <- as.numeric(input)

    if (is.na(index) || index < 1 || index > length(available_vars)) {
      stop("Invalid selection: must be between 1 and ",
           length(available_vars), call. = FALSE)
    }

    selected_vars <- available_vars[index]
  }

  # Display selection
  cat("\nSelected variables:\n")
  cat(paste("-", selected_vars, collapse = "\n"))
  cat("\n\n")

  return(selected_vars)
}


#' Interactive Grouping Variable Selection
#'
#' @description
#' Interactively select a grouping variable for grouped analyses.
#'
#' @param data A data frame
#' @param prompt Custom prompt message
#'
#' @return Character string with selected grouping variable name
#'
#' @examples
#' \dontrun{
#' group_var <- select_grouping_variable(iris)
#' }
#'
#' @export
select_grouping_variable <- function(data, prompt = NULL) {

  if (!interactive()) {
    stop("select_grouping_variable() requires an interactive R session",
         call. = FALSE)
  }

  # Get categorical or low-cardinality numeric variables
  suitable_vars <- names(data)[sapply(data, function(x) {
    is.factor(x) || is.character(x) ||
      (is.numeric(x) && length(unique(x)) <= 10)
  })]

  if (length(suitable_vars) == 0) {
    cat("\nNo suitable grouping variables found.\n")
    cat("Would you like to proceed without grouping? (y/n): ")
    response <- readline()
    if (tolower(trimws(response)) == "y") {
      return(NULL)
    } else {
      stop("Analysis cancelled", call. = FALSE)
    }
  }

  # Default prompt
  if (is.null(prompt)) {
    prompt <- "Select grouping variable (enter number, or 0 for no grouping):"
  }

  # Display menu
  cat("\n")
  cat("Suitable grouping variables:\n")
  cat("----------------------------\n")
  for (i in seq_along(suitable_vars)) {
    var_name <- suitable_vars[i]
    n_groups <- length(unique(data[[var_name]]))
    cat(sprintf("%2d. %s (%d groups)\n", i, var_name, n_groups))
  }
  cat(" 0. No grouping\n")
  cat("\n")

  # Get user input
  input <- readline(prompt = paste0(prompt, " "))
  index <- as.numeric(input)

  if (is.na(index)) {
    stop("Invalid selection", call. = FALSE)
  }

  if (index == 0) {
    cat("\nNo grouping variable selected.\n\n")
    return(NULL)
  }

  if (index < 1 || index > length(suitable_vars)) {
    stop("Invalid selection: must be between 0 and ",
         length(suitable_vars), call. = FALSE)
  }

  selected_var <- suitable_vars[index]

  cat(sprintf("\nSelected grouping variable: %s\n\n", selected_var))

  return(selected_var)
}


#' Analyze All - Comprehensive Analysis with All Methods
#'
#' @description
#' Performs ALL available analyses in descriptR and generates a single
#' comprehensive report containing:
#' - Descriptive statistics
#' - Missing data analysis
#' - Normality tests
#' - Outlier detection
#' - Correlation analysis
#' - PCA (if applicable)
#' - Grouped analysis (if grouping variable specified)
#' - All visualizations
#'
#' This is the MASTER function that runs everything and produces one complete report.
#'
#' @param data A data frame to analyze
#' @param output_file Output filename (without extension)
#' @param vars Variables to analyze (NULL for interactive selection or all numeric)
#' @param group Grouping variable (NULL for interactive selection or no grouping)
#' @param format Output format: "html", "word", "excel", "markdown", "all"
#' @param template Report template style
#' @param include_plots Include all visualizations? (default: TRUE)
#' @param interactive Use interactive variable selection? (default: FALSE)
#' @param title Report title
#' @param author Report author
#' @param verbose Logical, print progress messages? (default: TRUE)
#' @param ... Additional arguments
#'
#' @return Invisibly returns list with results and report paths
#'
#' @examples
#' \dontrun{
#' # Non-interactive: analyze all numeric variables
#' analyze_all(mtcars, "complete_analysis")
#'
#' # Interactive: select variables
#' analyze_all(iris, "iris_analysis", interactive = TRUE)
#'
#' # With grouping variable
#' analyze_all(iris, "iris_by_species", group = "Species")
#'
#' # Generate all formats
#' analyze_all(mtcars, "mtcars_complete", format = "all")
#'
#' # Silent mode (no progress messages)
#' analyze_all(mtcars, "silent_analysis", verbose = FALSE)
#' }
#'
#' @export
analyze_all <- function(data,
                       output_file,
                       vars = NULL,
                       group = NULL,
                       format = "html",
                       template = "default",
                       include_plots = TRUE,
                       interactive = FALSE,
                       title = NULL,
                       author = NULL,
                       verbose = TRUE,
                       ...) {

  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame", call. = FALSE)
  }

  # Interactive variable selection
  if (interactive && is.null(vars)) {
    cat("\n")
    cat("=======================================================\n")
    cat("  Welcome to descriptR Comprehensive Analysis Wizard  \n")
    cat("=======================================================\n")

    vars <- select_variables(
      data,
      type = "all",
      multiple = TRUE,
      prompt = "Select variables to analyze:"
    )
  }

  # Interactive grouping variable selection
  if (interactive && is.null(group)) {
    cat("\n")
    cat("Would you like to perform grouped analysis? (y/n): ")
    response <- readline()

    if (tolower(trimws(response)) == "y") {
      group <- select_grouping_variable(data)
    }
  }

  # Auto-generate title
  if (is.null(title)) {
    title <- "Complete Statistical Analysis Report - All Methods"
  }

  # Run comprehensive analysis (which includes all sub-analyses)
  if (verbose) {
    message("Running comprehensive analysis...")
    message("- Descriptive statistics")
    message("- Missing data analysis")
    message("- Normality tests")
    message("- Outlier detection")
    message("- Correlation analysis")
    message("- Regression analysis")
    if (!is.null(group)) {
      message("- Grouped analysis by ", group)
      message("- ANOVA/MANOVA analysis")
    }
    message("- Missing data imputation")
  }

  result <- perform_comprehensive_analysis(data, vars, group, ...)

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

  if (verbose) {
    message("\nAnalysis complete!")
    message("\nGenerated files:")
    for (path in report_paths) {
      message("  - ", basename(path))
    }
  }

  # Return invisibly
  invisible(list(
    result = result,
    report_paths = report_paths
  ))
}
