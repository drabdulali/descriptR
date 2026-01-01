#' Input Validation Functions for descriptR
#'
#' @description
#' Internal functions for validating inputs to descriptR functions.
#' These ensure data quality and provide informative error messages.
#'
#' @name validation_checks
#' @keywords internal
NULL

#' Validate Data Frame Input
#'
#' @param data Input to validate
#' @param allow_null Logical, allow NULL input?
#' @param min_rows Minimum number of rows required
#' @param arg_name Name of argument for error messages
#'
#' @return Invisibly returns TRUE if validation passes
#' @keywords internal
#' @noRd
validate_data_frame <- function(data,
                                allow_null = FALSE,
                                min_rows = 1,
                                arg_name = "data") {

  # Check for NULL
  if (is.null(data)) {
    if (allow_null) {
      return(invisible(TRUE))
    }
    stop(sprintf("`%s` cannot be NULL", arg_name), call. = FALSE)
  }

  # Check if data frame
  if (!is.data.frame(data)) {
    stop(sprintf("`%s` must be a data frame, not %s",
                arg_name, class(data)[1]),
         call. = FALSE)
  }

  # Check minimum rows
  if (nrow(data) < min_rows) {
    stop(sprintf("`%s` must have at least %d row%s, but has %d",
                arg_name,
                min_rows,
                if (min_rows == 1) "" else "s",
                nrow(data)),
         call. = FALSE)
  }

  # Check for zero columns
  if (ncol(data) == 0) {
    stop(sprintf("`%s` has no columns", arg_name), call. = FALSE)
  }

  invisible(TRUE)
}


#' Validate Variable Names
#'
#' @param data Data frame
#' @param vars Character vector of variable names
#' @param allow_null Allow NULL vars?
#' @param arg_name Name of argument for error messages
#'
#' @return Invisibly returns TRUE if validation passes
#' @keywords internal
#' @noRd
validate_variables <- function(data,
                               vars,
                               allow_null = TRUE,
                               arg_name = "vars") {

  # Check for NULL
  if (is.null(vars)) {
    if (allow_null) {
      return(invisible(TRUE))
    }
    stop(sprintf("`%s` cannot be NULL", arg_name), call. = FALSE)
  }

  # Check if character vector
  if (!is.character(vars)) {
    stop(sprintf("`%s` must be a character vector, not %s",
                arg_name, class(vars)[1]),
         call. = FALSE)
  }

  # Check if variables exist in data
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop(sprintf(
      "Variable%s not found in data: %s",
      if (length(missing_vars) == 1) "" else "s",
      paste(sprintf("'%s'", missing_vars), collapse = ", ")
    ), call. = FALSE)
  }

  invisible(TRUE)
}


#' Validate Grouping Variable
#'
#' @param data Data frame
#' @param group_var Name of grouping variable
#' @param allow_null Allow NULL group_var?
#' @param min_groups Minimum number of groups required
#' @param max_groups Maximum number of groups allowed
#'
#' @return Invisibly returns TRUE if validation passes
#' @keywords internal
#' @noRd
validate_group_var <- function(data,
                               group_var,
                               allow_null = TRUE,
                               min_groups = 2,
                               max_groups = Inf) {

  # Check for NULL
  if (is.null(group_var)) {
    if (allow_null) {
      return(invisible(TRUE))
    }
    stop("`group_var` cannot be NULL", call. = FALSE)
  }

  # Validate that variable exists
  validate_variables(data, group_var, allow_null = FALSE, arg_name = "group_var")

  # Get unique groups
  groups <- unique(data[[group_var]])
  n_groups <- length(groups[!is.na(groups)])

  # Check minimum groups
  if (n_groups < min_groups) {
    stop(sprintf(
      "Grouping variable '%s' has only %d group%s, but at least %d %s required",
      group_var,
      n_groups,
      if (n_groups == 1) "" else "s",
      min_groups,
      if (min_groups == 1) "is" else "are"
    ), call. = FALSE)
  }

  # Check maximum groups
  if (n_groups > max_groups) {
    warning(sprintf(
      "Grouping variable '%s' has %d groups, which exceeds recommended maximum of %d. Results may be difficult to interpret.",
      group_var,
      n_groups,
      max_groups
    ), call. = FALSE)
  }

  invisible(TRUE)
}


#' Validate Numeric Variables
#'
#' @param data Data frame
#' @param vars Variable names to check
#' @param allow_integer Allow integer variables?
#' @param allow_logical Allow logical variables?
#'
#' @return Invisibly returns TRUE if validation passes
#' @keywords internal
#' @noRd
validate_numeric <- function(data,
                             vars,
                             allow_integer = TRUE,
                             allow_logical = TRUE) {

  # Validate variables exist
  validate_variables(data, vars, allow_null = FALSE)

  # Check each variable
  non_numeric <- character(0)

  for (var in vars) {
    x <- data[[var]]

    is_valid <- is.numeric(x) ||
                (allow_integer && is.integer(x)) ||
                (allow_logical && is.logical(x))

    if (!is_valid) {
      non_numeric <- c(non_numeric, var)
    }
  }

  # Report errors
  if (length(non_numeric) > 0) {
    stop(sprintf(
      "Variable%s must be numeric: %s",
      if (length(non_numeric) == 1) "" else "s",
      paste(sprintf("'%s' (%s)",
                   non_numeric,
                   sapply(data[non_numeric], function(x) class(x)[1])),
           collapse = ", ")
    ), call. = FALSE)
  }

  invisible(TRUE)
}


#' Validate Function Parameters
#'
#' @param ... Named arguments to validate
#'
#' @return Invisibly returns TRUE if validation passes
#' @keywords internal
#' @noRd
validate_parameters <- function(...) {
  args <- list(...)

  for (arg_name in names(args)) {
    arg_info <- args[[arg_name]]

    value <- arg_info$value
    type <- arg_info$type
    allowed <- arg_info$allowed
    range <- arg_info$range

    # Type checking
    if (!is.null(type)) {
      type_match <- switch(
        type,
        "numeric" = is.numeric(value),
        "integer" = is.integer(value) || (is.numeric(value) && all(value == as.integer(value))),
        "character" = is.character(value),
        "logical" = is.logical(value),
        "factor" = is.factor(value),
        "list" = is.list(value),
        TRUE
      )

      if (!type_match) {
        stop(sprintf("`%s` must be %s, not %s",
                    arg_name, type, class(value)[1]),
             call. = FALSE)
      }
    }

    # Allowed values checking
    if (!is.null(allowed)) {
      if (!all(value %in% allowed)) {
        invalid <- setdiff(value, allowed)
        stop(sprintf(
          "Invalid value%s for `%s`: %s\nAllowed values: %s",
          if (length(invalid) == 1) "" else "s",
          arg_name,
          paste(sprintf("'%s'", invalid), collapse = ", "),
          paste(sprintf("'%s'", allowed), collapse = ", ")
        ), call. = FALSE)
      }
    }

    # Range checking
    if (!is.null(range) && is.numeric(value)) {
      if (any(value < range[1] | value > range[2], na.rm = TRUE)) {
        stop(sprintf(
          "`%s` must be between %s and %s",
          arg_name, range[1], range[2]
        ), call. = FALSE)
      }
    }
  }

  invisible(TRUE)
}


#' Validate Confidence Level
#'
#' @param conf.level Confidence level to validate
#'
#' @return Invisibly returns TRUE if validation passes
#' @keywords internal
#' @noRd
validate_conf_level <- function(conf.level) {
  if (!is.numeric(conf.level) || length(conf.level) != 1) {
    stop("`conf.level` must be a single numeric value", call. = FALSE)
  }

  if (conf.level <= 0 || conf.level >= 1) {
    stop("`conf.level` must be between 0 and 1 (exclusive)", call. = FALSE)
  }

  # Warn about unusual values
  if (conf.level < 0.5) {
    warning(sprintf(
      "Unusually low confidence level: %.2f. Did you mean %.2f?",
      conf.level,
      1 - (1 - conf.level)
    ), call. = FALSE)
  }

  invisible(TRUE)
}


#' Check for Sufficient Data
#'
#' @param x Vector to check
#' @param min_n Minimum sample size required
#' @param var_name Variable name for error message
#'
#' @return Invisibly returns TRUE if validation passes
#' @keywords internal
#' @noRd
check_sufficient_data <- function(x, min_n = 2, var_name = "Variable") {
  # Remove NA values
  x_clean <- x[!is.na(x)]

  n <- length(x_clean)

  if (n < min_n) {
    stop(sprintf(
      "%s has only %d non-missing observation%s, but at least %d %s required",
      var_name,
      n,
      if (n == 1) "" else "s",
      min_n,
      if (min_n == 1) "is" else "are"
    ), call. = FALSE)
  }

  # Check for zero variance
  if (n >= 2 && stats::var(x_clean) == 0) {
    warning(sprintf(
      "%s has zero variance (all values are identical)",
      var_name
    ), call. = FALSE)
  }

  invisible(TRUE)
}


#' Validate Trim Parameter
#'
#' @param trim Trim value to validate
#'
#' @return Invisibly returns TRUE if validation passes
#' @keywords internal
#' @noRd
validate_trim <- function(trim) {
  if (!is.numeric(trim) || length(trim) != 1) {
    stop("`trim` must be a single numeric value", call. = FALSE)
  }

  if (trim < 0 || trim >= 0.5) {
    stop("`trim` must be between 0 and 0.5 (exclusive of 0.5)", call. = FALSE)
  }

  invisible(TRUE)
}


#' Validate Output Format
#'
#' @param format Output format to validate
#' @param allowed_formats Allowed format values
#'
#' @return Invisibly returns TRUE if validation passes
#' @keywords internal
#' @noRd
validate_output_format <- function(format,
                                   allowed_formats = c("console", "html", "word",
                                                      "excel", "pdf", "latex")) {
  format <- match.arg(format, allowed_formats)
  invisible(TRUE)
}


#' Check Package Availability
#'
#' @param package Package name to check
#' @param function_name Function that requires the package
#'
#' @return Invisibly returns TRUE if package is available
#' @keywords internal
#' @noRd
check_package_available <- function(package, function_name = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    msg <- sprintf("Package '%s' is required", package)
    if (!is.null(function_name)) {
      msg <- sprintf("%s for %s()", msg, function_name)
    }
    msg <- sprintf("%s.\nInstall with: install.packages('%s')", msg, package)
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}
