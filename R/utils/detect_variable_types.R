#' Detect Variable Type
#'
#' @description
#' Intelligently detects the type of a variable based on its class and characteristics.
#' Returns one of: continuous, discrete_numeric, ordinal, nominal, binary, datetime, or text.
#'
#' @param x A vector or variable to classify
#' @param unique_threshold Threshold for considering numeric as continuous (default 20)
#'
#' @return A character string indicating the variable type:
#' \itemize{
#'   \item \strong{continuous}: Numeric with >20 unique values
#'   \item \strong{discrete_numeric}: Numeric with 2-20 unique values
#'   \item \strong{ordinal}: Ordered factor
#'   \item \strong{nominal}: Unordered factor or character with low cardinality
#'   \item \strong{binary}: Two-level factor, logical, or 0/1 numeric
#'   \item \strong{datetime}: Date, POSIXct, or POSIXlt
#'   \item \strong{text}: Character with high cardinality
#' }
#'
#' @details
#' The function uses the following logic:
#'
#' 1. **Date/Time**: POSIXct, POSIXlt, Date → datetime
#' 2. **Logical**: TRUE/FALSE → binary
#' 3. **Ordered Factor**: ordered = TRUE → ordinal
#' 4. **Factor/Character**:
#'    - 2 unique values → binary
#'    - ≤ 20 unique values or < 50% unique → nominal
#'    - > 50% unique → text
#' 5. **Numeric**:
#'    - 2 unique values (especially 0/1) → binary
#'    - 2-20 unique values → discrete_numeric
#'    - > 20 unique values → continuous
#'
#' @examples
#' # Continuous numeric
#' detect_var_type(iris$Sepal.Length)  # "continuous"
#'
#' # Discrete numeric
#' detect_var_type(mtcars$cyl)  # "discrete_numeric"
#'
#' # Binary
#' detect_var_type(mtcars$vs)  # "binary"
#'
#' # Nominal
#' detect_var_type(iris$Species)  # "nominal"
#'
#' # Datetime
#' detect_var_type(Sys.Date())  # "datetime"
#'
#' @export
detect_var_type <- function(x, unique_threshold = 20) {

  # Handle NULL or empty
  if (is.null(x) || length(x) == 0) {
    return("unknown")
  }

  # Remove NA for counting unique values
  x_clean <- x[!is.na(x)]
  n_total <- length(x)
  n_clean <- length(x_clean)

  # If all NA, unknown
  if (n_clean == 0) {
    return("unknown")
  }

  # Count unique values
  n_unique <- length(unique(x_clean))
  pct_unique <- n_unique / n_clean

  # 1. Date/Time variables
  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return("datetime")
  }

  # 2. Logical variables
  if (is.logical(x)) {
    return("binary")
  }

  # 3. Ordered factors
  if (is.factor(x) && is.ordered(x)) {
    return("ordinal")
  }

  # 4. Factors and character variables
  if (is.factor(x) || is.character(x)) {
    if (n_unique == 2) {
      return("binary")
    } else if (n_unique <= unique_threshold || pct_unique < 0.5) {
      return("nominal")
    } else {
      return("text")
    }
  }

  # 5. Numeric variables
  if (is.numeric(x)) {
    # Check for binary (especially 0/1)
    if (n_unique == 2) {
      unique_vals <- sort(unique(x_clean))
      # If values are 0 and 1, definitely binary
      if (identical(unique_vals, c(0, 1)) || identical(unique_vals, c(0L, 1L))) {
        return("binary")
      }
      # Otherwise, could be discrete numeric or binary
      # Use heuristic: if only 2 values, treat as binary
      return("binary")
    }

    # Discrete numeric (limited unique values)
    if (n_unique <= unique_threshold) {
      return("discrete_numeric")
    }

    # Continuous (many unique values)
    return("continuous")
  }

  # Default: unknown
  return("unknown")
}


#' Detect Types for All Variables in a Data Frame
#'
#' @description
#' Applies variable type detection to all columns in a data frame.
#'
#' @param data A data frame
#' @param vars Optional character vector of variable names. If NULL, all variables are analyzed.
#' @param unique_threshold Threshold for continuous vs discrete (default 20)
#'
#' @return A named character vector where names are variable names and values are types
#'
#' @examples
#' # Detect types for all variables in iris
#' detect_all_types(iris)
#'
#' # Detect types for specific variables
#' detect_all_types(mtcars, vars = c("mpg", "cyl", "vs"))
#'
#' @export
detect_all_types <- function(data, vars = NULL, unique_threshold = 20) {

  # Validate input
  validate_data_frame(data)

  # If vars not specified, use all variables
  if (is.null(vars)) {
    vars <- names(data)
  } else {
    validate_variables(data, vars)
  }

  # Detect type for each variable
  types <- sapply(vars, function(var) {
    detect_var_type(data[[var]], unique_threshold = unique_threshold)
  })

  return(types)
}


#' Get Variables by Type
#'
#' @description
#' Returns a list of variables grouped by their detected type.
#'
#' @param data A data frame
#' @param unique_threshold Threshold for continuous vs discrete (default 20)
#'
#' @return A named list where each element contains variable names of that type:
#' \itemize{
#'   \item \code{continuous}: Continuous numeric variables
#'   \item \code{discrete_numeric}: Discrete numeric variables
#'   \item \code{ordinal}: Ordered factors
#'   \item \code{nominal}: Unordered categorical variables
#'   \item \code{binary}: Binary variables
#'   \item \code{datetime}: Date/time variables
#'   \item \code{text}: High-cardinality text variables
#'   \item \code{unknown}: Unclassified variables
#' }
#'
#' @examples
#' # Get variables by type for iris
#' var_types <- get_variables_by_type(iris)
#' var_types$continuous  # Numeric measurements
#' var_types$nominal     # Species
#'
#' # For mtcars
#' var_types <- get_variables_by_type(mtcars)
#' var_types$continuous       # mpg, disp, hp, etc.
#' var_types$discrete_numeric # cyl, gear, carb
#' var_types$binary          # vs, am
#'
#' @export
get_variables_by_type <- function(data, unique_threshold = 20) {

  # Validate input
  validate_data_frame(data)

  # Detect all types
  all_types <- detect_all_types(data, unique_threshold = unique_threshold)

  # Group variables by type
  type_categories <- c("continuous", "discrete_numeric", "ordinal", "nominal",
                      "binary", "datetime", "text", "unknown")

  result <- lapply(type_categories, function(type) {
    names(all_types[all_types == type])
  })
  names(result) <- type_categories

  return(result)
}


#' Check if Variable is Numeric Type
#'
#' @description
#' Determines if a variable should be treated as numeric for analysis purposes.
#' Includes continuous, discrete_numeric, and binary numeric variables.
#'
#' @param x A vector to check
#' @param unique_threshold Threshold for continuous vs discrete (default 20)
#'
#' @return Logical indicating if variable is numeric type
#'
#' @examples
#' is_numeric_type(iris$Sepal.Length)  # TRUE
#' is_numeric_type(iris$Species)       # FALSE
#' is_numeric_type(mtcars$cyl)        # TRUE
#'
#' @export
is_numeric_type <- function(x, unique_threshold = 20) {
  var_type <- detect_var_type(x, unique_threshold = unique_threshold)
  var_type %in% c("continuous", "discrete_numeric", "binary")
}


#' Check if Variable is Categorical Type
#'
#' @description
#' Determines if a variable should be treated as categorical for analysis purposes.
#' Includes nominal, ordinal, and binary categorical variables.
#'
#' @param x A vector to check
#' @param unique_threshold Threshold for continuous vs discrete (default 20)
#'
#' @return Logical indicating if variable is categorical type
#'
#' @examples
#' is_categorical_type(iris$Species)       # TRUE
#' is_categorical_type(iris$Sepal.Length)  # FALSE
#' is_categorical_type(factor(mtcars$cyl)) # TRUE
#'
#' @export
is_categorical_type <- function(x, unique_threshold = 20) {
  var_type <- detect_var_type(x, unique_threshold = unique_threshold)
  var_type %in% c("nominal", "ordinal", "binary")
}


#' Suggest Analysis Type for Variable
#'
#' @description
#' Recommends appropriate statistical analyses based on variable type.
#'
#' @param x A vector to analyze
#' @param unique_threshold Threshold for continuous vs discrete (default 20)
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{type}: Detected variable type
#'   \item \code{descriptive}: Recommended descriptive statistics
#'   \item \code{visualization}: Recommended visualizations
#'   \item \code{tests}: Appropriate statistical tests
#' }
#'
#' @examples
#' # For continuous variable
#' suggest_analysis_type(iris$Sepal.Length)
#'
#' # For categorical variable
#' suggest_analysis_type(iris$Species)
#'
#' @export
suggest_analysis_type <- function(x, unique_threshold = 20) {

  var_type <- detect_var_type(x, unique_threshold = unique_threshold)

  suggestions <- switch(
    var_type,

    "continuous" = list(
      type = "continuous",
      descriptive = c("mean", "median", "SD", "IQR", "range", "skewness", "kurtosis"),
      visualization = c("histogram", "density plot", "boxplot", "Q-Q plot"),
      tests = c("t-test", "ANOVA", "correlation", "normality test")
    ),

    "discrete_numeric" = list(
      type = "discrete_numeric",
      descriptive = c("mean", "median", "mode", "SD", "range", "unique values"),
      visualization = c("bar plot", "boxplot", "violin plot"),
      tests = c("t-test", "ANOVA", "Kruskal-Wallis", "chi-square")
    ),

    "ordinal" = list(
      type = "ordinal",
      descriptive = c("median", "mode", "frequencies", "percentages"),
      visualization = c("ordered bar plot", "cumulative distribution"),
      tests = c("Mann-Whitney U", "Kruskal-Wallis", "Spearman correlation")
    ),

    "nominal" = list(
      type = "nominal",
      descriptive = c("mode", "frequencies", "percentages", "entropy"),
      visualization = c("bar plot", "pie chart", "mosaic plot"),
      tests = c("chi-square", "Fisher's exact", "Cramér's V")
    ),

    "binary" = list(
      type = "binary",
      descriptive = c("proportion", "count", "percentage", "odds"),
      visualization = c("bar plot", "pie chart"),
      tests = c("proportion test", "chi-square", "logistic regression")
    ),

    "datetime" = list(
      type = "datetime",
      descriptive = c("range", "duration", "frequency"),
      visualization = c("timeline", "time series plot"),
      tests = c("time series analysis", "survival analysis")
    ),

    "text" = list(
      type = "text",
      descriptive = c("unique values", "most common", "length distribution"),
      visualization = c("word cloud", "frequency plot"),
      tests = c("text analysis", "pattern matching")
    ),

    # Default for unknown
    list(
      type = "unknown",
      descriptive = c("basic summary"),
      visualization = c("depends on data structure"),
      tests = c("manual specification required")
    )
  )

  return(suggestions)
}


#' Print Summary of Variable Types
#'
#' @description
#' Provides a formatted summary of variable types in a data frame.
#'
#' @param data A data frame
#' @param unique_threshold Threshold for continuous vs discrete (default 20)
#'
#' @return Invisibly returns a data frame with variable type information
#'
#' @examples
#' # Print variable type summary for iris
#' print_variable_types(iris)
#'
#' # For mtcars
#' print_variable_types(mtcars)
#'
#' @export
print_variable_types <- function(data, unique_threshold = 20) {

  # Validate input
  validate_data_frame(data)

  # Detect all types
  all_types <- detect_all_types(data, unique_threshold = unique_threshold)

  # Create summary data frame
  summary_df <- data.frame(
    Variable = names(all_types),
    Type = as.character(all_types),
    Class = sapply(names(all_types), function(v) class(data[[v]])[1]),
    N_Unique = sapply(names(all_types), function(v) {
      length(unique(data[[v]][!is.na(data[[v]])]))
    }),
    N_Missing = sapply(names(all_types), function(v) sum(is.na(data[[v]]))),
    stringsAsFactors = FALSE
  )

  # Print formatted output
  cat("\n")
  cat("Variable Type Summary\n")
  cat(rep("=", 70), "\n", sep = "")
  cat(sprintf("Dataset: %d observations, %d variables\n\n", nrow(data), ncol(data)))

  # Count by type
  type_counts <- table(all_types)
  for (type in names(type_counts)) {
    cat(sprintf("  %-20s: %2d variable%s\n",
               type, type_counts[type],
               ifelse(type_counts[type] == 1, "", "s")))
  }

  cat("\n")
  cat(rep("-", 70), "\n", sep = "")
  cat(sprintf("%-20s %-15s %-10s %8s %8s\n",
             "Variable", "Type", "Class", "Unique", "Missing"))
  cat(rep("-", 70), "\n", sep = "")

  for (i in seq_len(nrow(summary_df))) {
    cat(sprintf("%-20s %-15s %-10s %8d %8d\n",
               substr(summary_df$Variable[i], 1, 20),
               summary_df$Type[i],
               substr(summary_df$Class[i], 1, 10),
               summary_df$N_Unique[i],
               summary_df$N_Missing[i]))
  }

  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  invisible(summary_df)
}
