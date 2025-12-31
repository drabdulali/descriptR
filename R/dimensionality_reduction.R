#' Principal Component Analysis
#'
#' @description
#' Performs Principal Component Analysis (PCA) with comprehensive diagnostics,
#' interpretation, and recommendations for dimensionality reduction.
#'
#' @param data Data frame or matrix
#' @param vars Character vector of variable names (NULL for all numeric)
#' @param scale Logical, scale variables to unit variance? (default TRUE)
#' @param center Logical, center variables? (default TRUE)
#' @param n_components Number of components to retain (NULL = all)
#' @param variance_threshold Retain components explaining this cumulative variance (default 0.95)
#' @param rotation Rotation method: "none", "varimax", "promax", etc. (default "none")
#'
#' @return Object of class "descriptR_pca" containing:
#' \itemize{
#'   \item \code{scores}: Principal component scores
#'   \item \code{loadings}: Variable loadings on components
#'   \item \code{variance_explained}: Variance explained by each component
#'   \item \code{eigenvalues}: Eigenvalues
#'   \item \code{scree_data}: Data for scree plot
#'   \item \code{interpretation}: Plain-language interpretation
#'   \item \code{recommendations}: Recommendations for component retention
#' }
#'
#' @details
#' ## When to Use PCA
#' - Reduce dimensionality of correlated variables
#' - Identify underlying patterns in data
#' - Address multicollinearity
#' - Data visualization (first 2-3 components)
#' - Feature extraction for modeling
#'
#' ## Component Retention Criteria
#'
#' **Kaiser Criterion**: Retain components with eigenvalue > 1
#' - Most common rule
#' - Can over-retain or under-retain
#'
#' **Scree Plot**: Look for "elbow" in eigenvalue plot
#' - Subjective but informative
#' - Retain components before sharp drop
#'
#' **Variance Explained**: Retain components explaining X% variance
#' - Typically 80-95%
#' - Domain-dependent threshold
#'
#' **Parallel Analysis**: Compare to random data eigenvalues
#' - More sophisticated
#' - Good for exploratory work
#'
#' ## Interpretation
#' - Loadings > 0.3: Weak relationship
#' - Loadings > 0.5: Moderate relationship
#' - Loadings > 0.7: Strong relationship
#'
#' @examples
#' # Basic PCA
#' perform_pca(mtcars)
#'
#' # Retain components explaining 90% variance
#' perform_pca(iris, vars = grep("^Sepal|^Petal", names(iris), value = TRUE),
#'             variance_threshold = 0.90)
#'
#' @export
perform_pca <- function(data,
                        vars = NULL,
                        scale = TRUE,
                        center = TRUE,
                        n_components = NULL,
                        variance_threshold = 0.95,
                        rotation = "none") {

  # Validate
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }

  validate_data_frame(data)

  # Select numeric variables
  if (is.null(vars)) {
    numeric_cols <- sapply(data, is.numeric)
    vars <- names(data)[numeric_cols]
  } else {
    validate_variables(data, vars)
  }

  if (length(vars) < 2) {
    stop("PCA requires at least 2 variables", call. = FALSE)
  }

  # Extract data
  pca_data <- data[, vars, drop = FALSE]

  # Remove rows with NA
  complete_cases <- complete.cases(pca_data)
  if (sum(complete_cases) < length(vars)) {
    stop("Insufficient complete cases for PCA", call. = FALSE)
  }

  pca_data <- pca_data[complete_cases, ]
  n_obs <- nrow(pca_data)

  # Perform PCA
  pca_result <- prcomp(pca_data, scale. = scale, center = center)

  # Eigenvalues
  eigenvalues <- pca_result$sdev^2
  n_pcs <- length(eigenvalues)

  # Variance explained
  var_explained <- eigenvalues / sum(eigenvalues)
  cumvar_explained <- cumsum(var_explained)

  # Determine number of components to retain
  n_retain <- determine_n_components(
    eigenvalues, cumvar_explained, n_components, variance_threshold
  )

  # Loadings (rotation matrix)
  loadings <- pca_result$rotation[, 1:n_retain, drop = FALSE]

  # Apply rotation if requested
  if (rotation != "none") {
    loadings <- apply_rotation(loadings, rotation)
  }

  # Scores
  scores <- pca_result$x[, 1:n_retain, drop = FALSE]

  # Scree plot data
  scree_data <- data.frame(
    Component = 1:n_pcs,
    Eigenvalue = eigenvalues,
    Variance_Explained = var_explained * 100,
    Cumulative_Variance = cumvar_explained * 100
  )

  # Component retention tests
  retention_tests <- list(
    kaiser = sum(eigenvalues > 1),
    variance_threshold = which(cumvar_explained >= variance_threshold)[1],
    elbow = find_elbow(eigenvalues)
  )

  # Interpretation
  interpretation <- generate_pca_interpretation(
    n_pcs, n_retain, var_explained, eigenvalues, retention_tests
  )

  # Recommendations
  recommendations <- generate_pca_recommendations(
    n_pcs, n_retain, cumvar_explained[n_retain], vars, eigenvalues
  )

  # Create result object
  result <- list(
    scores = scores,
    loadings = loadings,
    rotation_matrix = pca_result$rotation,
    variance_explained = data.frame(
      Component = 1:n_pcs,
      Eigenvalue = eigenvalues,
      Variance_Pct = var_explained * 100,
      Cumulative_Pct = cumvar_explained * 100
    ),
    eigenvalues = eigenvalues,
    scree_data = scree_data,
    retention_tests = retention_tests,
    interpretation = interpretation,
    recommendations = recommendations,
    n_components = n_retain,
    n_observations = n_obs,
    n_variables = length(vars),
    variables = vars,
    scaled = scale,
    centered = center,
    rotation = rotation
  )

  class(result) <- "descriptR_pca"
  return(result)
}


#' Determine Number of Components to Retain
#'
#' @keywords internal
#' @noRd
determine_n_components <- function(eigenvalues, cumvar_explained,
                                   n_components, variance_threshold) {

  # User-specified
  if (!is.null(n_components)) {
    return(min(n_components, length(eigenvalues)))
  }

  # Variance threshold
  n_var <- which(cumvar_explained >= variance_threshold)[1]

  # Kaiser criterion
  n_kaiser <- sum(eigenvalues > 1)

  # Use variance threshold, but at least Kaiser
  n_retain <- max(n_var, n_kaiser, 1)

  # Cap at total components
  min(n_retain, length(eigenvalues))
}


#' Find Elbow in Scree Plot
#'
#' @keywords internal
#' @noRd
find_elbow <- function(eigenvalues) {

  if (length(eigenvalues) < 3) {
    return(1)
  }

  # Compute second derivative (rate of change of slope)
  diffs <- diff(eigenvalues)
  second_diffs <- diff(diffs)

  # Find maximum change
  elbow <- which.max(abs(second_diffs)) + 1

  return(elbow)
}


#' Apply Rotation to Loadings
#'
#' @keywords internal
#' @noRd
apply_rotation <- function(loadings, rotation) {

  if (ncol(loadings) < 2) {
    warning("Rotation requires at least 2 components. Skipping rotation.",
            call. = FALSE)
    return(loadings)
  }

  tryCatch({
    if (rotation == "varimax") {
      rotated <- varimax(loadings)
      return(rotated$loadings[])
    } else if (rotation == "promax") {
      rotated <- promax(loadings)
      return(rotated$loadings[])
    } else {
      warning(sprintf("Unknown rotation method: %s", rotation), call. = FALSE)
      return(loadings)
    }
  }, error = function(e) {
    warning(sprintf("Rotation failed: %s", e$message), call. = FALSE)
    return(loadings)
  })
}


#' Generate PCA Interpretation
#'
#' @keywords internal
#' @noRd
generate_pca_interpretation <- function(n_pcs, n_retain, var_explained,
                                        eigenvalues, retention_tests) {

  interpretation <- character()

  interpretation <- c(interpretation,
    sprintf("Principal Component Analysis Results:"),
    sprintf("  • %d components extracted from original variables", n_pcs),
    sprintf("  • %d components retained for interpretation", n_retain),
    sprintf("  • First component explains %.1f%% of variance",
           var_explained[1] * 100),
    sprintf("  • Retained components explain %.1f%% cumulative variance",
           sum(var_explained[1:n_retain]) * 100),
    "")

  # Retention criteria
  interpretation <- c(interpretation,
    "Component Retention Criteria:",
    sprintf("  • Kaiser criterion (λ > 1): %d components",
           retention_tests$kaiser),
    sprintf("  • Scree plot elbow: ~%d components",
           retention_tests$elbow),
    sprintf("  • Variance threshold: %d components",
           retention_tests$variance_threshold),
    "")

  # Eigenvalue assessment
  if (eigenvalues[1] > 3) {
    interpretation <- c(interpretation,
      sprintf("First component has strong eigenvalue (%.2f), suggesting a dominant dimension.",
             eigenvalues[1]))
  } else if (eigenvalues[1] < 1.5) {
    interpretation <- c(interpretation,
      "Eigenvalues are relatively uniform, suggesting multiple important dimensions.")
  }

  return(interpretation)
}


#' Generate PCA Recommendations
#'
#' @keywords internal
#' @noRd
generate_pca_recommendations <- function(n_pcs, n_retain, cumvar, vars,
                                         eigenvalues) {

  recommendations <- character()

  # Dimensionality reduction achievement
  reduction_pct <- (1 - n_retain / length(vars)) * 100

  if (reduction_pct > 50) {
    recommendations <- c(recommendations,
      sprintf("• Excellent dimensionality reduction: %d variables → %d components (%.0f%% reduction)",
             length(vars), n_retain, reduction_pct))
  } else if (reduction_pct > 25) {
    recommendations <- c(recommendations,
      sprintf("• Moderate dimensionality reduction: %d variables → %d components (%.0f%% reduction)",
             length(vars), n_retain, reduction_pct))
  } else {
    recommendations <- c(recommendations,
      sprintf("• Limited dimensionality reduction: %d variables → %d components",
             length(vars), n_retain))
  }

  # Variance retention
  if (cumvar >= 0.90) {
    recommendations <- c(recommendations,
      sprintf("• Good variance retention: %.1f%% of original variance preserved",
             cumvar * 100))
  } else {
    recommendations <- c(recommendations,
      sprintf("• Moderate variance loss: %.1f%% of variance retained (consider more components)",
             cumvar * 100))
  }

  # General recommendations
  recommendations <- c(recommendations,
    "",
    "Recommended next steps:",
    "  1. Examine loadings to interpret component meaning",
    "  2. Inspect scree plot for visual confirmation",
    "  3. Check loading magnitudes (>0.3 weak, >0.5 moderate, >0.7 strong)",
    "  4. Consider rotation if components are difficult to interpret",
    "  5. Use component scores in downstream analyses")

  # Rotation suggestion
  if (n_retain >= 2 && n_retain <= 10) {
    recommendations <- c(recommendations,
      "  6. Try varimax rotation for simpler structure")
  }

  return(recommendations)
}


# Print Methods ===============================================================

#' @export
print.descriptR_pca <- function(x, digits = 3, max_loadings = 10, ...) {

  cat("\n")
  cat("Principal Component Analysis\n")
  cat(rep("=", 80), "\n", sep = "")

  cat(sprintf("\nData: %d observations × %d variables\n",
             x$n_observations, x$n_variables))
  cat(sprintf("Components retained: %d\n", x$n_components))
  cat(sprintf("Scaling: %s, Centering: %s\n",
             ifelse(x$scaled, "Yes", "No"),
             ifelse(x$centered, "Yes", "No")))

  if (x$rotation != "none") {
    cat(sprintf("Rotation: %s\n", x$rotation))
  }

  # Variance explained
  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nVariance Explained:\n\n")

  var_table <- head(x$variance_explained, x$n_components)
  print(var_table, row.names = FALSE, digits = digits)

  # Loadings
  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat(sprintf("\nComponent Loadings (top %d by absolute value):\n\n",
             max_loadings))

  # Show loadings for each retained component
  for (i in 1:min(3, x$n_components)) {  # Show first 3 components
    cat(sprintf("Component %d:\n", i))

    loadings_i <- x$loadings[, i]
    names(loadings_i) <- rownames(x$loadings)

    # Sort by absolute value
    sorted_idx <- order(abs(loadings_i), decreasing = TRUE)
    top_loadings <- loadings_i[sorted_idx[1:min(max_loadings, length(loadings_i))]]

    for (var_name in names(top_loadings)) {
      loading_val <- top_loadings[var_name]
      cat(sprintf("  %s: %s%.3f\n",
                 formatC(var_name, width = 20),
                 ifelse(loading_val >= 0, " ", ""),
                 loading_val))
    }
    cat("\n")
  }

  # Interpretation
  cat(rep("-", 80), "\n", sep = "")
  cat("\nInterpretation:\n\n")
  cat(paste(x$interpretation, collapse = "\n"))
  cat("\n")

  # Recommendations
  cat("\n")
  cat(rep("-", 80), "\n", sep = "")
  cat("\nRecommendations:\n\n")
  cat(paste(x$recommendations, collapse = "\n"))
  cat("\n")

  cat("\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("\n")

  invisible(x)
}


#' Summary Method for PCA
#'
#' @export
summary.descriptR_pca <- function(object, ...) {

  cat("\n")
  cat("PCA Summary\n")
  cat(rep("=", 70), "\n", sep = "")

  cat(sprintf("\nOriginal dimensions: %d variables\n", object$n_variables))
  cat(sprintf("Reduced dimensions: %d components\n", object$n_components))
  cat(sprintf("Variance retained: %.1f%%\n",
             object$variance_explained$Cumulative_Pct[object$n_components]))

  cat("\nFirst 3 Components:\n")
  for (i in 1:min(3, object$n_components)) {
    cat(sprintf("  PC%d: %.1f%% variance (λ = %.2f)\n",
               i,
               object$variance_explained$Variance_Pct[i],
               object$eigenvalues[i]))
  }

  cat("\nRetention Criteria:\n")
  cat(sprintf("  Kaiser (λ > 1): %d components\n",
             object$retention_tests$kaiser))
  cat(sprintf("  Elbow point: ~%d components\n",
             object$retention_tests$elbow))

  cat("\n")
  cat(rep("=", 70), "\n", sep = "")
  cat("\n")

  invisible(object)
}


#' Extract Component Scores
#'
#' @param pca_result Result from perform_pca()
#' @param components Which components to extract (default: all retained)
#'
#' @return Matrix of component scores
#'
#' @export
get_pca_scores <- function(pca_result, components = NULL) {

  if (!inherits(pca_result, "descriptR_pca")) {
    stop("Input must be a descriptR_pca object", call. = FALSE)
  }

  if (is.null(components)) {
    return(pca_result$scores)
  }

  if (max(components) > ncol(pca_result$scores)) {
    stop(sprintf("Requested component %d, but only %d components retained",
                max(components), ncol(pca_result$scores)),
         call. = FALSE)
  }

  pca_result$scores[, components, drop = FALSE]
}


#' Extract Component Loadings
#'
#' @param pca_result Result from perform_pca()
#' @param component Which component (default: 1)
#' @param threshold Minimum absolute loading to display (default: 0)
#'
#' @return Data frame with variable loadings
#'
#' @export
get_pca_loadings <- function(pca_result, component = 1, threshold = 0) {

  if (!inherits(pca_result, "descriptR_pca")) {
    stop("Input must be a descriptR_pca object", call. = FALSE)
  }

  if (component > ncol(pca_result$loadings)) {
    stop(sprintf("Component %d not available (only %d retained)",
                component, ncol(pca_result$loadings)),
         call. = FALSE)
  }

  loadings_vec <- pca_result$loadings[, component]

  # Filter by threshold
  keep <- abs(loadings_vec) >= threshold

  result <- data.frame(
    Variable = names(loadings_vec)[keep],
    Loading = loadings_vec[keep],
    Abs_Loading = abs(loadings_vec[keep]),
    stringsAsFactors = FALSE
  )

  # Sort by absolute value
  result <- result[order(-result$Abs_Loading), ]
  rownames(result) <- NULL

  return(result)
}


#' Interpret Component Based on Loadings
#'
#' @param pca_result Result from perform_pca()
#' @param component Which component to interpret (default: 1)
#' @param threshold Minimum loading for interpretation (default: 0.3)
#'
#' @return Character string with interpretation
#'
#' @export
interpret_pca_component <- function(pca_result, component = 1,
                                    threshold = 0.3) {

  loadings_df <- get_pca_loadings(pca_result, component, threshold)

  if (nrow(loadings_df) == 0) {
    return(sprintf("Component %d: No strong loadings (threshold = %.2f)",
                  component, threshold))
  }

  # Positive loadings
  positive <- loadings_df[loadings_df$Loading > threshold, ]
  negative <- loadings_df[loadings_df$Loading < -threshold, ]

  interpretation <- sprintf("Component %d (%.1f%% variance):\n",
                           component,
                           pca_result$variance_explained$Variance_Pct[component])

  if (nrow(positive) > 0) {
    interpretation <- paste0(interpretation,
      sprintf("  Positive loadings: %s\n",
             paste(positive$Variable, collapse = ", ")))
  }

  if (nrow(negative) > 0) {
    interpretation <- paste0(interpretation,
      sprintf("  Negative loadings: %s\n",
             paste(negative$Variable, collapse = ", ")))
  }

  # Suggest interpretation
  if (nrow(loadings_df) == length(pca_result$variables)) {
    interpretation <- paste0(interpretation,
      "  → Appears to be a general factor (all variables load)")
  } else if (nrow(positive) > 0 && nrow(negative) > 0) {
    interpretation <- paste0(interpretation,
      "  → Contrast between two sets of variables")
  }

  return(interpretation)
}
