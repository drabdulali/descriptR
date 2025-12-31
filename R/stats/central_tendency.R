#' Central Tendency Measures
#'
#' @description
#' Functions for computing measures of central tendency (center of distribution).
#'
#' @name central_tendency
#' @keywords internal
NULL

#' Compute Mean with Confidence Interval
#'
#' @param x Numeric vector
#' @param trim Proportion to trim from each end (default 0)
#' @param na.rm Logical, remove NA values? (default TRUE)
#' @param conf.level Confidence level for CI (default 0.95)
#'
#' @return List containing mean and confidence interval
#'
#' @examples
#' compute_mean(iris$Sepal.Length)
#' compute_mean(iris$Sepal.Length, trim = 0.1)
#'
#' @export
compute_mean <- function(x, trim = 0, na.rm = TRUE, conf.level = 0.95) {

  # Validate inputs
  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  validate_trim(trim)
  validate_conf_level(conf.level)

  # Remove NA if requested
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  # Check sufficient data
  n <- length(x)
  if (n == 0) {
    return(list(
      mean = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      se = NA_real_,
      n = 0
    ))
  }

  # Compute mean
  m <- mean(x, trim = trim)

  # Compute standard error and CI
  if (n >= 2) {
    se <- stats::sd(x) / sqrt(n)
    alpha <- 1 - conf.level
    t_crit <- stats::qt(1 - alpha/2, df = n - 1)

    ci_lower <- m - t_crit * se
    ci_upper <- m + t_crit * se
  } else {
    se <- NA_real_
    ci_lower <- NA_real_
    ci_upper <- NA_real_
  }

  return(list(
    mean = m,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    se = se,
    n = n
  ))
}


#' Compute Median with Confidence Interval
#'
#' @param x Numeric vector
#' @param na.rm Logical, remove NA values? (default TRUE)
#' @param conf.level Confidence level for CI (default 0.95)
#'
#' @return List containing median and confidence interval
#'
#' @examples
#' compute_median(iris$Sepal.Length)
#'
#' @export
compute_median <- function(x, na.rm = TRUE, conf.level = 0.95) {

  # Validate inputs
  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  validate_conf_level(conf.level)

  # Remove NA if requested
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  n <- length(x)
  if (n == 0) {
    return(list(
      median = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      n = 0
    ))
  }

  # Compute median
  med <- stats::median(x)

  # Compute CI using binomial method
  if (n >= 3) {
    alpha <- 1 - conf.level
    # Use normal approximation for CI
    j <- round((n/2) - stats::qnorm(1 - alpha/2) * sqrt(n)/2)
    k <- round((n/2) + stats::qnorm(1 - alpha/2) * sqrt(n)/2) + 1

    # Ensure indices are valid
    j <- max(1, j)
    k <- min(n, k)

    x_sorted <- sort(x)
    ci_lower <- x_sorted[j]
    ci_upper <- x_sorted[k]
  } else {
    ci_lower <- NA_real_
    ci_upper <- NA_real_
  }

  return(list(
    median = med,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n = n
  ))
}


#' Compute Mode(s)
#'
#' @param x Vector of any type
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return Vector of mode value(s). Returns NA if no data.
#'         May return multiple values if multimodal.
#'
#' @examples
#' compute_mode(c(1, 2, 2, 3, 3, 3))  # 3
#' compute_mode(c(1, 1, 2, 2, 3))     # c(1, 2) - bimodal
#'
#' @export
compute_mode <- function(x, na.rm = TRUE) {

  # Remove NA if requested
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(NA)
  }

  # Count frequencies
  freq_table <- table(x)

  # Find maximum frequency
  max_freq <- max(freq_table)

  # Get all values with maximum frequency
  modes <- names(freq_table[freq_table == max_freq])

  # Convert back to original type
  if (is.numeric(x)) {
    modes <- as.numeric(modes)
  } else if (is.logical(x)) {
    modes <- as.logical(modes)
  }

  return(modes)
}


#' Compute Trimmed Mean
#'
#' @param x Numeric vector
#' @param trim Proportion to trim from each end (default 0.1 = 10%)
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return Trimmed mean value
#'
#' @examples
#' # 10% trimmed mean (removes 10% from each end)
#' compute_trimmed_mean(iris$Sepal.Length, trim = 0.1)
#'
#' # With outliers
#' x <- c(1, 2, 3, 4, 5, 100)  # 100 is outlier
#' mean(x)  # 19.17 - affected by outlier
#' compute_trimmed_mean(x, trim = 0.2)  # 3 - robust to outlier
#'
#' @export
compute_trimmed_mean <- function(x, trim = 0.1, na.rm = TRUE) {

  # Validate inputs
  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  validate_trim(trim)

  # Remove NA if requested
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(NA_real_)
  }

  return(mean(x, trim = trim))
}


#' Compute Winsorized Mean
#'
#' @param x Numeric vector
#' @param trim Proportion to winsorize from each end (default 0.1 = 10%)
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return Winsorized mean value
#'
#' @details
#' Winsorization replaces extreme values with less extreme values,
#' rather than removing them (as in trimming). The proportion 'trim'
#' of lowest values are replaced with the value at the 'trim' quantile,
#' and similarly for the highest values.
#'
#' @examples
#' # 10% winsorized mean
#' x <- c(1, 2, 3, 4, 5, 100)
#' mean(x)  # 19.17 - affected by outlier
#' compute_winsorized_mean(x, trim = 0.2)  # More robust
#'
#' @export
compute_winsorized_mean <- function(x, trim = 0.1, na.rm = TRUE) {

  # Validate inputs
  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  validate_trim(trim)

  # Remove NA if requested
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  n <- length(x)
  if (n == 0) {
    return(NA_real_)
  }

  # Calculate number of values to winsorize at each end
  k <- floor(n * trim)

  if (k == 0) {
    # No winsorization needed
    return(mean(x))
  }

  # Sort values
  x_sorted <- sort(x)

  # Replace extreme values
  lower_limit <- x_sorted[k + 1]
  upper_limit <- x_sorted[n - k]

  x_winsorized <- x
  x_winsorized[x < lower_limit] <- lower_limit
  x_winsorized[x > upper_limit] <- upper_limit

  return(mean(x_winsorized))
}


#' Compute Geometric Mean
#'
#' @param x Numeric vector (must be positive)
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return Geometric mean value
#'
#' @details
#' The geometric mean is the nth root of the product of n values.
#' It is useful for rates of change, ratios, and positively skewed data.
#' All values must be positive.
#'
#' @examples
#' # Growth rates
#' rates <- c(1.05, 1.10, 1.08)  # 5%, 10%, 8% growth
#' compute_geometric_mean(rates)  # Average growth rate
#'
#' @export
compute_geometric_mean <- function(x, na.rm = TRUE) {

  # Validate inputs
  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  # Remove NA if requested
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(NA_real_)
  }

  # Check for non-positive values
  if (any(x <= 0)) {
    warning("Geometric mean requires positive values. Returning NA.", call. = FALSE)
    return(NA_real_)
  }

  # Compute using log transformation to avoid overflow
  exp(mean(log(x)))
}


#' Compute Harmonic Mean
#'
#' @param x Numeric vector (must be positive)
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return Harmonic mean value
#'
#' @details
#' The harmonic mean is the reciprocal of the arithmetic mean of reciprocals.
#' It is useful for rates and ratios. All values must be positive.
#'
#' @examples
#' # Average speed: if you travel 60 mph for half the distance
#' # and 40 mph for the other half, the average speed is the harmonic mean
#' speeds <- c(60, 40)
#' compute_harmonic_mean(speeds)  # 48 mph
#'
#' @export
compute_harmonic_mean <- function(x, na.rm = TRUE) {

  # Validate inputs
  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  # Remove NA if requested
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(NA_real_)
  }

  # Check for non-positive values
  if (any(x <= 0)) {
    warning("Harmonic mean requires positive values. Returning NA.", call. = FALSE)
    return(NA_real_)
  }

  # Check for zero values
  if (any(x == 0)) {
    warning("Harmonic mean undefined with zero values. Returning NA.", call. = FALSE)
    return(NA_real_)
  }

  # Compute
  1 / mean(1 / x)
}
