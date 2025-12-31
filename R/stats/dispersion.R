#' Dispersion Measures
#'
#' @description
#' Functions for computing measures of dispersion (spread of distribution).
#'
#' @name dispersion
#' @keywords internal
NULL

#' Compute Standard Deviation
#'
#' @param x Numeric vector
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return Standard deviation value
#'
#' @examples
#' compute_sd(iris$Sepal.Length)
#'
#' @export
compute_sd <- function(x, na.rm = TRUE) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) < 2) {
    return(NA_real_)
  }

  stats::sd(x)
}


#' Compute Variance
#'
#' @param x Numeric vector
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return Variance value
#'
#' @examples
#' compute_variance(iris$Sepal.Length)
#'
#' @export
compute_variance <- function(x, na.rm = TRUE) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) < 2) {
    return(NA_real_)
  }

  stats::var(x)
}


#' Compute Interquartile Range (IQR)
#'
#' @param x Numeric vector
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return IQR value
#'
#' @details
#' The interquartile range is Q3 - Q1, representing the middle 50% of data.
#' It is robust to outliers.
#'
#' @examples
#' compute_iqr(iris$Sepal.Length)
#'
#' @export
compute_iqr <- function(x, na.rm = TRUE) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(NA_real_)
  }

  stats::IQR(x, na.rm = FALSE)
}


#' Compute Median Absolute Deviation (MAD)
#'
#' @param x Numeric vector
#' @param constant Scaling constant (default 1.4826 for consistency with SD)
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return MAD value
#'
#' @details
#' The median absolute deviation is a robust measure of variability.
#' MAD = median(|x - median(x)|) * constant
#'
#' The default constant 1.4826 makes MAD consistent with the standard
#' deviation for normally distributed data.
#'
#' @examples
#' compute_mad(iris$Sepal.Length)
#'
#' # Robust to outliers
#' x <- c(1, 2, 3, 4, 5, 100)
#' sd(x)   # 39.7 - affected by outlier
#' compute_mad(x)  # 2.97 - robust
#'
#' @export
compute_mad <- function(x, constant = 1.4826, na.rm = TRUE) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(NA_real_)
  }

  stats::mad(x, constant = constant, na.rm = FALSE)
}


#' Compute Range
#'
#' @param x Numeric vector
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return List with min, max, and range (max - min)
#'
#' @examples
#' compute_range(iris$Sepal.Length)
#'
#' @export
compute_range <- function(x, na.rm = TRUE) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(list(
      min = NA_real_,
      max = NA_real_,
      range = NA_real_
    ))
  }

  min_val <- min(x)
  max_val <- max(x)

  list(
    min = min_val,
    max = max_val,
    range = max_val - min_val
  )
}


#' Compute Coefficient of Variation (CV)
#'
#' @param x Numeric vector
#' @param na.rm Logical, remove NA values? (default TRUE)
#' @param percent Logical, return as percentage? (default TRUE)
#'
#' @return Coefficient of variation value
#'
#' @details
#' The coefficient of variation is the ratio of the standard deviation
#' to the mean: CV = SD / mean * 100%
#'
#' It represents relative variability and is useful for comparing
#' variability across variables with different units or scales.
#'
#' @examples
#' compute_cv(iris$Sepal.Length)
#'
#' # Comparing variability
#' compute_cv(iris$Sepal.Length)  # ~14%
#' compute_cv(iris$Petal.Length)  # ~47% - more variable
#'
#' @export
compute_cv <- function(x, na.rm = TRUE, percent = TRUE) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) < 2) {
    return(NA_real_)
  }

  m <- mean(x)

  # Check for zero mean
  if (m == 0) {
    warning("Mean is zero. CV is undefined. Returning NA.", call. = FALSE)
    return(NA_real_)
  }

  cv <- stats::sd(x) / abs(m)

  if (percent) {
    cv <- cv * 100
  }

  return(cv)
}


#' Compute Quantiles
#'
#' @param x Numeric vector
#' @param probs Numeric vector of probabilities (default c(0.25, 0.5, 0.75))
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return Named vector of quantiles
#'
#' @examples
#' compute_quantiles(iris$Sepal.Length)
#' compute_quantiles(iris$Sepal.Length, probs = seq(0, 1, 0.1))
#'
#' @export
compute_quantiles <- function(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  if (length(x) == 0) {
    return(rep(NA_real_, length(probs)))
  }

  stats::quantile(x, probs = probs, na.rm = FALSE)
}


#' Compute Standard Error of the Mean
#'
#' @param x Numeric vector
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return Standard error value
#'
#' @details
#' Standard error of the mean: SE = SD / sqrt(n)
#'
#' @examples
#' compute_se(iris$Sepal.Length)
#'
#' @export
compute_se <- function(x, na.rm = TRUE) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  n <- length(x)

  if (n < 2) {
    return(NA_real_)
  }

  stats::sd(x) / sqrt(n)
}


#' Compute Confidence Interval
#'
#' @param x Numeric vector
#' @param conf.level Confidence level (default 0.95)
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return List with lower and upper confidence limits
#'
#' @examples
#' compute_ci(iris$Sepal.Length)
#' compute_ci(iris$Sepal.Length, conf.level = 0.99)
#'
#' @export
compute_ci <- function(x, conf.level = 0.95, na.rm = TRUE) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  validate_conf_level(conf.level)

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  n <- length(x)

  if (n < 2) {
    return(list(
      lower = NA_real_,
      upper = NA_real_,
      level = conf.level
    ))
  }

  m <- mean(x)
  se <- stats::sd(x) / sqrt(n)
  alpha <- 1 - conf.level
  t_crit <- stats::qt(1 - alpha/2, df = n - 1)

  list(
    lower = m - t_crit * se,
    upper = m + t_crit * se,
    level = conf.level
  )
}


#' Compute All Dispersion Measures
#'
#' @param x Numeric vector
#' @param na.rm Logical, remove NA values? (default TRUE)
#'
#' @return List containing all dispersion measures
#'
#' @examples
#' compute_all_dispersion(iris$Sepal.Length)
#'
#' @export
compute_all_dispersion <- function(x, na.rm = TRUE) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  range_vals <- compute_range(x, na.rm = FALSE)

  list(
    sd = compute_sd(x, na.rm = FALSE),
    variance = compute_variance(x, na.rm = FALSE),
    iqr = compute_iqr(x, na.rm = FALSE),
    mad = compute_mad(x, na.rm = FALSE),
    min = range_vals$min,
    max = range_vals$max,
    range = range_vals$range,
    cv = compute_cv(x, na.rm = FALSE),
    se = compute_se(x, na.rm = FALSE),
    q25 = stats::quantile(x, 0.25, na.rm = FALSE),
    q75 = stats::quantile(x, 0.75, na.rm = FALSE),
    n = length(x)
  )
}
