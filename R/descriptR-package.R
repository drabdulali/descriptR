#' @keywords internal
"_PACKAGE"

#' descriptR: Comprehensive Descriptive Statistics with Automated Insights
#'
#' @description
#' The descriptR package provides a unified, intelligent interface for descriptive
#' and inferential statistics. It automatically detects variable types, applies
#' appropriate analyses, generates narrative insights, and produces publication-ready
#' outputs in multiple formats.
#'
#' @section Main Features:
#'
#' **Unified Interface**
#' - Single function handles all data types
#' - Automatic variable type detection
#' - Smart default parameter selection
#'
#' **Comprehensive Statistics**
#' - Descriptive statistics for all variable types
#' - Inferential tests (t-tests, ANOVA, chi-square, correlation)
#' - Advanced methods (PCA, mixed models, mediation, Bayesian)
#' - Effect sizes and confidence intervals
#'
#' **Automated Insights**
#' - AI-powered narrative generation
#' - Plain language interpretations
#' - Practical significance assessment
#' - Anomaly detection and reporting
#'
#' **Publication-Ready Outputs**
#' - Multiple formats: HTML, Word, Excel, PDF
#' - Journal-specific templates (Nature, Science, NEJM, etc.)
#' - Advanced color schemes (subtle/medium/bold/vivid)
#' - Multi-resolution export (72 DPI to 8K)
#'
#' **Data Quality Tools**
#' - Missing data analysis and imputation
#' - Outlier detection (multiple methods)
#' - Normality assessment suite
#' - Data validation and quality checks
#'
#' @section Key Functions:
#'
#' **Descriptive Analysis**
#' \itemize{
#'   \item \code{\link{describe_data}}: Main comprehensive function
#'   \item \code{\link{describe_numeric}}: Specialized for continuous variables
#'   \item \code{\link{describe_categorical}}: Specialized for categorical variables
#'   \item \code{\link{describe_grouped}}: Group-wise comparisons
#' }
#'
#' **Inferential Statistics**
#' \itemize{
#'   \item \code{\link{perform_t_test}}: All t-test variants
#'   \item \code{\link{perform_anova}}: ANOVA with post-hoc tests
#'   \item \code{\link{perform_correlation_test}}: Correlation with inference
#'   \item \code{\link{compare_groups}}: Comprehensive group comparisons
#' }
#'
#' **Visualization**
#' \itemize{
#'   \item \code{\link{visualize_descriptives}}: Automated visualization
#'   \item \code{\link{export_plot}}: Multi-resolution export
#'   \item \code{\link{get_color_scheme}}: Color palette management
#' }
#'
#' **Data Quality**
#' \itemize{
#'   \item \code{\link{check_data_quality}}: Comprehensive quality check
#'   \item \code{\link{missing_analysis}}: Missing data analysis
#'   \item \code{\link{detect_outliers}}: Outlier detection
#'   \item \code{\link{assess_normality}}: Normality testing
#' }
#'
#' **Reporting**
#' \itemize{
#'   \item \code{\link{report_descriptives}}: Publication-ready reports
#'   \item \code{\link{export_to_excel}}: Excel workbooks
#'   \item \code{\link{export_to_word}}: Word documents
#' }
#'
#' @section Getting Started:
#'
#' Basic usage example:
#' \preformatted{
#' library(descriptR)
#'
#' # Load sample data
#' data(clinical_trial_data)
#'
#' # Basic descriptive statistics
#' describe_data(clinical_trial_data)
#'
#' # Grouped comparison
#' describe_grouped(clinical_trial_data,
#'                  vars = "followup_score",
#'                  group_by = "treatment_group")
#'
#' # With visualization
#' results <- visualize_descriptives(
#'   clinical_trial_data,
#'   vars = c("baseline_score", "followup_score"),
#'   group_by = "treatment_group",
#'   color_scheme = "medium"
#' )
#'
#' # Export for publication
#' export_plot(results, "figure1.png", resolution = "4k")
#' }
#'
#' @section Color Schemes:
#'
#' descriptR offers four color boldness levels:
#' \itemize{
#'   \item \strong{Subtle} (20-40\% saturation): Academic papers, professional reports
#'   \item \strong{Medium} (50-70\% saturation): General publications [DEFAULT]
#'   \item \strong{Bold} (80-95\% saturation): Posters, social media
#'   \item \strong{Vivid} (95-100\% saturation): Digital displays, infographics
#' }
#'
#' All schemes are color-blind safe and WCAG compliant.
#'
#' @section Export Resolutions:
#'
#' Available resolutions for different purposes:
#' \itemize{
#'   \item \strong{Draft}: 72 DPI (quick previews)
#'   \item \strong{Web}: 96-150 DPI (HD, 2K, 4K)
#'   \item \strong{Print}: 300-1200 DPI (standard to professional)
#'   \item \strong{Journal}: Nature, Science, NEJM formats
#'   \item \strong{Social}: Instagram, Twitter, LinkedIn formats
#'   \item \strong{Poster}: A0-A4 sizes at 300 DPI
#' }
#'
#' @section Sample Dataset:
#'
#' The package includes \code{\link{clinical_trial_data}}, a comprehensive
#' synthetic dataset with 100 subjects and 41 variables designed to demonstrate
#' all package features.
#'
#' @section Learn More:
#'
#' \itemize{
#'   \item Vignettes: \code{browseVignettes("descriptR")}
#'   \item GitHub: \url{https://github.com/drabdulali/descriptR}
#'   \item Issues: \url{https://github.com/drabdulali/descriptR/issues}
#' }
#'
#' @docType package
#' @name descriptR-package
#' @aliases descriptR
#'
#' @author
#' **Maintainer**: Dr. Abdul Ali \email{dr.abdulali@@example.com}
#'
#' @references
#' Ali, A. (2025). descriptR: Comprehensive Descriptive Statistics with
#' Automated Insights. R package version 0.1.0.
#' \url{https://github.com/drabdulali/descriptR}
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://github.com/drabdulali/descriptR}
#'   \item Report bugs at \url{https://github.com/drabdulali/descriptR/issues}
#' }
#'
#' @importFrom dplyr %>% mutate select filter group_by summarize ungroup arrange
#' @importFrom ggplot2 ggplot aes geom_histogram geom_boxplot geom_violin theme_minimal
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr map map_df map_chr map_dbl
#' @importFrom scales percent comma
#' @importFrom rlang .data := !!
#' @importFrom stats mean median sd var IQR quantile cor t.test aov chisq.test shapiro.test
#' @importFrom utils head tail str
#' @importFrom grDevices dev.off png pdf svg
#' @importFrom graphics par plot
NULL
