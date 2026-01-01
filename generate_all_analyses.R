#!/usr/bin/env Rscript
################################################################################
# Complete Analysis Generation Script for descriptR Package
# This script demonstrates all Phase 1 functions and analyze_all() integration
################################################################################

cat("================================================================================\n")
cat("  descriptR - Complete Analysis Generation Script\n")
cat("================================================================================\n\n")

# Load the package
cat("Loading descriptR package...\n")
library(descriptR)
cat("Package loaded successfully!\n\n")

# Create output directory
output_dir <- "descriptR_analysis_outputs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat("Created output directory:", output_dir, "\n\n")
}

################################################################################
# PART 1: INDIVIDUAL PHASE 1 FUNCTION DEMONSTRATIONS
################################################################################

cat("================================================================================\n")
cat("PART 1: INDIVIDUAL PHASE 1 FUNCTION DEMONSTRATIONS\n")
cat("================================================================================\n\n")

#-------------------------------------------------------------------------------
# 1.1 REGRESSION ANALYSIS
#-------------------------------------------------------------------------------
cat("1. REGRESSION ANALYSIS\n")
cat("----------------------\n\n")

# Example 1.1: Linear Regression
cat("1.1 Linear Regression (mtcars: mpg ~ wt + hp + cyl)\n")
cat("----------------------------------------------------\n")
reg_linear <- perform_regression_analysis(
  data = mtcars,
  outcome = "mpg",
  predictors = c("wt", "hp", "cyl"),
  type = "linear",
  diagnostics = TRUE
)
print(reg_linear)
cat("\n")

# Access regression components
cat("Accessing regression components:\n")
cat("  - R²:", round(reg_linear$model_fit$r_squared, 3), "\n")
cat("  - Adjusted R²:", round(reg_linear$model_fit$adj_r_squared, 3), "\n")
cat("  - Number of significant predictors:",
    sum(reg_linear$coefficients$p_value < 0.05, na.rm = TRUE) - 1, "\n")
cat("\n")

# Example 1.2: Logistic Regression
cat("1.2 Logistic Regression (mtcars: am ~ mpg + wt + hp)\n")
cat("------------------------------------------------------\n")
reg_logistic <- perform_regression_analysis(
  data = mtcars,
  outcome = "am",
  predictors = c("mpg", "wt", "hp"),
  type = "logistic"
)
print(reg_logistic)
cat("\n")

# Example 1.3: Regression with Interactions
cat("1.3 Linear Regression with Interactions\n")
cat("----------------------------------------\n")
reg_interactions <- perform_regression_analysis(
  data = mtcars,
  outcome = "mpg",
  predictors = c("wt", "hp"),
  type = "linear",
  interactions = TRUE,
  diagnostics = TRUE
)
cat("Model includes interaction term: wt:hp\n")
cat("VIF values:\n")
print(reg_interactions$assumptions$multicollinearity$vif)
cat("\n")

# Example 1.4: Stepwise Regression
cat("1.4 Stepwise Variable Selection\n")
cat("--------------------------------\n")
reg_stepwise <- perform_regression_analysis(
  data = mtcars,
  outcome = "mpg",
  predictors = c("cyl", "disp", "hp", "drat", "wt", "qsec"),
  type = "linear",
  step_wise = TRUE
)
cat("Stepwise regression completed\n")
cat("Final model predictors included\n\n")

#-------------------------------------------------------------------------------
# 1.2 ANOVA ANALYSIS
#-------------------------------------------------------------------------------
cat("\n2. ANOVA ANALYSIS\n")
cat("-----------------\n\n")

# Example 2.1: One-way ANOVA with Tukey HSD
cat("2.1 One-way ANOVA (iris: Sepal.Length ~ Species)\n")
cat("-------------------------------------------------\n")
anova_oneway <- perform_anova(
  data = iris,
  outcome = "Sepal.Length",
  groups = "Species",
  type = "one-way",
  post_hoc = "tukey",
  effect_size = TRUE
)
print(anova_oneway)
cat("\n")

# Access ANOVA components
cat("Accessing ANOVA components:\n")
cat("  - F-statistic:", round(anova_oneway$anova_table$`F value`[1], 2), "\n")
cat("  - Effect size (eta²):", round(anova_oneway$effect_sizes$eta_squared, 3), "\n")
cat("  - Interpretation:", anova_oneway$effect_sizes$interpretation, "\n")
cat("\n")

# Example 2.2: Two-way ANOVA
cat("2.2 Two-way ANOVA (mtcars: mpg ~ cyl + am)\n")
cat("-------------------------------------------\n")
mtcars_copy <- mtcars
mtcars_copy$cyl <- factor(mtcars_copy$cyl)
mtcars_copy$am <- factor(mtcars_copy$am)

anova_twoway <- perform_anova(
  data = mtcars_copy,
  outcome = "mpg",
  groups = c("cyl", "am"),
  type = "two-way",
  effect_size = TRUE
)
print(anova_twoway)
cat("\n")

# Example 2.3: MANOVA (Multiple Outcomes)
cat("2.3 MANOVA (iris: Sepal.Length + Sepal.Width ~ Species)\n")
cat("--------------------------------------------------------\n")
manova_result <- perform_anova(
  data = iris,
  outcome = c("Sepal.Length", "Sepal.Width", "Petal.Length"),
  groups = "Species",
  type = "manova"
)
cat("MANOVA completed for", length(manova_result$metadata$outcome), "outcome variables\n")
cat("All outcomes show significant group differences\n\n")

# Example 2.4: ANOVA with Bonferroni correction
cat("2.4 One-way ANOVA with Bonferroni post-hoc\n")
cat("-------------------------------------------\n")
anova_bonf <- perform_anova(
  data = iris,
  outcome = "Petal.Width",
  groups = "Species",
  type = "one-way",
  post_hoc = "bonferroni"
)
cat("Post-hoc tests with Bonferroni correction completed\n\n")

#-------------------------------------------------------------------------------
# 1.3 MISSING DATA IMPUTATION
#-------------------------------------------------------------------------------
cat("\n3. MISSING DATA IMPUTATION\n")
cat("--------------------------\n\n")

# Example 3.1: Mean Imputation
cat("3.1 Mean Imputation (airquality)\n")
cat("--------------------------------\n")
cat("Original missing data:\n")
cat("  - Ozone:", sum(is.na(airquality$Ozone)), "values\n")
cat("  - Solar.R:", sum(is.na(airquality$Solar.R)), "values\n\n")

impute_mean <- impute_missing(
  data = airquality,
  method = "mean",
  vars = c("Ozone", "Solar.R"),
  diagnostics = TRUE
)
print(impute_mean)
cat("\n")

# Verify imputation
cat("After imputation:\n")
cat("  - Ozone:", sum(is.na(impute_mean$imputed_data$Ozone)), "values\n")
cat("  - Solar.R:", sum(is.na(impute_mean$imputed_data$Solar.R)), "values\n\n")

# Example 3.2: Median Imputation
cat("3.2 Median Imputation\n")
cat("---------------------\n")
impute_median <- impute_missing(
  data = airquality,
  method = "median",
  vars = "Ozone"
)
cat("Imputed", impute_median$metadata$n_imputed, "values using median\n\n")

# Example 3.3: Regression Imputation
cat("3.3 Regression Imputation\n")
cat("-------------------------\n")
impute_regression <- impute_missing(
  data = airquality,
  method = "regression",
  diagnostics = TRUE
)
cat("Method:", impute_regression$method_used, "\n")
cat("Variables imputed:", paste(impute_regression$metadata$vars_imputed, collapse = ", "), "\n\n")

# Example 3.4: Automatic Method Selection
cat("3.4 Automatic Method Selection\n")
cat("-------------------------------\n")
impute_auto <- impute_missing(
  data = airquality,
  method = "auto"
)
cat("Auto-selected method:", impute_auto$method_used, "\n")
cat("Rationale: Based on percentage of missing data\n\n")

# Example 3.5: KNN Imputation
cat("3.5 KNN Imputation\n")
cat("------------------\n")
impute_knn <- impute_missing(
  data = airquality,
  method = "knn",
  diagnostics = FALSE
)
cat("KNN imputation completed\n")
cat("Preserves data distribution better than mean/median\n\n")

################################################################################
# PART 2: analyze_all() - COMPREHENSIVE ANALYSIS
################################################################################

cat("\n================================================================================\n")
cat("PART 2: analyze_all() - COMPREHENSIVE ANALYSIS WITH ALL PHASE 1 FEATURES\n")
cat("================================================================================\n\n")

#-------------------------------------------------------------------------------
# 2.1 Complete Analysis: iris dataset
#-------------------------------------------------------------------------------
cat("2.1 COMPLETE ANALYSIS: iris dataset\n")
cat("====================================\n\n")

cat("Running analyze_all() on iris with grouping by Species...\n\n")

result_iris <- analyze_all(
  data = iris,
  output_file = file.path(output_dir, "iris_complete"),
  group = "Species",
  format = "html",
  title = "Complete Iris Dataset Analysis - All Methods",
  author = "descriptR Package"
)

cat("\nAnalyses performed on iris:\n")
cat("  1. Descriptive statistics for all numeric variables\n")
cat("  2. Missing data analysis (none present)\n")
cat("  3. Normality tests for each variable\n")
cat("  4. Outlier detection\n")
cat("  5. Correlation analysis (4 numeric variables)\n")
cat("  6. Regression analysis (Sepal.Length ~ other variables)\n")
cat("  7. Grouped comparisons by Species\n")
cat("  8. ANOVA analysis (Sepal.Length ~ Species)\n")
cat("  9. No imputation needed (no missing data)\n\n")

# Access specific results
cat("Sample results from iris analysis:\n")
cat("  - Regression R²:",
    round(result_iris$result$analyses$regression$model_fit$r_squared, 3), "\n")
cat("  - ANOVA F-value:",
    round(result_iris$result$analyses$anova$anova_table$`F value`[1], 2), "\n")
cat("  - Total analyses performed:",
    result_iris$result$metadata$n_analyses, "\n\n")

#-------------------------------------------------------------------------------
# 2.2 Complete Analysis: airquality dataset
#-------------------------------------------------------------------------------
cat("\n2.2 COMPLETE ANALYSIS: airquality dataset\n")
cat("==========================================\n\n")

cat("Running analyze_all() on airquality (includes missing data)...\n\n")

result_air <- analyze_all(
  data = airquality,
  output_file = file.path(output_dir, "airquality_complete"),
  format = "html",
  title = "Air Quality Analysis with Missing Data Imputation",
  author = "descriptR Package"
)

cat("\nAnalyses performed on airquality:\n")
cat("  1. Descriptive statistics\n")
cat("  2. Missing data analysis (Ozone: 24%, Solar.R: 5%)\n")
cat("  3. Normality tests\n")
cat("  4. Outlier detection\n")
cat("  5. Correlation analysis\n")
cat("  6. Regression analysis\n")
cat("  7. No grouped analysis (no group variable)\n")
cat("  8. No ANOVA (no group variable)\n")
cat("  9. Missing data imputation (automatic method selection)\n\n")

# Access imputation results
if (!is.null(result_air$result$analyses$imputation)) {
  cat("Imputation results:\n")
  cat("  - Method used:", result_air$result$analyses$imputation$method_used, "\n")
  cat("  - Total values imputed:",
      result_air$result$analyses$imputation$metadata$n_imputed, "\n\n")
}

#-------------------------------------------------------------------------------
# 2.3 Complete Analysis: mtcars dataset
#-------------------------------------------------------------------------------
cat("\n2.3 COMPLETE ANALYSIS: mtcars dataset\n")
cat("======================================\n\n")

cat("Running analyze_all() on mtcars with grouping by cylinder...\n\n")

# Convert cyl to factor for grouping
mtcars_grouped <- mtcars
mtcars_grouped$cyl <- factor(mtcars_grouped$cyl)

result_cars <- analyze_all(
  data = mtcars_grouped,
  output_file = file.path(output_dir, "mtcars_complete"),
  group = "cyl",
  format = "html",
  title = "Motor Trend Car Analysis - Performance by Cylinder",
  author = "descriptR Package"
)

cat("\nAnalyses performed on mtcars:\n")
cat("  1. Descriptive statistics for all variables\n")
cat("  2. Missing data analysis (none present)\n")
cat("  3. Normality tests\n")
cat("  4. Outlier detection\n")
cat("  5. Correlation analysis (high correlation: disp-cyl, wt-disp)\n")
cat("  6. Regression analysis (mpg ~ all other variables)\n")
cat("  7. Grouped comparisons by cylinder count\n")
cat("  8. ANOVA analysis (mpg ~ cylinder)\n")
cat("  9. No imputation needed\n\n")

# Access regression and ANOVA results
cat("Sample results:\n")
if (!is.null(result_cars$result$analyses$regression)) {
  cat("  - Regression Adjusted R²:",
      round(result_cars$result$analyses$regression$model_fit$adj_r_squared, 3), "\n")
}
if (!is.null(result_cars$result$analyses$anova)) {
  cat("  - ANOVA effect size:",
      result_cars$result$analyses$anova$effect_sizes$interpretation, "\n")
}
cat("\n")

################################################################################
# PART 3: MULTIPLE OUTPUT FORMATS
################################################################################

cat("\n================================================================================\n")
cat("PART 3: MULTIPLE OUTPUT FORMAT DEMONSTRATION\n")
cat("================================================================================\n\n")

cat("3.1 Generating reports in ALL formats (HTML, Word, Excel, Markdown)...\n\n")

result_all_formats <- analyze_all(
  data = iris,
  output_file = file.path(output_dir, "iris_all_formats"),
  group = "Species",
  format = "all",  # Generate all formats
  title = "Iris Analysis - All Output Formats",
  author = "descriptR Package"
)

cat("\nGenerated files:\n")
for (path in result_all_formats$report_paths) {
  cat(sprintf("  - %s\n", basename(path)))
}
cat("\n")

################################################################################
# PART 4: ADVANCED USE CASES
################################################################################

cat("\n================================================================================\n")
cat("PART 4: ADVANCED USE CASES\n")
cat("================================================================================\n\n")

#-------------------------------------------------------------------------------
# 4.1 Custom Variable Selection
#-------------------------------------------------------------------------------
cat("4.1 Custom Variable Selection\n")
cat("------------------------------\n\n")

# Select specific variables for analysis
selected_vars <- c("Sepal.Length", "Sepal.Width", "Petal.Length")

result_custom <- analyze_all(
  data = iris,
  output_file = file.path(output_dir, "iris_custom_vars"),
  vars = selected_vars,
  group = "Species",
  format = "html",
  title = "Iris Analysis - Sepal Characteristics Only"
)

cat("Analyzed only selected variables:", paste(selected_vars, collapse = ", "), "\n\n")

#-------------------------------------------------------------------------------
# 4.2 Accessing Individual Analysis Results Programmatically
#-------------------------------------------------------------------------------
cat("4.2 Accessing Results Programmatically\n")
cat("---------------------------------------\n\n")

# Use iris results from earlier
analyses <- result_iris$result$analyses

cat("Available analyses:\n")
for (name in names(analyses)) {
  if (!is.null(analyses[[name]])) {
    cat(sprintf("  - %s: Available\n", name))
  }
}
cat("\n")

# Extract specific information
cat("Extracting specific information:\n\n")

# Descriptive statistics
if (!is.null(analyses$descriptive)) {
  cat("Descriptive Statistics (first variable):\n")
  desc_table <- analyses$descriptive$descriptive_table
  print(head(desc_table, 1))
  cat("\n")
}

# Regression coefficients
if (!is.null(analyses$regression)) {
  cat("Regression Coefficients:\n")
  print(analyses$regression$coefficients)
  cat("\n")
}

# ANOVA results
if (!is.null(analyses$anova)) {
  cat("ANOVA Table:\n")
  print(analyses$anova$anova_table)
  cat("\n")

  cat("Post-hoc Tests (Tukey HSD):\n")
  print(analyses$anova$post_hoc$tukey)
  cat("\n")
}

# Correlation matrix
if (!is.null(analyses$correlation)) {
  cat("Correlation Matrix:\n")
  print(round(analyses$correlation$cor_matrix, 3))
  cat("\n")
}

#-------------------------------------------------------------------------------
# 4.3 Using Imputed Data for Further Analysis
#-------------------------------------------------------------------------------
cat("4.3 Using Imputed Data for Further Analysis\n")
cat("--------------------------------------------\n\n")

# Get imputed dataset from airquality
if (!is.null(result_air$result$analyses$imputation)) {
  clean_data <- result_air$result$analyses$imputation$imputed_data

  cat("Original airquality missing values:\n")
  cat("  - Ozone:", sum(is.na(airquality$Ozone)), "\n")
  cat("  - Solar.R:", sum(is.na(airquality$Solar.R)), "\n\n")

  cat("Imputed dataset missing values:\n")
  cat("  - Ozone:", sum(is.na(clean_data$Ozone)), "\n")
  cat("  - Solar.R:", sum(is.na(clean_data$Solar.R)), "\n\n")

  # Run regression on imputed data
  cat("Running regression on imputed dataset...\n")
  reg_imputed <- perform_regression_analysis(
    clean_data,
    outcome = "Ozone",
    predictors = c("Solar.R", "Wind", "Temp"),
    type = "linear"
  )
  cat("Regression completed on complete dataset (no missing values)\n")
  cat("R² =", round(reg_imputed$model_fit$r_squared, 3), "\n\n")
}

################################################################################
# PART 5: SUMMARY OF ALL GENERATED FILES
################################################################################

cat("\n================================================================================\n")
cat("PART 5: SUMMARY OF ALL GENERATED FILES\n")
cat("================================================================================\n\n")

# List all generated files
generated_files <- list.files(output_dir, full.names = FALSE)

cat("Total files generated:", length(generated_files), "\n\n")

cat("Files by type:\n")
html_files <- grep("\\.html$", generated_files, value = TRUE)
docx_files <- grep("\\.docx$", generated_files, value = TRUE)
xlsx_files <- grep("\\.xlsx$", generated_files, value = TRUE)
md_files <- grep("\\.md$", generated_files, value = TRUE)

cat("  - HTML reports:", length(html_files), "\n")
cat("  - Word documents:", length(docx_files), "\n")
cat("  - Excel workbooks:", length(xlsx_files), "\n")
cat("  - Markdown files:", length(md_files), "\n\n")

cat("All generated files:\n")
for (file in sort(generated_files)) {
  cat(sprintf("  - %s\n", file))
}
cat("\n")

cat("Output location:", normalizePath(output_dir), "\n\n")

################################################################################
# FINAL SUMMARY
################################################################################

cat("================================================================================\n")
cat("ANALYSIS GENERATION COMPLETE!\n")
cat("================================================================================\n\n")

cat("Summary of what was generated:\n\n")

cat("INDIVIDUAL FUNCTION DEMONSTRATIONS:\n")
cat("  1. Regression Analysis:\n")
cat("     - Linear regression (standard)\n")
cat("     - Logistic regression\n")
cat("     - Regression with interactions\n")
cat("     - Stepwise variable selection\n\n")

cat("  2. ANOVA Analysis:\n")
cat("     - One-way ANOVA with Tukey HSD\n")
cat("     - Two-way ANOVA\n")
cat("     - MANOVA (multiple outcomes)\n")
cat("     - Bonferroni post-hoc correction\n\n")

cat("  3. Missing Data Imputation:\n")
cat("     - Mean imputation\n")
cat("     - Median imputation\n")
cat("     - Regression imputation\n")
cat("     - Automatic method selection\n")
cat("     - KNN imputation\n\n")

cat("COMPREHENSIVE REPORTS (analyze_all):\n")
cat("  1. Iris dataset - Complete analysis with grouping\n")
cat("  2. Airquality dataset - Analysis with missing data imputation\n")
cat("  3. mtcars dataset - Analysis with regression and ANOVA\n")
cat("  4. Multiple format outputs - HTML, Word, Excel, Markdown\n")
cat("  5. Custom variable selection demonstration\n\n")

cat("TOTAL ANALYSES PERFORMED:\n")
cat("  - Regression models: 5\n")
cat("  - ANOVA analyses: 4\n")
cat("  - Imputation methods: 5\n")
cat("  - Comprehensive reports: 5\n")
cat("  - Output files: ", length(generated_files), "\n\n")

cat("================================================================================\n")
cat("You can now open the HTML files in:", output_dir, "\n")
cat("All analyses demonstrate the complete Phase 1 functionality!\n")
cat("================================================================================\n")
