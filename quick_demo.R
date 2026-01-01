#!/usr/bin/env Rscript
################################################################################
# Quick Demo Script - All descriptR Analyses
################################################################################

library(descriptR)

cat("=================================================================\n")
cat("  descriptR - Quick Demo of All Analyses\n")
cat("=================================================================\n\n")

# Create output directory
dir.create("demo_outputs", showWarnings = FALSE)

#-------------------------------------------------------------------------------
# 1. REGRESSION ANALYSIS
#-------------------------------------------------------------------------------
cat("1. REGRESSION ANALYSIS\n")
cat("======================\n\n")

# Linear regression
cat("Linear Regression: mpg ~ wt + hp\n")
reg <- perform_regression_analysis(mtcars, "mpg", c("wt", "hp"))
print(reg)
cat("\n")

#-------------------------------------------------------------------------------
# 2. ANOVA ANALYSIS
#-------------------------------------------------------------------------------
cat("2. ANOVA ANALYSIS\n")
cat("=================\n\n")

# One-way ANOVA with post-hoc
cat("One-way ANOVA: Sepal.Length ~ Species\n")
anova <- perform_anova(iris, "Sepal.Length", "Species", post_hoc = "tukey")
print(anova)
cat("\n")

#-------------------------------------------------------------------------------
# 3. MISSING DATA IMPUTATION
#-------------------------------------------------------------------------------
cat("3. MISSING DATA IMPUTATION\n")
cat("==========================\n\n")

# Automatic imputation
cat("Auto imputation on airquality dataset\n")
imp <- impute_missing(airquality, method = "auto")
print(imp)
cat("\n")

#-------------------------------------------------------------------------------
# 4. COMPREHENSIVE ANALYSIS - analyze_all()
#-------------------------------------------------------------------------------
cat("4. COMPREHENSIVE ANALYSIS (analyze_all)\n")
cat("=======================================\n\n")

# Run complete analysis on iris
cat("Running analyze_all() on iris dataset...\n\n")
result <- analyze_all(
  iris,
  "demo_outputs/iris_complete",
  group = "Species",
  format = "html",
  title = "Complete Iris Analysis"
)

cat("\n=================================================================\n")
cat("DEMO COMPLETE!\n")
cat("=================================================================\n\n")

cat("What was generated:\n")
cat("  1. Linear regression on mtcars\n")
cat("  2. ANOVA on iris with post-hoc tests\n")
cat("  3. Missing data imputation on airquality\n")
cat("  4. Complete report in: demo_outputs/iris_complete.html\n\n")

cat("Open the HTML file to see all analyses including:\n")
cat("  - Descriptive statistics\n")
cat("  - Missing data analysis\n")
cat("  - Normality tests\n")
cat("  - Outlier detection\n")
cat("  - Correlation analysis\n")
cat("  - Regression analysis (NEW)\n")
cat("  - ANOVA analysis (NEW)\n")
cat("  - Grouped comparisons\n")
cat("  - 50+ visualizations\n\n")

cat("=================================================================\n")
