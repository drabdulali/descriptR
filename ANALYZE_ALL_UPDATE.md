# analyze_all() - Now Includes Phase 1 Analyses

## What Changed

`analyze_all()` has been updated to automatically include all Phase 1 advanced statistical methods:

### New Analyses Added to analyze_all()

1. **Regression Analysis** - Automatically runs when 2+ numeric variables present
   - Auto-detects type (linear, logistic, Poisson)
   - Includes diagnostics and VIF
   - Uses first numeric var as outcome, rest as predictors

2. **ANOVA/MANOVA** - Automatically runs when group variable specified
   - Auto-detects type (one-way, two-way, MANOVA)
   - Includes post-hoc tests (Tukey HSD or Bonferroni)
   - Calculates effect sizes (eta-squared, omega-squared)

3. **Missing Data Imputation** - Automatically runs when missing data present
   - Auto-selects best imputation method
   - Provides quality diagnostics
   - Returns imputed dataset

---

## Updated Function

### analyze_all() Output

When you run:
```r
library(descriptR)
analyze_all(iris, "iris_complete", group = "Species", format = "all")
```

You now see:
```
Running comprehensive analysis...
- Descriptive statistics
- Missing data analysis
- Normality tests
- Outlier detection
- Correlation analysis
- Regression analysis
- Grouped analysis by Species
- ANOVA/MANOVA analysis
- Missing data imputation

Analysis complete!
```

---

## What's Included in Each Report

### 1. Descriptive Statistics
- Mean, SD, Min, Max, Quartiles
- Skewness, Kurtosis
- Confidence intervals

### 2. Missing Data Analysis
- Missing counts and percentages
- Missing patterns
- Recommendations

### 3. Normality Tests
- Shapiro-Wilk test
- Anderson-Darling test
- QQ plots

### 4. Outlier Detection
- Z-score method
- Outlier counts
- Visualization with flagged points

### 5. Correlation Analysis
- Pairwise correlations
- P-values and significance
- Correlation heatmap

### 6. **NEW: Regression Analysis**
- Model coefficients with CI
- R² / Adjusted R² / AIC / BIC
- VIF for multicollinearity
- Diagnostic plots
- Assumption tests

### 7. Grouped Analysis
- Group comparisons
- Effect sizes
- Statistical tests

### 8. **NEW: ANOVA/MANOVA**
- F-statistics and p-values
- Post-hoc pairwise comparisons
- Effect sizes with interpretation
- Assumption checks

### 9. **NEW: Missing Data Imputation**
- Imputed dataset
- Imputation summary
- Quality diagnostics
- Method comparison

### 10. 50+ Visualizations
- All analyses include publication-ready figures

---

## Examples

### Example 1: Complete Analysis with Grouping
```r
library(descriptR)

# Run ALL analyses including Phase 1
analyze_all(
  iris,
  output_file = "iris_complete",
  group = "Species",
  format = "all"
)
```

**What you get:**
- HTML report with embedded plots
- Word document (300 DPI figures)
- Excel workbook with all tables
- Markdown file
- All 9+ analyses automatically performed

**Output includes:**
- Descriptive stats for all numeric variables
- Missing data analysis (if any)
- Normality tests for each variable
- Outlier detection
- Correlation matrix and heatmap
- **Regression model (Sepal.Length ~ other variables)**
- Grouped comparisons by Species
- **ANOVA for Sepal.Length by Species with Tukey HSD**
- **Imputation (if missing data present)**

### Example 2: mtcars with Regression Focus
```r
# Analyze car performance data
analyze_all(
  mtcars,
  output_file = "cars_analysis",
  format = "html"
)
```

**Automatic regression analysis:**
- Outcome: mpg (first numeric variable)
- Predictors: cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb
- Type: Auto-detected (linear regression)
- Diagnostics: VIF, Cook's distance, normality, homoscedasticity

### Example 3: airquality with Imputation
```r
# Dataset with missing data
analyze_all(
  airquality,
  output_file = "air_quality",
  group = "Month",
  format = "word"
)
```

**Automatic imputation:**
- Missing in Ozone: 37 values (24%)
- Missing in Solar.R: 7 values (5%)
- Method: Auto-selected (regression for Ozone, mean for Solar.R)
- Quality check: Distribution validation
- Output: Clean dataset included in report

### Example 4: Interactive Mode
```r
# Let analyze_all() guide you
analyze_all(
  mtcars,
  output_file = "custom_analysis",
  interactive = TRUE,
  format = "all"
)
```

**Interactive prompts:**
1. Select variables to analyze
2. Choose grouping variable (optional)
3. All Phase 1 analyses run automatically based on selections

---

## Accessing Results Programmatically

```r
# Run analysis and capture results
result <- analyze_all(iris, "test", group = "Species")

# Access individual analyses
result$result$analyses$descriptive   # Descriptive stats
result$result$analyses$regression    # Regression results
result$result$analyses$anova         # ANOVA results
result$result$analyses$imputation    # Imputation results
result$result$analyses$correlation   # Correlation matrix
result$result$analyses$normality     # Normality tests
result$result$analyses$outliers      # Outlier detection
result$result$analyses$grouped       # Grouped comparisons

# Access imputed dataset
clean_data <- result$result$analyses$imputation$imputed_data

# Access regression coefficients
coefs <- result$result$analyses$regression$coefficients

# Access ANOVA post-hoc tests
posthoc <- result$result$analyses$anova$post_hoc
```

---

## When Each Analysis Runs

| Analysis | Condition |
|----------|-----------|
| Descriptive | Always (if numeric vars present) |
| Missing Data | Always |
| Normality | If numeric vars present |
| Outliers | If numeric vars present |
| Correlation | If 2+ numeric vars present |
| **Regression** | **If 2+ numeric vars present** |
| Grouped | If group variable specified |
| **ANOVA** | **If group variable specified** |
| **Imputation** | **If missing data present** |

---

## Technical Details

### Updated Function: perform_comprehensive_analysis()

**Location:** `R/integrated_reporting.R`

**Changes:**
```r
# NEW: Regression Analysis
if (length(numeric_vars) >= 2) {
  results$regression <- perform_regression_analysis(
    data,
    outcome = numeric_vars[1],
    predictors = numeric_vars[-1],
    type = "auto",
    diagnostics = TRUE
  )
}

# NEW: ANOVA Analysis
if (!is.null(group) && group %in% names(data) && length(numeric_vars) > 0) {
  results$anova <- perform_anova(
    data,
    outcome = numeric_vars[1],
    groups = group,
    type = "auto",
    post_hoc = "auto",
    effect_size = TRUE
  )
}

# NEW: Missing Data Imputation
if (any missing data present) {
  results$imputation <- impute_missing(
    data,
    method = "auto",
    diagnostics = TRUE
  )
}
```

---

## Output Format

### HTML Report Structure
1. Executive Summary
2. Dataset Overview
3. **Descriptive Statistics** (tables + histograms)
4. **Missing Data Analysis** (patterns + visualization)
5. **Normality Assessment** (tests + QQ plots)
6. **Outlier Detection** (flagged points + boxplots)
7. **Correlation Analysis** (matrix + heatmap)
8. **Regression Analysis** (coefficients + diagnostics + plots)
9. **Grouped Comparisons** (tables + violin plots)
10. **ANOVA Results** (F-tests + post-hoc + effect sizes)
11. **Missing Data Imputation** (summary + quality checks)
12. Automated Insights
13. Appendix (all visualizations)

### Word Document
- Same structure as HTML
- All figures embedded at 300 DPI
- Professional journal formatting
- Ready for publication

### Excel Workbook
- Separate sheets for each analysis
- Descriptive stats sheet
- Correlation matrix sheet
- **Regression coefficients sheet**
- **ANOVA table sheet**
- **Imputation summary sheet**
- Missing data summary sheet
- Outlier details sheet

---

## Reinstalling Package

To get the updated `analyze_all()`:

```bash
cd /path/to/descriptR
Rscript reinstall.R
```

Or manually:
```r
remove.packages("descriptR")
devtools::install()
library(descriptR)
```

---

## Testing the Update

```r
library(descriptR)

# Test with iris (includes grouping for ANOVA)
analyze_all(iris, "test_iris", group = "Species", format = "html")

# Test with airquality (includes missing data for imputation)
analyze_all(airquality, "test_air", format = "html")

# Test with mtcars (includes regression)
analyze_all(mtcars, "test_cars", format = "html")
```

Check each HTML report to verify all analyses are included.

---

## Summary

**Before:** analyze_all() performed 6 core analyses
**Now:** analyze_all() performs 9+ analyses including advanced statistical methods

**New Capabilities:**
- Automatic regression modeling with diagnostics
- ANOVA/MANOVA with post-hoc tests when groups specified
- Smart missing data imputation when needed

**Benefits:**
- No extra function calls needed
- Everything happens automatically
- One function, complete analysis
- Publication-ready output

**Updated Files:**
- `R/integrated_reporting.R` - Added Phase 1 analyses to comprehensive function
- `R/interactive_analysis.R` - Updated console output messages
- `README.md` - Updated documentation

**Commit:** 976a868
**Status:** Live on GitHub (both branches)

---

**analyze_all() is now more powerful than ever!**
