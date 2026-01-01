# Testing Guide for Phase 1 Functions

## Quick Verification (3 methods)

### Method 1: Run Quick Test Script (Fastest)
```bash
cd /path/to/descriptR
Rscript quick_test.R
```

### Method 2: Run Comprehensive Test Suite
```bash
Rscript test_phase1_functions.R
```

### Method 3: Interactive R Session
```r
# In R console
setwd("/path/to/descriptR")
devtools::load_all(".")

# Test each function
reg <- perform_regression_analysis(mtcars, "mpg", c("wt", "hp"))
anova <- perform_anova(iris, "Sepal.Length", "Species")
imp <- impute_missing(airquality, method = "mean")
```

---

## Detailed Testing Examples

### 1. Regression Analysis

#### Linear Regression
```r
library(descriptR)

# Basic linear regression
result <- perform_regression_analysis(
  data = mtcars,
  outcome = "mpg",
  predictors = c("wt", "hp", "cyl")
)

# View results
print(result)

# Access components
result$coefficients          # Coefficient estimates
result$model_fit             # R², AIC, BIC
result$assumptions           # VIF, normality, homoscedasticity
result$insights              # Automated interpretations
```

#### Logistic Regression
```r
# Binary outcome (automatic detection)
result <- perform_regression_analysis(
  data = mtcars,
  outcome = "am",              # 0/1 variable
  predictors = c("mpg", "wt"),
  type = "auto"                # Auto-detects logistic
)

# View coefficients
print(result$coefficients)
```

#### Advanced Features
```r
# With interactions and polynomial terms
result <- perform_regression_analysis(
  data = mtcars,
  outcome = "mpg",
  predictors = c("wt", "hp"),
  interactions = TRUE,         # Include wt*hp interaction
  polynomial = TRUE,           # Include wt² and hp²
  step_wise = TRUE,            # Automatic variable selection
  diagnostics = TRUE           # Full diagnostics
)

# Check VIF for multicollinearity
result$assumptions$multicollinearity$vif

# Check influential points
result$diagnostics$influential_points
```

### 2. ANOVA Analysis

#### One-way ANOVA
```r
# Basic one-way ANOVA
result <- perform_anova(
  data = iris,
  outcome = "Sepal.Length",
  groups = "Species"
)

# With Tukey HSD post-hoc test
result <- perform_anova(
  data = iris,
  outcome = "Sepal.Length",
  groups = "Species",
  type = "one-way",
  post_hoc = "tukey"
)

# View ANOVA table
print(result$anova_table)

# View post-hoc results
print(result$post_hoc$tukey)

# View effect sizes
print(result$effect_sizes$interpretation)
```

#### Two-way ANOVA
```r
# Prepare data (factors required)
mtcars_copy <- mtcars
mtcars_copy$cyl <- factor(mtcars_copy$cyl)
mtcars_copy$am <- factor(mtcars_copy$am)

# Two-way ANOVA with interaction
result <- perform_anova(
  data = mtcars_copy,
  outcome = "mpg",
  groups = c("cyl", "am"),
  type = "two-way"
)

# Check for interaction effects
print(result$anova_table)
```

#### MANOVA (Multiple Outcomes)
```r
# Multiple dependent variables
result <- perform_anova(
  data = iris,
  outcome = c("Sepal.Length", "Sepal.Width", "Petal.Length"),
  groups = "Species",
  type = "manova"
)

print(result)
```

### 3. Missing Data Imputation

#### Mean Imputation
```r
# Simple mean imputation
result <- impute_missing(
  data = airquality,
  method = "mean",
  vars = c("Ozone", "Solar.R")
)

# Access imputed data
clean_data <- result$imputed_data

# View imputation summary
print(result$imputation_summary)

# Check diagnostics
print(result$diagnostics)
```

#### Automatic Method Selection
```r
# Let the function choose the best method
result <- impute_missing(
  data = airquality,
  method = "auto"  # Selects based on % missing
)

# See which method was used
print(result$method_used)
```

#### Regression Imputation
```r
# Predict missing values using other variables
result <- impute_missing(
  data = airquality,
  method = "regression",
  diagnostics = TRUE
)

# Compare original vs imputed
print(result$imputation_summary)
```

#### Multiple Imputation (MICE)
```r
# For higher missing data percentages
result <- impute_missing(
  data = airquality,
  method = "mice",
  m = 10,          # Number of imputations
  seed = 123       # For reproducibility
)
```

#### KNN Imputation
```r
result <- impute_missing(
  data = airquality,
  method = "knn"
)
```

---

## Verification Checklist

### Files Present
- [ ] `R/regression_analysis.R` (~528 lines)
- [ ] `R/anova_analysis.R` (~483 lines)
- [ ] `R/imputation.R` (~488 lines)

### NAMESPACE Exports
- [ ] `export(perform_regression_analysis)`
- [ ] `export(perform_anova)`
- [ ] `export(impute_missing)`

### Functions Work
- [ ] `perform_regression_analysis()` runs without errors
- [ ] `perform_anova()` runs without errors
- [ ] `impute_missing()` runs without errors

### Results Structure
- [ ] Returns proper S3 class objects
- [ ] Print methods work
- [ ] Components accessible via `$`

### Output Quality
- [ ] Coefficients include p-values and significance
- [ ] Effect sizes calculated correctly
- [ ] Insights are meaningful
- [ ] Diagnostics provide useful information

---

## Expected Output Examples

### Regression Output
```
Regression Analysis Results
==================================================
Type: linear regression
Outcome: mpg
Predictors: wt, hp
N = 32 observations

Model Fit:
  r_squared: 0.8268
  adj_r_squared: 0.8148
  AIC: 155.5
  RMSE: 2.593

Coefficients:
  term        estimate std_error statistic p_value significance
1 (Intercept)  37.227     1.599    23.284  <0.001          ***
2 wt           -3.878     0.633    -6.129  <0.001          ***
3 hp           -0.032     0.009    -3.519   0.001           **

Insights:
  - Model explains 81.5% of variance (Adjusted R² = 0.815)
  - Significant predictors: wt, hp
  - Strongest predictor: wt (negative effect, β = -3.878)
==================================================
```

### ANOVA Output
```
ANOVA Analysis Results
==================================================
Type: one-way ANOVA
Outcome: Sepal.Length
Groups: Species
N = 150 observations

ANOVA Table:
  term        Df Sum Sq Mean Sq F value  Pr(>F)
1 Species      2  63.21  31.606 119.265 <0.001
2 Residuals  147  38.96   0.265

Effect Sizes:
  term     eta_squared interpretation
1 Species        0.619          large

Insights:
  - Significant effects: Species
  - Large effect sizes for: Species
  - Significant pairwise differences: setosa-versicolor, ...
==================================================
```

### Imputation Output
```
Missing Data Imputation Results
==================================================
Method: mean
Total imputed: 44 values
Variables: Ozone, Solar.R

Imputation Summary:
  variable n_imputed original_mean imputed_mean
1    Ozone        37        42.129       42.129
2 Solar.R          7       185.931      185.931

Insights:
  - Imputed 44 missing values using mean method
  - Variables imputed: Ozone, Solar.R
==================================================

Use $imputed_data to access the imputed dataset
```

---

## Troubleshooting

### Function Not Found
```r
# Reload package
devtools::load_all(".")

# Check exports
getNamespaceExports("descriptR")
```

### Help Not Available
```r
# Regenerate documentation
devtools::document()
```

### Tests Fail
```r
# Check for package dependencies
devtools::check()

# Install missing packages
install.packages(c("stats", "dplyr", "ggplot2"))
```

---

## Build and Install

### Build from Source
```bash
cd /path/to/descriptR
R CMD build .
R CMD INSTALL descriptR_0.1.0.tar.gz
```

### Load and Test
```r
library(descriptR)
?perform_regression_analysis
?perform_anova
?impute_missing
```

---

## Next Steps

After verifying Phase 1 functions work:

1. Run comprehensive tests: `Rscript test_phase1_functions.R`
2. Check documentation: `devtools::check()`
3. Run examples in README
4. Test with your own data
5. Report any issues on GitHub

---

**All Phase 1 functions are ready to use!**
