# How to Run All Analyses

This guide shows you how to generate all analyses using the descriptR package with Phase 1 features.

---

## Quick Start (2 Methods)

### Method 1: Quick Demo (Fastest - 1 minute)

```bash
cd /path/to/descriptR
Rscript quick_demo.R
```

**What it does:**
- Tests regression analysis
- Tests ANOVA analysis
- Tests missing data imputation
- Generates one complete HTML report
- Creates `demo_outputs/` folder with results

**Output:** 1 HTML file with all analyses

---

### Method 2: Complete Analysis (Comprehensive - 5 minutes)

```bash
Rscript generate_all_analyses.R
```

**What it does:**
- Demonstrates ALL Phase 1 functions individually
- Generates multiple complete reports
- Creates output in ALL formats (HTML, Word, Excel, Markdown)
- Shows advanced use cases
- Creates `descriptR_analysis_outputs/` folder

**Output:** 15+ files demonstrating every feature

---

## What Each Script Does

### quick_demo.R

**Purpose:** Fast verification that everything works

**Demonstrates:**
1. Linear regression on mtcars
2. ANOVA with post-hoc on iris
3. Auto imputation on airquality
4. Complete analyze_all() report

**Runtime:** ~1 minute
**Output Files:** 1 HTML report

**When to use:** Quick testing after installation

---

### generate_all_analyses.R

**Purpose:** Complete demonstration of all features

**Part 1 - Individual Functions:**
- 4 regression examples (linear, logistic, interactions, stepwise)
- 4 ANOVA examples (one-way, two-way, MANOVA, Bonferroni)
- 5 imputation examples (mean, median, regression, auto, KNN)

**Part 2 - analyze_all() Integration:**
- Iris dataset (with grouping → includes ANOVA)
- Airquality dataset (with missing data → includes imputation)
- mtcars dataset (with regression focus)

**Part 3 - Multiple Formats:**
- Generates HTML, Word, Excel, and Markdown outputs

**Part 4 - Advanced:**
- Custom variable selection
- Programmatic result access
- Using imputed data for further analysis

**Runtime:** ~5 minutes
**Output Files:** 15+ analysis reports

**When to use:** Full feature demonstration, documentation examples

---

## Output Directory Structure

After running scripts, you'll have:

```
descriptR/
├── demo_outputs/                    # From quick_demo.R
│   └── iris_complete.html
│
└── descriptR_analysis_outputs/      # From generate_all_analyses.R
    ├── iris_complete.html
    ├── iris_complete.docx
    ├── iris_complete.xlsx
    ├── iris_complete.md
    ├── airquality_complete.html
    ├── mtcars_complete.html
    ├── iris_all_formats.html
    ├── iris_all_formats.docx
    ├── iris_all_formats.xlsx
    ├── iris_all_formats.md
    └── iris_custom_vars.html
```

---

## Detailed Examples

### Example 1: Just Test Regression

```r
library(descriptR)

# Simple linear regression
result <- perform_regression_analysis(
  mtcars,
  outcome = "mpg",
  predictors = c("wt", "hp", "cyl")
)

print(result)
```

**Output:**
- Coefficients with p-values
- R², Adjusted R², AIC, BIC
- VIF for multicollinearity
- Assumption tests
- Automated insights

---

### Example 2: Just Test ANOVA

```r
library(descriptR)

# One-way ANOVA with post-hoc
result <- perform_anova(
  iris,
  outcome = "Sepal.Length",
  groups = "Species",
  post_hoc = "tukey"
)

print(result)
```

**Output:**
- ANOVA table with F-statistics
- Tukey HSD pairwise comparisons
- Effect sizes (eta², omega²)
- Assumption checks
- Automated insights

---

### Example 3: Just Test Imputation

```r
library(descriptR)

# Auto-select best imputation method
result <- impute_missing(
  airquality,
  method = "auto"
)

print(result)

# Get clean data
clean_data <- result$imputed_data
```

**Output:**
- Imputation summary
- Method used
- Quality diagnostics
- Clean dataset (no missing values)

---

### Example 4: Complete Report on Your Data

```r
library(descriptR)

# Replace 'your_data' with your actual dataset
analyze_all(
  your_data,
  output_file = "my_analysis",
  group = "your_grouping_variable",  # Optional
  format = "all",                     # HTML, Word, Excel, Markdown
  title = "My Complete Analysis"
)
```

**Output:**
- HTML report (interactive, with plots)
- Word document (300 DPI figures embedded)
- Excel workbook (all tables)
- Markdown file (portable)

**Includes:**
- Descriptive statistics
- Missing data analysis
- Normality tests
- Outlier detection
- Correlation analysis
- **Regression analysis** ← NEW
- Grouped comparisons (if group specified)
- **ANOVA/MANOVA** ← NEW (if group specified)
- **Missing data imputation** ← NEW (if missing data present)
- 50+ visualizations

---

## Step-by-Step: First Time User

### Step 1: Install/Update Package

```bash
cd /path/to/descriptR
Rscript reinstall.R
```

### Step 2: Run Quick Demo

```bash
Rscript quick_demo.R
```

**Expected output:**
```
=================================================================
  descriptR - Quick Demo of All Analyses
=================================================================

1. REGRESSION ANALYSIS
======================

Linear Regression: mpg ~ wt + hp

Regression Analysis Results
==================================================
Type: linear regression
Outcome: mpg
...

2. ANOVA ANALYSIS
=================

One-way ANOVA: Sepal.Length ~ Species
...

3. MISSING DATA IMPUTATION
==========================
...

4. COMPREHENSIVE ANALYSIS (analyze_all)
=======================================

Running analyze_all() on iris dataset...
...

DEMO COMPLETE!
```

### Step 3: Open HTML Report

```bash
open demo_outputs/iris_complete.html
# or on Linux: xdg-open demo_outputs/iris_complete.html
```

**Check that the report includes:**
- ✓ Descriptive statistics tables
- ✓ Missing data analysis
- ✓ Normality test results
- ✓ Outlier detection plots
- ✓ Correlation heatmap
- ✓ **Regression analysis section** ← NEW
- ✓ Grouped comparison plots
- ✓ **ANOVA results with post-hoc** ← NEW
- ✓ 50+ visualizations

### Step 4: Run Complete Analysis (Optional)

```bash
Rscript generate_all_analyses.R
```

**This will take ~5 minutes and generate:**
- 15+ report files
- All output formats
- Complete feature demonstrations

---

## Troubleshooting

### Error: "could not find function 'perform_regression_analysis'"

**Solution:** Reinstall the package

```bash
Rscript reinstall.R
```

---

### Error: "package 'descriptR' is not available"

**Solution:** Make sure you're in the package directory

```bash
cd /path/to/descriptR
pwd  # Should show .../descriptR
ls   # Should show DESCRIPTION file
Rscript reinstall.R
```

---

### Scripts run but no Phase 1 analyses in report

**Solution:** You have an old version installed

```bash
# Remove completely
R --vanilla -e "remove.packages('descriptR')"

# Reinstall fresh
cd /path/to/descriptR
Rscript reinstall.R

# Verify
R --vanilla -e "library(descriptR); exists('perform_regression_analysis')"
# Should return: [1] TRUE
```

---

### Want to see console output only (no files)

```r
library(descriptR)

# Regression (prints to console)
perform_regression_analysis(mtcars, "mpg", c("wt", "hp"))

# ANOVA (prints to console)
perform_anova(iris, "Sepal.Length", "Species")

# Imputation (prints to console)
impute_missing(airquality, method = "auto")
```

---

## Customization

### Change Output Location

Edit the scripts to change `output_dir`:

```r
# In generate_all_analyses.R (line ~16)
output_dir <- "my_custom_folder"

# In quick_demo.R (line ~13)
dir.create("my_custom_folder", showWarnings = FALSE)
```

---

### Run Only Specific Analyses

```r
library(descriptR)

# Option 1: Use analyze_and_report() for specific analysis
analyze_and_report(
  mtcars,
  "regression_only",
  analysis_type = "regression",
  format = "html"
)

# Option 2: Use individual functions
reg <- perform_regression_analysis(mtcars, "mpg", c("wt", "hp"))
# ... do something with reg results
```

---

### Add Your Own Dataset

```r
library(descriptR)

# Load your data
my_data <- read.csv("path/to/your/data.csv")

# Run complete analysis
analyze_all(
  my_data,
  output_file = "my_analysis",
  group = "your_group_column",  # Optional
  format = "all"
)
```

---

## Performance Notes

### Runtime by Dataset Size

| Rows | Columns | Runtime (analyze_all) |
|------|---------|----------------------|
| 150  | 5       | ~10 seconds          |
| 500  | 10      | ~30 seconds          |
| 5000 | 20      | ~2 minutes           |
| 10000| 50      | ~5 minutes           |

### Speed Tips

1. **Use HTML format only** for faster generation
   ```r
   format = "html"  # Instead of format = "all"
   ```

2. **Select specific variables** instead of analyzing all
   ```r
   vars = c("var1", "var2", "var3")
   ```

3. **Disable plots** if you only need tables
   ```r
   include_plots = FALSE
   ```

---

## What to Check in Generated Reports

### HTML Report Checklist

Open the HTML file and verify:

- [ ] Executive summary at top
- [ ] Dataset overview (n rows, n columns)
- [ ] Descriptive statistics table
- [ ] Missing data visualization (if any missing)
- [ ] Normality QQ plots for each variable
- [ ] Outlier detection boxplots
- [ ] Correlation heatmap
- [ ] **Regression analysis section** ← NEW
  - [ ] Coefficient table
  - [ ] Model fit statistics
  - [ ] VIF table
  - [ ] Diagnostic plots
- [ ] Grouped comparison plots (if group specified)
- [ ] **ANOVA table** ← NEW (if group specified)
  - [ ] F-statistics and p-values
  - [ ] Post-hoc pairwise comparisons
  - [ ] Effect sizes
- [ ] **Imputation summary** ← NEW (if missing data)
  - [ ] Method used
  - [ ] Number of values imputed
  - [ ] Quality diagnostics
- [ ] Automated insights section
- [ ] All visualizations in appendix

---

## Next Steps

After running the demo scripts:

1. **Review the HTML reports** to see all analyses
2. **Check the Word documents** to see publication-ready format
3. **Open Excel files** to access raw tables
4. **Try with your own data** using analyze_all()
5. **Read TESTING_GUIDE.md** for more examples
6. **Read PHASE1_IMPLEMENTATION.md** for technical details

---

## Summary

**Two scripts available:**

1. **quick_demo.R** - Fast test (1 minute)
2. **generate_all_analyses.R** - Complete demo (5 minutes)

**Both demonstrate:**
- Regression analysis
- ANOVA/MANOVA
- Missing data imputation
- Complete analyze_all() integration

**All changes are live on GitHub and ready to use!**

---

**Questions?** Check:
- `TESTING_GUIDE.md` - Detailed testing instructions
- `PHASE1_IMPLEMENTATION.md` - Implementation details
- `ANALYZE_ALL_UPDATE.md` - analyze_all() documentation
- `README.md` - Complete package documentation
