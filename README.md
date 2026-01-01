# descriptR

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/descriptR)](https://CRAN.R-project.org/package=descriptR)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](https://github.com/drabdulali/descriptR)
<!-- badges: end -->

> **Comprehensive Statistical Analysis & Reporting Made Simple**

The complete R package for descriptive and inferential statistics with **automated insights**, **publication-ready visualizations**, and **one-function comprehensive reporting**.

---

## Key Features

### **One Function Does Everything: `analyze_all()`**
```r
# Run ALL analyses + Generate complete report with 50+ figures
analyze_all(mtcars, "complete_analysis", format = "all")
```

**What you get:**
- **9+ Statistical Analyses**: Descriptive, Missing Data, Normality, Outliers, Correlations, Grouped, **Regression**, **ANOVA/MANOVA**, **Imputation**
- **50+ Visualizations**: Histograms, Density, Boxplots, Violin, QQ, Scatter, Heatmaps, Diagnostic Plots
- **4 Output Formats**: HTML, Word (with embedded 300 DPI figures), Excel, Markdown
- **Automated Insights**: Human-readable interpretations of results
- **Publication Quality**: Professional formatting, colorblind-safe palettes
- **NEW**: Advanced regression with diagnostics, ANOVA with post-hoc tests, sophisticated imputation

### **Interactive Variable Selection**
```r
# Let the package guide you through variable selection
analyze_all(iris, "iris_analysis", interactive = TRUE)

# Available variables:
#  1. Sepal.Length (numeric)
#  2. Sepal.Width (numeric)
#  3. Petal.Length (numeric)
#  4. Petal.Width (numeric)
#  5. Species (factor)
#  0. All variables
#
# Select variables (comma-separated numbers, or 0 for all): 1,2,3
```

### **Word Documents with Embedded Figures**
Every Word report includes:
- Complete statistical tables
- **ALL visualizations embedded** as high-resolution (300 DPI) PNG images
- Professional formatting with journal templates

---

## Installation

```r
# From GitHub (recommended - latest features)
devtools::install_github("drabdulali/descriptR")

# From CRAN (stable release - coming soon)
install.packages("descriptR")
```

---

## Quick Start

### Option 1: Complete Analysis (Recommended)

```r
library(descriptR)

# Analyze EVERYTHING in one function call
analyze_all(
  mtcars,
  output_file = "mtcars_complete",
  format = "all",  # HTML, Word, Excel, Markdown
  title = "Motor Trend Car Analysis"
)
```

**Output:** 4 files with complete analysis + 50+ embedded visualizations!

### Option 2: Interactive Mode

```r
# Let descriptR guide you
analyze_all(
  iris,
  output_file = "iris_interactive",
  interactive = TRUE,
  format = "word"
)

# Follow the prompts to select variables and grouping
```

### Option 3: Custom Analysis

```r
# Specific analysis types
analyze_and_report(
  airquality,
  output_file = "air_descriptive",
  analysis_type = "descriptive",  # or "correlation", "normality", etc.
  format = "html"
)
```

---

## Complete Function Reference

### **Master Functions** (Start Here!)

#### `analyze_all()` - ONE Function for Everything
**Purpose:** Performs ALL statistical analyses and generates comprehensive report with all visualizations

```r
# Basic usage
analyze_all(mtcars, "output_name")

# With all options
analyze_all(
  data = iris,
  output_file = "iris_complete",
  vars = NULL,              # NULL = all variables, or specify c("var1", "var2")
  group = "Species",        # Grouping variable for comparisons
  format = "all",           # "html", "word", "excel", "markdown", "all"
  template = "default",     # "default", "apa", "nature", "science", "nejm"
  include_plots = TRUE,     # Include visualizations in report
  interactive = FALSE,      # TRUE = interactive variable selection
  title = "My Analysis",
  author = "Your Name"
)
```

**Example Applications:**

```r
# 1. Complete car analysis
analyze_all(mtcars, "cars", format = "all", title = "Car Performance Study")

# 2. Flower species comparison
analyze_all(iris, "flowers", group = "Species", format = "word")

# 3. Air quality with missing data
analyze_all(airquality, "air", format = "html")

# 4. Interactive selection
analyze_all(mtcars, "custom", interactive = TRUE)
```

**What `analyze_all()` includes:**
1. **Descriptive Statistics** - Mean, SD, Min, Max, Quartiles for all numeric variables
2. **Missing Data Analysis** - Patterns, percentages, recommendations
3. **Normality Tests** - Shapiro-Wilk, Anderson-Darling, visual QQ plots
4. **Outlier Detection** - Z-score method with visualization
5. **Correlation Analysis** - Pairwise correlations with p-values and heatmap
6. **Grouped Comparisons** - If grouping variable specified
7. **50+ Visualizations** - See visualization section below

---

#### `analyze_and_report()` - Specific Analysis + Report
**Purpose:** Run one specific type of analysis and generate report

```r
analyze_and_report(
  data,
  output_file,
  analysis_type = "descriptive",  # Choose one analysis type
  format = "html",
  template = "default",
  vars = NULL,
  group = NULL,
  include_plots = TRUE,
  title = NULL,
  author = NULL
)
```

**Analysis Types:**
- `"descriptive"` - Basic descriptive statistics
- `"grouped"` - Group comparisons with effect sizes
- `"correlation"` - Correlation analysis
- `"normality"` - Normality assessment
- `"missing"` - Missing data analysis
- `"outliers"` - Outlier detection
- `"pca"` - Principal Component Analysis
- `"comprehensive"` - All of the above (same as `analyze_all()`)

**Examples:**

```r
# Descriptive statistics only
analyze_and_report(mtcars, "desc", analysis_type = "descriptive")

# Grouped comparison
analyze_and_report(iris, "groups", analysis_type = "grouped", group = "Species")

# Correlation analysis
analyze_and_report(mtcars, "corr", analysis_type = "correlation")
```

---

### **Interactive Functions**

#### `select_variables()` - Interactive Variable Selection
**Purpose:** Choose which variables to analyze via interactive menu

```r
# Select multiple variables
vars <- select_variables(
  mtcars,
  type = "all",      # "all", "numeric", "categorical", "both"
  multiple = TRUE,   # TRUE = multiple selection, FALSE = single
  prompt = NULL      # Custom prompt message
)

# Use selected variables
analyze_all(mtcars, "selected", vars = vars)
```

**Example:**
```r
# Select numeric variables from iris
num_vars <- select_variables(iris, type = "numeric")

# Select single grouping variable
group_var <- select_grouping_variable(iris)

# Run analysis with selections
analyze_all(iris, "custom", vars = num_vars, group = group_var)
```

#### `select_grouping_variable()` - Interactive Grouping Selection
**Purpose:** Choose grouping variable for group comparisons

```r
group <- select_grouping_variable(iris)
# Shows suitable variables with number of groups
# Returns NULL if no grouping desired
```

---

### **Statistical Analysis Functions**

#### `perform_descriptive_analysis()` - Descriptive Statistics
**Purpose:** Calculate comprehensive descriptive statistics

```r
result <- perform_descriptive_analysis(
  data,
  vars = NULL  # NULL = all numeric variables
)

# Access results
print(result$statistics)  # Table with mean, sd, min, max, quartiles
cat(result$insights)      # Automated interpretation
```

**Example on mtcars:**
```r
desc <- perform_descriptive_analysis(mtcars)
print(desc$statistics)

#   variable  n missing  mean     sd    min    q25 median    q75    max
# 1      mpg 32       0 20.09  6.027  10.40  15.43  19.20  22.80  33.90
# 2      cyl 32       0  6.19  1.786   4.00   4.00   6.00   8.00   8.00
# 3     disp 32       0230.72123.939  71.10 120.83 196.30 326.00 472.00
# ...

cat(desc$insights)
# "mpg shows high variability (CV = 0.30)"
# "disp shows high variability (CV = 0.54)"
```

#### `perform_grouped_analysis()` - Group Comparisons
**Purpose:** Compare groups with descriptive statistics and effect sizes

```r
result <- perform_grouped_analysis(
  data,
  group,       # Grouping variable name
  vars = NULL  # Variables to compare
)
```

**Example on iris:**
```r
grouped <- perform_grouped_analysis(iris, group = "Species")
print(grouped$by_group)

#       variable     group  n  mean    sd
# 1 Sepal.Length    setosa 50  5.01 0.352
# 2 Sepal.Length versicolor 50  5.94 0.516
# 3 Sepal.Length  virginica 50  6.59 0.636
# ...

cat(grouped$insights)
# "Sepal.Length shows notable differences between groups (difference = 1.58)"
```

#### `perform_correlation_analysis()` - Correlations
**Purpose:** Compute correlation matrix with significance tests

```r
result <- perform_correlation_analysis(
  data,
  vars = NULL,
  method = "pearson"  # "pearson", "spearman", "kendall"
)
```

**Example:**
```r
corr <- perform_correlation_analysis(mtcars)
print(corr$correlations)  # Pairwise correlations with p-values

#     var1 var2 correlation   p_value
# 1    mpg  cyl      -0.852 6.11e-10
# 2    mpg disp      -0.848 9.38e-10
# 3    mpg   hp      -0.776 1.79e-07
# ...
```

#### `perform_normality_analysis()` - Normality Testing
**Purpose:** Assess normality with multiple tests

```r
result <- perform_normality_analysis(data, vars = NULL)
```

#### `perform_missing_analysis()` - Missing Data
**Purpose:** Analyze missing data patterns

```r
result <- perform_missing_analysis(data, vars = NULL)
```

#### `perform_outlier_analysis()` - Outlier Detection
**Purpose:** Detect outliers using Z-score method

```r
result <- perform_outlier_analysis(
  data,
  vars = NULL,
  method = "zscore"  # Z-score threshold method
)
```

#### `perform_pca_analysis()` - Principal Component Analysis
**Purpose:** Reduce dimensionality and identify patterns

```r
result <- perform_pca_analysis(
  data,
  vars = NULL,
  n_components = NULL  # NULL = auto-select
)
```

---

### **Advanced Statistical Methods** (NEW in v0.2.0!)

#### `perform_regression_analysis()` - Comprehensive Regression
**Purpose:** Linear, logistic, and Poisson regression with diagnostics

```r
# Linear regression
result <- perform_regression_analysis(
  mtcars,
  outcome = "mpg",
  predictors = c("wt", "hp", "cyl"),
  type = "auto"  # Auto-detects type
)

# Logistic regression
result <- perform_regression_analysis(
  mtcars,
  outcome = "am",
  predictors = c("mpg", "wt"),
  type = "logistic"
)

# With interactions and polynomial terms
result <- perform_regression_analysis(
  mtcars,
  outcome = "mpg",
  predictors = c("wt", "hp"),
  interactions = TRUE,
  polynomial = TRUE,
  step_wise = TRUE  # Automatic variable selection
)
```

**What you get:**
- Coefficient estimates with confidence intervals
- Model fit statistics (R², AIC, BIC)
- Assumption tests (normality, homoscedasticity, multicollinearity)
- Diagnostic plots (residuals, QQ, Cook's distance)
- VIF for multicollinearity
- Automated interpretations

**Example output:**
```r
print(result$coefficients)
#   term        estimate std_error statistic p_value significance
# 1 (Intercept)  37.23      1.60    23.28   <0.001     ***
# 2 wt           -3.88      0.63    -6.13   <0.001     ***
# 3 hp           -0.03      0.01    -2.83    0.008      **

# Model explains 83.2% of variance (Adjusted R² = 0.832)
# Strongest predictor: wt (negative effect, β = -3.88)
```

#### `perform_anova()` - ANOVA/MANOVA Analysis
**Purpose:** One-way, two-way, repeated measures, and multivariate ANOVA

```r
# One-way ANOVA with post-hoc
result <- perform_anova(
  iris,
  outcome = "Sepal.Length",
  groups = "Species",
  type = "one-way",
  post_hoc = "tukey"  # Tukey HSD post-hoc test
)

# Two-way ANOVA
result <- perform_anova(
  mtcars,
  outcome = "mpg",
  groups = c("cyl", "am"),
  type = "two-way"
)

# MANOVA (multiple outcomes)
result <- perform_anova(
  iris,
  outcome = c("Sepal.Length", "Sepal.Width", "Petal.Length"),
  groups = "Species",
  type = "manova"
)
```

**What you get:**
- ANOVA tables with F-statistics and p-values
- Effect sizes (eta-squared, omega-squared)
- Post-hoc pairwise comparisons (Tukey HSD, Bonferroni)
- Assumption tests (normality by group, homogeneity of variance)
- Group descriptive statistics
- Automated interpretations

**Example output:**
```r
# Significant effects: Species
# Large effect sizes for: Species (η² = 0.62)
# Significant pairwise differences: setosa-versicolor, setosa-virginica, versicolor-virginica
```

#### `impute_missing()` - Missing Data Imputation
**Purpose:** Sophisticated missing data imputation with multiple methods

```r
# Automatic method selection
result <- impute_missing(airquality)

# Mean imputation
result <- impute_missing(
  airquality,
  method = "mean",
  vars = c("Ozone", "Solar.R")
)

# Regression imputation
result <- impute_missing(
  airquality,
  method = "regression",
  diagnostics = TRUE
)

# Multiple imputation (MICE)
result <- impute_missing(
  airquality,
  method = "mice",
  m = 10,  # Number of imputations
  seed = 123
)

# KNN imputation
result <- impute_missing(
  airquality,
  method = "knn"
)

# Access imputed data
imputed_data <- result$imputed_data
```

**Imputation methods:**
- **mean/median/mode** - Simple imputation
- **regression** - Predict missing values using other variables
- **MICE** - Multiple Imputation by Chained Equations
- **KNN** - K-Nearest Neighbors imputation
- **auto** - Automatically selects best method based on missing data percentage

**What you get:**
- Imputed dataset
- Imputation summary (original vs imputed means/SDs)
- Quality diagnostics
- Distribution comparison
- Warnings for large distributional changes
- Recommendations

---

### **Coming Soon** (Phases 2 & 3)

#### Phase 2: Enhanced Reporting

**Interactive Dashboards**
```r
# Create interactive Shiny dashboard
create_dashboard(mtcars, output = "dashboard.html")
```

**PDF/LaTeX Output**
```r
# Direct PDF generation
generate_report(result, format = "pdf")
generate_report(result, format = "latex")
```

**Comparison Reports**
```r
# Before/After comparison
compare_datasets(before_data, after_data, output_file = "comparison")

# A/B testing
compare_groups(data, group_var = "treatment", output_file = "ab_test")
```

#### Phase 3: Advanced Methods

**Time Series Analysis**
```r
# Trend, seasonality, forecasting
analyze_timeseries(data, time_var = "date", value_var = "sales")
```

**Survival Analysis**
```r
# Kaplan-Meier curves, Cox regression
perform_survival_analysis(
  data,
  time = "time_to_event",
  event = "event_occurred",
  groups = "treatment"
)
```

**Mixed/Multilevel Models**
```r
# Hierarchical/nested data
perform_mixed_model(
  data,
  outcome = "test_score",
  fixed = c("age", "gender"),
  random = "school_id"
)
```

---

### **Visualization Functions**

#### `plot_variable()` - Smart Automatic Plotting
**Purpose:** Automatically creates the right plot type based on variable types

```r
plot <- plot_variable(
  data,
  x,                 # X variable name
  y = NULL,          # Y variable (optional)
  group = NULL,      # Grouping variable
  plot_type = "auto", # "auto", "histogram", "density", "boxplot", "violin", "scatter", "bar", "qq"
  color_scheme = "medium",  # "subtle", "medium", "bold", "vivid"
  boldness = 2,      # 1-4
  theme = "publication",    # "publication", "presentation", "minimal"
  title = NULL,
  subtitle = NULL,
  caption = NULL
)
```

**Examples:**

```r
# 1. Histogram (single numeric variable)
p1 <- plot_variable(mtcars, x = "mpg")

# 2. Scatter plot with groups
p2 <- plot_variable(iris, x = "Sepal.Length", y = "Petal.Length", group = "Species")

# 3. Boxplot by group
p3 <- plot_variable(iris, x = "Species", y = "Sepal.Length", plot_type = "boxplot")

# 4. Violin plot
p4 <- plot_variable(iris, x = "Species", y = "Petal.Width", plot_type = "violin")

# 5. QQ plot for normality
p5 <- plot_variable(mtcars, x = "mpg", plot_type = "qq")

# 6. Density plot
p6 <- plot_variable(mtcars, x = "mpg", plot_type = "density")

# 7. Bar plot for categorical
p7 <- plot_variable(iris, x = "Species", plot_type = "bar")
```

#### `plot_correlation_matrix()` - Correlation Heatmap
**Purpose:** Visualize correlation matrix

```r
p <- plot_correlation_matrix(
  data,
  method = "pearson",
  title = "Correlation Matrix",
  theme = "publication"
)
```

#### `save_plot()` - Export Plots
**Purpose:** Save plots in multiple formats and resolutions

```r
save_plot(
  plot,
  filename,
  width = 7,
  height = 5,
  dpi = 300,    # 300 = publication, 600 = high-res, 150 = screen
  device = NULL # NULL = auto-detect from filename
)
```

**Examples:**
```r
# Publication quality
save_plot(p, "figure1.png", dpi = 300)

# High resolution
save_plot(p, "figure1.png", dpi = 600)

# Vector format
save_plot(p, "figure1.pdf")
save_plot(p, "figure1.svg")

# Multiple formats
save_plot(p, "figure1.png", dpi = 300)
save_plot(p, "figure1.pdf")
```

#### `export_plots()` - Batch Export
**Purpose:** Export multiple plots at once

```r
plots <- list(
  histogram = p1,
  scatter = p2,
  boxplot = p3
)

export_plots(
  plots,
  output_dir = "figures",
  prefix = "fig",
  width = 7,
  height = 5,
  dpi = 300,
  formats = c("png", "pdf")
)
```

---

### **Color Schemes**

descriptR provides **16 colorblind-safe palettes** (4 schemes × 4 boldness levels) tested for deuteranopia, protanopia, and tritanopia. All palettes are publication-ready and accessible.

#### `get_color_scheme()` - Get Color Palette
**Purpose:** Retrieve publication-ready, colorblind-safe color palettes

```r
colors <- get_color_scheme(
  scheme = "medium",        # "subtle", "medium", "bold", "vivid"
  boldness = 2,             # 1 (lightest) to 4 (boldest)
  n = 8,                    # Number of colors needed
  colorblind_safe = TRUE,   # Ensure colorblind safety (default)
  reverse = FALSE           # Reverse palette order
)
```

#### All 16 Color Scheme Combinations

**1. SUBTLE (Pastels) - For Publications**

| Boldness | Description | Use Case | Example Colors |
|----------|-------------|----------|----------------|
| 1 | Very light pastels | Background elements, watermarks | `#E8F4F8`, `#F0E8F8`, `#F8E8F0` |
| 2 | Light pastels | Subtle differences, heatmaps | `#C5E3ED`, `#DCC5ED`, `#EDC5DC` |
| 3 | Medium pastels | General publication figures | `#9FD2E2`, `#C89FE2`, `#E29FC8` |
| 4 | Deep pastels | Emphasized publication figures | `#7AC1D7`, `#B47AD7`, `#D77AB4` |

**Example:**
```r
# Subtle scheme for Nature journal
colors_subtle <- get_color_scheme("subtle", boldness = 3, n = 5)

library(ggplot2)
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  scale_fill_manual(values = colors_subtle) +
  theme_minimal()
```

**2. MEDIUM (Balanced) - General Purpose (DEFAULT)**

| Boldness | Description | Use Case | Example Colors |
|----------|-------------|----------|----------------|
| 1 | Light balanced | Soft visualizations | `#A8D5E2`, `#D2A8E2`, `#E2A8D2` |
| 2 | Standard balanced | Most publications (DEFAULT) | `#6FB8D4`, `#B86FD4`, `#D46FB8` |
| 3 | Saturated balanced | Emphasis without harshness | `#4A9FBF`, `#9F4ABF`, `#BF4A9F` |
| 4 | Deep balanced | Strong but professional | `#2E86A9`, `#862EA9`, `#A92E86` |

**Example:**
```r
# Medium scheme (default) - most common use
colors_medium <- get_color_scheme("medium", boldness = 2, n = 3)

ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_medium) +
  theme_classic()
```

**3. BOLD (High Contrast) - For Presentations**

| Boldness | Description | Use Case | Example Colors |
|----------|-------------|----------|----------------|
| 1 | Bright colors | Digital presentations | `#5DADE2`, `#AD5DE2`, `#E25DAD` |
| 2 | Strong colors | Conference posters | `#3498DB`, `#9834DB`, `#DB3498` |
| 3 | Very strong colors | Large screens, emphasis | `#2E86C1`, `#862EC1`, `#C12E86` |
| 4 | Maximum saturation | Maximum visual impact | `#21618C`, `#61218C`, `#8C2161` |

**Example:**
```r
# Bold scheme for presentation
colors_bold <- get_color_scheme("bold", boldness = 3, n = 4)

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = colors_bold) +
  theme_dark() +
  theme(text = element_text(size = 16))
```

**4. VIVID (Vibrant) - For Digital/Infographics**

| Boldness | Description | Use Case | Example Colors |
|----------|-------------|----------|----------------|
| 1 | Vibrant light | Web dashboards | `#00D9FF`, `#D900FF`, `#FF00D9` |
| 2 | Vibrant medium | Digital infographics | `#00B8D4`, `#B800D4`, `#D400B8` |
| 3 | Vibrant deep | Interactive visualizations | `#00A3C4`, `#A300C4`, `#C400A3` |
| 4 | Neon-like intensity | Digital art, eye-catching graphics | `#008CAA`, `#8C00AA`, `#AA008C` |

**Example:**
```r
# Vivid scheme for web dashboard
colors_vivid <- get_color_scheme("vivid", boldness = 2, n = 5)

ggplot(mtcars, aes(x = factor(cyl), fill = factor(gear))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = colors_vivid) +
  theme_minimal() +
  labs(title = "Engine Cylinders vs Gears")
```

#### Complete Visualization Examples with Color Schemes

**Example 1: Scatter Plot with Medium Scheme**
```r
library(descriptR)
library(ggplot2)

# Get medium colors
colors <- get_color_scheme("medium", boldness = 2, n = 3)

# Create scatter plot
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = colors) +
  theme_minimal() +
  labs(title = "Iris Measurements",
       subtitle = "Using medium color scheme (boldness = 2)")
```

**Example 2: Grouped Boxplot with Bold Scheme**
```r
# Bold scheme for presentation
colors_bold <- get_color_scheme("bold", boldness = 3, n = 3)

ggplot(iris, aes(x = Species, y = Sepal.Width, fill = Species)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = colors_bold) +
  theme_classic(base_size = 14) +
  labs(title = "Sepal Width by Species",
       subtitle = "Bold color scheme for presentations")
```

**Example 3: Correlation Heatmap with Subtle Scheme**
```r
# Correlation matrix
cor_matrix <- cor(mtcars[, 1:7])

# Subtle colors for heatmap
colors_subtle <- get_color_scheme("subtle", boldness = 3, n = 100)

library(reshape2)
melted_cor <- melt(cor_matrix)

ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = colors_subtle) +
  theme_minimal() +
  labs(title = "Correlation Heatmap",
       subtitle = "Subtle color scheme")
```

**Example 4: Bar Chart with Vivid Scheme**
```r
# Count data
gear_counts <- as.data.frame(table(mtcars$gear))
colnames(gear_counts) <- c("Gears", "Count")

# Vivid colors
colors_vivid <- get_color_scheme("vivid", boldness = 2, n = 3)

ggplot(gear_counts, aes(x = Gears, y = Count, fill = Gears)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_vivid) +
  theme_minimal() +
  labs(title = "Number of Gears Distribution")
```

#### Advanced Color Functions

**Preview Color Scheme**
```r
# Preview colors before using
preview_color_scheme("medium", boldness = 2, n = 8)
# Displays color codes and visual preview
```

**Get Scheme Recommendations**
```r
# Get recommended scheme for variable type and context
rec <- get_scheme_for_type(
  var_type = "categorical",   # "continuous", "categorical", "diverging", "sequential"
  context = "publication"     # "publication", "presentation", "web"
)

# Use recommended colors
colors <- get_color_scheme(rec$scheme, rec$boldness)
```

**Create Diverging Palette**
```r
# For data with meaningful midpoint (e.g., correlation, change)
diverging_colors <- create_diverging_palette(
  low_color = "#3498DB",    # Blue for negative
  mid_color = "#FFFFFF",    # White for zero
  high_color = "#E74C3C",   # Red for positive
  n = 11                    # Odd number recommended
)

# Use in correlation heatmap
cor_data <- cor(mtcars[, 1:5])
library(reshape2)
melted <- melt(cor_data)

ggplot(melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = diverging_colors, limits = c(-1, 1)) +
  theme_minimal()
```

**Create Sequential Palette**
```r
# For ordered data (e.g., temperature, density)
sequential_colors <- create_sequential_palette(
  base_color = "#3498DB",
  n = 9,
  direction = "light_to_dark"
)

# Use for density visualization
ggplot(faithful, aes(x = eruptions, y = waiting)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_gradientn(colors = sequential_colors) +
  theme_minimal()
```

**Check Color Accessibility**
```r
# Ensure colors meet WCAG guidelines
check <- check_color_accessibility(
  foreground = "#3498DB",
  background = "#FFFFFF",
  level = "AA"  # or "AAA"
)

print(check)
# Shows contrast ratio and pass/fail for normal and large text
```

**Export Color Palette**
```r
# Export for use in other software
colors <- get_color_scheme("medium", boldness = 2, n = 5)

# As hex codes
export_color_palette(colors, format = "hex")

# As RGB
export_color_palette(colors, format = "rgb")
# Output: "rgb(111, 184, 212)" ...

# As CSS variables
export_color_palette(colors, format = "css", name = "my_palette")
# Output: :root { --my_palette-1: #6FB8D4; ... }

# As R code
export_color_palette(colors, format = "r", name = "my_colors")
# Output: my_colors <- c("#6FB8D4", ...)
```

#### Using Color Schemes in descriptR Functions

All descriptR plotting functions automatically use appropriate color schemes:

```r
# plot_variable() uses context-appropriate colors
plot_variable(
  iris,
  x = "Species",
  y = "Sepal.Length",
  color_scheme = "bold",    # Scheme name
  boldness = 3              # Boldness level
)

# analyze_all() generates all plots with consistent colors
analyze_all(
  iris,
  "output",
  format = "all",
  color_scheme = "medium",  # All 50+ plots use this scheme
  boldness = 2
)
```

#### Color Scheme Quick Reference

| Context | Recommended Scheme | Boldness | Reason |
|---------|-------------------|----------|--------|
| Journal publication | Subtle or Medium | 2-3 | Professional, not distracting |
| Presentation | Bold | 3 | High visibility, impact |
| Poster | Bold or Vivid | 3-4 | Attracts attention |
| Web/Digital | Vivid | 2 | Optimized for screens |
| Print (grayscale) | Medium | 2 | Converts well to grayscale |
| Accessibility focus | Medium | 2 | Best contrast ratios |
| Heatmap | Subtle | 3 | Gradients without harshness |
| Categorical (3-5 groups) | Medium | 2 | Clear differentiation |
| Categorical (6+ groups) | Bold | 2-3 | More distinction needed |

#### Colorblind Safety

All 16 palettes tested for:
- **Deuteranopia** (red-green, most common ~5% males)
- **Protanopia** (red-green)
- **Tritanopia** (blue-yellow, rare)

Colors remain distinguishable for all three types of colorblindness.

---

### **Report Generation Functions**

#### `generate_report()` - Create Formatted Reports
**Purpose:** Generate professional reports from analysis results

```r
generate_report(
  result,              # Analysis result object
  output_file,         # Filename (without extension)
  format = "html",     # "html", "word", "excel", "markdown", "all"
  template = "default", # "default", "apa", "nature", "science", "nejm"
  include_plots = TRUE,
  include_interpretations = TRUE,
  author = NULL,
  title = NULL,
  open_file = TRUE
)
```

**Journal Templates:**
1. **Default** - Clean, general-purpose
2. **APA** - APA 7th edition style
3. **Nature** - Nature journal format
4. **Science** - Science magazine format
5. **NEJM** - New England Journal of Medicine

**Example:**
```r
# Run analysis
result <- perform_descriptive_analysis(mtcars)

# Generate reports
generate_report(result, "report_html", format = "html")
generate_report(result, "report_word", format = "word", template = "apa")
generate_report(result, "report_all", format = "all")  # All 4 formats
```

#### `quick_export_excel()` - Quick Excel Export
**Purpose:** Quickly export data frame to Excel

```r
quick_export_excel(
  data,
  filename,
  sheet_name = "Data"
)
```

#### `batch_export_tables()` - Export Multiple Tables
**Purpose:** Export multiple tables as separate CSV files

```r
tables <- list(
  descriptive = desc_table,
  correlation = corr_table,
  grouped = group_table
)

batch_export_tables(tables, output_dir = "tables")
```

---

## Complete Examples by Dataset

### Example 1: Motor Trend Cars (mtcars)

```r
library(descriptR)

# COMPLETE ANALYSIS - Everything in one call
analyze_all(
  mtcars,
  output_file = "mtcars_complete",
  format = "all",
  title = "Motor Trend Car Road Tests Analysis",
  author = "Your Name"
)

# What you get:
# mtcars_complete.html   - Interactive HTML report
# mtcars_complete.docx   - Word with 50+ embedded figures (300 DPI)
# mtcars_complete.xlsx   - Excel with multiple sheets
# mtcars_complete.md     - Markdown for GitHub

# The Word document contains:
# 1. Descriptive Statistics for all 11 variables
# 2. Missing Data Analysis
# 3. Normality Tests (Shapiro-Wilk for all variables)
# 4. Outlier Detection (Z-score method)
# 5. Correlation Matrix (55 pairwise correlations)
# 6. Key Insights (automated interpretations)
# 7. VISUALIZATIONS SECTION with 50+ figures:
#    - 11 Histograms (mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)
#    - 11 Density Plots
#    - 11 Boxplots
#    - 11 QQ Plots
#    - 5 Scatter Plots
#    - 1 Correlation Heatmap
#    - 5 Outlier Plots
```

### Example 2: Iris Flowers with Grouping

```r
# Complete analysis with species comparison
analyze_all(
  iris,
  output_file = "iris_by_species",
  group = "Species",  # Compare species
  format = "all",
  title = "Iris Species Comparison"
)

# Additional features with grouping:
# Grouped Boxplots (4 variables × 3 species)
# Violin Plots (distribution shapes)
# Bar Plot (Species frequencies)
# Group comparison statistics
# Total: 60+ figures in Word document!
```

### Example 3: Air Quality with Missing Data

```r
# Analysis with missing data
analyze_all(
  airquality,
  output_file = "air_quality_analysis",
  format = "word",
  title = "New York Air Quality Measurements"
)

# Special features for missing data:
# Missing data pattern visualization
# Percentage missing by variable
# Recommendations for handling
# Complete case analysis
```

### Example 4: Interactive Custom Analysis

```r
# Let descriptR guide you through variable selection
analyze_all(
  mtcars,
  output_file = "custom_analysis",
  interactive = TRUE,
  format = "html"
)

# Interactive prompts will ask:
# 1. Which variables to analyze (select from menu)
# 2. Whether to perform grouped analysis
# 3. Which variable to use for grouping
#
# Then generates customized report with your selections
```

### Example 5: Specific Analysis Types

```r
# 1. Descriptive statistics only
analyze_and_report(
  mtcars,
  "descriptive_only",
  analysis_type = "descriptive",
  format = "word"
)

# 2. Correlation analysis
analyze_and_report(
  mtcars,
  "correlations",
  analysis_type = "correlation",
  format = "html"
)

# 3. Normality assessment
analyze_and_report(
  mtcars,
  "normality_check",
  analysis_type = "normality",
  format = "html"
)

# 4. PCA analysis
analyze_and_report(
  mtcars,
  "pca_analysis",
  analysis_type = "pca",
  format = "all"
)
```

---

## Visualizations Generated by `analyze_all()`

For a typical dataset with 10 numeric variables, `analyze_all()` generates **50+ publication-quality visualizations**:

### Distribution Plots
1. **Histograms** (10) - Distribution with density overlay for each variable
2. **Density Plots** (10) - Smooth kernel density estimates
3. **Boxplots** (10) - Median, quartiles, and outliers

### Group Comparisons (if group specified)
4. **Grouped Boxplots** (10) - Compare distributions across groups
5. **Violin Plots** (5) - Distribution shapes by group

### Diagnostic Plots
6. **QQ Plots** (10) - Normality assessment for ALL variables
7. **Outlier Plots** (5) - Z-score outlier detection visualizations

### Relationship Plots
8. **Scatter Plots** (5) - Pairwise relationships with trend lines
9. **Correlation Heatmap** (1) - All pairwise correlations visualized

### Data Quality
10. **Missing Data Pattern** (1) - If missing data present
11. **Bar Plots** - Categorical variable frequencies

**All figures are:**
- 300 DPI resolution (publication-quality)
- Colorblind-safe palettes
- Professional formatting
- Embedded in Word documents
- Saved as separate PNG files

---

## Complete Workflow Examples

### Workflow 1: Publication-Ready Analysis

```r
library(descriptR)

# 1. Load your data
data <- read.csv("your_data.csv")

# 2. Run complete analysis
analyze_all(
  data,
  output_file = "manuscript_analysis",
  format = "all",
  template = "apa",  # APA 7th edition style
  title = "Your Study Title",
  author = "Your Name"
)

# 3. Done! You now have:
#    - manuscript_analysis.docx (with all tables and 50+ figures)
#    - manuscript_analysis.html (interactive)
#    - manuscript_analysis.xlsx (data tables)
#    - manuscript_analysis.md (GitHub README)
```

### Workflow 2: Exploratory Data Analysis

```r
# Interactive exploration
analyze_all(
  your_data,
  "exploration",
  interactive = TRUE,  # Choose variables interactively
  format = "html"      # View in browser
)

# Review the HTML report
# Identify interesting patterns
# Run specific follow-up analyses
```

### Workflow 3: Group Comparison Study

```r
# Compare treatment groups
analyze_all(
  clinical_data,
  output_file = "treatment_comparison",
  group = "treatment_group",
  format = "word",
  template = "nejm",  # New England Journal of Medicine style
  title = "Treatment Efficacy Comparison"
)

# Word document includes:
# - Grouped statistics
# - Effect sizes
# - Grouped boxplots and violin plots
# - Statistical tests
```

---

## Tips & Best Practices

### When to Use Each Function

1. **Use `analyze_all()`** when:
   - Starting a new analysis
   - You want comprehensive results
   - Generating reports for publication
   - Need all visualizations

2. **Use `analyze_and_report()`** when:
   - You need one specific analysis type
   - Creating supplementary materials
   - Following up on specific findings

3. **Use interactive mode** when:
   - Exploring unfamiliar data
   - Selecting specific variables
   - Teaching or demonstrating analysis

### Output Format Guide

- **HTML**: Interactive viewing, web sharing
- **Word**: Manuscript preparation, editing needed
- **Excel**: Data tables, further analysis
- **Markdown**: GitHub README, documentation
- **All**: Generate everything at once

### Template Guide

- **default**: General purpose, clean
- **apa**: Psychology, social sciences
- **nature**: Biology, natural sciences
- **science**: Multidisciplinary research
- **nejm**: Medicine, clinical trials

---

## Documentation

### Vignettes
1. [Getting Started](vignettes/getting-started.Rmd) - Quick start guide
2. [Advanced Analysis](vignettes/advanced-analysis.Rmd) - Effect sizes, PCA, EFA, GLM
3. [Visualization & Reporting](vignettes/visualization-reporting.Rmd) - Colors, plots, export

### Help Resources
```r
# Browse all vignettes
browseVignettes("descriptR")

# Function help
?analyze_all
?analyze_and_report
?generate_report
?plot_variable

# Package overview
help(package = "descriptR")
```

---

## Advanced Usage

### Custom Workflows

```r
# 1. Run analysis
result <- perform_descriptive_analysis(mtcars)

# 2. Access components
stats <- result$statistics
insights <- result$insights

# 3. Create custom visualizations
p <- plot_variable(mtcars, x = "mpg", plot_type = "histogram")

# 4. Generate custom report
generate_report(
  result,
  "custom_report",
  format = "word",
  template = "nature",
  include_plots = TRUE,
  title = "Custom Title"
)
```

### Batch Processing

```r
# Analyze multiple datasets
datasets <- list(mtcars = mtcars, iris = iris, airquality = airquality)

for (name in names(datasets)) {
  analyze_all(
    datasets[[name]],
    output_file = paste0(name, "_analysis"),
    format = "all"
  )
}
```

---

## Package Statistics

- **Total Functions**: 70+
- **Statistical Methods**: 35+ (including regression, ANOVA, imputation)
- **Visualization Types**: 11
- **Output Formats**: 4 (HTML, Word, Excel, Markdown)
- **Journal Templates**: 5
- **Color Palettes**: 16 (colorblind-safe)
- **Imputation Methods**: 6 (mean, median, mode, regression, MICE, KNN)
- **Lines of Code**: ~18,000
- **Test Coverage**: Comprehensive

---

## Contributing

Contributions welcome! Please:

1. Report bugs at [GitHub Issues](https://github.com/drabdulali/descriptR/issues)
2. Suggest features via [Issues](https://github.com/drabdulali/descriptR/issues)
3. Submit pull requests following package style

---

## Citation

```bibtex
@Manual{descriptR,
  title = {descriptR: Comprehensive Descriptive Statistics with Automated Insights},
  author = {Abdul Ali},
  year = {2025},
  note = {R package version 0.1.0},
  url = {https://github.com/drabdulali/descriptR}
}
```

---

## License

MIT License - see [LICENSE](LICENSE) file for details.

---

## Author

**Abdul Ali**
- Email: abdulali.wallana125@gmail.com
- GitHub: [@drabdulali](https://github.com/drabdulali)

---

## Acknowledgments

- R Core Team for maintaining R
- Tidyverse team for foundational packages
- Statistical methods contributors
- R community for feedback and testing

---

**Version**: 0.1.0 | **Status**: Stable | **Release**: 2025-12-31

---

## What's Next?

```r
# Install now and start analyzing!
devtools::install_github("drabdulali/descriptR")

# Your first complete analysis in 2 lines:
library(descriptR)
analyze_all(mtcars, "my_first_analysis", format = "all")
```

**Happy Analyzing!**
