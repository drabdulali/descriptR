# descriptR

<!-- badges: start -->
[![R-CMD-check](https://github.com/drabdulali/descriptR/workflows/R-CMD-check/badge.svg)](https://github.com/drabdulali/descriptR/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/descriptR)](https://CRAN.R-project.org/package=descriptR)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](https://github.com/drabdulali/descriptR)
<!-- badges: end -->

## Overview

**descriptR** is a comprehensive R package for descriptive and inferential statistics that automatically detects variable types, applies appropriate analyses, and generates publication-ready outputs with narrative insights.

### ✨ What Makes descriptR Different?

- **🎯 One-Step Workflow**: `analyze_and_report()` does everything from analysis to formatted reports
- **🤖 Automated Insights**: Every analysis generates human-readable interpretations
- **📊 Comprehensive Methods**: 30+ statistical methods from basics to advanced (PCA, EFA, GLM)
- **📄 Multi-Format Export**: HTML, Word, Excel, Markdown in 5 journal templates
- **🎨 16 Colorblind-Safe Palettes**: 4 schemes × 4 boldness levels for any context
- **📐 Publication-Ready**: WCAG-compliant colors, high-DPI export, professional formatting
- **🔬 Complete Toolkit**: Descriptives, effect sizes, diagnostics, missing data, outliers, advanced models
- **✅ 440+ Tests**: Thoroughly tested and documented

## Installation

```r
# From GitHub (development version)
# install.packages("devtools")
devtools::install_github("drabdulali/descriptR")

# From CRAN (coming soon)
install.packages("descriptR")
```

## Quick Start

### Complete Analysis in One Line

```r
library(descriptR)

# Analyze data and generate report in one step
analyze_and_report(
  mtcars,
  output_file = "analysis",
  analysis_type = "comprehensive",
  format = "html",
  template = "apa"
)
```

### Basic Descriptive Statistics

```r
# Descriptive statistics
result <- perform_descriptive_analysis(mtcars)
print(result$statistics)
print(result$insights)

# Grouped comparison
result <- perform_grouped_analysis(
  iris,
  group = "Species",
  vars = c("Sepal.Length", "Petal.Length")
)
```

### Visualization

```r
# Auto-select plot type
plot <- plot_variable(mtcars, x = "mpg")

# Scatter plot with group colors
plot <- plot_variable(
  iris,
  x = "Sepal.Length",
  y = "Petal.Length",
  group = "Species",
  color_scheme = "bold"
)

# Export high-resolution
save_plot(plot, "figure1.png", dpi = 600)
```

## Main Features

### 📊 Descriptive Statistics
- `perform_descriptive_analysis()` - Complete descriptive statistics
- `perform_grouped_analysis()` - Group comparisons with effect sizes
- `perform_correlation_analysis()` - Correlation matrices with p-values

### 📏 Effect Sizes
- `compute_cohens_d()` - Cohen's d and Hedges' g
- `compute_eta_squared()` - Eta-squared (partial and full)
- `compute_omega_squared()` - Omega-squared (bias-corrected)
- `compute_cramers_v()` - Cramér's V for categorical data
- `convert_effect_size()` - Convert between effect size metrics

### 🔍 Data Quality
- `assess_normality()` - 5 normality tests + transformation suggestions
- `analyze_missing()` - Missing data patterns, MCAR testing
- `detect_outliers()` - 6 detection methods with consensus

### 🎓 Advanced Methods
- `perform_pca()` - Principal Component Analysis with rotation
- `perform_efa()` - Exploratory Factor Analysis
- `perform_glm()` - Generalized Linear Models (auto-family)
- `perform_mann_whitney()`, `perform_kruskal_wallis()` - Non-parametric tests

### 🎨 Visualization
- `plot_variable()` - Intelligent automatic plotting
- `plot_correlation_matrix()` - Heatmaps with clustering
- `get_color_scheme()` - 16 colorblind-safe palettes
- `save_plot()` - Multi-format export (PNG, PDF, SVG)
- Diagnostic plots for normality, outliers, missing data, PCA

### 📄 Reporting
- `generate_report()` - Multi-format reports (HTML, Word, Excel, MD)
- `analyze_and_report()` - One-step analysis + report
- 5 journal templates: Default, APA, Nature, Science, NEJM
- `batch_export_tables()` - Export multiple tables
- `export_plots()` - Batch plot export

## Color Schemes

16 colorblind-safe palettes across 4 schemes and 4 boldness levels:

**Schemes:**
- **Subtle** - Muted colors for publications
- **Medium** - Balanced (default)
- **Bold** - High saturation for presentations
- **Vivid** - Maximum impact for posters/social media

**Boldness Levels:** 1 (most subtle) to 4 (most bold)

```r
# Get color scheme
colors <- get_color_scheme("medium", boldness = 2, n = 8)

# Use in plots
plot <- plot_variable(iris, x = "Sepal.Length", y = "Petal.Length",
                      group = "Species", color_scheme = "bold")

# Check accessibility
check_color_accessibility("#000000", "#FFFFFF")

# Create custom palettes
diverging <- create_diverging_palette(n = 11)
sequential <- create_sequential_palette(n = 9)
```

## Export & Reporting

```r
# Generate report in multiple formats
generate_report(
  result,
  output_file = "report",
  format = "all",  # HTML, Word, Excel, Markdown
  template = "nature",
  title = "My Analysis"
)

# Export plots at different resolutions
save_plot(plot, "figure.png", dpi = 300)  # Publication
save_plot(plot, "figure.png", dpi = 600)  # High-res
save_plot(plot, "figure.pdf")  # Vector

# Batch export
plots <- list(hist = plot1, scatter = plot2)
export_plots(plots, "figures", formats = c("png", "pdf"))
```

## Documentation

**Vignettes:**
- [Getting Started](vignettes/getting-started.Rmd) - Quick start and basic usage
- [Advanced Analysis](vignettes/advanced-analysis.Rmd) - Effect sizes, PCA, EFA, GLM, non-parametric tests
- [Visualization and Reporting](vignettes/visualization-reporting.Rmd) - Colors, plots, and export

**Quick Help:**
```r
# View vignettes
browseVignettes("descriptR")

# Function documentation
?analyze_and_report
?generate_report
?get_color_scheme

# Package overview
help(package = "descriptR")
```

## Development Statistics

- **Total Functions**: 60+
- **Lines of Code**: ~15,000
- **Test Cases**: 440+
- **Code Coverage**: Comprehensive
- **Vignettes**: 3
- **Color Palettes**: 16
- **Journal Templates**: 5
- **Statistical Methods**: 30+

## Contributing

Contributions welcome! Please:

- Report bugs at [GitHub Issues](https://github.com/drabdulali/descriptR/issues)
- Suggest features via [Issues](https://github.com/drabdulali/descriptR/issues)
- Submit pull requests following package style

## Citation

If you use descriptR in your research:

```bibtex
@Manual{descriptR,
  title = {descriptR: Comprehensive Descriptive Statistics with Automated Insights},
  author = {Abdul Ali},
  year = {2025},
  note = {R package version 0.1.0},
  url = {https://github.com/drabdulali/descriptR}
}
```

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Author

**Abdul Ali**
- 📧 Email: dr.abdulali@example.com
- 🐙 GitHub: [@drabdulali](https://github.com/drabdulali)

## Acknowledgments

- R Core Team for maintaining R
- Tidyverse team for foundational packages (dplyr, ggplot2, etc.)
- Statistical methods contributors
- R community for feedback and testing

---

**Version**: 0.1.0 | **Status**: ✅ Stable | **Release**: 2025-12-31
