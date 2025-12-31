# descriptR <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/drabdulali/descriptR/workflows/R-CMD-check/badge.svg)](https://github.com/drabdulali/descriptR/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/descriptR)](https://CRAN.R-project.org/package=descriptR)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

**descriptR** is a comprehensive R package for descriptive and inferential statistics that automatically detects variable types, applies appropriate analyses, and generates publication-ready outputs with narrative insights.

## Key Features

- **🎯 Unified Interface**: Single function handles all data types
- **🤖 Automated Insights**: AI-powered narrative generation from your data
- **📊 Smart Defaults**: Intelligent parameter selection based on data characteristics
- **📄 Multi-Format Output**: Export to HTML, Word, Excel, and PDF
- **🎨 Advanced Color Schemes**: 4 boldness levels (subtle/medium/bold/vivid) with color-blind safe options
- **📐 Flexible Export Resolutions**: From 72 DPI draft to 8K ultra-high resolution
- **📱 Social Media Ready**: Pre-configured formats for Instagram, Twitter, LinkedIn
- **🔬 Journal-Specific**: Templates for Nature, Science, NEJM, Lancet, JAMA
- **🔍 Missing Data Intelligence**: Advanced pattern detection and imputation suggestions
- **📈 120+ Functions**: Comprehensive suite covering descriptive and inferential statistics

## Installation

### Development version from GitHub

```r
# install.packages("devtools")
devtools::install_github("drabdulali/descriptR")
```

### CRAN (coming soon)

```r
install.packages("descriptR")
```

## Quick Start

```r
library(descriptR)

# Basic descriptive statistics
describe_data(mtcars)

# Grouped comparison
describe_grouped(mtcars, vars = "mpg", group_by = "cyl")

# With visualization
results <- describe_data(iris, include_plots = TRUE)

# Export publication-ready plot
visualize_descriptives(mtcars, vars = c("mpg", "hp"),
                      color_scheme = "medium") %>%
  export_plot("figure1.png", resolution = "4k")
```

## Sample Dataset

The package includes a comprehensive synthetic clinical trial dataset with 100 subjects and 41 variables, perfect for testing all package features:

```r
# Load sample data
data(clinical_trial_data)

# Explore the dataset
describe_data(clinical_trial_data)

# Compare treatment groups
compare_groups(clinical_trial_data,
               outcome = "followup_score",
               group = "treatment_group")
```

## Main Functions

### Descriptive Statistics
- `describe_data()`: Comprehensive descriptive statistics
- `describe_numeric()`: Specialized for continuous variables
- `describe_categorical()`: Specialized for categorical variables
- `describe_grouped()`: Group-wise comparisons
- `describe_distribution()`: Distribution analysis
- `describe_association()`: Relationship analysis

### Inferential Statistics
- `perform_t_test()`: All t-test variants
- `perform_anova()`: ANOVA with post-hoc tests
- `perform_correlation_test()`: Correlation with inference
- `fit_linear_model()`: Linear regression
- `fit_logistic_model()`: Logistic regression
- `perform_principal_components()`: PCA
- `fit_mixed_model()`: Linear mixed models

### Visualization
- `visualize_descriptives()`: Automated visualization
- `plot_histogram()`, `plot_boxplot()`, `plot_violin()`
- `plot_correlation_matrix()`: Correlation heatmaps
- `export_plot()`: Multi-resolution export
- `export_plots_batch()`: Batch export in multiple formats

### Data Quality
- `check_data_quality()`: Comprehensive quality check
- `missing_analysis()`: Missing data pattern analysis
- `detect_outliers()`: Multiple outlier detection methods
- `assess_normality()`: Suite of normality tests

### Reporting
- `report_descriptives()`: Publication-ready reports
- `export_to_excel()`: Excel workbooks
- `export_to_word()`: Word documents
- `create_analysis_report()`: Complete analysis report

## Color Schemes

descriptR offers four color boldness levels optimized for different use cases:

- **Subtle** (20-40% saturation): Academic papers, professional reports
- **Medium** (50-70% saturation): General publications, presentations [DEFAULT]
- **Bold** (80-95% saturation): Posters, social media, attention-grabbing
- **Vivid** (95-100% saturation): Digital displays, infographics, dark backgrounds

All color schemes are color-blind safe and WCAG compliant.

```r
# Subtle colors for academic paper
plot_paper <- visualize_descriptives(data, color_scheme = "subtle")
export_plot(plot_paper, "figure1.tiff", resolution = "journal_double_column", dpi = 600)

# Bold colors for conference poster
plot_poster <- visualize_descriptives(data, color_scheme = "bold")
export_plot(plot_poster, "poster.png", resolution = "poster_a1")

# Vivid colors for social media
plot_social <- visualize_descriptives(data, color_scheme = "vivid")
export_plot(plot_social, "instagram.png", resolution = "instagram_square")
```

## Export Resolutions

Export plots at various resolutions for different purposes:

- **Draft**: 72 DPI (quick previews)
- **Web/Presentation**: 96-150 DPI (HD, 2K, 4K)
- **Print**: 300-1200 DPI (standard to professional printing)
- **Journal-specific**: Nature, Science, NEJM formats
- **Social Media**: Instagram, Twitter, LinkedIn formats
- **Posters**: A0-A4 sizes at 300 DPI

## Documentation

- [Getting Started Vignette](vignettes/introduction.Rmd)
- [Descriptive Statistics Guide](vignettes/descriptive_statistics.Rmd)
- [Grouped Comparisons](vignettes/grouped_comparisons.Rmd)
- [Visualization and Colors](vignettes/visualization_and_colors.Rmd)
- [Publication Outputs](vignettes/publication_outputs.Rmd)
- [Function Reference](https://drabdulali.github.io/descriptR/reference/)

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

- Report bugs: [GitHub Issues](https://github.com/drabdulali/descriptR/issues)
- Suggest features: [Feature Requests](https://github.com/drabdulali/descriptR/issues)
- Submit pull requests: [Contributing Guide](CONTRIBUTING.md)

## Citation

If you use descriptR in your research, please cite:

```
Ali, A. (2025). descriptR: Comprehensive Descriptive Statistics with Automated
Insights. R package version 0.1.0. https://github.com/drabdulali/descriptR
```

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Author

**Dr. Abdul Ali**
📧 dr.abdulali@example.com
🐙 [GitHub](https://github.com/drabdulali)

## Acknowledgments

- The R community for excellent statistical packages
- All contributors and beta testers
- Users who provide feedback and suggestions

---

**Status**: 🚧 Under active development - Phase 1 (Foundation) in progress
