# descriptR 0.1.0

## Initial Release (2025-12-31)

First public release of descriptR - a comprehensive R package for descriptive and inferential statistics with automated insights and publication-ready outputs.

### Major Features

#### ✅ Descriptive Statistics (COMPLETE)
* `perform_descriptive_analysis()` - Comprehensive descriptive statistics
* `perform_grouped_analysis()` - Group-wise comparisons with effect sizes
* `perform_correlation_analysis()` - Correlation matrices with significance tests
* Automatic variable type detection
* Missing data handling and reporting
* Narrative insight generation

#### ✅ Effect Sizes (COMPLETE)
* `compute_cohens_d()` - Cohen's d and Hedges' g
* `compute_eta_squared()` - Eta-squared for ANOVA (partial & full)
* `compute_omega_squared()` - Omega-squared (bias-corrected)
* `compute_cramers_v()` - Cramér's V for categorical data
* `compute_phi()` - Phi coefficient for 2×2 tables
* `convert_effect_size()` - Convert between metrics
* `compute_all_effect_sizes()` - Comprehensive analysis
* Confidence intervals for all effect sizes

#### ✅ Data Quality Assessment (COMPLETE)
* `assess_normality()` - 5 normality tests (Shapiro-Wilk, KS, Anderson-Darling, Jarque-Bera, D'Agostino)
* `analyze_missing()` - Missing data patterns, MCAR testing, recommendations
* `detect_outliers()` - 6 detection methods with consensus approach
* Transformation suggestions for non-normal data
* Pattern visualization and interpretation

#### ✅ Advanced Methods (COMPLETE)
* `perform_pca()` - Principal Component Analysis with rotation
* `perform_efa()` - Exploratory Factor Analysis with parallel analysis
* `perform_glm()` - Generalized Linear Models with auto-family detection
* `compare_models()` - Model comparison using AIC/BIC
* Comprehensive diagnostics and fit statistics

#### ✅ Non-Parametric Tests (COMPLETE)
* `perform_mann_whitney()` - Mann-Whitney U test
* `perform_kruskal_wallis()` - Kruskal-Wallis with post-hoc (Dunn's test)
* `perform_wilcoxon_signed()` - Wilcoxon signed-rank test
* `perform_friedman()` - Friedman test for repeated measures
* Effect sizes: rank-biserial r, epsilon-squared, Kendall's W

#### ✅ Visualization System (COMPLETE)
* 16 colorblind-safe palettes (4 schemes × 4 boldness levels)
* `get_color_scheme()` - Access publication-ready colors
* `plot_variable()` - Intelligent automatic plotting
* `plot_correlation_matrix()` - Heatmaps with clustering
* Diagnostic plots: normality, scree, missing patterns, outliers, effect sizes
* WCAG 2.1 accessibility checking
* Multi-format export (PNG, PDF, SVG, TIFF, EPS)
* DPI control (72 to 1200+)

#### ✅ Reporting & Export (COMPLETE)
* `generate_report()` - Multi-format report generation
* `analyze_and_report()` - One-step workflow (analyze + report)
* Formats: HTML, Word (.docx), Excel (.xlsx), Markdown (.md)
* Templates: Default, APA, Nature, Science, NEJM
* `quick_export_excel()` - Fast data export
* `batch_export_tables()` - Multi-table export
* `export_plots()` - Batch plot export
* Automatic narrative insights in all reports

### Package Infrastructure

#### Testing
* 440+ test cases using testthat
* Comprehensive coverage of all functions
* Integration tests for workflows
* Edge case handling

#### Documentation
* 3 comprehensive vignettes:
  - Getting Started with descriptR
  - Advanced Statistical Analysis
  - Visualization and Reporting
* Complete function documentation (roxygen2)
* Extensive examples throughout
* Best practices guides

#### Dependencies
* Core: dplyr, ggplot2, tibble, tidyr, purrr, scales, rlang, reshape2
* Optional: officer (Word), openxlsx (Excel), rmarkdown (vignettes)
* Suggested: flextable, kableExtra, DT, patchwork, viridis, psych, effectsize, nortest

### Development Statistics
* **Total Functions**: 60+
* **Lines of Code**: ~15,000
* **Test Cases**: 440+
* **Vignettes**: 3
* **Color Palettes**: 16
* **Export Formats**: 4 (HTML, Word, Excel, Markdown)
* **Journal Templates**: 5
* **Statistical Methods**: 30+

### Design Philosophy
* ✅ Intelligent defaults with automatic detection
* ✅ Narrative insights for all analyses
* ✅ Publication-ready outputs
* ✅ Tidyverse integration
* ✅ Accessibility (colorblind-safe, WCAG compliant)
* ✅ Reproducibility with full documentation
* ✅ Modular design (use components or complete workflow)

### Getting Help
* Documentation: `help(package = "descriptR")`
* Vignettes: `browseVignettes("descriptR")`
* Issues: https://github.com/drabdulali/descriptR/issues

### Acknowledgments
Built for the R community with contributions from modern statistical best practices and tidyverse principles
