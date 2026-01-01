# Install Updated descriptR Package

## Method 1: Install from Local Source (Recommended)

### Step 1: Remove Old Version
```r
# In R console
remove.packages("descriptR")
```

### Step 2: Install from Local Directory
```r
# Install from local source
setwd("/path/to/descriptR")  # Change to your actual path
devtools::install()

# OR use this alternative
install.packages("/path/to/descriptR", repos = NULL, type = "source")
```

### Step 3: Verify Installation
```r
library(descriptR)

# Check version
packageVersion("descriptR")

# Check new functions are available
exists("perform_regression_analysis")  # Should be TRUE
exists("perform_anova")                # Should be TRUE
exists("impute_missing")               # Should be TRUE

# View all exports
getNamespaceExports("descriptR")
```

---

## Method 2: Install from GitHub

### Remove and Reinstall
```r
# Remove old version
remove.packages("descriptR")

# Install from GitHub
devtools::install_github("drabdulali/descriptR")

# Or specify branch
devtools::install_github("drabdulali/descriptR@descriptR")
```

---

## Method 3: Build and Install (Most Complete)

### From Terminal/Command Line
```bash
cd /path/to/descriptR

# Build package
R CMD build .

# Install the built package
R CMD INSTALL descriptR_0.1.0.tar.gz
```

### Or in R
```r
setwd("/path/to/descriptR")

# Build
devtools::build()

# Install
devtools::install()
```

---

## Quick Test After Installation

```r
library(descriptR)

# Test 1: Regression
reg <- perform_regression_analysis(
  mtcars,
  outcome = "mpg",
  predictors = c("wt", "hp")
)
print(reg)

# Test 2: ANOVA
anova <- perform_anova(
  iris,
  outcome = "Sepal.Length",
  groups = "Species"
)
print(anova)

# Test 3: Imputation
imp <- impute_missing(
  airquality,
  method = "mean"
)
print(imp)

# If all three work, installation successful!
cat("\n✓ All Phase 1 functions working!\n")
```

---

## Troubleshooting

### Error: "package 'descriptR' is not available"
```r
# Make sure devtools is installed
install.packages("devtools")
library(devtools)

# Try installing from local source
setwd("/path/to/descriptR")
devtools::load_all(".")  # Test without installing
```

### Error: "there is no package called 'descriptR'"
```r
# Check installed packages
installed.packages()["descriptR",]

# If not found, reinstall
devtools::install()
```

### Error: "object 'perform_regression_analysis' not found"
```r
# Reload package
detach("package:descriptR", unload = TRUE)
library(descriptR)

# Or restart R session and try again
.rs.restartR()  # In RStudio
```

### Dependencies Missing
```r
# Install required packages
install.packages(c("dplyr", "ggplot2", "tidyr", "purrr"))
```

---

## Complete Reinstall (Clean Slate)

```r
# 1. Unload if loaded
if ("descriptR" %in% loadedNamespaces()) {
  detach("package:descriptR", unload = TRUE)
}

# 2. Remove package
remove.packages("descriptR")

# 3. Clear workspace
rm(list = ls())

# 4. Restart R session (in RStudio)
.rs.restartR()

# 5. Install fresh
setwd("/path/to/descriptR")
devtools::install()

# 6. Load and test
library(descriptR)
?perform_regression_analysis
```

---

## Verify All Phase 1 Functions

```r
library(descriptR)

# Check NAMESPACE exports
phase1_funcs <- c(
  "perform_regression_analysis",
  "perform_anova",
  "impute_missing"
)

exports <- getNamespaceExports("descriptR")
for (func in phase1_funcs) {
  cat(sprintf("%s: %s\n",
              func,
              ifelse(func %in% exports, "✓ EXPORTED", "✗ MISSING")))
}

# Check help files
?perform_regression_analysis
?perform_anova
?impute_missing
```

---

## Quick Installation Script

Save this as `install_descriptR.R` and run it:

```r
#!/usr/bin/env Rscript
# Quick installation script for descriptR

cat("Installing updated descriptR package...\n\n")

# Remove old version
tryCatch({
  remove.packages("descriptR")
  cat("✓ Old version removed\n")
}, error = function(e) {
  cat("No previous version found\n")
})

# Install devtools if needed
if (!require("devtools")) {
  install.packages("devtools")
}

# Install from current directory
setwd("/path/to/descriptR")  # CHANGE THIS PATH
devtools::install()

# Load and verify
library(descriptR)

cat("\n=== Verification ===\n")
cat("Package version:", as.character(packageVersion("descriptR")), "\n")
cat("Phase 1 functions available:\n")

funcs <- c("perform_regression_analysis", "perform_anova", "impute_missing")
for (f in funcs) {
  cat(sprintf("  - %s: %s\n", f, ifelse(exists(f), "✓", "✗")))
}

cat("\n✓ Installation complete!\n")
```

---

## After Installation: Next Steps

1. **Run Quick Test**
   ```bash
   Rscript quick_test.R
   ```

2. **Run Comprehensive Tests**
   ```bash
   Rscript test_phase1_functions.R
   ```

3. **Try Your Own Data**
   ```r
   library(descriptR)

   # Your regression analysis
   result <- perform_regression_analysis(
     your_data,
     outcome = "your_outcome",
     predictors = c("var1", "var2", "var3")
   )
   ```

4. **Read Documentation**
   ```r
   ?perform_regression_analysis
   ?perform_anova
   ?impute_missing
   ```

---

## Installation Status Check

Run this to verify everything is working:

```r
library(descriptR)

cat("=== descriptR Installation Status ===\n\n")

# Package info
cat("Version:", as.character(packageVersion("descriptR")), "\n")
cat("Location:", find.package("descriptR"), "\n\n")

# Phase 1 functions
cat("Phase 1 Functions:\n")
phase1 <- c("perform_regression_analysis", "perform_anova", "impute_missing")
for (f in phase1) {
  status <- tryCatch({
    get(f)
    "✓ Available"
  }, error = function(e) "✗ Not Found")
  cat(sprintf("  %s: %s\n", f, status))
}

# Total functions
exports <- getNamespaceExports("descriptR")
cat("\nTotal exported functions:", length(exports), "\n")

cat("\n✓ Package ready to use!\n")
```
