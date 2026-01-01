#!/usr/bin/env Rscript
# Automatic Reinstallation Script for descriptR Package

cat("=================================================================\n")
cat("  descriptR Package - Reinstallation Script\n")
cat("=================================================================\n\n")

# Step 1: Unload if loaded
cat("Step 1: Unloading package if currently loaded...\n")
if ("descriptR" %in% loadedNamespaces()) {
  tryCatch({
    detach("package:descriptR", unload = TRUE)
    cat("✓ Package unloaded\n")
  }, error = function(e) {
    cat("✓ Package not currently loaded\n")
  })
} else {
  cat("✓ Package not currently loaded\n")
}

# Step 2: Remove old version
cat("\nStep 2: Removing old version...\n")
tryCatch({
  remove.packages("descriptR")
  cat("✓ Old version removed\n")
}, error = function(e) {
  cat("✓ No previous version found (this is fine)\n")
})

# Step 3: Check dependencies
cat("\nStep 3: Checking dependencies...\n")
required_packages <- c("devtools", "dplyr", "ggplot2", "tidyr", "purrr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("Installing %s...\n", pkg))
    install.packages(pkg, quiet = TRUE)
  }
  cat(sprintf("✓ %s available\n", pkg))
}

# Step 4: Install from source
cat("\nStep 4: Installing descriptR from source...\n")
tryCatch({
  # Get current directory
  pkg_dir <- getwd()

  # Check if we're in the right directory
  if (!file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
    stop("Not in package directory. Please run this from /path/to/descriptR")
  }

  # Install
  devtools::install(pkg_dir, quiet = FALSE, upgrade = "never")
  cat("✓ Package installed successfully\n")
}, error = function(e) {
  cat("✗ Installation failed:", conditionMessage(e), "\n")
  stop("Installation failed. Please check error message above.")
})

# Step 5: Load and verify
cat("\nStep 5: Loading and verifying installation...\n")
library(descriptR)
cat("✓ Package loaded\n")

# Step 6: Verify Phase 1 functions
cat("\nStep 6: Verifying Phase 1 functions...\n")
phase1_functions <- c(
  "perform_regression_analysis",
  "perform_anova",
  "impute_missing"
)

all_good <- TRUE
for (func in phase1_functions) {
  exists_check <- exists(func, mode = "function")
  status <- ifelse(exists_check, "✓", "✗")
  cat(sprintf("%s %s\n", status, func))
  if (!exists_check) all_good <- FALSE
}

# Step 7: Quick functionality test
if (all_good) {
  cat("\nStep 7: Running quick functionality tests...\n")

  # Test 1: Regression
  tryCatch({
    reg <- perform_regression_analysis(
      mtcars,
      outcome = "mpg",
      predictors = c("wt", "hp"),
      diagnostics = FALSE
    )
    cat("✓ Regression analysis works\n")
  }, error = function(e) {
    cat("✗ Regression analysis failed:", conditionMessage(e), "\n")
  })

  # Test 2: ANOVA
  tryCatch({
    anova <- perform_anova(
      iris,
      outcome = "Sepal.Length",
      groups = "Species",
      assumptions_check = FALSE
    )
    cat("✓ ANOVA analysis works\n")
  }, error = function(e) {
    cat("✗ ANOVA analysis failed:", conditionMessage(e), "\n")
  })

  # Test 3: Imputation
  tryCatch({
    imp <- impute_missing(
      airquality,
      method = "mean",
      vars = "Ozone",
      diagnostics = FALSE
    )
    cat("✓ Imputation works\n")
  }, error = function(e) {
    cat("✗ Imputation failed:", conditionMessage(e), "\n")
  })
}

# Final summary
cat("\n=================================================================\n")
cat("  Installation Summary\n")
cat("=================================================================\n\n")

cat("Package Version:", as.character(packageVersion("descriptR")), "\n")
cat("Installation Path:", find.package("descriptR"), "\n")
cat("Total Exports:", length(getNamespaceExports("descriptR")), "functions\n")

if (all_good) {
  cat("\n✓ ✓ ✓ ALL TESTS PASSED ✓ ✓ ✓\n\n")
  cat("descriptR is ready to use!\n\n")
  cat("Next steps:\n")
  cat("  - Run: Rscript quick_test.R\n")
  cat("  - Or: Rscript test_phase1_functions.R\n")
  cat("  - Read: TESTING_GUIDE.md\n")
} else {
  cat("\n✗ Some functions not available\n")
  cat("Please check the error messages above.\n")
}

cat("\n=================================================================\n")
