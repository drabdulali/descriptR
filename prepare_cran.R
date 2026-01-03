# CRAN Preparation Script for descriptR
# Run this script before submitting to CRAN
#
# Usage: source("prepare_cran.R")

message("=== descriptR CRAN Preparation ===\n")

# Step 1: Generate documentation (CRITICAL)
message("Step 1: Generating documentation (man/ directory)...")
if (!requireNamespace("roxygen2", quietly = TRUE)) {
  stop("Please install roxygen2: install.packages('roxygen2')")
}
roxygen2::roxygenise()
message("  Done! man/ directory created with .Rd files.\n")

# Step 2: Check spelling (optional but recommended)
message("Step 2: Checking spelling...")
if (requireNamespace("spelling", quietly = TRUE)) {
  spelling_errors <- spelling::spell_check_package()
  if (nrow(spelling_errors) > 0) {
    message("  Found ", nrow(spelling_errors), " potential spelling issues:")
    print(spelling_errors)
  } else {
    message("  No spelling errors found.")
  }
} else {
  message("  Skipped (install 'spelling' package for spell check)")
}
message("")

# Step 3: Run R CMD check
message("Step 3: Running R CMD check...")
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Please install devtools: install.packages('devtools')")
}
check_results <- devtools::check(cran = TRUE)
message("")

# Step 4: Summary
message("=== CRAN Preparation Summary ===\n")

if (length(check_results$errors) == 0 &&
    length(check_results$warnings) == 0) {
  message("SUCCESS! Package is ready for CRAN submission.")
  message("\nNext steps:")
  message("1. Test on multiple platforms: rhub::check_for_cran()")
  message("2. Test on Windows: devtools::check_win_devel()")
  message("3. Submit: devtools::release()")
} else {
  message("Issues found:")
  if (length(check_results$errors) > 0) {
    message("  Errors: ", length(check_results$errors))
  }
  if (length(check_results$warnings) > 0) {
    message("  Warnings: ", length(check_results$warnings))
  }
  if (length(check_results$notes) > 0) {
    message("  Notes: ", length(check_results$notes))
  }
  message("\nPlease fix the issues above before CRAN submission.")
}

message("\n=== Done ===")
