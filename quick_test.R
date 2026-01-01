#!/usr/bin/env Rscript
# Quick Test - Run this to verify Phase 1 functions work

library(descriptR)

# Test 1: Regression
cat("Testing Regression...\n")
reg <- perform_regression_analysis(mtcars, "mpg", c("wt", "hp"))
print(reg)

# Test 2: ANOVA
cat("\n\nTesting ANOVA...\n")
anova <- perform_anova(iris, "Sepal.Length", "Species")
print(anova)

# Test 3: Imputation
cat("\n\nTesting Imputation...\n")
imp <- impute_missing(airquality, method = "mean")
print(imp)

cat("\n\nAll functions work!\n")
