# Test suite for variable type detection

test_that("detect_var_type correctly identifies continuous variables", {
  # Standard continuous numeric
  expect_equal(detect_var_type(iris$Sepal.Length), "continuous")
  expect_equal(detect_var_type(rnorm(100)), "continuous")

  # Many unique values
  expect_equal(detect_var_type(1:100), "continuous")
})

test_that("detect_var_type correctly identifies discrete numeric variables", {
  # Few unique values
  expect_equal(detect_var_type(mtcars$cyl), "discrete_numeric")
  expect_equal(detect_var_type(rep(1:5, each = 10)), "discrete_numeric")

  # Integer with limited range
  expect_equal(detect_var_type(sample(1:10, 100, replace = TRUE)), "discrete_numeric")
})

test_that("detect_var_type correctly identifies binary variables", {
  # Logical
  expect_equal(detect_var_type(c(TRUE, FALSE, TRUE, FALSE)), "binary")

  # 0/1 numeric
  expect_equal(detect_var_type(c(0, 1, 0, 1, 0, 1)), "binary")
  expect_equal(detect_var_type(mtcars$vs), "binary")

  # Two-level factor
  expect_equal(detect_var_type(factor(c("A", "B", "A", "B"))), "binary")

  # Two-level character
  expect_equal(detect_var_type(c("Yes", "No", "Yes", "No")), "binary")
})

test_that("detect_var_type correctly identifies ordinal variables", {
  # Ordered factor
  x <- factor(c("Low", "Medium", "High", "Low", "High"),
             levels = c("Low", "Medium", "High"),
             ordered = TRUE)
  expect_equal(detect_var_type(x), "ordinal")
})

test_that("detect_var_type correctly identifies nominal variables", {
  # Factor
  expect_equal(detect_var_type(iris$Species), "nominal")

  # Character with low cardinality
  expect_equal(detect_var_type(rep(letters[1:5], each = 10)), "nominal")
})

test_that("detect_var_type correctly identifies datetime variables", {
  # Date
  expect_equal(detect_var_type(Sys.Date() + 1:10), "datetime")

  # POSIXct
  expect_equal(detect_var_type(Sys.time() + 1:10), "datetime")
})

test_that("detect_var_type correctly identifies text variables", {
  # High cardinality character
  expect_equal(detect_var_type(paste0("ID_", 1:100)), "text")

  # Mostly unique values
  expect_equal(detect_var_type(as.character(1:100)), "text")
})

test_that("detect_var_type handles edge cases", {
  # NULL
  expect_equal(detect_var_type(NULL), "unknown")

  # Empty vector
  expect_equal(detect_var_type(numeric(0)), "unknown")

  # All NA
  expect_equal(detect_var_type(rep(NA, 10)), "unknown")
})

test_that("detect_all_types works correctly", {
  # Test with iris
  types <- detect_all_types(iris)

  expect_equal(length(types), 5)
  expect_equal(types["Sepal.Length"], c(Sepal.Length = "continuous"))
  expect_equal(types["Species"], c(Species = "nominal"))

  # Test with mtcars subset
  types <- detect_all_types(mtcars, vars = c("mpg", "cyl", "vs"))
  expect_equal(length(types), 3)
  expect_equal(types["mpg"], c(mpg = "continuous"))
  expect_equal(types["cyl"], c(cyl = "discrete_numeric"))
  expect_equal(types["vs"], c(vs = "binary"))
})

test_that("get_variables_by_type groups correctly", {
  # Test with iris
  vars_by_type <- get_variables_by_type(iris)

  expect_true("continuous" %in% names(vars_by_type))
  expect_true("nominal" %in% names(vars_by_type))

  # Check continuous variables
  expect_true("Sepal.Length" %in% vars_by_type$continuous)
  expect_true("Sepal.Width" %in% vars_by_type$continuous)

  # Check nominal variables
  expect_true("Species" %in% vars_by_type$nominal)

  # Test with mtcars
  vars_by_type <- get_variables_by_type(mtcars)

  expect_length(vars_by_type$continuous, 7)  # mpg, disp, hp, drat, wt, qsec
  expect_length(vars_by_type$discrete_numeric, 3)  # cyl, gear, carb
  expect_length(vars_by_type$binary, 2)  # vs, am
})

test_that("is_numeric_type works correctly", {
  expect_true(is_numeric_type(iris$Sepal.Length))
  expect_true(is_numeric_type(mtcars$cyl))
  expect_true(is_numeric_type(mtcars$vs))  # Binary numeric

  expect_false(is_numeric_type(iris$Species))
  expect_false(is_numeric_type(letters[1:10]))
})

test_that("is_categorical_type works correctly", {
  expect_true(is_categorical_type(iris$Species))
  expect_true(is_categorical_type(factor(c("A", "B", "A"))))

  expect_false(is_categorical_type(iris$Sepal.Length))
  expect_false(is_categorical_type(1:10))
})

test_that("suggest_analysis_type provides appropriate suggestions", {
  # Continuous
  suggestions <- suggest_analysis_type(iris$Sepal.Length)
  expect_equal(suggestions$type, "continuous")
  expect_true("mean" %in% suggestions$descriptive)
  expect_true("histogram" %in% suggestions$visualization)
  expect_true("t-test" %in% suggestions$tests)

  # Nominal
  suggestions <- suggest_analysis_type(iris$Species)
  expect_equal(suggestions$type, "nominal")
  expect_true("frequencies" %in% suggestions$descriptive)
  expect_true("bar plot" %in% suggestions$visualization)
  expect_true("chi-square" %in% suggestions$tests)

  # Binary
  suggestions <- suggest_analysis_type(c(0, 1, 0, 1, 0, 1))
  expect_equal(suggestions$type, "binary")
  expect_true("proportion" %in% suggestions$descriptive)
})

test_that("print_variable_types runs without error", {
  # Should run without error
  expect_invisible(
    suppressMessages(print_variable_types(iris))
  )

  # Return value should be a data frame
  result <- suppressMessages(
    print_variable_types(mtcars)
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), ncol(mtcars))
  expect_true("Variable" %in% names(result))
  expect_true("Type" %in% names(result))
})

test_that("custom unique_threshold works correctly", {
  # With default threshold (20), should be continuous
  x <- 1:25
  expect_equal(detect_var_type(x), "continuous")

  # With higher threshold, still continuous
  expect_equal(detect_var_type(x, unique_threshold = 30), "continuous")

  # With lower threshold, becomes discrete
  expect_equal(detect_var_type(x, unique_threshold = 15), "discrete_numeric")
})
