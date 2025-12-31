# Test suite for validation functions

test_that("validate_data_frame works correctly", {
  # Valid inputs
  expect_true(suppressMessages(
    descriptR:::validate_data_frame(mtcars)
  ))

  # NULL with allow_null = TRUE
  expect_true(suppressMessages(
    descriptR:::validate_data_frame(NULL, allow_null = TRUE)
  ))

  # Errors
  expect_error(
    descriptR:::validate_data_frame(NULL, allow_null = FALSE),
    "cannot be NULL"
  )

  expect_error(
    descriptR:::validate_data_frame("not a data frame"),
    "must be a data frame"
  )

  expect_error(
    descriptR:::validate_data_frame(mtcars[0, ], min_rows = 5),
    "must have at least 5 rows"
  )
})

test_that("validate_variables works correctly", {
  # Valid inputs
  expect_true(suppressMessages(
    descriptR:::validate_variables(mtcars, c("mpg", "cyl"))
  ))

  # NULL with allow_null = TRUE
  expect_true(suppressMessages(
    descriptR:::validate_variables(mtcars, NULL, allow_null = TRUE)
  ))

  # Errors
  expect_error(
    descriptR:::validate_variables(mtcars, NULL, allow_null = FALSE),
    "cannot be NULL"
  )

  expect_error(
    descriptR:::validate_variables(mtcars, 123),
    "must be a character vector"
  )

  expect_error(
    descriptR:::validate_variables(mtcars, c("mpg", "nonexistent")),
    "not found in data"
  )
})

test_that("validate_numeric works correctly", {
  # Valid numeric variables
  expect_true(suppressMessages(
    descriptR:::validate_numeric(mtcars, c("mpg", "hp"))
  ))

  # Errors for non-numeric
  df <- data.frame(
    num = 1:5,
    char = letters[1:5],
    stringsAsFactors = FALSE
  )

  expect_error(
    descriptR:::validate_numeric(df, "char"),
    "must be numeric"
  )
})

test_that("validate_conf_level works correctly", {
  # Valid confidence levels
  expect_true(suppressMessages(
    descriptR:::validate_conf_level(0.95)
  ))

  # Errors
  expect_error(
    descriptR:::validate_conf_level(0),
    "must be between 0 and 1"
  )

  expect_error(
    descriptR:::validate_conf_level(1),
    "must be between 0 and 1"
  )

  expect_error(
    descriptR:::validate_conf_level(1.5),
    "must be between 0 and 1"
  )

  expect_error(
    descriptR:::validate_conf_level(c(0.9, 0.95)),
    "must be a single numeric value"
  )
})

test_that("check_sufficient_data works correctly", {
  # Valid data
  expect_true(suppressMessages(
    descriptR:::check_sufficient_data(1:10, min_n = 5)
  ))

  # Handles NA values
  expect_true(suppressMessages(
    descriptR:::check_sufficient_data(c(1:10, NA, NA), min_n = 5)
  ))

  # Errors for insufficient data
  expect_error(
    descriptR:::check_sufficient_data(1:3, min_n = 5),
    "only 3 non-missing observations"
  )

  # Warning for zero variance
  expect_warning(
    descriptR:::check_sufficient_data(rep(5, 10)),
    "zero variance"
  )
})
