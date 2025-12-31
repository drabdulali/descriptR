# Test suite for categorical descriptive statistics

test_that("describe_categorical works with basic input", {
  result <- describe_categorical(iris, vars = "Species")

  expect_s3_class(result, "descriptR_categorical")
  expect_true("frequencies" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_equal(result$n_vars, 1)
})

test_that("compute_frequencies creates correct structure", {
  x <- rep(letters[1:3], c(10, 20, 30))
  freq <- descriptR:::compute_frequencies(x)

  expect_s3_class(freq, "data.frame")
  expect_true("Category" %in% names(freq))
  expect_true("Count" %in% names(freq))
  expect_true("Percentage" %in% names(freq))
  expect_equal(nrow(freq), 3)

  # Check counts
  expect_equal(sum(freq$Count), 60)
  expect_equal(sum(freq$Percentage), 100, tolerance = 0.01)
})

test_that("compute_entropy works correctly", {
  # Uniform distribution (maximum entropy)
  x_uniform <- rep(1:4, each = 25)
  entropy_uniform <- compute_entropy(x_uniform)
  expect_gt(entropy_uniform, 1.3)  # log(4) ≈ 1.386

  # Concentrated distribution (low entropy)
  x_concentrated <- c(rep(1, 95), rep(2, 5))
  entropy_concentrated <- compute_entropy(x_concentrated)
  expect_lt(entropy_concentrated, 0.5)
})

test_that("compute_herfindahl works correctly", {
  # Equal distribution (low concentration)
  x_equal <- rep(1:4, each = 25)
  h_equal <- compute_herfindahl(x_equal)
  expect_equal(h_equal, 0.25, tolerance = 0.01)

  # High concentration
  x_concentrated <- c(rep(1, 90), rep(2, 10))
  h_concentrated <- compute_herfindahl(x_concentrated)
  expect_gt(h_concentrated, 0.8)
})

test_that("test_goodness_of_fit works", {
  # Uniform distribution
  x <- sample(1:4, 100, replace = TRUE)
  result <- test_goodness_of_fit(x, p = rep(0.25, 4))

  expect_true("p_value" %in% names(result))
  expect_true("statistic" %in% names(result))
  expect_true("significant" %in% names(result))
  expect_equal(result$df, 3)
})

test_that("describe_categorical handles missing data", {
  x <- c(letters[1:3], NA, NA)
  df <- data.frame(cat = x)

  result <- describe_categorical(df, vars = "cat")

  expect_equal(result$summary$N_Missing, 2)
  expect_equal(result$summary$Pct_Missing, 40)
})

test_that("print method works for descriptR_categorical", {
  result <- describe_categorical(iris, vars = "Species")

  # Should print without error
  expect_output(print(result), "Categorical Variable Summary")
  expect_output(print(result), "Species")
})
