# Test suite for report generation

# Setup test data
test_data <- data.frame(
  age = c(25, 30, 35, 40, 45, 28, 32, 38, 42, 27),
  score = c(85, 90, 88, 92, 87, 86, 91, 89, 93, 84),
  group = factor(rep(c("A", "B"), each = 5)),
  stringsAsFactors = FALSE
)

# Basic Functionality =========================================================

test_that("generate_report requires result object", {
  expect_error(
    generate_report(NULL, "test.html"),
    "result cannot be NULL"
  )
})

test_that("generate_report validates format", {
  # Create a simple result object
  result <- structure(
    list(statistics = data.frame(n = 10, mean = 5)),
    class = "descriptR_result"
  )

  expect_error(
    generate_report(result, "test.txt", format = "invalid"),
    "format must be one of"
  )
})

test_that("generate_report validates template", {
  result <- structure(
    list(statistics = data.frame(n = 10, mean = 5)),
    class = "descriptR_result"
  )

  expect_error(
    generate_report(result, "test.html", template = "invalid"),
    "template must be one of"
  )
})

test_that("generate_report requires output_file", {
  result <- structure(
    list(statistics = data.frame(n = 10, mean = 5)),
    class = "descriptR_result"
  )

  expect_error(
    generate_report(result, NULL),
    "output_file is required"
  )
})

# Content Extraction ==========================================================

test_that("extract_report_content handles descriptR_result", {
  result <- structure(
    list(
      statistics = data.frame(
        variable = "age",
        n = 10,
        mean = 35,
        sd = 7.07
      ),
      insights = c("Sample size is adequate"),
      metadata = list(timestamp = Sys.time())
    ),
    class = "descriptR_result"
  )

  content <- extract_report_content(result)

  expect_true(is.list(content))
  expect_true("statistics" %in% names(content))
  expect_true("insights" %in% names(content))
  expect_true("metadata" %in% names(content))
})

test_that("extract_report_content handles missing components", {
  result <- structure(
    list(statistics = data.frame(n = 10)),
    class = "descriptR_result"
  )

  content <- extract_report_content(result)

  expect_true(is.list(content))
  expect_true("statistics" %in% names(content))
})

test_that("extract_report_content handles PCA results", {
  # Create minimal PCA result structure
  result <- structure(
    list(
      variance_explained = data.frame(
        component = 1:3,
        variance = c(0.6, 0.3, 0.1)
      ),
      loadings = matrix(runif(9), ncol = 3)
    ),
    class = "descriptR_pca"
  )

  content <- extract_report_content(result)

  expect_true(is.list(content))
  expect_true("variance_explained" %in% names(content))
})

# Markdown Generation =========================================================

test_that("generate_markdown_report creates markdown output", {
  result <- structure(
    list(
      statistics = data.frame(
        variable = "age",
        n = 10,
        mean = 35,
        sd = 7.07
      ),
      insights = c("Sample size is adequate", "Data shows normal distribution")
    ),
    class = "descriptR_result"
  )

  md_output <- generate_markdown_report(result, title = "Test Report")

  expect_true(is.character(md_output))
  expect_true(grepl("# Test Report", md_output, fixed = TRUE))
  expect_true(grepl("Sample size is adequate", md_output))
})

test_that("markdown report includes metadata", {
  result <- structure(
    list(
      statistics = data.frame(n = 10),
      metadata = list(
        timestamp = as.POSIXct("2025-01-01 12:00:00"),
        author = "Test Author"
      )
    ),
    class = "descriptR_result"
  )

  md_output <- generate_markdown_report(result, author = "Test Author")

  expect_true(grepl("Test Author", md_output))
  expect_true(grepl("Generated", md_output))
})

test_that("markdown report formats tables correctly", {
  result <- structure(
    list(
      statistics = data.frame(
        var = c("x", "y"),
        value = c(1.234, 5.678)
      )
    ),
    class = "descriptR_result"
  )

  md_output <- generate_markdown_report(result)

  # Should have table separators
  expect_true(grepl("\\|", md_output))
  expect_true(grepl("---", md_output))
})

# HTML Generation =============================================================

test_that("generate_html_report creates HTML output", {
  result <- structure(
    list(
      statistics = data.frame(
        variable = "age",
        n = 10,
        mean = 35
      ),
      insights = c("Test insight")
    ),
    class = "descriptR_result"
  )

  html_output <- generate_html_report(
    result,
    title = "Test Report",
    template = "default"
  )

  expect_true(is.character(html_output))
  expect_true(grepl("<html>", html_output, fixed = TRUE))
  expect_true(grepl("</html>", html_output, fixed = TRUE))
  expect_true(grepl("Test Report", html_output))
})

test_that("HTML report includes CSS styling", {
  result <- structure(
    list(statistics = data.frame(n = 10)),
    class = "descriptR_result"
  )

  html_output <- generate_html_report(result, template = "default")

  expect_true(grepl("<style>", html_output, fixed = TRUE))
  expect_true(grepl("</style>", html_output, fixed = TRUE))
})

test_that("HTML report handles different templates", {
  result <- structure(
    list(statistics = data.frame(n = 10)),
    class = "descriptR_result"
  )

  templates <- c("default", "apa", "nature", "science", "nejm")

  for (template in templates) {
    html_output <- generate_html_report(result, template = template)
    expect_true(grepl("<html>", html_output, fixed = TRUE))
  }
})

test_that("HTML report formats tables", {
  result <- structure(
    list(
      statistics = data.frame(
        variable = c("x", "y"),
        mean = c(10, 20),
        sd = c(2, 3)
      )
    ),
    class = "descriptR_result"
  )

  html_output <- generate_html_report(result)

  expect_true(grepl("<table>", html_output, fixed = TRUE))
  expect_true(grepl("</table>", html_output, fixed = TRUE))
  expect_true(grepl("<th>", html_output, fixed = TRUE))
  expect_true(grepl("<td>", html_output, fixed = TRUE))
})

test_that("HTML report includes insights section", {
  result <- structure(
    list(
      statistics = data.frame(n = 10),
      insights = c("Insight 1", "Insight 2")
    ),
    class = "descriptR_result"
  )

  html_output <- generate_html_report(result)

  expect_true(grepl("Key Insights", html_output))
  expect_true(grepl("Insight 1", html_output))
  expect_true(grepl("Insight 2", html_output))
})

# Template CSS ================================================================

test_that("get_template_css returns CSS for all templates", {
  templates <- c("default", "apa", "nature", "science", "nejm")

  for (template in templates) {
    css <- get_template_css(template)
    expect_true(is.character(css))
    expect_true(nchar(css) > 0)
  }
})

test_that("template CSS includes base styles", {
  css <- get_template_css("default")

  # Should include basic styling elements
  expect_true(grepl("body", css))
  expect_true(grepl("table", css))
})

test_that("different templates produce different CSS", {
  css_default <- get_template_css("default")
  css_apa <- get_template_css("apa")
  css_nature <- get_template_css("nature")

  # They should be different
  expect_false(identical(css_default, css_apa))
  expect_false(identical(css_apa, css_nature))
})

# Excel Export ================================================================

test_that("quick_export_excel requires data", {
  expect_error(
    quick_export_excel(NULL, "test.xlsx"),
    "data cannot be NULL"
  )
})

test_that("quick_export_excel requires filename", {
  expect_error(
    quick_export_excel(test_data, NULL),
    "filename is required"
  )
})

test_that("quick_export_excel validates sheet name", {
  # Sheet names have length limits
  long_name <- paste(rep("a", 100), collapse = "")

  expect_error(
    quick_export_excel(test_data, "test.xlsx", sheet_name = long_name),
    "Sheet name too long"
  )
})

test_that("quick_export_excel works with data frame", {
  skip_if_not_installed("openxlsx")

  temp_file <- tempfile(fileext = ".xlsx")

  result <- quick_export_excel(
    test_data,
    temp_file,
    sheet_name = "TestData",
    open_file = FALSE
  )

  expect_true(file.exists(temp_file))

  # Cleanup
  unlink(temp_file)
})

# Word Export =================================================================

test_that("generate_word_report requires officer package", {
  skip_if_installed("officer")

  result <- structure(
    list(statistics = data.frame(n = 10)),
    class = "descriptR_result"
  )

  expect_error(
    generate_word_report(result, title = "Test"),
    "officer.*required|not installed"
  )
})

test_that("generate_word_report creates document object", {
  skip_if_not_installed("officer")

  result <- structure(
    list(
      statistics = data.frame(
        variable = "age",
        mean = 35,
        sd = 7
      ),
      insights = c("Test insight")
    ),
    class = "descriptR_result"
  )

  doc <- generate_word_report(result, title = "Test Report")

  expect_s3_class(doc, "rdocx")
})

# Excel Report ================================================================

test_that("generate_excel_report requires openxlsx package", {
  skip_if_installed("openxlsx")

  result <- structure(
    list(statistics = data.frame(n = 10)),
    class = "descriptR_result"
  )

  expect_error(
    generate_excel_report(result, title = "Test"),
    "openxlsx.*required|not installed"
  )
})

test_that("generate_excel_report creates workbook object", {
  skip_if_not_installed("openxlsx")

  result <- structure(
    list(
      statistics = data.frame(
        variable = "age",
        mean = 35,
        sd = 7
      ),
      insights = c("Test insight")
    ),
    class = "descriptR_result"
  )

  wb <- generate_excel_report(result, title = "Test Report")

  expect_s4_class(wb, "Workbook")
})

test_that("generate_excel_report creates multiple sheets", {
  skip_if_not_installed("openxlsx")

  result <- structure(
    list(
      statistics = data.frame(variable = "age", mean = 35),
      insights = c("Test insight"),
      metadata = list(timestamp = Sys.time())
    ),
    class = "descriptR_result"
  )

  wb <- generate_excel_report(result, title = "Test Report")

  sheet_names <- names(wb)

  expect_true("Metadata" %in% sheet_names)
  expect_true("Statistics" %in% sheet_names)
})

# Integration Tests ===========================================================

test_that("full report generation workflow works for markdown", {
  result <- structure(
    list(
      statistics = data.frame(
        variable = c("age", "score"),
        n = c(10, 10),
        mean = c(35, 88),
        sd = c(7.07, 3.16)
      ),
      insights = c(
        "Sample size is adequate for both variables",
        "Age shows moderate variability",
        "Score shows low variability"
      ),
      metadata = list(
        timestamp = Sys.time(),
        package_version = "0.1.0"
      )
    ),
    class = "descriptR_result"
  )

  temp_file <- tempfile(fileext = ".md")

  generate_report(
    result,
    temp_file,
    format = "markdown",
    title = "Integration Test Report",
    author = "Test Suite",
    open_file = FALSE
  )

  expect_true(file.exists(temp_file))

  # Read and verify content
  content <- readLines(temp_file)
  full_content <- paste(content, collapse = "\n")

  expect_true(grepl("Integration Test Report", full_content))
  expect_true(grepl("Test Suite", full_content))
  expect_true(grepl("Sample size is adequate", full_content))

  # Cleanup
  unlink(temp_file)
})

test_that("full report generation workflow works for HTML", {
  result <- structure(
    list(
      statistics = data.frame(
        variable = c("age", "score"),
        n = c(10, 10),
        mean = c(35, 88)
      ),
      insights = c("Test insight")
    ),
    class = "descriptR_result"
  )

  temp_file <- tempfile(fileext = ".html")

  generate_report(
    result,
    temp_file,
    format = "html",
    template = "apa",
    title = "HTML Test",
    open_file = FALSE
  )

  expect_true(file.exists(temp_file))

  # Read and verify
  content <- readLines(temp_file)
  full_content <- paste(content, collapse = "\n")

  expect_true(grepl("<html>", full_content, fixed = TRUE))
  expect_true(grepl("HTML Test", full_content))

  # Cleanup
  unlink(temp_file)
})

test_that("report generation handles different result types", {
  # Test with descriptR_grouped
  grouped_result <- structure(
    list(
      by_group = data.frame(
        group = c("A", "B"),
        n = c(5, 5),
        mean = c(30, 40)
      )
    ),
    class = "descriptR_grouped"
  )

  temp_file <- tempfile(fileext = ".md")

  expect_no_error(
    generate_report(grouped_result, temp_file, format = "markdown", open_file = FALSE)
  )

  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

test_that("report generation preserves numeric precision", {
  result <- structure(
    list(
      statistics = data.frame(
        variable = "x",
        mean = 1.23456789,
        sd = 0.987654321
      )
    ),
    class = "descriptR_result"
  )

  temp_file <- tempfile(fileext = ".md")

  generate_report(result, temp_file, format = "markdown", open_file = FALSE)

  content <- paste(readLines(temp_file), collapse = "\n")

  # Should format to reasonable precision
  expect_true(grepl("1\\.23", content))
  expect_true(grepl("0\\.99", content))

  unlink(temp_file)
})

# Error Handling ==============================================================

test_that("report generation handles empty statistics gracefully", {
  result <- structure(
    list(statistics = data.frame()),
    class = "descriptR_result"
  )

  temp_file <- tempfile(fileext = ".md")

  # Should warn but not error
  expect_warning(
    generate_report(result, temp_file, format = "markdown", open_file = FALSE),
    "No statistics|empty"
  )
})

test_that("report generation handles missing insights", {
  result <- structure(
    list(
      statistics = data.frame(variable = "x", n = 10),
      insights = NULL
    ),
    class = "descriptR_result"
  )

  temp_file <- tempfile(fileext = ".md")

  expect_no_error(
    generate_report(result, temp_file, format = "markdown", open_file = FALSE)
  )

  unlink(temp_file)
})

test_that("report generation validates file extensions", {
  result <- structure(
    list(statistics = data.frame(n = 10)),
    class = "descriptR_result"
  )

  # Should warn if extension doesn't match format
  expect_warning(
    generate_report(result, "test.txt", format = "html", open_file = FALSE),
    "extension|mismatch"
  )

  # Cleanup if file was created
  if (file.exists("test.txt")) unlink("test.txt")
})

# Multiple Format Export ======================================================

test_that("generate_report can export all formats", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("officer")

  result <- structure(
    list(
      statistics = data.frame(variable = "age", mean = 35),
      insights = c("Test insight")
    ),
    class = "descriptR_result"
  )

  temp_base <- tempfile()

  generate_report(
    result,
    temp_base,
    format = "all",
    title = "Multi-Format Test",
    open_file = FALSE
  )

  # Check that files were created
  expect_true(file.exists(paste0(temp_base, ".html")))
  expect_true(file.exists(paste0(temp_base, ".md")))
  expect_true(file.exists(paste0(temp_base, ".docx")))
  expect_true(file.exists(paste0(temp_base, ".xlsx")))

  # Cleanup
  unlink(paste0(temp_base, ".html"))
  unlink(paste0(temp_base, ".md"))
  unlink(paste0(temp_base, ".docx"))
  unlink(paste0(temp_base, ".xlsx"))
})
