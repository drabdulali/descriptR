# Test suite for color schemes

# Basic Functionality =========================================================

test_that("get_color_scheme returns colors", {
  colors <- get_color_scheme("medium")

  expect_true(is.character(colors))
  expect_true(length(colors) > 0)
  expect_s3_class(colors, "descriptR_palette")
})

test_that("get_color_scheme works with all schemes", {
  schemes <- c("subtle", "medium", "bold", "vivid")

  for (scheme in schemes) {
    colors <- get_color_scheme(scheme)
    expect_true(length(colors) > 0)
  }
})

test_that("get_color_scheme works with all boldness levels", {
  for (bold in 1:4) {
    colors <- get_color_scheme("medium", boldness = bold)
    expect_true(length(colors) > 0)
  }
})

test_that("get_color_scheme returns requested number of colors", {
  colors_3 <- get_color_scheme("medium", n = 3)
  colors_5 <- get_color_scheme("medium", n = 5)
  colors_10 <- get_color_scheme("medium", n = 10)

  expect_equal(length(colors_3), 3)
  expect_equal(length(colors_5), 5)
  expect_equal(length(colors_10), 10)
})

test_that("get_color_scheme validates boldness", {
  expect_error(
    get_color_scheme("medium", boldness = 0),
    "boldness must be"
  )

  expect_error(
    get_color_scheme("medium", boldness = 5),
    "boldness must be"
  )
})

# Color Format ================================================================

test_that("colors are in hex format", {
  colors <- get_color_scheme("medium")

  # All should start with #
  expect_true(all(grepl("^#", colors)))

  # All should be valid hex colors (# followed by 6 hex digits)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
})

test_that("colors are unique within palette", {
  colors <- get_color_scheme("medium", n = 8)

  # Should have no duplicates (for reasonable n)
  expect_equal(length(colors), length(unique(colors)))
})

# Palette Extension ===========================================================

test_that("palette extends for large n", {
  # Base palette is 8 colors, requesting more should interpolate
  colors_15 <- get_color_scheme("medium", n = 15)

  expect_equal(length(colors_15), 15)
  expect_true(all(grepl("^#", colors_15)))
})

test_that("palette trims for small n", {
  colors_2 <- get_color_scheme("medium", n = 2)

  expect_equal(length(colors_2), 2)
})

# Reverse Option ==============================================================

test_that("reverse option works", {
  colors_normal <- get_color_scheme("medium", n = 5, reverse = FALSE)
  colors_reversed <- get_color_scheme("medium", n = 5, reverse = TRUE)

  # Reversed should be reverse of normal
  expect_equal(colors_reversed, rev(colors_normal))
})

# Scheme Differences ==========================================================

test_that("different schemes produce different colors", {
  subtle <- get_color_scheme("subtle", boldness = 2, n = 5)
  medium <- get_color_scheme("medium", boldness = 2, n = 5)
  bold <- get_color_scheme("bold", boldness = 2, n = 5)
  vivid <- get_color_scheme("vivid", boldness = 2, n = 5)

  # All should be different
  expect_false(identical(subtle, medium))
  expect_false(identical(medium, bold))
  expect_false(identical(bold, vivid))
})

test_that("different boldness levels produce different colors", {
  bold1 <- get_color_scheme("medium", boldness = 1, n = 5)
  bold2 <- get_color_scheme("medium", boldness = 2, n = 5)
  bold3 <- get_color_scheme("medium", boldness = 3, n = 5)
  bold4 <- get_color_scheme("medium", boldness = 4, n = 5)

  # All should be different
  expect_false(identical(bold1, bold2))
  expect_false(identical(bold2, bold3))
  expect_false(identical(bold3, bold4))
})

# Preview Function ============================================================

test_that("preview_color_scheme works", {
  expect_message(
    preview_color_scheme("medium", boldness = 2, n = 5),
    "Color Scheme"
  )
})

test_that("preview_color_scheme returns colors invisibly", {
  colors <- preview_color_scheme("medium", boldness = 2, n = 5)

  expect_true(is.character(colors))
  expect_equal(length(colors), 5)
})

# Scheme Recommendations ======================================================

test_that("get_scheme_for_type returns recommendations", {
  rec <- get_scheme_for_type("continuous", "publication")

  expect_true(!is.null(rec$scheme))
  expect_true(!is.null(rec$boldness))
  expect_true(rec$scheme %in% c("subtle", "medium", "bold", "vivid"))
  expect_true(rec$boldness %in% 1:4)
})

test_that("recommendations vary by context", {
  pub <- get_scheme_for_type("continuous", "publication")
  pres <- get_scheme_for_type("continuous", "presentation")
  web <- get_scheme_for_type("continuous", "web")

  # Should get different recommendations
  expect_false(identical(pub, pres) && identical(pres, web))
})

test_that("recommendations include notes for special types", {
  div <- get_scheme_for_type("diverging", "publication")
  seq <- get_scheme_for_type("sequential", "publication")

  expect_true(!is.null(div$note))
  expect_true(!is.null(seq$note))
})

# Diverging Palettes ==========================================================

test_that("create_diverging_palette works", {
  colors <- create_diverging_palette(n = 11)

  expect_equal(length(colors), 11)
  expect_true(all(grepl("^#", colors)))
})

test_that("diverging palette has middle color", {
  colors <- create_diverging_palette(
    low_color = "#0000FF",
    mid_color = "#FFFFFF",
    high_color = "#FF0000",
    n = 9
  )

  # Middle color should be closest to white
  middle_idx <- ceiling(length(colors) / 2)
  # Check it's light (high RGB values)
  middle_rgb <- col2rgb(colors[middle_idx])
  expect_true(all(middle_rgb > 200))
})

test_that("diverging palette warns for even n", {
  expect_warning(
    create_diverging_palette(n = 10),
    "odd number"
  )
})

# Sequential Palettes =========================================================

test_that("create_sequential_palette works", {
  colors <- create_sequential_palette(n = 9)

  expect_equal(length(colors), 9)
  expect_true(all(grepl("^#", colors)))
})

test_that("sequential palette direction works", {
  light_to_dark <- create_sequential_palette(direction = "light_to_dark", n = 5)
  dark_to_light <- create_sequential_palette(direction = "dark_to_light", n = 5)

  # Should be reverses of each other (approximately)
  # First of one should be similar to last of other
  expect_true(light_to_dark[1] != dark_to_light[1])
  expect_true(light_to_dark[length(light_to_dark)] !=
              dark_to_light[length(dark_to_light)])
})

# Accessibility ===============================================================

test_that("check_color_accessibility works", {
  result <- check_color_accessibility("#000000", "#FFFFFF")

  expect_s3_class(result, "descriptR_accessibility")
  expect_true(!is.null(result$contrast_ratio))
  expect_true(!is.null(result$passes_normal))
  expect_true(!is.null(result$passes_large))
})

test_that("black on white has high contrast", {
  result <- check_color_accessibility("#000000", "#FFFFFF", level = "AAA")

  # Black on white should pass AAA
  expect_true(result$contrast_ratio > 15)
  expect_true(result$passes_normal)
  expect_true(result$passes_large)
})

test_that("similar colors have low contrast", {
  result <- check_color_accessibility("#AAAAAA", "#BBBBBB")

  # Very similar colors should fail
  expect_true(result$contrast_ratio < 2)
  expect_false(result$passes_normal)
})

test_that("accessibility checks both AA and AAA", {
  # Color that passes AA but not AAA
  result_aa <- check_color_accessibility("#767676", "#FFFFFF", level = "AA")
  result_aaa <- check_color_accessibility("#767676", "#FFFFFF", level = "AAA")

  # May pass AA but not AAA
  expect_true(!is.null(result_aa$passes_normal))
  expect_true(!is.null(result_aaa$passes_normal))
})

# Export Functionality ========================================================

test_that("export_color_palette hex format works", {
  colors <- get_color_scheme("medium", n = 5)
  exported <- export_color_palette(colors, format = "hex")

  expect_equal(exported, as.character(colors))
})

test_that("export_color_palette rgb format works", {
  colors <- get_color_scheme("medium", n = 3)
  exported <- export_color_palette(colors, format = "rgb")

  # Should be in rgb() format
  expect_true(all(grepl("^rgb\\(", exported)))
})

test_that("export_color_palette css format works", {
  colors <- get_color_scheme("medium", n = 3)
  exported <- export_color_palette(colors, format = "css", name = "test")

  # Should be CSS format
  expect_true(grepl(":root", exported))
  expect_true(grepl("--test-", exported))
})

test_that("export_color_palette r format works", {
  colors <- get_color_scheme("medium", n = 3)
  exported <- export_color_palette(colors, format = "r", name = "my_palette")

  # Should be R code
  expect_true(grepl("my_palette <-", exported))
  expect_true(grepl("c\\(", exported))
})

# Print Methods ===============================================================

test_that("print method for palette works", {
  colors <- get_color_scheme("medium", boldness = 2, n = 5)

  expect_output(print(colors), "descriptR Color Palette")
  expect_output(print(colors), "Scheme: medium")
  expect_output(print(colors), "Boldness: 2")
})

test_that("print method for accessibility works", {
  result <- check_color_accessibility("#000000", "#FFFFFF")

  expect_output(print(result), "WCAG Accessibility")
  expect_output(print(result), "Contrast Ratio")
})

# Integration Tests ===========================================================

test_that("full color workflow works", {
  # Get recommendation
  rec <- get_scheme_for_type("categorical", "presentation")

  # Get colors based on recommendation
  colors <- get_color_scheme(rec$scheme, rec$boldness, n = 5)

  # Export colors
  exported <- export_color_palette(colors, format = "hex")

  # Check accessibility of first color
  access <- check_color_accessibility(colors[1], "#FFFFFF")

  expect_true(length(colors) == 5)
  expect_true(!is.null(access$contrast_ratio))
})

test_that("palettes work for different use cases", {
  # Publication
  pub_colors <- get_color_scheme("subtle", boldness = 2, n = 3)

  # Presentation
  pres_colors <- get_color_scheme("bold", boldness = 3, n = 5)

  # Web
  web_colors <- get_color_scheme("vivid", boldness = 2, n = 8)

  # All should work
  expect_equal(length(pub_colors), 3)
  expect_equal(length(pres_colors), 5)
  expect_equal(length(web_colors), 8)
})
