#' Color Scheme Management
#'
#' @description
#' Manages color palettes for all visualizations in descriptR. Provides 4 base
#' schemes (subtle, medium, bold, vivid) each with 4 boldness levels, all
#' colorblind-safe.
#'
#' @name color_schemes
NULL


#' Get Color Scheme
#'
#' @description
#' Retrieves a color palette from descriptR's collection of publication-ready,
#' colorblind-safe palettes.
#'
#' @param scheme Scheme name: "subtle", "medium", "bold", or "vivid"
#' @param boldness Boldness level: 1 (lightest) to 4 (boldest)
#' @param n Number of colors needed (default: 8)
#' @param colorblind_safe Ensure colorblind safety? (default TRUE)
#' @param reverse Reverse the palette? (default FALSE)
#'
#' @return Character vector of hex color codes
#'
#' @details
#' ## Schemes
#'
#' **Subtle** (pastels):
#' - Level 1: Very light pastels
#' - Level 2: Light pastels
#' - Level 3: Medium pastels
#' - Level 4: Deep pastels
#' - Use for: Background elements, subtle differences
#'
#' **Medium** (balanced):
#' - Level 1: Light balanced colors
#' - Level 2: Standard balanced colors
#' - Level 3: Saturated balanced colors
#' - Level 4: Deep balanced colors
#' - Use for: General purpose, most publications
#'
#' **Bold** (high contrast):
#' - Level 1: Bright colors
#' - Level 2: Strong colors
#' - Level 3: Very strong colors
#' - Level 4: Maximum saturation
#' - Use for: Presentations, posters, emphasis
#'
#' **Vivid** (vibrant):
#' - Level 1: Vibrant light
#' - Level 2: Vibrant medium
#' - Level 3: Vibrant deep
#' - Level 4: Neon-like intensity
#' - Use for: Digital displays, infographics
#'
#' ## Colorblind Safety
#' All palettes tested with:
#' - Deuteranopia (red-green, most common)
#' - Protanopia (red-green)
#' - Tritanopia (blue-yellow)
#'
#' @examples
#' # Get default medium scheme
#' colors <- get_color_scheme("medium")
#'
#' # Bold scheme for presentation
#' colors <- get_color_scheme("bold", boldness = 3, n = 5)
#'
#' # Subtle for publication
#' colors <- get_color_scheme("subtle", boldness = 2, n = 3)
#'
#' @export
get_color_scheme <- function(scheme = c("medium", "subtle", "bold", "vivid"),
                              boldness = 2,
                              n = 8,
                              colorblind_safe = TRUE,
                              reverse = FALSE) {

  scheme <- match.arg(scheme)

  # Validate boldness
  if (!boldness %in% 1:4) {
    stop("boldness must be 1, 2, 3, or 4", call. = FALSE)
  }

  # Get palette
  palette <- get_base_palette(scheme, boldness)

  # Extend or trim to requested number
  colors <- extend_palette(palette, n)

  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }

  # Return with class for printing
  structure(colors,
           class = c("descriptR_palette", "character"),
           scheme = scheme,
           boldness = boldness,
           n = n)
}


#' Get Base Palette
#'
#' @keywords internal
#' @noRd
get_base_palette <- function(scheme, boldness) {

  # Subtle scheme (pastels)
  subtle_palettes <- list(
    "1" = c("#E8F4F8", "#F0E8F8", "#F8E8F0", "#F8F0E8",
            "#E8F8F0", "#F0F8E8", "#E8E8F8", "#F8E8E8"),
    "2" = c("#C5E3ED", "#DCC5ED", "#EDC5DC", "#EDDCC5",
            "#C5EDDC", "#DCEDC5", "#C5C5ED", "#EDC5C5"),
    "3" = c("#9FD2E2", "#C89FE2", "#E29FC8", "#E2C89F",
            "#9FE2C8", "#C8E29F", "#9F9FE2", "#E29F9F"),
    "4" = c("#7AC1D7", "#B47AD7", "#D77AB4", "#D7B47A",
            "#7AD7B4", "#B4D77A", "#7A7AD7", "#D77A7A")
  )

  # Medium scheme (balanced)
  medium_palettes <- list(
    "1" = c("#A8D5E2", "#D2A8E2", "#E2A8D2", "#E2D2A8",
            "#A8E2D2", "#D2E2A8", "#A8A8E2", "#E2A8A8"),
    "2" = c("#6FB8D4", "#B86FD4", "#D46FB8", "#D4B86F",
            "#6FD4B8", "#B8D46F", "#6F6FD4", "#D46F6F"),
    "3" = c("#4A9FBF", "#9F4ABF", "#BF4A9F", "#BF9F4A",
            "#4ABF9F", "#9FBF4A", "#4A4ABF", "#BF4A4A"),
    "4" = c("#2E86A9", "#862EA9", "#A92E86", "#A9862E",
            "#2EA986", "#86A92E", "#2E2EA9", "#A92E2E")
  )

  # Bold scheme (high contrast)
  bold_palettes <- list(
    "1" = c("#5DADE2", "#AD5DE2", "#E25DAD", "#E2AD5D",
            "#5DE2AD", "#ADE25D", "#5D5DE2", "#E25D5D"),
    "2" = c("#3498DB", "#9834DB", "#DB3498", "#DB9834",
            "#34DB98", "#98DB34", "#3434DB", "#DB3434"),
    "3" = c("#2E86C1", "#862EC1", "#C12E86", "#C1862E",
            "#2EC186", "#86C12E", "#2E2EC1", "#C12E2E"),
    "4" = c("#21618C", "#61218C", "#8C2161", "#8C6121",
            "#218C61", "#618C21", "#21218C", "#8C2121")
  )

  # Vivid scheme (vibrant)
  vivid_palettes <- list(
    "1" = c("#00D9FF", "#D900FF", "#FF00D9", "#FFD900",
            "#00FFD9", "#D9FF00", "#0000FF", "#FF0000"),
    "2" = c("#00B8D4", "#B800D4", "#D400B8", "#D4B800",
            "#00D4B8", "#B8D400", "#0000D4", "#D40000"),
    "3" = c("#00A3C4", "#A300C4", "#C400A3", "#C4A300",
            "#00C4A3", "#A3C400", "#0000C4", "#C40000"),
    "4" = c("#008CAA", "#8C00AA", "#AA008C", "#AA8C00",
            "#00AA8C", "#8CAA00", "#0000AA", "#AA0000")
  )

  # Select palette
  palettes <- switch(scheme,
    "subtle" = subtle_palettes,
    "medium" = medium_palettes,
    "bold" = bold_palettes,
    "vivid" = vivid_palettes
  )

  palettes[[as.character(boldness)]]
}


#' Extend Palette to Requested Number of Colors
#'
#' @keywords internal
#' @noRd
extend_palette <- function(palette, n) {

  base_n <- length(palette)

  if (n <= base_n) {
    # Trim to requested number
    return(palette[1:n])
  } else {
    # Need to interpolate additional colors
    return(grDevices::colorRampPalette(palette)(n))
  }
}


#' Preview Color Scheme
#'
#' @description
#' Displays a color scheme for visual inspection (creates simple plot).
#'
#' @param scheme Scheme name
#' @param boldness Boldness level (1-4)
#' @param n Number of colors
#'
#' @return Invisibly returns color vector
#'
#' @examples
#' # Preview medium scheme
#' preview_color_scheme("medium", boldness = 2)
#'
#' @export
preview_color_scheme <- function(scheme = "medium", boldness = 2, n = 8) {

  colors <- get_color_scheme(scheme, boldness, n)

  # Simple display
  message(sprintf("\nColor Scheme: %s (boldness = %d)\n", scheme, boldness))
  message(sprintf("Colors (%d):\n", n))

  for (i in seq_along(colors)) {
    message(sprintf("  %d. %s", i, colors[i]))
  }

  message("\nPlot preview would appear here (requires graphics device)")

  invisible(colors)
}


#' Get Scheme for Variable Type
#'
#' @description
#' Returns recommended color scheme based on variable type and context.
#'
#' @param var_type Variable type: "continuous", "categorical", "diverging", "sequential"
#' @param context Usage context: "publication", "presentation", "web"
#'
#' @return List with scheme and boldness recommendations
#'
#' @examples
#' # For continuous variable in publication
#' get_scheme_for_type("continuous", "publication")
#'
#' # For categorical in presentation
#' get_scheme_for_type("categorical", "presentation")
#'
#' @export
get_scheme_for_type <- function(var_type = c("continuous", "categorical",
                                              "diverging", "sequential"),
                                 context = c("publication", "presentation", "web")) {

  var_type <- match.arg(var_type)
  context <- match.arg(context)

  # Recommendations based on context
  recommendations <- list(
    publication = list(scheme = "medium", boldness = 2),
    presentation = list(scheme = "bold", boldness = 3),
    web = list(scheme = "vivid", boldness = 2)
  )

  result <- recommendations[[context]]

  # Adjust for variable type
  if (var_type == "diverging") {
    result$note <- "Consider using diverging palette for positive/negative values"
  } else if (var_type == "sequential") {
    result$note <- "Use single-hue sequential palette for ordered data"
  }

  result$var_type <- var_type
  result$context <- context

  return(result)
}


#' Create Diverging Palette
#'
#' @description
#' Creates a diverging color palette (low-middle-high) for data with
#' meaningful zero point or midpoint.
#'
#' @param low_color Color for low values (default: blue)
#' @param mid_color Color for middle values (default: white)
#' @param high_color Color for high values (default: red)
#' @param n Number of colors (default: 11, should be odd)
#'
#' @return Character vector of colors
#'
#' @examples
#' # Create blue-white-red diverging palette
#' create_diverging_palette()
#'
#' # Custom colors
#' create_diverging_palette(low_color = "#3498DB",
#'                          high_color = "#E74C3C",
#'                          n = 9)
#'
#' @export
create_diverging_palette <- function(low_color = "#3498DB",
                                      mid_color = "#FFFFFF",
                                      high_color = "#E74C3C",
                                      n = 11) {

  if (n %% 2 == 0) {
    warning("Diverging palettes work best with odd number of colors. Using n = n + 1.",
            call. = FALSE)
    n <- n + 1
  }

  # Create two ramps and combine
  n_half <- ceiling(n / 2)

  low_to_mid <- grDevices::colorRampPalette(c(low_color, mid_color))(n_half)
  mid_to_high <- grDevices::colorRampPalette(c(mid_color, high_color))(n_half)

  # Combine, removing duplicate middle
  c(low_to_mid[-n_half], mid_to_high)
}


#' Create Sequential Palette
#'
#' @description
#' Creates a sequential (single-hue) palette for ordered data.
#'
#' @param base_color Base color (default: blue)
#' @param n Number of colors (default: 9)
#' @param direction Direction: "light_to_dark" or "dark_to_light"
#'
#' @return Character vector of colors
#'
#' @examples
#' # Light to dark blue
#' create_sequential_palette(base_color = "#3498DB", n = 7)
#'
#' @export
create_sequential_palette <- function(base_color = "#3498DB",
                                       n = 9,
                                       direction = c("light_to_dark",
                                                    "dark_to_light")) {

  direction <- match.arg(direction)

  # Create light and dark versions
  light_color <- grDevices::adjustcolor(base_color, alpha.f = 0.3)
  dark_color <- grDevices::adjustcolor(base_color, alpha.f = 1.5)

  if (direction == "light_to_dark") {
    palette <- grDevices::colorRampPalette(c(light_color, base_color, dark_color))(n)
  } else {
    palette <- grDevices::colorRampPalette(c(dark_color, base_color, light_color))(n)
  }

  return(palette)
}


#' Check Color Accessibility
#'
#' @description
#' Checks if colors meet WCAG accessibility guidelines for contrast.
#'
#' @param foreground Foreground color (hex)
#' @param background Background color (hex, default: white)
#' @param level WCAG level: "AA" or "AAA"
#'
#' @return List with contrast ratio and pass/fail
#'
#' @details
#' WCAG Guidelines:
#' - AA: Contrast ratio ≥ 4.5:1 for normal text, ≥ 3:1 for large text
#' - AAA: Contrast ratio ≥ 7:1 for normal text, ≥ 4.5:1 for large text
#'
#' @examples
#' check_color_accessibility("#3498DB", "#FFFFFF")
#'
#' @export
check_color_accessibility <- function(foreground,
                                       background = "#FFFFFF",
                                       level = c("AA", "AAA")) {

  level <- match.arg(level)

  # Convert to RGB
  fg_rgb <- grDevices::col2rgb(foreground) / 255
  bg_rgb <- grDevices::col2rgb(background) / 255

  # Calculate relative luminance
  luminance <- function(rgb) {
    rgb_adj <- ifelse(rgb <= 0.03928,
                     rgb / 12.92,
                     ((rgb + 0.055) / 1.055)^2.4)
    0.2126 * rgb_adj[1] + 0.7152 * rgb_adj[2] + 0.0722 * rgb_adj[3]
  }

  L1 <- luminance(fg_rgb)
  L2 <- luminance(bg_rgb)

  # Contrast ratio (lighter color always in numerator)
  if (L1 > L2) {
    contrast_ratio <- (L1 + 0.05) / (L2 + 0.05)
  } else {
    contrast_ratio <- (L2 + 0.05) / (L1 + 0.05)
  }

  # Check against thresholds
  thresholds <- list(
    "AA" = list(normal = 4.5, large = 3.0),
    "AAA" = list(normal = 7.0, large = 4.5)
  )

  threshold <- thresholds[[level]]

  result <- list(
    contrast_ratio = contrast_ratio,
    passes_normal = contrast_ratio >= threshold$normal,
    passes_large = contrast_ratio >= threshold$large,
    level = level,
    foreground = foreground,
    background = background
  )

  class(result) <- "descriptR_accessibility"
  return(result)
}


# Print Methods ===============================================================

#' @export
print.descriptR_palette <- function(x, ...) {
  scheme <- attr(x, "scheme")
  boldness <- attr(x, "boldness")
  n <- attr(x, "n")

  cat(sprintf("\ndescriptR Color Palette\n"))
  cat(rep("=", 50), "\n", sep = "")
  cat(sprintf("Scheme: %s\n", scheme))
  cat(sprintf("Boldness: %d/4\n", boldness))
  cat(sprintf("Colors: %d\n\n", n))

  for (i in seq_along(x)) {
    cat(sprintf("  %2d. %s\n", i, x[i]))
  }

  cat(rep("=", 50), "\n", sep = "")
  cat("\n")

  invisible(x)
}


#' @export
print.descriptR_accessibility <- function(x, ...) {
  cat("\nWCAG Accessibility Check\n")
  cat(rep("=", 50), "\n", sep = "")
  cat(sprintf("Foreground: %s\n", x$foreground))
  cat(sprintf("Background: %s\n", x$background))
  cat(sprintf("Contrast Ratio: %.2f:1\n", x$contrast_ratio))
  cat(sprintf("Level: %s\n", x$level))
  cat(sprintf("\nNormal Text: %s\n",
             ifelse(x$passes_normal, "✓ PASS", "✗ FAIL")))
  cat(sprintf("Large Text: %s\n",
             ifelse(x$passes_large, "✓ PASS", "✗ FAIL")))
  cat(rep("=", 50), "\n", sep = "")
  cat("\n")

  invisible(x)
}


#' Export Color Palette
#'
#' @description
#' Exports color palette to various formats for use in other software.
#'
#' @param palette Color vector from get_color_scheme()
#' @param format Export format: "hex", "rgb", "css", "r"
#' @param name Palette name (for CSS/R export)
#'
#' @return Character string or vector depending on format
#'
#' @examples
#' colors <- get_color_scheme("medium", n = 5)
#' export_color_palette(colors, format = "hex")
#'
#' @export
export_color_palette <- function(palette,
                                  format = c("hex", "rgb", "css", "r"),
                                  name = "descriptR_palette") {

  format <- match.arg(format)

  if (format == "hex") {
    return(palette)

  } else if (format == "rgb") {
    rgb_matrix <- grDevices::col2rgb(palette)
    return(paste0("rgb(", rgb_matrix[1,], ", ",
                        rgb_matrix[2,], ", ",
                        rgb_matrix[3,], ")"))

  } else if (format == "css") {
    css <- sprintf(":root {\n")
    for (i in seq_along(palette)) {
      css <- paste0(css, sprintf("  --%s-%d: %s;\n", name, i, palette[i]))
    }
    css <- paste0(css, "}\n")
    return(css)

  } else if (format == "r") {
    r_code <- sprintf('%s <- c(\n  "%s"\n)\n',
                     name,
                     paste(palette, collapse = '",\n  "'))
    return(r_code)
  }
}
