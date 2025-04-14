#' eCampus Thematic ggplot2 Theme
#'
#' This function defines a custom `ggplot2` theme with a clean and structured appearance,
#' designed for eCampus-related visualizations. It builds on `theme_minimal` and applies
#' specific color and text settings for a polished, professional look.
#'
#' @param base_size Numeric. Base font size for text elements. Default is 14.
#' @param base_family Character. Base font family for text elements. Default is "Arial".
#' @importFrom ggplot2 theme_minimal theme element_text element_rect element_line
#' @return A `ggplot2` theme object with customized elements including titles, axis text,
#' legend styles, and background settings.
#'
#' @details The theme applies:
#' - A centered, bold plot title in deep blue (`#003366`)
#' - Italicized axis titles in deep blue (`#003366`)
#' - Black axis text
#' - A white plot background with no grid lines
#' - A neatly formatted legend with a white background, positioned at the bottom
#' - Well-defined facet labels (strip text)
#' - Black axis lines and panel borders
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   eCampus_theme()
#'
#' @export

eCampus_theme <- function(base_size = 14, base_family = "Arial") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Plot title
      plot.title = ggplot2::element_text(size = 16, face = "bold", color = "#003366", hjust = 0.5),

      # Axis titles
      axis.title = ggplot2::element_text(size = 12, face = "italic", color = "#003366"),

      # Axis text
      axis.text = ggplot2::element_text(size = 10, color = "#000000"),

      # Remove panel background and grid lines
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # Legend
      legend.title = ggplot2::element_text(size = 12, face = "bold", color = "#003366"),
      legend.text = ggplot2::element_text(size = 10, color = "#000000"),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.position = "bottom",

      # Strip text (facet labels)
      strip.text = ggplot2::element_text(size = 12, face = "bold", color = "#003366"),

      # Axis lines
      axis.line = ggplot2::element_line(color = "#000000", size = 0.5),

      # Remove panel border
      panel.border = ggplot2::element_blank(),

      # White plot background
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", color = NA),

      # Complete theme
      complete = TRUE
    )
}
