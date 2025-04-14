#' eCampus Color Scale for ggplot2
#'
#' Applies a custom color scale to ggplot2 plots using eCampus palette colors.
#' Supports both discrete and continuous scales depending on the nature of the data.
#'
#' @param branch Character. The eCampus branch to use, such as "USG-eCore", "eMajor", etc.
#' @param type Character. The type of color palette to use (e.g., "Official Colors", "Accent Colors").
#' @param reverse Logical. Whether to reverse the order of the colors. Default is \code{FALSE}.
#' @param continuous Logical. If \code{TRUE}, uses a continuous gradient. If \code{FALSE}, uses a discrete palette. Default is \code{FALSE}.
#' @param ... Additional arguments passed to \code{ggplot2::scale_color_manual()} or \code{ggplot2::scale_color_gradientn()}.
#'
#' @return A ggplot2 scale object that applies the specified eCampus color palette.
#'
#' @examples
#' library(ggplot2)
#'
#' # Discrete example
#' df <- data.frame(
#'   category = rep(c("A", "B", "C", "D"), each = 5),
#'   x = rnorm(20),
#'   y = rnorm(20)
#' )
#' ggplot(df, aes(x = x, y = y, color = category)) +
#'   geom_point(size = 4) +
#'   scale_color_usg(branch = "USG-eCore", type = "Official Colors") +
#'   eCampus_theme()
#'
#' # Continuous example
#' df2 <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   z = rnorm(100)
#' )
#' ggplot(df2, aes(x = x, y = y, color = z)) +
#'   geom_point(size = 3) +
#'   scale_color_usg(branch = "USG-eCore", type = "Official Colors", continuous = TRUE) +
#'   eCampus_theme()
#'
#' @importFrom ggplot2 scale_color_manual scale_color_gradientn
#' @export

scale_color_usg <- function(branch = "USG-eCore", type = "Official Colors", reverse = FALSE,
                            continuous = FALSE, ...) {
  if (!exists("ecampus_palettes")) {
    stop("The 'ecampus_palettes' object is not found.")
  }

  if (!branch %in% names(ecampus_palettes)) {
    stop(paste("Branch", branch, "not found in ecampus_palettes."))
  }

  if (!type %in% names(ecampus_palettes[[branch]])) {
    stop(paste("Type", type, "not found for branch", branch, "in ecampus_palettes."))
  }

  colors <- ecampus_palettes[[branch]][[type]]

  if (reverse) {
    colors <- rev(colors)
  }

  if (continuous) {
    ggplot2::scale_color_gradientn(colors = colors, ...)
  } else {
    colors <- unname(colors)
    ggplot2::scale_color_manual(values = colors, ...)
  }
}
