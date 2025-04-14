#' Custom Color Scale for USG eCore
#'
#' This function applies a custom color palette to ggplot2 plots based on the `ecampus_palettes` object.
#' It supports both discrete and continuous data and allows for reversing the color order.
#'
#' @param branch A character string specifying the branch of the `ecampus_palettes` to use. Default is `"USG-eCore"`.
#' @param type A character string specifying the type of colors to use (e.g., "Official Colors"). Default is `"Official Colors"`.
#' @param reverse A logical value indicating whether to reverse the color order. Default is `FALSE`.
#' @param discrete A logical value indicating whether the data is discrete or continuous. Default is `TRUE`.
#' @param ... Additional arguments passed to `ggplot2::scale_fill_manual` for discrete data or `ggplot2::scale_fill_gradientn` for continuous data.
#'
#' @return A ggplot2 scale object to be used with the `fill` aesthetic.
#'
#' @details
#' This function checks for the existence of the `ecampus_palettes` object in the global environment.
#' It validates the `branch` and `type` arguments against the keys in `ecampus_palettes`.
#'
#' - If `discrete = TRUE`, it applies a manual color scale using `ggplot2::scale_fill_manual`.
#' - If `discrete = FALSE`, it applies a gradient color scale using `ggplot2::scale_fill_gradientn`.
#'
#' @examples
#' # Define a sample palette
#' ecampus_palettes <- list(
#'   "USG-eCore" = list(
#'     "Official Colors" = c("#FF0000", "#00FF00", "#0000FF")
#'   )
#' )
#'
#' # Example 1: Discrete data
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_usg(branch = "USG-eCore", type = "Official Colors")
#'
#' # Example 2: Continuous data
#' ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, fill = Sepal.Length)) +
#'   geom_tile() +
#'   scale_fill_usg(branch = "USG-eCore", type = "Official Colors", discrete = FALSE)
#'
#' @export

scale_fill_usg <- function(branch = "USG-eCore", type = "Official Colors", reverse = FALSE, discrete = TRUE, ...) {
  # Check if palette exists
  if (!exists("ecampus_palettes", envir = .GlobalEnv)) {
    stop("The 'ecampus_palettes' object is not found. Make sure it is loaded into the environment.")
  }

  # Validate inputs
  if (!branch %in% names(ecampus_palettes)) {
    stop(paste("Branch '", branch, "' not found in 'ecampus_palettes'.", sep = ""))
  }
  if (!type %in% names(ecampus_palettes[[branch]])) {
    stop(paste("Type '", type, "' not found for branch '", branch, "'.", sep = ""))
  }

  # Retrieve and reverse colors if needed
  colors <- ecampus_palettes[[branch]][[type]]
  if (reverse) colors <- rev(colors)

  # Return the appropriate scale
  if (discrete) {
    # Use manual color scale for discrete data
    ggplot2::scale_fill_manual(values = unname(colors), ...)
  } else {
    # Use gradient color scale for continuous data
    ggplot2::scale_fill_gradientn(colors = colors, ...)
  }
}
