#' Custom Color Scale for eCampus Data
#'
#' This function provides a custom discrete color scale for ggplot2 visualizations,
#' using predefined color palettes stored in `ecampus_palettes`. It allows users to
#' specify a branch, color type, and optionally reverse the color order.
#'
#' @param branch Character. The eCampus branch to retrieve colors from. Default is `"USG-eCore"`.
#' @param type Character. The type of colors to use within the selected branch. Default is `"Official Colors"`.
#' @param reverse Logical. If `TRUE`, the order of colors is reversed. Default is `FALSE`.
#' @param ... Additional arguments passed to `scale_color_manual()`.
#'
#' @return A function that, when applied to a ggplot object, maps categorical variables
#' to colors from the selected eCampus palette.
#'
#' @details This function:
#' - Extracts the specified color set from `ecampus_palettes`.
#' - Assigns colors to unique categorical levels in the data.
#' - Issues a warning if there are more categories than available colors, recycling colors if necessary.
#' - Returns a `scale_color_manual()` function that applies the selected colors.
#'
#' @examples
#' \dontrun{
#' # Example dataset
#' library(ggplot2)
#' df <- data.frame(
#'   category = rep(c("A", "B", "C", "D"), each = 5),
#'   x = rnorm(20),
#'   y = rnorm(20)
#' )
#'
#' # Apply the custom scale to a ggplot
#' ggplot(df, aes(x = x, y = y, color = category)) +
#'   geom_point(size = 4) +
#'   scale_color_usg(branch = "USG-eCore", type = "Official Colors", reverse = TRUE)(df) +
#'   theme_minimal() +
#'   labs(title = "Custom Color Scale Example (Discrete)")
#' }
#'
#' @export
scale_color_usg <- function(branch = "USG-eCore", type = "Official Colors", reverse = FALSE, ...) {
  # Ensure ecampus_palettes exists
  if (!exists("ecampus_palettes")) {
    stop("The 'ecampus_palettes' object is not found.")
  }

  # Validate branch
  if (!branch %in% names(ecampus_palettes)) {
    stop(paste("Branch", branch, "not found in ecampus_palettes."))
  }

  # Validate type
  if (!type %in% names(ecampus_palettes[[branch]])) {
    stop(paste("Type", type, "not found for branch", branch, "in ecampus_palettes."))
  }

  # Retrieve colors
  colors <- ecampus_palettes[[branch]][[type]]

  # Reverse colors if requested
  if (reverse) {
    colors <- rev(colors)
  }

  # Assign names to colors based on the categorical levels in the data
  function_for_mapping <- function(data) {
    unique_categories <- unique(data$category)  # Extract unique categories
    num_categories <- length(unique_categories)

    if (num_categories > length(colors)) {
      warning("Not enough colors for categories, some colors will be reused.")
      colors <- rep(colors, length.out = num_categories)  # Repeat colors if necessary
    }

    # Name the colors according to category levels
    named_colors <- setNames(colors[1:num_categories], unique_categories)

    scale_color_manual(values = named_colors, ...)
  }

  return(function_for_mapping)
}
