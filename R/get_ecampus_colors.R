#' Retrieve Colors from eCampus Palettes
#'
#' This function retrieves color values from the eCampus palettes based on the provided color names.
#' You can pass a single color name or multiple color names as arguments. The function will return
#' the corresponding color values from the `ecampus_palette`.
#'
#' @param ... One or more color names (as strings) from the eCampus palettes.
#'
#' @return A named character vector containing the color values corresponding to the provided color names.
#'
#' @examples
#' # Example 1: Retrieve a single color
#' get_ecampus_colors("eCore_Teal")
#'
#' # Example 2: Retrieve multiple colors
#' get_ecampus_colors("eMajor_Navy", "eCore_Teal", "eCore_Purple")
#'
#' @importFrom stats setNames
#' @export
get_ecampus_colors <- function(...) {
  # Accept both individual names and character vectors
  args <- list(...)
  color_names <- unlist(args, use.names = FALSE)

  # Initialize a vector to store results
  color_values <- character(length(color_names))
  names(color_values) <- color_names

  # Search each name in all ecampus_palettes
  for (i in seq_along(color_names)) {
    found <- FALSE
    for (branch in names(ecampus_palettes)) {
      for (category in names(ecampus_palettes[[branch]])) {
        palette <- ecampus_palettes[[branch]][[category]]
        if (color_names[i] %in% names(palette)) {
          color_values[i] <- palette[[color_names[i]]]
          found <- TRUE
          break
        }
      }
      if (found) break
    }
    if (!found) {
      warning("Color '", color_names[i], "' not found in ecampus_palettes.")
    }
  }

  return(color_values)
}


