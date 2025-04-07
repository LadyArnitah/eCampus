#' Get ECampus Colors by Name
#'
#' This function retrieves one or more color hex codes from the `ecampus_palettes` dataset.
#'
#' @param ... One or more color names as character strings.
#'
#' @return A named vector of hex codes for the requested colors.
#' @examples
#' get_ecampus_colors(c("eCore_Green", "eCore_Teal"))
#' get_ecampus_colors("eCore_OER_Orange", palette = "USG-eCore")
#' get_ecampus_colors("eMajor_Navy", palette = "all") # Search in all branches
#' @export
get_ecampus_colors <- function(...) {
  # Combine all input color names into a single character vector
  color_names <- unlist(list(...))

  # Initialize a vector to store the results
  color_values <- character(length(color_names))
  names(color_values) <- color_names

  # Iterate over each color name
  for (i in seq_along(color_names)) {
    # Search for the color in ecampus_palettes
    found <- FALSE
    for (branch in names(ecampus_palettes)) {
      for (category in names(ecampus_palettes[[branch]])) {
        if (color_names[i] %in% names(ecampus_palettes[[branch]][[category]])) {
          color_values[i] <- ecampus_palettes[[branch]][[category]][color_names[i]]
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

