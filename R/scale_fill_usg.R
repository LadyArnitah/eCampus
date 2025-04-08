#' Custom Fill Scale for USG eCampus Palettes
#'
#' This function applies a custom fill scale using predefined color palettes from `ecampus_palettes`.
#' It supports both discrete and continuous data, automatically determining the appropriate scale.
#'
#' @param branch The branch name. Default is "USG-eCore".
#' @param type The type of palette. Default is "Official Colors".
#' @param data The data to be used for mapping colors. Can be factor, character, or numeric.
#' @param reverse Logical, should the color order be reversed? Default is FALSE.
#' @param ... Additional arguments passed to `scale_fill_manual` or `scale_fill_gradientn`.
#' @importFrom ggplot2 scale_fill_manual scale_fill_gradientn
#' @importFrom stats setNames
#' @return A ggplot2 scale object, either `scale_fill_manual()` (for discrete data) or `scale_fill_gradientn()` (for continuous data).
#'
#' @examples
#' \dontrun{
#' # Example for discrete data
#' df_discrete <- data.frame(
#'   category = c("A", "B", "C", "D"),
#'   value = c(10, 15, 7, 12)
#' )
#' ggplot(df_discrete, aes(x = category, y = value, fill = category)) +
#'   geom_bar(stat = "identity") +
#'   scale_fill_usg(branch = "USG-eCore", type = "Official Colors", data = df_discrete$category) +
#'   theme_minimal()
#'
#' # Example for continuous data
#' df_continuous <- data.frame(
#'   x = 1:10,
#'   y = 1:10,
#'   z = rnorm(10)
#' )
#' ggplot(df_continuous, aes(x = x, y = y, fill = z)) +
#'   geom_tile() +
#'   scale_fill_usg(branch = "USG-eCore", type = "Official Colors", data = df_continuous$z) +
#'   theme_minimal()
#' }
#'
#' @export
scale_fill_usg <- function(branch = "USG-eCore", type = "Official Colors", data, reverse = FALSE, ...) {
  # Check if palette exists
  if (!exists("ecampus_palettes", envir = .GlobalEnv)) {
    stop("The 'ecampus_palettes' object is not found.")
  }

  # Validate inputs
  if (!branch %in% names(ecampus_palettes)) {
    stop(paste("Branch '", branch, "' not found in ecampus_palettes.", sep = ""))
  }
  if (!type %in% names(ecampus_palettes[[branch]])) {
    stop(paste("Type '", type, "' not found for branch '", branch, "'", sep = ""))
  }

  # Retrieve and reverse colors if needed
  colors <- ecampus_palettes[[branch]][[type]]
  if (reverse) colors <- rev(colors)

  # Determine if the data is discrete or continuous
  if (is.factor(data) || is.character(data)) {
    # Use a discrete scale
    return(scale_fill_manual(values = unname(colors), ...))
  } else if (is.numeric(data)) {
    # Use a continuous scale
    return(scale_fill_gradientn(colors = colors, ...))
  } else {
    stop("Unsupported data type: data must be factor, character, or numeric")
  }
}
