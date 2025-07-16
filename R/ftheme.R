#' Apply a black or white 'RStudio' Theme and Zoom Level
#'
#' This comes in hand when teaching, the function allows users to apply a "black" or "white" 'RStudio' theme and adjust the zoom level in the 'RStudio' IDE. It includes error handling for invalid inputs.
#'
#' @param color A character string. The theme color to apply. Must be either \code{"black"} (dark theme) or \code{"white"} (light theme). Default is \code{"black"}.
#' @param zlevel A numeric value. The zoom level to apply, ranging from \code{0} (default size) to \code{4} (maximum zoom). Default is \code{0}.
#'
#' @details
#' The function performs the following actions:
#' \enumerate{
#' \item Applies the specified 'RStudio' theme:
#' \itemize{
#'   \item \code{"black"}: Applies the "Tomorrow Night 80s" dark theme.
#'   \item \code{"white"}: Applies the "Textmate (default)" light theme.
#'   }
#' \item Adjusts the zoom level in 'RStudio':
#' \itemize{
#'   \item \code{zlevel = 0}: Resets to default zoom level.
#'   \item \code{zlevel = 1}: Zooms in once.
#'   \item \code{zlevel = 2}: Zooms in twice.
#'   \item \code{zlevel = 3}: Zooms in three times.
#'   \item \code{zlevel = 4}: Zooms in four times.
#'   }
#'}
#' The function includes error handling to ensure valid inputs:
#'  \itemize{
#' \item \code{color} must be a character string and one of \code{"black"} or \code{"white"}.
#' \item \code{zlevel} must be a numeric value, an integer, and within the range of 0 to 4. If a non-integer is provided, it will be rounded to the nearest integer with a warning.
#' }
#'
#'@return None. The function is called for its side effects of changing the 'RStudio' theme or Zoomlevel.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @return This function does not return a value. It applies changes directly to the 'RStudio' IDE.
#'
#' @examples
#' # NOTE: This example will change your RStudio theme hence the dont run warning.
#' \dontrun{
#' # Apply a dark theme with with zoom level 2:
#' f_theme(color = "black", zlevel = 2)
#'
#' # Apply a black theme with maximum zoom level:
#' f_theme(color = "black", zlevel = 4)
#'
#' # Apply the default light theme default zoom level:
#' f_theme(color = "black", zlevel = 0)
#' }
#'
#' @export
#'
f_theme <- function(color = "black", zlevel = 0) {

# Use tryCatch to catch missing object errors
tryCatch({

  # Check if theme color is character
  if (!is.character(color)) {
    stop("Error: theme color must be a character value either:  \"black\" or \"white\"")
  }

  # Check if zlevel is numeric
  if (!is.numeric(zlevel)) {
    stop("Error: zlevel must be a numeric value!")
  }
  # Additional checks
  if (is.na(zlevel)) {
    stop("Error: zlevel cannot be NA!")
  }
  if (zlevel %% 1 != 0) {
    warning("Error: the zlevel value: ", zlevel, " has been rounded to the nearest Integer: ", round(zlevel),"!")
  }
  # Check for invalid zlevel
  if (zlevel > 4) {
    stop("Error: zlevel cannot be higher than 4!")
  }


  # Reset the zoom function to default to prevent additional zoom
  rstudioapi::executeCommand('zoomActualSize')

  # Match the 'color' argument
  color <- match.arg(color, choices = c("black", "white"))

  # Calculate the statistic
  switch(
    color,
    black = rstudioapi::applyTheme("Tomorrow Night 80s"),
    white = rstudioapi::applyTheme("Textmate (default)")
  )


  #the switch function starts at 1 not at 0
  zlevel <- round(zlevel) + 1

  switch(
    zlevel,
  #set zoom level 0 = default, 1 = zoom 1x, 2 = zoom 2x, 3 = zoom 3x, 4 = zoom 4x
  rstudioapi::executeCommand('zoomActualSize'),
  rstudioapi::executeCommand('zoomIn'),
  c(rstudioapi::executeCommand('zoomIn'),
  rstudioapi::executeCommand('zoomIn')),
  c(rstudioapi::executeCommand('zoomIn'),
    rstudioapi::executeCommand('zoomIn'),
    rstudioapi::executeCommand('zoomIn')),
  c(rstudioapi::executeCommand('zoomIn'),
    rstudioapi::executeCommand('zoomIn'),
    rstudioapi::executeCommand('zoomIn'),
    rstudioapi::executeCommand('zoomIn'))
  )
}, error = function(e) {
  # Handle case where object is not found
  if (grepl("object.*not found", e$message)) {
    cat(crayon::red("Error: Did you mean to use quotes? Try using '\"white\"' or '\"black\"'.\n"))
  } else if (grepl("*zlevel*", e$message)) {
    # Re-throw other errors as-is
    cat(crayon::red(e$message))
  } else {
    stop(e$message)
  }
})
}
