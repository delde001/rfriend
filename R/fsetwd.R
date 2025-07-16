#' Set Working Directory Based on Current File or Specified Path
#'
#' A wrapper around \code{setwd()} that sets the working directory to the location of the currently open file in 'RStudio' if no path is provided. If a path is specified, it sets the working directory to that path instead.
#'
#' @param path A character string specifying the desired working directory. If \code{NULL} (default), the function sets the working directory to the location of the currently open and saved file in 'RStudio'.
#'
#' @details
#' If \code{path} is not provided (\code{NULL}), this function uses the \code{this.path} package to determine the location of the currently open file and sets that as the working directory. The file must be saved for this to work properly.
#'
#' If a valid \code{path} is provided, it directly sets the working directory to that path.
#'
#' @note
#' \itemize{
#' \item The function checks whether the currently open file is saved before setting its location as the working directory.
#' \item If the function is called from an unsaved script or directly from the console, an error will be thrown.
#' }
#'
#' @return None. The function is called for its side effects of changing the working directory.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # NOTE: The use of "if(interactive())" prevents this example from running
#' # during automated CRAN checks. This is necessary because the example
#' # requires to be run from an R script. You don't need to use
#' # "if(interactive())" in your own scripts.
#'if(interactive()) {
#' # Store the current working directory, so we can reset it after the example.
#' current_wd <-  getwd()
#' print(current_wd)
#'
#' # Run this commando from a saved R script file, or R Notebook to set the working
#' # directory to scripts' file location
#' f_setwd()
#'
#' # Restore your current working directory
#' f_setwd(current_wd)
#'}
#'
#'
#' @export
f_setwd <- function(path = NULL){

  # Check if a specific path is provided
  if (is.null(path)) {
    # Getting the path of the currently running script
    script_dir <- this.path::sys.dir()
    if (!is.null(script_dir)) {
      setwd(script_dir)  # Set working directory to the script's location
      cat("Working directory has been set to:", "\n")
      cat(getwd(), "\n")
    } else {
      warning("Could not determine the script's directory. The working directory remains unchanged.")
    }
  } else {
    # Ensure that the file path is a character string before proceeding
    if (!is.character(path) || !dir.exists(path)) {
      stop("Invalid or non-existent directory path provided.")
    }
    setwd(path)
    # Check if the working dir is correct
    cat("Working directory has been set to:", "\n")
    cat(getwd(), "\n")
  }
}

