#' Access the bundled example datasets
#'
#' @description
#' Convenience accessor for the example data files shipped with the package
#' (used throughout the rfriend course). With no arguments it lists the available
#' files. Given a file name it returns the full path to the installed file,
#' ready to pass to a reader such as \code{\link[utils]{read.csv}},
#' \code{\link[readxl]{read_excel}} or \code{\link{f_open_file}}. Optionally it
#' copies the file to a destination so learners can practise importing a file
#' that sits in their own working directory.
#'
#' @param file Character string: the name of an example file (e.g.
#'   \code{"plant_trial_raw.csv"}). If \code{NULL} (the default) the function
#'   returns the names of all available example files.
#' @param dest Optional path. If supplied, the example file is copied there and
#'   the path of the copy is returned (invisibly). \code{dest} is a folder when
#'   it is an existing folder, ends with \code{"/"}, or has no file extension
#'   (such as \code{"data"}): the folder is created if needed (with a message)
#'   and the original file name is kept. Otherwise \code{dest} is the full
#'   target file path (such as \code{"data/my_copy.csv"}); its folder must
#'   exist. The working directory is never changed.
#' @param overwrite Logical; passed to \code{\link[base]{file.copy}} when
#'   \code{dest} is supplied. Default \code{FALSE}.
#'
#' @return
#' If \code{file} is \code{NULL}, a character vector of available file names.
#' If \code{file} is supplied and \code{dest} is \code{NULL}, the full path to
#' the installed example file. If \code{dest} is supplied, the path to the
#' copied file (returned invisibly).
#'
#' @details
#' The files live in the package's \code{extdata} directory and are located
#' with \code{\link[base]{system.file}}. Because \code{system.file()} returns
#' an empty string for a missing file, this function checks explicitly and
#' fails with an informative error listing the valid names, rather than letting
#' a later reader fail on an empty path.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # List the available example files
#' f_example_data()
#'
#' # Get the path to one of them, then read it
#' path <- f_example_data("plant_trial_raw.csv")
#' df <- read.csv(path)
#'
#' \dontrun{
#' # Copy a file into the current working directory to import it "from disk"
#' f_example_data("plant_trial_raw.xlsx", dest = getwd())
#'
#' # Copy it into a subfolder "data" (created if it does not exist yet)
#' f_example_data("plant_trial_raw.csv", dest = "data")
#'
#' # Open an example file in its default application
#' f_open_file(f_example_data("plant_trial_raw.xlsx"))
#' }
#'
#' @export
f_example_data <- function(file = NULL, dest = NULL, overwrite = FALSE) {

  extdata_dir <- system.file("extdata", package = "rfriend")
  if (identical(extdata_dir, "")) {
    stop("Could not find the 'extdata' directory in package 'rfriend'. ",
         "Is the package installed correctly?")
  }

  available <- list.files(extdata_dir)

  # No file requested: list what is available.
  if (is.null(file)) {
    return(available)
  }

  if (!is.character(file) || length(file) != 1L) {
    stop("'file' must be a single file name, or NULL to list available files.")
  }

  # Resolve and verify the requested file (system.file() returns "" if absent).
  path <- system.file("extdata", file, package = "rfriend")
  if (identical(path, "")) {
    stop("Example file '", file, "' was not found.\nAvailable files:\n  ",
         paste(available, collapse = "\n  "))
  }

  # Just return the path unless a destination was given.
  if (is.null(dest)) {
    return(path)
  }

  if (!is.character(dest) || length(dest) != 1L || is.na(dest) || !nzchar(dest)) {
    stop("'dest' must be a single folder or file path.")
  }

  # Copy mode. dest is a folder when it exists as one, ends with a slash, or
  # has no file extension ("data"): create it if needed and keep the original
  # file name. Otherwise dest is the full target file path, and its folder
  # must exist (a missing folder would otherwise give a vague copy error).
  is_folder <- dir.exists(dest) || grepl("[/\\\\]$", dest) ||
    !grepl("\\.[A-Za-z0-9]+$", basename(dest))
  if (is_folder) {
    folder <- sub("[/\\\\]+$", "", dest)
    if (!nzchar(folder)) folder <- dest
    if (!dir.exists(folder)) {
      if (!dir.create(folder, recursive = TRUE)) {
        stop("Could not create the folder '", folder, "'. Is the location writable?")
      }
      message("Created folder '", folder, "'.")
    }
    target <- file.path(folder, basename(path))
  } else {
    folder <- dirname(dest)
    if (!dir.exists(folder)) {
      stop("The folder '", folder, "' does not exist. Create it first with ",
           "dir.create(\"", folder, "\"), or give only a folder as 'dest'.")
    }
    target <- dest
  }

  ok <- file.copy(path, target, overwrite = overwrite)
  if (!ok) {
    stop("Could not copy example file to '", target, "'. ",
         "Does it already exist (use overwrite = TRUE) ",
         "or is the destination not writable?")
  }
  message("Copied '", file, "' to: ", target)
  invisible(target)
}
