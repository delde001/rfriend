#' Open a File with the Default Application
#'
#' Opens a specified file using the default application associated with its file type. It automatically detects the operating system (Windows, Linux, or macOS) and uses the appropriate command to open the file.
#'
#' @param filepath A character string specifying the path to the file to be opened. The path can be absolute or relative.
#'
#' @details
#' - On \bold{Windows}, the \code{f_open_file()} function uses \code{shell.exec()} to open the file.
#' - On \bold{Linux}, it uses \code{xdg-open} via the \code{system()} function.
#' - On \bold{macOS}, it uses \code{open} via the \code{system()} function.
#'
#' If an unsupported operating system is detected, the function will throw a message.
#'
#' @return Does not return a value; it is called for its side effect of opening a file.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # NOTE: The use of "if(interactive())" prevents this example from running
#' # during automated CRAN checks. This is necessary because the example
#' # opens a file, a behavior restricted by CRAN policies for automated
#' # testing.You don't need to use "if(interactive())" in your own scripts.
#'if(interactive()) {
#' # Open a PDF file.
#' f_open_file("example.pdf")
#'
#' # Open an image file.
#' f_open_file("image.png")
#'
#' # Open a text file.
#' f_open_file("document.txt")
#' }
#'
#' @seealso [shell.exec()], [system()]
#'
#' @export


f_open_file <- function(filepath) {
  # Detect operating system
  os <- Sys.info()["sysname"]

  # Use appropriate command based on OS
  if (os == "Windows") {
    shell.exec(filepath)

  } else if (os == "Linux") {
    # 1. use 'env LD_LIBRARY_PATH=' to clear RStudio's custom libraries so they
    #    don't conflict with system apps (like LibreOffice).
    # 2. use wait = FALSE so R doesn't freeze while the file is open.
    # 3. ignore stdout/stderr to keep the R console clean.
    system(paste("env LD_LIBRARY_PATH= xdg-open", shQuote(filepath)),
           wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)

  } else if (os == "Darwin") { # macOS
    system(paste("open", shQuote(filepath)), wait = FALSE)

  } else {
    message("Unsupported operating system, the file will not be opened")
  }
}
