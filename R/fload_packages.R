#' Install and Load Multiple R Packages
#'
#' Checks if the specified packages are installed. If not, it installs them and then loads them into the global R session.
#'
#' @param ... Unquoted or quoted names of packages to be installed and loaded. These should be valid package names available on CRAN.
#'
#' @details The function takes a list or vector indicating package names, installs any that are missing, and loads all specified packages into the global environment of the R session. It uses \code{requireNamespace()} to check for installation and \code{library()} to load the packages.
#'
#' @return None. The function is called for its side effects of installing and loading packages.
#'
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#'
#' @export
# Load multiple packages
f_load_packages <- function(...) {
  packages <- as.character(match.call(expand.dots = TRUE)[-1])
  for(package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}

