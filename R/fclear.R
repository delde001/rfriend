#' f_clear: Clear Various Aspects of the R Environment
#'
#' Provides a convenient way to clear different components of the R environment, including the console, memory, graphics, and more. It also offers the option to restart the R session. This can come in handy at the start of an R script.
#'
#' @param env Logical. If \code{TRUE}, all objects in the global environment are removed. Default is \code{TRUE}.
#' @param gc Logical. If \code{TRUE}, garbage collection is performed to free up memory. Default is \code{TRUE}.
#' @param console Logical. If \code{TRUE}, the R console is cleared. Default is \code{TRUE}.
#' @param graph Logical. If \code{TRUE}, all open graphics devices are closed. Default is \code{TRUE}.
#' @param restart Logical. If \code{TRUE}, the R session is restarted using 'RStudio's' API. Default is \code{FALSE}.
#'
#' @details
#'\itemize{
#'\item Console Clearing: Clears the console output.
#'\item Garbage Collection: Performs garbage collection to free memory from unreferenced objects.
#'\item Graph Clearing: Closes all open graphics devices.
#'\item Environment Clearing: Removes all objects from the global environment.
#'\item Session Restart: Restarts the R session (only available in 'RStudio').
#' }
#'
#' @note The \code{restart} parameter requires 'RStudio' and its API package ('rstudioapi') to be installed and available.
#'
#' @return No return value, called for side effects, see details.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # Clear console, memory, graphs, and for example NOT the environment.
#' f_clear(env = FALSE)
#'
#'
#' @export


f_clear <- function(env = TRUE, gc = TRUE, console = TRUE, graph = TRUE, restart = FALSE) {

    # Console clearing
    if (console) {
      cat("\014")  # Clears console in R
      cat("Cleaned up console...\n")
    }

    # Garbage collection
    if (gc) {
      # Perform garbage collection, i.e. cleaning up unreferenced objects
      gc()
      cat("Freed up memory...\n")
    }

    # Graph generation
    if (graph) {
      # Clearing all open plots.
      graphics.off()
      cat("Closed all graphs...\n")
    }

    # Delete all objects from data environment
    if (env) {
      cat("Deleted all objects from data environment...\n")
      # Remove all objects
      rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
    }

    # Restart session
    if (restart) {
      # Restarting R session
      rstudioapi::restartSession()
      } else {
  message("To also restart R use: f_clear(restart = TRUE)")
    }
  }

