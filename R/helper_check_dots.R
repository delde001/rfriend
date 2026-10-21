#' Fail loud on unknown arguments passed through \code{...}
#'
#' Internal helper. Compares the names supplied in \code{...} against the
#' names a function legitimately reads from its dots (for example
#' \code{subset}, \code{na.action}, or internal backchannels such as
#' \code{internal_data_name}). Any other argument is a typo or an
#' unsupported option and raises an error, with a "did you mean"
#' suggestion when a close match exists among the function's formal
#' arguments or the allowed dot names.
#'
#' Uses \code{...names()}, which reads argument names without evaluating
#' them, so it works whether the caller later captures the dots with
#' \code{list(...)}, inspects them via \code{match.call()}, or forwards
#' them. (\code{rlang::check_dots_used()} cannot be used in those
#' functions: \code{list(...)} marks every dot as used, and
#' \code{subset} read via \code{match.call()} is never marked as used.)
#'
#' @param dot_names Result of \code{...names()} in the calling function.
#' @param n_dots Result of \code{...length()} in the calling function.
#'   Needed because \code{...names()} returns \code{NULL} when no dot is
#'   named, which would otherwise hide stray positional arguments.
#' @param allowed Character vector of names the caller reads from its dots.
#' @param fun_name Name of the user-facing function, used in the message.
#' @param valid_args Formal argument names of the calling function, used
#'   for suggestions. Typically \code{names(formals(sys.function()))}.
#' @return \code{invisible(NULL)} when all dot names are allowed;
#'   otherwise an error.
#' @keywords internal
#' @noRd
check_known_dots <- function(dot_names, n_dots, allowed, fun_name,
                             valid_args = character(0)) {
  if (n_dots == 0L) {
    return(invisible(NULL))
  }
  if (is.null(dot_names)) {
    dot_names <- rep("", n_dots)
  }
  dot_names[is.na(dot_names)] <- ""
  unknown <- dot_names[!(dot_names %in% allowed)]
  if (length(unknown) == 0L) {
    return(invisible(NULL))
  }

  # Only suggest public names: drop "..." and internal backchannels.
  candidates <- unique(c(setdiff(valid_args, "..."), allowed))
  candidates <- candidates[!grepl("^(\\.|internal_)", candidates)]

  label <- vapply(unknown, function(u) {
    if (!nzchar(u)) {
      return("an unnamed argument")
    }
    # Closest candidate by edit distance; suggest only if reasonably close
    # (at most 2 edits, or 30 percent of the name for longer names).
    d <- utils::adist(u, candidates, ignore.case = TRUE)[1, ]
    best <- which.min(d)
    if (length(best) == 1L && d[best] <= max(2L, ceiling(0.3 * nchar(u)))) {
      paste0("'", u, "' (did you mean '", candidates[best], "'?)")
    } else {
      paste0("'", u, "'")
    }
  }, character(1), USE.NAMES = FALSE)

  stop("Unknown argument(s) in ", fun_name, "(): ",
       paste(label, collapse = ", "), ".\n",
       "  These would otherwise be silently ignored. See ?", fun_name,
       " for the valid arguments.",
       call. = FALSE)
}
