#' Tell the user how many rows with missing values were left out
#'
#' Internal helpers. The test and model functions leave out rows with a
#' missing value (NA) in the columns of the formula, as R does. This prints
#' one message that says how many rows were removed and which columns held
#' the NA, so the loss is visible in the console and not only in the report.
#' Functions that build their results inside a report closure whose messages
#' are suppressed collect the notes with `na_removed_note()` and send them
#' after the closure has run.
#'
#' @param before Data frame before the NA rows were removed (after any
#'   `subset`).
#' @param n_after Number of rows left after removing the NA rows.
#' @param vars Character vector of the columns the analysis uses.
#' @param response Optional response name, for functions that remove NA rows
#'   per response instead of once for all responses.
#' @param n_removed Optional number of removed rows; when given it is used
#'   instead of `nrow(before) - n_after` (for example from
#'   `stats::na.action()` of a fitted model).
#'
#' @return `na_removed_note()`: the message text, or NULL when no rows were
#'   removed. `message_na_removed()`: the same text, invisibly, after
#'   sending it as a message.
#' @keywords internal
#' @noRd
na_removed_note <- function(before, n_after = NULL, vars,
                            response = NULL, n_removed = NULL) {
  if (is.null(n_removed)) n_removed <- nrow(before) - n_after
  if (is.null(n_removed) || length(n_removed) != 1L || is.na(n_removed) ||
      n_removed <= 0) {
    return(NULL)
  }
  vars    <- intersect(unique(vars), names(before))
  na_cols <- vars[vapply(vars, function(v) anyNA(before[[v]]), logical(1))]
  msg <- paste0(
    n_removed, if (n_removed == 1) " row" else " rows",
    " with missing values (NA) removed from analysis",
    if (!is.null(response)) paste0(" of ", response) else "",
    if (length(na_cols) > 0)
      paste0(" (NA in: ", paste(na_cols, collapse = ", "), ")") else "",
    "."
  )
  msg
}

#' @rdname na_removed_note
#' @noRd
message_na_removed <- function(...) {
  msg <- na_removed_note(...)
  if (!is.null(msg)) message(msg)
  invisible(msg)
}
