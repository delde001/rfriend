#' Convert a data frame to a contingency table
#'
#' @param df A data frame. Either (a) one column contains row labels and the rest are
#'   numeric, (b) a fully numeric data frame with meaningful \code{rownames()} set, or
#'   (c) a fully numeric data frame with no row labels at all (in which case the data
#'   frame is coerced directly to a table without row labels).
#' @param label_col Index or name of the column containing row labels. If NULL (default),
#'   the function auto-detects the first character/factor column. If no such column is
#'   found, the function falls back to using \code{rownames(df)} when these are
#'   meaningful, otherwise it coerces the data frame to a table as-is.
#' @return A contingency table.
#' @export

df_to_table <- function(df, label_col = NULL) {

  if (is.null(label_col)) {
    label_col <- which(sapply(df, function(col) is.character(col) || is.factor(col)))[1]

    if (is.na(label_col)) {
      # No character/factor column: try to fall back on rownames().
      rn <- rownames(df)
      default_rn <- is.null(rn) || identical(rn, as.character(seq_len(nrow(df))))

      mat <- as.matrix(df)

      if (!default_rn) {
        rownames(mat) <- rn
        message("Auto-detected rownames(df) as the label source.\n",
                "If not correct, use label_col = to pick the correct column.")
      } else {
        message("No label column or rownames() detected; ",
                "coerced data frame to a table as-is.\n",
                "If not correct, use label_col = to pick the correct column.")
      }

      return(as.table(mat))
    }

    message("Auto-detected column '", names(df)[label_col], "' as the label column.\n",
            "If not correct, use label_col = to pick the correct column.")
  }

  # Allow label_col to be given as a column NAME as well as an index.
  if (is.character(label_col)) {
    idx <- match(label_col, names(df))
    if (is.na(idx))
      stop("label_col '", label_col, "' not found in the data frame.")
    label_col <- idx
  }

  labels <- df[[label_col]]
  mat    <- as.matrix(df[, -label_col, drop = FALSE])
  rownames(mat) <- labels
  as.table(mat)
}
