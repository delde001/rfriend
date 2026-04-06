#' Convert a data frame to a contingency table
#'
#' @param df A data frame where one column contains row labels and the rest are numeric.
#' @param label_col Index or name of the column containing row labels. If NULL (default),
#'   the function auto-detects the first character/factor column.
#' @return A contingency table.
#' @export

df_to_table <- function(df, label_col = NULL) {

  if (is.null(label_col)) {
    label_col <- which(sapply(df, function(col) is.character(col) || is.factor(col)))[1]
    if (is.na(label_col)) stop("No character or factor column found to use as row labels.")
    message("Auto-detected column '", names(df)[label_col], "' as the label column.\nIf not correct, use label_col = to pick the correct column.")
  }

  labels <- df[[label_col]]
  mat    <- as.matrix(df[, -label_col, drop = FALSE])
  rownames(mat) <- labels
  as.table(mat)
}
