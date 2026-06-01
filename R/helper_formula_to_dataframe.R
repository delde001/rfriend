# create a dataframe based on the vector input in a formula
formula_to_dataframe <- function(formula_obj) {
  # Variable names (including "df$col" forms) parsed from the formula.
  all_variables <- formula_extract_clean_vars(formula_obj)

  # Resolve each name in the formula's own environment. Formulas carry
  # their construction env via environment(formula); this is what
  # stats::model.frame() and other base R formula consumers use. We
  # fall back to parent.frame() in the unlikely case the formula was
  # stripped of its env attribute via as.formula(x, env = NULL).
  env <- environment(formula_obj)
  if (is.null(env)) env <- parent.frame()

  values <- lapply(all_variables, function(var) {
    tryCatch(
      eval(parse(text = var), envir = env),
      error = function(e) {
        stop(
          "Variable '", var, "' from the formula was not found. ",
          "Check spelling, or pass the data explicitly via the ",
          "'data' argument. Original message: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  })

  # Length sanity check. Without this, as.data.frame() would silently
  # recycle a short vector and produce a misleading result.
  lens <- vapply(values, length, integer(1))
  if (length(unique(lens)) > 1L) {
    stop(
      "Formula variables have unequal lengths: ",
      paste0(all_variables, " (", lens, ")", collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  df <- stats::setNames(values, all_variables)
  df <- as.data.frame(df, stringsAsFactors = FALSE)

  # A "df$col" reference becomes "df.col" after as.data.frame(); strip
  # everything before the dot so downstream code sees the plain name.
  names(df) <- sub(".*\\.", "", names(df))

  df
}
