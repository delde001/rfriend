formula_extract_df_names <- function(formula) {
  # Extract both sides of the formula
  rhs <- formula[[3]]
  lhs <- formula[[2]]

  # Function to recursively search for $ operators
  find_df_names <- function(expr) {
    if (!is.call(expr)) return(character(0))

    if (length(expr) >= 3 && expr[[1]] == "$") {
      # Found a $ operator, extract the data frame name
      return(as.character(expr[[2]]))
    } else {
      # Recursively search other parts of the expression
      unique(unlist(lapply(expr[-1], function(e) {
        if (is.call(e)) find_df_names(e) else character(0)
      })))
    }
  }

  # Search both sides of the formula
  df_names <- character(0)
  if (is.call(rhs)) df_names <- c(df_names, find_df_names(rhs))
  if (is.call(lhs)) df_names <- c(df_names, find_df_names(lhs))

  # Return unique data frame names
  unique(df_names)
}
