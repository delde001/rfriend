formula_extract_clean_vars <- function(formula) {
  process_expr <- function(expr) {
    if (is.name(expr)) {
      return(as.character(expr))
    } else if (is.call(expr)) {
      if (expr[[1]] == "$") {
        return(deparse1(expr))
      } else {
        # Process only arguments, skip operator itself
        return(unlist(lapply(expr[-1], process_expr)))
      }
    }
    character(0)
  }

  unique(c(
    process_expr(formula[[2]]),  # LHS
    process_expr(formula[[3]])   # RHS
  ))
}
