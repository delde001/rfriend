# Function to rename terms in a formula
rename_formula_terms <- function(formula, name_map) {
  # Convert formula to character representation
  formula_str <- deparse(formula)

  # Replace terms in the formula string using the name map
  for (old_name in names(name_map)) {
    new_name    <- name_map[[old_name]]

    # Check if the name is not a valid syntactic name
    if (new_name != make.names(new_name)) {
      new_name_with_backticks <- paste0("`", new_name, "`")
    } else {
      new_name_with_backticks <- new_name
    }

    formula_str <- gsub(paste0("\\b", old_name, "\\b"), new_name_with_backticks, formula_str)
  }

  # Convert back to a formula
  as.formula(formula_str)

}
