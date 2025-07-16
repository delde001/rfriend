# Convert formula to text and clean using regex
clean_formula <- function(formula) {
  formula_text <- deparse(formula)
  cleaned <- gsub("\\w+\\$", "", formula_text)  # Remove text before $
  output <- as.formula(paste(cleaned, collapse = ""))
  return(output)
}
