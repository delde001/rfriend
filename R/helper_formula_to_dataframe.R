# create a dataframe based on the vector input in a formula
formula_to_dataframe <- function(formula_obj) {

  # Get all variable names including data frame names
  all_variables <- formula_extract_clean_vars(formula_obj)

  # Dynamically evaluate each variable in the current environment
  df <- stats::setNames(
    lapply(all_variables, function(var) eval(parse(text = var))),
    all_variables
  )

  # Convert the list to a data frame
  df <- as.data.frame(df)

  names(df) <- sub(".*\\.", "", names(df))

  return(df)
}
