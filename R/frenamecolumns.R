#' Rename Specific Columns in a Data Frame
#'
#' Renames specific columns in a data frame based on a named vector (name_map).
#' It ensures that only the specified columns are renamed, while others remain unchanged.
#'
#' @param df A data frame whose columns are to be renamed.
#' @param name_map A named vector where the names correspond to the current column names in \code{df}, and the values are the new names to assign. All names in \code{name_map} must exist in the column names of \code{df}. Yet, not all names in the data.frame have to be in \code{name_map}. This allows for selective renaming of just one or two columns.
#'
#' @return A data frame with updated column names. Columns not specified in \code{name_map} remain unchanged.
#'
#' @details
#' This function is particularly useful when you want to rename only a subset of columns in a
#' data frame. It performs input validation to ensure that:
#' \itemize{
#' \item \code{name_map} is a named vector.
#' \item All names in \code{name_map} exist as column names in \code{df}.
#' }
#' If these conditions are not met, the function will throw an error with an appropriate message.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#'
#' @examples
#' # Create a sample data frame.
#' df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
#'
#' # Define a named vector for renaming specific columns.
#' name_map <- c(a = "alpha", c = "gamma")
#'
#' # Rename columns.
#' df <- f_rename_columns(df, name_map)
#'
#' # View updated data frame.
#' print(df)
#'
#' @export

f_rename_columns <- function(df, name_map) {
  # Check if input is a named vector
  if (is.null(names(name_map))) {
    stop("The name_map argument must be a named vector.")
  }

  # Check if all names in the vector exist in the data frame
  unmatched <- setdiff(names(name_map), colnames(df))

  if (length(unmatched) > 0) {
    stop(paste("The following columns are not found in the data frame:", paste(unmatched, collapse = ", ")))
  }

  # Replace column names based on the named vector
  colnames(df)[colnames(df) %in% names(name_map)] <- name_map[colnames(df)[colnames(df) %in% names(name_map)]]

  return(df)
}
