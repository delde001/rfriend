#' Rename Elements of a Vector Based on a Mapping
#'
#' Renames elements of a vector based on a named mapping vector. Elements that match the names in the mapping vector are replaced with their corresponding values, while elements not found in the mapping remain unchanged.
#'
#' @param vector A character vector containing the elements to be renamed.
#' @param name_map A named vector where the names correspond to the elements in \code{vector} that should be renamed, and the values are the new names to assign.
#'
#' @return A character vector with updated element names. Elements not found in \code{name_map} remain unchanged.
#'
#' @details
#' This function iterates through each element of \code{vector} and checks if it exists in the names of \code{name_map}. If a match is found, the element is replaced with the corresponding value from \code{name_map}. If no match is found, the original element is retained. The result is returned as an unnamed character vector.
#'
#'
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # Define a vector and a name map.
#' vector   <- c("Species", "Weight", "L")
#' name_map <- c(Species = "New_species_name", L = "Length_cm")
#'
#' # Rename elements of the vector.
#' updated_vector <- f_rename_vector(vector, name_map)
#'
#' # View updated vector
#' print(updated_vector)
#'
#' @export

f_rename_vector <- function(vector, name_map) {
  updated_names <- vapply(vector, function(x) {
    if (x %in% names(name_map)) {
      name_map[x]
    } else {
      x
    }
  }, FUN.VALUE = character(1))

  updated_names_out <- as.vector(updated_names)
  return(updated_names_out)
}

