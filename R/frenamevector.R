#' Rename Elements of a Vector Based on a Mapping
#'
#' Renames elements of a vector based on a named mapping vector. Elements that match the names in the mapping vector are replaced with their corresponding values, while elements not found in the mapping remain unchanged. Use it to clean labels, for example \code{c("Low N" = "low_N")}.
#'
#' @param vector A character vector or a factor containing the elements to be renamed.
#' @param name_map A named vector where the names correspond to the elements in \code{vector} that should be renamed, and the values are the new names to assign.
#' @param trim Logical. If \code{TRUE}, removes leading and trailing spaces from every element before renaming, so \code{" high_N"} becomes \code{"high_N"}. Default \code{FALSE}.
#' @param warn_unmatched Logical. If \code{TRUE} (default), warns when a name in \code{name_map} does not occur in \code{vector}, which usually means a typing error in the old name. Set to \code{FALSE} when the map deliberately holds more names than the vector.
#'
#' @return A character vector with updated element names, or a factor with renamed levels when \code{vector} is a factor. Elements not found in \code{name_map} remain unchanged.
#'
#' @details
#' Each element of \code{vector} that occurs in the names of \code{name_map} is replaced with the corresponding value from \code{name_map}; other elements are kept. A character vector is returned as an unnamed character vector. A factor is returned as a factor: its levels are renamed and keep their order, and two old levels that get the same new name become one level. Missing values stay missing.
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
#' # Clean treatment labels: remove spaces and rename one label
#' treatment <- c(" high_N", "Low N", "control", "drought")
#' f_rename_vector(treatment, c("Low N" = "low_N"), trim = TRUE)
#'
#' @export

f_rename_vector <- function(vector, name_map, trim = FALSE, warn_unmatched = TRUE) {

  if (!is.logical(trim) || length(trim) != 1L || is.na(trim)) {
    stop("f_rename_vector(): 'trim' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(warn_unmatched) || length(warn_unmatched) != 1L || is.na(warn_unmatched)) {
    stop("f_rename_vector(): 'warn_unmatched' must be TRUE or FALSE.", call. = FALSE)
  }

  # NULL or an empty vector: nothing to rename (unchanged behaviour)
  if (length(vector) == 0L) {
    return(if (is.factor(vector)) vector else character(0))
  }

  is_fac <- is.factor(vector)
  if (!is_fac && !is.character(vector)) {
    stop("f_rename_vector(): 'vector' must be a character vector or a factor, not ",
         class(vector)[1], ".", call. = FALSE)
  }

  # The values to rename: the factor levels, or the text values themselves
  old <- if (is_fac) levels(vector) else vector
  if (trim) old <- trimws(old)

  map_names <- names(name_map)
  if (is.null(map_names)) {
    # An unnamed map has never renamed anything; say so instead of failing,
    # so scripts that passed one keep running.
    warning("f_rename_vector(): 'name_map' has no names, so nothing was renamed. ",
            "Use old = \"new\", for example c(\"Low N\" = \"low_N\").", call. = FALSE)
    map_names <- character(0)
  }

  if (warn_unmatched) {
    unmatched <- setdiff(map_names[!is.na(map_names) & map_names != ""], old)
    if (length(unmatched)) {
      present <- sort(unique(old[!is.na(old)]))
      warning("f_rename_vector(): not found in 'vector', so not renamed: ",
              paste0("\"", unmatched, "\"", collapse = ", "),
              ". Present: ", paste0("\"", present, "\"", collapse = ", "), ".",
              call. = FALSE)
    }
  }

  hit <- !is.na(old) & old %in% map_names
  new <- old
  new[hit] <- unname(as.character(name_map[old[hit]]))

  if (is_fac) {
    # factor(): labels that are now the same become one level, in the old order
    return(factor(as.integer(vector), levels = seq_along(new), labels = new,
                  ordered = is.ordered(vector)))
  }
  as.vector(new)
}
