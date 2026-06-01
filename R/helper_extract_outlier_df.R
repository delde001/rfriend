# Internal: pull the outlier data.frame out of an f_outliers() result.
# f_outliers() returns:
#   - NULL                              when no outliers were found
#   - a list of class "f_outliers"      when outliers were found
#     -> $output_df when a single column was scanned
# Returns NULL if no outlier data.frame can be recovered.
extract_outlier_df <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "f_outliers") && !is.null(x$output_df)) return(x$output_df)
  if (is.data.frame(x)) return(x)
  NULL
}
