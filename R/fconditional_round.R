#' Conditional Rounding for Numeric Values
#'
#' @description
#' Conditionally formats numeric values based on their magnitude. Values that are very small
#' or very large are formatted using scientific notation, while other values are rounded
#' to a specified number of decimal places. Integers are preserved without decimal places.
#' When applied to a data frame, only numeric columns are processed. All output is character string.
#'
#' @param x A numeric vector or data frame containing numeric columns to be formatted.
#' @param threshold_small Numeric value. Values with absolute magnitude smaller than this
#'        threshold will be formatted using scientific notation. Default is \code{0.01}.
#' @param threshold_large Numeric value. Values with absolute magnitude larger than or equal
#'        to this threshold will be formatted using scientific notation. Default is \code{9999}.
#' @param digits Integer. Number of significant digits to use in formatting. Default is \code{3}.
#' @param replace_na Logical. If TRUE, NA values will be replaced with empty strings ("")
#'        in the output. Default is TRUE.
#' @param detect_int_col Logical. If \code{TRUE}, columns in a data.frame containing only integers will be displayed without decimal digits. Columns containing a mix of integers and decimal values will display all values with the specified number of digits. If \code{FALSE}, each individual cell is evaluated: integer values are displayed without digits, and numbers containing digits with the specified number of digits. Default is \code{TRUE}.
#'
#' @return
#' \itemize{
#'   \item If input is a vector: A character vector of the same length as the input, with values
#' formatted according to the specified rules.
#'
#'   \item If input is a data frame: A data frame with the same structure as the input, but with
#' character columns formatted according to the specified rules.
#'  }
#'
#' @details
#' The function applies the following formatting rules:
#' \itemize{
#'   \item Values smaller than \code{threshold_small} or larger than \code{threshold_large}
#'         are formatted in scientific notation with \code{digits} significant digits.
#'   \item Integer values are formatted without decimal places.
#'   \item Non-integer values that don't require scientific notation are rounded to
#'         \code{digits} decimal places.
#'   \item NA values are replaced with empty strings if \code{replace_na = TRUE}.
#'   \item Empty strings in the input are preserved.
#'   \item For data frames, only numeric columns are processed; other columns remain unchanged.
#' }
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # Vector examples.
#' f_conditional_round(c(0.0001, 0.5, 3, 10000))
#' # Returns: "1.000e-04" "0.500" "3" "1.000e+04".
#'
#' f_conditional_round(c(0.0001, 0.5, 3, 10000, NA), replace_na = TRUE)
#' # Returns: "1.000e-04" "0.500" "3" "1.000e+04" ""
#'
#' # Data frame example.
#' df <- data.frame(
#'   name = c("A", "B", "C"),
#'   small_val = c(0.0001, 0.002, 0.5),
#'   integer = c(1, 2, 3),
#'   integer_mix = c(10, 20, 30.1),
#'   large_val = c(10000, 5000, NA)
#' )
#'
#' # Show only two digits.
#' f_conditional_round(df, digits = 2)
#'
#' # To keep Integers as Integers (no digits)
#' # in columns with mixed data (Integers and digits)
#' # set detect_int_col = FALSE
#' f_conditional_round(df, detect_int_col = FALSE)
#'
#' @export
f_conditional_round <- function(x,
                                threshold_small = 0.01,
                                threshold_large = 9999,
                                digits = 3,
                                replace_na = TRUE,
                                detect_int_col = TRUE) {

  # Helper function to check if all non-NA values are integers
  is_integer_column <- function(vec) {
    non_na_vals <- vec[!is.na(vec) & vec != ""]
    if (length(non_na_vals) == 0) return(FALSE)
    all(abs(non_na_vals %% 1) < .Machine$double.eps^0.5)
  }

  # Check if input is a data frame
  if (is.data.frame(x)) {
    # Identify numeric columns
    numeric_cols <- sapply(x, is.numeric)

    if (any(numeric_cols)) {
      result <- x

      for (col in names(x)[numeric_cols]) {
        if (detect_int_col == TRUE) {
          # Determine digits based on whether column contains only integers
          col_digits <- if (is_integer_column(x[[col]])) 0 else digits
        } else {
          # Use standard digits for all columns
          col_digits <- digits
        }

        result[[col]] <- f_conditional_round(x[[col]],
                                             threshold_small = threshold_small,
                                             threshold_large = threshold_large,
                                             digits = col_digits,
                                             replace_na = replace_na,
                                             detect_int_col = detect_int_col)
      }
      return(result)
    } else {
      warning("No numeric columns found in the data frame.")
      return(x)
    }
  } else {
    # For vectors, apply the logic based on detect_int_col
    result <- x
    non_na <- !is.na(x) & x != ""

    if (any(non_na)) {
      use_scientific <- abs(x[non_na]) < threshold_small | abs(x[non_na]) >= threshold_large

      # Scientific notation
      if (any(use_scientific)) {
        result[non_na][use_scientific] <- formatC(x[non_na][use_scientific],
                                                  format = "e", digits = digits)
      }

      # Regular values
      if (any(!use_scientific)) {
        if (detect_int_col == TRUE) {
          # All non-scientific values get formatted with trailing zeros
          result[non_na][!use_scientific] <- formatC(x[non_na][!use_scientific],
                                                     format = "f", digits = digits)
        } else {
          # Check each cell individually for integer status
          non_sci_values <- x[non_na][!use_scientific]
          is_integer <- abs(non_sci_values %% 1) < .Machine$double.eps^0.5

          # Non-integers get formatted with digits
          if (any(!is_integer)) {
            result[non_na][!use_scientific][!is_integer] <-
              formatC(non_sci_values[!is_integer], format = "f", digits = digits)
          }

          # Integers stay as integers (convert to character without formatting)
          if (any(is_integer)) {
            result[non_na][!use_scientific][is_integer] <-
              as.character(non_sci_values[is_integer])
          }
        }
      }

      # Special handling for zeros - always simple "0"
      is_zero <- !is.na(x) & x == 0
      if (any(is_zero)) {
        result[is_zero] <- "0"
      }
    }

    if (replace_na) {
      result[is.na(result)] <- ""
    }

    return(result)
  }
}
