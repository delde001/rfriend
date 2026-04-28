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
#'        to this threshold will be formatted using scientific notation. Default is \code{10000}.
#' @param digits Integer. Number of decimal digits to use in formatting. Default is \code{3}.
#' @param replace_na Logical. If \code{TRUE}, \code{NA} values in the output
#'   are replaced with the string specified by \code{na_string}. If
#'   \code{FALSE}, \code{NA} values are left as \code{NA}. Default is
#'   \code{TRUE}.
#' @param na_string Character string used to replace \code{NA} values in the
#'   output when \code{replace_na = TRUE}. Use \code{"-"} (default) for a
#'   standard "not available" dash, \code{""} for empty cells, or any other
#'   string as needed. Note that special characters such as the em dash
#'   (\code{"\u2014"}) may require additional LaTeX declarations when rendering
#'   to PDF via \code{pdflatex}. Default is \code{"-"}.
#' @param allow_integer_decimal_mix Logical. If \code{TRUE}, each individual cell is evaluated: integer values are displayed without decimal places, and non-integer values are displayed with the specified number of decimal places, i.e. \code{digits}. Default is \code{FALSE}, when a column contains a mix of integers and decimal values, all values are displayed with the specified number of decimal places. Note: columns containing only integers are **always** displayed without decimal places, regardless of \code{allow_integer_decimal_mix}.
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
#'         are formatted in scientific notation with decimal \code{digits}.
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
#' # set allow_integer_decimal_mix = TRUE
#' f_conditional_round(df, allow_integer_decimal_mix = TRUE)
#'
#' # Custom NA replacement string.
#' f_conditional_round(c(0.5, NA, 3), replace_na = TRUE, na_string = "-")
#' # Returns: "0.500" "-" "3"
#'
#' f_conditional_round(c(0.5, NA, 3), replace_na = TRUE, na_string = "")
#' # Returns: "0.500" "" "3"
#'
#' @export
f_conditional_round <- function(x,
                                threshold_small = 0.01,
                                threshold_large = 10000,
                                digits = 3,
                                replace_na = TRUE,
                                na_string = "-",
                                allow_integer_decimal_mix = FALSE) {

  if(threshold_small >= threshold_large){
    stop("threshold_small > threshold_large.
    Threshold_small should be smaller than threshold_large")
  }

  # If digits is NULL, return x unchanged (no rounding)
  if (is.null(digits)) return(x)

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


        result[[col]] <- f_conditional_round(x[[col]],
                                             threshold_small = threshold_small,
                                             threshold_large = threshold_large,
                                             digits = digits,
                                             replace_na = replace_na,
                                             allow_integer_decimal_mix = allow_integer_decimal_mix)
      }
      return(result)
    } else {
      warning("No numeric columns found in the data frame.")
      return(x)
    }
  } else {
    # For vectors, apply the logic based on allow_integer_decimal_mix
    result <- x
    non_na <- !is.na(x)

    # Exact zeros get "0" -- the existing is_zero guard at the bottom handles this

    if (any(non_na)) {
      use_scientific <- abs(x[non_na]) < threshold_small | abs(x[non_na]) >= threshold_large


      # Scientific notation
      if (any(use_scientific)) {
        result[non_na][use_scientific] <- formatC(x[non_na][use_scientific],
                                                  format = "e", digits = digits)
      }

      # Regular values
      if (any(!use_scientific)) {
        non_sci_values <- x[non_na][!use_scientific]
        is_exact_zero  <- non_sci_values == 0
        rounds_to_zero <- !is_exact_zero & round(abs(non_sci_values), digits) == 0

        # Mirror data frame column logic exactly
        if (is_integer_column(non_sci_values)) {
          # All-integer: always no decimals regardless of allow_integer_decimal_mix
          result[non_na][!use_scientific] <-
            as.character(non_sci_values)
        } else if (allow_integer_decimal_mix == FALSE) {
          # Mixed but uniform formatting requested
          if (any(!rounds_to_zero & !is_exact_zero)) {
            result[non_na][!use_scientific][!rounds_to_zero & !is_exact_zero] <-
              formatC(non_sci_values[!rounds_to_zero & !is_exact_zero],
                      format = "f", digits = digits)
          }
          if (any(rounds_to_zero)) {
            result[non_na][!use_scientific][rounds_to_zero] <-
              formatC(0, format = "f", digits = digits)
          }
        } else {
          # Mixed with per-cell detection
          is_integer <- abs(non_sci_values %% 1) < .Machine$double.eps^0.5
          if (any(!is_integer & !rounds_to_zero & !is_exact_zero)) {
            result[non_na][!use_scientific][!is_integer & !rounds_to_zero & !is_exact_zero] <-
              formatC(non_sci_values[!is_integer & !rounds_to_zero & !is_exact_zero],
                      format = "f", digits = digits)
          }
          if (any(rounds_to_zero)) {
            result[non_na][!use_scientific][rounds_to_zero] <-
              formatC(0, format = "f", digits = digits)
          }
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
      result[is.na(result) | (!is.na(result) & result == "")] <- na_string
    }

    return(result)
  }
}
