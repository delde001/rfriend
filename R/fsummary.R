#' Summarize a Data Frame with Grouping Variables
#'
#' @description
#' Computes summary statistics (n, mean, sd, etc.) for a specified numerical columns
#' in a data frame. The data can be analyzed as a whole or split by one or more
#' grouping variables.
#'
#' The function returns a formatted data frame and includes options to export
#' the results directly to an 'Excel' file.
#'
#' @param x A data.frame or formula (dispatches to the right method).
#' @param formula A formula specifying the columns (right hand side) to be summarized by groups (left hand side). More columns or groups can be added using \code{-} or \code{+} (e.g., \code{col1 + col2 ~ group1 + group2}) to do a sequential summary for each column parameter.
#' @param data A 'data.frame', 'data.table', or 'tibble'.
#' @param columns The numerical column(s) to summarize if no formula is used. Can be entered as a single character string (e.g., \code{"weight"}) or as a character vector \code{c("weight", "length"}).
#' @param group_vars A character vector specifying the grouping variables in \code{data}
#'   (e.g., \code{c("species", "fertilizer")}) if no formula is used. If \code{NULL}, the entire dataset is summarized.
#' @param show_name Logical. Include variable name. Default \code{TRUE}.
#' @param show_n Logical. Include count (\code{n}). Default \code{TRUE}.
#' @param show_mean Logical. Include mean. Default \code{TRUE}.
#' @param show_sd Logical. Include standard deviation. Default \code{TRUE}.
#' @param show_se Logical. Include standard error. Default \code{TRUE}.
#' @param show_min Logical. Include minimum value. Default \code{TRUE}.
#' @param show_max Logical. Include maximum value. Default \code{TRUE}.
#' @param show_median Logical. Include median. Default \code{TRUE}.
#' @param show_Q1 Logical. Include first quartile (25th percentile). Default \code{TRUE}.
#' @param show_Q3 Logical. Include third quartile (75th percentile). Default \code{TRUE}.
#' @param show_skew Logical. Include Skewness (measure of asymmetry). Default \code{FALSE}.
#' @param show_kurtosis Logical. Include Excess Kurtosis (measure of "tailedness"). Default \code{FALSE}.
#' @param digits Integer. Number of decimal places for the R console output.
#'   Default is \code{2}. If \code{NULL}, no rounding is applied.
#'   (Note: This does not affect the raw numbers exported to Excel).
#' @param export_to_excel Logical. If \code{TRUE}, exports results to an 'Excel' file. Default \code{FALSE}.
#' @param digits_excel Integer. Number of decimal places for the Excel file cells. Default \code{NULL} (no rounding). Defining \code{digits_excel}, sets \code{export_to_excel = TRUE} when excel output is not intended use: \code{digits} instead.
#' @param allow_integer_decimal_mix Logical. If \code{TRUE}, intergers in columns with a mix of integers and non-integers are displayed without decimals. Default \code{FALSE}, meaning if there are one or more numbers with decimals the whole column contains the number of decimals set by \code{digits}.
#' @param save_as Character string. Custom path or filename for the Excel export.
#'   \itemize{
#'     \item If full path: Saves to that location.
#'     \item If filename only: Saves to \code{tempdir()} (unless \code{save_in_wdir = TRUE}).
#'     \item If directory: Saves as "dataname_summary.xlsx" in that directory.
#'   }
#' @param save_in_wdir Logical. If \code{TRUE}, saves to the current working directory. Default \code{FALSE}.
#' @param close_generated_files Logical. If \code{TRUE}, forces Excel to close before saving (Windows only). Default \code{FALSE}.
#' @param open_generated_files Logical. Whether to open the generated output
#'   files after creation. Defaults to \code{TRUE} in an interactive R session
#'   and \code{FALSE} otherwise (e.g. in scripts or automated pipelines).
#'   Set to \code{TRUE} or \code{FALSE} to override this behaviour explicitly.
#' @param check_input Logical. If \code{TRUE}, performs validation checks on inputs. Default \code{TRUE}.
#' @param ... Further arguments forwarded to \code{f_summary.data.frame}.
#'
#'
#' @details
#' The function computes the following statistics:
#' \itemize{
#'   \item \code{n}: number of observations
#'   \item \code{mean}: arithmetic mean
#'   \item \code{sd}: standard deviation
#'   \item \code{se}: standard error (\eqn{sd / \sqrt{n}})
#'   \item \code{min}: minimum value
#'   \item \code{max}: maximum value
#'   \item \code{median}: median value
#'   \item \code{Q1}: 25th percentile
#'   \item \code{Q3}: 75th percentile
#'   \item \code{skew}: Sample skewness (if requested).
#'   \item \code{kurt}: Sample excess kurtosis (if requested).
#' }
#'
#' \code{skew} stands for Skewness which is a measure of asymmetry of a distribution around its mean. Where \code{skew} values near \bold{0} indicate approximate symmetry, while large positive or negative values indicate noticeable asymmetry.
#' \itemize{
#'   \item \code{> 0}: Right-skewed (long or heavier tail to the right).
#'   \item \code{< 0}: Left-skewed  (long or heavier tail to the left).
#'  }
#'
#' \code{kurt} stands for Excess Kurtosis: Tells you about the "tails" and the peak.
#'   \itemize{
#'     \item \code{0}: Same tail heaviness as the normal distribution (mesokurtic).
#'     \item \code{> 0}: Heavier tails than normal (Leptokurtic) -- indicates frequent outliers.
#'     \item \code{< 0}: Lighter tails than normal (Platykurtic) -- indicates fewer (or less extreme) outliers than a normal distribution.
#'  }
#' If \code{group_vars} are provided, the statistics are calculated for each group combination.
#' When \code{export_to_excel = TRUE}, the file is automatically generated.
#'
#' @return A list of class \code{f_summary} containing the results data frame.
#'
#' @author
#' Sander H. van Delden \email{plantmind@proton.me}
#'
#' @examples
#'
#' # --- Example 1: Basic Usage (data.frame notation) ---
#' # Summarize "hp" grouped by "cyl"; columns and group_vars can be positional
#' summary_mtcars <- f_summary(mtcars, columns = "hp", group_vars = "cyl")
#' summary_mtcars <- f_summary(mtcars, "hp", "cyl")  # shorthand equivalent
#' print(summary_mtcars)
#'
#' # --- Example 2: Multiple Columns & Groups with Custom Toggles ---
#' # Summarize "hp" and "disp", grouped by "cyl" and "gear", hide Q1/Q3
#' summary_custom <- f_summary(mtcars,
#'                             columns    = c("hp", "disp"),
#'                             group_vars = c("cyl", "gear"),
#'                             show_Q1    = FALSE,
#'                             show_Q3    = FALSE)
#' print(summary_custom)
#'
#' # --- Example 3: Formula Notation ---
#' # Identical result to Example 2 using formula interface
#' # and export output to excel
#' summary_formula <- f_summary(hp + disp ~ cyl + gear,
#'                              data    = mtcars,
#'                              show_Q1 = FALSE,
#'                              show_Q3 = FALSE,
#'                              export_to_excel = TRUE)
#' print(summary_formula)
#'
#' # --- Example 4: Distributional Stats & Digits ---
#' # Add skewness and kurtosis, control rounding
#' summary_dist <- f_summary(Sepal.Length + Petal.Length ~ Species,
#'                           data          = iris,
#'                           show_skew     = TRUE,
#'                           show_kurtosis = TRUE,
#'                           digits        = 3)
#' print(summary_dist)
#'
#' # --- Example 5: Custom Print Formatting ---
#' summary_iris <- f_summary(iris, "Sepal.Length", group_vars = "Species")
#' print(summary_iris, col_width = 10, table_width = 70)

#' @export
f_summary <- function(x, ...) {
  # Use match.call() to check for 'data' WITHOUT evaluating the arguments.
  # This prevents the "object x not found" error.
  mc <- match.call()

  if (missing(x)) {
    # Check if 'data' was provided in the call (e.g. f_summary(data = mtcars))
    if (!is.null(mc$data)) {
      # Manually retrieve the data object
      x_val <- eval(mc$data, envir = parent.frame())
      # Dispatch manually to the data.frame method
      return(f_summary.data.frame(x_val, ...))
    } else {
      stop("Argument 'x' (or 'data') is missing.")
    }
  }

  # Standard S3 Dispatch
  UseMethod("f_summary")
}

#' @export
#' @rdname f_summary
f_summary.formula <- function(x, data, ...) {
  # x is the formula (e.g., y ~ B0 + B1)

  # Warn if LHS has expressions like log(y) before silently stripping them
  check_lhs_is_names(x) #use helper_check_lhs.R

  # Parse LHS (Response Variable)
  lhs_vars <- all.vars(x[[2]])

  # Parse RHS (Grouping Variables)
  rhs_vars  <- all.vars(x[[3]])
  if (length(rhs_vars) == 0)
    rhs_vars <- NULL

  # Capture the name of the data object passed to the formula method
  data_name_str <- deparse(substitute(data))

  if(length(data_name_str) > 1) data_name_str <- "data"

  # Call the data.frame method
  f_summary.data.frame(x = data,
                       columns = lhs_vars,
                       group_vars = rhs_vars,
                       internal_data_name = data_name_str,
                       ...)
}

#' @export
#' @rdname f_summary
f_summary.data.frame <- function(x,
                                 columns,
                                 group_vars = NULL,
                                 show_name = TRUE,
                                 # Statistics Toggles
                                 show_n = TRUE,
                                 show_mean = TRUE,
                                 show_sd = TRUE,
                                 show_se = TRUE,
                                 show_min = TRUE,
                                 show_max = TRUE,
                                 show_median = TRUE,
                                 show_Q1 = TRUE,
                                 show_Q3 = TRUE,
                                 show_skew = FALSE,
                                 show_kurtosis = FALSE,
                                 digits = NULL,
                                 # File Options
                                 export_to_excel = FALSE,
                                 close_generated_files = FALSE,
                                 open_generated_files = interactive(),
                                 save_as = NULL,
                                 save_in_wdir = FALSE,
                                 check_input = TRUE,
                                 digits_excel = NULL,
                                 allow_integer_decimal_mix = FALSE,
                                 ...) {
  # Map 'x' (S3 standard) back to 'data' (Internal logic)
  data <- x

  # Input Validation & Setup
  if (!is.data.frame(data))
    stop("Input must be a data.frame.")

  # Check if there is one or multiple columns to summarize
  if(length(columns) == 1){
    single_col = TRUE
  } else if(length(columns) > 1){
    single_col = FALSE
  }

  # Validate Group Vars
  if (!is.null(group_vars)) {
    if (!all(group_vars %in% names(data))) {
      missing <- group_vars[!group_vars %in% names(data)]
      stop(paste("Group columns not found:", paste(missing, collapse = ", ")))
    }
  }

  # Safe filename logic
  # Default safety fallback
  data_name <- "data"

  # Check for the hidden internal name (passed from formula method)
  dots <- list(...)
  if ("internal_data_name" %in% names(dots)) {
    data_name <- dots[["internal_data_name"]]
  } else {
    # If no internal name, try to grab it from 'x' using:
    try_name <- try(deparse(substitute(x)), silent = TRUE)
    if (!inherits(try_name, "try-error") && length(try_name) == 1 && nchar(try_name) < 50) {
      data_name <- try_name
    }
  }

  # Create a list to store all outputs in this function
  output_list <- list()
  output_excel<- list()

  # --- WORKER FUNCTION ---
  summary_fun <- function(vec) {
    result <- c()
    if (show_n)
      result["n"] <- sum(!is.na(vec))
    if (show_mean)
      result["mean"] <- mean(vec, na.rm = TRUE)
    if (show_sd)
      result["sd"] <- sd(vec, na.rm = TRUE)
    if (show_se)
      result["se"] <- sd(vec, na.rm = TRUE) / sqrt(length(vec))
    if (show_min)
      result["min"] <- min(vec, na.rm = TRUE)
    if (show_Q1)
      result["Q1"] <- quantile(vec, probs = 0.25, na.rm = TRUE)
    if (show_median)
      result["median"] <- median(vec, na.rm = TRUE)
    if (show_Q3)
      result["Q3"] <- quantile(vec, probs = 0.75, na.rm = TRUE)
    if (show_max)
      result["max"] <- max(vec, na.rm = TRUE)

    if (show_skew || show_kurtosis) {
      n <- length(vec)
      s <- sd(vec, na.rm = TRUE)
      m <- mean(vec, na.rm = TRUE)
      if (show_skew) {
        if (n < 3 || s == 0)
          result["skew"] <- NA
        else {
          m3 <- sum((vec - m)^3) / n
          result["skew"] <- (n * (n - 1)) / (n - 2) * (m3 / s^3)
        }
      }
      if (show_kurtosis) {
        if (n < 4 || s == 0)
          result["kurt"] <- NA
        else {
          m4 <- sum((vec - m)^4) / n
          kurt_val <- ((n + 1) * n * (n - 1)) / ((n - 2) * (n - 3)) * (m4 / s^4)
          result["kurt"] <- kurt_val - (3 * (n - 1)^2) / ((n - 2) * (n - 3))
        }
      }
    }
    return(result)
  }



  for (column in columns) {

    # --- AGGREGATION ---
    if (is.null(group_vars)) {
      raw_result <- summary_fun(data[[column]])
      result <- as.data.frame(t(raw_result))
    } else {
      result <- do.call(data.frame, aggregate(data[[column]], by = data[group_vars], FUN = summary_fun))

      base_cols <- group_vars
      stat_cols <- names(result)[(length(base_cols) + 1):ncol(result)]
      stat_cols <- gsub("^x\\.", "", stat_cols)
      if (length(stat_cols) == 1 && stat_cols == "x") {
        stat_cols <- names(summary_fun(data[[column]]))
      }
      if (show_name == TRUE) {
        names(result) <- c(base_cols, paste(column, stat_cols, sep = "."))
      } else {
        names(result) <- c(base_cols, stat_cols)
      }
    }

    # --- FORMATTING ---

    # For backward compatability use the name "output_df" when only one column is produced.
    if (single_col) {
      output_list[["output_df"]] <- result
    } else {
      output_list[[column]] <- result
    }

    # Use separate rounding for excel or default to no rounding.
    if (!is.null(digits_excel) ) {
      output_excel[[column]] <- f_conditional_round(result,
                                         digits = digits_excel,
                                         allow_integer_decimal_mix = allow_integer_decimal_mix)
    } else {
      output_excel[[column]] <- result
    }

  }

  class(output_list) <- "f_summary"

  # --- EXCEL EXPORT ---
  if (!is.null(save_as) ||
      !is.null(digits_excel) || save_in_wdir == TRUE) {
    export_to_excel = TRUE
  }

  if (export_to_excel) {

    if (close_generated_files)
      system("taskkill /im EXCEL.EXE /f")

    if (save_in_wdir)
      save_dir <- getwd()
    else
      save_dir <- tempdir()

    output_path <- get_save_path(
      save_as = save_as,
      default_name = paste(data_name, "summary.xlsx", sep = "_"),
      default_dir = save_dir,
      file.ext = ".xlsx"
    )

    message(paste0("Saved output in: ", output_path))

    write_xlsx(output_excel, path = output_path)

    if (open_generated_files){
      f_open_file(output_path)
    }
  }

  attr(output_list, "digits") <- digits
  return(output_list)
}


#' Print method for f_summary objects
#'
#' Prints a formatted summary table to the console.
#'
#' @param x Object of class f_summary.
#' @param col_width Integer. Max characters in header before line break. Default \code{6}.
#' @param table_width Integer or \code{NULL}. Characters after which table splits. Default \code{90}.
#' @param digits Integer. Number of decimal digits to use in formatting. Default is \code{3}.
#' @param allow_integer_decimal_mix Logical. If \code{TRUE}, each individual cell is evaluated: integer values are displayed without decimal places, and non-integer values are displayed with the specified number of decimal places, i.e. \code{digits}. Default is \code{FALSE}, when a column contains a mix of integers and decimal values, all values are displayed with the specified number of decimal places. Note: columns containing only integers are **always** displayed without decimal places, regardless of \code{allow_integer_decimal_mix}.
#' @param ... Additional arguments passed to \code{pander}.
#' @return Invisibly returns \code{1}.
#' @export
print.f_summary <- function(x,
                            col_width = 6,
                            table_width = 90,
                            digits = 2,
                            allow_integer_decimal_mix = FALSE,
                            ...) {

  # Get digits from f_summary()
  if (missing(digits) && !is.null(attr(x, "digits"))) {
    digits <- attr(x, "digits")
  }

  # Loop over each category (a, b, etc.)
  for (category in names(x)) {
    # Get the sublist for this category
    sublist <- x[[category]]

    # Round numbers if digits is not NULL
    if (!is.null(digits)) {
      sublist <- f_conditional_round(sublist, digits = digits, allow_integer_decimal_mix = allow_integer_decimal_mix)
    }

    f_pander(sublist, col_width = col_width, table_width = table_width)
  }
}
