#' Identify Outliers within Groups using Tukey's Fences
#'
#' @description
#' `f_outliers()` scans numerical column(s) for outliers based on the Interquartile Range (IQR) method.
#' It can detect outliers across the entire dataset or within specified subgroups.
#'
#' It returns a dataframe containing only the outlier rows, preserving the original data structure
#' and adding a \code{row_id} column for traceability.
#'
#' @param x A data.frame or formula (dispatches to the right method).
#' @param formula A formula specifying the columns (right hand side) to be checked per subgroup(s) (left hand side).
#' More columns or groups can be added using \code{-} or \code{+} (e.g., \code{col1 + col2 ~ group1 + group2})
#' to do a sequential analysis for each column parameter.
#' @param data A  \code{vector}, \code{data.frame},  \code{data.table}, or  \code{tibble}.
#' @param columns The numerical columns to analyze if no formula is used. Can be entered
#' as a single character string (e.g., \code{"weight"}) or as a character vector \code{c("weight", "length"}).
#' @param group_vars A character vector specifying the grouping variables in \code{data}
#'   If \code{NULL} (default), the entire dataset is treated as one group (e.g., \code{c("species", "fertilizer")}).
#' @param id_var (Optional) A character string naming a user-specific ID columns (e.g., \code{"EmployeeID"}).
#'   If provided, this columns is placed at the start of the output for easy identification.
#' @param coef A number indicating the IQR multiplier. Default is \code{1.5} (standard Tukey fence).
#'   Common values:
#'   \itemize{
#'     \item \code{1.5}: Standard outliers (approximately +/- 2.7 SD in normal distribution).
#'     \item \code{3.0}: Extreme outliers.
#'   }
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
#' @param ... Further arguments forwarded to \code{f_outliers.data.frame}.
#'
#' @details
#' \strong{The Outlier Logic (Tukey's Method):}
#' An observation is flagged as an outlier if it falls outside the calculated fences:
#' \itemize{
#'   \item \strong{Lower Fence:} \eqn{Q1 - (coef \times IQR)}
#'   \item \strong{Upper Fence:} \eqn{Q3 + (coef \times IQR)}
#' }
#' Where \eqn{Q1} is the 25th percentile, \eqn{Q3} is the 75th percentile, and \eqn{IQR = Q3 - Q1}.
#'
#' \strong{Output Structure:}
#' The function returns a subset of the original \code{data}. It automatically adds a \code{row_id}
#' columns, which corresponds to the row number in the original dataframe. This ensures you can
#' strictly map the outliers back to the source data.
#'
#' @return A data.frame containing the identified outlier rows. Returns \code{NULL} (with a message)
#'   if no outliers are found.
#'
#' @seealso \code{\link{f_remove_outliers}} to remove the rows identified by this function.
#'
#' @examples
#' # --- Setup: Create Dummy Data ---
#' set.seed(42)
#' df <- data.frame(
#'   Team       = rep(c("A", "B"), each = 20),
#'   Department = rep(c("Sales", "IT"), each = 10, times = 2),
#'   Salary     = rnorm(40, mean = 50000, sd = 2000),
#'   Age        = rnorm(40, mean = 35, sd = 3),
#'   EmployeeID = paste0("E", sprintf("%03d", 1:40))
#' )
#'
#' # Inject outliers
#' df[2,  "Salary"] <- 57000   # Mild outlier (between 1.5 and 3.0 fence)
#' df[1,  "Salary"] <- 100000  # Extreme high
#' df[35, "Salary"] <- 1000    # Extreme low
#'
#' # --- Example 1: Basic detection (data.frame notation) ---
#' # Scan the entire dataset for Salary outliers (no grouping)
#' out <- f_outliers(df, columns = "Salary")
#' print(out)
#'
#' # --- Example 2: Basic detection (formula notation) ---
#' # Equivalent to Example 1 using the formula interface
#' # LHS = column(s) to scan, RHS = grouping variable(s)
#' out <- f_outliers(Salary ~ 1, data = df)
#' print(out)
#'
#' # --- Example 3: Grouped detection (both notations) ---
#' # Outliers are now evaluated *within* each Team separately,
#' # making detection sensitive to group-level distributions
#'
#' # data.frame notation:
#' out <- f_outliers(df, columns = "Salary", group_vars = "Team")
#'
#' # Formula notation (identical result):
#' out <- f_outliers(Salary ~ Team, data = df)
#' print(out)
#'
#' # --- Example 4: Multi-column + multi-group (formula notation) ---
#' # Scan both Salary and Age for outliers, grouped by Team and Department.
#' # Returns a named list: one data.frame per column scanned.
#' out <- f_outliers(Salary + Age ~ Team + Department, data = df)
#' print(out)            # prints both result tables
#' out$Salary            # access Salary outliers directly
#' out$Age               # access Age outliers directly
#'
#' # --- Example 5: Strict detection with a custom ID column ---
#' # coef = 3.0 flags only extreme outliers (the "far out" Tukey fence).
#' # id_var places EmployeeID first in the output for easy identification.
#'
#' # data.frame notation:
#' out <- f_outliers(df,
#'                   columns   = "Salary",
#'                   group_vars = "Team",
#'                   id_var    = "EmployeeID",
#'                   coef      = 3.0)
#'
#' # Formula notation (identical result):
#' out <- f_outliers(Salary ~ Team, data = df,
#'                   id_var = "EmployeeID",
#'                   coef   = 3.0)
#' print(out)
#'
#' # --- Example 6: Sensitivity comparison ---
#' # Compare how coef = 1.5 (standard) vs coef = 3.0 (extreme-only)
#' # affects the number of flagged rows.
#'
#' out_standard <- f_outliers(Salary ~ Team, data = df, coef = 1.5)
#' out_extreme  <- f_outliers(Salary ~ Team, data = df, coef = 3.0)
#'
#' nrow(out_standard$output_df)  # 3 -- catches mild + extreme outliers
#' nrow(out_extreme$output_df)   # 2 -- catches extreme outliers only
#'
#' # --- Example 7: Vector input ---
#' # Pass a column directly as a vector -- no data.frame needed.
#' # The column name is captured automatically from the call.
#'
#' out <- f_outliers(df$Salary)
#' print(out)
#'
#' # Works with coef and other parameters too
#' out <- f_outliers(df$Salary, coef = 3.0)
#' print(out)
#'
#' # Inline vectors fall back to the column name "value"
#' out <- f_outliers(c(1, 2, 3, 4, 5, 100))
#' print(out)
#'
#' @export
f_outliers <- function(x, ...) {
  mc <- match.call()
  if (missing(x)) {
    if (!is.null(mc$data)) {
      x_val <- eval(mc$data, envir = parent.frame())
      dots <- list(...)
      dots[["data"]] <- NULL
      return(do.call(f_outliers.data.frame, c(list(x = x_val), dots)))
    } else {
      stop("Argument 'x' (or 'data') is missing.")
    }
  }
  # Capture name here -- deparse(substitute()) is lost after dispatch
  if (is.numeric(x) || is.integer(x)) {
    raw_name <- deparse(substitute(x))
    # "df$Salary" -> "Salary", "my_vec" -> "my_vec"
    col_name <- gsub(".*\\$", "", raw_name)
    # Fallback for complex expressions like c(1, 2, 100)
    if (nchar(col_name) > 40 || grepl("[^a-zA-Z0-9_.]", col_name)) {
      col_name <- "value"
    }
    return(f_outliers.numeric(x, ..., internal_col_name = col_name))
  }
  UseMethod("f_outliers")
}

#' @export
#' @rdname f_outliers
f_outliers.numeric <- function(x, ...) {
  # Extract hidden internal argument -- not exposed in the help file
  data <- x
  dots <- list(...)
  col_name <- if ("internal_col_name" %in% names(dots)) {
    dots[["internal_col_name"]]
  } else {
    "value"
  }
  # Strip internal arg before passing to data.frame method
  dots[["internal_col_name"]] <- NULL

  df <- setNames(data.frame(data), col_name)
  do.call(f_outliers.data.frame,
          c(list(x = df, columns = col_name), dots))
}


# Integer piggybacks on numeric -- no duplication needed
#' @export
#' @rdname f_outliers
f_outliers.integer <- f_outliers.numeric


#' @export
#' @rdname f_outliers
f_outliers.formula <- function(formula, data, ...) {
  # formula is the formula (e.g., y ~ B0 + B1)

  if (length(formula) < 3)
    stop("Formula must be two-sided: columns ~ group_vars (e.g., Salary ~ Team)")

  # Warn if LHS has expressions like log(y) before silently stripping them
  check_lhs_is_names(formula) #use helper_check_lhs.R

  # Parse LHS (Response Variable)
  lhs_vars <- all.vars(formula[[2]])



  # Parse RHS (Grouping Variables)
  rhs_vars  <- all.vars(formula[[3]])
  if (length(rhs_vars) == 0)
    rhs_vars <- NULL

  # Capture the name of the data object passed to the formula method
  data_name_str <- deparse(substitute(data))

  if(length(data_name_str) > 1) data_name_str <- "data"


  # Call the data.frame method
  f_outliers.data.frame(x = data,
                        columns = lhs_vars,
                        group_vars = rhs_vars,
                        internal_data_name = data_name_str,
                        ...)
}

#' @export
#' @rdname f_outliers
f_outliers.data.frame <- function(x,
                                  columns,
                                  group_vars = NULL,
                                  id_var = NULL,
                                  coef = 1.5,
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

  # Validate Column Vars
  # Add this after the group_vars validation block:
  missing_cols <- columns[!columns %in% names(data)]
  if (length(missing_cols) > 0)
    stop(paste("Columns not found in data:", paste(missing_cols, collapse = ", ")))

  non_numeric <- columns[!sapply(data[columns], is.numeric)]
  if (length(non_numeric) > 0)
    stop(paste("Non-numeric columns specified:", paste(non_numeric, collapse = ", ")))

  # Safe filename logic
  # Default safety fallback
  data_name <- "data"

  # Check for the hidden internal name (passed from formula method)
  dots <- list(...)
  if ("internal_data_name" %in% names(dots)) {
    data_name <- dots[["internal_data_name"]]
  } else {
    # If no internal name, try to grab it from 'data' using:
    try_name <- try(deparse(substitute(x)), silent = TRUE)
    if (!inherits(try_name, "try-error") && length(try_name) == 1 && nchar(try_name) < 50) {
      data_name <- try_name
    }
  }

  # Create a list to store all outputs in this function
  output_list <- list()
  output_excel<- list()

  # Preserve Original Row Number
  work_data <- data
  work_data$row_id <- seq_len(nrow(work_data))

  # Create Grouping Factor
  if (is.null(group_vars)) {
    # If no groups, treat whole data as one group
    split_data <- list(all_data = work_data)
  } else {
    # Combine multiple groups (e.g. Team.Year)
    group_factor <- interaction(work_data[group_vars], drop = TRUE, sep = ".")
    split_data <- split(work_data, group_factor)
  }



  # --- The Logic (Tukey's Fences) ---
  find_outliers <- function(df) {
    values <- df[[target_col]]

    # Calculate Stats
    q1 <- quantile(values, 0.25, na.rm = TRUE)
    q3 <- quantile(values, 0.75, na.rm = TRUE)
    iqr_value <- q3 - q1

    lower_fence <- q1 - (coef * iqr_value)
    upper_fence <- q3 + (coef * iqr_value)

    # Filter
    is_outlier <- values < lower_fence | values > upper_fence
    is_outlier[is.na(is_outlier)] <- FALSE

    return(df[is_outlier, ])
  }


  for (target_col in columns) {

    # --- Execution ---
    outlier_list <- lapply(split_data, find_outliers)
    result <- do.call(rbind, outlier_list)

    # Handle empty results
    if (is.null(result) || nrow(result) == 0) {
      message(paste0("No outliers found in column: '", target_col, "'."))
      next
    }

    # Sort
    if (is.null(id_var)){
    result <- result[order(result$row_id), ]
    } else {
    result <- result[order(result[[id_var]]), ]
    }

    # --- columns Ordering ---
    # Define desired order: ID -> row_id -> Value -> Groups -> Rest
    priority_wishlist <- unique(c(id_var, "row_id", target_col, group_vars))

    # Filter this list to include ONLY columns that actually exist in 'result'
    priority_cols <- intersect(priority_wishlist, names(result))

    # Identify all other columns that are not in the priority list
    remaining_cols <- setdiff(names(result), priority_cols)

    # Reorder columns safely
    # drop = FALSE ensures it remains a dataframe even if only 1 column is selected
    final_result <- result[, c(priority_cols, remaining_cols), drop = FALSE]

    # Reset row names
    rownames(final_result) <- NULL

    # --- FORMATTING ---

    # For backward compatability use the name "output_df" when only one column is produced.
    if (single_col) {
      output_list[["output_df"]] <- final_result
    } else {
      output_list[[target_col]] <- final_result
    }

    # Use separate rounding for excel or default to no rounding.
    if (!is.null(digits_excel) ) {
      output_excel[[target_col]] <- f_conditional_round(final_result,
                                                    digits = digits_excel,
                                                    allow_integer_decimal_mix = allow_integer_decimal_mix)
    } else {
      output_excel[[target_col]] <- final_result
    }

  }

  class(output_list) <- "f_outliers"

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
      default_name = paste(data_name, "outliers.xlsx", sep = "_"),
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

  if (length(output_list) == 0) {
    message("No outliers found in any column.")
    return(NULL)
  } else {
  return(output_list)
  }
}

#' Print method for f_outliers objects
#'
#' Prints a formatted summary table to the console.
#'
#' @param x Object of class f_outliers.
#' @param col_width Integer. Max characters in header before line break. Default \code{6}.
#' @param table_width Integer or \code{NULL}. Characters after which table splits. Default \code{90}.
#' @param digits Integer. Number of decimal digits to use in formatting. Default is \code{3}.
#' @param allow_integer_decimal_mix Logical. If \code{TRUE}, each individual cell is evaluated: integer values are displayed without decimal places, and non-integer values are displayed with the specified number of decimal places, i.e. \code{digits}. Default is \code{FALSE}, when a column contains a mix of integers and decimal values, all values are displayed with the specified number of decimal places. Note: columns containing only integers are **always** displayed without decimal places, regardless of \code{allow_integer_decimal_mix}.
#' @param ... Additional arguments passed to \code{pander}.
#' @return Invisibly returns \code{1}.
#' @export
print.f_outliers <- function(x,
                            col_width = 6,
                            table_width = 90,
                            digits = 2,
                            allow_integer_decimal_mix = FALSE,
                            ...) {

  # Get digits from f_outliers()
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

