#' Remove Outliers from Data
#'
#' @description
#' `f_remove_outliers()` removes specific rows from a dataframe based on a list of identifiers,
#' or, with \code{set_na = TRUE}, keeps the rows and sets only the outlier values to \code{NA}.
#' It is designed to work seamlessly with the output of \code{\link{f_outliers}}, but can also
#' accept a custom vector of IDs.
#'
#' @param data A data.frame, tibble, or data.table containing the original data.
#' @param outliers Either:
#'   \itemize{
#'     \item A dataframe returned by \code{\link{f_outliers}}.
#'     \item A vector of IDs/row numbers to remove.
#'     \item \code{NULL}, which is what \code{\link{f_outliers}} returns when it
#'       finds no outliers. The data is then returned unchanged (with a
#'       message when \code{verbose = TRUE}), so the find-then-remove pair
#'       also runs on data without outliers.
#'   }
#' @param by A character string specifying the column to match on. Default is \code{"row_id"}.
#'   If the source \code{data} does not have a \code{row_id} column, the function effectively
#'   uses the row numbers (1, 2, 3...) to ensure safe deletion.
#' @param verbose Logical. If \code{TRUE} (default), prints a summary of what was removed.
#' @param set_na Logical. If \code{FALSE} (default), whole rows are removed. If
#'   \code{TRUE}, the rows are kept and only the values are set to \code{NA}
#'   (missing): with an \code{f_outliers} result, exactly the flagged values in
#'   the column(s) where they were flagged; with a vector of IDs or a data
#'   frame, the values in \code{columns} of those rows.
#' @param columns Character vector of column names. Only used with
#'   \code{set_na = TRUE}. With an \code{f_outliers} result it limits the
#'   change to these checked columns (default: all checked columns). With a
#'   vector of IDs or a data frame it is required: the values in these columns
#'   of the given rows are set to \code{NA}.
#'
#' @details
#' \strong{Safe Deletion Logic:}
#' This function performs a "anti-join" style filtering. It keeps rows where the identifier in
#' \code{by} is \strong{not} found in the \code{outliers} list.
#'
#' \strong{Handling Row IDs:}
#' If you use the default \code{by = "row_id"} and your original \code{data} does not have a
#' column named \code{"row_id"}, the function assumes you are referring to the intrinsic
#' row numbers of the data.frame, tibble, or data.table. It will temporarily generate IDs to
#' perform the deletion and then return the clean data with the original structure
#' (without adding a permanent \code{row_id} column to the result).
#'
#' @return An object of the same class as the input \code{data} (data.frame, tibble, or data.table)
#'   with the specified outlier rows removed.
#'
#' @seealso \code{\link{f_outliers}} to identify the rows to be removed.
#'
#' @examples
#' # --- Setup: Create Dummy Data ---
#' set.seed(42)
#' df <- data.frame(
#'   Team       = rep(c("A", "B"), each = 20),
#'   Department = rep(c("Sales", "IT"), each = 10, times = 2),
#'   Salary     = c(rnorm(19, 50000, 500), 100000,
#'                  rnorm(18, 50000, 500), 57000, 1000),
#'   Age        = c(rnorm(38, 35, 2), 90, 35),
#'   EmployeeID = paste0("E", sprintf("%03d", 1:40)),
#'   stringsAsFactors = FALSE
#' )
#' # row 20:  extreme high Salary (Team A)
#' # row 39:  mild Salary outlier at coef = 1.5 only
#' # row 40:  extreme low  Salary (Team B)
#' # row 39:  extreme high Age
#'
#' # --- Example 1: Basic two-step workflow (data.frame notation) ---
#' # The most common use case: find then remove in two lines.
#' bad_rows <- f_outliers(df, columns = "Salary")
#' clean_df <- f_remove_outliers(df, bad_rows)
#' nrow(df)       # 40
#' nrow(clean_df) # 40 minus flagged rows
#'
#' # --- Example 2: Basic two-step workflow (formula notation) ---
#' # Identical result to Example 1 using the formula interface.
#' bad_rows <- f_outliers(Salary ~ 1, data = df)
#' clean_df <- f_remove_outliers(df, bad_rows)
#' nrow(clean_df)
#'
#' # --- Example 3: Grouped detection then removal (both notations) ---
#' # Outliers are identified *within* each Team separately before removal.
#'
#' # data.frame notation:
#' bad_rows <- f_outliers(df, columns = "Salary", group_vars = "Team")
#' clean_df <- f_remove_outliers(df, bad_rows)
#'
#' # Formula notation (identical result):
#' bad_rows <- f_outliers(Salary ~ Team, data = df)
#' clean_df <- f_remove_outliers(df, bad_rows)
#' nrow(clean_df)
#'
#' # --- Example 4: Selective removal -- only act on a subset of outliers ---
#' # Find all flagged rows, but only remove the extreme high salaries.
#' # Step 1: Identify all Salary outliers grouped by Team
#' bad_rows    <- f_outliers(Salary ~ Team, data = df)
#' all_flagged <- bad_rows$output_df
#'
#' # Step 2: Filter to keep only the rows where Salary > 90000
#' really_bad  <- all_flagged[all_flagged$Salary > 90000, ]
#'
#' # Step 3: Remove only those rows -- low outlier (row 40) is preserved
#' clean_df <- f_remove_outliers(df, really_bad)
#' range(clean_df$Salary)  # low outlier still present, high one is gone
#'
#' # --- Example 5: Multi-column outlier removal ---
#' # f_outliers scans both Salary and Age; f_remove_outliers removes
#' # every row flagged by either column in one call.
#'
#' # Formula notation:
#' bad_rows <- f_outliers(Salary + Age ~ Team, data = df)
#' clean_df <- f_remove_outliers(df, bad_rows)
#'
#' # data.frame notation (identical result):
#' bad_rows <- f_outliers(df, columns = c("Salary", "Age"), group_vars = "Team")
#' clean_df <- f_remove_outliers(df, bad_rows)
#' nrow(clean_df)  # rows flagged by Salary OR Age are removed
#'
#' # --- Example 6: Strict detection + custom ID column ---
#' # coef = 3.0 flags only extreme outliers. EmployeeID is used
#' # as the matching key instead of the default row_id.
#'
#' # Formula notation:
#' bad_rows <- f_outliers(Salary ~ Team, data = df,
#'                        id_var = "EmployeeID", coef = 3.0)
#'
#' # data.frame notation (identical result):
#' bad_rows <- f_outliers(df, columns = "Salary", group_vars = "Team",
#'                        id_var = "EmployeeID", coef = 3.0)
#'
#' # Remove by EmployeeID rather than row position
#' clean_df <- f_remove_outliers(df, bad_rows$output_df, by = "EmployeeID")
#'
#' # Confirm the flagged employees are no longer in the clean data
#' bad_ids  <- bad_rows$output_df$EmployeeID
#' any(clean_df$EmployeeID %in% bad_ids)  # FALSE
#'
#' # --- Keep the rows, set only the flagged values to NA ---
#' bad_rows <- f_outliers(Salary ~ Team, data = df)
#' df_na <- f_remove_outliers(df, bad_rows, set_na = TRUE)
#' sum(is.na(df_na$Salary))   # the flagged salaries are now missing
#'
#' # --- By hand: set the Age of rows 39 and 40 to NA ---
#' df_na2 <- f_remove_outliers(df, c(39, 40), set_na = TRUE, columns = "Age")
#'
#' @export

f_remove_outliers <- function(data,
                              outliers,
                              by = "row_id",
                              verbose = TRUE,
                              set_na = FALSE,
                              columns = NULL) {

  # --- Check Input Data ---
  if (is.numeric(data) || is.integer(data)) {
    stop(
      "Vector input is not supported in f_remove_outliers.\n",
      "Wrap your vector in a data.frame first: \n  f_remove_outliers(data.frame(value = x), outliers)"
    )
  }

  # --- Check Input Data ---
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data.frame.")
  }

  if (!is.logical(set_na) || length(set_na) != 1L || is.na(set_na)) {
    stop("'set_na' must be TRUE or FALSE.")
  }
  if (!set_na && !is.null(columns)) {
    stop("'columns' is only used with set_na = TRUE: without it, ",
         "f_remove_outliers() removes whole rows.")
  }
  if (!is.null(columns)) {
    if (!is.character(columns) || length(columns) == 0L) {
      stop("'columns' must be a character vector of column names.")
    }
    missing_cols <- setdiff(columns, names(data))
    if (length(missing_cols) > 0L) {
      stop("Column(s) not found in 'data': ", paste(missing_cols, collapse = ", "), ".")
    }
  }

  # --- Check and Prepare Outlier IDs ---
  ids_to_remove <- NULL

  # f_outliers() returns NULL when it finds nothing. Removing nothing is a
  # valid result, so return the data unchanged instead of erroring.
  if (is.null(outliers)) {
    if (verbose) {
      message("rfriend: No outliers to remove ('outliers' is NULL, as returned ",
              "by f_outliers() when none are found). Data returned unchanged.")
    }
    return(data)
  }

  # cells to clear when set_na = TRUE: a list of (column, ids) pairs
  na_targets <- list()

  if (inherits(outliers, "f_outliers")) {
    tabs <- unclass(outliers)
    # Extract all row_ids across every result table in the list
    ids_to_remove <- unique(unlist(lapply(tabs, function(df) df[[by]])))
    if (set_na) {
      tab_cols <- attr(outliers, "outlier_columns")
      if (is.null(tab_cols)) {
        # result from an older rfriend version: a table per column is named
        # after its column; a single "output_df" cannot be resolved
        tab_cols <- stats::setNames(names(tabs), names(tabs))
        if ("output_df" %in% names(tabs)) {
          stop("Cannot tell which column 'output_df' belongs to. Run f_outliers() ",
               "again, or give the row numbers with 'columns'.")
        }
      }
      flagged_cols <- unique(unname(tab_cols))
      if (!is.null(columns)) {
        not_checked <- setdiff(columns, flagged_cols)
        if (length(not_checked) > 0L) {
          stop("Column(s) not checked by f_outliers(): ", paste(not_checked, collapse = ", "),
               ". Checked: ", paste(flagged_cols, collapse = ", "), ".")
        }
      }
      for (tab in names(tabs)) {
        col <- unname(tab_cols[[tab]])
        if (!is.null(columns) && !(col %in% columns)) next
        na_targets[[length(na_targets) + 1L]] <- list(column = col, ids = tabs[[tab]][[by]])
      }
    }

  } else if (is.data.frame(outliers)) {
    if (!by %in% names(outliers)) {
      stop(paste0("The column '", by, "' was not found in the 'outliers' dataframe."))
    }
    ids_to_remove <- outliers[[by]]

  } else if (is.vector(outliers) && !is.logical(outliers)) {
    ids_to_remove <- outliers
  } else {
    stop("'outliers' must be a dataframe or a vector of IDs.")
  }

  if (set_na && !inherits(outliers, "f_outliers")) {
    if (is.null(columns)) {
      stop("With set_na = TRUE and row numbers or IDs, name the column(s) to clear ",
           "with 'columns', for example columns = \"biomass_g\".")
    }
    for (col in columns) {
      na_targets[[length(na_targets) + 1L]] <- list(column = col, ids = ids_to_remove)
    }
  }

  # --- Check 'by' column in Source Data ---
  # Track if we added the ID so we can remove it later
  added_id_col <- FALSE

  if (!by %in% names(data)) {
    if (by == "row_id") {
      data$row_id <- seq_len(nrow(data))
      added_id_col <- TRUE # Mark that we added this
    } else {
      stop(paste0("The ID column '", by, "' was not found in the 'data'."))
    }
  }

  # --- Set values to NA, keep the rows ---
  if (set_na) {
    out <- data
    n_set <- 0L
    report <- character(0)
    for (tg in na_targets) {
      hit <- which(out[[by]] %in% tg$ids)
      was_value <- !is.na(out[[tg$column]][hit])
      n_set <- n_set + sum(was_value)
      if (length(hit) > 0L) {
        out[[tg$column]][hit] <- NA
        report <- c(report, paste0(tg$column, ": ", by, " ",
                                   paste(out[[by]][hit], collapse = ", ")))
      }
    }
    if (added_id_col) out$row_id <- NULL
    if (verbose) {
      if (n_set == 0L) {
        message("rfriend: No values were set to NA (no IDs matched, or the values were already missing).")
      } else {
        message("rfriend: Set ", n_set, if (n_set == 1L) " value" else " values",
                " to NA (", paste(report, collapse = "; "), "). Rows kept: ", nrow(out), ".")
      }
    }
    return(out)
  }

  # --- The Filtering Logic ---
  rows_to_cut <- data[[by]] %in% ids_to_remove
  clean_data <- data[!rows_to_cut, ]

  # --- Cleanup ---
  # If 'row_id' was added remove it from the result
  # so the user gets back exactly what they put in (minus outliers).
  if (added_id_col) {
    clean_data$row_id <- NULL
  }

  # Reset the rownames to have continues numbers
  # (this prevents problems when using clean_data df)
  rownames(clean_data) <- NULL

  # --- User Feedback ---
  if (verbose) {
    n_removed <- sum(rows_to_cut)
    n_remaining <- nrow(clean_data)

    if (n_removed == 0) {
      message("rfriend: No outliers were removed (no IDs matched).")
    } else {
      message(paste0("rfriend: Removed ", n_removed,
                     if (n_removed == 1) " outlier row." else " outlier rows."))
      message(paste0("rfriend: Rows: ", nrow(data), " -> ", n_remaining))
    }
  }

  return(clean_data)
}
