#' Transform 'Wide' (Excel) data to 'Long' (R) format
#'
#' @description
#' This function converts "wide" data (e.g. Excel tables) into a "long" list format.
#' This is the essential first step to prepare your data for analysis and plotting in R.
#'
#' @details
#' Research data in Excel or output from lab instruments often contains measurements
#' side-by-side (in columns). Many R functions require measurements in a single
#' column (rows). `f_long` performs this translation for you.
#'
#' It performs three actions in one go:
#' 1. Selects your measurement columns (`measure_columns`).
#' 2. Keeps your important ID columns (`keep_cols`) and removes the rest.
#' 3. (Optional) Renames cryptic column headers into readable labels (`category_labels`).
#'
#' @param data The input data frame (e.g., from \code{read_excel}).
#' @param measure_columns (Optional) The columns containing your numeric measurements.
#'   These values are often the response variables, i.e. will end up on the Y-axis.
#'   \itemize{
#'     \item Use column names: \code{c("OD_t0", "OD_t1")}
#'     \item Use column numbers: \code{2:5}
#'     \item Use helpers: \code{starts_with("Measure")}
#'   }
#'   If NULL (default), the function will pivot ALL columns except those in \code{keep_cols}.
#' @param keep_cols (Optional) The columns that identify your samples (IDs).
#'   E.g., "SampleID", "PatientID", "Treatment", "Student number".
#'   These are repeated for every measurement.
#'   *If left empty, all non-measured columns are kept.*
#'   Important: If \code{measure_columns} is NULL, you MUST specify \code{keep_cols}.
#' @param category_name Name for the new column containing the headers.
#'   Default is "name". Choose something logical like "Timepoints", "Genes", or "Condition".
#' @param value_name Name for the new column containing the numbers.
#'   Default is "value". Choose something logical like "Absorbance", "Ct_Value", or "Weight".
#' @param category_labels (Optional) A character vector of new, readable names for your categories, i.e. the measure_columns that you entered.
#'   \strong{Note:} The order must match the order of \code{measure_columns} exactly.
#'   Useful for renaming "t0_raw" to "Start" instantly.
#' @param ... Additional arguments passed to \code{tidyr::pivot_longer}.
#'   E.g., \code{values_drop_na = TRUE} to remove empty cells immediately.
#'
#' @return A "Tidy" data frame (tibble) of class \code{f_long}.
#'
#' @note The custom class and attributes (\code{f_long_value}, \code{f_long_category})
#'   are used by the \code{plot} and \code{summary} methods. Be aware that most
#'   \pkg{dplyr} or \pkg{tidyr} operations (e.g., \code{filter}, \code{mutate})
#'   will silently strip these attributes. If that happens, use \code{f_scan} or
#'   \code{f_summary} directly with explicit column names instead.

#' @export
#' @examples
#' # --- Example 1: Using the 'iris' dataset ---
#' # Scenario: The iris dataset looks clean, but it is actually "Wide".
#' # It has 4 columns of measurements side-by-side.
#' # To compare Sepal Length vs Width in a plot, we must stack them.
#'
#' head(iris)
#'
#' # Reshape: Combine Length and Width into one column and plot the data.
#' iris_long <- f_long(
#'   data = iris,
#'   measure_columns = c("Sepal.Length", "Sepal.Width"),
#'   keep_cols = "Species",
#'   category_name = "Sepal_Dimension",    # Describes the grouping (What did we measure?)
#'   value_name = "Size_cm",               # Describes the value (What is the number?)
#'   category_labels=c("Length", "Width")  # New category labels
#'   )
#'
#'  head(iris_long)
#'
#'  # Plot the data using f_scan
#'  plot(iris_long)
#'
#'  # Make a f_summary table of iris_long
#'  summary(iris_long)
#'
#' # --- Example 2: Using the 'airquality' dataset ---
#' # Scenario: Pivot daily measurements of Wind and Temperature over time.
#'
#' head(airquality)
#'
#' weather_long <- f_long(
#'   data = airquality,
#'   measure_columns = c("Wind", "Temp"),
#'   keep_cols = c("Month", "Day"),
#'   category_name = "Climate_Parameter", # Descriptive name
#'   value_name = "Reading_Value",        # Generic name (since units differ: mph vs F)
#'   values_drop_na = TRUE
#' )
#'
#' head(weather_long)

f_long <- function(data,
                   measure_columns = NULL,
                   keep_cols = NULL,
                   category_name = "name",
                   value_name = "value",
                   category_labels = NULL,
                   ...) {

  if (!is.data.frame(data)) stop("rfriend error: Input 'data' must be a data frame.")

  # --- LOGIC SWITCH: MANUAL VS AUTOMATIC ---

  # Variable to store columns to be pivoted (for checking purposes later)
  target_cols <- NULL

  # Scenario 1: Lazy Mode (Pivot everything except keep_cols)
  if (is.null(measure_columns)) {

    if (is.null(keep_cols)) {
      stop("rfriend error: Lazy Mode active (no measure_columns selected); that is why you MUST specify 'keep_cols' so I know which columns NOT to pivot.")
    }

    # First calculate which columns these are for the safety check
    # Use dplyr::select to simulate what remains
    temp_data <- dplyr::select(data, -{{ keep_cols }})
    target_cols <- names(temp_data)

    message(paste0("rfriend: Lazy Mode active. Pivoting all columns EXCEPT: ", paste(names(dplyr::select(data, {{ keep_cols }})), collapse=", ")))

  } else {
    # Scenario 2: Manual Mode
    # Here we are sure what the user wants, but we retrieve names for safety
    temp_data <- dplyr::select(data, {{ measure_columns }})
    target_cols <- names(temp_data)

    if (!is.null(keep_cols)) {
      data <- dplyr::select(data, {{ keep_cols }}, all_of(target_cols))
    }

  }

  # --- THE SAFETY CHECK FOR LABELS ---
  if (!is.null(category_labels)) {

    # 1. Length check
    if (length(target_cols) != length(category_labels)) {
      stop(paste0("rfriend error: Count mismatch! Found ", length(target_cols), " columns to pivot, but you provided ", length(category_labels), " labels."))
    }

    # 2. Visual Check (Mapping)
    # This is crucial for the user
    message("---------------------------------------------------")
    message("rfriend SAFETY CHECK: \nPlease verify the order and names below.")
    message("\nColumn Name          -->  New Label")
    message("---------------------------------------------------")

    # Print the first 10 mappings (to avoid spamming with large datasets)
    limit <- min(10, length(target_cols))
    for(i in 1:limit) {
      # Ensure clean alignment
      col_fixed <- format(target_cols[i], width = 20)
      message(paste0(col_fixed, " -->  ", category_labels[i]))
    }
    if (length(target_cols) > 10) message("... (and ", length(target_cols) - 10, " others)")
    message("---------------------------------------------------")

    # If Manual Mode, we trust the user.
    # If Lazy Mode, we give an extra warning.
    if (is.null(measure_columns)) {
      warning("rfriend tip: You are using automatic selection with renaming. Check the table above carefully! R determines the column order.", call. = FALSE)
    }
  }

  # --- EXECUTION ---

  tryCatch({
    # Use target_cols here because we already calculated them.
    # This is safer than repeating the logic.
    result <- tidyr::pivot_longer(
      data = data,
      cols = all_of(target_cols), # Use all_of because we have names as strings now
      names_to = category_name,
      values_to = value_name,
      ...
    )

    # Apply Labels
    if (!is.null(category_labels)) {
      result[[category_name]] <- factor(result[[category_name]],
                                             levels = target_cols,
                                             labels = category_labels)
    }

    # --- ADD CLASS AND ATTRIBUTES  ---
    # Store the column names so plot() knows what to use later
    attr(result, "f_long_value") <- value_name
    attr(result, "f_long_category") <- category_name

    # Add "f_long" to the class list (keeping existing classes like tbl_df)
    class(result) <- c("f_long", class(result))

    return(result)

  }, error = function(e) {
    stop(paste0("rfriend technical error: ", e$message))
  })
}


#' Plot method for f_long objects
#'
#' @description
#' Automatically runs a \code{f_scan} diagnostic plot on data created by \code{f_long}.
#'
#' @param x An object of class \code{f_long} (output from \code{f_long}).
#' @param summary Logical. If \code{TRUE}, generates the summary table within the scan. Default is \code{TRUE}.
#' @param ... Additional arguments passed to \code{f_scan}.
#'
#' @return
#' Returns the output of \code{f_scan} (an object of class \code{f_scan}) invisibly.
#'
#' @export
#' @method plot f_long
plot.f_long <- function(x, summary = TRUE, ...) {

  # 1. Retrieve the hidden column names from attributes
  val_col <- attr(x, "f_long_value")
  cat_col <- attr(x, "f_long_category")

  # 2. Safety check (in case attributes are lost)
  if (is.null(val_col) || is.null(cat_col)) {
    stop("rfriend error: This data seems to have lost its f_long attributes. Please specify columns manually in f_scan().")
  }

  # 3. Call f_scan
  result <- f_scan(data = x,
                   column = val_col,
                   group_vars = cat_col,
                   summary = FALSE,
                   outliers = FALSE,
                   ...)

  # 4. Return the result
  return(result)
}


#' Summary method for f_long objects
#'
#' @description
#' Automatically runs the \code{f_summary} function on data created by \code{f_long}
#' using the attributes stored in the object.
#'
#' @param object An object of class \code{f_long} (output from \code{f_long}).
#' @param ... Additional arguments passed to \code{f_summary}.
#'
#' @return
#' Returns the summary table (usually a data frame or tibble) produced by \code{f_summary}.
#'
#' @export
#' @method summary f_long
summary.f_long <- function(object, ...) {

  # 1. Retrieve the hidden column names from attributes
  val_col <- attr(object, "f_long_value")
  cat_col <- attr(object, "f_long_category")

  # 2. Safety check (in case attributes are lost)
  if (is.null(val_col) || is.null(cat_col)) {
    stop("rfriend error: This data seems to have lost its f_long attributes. Please specify columns manually in f_summary().")
  }

  # 3. Call f_summary
  # We return the result visibly. When the user types summary(x),
  # R will automatically print the returned table.
  f_summary(object,
            val_col,
            cat_col,
            ...)
}
