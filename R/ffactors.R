#' Convert multiple columns to Factors in a data frame
#'
#' Converts multiple specified columns of a data frame into factors. If no columns are specified, it automatically detects and converts columns that are suitable to be factors. The function returns the entire data frame including non factor columns and can report the properties of this new data frame in the console (\code{properties = TRUE}).
#'
#' @param data A data frame containing the columns to be converted.
#' @param select A character vector specifying the names of the columns to convert into factors. If \code{NULL}, the function automatically detects columns that should be factors based on their data type and unique value count. Default is \code{NULL}.
#' @param exclude A character vector specifying the names of the columns NOT to convert into factors. If \code{NULL}, no columns are excluded. Default is \code{NULL}.
#' @param force_factors Logical. If \code{TRUE} all columns in the data.frame will be converted to factors except for the excluded columns using \code{exclude}.
#' @param ref Optional reference level(s) to set after conversion, controlling
#'   which level a factor is contrasted against in models (e.g. \code{f_glm},
#'   \code{f_lmer}). Under R's default treatment contrasts the reference is the
#'   first level, which is alphabetical unless set deliberately. Supply either a
#'   single string (applied to every converted factor that contains it) or a
#'   named character vector mapping column names to reference levels, e.g.
#'   \code{ref = c(treatment = "control", dose = "low")}. A level that is not
#'   present in a given factor is skipped with a warning. Default \code{NULL}
#'   (leave the reference as the first level).
#' @param properties Logical. If \code{TRUE}, prints a detailed table about the properties of the new data frame to the console. If \code{FALSE} no property table will be printed to the console. Default is \code{FALSE}.
#' @param unique_num_treshold  Numeric. A threshold of the amount of unique numbers a numeric column should have to keep it numeric, i.e. omit factor conversion. Default \code{8}.
#' @param repeats_threshold  Numeric. A threshold of the minimal number of repeats a numeric column should have to convert it to a factor. Default \code{2}.
#' @param ... Additional arguments passed to the \code{factor()} function of baseR.
#'
#' @details
#' \itemize{
#' \item If \code{select} is \code{NULL}, the function identifies columns with character data or numeric data with fewer than 8 unique values as candidates for conversion to factors.
#' \item The function checks if all specified columns exist in the data frame and stops execution if any are missing.
#' \item Converts specified columns into factors, applying any additional arguments provided.
#' \item Outputs a summary data frame with details about each column, including its type, class, number of observations, missing values, factor levels, and labels.
#'}
#' @return Returns the modified data frame with the specified (or all suitable) columns converted to factors. Can also force a print of a summary of the data frame's structure to the console (properties = TRUE).
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # Make a data.frame:
#' df <- data.frame(a = c("yes", "no", "yes", "yes", "no",
#'                        "yes", "yes", "no", "yes"),
#'                  b = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'                  c = c("apple", "kiwi", "banana", "apple", "kiwi",
#'                         "banana", "apple", "kiwi", "banana"),
#'                  d = c(1.1, 1.1, 3.4, 4.5, 5.4, 6.7, 7.8, 8.1, 9.8)
#'                  )
#' str(df)
#'
#' # Convert specified columns to factors:
#' df1 <- f_factors(df, select = c("a", "c"))
#' str(df1)
#'
#'
#' # Convert all potential factor columns to factor but exclude column "b":
#' df2 <- f_factors(df, exclude = c("b"))
#' str(df2)
#'
#' # Convert all columns to factor but exclude column "b":
#' df3 <- f_factors(df, exclude = c("b"), force_factors = TRUE)
#' str(df3)
#'
#' # Or automatically detect and convert suitable columns to factors.
#' # Thus obtaining the same results as above automatically:
#' df4 <- f_factors(df)
#' str(df4)
#'
#' # Set the reference level of a factor (the level models contrast against).
#' # Useful before f_glm() / f_lmer(): by default the reference is the first
#' # (alphabetical) level. Use a named vector to target specific columns:
#' df1c <- f_factors(df, select = c("a", "c"))
#' levels(df1c$c)[1]   # default reference is "apple" (alphabetical)
#' df_ref <- f_factors(df, select = c("a", "c"),
#'                     ref = c(a = "yes", c = "kiwi"))
#' levels(df_ref$c)[1] # reference is now "kiwi"
#' levels(df_ref$a)[1] # reference is now "yes" (was "no")
#'
#' # In example above col b was converted to a factor as the number of repeats = 2
#' # and the amount of unique numbers < 8. In order to keep b numeric we can also
#' # adjust the unique_num_treshold and/or repeats_threshold:
#' df5 <- f_factors(df, unique_num_treshold = 2)
#' str(df5)
#'
#' # Use `properties = TRUE` to view the data frame's structure.
#' # This forces a printed output which is more insight than standard str() output.
#' df6 <- f_factors(df, properties = TRUE)
#'
#' @seealso
#'  \href{https://stat.ethz.ch/R-manual/R-devel/library/base/html/factor.html}{\code{factor}}
#'
#'
#' @export
#'
f_factors <- function(data,
                      select = NULL,
                      exclude = NULL,
                      properties = FALSE,
                      force_factors = FALSE,
                      ref = NULL,
                      unique_num_treshold = 8,
                      repeats_threshold = 2,
                      ...) {


    if (is.null(select) && force_factors == FALSE) {
      select <- names(data)[vapply(data, function(col) {
        is.character(col) ||
          (is.numeric(col) && length(unique(col)) < unique_num_treshold) && any(table(col) > repeats_threshold)
      }, logical(1))]
    }


  if(force_factors == TRUE){
    select <- names(data)
  }


    # If exclude is specified, remove those columns from select
    if (!is.null(exclude)) {
      select <- setdiff(select, exclude)
    }

  # Check if all specified columns exist in the data frame
  if (!all(select %in% names(data))) {
    stop("Some specified columns do not exist in the data frame.")
  }

  # Convert selected columns to factors
  data[select] <- lapply(data[select], function(col) factor(col, ...))

  # Optionally set reference levels (the level a factor is contrasted against
  # in a model). Accepts a single string applied to any converted factor that
  # contains it, or a named vector mapping column -> reference level.
  if (!is.null(ref)) {
    if (is.null(names(ref))) {
      # Unnamed: apply each supplied level to every converted factor that has it.
      for (col_name in select) {
        col <- data[[col_name]]
        if (is.factor(col)) {
          hit <- ref[ref %in% levels(col)]
          if (length(hit) >= 1L)
            data[[col_name]] <- stats::relevel(col, ref = hit[[1L]])
        }
      }
    } else {
      # Named: column -> reference level.
      for (col_name in names(ref)) {
        if (!col_name %in% names(data)) {
          warning("f_factors(): column '", col_name,
                  "' in 'ref' not found; skipped.", call. = FALSE)
          next
        }
        col <- data[[col_name]]
        if (!is.factor(col)) {
          warning("f_factors(): column '", col_name,
                  "' is not a factor; reference not set.", call. = FALSE)
          next
        }
        if (!ref[[col_name]] %in% levels(col)) {
          warning("f_factors(): level '", ref[[col_name]],
                  "' not found in '", col_name, "'; reference not set.",
                  call. = FALSE)
          next
        }
        data[[col_name]] <- stats::relevel(col, ref = ref[[col_name]])
      }
    }
  }


  if(properties == TRUE){
  # Create a data frame summarizing column names, classes, types, observations, and missing values
    summary <- data.frame(
      Column = names(data),
      Type = vapply(data, typeof, FUN.VALUE = character(1)),
      Class = vapply(data, class, FUN.VALUE = character(1)),
      Obs = vapply(data, function(col)
        sum(!is.na(col)), FUN.VALUE = integer(1)),
      Missing = vapply(data, function(col)
        sum(is.na(col)), FUN.VALUE = integer(1)),
      Factor_Levels = vapply(data, function(col)
        if (is.factor(col))
          nlevels(col)
        else
          0, FUN.VALUE = double(1)),
      Factor_Labels = vapply(data, function(col)
        if (is.factor(col))
          paste(levels(col), collapse = ", ")
        else
          "", FUN.VALUE = character(1))
    )

  # Print the data frame without row names
  # print(summary, row.names = FALSE)
  cat(paste(capture.output(summary), collapse = "\n"))
  cat("\n   \n")
  }

  return(data)
}
