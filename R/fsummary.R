#' Summarize a Data Frame with Grouping Variables
#'
#' Computes summary statistics (e.g., mean, standard deviation, median, etc.) for a specified column ("character string") in a data frame, grouped by one or more grouping variables in that data frame ("character strings"). Summary parameters can be customized and the results can be exported to an 'Excel' file.
#'
#' @param data A 'data.frame', 'data.table' or 'tibble', i.e. input data to be summarized.
#' @param data.column A character string, vector or list with characters. The name of the column(s) in \code{data} for which summary statistics will be calculated.
#' @param ... One or more character strings specifying the grouping variables in \code{data}. At least one grouping variable must be provided.
#' @param show_n Logical. If \code{TRUE}, the summary results \code{n} will be included in the output.
#' @param show_mean Logical. If \code{TRUE}, the summary results \code{mean} will be included in the output.
#' @param show_sd Logical. If \code{TRUE}, the summary results \code{sd} will be included in the output.
#' @param show_se Logical. If \code{TRUE}, the summary results \code{se} will be included in the output.
#' @param show_min Logical. If \code{TRUE}, the summary results \code{min} will be included in the output.
#' @param show_max Logical. If \code{TRUE}, the summary results \code{max} will be included in the output.
#' @param show_median Logical. If \code{TRUE}, the summary results \code{median} will be included in the output.
#' @param show_Q1 Logical. If \code{TRUE}, the summary results \code{Q1} will be included in the output.
#' @param show_Q3 Logical. If \code{TRUE}, the summary results \code{Q3} will be included in the output.
#' @param digits Integer. Round to the number of digits specified. If \code{digits = NULL} no rounding is applied (default is \code{digits = 2}). Note that this rounding is independent of the rounding in the exported excel file.
#' @param export_to_excel Logical. If \code{TRUE}, the (unrounded values) summary results will be exported to an 'Excel' file. Default is \code{FALSE}.
#' @param open_excel Logical. If \code{TRUE} and \code{export_to_excel} is also \code{TRUE}, the generated 'Excel' file will be opened automatically. Default is \code{TRUE}.
#' @param output_file Character string specifying the name of the output file. Default is "dataname_summary.xlsx".
#' @param output_dir Character string specifying the name of the directory of the output file. Default is  \code{tempdir()}. If the \code{output_file} already contains a directory name \code{output_dir} can be omitted, if used it overwrites the dir specified in \code{output_file}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory Default is \code{FALSE}, to avoid unintended changes to the global environment. If the \code{output_dir} is specified \code{save_in_wdir} is overwritten with \code{output_dir}.
#' @param close_generated_files Logical. If \code{TRUE}, closes open 'Excel' files. This to be able to save the newly generated file. Default is \code{FALSE}.
#' @param open_generated_files Logical. If \code{TRUE}, Opens the generated 'Excel' files. This to directly view the results after creation. Files are stored in tempdir(). Default is \code{TRUE}.
#' @param check_input If \code{TRUE}, checks the input and stops the function if the input is incorrect (default is \code{TRUE}).
#' @param eval_input Logical. If \code{TRUE}, the function evaluates the third function argument. This should be a character vector with the group by columns. Default is \code{FALSE}, which allows group by columns to be written without quotes.
#' @param digits_excel Integer. Round cells in the excel file to the number of digits specified. If \code{digits_excel = NULL} no rounding is applied (default is \code{digits_excel = NULL}). Note to preserve formatting numbers will be stored as text.
#' @param detect_int_col Logical. If \code{TRUE}, columns in a data.frame containing only integers will be displayed without decimal digits. Columns containing a mix of integers and decimal values will display all values with the specified number of digits. If \code{FALSE}, each individual cell is evaluated: integer values are displayed without digits, and numbers containing digits with the specified number of digits. Default is \code{TRUE}.
#'
#' @details
#' The function computes the following summary statistics for the specified column:
#'\itemize{
#' \item \code{n}: number of observations
#' \item \code{mean}: mean
#' \item \code{sd}: standard deviation
#' \item \code{se}: standard error of the mean
#' \item \code{min}: minimum value
#' \item \code{max}: maximum value
#' \item \code{median}: median
#' \item \code{Q1}: first quartile
#' \item \code{Q3}: third quartile
#'}
#' Each of these summary statistics can be removed by setting e.g. \code{show_n = FALSE}, The results are grouped by the specified grouping variables and returned as a data frame. If \code{export_to_excel} is set to \code{TRUE}, the results are saved as an 'Excel' file in the working directory with a dynamically generated filename.
#'
#' @return A data frame containing the computed summary statistics, grouped by the specified variables. This data frame can be automatically saved as an 'Excel' file using \code{export_to_excel = TRUE}.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # Example usage:
#' # Create a summary of mtcars for data column hp grouped by cyl and gear,
#' # and remove Q1 and Q3 from the output.
#' # Note that variable can be written as "hp" or as hp. Only data.frame must be data (no quotes)
#' summary_mtcars <- f_summary(mtcars, "hp", "cyl", "gear", show_Q1 = FALSE, show_Q3 = FALSE)
#' print(summary_mtcars)
#'
#' # Create a summary for iris
#' summary_iris <- f_summary(iris, Sepal.Length, Species)
#'
#' # Print the a table with column width of 10 characters and table length of 70 characters
#' print(summary_iris, col_width =  10, table_width = 70)
#'
#' @export
#'
f_summary <- function(data, data.column, ...,
                      show_n = TRUE,
                      show_mean = TRUE,
                      show_sd = TRUE,
                      show_se = TRUE,
                      show_min = TRUE,
                      show_max = TRUE,
                      show_median = TRUE,
                      show_Q1 = TRUE,
                      show_Q3 = TRUE,
                      digits = 2,
                      export_to_excel = FALSE,
                      close_generated_files = FALSE,
                      open_generated_files = TRUE,
                      output_file = NULL,
                      output_dir = NULL,
                      save_in_wdir = FALSE,
                      open_excel = TRUE,
                      check_input = TRUE,
                      eval_input = FALSE,
                      digits_excel = NULL,
                      detect_int_col = TRUE
                      ) {

  # Check if exactly 3 arguments were supplied
  if (nargs() < 3) {
    stop("f_summary requires at least 3 arguments:
         data        = data.frame,
         data.column = data column to summarize,
         and one or more grouping variables (data columns within the data.frame)
         ")
  }


  # Define the summary statistics function
  summary_fun <- function(x) {
    result <- c()
    if(show_n) result["n"] <- length(x)
    if(show_mean) result["mean"] <- mean(x, na.rm = TRUE)
    if(show_sd) result["sd"] <- sd(x, na.rm = TRUE)
    if(show_se) result["se"] <- sd(x, na.rm = TRUE) / sqrt(length(x))
    if(show_min) result["min"] <- min(x, na.rm = TRUE)
    if(show_Q1) result["Q1"] <- quantile(x, probs = 0.25, na.rm = TRUE)
    if(show_median) result["median"] <- median(x, na.rm = TRUE)
    if(show_Q3) result["Q3"] <- quantile(x, probs = 0.75, na.rm = TRUE)
    if(show_max) result["max"] <- max(x, na.rm = TRUE)
    return(result)
  }

  # Define function to allow noquotes or quotes for column names
  check_var <- function(expr) {
    expr_chr <- deparse(expr)

    if (is.character(expr)) {
      return(expr)
    }

    # if (exists(expr_chr, envir = .GlobalEnv, inherits = FALSE)) {
    #   obj <- get(expr_chr, envir = .GlobalEnv)

      if (exists(expr_chr, envir = parent.frame(2), inherits = TRUE)) {
        obj <- get(expr_chr, envir = parent.frame(2))


      if (!is.character(obj)) {
        stop(paste0("Error: ", dQuote(expr_chr),
                    " exists but is not a character vector. Use a column name in quotation marks.\n"))
      } else {
        return(obj)
      }
    } else {
      return(expr_chr)
    }
  }

  # Create a list to store all outputs in this function
  output_list <- list()

  # Colletct the data col to be summarized
  expr_data.column <- substitute(data.column, env = environment())
  data.column      <- check_var(expr_data.column)


  # Collect grouping columns from the ... argument
  # List of known options to exclude if named
  excluded_options <- c(
    "show_n", "show_mean", "show_sd", "show_se", "show_min", "show_max",
    "show_median", "show_Q1", "show_Q3", "digits", "export_to_excel",
    "close_generated_files", "output_file", "output_dir", "save_in_wdir",
    "open_excel", "check_input", "digits_excel"
  )

  if(eval_input == TRUE){
    # Collect grouping columns from the ... argument
    group_vars_list <- list(...)
    # Convert group_vars into a character vector (not a list)
    group_vars <- unlist(group_vars_list)
  } else {
  # Capture unevaluated expressions
  exprs <- as.list(substitute(alist(...)))[-1]

  # Convert expressions to character strings
  group_vars <- vapply(exprs, function(expr) {
    if (is.character(expr)) {
      expr
    } else {
      paste(deparse(expr), collapse = "")
    }
  }, character(1))

  # Keep only unnamed arguments or those not in the excluded list
  keep <- is.null(group_vars) | group_vars == "" | !(group_vars %in% excluded_options)

  group_vars <- group_vars[keep]
  }

  ######## Do some checks to increase user friendlyness #####################
  # Check if the input is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame, data.table or tibble")
  }

  # Get the column names of the data frame
  data_col_names <- names(data)

  if(check_input == TRUE){
    # Check if data.column exists in the data frame
    var_exists <- data.column %in% data_col_names
    if (!var_exists) {
        stop(paste0("Warning: column '", data.column, "' does not exist in the data frame."))
    }

    # Check if all group_vars_list exist in the data frame
    all_group_vars_exist <- all(group_vars %in% data_col_names)

    if (all_group_vars_exist == FALSE) {
      missing_group_vars <- group_vars[!group_vars %in% data_col_names]
        stop(paste("Warning: The following group column(s) do not exist in the data frame: ",
                   paste(missing_group_vars, collapse = ", "), sep = ""))
    }

    # Check if the data.column is numeric
    if (!is.numeric(data[[data.column]])) {
      stop("Summary column must be a numeric")
    }

    # Ensure the grouping variables are character strings
    if (any(!vapply(group_vars, is.character, logical(1)))) {
      stop("All group variables should be 'character strings'.")
    }

    # Check if at least one grouping variable is provided
    if (length(group_vars) == 0) {
      stop("At least one grouping variable must be provided.")
    }

    # Check if the data column is provided
    if (missing(data.column) || !any(vapply(data.column, is.character, logical(1)))) {
      stop("A valid column name ('character string') must be provided for 'data.column'")
    }
  }

  for(data.col.x in data.column){

  # Perform the aggregation
  result <-  do.call(data.frame, aggregate(data[[data.col.x]], by = data[group_vars], FUN = summary_fun))

  # Rename the result columns
  # Rename the grouping columns to match their names
  colnames(result)[seq_along(group_vars)] <- group_vars

  # Rename the summary statistics columns to be prefixed with the data column name
  summary_cols <- colnames(result)[(length(group_vars) + 1):ncol(result)]

  # Remove the 'x.' prefix from summary columns
  summary_cols <- gsub("^x\\.", "", summary_cols)

  # Apply the new names to the summary columns
  colnames(result)[(length(group_vars) + 1):ncol(result)] <- paste(data.col.x, summary_cols, sep = ".")

  output_list[[data.col.x]] <- result
  }

  if (length(output_list) == 1) {
    output_list <- data.frame(output_list)
    colnames(output_list) <- colnames(result)
  }


  # Store output as data.frame in output_list.
  output_list[["output_df"]] <- as.data.frame(output_list)
  output_df_no_rounding <- as.data.frame(output_list[["output_df"]])

  # Round unless user enters digits = NULL
  if(!is.null(digits)){
    output_list[["output_df"]] <- f_conditional_round(output_list[["output_df"]],
                                                      digits = digits,
                                                      detect_int_col = detect_int_col)
  }

  class(output_list) <- "f_summary"

  # If export_to_excel is TRUE, save the result to an Excel file


  if (!is.null(output_file) || !is.null(digits_excel) ) {
    export_to_excel = TRUE
  }


  if (export_to_excel) {

    if(!is.null(digits_excel)){
      output_df_no_rounding <- f_conditional_round(output_df_no_rounding,
                                                   digits = digits_excel,
                                                   detect_int_col = detect_int_col)
    }

    # If there is not output_file name specified use the data_name
    if (is.null(output_file)) {
    # Construct the file name dynamically using deparse(substitute())
    data_name <- deparse(substitute(data))  # Get the name of the data frame
    output_file <- paste(data_name, "summary.xlsx", sep = "_")  # Construct the file name
    }

   if(close_generated_files == TRUE){
     # Close all MS Excel files to avoid conflicts (so save your work first)
     system("taskkill /im EXCEL.EXE /f")

   }

    # If there is no output_dir specified and user setting is to save in working directory
    if(is.null(output_dir) && save_in_wdir == TRUE){
      # set the working dir to the location the file is saved
      output_dir <- getwd()

    } else if(is.null(output_dir) && save_in_wdir == FALSE){
      # Get the dirname of output_file
      output_dir <- dirname(output_file)

      # Check if there is a dir (path) in the output file, if not use tempdir()
      if(output_dir == "."){
        output_dir <- tempdir()
      }

    }

    # Stop if the output directory does not exist
    if (!dir_exists(output_dir)) {
      stop("The directory '", output_dir, "' does not exist.")
    }

    # dir_name is already extracted so rename file to basename.
    output_file <- basename(output_file)

    ensure_xlsx <- function(filename) {
      if (!grepl("\\.xlsx$", filename, ignore.case = TRUE)) {
        filename <- paste0(filename, ".xlsx")
      }
      return(filename)
    }

    output_file <- ensure_xlsx(output_file)

    message(paste0("Saved output in: ", output_dir, "\\", output_file))

    # Write to an Excel file with each table in its own sheet
    write_xlsx(output_df_no_rounding, path = paste0(output_dir, "/", output_file))

    # Open files after creation
    if(open_generated_files == TRUE){
    f_open_file(paste0(output_dir, "/", output_file))
    }
  }





  # Return the result as a data frame
  return(output_list)
}


#' Print method for f_summary objects
#'
#' This function prints \code{f_summary} objects.
#'
#' @param x Object of class f_summary
#' @param col_width Integer. Specifies the maximum number of characters allowed in table header columns before a line break is inserted. Defaults to \code{10}.
#' @param table_width Integer or \code{NULL}. Defines the number of characters after which the table is split into separate sections. Defaults to \code{NULL}, meaning no break is applied.
#' @param ... Additional arguments passed to the \code{pander} function.
#' @return This function is called for its side effect of printing a formatted output to the console
#' and does not return a useful value. It invisibly returns \code{1}.
#' @export
print.f_summary <- function(x, col_width = 6, table_width = 90, ...) {
  f_pander(x$output_df, col_width = col_width, table_width = table_width)

}
