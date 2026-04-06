#' Fancy Pander Table Output
#'
#' Is a wrapper around the \code{pander} function from the 'pander' package, designed to produce a fancy table output with specific formatting options.
#'
#' @param table A data frame, matrix, or other table-like structure to be rendered.
#' @param col_width Integer. Specifies the maximum number of characters allowed in table header columns before a line break is inserted. Defaults to \code{10}. Note that latex will not break the table header if the col with is longer due to long names below the header.
#' @param table_width Integer or \code{NULL}. Defines the number of characters after which the table is split into separate sections. Defaults to \code{NULL}, meaning no break is applied.
#' @param limit_columns Integer or \code{NULL}. Defines the number of columns shown in a table.
#' @param style Character. Pander table style. Defaults to \code{"multiline"}.
#' @param console Logical. Whether to process headers for console output. Defaults to \code{TRUE}.
#' @param ... Additional arguments passed to the \code{pander} function.
#'
#' @details
#' This function sets several \code{pander} options to ensure that the table output is formatted in a visually appealing manner. The options set include:
#' \itemize{
#'   \item \code{table.alignment.default}: Aligns all columns to the left.
#'   \item \code{table.alignment.rownames}: Aligns row names to the left.
#'   \item \code{keep.trailing.zeros}: Keeps trailing zeros in numeric values.
#'   \item \code{knitr.auto.asis}: Ensures output is not automatically treated as 'asis'.
#'   \item \code{table.caption.prefix}: Removes the default "Table" prefix in captions.
#'   \item \code{keep.line.breaks}: Preserves line breaks in cell content.
#'   \item \code{table.split.table}: Controls table splitting (set to \code{Inf} if \code{table_width} is \code{NULL} or \code{FALSE}).
#'   \item \code{table.split.cells}: Inserts line breaks in headers every \code{col_width} characters.
#' }
#'
#' This function requires [Pandoc](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3 or higher), a universal document converter.
#' \itemize{
#' \item \bold{Windows:} Install Pandoc and ensure the installation folder \cr (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your system PATH.
#' \item \bold{macOS:} If using Homebrew, Pandoc is typically installed in "/usr/local/bin". Alternatively, download the .pkg installer and verify that the binary's location is in your PATH.
#' \item \bold{Linux:} Install Pandoc through your distribution's package manager (commonly installed in "/usr/bin" or "/usr/local/bin") or manually, and ensure the directory containing Pandoc is in your PATH.
#' \item If Pandoc is not found, this function may not work as intended.
#' }
#'
#' @return None. The function is called for its side effects of setting 'pander' options and creates a pander formatted table in R Markdown.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # Example usage of f_pander
#' df <- data.frame(
#'   Name = c("Alice", "Bob", "Charlie"),
#'   Age = c(25, 30, 35),
#'   Score = c(88.5, 92.3, 85.0)
#' )
#'
#' # Render the data frame as a fancy table
#' f_pander(df)
#' @export
f_pander <- function(table,
                     col_width = 10,
                     table_width = NULL,
                     limit_columns = NULL,
                     style = "multiline",
                     console = TRUE,
                     ...) {
  # 1. SAVE EXISTING OPTIONS
  old_options <- panderOptions()

  # 2. RESTORE OPTIONS
  # We must loop through the list and restore them one by one.
  on.exit({
    for (o in names(old_options)) {
      panderOptions(o, old_options[[o]])
    }
  })

  #Set Pander options for a fancy output tabel
  panderOptions('table.alignment.default', 'left')
  panderOptions('table.alignment.rownames', 'left')
  panderOptions('keep.trailing.zeros', TRUE)
  panderOptions('knitr.auto.asis', FALSE)
  panderOptions('table.caption.prefix', '')  # Remove prefix Table in caption
  panderOptions('keep.line.breaks', TRUE)

  if(is.null(table_width) || table_width == FALSE){
    panderOptions("table.split.table", Inf)
  } else {
    panderOptions("table.split.table", table_width)
  }

  if(!is.null(col_width)){
    # Insert newline every X characters
    panderOptions('table.split.cells', col_width)
    names(table) <- insert_newline(names(table), col_width, console = console)

  }

  if(!is.null(limit_columns)){
    if(limit_columns < ncol(table) ){
    table <- table[, 1:limit_columns, drop = FALSE]
    }
  }

  pander(table, style = style, ...)
}
