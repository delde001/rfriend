# -------------------------------------------------------------------------
# Data check for f_scan(): reports problems in the raw data before any
# analysis. It only reports; it never changes the data.
# -------------------------------------------------------------------------

# Internal: build the data check.
#   data      data.frame as the user gave it (before fancy_names, before
#             grouping columns are converted to factors)
#   columns   columns to check (all columns for a bare data.frame call;
#             the formula variables for a formula call)
#   id_col    optional column that identifies the experimental unit
#   n_extreme number of lowest and highest values stored per numeric column
scan_data_check <- function(data, columns, id_col = NULL, n_extreme = 5L) {

  columns <- unique(c(columns, id_col))
  columns <- columns[columns %in% names(data)]

  # Column names that read.csv() changed: dots where the file had spaces or
  # brackets ("Height (cm)" -> "Height..cm.").
  dotted <- grep("\\.\\.|\\.$", columns, value = TRUE)

  label_cols <- setdiff(columns[vapply(data[columns], function(v)
    is.character(v) || is.factor(v), logical(1))], id_col)
  num_cols <- columns[vapply(data[columns], is.numeric, logical(1))]

  text <- lapply(stats::setNames(label_cols, label_cols), function(col) {
    v    <- as.character(data[[col]])
    tab  <- table(v, useNA = "ifany")
    vals <- names(tab)[!is.na(names(tab))]

    # 1. leading or trailing spaces
    spaces <- vals[vals != trimws(vals)]

    # 2. labels that are the same once case, spaces, dots, hyphens and
    #    underscores are ignored ("low_N" and "Low N")
    key     <- tolower(gsub("[[:space:]_.-]", "", vals))
    similar <- split(vals, key)
    similar <- unname(similar[lengths(similar) > 1L])

    # 3. labels written differently from most others: a capital letter or an
    #    inner space where fewer than half of the labels have one. Rule of
    #    thumb; only with three or more labels.
    style <- character(0)
    tv    <- trimws(vals)
    if (length(tv) >= 3L) {
      has_cap   <- grepl("[A-Z]", tv)
      has_space <- grepl(" ", tv)
      if (any(has_cap)   && mean(has_cap)   < 0.5) style <- c(style, vals[has_cap])
      if (any(has_space) && mean(has_space) < 0.5) style <- c(style, vals[has_space])
    }
    style <- unique(setdiff(style, unlist(similar)))

    # 4. numbers stored as text: most values read as numbers, a few do not
    #    ("n/a", "12,5")
    not_numeric <- character(0)
    if (length(vals) > 0L) {
      as_num <- suppressWarnings(as.numeric(trimws(vals)))
      if (mean(!is.na(as_num)) >= 0.8 && any(is.na(as_num)))
        not_numeric <- vals[is.na(as_num)]
    }

    list(values      = tab,
         n_missing   = sum(is.na(v)),
         spaces      = spaces,
         similar     = similar,
         style       = style,
         not_numeric = not_numeric)
  })

  numeric <- lapply(stats::setNames(num_cols, num_cols), function(col) {
    v <- data[[col]]
    s <- sort(v)                      # sort() drops NA
    list(n_missing = sum(is.na(v)),
         lowest    = utils::head(s, n_extreme),
         highest   = rev(utils::tail(s, n_extreme)))
  })

  id <- NULL
  if (!is.null(id_col)) {
    k       <- table(data[[id_col]], useNA = "ifany")
    counts  <- table(as.integer(k))
    usual   <- as.integer(names(counts)[which.max(counts)])
    id <- list(id_col      = id_col,
               n_ids       = length(k),
               rows_per_id = k,
               unusual     = names(k)[as.integer(k) != usual],
               usual       = usual)
  }

  structure(list(n_rows    = nrow(data),
                 n_cols    = ncol(data),
                 columns   = columns,
                 dotted    = dotted,
                 text      = text,
                 numeric   = numeric,
                 id        = id,
                 n_extreme = n_extreme),
            class = "f_data_check")
}

# Internal: quote labels for printing, so leading spaces stay visible.
dc_quote <- function(x) paste0("\"", x, "\"", collapse = ", ")

#' Print the data check of f_scan()
#'
#' Prints the data check that \code{\link{f_scan}} stores as
#' \code{attr(result, "data_check")}.
#'
#' @param x An \code{f_data_check} object.
#' @param extremes Logical. Also print the lowest and highest values of each
#'   numeric column? Default \code{FALSE}.
#' @param max_values Integer. Maximum number of distinct values printed per
#'   text column; the rest are counted. Default \code{12}.
#' @param ... Not used.
#' @return \code{x}, invisibly.
#' @export
#' @method print f_data_check
print.f_data_check <- function(x, extremes = FALSE, max_values = 12L, ...) {
  cat("\n--- Data check ---\n")
  cat(x$n_rows, "rows;", length(x$columns), "of", x$n_cols,
      "columns checked. The check reports; it does not change the data.\n")

  if (length(x$dotted)) {
    cat("\nColumn names with dots (from spaces or brackets in the file): ",
        paste(x$dotted, collapse = ", "),
        "\n  -> rename them with f_rename_columns().\n", sep = "")
  }

  if (length(x$text)) {
    cat("\nText columns (value and number of rows):\n")
    for (col in names(x$text)) {
      tx   <- x$text[[col]]
      tab  <- tx$values
      labs <- ifelse(is.na(names(tab)), "NA", paste0("\"", names(tab), "\""))
      shown <- utils::head(seq_along(tab), max_values)
      line <- paste(sprintf("%s (%d)", labs[shown], as.integer(tab)[shown]),
                    collapse = ", ")
      if (length(tab) > max_values)
        line <- paste0(line, ", ... and ", length(tab) - max_values,
                       " more values")
      cat("  ", col, ": ", line, "\n", sep = "")
      if (length(tx$spaces))
        cat("    ! leading or trailing space: ", dc_quote(tx$spaces), "\n", sep = "")
      for (s in tx$similar)
        cat("    ! may be the same label: ", dc_quote(s), "\n", sep = "")
      if (length(tx$style))
        cat("    ! written differently from the other labels: ",
            dc_quote(tx$style), "\n", sep = "")
      if (length(tx$not_numeric))
        cat("    ! numbers stored as text; these values are not numbers: ",
            dc_quote(tx$not_numeric), "\n", sep = "")
    }
  }

  if (length(x$numeric)) {
    cat("\nNumeric columns:\n")
    for (col in names(x$numeric)) {
      nm <- x$numeric[[col]]
      if (extremes) {
        cat(sprintf("  %s: %d missing; lowest %s; highest %s\n", col,
                    nm$n_missing, paste(nm$lowest, collapse = ", "),
                    paste(nm$highest, collapse = ", ")))
      } else {
        cat(sprintf("  %s: %d missing\n", col, nm$n_missing))
      }
    }
    if (!extremes)
      cat("  The ", x$n_extreme, " lowest and highest values per column: ",
          "print(result, extremes = TRUE).\n", sep = "")
  }

  if (!is.null(x$id)) {
    cat("\nRows per ", x$id$id_col, ":\n", sep = "")
    per <- table(as.integer(x$id$rows_per_id))
    cat("  ", x$id$n_ids, " IDs; ",
        paste(sprintf("%s IDs with %s rows", as.integer(per), names(per)),
              collapse = ", "), "\n", sep = "")
    if (length(x$id$unusual))
      cat("    ! not ", x$id$usual, " rows: ",
          paste(x$id$unusual, collapse = ", "), "\n", sep = "")
  }
  cat("\n")
  invisible(x)
}
