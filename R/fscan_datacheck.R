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
scan_data_check <- function(data, columns, id_col = NULL, n_extreme = 4L) {

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

# Internal: numbers as short text, each on its own scale (20, not 20.0).
dc_num <- function(v) {
  if (!length(v)) return("-")
  paste(vapply(v, function(z) format(z, digits = 6, trim = TRUE), character(1)),
        collapse = ", ")
}

# Internal: one table row; the last cell wraps under itself when too long.
dc_row <- function(cells, widths, width = getOption("width", 80L)) {
  n    <- length(cells)
  pad  <- function(z, w) paste0(z, strrep(" ", max(0L, w - nchar(z))))
  lead <- if (n > 1L) paste(mapply(pad, cells[-n], widths[-n]), collapse = "  ") else ""
  lead <- paste0("  ", lead, if (n > 1L) "  " else "")
  room <- max(20L, width - nchar(lead))
  last <- strwrap(cells[n], width = room)
  if (!length(last)) last <- ""
  cat(lead, last[1], "\n", sep = "")
  for (l in last[-1]) cat(strrep(" ", nchar(lead)), l, "\n", sep = "")
}

# Internal: every flag as one line "column: problem", in the order of the data.
dc_flags <- function(x) {
  f <- character(0)
  if (length(x$dotted))
    f <- c(f, paste0(paste(x$dotted, collapse = ", "),
                     ": dots in the column name (from spaces or brackets in the file); ",
                     "rename with f_rename_columns()"))
  for (col in names(x$text)) {
    tx <- x$text[[col]]
    if (length(tx$spaces))
      f <- c(f, paste0(col, ": leading or trailing space in ", dc_quote(tx$spaces)))
    for (s in tx$similar)
      f <- c(f, paste0(col, ": may be the same label: ", dc_quote(s)))
    if (length(tx$style))
      f <- c(f, paste0(col, ": written differently from the other labels: ",
                       dc_quote(tx$style)))
    if (length(tx$not_numeric))
      f <- c(f, paste0(col, ": numbers stored as text; not a number: ",
                       dc_quote(tx$not_numeric)))
    if (tx$n_missing > 0L)
      f <- c(f, paste0(col, ": ", tx$n_missing, " missing values"))
  }
  for (col in names(x$numeric)) {
    m <- x$numeric[[col]]$n_missing
    if (m > 0L) f <- c(f, paste0(col, ": ", m, " missing value", if (m > 1L) "s"))
  }
  if (!is.null(x$id) && length(x$id$unusual))
    f <- c(f, paste0(x$id$id_col, ": ",
                     paste(sprintf("%s has %d row%s", x$id$unusual,
                                   as.integer(x$id$rows_per_id[x$id$unusual]),
                                   ifelse(as.integer(x$id$rows_per_id[x$id$unusual]) == 1L, "", "s")),
                           collapse = ", "),
                     "; most IDs have ", x$id$usual))
  f
}

#' Print the data check of f_scan()
#'
#' Prints the data check that \code{\link{f_scan}} stores as
#' \code{attr(result, "data_check")}: first a list of possible problems, then
#' a table of the text columns, a table of the numeric columns with the
#' lowest and highest values, and the rows per ID.
#'
#' @param x An \code{f_data_check} object.
#' @param max_values Integer. Maximum number of distinct values listed per
#'   text column; a column with more values (such as an ID column) is
#'   summarised in one line. Default \code{12}.
#' @param ... Not used.
#' @return \code{x}, invisibly.
#' @export
#' @method print f_data_check
print.f_data_check <- function(x, max_values = 12L, ...) {
  cat("\n--- Data check ---\n")
  cat(x$n_rows, " rows; ", length(x$columns), " of ", x$n_cols,
      " columns checked.\nThis check only reports; it does not change the data.\n",
      sep = "")

  flags <- dc_flags(x)
  if (length(flags)) {
    cat("\nCheck these (", length(flags), "):\n", sep = "")
    room <- max(30L, getOption("width", 80L) - 4L)
    for (f in flags) {
      l <- strwrap(f, width = room)
      cat("  ! ", l[1], "\n", sep = "")
      for (r in l[-1]) cat("    ", r, "\n", sep = "")
    }
  } else {
    cat("\nNo problems found.\n")
  }

  if (length(x$text)) {
    cat("\nText columns\n")
    w <- max(nchar(c("Column", names(x$text))))
    dc_row(c("Column", "Values (rows)"), w)
    for (col in names(x$text)) {
      tab  <- x$text[[col]]$values
      labs <- ifelse(is.na(names(tab)), "NA", paste0("\"", names(tab), "\""))
      n    <- as.integer(tab)
      if (length(tab) > max_values) {
        rows <- if (min(n) == max(n)) paste(n[1], "rows each")
                else paste(min(n), "to", max(n), "rows each")
        cell <- paste0(length(tab), " values, ", rows)
      } else {
        # non-breaking spaces keep a value and its count on one line
        cell <- paste(gsub(" ", "\u00a0", sprintf("%s (%d)", labs, n)), collapse = ", ")
      }
      dc_row(c(col, cell), w)
    }
  }

  if (length(x$numeric)) {
    cat("\nNumeric columns\n")
    k   <- x$n_extreme
    low <- vapply(x$numeric, function(m) dc_num(m$lowest), character(1))
    hig <- vapply(x$numeric, function(m) dc_num(m$highest), character(1))
    mis <- vapply(x$numeric, function(m) as.character(m$n_missing), character(1))
    w   <- c(max(nchar(c("Column", names(x$numeric)))),
             nchar("Missing"),
             max(nchar(c(paste("Lowest", k), low))))
    dc_row(c("Column", "Missing", paste("Lowest", k), paste("Highest", k)), w)
    for (col in names(x$numeric)) {
      dc_row(c(col, formatC(mis[[col]], width = nchar("Missing")), low[[col]],
               hig[[col]]), w)
    }
  }

  if (!is.null(x$id)) {
    cat("\nRows per ", x$id$id_col, "\n", sep = "")
    per <- table(as.integer(x$id$rows_per_id))
    if (length(per) == 1L) {
      cat("  ", x$id$n_ids, " IDs, ", names(per), " rows each\n", sep = "")
    } else {
      cat("  ", x$id$n_ids, " IDs: ",
          paste(sprintf("%s with %s rows", as.integer(per), names(per)),
                collapse = ", "), "\n", sep = "")
    }
  }
  cat("\n")
  invisible(x)
}
