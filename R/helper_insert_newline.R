# Insert newline every X characters
insert_newline <- function(string, every_x_chars = 8, console = FALSE, keep_dots = FALSE) {
  # Pandoc-safe break: <br> works in PDF, Word, HTML
  # sep <- if (console) "  \n" else "  \\newline{}"
  # sep <- if (console) "  \n" else "  <br>"
  sep <- if (console) "  \n" else "  \n  \n"

  sapply(string, function(s) {

    parts <- unlist(strsplit(s, "\\."))

    parts <- sapply(parts, function(p) {

      # If the segment is empty (e.g. caused by double dots ".."), skip the math
      if (nchar(p) == 0) return("")

      chunks <- substring(
        p,
        seq(1, nchar(p), every_x_chars),
        pmin(seq(every_x_chars, nchar(p) + every_x_chars - 1, every_x_chars), nchar(p))
      )

      paste0(chunks, collapse = sep)
    })

    collapse_char <- if (keep_dots) paste0(".", sep) else sep

    paste(parts, collapse = collapse_char)

  }, USE.NAMES = FALSE)
}
