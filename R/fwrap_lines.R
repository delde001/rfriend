#' Wrap lines in rmd output documents
#' @details Does not have any options or arguments. It modifies 'knitr' settings and creates a function that is able to wrap lines.
#'
#' @return A list with 'knitr' setting and functions to wrap lines. Used for its side effects on 'knitr'
#'
#'
#' @noRd
f_wrap_lines <- function(){
  hook_output <- knit_hooks$get('output')
  knit_hooks$set(output = function(x, options) {
    # this hook is used only when the linewidth option is not NULL
    if (!is.null(n <- options$linewidth)) {
      x <- xfun::split_lines(x)
      # any lines with more character than n should be wrapped
      if (any(nchar(x) > n)) x = strwrap(x, width = n)
      x <- paste(x, collapse = '\n')
    }
    hook_output(x, options)
  })
}
