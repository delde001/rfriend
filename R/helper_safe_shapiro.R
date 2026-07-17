# =============================================================================
# Safe Shapiro-Wilk test
# =============================================================================
#
# Internal wrapper around stats::shapiro.test() that handles the two
# edge cases the base function does not:
#
#   1. n < 3  - shapiro.test() errors. This helper returns a shaped
#      htest object with NA values instead.
#   2. n > 5000 - shapiro.test() errors. This helper returns the same
#      shaped NA object, with a method label explaining why. Callers
#      are expected to fall back to qq-plots and Anderson-Darling in
#      this regime, where Shapiro-Wilk's power is anyway excessive.
#
# The returned object is ALWAYS of class "htest" with the same field
# layout that stats::shapiro.test() produces (statistic, p.value,
# method, data.name), so downstream code can treat every result
# uniformly. Callers should check is.na(result$p.value) to detect the
# edge cases and fall through to their preferred fallback logic.
#
# NAs in x are dropped before counting n and before the test is run.
# =============================================================================

safe_shapiro <- function(x, data_name = NULL) {

  if (is.null(data_name)) {
    data_name <- deparse(substitute(x))
    if (length(data_name) != 1L || nchar(data_name) > 60L) {
      data_name <- "x"
    }
  }

  x <- x[!is.na(x)]
  n <- length(x)

  skipped <- function(reason) {
    structure(
      list(
        statistic = c(W = NA_real_),
        p.value   = NA_real_,
        method    = paste0("Shapiro-Wilk (skipped: ", reason, ")"),
        data.name = data_name
      ),
      class = "htest"
    )
  }

  if (n < 3L)    return(skipped("n < 3"))
  if (n > 5000L) return(skipped("n > 5000"))
  if (length(unique(x)) < 3L || stats::sd(x) < .Machine$double.eps^0.5) {
    return(skipped("zero variance"))
  }

  result <- stats::shapiro.test(x)
  result$data.name <- data_name
  result
}
