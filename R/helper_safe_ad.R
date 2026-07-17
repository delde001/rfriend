# =============================================================================
# Safe Anderson-Darling test
# =============================================================================
#
# Internal wrapper around nortest::ad.test() that handles the edge case
# the base function does not:
#
#   n < 8 - ad.test() errors with "sample size must be greater than 7".
#      This helper returns a shaped htest object with NA values instead,
#      with a method label explaining why. Callers are expected to fall
#      back to Shapiro-Wilk and qq-plots in this regime.
#
# The returned object is ALWAYS of class "htest" with the same field
# layout that nortest::ad.test() produces (statistic, p.value, method,
# data.name), so downstream code can treat every result uniformly.
# Callers should check is.na(result$p.value) to detect the edge case and
# fall through to their preferred fallback logic. This mirrors
# safe_shapiro() so the two normality wrappers behave identically.
#
# NAs in x are dropped before counting n and before the test is run.
# =============================================================================

safe_ad <- function(x, data_name = NULL) {

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
        statistic = c(A = NA_real_),
        p.value   = NA_real_,
        method    = paste0("Anderson-Darling (skipped: ", reason, ")"),
        data.name = data_name
      ),
      class = "htest"
    )
  }

  if (n < 8L) return(skipped("n < 8"))
  if (length(unique(x)) < 2L || stats::sd(x) < .Machine$double.eps^0.5) {
    return(skipped("zero variance"))
  }

  result <- nortest::ad.test(x)
  result$data.name <- data_name
  result
}
