# =============================================================================
# helper_rank_response.R
# =============================================================================
# Internal helper for the rank-based tests (f_wilcox_test, f_kruskal_test,
# f_friedman). Prepares one response column before the test runs.
#
# Why this helper exists
# ----------------------
# Rank tests use only the ORDER of the values, so an ordinal response stored
# as an ordered factor (e.g. a leaf-damage score, factor(score, ordered =
# TRUE)) can be tested exactly on the rank codes of its levels
# (1 = lowest level). Without this step f_kruskal_test() and f_friedman()
# stopped with the internal quantile() error "'type' must be 1 or 3 for
# ordered factors" (BUGS #27), and f_wilcox_test() refused the response.
#
# Behaviour (fail loud, never fake)
# ---------------------------------
#   numeric response          -> returned unchanged
#   ordered factor (any size) -> converted to integer codes, with a message
#                                that shows the coding, so the user knows
#                                the summary statistics refer to codes
#   anything else             -> stop() with a clear message that points to
#                                f_chisq_test() for unordered categories
# =============================================================================

prepare_rank_response <- function(data, response, fun_name) {
  y <- data[[response]]
  if (is.numeric(y)) return(data)

  if (is.ordered(y)) {
    lv <- levels(y)
    message(fun_name, ": response '", response, "' is an ordered factor. ",
            "It is tested on the rank codes of its levels (",
            paste0(seq_along(lv), " = '", lv, "'", collapse = ", "), "). ",
            "Rank tests use only this order; means and sd in the summary ",
            "refer to the codes, so report medians or counts per level.")
    data[[response]] <- as.integer(y)
    return(data)
  }

  stop(fun_name, ": response '", response, "' must be numeric or an ordered ",
       "factor (an ordinal score), not ", class(y)[1], ". For a score, use ",
       "factor(", response, ", ordered = TRUE); for unordered categories, ",
       "use f_chisq_test().", call. = FALSE)
}
