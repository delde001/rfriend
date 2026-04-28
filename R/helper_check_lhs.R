# =============================================================================
# helper_check_lhs.R
# =============================================================================
# Internal helper. Warns when the LHS of a formula contains expressions
# (function calls, arithmetic) instead of bare variable names.
#
# Why this guard exists
# ---------------------
# rfriend's formula-based functions (f_aov, f_glm, f_kruskal_test, f_lmer,
# f_t_test, f_wilcox_test, f_summary, f_outliers, f_scan, f_stat_wizard,
# f_boxplot) all parse the LHS via:
#
#     lhs <- all.vars(formula[[2]])
#
# all.vars() silently strips function calls. That means:
#
#   log(y)  ~ group   -> LHS extracted as "y" (untransformed!)
#   I(y^2)  ~ group   -> LHS extracted as "y"
#   sqrt(y) ~ group   -> LHS extracted as "y"
#
# In every case the function then operates on the untransformed column,
# producing a correct-looking analysis of the wrong data. This warning
# alerts the user so they can precompute a column and use its name.
#
# Allowed syntax (no warning)
# ---------------------------
#   y ~ group                        bare name
#   y1 + y2 ~ group                  multiple responses (rfriend convention)
#   y1 + y2 + y3 ~ group             any number of responses
#   `my column` ~ group              backtick-quoted non-syntactic name
#   y1 + `my column` ~ group         mix of bare and backticked names
#
# Disallowed syntax (warning fires, naming the offending term)
# -----------------------------------------------------------
#   log(y) ~ group                   function call on LHS
#   I(y^2) ~ group                   I() wrapper on LHS
#   sqrt(y1) + y2 ~ group            one call + one bare name -> warns on sqrt(y1)
#   y * 2 ~ group                    arithmetic on LHS
#
# The warning is emitted with call. = FALSE so that users see a clean
# message without an unhelpful "Error in check_lhs_is_names(...)" prefix.
# =============================================================================

check_lhs_is_names <- function(formula) {

  # Deparse the LHS back to source text. Split on '+' to recover the
  # individual response terms as rfriend would see them.
  lhs_deparsed <- deparse(formula[[2]])
  lhs_terms    <- trimws(strsplit(lhs_deparsed, "\\+")[[1]])

  # Strip surrounding backticks from a term, if present.
  # A backtick-quoted name like `my column` is syntactically a valid
  # reference, so it counts as a bare name for the purpose of this check.
  strip_backticks <- function(t) {
    if (startsWith(t, "`") && endsWith(t, "`")) {
      substr(t, 2L, nchar(t) - 1L)
    } else {
      t
    }
  }

  # A term qualifies as a bare (or backticked) name if:
  #   - after stripping backticks the inner text is a syntactically
  #     valid R name (make.names() does not alter it), OR
  #   - the term as written already matches make.names() unchanged.
  #
  # The two-path check means both plain names ("y1") and backticked
  # non-syntactic names ("`my column`") pass cleanly, while function
  # calls like "log(y)" fail because make.names("log(y)") returns
  # "log.y." which differs from the original.
  is_bare <- vapply(lhs_terms, function(t) {
    if (startsWith(t, "`") && endsWith(t, "`")) {
      # Backtick-wrapped: the wrapping itself is syntactic; the inner
      # contents are allowed to be anything, so this always passes.
      return(TRUE)
    }
    t == make.names(t)
  }, logical(1))

  if (!all(is_bare)) {
    bad <- lhs_terms[!is_bare]
    warning(
      "Expressions on the LHS of the formula are ignored: ",
      paste(bad, collapse = ", "),
      ". \nOnly variable names are used by rfriend. \nPrecompute a column in `data` ",
      "instead (e.g. `data$log_value <- log(data$value)`).",
      call. = FALSE
    )
  }

  invisible(NULL)
}
