# =============================================================================
# helper-quiet.R
# =============================================================================
# Output-suppression wrappers used across the rfriend test suite.
#
# Almost every f_*() function in rfriend writes Markdown to the console via
# cat() when called with output_type = "default", and several also emit
# informational messages() or package-startup warnings. Letting all of that
# stream to stdout during testthat runs makes failures unreadable.
#
# Pre-refactor the suite had near-identical quiet_f_aov(), quiet_lmer(),
# quiet_bp(), quiet_bn(), quiet_tt(), quiet_summary(), quiet_boxcox()
# wrappers defined inline in each test file. They all did the same thing:
# suppress messages, capture stdout, return the result.
# =============================================================================


# -----------------------------------------------------------------------------
# Low-level wrappers
# -----------------------------------------------------------------------------
#
# These operate on an already-built call by capturing it unevaluated. They
# are suitable when you are NOT forwarding `...` and are safe to use from
# within a test_that() block.

# Suppress messages and capture stdout. Returns the value of `expr`.
run_quiet <- function(expr) {
  suppressMessages(
    utils::capture.output(
      result <- eval.parent(substitute(expr)),
      file = nullfile()
    )
  )
  result
}

# Same, but also swallow warnings. Use sparingly: a test that needs this
# is probably a test that should be checking for a specific warning with
# expect_warning() instead. The main legitimate use case is tests of
# edge-case model fits where lme4 / MASS emit convergence chatter that
# the rfriend wrapper is specifically designed to tolerate.
run_quiet_warn <- function(expr) {
  suppressMessages(suppressWarnings(
    utils::capture.output(
      result <- eval.parent(substitute(expr)),
      file = nullfile()
    )
  ))
  result
}


# -----------------------------------------------------------------------------
# Per-function convenience wrappers
# -----------------------------------------------------------------------------
#
# Each of these calls its namesake f_*() function with the canonical
# "no interactive side effects" argument set. They exist so that a
# test block reads:
#
#     out <- quiet_f_aov(y ~ grp, data = df)
#
# instead of:
#
#     suppressMessages(capture.output(out <- f_aov(y ~ grp, data = df,
#         output_type = "default", norm_plots = FALSE, intro_text = FALSE,
#         open_generated_files = FALSE), file = nullfile()))
#
# Implementation note: each wrapper captures its own `...` and forwards
# them explicitly rather than calling run_quiet(). This is deliberate:
# run_quiet() uses eval.parent(substitute(expr)), which does not expand
# `...` correctly when the expression itself contains `...`.
# -----------------------------------------------------------------------------

quiet_f_aov <- function(...) {
  suppressMessages(
    utils::capture.output(
      result <- f_aov(..., output_type = "default",
                      norm_plots = FALSE, intro_text = FALSE,
                      open_generated_files = FALSE),
      file = nullfile()
    )
  )
  result
}

quiet_f_lmer <- function(...) {
  suppressMessages(suppressWarnings(
    utils::capture.output(
      result <- f_lmer(..., output_type = "default",
                       norm_plots = FALSE, intro_text = FALSE,
                       open_generated_files = FALSE),
      file = nullfile()
    )
  ))
  result
}

quiet_f_glm <- function(...) {
  suppressMessages(
    utils::capture.output(
      result <- f_glm(..., output_type = "default",
                      norm_plots = FALSE, intro_text = FALSE,
                      open_generated_files = FALSE),
      file = nullfile()
    )
  )
  result
}

quiet_f_t_test <- function(...) {
  suppressMessages(
    utils::capture.output(
      result <- f_t_test(..., output_type = "default",
                         open_generated_files = FALSE),
      file = nullfile()
    )
  )
  result
}

quiet_f_wilcox_test <- function(...) {
  suppressMessages(
    utils::capture.output(
      result <- f_wilcox_test(..., output_type = "default",
                              open_generated_files = FALSE),
      file = nullfile()
    )
  )
  result
}

quiet_f_kruskal_test <- function(...) {
  suppressMessages(
    utils::capture.output(
      result <- f_kruskal_test(..., output_type = "default",
                               open_generated_files = FALSE),
      file = nullfile()
    )
  )
  result
}

quiet_f_lm <- function(...) {
  suppressMessages(
    utils::capture.output(
      result <- f_lm(..., output_type = "default",
                     norm_plots = FALSE, effect_plots = FALSE,
                     intro_text = FALSE, open_generated_files = FALSE),
      file = nullfile()
    )
  )
  result
}

quiet_f_friedman <- function(...) {
  suppressMessages(suppressWarnings(
    utils::capture.output(
      result <- f_friedman(..., output_type = "default",
                           plot = FALSE, intro_text = FALSE,
                           open_generated_files = FALSE),
      file = nullfile()
    )
  ))
  result
}

quiet_f_chisq_test <- function(...) {
  suppressMessages(
    utils::capture.output(
      result <- f_chisq_test(..., output_type = "default",
                             open_generated_files = FALSE),
      file = nullfile()
    )
  )
  result
}


quiet_f_boxplot <- function(...) {
  suppressMessages(
    utils::capture.output(
      result <- f_boxplot(..., output_type = "default",
                          open_generated_files = FALSE,
                          boxplot_explanation = FALSE,
                          outliers = FALSE),
      file = nullfile()
    )
  )
  result
}

quiet_f_boxcox <- function(x, ...) {
  suppressMessages(
    utils::capture.output(
      result <- f_boxcox(x, ..., output_type = "default",
                         open_generated_files = FALSE),
      file = nullfile()
    )
  )
  result
}

quiet_f_bestNormalize <- function(x, ...) {
  suppressMessages(
    utils::capture.output(
      result <- f_bestNormalize(x, plots = FALSE, output_type = "default", ...),
      file = nullfile()
    )
  )
  result
}
