# =============================================================================
# helper-skips.R
# =============================================================================
# Centralised skip conditions for the rfriend test suite.
#
# Most rfriend functions that produce "pretty" output rely on external
# machinery (Pandoc for .docx / .pdf, RStudio for f_theme / f_setwd,
# Suggests-only packages for certain model comparisons). The skip helpers
# below let individual tests declare their dependencies in one line instead
# of duplicating the underlying checks in every file.
#
# Call pattern inside a test:
#
#     test_that("word output contains expected heading", {
#       skip_on_cran()
#       skip_if_no_pandoc()
#       ...
#     })
#
# Never call these outside a test_that() / it() block. They use
# testthat::skip() which is only meaningful inside a test.
# =============================================================================


# -----------------------------------------------------------------------------
# External binaries
# -----------------------------------------------------------------------------

# Skip when Pandoc is not available on PATH. rmarkdown::pandoc_available()
# is preferred over nzchar(Sys.which("pandoc")) because it also honours
# RSTUDIO_PANDOC on systems where Pandoc ships with RStudio but is not on
# the shell PATH.
skip_if_no_pandoc <- function(min_version = "3.2") {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    testthat::skip("rmarkdown not installed")
  }
  if (!rmarkdown::pandoc_available(min_version)) {
    testthat::skip(sprintf("Pandoc (>= %s) not available", min_version))
  }
}


# -----------------------------------------------------------------------------
# Soft dependencies declared in Suggests:
# -----------------------------------------------------------------------------
#
# DESCRIPTION declares MASS, nnet, pbkrtest, testthat in Suggests.
# testthat::skip_if_not_installed() is the canonical check for each.
# We do NOT wrap it; call it directly at the top of the test. The list
# below is a reminder of which functions depend on which suggest.

# f_boxcox():       MASS
# f_glm() (multinomial path): nnet
# f_lmer() (pbkrtest-based denominator DF): pbkrtest


# -----------------------------------------------------------------------------
# Interactive / GUI dependencies
# -----------------------------------------------------------------------------

# f_theme() and f_setwd() call rstudioapi::* and only make sense inside
# RStudio. Skip on non-interactive or non-RStudio sessions.
skip_if_not_rstudio <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    testthat::skip("rstudioapi not installed")
  }
  if (!rstudioapi::isAvailable()) {
    testthat::skip("not running inside RStudio")
  }
}


# -----------------------------------------------------------------------------
# Slow tests
# -----------------------------------------------------------------------------
#
# CRAN budgets ~10 minutes total for examples + tests on their check farms.
# Several rfriend tests exercise bootstrap simulation (DHARMa), large lmer
# fits, or bestNormalize iterating over every transform. Mark those with:
#
#     skip_on_cran()
#
# directly inside the test_that() block. No wrapper needed.
#
# For tests that are fast on developer laptops but slow in CI, use:
#
#     skip_if(isTRUE(as.logical(Sys.getenv("CI"))))
#
# if and only if CI is consistently too slow. Prefer optimising the test.
