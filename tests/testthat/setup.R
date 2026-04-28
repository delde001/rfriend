# =============================================================================
# setup.R
# =============================================================================
# Runs ONCE at the very start of the test suite. Anything that should be
# reset between individual tests belongs in a helper-*.R or in the
# test_that() block itself via withr::local_*(), not here.
#
# testthat sources setup.R before any test-*.R file.
# =============================================================================


# -----------------------------------------------------------------------------
# Make output deterministic
# -----------------------------------------------------------------------------

# Many rfriend functions print numeric tables. Lock the formatting options
# so a developer's personal options() cannot influence test output.
old_opts <- options(
  OutDec          = ".",
  scipen          = 0,
  digits          = 7,
  warn            = 1,       # print warnings as they occur, do not defer
  warnPartialMatchArgs = FALSE,
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# Graphical device hygiene
# -----------------------------------------------------------------------------

# Any plotting done during the suite goes to a throwaway PDF device opened
# on tempfile(). This prevents tests from popping up RStudio graphics
# windows or leaving active devices behind that later tests then inherit.
#
# The device is closed in teardown.R.
grDevices::pdf(file = tempfile(fileext = ".pdf"))


# -----------------------------------------------------------------------------
# Teardown registration
# -----------------------------------------------------------------------------
# The paired teardown lives in teardown.R. We intentionally do NOT use
# withr::defer(envir = teardown_env()) here because teardown.R is already
# the canonical place for suite-wide cleanup and mixing both styles makes
# the sequence hard to trace.
#
# `old_opts` is left in the test-runner environment so teardown.R can
# restore it.
