# =============================================================================
# teardown.R
# =============================================================================
# Runs ONCE at the very end of the test suite, after every test-*.R file has
# finished. Use for cleanup that cannot be attached to an individual test.
#
# Anything that should be scoped to a single test belongs in the test block
# itself via withr::local_*() / defer(), not here.
# =============================================================================


# -----------------------------------------------------------------------------
# Close the throwaway graphics device opened in setup.R
# -----------------------------------------------------------------------------
# Wrapped in tryCatch because a test that mishandled dev.off() might have
# closed it already. A secondary dev.off() on a closed device raises an
# error we do not want bubbling up as a suite failure.
tryCatch(
  while (!is.null(grDevices::dev.list())) grDevices::dev.off(),
  error = function(e) invisible(NULL)
)


# -----------------------------------------------------------------------------
# Restore options() to whatever they were before setup.R ran
# -----------------------------------------------------------------------------
if (exists("old_opts", inherits = TRUE)) {
  options(old_opts)
}


# -----------------------------------------------------------------------------
# Wipe any stray rfriend-generated output files from tempdir()
# -----------------------------------------------------------------------------
# Several f_*() functions write PDFs / .docx / .xlsx to tempdir() by
# default. A well-behaved test unlinks its own output via
# withr::local_tempfile(), but this is a safety net for tests that forgot.
stray <- list.files(
  tempdir(),
  pattern    = "\\.(pdf|docx|xlsx|png|html|md|rmd)$",
  full.names = TRUE,
  ignore.case = TRUE
)
if (length(stray)) {
  unlink(stray, force = TRUE)
}
