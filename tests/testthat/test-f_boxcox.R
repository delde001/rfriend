# =============================================================================
# test_f_boxcox.R
# Comprehensive testthat stress tests for f_boxcox()
#
# Run with: testthat::test_file("f_boxcox_stress_tests.R")
#           or devtools::test() from the package root
#
# Test organisation (each describe() block is independent):
#   1.  Return object structure
#   2.  Input types (vector, data.frame, invalid)
#   3.  Edge cases in data values (NAs, zeros, negatives, n<3, n>5000)
#   4.  output_type variations ("default", "console", "rmd")
#   5.  File generation ("pdf", "excel") via save_as
#   6.  save_as combinations (full path, filename only, directory only, wdir)
#   7.  Lambda / digits / range parameters
#   8.  Output field completeness and naming
#   9.  print() and plot() methods
#  10.  Combined option stress
#  11.  predict.f_boxcox method (forward, inverse, round-trip, validation)
#  12.  Session state restoration (par, options)
#
# Style notes:
#  * Only ASCII characters are used in this file (CRAN preference).
#    Unicode is avoided entirely; use \uxxxx escapes in source strings
#    only when the content itself requires a symbol.
#  * output_type = "default" is the silent mode (NOT "off" - that value
#    would be rejected by the parameter validator).
# =============================================================================
# ---------------------------------------------------------------------------
# Helpers used across multiple describe() blocks
# ---------------------------------------------------------------------------

# Right-skewed, strictly positive data that Box-Cox can normalise.
make_skewed <- function(seed = 42, n = 100) {
  set.seed(seed)
  rlnorm(n, meanlog = 0, sdlog = 1)
}

# Approximately normal positive data (shifted to avoid zeros/negatives).
make_normal_pos <- function(seed = 42, n = 100) {
  set.seed(seed)
  abs(rnorm(n, mean = 10, sd = 2)) + 1
}

# Skewed data with injected NAs (trailing, so index-based checks are easy).
make_skewed_with_na <- function(seed = 42, n = 100, n_na = 5) {
  set.seed(seed)
  c(rlnorm(n - n_na, meanlog = 0, sdlog = 1), rep(NA_real_, n_na))
}

# Silence console / markdown output during tests. `plots`,
# `output_type`, and `open_generated_files` are accepted as named
# wrapper arguments rather than forwarded through ..., otherwise R
# raises "formal argument matched by multiple actual arguments" when
# a caller overrides one of them (e.g. plots = TRUE).
quiet_boxcox <- function(x,
                         plots                = FALSE,
                         output_type          = "default",
                         open_generated_files = FALSE,
                         ...) {
  suppressMessages(
    utils::capture.output(
      res <- f_boxcox(x,
                      plots                = plots,
                      output_type          = output_type,
                      open_generated_files = open_generated_files,
                      ...),
      file = nullfile()
    )
  )
  res
}

# For tests that need a specific output_type (e.g. "console", "rmd", "pdf").
# Does not wrap in capture.output because nested capture.output can leave
# the function's own markdown capture empty. Same named-wrapper pattern
# as quiet_boxcox() to avoid double-argument matching.
quiet_boxcox_typed <- function(x,
                               output_type,
                               plots                = FALSE,
                               open_generated_files = FALSE,
                               ...) {
  suppressMessages(
    f_boxcox(x,
             plots                = plots,
             output_type          = output_type,
             open_generated_files = open_generated_files,
             ...)
  )
}

# =============================================================================
# 1. Return object structure
# =============================================================================
# f_boxcox return object structure
x   <- make_skewed()
  res <- quiet_boxcox(x)

test_that("f_boxcox return object structure: returns an object of class 'f_boxcox'", {
  skip_on_cran()
    expect_s3_class(res, "f_boxcox")
    expect_true(is.list(res))
})

test_that("f_boxcox return object structure: contains every mandatory top-level field", {
  skip_on_cran()
    mandatory <- c("transformed_data", "original_data", "lambda", "n",
                   "missing values", "xlab", "ylab", "alpha",
                   "lambda_estimates", "loglik", "conf_limit",
                   "Shapiro_original_data", "Shapiro_transformed_data",
                   "plots", "data_name", "geom_mean")
    for (k in mandatory) {
      expect_true(k %in% names(res),
                  label = paste("mandatory field present:", k))
    }
})

test_that("f_boxcox return object structure: lambda is a finite numeric scalar", {
  skip_on_cran()
    expect_type(res$lambda, "double")
    expect_length(res$lambda, 1L)
    expect_true(is.finite(res$lambda))
})

test_that("f_boxcox return object structure: lambda_estimates and loglik have the same length", {
  skip_on_cran()
    expect_equal(length(res$lambda_estimates), length(res$loglik))
})

test_that("f_boxcox return object structure: transformed_data length equals the input length (NAs preserved)", {
  skip_on_cran()
    expect_length(res$transformed_data, length(x))
})

test_that("f_boxcox return object structure: Shapiro_original_data is a data frame with p-value column", {
  skip_on_cran()
    expect_s3_class(res$Shapiro_original_data, "data.frame")
    expect_true("Shapiro.p.value0" %in% names(res$Shapiro_original_data))
})

test_that("f_boxcox return object structure: Shapiro_transformed_data is a data frame with lambda and p-value", {
  skip_on_cran()
    expect_s3_class(res$Shapiro_transformed_data, "data.frame")
    expect_true(all(c("lambda", "W", "Shapiro.p.value") %in%
                    names(res$Shapiro_transformed_data)))
})

test_that("f_boxcox return object structure: data_name reflects the input object name", {
  skip_on_cran()
    expect_type(res$data_name, "character")
    expect_true(nzchar(res$data_name))
})

# =============================================================================
# 2. Input types
# =============================================================================
# f_boxcox input types
test_that("f_boxcox input types: accepts a plain numeric vector", {
  skip_on_cran()
    expect_s3_class(quiet_boxcox(make_skewed()), "f_boxcox")
})

test_that("f_boxcox input types: accepts a single-column data.frame and preserves the column name", {
  skip_on_cran()
    df  <- data.frame(values = make_skewed())
    res <- quiet_boxcox(df)
    expect_s3_class(res, "f_boxcox")
    expect_equal(res$data_name, "values")
})

test_that("f_boxcox input types: errors on a multi-column data.frame (regression test for the dead check)", {
  skip_on_cran()
    df_multi <- data.frame(a = make_skewed(),
                           b = make_skewed(seed = 2))
    expect_error(quiet_boxcox(df_multi), "multiple columns")
})

test_that("f_boxcox input types: errors on non-numeric input", {
  skip_on_cran()
    expect_error(quiet_boxcox(c("a", "b", "c")))
})

test_that("f_boxcox input types: errors on a matrix input (not a vector, not a data.frame)", {
  skip_on_cran()
    expect_error(quiet_boxcox(matrix(1:10, ncol = 2)))
})

# =============================================================================
# 3. Edge cases in data values
# =============================================================================
# f_boxcox data-value edge cases
test_that("f_boxcox data-value edge cases: handles NAs: records missing count and preserves length", {
  skip_on_cran()
    x   <- make_skewed_with_na(n = 100, n_na = 5)
    res <- quiet_boxcox(x)
    expect_equal(res[["missing values"]], 5L)
    expect_length(res$transformed_data, length(x))
})

test_that("f_boxcox data-value edge cases: errors informatively on zeros", {
  skip_on_cran()
    expect_error(quiet_boxcox(c(0, 1, 2, 3, 4)),
                 "higher than zero")
})

test_that("f_boxcox data-value edge cases: errors informatively on negative values", {
  skip_on_cran()
    expect_error(quiet_boxcox(c(-1, 2, 3, 4, 5)),
                 "higher than zero")
})

test_that("f_boxcox data-value edge cases: handles small samples (n = 10)", {
  skip_on_cran()
    res <- quiet_boxcox(make_skewed(n = 10))
    expect_s3_class(res, "f_boxcox")
    expect_true(is.finite(res$lambda))
})

test_that("f_boxcox data-value edge cases: handles already-normal positive data without error", {
  skip_on_cran()
    res <- quiet_boxcox(make_normal_pos())
    expect_s3_class(res, "f_boxcox")
    # Shapiro on original should not reject strongly (p > 0.001).
    p0 <- res$Shapiro_original_data$Shapiro.p.value0
    expect_true(is.na(p0) || p0 > 0.001)
})

test_that("f_boxcox data-value edge cases: does not crash at n > 5000 (safe_shapiro skip branch)", {
  skip_on_cran()
    set.seed(11)
    big <- rlnorm(5001, meanlog = 0, sdlog = 1)
    expect_no_error(res <- quiet_boxcox(big))
    expect_s3_class(res, "f_boxcox")
    # Shapiro is intentionally skipped; p-values should be NA.
    expect_true(is.na(res$Shapiro_original_data$Shapiro.p.value0))
    expect_true(is.na(res$Shapiro_transformed_data$Shapiro.p.value))
    expect_gt(res$n, 5000L)
})

# =============================================================================
# 4. output_type variations
# =============================================================================
# f_boxcox output_type variations
x <- make_skewed()

test_that("f_boxcox output_type variations: output_type = 'default' is silent and returns the object", {
  skip_on_cran()
    res <- quiet_boxcox(x)
    expect_s3_class(res, "f_boxcox")
})

test_that("f_boxcox output_type variations: output_type = 'console' prints to stdout and returns the object", {
  skip_on_cran()
    # console mode prints via cat(); capture it instead of nullfile().
    out <- capture.output(
      res <- suppressMessages(
        f_boxcox(x, plots = FALSE, output_type = "console",
                 open_generated_files = FALSE)
      )
    )
    expect_gt(length(out), 0L)
    expect_s3_class(res, "f_boxcox")
})

test_that("f_boxcox output_type variations: output_type = 'rmd' populates the rmd field with a non-empty string", {
  skip_on_cran()
    res <- quiet_boxcox_typed(x, "rmd")
    expect_s3_class(res, "f_boxcox")
    expect_type(res$rmd, "character")
    expect_true(nzchar(res$rmd))
})

test_that("f_boxcox output_type variations: errors informatively on an invalid output_type", {
  skip_on_cran()
    expect_error(
      f_boxcox(x, output_type = "html"),
      "output_type|output format"
    )
})

test_that("f_boxcox output_type variations: errors informatively on the common mis-value 'off'", {
  skip_on_cran()
    # 'off' is not a supported value; this is a guard so a future drift
    # back to the old broken stress-test style will fail loudly.
    expect_error(
      f_boxcox(x, output_type = "off"),
      "output_type|output format"
    )
})

# =============================================================================
# 5. File generation (pdf, excel)
#    Word generation is skipped by default because it requires LibreOffice
#    or MS Word at render time on many CI machines.
# =============================================================================
# f_boxcox file generation
test_that("f_boxcox file generation: output_type = 'pdf' produces a real PDF via save_as", {
  skip_on_cran()
    skip_if_not_installed("rmarkdown")
    out_path <- file.path(tempdir(),
                          paste0("test_bc_", as.integer(Sys.time()), ".pdf"))
    res <- suppressMessages(
      f_boxcox(make_skewed(),
               output_type          = "pdf",
               save_as              = out_path,
               open_generated_files = FALSE)
    )
    expect_true(file.exists(out_path))
    # Reasonable lower bound on PDF size; empty renders are a few hundred bytes.
    expect_gt(file.info(out_path)$size, 1000)
    expect_s3_class(res, "f_boxcox")
    unlink(out_path)
})

test_that("f_boxcox file generation: output_type = 'pdf' with plots = TRUE still produces a PDF", {
  skip_on_cran()
    skip_if_not_installed("rmarkdown")
    out_path <- file.path(tempdir(),
                          paste0("test_bc_plots_", as.integer(Sys.time()), ".pdf"))
    res <- suppressMessages(
      f_boxcox(make_skewed(),
               output_type          = "pdf",
               plots                = TRUE,
               save_as              = out_path,
               open_generated_files = FALSE)
    )
    expect_true(file.exists(out_path))
    expect_s3_class(res, "f_boxcox")
    unlink(out_path)
})

test_that("f_boxcox file generation: output_type = 'excel' produces a real xlsx via save_as", {
  skip_on_cran()
    skip_if_not_installed("writexl")
    out_path <- file.path(tempdir(),
                          paste0("test_bc_", as.integer(Sys.time()), ".xlsx"))
    res <- suppressMessages(
      f_boxcox(make_skewed(),
               output_type          = "excel",
               save_as              = out_path,
               open_generated_files = FALSE)
    )
    expect_true(file.exists(out_path))
    expect_gt(file.info(out_path)$size, 100)
    unlink(out_path)
})

# =============================================================================
# 6. save_as combinations
# =============================================================================
# f_boxcox save_as behaviour
test_that("f_boxcox save_as behaviour: accepts a full path with .pdf extension", {
  skip_on_cran()
    skip_if_not_installed("rmarkdown")
    out_path <- file.path(tempdir(),
                          paste0("full_path_", as.integer(Sys.time()), ".pdf"))
    res <- suppressMessages(
      f_boxcox(make_skewed(),
               save_as              = out_path,
               open_generated_files = FALSE)
    )
    expect_true(file.exists(out_path))
    unlink(out_path)
})

test_that("f_boxcox save_as behaviour: filename only -> saved in tempdir()", {
  skip_on_cran()
    skip_if_not_installed("rmarkdown")
    stem     <- paste0("name_only_", as.integer(Sys.time()))
    expected <- file.path(tempdir(), paste0(stem, ".pdf"))
    res <- suppressMessages(
      f_boxcox(make_skewed(),
               output_type          = "pdf",
               save_as              = stem,
               open_generated_files = FALSE)
    )
    expect_true(file.exists(expected))
    unlink(expected)
})

test_that("f_boxcox save_as behaviour: save_as with trailing slash (directory only) uses default filename", {
  skip_on_cran()
    skip_if_not_installed("rmarkdown")
    out_dir <- paste0(tempdir(), "/")
    res <- suppressMessages(
      f_boxcox(make_skewed(),
               output_type          = "pdf",
               save_as              = out_dir,
               open_generated_files = FALSE)
    )
    expect_s3_class(res, "f_boxcox")
})

test_that("f_boxcox save_as behaviour: save_in_wdir = TRUE writes to the working directory", {
  skip_on_cran()
    skip_if_not_installed("rmarkdown")
    wd_before <- getwd()
    res <- suppressMessages(
      f_boxcox(make_skewed(),
               output_type          = "pdf",
               save_in_wdir         = TRUE,
               open_generated_files = FALSE)
    )
    files_in_wd <- list.files(wd_before,
                              pattern     = "BoxCox_output\\.pdf$",
                              full.names  = TRUE)
    expect_gt(length(files_in_wd), 0L)
    unlink(files_in_wd)
    expect_s3_class(res, "f_boxcox")
})

test_that("f_boxcox save_as behaviour: extension in save_as overrides output_type (e.g. .xlsx over 'pdf')", {
  skip_on_cran()
    skip_if_not_installed("writexl")
    out_path <- file.path(tempdir(),
                          paste0("ext_override_", as.integer(Sys.time()), ".xlsx"))
    res <- suppressMessages(
      f_boxcox(make_skewed(),
               output_type          = "pdf",   # overridden by .xlsx
               save_as              = out_path,
               open_generated_files = FALSE)
    )
    expect_true(file.exists(out_path))
    unlink(out_path)
})

# =============================================================================
# 7. Lambda / digits / range parameters
# =============================================================================
# f_boxcox lambda / digits / range
x <- make_skewed()

test_that("f_boxcox lambda / digits / range: digits = 1 (coarse) still returns a finite lambda", {
  skip_on_cran()
    res <- quiet_boxcox(x, digits = 1)
    expect_true(is.finite(res$lambda))
})

test_that("f_boxcox lambda / digits / range: digits = 4 (fine) still returns a finite lambda", {
  skip_on_cran()
    res <- quiet_boxcox(x, digits = 4)
    expect_true(is.finite(res$lambda))
})

test_that("f_boxcox lambda / digits / range: narrow range constrains the selected lambda", {
  skip_on_cran()
    res <- quiet_boxcox(x, range = c(-0.5, 0.5))
    expect_true(abs(res$lambda) <= 0.5 + 1e-6)
})

test_that("f_boxcox lambda / digits / range: negative-only range forces a negative lambda", {
  skip_on_cran()
    res <- quiet_boxcox(x, range = c(-2, -0.1))
    expect_true(res$lambda < 0)
})

test_that("f_boxcox lambda / digits / range: degenerate range with plots = FALSE does not error", {
  skip_on_cran()
    # seq(-0.001, 0.001, by = 0.1) gives a single lambda value; the
    # spline branch must not run in this case (regression guard for the
    # length-1 lambda fix).
    expect_no_error(
      res <- quiet_boxcox(x,
                          range  = c(-0.001, 0.001),
                          digits = 1)
    )
    expect_s3_class(res, "f_boxcox")
})

test_that("f_boxcox lambda / digits / range: degenerate range with plots = TRUE still does not error", {
  skip_on_cran()
    # Same regression guard but with plots enabled; spline() would have
    # crashed before the length-1 guard was added.
    png_path <- tempfile(fileext = ".png")
    png(png_path, width = 600, height = 400)
    on.exit({ dev.off(); unlink(png_path) }, add = TRUE)
    expect_no_error(
      quiet_boxcox(x,
                   range  = c(-0.001, 0.001),
                   digits = 1,
                   plots  = TRUE)
    )
})

test_that("f_boxcox lambda / digits / range: lambda near zero produces log-like transformation", {
  skip_on_cran()
    # With a narrow range around 0 and digits = 3, lambda ~ 0 and the
    # transformation should track log(y) closely.
    res <- quiet_boxcox(x,
                        range  = c(-0.01, 0.01),
                        digits = 3)
    diff <- max(abs(res$transformed_data - log(res$original_data)),
                na.rm = TRUE)
    expect_lt(diff, 0.05)
})

# =============================================================================
# 8. Output field completeness
# =============================================================================
# f_boxcox output completeness and naming
x   <- make_skewed()
  res <- quiet_boxcox(x)

test_that("f_boxcox output completeness and naming: lambda_estimates is the correct field (not lambda_est)", {
  skip_on_cran()
    # Regression guard: plot.f_boxcox references 'lambda_estimates'. A
    # rename to 'lambda_est' would silently break plotting. We test this
    # via names() rather than `res$lambda_est`, because `$` does partial
    # matching in R: `res$lambda_est` would match `res$lambda_estimates`
    # and falsely report that the wrong name is present.
    expect_true("lambda_estimates" %in% names(res))
    expect_false("lambda_est" %in% names(res))
})

test_that("f_boxcox output completeness and naming: alpha defaults to 0.05 and is stored as-is", {
  skip_on_cran()
    expect_equal(res$alpha, 0.05)
    res2 <- quiet_boxcox(x, alpha = 0.01)
    expect_equal(res2$alpha, 0.01)
})

test_that("f_boxcox output completeness and naming: geom_mean is a positive finite scalar", {
  skip_on_cran()
    expect_length(res$geom_mean, 1L)
    expect_true(is.finite(res$geom_mean))
    expect_gt(res$geom_mean, 0)
})

test_that("f_boxcox output completeness and naming: xlab and ylab defaults are present", {
  skip_on_cran()
    expect_false(is.null(res$xlab))
    expect_equal(res$ylab, "log-Likelihood")
})

test_that("f_boxcox output completeness and naming: conf_limit is strictly less than max loglik", {
  skip_on_cran()
    expect_lt(res$conf_limit, max(res$loglik))
})

# =============================================================================
# 9. print() and plot() methods
# =============================================================================
# f_boxcox print and plot methods
x   <- make_skewed()
  res <- quiet_boxcox(x)

test_that("f_boxcox print and plot methods: print.f_boxcox runs without error and produces output", {
  skip_on_cran()
    out <- capture.output(print(res))
    expect_gt(length(out), 0L)
})

test_that("f_boxcox print and plot methods: print.f_boxcox handles NA Shapiro p-values (n > 5000)", {
  skip_on_cran()
    set.seed(12)
    big  <- rlnorm(5001)
    res2 <- quiet_boxcox(big)
    # Must not crash even though p-values are NA.
    expect_no_error(capture.output(print(res2)))
})

# =============================================================================
# 10. Combined option stress
# =============================================================================
# f_boxcox combined options
x <- make_skewed()

test_that("f_boxcox combined options: plots = TRUE with output_type = 'default' draws but stays silent", {
  skip_on_cran()
    png_path <- tempfile(fileext = ".png")
    png(png_path, width = 800, height = 500)
    on.exit({ dev.off(); unlink(png_path) }, add = TRUE)
    res <- quiet_boxcox(x, plots = TRUE)
    expect_s3_class(res, "f_boxcox")
})

test_that("f_boxcox combined options: digits = 2 with range = c(-1, 1) keeps lambda within range", {
  skip_on_cran()
    res <- quiet_boxcox(x, digits = 2, range = c(-1, 1))
    expect_s3_class(res, "f_boxcox")
    expect_true(abs(res$lambda) <= 1 + 1e-6)
})

test_that("f_boxcox combined options: data.frame input with output_type = 'rmd' populates rmd field", {
  skip_on_cran()
    df  <- data.frame(values = make_skewed())
    res <- quiet_boxcox_typed(df, "rmd")
    expect_true(nzchar(res$rmd))
})

test_that("f_boxcox combined options: large n (>5000) with plots = FALSE stays silent and succeeds", {
  skip_on_cran()
    set.seed(13)
    big <- rlnorm(5001)
    expect_no_error(res <- quiet_boxcox(big, plots = FALSE))
    expect_s3_class(res, "f_boxcox")
})

# =============================================================================
# 11. predict.f_boxcox method
# =============================================================================
# predict.f_boxcox forward and inverse transforms
x   <- make_skewed()
  res <- quiet_boxcox(x)

test_that("predict.f_boxcox forward and inverse transforms: forward transform on a single value is numeric and finite", {
  skip_on_cran()
    y <- predict(res, newdata = 10)
    expect_type(y, "double")
    expect_length(y, 1L)
    expect_true(is.finite(y))
})

test_that("predict.f_boxcox forward and inverse transforms: forward transform is vectorised", {
  skip_on_cran()
    y <- predict(res, newdata = c(1, 5, 10, 100))
    expect_length(y, 4L)
    expect_true(all(is.finite(y)))
})

test_that("predict.f_boxcox forward and inverse transforms: forward transform rejects non-positive values", {
  skip_on_cran()
    expect_error(predict(res, newdata = c(1, 0, 2)), "> 0")
    expect_error(predict(res, newdata = c(1, -1, 2)), "> 0")
})

test_that("predict.f_boxcox forward and inverse transforms: forward transform rejects non-numeric input", {
  skip_on_cran()
    expect_error(predict(res, newdata = c("a", "b")), "numeric")
})

test_that("predict.f_boxcox forward and inverse transforms: inverse transform undoes the forward transform (round-trip)", {
  skip_on_cran()
    mu      <- 100
    mu_fwd  <- predict(res, newdata = mu)
    mu_back <- predict(res, newdata = mu_fwd, inverse = TRUE)
    expect_equal(mu_back, mu, tolerance = 1e-8)
})

test_that("predict.f_boxcox forward and inverse transforms: inverse transform with lambda = 0 is exp()", {
  skip_on_cran()
    # Force lambda to 0 with a tight range around 0.
    res0 <- quiet_boxcox(x, range = c(-0.001, 0.001), digits = 3)
    if (isTRUE(res0$lambda == 0)) {
      y    <- c(0.5, 1, 2)
      back <- predict(res0, newdata = y, inverse = TRUE)
      expect_equal(back, exp(y), tolerance = 1e-12)
    } else {
      succeed()
    }
})

test_that("predict.f_boxcox forward and inverse transforms: inverse transform warns on out-of-domain values when lambda > 0", {
  skip_on_cran()
    # Pick a positive-lambda fit for the warning path.
    res_pos <- quiet_boxcox(x, range = c(0.1, 2))
    if (res_pos$lambda > 0) {
      # y * lambda + 1 <= 0  means  y <= -1 / lambda
      bad <- -1 / res_pos$lambda - 1
      expect_warning(
        out <- predict(res_pos, newdata = bad, inverse = TRUE),
        "outside|domain|NaN"
      )
      expect_true(any(is.nan(out)))
    } else {
      succeed()
    }
})

# =============================================================================
# 12. Session state restoration
# =============================================================================
# f_boxcox restores par() and options() on exit
test_that("f_boxcox restores par() and options() on exit: par('mfrow') is not clobbered by a normal call", {
  skip_on_cran()
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)

    mfrow_before <- par("mfrow")
    invisible(quiet_boxcox(make_skewed()))
    mfrow_after  <- par("mfrow")

    expect_equal(mfrow_after, mfrow_before)
})

test_that("f_boxcox restores par() and options() on exit: par('mfrow') is not clobbered by a plot = TRUE call", {
  skip_on_cran()
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)

    png_path <- tempfile(fileext = ".png")
    png(png_path, width = 800, height = 500)
    on.exit({ dev.off(); unlink(png_path) }, add = TRUE)

    mfrow_before <- par("mfrow")
    invisible(quiet_boxcox(make_skewed(), plots = TRUE))
    mfrow_after  <- par("mfrow")

    expect_equal(mfrow_after, mfrow_before)
})

