# =============================================================================
# test-f_boxplot.R
# =============================================================================
# Tests for f_boxplot().
#
# Fixtures: local make_test_df() and make_outlier_bp_df(). The local
# make_outlier_bp_df() is renamed from make_outlier_bp_df() to avoid colliding
# with the shared helper of the same name in helper-data.R (which has a
# different column layout).
# =============================================================================

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Small deterministic test data (avoids dependency on external datasets)
make_test_df <- function() {
  set.seed(42)
  data.frame(
    value1  = c(rnorm(10, 5), rnorm(10, 8), rnorm(10, 3)),
    value2  = c(rnorm(10, 1), rnorm(10, 4), rnorm(10, 7)),
    group   = factor(rep(c("A", "B", "C"), each = 10)),
    subgroup = factor(rep(c("X", "Y"), times = 15))
  )
}

# Data with an obvious outlier for outlier-detection tests
make_outlier_bp_df <- function() {
  set.seed(7)
  df <- make_test_df()
  df$value1[1] <- 999   # extreme outlier
  df
}

# A fancy_names map that covers all columns
test_fancy <- c(
  value1   = "Response 1 (units)",
  value2   = "Response 2 (units)",
  group    = "Treatment Group",
  subgroup = "Sub Group"
)

# =============================================================================
# 1.  INPUT VALIDATION
# =============================================================================
test_that("invalid output_type raises an error", {
  df <- make_test_df()
  expect_error(
    suppressMessages(f_boxplot(df, output_type = "html", open_generated_files = FALSE)),
    regexp = "output_type"
  )
})

test_that("data with no factor variables raises an error", {
  skip_on_cran()
  df_no_factor <- data.frame(a = 1:10, b = rnorm(10))   # no factor columns
  expect_error(
    suppressMessages(f_boxplot(df_no_factor, output_type = "png", open_generated_files = FALSE)),
    regexp = "factor"
  )
})

test_that("NULL data with no formula raises an error", {
  expect_error(f_boxplot(data = NULL, formula = NULL))
})

# =============================================================================
# 2.  BASIC DATA-ONLY USAGE  (no formula)
# =============================================================================
test_that("data-only call returns NULL invisibly for pdf output", {
  skip_on_cran()
  df <- make_test_df()
  suppressMessages(result <- f_boxplot(df,
                      output_type = "pdf",
                      open_generated_files = FALSE,
                      boxplot_explanation  = FALSE,
                      outliers             = FALSE))
  expect_null(result)
})

test_that("data-only call returns NULL invisibly for word output", {
  skip_on_cran()
  skip_if_no_pandoc()
  df <- make_test_df()
  suppressMessages(result <- f_boxplot(df,
                      output_type = "word",
                      open_generated_files = FALSE,
                      boxplot_explanation  = FALSE,
                      outliers             = FALSE))
  expect_null(result)
})

test_that("data-only PNG call creates files in tempdir", {
  skip_on_cran()
  df      <- make_test_df()
  out_dir <- tempdir()
  suppressMessages(result  <- f_boxplot(df,
                       output_type          = "png",
                       save_as              = out_dir,
                       open_generated_files = FALSE,
                       outliers             = FALSE))
  expect_null(result)
  # At least one PNG should have been created
  pngs <- list.files(out_dir, pattern = "\\.png$", full.names = FALSE)
  expect_gt(length(pngs), 0)
})

# =============================================================================
# 3.  FORMULA USAGE
# =============================================================================
test_that("formula with single response and single predictor works (pdf)", {
  skip_on_cran()
  df <- make_test_df()
  expect_null(
    suppressMessages(f_boxplot(value1 ~ group,
              data                 = df,
              output_type          = "pdf",
              open_generated_files = FALSE,
              boxplot_explanation  = FALSE,
              outliers             = FALSE))
  )
})

test_that("formula with multiple responses (LHS +) works (pdf)", {
  skip_on_cran()
  df <- make_test_df()
  expect_null(
    suppressMessages(f_boxplot(value1 + value2 ~ group,
              data                 = df,
              output_type          = "pdf",
              open_generated_files = FALSE,
              boxplot_explanation  = FALSE,
              outliers             = FALSE)
  ))
})

test_that("formula with interaction on RHS works (pdf)", {
  skip_on_cran()
  df <- make_test_df()
  expect_null(
    suppressMessages( f_boxplot(value1 ~ group * subgroup,
              data                 = df,
              output_type          = "pdf",
              open_generated_files = FALSE,
              boxplot_explanation  = FALSE,
              outliers             = FALSE))
  )
})

test_that("formula with + on RHS works (pdf)", {
  skip_on_cran()
  df <- make_test_df()
  expect_null(
    suppressMessages(f_boxplot(value1 ~ group + subgroup,
              data                 = df,
              output_type          = "pdf",
              open_generated_files = FALSE,
              boxplot_explanation  = FALSE,
              outliers             = FALSE))
  )
})

# =============================================================================
# 4.  OUTPUT TYPES COMBINED WITH KEY OPTIONS
# =============================================================================
test_that("rmd output returns (cat output) without error", {
  skip_on_cran()
  df <- make_test_df()
  # rmd should not error; we just check it runs
  expect_no_error(
    suppressMessages(f_boxplot(df,
                output_type         = "rmd",
                boxplot_explanation = FALSE,
                outliers            = FALSE)
    )
  )
})


test_that("PNG output produces one file per response x factor combination", {
  skip_on_cran()
  df      <- make_test_df()
  out_dir <- file.path(tempdir(), paste0("png_test_", Sys.getpid()))
  dir.create(out_dir, showWarnings = FALSE)
  suppressMessages(f_boxplot(df,
            output_type          = "png",
            save_as              = paste0(out_dir, "/"),
            open_generated_files = FALSE,
            outliers             = FALSE))
  pngs <- list.files(out_dir, pattern = "\\.png$")
  # 2 numeric vars x 2 factor vars = 4 PNGs expected
  expect_equal(length(pngs), 4)
})

# =============================================================================
# 5.  fancy_names
# =============================================================================
test_that("fancy_names renames columns without error (pdf)", {
  skip_on_cran()
  df <- make_test_df()
  expect_null(
    suppressMessages(f_boxplot(df,
              fancy_names          = test_fancy,
              output_type          = "pdf",
              open_generated_files = FALSE,
              boxplot_explanation  = FALSE,
              outliers             = FALSE)
  ))
})

test_that("fancy_names + formula combination works (pdf)", {
  skip_on_cran()
  df <- make_test_df()
  expect_null(
    suppressMessages(f_boxplot(value1 ~ group,
              data                 = df,
              fancy_names          = test_fancy,
              output_type          = "pdf",
              open_generated_files = FALSE,
              boxplot_explanation  = FALSE,
              outliers             = FALSE))
  )
})

# =============================================================================
# 6.  JITTER OPTION
# =============================================================================
test_that("jitter = TRUE works without error (pdf)", {
  skip_on_cran()
  df <- make_test_df()
  expect_null(
    suppressMessages(f_boxplot(df,
              jitter               = TRUE,
              output_type          = "pdf",
              open_generated_files = FALSE,
              boxplot_explanation  = FALSE,
              outliers             = FALSE)
  ))
})

test_that("jitter = TRUE + formula works without error (pdf)", {
  skip_on_cran()
  df <- make_test_df()
  expect_null(
    suppressMessages( f_boxplot(value1 + value2 ~ group,
              data                 = df,
              jitter               = TRUE,
              output_type          = "pdf",
              open_generated_files = FALSE,
              boxplot_explanation  = FALSE,
              outliers             = FALSE)
  ))
})

# =============================================================================
# 7.  OUTLIER DETECTION
# =============================================================================


test_that("outliers = TRUE includes outlier section in rmd output", {
  skip_on_cran()
  df  <- make_outlier_bp_df()
  out <-
    suppressMessages(f_boxplot(df,
              output_type         = "rmd",
              outliers            = TRUE,
              boxplot_explanation = FALSE)
  )
  combined <- paste(out, collapse = "\n")
  # Should contain an outlier heading or "No outliers" message
  has_outlier_section <- grepl("Outlier Table|No outliers", combined)
  expect_true(has_outlier_section)
})

test_that("outliers = FALSE skips outlier detection silently", {
  skip_on_cran()
  df  <- make_outlier_bp_df()
  out <-
    suppressMessages( f_boxplot(df,
              output_type         = "rmd",
              outliers            = FALSE,
              boxplot_explanation = FALSE)
  )
  combined <- paste(out, collapse = "\n")
  expect_false(grepl("Outlier Table", combined))
})

test_that("custom coef value is accepted without error", {
  skip_on_cran()
  df <- make_outlier_bp_df()
  expect_null(
    suppressMessages(f_boxplot(df,
              outliers             = TRUE,
              coef                 = 3.0,
              output_type          = "pdf",
              open_generated_files = FALSE,
              boxplot_explanation  = FALSE)
  ))
})

# =============================================================================
# 8.  boxplot_explanation FLAG
# =============================================================================
test_that("boxplot_explanation = TRUE adds explanation text to rmd", {
  skip_on_cran()
  df  <- make_test_df()
  out <- suppressMessages(f_boxplot(df,
              output_type         = "rmd",
              boxplot_explanation = TRUE,
              outliers            = FALSE)
  )
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "Understanding Boxplots", fixed = TRUE)
})

test_that("boxplot_explanation = FALSE omits explanation text from rmd", {
  skip_on_cran()
  df  <- make_test_df()
  out <-
    suppressMessages( f_boxplot(df,
              output_type         = "rmd",
              boxplot_explanation = FALSE,
              outliers            = FALSE)
  )
  combined <- paste(out, collapse = "\n")
  expect_false(grepl("Understanding Boxplots", combined, fixed = TRUE))
})

test_that("boxplot_explanation is forced FALSE for png output", {
  skip_on_cran()
  # If boxplot_explanation were TRUE for png it would cat() markdown mid-plot
  df      <- make_test_df()
  out_dir <- file.path(tempdir(), paste0("explain_png_", Sys.getpid()))
  dir.create(out_dir, showWarnings = FALSE)
  # Should not error even if user passes boxplot_explanation = TRUE with png
  expect_no_error(
    suppressMessages(f_boxplot(df,
              output_type          = "png",
              save_as              = paste0(out_dir, "/"),
              open_generated_files = FALSE,
              outliers             = FALSE,
              boxplot_explanation  = TRUE)   # function should override to FALSE
  ))
})

# =============================================================================
# 9.  save_as & FILE PATH HANDLING
# =============================================================================
test_that("save_as with full path creates file at that location (pdf)", {
  skip_on_cran()
  df       <- make_test_df()
  out_path <- file.path(tempdir(), paste0("custom_name_", Sys.getpid(), ".pdf"))
  suppressMessages(f_boxplot(df,
            save_as              = out_path,
            output_type          = "pdf",
            open_generated_files = FALSE,
            boxplot_explanation  = FALSE,
            outliers             = FALSE))
  expect_true(file.exists(out_path))
})

test_that("save_as with .docx extension overrides output_type to word", {
  skip_on_cran()
  df       <- make_test_df()
  out_path <- file.path(tempdir(), paste0("override_ext_", Sys.getpid(), ".docx"))
  # Pass output_type = "pdf" but extension says .docx - should produce docx
  suppressMessages(f_boxplot(df,
            save_as              = out_path,
            output_type          = "pdf",   # should be overridden
            open_generated_files = FALSE,
            boxplot_explanation  = FALSE,
            outliers             = FALSE))
  expect_true(file.exists(out_path))
})

test_that("save_as directory-only path saves file with default name", {
  skip_on_cran()
  df      <- make_test_df()
  out_dir <- file.path(tempdir(), paste0("dir_only_", Sys.getpid()))
  dir.create(out_dir, showWarnings = FALSE)
  suppressMessages(f_boxplot(df,
            save_as              = paste0(out_dir, "/"),
            output_type          = "pdf",
            open_generated_files = FALSE,
            boxplot_explanation  = FALSE,
            outliers             = FALSE))
  pdfs <- list.files(out_dir, pattern = "\\.pdf$")
  expect_equal(length(pdfs), 1)
  expect_match(pdfs[1], "BoxPlot")
})

# =============================================================================
# 10. COMBINED OPTIONS  (the combos most likely to interact badly)
# =============================================================================
test_that("formula + fancy_names + jitter + outliers + word output", {
  skip_on_cran()
  skip_if_no_pandoc()
  df <- make_outlier_bp_df()
  expect_null(
    suppressMessages(f_boxplot(value1 + value2 ~ group,
              data                 = df,
              fancy_names          = test_fancy,
              jitter               = TRUE,
              outliers             = TRUE,
              coef                 = 1.5,
              output_type          = "word",
              open_generated_files = FALSE,
              boxplot_explanation  = TRUE)
  ))
})

test_that("formula + fancy_names + jitter + outliers + pdf output", {
  skip_on_cran()
  df <- make_outlier_bp_df()
  expect_null(
    suppressMessages(f_boxplot(value1 + value2 ~ group * subgroup,
              data                 = df,
              fancy_names          = test_fancy,
              jitter               = TRUE,
              outliers             = TRUE,
              coef                 = 2.0,
              output_type          = "pdf",
              open_generated_files = FALSE,
              boxplot_explanation  = FALSE)
  ))
})

test_that("detect_factors = FALSE with pre-factored data works (pdf)", {
  skip_on_cran()
  df <- make_test_df()   # already has factor columns
  expect_null(
    suppressMessages(f_boxplot(df,
              detect_factors       = FALSE,
              output_type          = "pdf",
              open_generated_files = FALSE,
              boxplot_explanation  = FALSE,
              outliers             = FALSE)
  ))
})

test_that("iris dataset end-to-end with fancy_names (word)", {
  skip_on_cran()
  skip_if_no_pandoc()
  data(iris)
  new_names <- c(
    "Sepal.Length" = "Sepal length (cm)",
    "Sepal.Width"  = "Sepal width (cm)",
    "Petal.Length" = "Petal length (cm)",
    "Petal.Width"  = "Petal width (cm)",
    "Species"      = "Cultivar"
  )
  expect_null(
    suppressMessages( f_boxplot(iris,
              fancy_names          = new_names,
              output_type          = "word",
              open_generated_files = FALSE,
              boxplot_explanation  = FALSE,
              outliers             = TRUE)
  ))
})

test_that("mtcars formula example from documentation works (word)", {
  skip_on_cran()
  skip_if_no_pandoc()
  data(mtcars)
  expect_null(
    suppressMessages(f_boxplot(hp + disp ~ gear * cyl,
              data                 = mtcars,
              boxplot_explanation  = FALSE,
              output_type          = "word",
              open_generated_files = FALSE)
  ))
})

# =============================================================================
# 11. PNG SPECIFIC OPTIONS  (width / height / units / res)
# =============================================================================
test_that("custom PNG dimensions are accepted without error", {
  skip_on_cran()
  df      <- make_test_df()
  out_dir <- file.path(tempdir(), paste0("png_dims_", Sys.getpid()))
  dir.create(out_dir, showWarnings = FALSE)
  expect_no_error(
    suppressMessages(f_boxplot(df,
              output_type          = "png",
              save_as              = paste0(out_dir, "/"),
              open_generated_files = FALSE,
              outliers             = FALSE,
              width  = 12,
              height = 9,
              units  = "in",
              res    = 150)
  ))
})

test_that("las parameter variants (0-3) are accepted without error", {
  skip_on_cran()
  df <- make_test_df()
  for (las_val in 0:3) {
    # Use expect_error with regexp=NA as alternative - it does accept label
    expect_error(
      suppressMessages(f_boxplot(df,
                las                  = las_val,
                output_type          = "pdf",
                open_generated_files = FALSE,
                boxplot_explanation  = FALSE,
                outliers             = FALSE),
      regexp = NA,   # NA means "expect NO error"
      label  = paste("las =", las_val)
    ))
  }
})

# =============================================================================
# 12. GRAPHICAL PARAMETER RESTORATION  (tests bug #2 - par inside boxplot)
# =============================================================================
test_that("par() settings are restored after function call", {
  skip_on_cran()
  df       <- make_test_df()
  mar_before <- par("mar")
  suppressMessages( f_boxplot(df,
            output_type          = "pdf",
            open_generated_files = FALSE,
            boxplot_explanation  = FALSE,
            outliers             = FALSE))
  mar_after <- par("mar")
  expect_equal(mar_before, mar_after)
})


# =============================================================================
# S3 dispatch cleanup
# =============================================================================
#   A. f_boxplot.formula uses formula, data arguments (exempted name).
#   B. f_boxplot.data.frame uses x as the first argument.
#   C. Both dispatch paths produce a plot object without erroring.

# Local wrapper (differs from shared quiet_f_boxplot in helper-quiet.R:
# uses output_type = "rmd" and tryCatch-to-error pattern specific to
# these dispatch tests).
quiet_bp <- function(...) {
  suppressMessages(
    suppressWarnings(
      tryCatch(
        f_boxplot(..., open_generated_files = FALSE,
                  output_type = "rmd"),
        error = function(e) e
      )
    )
  )
}

# ---------------------------------------------------------------------------
# A. formula method
# ---------------------------------------------------------------------------

test_that("formula interface dispatches to f_boxplot.formula", {
  res <- quiet_bp(Sepal.Width ~ Species, data = iris)
  expect_false(inherits(res, "error"),
               info = if (inherits(res, "error")) conditionMessage(res)
                      else "")
})

test_that("formula interface with explicit data arg name works", {
  res <- quiet_bp(formula = Sepal.Width ~ Species, data = iris)
  expect_false(inherits(res, "error"))
})

# ---------------------------------------------------------------------------
# B. data.frame method
# ---------------------------------------------------------------------------

test_that("data.frame interface dispatches to f_boxplot.data.frame", {
  df <- iris[, c("Sepal.Width", "Species")]
  res <- quiet_bp(df)
  expect_false(inherits(res, "error"))
})

test_that("named call f_boxplot(x = df) works", {
  df <- iris[, c("Sepal.Width", "Species")]
  res <- quiet_bp(x = df)
  expect_false(inherits(res, "error"))
})
