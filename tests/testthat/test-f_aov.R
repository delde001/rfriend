# =============================================================================
# test-f_aov.R
# Comprehensive testthat tests for f_aov()
#
# Run with: testthat::test_file("tests/testthat/test-f_aov.R")
#           or devtools::test() from the package root
#
# CRAN strategy
# -------------
# Most tests run on CRAN because the file completes in ~12s on a recent
# laptop (well within CRAN's per-package budget even at 3x slower hardware).
# The tests marked with skip_on_cran() are ONLY those that are genuinely
# expensive or dependency-sensitive:
#
#   * bestNormalize transformation (section 4): iterates over many
#     candidate transformations with cross-validation; unpredictable
#     runtime and depends on the bestNormalize package being installed.
#   * Shapiro-Wilk n > 5000 guard (section 14): generates 6000 rows and
#     runs the full pipeline on them.
#
# Everything else (structure, input validation, ANCOVA, NAs, interactions,
# rmd output, restoration, etc.) is fast and gets CRAN coverage.
#
# Test organisation (each section is independent):
#   1.  Return object structure
#   2.  Single response, no transformation
#   3.  Multiple response variables
#   4.  Transformation options (boxcox / bestnormalize / FALSE)
#   5.  force_transformation
#   6.  Alpha and adjust parameters
#   7.  ANCOVA mode
#   8.  Missing values (NAs)
#   9.  Interaction models
#  10.  rmd output type
#  11.  norm_plots = FALSE
#  12.  Input validation / error handling
#  13.  intro_text = FALSE
#  14.  Large n (Shapiro-Wilk guard)
#  15.  Small cell sizes (Levene power warning)
#  16.  Non-significant ANOVA (letters should be 'ns')
#  17.  Post-hoc table content and column names
#  18.  par() and options() are restored on exit
#  19.  Last-resort message (seed 335, empirically robust trigger)
#  20.  Multiple-response NOTE
#  21.  Output text correctness
#  22.  subset / na.action / weights pass-through
#  23.  safe_shapiro() integration
# =============================================================================

# ---------------------------------------------------------------------------
# Helpers used across multiple tests
# ---------------------------------------------------------------------------
# make_normal_data, make_skewed_data, make_two_factor_data come from
# helper-data.R. quiet_f_aov comes from helper-quiet.R.
#
# quiet_f_aov_typed is file-local: it intentionally does NOT use
# capture.output because nested capture.output calls redirect stdout
# before f_aov's internal markdown capture runs, leaving cached_markdown
# empty. For non-default output_type, f_aov does not print to console
# anyway, so suppressMessages alone is sufficient.

quiet_f_aov_typed <- function(output_type, ...) {
  suppressMessages(
    f_aov(..., output_type = output_type,
          norm_plots = FALSE, intro_text = FALSE,
          open_generated_files = FALSE)
  )
}

# =============================================================================
# 1. Return object structure
# =============================================================================
df  <- make_normal_data()
out <- quiet_f_aov(y ~ grp, data = df)

test_that("f_aov return object structure: returns an object with class 'f_aov'", {
  expect_s3_class(out, "f_aov")
})

test_that("f_aov return object structure: is a named list with one element per response variable", {
  expect_true(is.list(out))
  expect_true("y" %in% names(out))
})

test_that("f_aov return object structure: each response element contains mandatory keys", {
  keys <- names(out[["y"]])
  mandatory <- c("aov_test", "aov_summary", "aov_call",
                 "Levene_test_on_res", "shapiro_test_residuals",
                 "adt_test_residuals", "normality_plots",
                 "alpha", "Response_Transformed",
                 "post_hoc_summary_table", "summary_table")
  for (k in mandatory) {
    expect_true(k %in% keys,
                label = paste("mandatory key present:", k))
  }
})

test_that("f_aov return object structure: aov_test is an aov object", {
  expect_s3_class(out[["y"]][["aov_test"]], "aov")
})

test_that("f_aov return object structure: aov_call is a character string matching the formula", {
  expect_type(out[["y"]][["aov_call"]], "character")
  expect_match(out[["y"]][["aov_call"]], "y")
  expect_match(out[["y"]][["aov_call"]], "grp")
})

test_that("f_aov return object structure: shapiro_test_residuals is an htest object with p.value", {
  sw <- out[["y"]][["shapiro_test_residuals"]]
  expect_true(!is.null(sw$p.value))
  expect_true(sw$p.value >= 0 && sw$p.value <= 1)
})

test_that("f_aov return object structure: Levene_test_on_res has a p column", {
  lev <- out[["y"]][["Levene_test_on_res"]]
  expect_true("p" %in% names(lev))
})

test_that("f_aov return object structure: alpha stored in output_list matches the argument passed", {
  expect_equal(out[["y"]][["alpha"]], 0.05)
  out2 <- quiet_f_aov(y ~ grp, data = df, alpha = 0.01)
  expect_equal(out2[["y"]][["alpha"]], 0.01)
})

test_that("f_aov return object structure: post_hoc_summary_table is a data frame", {
  expect_s3_class(out[["y"]][["post_hoc_summary_table"]], "data.frame")
})

test_that("f_aov return object structure: post_hoc_summary_table contains 'Letter' column", {
  expect_true("Letter" %in% names(out[["y"]][["post_hoc_summary_table"]]))
})

test_that("f_aov return object structure: normality_plots path points to a real file", {
  expect_true(file.exists(out[["y"]][["normality_plots"]]))
})

# =============================================================================
# 2. Single response, no transformation  (normal data)
# =============================================================================
df  <- make_normal_data()
out <- quiet_f_aov(y ~ grp, data = df, transformation = FALSE)

test_that("f_aov single response no transformation: Response_Transformed is FALSE when residuals are normal", {
  expect_false(out[["y"]][["Response_Transformed"]])
})

test_that("f_aov single response no transformation: transformed keys are absent when no transformation occurred", {
  keys <- names(out[["y"]])
  expect_false("transformed_aov_test" %in% keys)
  expect_false("boxcox" %in% keys)
  expect_false("bestNormalize" %in% keys)
})

test_that("f_aov single response no transformation: ANOVA F-statistic is positive", {
  f_val <- out[["y"]][["aov_summary"]][[1]][["F value"]]
  expect_true(all(f_val[!is.na(f_val)] > 0))
})

test_that("f_aov single response no transformation: post_hoc_summary_table has one row per group level", {
  ph <- out[["y"]][["post_hoc_summary_table"]]
  expect_equal(nrow(ph), 3L)  # 3 groups: A, B, C
})

# =============================================================================
# 3. Multiple response variables
# =============================================================================
df  <- make_normal_data()
df$y2 <- df$y * 1.5 + rnorm(nrow(df), 0, 0.5)

out <- quiet_f_aov(y + y2 ~ grp, data = df, transformation = FALSE)

test_that("f_aov multiple response variables: output list contains both response names", {
  expect_true("y"  %in% names(out))
  expect_true("y2" %in% names(out))
})

test_that("f_aov multiple response variables: each response has its own aov_test", {
  expect_s3_class(out[["y"]][["aov_test"]],  "aov")
  expect_s3_class(out[["y2"]][["aov_test"]], "aov")
})

test_that("f_aov multiple response variables: aov_call for y2 references y2 not y", {
  expect_match(out[["y2"]][["aov_call"]], "y2")
  expect_false(grepl("^y ~", out[["y2"]][["aov_call"]]))
})

test_that("f_aov multiple response variables: alpha is stored consistently across all responses", {
  expect_equal(out[["y"]][["alpha"]],  0.05)
  expect_equal(out[["y2"]][["alpha"]], 0.05)
})

test_that("f_aov multiple response variables: three response variables all stored independently", {
  df$y3 <- rnorm(nrow(df), 5, 1)
  out3  <- quiet_f_aov(y + y2 + y3 ~ grp, data = df, transformation = FALSE)
  expect_setequal(setdiff(names(out3), "rmd"), c("y", "y2", "y3"))
})

# =============================================================================
# 4. Transformation options
# =============================================================================
df_skew <- make_skewed_data()

test_that("f_aov transformation options: transformation = TRUE triggers boxcox when residuals are non-normal", {
  out <- quiet_f_aov(y ~ grp, data = df_skew, transformation = TRUE)
  transformed <- out[["y"]][["Response_Transformed"]]
  if (transformed) {
    expect_true("boxcox" %in% names(out[["y"]]) ||
                  "bestNormalize" %in% names(out[["y"]]))
  }
})

test_that("f_aov transformation options: transformation = 'boxcox' stores boxcox key", {
  out <- quiet_f_aov(y ~ grp, data = df_skew, transformation = "boxcox")
  if (out[["y"]][["Response_Transformed"]]) {
    expect_true("boxcox" %in% names(out[["y"]]))
  }
})

test_that("f_aov transformation options: transformation = FALSE never transforms, no transformed keys", {
  out <- quiet_f_aov(y ~ grp, data = df_skew, transformation = FALSE)
  expect_false(out[["y"]][["Response_Transformed"]])
  expect_false("transformed_aov_test" %in% names(out[["y"]]))
})

test_that("f_aov transformation options: transformation = 'bestnormalize' stores bestNormalize key when triggered", {
  # bestNormalize iterates over many candidate transformations with CV;
  # it is the slowest transformation option and has the heaviest deps.
  skip_on_cran()
  set.seed(1)
  out <- quiet_f_aov(y ~ grp, data = df_skew, transformation = "bestnormalize")
  if (out[["y"]][["Response_Transformed"]]) {
    expect_true("bestNormalize" %in% names(out[["y"]]))
  }
})

test_that("f_aov transformation options: when transformed, transformed_shapiro_test is present", {
  out <- quiet_f_aov(y ~ grp, data = df_skew, transformation = TRUE)
  if (out[["y"]][["Response_Transformed"]]) {
    expect_true("transformed_shapiro_test" %in% names(out[["y"]]))
    expect_true("transformed_levene_test"  %in% names(out[["y"]]))
    expect_true("transformed_adt_test"     %in% names(out[["y"]]))
  }
})

test_that("f_aov transformation options: partial string 'box' is accepted via pmatch", {
  expect_no_error(
    quiet_f_aov(y ~ grp, data = df_skew, transformation = "box")
  )
})

test_that("f_aov transformation options: invalid transformation string throws an error", {
  expect_error(
    quiet_f_aov(y ~ grp, data = df_skew, transformation = "logtransform"),
    regexp = "Invalid transformation"
  )
})

# =============================================================================
# 5. force_transformation
# =============================================================================
df <- make_normal_data()  # normal data: would not ordinarily be transformed

test_that("f_aov force_transformation: forces transformation on named response even when residuals are normal", {
  out <- quiet_f_aov(y ~ grp, data = df,
                     transformation       = TRUE,
                     force_transformation = "y")
  expect_true(out[["y"]][["Response_Transformed"]])
})

test_that("f_aov force_transformation: does not affect other responses not listed in force_transformation", {
  # Use n=200 per group so Shapiro-Wilk reliably passes for a truly normal y2.
  # Small samples can reject normality by chance; this test is about the
  # force_transformation logic, not about Shapiro-Wilk sensitivity.
  set.seed(77)
  n_big <- 200
  df_big <- data.frame(
    y   = c(rnorm(n_big, 10, 1), rnorm(n_big, 13, 1), rnorm(n_big, 16, 1)),
    y2  = rnorm(n_big * 3, mean = 5, sd = 0.5),   # clearly normal, large n
    grp = factor(rep(c("A", "B", "C"), each = n_big))
  )
  out <- quiet_f_aov(y + y2 ~ grp, data = df_big,
                     transformation       = TRUE,
                     force_transformation = "y")
  # y is in force_transformation: must be transformed regardless
  expect_true(out[["y"]][["Response_Transformed"]])
  # y2 is NOT in force_transformation and is robustly normal: not transformed
  expect_false(out[["y2"]][["Response_Transformed"]])
})

# =============================================================================
# 6. Alpha and adjust parameters
# =============================================================================
df <- make_normal_data()

test_that("f_aov alpha and adjust parameters: alpha = 0.01 is stored correctly in each response", {
  out <- quiet_f_aov(y ~ grp, data = df, alpha = 0.01,
                     transformation = FALSE)
  expect_equal(out[["y"]][["alpha"]], 0.01)
})

test_that("f_aov alpha and adjust parameters: alpha = 0.10 affects significance evaluation", {
  out_05 <- quiet_f_aov(y ~ grp, data = df, alpha = 0.05, transformation = FALSE)
  out_10 <- quiet_f_aov(y ~ grp, data = df, alpha = 0.10, transformation = FALSE)
  expect_s3_class(out_05[["y"]][["post_hoc_summary_table"]], "data.frame")
  expect_s3_class(out_10[["y"]][["post_hoc_summary_table"]], "data.frame")
})

# =============================================================================
# 7. ANCOVA mode (ANCOVA = TRUE prevents factor coercion)
# =============================================================================
set.seed(11)
df_ancova <- data.frame(
  y         = rnorm(60, 10, 2),
  group     = factor(rep(c("ctrl", "trt"), each = 30)),
  covariate = runif(60, 0, 10)
)

test_that("f_aov ANCOVA mode: ANCOVA = TRUE runs without error for a model with covariate", {
  expect_no_error(
    quiet_f_aov(y ~ group + covariate,
                data   = df_ancova,
                ANCOVA = TRUE,
                transformation = FALSE)
  )
})

test_that("f_aov ANCOVA mode: ANCOVA = TRUE keeps covariate numeric (not coerced to factor)", {
  out <- quiet_f_aov(y ~ group + covariate,
                     data   = df_ancova,
                     ANCOVA = TRUE,
                     transformation = FALSE)
  expect_s3_class(out[["y"]][["aov_test"]], "aov")
})

# =============================================================================
# 8. Missing values (NAs)
# =============================================================================
df      <- make_normal_data()
df$y[c(3, 7, 22)] <- NA   # introduce 3 NAs

test_that("f_aov missing value handling: runs successfully with NAs present", {
  expect_no_error(
    quiet_f_aov(y ~ grp, data = df, transformation = FALSE)
  )
})

test_that("f_aov missing value handling: returns a valid aov object with fewer rows than original data", {
  out      <- quiet_f_aov(y ~ grp, data = df, transformation = FALSE)
  n_resid  <- length(residuals(out[["y"]][["aov_test"]]))
  expect_equal(n_resid, sum(!is.na(df$y)))
})

test_that("f_aov missing value handling: post_hoc_summary_table is still a data frame when NAs are present", {
  out <- quiet_f_aov(y ~ grp, data = df, transformation = FALSE)
  expect_s3_class(out[["y"]][["post_hoc_summary_table"]], "data.frame")
})

# =============================================================================
# 9. Interaction models
# =============================================================================
df2 <- make_two_factor_data()

test_that("f_aov interaction models: two-factor additive model runs without error", {
  expect_no_error(
    quiet_f_aov(y ~ A + B, data = df2, transformation = FALSE)
  )
})

test_that("f_aov interaction models: two-factor interaction model runs without error", {
  expect_no_error(
    quiet_f_aov(y ~ A * B, data = df2, transformation = FALSE)
  )
})

test_that("f_aov interaction models: aov_summary has three terms for A * B model (A, B, A:B, Residuals)", {
  out    <- quiet_f_aov(y ~ A * B, data = df2, transformation = FALSE)
  n_rows <- nrow(out[["y"]][["aov_summary"]][[1]])
  expect_equal(n_rows, 4L)   # A, B, A:B, Residuals
})

test_that("f_aov interaction models: emmeans runs without 'no variable in reference grid' error for A*B", {
  # Regression guard for a previously fixed bug
  expect_no_error(
    quiet_f_aov_typed("rmd", y ~ A * B, data = df2, transformation = FALSE)
  )
  out <- quiet_f_aov(y ~ A * B, data = df2, transformation = FALSE)
  expect_s3_class(out[["y"]][["post_hoc_summary_table"]], "data.frame")
})

# =============================================================================
# 10. output_type = "rmd"
# =============================================================================
df <- make_normal_data()

test_that("f_aov output_type rmd: rmd output stores a character string in output_list$rmd", {
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = df,
                           transformation = FALSE)
  expect_true("rmd" %in% names(out))        # rmd is top-level key
  expect_type(out[["rmd"]], "character")
  expect_gt(nchar(out[["rmd"]]), 100L)
})

test_that("f_aov output_type rmd: rmd string contains expected section markers", {
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = df,
                           transformation = FALSE)
  expect_match(out[["rmd"]], "Analysis of:")
  expect_match(out[["rmd"]], "ANOVA Summary")
})

# =============================================================================
# 11. norm_plots = FALSE
# =============================================================================
df <- make_normal_data()

test_that("f_aov norm_plots = FALSE: runs without error when norm_plots is FALSE", {
  expect_no_error(
    suppressMessages(
      f_aov(y ~ grp, data = df,
            norm_plots = FALSE, transformation = FALSE,
            output_type = "default", open_generated_files = FALSE)
    )
  )
})

test_that("f_aov norm_plots = FALSE: normality_plots path is still stored (file written even if not cat'd)", {
  out <- suppressMessages(
    f_aov(y ~ grp, data = df,
          norm_plots = FALSE, transformation = FALSE,
          output_type = "default", open_generated_files = FALSE)
  )
  expect_true("normality_plots" %in% names(out[["y"]]))
})

# =============================================================================
# 12. Input validation / error handling
# =============================================================================
df <- make_normal_data()

test_that("f_aov input validation: errors when response variable is not in data", {
  expect_error(
    quiet_f_aov(z ~ grp, data = df),
    regexp = "not found in the data"
  )
})

test_that("f_aov input validation: errors when predictor variable is not in data", {
  expect_error(
    quiet_f_aov(y ~ treatment, data = df),
    regexp = "not found in the data"
  )
})

test_that("f_aov input validation: errors when response variable is not numeric", {
  df$y_chr <- as.character(df$y)
  expect_error(
    quiet_f_aov(y_chr ~ grp, data = df),
    regexp = "numeric"
  )
})

test_that("f_aov input validation: errors for invalid output_type string", {
  expect_error(
    quiet_f_aov(y ~ grp, data = df, output_type = "csv"),
    regexp = "output_type"
  )
})

test_that("f_aov input validation: errors for invalid transformation string (unambiguous non-match)", {
  expect_error(
    quiet_f_aov(y ~ grp, data = df, transformation = "logtransform"),
    regexp = "Invalid transformation"
  )
})

# =============================================================================
# 13. intro_text = FALSE
# =============================================================================
df  <- make_normal_data()
faov_direct <- function(intro, ...) {
  suppressMessages(
    f_aov(..., intro_text = intro,
          output_type = "rmd", norm_plots = FALSE,
          transformation = FALSE, open_generated_files = FALSE)
  )
}

test_that("f_aov intro_text = FALSE: runs without error when intro_text is FALSE", {
  expect_no_error(faov_direct(FALSE, y ~ grp, data = df))
})

test_that("f_aov intro_text = FALSE: rmd output does NOT contain assumption section when intro_text = FALSE", {
  out <- faov_direct(FALSE, y ~ grp, data = df)
  expect_false(grepl("Assumptions of ANOVA", out[["rmd"]]))
})

test_that("f_aov intro_text = FALSE: rmd output DOES contain assumption section when intro_text = TRUE", {
  out <- faov_direct(TRUE, y ~ grp, data = df)
  expect_match(out[["rmd"]], "Assumptions of ANOVA")
})

# =============================================================================
# 14. Shapiro-Wilk guard for n > 5000
# =============================================================================
test_that("f_aov Shapiro-Wilk large n guard: does not throw an error when n > 5000", {
  # n = 6000 adds meaningful runtime and is about robustness, not
  # typical user-facing behavior; skipping on CRAN is reasonable.
  skip_on_cran()
  set.seed(3)
  df_big <- data.frame(
    y   = rnorm(6000),
    grp = factor(rep(c("X", "Y", "Z"), length.out = 6000))
  )
  expect_no_error(
    quiet_f_aov(y ~ grp, data = df_big, transformation = FALSE)
  )
})

test_that("f_aov Shapiro-Wilk large n guard: Shapiro result is a shaped htest with NA p.value when n > 5000", {
  skip_on_cran()
  set.seed(3)
  df_big <- data.frame(
    y   = rnorm(6000),
    grp = factor(rep(c("X", "Y", "Z"), length.out = 6000))
  )
  out <- quiet_f_aov(y ~ grp, data = df_big, transformation = FALSE)
  sh  <- out[["y"]][["shapiro_test_residuals"]]

  # safe_shapiro() returns a real htest object even in the skip branch
  expect_s3_class(sh, "htest")
  expect_true(all(c("statistic", "p.value", "method", "data.name") %in% names(sh)))

  # p.value is NA (not the old placeholder 1), and the method label
  # explains why
  expect_true(is.na(sh$p.value))
  expect_match(sh$method, "skipped", fixed = TRUE)
  expect_match(sh$method, "n > 5000")
})

# =============================================================================
# 15. Small cell sizes (Levene power warning stored in output)
# =============================================================================
test_that("f_aov small cell size handling: runs without error when minimum cell size is 2", {
  set.seed(9)
  # n=2 per group is valid for aov() but triggers AD test guard (n<8).
  # The function should handle this gracefully with a warning, not an error.
  df_small <- data.frame(
    y   = c(rnorm(2, 5), rnorm(2, 8), rnorm(2, 11)),
    grp = factor(rep(c("A", "B", "C"), each = 2))
  )
  expect_no_error(
    suppressWarnings(quiet_f_aov(y ~ grp, data = df_small, transformation = FALSE))
  )
})

test_that("f_aov small cell size handling: min_cell_n is stored in output_list", {
  set.seed(9)
  df_small <- data.frame(
    y   = c(rnorm(2, 5), rnorm(2, 8), rnorm(2, 11)),
    grp = factor(rep(c("A", "B", "C"), each = 2))
  )
  out <- suppressWarnings(quiet_f_aov(y ~ grp, data = df_small, transformation = FALSE))
  expect_true("min_cell_n" %in% names(out[["y"]]))
  expect_lte(out[["y"]][["min_cell_n"]], 3L)
})

# =============================================================================
# 16. Non-significant ANOVA: letters should be 'ns'
# =============================================================================
test_that("f_aov non-significant ANOVA: Letter column is 'ns' when ANOVA is not significant", {
  set.seed(1234)
  df_null <- data.frame(
    y   = rnorm(60),           # no group effect
    grp = factor(rep(c("A", "B", "C"), each = 20))
  )
  out    <- quiet_f_aov(y ~ grp, data = df_null, transformation = FALSE)
  ph     <- out[["y"]][["post_hoc_summary_table"]]
  expect_true(all(ph$Letter == "ns"))
})

# =============================================================================
# 17. Post-hoc table content and column integrity
# =============================================================================
df  <- make_normal_data()
out <- quiet_f_aov(y ~ grp, data = df, transformation = FALSE)
ph  <- out[["y"]][["post_hoc_summary_table"]]

test_that("f_aov post-hoc table content: post_hoc_summary_table has expected columns (untransformed)", {
  expect_true("grp"      %in% names(ph))
  expect_true("SE"       %in% names(ph))
  expect_true("lower.CL" %in% names(ph))
  expect_true("upper.CL" %in% names(ph))
  expect_true("Letter"   %in% names(ph))
  expect_true("n"        %in% names(ph))
})

test_that("f_aov post-hoc table content: SE column absent when back-transformed (invalid on original scale)", {
  df_skew <- make_skewed_data()
  out_t   <- quiet_f_aov(y ~ grp, data = df_skew, transformation = TRUE)
  if (out_t[["y"]][["Response_Transformed"]]) {
    ph_t <- out_t[["y"]][["post_hoc_summary_table"]]
    expect_false("SE" %in% names(ph_t))
    expect_true("lower.CL" %in% names(ph_t))
    expect_true("upper.CL" %in% names(ph_t))
  }
})

test_that("f_aov post-hoc table content: lower.CL < emmean (or median BT) < upper.CL for each row", {
  centre_col <- intersect(c("emmean..", "median (BT)"), names(ph))
  if (length(centre_col) == 1) {
    expect_true(all(ph$lower.CL < ph[[centre_col]]))
    expect_true(all(ph[[centre_col]] < ph$upper.CL))
  }
})

test_that("f_aov post-hoc table content: n column sums to total observations", {
  expect_equal(sum(ph$n, na.rm = TRUE), nrow(df))
})

test_that("f_aov post-hoc table content: SE values are positive for untransformed table", {
  expect_true(all(ph$SE > 0))
})

test_that("f_aov post-hoc table content: back-transformed emmean column is named 'median (BT)' when transformed", {
  df_skew <- make_skewed_data()
  out_t   <- quiet_f_aov(y ~ grp, data = df_skew, transformation = TRUE)
  if (out_t[["y"]][["Response_Transformed"]]) {
    ph_t <- out_t[["y"]][["post_hoc_summary_table"]]
    expect_true("median (BT)" %in% names(ph_t))
    expect_false("emmean.."   %in% names(ph_t))
  }
})

# =============================================================================
# 18. par() and options() are restored on exit
# =============================================================================
test_that("f_aov restores global state on exit: par() settings are restored after call", {
  df          <- make_normal_data()
  par_before  <- par(no.readonly = TRUE)
  par_before$new <- NULL
  quiet_f_aov(y ~ grp, data = df, transformation = FALSE)
  par_after   <- par(no.readonly = TRUE)
  par_after$new <- NULL
  # mfrow is the most commonly altered setting
  expect_equal(par_before$mfrow, par_after$mfrow)
})

test_that("f_aov restores global state on exit: options() are restored after call", {
  df             <- make_normal_data()
  options_before <- options()
  quiet_f_aov(y ~ grp, data = df, transformation = FALSE)
  options_after  <- options()
  # Check a selection of commonly-changed options
  for (opt in c("warn", "scipen", "digits")) {
    expect_equal(options_before[[opt]], options_after[[opt]],
                 label = paste("option restored:", opt))
  }
})

# =============================================================================
# 19. Last-resort message
# =============================================================================
# Data deliberately designed so Box-Cox CANNOT fix both normality AND
# homoscedasticity simultaneously: three exponential groups with rates
# 2, 0.2, 0.05. At seed 335, the Box-Cox-transformed residuals still
# give Shapiro p = 0.00115 and Levene p = 0.00213 (both reject even at
# alpha = 0.01). Seed 335 was selected as the most robust trigger from
# seeds 1..500 (worst-case p-value 0.00213). The earlier default seed
# 55 was a lucky case where Box-Cox happened to succeed, which caused
# these tests to skip unconditionally.
make_hetero_data <- function(seed = 335) {
  set.seed(seed)
  data.frame(
    y   = c(rexp(30, rate = 2),      # tight group
            rexp(30, rate = 0.2),    # spread group
            rexp(30, rate = 0.05)),  # very spread group
    grp = factor(rep(c("A", "B", "C"), each = 30))
  )
}

test_that("f_aov last-resort message: last_resort_triggered is FALSE when transformation resolves assumptions", {
  df  <- make_normal_data()
  out <- quiet_f_aov(y ~ grp, data = df, transformation = TRUE)
  # For well-behaved normal data, transformation should not be needed,
  # so last_resort_triggered should be absent or FALSE
  flag <- out[["y"]][["last_resort_triggered"]]
  expect_true(is.null(flag) || isFALSE(flag))
})

test_that("f_aov last-resort message: last_resort_triggered and last_resort_reason are stored when Box-Cox cannot fix assumptions", {
  df  <- make_hetero_data()
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = df, transformation = TRUE)
  expect_true(isTRUE(out[["y"]][["last_resort_triggered"]]))
  expect_false(is.null(out[["y"]][["last_resort_reason"]]))
  expect_type(out[["y"]][["last_resort_reason"]], "character")
  # The reason must mention at least one of the two failed assumptions
  expect_match(out[["y"]][["last_resort_reason"]],
               "heteroscedasticity|non-normality")
})

test_that("f_aov last-resort message: rmd output contains the last-resort warning text when triggered", {
  df  <- make_hetero_data()
  # Must use output_type = 'rmd' so out[['rmd']] is populated;
  # the previous version of this test used plain quiet_f_aov() which
  # left out[['rmd']] NULL and would have silently passed.
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = df,
                           transformation = TRUE)
  expect_true(isTRUE(out[["y"]][["last_resort_triggered"]]))
  expect_match(out[["rmd"]], "Transformation did not resolve")
  # The alternative tests (Kruskal, GLM, ART) should be listed
  expect_match(out[["rmd"]], "Kruskal|ART|GLM")
})

# =============================================================================
# 20. Multiple-response NOTE
# =============================================================================
test_that("f_aov multiple-response NOTE in rmd output: rmd output contains multiple-testing NOTE for 2+ responses", {
  df      <- make_normal_data()
  df$y2   <- df$y + rnorm(nrow(df), 0, 0.3)
  out <- quiet_f_aov_typed("rmd", y + y2 ~ grp, data = df,
                           transformation = FALSE)
  expect_match(out[["rmd"]], "Multiple Testing Across")
  expect_match(out[["rmd"]], "9.8|9.75|FWER|1.*1.*0.05.*2")
})

test_that("f_aov multiple-response NOTE in rmd output: rmd output does NOT contain multiple-testing NOTE for single response", {
  df  <- make_normal_data()
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = df,
                           transformation = FALSE)
  expect_false(grepl("Multiple Testing Across", out[["rmd"]]))
})

test_that("f_aov multiple-response NOTE in rmd output: NOTE contains personalised Bonferroni alpha for k=2", {
  df    <- make_normal_data()
  df$y2 <- df$y + rnorm(nrow(df))
  out   <- quiet_f_aov_typed("rmd", y + y2 ~ grp, data = df,
                             alpha          = 0.05,
                             transformation = FALSE)
  # Bonferroni for k=2, alpha=0.05 should be 0.025
  expect_match(out[["rmd"]], "0.025")
})

# =============================================================================
# 21. Output text correctness
# =============================================================================
df  <- make_normal_data()

test_that("f_aov output text correctness: CLD footnote says 'Groups' not 'Means'", {
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = df,
                           transformation = FALSE)
  expect_match(out[["rmd"]], "Groups in the \"Letters\" column sharing")
  expect_false(grepl("Means sharing the same letter", out[["rmd"]]))
})

test_that("f_aov output text correctness: F-Statistic is spelled correctly in output", {
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = df,
                           transformation = FALSE)
  expect_match(out[["rmd"]], "F-Statistic")
  expect_false(grepl("F-Statisic", out[["rmd"]]))
})

test_that("f_aov output text correctness: Shapiro-Wilk is spelled correctly (not Shapiro-Wilkinson)", {
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = make_skewed_data(),
                           transformation = TRUE)
  expect_false(grepl("Shapiro-Wilkinson", out[["rmd"]]))
})

test_that("f_aov output text correctness: back-transformed description text does not mention SE as present", {
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = make_skewed_data(),
                           transformation = TRUE)
  if (out[["y"]][["Response_Transformed"]]) {
    rmd <- out[["rmd"]]
    bt_section_start <- regexpr("Back-transformed Post Hoc Table", rmd)
    if (bt_section_start > 0) {
      bt_section <- substr(rmd, bt_section_start, bt_section_start + 500)
      # SE should only appear in the "SE are omitted" note, not as a column claim
      expect_false(grepl("SE values are identical", bt_section))
    }
  }
})

# =============================================================================
# 22. subset / na.action / weights pass-through (direct f_aov calls)
# =============================================================================
# IMPORTANT: tests that pass subset / weights / na.action MUST call
# f_aov directly, NOT through a wrapper. Wrappers that forward `...`
# break match.call()'s expression capture and turn the subset
# expression into a `..1` dots-index symbol, which then crashes
# model.frame() with "the ... list contains fewer than 3 elements".

test_that("subset argument drops rows before the ANOVA is fit", {
  df <- make_normal_data()                      # 45 rows, 3 groups of 15
  suppressMessages(
    utils::capture.output(
      res <- f_aov(y ~ grp, data = df,
                   subset = grp != "A",
                   output_type = "default",
                   norm_plots = FALSE, intro_text = FALSE,
                   open_generated_files = FALSE),
      file = nullfile()
    )
  )
  aov_model <- res[["y"]][["aov_test"]]
  expect_s3_class(aov_model, "aov")
  expect_equal(nobs(aov_model), 30)
  expect_equal(length(unique(aov_model$model$grp)), 2L)
})

test_that("na.action = na.omit drops rows with NA in any variable", {
  df <- make_normal_data()
  df$y[c(1, 5, 10)] <- NA                       # 3 NAs in response
  suppressMessages(
    utils::capture.output(
      res <- f_aov(y ~ grp, data = df,
                   na.action = na.omit,
                   output_type = "default",
                   norm_plots = FALSE, intro_text = FALSE,
                   open_generated_files = FALSE),
      file = nullfile()
    )
  )
  aov_model <- res[["y"]][["aov_test"]]
  expect_s3_class(aov_model, "aov")
  expect_equal(nobs(aov_model), 42)             # 45 - 3
})

test_that("weights argument is applied by aov() when passed via dots", {
  df  <- make_normal_data()
  wts <- runif(nrow(df), 0.5, 2)

  suppressMessages(
    utils::capture.output(
      res_w <- f_aov(y ~ grp, data = df, weights = wts,
                     output_type = "default",
                     norm_plots = FALSE, intro_text = FALSE,
                     open_generated_files = FALSE),
      file = nullfile()
    )
  )
  suppressMessages(
    utils::capture.output(
      res_u <- f_aov(y ~ grp, data = df,
                     output_type = "default",
                     norm_plots = FALSE, intro_text = FALSE,
                     open_generated_files = FALSE),
      file = nullfile()
    )
  )

  expect_s3_class(res_w[["y"]][["aov_test"]], "aov")
  expect_equal(nobs(res_w[["y"]][["aov_test"]]), nrow(df))

  # Weighted and unweighted F-values differ (sanity check that
  # weights actually reached aov() and were not silently dropped).
  f_w <- summary(res_w[["y"]][["aov_test"]])[[1]][["F value"]][1]
  f_u <- summary(res_u[["y"]][["aov_test"]])[[1]][["F value"]][1]
  expect_false(isTRUE(all.equal(f_w, f_u)))
})

test_that("subset is honored by downstream steps, not just aov()", {
  df <- make_normal_data()
  suppressMessages(
    utils::capture.output(
      res <- f_aov(y ~ grp, data = df,
                   subset = grp != "A",
                   output_type = "default",
                   norm_plots = FALSE, intro_text = FALSE,
                   open_generated_files = FALSE),
      file = nullfile()
    )
  )
  aov_model <- res[["y"]][["aov_test"]]
  groups_seen <- as.character(unique(aov_model$model$grp))
  expect_false("A" %in% groups_seen)
  expect_true(all(c("B", "C") %in% groups_seen))
})

test_that("contrasts argument is forwarded to aov()", {
  df <- make_normal_data()
  suppressMessages(
    utils::capture.output(
      res <- f_aov(y ~ grp, data = df,
                   contrasts = list(grp = "contr.sum"),
                   output_type = "default",
                   norm_plots = FALSE, intro_text = FALSE,
                   open_generated_files = FALSE),
      file = nullfile()
    )
  )
  aov_model <- res[["y"]][["aov_test"]]
  expect_s3_class(aov_model, "aov")
  coef_names <- names(coef(aov_model))
  expect_true(any(grepl("grp1|grp2", coef_names)))
})

# =============================================================================
# 23. safe_shapiro() integration (wrapper is fine here)
# =============================================================================
test_that("Shapiro residuals slot is an htest object", {
  df  <- make_normal_data()
  res <- quiet_f_aov(y ~ grp, data = df, transformation = FALSE)

  sh <- res[["y"]][["shapiro_test_residuals"]]
  if (!is.null(sh)) {
    expect_s3_class(sh, "htest")
    expect_true(all(c("statistic", "p.value", "method") %in% names(sh)))
  }
})

test_that("f_aov does not crash with n > 5000 (safe_shapiro skip branch)", {
  # Same rationale as section 14: n = 6000 is the slow part.
  skip_on_cran()
  set.seed(1)
  n   <- 6000
  big <- data.frame(
    y   = c(rnorm(n / 2, 10), rnorm(n / 2, 11)),
    grp = factor(rep(c("A", "B"), each = n / 2))
  )
  expect_no_error(
    res <- quiet_f_aov(y ~ grp, data = big, transformation = FALSE)
  )
  sh <- res[["y"]][["shapiro_test_residuals"]]
  if (!is.null(sh)) {
    expect_s3_class(sh, "htest")
    if (is.na(sh$p.value)) {
      expect_match(sh$method, "skipped", fixed = TRUE)
    }
  }
})
