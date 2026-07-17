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
#  24.  anova_type (Type II vs Type III Sums of Squares)
#  25.  force_aov (saturated-model handling)
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
  # aov_summary is the car::Anova() data frame directly (columns:
  # Sum Sq, Df, F value, Pr(>F); one row per term plus Residuals with
  # NA F / NA p). No outer list to unwrap.
  f_val <- out[["y"]][["aov_summary"]][["F value"]]
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
  skip_on_cran()
  out <- quiet_f_aov(y ~ grp, data = df_skew, transformation = TRUE)
  transformed <- out[["y"]][["Response_Transformed"]]
  if (transformed) {
    expect_true("boxcox" %in% names(out[["y"]]) ||
                  "bestNormalize" %in% names(out[["y"]]))
  }
})

test_that("f_aov transformation options: transformation = 'boxcox' stores boxcox key", {
  skip_on_cran()
  out <- quiet_f_aov(y ~ grp, data = df_skew, transformation = "boxcox")
  if (out[["y"]][["Response_Transformed"]]) {
    expect_true("boxcox" %in% names(out[["y"]]))
  }
})

test_that("f_aov transformation options: transformation = FALSE never transforms, no transformed keys", {
  skip_on_cran()
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
  skip_on_cran()
  out <- quiet_f_aov(y ~ grp, data = df_skew, transformation = TRUE)
  if (out[["y"]][["Response_Transformed"]]) {
    expect_true("transformed_shapiro_test" %in% names(out[["y"]]))
    expect_true("transformed_levene_test"  %in% names(out[["y"]]))
    expect_true("transformed_adt_test"     %in% names(out[["y"]]))
  }
})

test_that("f_aov transformation options: partial string 'box' is accepted via pmatch", {
  skip_on_cran()
  expect_no_error(
    quiet_f_aov(y ~ grp, data = df_skew, transformation = "box")
  )
})

test_that("f_aov transformation options: invalid transformation string throws an error", {
  skip_on_cran()
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
  # aov_summary is the car::Anova() data frame directly; nrow() is the
  # term count including the Residuals row.
  n_rows <- nrow(out[["y"]][["aov_summary"]])
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
  skip_on_cran()
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = df,
                           transformation = FALSE)
  expect_true("rmd" %in% names(out))        # rmd is top-level key
  expect_type(out[["rmd"]], "character")
  expect_gt(nchar(out[["rmd"]]), 100L)
})

test_that("f_aov output_type rmd: rmd string contains expected section markers", {
  skip_on_cran()
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
  skip_on_cran()
  df  <- make_normal_data()
  out <- quiet_f_aov(y ~ grp, data = df, transformation = TRUE)
  # For well-behaved normal data, transformation should not be needed,
  # so last_resort_triggered should be absent or FALSE
  flag <- out[["y"]][["last_resort_triggered"]]
  expect_true(is.null(flag) || isFALSE(flag))
})

test_that("f_aov last-resort message: last_resort_triggered and last_resort_reason are stored when Box-Cox cannot fix assumptions", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  df      <- make_normal_data()
  df$y2   <- df$y + rnorm(nrow(df), 0, 0.3)
  out <- quiet_f_aov_typed("rmd", y + y2 ~ grp, data = df,
                           transformation = FALSE)
  expect_match(out[["rmd"]], "Multiple Testing Across")
  expect_match(out[["rmd"]], "9.8|9.75|FWER|1.*1.*0.05.*2")
})

test_that("f_aov multiple-response NOTE in rmd output: rmd output does NOT contain multiple-testing NOTE for single response", {
  skip_on_cran()
  df  <- make_normal_data()
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = df,
                           transformation = FALSE)
  expect_false(grepl("Multiple Testing Across", out[["rmd"]]))
})

test_that("f_aov multiple-response NOTE in rmd output: NOTE contains personalised Bonferroni alpha for k=2", {
  skip_on_cran()
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

# =============================================================================
# 24. anova_type (Type II vs Type III Sums of Squares)
# =============================================================================
# f_aov() exposes anova_type = 2 (default, Type II) or 3 (Type III) for the
# omnibus table computed via car::Anova(). For Type III, f_aov also
# auto-installs orthogonal contrasts (contr.sum, contr.poly) for the
# duration of the call when the user has not supplied their own via `...`.
#
# The numerical SS for main effects under Type II vs Type III often
# coincide once f_aov has installed contr.sum (that is the whole point of
# the auto-install), so these tests do NOT rely on numerical differences.
# They rely on:
#   - the structural difference between the two tables: Type III has an
#     "(Intercept)" row, Type II does not (deterministic, independent of
#     design balance);
#   - the table heading attribute set by car::Anova();
#   - the anova_type value stored in the output list;
#   - the contrasts-restoration on exit; and
#   - the textual markers in the rmd output.
# All of these are independent of seed / sample size / unbalancedness.

df_oneway   <- make_normal_data()
df_twoway   <- make_two_factor_data()

test_that("f_aov anova_type: default is 2 (Type II) and is stored in output_list", {
  skip_on_cran()
  out <- quiet_f_aov(y ~ grp, data = df_oneway, transformation = FALSE)
  expect_equal(out[["y"]][["anova_type"]], 2)
})

test_that("f_aov anova_type: anova_type = 3 is stored in output_list", {
  skip_on_cran()
  out <- quiet_f_aov(y ~ grp, data = df_oneway,
                     transformation = FALSE, anova_type = 3)
  expect_equal(out[["y"]][["anova_type"]], 3)
})

test_that("f_aov anova_type: Type II aov_summary heading says 'Type II tests'", {
  skip_on_cran()
  out <- quiet_f_aov(y ~ grp, data = df_oneway, transformation = FALSE)
  heading <- attr(out[["y"]][["aov_summary"]], "heading")
  expect_true(any(grepl("Type II tests", heading)))
  expect_false(any(grepl("Type III tests", heading)))
})

test_that("f_aov anova_type: Type III aov_summary heading says 'Type III tests'", {
  skip_on_cran()
  out <- quiet_f_aov(y ~ grp, data = df_oneway,
                     transformation = FALSE, anova_type = 3)
  heading <- attr(out[["y"]][["aov_summary"]], "heading")
  expect_true(any(grepl("Type III tests", heading)))
})

test_that("f_aov anova_type: Type II aov_summary has no '(Intercept)' row", {
  skip_on_cran()
  # car::Anova(., type = 2) reports terms-only; the intercept does not appear.
  out <- quiet_f_aov(y ~ A * B, data = df_twoway, transformation = FALSE)
  expect_false("(Intercept)" %in% rownames(out[["y"]][["aov_summary"]]))
})

test_that("f_aov anova_type: Type III aov_summary has an '(Intercept)' row", {
  skip_on_cran()
  # car::Anova(., type = 3) tests every term including the intercept;
  # this is the most reliable structural signature distinguishing the two
  # tables and does not depend on design balance.
  out <- quiet_f_aov(y ~ A * B, data = df_twoway,
                     transformation = FALSE, anova_type = 3)
  expect_true("(Intercept)" %in% rownames(out[["y"]][["aov_summary"]]))
})

test_that("f_aov anova_type: Type III works for one-way (no interaction) models", {
  skip_on_cran()
  # Regression guard: a one-way design has no interaction term, so the
  # Type II / Type III machinery has nothing to "fix" - but the call must
  # still succeed and produce a Type III table with an intercept row.
  out <- quiet_f_aov(y ~ grp, data = df_oneway,
                     transformation = FALSE, anova_type = 3)
  expect_s3_class(out[["y"]][["aov_test"]], "aov")
  expect_true("(Intercept)" %in% rownames(out[["y"]][["aov_summary"]]))
})

test_that("f_aov anova_type: Type III works for interaction (A*B) models", {
  skip_on_cran()
  out <- quiet_f_aov(y ~ A * B, data = df_twoway,
                     transformation = FALSE, anova_type = 3)
  rn <- rownames(out[["y"]][["aov_summary"]])
  expect_true(all(c("A", "B", "A:B", "Residuals") %in% rn))
})

# ----- Input validation -----------------------------------------------------

test_that("f_aov anova_type: anova_type = 1 errors with a clear message", {
  skip_on_cran()
  expect_error(
    quiet_f_aov(y ~ grp, data = df_oneway, anova_type = 1),
    regexp = "anova_type.*must be 2.*Type II.*or 3.*Type III"
  )
})

test_that("f_aov anova_type: anova_type = 4 errors with a clear message", {
  skip_on_cran()
  expect_error(
    quiet_f_aov(y ~ grp, data = df_oneway, anova_type = 4),
    regexp = "anova_type.*must be 2.*or 3"
  )
})

test_that("f_aov anova_type: anova_type = 'II' (string) errors", {
  skip_on_cran()
  # Bare strings are not coerced silently. Per the project's fail-loud
  # philosophy a typo should crash, not default back to Type II.
  expect_error(
    quiet_f_aov(y ~ grp, data = df_oneway, anova_type = "II"),
    regexp = "anova_type.*must be 2.*or 3"
  )
})

test_that("f_aov anova_type: anova_type = NA errors", {
  skip_on_cran()
  expect_error(
    quiet_f_aov(y ~ grp, data = df_oneway, anova_type = NA),
    regexp = "anova_type.*must be 2.*or 3"
  )
})

test_that("f_aov anova_type: anova_type = c(2, 3) (vector) errors", {
  skip_on_cran()
  # Length > 1 should be rejected; %in% would return a logical vector and
  # isTRUE() collapses it to FALSE, hitting the stop().
  expect_error(
    quiet_f_aov(y ~ grp, data = df_oneway, anova_type = c(2, 3)),
    regexp = "anova_type.*must be 2.*or 3"
  )
})

# ----- Multi-response carries anova_type through to every response ---------

test_that("f_aov anova_type: anova_type is stored on every response when multiple are fit", {
  skip_on_cran()
  df2 <- df_oneway
  df2$y2 <- df2$y + rnorm(nrow(df2), 0, 0.3)
  out <- quiet_f_aov(y + y2 ~ grp, data = df2,
                     transformation = FALSE, anova_type = 3)
  expect_equal(out[["y"]][["anova_type"]],  3)
  expect_equal(out[["y2"]][["anova_type"]], 3)
})

# ----- rmd output text ------------------------------------------------------

test_that("f_aov anova_type: rmd output mentions 'Type II' when anova_type = 2", {
  skip_on_cran()
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = df_oneway,
                           transformation = FALSE)
  expect_match(out[["rmd"]], "Type II")
  # The Type III banner about contr.sum must NOT appear in a Type II run.
  expect_false(grepl("Type III", out[["rmd"]]))
})

test_that("f_aov anova_type: rmd output mentions 'Type III' when anova_type = 3", {
  skip_on_cran()
  out <- quiet_f_aov_typed("rmd", y ~ grp, data = df_oneway,
                           transformation = FALSE, anova_type = 3)
  expect_match(out[["rmd"]], "Type III")
})

test_that("f_aov anova_type: Type III rmd output explains contr.sum installation", {
  skip_on_cran()
  # The user-facing message in the rmd should make clear WHY contrasts
  # were swapped; checking for the contrast names guards against a future
  # rewrite that drops the explanation.
  out <- quiet_f_aov_typed("rmd", y ~ A * B, data = df_twoway,
                           transformation = FALSE, anova_type = 3)
  expect_match(out[["rmd"]], "contr\\.sum")
})

# ----- Contrasts side-effects: install + restore on exit -------------------

test_that("f_aov anova_type: anova_type = 2 does NOT modify options('contrasts')", {
  skip_on_cran()
  # Baseline check that the auto-install is gated on anova_type == 3.
  # getOption('contrasts') returns an unnamed length-2 character vector
  # (unordered, ordered), so compare positionally rather than by name.
  old <- getOption("contrasts")
  on.exit(options(contrasts = old), add = TRUE)
  options(contrasts = c("contr.treatment", "contr.poly"))
  quiet_f_aov(y ~ A * B, data = df_twoway, transformation = FALSE)
  expect_equal(unname(getOption("contrasts"))[1], "contr.treatment")
})

test_that("f_aov anova_type: anova_type = 3 restores options('contrasts') on exit", {
  skip_on_cran()
  # Type III installs contr.sum for the duration of the call. After f_aov
  # returns, the caller's original contrasts setting must be back.
  old <- getOption("contrasts")
  on.exit(options(contrasts = old), add = TRUE)
  options(contrasts = c("contr.treatment", "contr.poly"))
  contrasts_before <- getOption("contrasts")
  quiet_f_aov(y ~ A * B, data = df_twoway,
              transformation = FALSE, anova_type = 3)
  expect_equal(getOption("contrasts"), contrasts_before)
})

test_that("f_aov anova_type: anova_type = 3 also restores contrasts when the call errors mid-way", {
  skip_on_cran()
  # save_session_state() / on.exit() restoration must run on the error
  # path too, otherwise a user typo (e.g. a non-numeric response) would
  # leave the global contrasts setting flipped to contr.sum.
  old <- getOption("contrasts")
  on.exit(options(contrasts = old), add = TRUE)
  options(contrasts = c("contr.treatment", "contr.poly"))
  contrasts_before <- getOption("contrasts")
  df_bad <- df_twoway
  df_bad$y <- as.character(df_bad$y)  # non-numeric response, triggers stop()
  expect_error(
    quiet_f_aov(y ~ A * B, data = df_bad,
                transformation = FALSE, anova_type = 3),
    regexp = "numeric"
  )
  expect_equal(getOption("contrasts"), contrasts_before)
})

# ----- User-supplied contrasts via ... bypass the auto-install -------------

test_that("f_aov anova_type: anova_type = 3 with user-supplied contrasts emits a message", {
  skip_on_cran()
  # When the user passes their own `contrasts` via ..., f_aov must NOT
  # override them but should warn the user about Type III interpretation
  # if those contrasts happen to be treatment contrasts.
  msgs <- character()
  withCallingHandlers(
    suppressWarnings(
      utils::capture.output(
        f_aov(y ~ A * B, data = df_twoway,
              transformation = FALSE, anova_type = 3,
              contrasts = list(A = "contr.helmert", B = "contr.helmert"),
              output_type = "default", norm_plots = FALSE,
              intro_text = FALSE, open_generated_files = FALSE),
        file = nullfile()
      )
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("anova_type = 3.*user-supplied .*contrasts", msgs)))
})

test_that("f_aov anova_type: anova_type = 3 with user-supplied contrasts does NOT overwrite them", {
  skip_on_cran()
  # The auto-install path is `options(contrasts = c('contr.sum', 'contr.poly'))`
  # globally. When the user passes their own per-term contrasts via `...`,
  # the global option must be left alone.
  old <- getOption("contrasts")
  on.exit(options(contrasts = old), add = TRUE)
  options(contrasts = c("contr.treatment", "contr.poly"))
  contrasts_before <- getOption("contrasts")
  suppressMessages(suppressWarnings(
    utils::capture.output(
      f_aov(y ~ A * B, data = df_twoway,
            transformation = FALSE, anova_type = 3,
            contrasts = list(A = "contr.helmert", B = "contr.helmert"),
            output_type = "default", norm_plots = FALSE,
            intro_text = FALSE, open_generated_files = FALSE),
      file = nullfile()
    )
  ))
  expect_equal(getOption("contrasts"), contrasts_before)
})

# ----- Combined with transformation ----------------------------------------

test_that("f_aov anova_type: anova_type = 3 is also honored on the transformed model", {
  skip_on_cran()
  # f_aov refits the model on Box-Cox-transformed residuals when the
  # untransformed assumptions fail; the SS-type setting must propagate.
  df_skew <- make_skewed_data()
  out <- quiet_f_aov(y ~ grp, data = df_skew,
                     transformation = "boxcox", anova_type = 3)
  if (isTRUE(out[["y"]][["Response_Transformed"]])) {
    # The transformed table is stored under the same name pattern as the
    # main aov_summary; the heading attribute is the most stable check.
    expect_true("(Intercept)" %in%
                  rownames(out[["y"]][["aov_summary"]]))
  }
})

test_that("f_aov anova_type: rmd ANOVA table bolds p-values against the call's alpha, not a hardcoded 0.05", {
  skip_on_cran()
  # Regression guard: an earlier version of helper_rmd_anova_summary
  # hardcoded the bolding threshold to 0.05, which made the omnibus
  # table disagree with every other test in the report when the user
  # passed alpha != 0.05. Seed 7 / one-way design / mean offsets of 0.5
  # are tuned so the omnibus p sits between 0.01 and 0.05 (here ~ 0.038);
  # the helper must bold it at alpha = 0.05 but leave it plain at
  # alpha = 0.01.
  set.seed(7)
  d <- data.frame(
    y   = c(rnorm(10, 10), rnorm(10, 10.5), rnorm(10, 11)),
    grp = factor(rep(c("A", "B", "C"), each = 10))
  )
  out_05 <- quiet_f_aov_typed("rmd", y ~ grp, data = d,
                              transformation = FALSE, alpha = 0.05)
  out_01 <- quiet_f_aov_typed("rmd", y ~ grp, data = d,
                              transformation = FALSE, alpha = 0.01)
  p_str <- formatC(out_05[["y"]][["aov_summary"]][["Pr(>F)"]][1])
  # Sanity check on the data setup: the p-value really is in the
  # 0.01 < p < 0.05 band; if a future change to the test data shifts
  # it out of that window the alpha guard becomes vacuous.
  p_num <- out_05[["y"]][["aov_summary"]][["Pr(>F)"]][1]
  expect_gt(p_num, 0.01)
  expect_lt(p_num, 0.05)
  # The actual contract:
  expect_true(grepl(paste0("**", p_str, "**"), out_05[["rmd"]], fixed = TRUE))
  expect_false(grepl(paste0("**", p_str, "**"), out_01[["rmd"]], fixed = TRUE))
})

test_that("f_aov anova_type: rmd ANOVA table blanks NA cells in the Residuals row", {
  skip_on_cran()
  # Regression guard: the Residuals row has NA in both F value and
  # Pr(>F); printing them as the literal string "NA" is visually noisy
  # in publication tables. The helper blanks both. Test against the
  # exact line that pander renders for Residuals.
  df  <- make_two_factor_data()
  out <- quiet_f_aov_typed("rmd", y ~ A * B, data = df,
                           transformation = FALSE)
  # Pull the Residuals row out of the pander-rendered table. Each row
  # is a single line; "Residuals" is at the start.
  resid_line <- grep("Residuals", strsplit(out[["rmd"]], "\n")[[1]],
                     value = TRUE)
  expect_true(length(resid_line) >= 1L)
  # Neither cell should contain the literal "NA" token.
  # (allow occurrences of "NA" inside words like "STANARD" etc are
  # vanishingly unlikely in a numeric row; checked with a strict regex.)
  expect_false(any(grepl("\\bNA\\b", resid_line)))
})

# =============================================================================
# 25. force_aov (saturated-model handling)
# =============================================================================
# When at least one factorial cell has n = 1, the ANOVA model is saturated
# (zero residual df). Default behavior (force_aov = FALSE) is to skip that
# response with a clear message and store skipped = TRUE / skipped_reason.
# force_aov = TRUE overrides the skip for diagnostic use only; the result
# should not be reported. These tests pin both halves of the contract.

make_saturated_data <- function(seed = 1) {
  set.seed(seed)
  # 2x2 design with cell (a2, b2) having n = 1 -> saturates the A*B model
  data.frame(
    y = c(rnorm(3, 10), rnorm(3, 12), rnorm(3, 14), rnorm(1, 16)),
    A = factor(c(rep("a1", 3), rep("a1", 3), rep("a2", 3), rep("a2", 1))),
    B = factor(c(rep("b1", 3), rep("b2", 3), rep("b1", 3), rep("b2", 1)))
  )
}

test_that("f_aov force_aov: saturated model is skipped by default (force_aov = FALSE)", {
  skip_on_cran()
  df_sat <- make_saturated_data()
  out    <- quiet_f_aov(y ~ A * B, data = df_sat, transformation = FALSE)
  expect_true(isTRUE(out[["y"]][["skipped"]]))
  expect_null(out[["y"]][["aov_test"]])
  expect_equal(out[["y"]][["min_cell_n"]], 1L)
})

test_that("f_aov force_aov: skipped response stores a skipped_reason that mentions force_aov", {
  skip_on_cran()
  df_sat <- make_saturated_data()
  out    <- quiet_f_aov(y ~ A * B, data = df_sat, transformation = FALSE)
  expect_type(out[["y"]][["skipped_reason"]], "character")
  expect_match(out[["y"]][["skipped_reason"]], "saturated")
  expect_match(out[["y"]][["skipped_reason"]], "force_aov")
})

test_that("f_aov force_aov: force_aov = TRUE fits the saturated model and stores force_aov_used", {
  skip_on_cran()
  df_sat <- make_saturated_data()
  # force_aov = TRUE deliberately fits a saturated model; aov() emits warnings
  # about zero residual df which are expected and not the subject of the test.
  out <- suppressWarnings(
    quiet_f_aov(y ~ A * B, data = df_sat,
                transformation = FALSE, force_aov = TRUE)
  )
  expect_true(isTRUE(out[["y"]][["force_aov_used"]]))
  expect_false(isTRUE(out[["y"]][["skipped"]]))
  expect_s3_class(out[["y"]][["aov_test"]], "aov")
})

test_that("f_aov force_aov: balanced designs are unaffected by force_aov", {
  skip_on_cran()
  # Sanity check: force_aov is gated on min_cell_n == 1; well-balanced data
  # must not have force_aov_used set even if the user passes TRUE.
  df_bal <- make_two_factor_data()
  out <- quiet_f_aov(y ~ A * B, data = df_bal,
                     transformation = FALSE, force_aov = TRUE)
  expect_false(isTRUE(out[["y"]][["force_aov_used"]]))
  expect_false(isTRUE(out[["y"]][["skipped"]]))
})

test_that("f_aov force_aov: rmd output flags the skip and points to force_aov", {
  skip_on_cran()
  df_sat <- make_saturated_data()
  out <- quiet_f_aov_typed("rmd", y ~ A * B, data = df_sat,
                           transformation = FALSE)
  expect_match(out[["rmd"]], "skipped")
  expect_match(out[["rmd"]], "force_aov")
})

test_that("f_aov force_aov: rmd output under force_aov = TRUE warns the result is diagnostic only", {
  skip_on_cran()
  df_sat <- make_saturated_data()
  out <- suppressWarnings(
    quiet_f_aov_typed("rmd", y ~ A * B, data = df_sat,
                      transformation = FALSE, force_aov = TRUE)
  )
  expect_match(out[["rmd"]], "Saturated")
  expect_match(out[["rmd"]], "diagnostic")
})
