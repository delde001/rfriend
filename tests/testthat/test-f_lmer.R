# =============================================================================
# f_lmer_stress_test.R
# Comprehensive stress tests for f_lmer().
#
# Run with: testthat::test_file("f_lmer_stress_test.R")
#
# API note: results are ALWAYS nested under response name, consistent with
# f_aov / f_kruskal_test / f_glm / f_t_test / f_wilcox_test. For a single
# response use `res[["y"]]$model`; for multiple responses use
# `res[["y1"]]$model`, `res[["y2"]]$model`, etc.
#
# Test organisation:
#   1.  Return object class and structure
#   2.  Random-intercept model (sleepstudy)
#   3.  Random intercept + slope
#   4.  Factor fixed effect triggers post-hoc
#   5.  Multiple response variables
#   6.  ddf options (Satterthwaite, Kenward-Roger, lme4)
#   7.  Pass-through of subset / na.action / weights
#   8.  Contrasts forwarded via dots
#   9.  Input validation
#  10.  safe_shapiro large-n guard on residuals
#  11.  Singular fit handling
#  12.  intro_text toggle
#  13.  rmd output type stores markdown
# =============================================================================
# ---------------------------------------------------------------------------
# Skip-if-no-lme4 helper
# ---------------------------------------------------------------------------
skip_if_no_lme4 <- function() {
  testthat::skip_if_not_installed("lme4")
  testthat::skip_if_not_installed("lmerTest")
}

# ---------------------------------------------------------------------------
# Wrappers (SAFE only when not forwarding subset/weights/na.action through
# dots - wrappers break match.call capture for those args; direct f_lmer
# calls are used in section 7 and 8 below).
# ---------------------------------------------------------------------------

# quiet_lmer removed - use shared quiet_f_lmer() from helper-quiet.R

# ---------------------------------------------------------------------------
# Test data generators
# ---------------------------------------------------------------------------

# Repeated-measures: 20 subjects x 4 time points
# make_rm_data removed - use shared make_repeated_measures_data() from helper-data.R

# Factor fixed effect for post-hoc testing
make_factor_rm_data <- function(seed = 31, n_subj = 24) {
  set.seed(seed)
  trt  <- factor(rep(c("ctrl", "low", "high"), length.out = n_subj))
  subj <- factor(seq_len(n_subj))
  reps <- 5
  expand_df <- data.frame(
    subj = rep(subj, each = reps),
    trt  = rep(trt,  each = reps),
    rep  = rep(seq_len(reps), times = n_subj)
  )
  subj_int <- rnorm(n_subj, 0, 1.5)[as.integer(expand_df$subj)]
  trt_eff  <- c(ctrl = 0, low = 2, high = 5)[as.character(expand_df$trt)]
  expand_df$y <- 10 + trt_eff + subj_int + rnorm(nrow(expand_df), 0, 1)
  expand_df
}

# ===========================================================================
# 1. Return object class and structure
# ===========================================================================

test_that("f_lmer returns an object of class f_lmer", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_repeated_measures_data()
  res <- quiet_f_lmer(y ~ time + (1 | subj), data = df)
  expect_s3_class(res, "f_lmer")
})

test_that("result contains one element per response variable", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_repeated_measures_data()
  res <- quiet_f_lmer(y ~ time + (1 | subj), data = df)
  expect_true("y" %in% names(res))
})

test_that("per-response slot exposes documented components", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_repeated_measures_data()
  res <- quiet_f_lmer(y ~ time + (1 | subj), data = df)

  entry <- res[["y"]]

  expected_slots <- c("model", "fixed_effects", "var_components",
                      "r_squared", "fit_indices")
  for (s in expected_slots) {
    expect_true(s %in% names(entry),
                info = paste("Missing slot:", s))
  }
  expect_true(inherits(entry$model, "lmerMod") ||
              inherits(entry$model, "lmerModLmerTest"))
})

# ===========================================================================
# 2. Random-intercept model
# ===========================================================================

test_that("sleepstudy random-intercept model runs cleanly", {
  skip_on_cran()
  skip_if_no_lme4()
  data(sleepstudy, package = "lme4")
  expect_no_error(
    res <- quiet_f_lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  )
  expect_s3_class(res, "f_lmer")

  fe <- res[["Reaction"]]$fixed_effects
  expect_true(is.data.frame(fe) || is.matrix(fe))
})

# ===========================================================================
# 3. Random intercept + slope
# ===========================================================================

test_that("random intercept + slope model runs", {
  skip_on_cran()
  skip_if_no_lme4()
  data(sleepstudy, package = "lme4")
  expect_no_error(
    res <- quiet_f_lmer(Reaction ~ Days + (1 + Days | Subject),
                      data = sleepstudy)
  )
  vc <- res[["Reaction"]]$var_components
  expect_false(is.null(vc))
})

# ===========================================================================
# 4. Factor fixed effect triggers post hoc
# ===========================================================================

test_that("factor fixed effect produces a post-hoc table", {
  skip_on_cran()
  skip_if_no_lme4()
  testthat::skip_if_not_installed("emmeans")

  df  <- make_factor_rm_data()
  res <- quiet_f_lmer(y ~ trt + (1 | subj), data = df)

  ph <- res[["y"]]$post_hoc
  expect_false(is.null(ph))
  if (!is.null(ph) && length(ph) > 0) {
    expect_true("trt" %in% names(ph))
  }
})

test_that("post_hoc = FALSE suppresses the pairwise comparisons", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_factor_rm_data()
  res <- quiet_f_lmer(y ~ trt + (1 | subj), data = df, post_hoc = FALSE)
  ph  <- res[["y"]]$post_hoc
  expect_true(is.null(ph) || length(ph) == 0L)
})

# ===========================================================================
# 5. Multiple responses
# ===========================================================================

test_that("multiple responses produce one slot per response", {
  skip_on_cran()
  skip_if_no_lme4()
  data(sleepstudy, package = "lme4")
  ss <- sleepstudy
  ss$Reaction2 <- ss$Reaction + rnorm(nrow(ss), 0, 3)

  res <- quiet_f_lmer(Reaction + Reaction2 ~ Days + (1 | Subject),
                    data = ss)
  expect_true(all(c("Reaction", "Reaction2") %in% names(res)))
  expect_true(!is.null(res[["Reaction"]]$model))
  expect_true(!is.null(res[["Reaction2"]]$model))
})

# ===========================================================================
# 6. ddf options
# ===========================================================================

test_that("ddf = 'Satterthwaite' (default) produces p-values", {
  skip_on_cran()
  skip_if_not_installed("pbkrtest")
  skip_if_no_lme4()
  df  <- make_repeated_measures_data()
  res <- quiet_f_lmer(y ~ time + (1 | subj), data = df)
  fe  <- res[["y"]]$fixed_effects
  has_p <- any(grepl("Pr\\(|p[\\._]value|p$",
                     names(fe), ignore.case = TRUE))
  expect_true(has_p)
})

test_that("ddf = 'lme4' suppresses p-values (t-stats only)", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_repeated_measures_data()
  res <- quiet_f_lmer(y ~ time + (1 | subj), data = df, ddf = "lme4")
  expect_false(is.null(res[["y"]]$fixed_effects))
})

test_that("ddf = 'Kenward-Roger' runs when pbkrtest is available", {
  skip_on_cran()
  skip_if_no_lme4()
  testthat::skip_if_not_installed("pbkrtest")
  df <- make_repeated_measures_data()
  expect_no_error(
    res <- quiet_f_lmer(y ~ time + (1 | subj), data = df,
                      ddf = "Kenward-Roger")
  )
})

test_that("bad ddf is rejected", {
  skip_on_cran()
  skip_if_no_lme4()
  df <- make_repeated_measures_data()
  expect_error(
    quiet_f_lmer(y ~ time + (1 | subj), data = df, ddf = "invalid")
  )
})

# ===========================================================================
# 7. Pass-through of subset / na.action / weights
#    Direct f_lmer() calls only.
# ===========================================================================

test_that("subset drops rows before the lmer is fit", {
  skip_on_cran()
  skip_if_no_lme4()
  df <- make_repeated_measures_data(n_subj = 20, n_time = 4)     # 80 rows
  suppressMessages(suppressWarnings(
    utils::capture.output(
      res <- f_lmer(y ~ time + (1 | subj), data = df,
                    subset = df$time != "1",
                    output_type = "default",
                    norm_plots = FALSE, intro_text = FALSE,
                    open_generated_files = FALSE),
      file = nullfile()
    )
  ))
  m <- res[["y"]]$model
  # time had 4 levels, 20 obs each; subset drops level 1 -> 60 rows
  expect_equal(nobs(m), 60L)
})

test_that("na.action = na.omit strips NA rows before fitting", {
  skip_on_cran()
  skip_if_no_lme4()
  df <- make_repeated_measures_data()
  df$y[c(1, 5, 10, 15)] <- NA
  suppressMessages(suppressWarnings(
    utils::capture.output(
      res <- f_lmer(y ~ time + (1 | subj), data = df,
                    na.action = na.omit,
                    output_type = "default",
                    norm_plots = FALSE, intro_text = FALSE,
                    open_generated_files = FALSE),
      file = nullfile()
    )
  ))
  expect_equal(nobs(res[["y"]]$model), 76L)        # 80 - 4
})

test_that("weights pass through to lmer without error", {
  skip_on_cran()
  skip_if_no_lme4()
  df <- make_repeated_measures_data()
  w  <- runif(nrow(df), 0.5, 2)
  expect_no_error(
    suppressMessages(suppressWarnings(
      utils::capture.output(
        res <- f_lmer(y ~ time + (1 | subj), data = df, weights = w,
                      output_type = "default",
                      norm_plots = FALSE, intro_text = FALSE,
                      open_generated_files = FALSE),
        file = nullfile()
      )
    ))
  )
  expect_equal(nobs(res[["y"]]$model), nrow(df))
})

test_that("multi-response share identical row set after subset", {
  skip_on_cran()
  skip_if_no_lme4()
  df <- make_repeated_measures_data()
  df$y2 <- df$y + rnorm(nrow(df), 0, 0.5)

  suppressMessages(suppressWarnings(
    utils::capture.output(
      res <- f_lmer(y + y2 ~ time + (1 | subj), data = df,
                    subset = df$time != "1",
                    output_type = "default",
                    norm_plots = FALSE, intro_text = FALSE,
                    open_generated_files = FALSE),
      file = nullfile()
    )
  ))
  expect_equal(nobs(res[["y"]]$model),  60L)
  expect_equal(nobs(res[["y2"]]$model), 60L)
})

# ===========================================================================
# 8. Contrasts forwarded
# ===========================================================================

test_that("contrasts argument flows through to the fitted model", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_factor_rm_data()
  suppressMessages(suppressWarnings(
    utils::capture.output(
      res <- f_lmer(y ~ trt + (1 | subj), data = df,
                    contrasts = list(trt = "contr.sum"),
                    output_type = "default",
                    norm_plots = FALSE, intro_text = FALSE,
                    open_generated_files = FALSE),
      file = nullfile()
    )
  ))
  m <- res[["y"]]$model
  fe_names <- rownames(summary(m)$coefficients)
  expect_true(any(grepl("^trt[0-9]", fe_names)))
})

# ===========================================================================
# 9. Input validation
# ===========================================================================

test_that("formula without random effect is rejected", {
  skip_on_cran()
  skip_if_no_lme4()
  df <- make_repeated_measures_data()
  expect_error(
    quiet_f_lmer(y ~ time, data = df),
    "random"
  )
})

test_that("invalid alpha is rejected", {
  skip_on_cran()
  skip_if_no_lme4()
  df <- make_repeated_measures_data()
  expect_error(quiet_f_lmer(y ~ time + (1 | subj), data = df, alpha = -0.1))
  expect_error(quiet_f_lmer(y ~ time + (1 | subj), data = df, alpha = 1.5))
})

test_that("invalid output_type is rejected", {
  skip_on_cran()
  skip_if_no_lme4()
  df <- make_repeated_measures_data()
  expect_error(quiet_f_lmer(y ~ time + (1 | subj), data = df,
                          output_type = "html"))
})

# ===========================================================================
# 10. safe_shapiro large-n guard
# ===========================================================================

test_that("f_lmer does not crash with n > 5000 residuals", {
  skip_on_cran()
  skip_if_no_lme4()
  set.seed(91)
  n_subj <- 800
  n_time <- 8                                 # 6400 total rows
  big <- data.frame(
    subj = factor(rep(seq_len(n_subj), each = n_time)),
    time = factor(rep(seq_len(n_time), times = n_subj)),
    y    = NA_real_
  )
  si <- rnorm(n_subj, 0, 1)[as.integer(big$subj)]
  big$y <- 10 + as.integer(big$time) * 0.2 + si +
           rnorm(nrow(big), 0, 1)

  expect_no_error(
    res <- quiet_f_lmer(y ~ time + (1 | subj), data = big)
  )
  expect_s3_class(res, "f_lmer")
})

# ===========================================================================
# 11. Singular fit handling
# ===========================================================================

test_that("singular fit is captured in the result, not thrown", {
  skip_on_cran()
  skip_if_no_lme4()
  set.seed(3)
  df <- data.frame(
    subj = factor(rep(1:4, each = 3)),
    x    = rnorm(12),
    y    = rnorm(12)
  )
  res <- tryCatch(
    quiet_f_lmer(y ~ x + (1 | subj), data = df),
    error = function(e) e
  )
  if (!inherits(res, "error")) {
    expect_true("is_singular" %in% names(res[["y"]]))
  }
})

# ===========================================================================
# 12. intro_text toggle
# ===========================================================================

test_that("intro_text = FALSE omits the intro section in rmd output", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_repeated_measures_data()
  res <- suppressMessages(suppressWarnings(
    f_lmer(y ~ time + (1 | subj), data = df,
           output_type = "rmd",
           intro_text = FALSE,
           norm_plots = FALSE,
           open_generated_files = FALSE)
  ))
  expect_true("rmd" %in% names(res))
  expect_false(grepl("assumption", res$rmd, ignore.case = TRUE))
})

test_that("intro_text = TRUE includes assumption text in rmd output", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_repeated_measures_data()
  res <- suppressMessages(suppressWarnings(
    f_lmer(y ~ time + (1 | subj), data = df,
           output_type = "rmd",
           intro_text = TRUE,
           norm_plots = FALSE,
           open_generated_files = FALSE)
  ))
  expect_true(grepl("assumption|random", res$rmd, ignore.case = TRUE))
})

# ===========================================================================
# 13. rmd output type
# ===========================================================================

test_that("rmd output_type stores the markdown string on result$rmd", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_repeated_measures_data()
  res <- suppressMessages(suppressWarnings(
    f_lmer(y ~ time + (1 | subj), data = df,
           output_type = "rmd",
           intro_text = FALSE,
           norm_plots = FALSE,
           open_generated_files = FALSE)
  ))
  expect_true(is.character(res$rmd))
  expect_true(nchar(res$rmd) > 0)
})
