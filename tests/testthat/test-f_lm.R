# =============================================================================
# test-f_lm.R
# Comprehensive testthat tests for f_lm()
#
# Run with: testthat::test_file("tests/testthat/test-f_lm.R")
#           or devtools::test() from the package root
#
# API note
# --------
# Results are ALWAYS nested under the bare response name, consistent with
# f_aov / f_lmer / f_t_test. For a single response use res[["y"]]; for
# multiple responses res[["y1"]], res[["y2"]], etc. When output_type = "rmd"
# the raw markdown string is stored in the top-level element res[["rmd"]].
#
# f_lm() is the regression sibling of f_aov(): it shares the same assumption
# checks, the same optional Box-Cox / bestNormalize transformation workflow,
# and the same object layout, but it keeps numeric predictors numeric (so they
# are continuous regression terms) and adds regression-specific output: a
# coefficient table, a coefficient forest plot, a Type II ANOVA table, the
# overall R^2 / adjusted R^2 / model F-test, and regression plots.
#
# Shared fixtures and wrappers
# ----------------------------
#   * make_normal_data(), make_skewed_data()  -> helper-data.R
#   * quiet_f_lm(), run_quiet()               -> helper-quiet.R
# make_reg_data() below is file-local on purpose: no shared fixture carries a
# numeric predictor, and a continuous regression term is exactly the feature
# that distinguishes f_lm() from f_aov(). The graphics device is opened once
# in setup.R, so plot() tests do not manage their own device.
#
# CRAN strategy
# -------------
# The transformation paths (Box-Cox, and especially bestNormalize) are the
# slow / dependency-sensitive parts and are guarded with skip_on_cran().
# Structure, validation, plain-fit, ANOVA, post-hoc, NA, interaction, rmd and
# pass-through tests are fast and run everywhere.
#
# Test organisation (each section is independent):
#   1.  Return object structure
#   2.  Continuous predictor (true regression, no factor coercion)
#   3.  Categorical predictor triggers post hoc + CLD
#   4.  Multiple response variables
#   5.  Transformation options (boxcox / bestnormalize / FALSE)
#   6.  force_transformation
#   7.  alpha and adjust parameters
#   8.  Missing values (NAs)
#   9.  Interaction / multiple-predictor models
#  10.  output_type = "rmd"
#  11.  Coefficient table and model-fit statistics correctness
#  12.  Input validation / error handling
#  13.  intro_text toggle
#  14.  Stored plots (coef_forest_plot, effect plots, obs_fitted_plot)
#  15.  subset / weights / na.action pass-through (direct f_lm calls)
#  16.  Statistical correctness vs base stats::lm()
#  17.  print and plot methods run without error
# =============================================================================

# ---------------------------------------------------------------------------
# File-local fixture: continuous-predictor regression data.
# y depends linearly on two numeric predictors with normal noise. Used where a
# genuine numeric regression term is wanted (the key f_lm vs f_aov difference).
# ---------------------------------------------------------------------------
make_reg_data <- function(seed = 101, n = 60) {
  set.seed(seed)
  x  <- runif(n, 0, 10)
  x2 <- runif(n, -5, 5)
  data.frame(
    x  = x,
    x2 = x2,
    y  = 3 + 1.5 * x - 0.7 * x2 + rnorm(n, 0, 1)
  )
}

# =============================================================================
# 1. Return object structure
# =============================================================================
df  <- make_reg_data()
out <- quiet_f_lm(y ~ x, data = df, transformation = FALSE)

test_that("f_lm structure: returns an object of class 'f_lm'", {
  expect_s3_class(out, "f_lm")
})

test_that("f_lm structure: is a named list with one entry per response", {
  expect_true(is.list(out))
  expect_true("y" %in% names(out))
})

test_that("f_lm structure: response entry contains the mandatory keys", {
  keys <- names(out[["y"]])
  mandatory <- c("lm_fit", "lm_call", "summary", "coefficients", "anova",
                 "r_squared", "adj_r_squared", "f_statistic", "model_p_value",
                 "breusch_pagan_test", "shapiro_test_residuals",
                 "adt_test_residuals", "alpha", "Response_Transformed",
                 "lm_fit_active")
  for (k in mandatory) {
    expect_true(k %in% keys, label = paste("mandatory key present:", k))
  }
})

test_that("f_lm structure: lm_fit is an lm object", {
  expect_s3_class(out[["y"]][["lm_fit"]], "lm")
})

test_that("f_lm structure: lm_call is a character string naming the formula", {
  expect_type(out[["y"]][["lm_call"]], "character")
  expect_match(out[["y"]][["lm_call"]], "y")
  expect_match(out[["y"]][["lm_call"]], "x")
})

test_that("f_lm structure: shapiro_test_residuals is htest with a valid p", {
  sw <- out[["y"]][["shapiro_test_residuals"]]
  expect_true(!is.null(sw$p.value))
  expect_true(sw$p.value >= 0 && sw$p.value <= 1)
})

test_that("f_lm structure: breusch_pagan_test reports statistic, p and method", {
  bp <- out[["y"]][["breusch_pagan_test"]]
  expect_true(all(c("statistic", "p.value", "method") %in% names(bp)))
})

test_that("f_lm structure: alpha stored matches the argument passed", {
  expect_equal(out[["y"]][["alpha"]], 0.05)
  out2 <- quiet_f_lm(y ~ x, data = df, alpha = 0.01, transformation = FALSE)
  expect_equal(out2[["y"]][["alpha"]], 0.01)
})

test_that("f_lm structure: coefficients slot is a data frame", {
  expect_s3_class(out[["y"]][["coefficients"]], "data.frame")
})

test_that("f_lm structure: anova slot is present and tabular", {
  an <- out[["y"]][["anova"]]
  expect_true(is.data.frame(an) || inherits(an, "anova"))
})

# =============================================================================
# 2. Continuous predictor: numeric predictor stays numeric (no factor coercion)
# =============================================================================
df  <- make_reg_data()
out <- quiet_f_lm(y ~ x + x2, data = df, transformation = FALSE)

test_that("f_lm continuous predictor: model term for x is a single slope, not factor levels", {
  cf <- out[["y"]][["coefficients"]]
  # A factor would create x<level> rows; a numeric slope is a single 'x' row.
  expect_true("x" %in% rownames(cf) || "x" %in% cf[[1]])
})

test_that("f_lm continuous predictor: model frame keeps x numeric", {
  mf <- out[["y"]][["lm_fit"]][["model"]]
  expect_true(is.numeric(mf[["x"]]))
})

test_that("f_lm continuous predictor: no post hoc CLD table for a purely numeric model", {
  # emmeans CLD is only built for categorical predictors.
  expect_null(out[["y"]][["post_hoc_summary_table"]])
})

# =============================================================================
# 3. Categorical predictor triggers post hoc + compact letter display
# =============================================================================
# make_normal_data(): 3 well-separated groups A/B/C in column 'grp'.
dfg  <- make_normal_data()
outg <- quiet_f_lm(y ~ grp, data = dfg, transformation = FALSE)

test_that("f_lm categorical predictor: post_hoc_summary_table is a data frame", {
  expect_s3_class(outg[["y"]][["post_hoc_summary_table"]], "data.frame")
})

test_that("f_lm categorical predictor: post hoc table has one row per group level", {
  expect_equal(nrow(outg[["y"]][["post_hoc_summary_table"]]), 3L)
})

test_that("f_lm categorical predictor: post hoc table has the CLD 'Letter' column", {
  expect_true("Letter" %in% names(outg[["y"]][["post_hoc_summary_table"]]))
})

test_that("f_lm categorical predictor: well-separated groups get more than one CLD letter", {
  ph <- outg[["y"]][["post_hoc_summary_table"]]
  expect_true(length(unique(ph[["Letter"]])) > 1)
})

test_that("f_lm categorical predictor: character predictor is coerced to factor", {
  dfc <- dfg
  dfc$grp <- as.character(dfc$grp)
  outc <- quiet_f_lm(y ~ grp, data = dfc, transformation = FALSE)
  expect_s3_class(outc[["y"]][["post_hoc_summary_table"]], "data.frame")
})

# =============================================================================
# 4. Multiple response variables
# =============================================================================
df  <- make_reg_data()
df$y2 <- 2 - 0.5 * df$x + rnorm(nrow(df), 0, 1)
out <- quiet_f_lm(y + y2 ~ x, data = df, transformation = FALSE)

test_that("f_lm multiple responses: both response names are present", {
  expect_true("y"  %in% names(out))
  expect_true("y2" %in% names(out))
})

test_that("f_lm multiple responses: each response has its own lm_fit", {
  expect_s3_class(out[["y"]][["lm_fit"]],  "lm")
  expect_s3_class(out[["y2"]][["lm_fit"]], "lm")
})

test_that("f_lm multiple responses: lm_call for y2 references y2 not y", {
  expect_match(out[["y2"]][["lm_call"]], "y2")
  expect_false(grepl("^y ~", out[["y2"]][["lm_call"]]))
})

test_that("f_lm multiple responses: three responses all stored independently", {
  df$y3 <- rnorm(nrow(df), 5, 1)
  out3  <- quiet_f_lm(y + y2 + y3 ~ x, data = df, transformation = FALSE)
  expect_true(all(c("y", "y2", "y3") %in% names(out3)))
})

# =============================================================================
# 5. Transformation options
# =============================================================================
# make_skewed_data(): right-skewed (rexp) response in column 'y', 3 groups.
df_skew <- make_skewed_data()

test_that("f_lm transformation: FALSE never transforms, no transformed keys", {
  out <- quiet_f_lm(y ~ grp, data = df_skew, transformation = FALSE)
  expect_false(out[["y"]][["Response_Transformed"]])
  expect_false("transformed_lm_fit" %in% names(out[["y"]]))
})

test_that("f_lm transformation: TRUE may transform skewed residuals and store keys", {
  skip_on_cran()
  out <- quiet_f_lm(y ~ grp, data = df_skew, transformation = TRUE)
  if (isTRUE(out[["y"]][["Response_Transformed"]])) {
    expect_true("boxcox" %in% names(out[["y"]]) ||
                "bestNormalize" %in% names(out[["y"]]))
    expect_true("transformed_shapiro_test" %in% names(out[["y"]]))
  }
})

test_that("f_lm transformation: 'boxcox' stores boxcox key when triggered", {
  skip_on_cran()
  out <- quiet_f_lm(y ~ grp, data = df_skew, transformation = "boxcox")
  if (isTRUE(out[["y"]][["Response_Transformed"]])) {
    expect_true("boxcox" %in% names(out[["y"]]))
  }
})

test_that("f_lm transformation: 'bestnormalize' stores bestNormalize key when triggered", {
  skip_on_cran()
  set.seed(1)
  out <- quiet_f_lm(y ~ grp, data = df_skew, transformation = "bestnormalize")
  if (isTRUE(out[["y"]][["Response_Transformed"]])) {
    expect_true("bestNormalize" %in% names(out[["y"]]))
  }
})

test_that("f_lm transformation: invalid transformation string errors", {
  expect_error(
    quiet_f_lm(y ~ grp, data = df_skew, transformation = "logtransform"),
    regexp = "Invalid transformation"
  )
})

# =============================================================================
# 6. force_transformation
# =============================================================================
test_that("f_lm force_transformation: forces transform on a normal response", {
  skip_on_cran()
  df <- make_reg_data()
  out <- quiet_f_lm(y ~ x, data = df,
                    transformation       = TRUE,
                    force_transformation = "y")
  expect_true(out[["y"]][["Response_Transformed"]])
})

# =============================================================================
# 7. alpha and adjust parameters
# =============================================================================
dfg <- make_normal_data()

test_that("f_lm alpha: alpha = 0.01 stored correctly", {
  out <- quiet_f_lm(y ~ grp, data = dfg, alpha = 0.01, transformation = FALSE)
  expect_equal(out[["y"]][["alpha"]], 0.01)
})

test_that("f_lm adjust: adjust value is stored on the response entry", {
  out <- quiet_f_lm(y ~ grp, data = dfg, adjust = "bonferroni",
                    transformation = FALSE)
  expect_equal(out[["y"]][["adjust"]], "bonferroni")
})

test_that("f_lm adjust: different adjust methods both run", {
  expect_no_error(quiet_f_lm(y ~ grp, data = dfg, adjust = "tukey",
                             transformation = FALSE))
  expect_no_error(quiet_f_lm(y ~ grp, data = dfg, adjust = "fdr",
                             transformation = FALSE))
})

# =============================================================================
# 8. Missing values (NAs)
# =============================================================================
df <- make_reg_data()
df$y[c(3, 7, 22)] <- NA

test_that("f_lm NA handling: runs successfully with NAs present", {
  expect_no_error(quiet_f_lm(y ~ x, data = df, transformation = FALSE))
})

test_that("f_lm NA handling: model is fit on complete cases only", {
  out     <- quiet_f_lm(y ~ x, data = df, transformation = FALSE)
  n_resid <- length(residuals(out[["y"]][["lm_fit"]]))
  expect_equal(n_resid, sum(!is.na(df$y)))
})

# =============================================================================
# 9. Interaction / multiple-predictor models
# =============================================================================
test_that("f_lm interaction: numeric x categorical model runs", {
  set.seed(55)
  dfi <- data.frame(
    x   = runif(60, 0, 10),
    grp = factor(rep(c("A", "B"), each = 30))
  )
  dfi$y <- 2 + 1.5 * dfi$x +
    ifelse(dfi$grp == "B", 1.2 * dfi$x, 0) + rnorm(60, 0, 1)
  expect_no_error(quiet_f_lm(y ~ x * grp, data = dfi, transformation = FALSE))
})

test_that("f_lm multiple predictors: two numeric predictors give an ANOVA table", {
  df  <- make_reg_data()
  out <- quiet_f_lm(y ~ x + x2, data = df, transformation = FALSE)
  an  <- out[["y"]][["anova"]]
  expect_true(is.data.frame(an) || inherits(an, "anova"))
})

# =============================================================================
# 10. output_type = "rmd"
# =============================================================================
# rmd does not print to console, so f_lm() is called directly with the
# non-default output_type rather than through quiet_f_lm().
df <- make_reg_data()

test_that("f_lm rmd: stores a character markdown string under res$rmd", {
  skip_on_cran()
  out <- f_lm(y ~ x, data = df, output_type = "rmd",
              transformation = FALSE, open_generated_files = FALSE)
  expect_true("rmd" %in% names(out))
  expect_type(out[["rmd"]], "character")
  expect_gt(nchar(out[["rmd"]]), 100L)
})

test_that("f_lm rmd: markdown contains expected section markers", {
  skip_on_cran()
  out <- f_lm(y ~ x, data = df, output_type = "rmd",
              transformation = FALSE, open_generated_files = FALSE)
  expect_match(out[["rmd"]], "Analysis of:")
})

test_that("f_lm rmd: multiple-response NOTE appears for k > 1 only", {
  skip_on_cran()
  df$y2 <- df$y + rnorm(nrow(df))
  out_multi <- f_lm(y + y2 ~ x, data = df, output_type = "rmd",
                    transformation = FALSE, open_generated_files = FALSE)
  out_one   <- f_lm(y ~ x, data = df, output_type = "rmd",
                    transformation = FALSE, open_generated_files = FALSE)
  expect_match(out_multi[["rmd"]], "Multiple Testing")
  expect_false(grepl("Multiple Testing", out_one[["rmd"]]))
})

# =============================================================================
# 11. Coefficient table and model-fit statistics correctness
# =============================================================================
df  <- make_reg_data()
out <- quiet_f_lm(y ~ x + x2, data = df, transformation = FALSE)

test_that("f_lm model fit: r_squared is between 0 and 1", {
  r2 <- out[["y"]][["r_squared"]]
  expect_true(r2 >= 0 && r2 <= 1)
})

test_that("f_lm model fit: adjusted r_squared does not exceed r_squared", {
  expect_lte(out[["y"]][["adj_r_squared"]], out[["y"]][["r_squared"]])
})

test_that("f_lm model fit: model_p_value is a valid probability", {
  p <- out[["y"]][["model_p_value"]]
  expect_true(p >= 0 && p <= 1)
})

test_that("f_lm model fit: strong linear signal yields a high r_squared", {
  # make_reg_data has a strong linear relationship; R^2 should be large.
  expect_gt(out[["y"]][["r_squared"]], 0.8)
})

# =============================================================================
# 12. Input validation / error handling
# =============================================================================
df <- make_reg_data()

test_that("f_lm validation: errors when response is not in the data", {
  expect_error(quiet_f_lm(z ~ x, data = df), regexp = "not found in the data")
})

test_that("f_lm validation: errors when predictor is not in the data", {
  expect_error(quiet_f_lm(y ~ w, data = df), regexp = "not found in the data")
})

test_that("f_lm validation: errors when response is not numeric", {
  df$y_chr <- as.character(df$y)
  expect_error(quiet_f_lm(y_chr ~ x, data = df), regexp = "numeric")
})

test_that("f_lm validation: errors for invalid output_type", {
  expect_error(quiet_f_lm(y ~ x, data = df, output_type = "csv"),
               regexp = "output_type|output format")
})

test_that("f_lm validation: errors for alpha outside (0, 1)", {
  expect_error(quiet_f_lm(y ~ x, data = df, alpha = 1.5), regexp = "alpha")
  expect_error(quiet_f_lm(y ~ x, data = df, alpha = 0),   regexp = "alpha")
})

# =============================================================================
# 13. intro_text toggle
# =============================================================================
# intro_text governs an rmd section, so these call f_lm() directly with rmd.
df <- make_reg_data()

test_that("f_lm intro_text: TRUE includes the regression-assumptions section", {
  out <- f_lm(y ~ x, data = df, output_type = "rmd",
              intro_text = TRUE, transformation = FALSE,
              open_generated_files = FALSE)
  expect_match(out[["rmd"]], "Assumptions of Linear Regression")
})

test_that("f_lm intro_text: FALSE omits the regression-assumptions section", {
  out <- f_lm(y ~ x, data = df, output_type = "rmd",
              intro_text = FALSE, transformation = FALSE,
              open_generated_files = FALSE)
  expect_false(grepl("Assumptions of Linear Regression", out[["rmd"]]))
})

# =============================================================================
# 14. Stored plots
# =============================================================================
# effect_plots = TRUE is needed to populate the plot slots, so these tests
# call f_lm() directly (quiet_f_lm hardcodes effect_plots = FALSE). The
# suite-wide graphics device from setup.R absorbs any drawing. run_quiet()
# suppresses the markdown / messages while returning the object.
test_that("f_lm plots: coef_forest_plot is a ggplot when there is > 1 coefficient", {
  skip_on_cran()
  df  <- make_reg_data()
  out <- run_quiet(
    f_lm(y ~ x + x2, data = df, effect_plots = TRUE,
         transformation = FALSE, open_generated_files = FALSE)
  )
  if (!is.null(out[["y"]][["coef_forest_plot"]])) {
    expect_s3_class(out[["y"]][["coef_forest_plot"]], "ggplot")
  }
})

test_that("f_lm plots: obs_fitted_plot is stored as a ggplot", {
  skip_on_cran()
  df  <- make_reg_data()
  out <- run_quiet(
    f_lm(y ~ x, data = df, effect_plots = TRUE,
         transformation = FALSE, open_generated_files = FALSE)
  )
  if (!is.null(out[["y"]][["obs_fitted_plot"]])) {
    expect_s3_class(out[["y"]][["obs_fitted_plot"]], "ggplot")
  }
})

test_that("f_lm plots: an effect_plot_<predictor> is stored for a predictor", {
  skip_on_cran()
  df  <- make_reg_data()
  out <- run_quiet(
    f_lm(y ~ x, data = df, effect_plots = TRUE,
         transformation = FALSE, open_generated_files = FALSE)
  )
  eff_keys <- grep("^effect_plot_", names(out[["y"]]), value = TRUE)
  if (length(eff_keys) > 0) {
    expect_s3_class(out[["y"]][[eff_keys[1]]], "ggplot")
  }
})

# =============================================================================
# 15. subset / weights / na.action pass-through (DIRECT f_lm calls)
# =============================================================================
# These args must be passed to f_lm directly, not through a wrapper that
# forwards `...`: a wrapper breaks match.call()'s expression capture and turns
# the subset expression into a `..1` dots-index symbol. run_quiet() captures
# the call unevaluated, so the subset expression survives intact.

test_that("f_lm subset: drops rows before the model is fit", {
  skip_on_cran()
  df <- make_normal_data()                 # 45 rows, 3 groups of 15
  res <- run_quiet(
    f_lm(y ~ grp, data = df, subset = grp != "A",
         transformation = FALSE, open_generated_files = FALSE)
  )
  fit <- res[["y"]][["lm_fit"]]
  expect_s3_class(fit, "lm")
  expect_equal(nobs(fit), 30)
  expect_equal(length(unique(fit$model$grp)), 2L)
})

test_that("f_lm na.action: na.omit drops NA rows", {
  skip_on_cran()
  df <- make_reg_data()
  df$y[c(1, 5, 10)] <- NA
  res <- run_quiet(
    f_lm(y ~ x, data = df, na.action = na.omit,
         transformation = FALSE, open_generated_files = FALSE)
  )
  expect_equal(nobs(res[["y"]][["lm_fit"]]), nrow(df) - 3)
})

test_that("f_lm weights: weighted fit differs from unweighted", {
  skip_on_cran()
  df  <- make_reg_data()
  wts <- runif(nrow(df), 0.5, 2)
  res_w <- run_quiet(
    f_lm(y ~ x, data = df, weights = wts,
         transformation = FALSE, open_generated_files = FALSE)
  )
  res_u <- run_quiet(
    f_lm(y ~ x, data = df,
         transformation = FALSE, open_generated_files = FALSE)
  )
  cw <- coef(res_w[["y"]][["lm_fit"]])
  cu <- coef(res_u[["y"]][["lm_fit"]])
  expect_false(isTRUE(all.equal(cw, cu)))
})

# =============================================================================
# 16. Statistical correctness vs base stats::lm()
# =============================================================================
test_that("f_lm matches base lm() coefficients and R^2", {
  df       <- make_reg_data()
  base_fit <- stats::lm(y ~ x + x2, data = df)
  pkg      <- quiet_f_lm(y ~ x + x2, data = df, transformation = FALSE)
  pkg_fit  <- pkg[["y"]][["lm_fit"]]

  expect_equal(unname(coef(pkg_fit)), unname(coef(base_fit)), tolerance = 1e-8)
  expect_equal(pkg[["y"]][["r_squared"]],
               summary(base_fit)$r.squared, tolerance = 1e-8)
})

# =============================================================================
# 17. print and plot methods
# =============================================================================
test_that("f_lm print method runs without error", {
  skip_on_cran()
  df  <- make_reg_data()
  out <- quiet_f_lm(y ~ x, data = df, transformation = FALSE)
  expect_no_error(
    suppressWarnings(utils::capture.output(print(out)))
  )
})

test_that("f_lm plot method runs without error", {
  skip_on_cran()
  # The suite-wide PDF device from setup.R receives the plots.
  df  <- make_normal_data()
  out <- run_quiet(
    f_lm(y ~ grp, data = df, effect_plots = TRUE,
         transformation = FALSE, open_generated_files = FALSE)
  )
  expect_no_error(suppressWarnings(plot(out)))
})
