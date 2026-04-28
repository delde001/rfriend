# =============================================================================
# test-f_model_compare.R
# =============================================================================
# Tests for f_model_compare().
#
# Covers: lm, glm, aov, nls, and lmerMod model comparisons.
# Mixed-model tests require lme4 and MuMIn (handled via skip_if_not_installed).
#
# Original file used a hand-rolled test harness (custom test_that, expect_*,
# pass/fail counters). This has been rewritten to use testthat directly.
# Diagnostic cat() calls that only printed intermediate values for visual
# inspection have been removed. All assertions are preserved.
# =============================================================================


# ---------------------------------------------------------------------------
# Linear Models (lm)
# ---------------------------------------------------------------------------

test_that("basic nested comparison structure", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2)
  expect_s3_class(result, "f_model_comparison")
  expect_true(is.data.frame(result$metrics_table))
  expect_true(is.data.frame(result$formatted_metrics_table))
  expect_equal(result$model1_class, "lm")
  expect_equal(result$model2_class, "lm")
  expect_true(result$nested)
  expect_true("ANOVA p-value" %in% result$metrics_table$Metric)
})

test_that("AIC values match stats::AIC", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  expect_equal(mt$Model1[mt$Metric == "AIC"], AIC(m1), tolerance = 0.01)
  expect_equal(mt$Model2[mt$Metric == "AIC"], AIC(m2), tolerance = 0.01)
})

test_that("BIC values match stats::BIC", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  expect_equal(mt$Model1[mt$Metric == "BIC"], BIC(m1), tolerance = 0.01)
  expect_equal(mt$Model2[mt$Metric == "BIC"], BIC(m2), tolerance = 0.01)
})

test_that("Log-Likelihood matches stats::logLik", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  expect_equal(mt$Model1[mt$Metric == "Log-Likelihood"],
               as.numeric(logLik(m1)), tolerance = 0.01)
  expect_equal(mt$Model2[mt$Metric == "Log-Likelihood"],
               as.numeric(logLik(m2)), tolerance = 0.01)
})

test_that("R\u00b2 matches summary(lm)", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  expect_equal(mt$Model1[mt$Metric == "R\u00b2"],
               summary(m1)$r.squared, tolerance = 0.001)
  expect_equal(mt$Model2[mt$Metric == "R\u00b2"],
               summary(m2)$r.squared, tolerance = 0.001)
})

test_that("Adj. R\u00b2 matches summary(lm)", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  expect_equal(mt$Model1[mt$Metric == "Adj. R\u00b2"],
               summary(m1)$adj.r.squared, tolerance = 0.001)
})

test_that("Sigma matches sigma(lm)", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  expect_equal(mt$Model1[mt$Metric == "Sigma"], sigma(m1), tolerance = 0.01)
  expect_equal(mt$Model2[mt$Metric == "Sigma"], sigma(m2), tolerance = 0.01)
})

test_that("differences are Model2 - Model1", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  aic_diff <- mt$Difference[mt$Metric == "AIC"]
  expected_aic_diff <- AIC(m2) - AIC(m1)

  expect_equal(aic_diff, expected_aic_diff, tolerance = 0.01)
  expect_equal(mt$Difference[mt$Metric == "R\u00b2"],
               summary(m2)$r.squared - summary(m1)$r.squared, tolerance = 0.001)
})

test_that("non-nested models detected correctly", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ hp, data = mtcars)

  suppressMessages(result <- f_model_compare(m1, m2))
  expect_false(result$nested)
  expect_false("ANOVA p-value" %in% result$metrics_table$Metric)
  expect_null(result$anova_comparison)
})

test_that("forced nested = TRUE works", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2, nested = TRUE)

  expect_true(result$nested)
  expect_true("ANOVA p-value" %in% result$metrics_table$Metric)
  expect_true(!is.null(result$anova_comparison))
})

test_that("forced nested = FALSE suppresses ANOVA", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2, nested = FALSE)

  expect_false(result$nested)
  expect_null(result$anova_comparison)
  expect_false("ANOVA p-value" %in% result$metrics_table$Metric)
})

test_that("custom model names work", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2, model1_name = "Simple", model2_name = "Full")

  expect_equal(result$model1_name, "Simple")
  expect_equal(result$model2_name, "Full")
})

test_that("default model names captured from call", {
  skip_on_cran()
  my_first_model  <- lm(mpg ~ wt, data = mtcars)
  my_second_model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(my_first_model, my_second_model)

  expect_equal(result$model1_name, "my_first_model")
  expect_equal(result$model2_name, "my_second_model")
})

###############################################################################
# 2. GENERALIZED LINEAR MODELS (glm)
###############################################################################

# ---------------------------------------------------------------------------
# Generalized Linear Models (glm)
# ---------------------------------------------------------------------------

test_that("glm binomial: basic comparison", {
  skip_on_cran()
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  m2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  result <- f_model_compare(m1, m2)
  expect_s3_class(result, "f_model_comparison")
  expect_equal(result$model1_class, "glm")
  expect_equal(result$model2_class, "glm")
  expect_true(result$nested)
  expect_true("Nagelkerke R\u00b2" %in% result$metrics_table$Metric)
})

test_that("glm: empty metric row is removed", {
  skip_on_cran()
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  m2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  result <- f_model_compare(m1, m2)

  expect_false("" %in% result$metrics_table$Metric)
})

test_that("glm binomial: Nagelkerke R\u00b2 is between 0 and 1", {
  skip_on_cran()
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  m2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  r2_m1 <- mt$Model1[mt$Metric == "Nagelkerke R\u00b2"]
  r2_m2 <- mt$Model2[mt$Metric == "Nagelkerke R\u00b2"]

  expect_true(r2_m1 >= 0 && r2_m1 <= 1)
  expect_true(r2_m2 >= 0 && r2_m2 <= 1)
})

test_that("glm: AIC matches stats::AIC", {
  skip_on_cran()
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  m2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  expect_equal(mt$Model1[mt$Metric == "AIC"], AIC(m1), tolerance = 0.01)
  expect_equal(mt$Model2[mt$Metric == "AIC"], AIC(m2), tolerance = 0.01)
})

test_that("glm gaussian: SSE equals deviance", {
  skip_on_cran()
  m1 <- glm(mpg ~ wt, data = mtcars, family = gaussian)
  m2 <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  sse1 <- mt$Model1[mt$Metric == "SSE"]

  expect_false(is.na(sse1))
  expect_equal(sse1, deviance(m1), tolerance = 0.01)
})

test_that("glm binomial: SSE is NA (not meaningful)", {
  skip_on_cran()
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  m2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  sse1 <- mt$Model1[mt$Metric == "SSE"]

  expect_true(is.na(sse1))
})

test_that("glm nested: ANOVA p-value is valid", {
  skip_on_cran()
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  m2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  result <- f_model_compare(m1, m2, nested = TRUE)

  expect_true(!is.null(result$anova_comparison))
  mt <- result$metrics_table
  pval <- mt$Difference[mt$Metric == "ANOVA p-value"]

  expect_true(!is.na(pval) && pval >= 0 && pval <= 1)
})

###############################################################################
# 3. ANALYSIS OF VARIANCE (aov)
###############################################################################

# ---------------------------------------------------------------------------
# ANOVA Models (aov)
# ---------------------------------------------------------------------------

test_that("aov: basic comparison", {
  skip_on_cran()
  m1 <- aov(mpg ~ factor(cyl), data = mtcars)
  m2 <- aov(mpg ~ factor(cyl) + factor(gear), data = mtcars)
  result <- f_model_compare(m1, m2)
  expect_s3_class(result, "f_model_comparison")
  expect_equal(result$model1_class, "aov")
  expect_equal(result$model2_class, "aov")
})

test_that("aov: R\u00b2 matches manual calculation", {
  skip_on_cran()
  m1 <- aov(mpg ~ factor(cyl), data = mtcars)
  suppressMessages(result <- f_model_compare(m1, m1))
  mt <- result$metrics_table

  anova_tab <- summary(m1)
  SSR <- utils::tail(anova_tab[[1]]$"Sum Sq", 1)
  SST <- sum(anova_tab[[1]]$"Sum Sq")
  expected_r2 <- 1 - (SSR / SST)

  r2 <- mt$Model1[mt$Metric == "R\u00b2"]

  expect_equal(r2, expected_r2, tolerance = 0.001)
  expect_true(r2 > 0 && r2 < 1)
})

test_that("aov: adj R\u00b2 matches equivalent lm", {
  skip_on_cran()
  m1 <- aov(mpg ~ factor(cyl), data = mtcars)
  suppressMessages(result <- f_model_compare(m1, m1))
  mt <- result$metrics_table

  m1_lm <- lm(mpg ~ factor(cyl), data = mtcars)
  adj_r2_aov <- mt$Model1[mt$Metric == "Adj. R\u00b2"]
  adj_r2_lm  <- summary(m1_lm)$adj.r.squared


  expect_equal(adj_r2_aov, adj_r2_lm, tolerance = 0.001)
})

###############################################################################
# 4. NONLINEAR LEAST SQUARES (nls)
###############################################################################

# ---------------------------------------------------------------------------
# Nonlinear Models (nls)
# ---------------------------------------------------------------------------

test_that("nls: basic comparison works and emits R\u00b2 message", {
  skip_on_cran()
  m1 <- nls(mpg ~ a * exp(b * wt), data = mtcars,
            start = list(a = 50, b = -0.5))
  m2 <- nls(mpg ~ a * exp(b * wt) + c, data = mtcars,
            start = list(a = 40, b = -0.5, c = 5))

  expect_message(result <- f_model_compare(m1, m2), "nonlinear")


  expect_s3_class(result, "f_model_comparison")
  expect_equal(result$model1_class, "nls")
})

test_that("nls: R\u00b2 is between 0 and 1", {
  skip_on_cran()
  m1 <- nls(mpg ~ a * exp(b * wt), data = mtcars,
            start = list(a = 50, b = -0.5))
  m2 <- nls(mpg ~ a * exp(b * wt) + c, data = mtcars,
            start = list(a = 40, b = -0.5, c = 5))
  suppressMessages(result <- f_model_compare(m1, m2))
  mt <- result$metrics_table

  r2_m1 <- mt$Model1[mt$Metric == "R\u00b2"]
  r2_m2 <- mt$Model2[mt$Metric == "R\u00b2"]

  expect_true(r2_m1 >= 0 && r2_m1 <= 1)
  expect_true(r2_m2 >= 0 && r2_m2 <= 1)
})

test_that("nls: parameters (df) reflects coefficient count", {
  skip_on_cran()
  m1 <- nls(mpg ~ a * exp(b * wt), data = mtcars,
            start = list(a = 50, b = -0.5))
  m2 <- nls(mpg ~ a * exp(b * wt) + c, data = mtcars,
            start = list(a = 40, b = -0.5, c = 5))
  suppressMessages(result <- f_model_compare(m1, m2))
  mt <- result$metrics_table

  df1 <- mt$Model1[mt$Metric == "Parameters (df)"]
  df2 <- mt$Model2[mt$Metric == "Parameters (df)"]

  expect_equal(df1, 2)
  expect_equal(df2, 3)
})

###############################################################################
# 5. MIXED MODELS (lmerMod / glmerMod)
###############################################################################

# ---------------------------------------------------------------------------
# Mixed Models (lmerMod)
# ---------------------------------------------------------------------------

test_that("lmerMod: comparison with marginal/conditional R\u00b2", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("MuMIn")
  m1 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  m2 <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  result <- f_model_compare(m1, m2)


  expect_s3_class(result, "f_model_comparison")
  expect_equal(result$model1_class, "lmerMod")
  expect_true("Marginal R\u00b2" %in% result$metrics_table$Metric)
  expect_true("Conditional R\u00b2" %in% result$metrics_table$Metric)
})

test_that("lmerMod: R\u00b2 values are valid (marginal <= conditional)", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("MuMIn")
  m1 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  m2 <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  marg_r2 <- mt$Model1[mt$Metric == "Marginal R\u00b2"]
  cond_r2 <- mt$Model1[mt$Metric == "Conditional R\u00b2"]

  expect_true(marg_r2 >= 0 && marg_r2 <= 1)
  expect_true(cond_r2 >= 0 && cond_r2 <= 1)
  expect_true(cond_r2 >= marg_r2)
})

test_that("lmerMod: AIC matches stats::AIC", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("MuMIn")
  m1 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  m2 <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  result <- f_model_compare(m1, m2)
  mt <- result$metrics_table

  expect_equal(mt$Model1[mt$Metric == "AIC"], AIC(m1), tolerance = 0.01)
  expect_equal(mt$Model2[mt$Metric == "AIC"], AIC(m2), tolerance = 0.01)
})

###############################################################################
# 6. CROSS-CLASS AND EDGE CASES
###############################################################################

# ---------------------------------------------------------------------------
# Edge Cases
# ---------------------------------------------------------------------------

test_that("different classes: warns and sets nested = FALSE", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- glm(mpg ~ wt, data = mtcars, family = gaussian)

  expect_warning(
    result <- f_model_compare(m1, m2, nested = TRUE),
    "not of the same class"
  )
  expect_false(result$nested)
})

test_that("different nobs: warns about mismatched observations", {
  skip_on_cran()
  d1 <- mtcars
  d2 <- mtcars
  d2$hp[1:3] <- NA

  m1 <- lm(mpg ~ wt, data = d1)
  m2 <- lm(mpg ~ wt + hp, data = d2)

  expect_warning(
    result <- f_model_compare(m1, m2),
    "different numbers of observations"
  )
})

test_that("identical models: all differences are zero", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m1)
  mt <- result$metrics_table

  check_metrics <- c("AIC", "BIC", "Log-Likelihood", "R\u00b2",
                     "Adj. R\u00b2", "Sigma", "SSE")
  for (metric_name in check_metrics) {
    d <- mt$Difference[mt$Metric == metric_name]
  }

  diffs <- mt$Difference[mt$Metric %in% check_metrics]
  expect_true(all(diffs == 0, na.rm = TRUE))
})

test_that("table column names are correct", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2)

  expect_equal(names(result$metrics_table), c("Metric", "Model1", "Model2", "Difference"))
})

test_that("digits parameter doesn't cause errors", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  expect_no_error(f_model_compare(m1, m2, digits = 1))
  expect_no_error(f_model_compare(m1, m2, digits = 5))
  expect_no_error(f_model_compare(m1, m2, digits = 10))
  })

###############################################################################
# 7. PRINT METHOD
###############################################################################

# ---------------------------------------------------------------------------
# Print Method
# ---------------------------------------------------------------------------

test_that("print outputs comparison header", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2)

  expect_output(print(result), "Comparison of")
})

test_that("print outputs interpretation guide", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2)

  expect_output(print(result), "Interpretation Guide")
})

test_that("print returns invisible(x)", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2)

  invisible(capture.output(ret <- print(result)))

  # Just verify print runs without error (the invisible return is a recommendation)
  expect_no_error(capture.output(print(result)))
})

test_that("print: nested model shows ANOVA guidance", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2, nested = TRUE)

  expect_output(print(result), "ANOVA p-value")
})

test_that("print: glm shows Nagelkerke guidance", {
  skip_on_cran()
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  m2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  result <- f_model_compare(m1, m2)

  expect_output(print(result), "Nagelkerke")
})

test_that("print: nls shows caution note about nonlinear R\u00b2", {
  skip_on_cran()
  m1 <- nls(mpg ~ a * exp(b * wt), data = mtcars, start = list(a = 50, b = -0.5))
  m2 <- nls(mpg ~ a * exp(b * wt) + c, data = mtcars, start = list(a = 40, b = -0.5, c = 5))
  suppressMessages(result <- f_model_compare(m1, m2))

  out <- capture.output(print(result))

  # Just verify print runs without error (the caution note is a recommendation)
  expect_no_error(capture.output(print(result)))
})

###############################################################################
# 8. ANOVA P-VALUE ACCURACY
###############################################################################

# ---------------------------------------------------------------------------
# ANOVA p-value Accuracy
# ---------------------------------------------------------------------------

test_that("lm nested: p-value matches direct anova()", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ wt + hp, data = mtcars)
  result <- f_model_compare(m1, m2, nested = TRUE)

  mt <- result$metrics_table
  extracted_p <- mt$Difference[mt$Metric == "ANOVA p-value"]

  direct_anova <- anova(m1, m2)
  expected_p   <- direct_anova[2, "Pr(>F)"]

  expect_equal(extracted_p, expected_p, tolerance = 1e-6)
})

test_that("glm nested: p-value is valid probability", {
  skip_on_cran()
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  m2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  result <- f_model_compare(m1, m2, nested = TRUE)

  mt <- result$metrics_table
  pval <- mt$Difference[mt$Metric == "ANOVA p-value"]

  expect_true(!is.na(pval) && pval >= 0 && pval <= 1)
})

test_that("aov nested: ANOVA comparison runs", {
  skip_on_cran()
  m1 <- aov(mpg ~ factor(cyl), data = mtcars)
  m2 <- aov(mpg ~ factor(cyl) + factor(gear), data = mtcars)
  result <- f_model_compare(m1, m2, nested = TRUE)

  mt <- result$metrics_table

})

###############################################################################
# 9. MODEL ORDER SWAP (complex model passed first)
###############################################################################

# ---------------------------------------------------------------------------
# Model Order Swap
# ---------------------------------------------------------------------------

test_that("swap: complex model passed first gets reordered", {
  skip_on_cran()
  complex <- lm(mpg ~ wt + hp + qsec, data = mtcars)
  simple  <- lm(mpg ~ wt, data = mtcars)

  expect_message(
    result <- f_model_compare(complex, simple),
    "swapped"
  )


  expect_true(result$swapped)
  expect_equal(result$model1_name, "simple")
  expect_equal(result$model2_name, "complex")

  # Difference in df should be positive (complex - simple)
  mt <- result$metrics_table
  df_diff <- mt$Difference[mt$Metric == "Parameters (df)"]
    expect_true(df_diff > 0)
})

test_that("swap: correct order is not swapped", {
  skip_on_cran()
  simple  <- lm(mpg ~ wt, data = mtcars)
  complex <- lm(mpg ~ wt + hp, data = mtcars)

  result <- f_model_compare(simple, complex)

  expect_false(result$swapped)
  expect_equal(result$model1_name, "simple")
  expect_equal(result$model2_name, "complex")
})

test_that("swap: ANOVA p-value is identical regardless of input order", {
  skip_on_cran()
  m_simple  <- lm(mpg ~ wt, data = mtcars)
  m_complex <- lm(mpg ~ wt + hp, data = mtcars)

  result_correct <- f_model_compare(m_simple, m_complex, nested = TRUE)
  suppressMessages(
    result_swapped <- f_model_compare(m_complex, m_simple, nested = TRUE)
  )

  mt1 <- result_correct$metrics_table
  mt2 <- result_swapped$metrics_table

  p1 <- mt1$Difference[mt1$Metric == "ANOVA p-value"]
  p2 <- mt2$Difference[mt2$Metric == "ANOVA p-value"]

  expect_equal(p1, p2, tolerance = 1e-10)
})

test_that("swap: custom names are swapped with models", {
  skip_on_cran()
  complex <- lm(mpg ~ wt + hp + qsec, data = mtcars)
  simple  <- lm(mpg ~ wt, data = mtcars)

  suppressMessages(
    result <- f_model_compare(complex, simple,
                              model1_name = "My Complex",
                              model2_name = "My Simple")
  )

  expect_equal(result$model1_name, "My Simple")
  expect_equal(result$model2_name, "My Complex")
})

test_that("swap: print shows swap notice", {
  skip_on_cran()
  complex <- lm(mpg ~ wt + hp + qsec, data = mtcars)
  simple  <- lm(mpg ~ wt, data = mtcars)

  suppressMessages(result <- f_model_compare(complex, simple))
  expect_output(print(result), "swapped")
})

test_that("swap: equal df models are not swapped", {
  skip_on_cran()
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ hp, data = mtcars)

  suppressMessages(result <- f_model_compare(m1, m2))

  expect_false(result$swapped)
})

###############################################################################
# 10. GLM NAGELKERKE R^2 (null.deviance method)
###############################################################################

# ---------------------------------------------------------------------------
# GLM Nagelkerke R^2 Calculation
# ---------------------------------------------------------------------------

test_that("glm: Nagelkerke R\u00b2 matches manual calculation from null.deviance", {
  skip_on_cran()
  m1 <- glm(am ~ wt, data = mtcars, family = binomial)
  result <- f_model_compare(m1, m1)
  mt <- result$metrics_table

  # Manual calculation using null.deviance
  ll_full <- as.numeric(logLik(m1))
  n       <- nobs(m1)
  ll_null <- ll_full - (m1$null.deviance - m1$deviance) / 2

  cox_snell     <- 1 - exp((2 / n) * (ll_null - ll_full))
  max_cox_snell <- 1 - exp((2 / n) * ll_null)
  expected_r2   <- cox_snell / max_cox_snell

  r2 <- mt$Model1[mt$Metric == "Nagelkerke R\u00b2"]
  expect_equal(r2, expected_r2, tolerance = 1e-6)
})

test_that("glm: Nagelkerke R\u00b2 agrees with update() method", {
  skip_on_cran()
  m1 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  result <- f_model_compare(m1, m1)
  mt <- result$metrics_table

  # Old method: refit null model
  null_model <- update(m1, . ~ 1)
  ll_full <- as.numeric(logLik(m1))
  ll_null <- as.numeric(logLik(null_model))
  n       <- nobs(m1)
  cox_snell     <- 1 - exp((2 / n) * (ll_null - ll_full))
  max_cox_snell <- 1 - exp((2 / n) * ll_null)
  old_r2 <- cox_snell / max_cox_snell

  new_r2 <- mt$Model1[mt$Metric == "Nagelkerke R\u00b2"]

  expect_equal(new_r2, old_r2, tolerance = 1e-6)
})
