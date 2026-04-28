# test-f_glm.R
# Comprehensive stress tests for f_glm()
# Run with: testthat::test_file("test-f_glm.R")
# =============================================================================
# SHARED TEST DATA
# =============================================================================

# mtcars with categorical predictors and binary responses
mtcars_cat <- local({
  d        <- mtcars
  d$cyl    <- as.factor(d$cyl)
  d$gear   <- as.factor(d$gear)
  d$vs     <- as.integer(d$vs)
  d$am     <- as.integer(d$am)
  d
})

# Known non-significant dataset: perfect 50/50 split in every group
d_null <- data.frame(
  y   = c(rep(0L, 15), rep(1L, 15), rep(0L, 15), rep(1L, 15)),
  grp = factor(c(rep("A", 30), rep("B", 30)))
)

# Helper: call f_glm silently (use directly when testing warnings/messages)
run_glm <- function(...) {
  suppressMessages(suppressWarnings(f_glm(...)))
}

# =============================================================================
# 1. INPUT VALIDATION
# =============================================================================

test_that("alpha validation rejects values outside (0, 1)", {
  skip_on_cran()
  expect_error(f_glm(mpg ~ cyl, data = mtcars_cat, alpha = 0),
               regexp = "alpha")
  expect_error(f_glm(mpg ~ cyl, data = mtcars_cat, alpha = 1),
               regexp = "alpha")
  expect_error(f_glm(mpg ~ cyl, data = mtcars_cat, alpha = -0.1),
               regexp = "alpha")
  expect_error(f_glm(mpg ~ cyl, data = mtcars_cat, alpha = 1.1),
               regexp = "alpha")
  expect_error(f_glm(mpg ~ cyl, data = mtcars_cat, alpha = "0.05"),
               regexp = "alpha")
})

test_that("match.arg rejects invalid string arguments", {
  skip_on_cran()
  expect_error(
    f_glm(mpg ~ cyl, data = mtcars_cat, output_type = "INVALID"),
    regexp = "should be one of"
  )
  expect_error(
    f_glm(mpg ~ cyl, data = mtcars_cat, adjust = "INVALID"),
    regexp = "should be one of"
  )
  expect_error(
    f_glm(mpg ~ cyl, data = mtcars_cat, type = "INVALID"),
    regexp = "should be one of"
  )
})

test_that("missing variables in formula cause informative errors", {
  skip_on_cran()
  expect_error(
    f_glm(nonexistent ~ cyl, data = mtcars_cat),
    regexp = "not found in the data"
  )
  expect_error(
    f_glm(mpg ~ nonexistent, data = mtcars_cat),
    regexp = "not found in the data"
  )
})

test_that("non-numeric response variable is rejected", {
  skip_on_cran()
  # cyl is a factor - not numeric
  expect_error(
    f_glm(cyl ~ am, data = mtcars_cat),
    regexp = "must be numeric"
  )
})

test_that("invalid family causes an error", {
  skip_on_cran()
  expect_error(
    f_glm(mpg ~ cyl, data = mtcars_cat, family = "not_a_family")
  )
})

test_that("valid alpha values are accepted without error", {
  skip_on_cran()
  expect_error(
    run_glm(mpg ~ cyl, family = gaussian, data = mtcars_cat,
            alpha = 0.01, output_type = "default"),
    NA  # NA means expect no error
  )
  expect_error(
    run_glm(mpg ~ cyl, family = gaussian, data = mtcars_cat,
            alpha = 0.1, output_type = "default"),
    NA
  )
})

# =============================================================================
# 2. RETURN STRUCTURE
# =============================================================================

test_that("f_glm returns correct S3 class", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(mpg ~ cyl, family = gaussian, data = mtcars_cat,
                    output_type = "default")
  expect_s3_class(result, "f_glm")
})

test_that("f_glm return list has all expected elements per response", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(mpg ~ cyl, family = gaussian, data = mtcars_cat,
                    output_type = "default")

  expect_true("mpg" %in% names(result))
  resp <- result$mpg
  required_keys <- c("model", "summary", "diagnostics", "posthoc",
                     "drop1", "lrt", "sig_effects", "sep_flag",
                     "lrt_null_dev", "lrt_resid_dev", "lrt_pct_explained")
  expect_true(all(required_keys %in% names(resp)),
              info = paste("Missing:", paste(setdiff(required_keys, names(resp)),
                                             collapse = ", ")))
})

test_that("GLM model object embedded in result is correct class", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(mpg ~ cyl, family = gaussian, data = mtcars_cat,
                    output_type = "default")

  expect_s3_class(result$mpg$model, "glm")
  expect_s3_class(result$mpg$model, "lm")
  expect_equal(result$mpg$model$family$family, "gaussian")
})

test_that("deviance values satisfy model fit inequality", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(mpg ~ cyl, family = gaussian, data = mtcars_cat,
                    output_type = "default")

  # Residual deviance must be less than null for any predictor
  expect_lt(result$mpg$lrt_resid_dev, result$mpg$lrt_null_dev)
  # McFadden pseudo-R2 must be in (0, 100)
  expect_gt(result$mpg$lrt_pct_explained, 0)
  expect_lt(result$mpg$lrt_pct_explained, 100)
})

# =============================================================================
# 3. BINOMIAL FAMILY - SEPARATION AND LETTERS
# =============================================================================

test_that("complete separation in vs ~ cyl sets sep_flag to TRUE", {
  skip_on_cran()
  # All 8-cylinder cars in mtcars have vs = 0 - textbook complete separation
  expect_true(all(mtcars[mtcars$cyl == 8, "vs"] == 0))

  set.seed(42)
  result <- run_glm(vs ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  expect_true(result$vs$sep_flag)
})

test_that("am ~ cyl has no separation and sep_flag is FALSE", {
  skip_on_cran()
  # am has mix of 0s and 1s across all cyl levels
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  expect_false(result$am$sep_flag)
})

test_that("sig_effects is TRUE for vs ~ cyl despite Wald p ~ 1 on cyl8", {
  skip_on_cran()
  # Wald p for cyl8 is ~0.99 (separation), but LRT p << 0.05
  # sig_effects must be based on LRT, not Wald
  set.seed(42)
  result <- run_glm(vs ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  expect_true(result$vs$sig_effects)
})

test_that("with separation: LRT pairwise letters differ between groups", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(vs ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  letters_col <- result$vs$posthoc$post_hoc_summary_table$Letter

  # LRT should produce differentiation - not all the same letter
  expect_false(all(letters_col == letters_col[1]),
               info = "All letters identical - LRT pairwise comparison likely failed")

  # Should not fall back to all em-dashes (that means LRT computation failed)
  expect_false(all(letters_col == "\u2014"),
               info = "All dashes - LRT letters were not computed")

  # Should not be 'ns' (overall model IS significant)
  expect_false(any(letters_col == "ns"))
})

test_that("non-significant model gives 'ns' letters for all groups", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(y ~ grp, family = binomial, data = d_null,
                    output_type = "default")

  expect_false(result$y$sig_effects)
  letters_col <- result$y$posthoc$post_hoc_summary_table$Letter
  expect_true(all(letters_col == "ns"))
})

test_that("sig_effects is TRUE and normal letters for am ~ cyl", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  expect_true(result$am$sig_effects)
  letters_col <- result$am$posthoc$post_hoc_summary_table$Letter
  expect_false(all(letters_col == "ns"))
  expect_false(all(letters_col == "\u2014"))
})

# =============================================================================
# 4. POISSON FAMILY
# =============================================================================

test_that("Poisson model returns correct structure", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(breaks ~ wool + tension, family = poisson,
                    data = warpbreaks, output_type = "default")

  expect_s3_class(result, "f_glm")
  expect_true("breaks" %in% names(result))
  expect_equal(result$breaks$model$family$family, "poisson")
})

test_that("Poisson model: wool + tension IS significant", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(breaks ~ wool + tension, family = poisson,
                    data = warpbreaks, output_type = "default")

  expect_true(result$breaks$sig_effects)
  expect_lt(result$breaks$lrt_resid_dev, result$breaks$lrt_null_dev)
})

test_that("drop1 result for Poisson has correct structure", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(breaks ~ wool, family = poisson,
                    data = warpbreaks, output_type = "default")

  d1 <- result$breaks$drop1
  expect_false(is.null(d1))
  expect_true(inherits(d1, "data.frame"))
  expect_true("Df" %in% names(d1))
  expect_true("Deviance" %in% names(d1))
})

# =============================================================================
# 5. QUASI FAMILIES
# =============================================================================

test_that("quasipoisson model runs and has correct family", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(breaks ~ wool, family = quasipoisson,
                    data = warpbreaks, output_type = "default")

  expect_s3_class(result, "f_glm")
  expect_equal(result$breaks$model$family$family, "quasipoisson")
})

test_that("quasipoisson: DHARMa sim_res is NULL (quasi-families not simulatable)", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(breaks ~ wool, family = quasipoisson,
                    data = warpbreaks, output_type = "default")

  expect_null(result$breaks$diagnostics$residuals$sim_res)
})

test_that("plot.f_glm emits message and does not crash for quasi-family", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(breaks ~ wool, family = quasipoisson,
                    data = warpbreaks, output_type = "default")

  expect_message(
    plot(result),
    regexp = "skipped"
  )
})

test_that("quasibinomial model runs without error", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = quasibinomial,
                    data = mtcars_cat, output_type = "default")

  expect_s3_class(result, "f_glm")
  expect_equal(result$am$model$family$family, "quasibinomial")
  expect_null(result$am$diagnostics$residuals$sim_res)
})

# =============================================================================
# 6. MULTIPLE RESPONSE VARIABLES
# =============================================================================

test_that("multiple responses produce separate entries in result", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am + vs ~ cyl, family = binomial,
                    data = mtcars_cat, output_type = "default")

  expect_s3_class(result, "f_glm")
  expect_true("am" %in% names(result))
  expect_true("vs" %in% names(result))
})

test_that("multiple responses: each has its own independent model", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am + vs ~ cyl, family = binomial,
                    data = mtcars_cat, output_type = "default")

  # am: no separation; vs: complete separation
  expect_false(result$am$sep_flag)
  expect_true(result$vs$sep_flag)

  # Both should have sig_effects = TRUE (LRT confirms both)
  expect_true(result$am$sig_effects)
  expect_true(result$vs$sig_effects)
})

# =============================================================================
# 7. ALL ADJUST METHODS
# =============================================================================

test_that("all adjust methods produce valid post-hoc tables", {
  skip_on_cran()
  adjust_methods <- c("sidak", "bonferroni", "tukey", "none", "fdr")

  for (adj in adjust_methods) {
    set.seed(42)
    result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                      adjust = adj, output_type = "default")

    ph <- result$am$posthoc$post_hoc_summary_table
    expect_true(is.data.frame(ph),
                info = paste("adjust =", adj, "did not return data.frame"))
    expect_true("Letter" %in% names(ph),
                info = paste("adjust =", adj, "missing Letter column"))
    expect_true("n" %in% names(ph),
                info = paste("adjust =", adj, "missing n column"))
    expect_equal(nrow(ph), 3L,
                 info = paste("adjust =", adj, "wrong number of rows (expected 3 cyl levels)"))
  }
})

# =============================================================================
# 8. OUTPUT TYPES
# =============================================================================

test_that("output_type = 'rmd' stores markdown string at top level", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "rmd", intro_text = FALSE)

  expect_true("rmd" %in% names(result))
  expect_type(result$rmd, "character")
  expect_gt(nchar(result$rmd), 200)   # non-trivial content
})

test_that("output_type = 'rmd' still has response entries alongside rmd element", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "rmd", intro_text = FALSE)

  # rmd and am entries coexist
  expect_true("am" %in% names(result))
  expect_true("rmd" %in% names(result))
})

test_that("output_type = 'console' forces output to console and returns object", {
  skip_on_cran()
  set.seed(42)
  expect_output(
    result <- f_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "console"),
    regexp = "GLM"
  )
  expect_s3_class(result, "f_glm")
})

test_that("output_type = 'excel' creates a valid non-empty file", {
  skip_on_cran()
  set.seed(42)
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "excel", save_as = tmp,
                    open_generated_files = FALSE)

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
  expect_s3_class(result, "f_glm")
})

test_that("output_type = 'word' creates a .docx file (requires pandoc)", {
  skip_on_cran()
  skip_if_no_pandoc()
  skip_if(!rmarkdown::pandoc_available(), "Pandoc not available")

  set.seed(42)
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)

  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "word", save_as = tmp,
                    open_generated_files = FALSE, intro_text = FALSE)

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
})

# =============================================================================
# 9. PRINT AND PLOT METHODS
# =============================================================================

test_that("print.f_glm works for Gaussian model", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(mpg ~ cyl, family = gaussian, data = mtcars_cat,
                    output_type = "default")

  expect_output(print(result), regexp = "GLM of response variable")
  expect_output(print(result), regexp = "Coefficients")
  expect_output(print(result), regexp = "Post hoc")
})

test_that("print.f_glm works for Binomial with separation", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(vs ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  expect_output(print(result), regexp = "GLM of response variable")
})

test_that("print.f_glm skips 'rmd' character entry without crash", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "rmd", intro_text = FALSE)

  # 'rmd' is a character string in the list - print must skip it gracefully
  expect_output(print(result), regexp = "GLM of response variable")
})

test_that("plot.f_glm runs without error for non-quasi binomial", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  expect_error(plot(result), NA)   # NA = expect no error
})

test_that("plot.f_glm skips 'rmd' element without crash", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "rmd", intro_text = FALSE)

  # Should not crash despite 'rmd' character element
  expect_error(
    suppressMessages(plot(result)),
    NA
  )
})

# =============================================================================
# 10. DISPERSION TEST PARAMETER
# =============================================================================

test_that("dispersion_test = FALSE omits Dispersion Diagnostics section", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(breaks ~ wool, family = poisson, data = warpbreaks,
                    output_type = "rmd", intro_text = FALSE,
                    dispersion_test = FALSE)

  expect_false(grepl("Dispersion Diagnostics", result$rmd, fixed = TRUE))
})

test_that("dispersion_test = TRUE includes Dispersion Diagnostics section for Poisson", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(breaks ~ wool, family = poisson, data = warpbreaks,
                    output_type = "rmd", intro_text = FALSE,
                    dispersion_test = TRUE)

  expect_true(grepl("Dispersion Diagnostics", result$rmd, fixed = TRUE))
})

test_that("Bernoulli 0/1 data skips dispersion test with Bernoulli explanation", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(vs ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "rmd", intro_text = FALSE,
                    dispersion_test = TRUE)

  # Must explain the skip, not run the DHARMa test
  expect_true(grepl("Bernoulli", result$rmd, fixed = TRUE))
  expect_false(grepl("DHARMa Dispersion Test", result$rmd, fixed = TRUE))
})

test_that("quasipoisson shows phi parameter, not DHARMa test", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(breaks ~ wool, family = quasipoisson, data = warpbreaks,
                    output_type = "rmd", intro_text = FALSE,
                    dispersion_test = TRUE)

  expect_true(grepl("Quasi-dispersion parameter", result$rmd, fixed = TRUE))
  expect_false(grepl("DHARMa Dispersion Test", result$rmd, fixed = TRUE))
})

# =============================================================================
# 11. NUMERIC PREDICTOR WARNING
# =============================================================================

test_that("numeric predictor with <= 10 unique values triggers a warning", {
  skip_on_cran()
  d <- mtcars
  d$vs <- as.integer(d$vs)
  # cyl is numeric with 3 unique values (4, 6, 8) - should warn

  expect_warning(
    suppressMessages(
      f_glm(vs ~ cyl, family = binomial, data = d, output_type = "default")
    ),
    regexp = "numeric"
  )
})

test_that("factor predictor does NOT trigger a numeric predictor warning", {
  skip_on_cran()
  # cyl IS a factor - must call f_glm directly since run_glm() suppresses warnings
  expect_warning(
    suppressMessages(
      f_glm(vs ~ cyl, family = binomial, data = mtcars_cat,
            output_type = "default")
    ),
    NA   # NA = expect no warning
  )
})

# =============================================================================
# 12. CONTINUOUS PREDICTOR EXCLUDED FROM EMMEANS
# =============================================================================

test_that("continuous covariate is excluded from emmeans specs with a warning", {
  skip_on_cran()
  d <- mtcars
  d$cyl <- as.factor(d$cyl)

  # Uses warning() not message() so it propagates through suppressMessages()
  # inside f_glm's internal caching - suppressMessages() would swallow message()
  expect_warning(
    suppressMessages(
      f_glm(mpg ~ cyl + hp, family = gaussian, data = d,
            output_type = "default")
    ),
    regexp = "continuous predictor"
  )
})

test_that("with continuous covariate: post-hoc table rows = factor levels only", {
  skip_on_cran()
  d <- mtcars
  d$cyl <- as.factor(d$cyl)

  result <- suppressMessages(
    run_glm(mpg ~ cyl + hp, family = gaussian, data = d,
            output_type = "default")
  )

  ph <- result$mpg$posthoc$post_hoc_summary_table
  # emmeans over cyl only (3 levels), hp held at mean
  expect_equal(nrow(ph), 3L)
})

# =============================================================================
# 13. TYPE ARGUMENT (response vs link scale)
# =============================================================================

test_that("type = 'response' gives probability column for binomial", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    type = "response", output_type = "default")

  ph <- result$am$posthoc$post_hoc_summary_table
  expect_true("prob" %in% names(ph))
  # Probabilities must lie in [0, 1]
  expect_true(all(ph$prob >= 0 & ph$prob <= 1, na.rm = TRUE))
})

test_that("type = 'link' gives emmean column on logit scale for binomial", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    type = "link", output_type = "default")

  ph <- result$am$posthoc$post_hoc_summary_table
  # Should have an emmean-like column (possibly renamed "emmean..")
  emm_present <- any(grepl("emmean", names(ph), ignore.case = TRUE))
  expect_true(emm_present)
})

# =============================================================================
# 14. POST-HOC TABLE STRUCTURE
# =============================================================================

test_that("post-hoc table has required columns", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  ph <- result$am$posthoc$post_hoc_summary_table
  expect_true(is.data.frame(ph))
  expect_true("Letter" %in% names(ph))
  expect_true("n"      %in% names(ph))
  expect_true("cyl"    %in% names(ph))
})

test_that("post-hoc table has one row per factor level", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  ph <- result$am$posthoc$post_hoc_summary_table
  expect_equal(nrow(ph), nlevels(mtcars_cat$cyl))  # 3
})

test_that("cld_text is a non-empty string", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  cld <- result$am$posthoc$cld_text
  expect_type(cld, "character")
  expect_true(nchar(cld) > 0)
})

# =============================================================================
# 15. DROP1 AND LRT STRUCTURE
# =============================================================================

test_that("drop1 result has anova class and key columns", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  d1 <- result$am$drop1
  expect_false(is.null(d1))
  expect_true(inherits(d1, "data.frame"))
  expect_true("Df"       %in% names(d1))
  expect_true("Deviance" %in% names(d1))
})

test_that("lrt result is a data.frame with NULL and predictor rows", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  lrt <- result$am$lrt
  expect_false(is.null(lrt))
  expect_true(inherits(lrt, "data.frame"))
  # Sequential anova: NULL row + cyl row = 2 rows
  expect_equal(nrow(lrt), 2L)
})

test_that("lrt_pct_explained is between 0 and 100 for significant model", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  pct <- result$am$lrt_pct_explained
  expect_true(is.numeric(pct))
  expect_gt(pct, 0)
  expect_lt(pct, 100)
})

# =============================================================================
# 16. FAMILY AS STRING VS FUNCTION
# =============================================================================

test_that("family passed as string gives same model as function", {
  skip_on_cran()
  set.seed(42)
  result_fn  <- run_glm(am ~ cyl, family = binomial,
                        data = mtcars_cat, output_type = "default")
  set.seed(42)
  result_str <- run_glm(am ~ cyl, family = "binomial",
                        data = mtcars_cat, output_type = "default")

  expect_equal(coef(result_fn$am$model),
               coef(result_str$am$model))
})

# =============================================================================
# 17. MISC PARAMETERS
# =============================================================================

test_that("diagnostic_plots = FALSE stores show_plot = FALSE in diagnostics", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    diagnostic_plots = FALSE, output_type = "default")

  expect_false(result$am$diagnostics$show_plot)
})

test_that("intro_text = FALSE produces shorter rmd output", {
  skip_on_cran()
  set.seed(42)
  result_with <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                         output_type = "rmd", intro_text = TRUE)
  result_without <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                            output_type = "rmd", intro_text = FALSE)

  expect_gt(nchar(result_with$rmd), nchar(result_without$rmd))
})

test_that("sig_effects and sep_flag are stored as logical scalars", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  expect_type(result$am$sig_effects, "logical")
  expect_length(result$am$sig_effects, 1L)
  expect_type(result$am$sep_flag, "logical")
  expect_length(result$am$sep_flag, 1L)
})

test_that("influential points structure is present and correct type", {
  skip_on_cran()
  set.seed(42)
  result <- run_glm(am ~ cyl, family = binomial, data = mtcars_cat,
                    output_type = "default")

  infl <- result$am$diagnostics$influence
  expect_true(!is.null(infl))
  expect_true("hat_values" %in% names(infl))
  expect_true("influential_points" %in% names(infl))
  expect_true(is.numeric(infl$hat_values))
  expect_equal(length(infl$hat_values), nrow(mtcars_cat))
})

test_that("higher influence_threshold detects fewer influential points", {
  skip_on_cran()
  set.seed(42)
  result_strict <- run_glm(mpg ~ cyl, family = gaussian, data = mtcars_cat,
                           influence_threshold = 1,
                           output_type = "default")
  result_lax    <- run_glm(mpg ~ cyl, family = gaussian, data = mtcars_cat,
                           influence_threshold = 10,
                           output_type = "default")

  n_strict <- length(result_strict$mpg$diagnostics$influence$influential_points)
  n_lax    <- length(result_lax$mpg$diagnostics$influence$influential_points)

  expect_gte(n_strict, n_lax)
})



# =============================================================================
# f_glm_new_tests.R
# Additional tests for f_glm() covering the fix where the descriptive
# f_summary() table now reads from glm_fit$model so it stays in sync
# with the rows actually fitted when subset / na.action / weights /
# offset are used via `...`.
# =============================================================================
# Deterministic 2-group binomial data
make_bin_data <- function(seed = 2026, n_per_group = 40) {
  set.seed(seed)
  data.frame(
    y   = rbinom(2 * n_per_group, 1,
                 prob = rep(c(0.3, 0.7), each = n_per_group)),
    grp = factor(rep(c("ctrl", "trt"), each = n_per_group)),
    x   = rnorm(2 * n_per_group)
  )
}

test_that("f_glm runs with subset passed through dots", {
  skip_on_cran()
  df <- make_bin_data()
  keep <- seq_len(nrow(df)) %% 2L == 0L

  expect_no_error(
    res <- suppressMessages(
      f_glm(y ~ grp, data = df, family = binomial,
            subset               = keep,
            output_type          = "default",
            open_generated_files = FALSE,
            intro_text           = FALSE)
    )
  )
  # Fitted model saw only the subset rows.
  expect_equal(nobs(res[[1]]$model), sum(keep))
})

test_that("descriptive summary table reflects fitted rows, not original data", {
  df   <- make_bin_data()
  keep <- seq_len(nrow(df)) %% 2L == 0L

  res <- suppressWarnings(suppressMessages(
    f_glm(y ~ grp, data = df, family = binomial,
          subset               = keep,
          output_type          = "default",
          open_generated_files = FALSE,
          intro_text           = FALSE)
  ))

  # Sanity: the model saw half the rows.
  expect_equal(nobs(res[[1]]$model), sum(keep))

  # Primary assertion: the per-group n column in the post-hoc summary
  # table sums to nobs(model), not nrow(df). If the table were built
  # from the raw input data, sum(ph$n) would equal nrow(df) (= 80).
  ph <- res[[1]]$posthoc$post_hoc_summary_table
  expect_false(is.null(ph))
  expect_true("n" %in% names(ph))
  expect_equal(sum(ph$n, na.rm = TRUE), sum(keep))

  # Both factor levels remain in the subset, so both should still be
  # listed in the post-hoc table.
  expect_setequal(as.character(ph$grp), c("ctrl", "trt"))
})


test_that("na.action = na.omit is honored by glm via dots", {
  skip_on_cran()
  df <- make_bin_data()
  df$x[c(1, 2, 3, 80)] <- NA

  res <- suppressWarnings(
    f_glm(y ~ grp + x, data = df, family = binomial,
          na.action = na.omit,
          output_type = "default",
          open_generated_files = FALSE,
          intro_text = FALSE)
  )
  expect_equal(nobs(res[[1]]$model), 76L)      # 80 - 4
})
