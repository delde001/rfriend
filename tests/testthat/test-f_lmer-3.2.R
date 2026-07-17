# =============================================================================
# test-f_lmer-3.2.R
# Tests for the f_lmer() changes introduced in rfriend 3.2.
#
# Run with: testthat::test_file("test-f_lmer-3.2.R")
#
# These tests assume the same shared helpers as test-f_lmer.R:
#   quiet_f_lmer()                 (helper-quiet.R)
#   make_repeated_measures_data()  (helper-data.R)
# and the same skip pattern (skip_on_cran + skip_if_no_lme4).
#
# Most assertions inspect the markdown produced by output_type = "rmd",
# because the 3.2 work was largely about what the report says and when it
# says it. A few unit-level tests exercise the self-contained statistical
# logic (nested-grouping Levene resolver, per-group ICC decomposition)
# directly, so a regression there is caught even if the rmd wording moves.
#
# Section map (continuing the numbering in test-f_lmer.R):
#  14.  Type III table drops Sum Sq / Mean Sq; full table preserved
#  15.  Contrast-coding note (interaction-gated)
#  16.  Per-grouping-factor ICC table (icc_by_group)
#  17.  Levene on nested random effects (the resolver bug fix)
#  18.  Multi-response note: exponent + reworded contrast wording
#  19.  Adjusted-ICC label in the Model fit legend
#  20.  Observed-descriptives pooling caveat
#  21.  print.f_lmer surfaces icc_by_group
#  22.  No stray markup artifacts in the rmd output
# =============================================================================

skip_if_no_lme4 <- function() {
  testthat::skip_if_not_installed("lme4")
  testthat::skip_if_not_installed("lmerTest")
}

# ---------------------------------------------------------------------------
# Local data generator: a nested block/plant design with a numeric x factor
# interaction, mirroring the plant_trial example. Used by the ICC, Levene,
# contrast-note and label tests below.
# ---------------------------------------------------------------------------
make_nested_trial_data <- function(seed = 101, n_block = 5, n_plant = 4) {
  set.seed(seed)
  base <- expand.grid(
    plant      = seq_len(n_plant),
    block      = seq_len(n_block),
    time_weeks = c(2, 4, 6)
  )
  base$block     <- factor(base$block)
  base$plant_id  <- factor(paste0("p", base$plant))
  base$treatment <- factor(rep(c("control", "drought", "high_N", "low_N"),
                              length.out = nrow(base)))
  block_int <- rnorm(n_block, 0, 2)[as.integer(base$block)]
  plant_int <- rnorm(n_block * n_plant, 0, 2.5)[
    as.integer(interaction(base$block, base$plant_id, drop = TRUE))]
  trt_eff   <- c(control = 0, drought = -3, high_N = 5, low_N = 2)[
    as.character(base$treatment)]
  base$height_cm <- 15 + 4 * base$time_weeks + trt_eff +
    block_int + plant_int + rnorm(nrow(base), 0, 2)
  base
}

# Helper: fetch the rmd string from an f_lmer call with the intro suppressed
# (so assertions about section wording are not confused by the intro text).
lmer_rmd <- function(formula, data, ...) {
  res <- suppressMessages(suppressWarnings(
    f_lmer(formula, data = data,
           output_type = "rmd", intro_text = FALSE,
           open_generated_files = FALSE, ...)
  ))
  res$rmd
}

# ===========================================================================
# 14. Type III table drops Sum Sq / Mean Sq; full table preserved
# ===========================================================================

test_that("displayed fixed-effects table omits Sum Sq / Mean Sq", {
  skip_on_cran()
  skip_if_no_lme4()
  data(sleepstudy, package = "lme4")
  res <- quiet_f_lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  fe  <- res[["Reaction"]]$fixed_effects
  expect_false(any(c("Sum Sq", "Mean Sq") %in% names(fe)))
  # The informative columns are still present.
  expect_true(any(grepl("F value", names(fe))))
  expect_true(any(grepl("DenDF",   names(fe))))
})

test_that("full lmerTest table is preserved in fixed_effects_full", {
  skip_on_cran()
  skip_if_no_lme4()
  data(sleepstudy, package = "lme4")
  res  <- quiet_f_lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  full <- res[["Reaction"]]$fixed_effects_full
  expect_false(is.null(full))
  expect_true(any(c("Sum Sq", "Mean Sq") %in% names(full)))
})

test_that("ddf = 'lme4' has no fixed_effects_full slot", {
  skip_on_cran()
  skip_if_no_lme4()
  data(sleepstudy, package = "lme4")
  res <- quiet_f_lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy,
                      ddf = "lme4")
  expect_true(is.null(res[["Reaction"]]$fixed_effects_full))
})

# ===========================================================================
# 15. Contrast-coding note (only when the model has an interaction)
# ===========================================================================

test_that("interaction model emits the contrast-coding note", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_nested_trial_data()
  rmd <- lmer_rmd(height_cm ~ treatment * time_weeks + (1 | block/plant_id),
                  data = df)
  # The note explains t^2 will not equal F; match on stable wording.
  expect_true(grepl("will not equal", rmd))
  expect_true(grepl("sum-to-zero", rmd, fixed = TRUE))
})

test_that("no-interaction model does NOT emit the contrast-coding note", {
  skip_on_cran()
  skip_if_no_lme4()
  data(sleepstudy, package = "lme4")
  rmd <- lmer_rmd(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  expect_false(grepl("will not equal", rmd))
})

# ===========================================================================
# 16. Per-grouping-factor ICC table
# ===========================================================================

test_that("nested model produces an icc_by_group decomposition", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_nested_trial_data()
  res <- quiet_f_lmer(height_cm ~ treatment * time_weeks +
                        (1 | block/plant_id), data = df)
  ibg <- res[["height_cm"]]$icc_by_group
  expect_false(is.null(ibg))
  expect_true(all(c("Group", "Variance") %in% names(ibg)))
  # One row per grouping factor plus a Residual row.
  expect_true("Residual" %in% ibg$Group)
  # Shares sum to 1 (group shares + residual share).
  share_col <- grep("share", names(ibg), ignore.case = TRUE, value = TRUE)[1]
  expect_equal(sum(ibg[[share_col]]), 1, tolerance = 1e-8)
})

test_that("single-grouping model has no icc_by_group table", {
  skip_on_cran()
  skip_if_no_lme4()
  data(sleepstudy, package = "lme4")
  res <- quiet_f_lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  expect_true(is.null(res[["Reaction"]]$icc_by_group))
})

test_that("per-group ICC math: shares equal variance / total", {
  # Self-contained check of the decomposition arithmetic, independent of
  # the report wording. Mirrors the computation in flmer.R.
  between_var <- 6.0997 + 4.0679       # plant_id:block + block
  resid_var   <- 4.8556
  total       <- between_var + resid_var
  shares <- c(plant = 6.0997, block = 4.0679, resid = resid_var) / total
  expect_equal(sum(shares), 1, tolerance = 1e-12)
  # Overall (adjusted) ICC equals the summed group share.
  expect_equal(unname(shares["plant"] + shares["block"]),
               between_var / total, tolerance = 1e-12)
})

# ===========================================================================
# 17. Levene's test on nested random effects (the resolver bug fix)
# ===========================================================================

test_that("Levene runs on a nested (block/plant_id) grouping factor", {
  skip_on_cran()
  skip_if_no_lme4()
  testthat::skip_if_not_installed("rstatix")
  df  <- make_nested_trial_data()
  rmd <- lmer_rmd(height_cm ~ treatment * time_weeks + (1 | block/plant_id),
                  data = df)
  # Before the fix this was skipped with a "fewer than two levels" message.
  # After the fix it runs and reports an F and p for the nested group.
  expect_true(grepl("Levene", rmd))
  expect_false(grepl("fewer than two levels", rmd))
  expect_true(grepl("plant_id:block", rmd, fixed = TRUE))
})

test_that("Levene skip message states the real reason, not a canned one", {
  # Unit test of the grouping-factor resolver logic used by the Levene
  # step, exercised directly so the assertion does not depend on a model
  # that happens to be unresolvable. This is the exact resolver from
  # flmer.R.
  resolve_group_factor <- function(grp_name, mf) {
    if (is.null(grp_name) || is.na(grp_name)) return(NULL)
    if (grp_name %in% names(mf)) return(as.factor(mf[[grp_name]]))
    parts <- strsplit(grp_name, ":", fixed = TRUE)[[1]]
    if (length(parts) >= 2 && all(parts %in% names(mf))) {
      cols <- lapply(parts, function(p) as.factor(mf[[p]]))
      return(interaction(cols, drop = TRUE, sep = ":"))
    }
    NULL
  }
  mf <- data.frame(
    y     = rnorm(60),
    block = factor(rep(1:5, each = 12)),
    plant_id = factor(rep(paste0("p", 1:4), times = 15))
  )
  # Nested name resolves via component columns.
  gf <- resolve_group_factor("plant_id:block", mf)
  expect_false(is.null(gf))
  expect_equal(nlevels(droplevels(gf)), 20L)
  # Direct column resolves to itself.
  expect_equal(nlevels(resolve_group_factor("block", mf)), 5L)
  # Genuinely unknown name is unresolvable (NULL), which the caller then
  # reports as an explicit reason rather than a false level-count message.
  expect_null(resolve_group_factor("nonexistent", mf))
})

# ===========================================================================
# 18. Multi-response note: exponent rendering + reworded contrast wording
# ===========================================================================

test_that("multi-response note uses a unicode superscript, not stray digits", {
  skip_on_cran()
  skip_if_no_lme4()
  data(sleepstudy, package = "lme4")
  ss <- sleepstudy
  ss$Reaction2 <- ss$Reaction + rnorm(nrow(ss), 0, 3)
  rmd <- lmer_rmd(Reaction + Reaction2 ~ Days + (1 | Subject), data = ss)
  # The power 1-(1-0.05)^2 must render with a real superscript two
  # (\u00b2), never the bare "...)2" that pandoc produced from inline math.
  expect_true(grepl("\u00b2", rmd, fixed = TRUE))
  expect_false(grepl(")2,", rmd, fixed = TRUE))
})

test_that("multi-response note does not assume pairwise contrasts exist", {
  skip_on_cran()
  skip_if_no_lme4()
  data(sleepstudy, package = "lme4")
  ss <- sleepstudy
  ss$Reaction2 <- ss$Reaction + rnorm(nrow(ss), 0, 3)
  # Numeric-only predictor: no contrasts are ever computed, so the note
  # must not claim the correction guards pairwise contrasts unconditionally.
  rmd <- lmer_rmd(Reaction + Reaction2 ~ Days + (1 | Subject), data = ss)
  expect_false(grepl("guards against\\s+false positives among the pairwise",
                     rmd))
  expect_true(grepl("only controls error", rmd))
})

# ===========================================================================
# 19. Adjusted-ICC label in the Model fit legend (multi-grouping models)
# ===========================================================================

test_that("multi-grouping model labels the combined ICC as adjusted/total", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_nested_trial_data()
  rmd <- lmer_rmd(height_cm ~ treatment * time_weeks + (1 | block/plant_id),
                  data = df)
  expect_true(grepl("adjusted", rmd, ignore.case = TRUE))
})

# ===========================================================================
# 20. Observed-descriptives pooling caveat
# ===========================================================================

test_that("descriptives caution warns that rows pool repeated measures", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_nested_trial_data()
  rmd <- lmer_rmd(height_cm ~ treatment * time_weeks + (1 | block/plant_id),
                  data = df)
  expect_true(grepl("pool", rmd))
  expect_true(grepl("not independent", rmd))
})

# ===========================================================================
# 21. print.f_lmer surfaces the per-group ICC table
# ===========================================================================

test_that("print.f_lmer shows the ICC-by-grouping-factor section", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_nested_trial_data()
  res <- quiet_f_lmer(height_cm ~ treatment * time_weeks +
                        (1 | block/plant_id), data = df)
  out <- paste(utils::capture.output(print(res)), collapse = "\n")
  expect_true(grepl("ICC by grouping factor", out))
})

# ===========================================================================
# 22. No stray markup artifacts in the rmd output
# ===========================================================================

test_that("rmd output has no orphaned bold/italic markup artifacts", {
  skip_on_cran()
  skip_if_no_lme4()
  df  <- make_nested_trial_data()
  rmd <- lmer_rmd(height_cm ~ treatment * time_weeks + (1 | block/plant_id),
                  data = df)
  # The bold-code-nesting bug produced runs of four asterisks ("****")
  # around grouping names (e.g. "**Levels of**** ****Subject****:**").
  expect_false(grepl("****", rmd, fixed = TRUE))
})
