# =============================================================================
# test-f_stat_wizard.R
# =============================================================================
# Tests for f_stat_wizard().
#
# Covers:
#   A. S3 dispatch (formula and data.frame methods)
#   B. formula / data argument order
#   C. Structured list return with metadata slots
#   D. paired / id_col handling
#   E. run = TRUE executes the recommended rfriend function
#   F. Intercept-only formula (y ~ 1)
#   G. Large-n safe_shapiro skip branch does not crash the wizard
# =============================================================================


# ---------------------------------------------------------------------------
# A. S3 dispatch
# ---------------------------------------------------------------------------

test_that("formula interface returns an f_stat_wizard object", {
  res <- run_quiet(f_stat_wizard(Sepal.Length ~ Species, data = iris))
  expect_s3_class(res, "f_stat_wizard")
})

test_that("data.frame interface with explicit formula arg works", {
  res <- run_quiet(f_stat_wizard(iris, formula = Sepal.Length ~ Species))
  expect_s3_class(res, "f_stat_wizard")
})

test_that("data.frame interface errors when formula is missing", {
  expect_error(
    suppressMessages(f_stat_wizard(iris)),
    "formula"
  )
})

test_that("named-only call f_stat_wizard(data = ..., formula = ...) works", {
  # Regression guard for the match.call() shim that strips data/formula
  # from dots to avoid 'matched by multiple actual arguments'.
  res <- run_quiet(
    f_stat_wizard(data = iris, formula = Sepal.Length ~ Species)
  )
  expect_s3_class(res, "f_stat_wizard")
})


# ---------------------------------------------------------------------------
# B, C. Structured return with metadata slots
# ---------------------------------------------------------------------------

test_that("return object exposes the documented metadata slots", {
  res <- run_quiet(f_stat_wizard(Sepal.Length ~ Species, data = iris))

  expected_slots <- c("y_type", "x_types", "n_groups", "group_sizes",
                      "recommended_call", "report")
  for (s in expected_slots) {
    expect_true(s %in% names(res))
  }

  expect_equal(res$n_groups, 3L)
  expect_true(is.call(res$recommended_call) ||
              is.character(res$recommended_call) ||
              is.null(res$recommended_call))
  expect_true(is.character(res$report) || is.list(res$report))
})

test_that("group_sizes reports correct counts per level", {
  res <- run_quiet(f_stat_wizard(Sepal.Length ~ Species, data = iris))
  gs  <- res$group_sizes
  expect_true(is.numeric(gs) || is.integer(gs))
  expect_true(all(gs == 50))  # iris has 50 per Species
})

test_that("recommended_call references an rfriend function when appropriate", {
  # 3 groups, normal-ish data should recommend f_aov()
  res <- run_quiet(f_stat_wizard(Sepal.Length ~ Species, data = iris))
  if (!is.null(res$recommended_call)) {
    rc <- if (is.call(res$recommended_call)) {
      deparse(res$recommended_call)
    } else {
      res$recommended_call
    }
    expect_match(rc, "f_aov|aov", ignore.case = TRUE)
  }
})


# ---------------------------------------------------------------------------
# D. paired (via id_col)
# ---------------------------------------------------------------------------

test_that("id_col triggers paired-design handling", {
  set.seed(1)
  df <- data.frame(
    subject = factor(rep(1:20, each = 2)),
    time    = factor(rep(c("pre", "post"), times = 20)),
    score   = c(rnorm(20, 10, 2), rnorm(20, 12, 2))
  )

  res <- run_quiet(
    f_stat_wizard(score ~ time, data = df, id_col = "subject")
  )
  expect_s3_class(res, "f_stat_wizard")

  # A paired 2-group numeric recommendation should mention paired t-test
  # or Wilcoxon signed rank.
  if (!is.null(res$recommended_call)) {
    rc <- if (is.call(res$recommended_call)) {
      deparse(res$recommended_call)
    } else {
      res$recommended_call
    }
    expect_match(rc, "paired|signed|f_t_test|f_wilcox", ignore.case = TRUE)
  }
})

test_that("id_col not in data is rejected", {
  expect_error(
    suppressMessages(
      f_stat_wizard(Sepal.Length ~ Species, data = iris,
                    id_col = "no_such_column")
    ),
    "id_col"
  )
})


# ---------------------------------------------------------------------------
# E. run = TRUE
# ---------------------------------------------------------------------------

test_that("run = TRUE executes the recommended function and stores result", {
  # Skipped on CRAN: run = TRUE fires the full recommended pipeline
  # (e.g. f_aov with Pandoc) which is far too heavy for CRAN's budget.
  skip_on_cran()

  res <- run_quiet_warn(
    f_stat_wizard(Sepal.Length ~ Species, data = iris, run = TRUE, output_type = "default")
  )
  expect_true("run_result" %in% names(res))
})


# ---------------------------------------------------------------------------
# F. Intercept-only formula
# ---------------------------------------------------------------------------

test_that("y ~ 1 is classified before advice is generated", {
  # Regression guard: before the fix, non-numeric Y blindly triggered
  # a one-sample t-test recommendation.
  res <- run_quiet(f_stat_wizard(Sepal.Length ~ 1, data = iris))
  expect_s3_class(res, "f_stat_wizard")
  expect_true(!is.null(res$y_type))
})


# ---------------------------------------------------------------------------
# G. Large-n safe_shapiro skip branch
# ---------------------------------------------------------------------------

test_that("wizard does not crash with n > 5000 (safe_shapiro skip)", {
  skip_on_cran()  # n = 6000 exceeds CRAN's fast-test budget

  set.seed(7)
  big <- data.frame(
    y = rnorm(6000),
    g = factor(sample(c("A", "B"), 6000, replace = TRUE))
  )
  expect_no_error(
    run_quiet(f_stat_wizard(y ~ g, data = big))
  )
})

test_that("binary factor response: wizard's f_glm code names the event", {
  res <- suppressWarnings(suppressMessages(
    f_stat_wizard(infected ~ chloroplast_count, data = plant_trial)))
  txt <- paste(res$report, collapse = "\n")
  expect_match(txt, "event='yes'", fixed = TRUE)
  expect_match(txt, "Change it to 'no'", fixed = TRUE)
})

# ---- recommendations for the 3.2.0 functions and runnable chi-square code --

wiz_report <- function(...) {
  res <- suppressWarnings(suppressMessages(f_stat_wizard(...)))
  list(res = res, txt = paste(res$report, collapse = "\n"))
}
wk6 <- subset(plant_trial, time_weeks == 6)
pt  <- plant_trial
pt$time_f <- factor(pt$time_weeks)

test_that("numeric predictor, normal response -> f_lm()", {
  w <- wiz_report(height_cm ~ chloroplast_count, data = wk6)
  expect_match(w$txt, "Code: f_lm(height_cm ~ chloroplast_count", fixed = TRUE)
  expect_false(grepl("Code: f_glm(height_cm", w$txt, fixed = TRUE))
})

test_that("several numeric predictors -> f_lm(), not f_aov()", {
  w <- wiz_report(height_cm ~ chloroplast_count + biomass_g, data = wk6)
  expect_match(w$txt, "Code: f_lm(", fixed = TRUE)
  expect_false(grepl("Code: f_aov(", w$txt, fixed = TRUE))
})

test_that("non-normal repeated measures -> f_friedman(), and run = TRUE runs it", {
  w <- wiz_report(biomass_g ~ time_f, data = pt, id_col = "plant_id", run = TRUE)
  expect_match(w$txt, "Code: f_friedman(biomass_g ~ time_f | plant_id", fixed = TRUE)
  expect_s3_class(w$res$run_result, "f_friedman")
})

test_that("Box-Cox fixable repeated measures also offers the mixed-model route", {
  w <- wiz_report(biomass_g ~ time_f, data = pt, id_col = "plant_id")
  expect_match(w$txt, "f_boxcox() fixes normality", fixed = TRUE)
  expect_match(w$txt, "f_lmer(biomass_g_t ~ time_f + (1|plant_id)", fixed = TRUE)
})

test_that("normal repeated measures no longer suggests f_aov() with Error()", {
  w <- wiz_report(height_cm ~ time_f, data = pt, id_col = "plant_id", run = TRUE)
  expect_false(grepl("f_aov(height_cm ~ time_f + Error", w$txt, fixed = TRUE))
  expect_s3_class(w$res$run_result, "f_lmer")
})

test_that("chi-square recommendation is runnable code", {
  w <- wiz_report(infected ~ treatment, data = wk6, run = TRUE)
  expect_match(w$txt, "Code: f_chisq_test(table(wk6$treatment, wk6$infected))", fixed = TRUE)
  expect_s3_class(w$res$run_result, "f_chisq_test")
  code <- sub(".*Code: ", "", grep("Code: f_chisq_test", w$res$report, value = TRUE)[1])
  expect_no_error(suppressMessages(eval(parse(text = code))))
})

test_that("the report contains no em dash or multiplication sign (ASCII only)", {
  # failed pairing: these lines used an em dash and a multiplication sign
  w <- wiz_report(height_cm ~ treatment, data = pt, id_col = "plant_id")
  expect_match(w$txt, "WARN: Unbalanced pairing: 60 subject x group", fixed = TRUE)
  expect_match(w$txt, "20 subject x group combination(s) have multiple", fixed = TRUE)
  expect_false(grepl("\u2014", w$txt, fixed = TRUE))
  expect_false(grepl("\u00d7", w$txt, fixed = TRUE))
  # the closing notes (BETA line) appear in every report
  w2 <- wiz_report(height_cm ~ treatment, data = pt)
  expect_match(w2$txt, "under active development: verify recommendations.",
               fixed = TRUE)
  expect_false(grepl("\u2014", w2$txt, fixed = TRUE))
  # verified pairing line
  w3 <- wiz_report(height_cm ~ time_f, data = pt, id_col = "plant_id")
  expect_match(w3$txt, "(20 subjects x 3 groups, one observation per cell)",
               fixed = TRUE)
  expect_false(grepl("\u00d7", w3$txt, fixed = TRUE))
})


# ---------------------------------------------------------------------------
# H. Ordinal responses (BUGS #26): an ordered factor gets rank-based tests;
#    an integer response on a short scale gets a "may be a score" note.
# ---------------------------------------------------------------------------

ord_data <- function() {
  set.seed(2)
  d <- data.frame(treatment = factor(rep(c("A", "B", "C"), each = 8)),
                  id        = factor(rep(1:8, 3)),
                  damage    = c(sample(1:3, 8, TRUE), sample(2:4, 8, TRUE),
                                sample(3:5, 8, TRUE)),
                  x         = rnorm(24))
  d$damage_o <- factor(d$damage, ordered = TRUE)
  d
}

test_that("ordered factor response is classified as ordinal", {
  d <- ord_data()
  w <- wiz_report(damage_o ~ treatment, data = d)
  expect_identical(w$res$y_type, "ordinal")
  expect_match(w$txt, "Ordered factor (ORDINAL, 5 levels: 1 < 2 < 3 < 4 < 5)", fixed = TRUE)
  expect_false(grepl("Chi-square", w$txt, fixed = TRUE))
})

test_that("ordinal response, 3+ independent groups -> f_kruskal_test on rank codes", {
  d <- ord_data()
  w <- wiz_report(damage_o ~ treatment, data = d, run = TRUE, output_type = "default")
  expect_match(w$txt, "as.integer(d$damage_o)", fixed = TRUE)
  expect_match(w$txt, "Step 2: f_kruskal_test(damage_o_rank ~ treatment", fixed = TRUE)
  expect_s3_class(w$res$run_result, "f_kruskal_test")
})

test_that("ordinal response, 3+ paired groups -> f_friedman", {
  d <- ord_data()
  w <- wiz_report(damage_o ~ treatment, data = d, id_col = "id", run = TRUE,
                  output_type = "default")
  expect_match(w$txt, "Step 2: f_friedman(damage_o_rank ~ treatment | id", fixed = TRUE)
  expect_s3_class(w$res$run_result, "f_friedman")
})

test_that("ordinal response, 2 groups -> f_wilcox_test (independent and paired)", {
  d <- droplevels(subset(ord_data(), treatment != "C"))
  w <- wiz_report(damage_o ~ treatment, data = d, run = TRUE, output_type = "default")
  expect_match(w$txt, "Test: Mann-Whitney U / Wilcoxon Rank-Sum Test (ordinal response)", fixed = TRUE)
  expect_s3_class(w$res$run_result, "f_wilcox_test")
  wp <- wiz_report(damage_o ~ treatment, data = d, id_col = "id", run = TRUE,
                   output_type = "default")
  expect_match(wp$txt, "paired=TRUE", fixed = TRUE)
  expect_s3_class(wp$res$run_result, "f_wilcox_test")
})

test_that("ordinal response with numeric X -> Spearman; several predictors -> polr", {
  d <- ord_data()
  w <- wiz_report(damage_o ~ x, data = d)
  expect_match(w$txt, "method='spearman'", fixed = TRUE)
  w2 <- wiz_report(damage_o ~ treatment + x, data = d)
  expect_match(w2$txt, "MASS::polr(damage_o ~ treatment + x", fixed = TRUE)
  expect_null(w2$res$recommended_call)
})

test_that("integer response on a short scale keeps count advice but notes a possible score", {
  d <- ord_data()
  w <- wiz_report(damage ~ treatment, data = d)
  expect_identical(w$res$y_type, "count")
  expect_match(w$txt, "it is ORDINAL, not a count", fixed = TRUE)
  expect_match(w$txt, "factor(d$damage, ordered = TRUE)", fixed = TRUE)
  # a count with a wide range gets no score note
  w_wide <- wiz_report(chloroplast_count ~ treatment, data = wk6)
  expect_false(grepl("ORDINAL", w_wide$txt, fixed = TRUE))
})

test_that("a two-level ordered factor stays binary", {
  d <- data.frame(g = factor(rep(c("a", "b"), each = 10)),
                  y = factor(rep(c("low", "high"), 10), levels = c("low", "high"), ordered = TRUE))
  w <- wiz_report(y ~ g, data = d)
  expect_identical(w$res$y_type, "binary")
})
