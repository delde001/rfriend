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
