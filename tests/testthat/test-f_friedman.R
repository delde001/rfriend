# =============================================================================
# test-f_friedman.R
# Comprehensive testthat tests for f_friedman()
#
# Run with: testthat::test_file("tests/testthat/test-f_friedman.R")
#           or devtools::test() from the package root
#
# API note
# --------
# Results are nested under the bare response name (one entry per response),
# consistent with the other rfriend test wrappers. For a single response use
# res[["score"]]; for multiple responses res[["y1"]], res[["y2"]], etc. When
# output_type = "rmd" the markdown string is stored in res[["rmd"]].
#
# f_friedman() expects a 'response ~ group | block' formula (unreplicated
# complete block design). It runs friedman.test(), reports Kendall's W
# (rstatix::friedman_effsize), and uses pairwise paired Wilcoxon signed-rank
# tests (rstatix::wilcox_test) as the post hoc, summarised with a compact
# letter display in the summary_table's "Letters" column.
#
# Shared fixtures and wrappers
# ----------------------------
#   * quiet_f_friedman()  -> helper-quiet.R (forces plot = FALSE)
# The make_rcb_*() builders below are file-local on purpose: the Friedman test
# needs an unreplicated complete block design (response ~ group | block), and
# no shared fixture in helper-data.R has that shape. quiet_f_friedman() forces
# plot = FALSE, so tests that need stored ggplots (plot = TRUE) call
# f_friedman() directly. The graphics device is opened once in setup.R.
#
# CRAN strategy
# -------------
# Every test is guarded with skip_on_cran() because the workflow pulls in
# rstatix (and therefore broom) plus ggplot2 rendering, mirroring the existing
# f_kruskal_test test file in this package.
#
# Test organisation (each section is independent):
#   1.  Input validation
#   2.  Output structure, single response
#   3.  plot toggle
#   4.  summary_table column ordering
#   5.  Multiple responses
#   6.  Statistical correctness vs base friedman.test()
#   7.  Effect size (Kendall's W)
#   8.  Post hoc: number of pairwise comparisons and CLD letters
#   9.  alpha / adjust handling
#  10.  Invalid-design handling (fail loud, skip response)
#  11.  rmd output and multiple-testing warning
#  12.  print and plot methods
# =============================================================================

# ---------------------------------------------------------------------------
# File-local fixtures: unreplicated complete block designs.
# ---------------------------------------------------------------------------
# A clean RCB design: n_subj blocks (subjects), each measured once under every
# condition. Means differ across conditions so the Friedman test is
# significant and the post hoc produces distinct CLD letters.
make_rcb_data <- function(seed = 1, n_subj = 12,
                          conditions = c("A", "B", "C"),
                          means = c(4, 6, 8)) {
  set.seed(seed)
  k <- length(conditions)
  do.call(rbind, lapply(seq_len(n_subj), function(s) {
    data.frame(
      subject   = factor(rep(s, k)),
      condition = factor(conditions, levels = conditions),
      score     = rnorm(k, mean = means, sd = 1)
    )
  }))
}

# A null design: same block structure but identical condition means, so the
# Friedman test should be non-significant and all CLD letters collapse to "ns".
make_rcb_null_data <- function(seed = 99, n_subj = 15,
                               conditions = c("A", "B", "C")) {
  set.seed(seed)
  k <- length(conditions)
  do.call(rbind, lapply(seq_len(n_subj), function(s) {
    data.frame(
      subject   = factor(rep(s, k)),
      condition = factor(conditions, levels = conditions),
      score     = rnorm(k, mean = 50, sd = 10)
    )
  }))
}

# =============================================================================
# 1. Input validation
# =============================================================================
test_that("f_friedman validation: rejects invalid alpha values", {
  skip_on_cran()
  df <- make_rcb_data()
  expect_error(quiet_f_friedman(score ~ condition | subject, data = df,
                                alpha = -1), "alpha")
  expect_error(quiet_f_friedman(score ~ condition | subject, data = df,
                                alpha = 0), "alpha")
  expect_error(quiet_f_friedman(score ~ condition | subject, data = df,
                                alpha = 1), "alpha")
  expect_error(quiet_f_friedman(score ~ condition | subject, data = df,
                                alpha = c(0.01, 0.05)), "alpha")
})

test_that("f_friedman validation: rejects invalid adjust method", {
  skip_on_cran()
  df <- make_rcb_data()
  expect_error(quiet_f_friedman(score ~ condition | subject, data = df,
                                adjust = "invalid"))
})

test_that("f_friedman validation: rejects invalid output_type", {
  skip_on_cran()
  df <- make_rcb_data()
  expect_error(quiet_f_friedman(score ~ condition | subject, data = df,
                                output_type = "html"))
})

test_that("f_friedman validation: requires the 'group | block' formula form", {
  skip_on_cran()
  df <- make_rcb_data()
  # No block term: must fail loudly explaining the required form.
  expect_error(quiet_f_friedman(score ~ condition, data = df),
               "group . block|response ~ group")
})

test_that("f_friedman validation: errors when variables are missing from data", {
  skip_on_cran()
  df <- make_rcb_data()
  expect_error(quiet_f_friedman(nope ~ condition | subject, data = df),
               "not found")
  expect_error(quiet_f_friedman(score ~ nope | subject, data = df),
               "not found")
  expect_error(quiet_f_friedman(score ~ condition | nope, data = df),
               "not found")
})

# =============================================================================
# 2. Output structure, single response
# =============================================================================
test_that("f_friedman structure: correct class and named entry", {
  skip_on_cran()
  df  <- make_rcb_data()
  res <- quiet_f_friedman(score ~ condition | subject, data = df)

  expect_s3_class(res, "f_friedman")
  expect_true("score" %in% names(res))

  entry <- res[["score"]]
  expect_true(all(c("friedman.test", "effect_size", "posthoc_test",
                    "summary_table", "alpha", "adjust") %in% names(entry)))
})

test_that("f_friedman structure: friedman.test is an htest object", {
  skip_on_cran()
  df    <- make_rcb_data()
  entry <- quiet_f_friedman(score ~ condition | subject, data = df)[["score"]]
  expect_s3_class(entry$friedman.test, "htest")
  expect_match(entry$friedman.test$method, "Friedman")
})

test_that("f_friedman structure: posthoc_test is a data frame with pairwise columns", {
  skip_on_cran()
  df    <- make_rcb_data()
  entry <- quiet_f_friedman(score ~ condition | subject, data = df)[["score"]]
  expect_true(is.data.frame(entry$posthoc_test))
  expect_true(all(c("group1", "group2", "p", "p.adj") %in%
                  names(entry$posthoc_test)))
})

test_that("f_friedman structure: summary_table has group, Letters and median", {
  skip_on_cran()
  df    <- make_rcb_data()
  entry <- quiet_f_friedman(score ~ condition | subject, data = df)[["score"]]
  expect_true(is.data.frame(entry$summary_table))
  expect_true(all(c("condition", "Letters", "n", "median") %in%
                  names(entry$summary_table)))
})

test_that("f_friedman structure: alpha and adjust stored correctly", {
  skip_on_cran()
  df    <- make_rcb_data()
  entry <- quiet_f_friedman(score ~ condition | subject, data = df)[["score"]]
  expect_equal(entry$alpha, 0.05)
  expect_equal(entry$adjust, "bonferroni")
})

# =============================================================================
# 3. plot toggle
# =============================================================================
test_that("f_friedman plot = FALSE: no ggplot objects stored", {
  skip_on_cran()
  df    <- make_rcb_data()
  # quiet_f_friedman() already forces plot = FALSE.
  entry <- quiet_f_friedman(score ~ condition | subject, data = df)[["score"]]
  expect_null(entry$distributions)
  expect_null(entry$Boxplot)
})

test_that("f_friedman plot = TRUE: density and boxplot ggplots are stored", {
  skip_on_cran()
  df <- make_rcb_data()
  # plot = TRUE cannot go through quiet_f_friedman() (it hardcodes plot = FALSE);
  # call f_friedman() directly. The suite-wide device from setup.R absorbs any
  # drawing; run_quiet_warn() suppresses the markdown and any chatter.
  entry <- run_quiet_warn(
    f_friedman(score ~ condition | subject, data = df,
               plot = TRUE, intro_text = FALSE, open_generated_files = FALSE)
  )[["score"]]
  expect_s3_class(entry$distributions, "ggplot")
  expect_s3_class(entry$Boxplot, "ggplot")
})

# =============================================================================
# 4. summary_table column ordering
# =============================================================================
test_that("f_friedman summary_table: Letters second, median before Q3, mean last", {
  skip_on_cran()
  df   <- make_rcb_data()
  cols <- names(quiet_f_friedman(score ~ condition | subject,
                                 data = df)[["score"]]$summary_table)

  expect_equal(cols[1], "condition")
  expect_equal(cols[2], "Letters")
  if (all(c("median", "Q3") %in% cols)) {
    expect_true(which(cols == "median") < which(cols == "Q3"))
  }
  standard_order <- c("condition", "Letters", "n", "min", "Q1",
                      "median", "Q3", "max", "mean")
  present <- standard_order[standard_order %in% cols]
  expect_equal(cols[seq_along(present)], present)
})

# =============================================================================
# 5. Multiple responses
# =============================================================================
test_that("f_friedman multiple responses: each response gets its own entry", {
  skip_on_cran()
  df <- make_rcb_data()
  # Add a second response on the same block/group structure.
  set.seed(7)
  df$score2 <- df$score + rnorm(nrow(df), 0, 1)

  res <- quiet_f_friedman(score + score2 ~ condition | subject, data = df)
  expect_true("score"  %in% names(res))
  expect_true("score2" %in% names(res))
  expect_s3_class(res[["score"]]$friedman.test,  "htest")
  expect_s3_class(res[["score2"]]$friedman.test, "htest")
})

# =============================================================================
# 6. Statistical correctness vs base friedman.test()
# =============================================================================
test_that("f_friedman matches base friedman.test() statistic and p-value", {
  skip_on_cran()
  df <- make_rcb_data()
  base_res <- friedman.test(score ~ condition | subject, data = df)
  pkg      <- quiet_f_friedman(score ~ condition | subject,
                               data = df)[["score"]]$friedman.test

  expect_equal(unname(pkg$statistic), unname(base_res$statistic),
               tolerance = 1e-8)
  expect_equal(pkg$p.value, base_res$p.value, tolerance = 1e-8)
  expect_equal(unname(pkg$parameter), unname(base_res$parameter))
})

# =============================================================================
# 7. Effect size (Kendall's W)
# =============================================================================
test_that("f_friedman effect_size: Kendall's W is present and in [0, 1]", {
  skip_on_cran()
  df  <- make_rcb_data()
  eff <- quiet_f_friedman(score ~ condition | subject,
                          data = df)[["score"]]$effect_size
  expect_true(is.data.frame(eff))
  expect_true("effsize" %in% names(eff))
  w <- eff$effsize[1]
  expect_true(w >= 0 && w <= 1)
})

# =============================================================================
# 8. Post hoc: number of comparisons and CLD letters
# =============================================================================
test_that("f_friedman post hoc: 3 conditions give C(3,2) = 3 comparisons", {
  skip_on_cran()
  df <- make_rcb_data()
  ph <- quiet_f_friedman(score ~ condition | subject,
                         data = df)[["score"]]$posthoc_test
  expect_equal(nrow(ph), 3L)
})

test_that("f_friedman post hoc: 4 conditions give C(4,2) = 6 comparisons", {
  skip_on_cran()
  df <- make_rcb_data(conditions = c("A", "B", "C", "D"),
                      means = c(3, 6, 9, 12), n_subj = 14)
  ph <- quiet_f_friedman(score ~ condition | subject,
                         data = df)[["score"]]$posthoc_test
  expect_equal(nrow(ph), 6L)
})

test_that("f_friedman post hoc: significant test yields more than one CLD letter", {
  skip_on_cran()
  df  <- make_rcb_data()
  ent <- quiet_f_friedman(score ~ condition | subject, data = df)[["score"]]
  if (ent$friedman.test$p.value < 0.05) {
    letters_seen <- ent$summary_table$Letters
    expect_true(length(unique(letters_seen)) > 1)
    expect_false(any(letters_seen == "ns"))
  }
})

test_that("f_friedman post hoc: non-significant test collapses letters to 'ns'", {
  skip_on_cran()
  df  <- make_rcb_null_data()
  ent <- quiet_f_friedman(score ~ condition | subject, data = df)[["score"]]
  if (ent$friedman.test$p.value >= 0.05) {
    expect_true(all(ent$summary_table$Letters == "ns"))
  }
})

# =============================================================================
# 9. alpha / adjust handling
# =============================================================================
test_that("f_friedman alpha: custom alpha is stored", {
  skip_on_cran()
  df <- make_rcb_data()
  ent <- quiet_f_friedman(score ~ condition | subject, data = df,
                          alpha = 0.01)[["score"]]
  expect_equal(ent$alpha, 0.01)
})

test_that("f_friedman adjust: holm is accepted and stored", {
  skip_on_cran()
  df <- make_rcb_data()
  ent <- quiet_f_friedman(score ~ condition | subject, data = df,
                          adjust = "holm")[["score"]]
  expect_equal(ent$adjust, "holm")
})

test_that("f_friedman adjust: 'BH'/'bh' case folding is accepted", {
  skip_on_cran()
  df <- make_rcb_data()
  expect_no_error(
    quiet_f_friedman(score ~ condition | subject, data = df, adjust = "BH")
  )
})

# =============================================================================
# 10. Invalid-design handling (fail loud, skip response)
# =============================================================================
test_that("f_friedman design: replicated cells cause the response to be skipped", {
  skip_on_cran()
  # Two observations for one subject x condition cell -> replicated design.
  df  <- make_rcb_data(n_subj = 8)
  dup <- df[1, , drop = FALSE]            # duplicate a subject1/condA cell
  df_bad <- rbind(df, dup)

  res <- quiet_f_friedman(score ~ condition | subject, data = df_bad)
  # Function should not error; the response is flagged skipped instead.
  expect_s3_class(res, "f_friedman")
  expect_true(isTRUE(res[["score"]]$skipped))
})

test_that("f_friedman design: fewer than 3 groups skips the response", {
  skip_on_cran()
  df2 <- make_rcb_data(conditions = c("A", "B"), means = c(4, 6), n_subj = 10)
  res <- quiet_f_friedman(score ~ condition | subject, data = df2)
  expect_s3_class(res, "f_friedman")
  expect_true(isTRUE(res[["score"]]$skipped))
})

# =============================================================================
# 11. rmd output and multiple-testing warning
# =============================================================================
# rmd does not print to console, so f_friedman() is called directly with the
# non-default output_type rather than through quiet_f_friedman().
test_that("f_friedman rmd: stores a character markdown string under res$rmd", {
  skip_on_cran()
  df  <- make_rcb_data()
  res <- f_friedman(score ~ condition | subject, data = df,
                    plot = FALSE, output_type = "rmd",
                    open_generated_files = FALSE)
  expect_true("rmd" %in% names(res))
  expect_type(res[["rmd"]], "character")
  expect_gt(nchar(res[["rmd"]]), 100L)
})

test_that("f_friedman rmd: multiple-testing warning present for k > 1, absent for k = 1", {
  skip_on_cran()
  df <- make_rcb_data()
  set.seed(8)
  df$score2 <- df$score + rnorm(nrow(df), 0, 1)

  res_multi <- f_friedman(score + score2 ~ condition | subject, data = df,
                          plot = FALSE, output_type = "rmd",
                          open_generated_files = FALSE)
  res_one   <- f_friedman(score ~ condition | subject, data = df,
                          plot = FALSE, output_type = "rmd",
                          open_generated_files = FALSE)

  expect_true(grepl("Multiple Testing", res_multi[["rmd"]]))
  expect_false(grepl("Multiple Testing", res_one[["rmd"]]))
})

test_that("f_friedman intro_text = FALSE runs and omits the assumptions section", {
  skip_on_cran()
  df  <- make_rcb_data()
  res <- f_friedman(score ~ condition | subject, data = df,
                    plot = FALSE, intro_text = FALSE, output_type = "rmd",
                    open_generated_files = FALSE)
  expect_false(grepl("Assumptions of the Friedman Test", res[["rmd"]]))
})

# =============================================================================
# 12. print and plot methods
# =============================================================================
test_that("f_friedman print method runs and mentions Friedman", {
  skip_on_cran()
  df  <- make_rcb_data()
  res <- quiet_f_friedman(score ~ condition | subject, data = df)
  out <- capture.output(suppressWarnings(print(res)))
  expect_true(length(out) > 0)
  expect_true(any(grepl("Friedman", out, ignore.case = TRUE)))
})

test_that("f_friedman plot method runs without error", {
  skip_on_cran()
  df  <- make_rcb_data()
  # Need plot = TRUE so there is something to re-print; call f_friedman directly.
  res <- run_quiet_warn(
    f_friedman(score ~ condition | subject, data = df,
               plot = TRUE, intro_text = FALSE, open_generated_files = FALSE)
  )
  # The suite-wide PDF device from setup.R receives the plots.
  expect_no_error(suppressWarnings(plot(res)))
})
