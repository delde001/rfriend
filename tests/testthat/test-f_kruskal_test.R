# test-f_kruskal_test.R
# Comprehensive stress tests for f_kruskal_test()
# Uses testthat framework
# ============================================================
# 1. INPUT VALIDATION
# ============================================================

test_that("rejects invalid alpha values", {
  skip_on_cran()
  expect_error(f_kruskal_test(Sepal.Width ~ Species, data = iris, alpha = -1,          plot = FALSE), "alpha")
  expect_error(f_kruskal_test(Sepal.Width ~ Species, data = iris, alpha = 0,           plot = FALSE), "alpha")
  expect_error(f_kruskal_test(Sepal.Width ~ Species, data = iris, alpha = 1,           plot = FALSE), "alpha")
  expect_error(f_kruskal_test(Sepal.Width ~ Species, data = iris, alpha = 1.5,         plot = FALSE), "alpha")
  expect_error(f_kruskal_test(Sepal.Width ~ Species, data = iris, alpha = "high",      plot = FALSE), "alpha")
  expect_error(f_kruskal_test(Sepal.Width ~ Species, data = iris, alpha = c(0.01, 0.05), plot = FALSE), "alpha")
})

test_that("rejects invalid adjust method", {
  skip_on_cran()
  expect_error(f_kruskal_test(Sepal.Width ~ Species, data = iris, adjust = "invalid",    plot = FALSE))
  expect_error(f_kruskal_test(Sepal.Width ~ Species, data = iris, adjust = "bonfferoni", plot = FALSE))
})

test_that("rejects invalid output_type", {
  skip_on_cran()
  expect_error(f_kruskal_test(Sepal.Width ~ Species, data = iris, output_type = "html", plot = FALSE))
  expect_error(f_kruskal_test(Sepal.Width ~ Species, data = iris, output_type = "pptx", plot = FALSE))
})

test_that("errors when variables are missing from data", {
  skip_on_cran()
  expect_error(f_kruskal_test(Nonexistent ~ Species,    data = iris, plot = FALSE), "not found")
  expect_error(f_kruskal_test(Sepal.Width ~ Nonexistent, data = iris, plot = FALSE), "not found")
})


# ============================================================
# 2. OUTPUT STRUCTURE - SINGLE RESPONSE, SINGLE PREDICTOR
# ============================================================

test_that("returns correct class and named structure", {
  skip_on_cran()
  res <- f_kruskal_test(Sepal.Width ~ Species, data = iris, plot = FALSE)

  expect_s3_class(res, "f_kruskal_test")
  expect_true("Sepal.Width_Species" %in% names(res))

  entry <- res[["Sepal.Width_Species"]]

  # All expected components present
  expect_true(all(c("kruskal.test", "dunn_test", "summary_table",
                     "alpha", "DunnTest_adjust", "adjust") %in% names(entry)))

  # kruskal.test is an htest object
  expect_s3_class(entry$kruskal.test, "htest")
  expect_equal(entry$kruskal.test$method, "Kruskal-Wallis rank sum test")

  # dunn_test is a data frame with expected columns
  expect_true(is.data.frame(entry$dunn_test))
  expect_true(all(c("group1", "group2", "statistic", "p", "p.adj") %in%
                     names(entry$dunn_test)))

  # summary_table is a data frame with expected columns
  expect_true(is.data.frame(entry$summary_table))
  expect_true(all(c("Species", "Letters", "n", "median", "mean") %in%
                     names(entry$summary_table)))

  # alpha and adjust stored correctly
  expect_equal(entry$alpha, 0.05)
  expect_equal(entry$DunnTest_adjust, "bonferroni")
  expect_equal(entry$adjust, "bonferroni")
})


# ============================================================
# 3. PLOT TOGGLE
# ============================================================

test_that("plot = FALSE: no ggplot objects stored", {
  skip_on_cran()
  res <- f_kruskal_test(Sepal.Width ~ Species, data = iris, plot = FALSE)
  entry <- res[["Sepal.Width_Species"]]

  expect_null(entry$distributions)
  expect_null(entry$Boxplot)
})

test_that("plot = TRUE: ggplot objects are stored", {
  skip_on_cran()
  res <- f_kruskal_test(Sepal.Width ~ Species, data = iris, plot = TRUE)
  entry <- res[["Sepal.Width_Species"]]

  expect_s3_class(entry$distributions, "ggplot")
  expect_s3_class(entry$Boxplot, "ggplot")
})


# ============================================================
# 4. COLUMN ORDERING OF SUMMARY TABLE
# ============================================================

test_that("summary_table: Letters 2nd, median-centric layout, mean last", {
  skip_on_cran()
  res  <- f_kruskal_test(Sepal.Width ~ Species, data = iris, plot = FALSE)
  cols <- names(res[["Sepal.Width_Species"]]$summary_table)

  # First column is the predictor

  expect_equal(cols[1], "Species")

  # Second column is Letters
  expect_equal(cols[2], "Letters")

  # median comes before Q3
  expect_true(which(cols == "median") < which(cols == "Q3"))

  # mean is the last of the standard columns
  standard_order <- c("Species", "Letters", "n", "min", "Q1",
                       "median", "Q3", "max", "mean")
  present <- standard_order[standard_order %in% cols]
  expect_equal(cols[seq_along(present)], present)
})


# ============================================================
# 5. MULTIPLE RESPONSES AND/OR PREDICTORS
# ============================================================

test_that("multiple responses: each gets its own entry", {
  skip_on_cran()
  res <- f_kruskal_test(Sepal.Width + Sepal.Length ~ Species,
                         data = iris, plot = FALSE)

  expect_true("Sepal.Width_Species"  %in% names(res))
  expect_true("Sepal.Length_Species" %in% names(res))
})

test_that("formula with minus separator on LHS works", {
  skip_on_cran()
  res <- f_kruskal_test(Sepal.Width - Sepal.Length ~ Species,
                         data = iris, plot = FALSE)

  expect_true("Sepal.Width_Species"  %in% names(res))
  expect_true("Sepal.Length_Species" %in% names(res))
})

test_that("multiple predictors: all response x predictor combinations", {
  skip_on_cran()
  df <- data.frame(
    y1 = rnorm(60), y2 = rnorm(60, 5),
    grpA = factor(rep(c("a1", "a2", "a3"), each = 20)),
    grpB = factor(rep(c("b1", "b2"), times = 30))
  )

  res <- f_kruskal_test(y1 + y2 ~ grpA + grpB, data = df, plot = FALSE)

  expected <- c("y1_grpA", "y1_grpB", "y2_grpA", "y2_grpB")
  expect_true(all(expected %in% names(res)))
  # Only these 4 entries (no extras)
  expect_equal(length(res), length(expected))
})


# ============================================================
# 6. STATISTICAL CORRECTNESS
# ============================================================

test_that("KW statistic and p-value match base R kruskal.test()", {
  skip_on_cran()
  base_res <- kruskal.test(Sepal.Width ~ Species, data = iris)
  pkg_res  <- f_kruskal_test(Sepal.Width ~ Species, data = iris, plot = FALSE)
  stored   <- pkg_res[["Sepal.Width_Species"]]$kruskal.test

  expect_equal(stored$statistic, base_res$statistic)
  expect_equal(stored$p.value,   base_res$p.value)
  expect_equal(stored$parameter, base_res$parameter)
})

test_that("significant KW: letters differ across groups", {
  skip_on_cran()
  # iris Sepal.Width ~ Species is highly significant (p << 0.001)
  res     <- f_kruskal_test(Sepal.Width ~ Species, data = iris, plot = FALSE)
  letters <- res[["Sepal.Width_Species"]]$summary_table$Letters

  expect_true(length(unique(letters)) > 1)
  expect_false(any(letters == "ns"))
})

test_that("non-significant KW: all letters are 'ns'", {
  skip_on_cran()
  set.seed(42)
  ns_df <- data.frame(
    y   = rnorm(150, mean = 50, sd = 10),
    grp = factor(rep(c("A", "B", "C"), each = 50))
  )

  res   <- f_kruskal_test(y ~ grp, data = ns_df, plot = FALSE)
  entry <- res[["y_grp"]]

  # Guard: only check letters if KW was indeed non-significant
  # (with this seed and n = 150, it should be, but we guard against rare flukes)
  if (entry$kruskal.test$p.value >= 0.05) {
    expect_true(all(entry$summary_table$Letters == "ns"))
  }
})

test_that("custom alpha is stored and propagated", {
  skip_on_cran()
  res_strict <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                                alpha = 0.001, plot = FALSE)
  res_lax    <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                                alpha = 0.10,  plot = FALSE)

  expect_equal(res_strict[["Sepal.Width_Species"]]$alpha, 0.001)
  expect_equal(res_lax[["Sepal.Width_Species"]]$alpha,    0.10)
})

test_that("Dunn's test has correct number of pairwise comparisons", {
  skip_on_cran()
  # 3 groups -> C(3,2) = 3
  res3 <- f_kruskal_test(Sepal.Width ~ Species, data = iris, plot = FALSE)
  expect_equal(nrow(res3[["Sepal.Width_Species"]]$dunn_test), 3)

  # 6 groups -> C(6,2) = 15
  set.seed(7)
  df6 <- data.frame(
    val   = rnorm(120),
    group = factor(rep(LETTERS[1:6], each = 20))
  )
  res6 <- f_kruskal_test(val ~ group, data = df6, plot = FALSE)
  expect_equal(nrow(res6[["val_group"]]$dunn_test), 15)
})

test_that("Dunn's test: adjust method is passed to rstatix", {
  skip_on_cran()
  res_holm <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                              adjust = "holm", plot = FALSE)
  res_none <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                              adjust = "none", plot = FALSE)

  dunn_holm <- res_holm[["Sepal.Width_Species"]]$dunn_test
  dunn_none <- res_none[["Sepal.Width_Species"]]$dunn_test

  # Raw p-values should be identical (same z-statistics)
  expect_equal(dunn_holm$p, dunn_none$p)

  # "none" means adjusted == raw
  expect_equal(dunn_none$p.adj, dunn_none$p)

  # Holm-adjusted p >= raw p (with floating-point tolerance)
  expect_true(all(dunn_holm$p.adj >= dunn_holm$p - 1e-10))
})


# ============================================================
# 7. ALL VALID ADJUST METHODS
# ============================================================

test_that("all adjust methods run without error", {
  skip_on_cran()
  methods <- c("holm", "hommel", "bonferroni",
               "hochberg", "bh", "by", "fdr", "none")

  for (m in methods) {
    res <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                           adjust = m, plot = FALSE)
    expect_true(inherits(res, "f_kruskal_test"),
                info = paste("adjust =", m))
    expected_stored <- switch(m, "bh" = "BH", "by" = "BY", m)
    expect_equal(res[["Sepal.Width_Species"]]$DunnTest_adjust, expected_stored,
                 info = paste("stored adjust should match for method:", m))
  }
})


# ============================================================
# 8. EDGE CASES
# ============================================================

test_that("two-group data works (KW with k = 2)", {
  skip_on_cran()
  set.seed(11)
  df2 <- data.frame(
    score = c(rnorm(30, 5), rnorm(30, 8)),
    grp   = factor(rep(c("X", "Y"), each = 30))
  )

  res   <- f_kruskal_test(score ~ grp, data = df2, plot = FALSE)
  entry <- res[["score_grp"]]

  expect_s3_class(entry$kruskal.test, "htest")
  expect_equal(unname(entry$kruskal.test$parameter), 1)   # df = k - 1 = 1
  expect_equal(nrow(entry$summary_table), 2)
  # Dunn's test: C(2,2) = 1 pairwise comparison
  expect_equal(nrow(entry$dunn_test), 1)
})

test_that("integer/ordinal data with many ties works", {
  skip_on_cran()
  set.seed(99)
  df_tie <- data.frame(
    rating    = c(sample(1:5, 50, replace = TRUE, prob = c(.40, .30, .20, .08, .02)),
                  sample(1:5, 50, replace = TRUE, prob = c(.05, .10, .25, .30, .30)),
                  sample(1:5, 50, replace = TRUE, prob = c(.10, .15, .25, .25, .25))),
    condition = factor(rep(c("low", "med", "high"), each = 50))
  )

  res <- f_kruskal_test(rating ~ condition, data = df_tie, plot = FALSE)
  expect_s3_class(res, "f_kruskal_test")
  expect_true(is.data.frame(res[["rating_condition"]]$summary_table))
  expect_equal(nrow(res[["rating_condition"]]$summary_table), 3)
})

test_that("data with NAs: runs without error, structure intact", {
  skip_on_cran()
  na_df <- iris[, c("Sepal.Width", "Species")]
  na_df$Sepal.Width[c(1, 25, 75, 130)] <- NA

  res <- f_kruskal_test(Sepal.Width ~ Species, data = na_df, plot = FALSE)

  expect_s3_class(res, "f_kruskal_test")
  expect_true(is.data.frame(res[["Sepal.Width_Species"]]$summary_table))
  expect_s3_class(res[["Sepal.Width_Species"]]$kruskal.test, "htest")
})

test_that("many groups (k = 6) work", {
  skip_on_cran()
  set.seed(7)
  df6 <- data.frame(
    val   = rnorm(120),
    group = factor(rep(LETTERS[1:6], each = 20))
  )

  res   <- f_kruskal_test(val ~ group, data = df6, plot = FALSE)
  entry <- res[["val_group"]]

  expect_equal(nrow(entry$summary_table), 6)
  expect_equal(unname(entry$kruskal.test$parameter), 5)  # df = 6 - 1
})

test_that("numeric predictor is auto-converted to factor", {
  skip_on_cran()
  df_num <- data.frame(
    y = rnorm(60),
    x = rep(c(1, 2, 3), each = 20)
  )

  # x is numeric, should be silently coerced to factor
  res <- f_kruskal_test(y ~ x, data = df_num, plot = FALSE)
  expect_s3_class(res, "f_kruskal_test")
  expect_equal(nrow(res[["y_x"]]$summary_table), 3)
})

test_that("character predictor works (coerced to factor)", {
  skip_on_cran()
  df_chr <- data.frame(
    y = rnorm(60),
    g = rep(c("alpha", "beta", "gamma"), each = 20),
    stringsAsFactors = FALSE
  )

  res <- f_kruskal_test(y ~ g, data = df_chr, plot = FALSE)
  expect_s3_class(res, "f_kruskal_test")
  expect_equal(nrow(res[["y_g"]]$summary_table), 3)
})


# ============================================================
# 9. OUTPUT TYPES
# ============================================================

test_that("output_type = 'default': returns list silently", {
  skip_on_cran()
  res <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                         plot = FALSE, output_type = "default")
  expect_s3_class(res, "f_kruskal_test")
})

test_that("output_type = 'console': prints and returns invisibly", {
  skip_on_cran()
  out <- capture.output(
    res <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                           plot = FALSE, output_type = "console")
  )
  expect_s3_class(res, "f_kruskal_test")
  expect_true(length(out) > 0)
})

test_that("output_type = 'rmd': includes rmd character element", {
  skip_on_cran()
  res <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                         plot = FALSE, output_type = "rmd")

  expect_true("rmd" %in% names(res))
  expect_true(is.character(res$rmd))
  expect_true(nchar(res$rmd) > 100)
})

test_that("intro_text controls assumptions section in rmd output", {
  skip_on_cran()
  res_with <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                              plot = FALSE, output_type = "rmd", intro_text = TRUE)
  res_without <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                                 plot = FALSE, output_type = "rmd", intro_text = FALSE)

  expect_true(grepl("# Assumptions of the Kruskal-Wallis Test",  res_with$rmd))
  expect_false(grepl("# Assumptions of the Kruskal-Wallis Test", res_without$rmd))

})


# ============================================================
# 10. PRINT METHOD
# ============================================================

test_that("print method runs without error", {
  skip_on_cran()
  res <- f_kruskal_test(Sepal.Width ~ Species, data = iris, plot = FALSE)
  out <- capture.output(print(res))

  expect_true(length(out) > 0)
  expect_true(any(grepl("Kruskal", out, ignore.case = TRUE)))
})

test_that("print method uses stored alpha, not hardcoded 0.05", {
  skip_on_cran()
  res <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                         alpha = 0.10, plot = FALSE)
  out <- capture.output(print(res))

  # The stored alpha 0.1 should appear in the printed output
  expect_true(any(grepl("0.1", out)))
})

test_that("print method handles non-significant result gracefully", {
  skip_on_cran()
  set.seed(42)
  ns_df <- data.frame(
    y   = rnorm(150, 50, 10),
    grp = factor(rep(c("A", "B", "C"), each = 50))
  )

  res <- f_kruskal_test(y ~ grp, data = ns_df, plot = FALSE)
  # Should not error regardless of significance
  out <- capture.output(print(res))
  expect_true(length(out) > 0)
})


# ============================================================
# 11. INTRO TEXT AND MULTIPLE-TESTING WARNING
# ============================================================

test_that("intro_text = FALSE does not cause errors", {
  skip_on_cran()
  res <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                         plot = FALSE, intro_text = FALSE)
  expect_s3_class(res, "f_kruskal_test")
})

test_that("multiple-testing warning appears in rmd when k > 1", {
  skip_on_cran()
  res <- f_kruskal_test(Sepal.Width + Sepal.Length ~ Species,
                         data = iris, plot = FALSE, output_type = "rmd")

  expect_true(grepl("Multiple Testing", res$rmd))
})

test_that("multiple-testing warning absent in rmd when k = 1", {
  skip_on_cran()
  res <- f_kruskal_test(Sepal.Width ~ Species,
                         data = iris, plot = FALSE, output_type = "rmd")

  expect_false(grepl("Multiple Testing", res$rmd))
})


# ============================================================
# 12. STRESS: COMBINE MANY OPTIONS AT ONCE
# ============================================================

test_that("stress: multiple responses, custom alpha, holm adjust, no plots, rmd output", {
  skip_on_cran()
  res <- f_kruskal_test(
    Sepal.Width + Sepal.Length + Petal.Length + Petal.Width ~ Species,
    data   = iris,
    alpha  = 0.01,
    adjust = "holm",
    plot   = FALSE,
    intro_text = FALSE,
    output_type = "rmd"
  )

  # 4 responses x 1 predictor = 4 entries + 1 rmd element
  expect_equal(length(res), 5)
  expect_true("rmd" %in% names(res))

  # All 4 response entries present
  for (resp in c("Sepal.Width", "Sepal.Length", "Petal.Length", "Petal.Width")) {
    key <- paste0(resp, "_Species")
    expect_true(key %in% names(res), info = paste("missing:", key))
    expect_equal(res[[key]]$alpha, 0.01, info = key)
    expect_equal(res[[key]]$DunnTest_adjust, "holm", info = key)
  }

  # rmd should contain the multiple-testing warning (k = 4)
  expect_true(grepl("Multiple Testing", res$rmd))
  expect_true(grepl("4.*Kruskal-Wallis Tests", res$rmd))  # "4 Kruskal-Wallis Tests"
})

test_that("stress: 2 responses x 2 predictors, fdr adjust, plots, default output", {
  skip_on_cran()
  df <- data.frame(
    y1 = rnorm(90), y2 = rnorm(90, 10),
    f1 = factor(rep(c("lo", "mid", "hi"), each = 30)),
    f2 = factor(rep(c("ctrl", "trt"), times = 45))
  )

  res <- f_kruskal_test(
    y1 + y2 ~ f1 + f2,
    data   = df,
    alpha  = 0.05,
    adjust = "fdr",
    plot   = TRUE,
    output_type = "default"
  )

  expected <- c("y1_f1", "y1_f2", "y2_f1", "y2_f2")
  expect_true(all(expected %in% names(res)))

  # Each entry should have ggplot objects (plot = TRUE)
  for (key in expected) {
    expect_true(inherits(res[[key]]$distributions, "ggplot"), info = key)
    expect_true(inherits(res[[key]]$Boxplot,       "ggplot"), info = key)
    expect_equal(res[[key]]$DunnTest_adjust,  "fdr",    info = key)
  }
})

test_that("stress: large dataset (1000 obs x 10 groups)", {
  skip_on_cran()
  set.seed(321)
  big_df <- data.frame(
    value = c(rnorm(500, 10, 3), rnorm(500, 12, 3)),
    group = factor(rep(paste0("G", sprintf("%02d", 1:10)), each = 100))
  )

  res <- f_kruskal_test(value ~ group, data = big_df, plot = FALSE)

  expect_s3_class(res, "f_kruskal_test")
  entry <- res[["value_group"]]

  expect_equal(nrow(entry$summary_table), 10)
  expect_equal(unname(entry$kruskal.test$parameter), 9)  # df = 10 - 1
  # C(10,2) = 45 pairwise comparisons
  expect_equal(nrow(entry$dunn_test), 45)
})

# =============================================================================
# f_kruskal_test_new_tests.R
# Additional tests for f_kruskal_test() covering NEW behavior:
#
#   subset / na.action pass-through via stats::model.frame(), so the
#   descriptive summary, density plot, boxplot, Dunn's test and the
#   KW test itself all see the exact same row set.
# =============================================================================
# ---------------------------------------------------------------------------
# A. subset pass-through
# ---------------------------------------------------------------------------

test_that("subset drops rows before kruskal.test is called", {
  skip_on_cran()
  # iris has 50 rows per Species. Subsetting away one Species should
  # leave 100 rows in the model and only two groups in the summary.
  res <- suppressMessages(
    f_kruskal_test(Sepal.Width ~ Species, data = iris,
                   subset = iris$Species != "setosa",
                   plot = FALSE, output_type = "default")
  )

  entry <- res[[1]]
  # Summary table reflects the subset (2 Species only)
  expect_equal(nrow(entry$summary_table), 2L)
  expect_false("setosa" %in% as.character(entry$summary_table$Species))

  # The n column should sum to 100 (50 versicolor + 50 virginica)
  expect_equal(sum(entry$summary_table$n), 100L)
})

test_that("subset flows through to Dunn's test (same row set)", {
  skip_on_cran()
  res <- suppressMessages(
    f_kruskal_test(Sepal.Width ~ Species, data = iris,
                   subset = iris$Species != "virginica",
                   plot = FALSE, output_type = "default")
  )

  dunn <- res[[1]]$dunn_test
  # With only two Species left, Dunn's test has exactly 1 comparison
  expect_equal(nrow(dunn), 1L)
  compared <- c(dunn$group1, dunn$group2)
  expect_false("virginica" %in% compared)
})

# ---------------------------------------------------------------------------
# B. na.action pass-through
# ---------------------------------------------------------------------------

test_that("na.action = na.omit strips NA rows consistently", {
  skip_on_cran()
  df <- iris
  df$Sepal.Width[c(1, 2, 3, 60, 120)] <- NA    # 5 NAs across groups

  res <- suppressMessages(
    f_kruskal_test(Sepal.Width ~ Species, data = df,
                   na.action = na.omit,
                   plot = FALSE, output_type = "default")
  )

  entry <- res[[1]]
  # Total n after NA drop
  expect_equal(sum(entry$summary_table$n), 145L)   # 150 - 5
})

test_that("default behavior (no dots) still works and uses full data", {
  skip_on_cran()
  # Regression guard: default path with no subset / na.action should
  # fit on all 150 iris rows exactly as before.
  res <- suppressMessages(
    f_kruskal_test(Sepal.Width ~ Species, data = iris,
                   plot = FALSE, output_type = "default")
  )
  entry <- res[[1]]
  expect_equal(sum(entry$summary_table$n), 150L)
  expect_equal(nrow(entry$summary_table), 3L)
})
