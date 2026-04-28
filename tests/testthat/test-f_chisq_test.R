# =============================================================================
# test-f_chisq_test.R
# =============================================================================
# Tests for f_chisq_test(). Uses inline contingency tables; no shared
# fixtures required.
# =============================================================================


# =============================================================================
# SECTION 1: Basic return structure
# =============================================================================

test_that("contingency table input: returns correct S3 class", {
  tbl <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  dimnames(tbl) <- list(Gender = c("Male", "Female"),
                        Response = c("Agree", "Neutral", "Disagree"))
  result <- f_chisq_test(tbl)
  expect_s3_class(result, "f_chisq_test")
})

test_that("contingency table input: chisq_test_output is htest object", {
  tbl <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  result <- f_chisq_test(tbl)
  expect_s3_class(result$chisq_test_output, "htest")
})

test_that("contingency table input: settings are stored correctly", {
  tbl <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  result <- f_chisq_test(tbl, method = "holm", digits = 4, alpha = 0.01)
  expect_equal(result$settings$method, "holm")
  expect_equal(result$settings$digits, 4)
  expect_equal(result$settings$alpha, 0.01)
  expect_false(result$settings$force_posthoc)
})

# =============================================================================
# SECTION 2: Contingency table - significant result with post hoc
# =============================================================================

test_that("significant table: adjusted_p_values matrix is returned", {
  tbl <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  result <- f_chisq_test(tbl)
  expect_true(!is.null(result$adjusted_p_values))
  expect_equal(dim(result$adjusted_p_values), dim(tbl))
})

test_that("significant table: observed_vs_adj_p_value has correct row count (2x nrow)", {
  tbl <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  result <- f_chisq_test(tbl)
  expect_equal(nrow(result$observed_vs_adj_p_value), 2 * nrow(tbl))
})

test_that("significant table: stdres_vs_adj_p_value has correct row count (2x nrow)", {
  tbl <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  result <- f_chisq_test(tbl)
  expect_equal(nrow(result$stdres_vs_adj_p_value), 2 * nrow(tbl))
})

test_that("significant table: spaced_observed_vs_adj_p_value has correct row count (3x nrow)", {
  tbl <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  result <- f_chisq_test(tbl)
  expect_equal(nrow(result$spaced_observed_vs_adj_p_value), 3 * nrow(tbl))
})

test_that("significant table: column count of post hoc matrices equals ncol of input", {
  tbl <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  result <- f_chisq_test(tbl)
  expect_equal(ncol(result$observed_vs_adj_p_value), ncol(tbl))
  expect_equal(ncol(result$stdres_vs_adj_p_value),   ncol(tbl))
})

# =============================================================================
# SECTION 3: Goodness-of-fit test (vector + p)
# =============================================================================

test_that("GOF: returns correct class with p argument", {
  observed      <- c(2, 2, 10, 20, 15, 11)
  expected_prob <- rep(1/6, 6)
  result <- f_chisq_test(x = observed, p = expected_prob)
  expect_s3_class(result, "f_chisq_test")
})

test_that("GOF: p is stored at top level (x$p), not in settings (BUG 1 check)", {
  observed      <- c(2, 2, 10, 20, 15, 11)
  expected_prob <- rep(1/6, 6)
  result <- f_chisq_test(x = observed, p = expected_prob)
  expect_equal(result$p, expected_prob)
})

test_that("GOF significant: posthoc_output_table is returned when significant", {
  observed      <- c(2, 2, 10, 20, 15, 11)
  expected_prob <- rep(1/6, 6)
  result <- f_chisq_test(x = observed, p = expected_prob)
  if (result$chisq_test_output$p.value < result$settings$alpha) {
    expect_true(!is.null(result$posthoc_output_table))
    expect_s3_class(result$posthoc_output_table, "data.frame")
    expect_true(all(c("Observed", "Expected", "Std.Residuals", "p-value") %in%
                      names(result$posthoc_output_table)))
  }
})

test_that("GOF: posthoc_output_table has same length as observed", {
  observed      <- c(2, 2, 10, 20, 15, 11)
  expected_prob <- rep(1/6, 6)
  result <- f_chisq_test(x = observed, p = expected_prob, force_posthoc = TRUE)
  expect_equal(nrow(result$posthoc_output_table), length(observed))
})

test_that("GOF: adj_p_values length equals length of input vector", {
  observed      <- c(2, 2, 10, 20, 15, 11)
  expected_prob <- rep(1/6, 6)
  result <- f_chisq_test(x = observed, p = expected_prob, force_posthoc = TRUE)
  expect_equal(length(result$adj_p_values), length(observed))
})

# =============================================================================
# SECTION 4: Two-vector input with y
# =============================================================================

test_that("two-vector input: y argument is not silently dropped (BUG 2 check)", {
  set.seed(42)
  x_vec <- sample(c("A", "B", "C"), 100, replace = TRUE)
  y_vec <- sample(c("X", "Y"),      100, replace = TRUE)

  result_f   <- f_chisq_test(x_vec, y = y_vec)
  result_ref <- chisq.test(x_vec, y = y_vec)

  expect_equal(result_f$chisq_test_output$statistic,
               result_ref$statistic,
               tolerance = 1e-6)
  expect_equal(result_f$chisq_test_output$parameter,
               result_ref$parameter)
})

# =============================================================================
# SECTION 5: Non-significant result - no post hoc
# =============================================================================

test_that("non-significant table: post hoc items are absent from output", {
  tbl_ns <- as.table(rbind(c(50, 50), c(50, 50)))
  result <- f_chisq_test(tbl_ns, alpha = 0.05)
  expect_true(result$chisq_test_output$p.value >= result$settings$alpha)
  expect_null(result$adjusted_p_values)
  expect_null(result$observed_vs_adj_p_value)
  expect_null(result$stdres_vs_adj_p_value)
})

test_that("non-significant: force_posthoc = TRUE overrides and returns post hoc", {
  tbl_ns <- as.table(rbind(c(50, 50), c(50, 50)))
  result  <- f_chisq_test(tbl_ns, alpha = 0.05, force_posthoc = TRUE)
  expect_true(!is.null(result$adjusted_p_values))
  expect_true(!is.null(result$observed_vs_adj_p_value))
  expect_true(result$settings$force_posthoc)
})

# =============================================================================
# SECTION 6: Correction method combinations
# =============================================================================

test_that("all p.adjust methods run without error on contingency table", {
  tbl <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  methods <- c("bonferroni", "holm", "hochberg", "hommel", "BH", "BY", "fdr", "none")
  for (m in methods) {
    expect_no_error(f_chisq_test(tbl, method = m))
  }
})

test_that("bonferroni p-values are >= unadjusted p-values for contingency table", {
  tbl <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  result_bon  <- f_chisq_test(tbl, method = "bonferroni", force_posthoc = TRUE)
  result_none <- f_chisq_test(tbl, method = "none",       force_posthoc = TRUE)

  adj_bon  <- as.numeric(result_bon$adjusted_p_values)
  adj_none <- as.numeric(result_none$adjusted_p_values)

  expect_true(all(adj_bon >= adj_none - 1e-9))
})

test_that("correction method is preserved in output settings", {
  tbl <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  for (m in c("bonferroni", "BH", "none")) {
    result <- f_chisq_test(tbl, method = m)
    expect_equal(result$settings$method, m)
  }
})

# =============================================================================
# SECTION 7: Data frame input conversion
# =============================================================================

test_that("data.frame input: converts to table without error", {
  df <- data.frame(Agree = c(100, 120), Neutral = c(150, 90), Disagree = c(50, 40))
  rownames(df) <- c("Male", "Female")
  expect_no_error(suppressMessages(f_chisq_test(df)))
})

test_that("data.frame input: result matches equivalent table input", {
  df  <- data.frame(Agree = c(100, 120), Neutral = c(150, 90), Disagree = c(50, 40))
  tbl <- as.table(as.matrix(df))
  result_df  <- suppressMessages(f_chisq_test(df))
  result_tbl <- f_chisq_test(tbl)
  expect_equal(result_df$chisq_test_output$statistic,
               result_tbl$chisq_test_output$statistic,
               tolerance = 1e-6)
})

# =============================================================================
# SECTION 8: Matrix input
# =============================================================================

test_that("matrix input: returns correct structure", {
  mat <- matrix(c(100, 120, 150, 90, 50, 40), nrow = 2)
  result <- f_chisq_test(mat)
  expect_s3_class(result, "f_chisq_test")
  expect_s3_class(result$chisq_test_output, "htest")
})

test_that("matrix and equivalent table give identical chi-square statistics", {
  mat <- matrix(c(100, 120, 150, 90, 50, 40), nrow = 2)
  tbl <- as.table(mat)
  result_mat <- suppressMessages(f_chisq_test(mat))
  result_tbl <- suppressMessages(f_chisq_test(tbl))
  expect_equal(result_mat$chisq_test_output$statistic,
               result_tbl$chisq_test_output$statistic,
               tolerance = 1e-6)
})

# =============================================================================
# SECTION 9: Edge cases
# =============================================================================

test_that("2x2 table: runs correctly", {
  tbl_2x2 <- as.table(matrix(c(30, 10, 20, 40), 2, 2))
  expect_no_error(f_chisq_test(tbl_2x2))
})

test_that("2x2 table: post hoc output has correct dimensions", {
  tbl_2x2 <- as.table(matrix(c(30, 10, 20, 40), 2, 2))
  result   <- f_chisq_test(tbl_2x2, force_posthoc = TRUE)
  expect_equal(dim(result$adjusted_p_values), c(2, 2))
  expect_equal(nrow(result$observed_vs_adj_p_value), 4)
})

test_that("large table (5x5): runs without error", {
  set.seed(7)
  mat    <- matrix(sample(10:200, 25), nrow = 5)
  result <- f_chisq_test(as.table(mat))
  expect_s3_class(result, "f_chisq_test")
})

test_that("single-column table raises an error or warning", {
  tbl_1col <- as.table(matrix(c(10, 20, 30), ncol = 1))
  expect_error(f_chisq_test(tbl_1col))
})

test_that("GOF: p not summing to 1 gives informative error", {
  expect_error(f_chisq_test(x = c(10, 20, 30), p = c(0.1, 0.1, 0.1)),
               regexp = "sum to 1")
})

# =============================================================================
# SECTION 10: Alpha threshold combinations
# =============================================================================

test_that("alpha = 1: post hoc always runs (everything significant)", {
  tbl_ns <- as.table(rbind(c(50, 50), c(50, 50)))
  result  <- f_chisq_test(tbl_ns, alpha = 1, force_posthoc = TRUE)
  expect_true(!is.null(result$adjusted_p_values))
})

test_that("alpha = 0: post hoc never runs (nothing ever significant)", {
  tbl <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  result <- f_chisq_test(tbl, alpha = 0)
  expect_null(result$adjusted_p_values)
})

test_that("force_posthoc = TRUE overrides alpha = 0", {
  tbl    <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  result <- f_chisq_test(tbl, alpha = 0, force_posthoc = TRUE)
  expect_true(!is.null(result$adjusted_p_values))
})

# =============================================================================
# SECTION 11: print method
# =============================================================================

test_that("print.f_chisq_test: runs without error for significant table", {
  tbl    <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
  result <- f_chisq_test(tbl)
  expect_no_error(capture.output(print(result)))
})

test_that("print.f_chisq_test: runs without error for non-significant table", {
  tbl_ns <- as.table(rbind(c(50, 50), c(50, 50)))
  result <- f_chisq_test(tbl_ns)
  output <- capture.output(print(result))
  expect_true(any(grepl("No post hoc", output)))
})

test_that("print.f_chisq_test: GOF output is actually printed", {
  observed      <- c(2, 2, 10, 20, 15, 11)
  expected_prob <- rep(1/6, 6)
  result <- f_chisq_test(x = observed, p = expected_prob, force_posthoc = TRUE)
  output <- capture.output(print(result))
  expect_true(any(grepl("Observed|p.value|p-value", output)))
})

# =============================================================================
# SECTION 12: Combined stress
# =============================================================================

test_that("combined: large table + BH + alpha=0.01 + force_posthoc", {
  set.seed(99)
  mat    <- matrix(sample(5:300, 20), nrow = 4)
  result <- f_chisq_test(as.table(mat),
                         method        = "BH",
                         alpha         = 0.01,
                         force_posthoc = TRUE,
                         digits        = 5)
  expect_s3_class(result, "f_chisq_test")
  expect_equal(result$settings$method, "BH")
  expect_equal(result$settings$digits, 5)
  expect_true(!is.null(result$adjusted_p_values))
  expect_equal(dim(result$adjusted_p_values), c(4, 5))
})

test_that("combined: GOF + holm + force_posthoc + simulate.p.value via ...", {
  observed      <- c(15, 25, 10, 30, 20)
  expected_prob <- c(0.1, 0.3, 0.1, 0.3, 0.2)
  result <- f_chisq_test(x             = observed,
                         p             = expected_prob,
                         method        = "holm",
                         force_posthoc = TRUE,
                         simulate.p.value = TRUE,
                         B             = 2000)
  expect_s3_class(result, "f_chisq_test")
  expect_true(!is.null(result$posthoc_output_table))
})

test_that("combined: 3x4 table + hommel + alpha=0.10: output dimensions correct", {
  set.seed(3)
  mat <- matrix(sample(20:100, 12), nrow = 3)
  result <- f_chisq_test(as.table(mat),
                         method        = "hommel",
                         alpha         = 0.10,
                         force_posthoc = TRUE)
  expect_equal(ncol(result$adjusted_p_values), 4)
  expect_equal(nrow(result$adjusted_p_values), 3)
  expect_equal(nrow(result$observed_vs_adj_p_value), 6)
})
