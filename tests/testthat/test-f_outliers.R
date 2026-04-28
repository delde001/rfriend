# =============================================================================
# test-f_outliers.R
# =============================================================================
# Tests for f_outliers() including S3 dispatch (numeric, integer, formula,
# data.frame methods).
#
# Fixtures:
#   make_outlier_df()  from helper-data.R
#   make_outlier_vec() defined locally (this file uses a different seed and
#                      length than the shared helper, so it stays local)
# =============================================================================

df <- make_outlier_df()

# ---------------------------------------------------------------------------
# 1. Basic: single column, no groups
# ---------------------------------------------------------------------------
test_that("detects outlier in single column without groups", {
  result <- f_outliers(df, columns = "Salary")
  expect_s3_class(result, "f_outliers")
  expect_true("output_df" %in% names(result))
  out_df <- result$output_df
  expect_true(nrow(out_df) >= 1)
  expect_true(all(c("row_id", "Salary") %in% names(out_df)))
  # The injected outliers should appear
  expect_true(20 %in% out_df$row_id || 40 %in% out_df$row_id)
})

# ---------------------------------------------------------------------------
# 2. Grouped detection
# ---------------------------------------------------------------------------
test_that("grouped detection finds within-group outliers", {
  result <- f_outliers(df, columns = "Salary", group_vars = "Team")
  out_df <- result$output_df
  # Row 20 is outlier in group A, row 40 in group B
  expect_true(20 %in% out_df$row_id)
  expect_true(40 %in% out_df$row_id)
})

test_that("multi-group interaction works", {
  result <- f_outliers(df, columns = "Salary", group_vars = c("Team", "Department"))
  expect_s3_class(result, "f_outliers")
  expect_true(nrow(result$output_df) >= 1)
})

# ---------------------------------------------------------------------------
# 3. Formula interface
# ---------------------------------------------------------------------------
test_that("formula interface matches direct call", {
  res_formula <- f_outliers(Salary ~ Team, data = df)
  res_direct  <- f_outliers(df, columns = "Salary", group_vars = "Team")
  expect_equal(res_formula$output_df$row_id,
               res_direct$output_df$row_id)
})

test_that("formula with multiple LHS columns works", {
  result <- f_outliers(Salary + Age ~ Team, data = df)
  expect_true("Salary" %in% names(result))
  expect_true("Age"    %in% names(result))
})

test_that("one-sided formula throws informative error", {
  expect_error(f_outliers(~ Team, data = df),
               regexp = "two-sided")
})

# ---------------------------------------------------------------------------
# 4. Multiple columns: no-outlier in first column does not suppress second
# ---------------------------------------------------------------------------
test_that("no-outlier in first column does not suppress second column results", {
  df_test <- df
  df_test$CleanCol <- rep(1:2, 20)
  result <- f_outliers(df_test, columns = c("CleanCol", "Age"))
  expect_false(is.null(result))
  expect_true("Age" %in% names(result))
  expect_true(39 %in% result$Age$row_id)
})

# ---------------------------------------------------------------------------
# 5. No outliers at all: returns NULL
# ---------------------------------------------------------------------------
test_that("returns NULL with message when no outliers exist", {
  clean_df <- data.frame(x = 1:20)
  expect_message(
    result <- f_outliers(clean_df, columns = "x"),
    "No outliers"
  )
  expect_null(result)
})

# ---------------------------------------------------------------------------
# 6. coef parameter changes sensitivity
# ---------------------------------------------------------------------------
test_that("higher coef finds fewer outliers than lower coef", {
  res_loose  <- f_outliers(df, columns = "Salary", coef = 1.5)
  res_strict <- f_outliers(df, columns = "Salary", coef = 3.0)
  n_loose    <- if (is.null(res_loose))  0L else nrow(res_loose$output_df)
  n_strict   <- if (is.null(res_strict)) 0L else nrow(res_strict$output_df)
  expect_gte(n_loose, n_strict)
})

# ---------------------------------------------------------------------------
# 7. id_var: placed first, output sorted by it
# ---------------------------------------------------------------------------
test_that("id_var appears as first column and output is sorted by it", {
  result <- f_outliers(df, columns = "Salary", id_var = "EmployeeID")
  out_df <- result$output_df
  expect_equal(names(out_df)[1], "EmployeeID")
  expect_equal(out_df$EmployeeID, sort(out_df$EmployeeID))
})

# ---------------------------------------------------------------------------
# 8. Input validation
# ---------------------------------------------------------------------------
test_that("non-existent column throws error", {
  expect_error(f_outliers(df, columns = "NonExistent"),
               regexp = "not found")
})

test_that("non-existent group_var throws error", {
  expect_error(f_outliers(df, columns = "Salary", group_vars = "Ghost"),
               regexp = "not found")
})

test_that("non-numeric column throws error", {
  expect_error(f_outliers(df, columns = "Team"),
               regexp = "[Nn]umeric|[Nn]on-numeric")
})

test_that("missing x with no data arg throws error", {
  expect_error(f_outliers(), regexp = "missing")
})

# ---------------------------------------------------------------------------
# 9. row_id maps back to original data
# ---------------------------------------------------------------------------
test_that("row_id in output maps exactly to original rows", {
  result <- f_outliers(df, columns = "Salary")
  out_df <- result$output_df
  for (i in seq_len(nrow(out_df))) {
    rid <- out_df$row_id[i]
    expect_equal(out_df$Salary[i], df$Salary[rid])
  }
})

# ---------------------------------------------------------------------------
# 10. Edge cases
# ---------------------------------------------------------------------------
test_that("handles NA values without error", {
  df_na <- df
  df_na$Salary[5] <- NA
  expect_no_error(f_outliers(df_na, columns = "Salary"))
})

test_that("single-row group does not crash", {
  df_small <- data.frame(
    G   = c(rep("A", 10), "B"),
    Val = c(rnorm(9, 50, 1), 200, 50)
  )
  expect_no_error(f_outliers(df_small, columns = "Val", group_vars = "G"))
})

test_that("all-NA column returns NULL gracefully", {
  df_allna <- data.frame(x = rep(NA_real_, 10))
  expect_no_error(
    res <- f_outliers(df_allna, columns = "x")
  )
  expect_null(res)
})


# =============================================================================
# S3 dispatch (numeric, integer, formula, data.frame methods)
# =============================================================================

# Local fixture: uses seed 5 and length 32, distinct from the shared
# helper-data.R version (seed 42, length 30).
make_outlier_vec_local <- function() {
  set.seed(5)
  c(rnorm(30, 10, 1), 25, 26)    # two clear high outliers
}

# ---------------------------------------------------------------------------
# A, B. numeric and integer dispatch
# ---------------------------------------------------------------------------
test_that("numeric vector dispatches to f_outliers.numeric", {
  v <- make_outlier_vec_local()
  expect_no_error(res <- suppressMessages(f_outliers(v)))
  expect_true(is.list(res) || is.data.frame(res) ||
              inherits(res, "f_outliers"))
})

test_that("integer vector dispatches to f_outliers.integer", {
  v <- as.integer(c(1, 2, 3, 4, 5, 100))
  expect_no_error(res <- suppressMessages(f_outliers(v)))
})

# ---------------------------------------------------------------------------
# C. formula method
# ---------------------------------------------------------------------------
test_that("formula interface works with a data frame", {
  df_loc <- data.frame(
    y = make_outlier_vec_local(),
    g = factor(rep(c("A", "B"), length.out = 32))
  )
  expect_no_error(
    res <- suppressMessages(f_outliers(y ~ g, data = df_loc))
  )
})

# ---------------------------------------------------------------------------
# D. data.frame method with x as the dispatch argument
# ---------------------------------------------------------------------------
test_that("data.frame interface uses x as first argument", {
  df_loc <- data.frame(
    v1 = make_outlier_vec_local(),
    v2 = rnorm(32)
  )
  expect_no_error(
    res <- suppressMessages(f_outliers(df_loc, columns = c("v1", "v2")))
  )
})

test_that("data.frame interface via named x also works", {
  df_loc <- data.frame(v1 = make_outlier_vec_local())
  expect_no_error(
    res <- suppressMessages(f_outliers(x = df_loc, columns = "v1"))
  )
})
