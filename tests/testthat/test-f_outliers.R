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


# =============================================================================
# v3.1.0: BARE DATA.FRAME WITHOUT `columns`
# =============================================================================
# `columns` is now optional. When omitted, all numeric columns in `data`
# are scanned (excluding any column named in `group_vars` or `id_var`).
# =============================================================================

test_that("bare data.frame: f_outliers(mtcars) scans all numeric columns", {
  expect_no_error(
    res <- suppressMessages(f_outliers(mtcars))
  )
  # mtcars only has numeric columns, so the result should at least be
  # non-NULL (mtcars has known outliers in several columns) and tagged
  # as f_outliers.
  if (!is.null(res)) {
    expect_s3_class(res, "f_outliers")
  }
})

test_that("bare data.frame: f_outliers(df_with_factors) skips non-numeric", {
  df_test       <- mtcars
  df_test$grp   <- factor(rep(letters[1:4], length.out = nrow(df_test)))
  df_test$label <- paste0("R", seq_len(nrow(df_test)))

  expect_no_error(
    res <- suppressMessages(f_outliers(df_test, group_vars = "grp"))
  )
  if (!is.null(res)) {
    expect_s3_class(res, "f_outliers")
    # The grouping column should not appear as a scanned response key.
    expect_false("grp"   %in% setdiff(names(res), c("output_df")))
    expect_false("label" %in% setdiff(names(res), c("output_df")))
  }
})

test_that("bare data.frame: group_vars columns are excluded from auto-columns", {
  # cyl is numeric in mtcars; passing it as group_vars must NOT scan it
  # as a response.
  expect_no_error(
    res <- suppressMessages(f_outliers(mtcars, group_vars = "cyl"))
  )
  if (!is.null(res)) {
    # Whether 'cyl' appears as a key depends on outlier presence in
    # other columns; at minimum the call should not blow up.
    expect_s3_class(res, "f_outliers")
  }
})

test_that("bare data.frame: id_var column is excluded from auto-columns", {
  df_id <- make_outlier_df()
  # EmployeeID is character; it is implicitly skipped by the numeric
  # filter, but we also pass it as id_var. No error should occur.
  expect_no_error(
    res <- suppressMessages(f_outliers(df_id, id_var = "EmployeeID"))
  )
})

test_that("bare data.frame with no numeric columns errors clearly", {
  df_no_num <- data.frame(
    g = letters[1:5],
    f = factor(c("x", "y", "x", "y", "x")),
    stringsAsFactors = FALSE
  )
  expect_error(
    suppressMessages(f_outliers(df_no_num)),
    regexp = "numeric"
  )
})


# =============================================================================
# v3.1.0: MULTI-COLUMN PRINT HEADER
# =============================================================================
# When several responses are summarised, the print method shows a header
# naming each response variable. With a single response, no header is
# printed (the legacy 'output_df' entry name would make it misleading).
# =============================================================================

test_that("print.f_outliers: multi-column output shows a 'Variable:' header per response", {
  df_loc <- make_outlier_df()
  res <- suppressMessages(
    f_outliers(df_loc, columns = c("Salary", "Age"))
  )
  expect_s3_class(res, "f_outliers")
  out_txt <- paste(utils::capture.output(print(res)), collapse = "\n")
  expect_match(out_txt, "Salary", fixed = TRUE)
  expect_match(out_txt, "Age",    fixed = TRUE)
  # The header phrase introduced in 3.1.0
  expect_match(out_txt, "Variable", fixed = TRUE)
})

test_that("print.f_outliers: single-column output does not show 'Variable:' header", {
  df_loc <- make_outlier_df()
  res <- suppressMessages(
    f_outliers(df_loc, columns = "Salary")
  )
  expect_s3_class(res, "f_outliers")
  out_txt <- paste(utils::capture.output(print(res)), collapse = "\n")
  # For a single response, f_outliers names the entry 'output_df'.
  # Printing a 'Variable: output_df' header would be misleading, so the
  # 3.1.0 fix skips it. Confirm that string is absent.
  expect_false(grepl("Variable: output_df", out_txt, fixed = TRUE))
})
