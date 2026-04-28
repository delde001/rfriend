# =============================================================================
# test-f_remove_outliers.R
# =============================================================================
# Tests for f_remove_outliers().
#
# Fixtures: make_outlier_df() from helper-data.R
#           (replaces the inline make_df() from the pre-refactor file)
# =============================================================================


# ---------------------------------------------------------------------------
# 1. Core: removes correct rows via f_outliers output
# ---------------------------------------------------------------------------

test_that("removes correct rows when passed f_outliers output", {
  df       <- make_outlier_df()
  outliers <- f_outliers(df, columns = "Salary")
  clean    <- f_remove_outliers(df, outliers)

  # Injected outlier rows were row 20 (Salary = 100000, EmployeeID = E020)
  # and row 40 (Salary = 1000, EmployeeID = E040). Use EmployeeID to verify
  # removal: rownames are reset to 1..nrow(clean) by f_remove_outliers, so
  # checking rownames would always find "20" whenever nrow(clean) >= 20.
  expect_false("E020" %in% clean$EmployeeID)
  expect_false("E040" %in% clean$EmployeeID)
  expect_false(100000 %in% clean$Salary)
  expect_false(1000   %in% clean$Salary)
  expect_equal(nrow(clean), nrow(df) - nrow(outliers$output_df))
})

test_that("returned row names are sequential after removal", {
  df       <- make_outlier_df()
  outliers <- f_outliers(df, columns = "Salary")
  clean    <- f_remove_outliers(df, outliers)

  expect_equal(rownames(clean), as.character(seq_len(nrow(clean))))
})


# ---------------------------------------------------------------------------
# 2. f_outliers multi-column list output
# ---------------------------------------------------------------------------

test_that("handles multi-column f_outliers list: all flagged rows removed", {
  df       <- make_outlier_df()
  # Salary flags rows 20, 40; Age flags row 39
  outliers <- f_outliers(df, columns = c("Salary", "Age"))
  clean    <- f_remove_outliers(df, outliers)

  removed_ids <- unique(c(
    outliers$Salary$row_id,
    outliers$Age$row_id
  ))
  expect_equal(nrow(clean), nrow(df) - length(removed_ids))
})


# ---------------------------------------------------------------------------
# 3. Manual data.frame input (subset of outliers)
# ---------------------------------------------------------------------------

test_that("accepts a plain data.frame with row_id for selective removal", {
  df       <- make_outlier_df()
  outliers <- f_outliers(df, columns = "Salary")
  # Remove only the high outlier (row 20); keep the low one (row 40)
  just_high <- outliers$output_df[outliers$output_df$Salary > 90000, ]
  clean     <- f_remove_outliers(df, just_high)

  expect_equal(nrow(clean), nrow(df) - 1)
  expect_false(100000 %in% clean$Salary)  # high outlier gone
  expect_true(1000    %in% clean$Salary)  # low outlier still present
})


# ---------------------------------------------------------------------------
# 4. Manual vector of row numbers
# ---------------------------------------------------------------------------

test_that("accepts a plain numeric vector of row numbers", {
  df    <- make_outlier_df()
  clean <- f_remove_outliers(df, outliers = c(20, 40))

  expect_equal(nrow(clean), 38)
  expect_false(100000 %in% clean$Salary)
  expect_false(1000   %in% clean$Salary)
})


# ---------------------------------------------------------------------------
# 5. Custom 'by' column (e.g. EmployeeID)
# ---------------------------------------------------------------------------

test_that("removes rows by a custom id column", {
  df       <- make_outlier_df()
  outliers <- f_outliers(df, columns = "Salary", id_var = "EmployeeID")
  ids      <- outliers$output_df$EmployeeID
  clean    <- f_remove_outliers(df, outliers$output_df, by = "EmployeeID")

  expect_false(any(clean$EmployeeID %in% ids))
  expect_equal(nrow(clean), nrow(df) - length(ids))
})


# ---------------------------------------------------------------------------
# 6. No matches: returns original data unchanged
# ---------------------------------------------------------------------------

test_that("returns original data unchanged when no IDs match", {
  df    <- make_outlier_df()
  expect_message(
    clean <- f_remove_outliers(df, outliers = c(999, 1000)),
    regexp = "No outliers were removed"
  )
  expect_equal(nrow(clean), nrow(df))
})


# ---------------------------------------------------------------------------
# 7. verbose = FALSE suppresses messages
# ---------------------------------------------------------------------------

test_that("verbose = FALSE produces no messages", {
  df       <- make_outlier_df()
  outliers <- f_outliers(df, columns = "Salary")
  expect_no_message(
    f_remove_outliers(df, outliers, verbose = FALSE)
  )
})


# ---------------------------------------------------------------------------
# 8. Output class matches input class
# ---------------------------------------------------------------------------

test_that("output class matches data.frame input", {
  df       <- make_outlier_df()
  outliers <- f_outliers(df, columns = "Salary")
  clean    <- f_remove_outliers(df, outliers, verbose = FALSE)
  expect_s3_class(clean, "data.frame")
})

test_that("output class matches tibble input", {
  skip_if_not_installed("tibble")
  df       <- tibble::as_tibble(make_outlier_df())
  outliers <- f_outliers(df, columns = "Salary")
  clean    <- f_remove_outliers(df, outliers, verbose = FALSE)
  expect_s3_class(clean, "tbl_df")
})


# ---------------------------------------------------------------------------
# 9. Input validation errors
# ---------------------------------------------------------------------------

test_that("stops if data is not a data.frame", {
  expect_error(
    f_remove_outliers(list(a = 1), outliers = 1L),
    regexp = "data.frame"
  )
})

test_that("stops if 'by' column missing from outliers data.frame", {
  df        <- make_outlier_df()
  bad_input <- data.frame(NotRowId = 1:2)
  expect_error(
    f_remove_outliers(df, bad_input, by = "row_id"),
    regexp = "row_id"
  )
})

test_that("stops if custom 'by' column missing from source data", {
  df <- make_outlier_df()
  expect_error(
    f_remove_outliers(df, outliers = c(1, 2), by = "ghost_col"),
    regexp = "ghost_col"
  )
})

test_that("stops if outliers is an unsupported type", {
  df <- make_outlier_df()
  expect_error(
    f_remove_outliers(df, outliers = TRUE),
    regexp = "dataframe or a vector"
  )
})


# ---------------------------------------------------------------------------
# 10. Round-trip: f_outliers |> f_remove_outliers
# ---------------------------------------------------------------------------

test_that("round-trip: removed rows match exactly what f_outliers flagged", {
  df       <- make_outlier_df()
  outliers <- f_outliers(df, columns = "Salary")
  clean    <- f_remove_outliers(df, outliers, verbose = FALSE)

  flagged_salaries <- outliers$output_df$Salary
  expect_false(any(clean$Salary %in% flagged_salaries))
  expect_equal(nrow(clean) + nrow(outliers$output_df), nrow(df))
})
