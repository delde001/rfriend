# BUGS #32: bad_rows$row_id returned NULL without a warning, so code like
# data$x[bad_rows$row_id] <- NA silently changed nothing. Now $ reaches the
# columns of the outlier table and stops for unknown names, and
# f_remove_outliers(set_na = TRUE, columns = ...) keeps the rows and clears
# only the chosen values.

two_col_df <- function() {
  # evenly spaced values have no outliers of their own
  d <- data.frame(g = rep(c("a", "b"), each = 20),
                  x = rep(seq(48, 52, length.out = 20), 2),
                  y = rep(seq(9, 11, length.out = 20), 2))
  d$x[3]  <- 90    # outlier in x only
  d$y[25] <- 30    # outlier in y only
  d
}

test_that("$ gives row_id and columns of a single-column result; unknown names stop", {
  d <- two_col_df()
  b <- f_outliers(x ~ g, data = d)
  expect_identical(as.integer(b$row_id), 3L)
  expect_equal(b$x, 90)
  expect_s3_class(b$output_df, "data.frame")
  expect_error(b$rowid, "'rowid' is not part of this f_outliers result. Available:", fixed = TRUE)
  expect_null(b[["row_id"]])                   # [[ keeps the list behaviour
  dd <- d; dd$x[b$row_id] <- NA
  expect_equal(sum(is.na(dd$x)), 1L)           # the old silent no-op now works
})

test_that("$row_id on a multi-column result gives all flagged rows", {
  d <- two_col_df()
  b <- suppressMessages(f_outliers(d, columns = c("x", "y")))
  expect_identical(sort(names(b)), c("x", "y"))
  expect_identical(as.integer(b$row_id), c(3L, 25L))
  expect_error(b$g, "is not part of this f_outliers result")
})

test_that("set_na = TRUE clears only the flagged values, in their own column", {
  d <- two_col_df()
  b <- suppressMessages(f_outliers(d, columns = c("x", "y")))
  expect_message(out <- f_remove_outliers(d, b, set_na = TRUE),
                 "Set 2 values to NA (x: row_id 3; y: row_id 25). Rows kept: 40.", fixed = TRUE)
  expect_equal(nrow(out), 40L)
  expect_true(is.na(out$x[3]) && is.na(out$y[25]))
  expect_false(is.na(out$y[3]) || is.na(out$x[25]))   # not cross-cleared
  expect_equal(sum(is.na(out$x)) + sum(is.na(out$y)), 2L)
  expect_identical(names(out), names(d))               # no helper row_id left
})

test_that("columns limits set_na to some checked columns", {
  d <- two_col_df()
  b <- suppressMessages(f_outliers(d, columns = c("x", "y")))
  out <- suppressMessages(f_remove_outliers(d, b, set_na = TRUE, columns = "y"))
  expect_equal(sum(is.na(out$x)), 0L)
  expect_true(is.na(out$y[25]))
  expect_error(f_remove_outliers(d, b, set_na = TRUE, columns = "g"),
               "Column(s) not checked by f_outliers(): g", fixed = TRUE)
})

test_that("by hand: row numbers plus columns", {
  d <- two_col_df()
  expect_message(out <- f_remove_outliers(d, c(1, 2), set_na = TRUE, columns = c("x", "y")),
                 "Set 4 values to NA", fixed = TRUE)
  expect_equal(nrow(out), 40L)
  expect_true(all(is.na(out$x[1:2])) && all(is.na(out$y[1:2])))
  expect_error(f_remove_outliers(d, c(1, 2), set_na = TRUE),
               "name the column(s) to clear with 'columns'", fixed = TRUE)
  expect_error(f_remove_outliers(d, c(1, 2), columns = "x"),
               "'columns' is only used with set_na = TRUE", fixed = TRUE)
  expect_error(f_remove_outliers(d, c(1, 2), set_na = TRUE, columns = "z"),
               "Column(s) not found in 'data': z.", fixed = TRUE)
})

test_that("row removal messages use the right number", {
  d <- two_col_df()
  expect_message(f_remove_outliers(d, 3), "Removed 1 outlier row.", fixed = TRUE)
  expect_message(f_remove_outliers(d, c(3, 25)), "Removed 2 outlier rows.", fixed = TRUE)
})
