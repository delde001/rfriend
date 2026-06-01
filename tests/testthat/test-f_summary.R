# =============================================================================
# test-f_summary.R
# =============================================================================
# Covers: basic usage, formula notation, multiple columns/groups, toggle
# combinations, edge cases, input validation, output structure,
# and S3 dispatch cleanup.
#
# Fixtures: local (test_data, na_data, tiny_data, flat_data) - specific to
# this file's assertions about exact statistic values.
# =============================================================================

# --- Shared test data --------------------------------------------------------

test_data <- data.frame(
  value      = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
  weight     = c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 11.0),
  group_a    = rep(c("A", "B"), each = 5),
  group_b    = rep(c("X", "Y"), times = 5),
  int_col    = as.integer(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)),
  stringsAsFactors = FALSE
)

na_data <- data.frame(
  value   = c(1, 2, NA, 4, 5, NA, 7, 8, 9, NA),
  group   = rep(c("A", "B"), each = 5),
  stringsAsFactors = FALSE
)

tiny_data <- data.frame(
  value = c(10, 20),
  group = c("A", "B"),
  stringsAsFactors = FALSE
)

flat_data <- data.frame(
  value = rep(5, 10),
  group = rep(c("A", "B"), each = 5),
  stringsAsFactors = FALSE
)


# =============================================================================
# 1. RETURN STRUCTURE
# =============================================================================
test_that("returns list of class f_summary", {
  res <- f_summary(test_data, columns = "value")
  expect_s3_class(res, "f_summary")
  expect_type(res, "list")
})

test_that("single-column result stored under 'output_df'", {
  res <- f_summary(test_data, columns = "value")
  expect_true("output_df" %in% names(res))
})

test_that("multi-column results stored under each column name", {
  res <- f_summary(test_data, columns = c("value", "weight"))
  expect_true("value"  %in% names(res))
  expect_true("weight" %in% names(res))
  expect_false("output_df" %in% names(res))
})

test_that("result data frame has correct number of rows (no groups)", {
  res <- f_summary(test_data, columns = "value")
  expect_equal(nrow(res$output_df), 1L)
})

test_that("result data frame has correct number of rows (one group)", {
  res <- f_summary(test_data, columns = "value", group_vars = "group_a")
  expect_equal(nrow(res$output_df), 2L)
})

test_that("result data frame has correct number of rows (two groups)", {
  res <- f_summary(test_data, columns = "value", group_vars = c("group_a", "group_b"))
  expect_equal(nrow(res$output_df), 4L)
})


# =============================================================================
# 2. FORMULA INTERFACE
# =============================================================================
test_that("formula notation matches direct call (single column, single group)", {
  res_formula <- f_summary(value ~ group_a, data = test_data)
  res_direct  <- f_summary(test_data, columns = "value", group_vars = "group_a")
  expect_equal(res_formula$output_df, res_direct$output_df)
})

test_that("formula notation works for multiple LHS columns", {
  res <- f_summary(value + weight ~ group_a, data = test_data)
  expect_true("value"  %in% names(res))
  expect_true("weight" %in% names(res))
})

test_that("formula notation works for multiple RHS groups", {
  res <- f_summary(value ~ group_a + group_b, data = test_data)
  expect_equal(nrow(res$output_df), 4L)
})

test_that("formula notation: multiple columns AND multiple groups", {
  res <- f_summary(value + weight ~ group_a + group_b, data = test_data)
  expect_true("value"  %in% names(res))
  expect_true("weight" %in% names(res))
  expect_equal(nrow(res$value), 4L)
  expect_equal(nrow(res$weight), 4L)
})


# =============================================================================
# 3. STATISTIC TOGGLES - ALL ON (default)
# =============================================================================
test_that("default output contains all expected statistic columns (no groups)", {
  res  <- f_summary(test_data, columns = "value")
  cols <- names(res$output_df)
  for (stat in c("n", "mean", "sd", "se", "min", "Q1", "median", "Q3", "max")) {
    expect_true(stat %in% cols, info = paste("Missing stat column:", stat))
  }
})

test_that("skew and kurt absent by default", {
  res  <- f_summary(test_data, columns = "value")
  cols <- names(res$output_df)
  expect_false("skew" %in% cols)
  expect_false("kurt" %in% cols)
})


# =============================================================================
# 4. STATISTIC TOGGLES - INDIVIDUAL ON/OFF
# =============================================================================
toggle_params <- list(
  list(param = "show_n",        col = "n"),
  list(param = "show_mean",     col = "mean"),
  list(param = "show_sd",       col = "sd"),
  list(param = "show_se",       col = "se"),
  list(param = "show_min",      col = "min"),
  list(param = "show_max",      col = "max"),
  list(param = "show_median",   col = "median"),
  list(param = "show_Q1",       col = "Q1"),
  list(param = "show_Q3",       col = "Q3")
)

for (tp in toggle_params) {
  local({
    p <- tp$param
    c <- tp$col
    test_that(paste("setting", p, "= FALSE removes column", c), {
      args      <- list(test_data, columns = "value")
      args[[p]] <- FALSE
      res       <- do.call(f_summary, args)
      expect_false(c %in% names(res$output_df))
    })
  })
}

test_that("show_skew = TRUE adds skew column", {
  res <- f_summary(test_data, columns = "value", show_skew = TRUE)
  expect_true("skew" %in% names(res$output_df))
})

test_that("show_kurtosis = TRUE adds kurt column", {
  res <- f_summary(test_data, columns = "value", show_kurtosis = TRUE)
  expect_true("kurt" %in% names(res$output_df))
})

test_that("show_skew + show_kurtosis = TRUE both present", {
  res <- f_summary(test_data, columns = "value", show_skew = TRUE, show_kurtosis = TRUE)
  expect_true("skew" %in% names(res$output_df))
  expect_true("kurt" %in% names(res$output_df))
})

test_that("turning off ALL optional stats leaves only n", {
  res <- f_summary(test_data, columns = "value",
                   show_mean = FALSE, show_sd = FALSE, show_se = FALSE,
                   show_min = FALSE,  show_max = FALSE, show_median = FALSE,
                   show_Q1 = FALSE,   show_Q3 = FALSE)
  expect_equal(names(res$output_df), "n")
})


# =============================================================================
# 5. TOGGLE COMBINATIONS (grouped)
# =============================================================================
test_that("Q1/Q3 hidden, mean/sd shown, grouped", {
  res  <- f_summary(test_data, columns = "value", group_vars = "group_a",
                    show_Q1 = FALSE, show_Q3 = FALSE)
  cols <- names(res$output_df)
  expect_true("value.mean" %in% cols)
  expect_true("value.sd"   %in% cols)
  expect_false("value.Q1"  %in% cols)
  expect_false("value.Q3"  %in% cols)
})

test_that("only median returned when all other stats off (grouped)", {
  res <- f_summary(test_data, columns = "value", group_vars = "group_a",
                   show_n = FALSE, show_mean = FALSE, show_sd = FALSE,
                   show_se = FALSE, show_min = FALSE, show_max = FALSE,
                   show_Q1 = FALSE, show_Q3 = FALSE)
  stat_cols <- setdiff(names(res$output_df), "group_a")
  expect_equal(stat_cols, "value.median")
})

test_that("skew + kurtosis with groups returns correct columns", {
  res  <- f_summary(test_data, columns = "value", group_vars = "group_a",
                    show_skew = TRUE, show_kurtosis = TRUE)
  cols <- names(res$output_df)
  expect_true(any(grepl("value.skew", cols)))
  expect_true(any(grepl("value.kurt", cols)))
})


# =============================================================================
# 6. CORRECTNESS OF COMPUTED VALUES
# =============================================================================
test_that("n is correct (no groups)", {
  res <- f_summary(test_data, columns = "value")
  expect_equal(res$output_df$n, nrow(test_data))
})

test_that("mean is correct (no groups)", {
  res <- f_summary(test_data, columns = "value")
  expect_equal(res$output_df$mean, mean(test_data$value))
})

test_that("sd is correct (no groups)", {
  res <- f_summary(test_data, columns = "value")
  expect_equal(res$output_df$sd, sd(test_data$value))
})

test_that("se equals sd / sqrt(n) (no groups)", {
  res <- f_summary(test_data, columns = "value")
  expected_se <- sd(test_data$value) / sqrt(nrow(test_data))
  expect_equal(res$output_df$se, expected_se)
})

test_that("min and max are correct (no groups)", {
  res <- f_summary(test_data, columns = "value")
  expect_equal(res$output_df$min, min(test_data$value))
  expect_equal(res$output_df$max, max(test_data$value))
})

test_that("median is correct (no groups)", {
  res <- f_summary(test_data, columns = "value")
  expect_equal(res$output_df$median, median(test_data$value))
})

test_that("Q1 and Q3 are correct (no groups)", {
  res <- f_summary(test_data, columns = "value")
  expect_equal(res$output_df$Q1, unname(quantile(test_data$value, 0.25)))
  expect_equal(res$output_df$Q3, unname(quantile(test_data$value, 0.75)))
})

test_that("group means are correct (one group)", {
  res <- f_summary(test_data, columns = "value", group_vars = "group_a")
  a_mean <- mean(test_data$value[test_data$group_a == "A"])
  b_mean <- mean(test_data$value[test_data$group_a == "B"])
  row_a  <- res$output_df[res$output_df$group_a == "A", ]
  row_b  <- res$output_df[res$output_df$group_a == "B", ]
  expect_equal(row_a$value.mean, a_mean)
  expect_equal(row_b$value.mean, b_mean)
})


# =============================================================================
# 7. DIGITS / ROUNDING
# =============================================================================
test_that("digits = 0 rounds mean to whole number", {
  res <- f_summary(test_data, columns = "value", digits = 0)
  res$output_df$mean <- as.numeric(res$output_df$mean)
  expect_equal(res$output_df$mean %% 1, 0)
})

test_that("digits = 4 preserves at least 4 decimal places for non-integer values", {
  res <- f_summary(data.frame(value = c(1, 2, 3, 7)), columns = "value", digits = 4)
  res$output_df$mean <- as.numeric(res$output_df$mean)
  expect_equal(res$output_df$mean, round(mean(c(1, 2, 3, 7)), 4))
})

test_that("digits = NULL applies no rounding", {
  res <- f_summary(test_data, columns = "value", digits = NULL)
  expect_equal(res$output_df$mean, mean(test_data$value))
})


# =============================================================================
# 8. NA HANDLING
# =============================================================================
test_that("n reflects only non-NA values", {
  res <- f_summary(na_data, columns = "value")
  expect_equal(res$output_df$n, sum(!is.na(na_data$value)))
})

test_that("mean ignores NAs", {
  res <- f_summary(na_data, columns = "value")
  expect_equal(res$output_df$mean, mean(na_data$value, na.rm = TRUE))
})

test_that("grouped summary with NAs: group means are correct", {
  res    <- f_summary(na_data, columns = "value", group_vars = "group")
  a_mean <- mean(na_data$value[na_data$group == "A"], na.rm = TRUE)
  row_a  <- res$output_df[res$output_df$group == "A", ]
  expect_equal(row_a$value.mean, a_mean)
})


# =============================================================================
# 9. EDGE CASES
# =============================================================================
test_that("single-row data returns without error", {
  single <- data.frame(value = 42, group = "A", stringsAsFactors = FALSE)
  expect_no_error(f_summary(single, columns = "value"))
})

test_that("flat (zero-variance) data: sd = 0, skew = NA (guarded)", {
  res <- f_summary(flat_data, columns = "value",
                   show_skew = TRUE, show_kurtosis = TRUE)
  expect_equal(res$output_df$sd, 0)
})

test_that("flat grouped data: skew and kurt are NA (s = 0 guard)", {
  res <- f_summary(flat_data, columns = "value", group_vars = "group",
                   show_skew = TRUE, show_kurtosis = TRUE)
  expect_true(all(is.na(res$output_df[, grep("skew", names(res$output_df))])))
  expect_true(all(is.na(res$output_df[, grep("kurt", names(res$output_df))])))
})

test_that("two-group combo uses both group columns as identifiers", {
  res   <- f_summary(test_data, columns = "value",
                     group_vars = c("group_a", "group_b"))
  cols  <- names(res$output_df)
  expect_true("group_a" %in% cols)
  expect_true("group_b" %in% cols)
})


# =============================================================================
# 10. INPUT VALIDATION
# =============================================================================
test_that("non-existent group column raises error", {
  expect_error(
    f_summary(test_data, columns = "value", group_vars = "nonexistent_col"),
    regexp = "not found"
  )
})

test_that("non-data.frame input raises error", {
  expect_error(f_summary(list(a = 1:5), columns = "a"))
})


# =============================================================================
# 11. MULTIPLE COLUMNS x MULTIPLE GROUPS x TOGGLES (combined stress tests)
# =============================================================================
test_that("stress: 2 columns x 2 groups x skew + kurtosis on", {
  res <- f_summary(test_data,
                   columns    = c("value", "weight"),
                   group_vars = c("group_a", "group_b"),
                   show_skew  = TRUE, show_kurtosis = TRUE)
  for (col in c("value", "weight")) {
    df   <- res[[col]]
    cols <- names(df)
    expect_true("group_a" %in% cols)
    expect_true("group_b" %in% cols)
    expect_true(any(grepl("skew", cols)))
    expect_true(any(grepl("kurt", cols)))
    expect_equal(nrow(df), 4L)
  }
})

test_that("stress: formula 2 cols x 2 groups x Q1/Q3 off x digits = 3", {
  res <- f_summary(value + weight ~ group_a + group_b,
                   data    = test_data,
                   show_Q1 = FALSE, show_Q3 = FALSE,
                   digits  = 3)
  for (col in c("value", "weight")) {
    df   <- res[[col]]
    cols <- names(df)
    expect_false(any(grepl("Q1", cols)))
    expect_false(any(grepl("Q3", cols)))
    expect_equal(nrow(df), 4L)
  }
})

test_that("stress: all toggles on simultaneously", {
  res <- f_summary(test_data,
                   columns      = c("value", "weight"),
                   group_vars   = "group_a",
                   show_n       = TRUE, show_mean     = TRUE, show_sd   = TRUE,
                   show_se      = TRUE, show_min      = TRUE, show_max  = TRUE,
                   show_median  = TRUE, show_Q1       = TRUE, show_Q3   = TRUE,
                   show_skew    = TRUE, show_kurtosis = TRUE,
                   digits       = 2)
  expect_equal(length(res), 2L)
  for (col in c("value", "weight")) {
    expect_equal(nrow(res[[col]]), 2L)
  }
})

test_that("stress: all non-essential stats turned off across 2 groups", {
  res <- f_summary(test_data,
                   columns    = c("value", "weight"),
                   group_vars = c("group_a", "group_b"),
                   show_mean  = FALSE, show_sd     = FALSE, show_se  = FALSE,
                   show_min   = FALSE, show_max    = FALSE, show_Q1  = FALSE,
                   show_Q3    = FALSE, show_median = FALSE)
  for (col in c("value")) {
    stat_cols <- setdiff(names(res[[col]]), c("group_a", "group_b"))
    expect_equal(stat_cols, "value.n")
  }
})

test_that("stress: built-in iris dataset, grouped by Species, all stats", {
  res <- f_summary(iris,
                   columns       = c("Sepal.Length", "Sepal.Width",
                                     "Petal.Length", "Petal.Width"),
                   group_vars    = "Species",
                   show_skew     = TRUE,
                   show_kurtosis = TRUE,
                   digits        = 3)
  expect_equal(length(res), 4L)
  for (col in c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")) {
    expect_equal(nrow(res[[col]]), 3L)
  }
})

test_that("stress: mtcars, 3 columns, 2 groups, formula, digits = 1", {
  res <- f_summary(hp + disp + wt ~ cyl + gear,
                   data   = mtcars,
                   digits = 1)
  expect_true(all(c("hp", "disp", "wt") %in% names(res)))
})


# =============================================================================
# 11b. CONFIDENCE INTERVAL (show_ci / conf_level) and NA-aware se
# =============================================================================
test_that("show_ci = TRUE adds CI_lower and CI_upper (no groups)", {
  res  <- f_summary(test_data, columns = "value", show_ci = TRUE)
  cols <- names(res$output_df)
  expect_true("CI_lower" %in% cols)
  expect_true("CI_upper" %in% cols)
})

test_that("CI columns absent by default", {
  res  <- f_summary(test_data, columns = "value")
  cols <- names(res$output_df)
  expect_false("CI_lower" %in% cols)
  expect_false("CI_upper" %in% cols)
})

test_that("CI bounds match t.test (no groups)", {
  res <- f_summary(test_data, columns = "value", show_ci = TRUE)
  tt  <- t.test(test_data$value, conf.level = 0.95)
  expect_equal(res$output_df$CI_lower, unname(tt$conf.int[1]))
  expect_equal(res$output_df$CI_upper, unname(tt$conf.int[2]))
})

test_that("CI bounds match t.test at conf_level = 0.90", {
  res <- f_summary(test_data, columns = "value",
                   show_ci = TRUE, conf_level = 0.90)
  tt  <- t.test(test_data$value, conf.level = 0.90)
  expect_equal(res$output_df$CI_lower, unname(tt$conf.int[1]))
  expect_equal(res$output_df$CI_upper, unname(tt$conf.int[2]))
})

test_that("CI is symmetric around the mean", {
  res  <- f_summary(test_data, columns = "value", show_ci = TRUE)
  mid  <- (res$output_df$CI_lower + res$output_df$CI_upper) / 2
  expect_equal(mid, res$output_df$mean)
})

test_that("grouped CI bounds match per-group t.test", {
  res    <- f_summary(test_data, columns = "value",
                      group_vars = "group_a", show_ci = TRUE)
  tt_a   <- t.test(test_data$value[test_data$group_a == "A"])
  row_a  <- res$output_df[res$output_df$group_a == "A", ]
  expect_equal(row_a$value.CI_lower, unname(tt_a$conf.int[1]))
  expect_equal(row_a$value.CI_upper, unname(tt_a$conf.int[2]))
})

test_that("CI bounds are NA when a group has fewer than two observations", {
  res   <- f_summary(tiny_data, columns = "value",
                     group_vars = "group", show_ci = TRUE)
  expect_true(all(is.na(res$output_df$value.CI_lower)))
  expect_true(all(is.na(res$output_df$value.CI_upper)))
})

test_that("CI columns honour show_ci = FALSE explicitly", {
  res  <- f_summary(test_data, columns = "value", show_ci = FALSE)
  expect_false("CI_lower" %in% names(res$output_df))
})

test_that("invalid conf_level raises an error", {
  expect_error(
    f_summary(test_data, columns = "value", show_ci = TRUE, conf_level = 1.5),
    regexp = "conf_level"
  )
  expect_error(
    f_summary(test_data, columns = "value", show_ci = TRUE, conf_level = 0),
    regexp = "conf_level"
  )
})

test_that("se uses non-NA count, matching sd / sqrt(n_obs)", {
  res    <- f_summary(na_data, columns = "value")
  n_obs  <- sum(!is.na(na_data$value))
  exp_se <- sd(na_data$value, na.rm = TRUE) / sqrt(n_obs)
  expect_equal(res$output_df$se, exp_se)
})

test_that("CI with NAs matches t.test on the non-missing values", {
  res <- f_summary(na_data, columns = "value", show_ci = TRUE)
  tt  <- t.test(na_data$value)  # t.test drops NAs by default
  expect_equal(res$output_df$CI_lower, unname(tt$conf.int[1]))
  expect_equal(res$output_df$CI_upper, unname(tt$conf.int[2]))
})


# =============================================================================
# 12. PRINT METHOD SMOKE TEST
# =============================================================================
test_that("print.f_summary runs without error", {
  res <- f_summary(test_data, columns = "value", group_vars = "group_a")
  expect_no_error(print(res))
})

test_that("print.f_summary with custom col_width and table_width", {
  res <- f_summary(iris, "Sepal.Length", group_vars = "Species")
  expect_no_error(print(res, col_width = 10, table_width = 70))
})


# =============================================================================
# 13. S3 dispatch cleanup
# =============================================================================


# ---------------------------------------------------------------------------
# A. Formula interface
# ---------------------------------------------------------------------------
test_that("formula interface works with positional formula", {
  expect_no_error(
    res <- suppressMessages(f_summary(Sepal.Length ~ Species, data = iris))
  )
})

test_that("formula interface works with no grouping (y ~ 1)", {
  expect_no_error(
    res <- suppressMessages(f_summary(Sepal.Length ~ 1, data = iris))
  )
})

# ---------------------------------------------------------------------------
# B. data.frame interface
# ---------------------------------------------------------------------------
test_that("data.frame interface via positional x works", {
  expect_no_error(
    res <- suppressMessages(f_summary(iris, columns = "Sepal.Length",
                           group_vars = "Species"))
  )
})

test_that("data.frame interface via named x works", {
  expect_no_error(
    res <- suppressMessages(f_summary(x = iris, columns = "Sepal.Length",
                           group_vars = "Species"))
  )
})

# ---------------------------------------------------------------------------
# C. match.call shim: f_summary(data = iris, ...)
# ---------------------------------------------------------------------------
test_that("named-only call f_summary(data = iris, columns = ...) works", {
  expect_no_error(
    res <- suppressMessages(f_summary(data = iris, columns = "Sepal.Length",
                           group_vars = "Species"))
  )
})

test_that("missing x AND data errors with informative message", {
  expect_error(
    suppressMessages(f_summary(columns = "Sepal.Length"),
    "missing"
  ))
})
