# test-f_conditional_round.R
# BUGS #37: a rounded table is text, so it must print its rounded values.
# Classes such as "anova" are dropped; na_string is used for data frames too.
# =============================================================================

wk6 <- subset(plant_trial, time_weeks == 6)

test_that("a rounded ANOVA table is a plain data frame and prints its values", {
  out <- suppressMessages(f_aov(height_cm ~ treatment, data = wk6))
  ar  <- f_conditional_round(out$height_cm$aov_summary)
  expect_identical(class(ar), "data.frame")
  printed <- capture.output(print(ar))
  expect_true(any(grepl("1556.868", printed, fixed = TRUE)))
  expect_true(any(grepl("28.391", printed, fixed = TRUE)))
  expect_true(any(grepl("1.211e-06", printed, fixed = TRUE)))
  expect_true(any(grepl("1556.868", capture.output(f_pander(ar)), fixed = TRUE)))
})

test_that("tibbles stay tibbles; other classes are dropped", {
  tb <- structure(data.frame(x = c(1.23456, 2)), class = c("rstatix_test", "tbl_df", "tbl", "data.frame"))
  expect_identical(class(f_conditional_round(tb)), c("tbl_df", "tbl", "data.frame"))
  df <- data.frame(x = c(1.23456, 2))
  expect_identical(class(f_conditional_round(df)), "data.frame")
})

test_that("na_string is used for the columns of a data frame", {
  df <- data.frame(a = c(1.23456, NA), b = c("x", "y"))
  expect_identical(f_conditional_round(df, na_string = "n/a")$a, c("1.235", "n/a"))
  expect_identical(f_conditional_round(df)$a, c("1.235", "-"))
  expect_identical(f_conditional_round(c(1.23456, NA), na_string = "n/a"), c("1.235", "n/a"))
})
