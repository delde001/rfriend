# BUGS #33: tidyselect helpers in measure_columns / keep_cols

wide <- data.frame(
  plant = c("P1", "P2"),
  trt = c("a", "b"),
  h_w2 = c(1, 2),
  h_w4 = c(3, 4),
  stringsAsFactors = FALSE
)

test_that("starts_with() works in measure_columns without loading dplyr", {
  out <- f_long(wide, measure_columns = starts_with("h_"),
                keep_cols = c("plant", "trt"))
  expect_equal(nrow(out), 4)
  expect_equal(sort(unique(out$name)), c("h_w2", "h_w4"))
  expect_equal(names(out), c("plant", "trt", "name", "value"))
})

test_that("a helper in keep_cols works in lazy mode", {
  expect_message(out <- f_long(wide, keep_cols = !starts_with("h_")),
                 "EXCEPT: plant, trt")
  expect_equal(nrow(out), 4)
  expect_equal(sort(unique(out$name)), c("h_w2", "h_w4"))
})

test_that("names, numbers and a all_of() with a variable still work", {
  cols <- c("h_w2", "h_w4")
  a <- f_long(wide, measure_columns = c("h_w2", "h_w4"))
  b <- f_long(wide, measure_columns = 3:4)
  d <- f_long(wide, measure_columns = all_of(cols))
  expect_equal(a$value, b$value)
  expect_equal(a$value, d$value)
})

test_that("a variable that is NULL means lazy mode, as before", {
  mc <- NULL
  expect_message(out <- f_long(wide, measure_columns = mc, keep_cols = c("plant", "trt")),
                 "Lazy Mode")
  expect_equal(nrow(out), 4)
  expect_error(f_long(wide), "MUST specify 'keep_cols'")
})

test_that("labels with a helper are applied in column order", {
  out <- suppressMessages(f_long(wide, measure_columns = starts_with("h_"),
                                 category_labels = c("week 2", "week 4")))
  expect_equal(levels(out$name), c("week 2", "week 4"))
  expect_equal(as.character(out$name[out$value == 3]), "week 4")
})

test_that("a selection that matches no column stops with a clear error", {
  expect_error(f_long(wide, measure_columns = starts_with("zzz")),
               "measure_columns.*no columns")
})
