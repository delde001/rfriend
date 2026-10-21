# Tests for f_factors(): automatic detection rules and the two accepted
# spellings of unique_num_threshold (unique_num_treshold is the original).

classes <- function(df) vapply(df, function(x) class(x)[1], character(1))

test_that("automatic detection: text, and few distinct values that repeat", {
  df <- data.frame(
    txt   = rep(c("a", "b"), 6),
    three = rep(c(1, 2, 3), 4),          # 3 unique, each 4 times
    many  = seq_len(12) + 0.5            # 12 unique
  )
  cl <- classes(f_factors(df))
  expect_equal(unname(cl), c("factor", "factor", "numeric"))
})

test_that("repeats_threshold: a value must occur MORE than the threshold", {
  twice  <- data.frame(dose = c(0, 0, 10, 10, 20, 20))
  thrice <- data.frame(dose = c(0, 0, 0, 10, 10, 10))
  expect_equal(class(f_factors(twice)$dose), "numeric")
  expect_equal(class(f_factors(thrice)$dose), "factor")
  # lowering the threshold to 1 converts the twice-repeated column
  expect_equal(class(f_factors(twice, repeats_threshold = 1)$dose), "factor")
})

test_that("unique_num_threshold: fewer than the threshold becomes a factor", {
  df <- data.frame(k = rep(c(1, 2, 3), 4))
  expect_equal(class(f_factors(df, unique_num_threshold = 4)$k), "factor")
  expect_equal(class(f_factors(df, unique_num_threshold = 3)$k), "numeric")
})

test_that("both spellings are accepted and give the same result", {
  df <- data.frame(k = rep(c(1, 2, 3), 4), y = seq_len(12) + 0.5)
  new <- f_factors(df, unique_num_threshold = 3)
  old <- f_factors(df, unique_num_treshold  = 3)
  expect_identical(new, old)
  expect_equal(class(old$k), "numeric")
})

test_that("both spellings with the same value is fine, different values error", {
  df <- data.frame(k = rep(c(1, 2, 3), 4))
  expect_no_error(f_factors(df, unique_num_threshold = 3,
                            unique_num_treshold = 3))
  expect_error(f_factors(df, unique_num_threshold = 3,
                         unique_num_treshold = 5),
               "two spellings of the same argument")
})

test_that("the default is unchanged when neither spelling is given", {
  df <- data.frame(k = rep(1:7, length.out = 24), m = rep(1:8, 3))
  cl <- classes(f_factors(df))
  expect_equal(unname(cl), c("factor", "integer"))
})

test_that("f_corplot() accepts both spellings and passes them to f_factors()", {
  skip_on_cran()
  set.seed(1)
  df <- data.frame(x = rnorm(24), y = rnorm(24), k = rep(c(1, 2, 3), 8))
  run <- function(...) {
    out <- tempfile(fileext = "_CorPlot.png")
    capture.output(suppressMessages(
      f_corplot(df, factor_table = TRUE, output_type = "png",
                save_as = out, open_generated_files = FALSE, ...)))
  }
  k_line <- function(txt) grep("^k ", txt, value = TRUE)[1]
  expect_match(k_line(run()), "factor")
  expect_match(k_line(run(unique_num_threshold = 3)), "numeric")
  expect_match(k_line(run(unique_num_treshold  = 3)), "numeric")
  expect_error(run(unique_num_threshold = 3, unique_num_treshold = 5),
               "two spellings of the same argument")
})
