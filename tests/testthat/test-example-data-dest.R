# BUGS #31: f_example_data(dest = ...) with a folder that does not exist yet.
# A dest without a file extension (or ending in "/") is a folder: it is
# created and the original file name is kept. A dest with a file extension is
# a file path, and a missing folder stops with a clear message.

in_temp <- function(code) {
  d <- tempfile("exdest_"); dir.create(d)
  old <- setwd(d); on.exit({ setwd(old); unlink(d, recursive = TRUE) }, add = TRUE)
  force(code)
}

test_that("dest = 'data' (no folder yet) creates the folder and copies into it", {
  in_temp({
    expect_message(out <- f_example_data("plant_trial_raw.csv", dest = "data"),
                   "Created folder 'data'")
    expect_true(dir.exists("data"))
    expect_true(file.exists(file.path("data", "plant_trial_raw.csv")))
    expect_identical(out, file.path("data", "plant_trial_raw.csv"))
    expect_equal(nrow(utils::read.csv(out)), 60L)
  })
})

test_that("dest with a trailing slash and nested folders works the same way", {
  in_temp({
    out <- suppressMessages(f_example_data("plant_trial_raw.csv", dest = "a/b/"))
    expect_true(file.exists(file.path("a", "b", "plant_trial_raw.csv")))
    expect_identical(out, file.path("a/b", "plant_trial_raw.csv"))
  })
})

test_that("an existing folder keeps working, without a 'Created' message", {
  in_temp({
    dir.create("data")
    msgs <- character(0)
    withCallingHandlers(f_example_data("plant_trial_raw.csv", dest = "data"),
      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
    expect_false(any(grepl("Created folder", msgs)))
    expect_true(file.exists(file.path("data", "plant_trial_raw.csv")))
  })
})

test_that("a file path copies under the new name; a missing folder stops clearly", {
  in_temp({
    dir.create("data")
    out <- suppressMessages(f_example_data("plant_trial_raw.csv", dest = "data/my_copy.csv"))
    expect_identical(out, "data/my_copy.csv")
    expect_true(file.exists("data/my_copy.csv"))
    expect_error(f_example_data("plant_trial_raw.csv", dest = "missing/my_copy.csv"),
                 "The folder 'missing' does not exist. Create it first with dir.create",
                 fixed = TRUE)
    expect_false(file.exists("missing"))
  })
})

test_that("dest must be a single path", {
  expect_error(f_example_data("plant_trial_raw.csv", dest = c("a", "b")),
               "'dest' must be a single folder or file path.", fixed = TRUE)
})
