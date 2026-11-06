# =============================================================================
# Tests for f_rename_vector()
# =============================================================================

test_that("character vector: renames matches, keeps the rest (unchanged behaviour)", {
  expect_identical(
    f_rename_vector(c("Species", "Weight", "L"), c(Species = "New_species_name", L = "Length_cm")),
    c("New_species_name", "Weight", "Length_cm"))
  expect_identical(f_rename_vector(NULL, c(a = "b")), character(0))
  expect_identical(f_rename_vector(c("a", NA), c(a = "A")), c("A", NA))
})

test_that("trim = TRUE removes leading and trailing spaces first", {
  x <- c(" high_N", "Low N", "control")
  expect_identical(f_rename_vector(x, c("Low N" = "low_N"), trim = TRUE),
                   c("high_N", "low_N", "control"))
  expect_identical(f_rename_vector(x, c("Low N" = "low_N")),
                   c(" high_N", "low_N", "control"))
})

test_that("factor: levels renamed, order kept, merged when two get the same name", {
  f   <- factor(c("b", "a", "c"), levels = c("c", "b", "a"))
  out <- f_rename_vector(f, c(b = "B"))
  expect_s3_class(out, "factor")
  expect_identical(levels(out), c("c", "B", "a"))
  expect_identical(as.character(out), c("B", "a", "c"))
  m <- f_rename_vector(factor(c("Low N", "low_N")), c("Low N" = "low_N"))
  expect_identical(levels(m), "low_N")
  o <- f_rename_vector(factor(c("a", "b"), ordered = TRUE), c(a = "A"))
  expect_true(is.ordered(o))
})

test_that("a name that matches nothing warns, and can be silenced", {
  x <- c("control", "Low N")
  expect_warning(out <- f_rename_vector(x, c("Low_N" = "low_N")),
                 "not found in 'vector', so not renamed: \"Low_N\"")
  expect_identical(out, x)
  expect_silent(f_rename_vector(x, c("Low_N" = "low_N"), warn_unmatched = FALSE))
})

test_that("an unnamed map warns and changes nothing", {
  expect_warning(out <- f_rename_vector(c("a", "b"), c("x", "y")), "has no names")
  expect_identical(out, c("a", "b"))
})

test_that("other types give a clear error", {
  expect_error(f_rename_vector(1:3, c("1" = "a")),
               "must be a character vector or a factor, not integer")
  expect_error(f_rename_vector("a", c(a = "b"), trim = NA), "'trim' must be TRUE or FALSE")
})
