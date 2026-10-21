# The example files that ship in inst/extdata, reached with f_example_data().

test_that("plant_trial_raw.xlsx: raw data on Sheet1, codebook on meta_data", {
  skip_if_not_installed("readxl")
  f <- f_example_data("plant_trial_raw.xlsx")
  expect_identical(readxl::excel_sheets(f), c("Sheet1", "meta_data"))

  # Sheet1 is the same raw data as the CSV (readxl trims the stray leading
  # space of " high_N", so compare with trimmed text)
  xl  <- as.data.frame(readxl::read_excel(f, sheet = "Sheet1"))
  csv <- utils::read.csv(f_example_data("plant_trial_raw.csv"),
                         check.names = FALSE, stringsAsFactors = FALSE)
  expect_identical(names(xl), names(csv))
  expect_equal(nrow(xl), 60L)
  expect_identical(xl$Treatment, trimws(csv$Treatment))
  expect_equal(xl$`Height (cm)`, csv$`Height (cm)`)
  expect_equal(sum(is.na(xl$`Chloroplast count`)), 3L)

  # meta_data describes every column of Sheet1, in the same order
  meta <- readxl::read_excel(f, sheet = "meta_data")
  expect_identical(names(meta), c("variable", "description", "unit", "values"))
  expect_identical(meta$variable, names(xl))
  expect_false(anyNA(meta$description))
  expect_identical(meta$values[meta$variable == "Treatment"],
                   "control, low_N, high_N, drought")
})
