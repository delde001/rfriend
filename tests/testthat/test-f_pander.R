# Tests for f_pander() on f_summary() results.

test_that("f_pander() renders the table inside a grouped f_summary()", {
  s <- f_summary(plant_trial, columns = "height_cm", group_vars = "treatment")
  expect_no_warning(out <- utils::capture.output(f_pander(s)))
  txt <- paste(out, collapse = "\n")
  expect_false(grepl("output_df", txt, fixed = TRUE))
  expect_match(txt, "control", fixed = TRUE)
})

test_that("f_pander() renders one table per variable for an ungrouped f_summary()", {
  s <- f_summary(plant_trial[, c("height_cm", "biomass_g")])
  out <- paste(utils::capture.output(f_pander(s)), collapse = "\n")
  expect_match(out, "Variable: height_cm", fixed = TRUE)
  expect_match(out, "Variable: biomass_g", fixed = TRUE)
})
