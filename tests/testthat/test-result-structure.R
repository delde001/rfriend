# test-result-structure.R
# Uniform result structure (rfriend 3.2.1):
# - f_kruskal_test(): results per response, then per predictor (out$y$x)
# - f_glm(): post hoc as post_hoc[[term]] with emmeans, pairs, cld, cld_text,
#   caution, as in f_aov(), f_lm() and f_lmer()
# - the old names ("y_x", "posthoc") stop with the new path instead of
#   silently returning NULL
# =============================================================================

wk6 <- subset(plant_trial, time_weeks == 6)

test_that("f_kruskal_test() stores results per response, then per predictor", {
  k <- suppressMessages(f_kruskal_test(biomass_g + height_cm ~ treatment + block,
                                       data = wk6, diagnostic_plots = FALSE))
  expect_true(all(c("biomass_g", "height_cm") %in% names(k)))
  expect_identical(names(k$biomass_g), c("treatment", "block"))
  expect_s3_class(k$biomass_g$treatment$kruskal.test, "htest")
  expect_s3_class(k$height_cm$block$Boxplot, "ggplot")
  expect_true(is.data.frame(k[["biomass_g"]][["treatment"]][["summary_table"]]))
})

test_that("old f_kruskal_test() names stop with the new path", {
  k <- suppressMessages(f_kruskal_test(biomass_g ~ treatment, data = wk6,
                                       diagnostic_plots = FALSE))
  expect_error(k$biomass_g_treatment, "Use x\\$biomass_g\\$treatment instead")
  expect_error(k[["biomass_g_treatment"]], "Use x\\$biomass_g\\$treatment instead")
  expect_null(k$not_a_part)
})

test_that("f_kruskal_test() print, plot and Excel work with the nested structure", {
  k <- suppressMessages(f_kruskal_test(biomass_g + height_cm ~ treatment + block,
                                       data = wk6))
  printed <- capture.output(print(k))
  expect_equal(sum(grepl("^Results of testing", printed)), 4)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(plot(k, which = "Boxplot"))
  skip_if_not_installed("readxl")
  f <- tempfile(fileext = ".xlsx")
  suppressMessages(f_kruskal_test(biomass_g + height_cm ~ treatment + block, data = wk6,
                                  output_type = "excel", save_as = f,
                                  open_generated_files = FALSE))
  expect_identical(readxl::excel_sheets(f),
                   c("biomass_g_treatment", "biomass_g_block",
                     "height_cm_treatment", "height_cm_block"))
  f1 <- tempfile(fileext = ".xlsx")
  suppressMessages(f_kruskal_test(biomass_g ~ treatment, data = wk6,
                                  output_type = "excel", save_as = f1,
                                  open_generated_files = FALSE))
  expect_identical(readxl::excel_sheets(f1), "biomass_g")
})

test_that("f_glm() stores the post hoc as post_hoc[[term]], like f_aov()", {
  g <- suppressWarnings(suppressMessages(
    f_glm(chloroplast_count ~ treatment, data = wk6, family = "poisson")))
  a <- suppressMessages(f_aov(height_cm ~ treatment, data = wk6))
  expect_identical(names(g$chloroplast_count$post_hoc), "treatment")
  expect_true(all(names(a$height_cm$post_hoc$treatment) %in%
                    names(g$chloroplast_count$post_hoc$treatment)))
  ph <- g$chloroplast_count$post_hoc$treatment
  expect_s4_class(ph$emmeans, "emmGrid")
  expect_true(is.data.frame(ph$cld))
  expect_true("Letter" %in% names(ph$cld))
  expect_equal(round(ph$cld$rate[ph$cld$treatment == "drought"], 1), 28.8)
  expect_type(ph$cld_text, "character")
})

test_that("f_glm() with two factors keys its one cell table by both", {
  g2 <- suppressWarnings(suppressMessages(
    f_glm(chloroplast_count ~ block + treatment, data = wk6, family = "poisson")))
  expect_identical(names(g2$chloroplast_count$post_hoc), "block:treatment")
  g0 <- suppressWarnings(suppressMessages(
    f_glm(chloroplast_count ~ 1, data = wk6, family = "poisson")))
  expect_null(g0$chloroplast_count$post_hoc)
})

test_that("the old f_glm() name 'posthoc' stops with the new path", {
  g <- suppressWarnings(suppressMessages(
    f_glm(chloroplast_count ~ treatment, data = wk6, family = "poisson")))
  expect_error(g$chloroplast_count$posthoc, "stored as 'post_hoc'")
  expect_error(g$chloroplast_count[["posthoc"]], "stored as 'post_hoc'")
  printed <- capture.output(print(g$chloroplast_count))
  expect_false(any(grepl("f_glm_response", printed)))
  expect_true(any(grepl("Post hoc Comparisons", capture.output(print(g)))))
})
