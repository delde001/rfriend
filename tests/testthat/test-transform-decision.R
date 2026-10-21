# Tests for the transformation convention (rfriend 3.2.1), shared by f_aov(),
# f_lm() and f_t_test():
#   * a Box-Cox / bestNormalize transformation is kept only when it improves
#     the residual check that triggered it (force_transformation always keeps)
#   * plain element = untransformed result; transformed_<name> = transformed
#     result or NULL; Response_Transformed is TRUE exactly when it is not NULL
#   * print() and the report name the scale of the reported table

quiet_out <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  invisible(utils::capture.output(res <- suppressWarnings(suppressMessages(expr))))
  res
}
printed <- function(x) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  paste(utils::capture.output(suppressWarnings(suppressMessages(print(x)))),
        collapse = "\n")
}
wk6_td <- subset(plant_trial, time_weeks == 6)
two_td <- subset(plant_trial, treatment %in% c("control", "high_N"))
# Right-skewed responses where Box-Cox is kept (independent of plant_trial,
# whose biomass_g changed in version 2 of the dataset)
skewed_td <- local({
  set.seed(11)
  g <- rep(c("a", "b"), each = 20)
  data.frame(g = factor(g), x = round(rnorm(40, 30, 6)),
             y = round(exp(rnorm(40, ifelse(g == "a", 3, 3.4), 0.6)), 2))
})
rejected_td <- local({  # Box-Cox lowers Shapiro p slightly: 0.00802 -> 0.00801
  set.seed(12)
  y <- c(rnorm(15, 50, 5), rnorm(15, 55, 5))
  y[c(3, 20)] <- y[c(3, 20)] + c(25, -20)
  data.frame(y = y, g = rep(c("a", "b"), each = 15))
})

# ---------------------------------------------------------------- f_aov ----

test_that("f_aov: untransformed fit has transformed_aov_summary NULL and flag FALSE", {
  out <- quiet_out(f_aov(height_cm ~ treatment, data = plant_trial))
  s <- out$height_cm
  expect_false(s$Response_Transformed)
  expect_true("transformed_aov_summary" %in% names(s))
  expect_null(s$transformed_aov_summary)
  expect_identical(s$anova_heading, "ANOVA on height_cm (untransformed)")
  expect_match(printed(out), "ANOVA on height_cm (untransformed)", fixed = TRUE)
})

test_that("f_aov: transformed fit has both tables; print shows the transformed one", {
  out <- quiet_out(f_aov(biomass_g ~ treatment, data = plant_trial))
  s <- out$biomass_g
  expect_true(s$Response_Transformed)
  expect_false(is.null(s$transformed_aov_summary))
  expect_equal(round(s$transformed_aov_summary["treatment", "F value"], 2), 4.09)
  expect_equal(round(s$aov_summary["treatment", "F value"], 2), 4.08)
  expect_identical(s$anova_heading,
                   "ANOVA on Box-Cox transformed biomass_g (lambda = -0.019)")
  txt <- printed(out)
  expect_match(txt, "ANOVA on Box-Cox transformed biomass_g (lambda = -0.019)", fixed = TRUE)
  expect_match(txt, "4.0926", fixed = TRUE)
  expect_false(grepl("4.0808", txt, fixed = TRUE))
  rmd <- quiet_out(f_aov(biomass_g ~ treatment, data = plant_trial,
                         output_type = "rmd"))$rmd
  expect_match(rmd, "## ANOVA on Box-Cox transformed biomass_g (lambda = -0.019)", fixed = TRUE)
})

test_that("f_aov: RCBD - Box-Cox does not improve normality, so it is not kept", {
  out <- quiet_out(f_aov(height_cm ~ block + treatment, data = wk6_td,
                         post_hoc = "treatment"))
  s <- out$height_cm
  expect_false(s$Response_Transformed)
  expect_null(s$transformed_aov_summary)
  expect_match(s$transformation_rejected$reason,
               "did not improve the normality of the residuals", fixed = TRUE)
  expect_lt(s$transformation_rejected$normality_p_after,
            s$transformation_rejected$normality_p_before)
  # the reported (printed) table is the untransformed one: treatment F = 44.6
  expect_equal(round(s$aov_summary["treatment", "F value"], 1), 44.6)
  txt <- printed(out)
  expect_match(txt, "44.6136", fixed = TRUE)
  expect_match(txt, "ANOVA on height_cm (untransformed)", fixed = TRUE)
  rmd <- quiet_out(f_aov(height_cm ~ block + treatment, data = wk6_td,
                         post_hoc = "treatment", output_type = "rmd"))$rmd
  expect_match(rmd, "Transformation not kept.", fixed = TRUE)
})

test_that("print.f_aov: residual warning does not advise enabling a rejected or active transformation", {
  # captures print() warnings as text
  print_warnings <- function(x) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    w <- character(0)
    withCallingHandlers(
      invisible(utils::capture.output(suppressMessages(print(x)))),
      warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
    paste(w, collapse = "\n")
  }
  # tried and rejected (RCBD, Shapiro p = 0.0375)
  rej <- quiet_out(f_aov(height_cm ~ block + treatment, data = wk6_td,
                         post_hoc = "treatment"))
  w_rej <- print_warnings(rej)
  expect_match(w_rej, "NOT** normally distributed", fixed = TRUE)
  expect_match(w_rej, "A transformation was tried but not kept (see above)", fixed = TRUE)
  expect_no_match(w_rej, "nable the transformation", fixed = TRUE)
  # switched off by the user: the advice to enable it stays
  off <- quiet_out(f_aov(height_cm ~ block + treatment, data = wk6_td,
                         post_hoc = "treatment", transformation = FALSE))
  w_off <- print_warnings(off)
  expect_match(w_off, "Enable the transformation option (`transformation = TRUE`)", fixed = TRUE)
})

test_that("f_aov: force_transformation keeps the transformation regardless", {
  out <- quiet_out(f_aov(height_cm ~ block + treatment, data = wk6_td,
                         post_hoc = "treatment",
                         force_transformation = "height_cm"))
  s <- out$height_cm
  expect_true(s$Response_Transformed)
  expect_equal(round(s$transformed_aov_summary["treatment", "F value"], 1), 42.5)
  expect_null(s$transformation_rejected)
})

test_that("f_aov: Response_Transformed is TRUE exactly when transformed_aov_summary is not NULL", {
  fits <- list(
    quiet_out(f_aov(height_cm ~ treatment, data = plant_trial))$height_cm,
    quiet_out(f_aov(biomass_g ~ treatment, data = plant_trial))$biomass_g,
    quiet_out(f_aov(height_cm ~ block + treatment, data = wk6_td))$height_cm,
    quiet_out(f_aov(biomass_g ~ treatment, data = plant_trial,
                    transformation = "bestnormalize"))$biomass_g)
  for (s in fits)
    expect_identical(isTRUE(s$Response_Transformed), !is.null(s$transformed_aov_summary))
})

# ----------------------------------------------------------------- f_lm ----

test_that("f_lm: plain names untransformed, transformed_* transformed or NULL", {
  out <- quiet_out(f_lm(height_cm ~ block + treatment, data = wk6_td))
  s <- out$height_cm
  expect_false(s$Response_Transformed)          # tried, not kept
  expect_false(is.null(s$transformation_rejected))
  for (nm in c("transformed_anova", "transformed_summary", "transformed_coefficients")) {
    expect_true(nm %in% names(s))
    expect_null(s[[nm]])
  }
  ref <- car::Anova(lm(height_cm ~ block + treatment, data = wk6_td), type = 2)
  expect_equal(s$anova["treatment", "F value"], ref["treatment", "F value"])
  expect_match(printed(out), "Type II Analysis of Variance on height_cm (untransformed)",
               fixed = TRUE)
})

test_that("f_lm: transformed fit keeps both, print shows the transformed one", {
  out <- quiet_out(f_lm(y ~ g + x, data = skewed_td))
  s <- out$y
  expect_true(s$Response_Transformed)
  expect_false(is.null(s$transformed_anova))
  ref_u <- car::Anova(lm(y ~ g + x, data = skewed_td), type = 2)
  expect_equal(s$anova["x", "F value"], ref_u["x", "F value"])
  expect_equal(s$transformed_anova["x", "F value"],
               car::Anova(s$transformed_lm_fit, type = 2)["x", "F value"])
  expect_equal(s$summary$coefficients, summary(s$lm_fit)$coefficients)
  expect_equal(as.matrix(s$transformed_coefficients),
               summary(s$transformed_lm_fit)$coefficients)
  txt <- printed(out)
  expect_match(txt, "Type II Analysis of Variance on Box-Cox transformed y (lambda = ",
               fixed = TRUE)
  f_tr <- s$transformed_anova["x", "F value"]
  expect_match(txt, as.character(trunc(f_tr * 1000) / 1000), fixed = TRUE)
  expect_identical(isTRUE(s$Response_Transformed), !is.null(s$transformed_anova))
})

# ------------------------------------------------------------- f_t_test ----

test_that("f_t_test: untransformed test has transformed_t_test NULL", {
  out <- quiet_out(f_t_test(height_cm ~ treatment, data = two_td))
  s <- out$height_cm
  expect_false(s$Response_Transformed)
  expect_true("transformed_t_test" %in% names(s))
  expect_null(s$transformed_t_test)
  expect_match(printed(out), "on height_cm (untransformed)", fixed = TRUE)
})

test_that("f_t_test: transformed test, deprecated alias, heading names the scale", {
  out <- quiet_out(f_t_test(y ~ g, data = skewed_td))
  s <- out$y
  expect_true(s$Response_Transformed)
  expect_s3_class(s$transformed_t_test, "htest")
  expect_identical(s$t_test_transformed, s$transformed_t_test)
  txt <- printed(out)
  expect_match(txt, "on Box-Cox transformed y (lambda = ", fixed = TRUE)
  expect_match(txt, format(round(s$transformed_t_test$statistic, 3), nsmall = 3), fixed = TRUE)
})

test_that("f_t_test: a transformation that does not improve normality is not kept", {
  out <- quiet_out(f_t_test(y ~ g, data = rejected_td))
  s <- out$y
  expect_false(s$Response_Transformed)
  expect_null(s$transformed_t_test)
  expect_match(s$transformation_rejected$reason, "did not improve", fixed = TRUE)
  forced <- quiet_out(f_t_test(y ~ g, data = rejected_td, force_transformation = "y"))
  expect_true(forced$y$Response_Transformed)
})
