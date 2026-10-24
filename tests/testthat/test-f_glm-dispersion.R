# test-f_glm-dispersion.R
# BUGS #35: the dispersion verdict must reach the console and the result,
# not only the Word/PDF report. A significant overdispersion in a Poisson or
# binomial model warns; the verdict is stored in $dispersion and printed.
# =============================================================================

wk6 <- subset(plant_trial, time_weeks == 6)

# collect warnings, keep the result
glm_warnings <- function(...) {
  w <- character(0)
  res <- withCallingHandlers(
    suppressMessages(f_glm(...)),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
  list(result = res, warnings = w)
}

test_that("overdispersed Poisson model warns, stores and prints the verdict", {
  out <- glm_warnings(chloroplast_count ~ treatment, data = wk6, family = "poisson")
  d <- out$result$chloroplast_count$dispersion
  expect_type(d, "list")
  expect_identical(d$type, "DHARMa")
  expect_true(d$significant)
  expect_identical(d$direction, "overdispersion")
  expect_equal(d$p.value, 0.024)
  expect_equal(d$statistic, 1.7906)
  expect_true(any(grepl("f_glm: overdispersion detected for response 'chloroplast_count'",
                        out$warnings, fixed = TRUE)))
  expect_true(any(grepl('family = "quasipoisson"', out$warnings, fixed = TRUE)))
  printed <- capture.output(print(out$result))
  expect_true(any(grepl("--- Dispersion test ---", printed, fixed = TRUE)))
  expect_true(any(grepl("Significant overdispersion (DHARMa ratio = 1.7906, p = 0.024)",
                        printed, fixed = TRUE)))
})

test_that("the stored verdict agrees with the report text", {
  res <- suppressWarnings(suppressMessages(
    f_glm(chloroplast_count ~ treatment, data = wk6, family = "poisson",
          output_type = "rmd")))
  rmd <- paste(res$rmd, collapse = "\n")
  expect_true(grepl("The dispersion test is **significant** (p = 0.024", rmd, fixed = TRUE))
  expect_true(res$chloroplast_count$dispersion$significant)
})

test_that("a Poisson model without overdispersion does not warn", {
  set.seed(1)
  d <- data.frame(g = factor(rep(c("a", "b", "c"), each = 30)))
  d$y <- stats::rpois(90, lambda = c(5, 8, 12)[as.integer(d$g)])
  out <- glm_warnings(y ~ g, data = d, family = "poisson")
  v <- out$result$y$dispersion
  expect_identical(v$type, "DHARMa")
  expect_false(v$significant)
  expect_false(any(grepl("overdispersion detected", out$warnings, fixed = TRUE)))
  printed <- capture.output(print(out$result))
  expect_true(any(grepl("Not significant (DHARMa ratio =", printed, fixed = TRUE)))
})

test_that("0/1 data: not applicable, no dispersion warning", {
  out <- glm_warnings(infected ~ treatment, data = wk6, family = "binomial", event = "yes")
  v <- out$result$infected$dispersion
  expect_identical(v$type, "not applicable")
  expect_false(v$significant)
  expect_false(any(grepl("overdispersion detected", out$warnings, fixed = TRUE)))
  printed <- capture.output(print(out$result))
  expect_true(any(grepl("Not applicable: 0/1 (Bernoulli) data", printed, fixed = TRUE)))
})

test_that("quasipoisson: the quasi-dispersion parameter, no dispersion warning", {
  out <- glm_warnings(chloroplast_count ~ treatment, data = wk6, family = "quasipoisson")
  v <- out$result$chloroplast_count$dispersion
  expect_identical(v$type, "quasi-dispersion")
  expect_gt(v$statistic, 1)
  expect_false(any(grepl("overdispersion detected", out$warnings, fixed = TRUE)))
  printed <- capture.output(print(out$result))
  expect_true(any(grepl("Quasi-dispersion parameter phi =", printed, fixed = TRUE)))
})

test_that("dispersion_test = FALSE still stores, prints and warns", {
  out <- glm_warnings(chloroplast_count ~ treatment, data = wk6, family = "poisson",
                      dispersion_test = FALSE)
  expect_true(out$result$chloroplast_count$dispersion$significant)
  expect_true(any(grepl("overdispersion detected", out$warnings, fixed = TRUE)))
})
