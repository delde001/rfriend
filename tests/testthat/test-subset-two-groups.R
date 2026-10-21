# BUGS #29: `subset` can pick two groups of a factor with more levels in
# f_t_test() and f_wilcox_test(). The two-group check runs after the subset.

wk6_data <- function() subset(plant_trial, time_weeks == 6)

test_that("f_t_test: subset picks two of four groups", {
  wk6 <- wk6_data()
  two <- subset(wk6, treatment %in% c("control", "high_N"))
  res_sub <- suppressMessages(suppressWarnings(f_t_test(
    height_cm ~ treatment, data = wk6,
    subset = treatment %in% c("control", "high_N"))))
  res_two <- suppressMessages(suppressWarnings(f_t_test(height_cm ~ treatment, data = two)))
  expect_equal(res_sub$height_cm$t_test$p.value, res_two$height_cm$t_test$p.value)
  expect_equal(unname(res_sub$height_cm$t_test$statistic),
               unname(res_two$height_cm$t_test$statistic))
})

test_that("f_wilcox_test: subset picks two of four groups", {
  wk6 <- wk6_data()
  two <- subset(wk6, treatment %in% c("control", "high_N"))
  res_sub <- suppressMessages(suppressWarnings(f_wilcox_test(
    biomass_g ~ treatment, data = wk6,
    subset = treatment %in% c("control", "high_N"))))
  res_two <- suppressMessages(suppressWarnings(f_wilcox_test(biomass_g ~ treatment, data = two)))
  get_p <- function(res) res[[1]]$wilcox_test$p.value
  expect_false(is.null(get_p(res_sub)))
  expect_equal(get_p(res_sub), get_p(res_two))
})

test_that("more than two groups still stops, naming the levels", {
  wk6 <- wk6_data()
  expect_error(suppressMessages(f_t_test(height_cm ~ treatment, data = wk6)),
               "exactly 2 levels for a t-test; 'treatment' has 4 \\(control, drought, high_N, low_N\\)")
  expect_error(suppressMessages(f_wilcox_test(height_cm ~ treatment, data = wk6,
                                              subset = treatment != "drought")),
               "exactly 2 levels for a Wilcoxon test; 'treatment' has 3")
})
