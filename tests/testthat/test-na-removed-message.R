# BUGS #28: the test and model functions leave out rows with a missing value
# (NA) in the formula columns, as R does. They now say so in the console
# with one message: "<n> rows with missing values (NA) removed from analysis
# (NA in: <columns>)." No message when nothing was removed.

na_data <- function() {
  d <- subset(plant_trial, time_weeks == 6)
  d$height_cm[1:3] <- NA   # three control plants
  d
}

# Collect the NA messages of one call; all other output is hidden.
na_messages <- function(expr) {
  got <- character(0)
  withCallingHandlers(
    utils::capture.output(eval.parent(substitute(expr))),
    message = function(m) {
      if (grepl("(NA)", conditionMessage(m), fixed = TRUE))
        got <<- c(got, trimws(conditionMessage(m)))
      invokeRestart("muffleMessage")
    },
    warning = function(w) invokeRestart("muffleWarning"))
  got
}

expected <- "3 rows with missing values (NA) removed from analysis (NA in: height_cm)."

test_that("f_aov, f_lm and f_kruskal_test report removed NA rows once", {
  d <- na_data()
  expect_identical(na_messages(f_aov(height_cm ~ treatment, data = d)), expected)
  expect_identical(na_messages(f_lm(height_cm ~ chloroplast_count, data = d)), expected)
  expect_identical(na_messages(f_kruskal_test(height_cm ~ treatment, data = d,
                                              diagnostic_plots = FALSE,
                                              effect_plots = FALSE)), expected)
})

test_that("f_t_test and f_wilcox_test report removed NA rows", {
  two <- subset(na_data(), treatment %in% c("control", "high_N"))
  expect_identical(na_messages(f_t_test(height_cm ~ treatment, data = two)), expected)
  expect_identical(na_messages(f_wilcox_test(height_cm ~ treatment, data = two)), expected)
})

test_that("rows removed by subset are not counted as missing", {
  d <- na_data()
  expect_identical(
    na_messages(f_aov(height_cm ~ treatment, data = d, subset = treatment != "drought")),
    expected)
})

test_that("one row is singular; several columns are all named", {
  d <- subset(plant_trial, time_weeks == 6)
  d$height_cm[1]  <- NA
  expect_identical(na_messages(f_aov(height_cm ~ treatment, data = d)),
    "1 row with missing values (NA) removed from analysis (NA in: height_cm).")
  d$biomass_g[20] <- NA
  expect_identical(na_messages(f_aov(height_cm + biomass_g ~ treatment, data = d)),
    "2 rows with missing values (NA) removed from analysis (NA in: height_cm, biomass_g).")
})

test_that("no message without missing values", {
  d <- subset(plant_trial, time_weeks == 6)
  expect_length(na_messages(f_aov(height_cm ~ treatment, data = d)), 0)
  expect_length(na_messages(f_t_test(height_cm ~ treatment,
    data = subset(d, treatment %in% c("control", "high_N")))), 0)
})

test_that("f_glm reports removed NA rows per response", {
  d <- na_data()
  expect_identical(
    na_messages(f_glm(height_cm + chloroplast_count ~ treatment, data = d,
                      family = gaussian)),
    "3 rows with missing values (NA) removed from analysis of height_cm (NA in: height_cm).")
})

test_that("f_lmer reports removed NA rows", {
  pt <- plant_trial
  pt$height_cm[c(1, 5)] <- NA
  expect_identical(
    na_messages(f_lmer(height_cm ~ treatment + (1 | plant_id), data = pt)),
    "2 rows with missing values (NA) removed from analysis (NA in: height_cm).")
})

test_that("f_friedman reports rows removed for NA in the block column", {
  pt <- plant_trial
  pt$time_f <- factor(pt$time_weeks)
  pt <- pt[!(pt$plant_id == "P01" & pt$time_weeks != 2), ]  # P01: one row left
  pt$plant_id[pt$plant_id == "P01"] <- NA
  expect_identical(
    na_messages(f_friedman(height_cm ~ time_f | plant_id, data = pt,
                           diagnostic_plots = FALSE, effect_plots = FALSE)),
    "1 row with missing values (NA) removed from analysis of height_cm (NA in: plant_id).")
})

test_that("f_t_test / f_wilcox_test reports do not count subset rows as missing", {
  d <- subset(plant_trial, time_weeks == 6 & treatment %in% c("control", "high_N"))
  d$height_cm[1:2] <- NA   # blocks B1 and B2 of control
  removed_lines <- function(res) {
    grep("Removed", strsplit(res$rmd, "\n")[[1]], value = TRUE)
  }
  t_out <- suppressMessages(f_t_test(height_cm ~ treatment, data = d,
                                     subset = block != "B4", output_type = "rmd"))
  w_out <- suppressMessages(f_wilcox_test(height_cm ~ treatment, data = d,
                                          subset = block != "B4", output_type = "rmd"))
  expect_true(length(removed_lines(t_out)) > 0)
  expect_true(all(grepl("Removed 2 rows with missing values", removed_lines(t_out))))
  expect_true(all(grepl("Removed 2 rows with missing values", removed_lines(w_out))))
})
