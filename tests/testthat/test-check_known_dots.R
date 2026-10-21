# Tests for check_known_dots(): unknown or misspelled arguments must stop
# with a clear error, while every existing interface keeps working.
#
# Place in tests/testthat/. Nothing here writes files: tests use
# output_type = "console" (or "rmd" for f_boxplot, which keeps the output
# in memory).

# ---- Helpers ----------------------------------------------------------------

# Run quietly: rfriend prints reports to the console, and some functions
# draw base-graphics plots. A null device keeps plots from going anywhere
# (no Rplots.pdf left behind) and is always closed again.
quiet <- function(expr) {
  grDevices::pdf(NULL)
  dev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(dev), add = TRUE)
  invisible(utils::capture.output(suppressMessages(expr)))
}

# Pattern for the error message raised by check_known_dots().
unknown_arg <- function(fun_name) {
  paste0("Unknown argument\\(s\\) in ", fun_name, "\\(\\)")
}

two_groups <- droplevels(iris[iris$Species != "setosa", ])
friedman_df <- data.frame(
  y = c(5.1, 4.9, 6.2, 5.8, 6.0, 5.5, 4.7, 5.2, 6.6, 5.9, 6.3, 5.0),
  treatment = factor(rep(c("A", "B", "C"), times = 4)),
  block = factor(rep(1:4, each = 3))
)

# ---- f_t_test ---------------------------------------------------------------

test_that("f_t_test errors on misspelled arguments with a suggestion", {
  expect_error(
    f_t_test(Sepal.Length ~ Species, data = two_groups,
             alternatve = "less", output_type = "console"),
    paste0(unknown_arg("f_t_test"), ".*did you mean 'alternative'")
  )
  # Vector interface routes through the formula method and is covered too.
  expect_error(
    f_t_test(two_groups$Sepal.Length, mu = 6, bogus = 1,
             output_type = "console"),
    unknown_arg("f_t_test")
  )
})

test_that("f_t_test still works for valid calls", {
  expect_no_error(quiet(
    f_t_test(Sepal.Length ~ Species, data = two_groups,
             alternative = "less", output_type = "console")))
  expect_no_error(quiet(
    f_t_test(Sepal.Length ~ Species, data = two_groups,
             subset = Sepal.Width > 2.5, output_type = "console")))
  expect_no_error(quiet(
    f_t_test(two_groups$Sepal.Length, mu = 6, output_type = "console")))
})

# ---- f_wilcox_test ----------------------------------------------------------

test_that("f_wilcox_test errors on misspelled arguments", {
  expect_error(
    f_wilcox_test(Sepal.Length ~ Species, data = two_groups,
                  pared = TRUE, output_type = "console"),
    paste0(unknown_arg("f_wilcox_test"), ".*did you mean 'paired'")
  )
  expect_error(
    f_wilcox_test(two_groups$Sepal.Length, mu = 6, bogus = 1,
                  output_type = "console"),
    unknown_arg("f_wilcox_test")
  )
})

test_that("f_wilcox_test still works for valid calls", {
  expect_no_error(quiet(
    f_wilcox_test(Sepal.Length ~ Species, data = two_groups,
                  output_type = "console")))
  expect_no_error(quiet(
    f_wilcox_test(Sepal.Length ~ Species, data = two_groups,
                  subset = Sepal.Width > 2.5, output_type = "console")))
  # Vector interface passes the internal .original_name through the dots.
  expect_no_error(quiet(
    f_wilcox_test(two_groups$Sepal.Length, mu = 6, output_type = "console")))
})

# ---- f_kruskal_test ---------------------------------------------------------

test_that("f_kruskal_test errors on misspelled arguments", {
  expect_error(
    f_kruskal_test(Sepal.Length ~ Species, data = iris,
                   ajust = "holm", output_type = "console"),
    paste0(unknown_arg("f_kruskal_test"), ".*did you mean 'adjust'")
  )
})

test_that("f_kruskal_test still works for valid calls", {
  expect_no_error(quiet(
    f_kruskal_test(Sepal.Length ~ Species, data = iris,
                   adjust = "holm", output_type = "console")))
  expect_no_error(quiet(
    f_kruskal_test(Sepal.Length ~ Species, data = iris,
                   subset = Sepal.Width > 2.5, output_type = "console")))
})

# ---- f_friedman -------------------------------------------------------------

test_that("f_friedman gives a clear error on misspelled arguments", {
  expect_error(
    f_friedman(y ~ treatment | block, data = friedman_df,
               ajust = "holm", output_type = "console"),
    paste0(unknown_arg("f_friedman"), ".*did you mean 'adjust'")
  )
})

test_that("f_friedman still works for valid calls", {
  expect_no_error(quiet(
    f_friedman(y ~ treatment | block, data = friedman_df,
               adjust = "holm", output_type = "console")))
})

# ---- f_outliers -------------------------------------------------------------

test_that("f_outliers errors on misspelled arguments in every interface", {
  expect_error(quiet(f_outliers(iris$Sepal.Length, cof = 3)),
               paste0(unknown_arg("f_outliers"), ".*did you mean 'coef'"))
  expect_error(quiet(f_outliers(iris, columns = "Sepal.Length", cof = 3)),
               unknown_arg("f_outliers"))
  expect_error(quiet(f_outliers(Sepal.Length ~ Species, data = iris, cof = 3)),
               unknown_arg("f_outliers"))
})

test_that("f_outliers still works in every interface", {
  # Options for the vector interface travel through the dots.
  expect_no_error(quiet(f_outliers(iris$Sepal.Length, coef = 3)))
  expect_no_error(quiet(f_outliers(iris, columns = "Sepal.Length")))
  expect_no_error(quiet(f_outliers(Sepal.Length ~ Species, data = iris)))
  expect_no_error(quiet(f_outliers(data = iris, columns = "Sepal.Length")))
})

# ---- f_summary --------------------------------------------------------------

test_that("f_summary errors on misspelled arguments", {
  expect_error(quiet(f_summary(iris, show_mena = FALSE)),
               paste0(unknown_arg("f_summary"), ".*did you mean 'show_mean'"))
  expect_error(quiet(f_summary(Sepal.Length ~ Species, data = iris,
                               show_mena = FALSE)),
               unknown_arg("f_summary"))
})

test_that("f_summary still works for valid calls", {
  expect_no_error(quiet(f_summary(iris)))
  expect_no_error(quiet(f_summary(iris, show_mean = FALSE)))
  expect_no_error(quiet(f_summary(Sepal.Length ~ Species, data = iris)))
})

# ---- f_scan -----------------------------------------------------------------

test_that("f_scan errors on misspelled arguments in every interface", {
  expect_error(quiet(f_scan(iris, outlier_s = FALSE, output_type = "console")),
               unknown_arg("f_scan"))
  expect_error(quiet(f_scan(Sepal.Length ~ Species, data = iris,
                            cof = 3, output_type = "console")),
               paste0(unknown_arg("f_scan"), ".*did you mean 'coef'"))
  expect_error(quiet(f_scan(iris$Sepal.Length, cof = 3,
                            output_type = "console")),
               unknown_arg("f_scan"))
})

test_that("f_scan still works in every interface", {
  expect_no_error(quiet(f_scan(iris, output_type = "console")))
  expect_no_error(quiet(f_scan(Sepal.Length ~ Species, data = iris,
                               output_type = "console")))
  expect_no_error(quiet(f_scan(iris$Sepal.Length, output_type = "console")))
})

# ---- f_stat_wizard ----------------------------------------------------------

test_that("f_stat_wizard errors on misspelled arguments in both methods", {
  expect_error(quiet(f_stat_wizard(Sepal.Length ~ Species, data = iris,
                                   ouput_type = "pdf")),
               paste0(unknown_arg("f_stat_wizard"),
                      ".*did you mean 'output_type'"))
  expect_error(quiet(f_stat_wizard(iris, Sepal.Length ~ Species,
                                   ouput_type = "pdf")),
               unknown_arg("f_stat_wizard"))
})

test_that("f_stat_wizard still works in both methods", {
  expect_no_error(quiet(f_stat_wizard(Sepal.Length ~ Species, data = iris)))
  expect_no_error(quiet(f_stat_wizard(iris, Sepal.Length ~ Species)))
})

# ---- f_boxplot --------------------------------------------------------------

test_that("f_boxplot errors on unknown arguments in every interface", {
  expect_error(quiet(f_boxplot(Sepal.Length ~ Species, data = iris,
                               jiter = TRUE, output_type = "rmd")),
               paste0(unknown_arg("f_boxplot"), ".*did you mean 'jitter'"))
  expect_error(quiet(f_boxplot(iris, intro_text = FALSE,
                               output_type = "rmd")),
               unknown_arg("f_boxplot"))
  expect_error(quiet(f_boxplot(iris$Sepal.Length, jiter = TRUE,
                               output_type = "rmd")),
               unknown_arg("f_boxplot"))
})

test_that("f_boxplot still works in every interface", {
  expect_no_error(quiet(f_boxplot(Sepal.Length ~ Species, data = iris,
                                  output_type = "rmd")))
  expect_no_error(quiet(f_boxplot(iris, output_type = "rmd")))
  # Single vector, and several vectors passed as unnamed extras.
  expect_no_error(quiet(f_boxplot(iris$Sepal.Length, output_type = "rmd")))
  expect_no_error(quiet(f_boxplot(iris$Sepal.Length, iris$Petal.Length,
                                  output_type = "rmd")))
})

# ---- f_boxcox ---------------------------------------------------------------

test_that("f_boxcox errors on misspelled arguments", {
  expect_error(quiet(f_boxcox(iris$Sepal.Length, digts = 2,
                              output_type = "console")),
               paste0(unknown_arg("f_boxcox"), ".*did you mean 'digits'"))
})

test_that("f_boxcox still works for valid calls", {
  expect_no_error(quiet(f_boxcox(iris$Sepal.Length, digits = 2,
                                 output_type = "console")))
})

# ---- f_aov() / f_lm(): dots are forwarded to aov() / lm() -----------------
# Before 3.2.1 a typo here only produced lm.fit()'s "extra argument ... will
# be disregarded" warning and the analysis ran with the defaults.

test_that("f_aov() rejects a misspelled argument with a suggestion", {
  expect_error(quiet(f_aov(Sepal.Width ~ Species, data = iris,
                           adjsut = "tukey", output_type = "console")),
               "'adjsut' \\(did you mean 'adjust'\\?\\)")
})

test_that("f_aov() still accepts the aov()/lm() pass-through arguments", {
  expect_no_error(quiet(f_aov(Sepal.Width ~ Species, data = iris,
                              subset = Sepal.Length > 4.5,
                              output_type = "console")))
  expect_no_error(quiet(f_aov(Sepal.Width ~ Species, data = iris,
                              weights = rep(1, 150),
                              output_type = "console")))
})

test_that("f_lm() rejects a misspelled argument with a suggestion", {
  expect_error(quiet(f_lm(Sepal.Width ~ Sepal.Length, data = iris,
                          wieghts = rep(1, 150), output_type = "console")),
               "'wieghts' \\(did you mean 'weights'\\?\\)")
})

test_that("f_lm() still accepts the lm() pass-through arguments", {
  expect_no_error(quiet(f_lm(Sepal.Width ~ Sepal.Length, data = iris,
                             weights = rep(1, 150), output_type = "console")))
})
