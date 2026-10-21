# Report plot switches have the same names in all test functions:
#   diagnostic_plots  assumption / residual check figures
#   effect_plots      effect, estimated-means, interaction and forest plots
# Old names (norm_plots, interaction_plots, effect_plot, plot) still work
# for one release, with a lifecycle deprecation warning.
# Also BUGS #24: f_wilcox_test() opens files only in interactive sessions.

sw_data <- function() {
  set.seed(1)
  data.frame(
    grp   = factor(rep(c("a", "b", "c"), each = 8)),
    blk   = factor(rep(1:8, times = 3)),
    y     = c(rnorm(8, 10), rnorm(8, 12), rnorm(8, 15)),
    count = rpois(24, 6),
    x     = rnorm(24)
  )
}

n_img <- function(r) {
  m <- gregexpr("!\\[", r)[[1]]
  if (m[1] == -1) 0L else length(m)
}

rmd_of <- function(expr) {
  out <- suppressMessages(suppressWarnings(expr))
  out$rmd
}

test_that("all eight test functions use diagnostic_plots and effect_plots", {
  funs <- list(
    f_t_test      = rfriend:::f_t_test.formula,
    f_wilcox_test = rfriend:::f_wilcox_test.formula,
    f_aov = f_aov, f_kruskal_test = f_kruskal_test, f_lm = f_lm,
    f_glm = f_glm, f_lmer = f_lmer, f_friedman = f_friedman
  )
  for (nm in names(funs)) {
    fm <- formals(funs[[nm]])
    expect_true(isTRUE(fm$diagnostic_plots), info = nm)
    expect_true(isTRUE(fm$effect_plots), info = nm)
  }
  expect_true(isTRUE(formals(rfriend:::f_t_test.default)$effect_plots))
  expect_true(isTRUE(formals(rfriend:::f_wilcox_test.default)$effect_plots))
})

test_that("old switch names are deprecated but still honoured", {
  df <- sw_data()
  two <- droplevels(df[df$grp != "c", ])
  rlang::local_options(lifecycle_verbosity = "warning")

  # f_aov: norm_plots and interaction_plots
  base <- rmd_of(f_aov(y ~ grp, data = df, output_type = "rmd"))
  new_off <- rmd_of(f_aov(y ~ grp, data = df, output_type = "rmd", effect_plots = FALSE))
  lifecycle::expect_deprecated(
    old_off <- f_aov(y ~ grp, data = df, output_type = "rmd", interaction_plots = FALSE))
  expect_equal(n_img(old_off$rmd), n_img(new_off))
  expect_lt(n_img(new_off), n_img(base))
  lifecycle::expect_deprecated(
    f_aov(y ~ grp, data = df, output_type = "rmd", norm_plots = FALSE))

  # f_lm: norm_plots
  lifecycle::expect_deprecated(
    f_lm(y ~ x, data = df, output_type = "rmd", norm_plots = FALSE))

  # f_glm / f_lmer: effect_plot
  lifecycle::expect_deprecated(
    old_glm <- f_glm(count ~ grp, data = df, family = "poisson",
                     output_type = "rmd", effect_plot = FALSE))
  new_glm <- rmd_of(f_glm(count ~ grp, data = df, family = "poisson",
                          output_type = "rmd", effect_plots = FALSE))
  expect_equal(n_img(old_glm$rmd), n_img(new_glm))
  lifecycle::expect_deprecated(
    f_lmer(y ~ grp + (1 | blk), data = df, output_type = "rmd", effect_plot = FALSE))
  lifecycle::expect_deprecated(
    f_lmer(y ~ grp + (1 | blk), data = df, output_type = "rmd", norm_plots = FALSE))

  # f_t_test / f_wilcox_test: norm_plots (formula and vector interface)
  lifecycle::expect_deprecated(
    f_t_test(y ~ grp, data = two, output_type = "rmd", norm_plots = FALSE))
  lifecycle::expect_deprecated(
    f_t_test(df$y[1:8], df$y[9:16], output_type = "rmd", norm_plots = FALSE))
  lifecycle::expect_deprecated(
    f_wilcox_test(y ~ grp, data = two, output_type = "rmd", norm_plots = FALSE))

  # f_kruskal_test / f_friedman: plot sets both switches
  lifecycle::expect_deprecated(
    k_old <- f_kruskal_test(y ~ grp, data = df, output_type = "rmd", plot = FALSE))
  expect_equal(n_img(k_old$rmd), 0L)
  lifecycle::expect_deprecated(
    f_old <- f_friedman(y ~ grp | blk, data = df, output_type = "rmd", plot = FALSE))
  expect_equal(n_img(f_old$rmd), 0L)
})

test_that("effect_plots = FALSE drops the main effect plot of f_t_test and f_wilcox_test", {
  df <- sw_data()
  two <- droplevels(df[df$grp != "c", ])
  t_on  <- suppressMessages(suppressWarnings(f_t_test(y ~ grp, data = two, output_type = "rmd")))
  t_off <- suppressMessages(suppressWarnings(f_t_test(y ~ grp, data = two, output_type = "rmd",
                                                      effect_plots = FALSE)))
  expect_true(grepl("Main Effect Plot", t_on$rmd, fixed = TRUE))
  expect_false(grepl("Main Effect Plot", t_off$rmd, fixed = TRUE))
  expect_null(t_off$y$main_effect_plot)
  expect_s3_class(t_on$y$main_effect_plot, "ggplot")

  w_off <- suppressMessages(suppressWarnings(f_wilcox_test(y ~ grp, data = two, output_type = "rmd",
                                                           effect_plots = FALSE)))
  expect_false(grepl("Main Effect Plot", w_off$rmd, fixed = TRUE))
  expect_null(w_off$y$main_effect_plot)
})

test_that("f_kruskal_test and f_friedman switch the check plot and the boxplot separately", {
  df <- sw_data()
  k_diag <- rmd_of(f_kruskal_test(y ~ grp, data = df, output_type = "rmd", diagnostic_plots = FALSE))
  k_eff  <- rmd_of(f_kruskal_test(y ~ grp, data = df, output_type = "rmd", effect_plots = FALSE))
  expect_false(grepl("Visual check on similarity of distributions", k_diag, fixed = TRUE))
  expect_true(grepl("## Boxplot of:", k_diag, fixed = TRUE))
  expect_true(grepl("Visual check on similarity of distributions", k_eff, fixed = TRUE))
  expect_false(grepl("## Boxplot of:", k_eff, fixed = TRUE))

  f_diag <- rmd_of(f_friedman(y ~ grp | blk, data = df, output_type = "rmd", diagnostic_plots = FALSE))
  f_eff  <- rmd_of(f_friedman(y ~ grp | blk, data = df, output_type = "rmd", effect_plots = FALSE))
  expect_false(grepl("Visual check on within-block traces", f_diag, fixed = TRUE))
  expect_true(grepl("## Boxplot of:", f_diag, fixed = TRUE))
  expect_true(grepl("Visual check on within-block traces", f_eff, fixed = TRUE))
  expect_false(grepl("## Boxplot of:", f_eff, fixed = TRUE))
})

test_that("BUGS #24: f_wilcox_test opens generated files only in interactive sessions", {
  expect_identical(formals(rfriend:::f_wilcox_test.formula)$open_generated_files,
                   quote(interactive()))
  expect_identical(formals(rfriend:::f_wilcox_test.default)$open_generated_files,
                   quote(interactive()))
})
