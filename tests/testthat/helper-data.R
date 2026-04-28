# =============================================================================
# helper-data.R
# =============================================================================
# Shared test fixtures used across the rfriend test suite.
#
# Files starting with "helper-" are sourced automatically by testthat before
# any test is run. Keep this file free of expect_*() calls: it exists only to
# define reusable fixture builders.
#
# Naming convention:
#   make_<shape>_<variant>()   returns a data.frame / vector, seeded for
#                              reproducibility. Every builder accepts a `seed`
#                              argument so a test can override it if needed.
#
# Rationale for consolidation:
#   Pre-refactor the suite contained ~12 near-duplicate builders scattered
#   across files (make_df, make_test_df, make_two_factor_data vs
#   make_two_factor_df, etc.). They diverged only in cosmetic ways. Anything
#   test-specific (edge cases a single test needs) should stay inline in that
#   test file, not bloat this module.
# =============================================================================


# -----------------------------------------------------------------------------
# One-way designs: y ~ group
# -----------------------------------------------------------------------------

# Normal-ish, 3 groups, well-separated means. Good default for happy-path tests
# of f_aov(), f_t_test() (pairwise), f_kruskal_test(), f_boxplot().
make_normal_data <- function(seed = 42, n_per_group = 15) {
  set.seed(seed)
  data.frame(
    y   = c(rnorm(n_per_group, 10, 1),
            rnorm(n_per_group, 13, 1),
            rnorm(n_per_group, 16, 1)),
    grp = factor(rep(c("A", "B", "C"), each = n_per_group))
  )
}

# Right-skewed data where Box-Cox / bestNormalize transformations should fire.
make_skewed_data <- function(seed = 7, n_per_group = 20) {
  set.seed(seed)
  data.frame(
    y   = c(rexp(n_per_group, 0.10),
            rexp(n_per_group, 0.05),
            rexp(n_per_group, 0.02)),
    grp = factor(rep(c("low", "med", "high"), each = n_per_group))
  )
}

# Plain numeric vector of skewed values. Used by transform-only tests
# (f_boxcox, f_bestNormalize) that do not need a grouping factor.
make_skewed_vec <- function(seed = 3, n = 100) {
  set.seed(seed)
  rexp(n, rate = 0.1)
}


# -----------------------------------------------------------------------------
# Factorial / mixed designs
# -----------------------------------------------------------------------------

# Two crossed factors, balanced cells. Used by interaction-model tests.
make_two_factor_data <- function(seed = 99, n_per_cell = 8) {
  set.seed(seed)
  d <- expand.grid(
    A   = factor(c("a1", "a2")),
    B   = factor(c("b1", "b2")),
    rep = seq_len(n_per_cell)
  )
  d$y <- rnorm(nrow(d), mean = as.integer(d$A) + as.integer(d$B), sd = 0.8)
  d
}

# Repeated-measures: n_subj subjects, n_time time points, random intercept
# per subject plus a linear time effect. Used by f_lmer() tests.
make_repeated_measures_data <- function(seed = 2026, n_subj = 20, n_time = 4) {
  set.seed(seed)
  subj     <- factor(rep(seq_len(n_subj), each = n_time))
  tm       <- factor(rep(seq_len(n_time), times = n_subj))
  subj_int <- rnorm(n_subj, 0, 2)[as.integer(subj)]
  y        <- 10 + as.integer(tm) * 1.5 + subj_int +
              rnorm(n_subj * n_time, 0, 1)
  data.frame(y = y, time = tm, subj = subj)
}

# Same shape but with a treatment factor replacing continuous time, for
# factor-by-subject tests.
make_factor_repeated_data <- function(seed = 31, n_subj = 24, reps = 5) {
  set.seed(seed)
  trt  <- factor(rep(c("ctrl", "low", "high"), length.out = n_subj))
  subj <- factor(seq_len(n_subj))
  d <- data.frame(
    subj = rep(subj, each = reps),
    trt  = rep(trt,  each = reps),
    rep  = rep(seq_len(reps), times = n_subj)
  )
  subj_int <- rnorm(n_subj, 0, 1.5)[as.integer(d$subj)]
  trt_eff  <- c(ctrl = 0, low = 2, high = 5)[as.character(d$trt)]
  d$y      <- 10 + trt_eff + subj_int + rnorm(nrow(d), 0, 1)
  d
}


# -----------------------------------------------------------------------------
# GLM / binary response
# -----------------------------------------------------------------------------

# Binary outcome with a group effect. Used by f_glm() logistic tests.
make_binary_data <- function(seed = 2026, n_per_group = 40) {
  set.seed(seed)
  grp <- factor(rep(c("ctrl", "trt"), each = n_per_group))
  p   <- ifelse(grp == "ctrl", 0.3, 0.7)
  data.frame(
    y   = rbinom(2 * n_per_group, 1, p),
    grp = grp
  )
}


# -----------------------------------------------------------------------------
# Outlier / cleaning fixtures
# -----------------------------------------------------------------------------

# Mixed data frame with one deliberately injected numeric outlier per relevant
# column. Used by f_outliers(), f_remove_outliers(), f_scan(), f_boxplot()
# (outlier-labeling path).
make_outlier_df <- function(seed = 42) {
  set.seed(seed)
  data.frame(
    Team       = rep(c("A", "B"), each = 20),
    Department = rep(c("Sales", "IT"), each = 10, times = 2),
    Salary     = c(rnorm(19, 50000, 500), 100000,
                   rnorm(19, 50000, 500),   1000),
    Age        = c(rnorm(38, 35, 2), 90, 35),
    EmployeeID = paste0("E", sprintf("%03d", seq_len(40))),
    stringsAsFactors = FALSE
  )
}

# Plain numeric vector with a single extreme value. Used for the
# vector-method tests of f_outliers().
make_outlier_vec <- function(seed = 42, n = 30) {
  set.seed(seed)
  x <- rnorm(n, 10, 1)
  x[1] <- 999
  x
}


# -----------------------------------------------------------------------------
# Correlation / multi-column fixtures
# -----------------------------------------------------------------------------

# All-numeric frame. Used by f_corplot() numeric-only tests.
make_numeric_df <- function(seed = 42, n = 50) {
  set.seed(seed)
  data.frame(
    a = rnorm(n), b = rnorm(n), c = rnorm(n), d = rnorm(n)
  )
}

# Numeric frame plus k factor columns. The factor columns are named
# f1, f2, ..., fk so tests can reference them positionally.
make_mixed_df <- function(seed = 42, n = 60, n_factors = 1L,
                          factor_levels = c("A", "B", "C")) {
  set.seed(seed)
  nums <- as.data.frame(
    replicate(4, rnorm(n), simplify = FALSE),
    col.names = paste0("num", seq_len(4))
  )
  facs <- as.data.frame(
    replicate(
      n_factors,
      factor(sample(factor_levels, n, replace = TRUE)),
      simplify = FALSE
    )
  )
  names(facs) <- paste0("f", seq_len(n_factors))
  cbind(nums, facs)
}


# -----------------------------------------------------------------------------
# Summary-statistics fixtures
# -----------------------------------------------------------------------------

# Small, fully-specified frame used by f_summary() tests. Kept non-random so
# exact-value assertions (sum, mean, sd) stay stable without a seed.
make_summary_df <- function() {
  data.frame(
    value   = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    weight  = c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8, 9.9, 11.0),
    group_a = rep(c("A", "B"), each = 5),
    group_b = rep(c("X", "Y"), times = 5),
    int_col = as.integer(seq_len(10)),
    stringsAsFactors = FALSE
  )
}
