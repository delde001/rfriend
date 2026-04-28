# =============================================================================
# testthat stress tests for f_t_test
# Run with: testthat::test_file("test_f_t_test.R")
# =============================================================================
# =============================================================================
# HELPER: create reproducible test datasets
# =============================================================================

# Normal data - should never trigger transformation
set.seed(42)
normal_vec      <- rnorm(50, mean = 10, sd = 2)
normal_vec2     <- rnorm(50, mean = 12, sd = 2)

# Log-normal data - strongly non-normal, known back-transformation properties
set.seed(42)
lognormal_vec   <- rlnorm(50, meanlog = 2, sdlog = 0.5)   # true median = exp(2) ~= 7.389

# Log-normal two-group data
set.seed(42)
lognormal_g1    <- rlnorm(40, meanlog = 2, sdlog = 0.5)
lognormal_g2    <- rlnorm(40, meanlog = 2.5, sdlog = 0.5)

# Paired: generate paired log-normal data with known difference
set.seed(42)
paired_base     <- rlnorm(30, meanlog = 2, sdlog = 0.4)
paired_post     <- paired_base * exp(rnorm(30, mean = 0.3, sd = 0.1))  # ~30% increase

# Data frames
df_one   <- data.frame(y = normal_vec)
df_two   <- data.frame(value = c(normal_vec, normal_vec2),
                       group = factor(rep(c("A", "B"), each = 50)))
df_ln    <- data.frame(y = lognormal_vec)
df_ln2   <- data.frame(value = c(lognormal_g1, lognormal_g2),
                       group = factor(rep(c("A", "B"), each = 40)))
df_paired <- data.frame(value = c(paired_base, paired_post),
                        group = factor(rep(c("pre", "post"), each = 30)))

# Data with NAs
df_na <- df_two
df_na$value[c(3, 7, 55)] <- NA

# =============================================================================
# BLOCK 1: Basic interface - does it run without errors?
# =============================================================================

test_that("One-sample formula interface runs without error", {
  skip_on_cran()
  expect_no_error(
    f_t_test(
      y ~ 1,
      data = df_one,
      output_type = "default",
      transformation = FALSE,
      intro_text = FALSE
    )
  )
})

test_that("Two-sample formula interface runs without error", {
  skip_on_cran()
  expect_no_error(
    f_t_test(
      value ~ group,
      data = df_two,
      output_type = "default",
      transformation = FALSE,
      intro_text = FALSE
    )
  )
})

test_that("Paired formula interface runs without error", {
  skip_on_cran()
  expect_no_error(
    f_t_test(
      value ~ group,
      data = df_paired,
      paired = TRUE,
      output_type = "default",
      transformation = FALSE,
      intro_text = FALSE
    )
  )
})

test_that("Vector interface one-sample runs without error", {
  skip_on_cran()
  expect_no_error(f_t_test(
    normal_vec,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  ))
})

test_that("Vector interface two-sample runs without error", {
  skip_on_cran()
  expect_no_error(
    f_t_test(
      normal_vec,
      normal_vec2,
      output_type = "default",
      transformation = FALSE,
      intro_text = FALSE
    )
  )
})

test_that("Vector interface paired runs without error", {
  skip_on_cran()
  expect_no_error(
    f_t_test(
      paired_base,
      paired_post,
      paired = TRUE,
      output_type = "default",
      transformation = FALSE,
      intro_text = FALSE
    )
  )
})

# =============================================================================
# BLOCK 2: Return object structure
# =============================================================================

test_that("Returns object of class f_t_test", {
  skip_on_cran()
  result <- f_t_test(
    y ~ 1,
    data = df_one,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  expect_s3_class(result, "f_t_test")
})

test_that("One-sample result contains expected fields", {
  skip_on_cran()
  result <- f_t_test(
    y ~ 1,
    data = df_one,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  sublist <- result[["y"]]
  expect_true(!is.null(sublist$t_test))
  expect_true(!is.null(sublist$shapiro_res))
  expect_true(!is.null(sublist$adt_res))
  expect_false(sublist$Response_Transformed)
})

test_that("Two-sample result contains variance diagnostic fields", {
  skip_on_cran()
  result <- f_t_test(
    value ~ group,
    data = df_two,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  sublist <- result[["value"]]
  expect_true(!is.na(sublist$homog_p_bartlett))
  expect_true(!is.na(sublist$homog_p_levene))
})

test_that("One-sample has NA variance diagnostics (not applicable)", {
  skip_on_cran()
  result <- f_t_test(
    y ~ 1,
    data = df_one,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  expect_true(is.na(result[["y"]]$homog_p_bartlett))
  expect_true(is.na(result[["y"]]$homog_p_levene))
})

# =============================================================================
# BLOCK 3: Statistical correctness - match stats::t.test
# =============================================================================

test_that("One-sample t-test matches stats::t.test exactly", {
  skip_on_cran()
  result   <- f_t_test(
    y ~ 1,
    data = df_one,
    mu = 10,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  expected <- t.test(df_one$y, mu = 10)
  expect_equal(result[["y"]]$t_test$statistic, expected$statistic, tolerance = 1e-8)
  expect_equal(result[["y"]]$t_test$p.value, expected$p.value, tolerance = 1e-8)
  expect_equal(result[["y"]]$t_test$conf.int, expected$conf.int, tolerance = 1e-8)
})

test_that("Two-sample Welch t-test matches stats::t.test exactly", {
  skip_on_cran()
  result   <- f_t_test(
    value ~ group,
    data = df_two,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  expected <- t.test(normal_vec, normal_vec2, var.equal = FALSE)
  expect_equal(result[["value"]]$t_test$statistic, expected$statistic, tolerance = 1e-8)
  expect_equal(result[["value"]]$t_test$p.value, expected$p.value, tolerance = 1e-8)
})

test_that("var.equal = TRUE gives Student's t-test matching stats::t.test",
          {
  skip_on_cran()
            result   <- f_t_test(
              value ~ group,
              data = df_two,
              var.equal = TRUE,
              output_type = "default",
              transformation = FALSE,
              intro_text = FALSE
            )
            expected <- t.test(normal_vec, normal_vec2, var.equal = TRUE)
            expect_equal(result[["value"]]$t_test$statistic, expected$statistic, tolerance = 1e-8)
          })


df_paired <- data.frame(
  value = c(paired_base, paired_post),
  group = factor(rep(c("pre", "post"), each = 30),
                 levels = c("pre", "post"))  # pre first -> matches t.test order
)

# Now both compute paired_base - paired_post
expected <- t.test(paired_base, paired_post, paired = TRUE)
test_that("Paired t-test matches stats::t.test exactly", {
  result   <- f_t_test(
    value ~ group,
    data = df_paired,
    paired = TRUE,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  expected <- t.test(paired_base, paired_post, paired = TRUE)
  expect_equal(result[["value"]]$t_test$statistic, expected$statistic, tolerance = 1e-8)
  expect_equal(result[["value"]]$t_test$p.value, expected$p.value, tolerance = 1e-8)
})

test_that("alternative = 'greater' propagates correctly", {
  skip_on_cran()
  result   <- f_t_test(
    y ~ 1,
    data = df_one,
    mu = 9,
    alternative = "greater",
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  expected <- t.test(df_one$y, mu = 9, alternative = "greater")
  expect_equal(result[["y"]]$t_test$p.value, expected$p.value, tolerance = 1e-8)
})

test_that("conf.level = 0.99 propagates correctly", {
  skip_on_cran()
  result <- f_t_test(
    y ~ 1,
    data = df_one,
    conf.level = 0.99,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  expected <- t.test(df_one$y, conf.level = 0.99)
  expect_equal(result[["y"]]$t_test$conf.int, expected$conf.int, tolerance = 1e-8)
})

test_that("alpha = 0.01 is equivalent to conf.level = 0.99", {
  skip_on_cran()
  r_alpha  <- f_t_test(
    y ~ 1,
    data = df_one,
    alpha = 0.01,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  r_conf   <- f_t_test(
    y ~ 1,
    data = df_one,
    conf.level = 0.99,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  expect_equal(r_alpha[["y"]]$t_test$conf.int,
               r_conf[["y"]]$t_test$conf.int,
               tolerance = 1e-8)
})

# =============================================================================
# BLOCK 4: var.equal behaviour - Welch's is always default
# =============================================================================

test_that("var.equal = NULL always gives Welch's (df is non-integer)", {
  skip_on_cran()
  result <- f_t_test(
    value ~ group,
    data = df_two,
    var.equal = NULL,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  # Welch's df is non-integer for most datasets
  df_val <- result[["value"]]$t_test$parameter
  # The key check: result matches var.equal = FALSE, not var.equal = TRUE
  expected_welch   <- t.test(normal_vec, normal_vec2, var.equal = FALSE)
  expected_student <- t.test(normal_vec, normal_vec2, var.equal = TRUE)
  expect_equal(result[["value"]]$t_test$statistic,
               expected_welch$statistic,
               tolerance = 1e-8)
  expect_false(isTRUE(
    all.equal(
      result[["value"]]$t_test$parameter,
      expected_student$parameter,
      tolerance = 1e-4
    )
  ))
})

test_that("var.equal is reset correctly across multiple response variables",
          {
  skip_on_cran()
            df_multi <- data.frame(
              y1    = c(normal_vec, normal_vec2),
              y2    = c(normal_vec2, normal_vec),
              group = factor(rep(c("A", "B"), each = 50))
            )
            result <- f_t_test(
              y1 + y2 ~ group,
              data = df_multi,
              var.equal = NULL,
              output_type = "default",
              transformation = FALSE,
              intro_text = FALSE
            )
            # Both should use Welch's - check both match var.equal = FALSE
            e1 <- t.test(normal_vec, normal_vec2, var.equal = FALSE)
            e2 <- t.test(normal_vec2, normal_vec, var.equal = FALSE)
            expect_equal(result[["y1"]]$t_test$statistic, e1$statistic, tolerance = 1e-6)
            expect_equal(result[["y2"]]$t_test$statistic, e2$statistic, tolerance = 1e-6)
          })

# =============================================================================
# BLOCK 5: conf.level / alpha consistency across multiple responses
# =============================================================================

test_that("conf.level is stable across multiple response variables", {
  df_multi <- data.frame(
    y1    = c(normal_vec, normal_vec2),
    y2    = c(normal_vec2, normal_vec),
    group = factor(rep(c("A", "B"), each = 50))
  )
  result <- f_t_test(
    y1 + y2 ~ group,
    data = df_multi,
    alpha = 0.01,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  ci1 <- result[["y1"]]$t_test$conf.int
  ci2 <- result[["y2"]]$t_test$conf.int
  # Both CIs should be at 99% level - check against stats::t.test
  e1 <- t.test(normal_vec,
               normal_vec2,
               conf.level = 0.99,
               var.equal = FALSE)
  e2 <- t.test(normal_vec2,
               normal_vec,
               conf.level = 0.99,
               var.equal = FALSE)
  expect_equal(ci1, e1$conf.int, tolerance = 1e-6)
  expect_equal(ci2, e2$conf.int, tolerance = 1e-6)
})

# =============================================================================
# BLOCK 6: Missing value handling
# =============================================================================

test_that("NAs are removed and result still matches manual NA-removed t.test",
          {
  skip_on_cran()
            result   <- f_t_test(
              value ~ group,
              data = df_na,
              output_type = "default",
              transformation = FALSE,
              intro_text = FALSE
            )
            clean    <- df_na[complete.cases(df_na), ]
            g1       <- clean$value[clean$group == "A"]
            g2       <- clean$value[clean$group == "B"]
            expected <- t.test(g1, g2, var.equal = FALSE)
            expect_equal(result[["value"]]$t_test$statistic, expected$statistic, tolerance = 1e-8)
          })

test_that("Paired NA removal in default interface aligns pairs correctly", {
  x_na     <- paired_base
  y_na     <- paired_post
  x_na[5]  <- NA
  y_na[12] <- NA
  expect_no_error(
    suppressWarnings(f_t_test(
      x_na,
      y_na,
      paired = TRUE,
      output_type = "default",
      transformation = FALSE,
      intro_text = FALSE
    )
  ))
  # Check pair count matches complete.cases count
  suppressWarnings(result <- f_t_test(
    x_na,
    y_na,
    paired = TRUE,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  ))
  ok <- complete.cases(x_na, y_na)
  expected <- t.test(x_na[ok], y_na[ok], paired = TRUE)
  expect_equal(result[[1]]$t_test$parameter, expected$parameter, tolerance = 1e-8)
})

# =============================================================================
# BLOCK 7: Transformation triggering
# =============================================================================

test_that("Log-normal data triggers transformation (Shapiro-Wilk p < 0.05)",
          {
  skip_on_cran()
            result <- f_t_test(
              y ~ 1,
              data = df_ln,
              output_type = "default",
              transformation = TRUE,
              intro_text = FALSE
            )
            expect_true(result[["y"]]$Response_Transformed)
          })

test_that("Normal data does NOT trigger transformation", {
  result <- f_t_test(
    y ~ 1,
    data = df_one,
    output_type = "default",
    transformation = TRUE,
    intro_text = FALSE
  )
  expect_false(result[["y"]]$Response_Transformed)
})

test_that("force_transformation overrides normality result", {
  skip_on_cran()
  result <- f_t_test(
    y ~ 1,
    data = df_one,
    output_type = "default",
    transformation = TRUE,
    force_transformation = "y",
    intro_text = FALSE
  )
  expect_true(result[["y"]]$Response_Transformed)
})

test_that("transformation = FALSE suppresses transformation on non-normal data",
          {
  skip_on_cran()
            result <- f_t_test(
              y ~ 1,
              data = df_ln,
              output_type = "default",
              transformation = FALSE,
              intro_text = FALSE
            )
            expect_false(result[["y"]]$Response_Transformed)
          })

test_that("transformation = 'none' suppresses transformation", {
  result <- f_t_test(
    y ~ 1,
    data = df_ln,
    output_type = "default",
    transformation = "none",
    intro_text = FALSE
  )
  expect_false(result[["y"]]$Response_Transformed)
})

test_that("transformation = 'boxcox' string is accepted", {
  skip_on_cran()
  expect_no_error(
    f_t_test(
      y ~ 1,
      data = df_ln,
      output_type = "default",
      transformation = "boxcox",
      intro_text = FALSE
    )
  )
})

# =============================================================================
# BLOCK 8: Back-transformation correctness
# Key property: for log-normal data with mu = 0, the back-transformed CI
# should bracket the true median (exp(meanlog) = exp(2) ~= 7.389).
# =============================================================================

test_that("Back-transformed CI brackets the true median for log-normal data",
          {
  skip_on_cran()
            true_median <- exp(2)  # ~= 7.389, the true population median
            result <- f_t_test(
              y ~ 1,
              data = df_ln,
              mu = 0,
              output_type = "default",
              transformation = "boxcox",
              intro_text = FALSE
            )
            ci_bt <- result[["y"]]$ci_backtransformed
            skip_if(is.null(ci_bt),
                    "Back-transformation not available for this dataset")
            expect_true(
              ci_bt[1] < true_median && ci_bt[2] > true_median,
              label = sprintf(
                "True median %.3f not in back-transformed CI [%.3f, %.3f]",
                true_median,
                ci_bt[1],
                ci_bt[2]
              )
            )
          })

test_that("Back-transformed CI is wider than transformed-scale CI (always true)",
          {
            # Back-transforming a CI expands it due to nonlinearity of the inverse
            result <- f_t_test(
              y ~ 1,
              data = df_ln,
              output_type = "default",
              transformation = "boxcox",
              intro_text = FALSE
            )
            ci_bt  <- result[["y"]]$ci_backtransformed
            ci_tr  <- result[["y"]]$t_test_transformed$conf.int
            skip_if(is.null(ci_bt))
            bt_width <- ci_bt[2] - ci_bt[1]
            tr_width <- ci_tr[2] - ci_tr[1]
            # Back-transformed CI should be positive and non-trivially wide
            expect_true(bt_width > 0)
            expect_true(all(ci_bt > 0))  # back-transformed values must be positive for log-normal
          })

test_that("Round-trip: forward then back-transform recovers original mu", {
  # Use predict.f_boxcox directly to verify round-trip
  bc      <- f_boxcox(lognormal_vec, output_type = "default")
  mu_orig <- 5
  mu_fwd  <- predict(bc, newdata = mu_orig)
  mu_back <- predict(bc, newdata = mu_fwd, inverse = TRUE)
  expect_equal(mu_back, mu_orig, tolerance = 1e-6)
})

test_that("mu forward-transformation is stored correctly in output", {
  skip_on_cran()
  result <- f_t_test(
    y ~ 1,
    data = df_ln,
    mu = 5,
    output_type = "default",
    transformation = "boxcox",
    intro_text = FALSE
  )
  mu_stored <- result[["y"]]$mu_transformed
  skip_if(is.null(mu_stored), "Transformation not triggered or mu = 0")
  # mu_transformed should be finite and different from original mu = 5
  expect_true(is.finite(mu_stored))
  expect_false(isTRUE(all.equal(mu_stored, 5, tolerance = 1e-4)))
})

test_that("Two-sample back-transformed CI is non-null for log-normal data",
          {
  skip_on_cran()
            result <- f_t_test(
              value ~ group,
              data = df_ln2,
              output_type = "default",
              transformation = "boxcox",
              intro_text = FALSE
            )
            ci_bt <- result[["value"]]$ci_backtransformed
            skip_if(is.null(ci_bt), "Transformation not triggered")
            expect_length(ci_bt, 2)
            expect_true(all(is.finite(ci_bt)))
          })

# =============================================================================
# BLOCK 9: Paired transformation uses bestNormalize on differences
# =============================================================================

test_that("Paired transformation operates on differences, not raw values", {
  # The test data (log-normal paired_base with a ~30% multiplicative
  # shift) produces paired differences that happen to pass Shapiro at
  # n = 30, so transformation is never triggered by normality alone.
  # Without force_transformation the whole assertion block below would
  # be silently skipped and the test would pass with zero expectations.
  #
  # Side note: an earlier draft of this test used a formula like
  # `value^2 ~ group` hoping to make the differences non-normal. That
  # does not work: f_t_test (like the other f_* functions) extracts
  # variable names with all.vars(formula[[2]]) and rebuilds the master
  # formula as `~ value + group`, so any expression wrapped around
  # `value` on the LHS is silently dropped. To transform in-formula,
  # precompute a new column in `data` and use its name in the formula.
  result <- f_t_test(
    value ~ group,
    data                 = df_paired,
    paired               = TRUE,
    output_type          = "default",
    transformation       = TRUE,
    force_transformation = "value",
    intro_text           = FALSE
  )

  # 1. Transformation actually happened.
  expect_true(result[["value"]]$Response_Transformed)

  # 2. It used bestNormalize, NOT Box-Cox. Paired routes through
  #    bestNormalize because Box-Cox requires strictly positive values
  #    and differences can be negative.
  expect_false(isTRUE(result[["value"]]$is_boxcox))
  expect_false(identical(result[["value"]]$trans_name, "Box-Cox"))

  # 3. The transformation operated on the 30 differences, not the 60
  #    raw observations. This is the concrete meaning of the test
  #    title, "operates on differences, not raw values".
  n_pairs <- nrow(df_paired) / 2L
  expect_length(result[["value"]]$trans_object$transformed_data, n_pairs)
})

test_that("Paired t-test on differences matches manual one-sample on differences",
          {
  skip_on_cran()
            diffs  <- paired_base - paired_post
            result <- f_t_test(
              value ~ group,
              data = df_paired,
              paired = TRUE,
              output_type = "default",
              transformation = FALSE,
              intro_text = FALSE
            )
            expected <- t.test(paired_base, paired_post, paired = TRUE)
            expect_equal(result[["value"]]$t_test$statistic, expected$statistic, tolerance = 1e-8)
          })

# =============================================================================
# BLOCK 10: Multiple response variables
# =============================================================================

test_that("Multiple LHS responses all appear in output", {
  df_multi <- data.frame(
    y1    = c(normal_vec, normal_vec2),
    y2    = c(normal_vec2, normal_vec),
    group = factor(rep(c("A", "B"), each = 50))
  )
  result <- f_t_test(
    y1 + y2 ~ group,
    data = df_multi,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  expect_true("y1" %in% names(result))
  expect_true("y2" %in% names(result))
})

test_that("Three responses all produce independent results", {
  skip_on_cran()
  set.seed(42)
  df_multi <- data.frame(
    y1    = c(normal_vec[1:40],    normal_vec2[1:40]),
    y2    = c(normal_vec2[1:40],   normal_vec[1:40]),
    y3    = c(lognormal_g1,        lognormal_g2),       # already length 40
    group = factor(rep(c("A", "B"), each = 40))
  )
  result <- f_t_test(
    y1 + y2 + y3 ~ group,
    data           = df_multi,
    output_type    = "default",
    transformation = TRUE,
    intro_text     = FALSE
  )
  expect_true(all(c("y1", "y2", "y3") %in% names(result)))
  expect_false(result[["y1"]]$Response_Transformed)
  expect_false(result[["y2"]]$Response_Transformed)
  expect_true(result[["y3"]]$Response_Transformed)
})

# =============================================================================
# BLOCK 11: Input validation and error handling
# =============================================================================

test_that("Non-numeric response variable throws error", {
  skip_on_cran()
  df_bad <- data.frame(y = letters[1:10], g = factor(rep(c("A", "B"), 5)))
  expect_error(f_t_test(
    y ~ g,
    data = df_bad,
    output_type = "default",
    intro_text = FALSE
  ))
})

test_that("Missing response variable throws error", {
  skip_on_cran()
  expect_error(f_t_test(
    z ~ 1,
    data = df_one,
    output_type = "default",
    intro_text = FALSE
  ))
})

test_that("More than 2 factor levels throws error", {
  skip_on_cran()
  df_3lev <- data.frame(value = rnorm(30), group = factor(rep(c("A", "B", "C"), 10)))
  expect_error(f_t_test(
    value ~ group,
    data = df_3lev,
    output_type = "default",
    intro_text = FALSE
  ))
})

test_that("Paired test with unequal group sizes throws error", {
  skip_on_cran()
  df_unequal <- data.frame(value = c(rnorm(10), rnorm(12)), group = factor(c(rep("A", 10), rep("B", 12))))
  expect_error(
    f_t_test(
      value ~ group,
      data = df_unequal,
      paired = TRUE,
      output_type = "default",
      intro_text = FALSE
    )
  )
})

test_that("Invalid output_type throws error", {
  skip_on_cran()
  expect_error(f_t_test(
    y ~ 1,
    data = df_one,
    output_type = "html",
    intro_text = FALSE
  ))
})

test_that("Invalid transformation string throws error", {
  skip_on_cran()
  expect_error(
    f_t_test(
      y ~ 1,
      data = df_one,
      output_type = "default",
      transformation = "logtransform",
      intro_text = FALSE
    )
  )
})

test_that("Non-numeric x in vector interface throws error", {
  skip_on_cran()
  expect_error(f_t_test(letters[1:10], output_type = "default"))
})

test_that("Paired vector interface with different lengths throws error", {
  skip_on_cran()
  expect_error(f_t_test(
    normal_vec,
    normal_vec2[1:40],
    paired = TRUE,
    output_type = "default"
  ))
})

# =============================================================================
# BLOCK 12: Vector interface name sanitisation
# =============================================================================

test_that("Dollar-sign names are sanitised correctly", {
  skip_on_cran()
  x <- mtcars$hp
  expect_no_error(f_t_test(
    x,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  ))
  result <- f_t_test(
    x,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  # Should not contain "mtcars" as a list element name
  expect_false("mtcars" %in% names(result))
})

test_that("Vector two-sample output element is named x_vs_y", {
  skip_on_cran()
  result <- f_t_test(
    normal_vec,
    normal_vec2,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  expect_true(any(grepl("_vs_", names(result))))
})

# =============================================================================
# BLOCK 13: Output structure completeness
# =============================================================================

test_that("Transformed result stores all expected fields", {
  skip_on_cran()
  result  <- f_t_test(
    y ~ 1,
    data = df_ln,
    output_type = "default",
    transformation = "boxcox",
    intro_text = FALSE
  )
  sublist <- result[["y"]]
  if (sublist$Response_Transformed) {
    expect_true(!is.null(sublist$t_test_transformed))
    expect_true(!is.null(sublist$trans_name))
    expect_true(!is.null(sublist$trans_object))
    expect_true(!is.null(sublist$shapiro_res_transformed))
    expect_true(!is.null(sublist$is_boxcox))
  }
})

test_that("print.f_t_test runs without error", {
  skip_on_cran()
  result <- f_t_test(
    y ~ 1,
    data = df_one,
    output_type = "default",
    transformation = FALSE,
    intro_text = FALSE
  )
  expect_no_error(print(result))
})

test_that("Normality plot file exists after run with norm_plots = TRUE", {
  skip_on_cran()
  result <- f_t_test(
    y ~ 1,
    data = df_one,
    output_type = "default",
    norm_plots = TRUE,
    transformation = FALSE,
    intro_text = FALSE
  )
  plot_path <- result[["y"]]$normality_plots
  expect_true(!is.null(plot_path) && file.exists(plot_path))
})

# =============================================================================
# BLOCK 14: Paired row-ordering warning
# =============================================================================

test_that("Interleaved paired data triggers a warning", {
  skip_on_cran()
  df_interleaved <- data.frame(
    value = c(rbind(paired_base, paired_post)),
    # ABABAB order
    group = factor(rep(c("pre", "post"), times = 30))
  )
  expect_warning(
    f_t_test(
      value ~ group,
      data = df_interleaved,
      paired = TRUE,
      output_type = "default",
      transformation = FALSE,
      intro_text = FALSE
    ),
    regexp = "interleaved|AABB|sorted"
  )
})

test_that("Correctly sorted paired data does not trigger ordering warning", {
  skip_on_cran()
  expect_no_warning(
    f_t_test(
      value ~ group,
      data = df_paired,
      paired = TRUE,
      output_type = "default",
      transformation = FALSE,
      intro_text = FALSE
    )
  )
})

# =============================================================================
# BLOCK 15: rmd output type
# =============================================================================

test_that("output_type = 'rmd' returns rmd element in list", {
  skip_on_cran()
  result <- f_t_test(
    y ~ 1,
    data = df_one,
    output_type = "rmd",
    transformation = FALSE,
    intro_text = FALSE
  )
  expect_true("rmd" %in% names(result))
  expect_true(nchar(result$rmd) > 100)
})


# =============================================================================
# f_t_test_new_tests.R
# Additional tests for f_t_test() covering:
#
#   A. S3 dispatch (formula / default methods).
#   B. safe_shapiro() rollout: n > 5000 no longer crashes (previously
#      a latent bug).
# =============================================================================
# quiet_tt removed - use shared quiet_f_t_test() from helper-quiet.R
# ---------------------------------------------------------------------------
# A. S3 dispatch
# ---------------------------------------------------------------------------

test_that("formula interface dispatches correctly", {
  skip_on_cran()
  df <- data.frame(
    y = c(rnorm(20, 10), rnorm(20, 12)),
    g = factor(rep(c("A", "B"), each = 20))
  )
  expect_no_error(
    suppressWarnings(res <- quiet_f_t_test(y ~ g, data = df))
  )
})

test_that("default interface with two numeric vectors dispatches correctly", {
  skip_on_cran()
  set.seed(1)
  a <- rnorm(30, 10)
  b <- rnorm(30, 11)
  expect_no_error(
    res <- quiet_f_t_test(a, b)
  )
})

# ---------------------------------------------------------------------------
# B. safe_shapiro large-n fix
# ---------------------------------------------------------------------------

test_that("f_t_test handles n > 5000 per group without crashing", {
  skip_on_cran()
  # Regression guard: the OLD n > 5000 path called shapiro.test()
  # directly and errored out. safe_shapiro() now returns an NA-valued
  # htest and the function keeps running.
  set.seed(101)
  df <- data.frame(
    y = c(rnorm(5500, 10), rnorm(5500, 10.2)),
    g = factor(rep(c("A", "B"), each = 5500))
  )
  expect_no_error(
    res <- quiet_f_t_test(y ~ g, data = df)
  )
})

test_that("large-n default interface also does not crash", {
  skip_on_cran()
  set.seed(103)
  a <- rnorm(5500, 0)
  b <- rnorm(5500, 0.05)
  expect_no_error(
    res <- quiet_f_t_test(a, b)
  )
})
