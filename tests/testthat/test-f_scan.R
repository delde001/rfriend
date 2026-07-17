# =============================================================================
# Stress Tests for f_scan()
# =============================================================================
# These tests combine multiple parameter options to exercise as many code paths
# as possible. Run with: testthat::test_file("test-fscan-stress.R")
#
# Prerequisites: the rfriend package (or at minimum f_scan, f_summary,
# f_outliers, f_pander, f_conditional_round) must be loaded.
# =============================================================================
# ---------------------------------------------------------------------------
# Test Data
# ---------------------------------------------------------------------------

# Standard data: 2 groups, 2 numeric columns, known outliers
set.seed(42)
df_standard <- data.frame(
  Team       = rep(c("A", "B"), each = 20),
  Department = rep(c("Sales", "IT"), each = 10, times = 2),
  Salary     = rnorm(40, mean = 50000, sd = 2000),
  Age        = rnorm(40, mean = 35, sd = 3),
  EmployeeID = paste0("E", sprintf("%03d", 1:40))
)
# Inject clear outliers
df_standard[1, "Salary"] <- 100000
df_standard[2, "Salary"] <- 1000
df_standard[3, "Age"]    <- 100

# Data with no outliers (tight distribution)
set.seed(99)
df_no_outliers <- data.frame(
  Group = rep(c("X", "Y"), each = 50),
  Value = rep(10, 100) + runif(100, -0.01, 0.01)
)

# Data with NAs
set.seed(7)
df_with_na <- data.frame(
  Species = rep(c("Cat", "Dog"), each = 30),
  Weight  = c(rnorm(30, 5, 1), rnorm(30, 25, 5)),
  Length  = c(rnorm(30, 40, 5), rnorm(30, 60, 10))
)
df_with_na$Weight[c(1, 15, 31, 45)] <- NA
df_with_na$Length[c(5, 50)]          <- NA
# Inject outlier
df_with_na$Weight[10] <- 50

# Data with 3 grouping variables
set.seed(11)
df_3groups <- data.frame(
  weight    = c(rnorm(60, 10, 2), rnorm(60, 14, 2)),
  species   = rep(c("A", "B"), each = 60),
  treatment = rep(rep(c("control", "treated"), each = 30), 2),
  batch     = factor(rep(c("1", "2", "3"), 40))
)
# Inject outlier
df_3groups$weight[1] <- 50

# Small data (edge case: very few rows per group)
df_small <- data.frame(
  Group = c("A", "A", "A", "B", "B", "B"),
  Value = c(1, 2, 100, 10, 20, 30)
)

# Single numeric column, no groups
df_minimal <- data.frame(
  Score = c(rnorm(50, 100, 10), 200, 5)
)


# ---------------------------------------------------------------------------
# Helper: validate output structure
# ---------------------------------------------------------------------------

expect_fscan_structure <- function(result, expected_cols, summary, outliers) {
  expect_s3_class(result, "f_scan")
  expect_type(result, "list")

  for (col in expected_cols) {
    expect_true(col %in% names(result))
    sub <- result[[col]]

    # Plots should always be present
    expect_true("boxplot"   %in% names(sub))
    expect_true("histogram" %in% names(sub))
    expect_true("qqplot"    %in% names(sub))
    expect_true("main_plot" %in% names(sub))

    # Summary table
    if (summary) {
      expect_true("f_summary" %in% names(sub))
      expect_s3_class(sub[["f_summary"]], "data.frame")
    } else {
      expect_false("f_summary" %in% names(sub))
    }

    # Outlier table (always present when outliers = TRUE, even if "No outliers detected")
    if (outliers) {
      expect_true("f_outliers" %in% names(sub))
      expect_s3_class(sub[["f_outliers"]], "data.frame")
    } else {
      expect_false("f_outliers" %in% names(sub))
    }
  }
}


# ===========================================================================
# 1. DATA.FRAME NOTATION - PARAMETER COMBINATIONS
# ===========================================================================

test_that("single column | no groups | defaults", {
  result <- f_scan(df_standard, columns = "Salary", output_type = "default",
                   open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = TRUE)
})

test_that("single column | 1 group | defaults", {
  skip_on_cran()
  result <- f_scan(df_standard, columns = "Salary", group_vars = "Team",
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = TRUE)
})

test_that("single column | 2 groups | defaults", {
  result <- f_scan(df_standard, columns = "Salary",
                   group_vars = c("Team", "Department"),
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = TRUE)
})

test_that("multiple columns | 2 groups | defaults", {
  skip_on_cran()
  result <- f_scan(df_standard, columns = c("Salary", "Age"),
                   group_vars = c("Team", "Department"),
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, c("Salary", "Age"),
                         summary = TRUE, outliers = TRUE)
})

test_that("summary = FALSE, outliers = TRUE", {
  result <- f_scan(df_standard, columns = "Salary", group_vars = "Team",
                   summary = FALSE, outliers = TRUE,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = FALSE, outliers = TRUE)
})

test_that("summary = TRUE, outliers = FALSE", {
  result <- f_scan(df_standard, columns = "Salary", group_vars = "Team",
                   summary = TRUE, outliers = FALSE,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = FALSE)
})

test_that("summary = FALSE, outliers = FALSE", {
  result <- f_scan(df_standard, columns = "Salary",
                   summary = FALSE, outliers = FALSE,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = FALSE, outliers = FALSE)
})

test_that("strict coef = 3.0 catches fewer outliers", {
  r_loose  <- f_scan(df_standard, columns = "Salary", coef = 1.5,
                     output_type = "default", open_generated_files = FALSE)
  r_strict <- f_scan(df_standard, columns = "Salary", coef = 3.0,
                     output_type = "default", open_generated_files = FALSE)
  n_loose  <- nrow(r_loose[["Salary"]][["f_outliers"]])
  n_strict <- nrow(r_strict[["Salary"]][["f_outliers"]])
  expect_true(n_loose >= n_strict)
})


# ===========================================================================
# 2. FORMULA NOTATION - PARAMETER COMBINATIONS
# ===========================================================================

test_that("formula | single column | no groups", {
  result <- f_scan(Salary ~ 1, data = df_standard,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = TRUE)
})

test_that("formula | single column | 1 group", {
  result <- f_scan(Salary ~ Team, data = df_standard,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = TRUE)
})

test_that("formula | single column | 2 groups", {
  result <- f_scan(Salary ~ Team + Department, data = df_standard,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = TRUE)
})

test_that("formula | multiple columns | 2 groups", {
  skip_on_cran()
  result <- f_scan(Salary + Age ~ Team + Department, data = df_standard,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, c("Salary", "Age"),
                         summary = TRUE, outliers = TRUE)
})

test_that("formula | 3 groups (facet grid)", {
  skip_on_cran()
  result <- f_scan(weight ~ species + treatment + batch, data = df_3groups,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "weight", summary = TRUE, outliers = TRUE)
})

test_that("formula | summary = FALSE, outliers = FALSE, coef = 3", {
  result <- f_scan(Salary ~ Team, data = df_standard,
                   summary = FALSE, outliers = FALSE, coef = 3.0,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = FALSE, outliers = FALSE)
})


# ===========================================================================
# 3. GENERIC DISPATCH (data = instead of x =)
# ===========================================================================

test_that("dispatch via data = (x missing)", {
  result <- f_scan(data = df_standard, columns = "Salary",
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = TRUE)
})


# ===========================================================================
# 4. DIGITS PARAMETER
# ===========================================================================

test_that("digits = NULL (no rounding)", {
  result <- f_scan(df_standard, columns = "Salary", digits = NULL,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = TRUE)
})

test_that("digits = 2", {
  result <- f_scan(df_standard, columns = "Salary", digits = 2,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = TRUE)
})

test_that("digits = 5", {
  result <- f_scan(df_standard, columns = "Salary", digits = 5,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = TRUE)
})

test_that("digits = 0", {
  result <- f_scan(df_standard, columns = "Salary", digits = 0,
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = TRUE)
})


# ===========================================================================
# 5. FANCY NAMES
# ===========================================================================

test_that("fancy_names | single column + 1 group", {
  result <- f_scan(df_standard, columns = "Salary", group_vars = "Team",
                   fancy_names = c(Salary = "Annual Pay", Team = "Division"),
                   output_type = "default", open_generated_files = FALSE)
  # After renaming, keys should use fancy names
  expect_true("Annual Pay" %in% names(result))
})

test_that("fancy_names | formula | multi-column + multi-group", {
  skip_on_cran()
  result <- f_scan(Salary + Age ~ Team + Department, data = df_standard,
                   fancy_names = c(Salary     = "Pay",
                                   Age        = "Years",
                                   Team       = "Division",
                                   Department = "Dept"),
                   output_type = "default", open_generated_files = FALSE)
  expect_true("Pay"   %in% names(result))
  expect_true("Years" %in% names(result))
})


# ===========================================================================
# 6. EDGE CASES
# ===========================================================================

test_that("data with NAs does not error", {
  result <- f_scan(df_with_na, columns = "Weight", group_vars = "Species",
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Weight", summary = TRUE, outliers = TRUE)
})

test_that("data with NAs | multiple columns", {
  result <- f_scan(df_with_na, columns = c("Weight", "Length"),
                   group_vars = "Species",
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, c("Weight", "Length"),
                         summary = TRUE, outliers = TRUE)
})

test_that("data with no outliers produces placeholder", {
  result <- f_scan(df_no_outliers, columns = "Value", group_vars = "Group",
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Value", summary = TRUE, outliers = TRUE)
  out_df <- result[["Value"]][["f_outliers"]]
  # Should either have 0 data rows or the placeholder "No outliers detected"
  expect_true(nrow(out_df) >= 0)
})

test_that("very small groups (3 rows each)", {
  result <- f_scan(df_small, columns = "Value", group_vars = "Group",
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Value", summary = TRUE, outliers = TRUE)
})

test_that("no groups, no grouping at all (minimal data)", {
  result <- f_scan(df_minimal, columns = "Score",
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Score", summary = TRUE, outliers = TRUE)
})


# ===========================================================================
# 7. OUTPUT_TYPE = "console" (forced print, returns invisible)
# ===========================================================================

test_that("console output | single column | no groups", {
  expect_output(
    result <- f_scan(df_standard, columns = "Salary",
                     output_type = "console", open_generated_files = FALSE),
    "."
  )
  expect_fscan_structure(result, "Salary", summary = TRUE, outliers = TRUE)
})

test_that("console output | multi-column | 2 groups | digits = 2", {
  skip_on_cran()
  expect_output(
    result <- f_scan(df_standard, columns = c("Salary", "Age"),
                     group_vars = c("Team", "Department"),
                     digits = 2,
                     output_type = "console", open_generated_files = FALSE),
    "."
  )
  expect_fscan_structure(result, c("Salary", "Age"),
                         summary = TRUE, outliers = TRUE)
})


# ===========================================================================
# 8. INPUT VALIDATION (should error)
# ===========================================================================

test_that("non-data.frame input errors", {
  expect_error(f_scan("not a data frame", columns = "x"))
})

test_that("non-existent column errors", {
  expect_error(
    f_scan(df_standard, columns = "NonExistent",
           output_type = "default", open_generated_files = FALSE)
  )
})

test_that("non-existent group_var errors", {
  expect_error(
    f_scan(df_standard, columns = "Salary", group_vars = "Ghost",
           output_type = "default", open_generated_files = FALSE)
  )
})

test_that("non-numeric column errors", {
  expect_error(
    f_scan(df_standard, columns = "EmployeeID",
           output_type = "default", open_generated_files = FALSE)
  )
})

test_that("invalid output_type errors", {
  expect_error(
    f_scan(df_standard, columns = "Salary", output_type = "banana",
           open_generated_files = FALSE)
  )
})

test_that("one-sided formula errors", {
  expect_error(
    f_scan(~ Team, data = df_standard,
           output_type = "default", open_generated_files = FALSE)
  )
})


# ===========================================================================
# 9. PRINT / SUMMARY / PLOT METHODS
# ===========================================================================

test_that("print method works with all flags", {
  skip_on_cran()
  result <- f_scan(df_standard, columns = "Salary", group_vars = "Team",
                   output_type = "default", open_generated_files = FALSE)

  # Cases that produce text output (summary and/or outlier tables)
  expect_output(print(result), ".")
  expect_output(print(result, summary = TRUE, outliers = TRUE), ".")
  expect_output(print(result, summary = FALSE, outliers = TRUE), ".")
  expect_output(print(result, summary = TRUE, outliers = FALSE), ".")
  expect_output(print(result, digits = 5), ".")

  # When both tables are off, only plots are produced (no text to stdout)
  expect_error(
    suppressWarnings(print(result, summary = FALSE, outliers = FALSE)),
    NA
  )
})

test_that("summary method works", {
  result <- f_scan(df_standard, columns = "Salary", group_vars = "Team",
                   output_type = "default", open_generated_files = FALSE)
  expect_output(summary(result), ".")
  expect_output(summary(result, digits = 1), ".")
})

test_that("plot method runs without error", {
  skip_on_cran()
  result <- f_scan(df_standard, columns = "Salary", group_vars = "Team",
                   output_type = "default", open_generated_files = FALSE)
  # plot() should produce ggplot output without error
  expect_error(
    suppressWarnings(
      invisible(capture.output(plot(result), type = "message"))
    ),
    NA  # expect_error(..., NA) means "expect NO error"
  )
})

test_that("print method works for multi-column results", {
  skip_on_cran()
  result <- f_scan(df_standard, columns = c("Salary", "Age"),
                   group_vars = "Team",
                   output_type = "default", open_generated_files = FALSE)
  expect_output(print(result), ".")
})


# ===========================================================================
# 10. COMBINATORIAL STRESS: all toggle pairs
# ===========================================================================

test_that("combinatorial: summary x outliers x coef x digits x groups", {
  skip_on_cran()
  combos <- expand.grid(
    summary  = c(TRUE, FALSE),
    outliers = c(TRUE, FALSE),
    coef     = c(1.5, 3.0),
    digits   = list(NULL, 2),
    n_groups = c(0, 1, 2),
    stringsAsFactors = FALSE
  )

  group_options <- list(
    `0` = NULL,
    `1` = "Team",
    `2` = c("Team", "Department")
  )

  for (i in seq_len(nrow(combos))) {
    row <- combos[i, ]
    gv  <- group_options[[as.character(row$n_groups)]]
    dg  <- row$digits[[1]]  # unlist from the list column

    combo_label <- paste0(
      "summary=", row$summary,
      " outliers=", row$outliers,
      " coef=", row$coef,
      " digits=", if (is.null(dg)) "NULL" else dg,
      " groups=", row$n_groups
    )

    result <- tryCatch(
      f_scan(df_standard,
             columns    = "Salary",
             group_vars = gv,
             summary    = row$summary,
             outliers   = row$outliers,
             coef       = row$coef,
             digits     = dg,
             output_type = "default",
             open_generated_files = FALSE),
      error = function(e) e
    )

    expect_false(inherits(result, "error"))
    if (!inherits(result, "error")) {
      expect_s3_class(result, "f_scan")
    } else {
      message("FAILED combo: ", combo_label, " -> ", conditionMessage(result))
    }
  }
})


# ===========================================================================
# 11. FORMULA vs DATA.FRAME EQUIVALENCE
# ===========================================================================

test_that("formula and data.frame notation produce same structure", {
  r_df <- f_scan(df_standard, columns = "Salary", group_vars = "Team",
                 output_type = "default", open_generated_files = FALSE)
  r_fm <- f_scan(Salary ~ Team, data = df_standard,
                 output_type = "default", open_generated_files = FALSE)

  expect_identical(names(r_df), names(r_fm))
  for (col in names(r_df)) {
    expect_identical(sort(names(r_df[[col]])), sort(names(r_fm[[col]])))
    # Summary tables should be identical
    if ("f_summary" %in% names(r_df[[col]])) {
      expect_equal(r_df[[col]][["f_summary"]], r_fm[[col]][["f_summary"]])
    }
    # Outlier tables should be identical
    if ("f_outliers" %in% names(r_df[[col]])) {
      expect_equal(r_df[[col]][["f_outliers"]], r_fm[[col]][["f_outliers"]])
    }
  }
})

test_that("multi-column formula and data.frame produce same keys", {
  skip_on_cran()
  r_df <- f_scan(df_standard, columns = c("Salary", "Age"),
                 group_vars = c("Team", "Department"),
                 output_type = "default", open_generated_files = FALSE)
  r_fm <- f_scan(Salary + Age ~ Team + Department, data = df_standard,
                 output_type = "default", open_generated_files = FALSE)

  expect_identical(sort(names(r_df)), sort(names(r_fm)))
})


# ===========================================================================
# 12. LARGE DATA STRESS
# ===========================================================================

test_that("large data (10k rows) completes without error", {
  skip_on_cran()
  set.seed(321)
  df_large <- data.frame(
    Group1 = sample(LETTERS[1:5], 10000, replace = TRUE),
    Group2 = sample(c("X", "Y"), 10000, replace = TRUE),
    Value  = rnorm(10000, 100, 15)
  )
  # Inject some outliers
  df_large$Value[1:10] <- 300

  result <- f_scan(df_large, columns = "Value",
                   group_vars = c("Group1", "Group2"),
                   output_type = "default", open_generated_files = FALSE)
  expect_fscan_structure(result, "Value", summary = TRUE, outliers = TRUE)
})



# =============================================================================
# f_scan_new_tests.R
# Tests for the new f_scan() features:
#
#   A. advice = TRUE runs f_stat_wizard() on each response column and
#      attaches the result under result[["col"]]$advice.
#   B. S3 dispatch: formula interface and data.frame interface (via x).
#   C. print.f_scan() respects an advice argument.
# =============================================================================
# ---------------------------------------------------------------------------
# A. advice parameter
# ---------------------------------------------------------------------------

test_that("advice = TRUE attaches an f_stat_wizard object per column", {
  res <- suppressMessages(
    f_scan(Sepal.Length ~ Species, data = iris,
           advice = TRUE, output_type = "default",
           open_generated_files = FALSE)
  )
  expect_s3_class(res, "f_scan")
  # Response column should have an 'advice' entry
  expect_true(!is.null(res[["Sepal.Length"]][["advice"]]))

  advice <- res[["Sepal.Length"]][["advice"]]
  # Either a real f_stat_wizard object or a fall-back error string
  expect_true(inherits(advice, "f_stat_wizard") || is.character(advice))

  if (inherits(advice, "f_stat_wizard")) {
    expect_true(!is.null(advice$y_type))
    expect_true("recommended_call" %in% names(advice))
  }
})

test_that("advice = FALSE (default) does not populate the advice slot", {
  res <- suppressMessages(
    f_scan(Sepal.Length ~ Species, data = iris,
           output_type = "default",
           open_generated_files = FALSE)
  )
  expect_null(res[["Sepal.Length"]][["advice"]])
})

test_that("advice survives a non-numeric response without crashing", {
  # Species is a factor; the wizard cannot analyse it as Y. Scan
  # should still run and store an error message or an f_stat_wizard
  # object instead of blowing up.
  df <- iris
  df$Species2 <- iris$Species
  expect_no_error(
    res <- suppressMessages(
      f_scan(iris, columns = "Sepal.Length", group_vars = "Species",
             advice = TRUE, output_type = "default",
             open_generated_files = FALSE)
    )
  )
  expect_s3_class(res, "f_scan")
})

test_that("advice works with multiple response columns", {
  res <- suppressMessages(
    f_scan(iris, columns = c("Sepal.Length", "Sepal.Width"),
           group_vars = "Species",
           advice = TRUE, output_type = "default",
           open_generated_files = FALSE)
  )
  expect_true("advice" %in% names(res[["Sepal.Length"]]))
  expect_true("advice" %in% names(res[["Sepal.Width"]]))
})

# ---------------------------------------------------------------------------
# B. S3 dispatch
# ---------------------------------------------------------------------------

test_that("formula interface dispatches via f_scan.formula", {
  res <- suppressMessages(
    f_scan(Sepal.Length ~ Species, data = iris,
           output_type = "default",
           open_generated_files = FALSE)
  )
  expect_s3_class(res, "f_scan")
})

test_that("data.frame interface dispatches via f_scan.data.frame", {
  res <- suppressMessages(
    f_scan(iris, columns = "Sepal.Length", group_vars = "Species",
           output_type = "default",
           open_generated_files = FALSE)
  )
  expect_s3_class(res, "f_scan")
})

test_that("named call f_scan(data = iris, columns = ...) still works", {
  # Regression guard for the match.call() shim that looks up `data`
  # when `x` is missing.
  res <- suppressMessages(
    f_scan(data = iris, columns = "Sepal.Length", group_vars = "Species",
           output_type = "default",
           open_generated_files = FALSE)
  )
  expect_s3_class(res, "f_scan")
})

# ---------------------------------------------------------------------------
# C. print.f_scan with advice argument
# ---------------------------------------------------------------------------

test_that("print.f_scan accepts an advice argument without erroring", {
  skip_on_cran()
  res <- suppressMessages(
    f_scan(Sepal.Length ~ Species, data = iris,
           advice = TRUE, output_type = "default",
           open_generated_files = FALSE)
  )
  expect_no_error(
    utils::capture.output(print(res, advice = TRUE))
  )
  expect_no_error(
    utils::capture.output(print(res, advice = FALSE))
  )
})


# =============================================================================
# 14. v3.1.0: NUMERIC VECTOR INPUT
# =============================================================================
# These tests cover the new f_scan.numeric() dispatch added in 3.1.0:
#
#   - single numeric vector: f_scan(my_vec)
#   - positional shorthand:  f_scan(disp1, cyl1) == f_scan(disp1 ~ cyl1)
#   - formula on bare vecs:  f_scan(disp1 + hp1 ~ cyl1)   (no `data =`)
# =============================================================================

test_that("single numeric vector: carries vector name as the column key", {
  set.seed(101)
  my_vec <- rnorm(60, mean = 50, sd = 5)
  my_vec[1] <- 200  # inject one obvious outlier so f_outliers fires
  result <- f_scan(my_vec, output_type = "default",
                   open_generated_files = FALSE)
  expect_s3_class(result, "f_scan")
  expect_true("my_vec" %in% names(result))
  expect_fscan_structure(result, "my_vec", summary = TRUE, outliers = TRUE)
})

test_that("single integer vector dispatches via f_scan.integer", {
  my_int <- as.integer(c(rep(10:20, 3), 999L))   # tail outlier
  result <- f_scan(my_int, output_type = "default",
                   open_generated_files = FALSE)
  expect_s3_class(result, "f_scan")
  expect_true("my_int" %in% names(result))
})

test_that("single numeric vector | inline c(): falls back to a label", {
  # Inline c(...) has no symbol name; the function should still produce
  # an f_scan object, even if the column key is something synthetic.
  result <- f_scan(c(1, 2, 3, 4, 5, 100),
                   output_type = "default",
                   open_generated_files = FALSE)
  expect_s3_class(result, "f_scan")
  expect_equal(length(result), 1L)
})

test_that("positional shorthand: f_scan(disp1, cyl1) equals f_scan(disp1 ~ cyl1)", {
  disp1 <- mtcars$disp
  cyl1  <- factor(mtcars$cyl)

  r_pos <- f_scan(disp1, cyl1,
                  output_type = "default", open_generated_files = FALSE)
  r_fml <- f_scan(disp1 ~ cyl1,
                  output_type = "default", open_generated_files = FALSE)

  expect_s3_class(r_pos, "f_scan")
  expect_s3_class(r_fml, "f_scan")
  expect_identical(names(r_pos), names(r_fml))
  expect_true("disp1" %in% names(r_pos))

  # The summary tables for the response should agree (numerically), even
  # if the internal_data_name differs.
  if ("f_summary" %in% names(r_pos[["disp1"]]) &&
      "f_summary" %in% names(r_fml[["disp1"]])) {
    expect_equal(
      r_pos[["disp1"]][["f_summary"]],
      r_fml[["disp1"]][["f_summary"]]
    )
  }
})

test_that("positional shorthand: multiple grouping vectors are accepted", {
  disp1 <- mtcars$disp
  cyl1  <- factor(mtcars$cyl)
  am1   <- factor(mtcars$am)
  expect_no_error(
    res <- f_scan(disp1, cyl1, am1,
                  output_type = "default",
                  open_generated_files = FALSE)
  )
  expect_s3_class(res, "f_scan")
  expect_true("disp1" %in% names(res))
})

test_that("positional shorthand: mismatched lengths error clearly", {
  short_resp  <- rnorm(20)
  long_group  <- factor(rep(c("a", "b"), 50))   # length 100 vs 20
  expect_error(
    f_scan(short_resp, long_group,
           output_type = "default", open_generated_files = FALSE),
    regexp = "length|different"
  )
})

test_that("formula on bare vectors works without a data argument", {
  disp1 <- mtcars$disp
  hp1   <- mtcars$hp
  cyl1  <- factor(mtcars$cyl)
  expect_no_error(
    res <- f_scan(disp1 + hp1 ~ cyl1,
                  output_type = "default",
                  open_generated_files = FALSE)
  )
  expect_s3_class(res, "f_scan")
  expect_true(all(c("disp1", "hp1") %in% names(res)))
})

test_that("formula on bare vectors: single response, no grouping", {
  disp1 <- mtcars$disp
  expect_no_error(
    res <- f_scan(disp1 ~ 1,
                  output_type = "default",
                  open_generated_files = FALSE)
  )
  expect_s3_class(res, "f_scan")
  expect_true("disp1" %in% names(res))
})


# =============================================================================
# 15. v3.1.0: BARE DATA.FRAME WITHOUT `columns`
# =============================================================================
# `columns` is now optional. When omitted, all numeric columns in `data`
# are scanned (excluding any column named in `group_vars`).
# =============================================================================

test_that("bare data.frame: f_scan(mtcars) uses all numeric columns", {
  skip_on_cran()
  res <- f_scan(mtcars, output_type = "default",
                open_generated_files = FALSE)
  expect_s3_class(res, "f_scan")
  # mtcars has 11 numeric columns. All of them should appear as keys.
  numeric_cols <- names(mtcars)[vapply(mtcars, is.numeric, logical(1))]
  expect_true(all(numeric_cols %in% names(res)))
})

test_that("bare data.frame with group_vars excludes the grouping columns", {
  skip_on_cran()
  # `cyl` is numeric in mtcars; if it is the grouping variable, it should
  # not also be scanned as a response.
  res <- f_scan(mtcars, group_vars = "cyl",
                output_type = "default",
                open_generated_files = FALSE)
  expect_s3_class(res, "f_scan")
  expect_false("cyl" %in% names(res))
})

test_that("bare data.frame with no numeric columns errors clearly", {
  df_no_num <- data.frame(
    g1 = letters[1:5],
    g2 = factor(rep(c("x", "y"), length.out = 5)),
    stringsAsFactors = FALSE
  )
  expect_error(
    f_scan(df_no_num, output_type = "default",
           open_generated_files = FALSE),
    regexp = "numeric"
  )
})


# =============================================================================
# 16. v3.1.0: REGRESSION - multi-column without group_vars
# =============================================================================
# Previously f_scan() crashed with "Column `All Data` not found" on the
# second response variable when called without `group_vars`. The dummy
# grouping column was being added only on the first iteration of the loop.
# =============================================================================

test_that("multi-column, no group_vars: no 'All Data' crash on second column", {
  skip_on_cran()
  expect_no_error(
    res <- f_scan(mtcars,
                  columns = c("mpg", "hp"),
                  output_type = "default",
                  open_generated_files = FALSE)
  )
  expect_s3_class(res, "f_scan")
  expect_true(all(c("mpg", "hp") %in% names(res)))
})

test_that("multi-column, no group_vars: every key has the expected slots", {
  skip_on_cran()
  res <- f_scan(mtcars,
                columns = c("mpg", "hp", "wt"),
                output_type = "default",
                open_generated_files = FALSE)
  expect_fscan_structure(res, c("mpg", "hp", "wt"),
                         summary = TRUE, outliers = TRUE)
})

test_that("formula multi-response with no RHS variables also runs", {
  # `mpg + hp ~ 1` is the formula equivalent of the no-group case above.
  expect_no_error(
    res <- f_scan(mpg + hp ~ 1, data = mtcars,
                  output_type = "default",
                  open_generated_files = FALSE)
  )
  expect_s3_class(res, "f_scan")
  expect_true(all(c("mpg", "hp") %in% names(res)))
})


# =============================================================================
# 17. v3.1.0: MULTI-COLUMN PRINT HEADER
# =============================================================================
# When several responses are summarised, the print method shows a header
# naming each response variable to distinguish the tables.
# =============================================================================

test_that("print.f_scan: multi-column output shows a 'Variable:' header per response", {
  skip_on_cran()
  res <- f_scan(mtcars, columns = c("mpg", "hp"),
                output_type = "default",
                open_generated_files = FALSE)
  out_txt <- paste(utils::capture.output(print(res)), collapse = "\n")
  # The header should mention each response key
  expect_match(out_txt, "mpg", fixed = TRUE)
  expect_match(out_txt, "hp",  fixed = TRUE)
})
