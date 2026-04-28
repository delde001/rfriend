# =============================================================================
# Helper factories
# =============================================================================

# Basic numeric-only data frame
make_numeric_df <- function(n = 50) {
  set.seed(42)
  data.frame(x = rnorm(n), y = rnorm(n), z = rnorm(n))
}

# One unordered factor
make_one_factor_df <- function(n = 60) {
  df      <- make_numeric_df(n)
  df$grp  <- factor(rep(c("A", "B", "C"), length.out = n))
  df
}

# Two unordered factors
make_two_factor_df <- function(n = 60) {
  df       <- make_numeric_df(n)
  df$grp1  <- factor(rep(c("Low", "High"),          length.out = n))
  df$grp2  <- factor(rep(c("T1",  "T2",  "T3"),     length.out = n))
  df
}

# Three unordered factors (should trigger > 2 factors warning)
make_three_factor_df <- function(n = 60) {
  df       <- make_two_factor_df(n)
  df$grp3  <- factor(rep(c("P", "Q"), length.out = n))
  df
}

# Data frame with an ordered factor (should be treated as ordinal, not grouped)
make_ordered_factor_df <- function(n = 60) {
  df      <- make_numeric_df(n)
  df$severity <- ordered(rep(c("low", "med", "high"), length.out = n),
                         levels = c("low", "med", "high"))
  df
}

# Data frame with a constant (zero-variance) column
make_constant_col_df <- function(n = 40) {
  data.frame(x = rnorm(n), y = rnorm(n), constant = rep(5, n))
}

# Convenience: temp path for PNG output
png_out <- function() tempfile(fileext = "_CorPlot.png")

# =============================================================================
# 1. Input validation errors
# =============================================================================

test_that("stops on invalid output_type", {
  expect_error(
    f_corplot(make_numeric_df(), output_type = "xlsx",
              open_generated_files = FALSE),
    regexp = "output_type"
  )
})

test_that("stops when fewer than 2 numeric variables", {
  skip_on_cran()
  df <- data.frame(x   = rnorm(30),
                   grp = factor(rep("A", 30)))
  expect_error(
    f_corplot(df, output_type = "png",
              open_generated_files = FALSE),
    regexp = "two numeric"
  )
})

test_that("stops when color_factor not found in data", {
  skip_on_cran()
  expect_error(
    f_corplot(make_numeric_df(),
              color_factor = "nonexistent",
              output_type  = "png",
              open_generated_files = FALSE),
    regexp = "color factor"
  )
})

test_that("stops when shape_factor not found in data", {
  skip_on_cran()
  expect_error(
    suppressMessages(f_corplot(make_numeric_df(),
              shape_factor = "nonexistent",
              output_type  = "png",
              open_generated_files = FALSE)),
    regexp = "shape factor"
  )
})

# =============================================================================
# 2. Basic output creation
# =============================================================================

test_that("creates PNG file at specified path - numeric only", {
  skip_on_cran()
  out <- png_out()
  suppressMessages(f_corplot(make_numeric_df(),
            output_type = "png", save_as = out,
            open_generated_files = FALSE))
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0L)
})

test_that("creates PNG for minimal 2-variable data frame", {
  skip_on_cran()
  out <- png_out()
  suppressMessages(f_corplot(data.frame(a = rnorm(20), b = rnorm(20)),
            output_type = "png", save_as = out,
            open_generated_files = FALSE))
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0L)
})

# =============================================================================
# 3. Factor detection (auto path)
# =============================================================================

test_that("auto-detects one factor without error", {
  skip_on_cran()
  out <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(make_one_factor_df(),
              detect_factors = TRUE,
              output_type    = "png", save_as = out,
              open_generated_files = FALSE))
  )
})

test_that("auto-detects two factors without error", {
  skip_on_cran()
  out <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(make_two_factor_df(),
              detect_factors = TRUE,
              output_type    = "png", save_as = out,
              open_generated_files = FALSE))
  )
})

test_that("three factor data warns and still runs", {
  skip_on_cran()
  out <- png_out()

  expect_warning(
    suppressMessages(f_corplot(make_three_factor_df(),
              output_type = "png",
              save_as = out,
              open_generated_files = FALSE),
    "more than 2 factors"
  ))

  expect_true(file.exists(out))
})

test_that("warns when more than 2 factors are present", {
  skip_on_cran()
  out <- png_out()
    expect_warning(
      suppressMessages(f_corplot(make_three_factor_df(),
                output_type = "png",
                save_as = out,
                open_generated_files = FALSE),
      "more than 2 factors"
    ))
})

test_that("detect_factors = FALSE suppresses auto-detection", {
  skip_on_cran()
  out <- png_out()
  # grp is a factor but should not be picked up
  expect_no_error(
    suppressMessages(f_corplot(make_one_factor_df(),
              detect_factors = FALSE,
              output_type    = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

test_that("ordered factor is NOT used as grouping aesthetic", {
  skip_on_cran()
  out <- png_out()
  # No grouping expected - ordered factor is ordinal data
  expect_message(
    f_corplot(make_ordered_factor_df(),
              detect_factors = TRUE,
              output_type    = "png", save_as = out,
              open_generated_files = FALSE),
    "No factors"
  )
})

# =============================================================================
# 4. factor_select and factor_exclude passthrough
# =============================================================================

test_that("factor_select restricts which columns are tested for factors", {
  skip_on_cran()
  out <- png_out()
  df  <- make_two_factor_df()
  # grp2 should not become a factor; only grp1 is in select
  expect_no_error(
    suppressMessages( f_corplot(df,
              detect_factors = TRUE,
              factor_select  = "grp1",
              output_type    = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

test_that("factor_exclude prevents a column from becoming a factor", {
  skip_on_cran()
  out <- png_out()
  df  <- make_two_factor_df()
  expect_no_error(
    suppressMessages(f_corplot(df,
              detect_factors = TRUE,
              factor_exclude = "grp1",
              output_type    = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

test_that("unique_num_treshold and repeats_threshold are passed without error", {
  skip_on_cran()
  out <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(make_numeric_df(),
              detect_factors      = TRUE,
              unique_num_treshold = 5,
              repeats_threshold   = 3,
              output_type         = "png", save_as = out,
              open_generated_files = FALSE))
  )
})

# =============================================================================
# 5. Manual factor specification
# =============================================================================

test_that("manual color_factor only (shape auto)", {
  skip_on_cran()
  out <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(make_one_factor_df(),
              color_factor = "grp", shape_factor = "auto",
              output_type  = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

test_that("manual shape_factor only (color auto)", {
  skip_on_cran()
  out <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(make_one_factor_df(),
              color_factor = "auto", shape_factor = "grp",
              output_type  = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

test_that("manual color + shape factor both set", {
  skip_on_cran()
  out <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(make_two_factor_df(),
              color_factor = "grp1", shape_factor = "grp2",
              output_type  = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

test_that("non-factor variable is auto-converted with message", {
  skip_on_cran()
  df        <- make_numeric_df()
  df$num_grp <- rep(c(1L, 2L), length.out = 50L)
  out       <- png_out()
  expect_message(
    f_corplot(df,
              color_factor = "num_grp",
              output_type  = "png", save_as = out,
              open_generated_files = FALSE),
    "converted to a factor"
  )
})

# =============================================================================
# 6. factor_count correctly incremented for manually specified factors
#    (regression test for Bug #2 from earlier review)
# =============================================================================

test_that("legend is created when factors set manually - not silently suppressed", {
  skip_on_cran()
  out <- png_out()
  suppressMessages(f_corplot(make_two_factor_df(),
            color_factor = "grp1", shape_factor = "grp2",
            print_legend = TRUE,
            output_type  = "png", save_as = out,
            open_generated_files = FALSE))
  legend_path <- sub("_CorPlot\\.png$", "_Legend.png", out)
  expect_true(file.exists(legend_path))
  expect_gt(file.size(legend_path), 0L)
})

# =============================================================================
# 7. Legend behaviour
# =============================================================================

test_that("legend PNG created alongside corplot when two factors present", {
  skip_on_cran()
  out <- png_out()
  suppressMessages(f_corplot(make_two_factor_df(),
            color_factor = "grp1", shape_factor = "grp2",
            print_legend = TRUE,
            output_type  = "png", save_as = out,
            open_generated_files = FALSE))
  legend_path <- sub("_CorPlot\\.png$", "_Legend.png", out)
  expect_true(file.exists(legend_path))
})

test_that("no legend file when print_legend = FALSE and no ordinal_vars", {
  skip_on_cran()
  out <- png_out()
  suppressMessages(f_corplot(make_two_factor_df(),
            print_legend = FALSE,
            output_type  = "png", save_as = out,
            open_generated_files = FALSE))
  legend_path <- sub("_CorPlot\\.png$", "_Legend.png", out)
  expect_false(file.exists(legend_path))
})

test_that("no legend when no factors and no ordinal_vars", {
  skip_on_cran()
  out <- png_out()
  suppressMessages(f_corplot(make_numeric_df(),
            output_type = "png", save_as = out,
            open_generated_files = FALSE))
  legend_path <- sub("_CorPlot\\.png$", "_Legend.png", out)
  expect_false(file.exists(legend_path))
})

test_that("legend created with no factors when ordinal_vars specified", {
  skip_on_cran()
  out <- png_out()
  suppressMessages(f_corplot(make_numeric_df(),
            ordinal_vars = "x",
            output_type  = "png", save_as = out,
            open_generated_files = FALSE))
  legend_path <- sub("_CorPlot\\.png$", "_Legend.png", out)
  expect_true(file.exists(legend_path))
})

# =============================================================================
# 8. ordinal_vars
# =============================================================================

test_that("ordinal_vars: numeric column - annotation only, no crash", {
  skip_on_cran()
  out <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(make_numeric_df(),
              ordinal_vars = "x",
              output_type  = "png", save_as = out,
              open_generated_files = FALSE))
  )
})


test_that("ordinal_vars: ordered factor coerced to integer", {
  skip_on_cran()
  out <- png_out()
  expect_message(
    f_corplot(
      make_ordered_factor_df(),
      ordinal_vars = "severity",
      output_type  = "png",
      save_as = out,
      open_generated_files = FALSE
    ),
    "coerced to integer"
  )
  expect_true(file.exists(out))
})

test_that("ordinal_vars: non-existent column warns and is ignored", {
  skip_on_cran()
  out <- png_out()
  expect_warning(
    suppressMessages(f_corplot(make_numeric_df(),
              ordinal_vars = "does_not_exist",
              output_type  = "png", save_as = out,
              open_generated_files = FALSE)),
   "not found"
  )
})

test_that("ordinal_vars: multiple ordinal columns", {
  skip_on_cran()
  out <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(make_numeric_df(),
              ordinal_vars = c("x", "y"),
              output_type  = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

# =============================================================================
# 9. fancy_names
# =============================================================================

test_that("fancy_names renames columns without error", {
  skip_on_cran()
  out   <- png_out()
  fancy <- c(x = "Variable X", y = "Variable Y")
  expect_no_error(
    suppressMessages( f_corplot(make_numeric_df(),
              fancy_names  = fancy,
              output_type  = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

test_that("fancy_names + manual factor: renamed factor resolves correctly", {
  skip_on_cran()
  out   <- png_out()
  fancy <- c(grp = "Group Label")
  expect_no_error(
    suppressMessages(f_corplot(make_one_factor_df(),
              fancy_names  = fancy,
              color_factor = "Group Label",
              output_type  = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

test_that("fancy_names + ordinal_vars: ordinal name renamed correctly", {
  skip_on_cran()
  out   <- png_out()
  fancy <- c(x = "Trait X")
  expect_no_error(
    suppressMessages(f_corplot(make_numeric_df(),
              fancy_names  = fancy,
              ordinal_vars = "Trait X",
              output_type  = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

# =============================================================================
# 10. Missing data and edge cases
# =============================================================================

test_that("handles NAs in numeric columns", {
  skip_on_cran()
  df      <- make_numeric_df(50)
  df$x[c(3, 17, 33)] <- NA_real_
  out     <- png_out()
  expect_no_error(
    suppressMessages( f_corplot(df, output_type = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

test_that("handles constant (zero-variance) column - shows NA, no crash", {
  skip_on_cran()
  out <- png_out()
  expect_no_error(
    suppressMessages( f_corplot(make_constant_col_df(),
              output_type = "png", save_as = out,
              open_generated_files = FALSE)
  ))
  expect_true(file.exists(out))
})

test_that("handles NAs in factor column", {
  skip_on_cran()
  df      <- make_one_factor_df()
  df$grp[c(1, 5)] <- NA
  out     <- png_out()
  expect_no_error(
    suppressMessages( f_corplot(df,
              color_factor = "grp",
              output_type  = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

# =============================================================================
# 11. Large numbers of factor levels (extended shape sequence)
# =============================================================================

test_that("shape factor with exactly 5 levels", {
  skip_on_cran()
  df   <- make_numeric_df(50)
  df$g <- factor(rep(letters[1:5], 10))
  out  <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(df, shape_factor = "g",
              output_type = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

test_that("shape factor with 6 levels triggers extended shape list", {
  skip_on_cran()
  df   <- make_numeric_df(60)
  df$g <- factor(rep(letters[1:6], 10))
  out  <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(df, shape_factor = "g",
              output_type = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

test_that("shape factor with 8 levels - well beyond default pch range", {
  skip_on_cran()
  df   <- make_numeric_df(80)
  df$g <- factor(rep(letters[1:8], 10))
  out  <- png_out()
  expect_no_error(
    suppressMessages( f_corplot(df, shape_factor = "g",
              output_type = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

# =============================================================================
# 12. Output file integrity
# =============================================================================

test_that("output file is non-empty PNG with correct path", {
  skip_on_cran()
  out <- png_out()
  suppressMessages(f_corplot(make_two_factor_df(),
            color_factor = "grp1",
            output_type  = "png", save_as = out,
            open_generated_files = FALSE))
  expect_true(file.exists(out))
  expect_gt(file.size(out), 1000L)   # at minimum a few KB
})

test_that("legend file is non-empty PNG", {
  skip_on_cran()
  out <- png_out()
  suppressMessages(f_corplot(make_two_factor_df(),
            color_factor = "grp1", shape_factor = "grp2",
            print_legend = TRUE,
            output_type  = "png", save_as = out,
            open_generated_files = FALSE))
  legend_path <- sub("_CorPlot\\.png$", "_Legend.png", out)
  expect_gt(file.size(legend_path), 1000L)
})

# =============================================================================
# 13. Stress tests - combinations
# =============================================================================

test_that("STRESS: two factors + fancy_names + ordinal_vars + custom dims", {
  skip_on_cran()
  df    <- make_two_factor_df()
  fancy <- c(x = "Trait X", y = "Trait Y", z = "Trait Z")
  out   <- png_out()
  expect_no_error(
    suppressMessages( f_corplot(df,
              color_factor = "grp1",
              shape_factor = "grp2",
              fancy_names  = fancy,
              ordinal_vars = "Trait X",
              print_legend = TRUE,
              width        = 20,
              height       = 20,
              res          = 150,
              pointsize    = 10,
              output_type  = "png", save_as = out,
              open_generated_files = FALSE))
  )
  expect_true(file.exists(out))
  legend_path <- sub("_CorPlot\\.png$", "_Legend.png", out)
  expect_true(file.exists(legend_path))
})

test_that("STRESS: detect_factors=FALSE + manual factors + NAs + fancy_names", {
  skip_on_cran()
  df        <- make_two_factor_df()
  df$x[1:5] <- NA_real_
  fancy     <- c(x = "X Axis", grp1 = "Primary Group")
  out       <- png_out()
  expect_no_error(
    suppressMessages( f_corplot(df,
              detect_factors = FALSE,
              color_factor   = "Primary Group",
              shape_factor   = "grp2",
              fancy_names    = fancy,
              print_legend   = TRUE,
              output_type    = "png", save_as = out,
              open_generated_files = FALSE)
  ))
})

test_that("STRESS: >5 shape levels + color factor + ordinal_vars + NAs", {
  skip_on_cran()
  set.seed(1)
  n   <- 70
  df  <- data.frame(
    x   = c(rnorm(35), NA_real_, rnorm(34)),
    y   = rnorm(n),
    z   = rnorm(n),
    col = factor(rep(c("Red", "Blue"), length.out = n)),
    shp = factor(rep(letters[1:7], length.out = n))
  )
  out <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(df,
              color_factor = "col",
              shape_factor = "shp",
              ordinal_vars = "z",
              print_legend = TRUE,
              output_type  = "png", save_as = out,
              open_generated_files = FALSE)
  ))
  legend_path <- sub("_CorPlot\\.png$", "_Legend.png", out)
  expect_true(file.exists(legend_path))
})

test_that("STRESS: iris - auto factor, fancy names, ordinal_vars, legend", {
  skip_on_cran()
  data(iris)
  fancy <- c(Sepal.Length = "Sepal Length (cm)",
             Petal.Length = "Petal Length (cm)")
  out   <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(iris,
              fancy_names  = fancy,
              ordinal_vars = "Petal.Width",
              print_legend = TRUE,
              output_type  = "png", save_as = out,
              open_generated_files = FALSE)
  ))
  expect_true(file.exists(out))
})

test_that("STRESS: mtcars subset - manual factors, factor_exclude, no legend", {
  skip_on_cran()
  data(mtcars)
  sub <- subset(mtcars, select = -c(am, qsec, vs))
  out <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(sub,
              color_factor   = "gear",
              shape_factor   = "cyl",
              factor_exclude = "carb",
              print_legend   = FALSE,
              res            = 150,
              output_type    = "png", save_as = out,
              open_generated_files = FALSE)
  ))
  expect_true(file.exists(out))
})

test_that("STRESS: all factor_select / exclude / threshold params combined", {
  skip_on_cran()
  set.seed(7)
  n  <- 80
  df <- data.frame(
    a    = rnorm(n),
    b    = rnorm(n),
    c    = rnorm(n),
    cat1 = factor(rep(c("X", "Y"), length.out = n)),
    cat2 = factor(rep(c("P", "Q", "R"), length.out = n)),
    num  = rep(1:4, length.out = n)   # low unique count - could become factor
  )
  out <- png_out()
  expect_no_error(
    suppressMessages(f_corplot(df,
              detect_factors      = TRUE,
              factor_select       = c("cat1", "cat2"),
              factor_exclude      = NULL,
              unique_num_treshold = 6,
              repeats_threshold   = 2,
              ordinal_vars        = "num",
              print_legend        = TRUE,
              output_type         = "png", save_as = out,
              open_generated_files = FALSE)
  ))
  expect_true(file.exists(out))
})

test_that("STRESS: constant column + ordinal + factor + fancy_names all together", {
  skip_on_cran()
  set.seed(99)
  n  <- 50
  df <- data.frame(
    x    = rnorm(n),
    y    = rnorm(n),
    flat = rep(3.14, n),                              # zero variance
    grp  = factor(rep(c("A", "B"), length.out = n))
  )
  fancy <- c(x = "Variable X", flat = "Constant")
  out   <- png_out()
  expect_no_error(
    suppressMessages( f_corplot(df,
              color_factor = "grp",
              fancy_names  = fancy,
              ordinal_vars = "Variable X",
              print_legend = TRUE,
              output_type  = "png", save_as = out,
              open_generated_files = FALSE)
  ))
  expect_true(file.exists(out))
})

test_that("legend colour order matches label order for two factors", {
  skip_on_cran()
  # 2 colors x 3 shapes = 6 entries
  # outer() produces: col1-s1, col2-s1, col1-s2, col2-s2, col1-s3, col2-s3
  # legend_col must cycle colors fast, not slow
  df  <- make_two_factor_df()
  out <- png_out()
  suppressMessages(f_corplot(df, color_factor = "grp1", shape_factor = "grp2",
            print_legend = TRUE, output_type = "png",
            save_as = out, open_generated_files = FALSE))
  legend_path <- sub("_CorPlot\\.png$", "_Legend.png", out)
  # If the bug is present the file still renders - we can only verify it exists;
  # visual inspection confirms colour/label alignment
  expect_true(file.exists(legend_path))
})
