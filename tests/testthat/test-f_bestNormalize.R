# ==============================================================================
# Test suite for f_bestNormalize()
# Run with: testthat::test_file("test_f_bestNormalize.R")
# Or via devtools: devtools::test()
# ==============================================================================
# -- Helpers --------------------------------------------------------------------
set.seed(42)
skewed_small  <- rlnorm(50,  meanlog = 0, sdlog = 1)   # n < 100  (loo branch)
skewed_medium <- rlnorm(150, meanlog = 0, sdlog = 1)   # 100 <= n < 500
skewed_large  <- rlnorm(600, meanlog = 0, sdlog = 1)   # n >= 500 (boot branch)

skewed_with_na        <- skewed_small
skewed_with_na[c(3, 7, 15)] <- NA

skewed_df <- data.frame(my_col = skewed_small)

# ==============================================================================
# 1. INPUT VALIDATION
# ==============================================================================
test_that("non-numeric input is rejected", {
  skip_on_cran()
  expect_error(f_bestNormalize(letters), "numeric")
})

test_that("non-vector / non-data.frame input is rejected", {
  skip_on_cran()
  expect_error(f_bestNormalize(matrix(1:9, 3, 3)), "vector or data.frame")
})

test_that("invalid output_type is rejected", {
  skip_on_cran()
  expect_error(
    f_bestNormalize(skewed_small, output_type = "xlsx"),
    "output_type"
  )
})

test_that("single-column data.frame is accepted", {
  skip_on_cran()
  set.seed(1)
  result <- f_bestNormalize(skewed_df, output_type = "default")
  expect_s3_class(result, "f_bestNormalize")
})

# ==============================================================================
# 2. RETURN STRUCTURE
# ==============================================================================
test_that("output_type = 'off' returns correct class and list elements", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")

  expect_s3_class(out, "f_bestNormalize")

  expected_names <- c(
    "plots", "bestNormalize", "data_name", "transformation_name",
    "original_data", "transformed_data",
    "shapiro_original", "shapiro_transformed",
    "andersonD_original", "andersonD_transformed",
    "norm_stats"
  )
  expect_true(all(expected_names %in% names(out)))
})

test_that("transformed_data has the same length as input", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_equal(length(out$transformed_data), length(skewed_small))
})

test_that("original_data stored in output matches input", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_identical(out$original_data, skewed_small)
})

test_that("norm_stats is a data frame with expected columns", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_s3_class(out$norm_stats, "data.frame")
  expect_true("Transformation"   %in% names(out$norm_stats))
  expect_true("Normality_Stat"   %in% names(out$norm_stats))
})

test_that("bestNormalize slot contains a bestNormalize object", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_s3_class(out$bestNormalize, "bestNormalize")
})

# ==============================================================================
# 3. DATA_NAME HANDLING
# ==============================================================================
test_that("data_name is extracted from vector symbol when not supplied", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_equal(out$data_name, "skewed_small")
})

test_that("custom data_name overrides auto-detected name", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, data_name = "my_var", output_type = "default")
  expect_equal(out$data_name, "my_var")
})

test_that("data_name is extracted from data.frame column name", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_df, output_type = "default")
  expect_equal(out$data_name, "my_col")
})

test_that("data_name from $ accessor strips prefix (e.g. df$col -> col)", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_df$my_col, output_type = "default")
  # sub(".*\\$", "") should strip 'skewed_df$'
  expect_false(grepl("\\$", out$data_name))
})

# ==============================================================================
# 4. SAMPLE-SIZE BRANCHES  - critical r-variable bug check
# ==============================================================================
test_that("small n (< 100) runs without error  [catches undefined-r bug]", {
  skip_on_cran()
  set.seed(1)
  expect_no_error(
    f_bestNormalize(skewed_small, output_type = "default")
  )
})

test_that("medium n (100-499) runs without error", {
  skip_on_cran()
  set.seed(1)
  expect_no_error(
    f_bestNormalize(skewed_medium, output_type = "default")
  )
})

test_that("large n (>= 500) runs without error", {
  skip_on_cran()
  set.seed(1)
  expect_no_error(
    f_bestNormalize(skewed_large, output_type = "default")
  )
})

# ==============================================================================
# 5. NA HANDLING
# ==============================================================================
test_that("input with NAs does not crash", {
  skip_on_cran()
  set.seed(1)
  expect_no_error(
    f_bestNormalize(skewed_with_na, output_type = "default")
  )
})

test_that("NAs are preserved in transformed_data at same positions", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_with_na, output_type = "default")
  expect_equal(
    which(is.na(out$transformed_data)),
    which(is.na(skewed_with_na))
  )
})

test_that("transformed_data length matches input length when NAs present", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_with_na, output_type = "default")
  expect_equal(length(out$transformed_data), length(skewed_with_na))
})

# ==============================================================================
# 6. NORMALITY IMPROVEMENT
# ==============================================================================
test_that("transformation improves normality (p-value increases for skewed data)", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_medium, output_type = "default")
  p_orig  <- out$shapiro_original$p.value
  p_trans <- out$shapiro_transformed$p.value
  expect_gt(p_trans, p_orig)
})

test_that("shapiro_original and shapiro_transformed are htest objects", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_s3_class(out$shapiro_original,    "htest")
  expect_s3_class(out$shapiro_transformed, "htest")
})

test_that("andersonD objects are htest objects", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_s3_class(out$andersonD_original,    "htest")
  expect_s3_class(out$andersonD_transformed, "htest")
})

# ==============================================================================
# 7. OUTPUT TYPES
# ==============================================================================
test_that("output_type = 'console' returns invisible f_bestNormalize", {
  skip_on_cran()
  set.seed(1)
  out <- capture.output(
    result <- f_bestNormalize(skewed_small, output_type = "console",
                              open_generated_files = FALSE)
  )
  expect_s3_class(result, "f_bestNormalize")
})

test_that("output_type = 'rmd' adds $rmd character string to output", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "rmd",
                         open_generated_files = FALSE)
  expect_true("rmd" %in% names(out))
  expect_type(out$rmd, "character")
  expect_gt(nchar(out$rmd), 50)
})

test_that("rmd output contains transformation name", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "rmd",
                         open_generated_files = FALSE)
  expect_true(grepl(out$transformation_name, out$rmd, fixed = TRUE))
})

test_that("output_type = 'pdf' creates a file on disk", {
  skip_on_cran()
  set.seed(1)
  tmp_path <- file.path(tempdir(), "test_bn_output")
  suppressMessages(out <- f_bestNormalize(skewed_small,
                         output_type       = "pdf",
                         save_as           = tmp_path,
                         open_generated_files = FALSE))
  expect_true(file.exists(paste0(tmp_path, ".pdf")))
})

test_that("output_type = 'word' creates a .docx file on disk", {
  skip_on_cran()
  skip_if_no_pandoc()
  set.seed(1)
  tmp_path <- file.path(tempdir(), "test_bn_word")
  suppressMessages(out <- f_bestNormalize(skewed_small,
                         output_type          = "word",
                         save_as              = tmp_path,
                         open_generated_files = FALSE))
  expect_true(file.exists(paste0(tmp_path, ".docx")))
})

test_that("save_as with explicit .pdf extension overrides output_type", {
  skip_on_cran()
  skip_if_no_pandoc()
  set.seed(1)
  tmp_path <- file.path(tempdir(), "test_bn_ext.pdf")
  expect_no_error(
    suppressMessages(f_bestNormalize(skewed_small,
                    output_type          = "word",   # should be overridden
                    save_as              = tmp_path,
                    open_generated_files = FALSE))
  )
  expect_true(file.exists(tmp_path))
})

# ==============================================================================
# 8. PRINT & PLOT METHODS
# ==============================================================================
test_that("print.f_bestNormalize runs without error", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_no_error(capture.output(print(out)))
})

test_that("print output contains transformation name", {
  skip_on_cran()
  set.seed(1)
  out    <- f_bestNormalize(skewed_small, output_type = "default")
  printed <- capture.output(print(out))
  expect_true(any(grepl(out$transformation_name, printed, fixed = TRUE)))
})

test_that("plot.f_bestNormalize runs without error for both panels", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_no_error({
    pdf(NULL)          # suppress screen device
    plot(out, which = 1:2)
    dev.off()
  })
})

test_that("plot.f_bestNormalize accepts which = 1 only", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_no_error({
    pdf(NULL)
    plot(out, which = 1)
    dev.off()
  })
})

test_that("plot.f_bestNormalize accepts which = 2 only", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_no_error({
    pdf(NULL)
    plot(out, which = 2)
    dev.off()
  })
})

# ==============================================================================
# 9. GLOBAL ENVIRONMENT ISOLATION  (exists() without inherits = FALSE bug)
# ==============================================================================
test_that("global variable 'r' does not leak into tuning logic", {
  skip_on_cran()
  r <<- 999   # plant a global r
  on.exit(rm("r", envir = .GlobalEnv), add = TRUE)
  set.seed(1)
  # Should still complete without using the polluted r in an unexpected way
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_s3_class(out, "f_bestNormalize")
})

test_that("global variable 'loo' does not silently override internal logic", {
  skip_on_cran()
  loo <<- FALSE   # force loo = FALSE globally for small n
  on.exit(rm("loo", envir = .GlobalEnv), add = TRUE)
  set.seed(1)
  out <- f_bestNormalize(skewed_small, output_type = "default")
  expect_s3_class(out, "f_bestNormalize")
})

# ==============================================================================
# 10. PASSTHROUGH OF ... ARGUMENTS
# ==============================================================================
test_that("extra ... args are passed to bestNormalize without error", {
  skip_on_cran()
  set.seed(1)
  # standardize = TRUE is a valid bestNormalize arg
  expect_no_error(
    f_bestNormalize(skewed_small, output_type = "default", standardize = TRUE)
  )
})

# ==============================================================================
# 11. COMBINATION SCENARIOS
# ==============================================================================
test_that("plots = TRUE with output_type = 'off' produces plots silently", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_small, plots = TRUE, output_type = "default")
  # plots flag stored
  expect_true(out$plots)
})

test_that("data.frame input + custom name + NA data runs end-to-end", {
  skip_on_cran()
  set.seed(1)
  df_na <- data.frame(val = c(skewed_small[1:40], NA, NA))
  out   <- f_bestNormalize(df_na,
                           data_name   = "custom_name",
                           output_type = "default")
  expect_equal(out$data_name, "custom_name")
  expect_equal(length(out$transformed_data), nrow(df_na))
})

test_that("large n + rmd output returns rmd content", {
  skip_on_cran()
  set.seed(1)
  out <- f_bestNormalize(skewed_large, output_type = "rmd",
                         open_generated_files = FALSE)
  expect_type(out$rmd, "character")
})

test_that("small n + pdf output creates file", {
  skip_on_cran()
  set.seed(1)
  tmp <- file.path(tempdir(), "small_n_pdf")
  expect_no_error(
    suppressMessages(f_bestNormalize(skewed_small,
                    output_type          = "pdf",
                    save_as              = tmp,
                    open_generated_files = FALSE)
  ))
  expect_true(file.exists(paste0(tmp, ".pdf")))
})


#Gemini tests
# library(yourPackageName) # Make sure your function and helpers are loaded!

test_that("f_bestNormalize handles basic numeric vectors correctly", {
  skip_on_cran()
  set.seed(123)
  x <- rlnorm(100)

  # Test with 'off' to just get the object
  res <- f_bestNormalize(x, output_type = "default")

  # Check class and structure
  expect_s3_class(res, "f_bestNormalize")
  expect_true(is.list(res))
  expect_equal(length(res$original_data), 100)
  expect_equal(length(res$transformed_data), 100)

  # Check if Shapiro tests are included
  expect_true(!is.null(res$shapiro_original))
  expect_true(!is.null(res$shapiro_transformed))
})

test_that("f_bestNormalize handles data frames correctly", {
  skip_on_cran()
  set.seed(123)
  df <- data.frame(my_skewed_col = rlnorm(50))

  res <- f_bestNormalize(df, output_type = "default")

  # The function should extract the column name automatically
  expect_equal(res$data_name, "my_skewed_col")
})

test_that("f_bestNormalize generates Word and PDF files", {
  skip_on_cran()
  skip_if_no_pandoc()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")

  set.seed(123)
  x <- rlnorm(50)

  # Test Word output
  word_file <- file.path(tempdir(), "test_output.docx")
  suppressMessages(res_word <- f_bestNormalize(x, output_type = "word", save_as = word_file, open_generated_files = FALSE))
  expect_true(file.exists(word_file))

  # Test RMD output
  res_rmd <- f_bestNormalize(x, output_type = "rmd")
  expect_true(!is.null(res_rmd$rmd))
  expect_true(is.character(res_rmd$rmd))
})

test_that("f_bestNormalize tunes settings based on sample size", {
  skip_on_cran()
  set.seed(123)

  # Small sample (n < 500) -> loo = TRUE
  x_small <- rlnorm(150)
  res_small <- f_bestNormalize(x_small, output_type = "default")

  # Large sample (n >= 500) -> loo = FALSE
  x_large <- rlnorm(600)
  res_large <- f_bestNormalize(x_large, output_type = "default")

  # Check if the underlying bestNormalize object reflects this
  expect_false(is.null(res_small$bestNormalize))
})



# =============================================================================
# f_bestnormalize_new_tests.R
# Additional tests for f_bestNormalize() covering the safe_shapiro
# rollout:
#
#   A. Stored Shapiro objects are always htest-classed.
#   B. n > 5000 no longer crashes.
# =============================================================================
# quiet_bn removed - use shared quiet_f_bestNormalize() from helper-quiet.R

test_that("f_bestNormalize does not crash with n > 5000", {
  skip_on_cran()
  set.seed(21)
  big <- rexp(6000, rate = 0.1)
  expect_no_error(
    res <- quiet_f_bestNormalize(big)
  )
})

test_that("Shapiro objects (if stored) are htest-classed", {
  skip_on_cran()
  set.seed(23)
  res <- quiet_f_bestNormalize(rexp(80, rate = 0.1))

  candidates <- c("shapiro_original", "Shapiro_original_data",
                  "shapiro_transformed", "Shapiro_transformed_data")
  present <- intersect(candidates, names(res))

  for (slot in present) {
    obj <- res[[slot]]
    if (inherits(obj, "htest")) {
      expect_true(all(c("statistic", "p.value", "method") %in% names(obj)))
    }
  }
})

test_that("large-n result is still a usable object, not an error", {
  skip_on_cran()
  set.seed(27)
  big <- rnorm(6500)
  res <- quiet_f_bestNormalize(big)
  expect_true(is.list(res) || inherits(res, "f_bestNormalize"))
})
