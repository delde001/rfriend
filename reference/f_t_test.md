# Perform multiple t-tests with optional data transformation, inspection and visualization.

Performs One-sample, Two-sample (Independent), or Paired t-tests on a
given dataset with options for (Box-Cox/BestNormalize) transformations,
normality tests, and visualization. Several response parameters can be
analysed in sequence (formula interface). Additionally, a vector
interface similar to
[`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) is supported.

## Usage

``` r
f_t_test(x, ...)

# S3 method for class 'formula'
f_t_test(
  formula,
  data = NULL,
  paired = FALSE,
  var.equal = NULL,
  conf.level = NULL,
  mu = 0,
  alternative = "two.sided",
  norm_plots = TRUE,
  transformation = TRUE,
  force_transformation = NULL,
  alpha = 0.05,
  intro_text = TRUE,
  close_generated_files = FALSE,
  open_generated_files = TRUE,
  output_type = "default",
  save_as = NULL,
  save_in_wdir = FALSE,
  ...
)

# Default S3 method
f_t_test(
  x,
  y = NULL,
  paired = FALSE,
  var.equal = NULL,
  conf.level = NULL,
  mu = 0,
  alternative = "two.sided",
  norm_plots = TRUE,
  transformation = TRUE,
  force_transformation = NULL,
  alpha = 0.05,
  intro_text = TRUE,
  close_generated_files = FALSE,
  open_generated_files = TRUE,
  output_type = "default",
  save_as = NULL,
  save_in_wdir = FALSE,
  ...
)
```

## Arguments

- x:

  Numeric vector of data values (one-sample or first group for
  two-sample), or a formula of the form `response ~ group` or
  `response ~ 1`.

- formula:

  A formula specifying the model (alternative to using x/y).

  - **Two-sample (Independent/Paired):** `response ~ group` (where group
    has exactly 2 levels).

  - **One-sample:** `response ~ 1` or `response ~ NULL`.

  More response variables can be added using `+` (e.g.,
  `y1 + y2 ~ group`).

- data:

  A data frame containing the variables when using the formula
  interface.

- paired:

  Logical. If `TRUE`, performs a paired t-test. **Note:** For the
  formula interface, data must be sorted so that all observations of
  group 1 appear before group 2 (AABB order). For the vector interface,
  `x` and `y` must have the same length.

- var.equal:

  Logical or `NULL`. If `TRUE`, forces Student's t-test (equal
  variances). If `FALSE` or `NULL` (default), Welch's t-test is used.
  Bartlett's and Levene's tests are always reported as diagnostics but
  do **not** affect this choice. See Delacre, Lakens & Leys (2017).

- conf.level:

  Numeric. Confidence level. Default is `1 - alpha`. If `conf.level` is
  specified, `alpha` is set to `1 - conf.level`.

- mu:

  Numeric. The true value to test against: the mean (one-sample), the
  mean of differences (paired), or the difference in means (two-sample).
  Default is 0. For transformed analyses, `mu` is forward-transformed
  for one-sample and paired tests. For two-sample tests with `mu != 0` a
  warning is issued.

- alternative:

  Character string. `"two.sided"` (default), `"greater"`, or `"less"`.

- norm_plots:

  Logical. If `TRUE`, diagnostic plots are included in the output.
  Default is `TRUE`.

- transformation:

  Logical or character string. If `TRUE` or `"boxcox"`, applies
  [`f_boxcox()`](https://delde001.github.io/rfriend/reference/f_boxcox.md)
  when Shapiro-Wilk indicates non-normality. If `"bestnormalize"`,
  applies
  [`f_bestNormalize()`](https://delde001.github.io/rfriend/reference/f_bestNormalize.md).
  If `FALSE` or `"none"`, no transformation is applied. **Note:** For
  paired tests, `bestNormalize` (Yeo-Johnson) is always used on the
  differences, since Box-Cox requires strictly positive values. Default
  is `TRUE`.

- force_transformation:

  Character vector. Names of variables to transform regardless of
  normality results.

- alpha:

  Numeric. Significance level. Default is `0.05`.

- intro_text:

  Logical. If `TRUE`, includes explanation of t-test assumptions.
  Default is `TRUE`.

- close_generated_files:

  Logical. Closes open Excel/Word files before writing. Default `FALSE`.
  **Windows only.**

- open_generated_files:

  Logical. Opens the generated file after creation. Default `TRUE`.

- output_type:

  Character string specifying the output format. Default is `"default"`.

  - `"default"`: Returns the object and lets R decide whether to print;
    auto-prints if unassigned, silent if assigned to a variable. Use
    `print(result)` or `plot(result)` to display the returned object.

  - `"console"`: Forces immediate printing to the console regardless of
    object assignment.

  - `"pdf"`, `"word"`, `"excel"`: Saves results to a file of the
    corresponding format. See `save_as`, `save_in_wdir`, and
    `open_generated_files` for file path and opening behavior.

  - `"rmd"`: Stores the raw markdown string inside the returned object
    for use in R Markdown documents.

- save_as:

  Character. Specific path/filename for output.

- save_in_wdir:

  Logical. Save in working directory. Default `FALSE`.

- y:

  Optional numeric vector (second group) for two-sample tests if using
  the vector interface. Ignored when a formula is supplied.

## Value

An object of class `'f_t_test'`, a named list with one element per
response variable. Each element contains the t-test result, normality
test results, variance diagnostic results, transformation object (if
applied), and back-transformed confidence interval (if applicable).

## References

Delacre, M., Lakens, D., & Leys, C. (2017). Why psychologists should by
default use Welch's t-test instead of Student's t-test. *International
Review of Social Psychology*, 30(1), 92–101.
[doi:10.5334/irsp.82](https://doi.org/10.5334/irsp.82)

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# \donttest{
# 1. Two-sample independent Welch's t-test (default)
f_t_test(mpg ~ am, data = mtcars, output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> Welch Two Sample t-test (two.sided) of: mpg
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean 0: 17.147
#>   Mean 1: 24.392
#>   Difference (0 - 1): -7.245
#> 
#> HYPOTHESES:
#>   H0: True difference in means (0 - 1) is equal to 0
#>   H1: True difference in means (0 - 1) is not equal to 0
#> 
#> TEST RESULTS:
#>   t = -3.767,  df = 18.332,  p-value = 0.0014  *
#> * -> Significant, H0 is rejected  (p ≤ α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI for the difference in population means (μ1 - μ2) : [ -11.28, -3.21 ]
#> 
#> 

# 2. Multiple response variables in one call
f_t_test(mpg + hp ~ am, data = mtcars, output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> Welch Two Sample t-test (two.sided) of: mpg
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean 0: 17.147
#>   Mean 1: 24.392
#>   Difference (0 - 1): -7.245
#> 
#> HYPOTHESES:
#>   H0: True difference in means (0 - 1) is equal to 0
#>   H1: True difference in means (0 - 1) is not equal to 0
#> 
#> TEST RESULTS:
#>   t = -3.767,  df = 18.332,  p-value = 0.0014  *
#> * -> Significant, H0 is rejected  (p ≤ α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI for the difference in population means (μ1 - μ2) : [ -11.28, -3.21 ]
#> 
#> ==========================================================
#> Welch Two Sample t-test (Box-Cox transformed) of: hp
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean 0 (transformed scale): 6.418
#>   Mean 1 (transformed scale): 5.909
#>   Difference (0 - 1): 0.509
#>   --- back-transformed (original scale) ---
#>   Back-transformed mean 0: 151.699
#>   Back-transformed mean 1: 109.917
#>   Sample median 0 (raw data): 175
#>   Sample median 1 (raw data): 109
#> 
#> HYPOTHESES:
#>   H0: True difference in means (0 - 1) is equal to 0
#>   H1: True difference in means (0 - 1) is not equal to 0
#> 
#> TEST RESULTS (transformed scale):
#>   t = 1.831,  df = 19.479,  p-value = 0.0825  
#> -> Not significant, H0 is NOT rejected  (p > α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI (transformed scale): [ -0.072, 1.09 ]
#>   95% CI (back-transformed):  [ 0.93, 2.821 ]  *interpret carefully*
#> Note on transformation:
#>   The t-test was conducted on the Box-Cox-transformed scale.
#>   The back-transformed mean and the sample median of the raw data will
#>   differ when the transformation is not perfectly normalizing.
#>   For non-normal data consider f_wilcox_test() which tests the median
#>   directly without transformation assumptions.
#> 
#> 

# 3. One-sample t-test: test if mean mpg equals 20
f_t_test(mpg ~ 1, data = mtcars, mu = 20,
         output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> One Sample t-test (two.sided) of: mpg
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean mpg: 20.091
#> 
#> HYPOTHESES:
#>   H0: True mean of mpg is equal to 20
#>   H1: True mean of mpg is not equal to 20
#> 
#> TEST RESULTS:
#>   t = 0.085,  df = 31.000,  p-value = 0.9328  
#> -> Not significant, H0 is NOT rejected  (p > α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI for the population mean (μ) : [ 17.918, 22.264 ]
#> 
#> 

# 4. Paired t-test (sleep dataset is already in AABB order)
f_t_test(extra ~ group, data = sleep, paired = TRUE,
         output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> One Sample t-test (arcsinh(x) transformed) of: extra
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean of differences (transformed scale): 0
#>   --- back-transformed (original scale) ---
#>   Back-transformed mean of differences: -1.355
#>   Median of raw differences: -1.3
#> 
#> HYPOTHESES:
#>   H0: True mean difference (1 - 2) is equal to 0
#>   H1: True mean difference (1 - 2) is not equal to 0
#> 
#> TEST RESULTS (transformed scale):
#>   t = -0.000,  df = 9.000,  p-value = 1.0000  
#> -> Not significant, H0 is NOT rejected  (p > α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI (transformed scale): [ -0.715, 0.715 ]
#>   95% CI (back-transformed):  [ -2.188, -0.757 ]
#> Note on transformation:
#>   The t-test was conducted on the arcsinh(x)-transformed scale.
#>   The back-transformed mean and the sample median of the raw data will
#>   differ when the transformation is not perfectly normalizing.
#>   For non-normal data consider f_wilcox_test() which tests the median
#>   directly without transformation assumptions.
#> 
#> 

# 5. Vector interface: two-sample independent
group_auto   <- mtcars$mpg[mtcars$am == 0]
group_manual <- mtcars$mpg[mtcars$am == 1]
f_t_test(group_auto, group_manual, output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> Welch Two Sample t-test (two.sided) of: y
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean group_auto: 17.147
#>   Mean group_manual: 24.392
#>   Difference (group_auto - group_manual): -7.245
#> 
#> HYPOTHESES:
#>   H0: True difference in means (group_auto - group_manual) is equal to 0
#>   H1: True difference in means (group_auto - group_manual) is not equal to 0
#> 
#> TEST RESULTS:
#>   t = -3.767,  df = 18.332,  p-value = 0.0014  *
#> * -> Significant, H0 is rejected  (p ≤ α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI for the difference in population means (μ1 - μ2) : [ -11.28, -3.21 ]
#> 
#> 
#> 
#> ==========================================================
#> Welch Two Sample t-test (two.sided) of: group_auto_vs_group_manual
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean group_auto: 17.147
#>   Mean group_manual: 24.392
#>   Difference (group_auto - group_manual): -7.245
#> 
#> HYPOTHESES:
#>   H0: True difference in means (group_auto - group_manual) is equal to 0
#>   H1: True difference in means (group_auto - group_manual) is not equal to 0
#> 
#> TEST RESULTS:
#>   t = -3.767,  df = 18.332,  p-value = 0.0014  *
#> * -> Significant, H0 is rejected  (p ≤ α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI for the difference in population means (μ1 - μ2) : [ -11.28, -3.21 ]
#> 
#> 

# 6. Vector interface: one-sample
f_t_test(mtcars$mpg, mu = 20, output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> One Sample t-test (two.sided) of: mpg
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean mpg: 20.091
#> 
#> HYPOTHESES:
#>   H0: True mean of mpg is equal to 20
#>   H1: True mean of mpg is not equal to 20
#> 
#> TEST RESULTS:
#>   t = 0.085,  df = 31.000,  p-value = 0.9328  
#> -> Not significant, H0 is NOT rejected  (p > α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI for the population mean (μ) : [ 17.918, 22.264 ]
#> 
#> 
#> 
#> ==========================================================
#> One Sample t-test (two.sided) of: mpg
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean mpg: 20.091
#> 
#> HYPOTHESES:
#>   H0: True mean of mpg is equal to 20
#>   H1: True mean of mpg is not equal to 20
#> 
#> TEST RESULTS:
#>   t = 0.085,  df = 31.000,  p-value = 0.9328  
#> -> Not significant, H0 is NOT rejected  (p > α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI for the population mean (μ) : [ 17.918, 22.264 ]
#> 
#> 

# 7. Force Student's t-test (equal variances assumed)
f_t_test(mpg ~ am, data = mtcars, var.equal = TRUE,
         output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#>  Two Sample t-test (two.sided) of: mpg
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean 0: 17.147
#>   Mean 1: 24.392
#>   Difference (0 - 1): -7.245
#> 
#> HYPOTHESES:
#>   H0: True difference in means (0 - 1) is equal to 0
#>   H1: True difference in means (0 - 1) is not equal to 0
#> 
#> TEST RESULTS:
#>   t = -4.106,  df = 30.000,  p-value = 0.0003  *
#> * -> Significant, H0 is rejected  (p ≤ α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI for the difference in population means (μ1 - μ2) : [ -10.848, -3.642 ]
#> 
#> 

# 8. One-sided test
f_t_test(mpg ~ am, data = mtcars, alternative = "greater",
         output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> Welch Two Sample t-test (greater) of: mpg
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean 0: 17.147
#>   Mean 1: 24.392
#>   Difference (0 - 1): -7.245
#> 
#> HYPOTHESES:
#>   H0: True difference in means (0 - 1) is less than or equal to 0
#>   H1: True difference in means (0 - 1) is greater than 0
#> 
#> TEST RESULTS:
#>   t = -3.767,  df = 18.332,  p-value = 0.9993  
#> -> Not significant, H0 is NOT rejected  (p > α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI for the difference in population means (μ1 - μ2) : [ -10.577, Inf ]
#> 
#> 

# 9. Custom significance level (alpha = 0.01 is equivalent to conf.level = 0.99)
f_t_test(mpg ~ am, data = mtcars, alpha = 0.01,
         output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> Welch Two Sample t-test (two.sided) of: mpg
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean 0: 17.147
#>   Mean 1: 24.392
#>   Difference (0 - 1): -7.245
#> 
#> HYPOTHESES:
#>   H0: True difference in means (0 - 1) is equal to 0
#>   H1: True difference in means (0 - 1) is not equal to 0
#> 
#> TEST RESULTS:
#>   t = -3.767,  df = 18.332,  p-value = 0.0014  *
#> * -> Significant, H0 is rejected  (p ≤ α = 0.01)
#> 
#> ESTIMATE:
#>   99% CI for the difference in population means (μ1 - μ2) : [ -12.769, -1.721 ]
#> 
#> 

# 10. Box-Cox transformation with back-transformed CI
# The back-transformed CI estimates the MEDIAN, not the arithmetic mean.
result <- f_t_test(hp ~ am, data = mtcars, transformation = TRUE,
                   output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> Welch Two Sample t-test (Box-Cox transformed) of: hp
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean 0 (transformed scale): 6.418
#>   Mean 1 (transformed scale): 5.909
#>   Difference (0 - 1): 0.509
#>   --- back-transformed (original scale) ---
#>   Back-transformed mean 0: 151.699
#>   Back-transformed mean 1: 109.917
#>   Sample median 0 (raw data): 175
#>   Sample median 1 (raw data): 109
#> 
#> HYPOTHESES:
#>   H0: True difference in means (0 - 1) is equal to 0
#>   H1: True difference in means (0 - 1) is not equal to 0
#> 
#> TEST RESULTS (transformed scale):
#>   t = 1.831,  df = 19.479,  p-value = 0.0825  
#> -> Not significant, H0 is NOT rejected  (p > α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI (transformed scale): [ -0.072, 1.09 ]
#>   95% CI (back-transformed):  [ 0.93, 2.821 ]  *interpret carefully*
#> Note on transformation:
#>   The t-test was conducted on the Box-Cox-transformed scale.
#>   The back-transformed mean and the sample median of the raw data will
#>   differ when the transformation is not perfectly normalizing.
#>   For non-normal data consider f_wilcox_test() which tests the median
#>   directly without transformation assumptions.
#> 
#> 
result[["hp"]]$ci_backtransformed
#> [1] 0.9303737 2.8214855
#> attr(,"conf.level")
#> [1] 0.95

# 11. One-sample with non-zero mu and back-transformation
f_t_test(hp ~ 1, data = mtcars, mu = 100, transformation = TRUE,
         output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> One Sample t-test (Box-Cox transformed) of: hp
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean hp: 6.211 (transformed scale)
#>   --- back-transformed (original scale) ---
#>   Back-transformed mean: 133.245
#>   Sample median (raw data): 123
#> 
#> HYPOTHESES:
#>   H0: True mean of hp is equal to 100
#>   H1: True mean of hp is not equal to 100
#>   (mu on transformed scale: 5.7628)
#> 
#> TEST RESULTS (transformed scale):
#>   t = 3.377,  df = 31.000,  p-value = 0.0020  *
#> * -> Significant, H0 is rejected  (p ≤ α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI (transformed scale): [ 5.94, 6.482 ]
#>   95% CI (back-transformed):  [ 112.145, 157.88 ]
#> Note on transformation:
#>   The t-test was conducted on the Box-Cox-transformed scale.
#>   The back-transformed mean and the sample median of the raw data will
#>   differ when the transformation is not perfectly normalizing.
#>   For non-normal data consider f_wilcox_test() which tests the median
#>   directly without transformation assumptions.
#> 
#> 

# 12. BestNormalize transformation (set seed for reproducibility)
set.seed(123)
f_t_test(hp ~ am, data = mtcars, transformation = "bestnormalize",
         output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> Welch Two Sample t-test (sqrt(x + a) transformed) of: hp
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean 0 (transformed scale): 0.244
#>   Mean 1 (transformed scale): -0.356
#>   Difference (0 - 1): 0.6
#>   --- back-transformed (original scale) ---
#>   Back-transformed mean 0: 155.644
#>   Back-transformed mean 1: 116.858
#>   Sample median 0 (raw data): 175
#>   Sample median 1 (raw data): 109
#> 
#> HYPOTHESES:
#>   H0: True difference in means (0 - 1) is equal to 0
#>   H1: True difference in means (0 - 1) is not equal to 0
#> 
#> TEST RESULTS (transformed scale):
#>   t = 1.596,  df = 19.316,  p-value = 0.1267  
#> -> Not significant, H0 is NOT rejected  (p > α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI (transformed scale): [ -0.186, 1.386 ]
#>   95% CI (back-transformed):  [ 127.305, 244.812 ]  *interpret carefully*
#> Note on transformation:
#>   The t-test was conducted on the sqrt(x + a)-transformed scale.
#>   The back-transformed mean and the sample median of the raw data will
#>   differ when the transformation is not perfectly normalizing.
#>   For non-normal data consider f_wilcox_test() which tests the median
#>   directly without transformation assumptions.
#> 
#> 

# 13. Force transformation regardless of normality
f_t_test(mpg + hp ~ am, data = mtcars, force_transformation = "mpg",
         output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> Welch Two Sample t-test (Box-Cox transformed) of: mpg
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean 0 (transformed scale): 2.894
#>   Mean 1 (transformed scale): 3.261
#>   Difference (0 - 1): -0.367
#>   --- back-transformed (original scale) ---
#>   Back-transformed mean 0: 16.73
#>   Back-transformed mean 1: 23.664
#>   Sample median 0 (raw data): 17.3
#>   Sample median 1 (raw data): 22.8
#> 
#> HYPOTHESES:
#>   H0: True difference in means (0 - 1) is equal to 0
#>   H1: True difference in means (0 - 1) is not equal to 0
#> 
#> TEST RESULTS (transformed scale):
#>   t = -3.826,  df = 23.833,  p-value = 0.0008  *
#> * -> Significant, H0 is rejected  (p ≤ α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI (transformed scale): [ -0.565, -0.169 ]
#>   95% CI (back-transformed):  [ 0.567, 0.844 ]  *interpret carefully*
#> Note on transformation:
#>   The t-test was conducted on the Box-Cox-transformed scale.
#>   The back-transformed mean and the sample median of the raw data will
#>   differ when the transformation is not perfectly normalizing.
#>   For non-normal data consider f_wilcox_test() which tests the median
#>   directly without transformation assumptions.
#> 
#> ==========================================================
#> Welch Two Sample t-test (Box-Cox transformed) of: hp
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean 0 (transformed scale): 6.418
#>   Mean 1 (transformed scale): 5.909
#>   Difference (0 - 1): 0.509
#>   --- back-transformed (original scale) ---
#>   Back-transformed mean 0: 151.699
#>   Back-transformed mean 1: 109.917
#>   Sample median 0 (raw data): 175
#>   Sample median 1 (raw data): 109
#> 
#> HYPOTHESES:
#>   H0: True difference in means (0 - 1) is equal to 0
#>   H1: True difference in means (0 - 1) is not equal to 0
#> 
#> TEST RESULTS (transformed scale):
#>   t = 1.831,  df = 19.479,  p-value = 0.0825  
#> -> Not significant, H0 is NOT rejected  (p > α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI (transformed scale): [ -0.072, 1.09 ]
#>   95% CI (back-transformed):  [ 0.93, 2.821 ]  *interpret carefully*
#> Note on transformation:
#>   The t-test was conducted on the Box-Cox-transformed scale.
#>   The back-transformed mean and the sample median of the raw data will
#>   differ when the transformation is not perfectly normalizing.
#>   For non-normal data consider f_wilcox_test() which tests the median
#>   directly without transformation assumptions.
#> 
#> 

# 14. Suppress transformation (diagnostic mode)
f_t_test(hp ~ am, data = mtcars, transformation = FALSE,
         output_type = "console", norm_plots = FALSE)
#> 
#> ==========================================================
#> Welch Two Sample t-test (two.sided) of: hp
#> ==========================================================
#> 
#> SAMPLE STATISTICS:
#>   Mean 0: 160.263
#>   Mean 1: 126.846
#>   Difference (0 - 1): 33.417
#> 
#> HYPOTHESES:
#>   H0: True difference in means (0 - 1) is equal to 0
#>   H1: True difference in means (0 - 1) is not equal to 0
#> 
#> TEST RESULTS:
#>   t = 1.266,  df = 18.715,  p-value = 0.2210  
#> -> Not significant, H0 is NOT rejected  (p > α = 0.05)
#> 
#> ESTIMATE:
#>   95% CI for the difference in population means (μ1 - μ2) : [ -21.879, 88.713 ]
#> 
#> 

# 15. Access return object fields directly
result <- f_t_test(mpg + hp ~ am, data = mtcars,
                   output_type = "default", norm_plots = FALSE, intro_text = FALSE)
result[["mpg"]]$t_test          # standard htest object
#> 
#>  Welch Two Sample t-test
#> 
#> data:  group1 and group2
#> t = -3.7671, df = 18.332, p-value = 0.001374
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -11.280194  -3.209684
#> sample estimates:
#> mean of x mean of y 
#>  17.14737  24.39231 
#> 
result[["hp"]]$shapiro_res      # Shapiro-Wilk result
#> 
#>  Shapiro-Wilk normality test
#> 
#> data:  normality_values
#> W = 0.91286, p-value = 0.01338
#> 
result[["hp"]]$homog_p_bartlett # Bartlett p-value (diagnostic only)
#> [1] 0.09304755
result[["hp"]]$homog_p_levene   # Levene p-value (diagnostic only)
#> [1] 0.6708527
result[["hp"]]$ci_backtransformed # back-transformed CI if transformed
#> [1] 0.9303737 2.8214855
#> attr(,"conf.level")
#> [1] 0.95
# }
```
