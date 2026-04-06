# Perform multiple Wilcoxon rank sum and signed rank tests with inspection and visualization.

Performs One-sample (Wilcoxon signed rank), Two-sample independent
(Wilcoxon rank sum / Mann-Whitney U), or Paired (Wilcoxon signed rank)
tests on a given dataset. Several response parameters can be analysed in
sequence (formula interface). Additionally, a vector interface similar
to stats::wilcox.test() is supported.

## Usage

``` r
f_wilcox_test(x, ...)

# S3 method for class 'formula'
f_wilcox_test(
  formula,
  data = NULL,
  paired = FALSE,
  conf.level = NULL,
  mu = 0,
  alternative = "two.sided",
  norm_plots = TRUE,
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
f_wilcox_test(
  x,
  y = NULL,
  paired = FALSE,
  conf.level = NULL,
  mu = 0,
  alternative = "two.sided",
  norm_plots = TRUE,
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

  Logical. If `TRUE`, performs a paired Wilcoxon test. **Note on row
  order:** For the formula interface, data must be sorted so that the
  observations in the two groups match row-for-row (i.e. row 1 of group
  1 is paired with row 1 of group 2). For the vector interface, `x` and
  `y` must have the same length.

  **Note on factor level order:** The formula interface computes
  differences as `level1 - level2` based on factor level order, which
  defaults to alphabetical. To control the direction, set the reference
  level explicitly:

        # Set levels at creation
        group <- factor(group, levels = c("pre", "post"))

        # Or relevel an existing factor
        group <- relevel(group, ref = "pre")
        

  A reversed level order flips the sign of the estimate and CI but does
  not affect the W statistic or p-value.

- conf.level:

  Numeric. Confidence level of the interval. Default is `1 - alpha`. If
  `conf.level` is specified, then `alpha <- 1 - conf.level`.

- mu:

  Numeric. The hypothesised value of the pseudo-median (one-sample) or
  location shift (paired/two-sample) under H0. Default is 0.

- alternative:

  Character string. `"two.sided"` (default), `"greater"`, or `"less"`.

- norm_plots:

  Logical. If `TRUE`, descriptive diagnostic plots are included in the
  output. Default is `TRUE`.

- alpha:

  Numeric. Significance level. Default is `0.05`.

- intro_text:

  Logical. If `TRUE`, includes explanation about Wilcoxon test
  assumptions.

- close_generated_files:

  Logical. Closes open Excel/Word files before writing. Works on Windows
  (taskkill), macOS (pkill) and Linux (pkill/soffice). Default `FALSE`.
  **WARNING:** Always save your work before using this option!

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

  Logical. Save in working directory.

- y:

  Optional numeric vector (second group) for two-sample tests if using
  the vector interface. Ignored when a formula is supplied.

## Value

An object of class `'f_wilcox_test'`.

## Median vs Pseudo-median

By default this function calls `stats::wilcox.test(conf.int = TRUE)`,
which bases its confidence interval and hypothesis test on the
**Hodges-Lehmann estimator**, not the raw sample median. This is
standard behaviour of the Wilcoxon test, not something specific to this
function. This function explicitly labels the estimator for what it is,
because it is commonly mislabelled as "CI for the median" in textbooks
and software output.

The estimator works differently depending on the test type:

**One-sample:** The *pseudo-median* is the middle value of all possible
pairwise averages of your data points (including each value paired with
itself). For a perfectly symmetric distribution it equals the sample
median; for skewed data the two can differ.

**Paired:** The paired differences (observation 1 minus observation 2
within each pair) are computed first, and the pseudo-median of those
differences is estimated. This is conceptually a one-sample problem
applied to the differences, not a comparison of two independent groups.
The CI is for the pseudo-median of the differences, **not** for the
difference between the two separate sample medians.

**Two-sample independent:** The *location shift* is the median of all
\\n_1 \times n_2\\ pairwise differences (one value from Group 1 minus
one from Group 2). It answers: *by how much does a randomly chosen value
from Group 1 tend to exceed a randomly chosen value from Group 2?* When
both groups have the same distributional shape it equals the raw
difference in sample medians; when shapes differ, the two values can
diverge.

In all three cases the sample median(s) are reported separately for
descriptive purposes only.
