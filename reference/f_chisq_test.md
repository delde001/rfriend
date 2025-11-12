# Chi-squared Test with Post-hoc Analysis

Performs a chi-squared test
[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html), then
automatically conducts post-hoc analysis if the test is significant. The
function provides adjusted p-values for each cell in the contingency
table using a specified correction method.

## Usage

``` r
f_chisq_test(
  x,
  y,
  p = NULL,
  method = "bonferroni",
  digits = 3,
  alpha = 0.05,
  force_posthoc = FALSE,
  ...
)
```

## Arguments

- x:

  A numeric vector (or factor), or a contingency table in matrix or
  table form. If a data frame is entered the function will try to
  convert it to a table.

- y:

  A numeric vector; ignored if x is a matrix, table or data.frame. If x
  is a factor, y should be a factor of the same length.

- p:

  A vector of probabilities of the same length as x. Default is `NULL`.
  An error is given if any entry of p is negative.

- method:

  Character string specifying the adjustment method for p-values.
  Default is `"bonferroni"`. Other options include
  `"holm", "hochberg", "hommel", "BH", "BY", "fdr", and "none"`.

- digits:

  Integer specifying the number of decimal places for rounding. Default
  is `3`.

- alpha:

  Numeric threshold for significance. Default is `0.05`.

- force_posthoc:

  Logical indicating whether to perform post-hoc tests even if the
  chi-squared test is not significant. Default is `FALSE`.

- ...:

  Additional arguments passed to
  [`chisq.test`](https://rdrr.io/r/stats/chisq.test.html).

## Value

An object of class f_chisq_test containing:

- `chisq_test_output`: The output from
  [`chisq.test`](https://rdrr.io/r/stats/chisq.test.html).

- `adjusted_p_values`: Matrix of adjusted p-values (for table/matrix
  input).

- `observed_vs_adj_p_value`: Interleaved table of observed values and
  adjusted p-values.

- `stdres_vs_adj_p_value`: Interleaved table of standardized residuals
  and adjusted p-values.

- `adj_p_values`: Vector of adjusted p-values (for vector input).

- `posthoc_output_table`: Data frame with observed values, expected
  values, standardized residuals, and adjusted p-values (for vector
  input).

## Details

The function first performs a chi-squared test using
[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html). If the test is
significant (p \< alpha) or if `force_posthoc = TRUE`, it conducts
post-hoc analysis by examining the standardized residuals. The p-values
for these residuals are adjusted using the specified method to control
for multiple comparisons.

If the input is a data frame, the function attempts to convert it to a
table and displays the resulting table for verification.

## References

This function implements a post-hoc analysis for chi-squared tests
inspired by the methodology in:

Beasley, T. M., & Schumacker, R. E. (1995). Multiple Regression Approach
to Analyzing Contingency Tables: Post Hoc and Planned Comparison
Procedures. The Journal of Experimental Education, 64(1), 79-93.

The implementation draws inspiration from the
['chisq.posthoc.test'](https://CRAN.R-project.org/package=chisq.posthoc.test)
package by Daniel Ebbert.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Chi.square on independence: Association between two variables.
# Create a contingency table.
my_table <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
dimnames(my_table) <- list(Gender = c("Male", "Female"),
                           Response = c("Agree", "Neutral", "Disagree"))

# Perform chi-squared test with post-hoc analysis.
f_chisq_test(my_table)
#> Error in eval(bquote(stats::chisq.test(.(as.name(x_name)), ...)), envir = env): object 'my_table' not found

# Use a different adjustment method.
f_chisq_test(my_table, method = "holm")
#> Error in eval(bquote(stats::chisq.test(.(as.name(x_name)), ...)), envir = env): object 'my_table' not found

# Other forms still work like Goodness-of-Fit: Match to theoretical distribution.
# Observed frequencies of rolling with a die 1 - 6.
observed <- c(2, 2, 10, 20, 15, 11)

# Expected probabilities under a fair die.
expected_probs <- rep(1/6, 6)

# Chi-Square Goodness-of-Fit Test.
f_chisq_test(x = observed, p = expected_probs)
#> Error in eval(bquote(stats::chisq.test(.(as.name(x_name)), ...)), envir = env): object 'observed' not found
```
