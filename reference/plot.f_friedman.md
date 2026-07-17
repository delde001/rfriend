# Plot method for f_friedman objects

Displays the within-block trace plot and/or the boxplot stored in an
`f_friedman` object. Plots are only available when the original call
used `plot = TRUE`.

## Usage

``` r
# S3 method for class 'f_friedman'
plot(x, which = c("distributions", "Boxplot"), ...)
```

## Arguments

- x:

  An object of class `f_friedman`.

- which:

  Character vector indicating which plots to show. Options are
  `"distributions"` (within-block trace plot with median trend line),
  `"Boxplot"`, or both (default).

- ...:

  Additional arguments (currently ignored).

## Value

Returns `x` invisibly.

## Examples

``` r
set.seed(1)
df_rm <- data.frame(
  subject   = factor(rep(1:10, each = 3)),
  condition = factor(rep(c("A", "B", "C"), times = 10)),
  score     = c(rbind(rnorm(10, 4), rnorm(10, 6), rnorm(10, 8)))
)
result <- f_friedman(score ~ condition | subject, data = df_rm,
                     output_type = "default")
plot(result)                          # both plots


plot(result, which = "Boxplot")       # boxplot only

```
