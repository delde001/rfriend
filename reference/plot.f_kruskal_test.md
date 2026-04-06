# Plot method for f_kruskal_test objects

Displays the density plot and/or boxplot stored in an `f_kruskal_test`
object. Plots are only available when the original call used
`plot = TRUE`.

## Usage

``` r
# S3 method for class 'f_kruskal_test'
plot(x, which = c("distributions", "Boxplot"), ...)
```

## Arguments

- x:

  An object of class `f_kruskal_test`.

- which:

  Character vector indicating which plots to show. Options are
  `"distributions"` (density plot), `"Boxplot"`, or both (default).

- ...:

  Additional arguments (currently ignored).

## Value

Returns `x` invisibly.

## Examples

``` r
result <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
                         output_type = "default")
plot(result)                          # both plots


plot(result, which = "Boxplot")       # boxplot only

```
