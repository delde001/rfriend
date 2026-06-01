# Print method for f_scan objects

Print method for f_scan objects

Summary method for f_scan objects

Plot method for f_scan objects

## Usage

``` r
# S3 method for class 'f_scan'
print(
  x,
  summary = TRUE,
  outliers = TRUE,
  boxplot = TRUE,
  histogram = TRUE,
  qqplot = TRUE,
  main_plot = TRUE,
  advice = TRUE,
  digits = 3,
  ...
)

# S3 method for class 'f_scan'
summary(object, digits = 3, ...)

# S3 method for class 'f_scan'
plot(x, boxplot = TRUE, histogram = TRUE, qqplot = TRUE, main_plot = TRUE, ...)
```

## Arguments

- x:

  An `f_scan` object.

- summary:

  Logical. Print summary statistics table? Default `TRUE`.

- outliers:

  Logical. Print outlier table? Default `TRUE`.

- boxplot, histogram, qqplot, main_plot:

  Logical. Which plots to render?

- advice:

  Logical. Print statistical test recommendations? Default `TRUE` (shown
  only if `advice=TRUE` was used during `f_scan`).

- digits:

  Integer. Decimal places for printed tables. Default `3`.

- ...:

  Further arguments passed to or from other methods. Currently unused by
  the `f_scan` methods themselves, but accepted so the methods remain
  consistent with the base generics `print`, `summary`, and `plot`.

- object:

  f_scan object to make a summary table from.
