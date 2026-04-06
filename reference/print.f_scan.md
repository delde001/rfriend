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
  digits = 3
)

# S3 method for class 'f_scan'
summary(object, digits = 3)

# S3 method for class 'f_scan'
plot(x, boxplot = TRUE, histogram = TRUE, qqplot = TRUE, main_plot = TRUE)
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

- digits:

  Integer. Decimal places for printed tables. Default `3`.
