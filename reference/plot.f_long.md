# Plot method for f_long objects

Automatically runs a `f_scan` diagnostic plot on data created by
`f_long`.

## Usage

``` r
# S3 method for class 'f_long'
plot(x, summary = TRUE, ...)
```

## Arguments

- x:

  An object of class `f_long` (output from `f_long`).

- summary:

  Logical. If `TRUE`, generates the summary table within the scan.
  Default is `TRUE`.

- ...:

  Additional arguments passed to `f_scan`.

## Value

Returns the output of `f_scan` (an object of class `f_scan`) invisibly.
