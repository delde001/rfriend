# Print method for f_outliers objects

Prints a formatted summary table to the console.

## Usage

``` r
# S3 method for class 'f_outliers'
print(
  x,
  col_width = 6,
  table_width = 90,
  digits = 2,
  allow_integer_decimal_mix = FALSE,
  ...
)
```

## Arguments

- x:

  Object of class f_outliers.

- col_width:

  Integer. Max characters in header before line break. Default `6`.

- table_width:

  Integer or `NULL`. Characters after which table splits. Default `90`.

- digits:

  Integer. Number of decimal digits to use in formatting. Default is
  `3`.

- allow_integer_decimal_mix:

  Logical. If `TRUE`, each individual cell is evaluated: integer values
  are displayed without decimal places, and non-integer values are
  displayed with the specified number of decimal places, i.e. `digits`.
  Default is `FALSE`, when a column contains a mix of integers and
  decimal values, all values are displayed with the specified number of
  decimal places. Note: columns containing only integers are
  \*\*always\*\* displayed without decimal places, regardless of
  `allow_integer_decimal_mix`.

- ...:

  Additional arguments passed to `pander`.

## Value

Invisibly returns `1`.
