# Print method for f_summary objects

Prints a formatted summary table to the console.

## Usage

``` r
# S3 method for class 'f_summary'
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

  Object of class f_summary.

- col_width:

  Integer. Max characters in header before line break. Default `6`.

- table_width:

  Integer or `NULL`. Characters after which table splits. Default `90`.

- ...:

  Additional arguments passed to `pander`.

## Value

Invisibly returns `1`.
