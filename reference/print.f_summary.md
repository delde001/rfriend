# Print method for f_summary objects

This function prints `f_summary` objects.

## Usage

``` r
# S3 method for class 'f_summary'
print(x, col_width = 6, table_width = 90, ...)
```

## Arguments

- x:

  Object of class f_summary

- col_width:

  Integer. Specifies the maximum number of characters allowed in table
  header columns before a line break is inserted. Defaults to `6`.

- table_width:

  Integer or `NULL`. Defines the number of characters after which the
  table is split into separate sections. Defaults to `NULL`, meaning no
  break is applied.

- ...:

  Additional arguments passed to the `pander` function.

## Value

This function is called for its side effect of printing a formatted
output to the console and does not return a useful value. It invisibly
returns `1`.
