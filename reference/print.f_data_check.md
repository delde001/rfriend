# Print the data check of f_scan()

Prints the data check that
[`f_scan`](https://delde001.github.io/rfriend/reference/f_scan.md)
stores as `attr(result, "data_check")`: first a list of possible
problems, then a table of the text columns, a table of the numeric
columns with the lowest and highest values, and the rows per ID.

## Usage

``` r
# S3 method for class 'f_data_check'
print(x, max_values = 12L, ...)
```

## Arguments

- x:

  An `f_data_check` object.

- max_values:

  Integer. Maximum number of distinct values listed per text column; a
  column with more values (such as an ID column) is summarised in one
  line. Default `12`.

- ...:

  Not used.

## Value

`x`, invisibly.
