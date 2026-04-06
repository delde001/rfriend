# Conditional Rounding for Numeric Values

Conditionally formats numeric values based on their magnitude. Values
that are very small or very large are formatted using scientific
notation, while other values are rounded to a specified number of
decimal places. Integers are preserved without decimal places. When
applied to a data frame, only numeric columns are processed. All output
is character string.

## Usage

``` r
f_conditional_round(
  x,
  threshold_small = 0.01,
  threshold_large = 10000,
  digits = 3,
  replace_na = TRUE,
  na_string = "-",
  allow_integer_decimal_mix = FALSE
)
```

## Arguments

- x:

  A numeric vector or data frame containing numeric columns to be
  formatted.

- threshold_small:

  Numeric value. Values with absolute magnitude smaller than this
  threshold will be formatted using scientific notation. Default is
  `0.01`.

- threshold_large:

  Numeric value. Values with absolute magnitude larger than or equal to
  this threshold will be formatted using scientific notation. Default is
  `10000`.

- digits:

  Integer. Number of decimal digits to use in formatting. Default is
  `3`.

- replace_na:

  Logical. If `TRUE`, `NA` values in the output are replaced with the
  string specified by `na_string`. If `FALSE`, `NA` values are left as
  `NA`. Default is `TRUE`.

- na_string:

  Character string used to replace `NA` values in the output when
  `replace_na = TRUE`. Use `"-"` (default) for a standard "not
  available" dash, `""` for empty cells, or any other string as needed.
  Note that special characters such as the em dash (`"\u2014"`) may
  require additional LaTeX declarations when rendering to PDF via
  `pdflatex`. Default is `"-"`.

- allow_integer_decimal_mix:

  Logical. If `TRUE`, each individual cell is evaluated: integer values
  are displayed without decimal places, and non-integer values are
  displayed with the specified number of decimal places, i.e. `digits`.
  Default is `FALSE`, when a column contains a mix of integers and
  decimal values, all values are displayed with the specified number of
  decimal places. Note: columns containing only integers are
  \*\*always\*\* displayed without decimal places, regardless of
  `allow_integer_decimal_mix`.

## Value

- If input is a vector: A character vector of the same length as the
  input, with values formatted according to the specified rules.

- If input is a data frame: A data frame with the same structure as the
  input, but with character columns formatted according to the specified
  rules.

## Details

The function applies the following formatting rules:

- Values smaller than `threshold_small` or larger than `threshold_large`
  are formatted in scientific notation with decimal `digits`.

- Integer values are formatted without decimal places.

- Non-integer values that don't require scientific notation are rounded
  to `digits` decimal places.

- NA values are replaced with empty strings if `replace_na = TRUE`.

- Empty strings in the input are preserved.

- For data frames, only numeric columns are processed; other columns
  remain unchanged.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Vector examples.
f_conditional_round(c(0.0001, 0.5, 3, 10000))
#> [1] "1.000e-04" "0.500"     "3.000"     "1.000e+04"
# Returns: "1.000e-04" "0.500" "3" "1.000e+04".

f_conditional_round(c(0.0001, 0.5, 3, 10000, NA), replace_na = TRUE)
#> [1] "1.000e-04" "0.500"     "3.000"     "1.000e+04" "-"        
# Returns: "1.000e-04" "0.500" "3" "1.000e+04" ""

# Data frame example.
df <- data.frame(
  name = c("A", "B", "C"),
  small_val = c(0.0001, 0.002, 0.5),
  integer = c(1, 2, 3),
  integer_mix = c(10, 20, 30.1),
  large_val = c(10000, 5000, NA)
)

# Show only two digits.
f_conditional_round(df, digits = 2)
#>   name small_val integer integer_mix large_val
#> 1    A  1.00e-04       1       10.00  1.00e+04
#> 2    B  2.00e-03       2       20.00      5000
#> 3    C      0.50       3       30.10         -

# To keep Integers as Integers (no digits)
# in columns with mixed data (Integers and digits)
# set allow_integer_decimal_mix = TRUE
f_conditional_round(df, allow_integer_decimal_mix = TRUE)
#>   name small_val integer integer_mix large_val
#> 1    A 1.000e-04       1          10 1.000e+04
#> 2    B 2.000e-03       2          20      5000
#> 3    C     0.500       3      30.100         -

# Custom NA replacement string.
f_conditional_round(c(0.5, NA, 3), replace_na = TRUE, na_string = "-")
#> [1] "0.500" "-"     "3.000"
# Returns: "0.500" "-" "3"

f_conditional_round(c(0.5, NA, 3), replace_na = TRUE, na_string = "")
#> [1] "0.500" ""      "3.000"
# Returns: "0.500" "" "3"
```
