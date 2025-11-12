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
  threshold_large = 9999,
  digits = 3,
  replace_na = TRUE,
  detect_int_col = TRUE
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
  `9999`.

- digits:

  Integer. Number of significant digits to use in formatting. Default is
  `3`.

- replace_na:

  Logical. If TRUE, NA values will be replaced with empty strings ("")
  in the output. Default is TRUE.

- detect_int_col:

  Logical. If `TRUE`, columns in a data.frame containing only integers
  will be displayed without decimal digits. Columns containing a mix of
  integers and decimal values will display all values with the specified
  number of digits. If `FALSE`, each individual cell is evaluated:
  integer values are displayed without digits, and numbers containing
  digits with the specified number of digits. Default is `TRUE`.

## Value

- If input is a vector: A character vector of the same length as the
  input, with values formatted according to the specified rules.

- If input is a data frame: A data frame with the same structure as the
  input, but with character columns formatted according to the specified
  rules.

## Details

The function applies the following formatting rules:

- Values smaller than `threshold_small` or larger than `threshold_large`
  are formatted in scientific notation with `digits` significant digits.

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
#> [1] "1.000e-04" "0.500"     "3.000"     "1.000e+04" ""         
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
#> 1    A  1.00e-04       1       10.00     1e+04
#> 2    B  2.00e-03       2       20.00      5000
#> 3    C      0.50       3       30.10          

# To keep Integers as Integers (no digits)
# in columns with mixed data (Integers and digits)
# set detect_int_col = FALSE
f_conditional_round(df, detect_int_col = FALSE)
#>   name small_val integer integer_mix large_val
#> 1    A 1.000e-04       1          10 1.000e+04
#> 2    B 2.000e-03       2          20      5000
#> 3    C     0.500       3      30.100          
```
