# Convert multiple columns to Factors in a data frame

Converts multiple specified columns of a data frame into factors. If no
columns are specified, it automatically detects and converts columns
that are suitable to be factors. The function returns the entire data
frame including non factor columns and reports the properties of this
new data frame in the console.

## Usage

``` r
f_factors(
  data,
  select = NULL,
  exclude = NULL,
  properties = FALSE,
  force_factors = FALSE,
  unique_num_treshold = 8,
  repeats_threshold = 2,
  ...
)
```

## Arguments

- data:

  A data frame containing the columns to be converted.

- select:

  A character vector specifying the names of the columns to convert into
  factors. If `NULL`, the function automatically detects columns that
  should be factors based on their data type and unique value count.
  Default is `NULL`.

- exclude:

  A character vector specifying the names of the columns NOT to convert
  into factors. If `NULL`, no columns are excluded. Default is `NULL`.

- properties:

  Logical. If `TRUE`, prints a detailed table about the properties of
  the new data frame to the console. If `FALSE` no property table will
  be printed to the console. Default is `FALSE`.

- force_factors:

  Logical. If `TRUE` all columns in the data.frame will be converted to
  factors except for the excluded columns using `exclude`.

- unique_num_treshold:

  Numeric. A threshold of the amount of unique numbers a numeric column
  should have to keep it numeric, i.e. omit factor conversion. Default
  `8`.

- repeats_threshold:

  Numeric. A threshold of the minimal number of repeats a numeric column
  should have to convert it to a factor. Default `2`.

- ...:

  Additional arguments passed to the
  [`factor()`](https://rdrr.io/r/base/factor.html) function of baseR.

## Value

Returns the modified data frame with the specified (or all suitable)
columns converted to factors. Can also force a print of a summary of the
data frame's structure to the console (properties = TRUE).

## Details

- If `select` is `NULL`, the function identifies columns with character
  data or numeric data with fewer than 8 unique values as candidates for
  conversion to factors.

- The function checks if all specified columns exist in the data frame
  and stops execution if any are missing.

- Converts specified columns into factors, applying any additional
  arguments provided.

- Outputs a summary data frame with details about each column, including
  its type, class, number of observations, missing values, factor
  levels, and labels.

## See also

[`factor`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/factor.html)

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Make a data.frame:
df <- data.frame(a = c("yes", "no", "yes", "yes", "no",
                       "yes", "yes", "no", "yes"),
                 b = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
                 c = c("apple", "kiwi", "banana", "apple", "kiwi",
                        "banana", "apple", "kiwi", "banana"),
                 d = c(1.1, 1.1, 3.4, 4.5, 5.4, 6.7, 7.8, 8.1, 9.8)
                 )
str(df)
#> 'data.frame':    9 obs. of  4 variables:
#>  $ a: chr  "yes" "no" "yes" "yes" ...
#>  $ b: num  1 2 3 1 2 3 1 2 3
#>  $ c: chr  "apple" "kiwi" "banana" "apple" ...
#>  $ d: num  1.1 1.1 3.4 4.5 5.4 6.7 7.8 8.1 9.8

# Convert specified columns to factors:
df1 <- f_factors(df, select = c("a", "c"))
str(df1)
#> 'data.frame':    9 obs. of  4 variables:
#>  $ a: Factor w/ 2 levels "no","yes": 2 1 2 2 1 2 2 1 2
#>  $ b: num  1 2 3 1 2 3 1 2 3
#>  $ c: Factor w/ 3 levels "apple","banana",..: 1 3 2 1 3 2 1 3 2
#>  $ d: num  1.1 1.1 3.4 4.5 5.4 6.7 7.8 8.1 9.8


# Convert all potential factor columns to factor but exclude column "b":
df2 <- f_factors(df, exclude = c("b"))
str(df2)
#> 'data.frame':    9 obs. of  4 variables:
#>  $ a: Factor w/ 2 levels "no","yes": 2 1 2 2 1 2 2 1 2
#>  $ b: num  1 2 3 1 2 3 1 2 3
#>  $ c: Factor w/ 3 levels "apple","banana",..: 1 3 2 1 3 2 1 3 2
#>  $ d: num  1.1 1.1 3.4 4.5 5.4 6.7 7.8 8.1 9.8

# Convert all columns to factor but exclude column "b":
df3 <- f_factors(df, exclude = c("b"), force_factors = TRUE)
str(df3)
#> 'data.frame':    9 obs. of  4 variables:
#>  $ a: Factor w/ 2 levels "no","yes": 2 1 2 2 1 2 2 1 2
#>  $ b: num  1 2 3 1 2 3 1 2 3
#>  $ c: Factor w/ 3 levels "apple","banana",..: 1 3 2 1 3 2 1 3 2
#>  $ d: Factor w/ 8 levels "1.1","3.4","4.5",..: 1 1 2 3 4 5 6 7 8

# Or automatically detect and convert suitable columns to factors.
# Thus obtaining the same results as above automatically:
df4 <- f_factors(df)
str(df4)
#> 'data.frame':    9 obs. of  4 variables:
#>  $ a: Factor w/ 2 levels "no","yes": 2 1 2 2 1 2 2 1 2
#>  $ b: Factor w/ 3 levels "1","2","3": 1 2 3 1 2 3 1 2 3
#>  $ c: Factor w/ 3 levels "apple","banana",..: 1 3 2 1 3 2 1 3 2
#>  $ d: num  1.1 1.1 3.4 4.5 5.4 6.7 7.8 8.1 9.8

# In example above col b was converted to a factor as the number of repeats = 2
# and the amount of unique numbers < 8. In order to keep b numeric we can also
# adjust the unique_num_treshold and/or repeats_threshold:
df5 <- f_factors(df, unique_num_treshold = 2)
str(df5)
#> 'data.frame':    9 obs. of  4 variables:
#>  $ a: Factor w/ 2 levels "no","yes": 2 1 2 2 1 2 2 1 2
#>  $ b: num  1 2 3 1 2 3 1 2 3
#>  $ c: Factor w/ 3 levels "apple","banana",..: 1 3 2 1 3 2 1 3 2
#>  $ d: num  1.1 1.1 3.4 4.5 5.4 6.7 7.8 8.1 9.8

# Use `properties = TRUE` to view the data frame's structure.
# This forces a printed output which is more insight than standard str() output.
df6 <- f_factors(df, properties = TRUE)
#>   Column    Type   Class Obs Missing Factor_Levels       Factor_Labels
#> a      a integer  factor   9       0             2             no, yes
#> b      b integer  factor   9       0             3             1, 2, 3
#> c      c integer  factor   9       0             3 apple, banana, kiwi
#> d      d  double numeric   9       0             0                    
#>    
```
