# Convert multiple columns to Factors in a data frame

Converts multiple specified columns of a data frame into factors. If no
columns are specified, it automatically detects and converts columns
that are suitable to be factors. The function returns the entire data
frame including non factor columns and can report the properties of this
new data frame in the console (`properties = TRUE`).

## Usage

``` r
f_factors(
  data,
  select = NULL,
  exclude = NULL,
  properties = FALSE,
  force_factors = FALSE,
  ref = NULL,
  unique_num_threshold = 8,
  repeats_threshold = 2,
  unique_num_treshold = NULL,
  ordered = NULL,
  levels = NULL,
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

- ref:

  Optional reference level(s) to set after conversion, controlling which
  level a factor is contrasted against in models (e.g. `f_glm`,
  `f_lmer`). Under R's default treatment contrasts the reference is the
  first level, which is alphabetical unless set deliberately. Supply
  either a single string (applied to every converted factor that
  contains it) or a named character vector mapping column names to
  reference levels, e.g. `ref = c(treatment = "control", dose = "low")`.
  A level that is not present in a given factor is skipped with a
  warning. Default `NULL` (leave the reference as the first level).

- unique_num_threshold:

  Numeric. A threshold of the amount of unique numbers a numeric column
  should have to keep it numeric, i.e. omit factor conversion. A numeric
  column is a factor candidate only when it has fewer than
  `unique_num_threshold` unique values. Default `8`.

- repeats_threshold:

  Numeric. A numeric column is converted to a factor only when at least
  one of its values occurs more than `repeats_threshold` times. Default
  `2`, so a value has to occur at least three times.

- unique_num_treshold:

  Alternative spelling of `unique_num_threshold`, accepted so that
  existing scripts keep working. Default `NULL` (not used). Supplying
  both spellings with different values is an error.

- ordered:

  Which converted columns become ordered factors (for ordinal data such
  as a damage score). Supply the column names, e.g. `ordered = "damage"`
  or `ordered = c("damage", "stage")`; `TRUE` orders all converted
  columns, `FALSE` makes all of them unordered. Default `NULL`:
  [`factor()`](https://rdrr.io/r/base/factor.html)'s own default (a
  column that is already ordered stays ordered). A named column must
  exist and be among the converted columns, otherwise the function stops
  with an error.

- levels:

  Optional named list with the level order per column, e.g.
  `levels = list(damage = c("low", "medium", "high"))`. Without it,
  [`factor()`](https://rdrr.io/r/base/factor.html) sorts the levels
  (text alphabetically), which is rarely the right order for an ordered
  factor. Every value in the column must be one of the given levels
  (missing values excepted), otherwise the function stops: a value
  outside the levels would silently become `NA`. Levels without
  observations are allowed. A named column must exist and be among the
  converted columns. Default `NULL`.

- ...:

  Additional arguments passed to the
  [`factor()`](https://rdrr.io/r/base/factor.html) function of base R,
  such as `labels`. `ordered` and `levels` are arguments of
  `f_factors()` itself (see above).

## Value

Returns the modified data frame with the specified (or all suitable)
columns converted to factors. Can also force a print of a summary of the
data frame's structure to the console (properties = TRUE).

## Details

- If `select` is `NULL`, the function identifies columns with character
  data, and numeric columns with fewer than `unique_num_threshold`
  (default 8) unique values of which at least one value occurs more than
  `repeats_threshold` (default 2) times, as candidates for conversion to
  factors.

- The function checks if all specified columns exist in the data frame
  and stops execution if any are missing.

- Converts specified columns into factors, applying any additional
  arguments provided.

- `ordered` and `levels` make ordinal columns: name the columns and give
  the level order. `ref` does not apply to an ordered factor (it uses
  its level order, not a reference level) and stops with an error.

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

# Set the reference level of a factor (the level models contrast against).
# Useful before f_glm() / f_lmer(): by default the reference is the first
# (alphabetical) level. Use a named vector to target specific columns:
df1c <- f_factors(df, select = c("a", "c"))
levels(df1c$c)[1]   # default reference is "apple" (alphabetical)
#> [1] "apple"
df_ref <- f_factors(df, select = c("a", "c"),
                    ref = c(a = "yes", c = "kiwi"))
levels(df_ref$c)[1] # reference is now "kiwi"
#> [1] "kiwi"
levels(df_ref$a)[1] # reference is now "yes" (was "no")
#> [1] "yes"

# In example above col b was converted to a factor as each value occurs 3 times
# (more than repeats_threshold = 2) and there are fewer than 8 unique numbers.
# In order to keep b numeric we can also adjust the unique_num_threshold
# and/or repeats_threshold:
df5 <- f_factors(df, unique_num_threshold = 2)
str(df5)
#> 'data.frame':    9 obs. of  4 variables:
#>  $ a: Factor w/ 2 levels "no","yes": 2 1 2 2 1 2 2 1 2
#>  $ b: num  1 2 3 1 2 3 1 2 3
#>  $ c: Factor w/ 3 levels "apple","banana",..: 1 3 2 1 3 2 1 3 2
#>  $ d: num  1.1 1.1 3.4 4.5 5.4 6.7 7.8 8.1 9.8

# An ordinal column: name it in 'ordered' and give the level order in 'levels'
# (without 'levels' the order would be alphabetical: high, low, medium).
scores <- data.frame(damage = c("low", "high", "medium", "low", "high", "medium"),
                     trt = c("a", "b", "a", "b", "a", "b"))
scores_f <- f_factors(scores, select = c("damage", "trt"), ordered = "damage",
                      levels = list(damage = c("low", "medium", "high")))
str(scores_f)
#> 'data.frame':    6 obs. of  2 variables:
#>  $ damage: Ord.factor w/ 3 levels "low"<"medium"<..: 1 3 2 1 3 2
#>  $ trt   : Factor w/ 2 levels "a","b": 1 2 1 2 1 2

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
