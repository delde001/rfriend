# Summarize a Data Frame with Grouping Variables

Computes summary statistics (n, mean, sd, etc.) for a specified
numerical columns in a data frame. The dataset can be analyzed as a
whole or split by one or more grouping variables.

The function returns a formatted data frame and includes options to
export the results directly to an 'Excel' file.

## Usage

``` r
f_summary(x, ...)

# S3 method for class 'formula'
f_summary(x, data, ...)

# S3 method for class 'data.frame'
f_summary(
  x,
  columns = NULL,
  group_vars = NULL,
  show_name = TRUE,
  show_n = TRUE,
  show_mean = TRUE,
  show_sd = TRUE,
  show_se = TRUE,
  show_ci = FALSE,
  conf_level = 0.95,
  show_min = TRUE,
  show_max = TRUE,
  show_median = TRUE,
  show_Q1 = TRUE,
  show_Q3 = TRUE,
  show_skew = FALSE,
  show_kurtosis = FALSE,
  digits = NULL,
  export_to_excel = FALSE,
  close_generated_files = FALSE,
  open_generated_files = interactive(),
  save_as = NULL,
  save_in_wdir = FALSE,
  check_input = TRUE,
  digits_excel = NULL,
  allow_integer_decimal_mix = FALSE,
  ...
)
```

## Arguments

- x:

  A data.frame or formula (dispatches to the right method).

- ...:

  Further arguments forwarded to `f_summary.data.frame`.

- data:

  A 'data.frame', 'data.table', or 'tibble'.

- columns:

  The numerical column(s) to summarize if no formula is used. Can be
  entered as a single character string (e.g., `"weight"`) or as a
  character vector `c("weight", "length"`). When omitted, defaults to
  all numeric columns in `data` (excluding any columns named in
  `group_vars`).

- group_vars:

  A character vector specifying the grouping variables in `data` (e.g.,
  `c("species", "fertilizer")`) if no formula is used. If `NULL`, the
  entire dataset is summarized.

- show_name:

  Logical. Include variable name. Default `TRUE`.

- show_n:

  Logical. Include count (`n`). Default `TRUE`.

- show_mean:

  Logical. Include mean. Default `TRUE`.

- show_sd:

  Logical. Include standard deviation. Default `TRUE`.

- show_se:

  Logical. Include standard error. Default `TRUE`.

- show_ci:

  Logical. Include the lower and upper bounds of a confidence interval
  for the mean (columns `CI_lower` and `CI_upper`). Default `FALSE`.
  This interval is most meaningful when the data are approximately
  normal or `n` is large; see Details.

- conf_level:

  Numeric. Confidence level for the interval requested by `show_ci`,
  given as a proportion between 0 and 1. Default `0.95` (a 95%
  confidence interval).

- show_min:

  Logical. Include minimum value. Default `TRUE`.

- show_max:

  Logical. Include maximum value. Default `TRUE`.

- show_median:

  Logical. Include median. Default `TRUE`.

- show_Q1:

  Logical. Include first quartile (25th percentile). Default `TRUE`.

- show_Q3:

  Logical. Include third quartile (75th percentile). Default `TRUE`.

- show_skew:

  Logical. Include Skewness (measure of asymmetry). Default `FALSE`.

- show_kurtosis:

  Logical. Include Excess Kurtosis (measure of "tailedness"). Default
  `FALSE`.

- digits:

  Integer. Number of decimal places for the R console output. Default is
  `2`. If `NULL`, no rounding is applied. (Note: This does not affect
  the raw numbers exported to Excel).

- export_to_excel:

  Logical. If `TRUE`, exports results to an 'Excel' file. Default
  `FALSE`.

- close_generated_files:

  Logical. If `TRUE`, forces Excel to close before saving (Windows
  only). Default `FALSE`.

- open_generated_files:

  Logical. Whether to open the generated output files after creation.
  Defaults to `TRUE` in an interactive R session and `FALSE` otherwise
  (e.g. in scripts or automated pipelines). Set to `TRUE` or `FALSE` to
  override this behaviour explicitly.

- save_as:

  Character string. Custom path or filename for the Excel export.

  - If full path: Saves to that location.

  - If filename only: Saves to
    [`tempdir()`](https://rdrr.io/r/base/tempfile.html) (unless
    `save_in_wdir = TRUE`).

  - If directory: Saves as "dataname_summary.xlsx" in that directory.

- save_in_wdir:

  Logical. If `TRUE`, saves to the current working directory. Default
  `FALSE`.

- check_input:

  Logical. If `TRUE`, performs validation checks on inputs. Default
  `TRUE`.

- digits_excel:

  Integer. Number of decimal places for the Excel file cells. Default
  `NULL` (no rounding). Defining `digits_excel`, sets
  `export_to_excel = TRUE` when excel output is not intended use:
  `digits` instead.

- allow_integer_decimal_mix:

  Logical. If `TRUE`, intergers in columns with a mix of integers and
  non-integers are displayed without decimals. Default `FALSE`, meaning
  if there are one or more numbers with decimals the whole column
  contains the number of decimals set by `digits`.

- formula:

  A formula specifying the columns (right hand side) to be summarized by
  groups (left hand side). More columns or groups can be added using `-`
  or `+` (e.g., `col1 + col2 ~ group1 + group2`) to do a sequential
  summary for each column parameter.

## Value

A list of class `f_summary` containing the results data frame.

## Details

The function computes the following statistics:

- `n`: number of observations

- `mean`: arithmetic mean

- `sd`: standard deviation

- `se`: standard error (\\sd / \sqrt{n}\\)

- `CI_lower`, `CI_upper`: lower and upper bounds of the confidence
  interval for the mean (if requested)

- `min`: minimum value

- `max`: maximum value

- `median`: median value

- `Q1`: 25th percentile

- `Q3`: 75th percentile

- `skew`: Sample skewness (if requested).

- `kurt`: Sample excess kurtosis (if requested).

`skew` stands for Skewness which is a measure of asymmetry of a
distribution around its mean. Where `skew` values near **0** indicate
approximate symmetry, while large positive or negative values indicate
noticeable asymmetry.

- `> 0`: Right-skewed (long or heavier tail to the right).

- `< 0`: Left-skewed (long or heavier tail to the left).

`kurt` stands for Excess Kurtosis: Tells you about the "tails" and the
peak.

- `0`: Same tail heaviness as the normal distribution (mesokurtic).

- `> 0`: Heavier tails than normal (Leptokurtic) – indicates frequent
  outliers.

- `< 0`: Lighter tails than normal (Platykurtic) – indicates fewer (or
  less extreme) outliers than a normal distribution.

The confidence interval reported when `show_ci = TRUE` is a parametric
interval for the mean based on the t-distribution, computed as \\mean
\pm t\_{(1 - (1 - conf\\level)/2,\\ n - 1)} \times se\\, where `n` is
the number of non-missing observations. This matches the interval
reported by [`t.test`](https://rdrr.io/r/stats/t.test.html). It assumes
the data are approximately normally distributed (or that `n` is large
enough for the central limit theorem to apply); for strongly skewed
data, indicated for example by a large `skew` or `kurt`, the interval
may be unreliable. Groups with fewer than two non-missing observations
yield `NA` bounds.

If `group_vars` are provided, the statistics are calculated for each
group combination. When `export_to_excel = TRUE`, the file is
automatically generated.

## Author

Sander H. van Delden <plantmind@proton.me>

## Examples

``` r

# --- Example 1: Basic Usage (data.frame notation) ---
# Summarize "hp" grouped by "cyl"; columns and group_vars can be positional
summary_mtcars <- f_summary(mtcars, columns = "hp", group_vars = "cyl")
summary_mtcars <- f_summary(mtcars, "hp", "cyl")  # shorthand equivalent
print(summary_mtcars)
#> 
#> ------------------------------------------------------------------------------
#> cyl   hp     hp       hp      hp      hp     hp       hp       hp       hp    
#>       n      mean     sd      se      min    Q1       median   Q3       max   
#> ----- ------ -------- ------- ------- ------ -------- -------- -------- ------
#> 4     11     82.64    20.93   6.31    52     65.50    91.00    96.00    113   
#> 
#> 6     7      122.29   24.26   9.17    105    110.00   110.00   123.00   175   
#> 
#> 8     14     209.21   50.98   13.62   150    176.25   192.50   241.25   335   
#> ------------------------------------------------------------------------------
#> 

# --- Example 2: Multiple Columns & Groups with Custom Toggles ---
# Summarize "hp" and "disp", grouped by "cyl" and "gear", hide Q1/Q3
summary_custom <- f_summary(mtcars,
                            columns    = c("hp", "disp"),
                            group_vars = c("cyl", "gear"),
                            show_Q1    = FALSE,
                            show_Q3    = FALSE)
print(summary_custom)
#> 
#>  Variable: hp
#> -------------------------------------------------------------------
#> cyl   gear   hp     hp       hp      hp      hp     hp       hp    
#>              n      mean     sd      se      min    median   max   
#> ----- ------ ------ -------- ------- ------- ------ -------- ------
#> 4     3      1      97.00    -       -       97     97.00    97    
#> 
#> 6     3      2      107.50   3.54    2.50    105    107.50   110   
#> 
#> 8     3      12     194.17   33.36   9.63    150    180.00   245   
#> 
#> 4     4      8      76.00    20.11   7.11    52     66.00    109   
#> 
#> 6     4      4      116.50   7.51    3.75    110    116.50   123   
#> 
#> 4     5      2      102.00   15.56   11.00   91     102.00   113   
#> 
#> 6     5      1      175.00   -       -       175    175.00   175   
#> 
#> 8     5      2      299.50   50.20   35.50   264    299.50   335   
#> -------------------------------------------------------------------
#> 
#> 
#>  Variable: disp
#> ---------------------------------------------------------------------------
#> cyl   gear   disp     disp     disp     disp     disp     disp     disp    
#>              n        mean     sd       se       min      median   max     
#> ----- ------ -------- -------- -------- -------- -------- -------- --------
#> 4     3      1        120.10   -        -        120.10   120.10   120.10  
#> 
#> 6     3      2        241.50   23.33    16.50    225.00   241.50   258.00  
#> 
#> 8     3      12       357.62   71.82    20.73    275.80   355.00   472.00  
#> 
#> 4     4      8        102.62   30.74    10.87    71.10    93.50    146.70  
#> 
#> 6     4      4        163.80   4.39     2.19     160.00   163.80   167.60  
#> 
#> 4     5      2        107.70   17.82    12.60    95.10    107.70   120.30  
#> 
#> 6     5      1        145.00   -        -        145.00   145.00   145.00  
#> 
#> 8     5      2        326.00   35.36    25.00    301.00   326.00   351.00  
#> ---------------------------------------------------------------------------
#> 

# --- Example 3: Formula Notation ---
# Identical result to Example 2 using formula interface
# and export output to excel
summary_formula <- f_summary(hp + disp ~ cyl + gear,
                             data    = mtcars,
                             show_Q1 = FALSE,
                             show_Q3 = FALSE,
                             export_to_excel = TRUE)
#> Saved output in: /tmp/RtmpG5HCTF/mtcars_summary.xlsx
print(summary_formula)
#> 
#>  Variable: hp
#> -------------------------------------------------------------------
#> cyl   gear   hp     hp       hp      hp      hp     hp       hp    
#>              n      mean     sd      se      min    median   max   
#> ----- ------ ------ -------- ------- ------- ------ -------- ------
#> 4     3      1      97.00    -       -       97     97.00    97    
#> 
#> 6     3      2      107.50   3.54    2.50    105    107.50   110   
#> 
#> 8     3      12     194.17   33.36   9.63    150    180.00   245   
#> 
#> 4     4      8      76.00    20.11   7.11    52     66.00    109   
#> 
#> 6     4      4      116.50   7.51    3.75    110    116.50   123   
#> 
#> 4     5      2      102.00   15.56   11.00   91     102.00   113   
#> 
#> 6     5      1      175.00   -       -       175    175.00   175   
#> 
#> 8     5      2      299.50   50.20   35.50   264    299.50   335   
#> -------------------------------------------------------------------
#> 
#> 
#>  Variable: disp
#> ---------------------------------------------------------------------------
#> cyl   gear   disp     disp     disp     disp     disp     disp     disp    
#>              n        mean     sd       se       min      median   max     
#> ----- ------ -------- -------- -------- -------- -------- -------- --------
#> 4     3      1        120.10   -        -        120.10   120.10   120.10  
#> 
#> 6     3      2        241.50   23.33    16.50    225.00   241.50   258.00  
#> 
#> 8     3      12       357.62   71.82    20.73    275.80   355.00   472.00  
#> 
#> 4     4      8        102.62   30.74    10.87    71.10    93.50    146.70  
#> 
#> 6     4      4        163.80   4.39     2.19     160.00   163.80   167.60  
#> 
#> 4     5      2        107.70   17.82    12.60    95.10    107.70   120.30  
#> 
#> 6     5      1        145.00   -        -        145.00   145.00   145.00  
#> 
#> 8     5      2        326.00   35.36    25.00    301.00   326.00   351.00  
#> ---------------------------------------------------------------------------
#> 

# --- Example 4: Distributional Stats & Digits ---
# Add skewness and kurtosis, control rounding
summary_dist <- f_summary(Sepal.Length + Petal.Length ~ Species,
                          data          = iris,
                          show_skew     = TRUE,
                          show_kurtosis = TRUE,
                          digits        = 3)
print(summary_dist)
#> 
#>  Variable: Sepal.Length
#> ------------------------------------------------------------------------------------
#> Specie       Sepal    Sepal    Sepal    Sepal    Sepal    Sepal    Sepal    Sepal   
#> s            Length   Length   Length   Length   Length   Length   Length   Length  
#>              n        mean     sd       se       min      Q1       median   Q3      
#> ------------ -------- -------- -------- -------- -------- -------- -------- --------
#> setosa       50       5.006    0.352    0.050    4.300    4.800    5.000    5.200   
#> 
#> versicolor   50       5.936    0.516    0.073    4.900    5.600    5.900    6.300   
#> 
#> virginica    50       6.588    0.636    0.090    4.900    6.225    6.500    6.900   
#> ------------------------------------------------------------------------------------
#> 
#> Table continues below
#> 
#>  
#> ---------------------------
#> Sepal    Sepal    Sepal    
#> Length   Length   Length   
#> max      skew     kurt     
#> -------- -------- ---------
#> 5.800    5.767    137.992  
#> 
#> 7.000    5.060    124.531  
#> 
#> 7.900    5.667    151.706  
#> ---------------------------
#> 
#> 
#>  Variable: Petal.Length
#> ------------------------------------------------------------------------------------
#> Specie       Petal    Petal    Petal    Petal    Petal    Petal    Petal    Petal   
#> s            Length   Length   Length   Length   Length   Length   Length   Length  
#>              n        mean     sd       se       min      Q1       median   Q3      
#> ------------ -------- -------- -------- -------- -------- -------- -------- --------
#> setosa       50       1.462    0.174    0.025    1.000    1.400    1.500    1.575   
#> 
#> versicolor   50       4.260    0.470    0.066    3.000    4.000    4.350    4.600   
#> 
#> virginica    50       5.552    0.552    0.078    4.500    5.100    5.550    5.875   
#> ------------------------------------------------------------------------------------
#> 
#> Table continues below
#> 
#>  
#> ----------------------------
#> Petal    Petal     Petal    
#> Length   Length    Length   
#> max      skew      kurt     
#> -------- --------- ---------
#> 1.900    5.109     199.182  
#> 
#> 5.100    -29.124   152.427  
#> 
#> 6.900    26.384    142.742  
#> ----------------------------
#> 

# --- Example 5: Custom Print Formatting ---
summary_iris <- f_summary(iris, "Sepal.Length", group_vars = "Species")
print(summary_iris, col_width = 10, table_width = 70)
#> 
#> -------------------------------------------------------------------
#> Species      Sepal      Sepal      Sepal      Sepal      Sepal     
#>              Length     Length     Length     Length     Length    
#>              n          mean       sd         se         min       
#> ------------ ---------- ---------- ---------- ---------- ----------
#> setosa       50         5.01       0.35       0.05       4.30      
#> 
#> versicolor   50         5.94       0.52       0.07       4.90      
#> 
#> virginica    50         6.59       0.64       0.09       4.90      
#> -------------------------------------------------------------------
#> 
#> Table continues below
#> 
#>  
#> -------------------------------------------
#> Sepal      Sepal      Sepal      Sepal     
#> Length     Length     Length     Length    
#> Q1         median     Q3         max       
#> ---------- ---------- ---------- ----------
#> 4.80       5.00       5.20       5.80      
#> 
#> 5.60       5.90       6.30       7.00      
#> 
#> 6.23       6.50       6.90       7.90      
#> -------------------------------------------
#> 


# --- Example 6: Confidence Interval for the Mean ---
# Add a 95% CI for the mean of Sepal.Length within each Species.
summary_ci <- f_summary(Sepal.Length ~ Species,
                        data    = iris,
                        show_ci = TRUE)
print(summary_ci)
#> Confidence interval: 95% (t-distribution)
#> 
#> ------------------------------------------------------------------------------------
#> Specie       Sepal    Sepal    Sepal    Sepal    Sepal    Sepal    Sepal    Sepal   
#> s            Length   Length   Length   Length   Length   Length   Length   Length  
#>              n        mean     sd       se       CI_low   CI_upp   min      Q1      
#>                                                  er       er                        
#> ------------ -------- -------- -------- -------- -------- -------- -------- --------
#> setosa       50       5.01     0.35     0.05     4.91     5.11     4.30     4.80    
#> 
#> versicolor   50       5.94     0.52     0.07     5.79     6.08     4.90     5.60    
#> 
#> virginica    50       6.59     0.64     0.09     6.41     6.77     4.90     6.23    
#> ------------------------------------------------------------------------------------
#> 
#> Table continues below
#> 
#>  
#> --------------------------
#> Sepal    Sepal    Sepal   
#> Length   Length   Length  
#> median   Q3       max     
#> -------- -------- --------
#> 5.00     5.20     5.80    
#> 
#> 5.90     6.30     7.00    
#> 
#> 6.50     6.90     7.90    
#> --------------------------
#> 

# Use a 90% interval instead
summary_ci90 <- f_summary(Sepal.Length ~ Species,
                          data       = iris,
                          show_ci    = TRUE,
                          conf_level = 0.90)
print(summary_ci90)
#> Confidence interval: 90% (t-distribution)
#> 
#> ------------------------------------------------------------------------------------
#> Specie       Sepal    Sepal    Sepal    Sepal    Sepal    Sepal    Sepal    Sepal   
#> s            Length   Length   Length   Length   Length   Length   Length   Length  
#>              n        mean     sd       se       CI_low   CI_upp   min      Q1      
#>                                                  er       er                        
#> ------------ -------- -------- -------- -------- -------- -------- -------- --------
#> setosa       50       5.01     0.35     0.05     4.92     5.09     4.30     4.80    
#> 
#> versicolor   50       5.94     0.52     0.07     5.81     6.06     4.90     5.60    
#> 
#> virginica    50       6.59     0.64     0.09     6.44     6.74     4.90     6.23    
#> ------------------------------------------------------------------------------------
#> 
#> Table continues below
#> 
#>  
#> --------------------------
#> Sepal    Sepal    Sepal   
#> Length   Length   Length  
#> median   Q3       max     
#> -------- -------- --------
#> 5.00     5.20     5.80    
#> 
#> 5.90     6.30     7.00    
#> 
#> 6.50     6.90     7.90    
#> --------------------------
#> 
```
