# Summarize a Data Frame with Grouping Variables

Computes summary statistics (e.g., mean, standard deviation, median,
etc.) for a specified column ("character string") in a data frame,
grouped by one or more grouping variables in that data frame ("character
strings"). Summary parameters can be customized and the results can be
exported to an 'Excel' file.

## Usage

``` r
f_summary(
  data,
  data.column,
  ...,
  show_n = TRUE,
  show_mean = TRUE,
  show_sd = TRUE,
  show_se = TRUE,
  show_min = TRUE,
  show_max = TRUE,
  show_median = TRUE,
  show_Q1 = TRUE,
  show_Q3 = TRUE,
  digits = 2,
  export_to_excel = FALSE,
  close_generated_files = FALSE,
  open_generated_files = TRUE,
  save_as = NULL,
  save_in_wdir = FALSE,
  check_input = TRUE,
  eval_input = FALSE,
  digits_excel = NULL,
  detect_int_col = TRUE
)
```

## Arguments

- data:

  A 'data.frame', 'data.table' or 'tibble', i.e. input data to be
  summarized.

- data.column:

  A character string, vector or list with characters. The name of the
  column(s) in `data` for which summary statistics will be calculated.

- ...:

  One or more character strings specifying the grouping variables in
  `data`.

- show_n:

  Logical. If `TRUE`, the summary results `n` will be included in the
  output.

- show_mean:

  Logical. If `TRUE`, the summary results `mean` will be included in the
  output.

- show_sd:

  Logical. If `TRUE`, the summary results `sd` will be included in the
  output.

- show_se:

  Logical. If `TRUE`, the summary results `se` will be included in the
  output.

- show_min:

  Logical. If `TRUE`, the summary results `min` will be included in the
  output.

- show_max:

  Logical. If `TRUE`, the summary results `max` will be included in the
  output.

- show_median:

  Logical. If `TRUE`, the summary results `median` will be included in
  the output.

- show_Q1:

  Logical. If `TRUE`, the summary results `Q1` will be included in the
  output.

- show_Q3:

  Logical. If `TRUE`, the summary results `Q3` will be included in the
  output.

- digits:

  Integer. Round to the number of digits specified. If `digits = NULL`
  no rounding is applied (default is `digits = 2`). Note that this
  rounding is independent of the rounding in the exported excel file.

- export_to_excel:

  Logical. If `TRUE`, the (unrounded values) summary results will be
  exported to an 'Excel' file. Default is `FALSE`.

- close_generated_files:

  Logical. If `TRUE`, closes open 'Excel' files. This to be able to save
  the newly generated file. Default is `FALSE`.

- open_generated_files:

  Logical. If `TRUE`, Opens the generated 'Excel' files. This to
  directly view the results after creation. Files are stored in
  tempdir(). Default is `TRUE`.

- save_as:

  Character string specifying the output file path (without extension).
  If a full path is provided, output is saved to that location. If only
  a filename is given, the file is saved in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). If only a
  directory is specified (providing an existing directory with trailing
  slash), the file is named "dataname_summary.xlsx" in that directory.
  Defaults to `file.path(tempdir(), "dataname_summary.xlsx")`.

- save_in_wdir:

  Logical. If `TRUE`, saves the file in the working directory. Default
  is `FALSE`, to avoid unintended changes to the global environment. If
  `save_as` location is specified `save_in_wdir` is overwritten by
  `save_as`.

- check_input:

  If `TRUE`, checks the input and stops the function if the input is
  incorrect (default is `TRUE`).

- eval_input:

  Logical. If `TRUE`, the function evaluates the third function
  argument. This should be a character vector with the group by columns.
  Default is `FALSE`, which allows group by columns to be written
  without quotes.

- digits_excel:

  Integer. Round cells in the excel file to the number of digits
  specified. If `digits_excel = NULL` no rounding is applied (default is
  `digits_excel = NULL`). Note to preserve formatting numbers will be
  stored as text.

- detect_int_col:

  Logical. If `TRUE`, columns in a data.frame containing only integers
  will be displayed without decimal digits. Columns containing a mix of
  integers and decimal values will display all values with the specified
  number of digits. If `FALSE`, each individual cell is evaluated:
  integer values are displayed without digits, and numbers containing
  digits with the specified number of digits. Default is `TRUE`.

## Value

A data frame containing the computed summary statistics, grouped by the
specified variables. This data frame can be automatically saved as an
'Excel' file using `export_to_excel = TRUE`.

## Details

The function computes the following summary statistics for the specified
column:

- `n`: number of observations

- `mean`: mean

- `sd`: standard deviation

- `se`: standard error of the mean

- `min`: minimum value

- `max`: maximum value

- `median`: median

- `Q1`: first quartile

- `Q3`: third quartile

Each of these summary statistics can be removed by setting e.g.
`show_n = FALSE`, The results are grouped by the specified grouping
variables and returned as a data frame. If `export_to_excel` is set to
`TRUE`, the results are saved as an 'Excel' file in the working
directory with a dynamically generated filename.

When only `data` and `data.column` is provided the function defaults to
base:summary(data).

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Example usage:
# Create a summary of mtcars for data column hp grouped by cyl and gear,
# and remove Q1 and Q3 from the output.
# Note that variable can be written as "hp" or as hp. Only data.frame must be data (no quotes)
summary_mtcars <- f_summary(mtcars, "hp", "cyl", "gear", show_Q1 = FALSE, show_Q3 = FALSE)
print(summary_mtcars)
#> 
#> -----------------------------------------------------------------------
#> cyl   gear   hp.n   hp.mea   hp.sd   hp.se   hp.min   hp.med   hp.max  
#>                     n                                 ian              
#> ----- ------ ------ -------- ------- ------- -------- -------- --------
#> 4     3      1      97.00                    97       97.00    97      
#> 
#> 6     3      2      107.50   3.54    2.50    105      107.50   110     
#> 
#> 8     3      12     194.17   33.36   9.63    150      180.00   245     
#> 
#> 4     4      8      76.00    20.11   7.11    52       66.00    109     
#> 
#> 6     4      4      116.50   7.51    3.75    110      116.50   123     
#> 
#> 4     5      2      102.00   15.56   11.00   91       102.00   113     
#> 
#> 6     5      1      175.00                   175      175.00   175     
#> 
#> 8     5      2      299.50   50.20   35.50   264      299.50   335     
#> -----------------------------------------------------------------------
#> 

# Create a summary for iris
summary_iris <- f_summary(iris, Sepal.Length, Species)

# Print the a table with column width of 10 characters and table length of 70 characters
print(summary_iris, col_width =  10, table_width = 70)
#> 
#> ----------------------------------------------------------------
#> Species      Sepal.Leng   Sepal.Leng   Sepal.Leng   Sepal.Leng  
#>              th.n         th.mean      th.sd        th.se       
#> ------------ ------------ ------------ ------------ ------------
#> setosa       50           5.01         0.35         0.05        
#> 
#> versicolor   50           5.94         0.52         0.07        
#> 
#> virginica    50           6.59         0.64         0.09        
#> ----------------------------------------------------------------
#> 
#> Table continues below
#> 
#>  
#> ----------------------------------------------------------------
#> Sepal.Leng   Sepal.Leng   Sepal.Leng   Sepal.Leng   Sepal.Leng  
#> th.min       th.Q1        th.median    th.Q3        th.max      
#> ------------ ------------ ------------ ------------ ------------
#> 4.30         4.80         5.00         5.20         5.80        
#> 
#> 4.90         5.60         5.90         6.30         7.00        
#> 
#> 4.90         6.23         6.50         6.90         7.90        
#> ----------------------------------------------------------------
#> 
```
