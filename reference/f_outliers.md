# Identify Outliers within Groups using Tukey's Fences

\`f_outliers()\` scans numerical column(s) for outliers based on the
Interquartile Range (IQR) method. It can detect outliers across the
entire dataset or within specified subgroups.

It returns a dataframe containing only the outlier rows, preserving the
original data structure and adding a `row_id` column for traceability.

## Usage

``` r
f_outliers(x, ...)

# S3 method for class 'numeric'
f_outliers(x, ...)

# S3 method for class 'integer'
f_outliers(x, ...)

# S3 method for class 'formula'
f_outliers(formula, data, ...)

# S3 method for class 'data.frame'
f_outliers(
  x,
  columns = NULL,
  group_vars = NULL,
  id_var = NULL,
  coef = 1.5,
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

  Further arguments forwarded to `f_outliers.data.frame`.

- formula:

  A formula specifying the columns (right hand side) to be checked per
  subgroup(s) (left hand side). More columns or groups can be added
  using `-` or `+` (e.g., `col1 + col2 ~ group1 + group2`) to do a
  sequential analysis for each column parameter.

- data:

  A `vector`, `data.frame`, `data.table`, or `tibble`.

- columns:

  The numerical columns to analyze if no formula is used. Can be entered
  as a single character string (e.g., `"weight"`) or as a character
  vector `c("weight", "length"`). When omitted, defaults to all numeric
  columns in `data` (excluding any columns named in `group_vars` or
  `id_var`).

- group_vars:

  A character vector specifying the grouping variables in `data` If
  `NULL` (default), the entire dataset is treated as one group (e.g.,
  `c("species", "fertilizer")`).

- id_var:

  (Optional) A character string naming a user-specific ID columns (e.g.,
  `"EmployeeID"`). If provided, this columns is placed at the start of
  the output for easy identification.

- coef:

  A number indicating the IQR multiplier. Default is `1.5` (standard
  Tukey fence). Common values:

  - `1.5`: Standard outliers (approximately +/- 2.7 SD in normal
    distribution).

  - `3.0`: Extreme outliers.

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

## Value

A data.frame containing the identified outlier rows. Returns `NULL`
(with a message) if no outliers are found.

## Details

**The Outlier Logic (Tukey's Method):** An observation is flagged as an
outlier if it falls outside the calculated fences:

- **Lower Fence:** \\Q1 - (coef \times IQR)\\

- **Upper Fence:** \\Q3 + (coef \times IQR)\\

Where \\Q1\\ is the 25th percentile, \\Q3\\ is the 75th percentile, and
\\IQR = Q3 - Q1\\.

**Output Structure:** The function returns a subset of the original
`data`. It automatically adds a `row_id` columns, which corresponds to
the row number in the original dataframe. This ensures you can strictly
map the outliers back to the source data.

## See also

[`f_remove_outliers`](https://delde001.github.io/rfriend/reference/f_remove_outliers.md)
to remove the rows identified by this function.

## Examples

``` r
# --- Setup: Create Dummy Data ---
set.seed(42)
df <- data.frame(
  Team       = rep(c("A", "B"), each = 20),
  Department = rep(c("Sales", "IT"), each = 10, times = 2),
  Salary     = rnorm(40, mean = 50000, sd = 2000),
  Age        = rnorm(40, mean = 35, sd = 3),
  EmployeeID = paste0("E", sprintf("%03d", 1:40))
)

# Inject outliers
df[2,  "Salary"] <- 57000   # Mild outlier (between 1.5 and 3.0 fence)
df[1,  "Salary"] <- 100000  # Extreme high
df[35, "Salary"] <- 1000    # Extreme low

# --- Example 1: Basic detection (data.frame notation) ---
# Scan the entire dataset for Salary outliers (no grouping)
out <- f_outliers(df, columns = "Salary")
print(out)
#> 
#> ----------------------------------------------------
#> row_id   Salary     Team   Depart   Age     Employ  
#>                            ment             eeID    
#> -------- ---------- ------ -------- ------- --------
#> 1        1.00e+05   A      Sales    35.62   E001    
#> 
#> 2        5.70e+04   A      Sales    33.92   E002    
#> 
#> 35       1000       B      IT       33.37   E035    
#> ----------------------------------------------------
#> 

# --- Example 2: Basic detection (formula notation) ---
# Equivalent to Example 1 using the formula interface
# LHS = column(s) to scan, RHS = grouping variable(s)
out <- f_outliers(Salary ~ 1, data = df)
print(out)
#> 
#> ----------------------------------------------------
#> row_id   Salary     Team   Depart   Age     Employ  
#>                            ment             eeID    
#> -------- ---------- ------ -------- ------- --------
#> 1        1.00e+05   A      Sales    35.62   E001    
#> 
#> 2        5.70e+04   A      Sales    33.92   E002    
#> 
#> 35       1000       B      IT       33.37   E035    
#> ----------------------------------------------------
#> 

# --- Example 3: Grouped detection (both notations) ---
# Outliers are now evaluated *within* each Team separately,
# making detection sensitive to group-level distributions

# data.frame notation:
out <- f_outliers(df, columns = "Salary", group_vars = "Team")

# Formula notation (identical result):
out <- f_outliers(Salary ~ Team, data = df)
print(out)
#> 
#> ----------------------------------------------------
#> row_id   Salary     Team   Depart   Age     Employ  
#>                            ment             eeID    
#> -------- ---------- ------ -------- ------- --------
#> 1        1.00e+05   A      Sales    35.62   E001    
#> 
#> 18       4.47e+04   A      IT       35.27   E018    
#> 
#> 35       1000       B      IT       33.37   E035    
#> ----------------------------------------------------
#> 

# --- Example 4: Multi-column + multi-group (formula notation) ---
# Scan both Salary and Age for outliers, grouped by Team and Department.
# Returns a named list: one data.frame per column scanned.
out <- f_outliers(Salary + Age ~ Team + Department, data = df)
print(out)            # prints both result tables
#> 
#>  Variable: Salary
#> ----------------------------------------------------
#> row_id   Salary     Team   Depart   Age     Employ  
#>                            ment             eeID    
#> -------- ---------- ------ -------- ------- --------
#> 1        1.00e+05   A      Sales    35.62   E001    
#> 
#> 25       5.38e+04   B      Sales    32.82   E025    
#> 
#> 35       1000       B      IT       33.37   E035    
#> ----------------------------------------------------
#> 
#> 
#>  Variable: Age
#> ----------------------------------------------------
#> row_id   Age     Team   Depart   Salary     Employ  
#>                         ment                eeID    
#> -------- ------- ------ -------- ---------- --------
#> 12       32.65   A      IT       5.46e+04   E012    
#> 
#> 13       39.73   A      IT       4.72e+04   E013    
#> 
#> 19       26.02   A      IT       4.51e+04   E019    
#> ----------------------------------------------------
#> 
out$Salary            # access Salary outliers directly
#>   row_id    Salary Team Department      Age EmployeeID
#> 1      1 100000.00    A      Sales 35.61800       E001
#> 2     25  53790.39    B      Sales 32.81812       E025
#> 3     35   1000.00    B         IT 33.37151       E035
out$Age               # access Age outliers directly
#>   row_id      Age Team Department   Salary EmployeeID
#> 1     12 32.64848    A         IT 54573.29       E012
#> 2     13 39.72718    A         IT 47222.28       E013
#> 3     19 26.02073    A         IT 45119.07       E019

# --- Example 5: Strict detection with a custom ID column ---
# coef = 3.0 flags only extreme outliers (the "far out" Tukey fence).
# id_var places EmployeeID first in the output for easy identification.

# data.frame notation:
out <- f_outliers(df,
                  columns   = "Salary",
                  group_vars = "Team",
                  id_var    = "EmployeeID",
                  coef      = 3.0)

# Formula notation (identical result):
out <- f_outliers(Salary ~ Team, data = df,
                  id_var = "EmployeeID",
                  coef   = 3.0)
print(out)
#> 
#> ----------------------------------------------------
#> Employ   row_id   Salary     Team   Depart   Age    
#> eeID                                ment            
#> -------- -------- ---------- ------ -------- -------
#> E001     1        1.00e+05   A      Sales    35.62  
#> 
#> E035     35       1000       B      IT       33.37  
#> ----------------------------------------------------
#> 

# --- Example 6: Sensitivity comparison ---
# Compare how coef = 1.5 (standard) vs coef = 3.0 (extreme-only)
# affects the number of flagged rows.

out_standard <- f_outliers(Salary ~ Team, data = df, coef = 1.5)
out_extreme  <- f_outliers(Salary ~ Team, data = df, coef = 3.0)

nrow(out_standard$output_df)  # 3 -- catches mild + extreme outliers
#> [1] 3
nrow(out_extreme$output_df)   # 2 -- catches extreme outliers only
#> [1] 2

# --- Example 7: Vector input ---
# Pass a column directly as a vector -- no data.frame needed.
# The column name is captured automatically from the call.

out <- f_outliers(df$Salary)
print(out)
#> 
#> -------------------
#> row_id   Salary    
#> -------- ----------
#> 1        1.00e+05  
#> 
#> 2        5.70e+04  
#> 
#> 35       1000      
#> -------------------
#> 

# Works with coef and other parameters too
out <- f_outliers(df$Salary, coef = 3.0)
print(out)
#> 
#> -------------------
#> row_id   Salary    
#> -------- ----------
#> 1        1.00e+05  
#> 
#> 35       1000      
#> -------------------
#> 

# Inline vectors fall back to the column name "value"
out <- f_outliers(c(1, 2, 3, 4, 5, 100))
print(out)
#> 
#> ----------------
#> row_id   value  
#> -------- -------
#> 6        100    
#> ----------------
#> 
```
