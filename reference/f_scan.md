# Perform a visual check on your data

Creates a 3-panel diagnostic dashboard to check data distribution and
assumptions. It can also output a data summary table and identify
outliers.

## Usage

``` r
f_scan(x, ...)

# S3 method for class 'formula'
f_scan(formula, data = NULL, ...)

# S3 method for class 'numeric'
f_scan(x, ...)

# S3 method for class 'integer'
f_scan(x, ...)

# S3 method for class 'data.frame'
f_scan(
  x,
  columns = NULL,
  group_vars = NULL,
  summary = TRUE,
  outliers = TRUE,
  coef = 1.5,
  limit_columns = 7,
  fancy_names = NULL,
  advice = FALSE,
  close_generated_files = FALSE,
  open_generated_files = interactive(),
  output_type = "default",
  save_as = NULL,
  save_in_wdir = FALSE,
  digits = NULL,
  ...
)
```

## Arguments

- x:

  A data.frame or formula (dispatches to the right method).

- ...:

  Further arguments forwarded to `f_scan.data.frame`.

- formula:

  A formula specifying the columns (right hand side) to be summarized by
  maximal 3 groups (left hand side). More columns or groups can be added
  using `-` or `+` (e.g., `col1 + col2 ~ group1 + group2`) to do a
  sequential summary for each column parameter.

- data:

  A 'data.frame', 'data.table', or 'tibble'.

- columns:

  The numerical column(s) to summarize if no formula is used. Can be
  entered as a single character string (e.g., `"weight"`) or as a
  character vector `c("weight", "length"`). When omitted, defaults to
  all numeric columns in `data` (excluding any columns named in
  `group_vars`).

- group_vars:

  Character vector of up to 3 grouping variables (e.g.,
  `c("species", "fertilizer")`).

- summary:

  Logical. Show a summary table of the data. Default is `TRUE`.

- outliers:

  Logical. If `TRUE`, scans for outliers using Tukey's fences and if
  they exist, adds them to the result object. Default `TRUE`.

- coef:

  Numeric. The multiplier for the Interquartile Range (IQR) used for
  outlier detection. Default `1.5`.

- limit_columns:

  Integer or `NULL`. Defines the number of columns shown in the outlier
  table. Default = `7`. `NULL` = all columns are shown.

- fancy_names:

  Named character vector or `NULL`. Optional mapping of column names to
  more readable names for display in plots and legends.

- advice:

  Logical. If `TRUE`, runs
  [`f_stat_wizard()`](https://delde001.github.io/rfriend/reference/f_stat_wizard.md)
  on each response column and appends the recommendation to the result.
  The advice is accessible via `result[["column_name"]]$advice` and is
  printed automatically. Default `FALSE`.

- close_generated_files:

  Logical. Closes open Excel or Word (NOT pdf) files before writing,
  depending on the output format. Works on Windows (taskkill), macOS
  (pkill) and Linux (pkill/soffice). Default `FALSE`. **WARNING:**
  Always save your work before using this option!!

- open_generated_files:

  Logical. Whether to open the generated output files after creation.
  Defaults to `TRUE` in an interactive R session and `FALSE` otherwise
  (e.g. in scripts or automated pipelines). Set to `TRUE` or `FALSE` to
  override this behaviour explicitly.

- output_type:

  Character string specifying the output format. Default is `"default"`.

  - `"default"`: Returns the object and lets R decide whether to print;
    auto-prints if unassigned, silent if assigned to a variable. Use
    `print(result)` or `plot(result)` to display the returned object.

  - `"console"`: Forces immediate printing to the console regardless of
    object assignment.

  - `"pdf"`, `"word"`, `"excel"`: Saves results to a file of the
    corresponding format. See `save_as`, `save_in_wdir`, and
    `open_generated_files` for file path and opening behavior.

  - `"rmd"`: Stores the raw markdown string inside the returned object
    for use in R Markdown documents.

- save_as:

  Character string specifying the output file path (without extension).
  If a full path is provided, output is saved to that location. If only
  a filename is given, the file is saved in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). If only a
  directory is specified (providing an existing directory with trailing
  slash), the file is named "dataname_fscan" in that directory. If an
  extension is provided the output format specified with option
  "output_type" will be overruled. Defaults to
  `file.path(tempdir(), "dataname_fscan.pdf")`.

- save_in_wdir:

  Logical. If `TRUE`, saves the file in the working directory. Default
  is `FALSE`, this avoid unintended changes to the global environment.
  If `save_as` location is specified `save_in_wdir` is overwritten by
  `save_as`.

- digits:

  Integer. Decimal places for printed tables in 'pdf' and 'Word' output
  files. Default `3`.

## Value

A list of class `f_scan` containing plots, the summary table, and the
outlier table. Using the option "output_type", it can also generate
output in the form of: R Markdown code, 'Word', 'pdf', or 'Excel' files.
Includes print, summary and plot methods for 'f_scan' objects.

## Details

`f_scan` automatically adapts the visualization based on the number of
grouping variables provided:

- **0 groups:** Univariate analysis (Single density/boxplot).

- **1 group :** Main grouping variable (X-axis and Color).

- **2 groups:** Adds Facet Wrapping.

- **3 groups:** Adds Facet Grid (Row vs Column).

This function requires
\[Pandoc\](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3
or higher), a universal document converter.

- **Windows:** Install Pandoc and ensure the installation folder.  
  (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your
  system PATH.

- **macOS:** If using Homebrew, Pandoc is typically installed in
  "/usr/local/bin". Alternatively, download the .pkg installer and
  verify that the binary's location is in your PATH.

- **Linux:** Install Pandoc through your distribution's package manager
  (commonly installed in "/usr/bin" or "/usr/local/bin") or manually,
  and ensure the directory containing Pandoc is in your PATH.

- If Pandoc is not found, this function may not work as intended.

## Examples

``` r
# 1. Non-formula | No groups | Default output (default)
result <- f_scan(iris, columns = "Sepal.Length")
print(result)
#> 
#> --- Summary Statistics ---
#> 
#> --------------------------------------------------------------
#> n     mean   sd      se       min   Q1    median   Q3    max  
#> ----- ------ ------- -------- ----- ----- -------- ----- -----
#> 150   5.84   0.828   0.0676   4.3   5.1   5.8      6.4   7.9  
#> --------------------------------------------------------------
#> 
#> 
#> 
#> --- Outlier Detection ---
#> No outliers detected.
#> 





# 2. Non-formula | 1 group | Console output
result <- f_scan(
  mtcars,
  columns   = "mpg",
  group_vars = "cyl",
  output_type = "console"
)
#> 
#> --- Summary Statistics ---
#> 
#> ---------------------------------------------------------------------
#> cyl   n    mean   sd     se      min    Q1     median   Q3     max   
#> ----- ---- ------ ------ ------- ------ ------ -------- ------ ------
#> 4     11   26.7   4.51   1.360   21.4   22.8   26.0     30.4   33.9  
#> 
#> 6     7    19.7   1.45   0.549   17.8   18.6   19.7     21.0   21.4  
#> 
#> 8     14   15.1   2.56   0.684   10.4   14.4   15.2     16.2   19.2  
#> ---------------------------------------------------------------------
#> 
#> 
#> 
#> --- Outlier Detection ---
#> 
#> Found 3 outliers:
#> 
#> -------------------------------------------------------
#> row_id   mpg    cyl   disp   hp    drat   wt     qsec  
#> -------- ------ ----- ------ ----- ------ ------ ------
#> 15       10.4   8     472    205   2.93   5.25   18.0  
#> 
#> 16       10.4   8     460    215   3.00   5.42   17.8  
#> 
#> 25       19.2   8     400    175   3.08   3.85   17.1  
#> -------------------------------------------------------
#> 
#> 





# \donttest{
# 3. Non-formula | 2 groups | Multiple columns | Excel output
 result <- f_scan(
   mtcars,
   columns    = c("mpg", "hp"),
   group_vars = c("cyl", "am"),
   outliers   = TRUE,
   coef       = 1.5,
   output_type = "excel",
   save_as    = "mtcars_scan"
 )
#> Saving output in: /tmp/RtmpG5HCTF/mtcars_scan.xlsx

# 4. Formula | 1 group | Strict outlier detection | Word output
result <- f_scan(
  Sepal.Width ~ Species,
  data        = iris,
  outliers    = TRUE,
  coef        = 3.0,
  output_type = "word",
  save_as     = "iris_scan"
 )
#> Saving output in: /tmp/RtmpG5HCTF/iris_scan.docx

# 5. Formula | 2 groups | Multiple columns | Fancy names
result <- f_scan(
 mpg + hp + wt ~ vs + am,
 data        = mtcars,
 fancy_names = c(mpg = "Fuel Efficiency", hp = "Horsepower",
                 wt  = "Weight",          vs = "Engine Type",
                 am  = "Transmission"),
 summary     = TRUE
)
print(result)
#> 
#>  Variable: Fuel Efficiency
#> --- Summary Statistics ---
#> 
#> -----------------------------------------------------------------------------------------
#> Engine Typ   Transmissi   n    mean   sd     se      min    Q1     median   Q3     max   
#> e            on                                                                          
#> ------------ ------------ ---- ------ ------ ------- ------ ------ -------- ------ ------
#> 0            0            12   15.1   2.77   0.801   10.4   14.1   15.2     16.6   19.2  
#> 
#> 1            0            7    20.7   2.47   0.934   17.8   18.6   21.4     22.1   24.4  
#> 
#> 0            1            6    19.8   4.01   1.637   15.0   16.8   20.4     21.0   26.0  
#> 
#> 1            1            7    28.4   4.76   1.798   21.4   25.1   30.4     31.4   33.9  
#> -----------------------------------------------------------------------------------------
#> 
#> 
#> 
#> --- Outlier Detection ---
#> No outliers detected.
#> 




#> 
#>  Variable: Horsepower
#> --- Summary Statistics ---
#> 
#> ----------------------------------------------------------------------------------------
#> Engine Typ   Transmissi   n    mean    sd     se      min   Q1      median   Q3    max  
#> e            on                                                                         
#> ------------ ------------ ---- ------- ------ ------- ----- ------- -------- ----- -----
#> 0            0            12   194.2   33.4   9.63    150   175.0   180      219   245  
#> 
#> 1            0            7    102.1   20.9   7.91    62    96.0    105      116   123  
#> 
#> 0            1            6    180.8   98.8   40.34   91    110.0   142      242   335  
#> 
#> 1            1            7    80.6    24.1   9.13    52    65.5    66       101   113  
#> ----------------------------------------------------------------------------------------
#> 
#> 
#> 
#> --- Outlier Detection ---
#> 
#> Found 1 outliers:
#> 
#> ----------------------------------------------------------------
#> row_id   Horsep   Engine   Transm   Fuel E   cyl   disp   drat  
#>          ower     Type     ission   fficie                      
#>                                     ncy                         
#> -------- -------- -------- -------- -------- ----- ------ ------
#> 8        62       1        0        24.4     4     147    3.69  
#> ----------------------------------------------------------------
#> 
#> 




#> 
#>  Variable: Weight
#> --- Summary Statistics ---
#> 
#> ------------------------------------------------------------------------------------------
#> Engine Typ   Transmissi   n    mean   sd      se      min    Q1     median   Q3     max   
#> e            on                                                                           
#> ------------ ------------ ---- ------ ------- ------- ------ ------ -------- ------ ------
#> 0            0            12   4.10   0.768   0.222   3.44   3.56   3.81     4.37   5.42  
#> 
#> 1            0            7    3.19   0.348   0.131   2.46   3.17   3.21     3.44   3.46  
#> 
#> 0            1            6    2.86   0.487   0.199   2.14   2.66   2.82     3.10   3.57  
#> 
#> 1            1            7    2.03   0.440   0.166   1.51   1.73   1.94     2.26   2.78  
#> ------------------------------------------------------------------------------------------
#> 
#> 
#> 
#> --- Outlier Detection ---
#> 
#> Found 1 outliers:
#> 
#> ------------------------------------------------------------------
#> row_id   Weight   Engine   Transm   Fuel E   cyl   disp   Horsep  
#>                   Type     ission   fficie                ower    
#>                                     ncy                           
#> -------- -------- -------- -------- -------- ----- ------ --------
#> 21       2.46     1        0        21.5     4     120    97      
#> ------------------------------------------------------------------
#> 
#> 






#Create a small reproducible dataset with 3 grouping variables
set.seed(42)
plant_data <- data.frame(
  weight    = c(rnorm(60, 10, 2), rnorm(60, 14, 2)),
  species   = rep(c("A", "B"), each = 60),
  treatment = rep(rep(c("control", "treated"), each = 30), 2),
  batch     = factor(rep(c("1", "2", "3"), 40))
 )

# 6. Formula | 3 groups | Facet Grid
result <- f_scan(
  weight ~ species + treatment + batch,
  data         = plant_data,
  coef         = 2.0,
  digits       = 2,
  output_type  = "word"
)
#> Saving output in: /tmp/RtmpG5HCTF/plant_data_fscan_output.docx
print(result)
#> 
#> --- Summary Statistics ---
#> 
#> ------------------------------------------------------------------------------------------------
#> species   treatment   batch   n    mean    sd     se      min     Q1      median   Q3     max   
#> --------- ----------- ------- ---- ------- ------ ------- ------- ------- -------- ------ ------
#> A         control     1       10   9.72    3.16   1.001   5.12    6.66    10.57    12.4   13.8  
#> 
#> B         control     1       10   14.26   1.58   0.499   11.61   13.36   14.59    15.2   16.8  
#> 
#> A         treated     1       10   9.85    1.10   0.348   8.43    8.87    10.13    10.7   11.5  
#> 
#> B         treated     1       10   14.55   2.66   0.842   10.68   12.55   14.30    16.4   19.4  
#> 
#> A         control     2       10   10.33   1.37   0.434   8.87    9.43    9.73     10.9   12.6  
#> 
#> B         control     2       10   13.96   1.72   0.545   11.80   12.21   14.27    15.5   16.1  
#> 
#> A         treated     2       10   9.71    2.53   0.801   4.01    8.42    10.48    11.2   13.2  
#> 
#> B         treated     2       10   13.49   2.13   0.675   11.08   12.09   13.10    14.0   17.7  
#> 
#> A         control     3       10   10.36   2.85   0.901   4.69    9.41    9.76     12.0   14.6  
#> 
#> B         control     3       10   14.93   1.38   0.436   12.91   13.77   15.05    15.8   17.0  
#> 
#> A         treated     3       10   9.71    2.56   0.811   5.17    7.77    10.61    11.3   12.9  
#> 
#> B         treated     3       10   13.84   1.19   0.377   12.28   12.98   13.85    14.2   16.1  
#> ------------------------------------------------------------------------------------------------
#> 
#> 
#> 
#> --- Outlier Detection ---
#> No outliers detected.
#> 





# 7. With statistical advice
result <- f_scan(
  Sepal.Length ~ Species,
  data    = iris,
  advice  = TRUE
)
#' print(result)
result[["Sepal.Length"]]$advice$y_type
#> [1] "ratio_normal"


# 8. Vector input | Single numeric vector (no formula, no data.frame)
# When you only have loose vectors in your workspace, pass one
# directly to f_scan(). The vector's name is used as the column label
# in the dashboard and outlier table.
disp1 <- mtcars$disp
result <- f_scan(disp1)
print(result)
#> 
#> --- Summary Statistics ---
#> 
#> ----------------------------------------------------------
#> n    mean   sd    se     min    Q1    median   Q3    max  
#> ---- ------ ----- ------ ------ ----- -------- ----- -----
#> 32   231    124   21.9   71.1   121   196      326   472  
#> ----------------------------------------------------------
#> 
#> 
#> 
#> --- Outlier Detection ---
#> No outliers detected.
#> 





# 9. Formula on vectors | Multiple responses | One grouping vector
# f_scan() also accepts a formula built from bare vectors, i.e.
# no `data =` argument is needed. Multiple
# response variables are combined with `+` on the
# left hand side of the formula, exactly as
# in the data.frame form.
disp1 <- mtcars$disp
hp1   <- mtcars$hp
cyl1  <- factor(mtcars$cyl)
result <- f_scan(disp1 + hp1 ~ cyl1)
print(result)
#> 
#>  Variable: disp1
#> --- Summary Statistics ---
#> 
#> ---------------------------------------------------------------------
#> cyl1   n    mean   sd     se     min     Q1      median   Q3    max  
#> ------ ---- ------ ------ ------ ------- ------- -------- ----- -----
#> 4      11   105    26.9   8.1    71.1    78.8    108      121   147  
#> 
#> 6      7    183    41.6   15.7   145.0   160.0   168      196   258  
#> 
#> 8      14   353    67.8   18.1   275.8   301.8   350      390   472  
#> ---------------------------------------------------------------------
#> 
#> 
#> 
#> --- Outlier Detection ---
#> 
#> Found 1 outliers:
#> 
#> -----------------------------
#> row_id   disp1   cyl1   hp1  
#> -------- ------- ------ -----
#> 4        258     6      110  
#> -----------------------------
#> 
#> 




#> 
#>  Variable: hp1
#> --- Summary Statistics ---
#> 
#> ---------------------------------------------------------------------
#> cyl1   n    mean    sd     se      min   Q1      median   Q3    max  
#> ------ ---- ------- ------ ------- ----- ------- -------- ----- -----
#> 4      11   82.6    20.9   6.31    52    65.5    91       96    113  
#> 
#> 6      7    122.3   24.3   9.17    105   110.0   110      123   175  
#> 
#> 8      14   209.2   51.0   13.62   150   176.2   192      241   335  
#> ---------------------------------------------------------------------
#> 
#> 
#> 
#> --- Outlier Detection ---
#> 
#> Found 1 outliers:
#> 
#> -----------------------------
#> row_id   hp1   cyl1   disp1  
#> -------- ----- ------ -------
#> 30       175   6      145    
#> -----------------------------
#> 
#> 





# 10. Positional vector form: equivalent to f_scan(disp1 ~ cyl1).
# The first vector is the response, the rest are grouping variables.
disp1 <- mtcars$disp
cyl1  <- factor(mtcars$cyl)
f_scan(disp1, cyl1)
#> 
#> --- Summary Statistics ---
#> 
#> ---------------------------------------------------------------------
#> cyl1   n    mean   sd     se     min     Q1      median   Q3    max  
#> ------ ---- ------ ------ ------ ------- ------- -------- ----- -----
#> 4      11   105    26.9   8.1    71.1    78.8    108      121   147  
#> 
#> 6      7    183    41.6   15.7   145.0   160.0   168      196   258  
#> 
#> 8      14   353    67.8   18.1   275.8   301.8   350      390   472  
#> ---------------------------------------------------------------------
#> 
#> 
#> 
#> --- Outlier Detection ---
#> 
#> Found 1 outliers:
#> 
#> -----------------------
#> row_id   disp1   cyl1  
#> -------- ------- ------
#> 4        258     6     
#> -----------------------
#> 
#> 





# }
```
