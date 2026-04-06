# Perform a visual check on your data

Creates a 3-panel diagnostic dashboard to check data distribution and
assumptions. It can also output a data summary table and identify
outliers.

## Usage

``` r
f_scan(x, ...)

# S3 method for class 'formula'
f_scan(formula, data, ...)

# S3 method for class 'data.frame'
f_scan(
  data,
  columns,
  group_vars = NULL,
  summary = TRUE,
  outliers = TRUE,
  coef = 1.5,
  limit_columns = 7,
  fancy_names = NULL,
  close_generated_files = FALSE,
  open_generated_files = TRUE,
  output_type = "default",
  save_as = NULL,
  save_in_wdir = FALSE,
  digits = NULL,
  ...
)
```

## Arguments

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
  character vector `c("weight", "length"`).

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

- close_generated_files:

  Logical. Closes open Excel or Word (NOT pdf) files before writing,
  depending on the output format. Works on Windows (taskkill), macOS
  (pkill) and Linux (pkill/soffice). Default `FALSE`. **WARNING:**
  Always save your work before using this option!!

- open_generated_files:

  Logical. If `TRUE`, Opens the generated output files ('pdf', 'Word' or
  'Excel') files depending on the output format. This to directly view
  the results after creation. Files are stored in tempdir(). Default is
  `TRUE`.

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
  verify that the binary’s location is in your PATH.

- **Linux:** Install Pandoc through your distribution’s package manager
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
#> Saving output in: /tmp/RtmpyM0xyc/mtcars_scan.xlsx

# 4. Formula | 1 group | Strict outlier detection | Word output
result <- f_scan(
  Sepal.Width ~ Species,
  data        = iris,
  outliers    = TRUE,
  coef        = 3.0,
  output_type = "word",
  save_as     = "iris_scan"
 )
#> Saving output in: /tmp/RtmpyM0xyc/iris_scan.docx

# 5. Formula | 2 groups | Multiple columns | Fancy names
result <- f_scan(
 mpg + hp + wt ~ vs + am,
 data        = mtcars,
 fancy_names = c(mpg = "Fuel Efficiency", hp = "Horsepower",
                 wt  = "Weight",          vs = "Engine Type",
                 am  = "Transmission"),
 summary     = TRUE,
 outliers    = FALSE
)
print(result)
#> 
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





#Create a small reproducible dataset with 3 grouping variables
set.seed(42)
plant_data <- data.frame(
  weight    = c(rnorm(60, 10, 2), rnorm(60, 14, 2)),
  species   = rep(c("A", "B"), each = 60),
  treatment = rep(rep(c("control", "treated"), each = 30), 2),
  batch     = factor(rep(c("1", "2", "3"), 40))
 )

# 6. Formula | 3 groups | Facet Grid | Saved to working directory
result <- f_scan(
  weight ~ species + treatment + batch,
  data         = plant_data,
  coef         = 2.0,
  digits       = 2,
  save_in_wdir = TRUE,
  output_type  = "pdf"
)
#> Saving output in: /home/runner/work/rfriend/rfriend/docs/reference/plant_data_fscan_output.pdf
#> Warning: error in running command
#> ! sh: 1: pdflatex: not found
#> Error: LaTeX failed to compile /home/runner/work/rfriend/rfriend/docs/reference/plant_data_fscan_output.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See plant_data_fscan_output.log for more info.
print(result)
#> 
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





```
