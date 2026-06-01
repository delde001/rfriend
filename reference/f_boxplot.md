# Generate a Boxplot Report of a data.frame

Generates boxplots for all numeric variables in a given dataset, grouped
by factor variables. The function automatically detects numeric and
factor variables. It allows two output formats ('pdf', 'Word') and
includes an option to add a general explanation about interpreting
boxplots.

## Usage

``` r
f_boxplot(x, ...)

# S3 method for class 'formula'
f_boxplot(formula, data = NULL, ...)

# S3 method for class 'data.frame'
f_boxplot(x, ...)

# S3 method for class 'numeric'
f_boxplot(x, ...)

# S3 method for class 'integer'
f_boxplot(x, ...)

f_boxplot_worker(
  formula = NULL,
  data,
  fancy_names = NULL,
  output_type = "pdf",
  outliers = TRUE,
  coef = 1.5,
  limit_columns = 7,
  save_as = NULL,
  save_in_wdir = FALSE,
  close_generated_files = FALSE,
  open_generated_files = interactive(),
  boxplot_explanation = TRUE,
  detect_factors = TRUE,
  jitter = FALSE,
  width = 8,
  height = 7,
  units = "in",
  res = 300,
  las = 2,
  color = "rainbow",
  boxwidth = NULL,
  ...
)
```

## Arguments

- x:

  A data.frame, formula, or numeric/integer vector (dispatches to the
  correct method). When a single numeric or integer vector is supplied,
  it is treated as a single response variable, plotted on the y-axis
  with the variable name as label, and grouped by a single dummy factor
  (one box). When several unnamed numeric vectors are supplied (as in
  base R's [`boxplot()`](https://rdrr.io/r/graphics/boxplot.html), e.g.
  `f_boxplot(x1, x2)`), each becomes its own box side by side, labelled
  with the original variable name on the x-axis.

- ...:

  Further arguments forwarded to `f_boxplot_worker`, such as
  `fancy_names`, `title`, `fill`, etc.

- formula:

  A formula specifying the factor to be plotted. More response variables
  can be added using `-` or `+` (e.g.,
  `response1 + response2 ~ predictor`) to generate multiple boxplots. If
  the formula is omitted and only `data` is provided all data will be
  used for creating boxplots.

- data:

  A `data.frame` containing the data to be used for creating boxplots.

- fancy_names:

  An optional named vector mapping column names in `data` to more
  readable names for display in plots (name map). Defaults to `NULL`.

- output_type:

  Character string, specifying the output format: `"pdf"`, `"word"`,
  `"rmd"` or `"png"`. The option `"rmd"` saves rmd code in the output
  object not in a file. Default is `"pdf"`.

- outliers:

  Logical. If `TRUE`, scans for outliers using Tukey's fences and if
  they exist, adds them to the report using `f_outliers`. Default
  `TRUE`.

- coef:

  Numeric. The multiplier for the Interquartile Range (IQR) used for
  outlier detection. Default `1.5`.

- limit_columns:

  Integer or `NULL`. Defines the number of columns shown in the outlier
  table. Default = `7`. `NULL` = all columns are shown.

- save_as:

  Character string specifying the output file path (without extension).
  If a full path is provided, output is saved to that location. If only
  a filename is given, the file is saved in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). If only a
  directory is specified (providing an existing directory with trailing
  slash), the file is named "dataname_BoxPlot" in that directory. If an
  extension is provided the output format specified with option
  "output_type" will be overruled. Defaults to
  `file.path(tempdir(), "dataname_BoxPlot.pdf")`.

- save_in_wdir:

  Logical. If `TRUE`, saves the file in the working directory. Default
  is `FALSE`, this avoid unintended changes to the global environment.
  If `save_as` location is specified `save_in_wdir` is overwritten by
  `save_as`.

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

- boxplot_explanation:

  A logical value indicating whether to include an explanation of how to
  interpret boxplots in the report. Defaults to `TRUE`.

- detect_factors:

  A logical value indicating whether to automatically detect factor
  variables in the dataset. Defaults to `TRUE`.

- jitter:

  A logical value, if `TRUE` all data per boxplot is shown, if `FALSE`
  (default) individual data points (except for outliers) are omitted.

- width:

  Numeric, png figure width default `8` inch

- height:

  Numeric, png figure height default `7` inch

- units:

  Character string, png figure units default `"in"` = inch, other
  options are: `"px"` = Pixels, `"cm"` = centimeters, `"mm"` =
  millimeters.

- res:

  Numeric, png figure resolution default 300 dpi

- las:

  An integer (`0` t/m `3`), `las = 0`: Axis labels are parallel to the
  axis. `las = 1`: Axis labels are always horizontal. `las = 2`: Axis
  labels are perpendicular to the axis. (default setting). `las = 3`:
  Axis labels are always vertical.

- color:

  Colour scheme for the boxes. One of: `"rainbow"` (default; one hue per
  group), `"bw"` (white fill with black lines, outliers and mean marker,
  suitable for publication), a single R colour name or hex string
  applied to all boxes (with a transparent fill and a darker outline
  derived from it), or a vector of colours which is recycled to the
  number of groups for a custom per-group palette.

- boxwidth:

  Numeric or `NULL`. Relative width of each box, passed as `boxwex` to
  [`boxplot()`](https://rdrr.io/r/graphics/boxplot.html). When `NULL`
  (default) the width is computed automatically as `num.bars/18`,
  keeping boxes roughly comparable across plots with different numbers
  of groups. Supply a numeric value (for example `0.5`) to override. The
  numeric/integer vector method uses `0.4` by default to avoid an overly
  thin single box.

## Value

The return value depends on `output_type`:

- `"pdf"` and `"word"`: Writes a report file to `save_as` (or
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) by default) and
  returns `NULL` invisibly. The file can optionally be opened with
  `open_generated_files = TRUE`.

- `"png"`: Writes one PNG file per response x factor combination into
  the directory given by `save_as` and returns `NULL` invisibly.

- `"rmd"`: Returns the generated R Markdown content as a single
  character string (invisibly). No file is written and nothing is
  printed to the console. The caller can
  [`cat()`](https://rdrr.io/r/base/cat.html) the string, assign it to a
  variable, or embed it in a larger report (see Examples).

## Details

The function performs the following steps:

- Detects numeric and factor variables in the dataset.

- Generates boxplots for each numeric variable grouped by each factor
  variable.

- Outputs the report in the specified format ('pdf', 'Word' or 'Rmd').

If `output_type = "rmd"` is used it is adviced to use it in a chunk with
{r, echo=FALSE, results='asis'}

If no factor variables are detected, the function stops with an error
message since factors are required for creating boxplots.

This function will plot all numeric and factor candidates, use the
function [`subset()`](https://rdrr.io/r/base/subset.html) to prepare a
selection of columns before submitting to `f_boxplot()`.

Note that there is an optional `jitter` option to plot all individual
data points over the boxplots.

This function requires
\[Pandoc\](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3
or higher), a universal document converter.

**Windows:** Install Pandoc and ensure the installation folder  
(e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your
system PATH.

**macOS:** If using Homebrew, Pandoc is typically installed in
"/usr/local/bin". Alternatively, download the .pkg installer and verify
that the binary's location is in your PATH.

**Linux:** Install Pandoc through your distribution's package manager
(commonly installed in "/usr/bin" or "/usr/local/bin") or manually, and
ensure the directory containing Pandoc is in your PATH.

If Pandoc is not found, this function may not work as intended.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# \donttest{
# Example usage:
data(iris)

new_names = c(
  "Sepal.Length" = "Sepal length (cm)" ,
  "Sepal.Width" = "Sepal width (cm)",
  "Petal.Length" = "Petal length (cm)",
  "Petal.Width" = "Petal width (cm)",
  "Species" = "Cultivar"
)

# Use the whole data.frame to generate an MS Word report and don't open it.
f_boxplot(iris,
           fancy_names = new_names,
           output_type = "word"
           )
#> Saving output in: /tmp/RtmpG5HCTF/data_BoxPlot.docx

# Use a formula to plot several response parameters (response 1 + response 2 etc)
# and generate a rmd output without boxplot_explanation.
data(mtcars)
f_boxplot(hp + disp ~ gear*cyl,
           data=mtcars,
           boxplot_explanation = FALSE,
           output_type = "word"
           )
#> Saving output in: /tmp/RtmpG5HCTF/mtcars_BoxPlot.docx

# Pass a bare numeric vector. Its name is used as the y-axis label
# and as the data_name in the output filename.
set.seed(1)
my_vec <- rnorm(50, mean = 10)
f_boxplot(my_vec, output_type = "png")
#> PNG files saved in: /tmp/RtmpG5HCTF
#>    

# Formula with bare vectors (no data.frame): group hp by cyl.
hp1  <- mtcars$hp
cyl1 <- mtcars$cyl
f_boxplot(hp1 ~ cyl1, output_type = "png")
#> PNG files saved in: /tmp/RtmpG5HCTF
#>    

# Multiple unnamed numeric vectors, base R's boxplot() convention:
# each vector becomes its own box, labelled on the x-axis with its
# original variable name. Use the formula syntax above when you
# instead want to group one response by a factor.
f_boxplot(hp1, cyl1, output_type = "png")
#> PNG files saved in: /tmp/RtmpG5HCTF
#>    

# Capture the R Markdown output as a string and render it inline.
# Use output_type = "rmd" to get the markdown back as a character value
# instead of writing a file. Useful for embedding in a larger knitr document.
rmd <- f_boxplot(iris,
                 output_type         = "rmd",
                 boxplot_explanation = FALSE,
                 outliers            = FALSE
                 )

# Display it in the console
cat(rmd)
#>   
#>   
#> #  Boxplots of:  Sepal.Length   
#> ##  Boxplot of:  Sepal.Length  as function of  Species   
#>    
#> ![](/tmp/RtmpG5HCTF/file1d94655b6cb1.png)    
#>   
#>   
#>   
#> #  Boxplots of:  Sepal.Width   
#> ##  Boxplot of:  Sepal.Width  as function of  Species   
#>    
#> ![](/tmp/RtmpG5HCTF/file1d9445cede05.png)    
#>   
#>   
#>   
#> #  Boxplots of:  Petal.Length   
#> ##  Boxplot of:  Petal.Length  as function of  Species   
#>    
#> ![](/tmp/RtmpG5HCTF/file1d9445f0bbb0.png)    
#>   
#>   
#>   
#> #  Boxplots of:  Petal.Width   
#> ##  Boxplot of:  Petal.Width  as function of  Species   
#>    
#> ![](/tmp/RtmpG5HCTF/file1d946a2069ab.png)    
#>   

# ...or splice it into a knitr child chunk with results = "asis":
#   ```{r, echo=FALSE, results='asis'}
#   cat(rmd)
#   ```


# }
```
