# Generate a Boxplot Report of a data.frame

Generates boxplots for all numeric variables in a given dataset, grouped
by factor variables. The function automatically detects numeric and
factor variables. It allows two output formats ('pdf', 'Word') and
includes an option to add a general explanation about interpreting
boxplots.

## Usage

``` r
f_boxplot(
  data = NULL,
  formula = NULL,
  fancy_names = NULL,
  output_type = "pdf",
  save_as = NULL,
  save_in_wdir = FALSE,
  close_generated_files = FALSE,
  open_generated_files = TRUE,
  boxplot_explanation = TRUE,
  detect_factors = TRUE,
  jitter = FALSE,
  width = 8,
  height = 7,
  units = "in",
  res = 300,
  las = 2
)
```

## Arguments

- data:

  A `data.frame` containing the data to be used for creating boxplots.

- formula:

  A formula specifying the factor to be plotted. More response variables
  can be added using `-` or `+` (e.g.,
  `response1 + response2 ~ predictor`) to generate multiple boxplots. If
  the formula is omitted and only `data` is provided all data will be
  used for creating boxplots.

- fancy_names:

  An optional named vector mapping column names in `data` to more
  readable names for display in plots (name map). Defaults to `NULL`.

- output_type:

  Character string, specifying the output format: `"pdf"`, `"word"`,
  `"rmd"` or `"png"`. The option `"rmd"` saves rmd code in the output
  object not in a file. Default is `"pdf"`.

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

  Logical. If `TRUE`, closes open 'Word' files depending on the output
  format. This to be able to save the newly generated files. 'Pdf' files
  should also be closed before using the function and cannot be
  automatically closed.

- open_generated_files:

  Logical. If `TRUE`, Opens the generated output files ('pdf', 'Word' or
  'png') files depending on the output format. This to directly view the
  results after creation. Files are stored in tempdir(). Default is
  `TRUE`.

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

## Value

Generates a report file ('pdf' or 'Word') with boxplots and, optionally,
opens it with the default program. Returns NULL (no R object) when
generating 'pdf' or 'Word' files. Can also return R Markdown code or
'PNG' files depending on the output format.

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
that the binary’s location is in your PATH.

**Linux:** Install Pandoc through your distribution’s package manager
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
           output_type = "word",
           open_generated_files = FALSE
           )
#> Saving output in: /tmp/RtmpDUIw9V/iris_BoxPlot.docx

# Use a formula to plot several response parameters (response 1 + response 2 etc)
# and generate a rmd output without boxplot_explanation.
data(mtcars)
f_boxplot(hp + disp ~ gear*cyl,
           data=mtcars,
           boxplot_explanation = FALSE,
           output_type = "word",
           open_generated_files = FALSE) # Do not automatically open the 'Word' file.
#> Saving output in: /tmp/RtmpDUIw9V/mtcars_BoxPlot.docx
# }
```
