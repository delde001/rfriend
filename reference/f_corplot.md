# Correlation Plots with Factor Detection and Multiple Correlation Coefficients

Creates correlation plots for numeric variables in a data frame. The
upper triangle displays Pearson \\r\\, Spearman \\\rho\\, and Kendall
\\\tau\\ simultaneously for each pair. Factor variables are
automatically detected and used for grouping, i.e. point colouring and
shaping. Ordinal variables are supported via `ordinal_vars`: their
diagonal labels are italicised and Pearson \\r\\ is greyed and bracketed
for any pair that involves them.A separate legend file documents both
the grouping factors and the meaning of all three correlation symbols.

## Usage

``` r
f_corplot(
  data,
  detect_factors = TRUE,
  factor_table = FALSE,
  factor_exclude = NULL,
  factor_select = NULL,
  unique_num_treshold = 8,
  repeats_threshold = 2,
  color_factor = "auto",
  shape_factor = "auto",
  print_legend = TRUE,
  fancy_names = NULL,
  ordinal_vars = NULL,
  width = 15,
  height = 15,
  res = 600,
  pointsize = 10,
  close_generated_files = FALSE,
  open_generated_files = interactive(),
  output_type = "word",
  save_as = NULL,
  save_in_wdir = FALSE
)
```

## Arguments

- data:

  A `data.frame` containing the dataset. Must include at least two
  numeric variables.

- detect_factors:

  Logical. If `TRUE`, factor variables are automatically detected for
  colouring and shaping points. Default `TRUE`.

- factor_table:

  Logical. If `TRUE`, prints a detailed table of converted factors to
  the console. Default `FALSE`.

- factor_exclude:

  A character vector specifying the names of the columns NOT to convert
  into factors. If `NULL`, no columns are excluded. Default is `NULL`.

- factor_select:

  A character vector specifying the names of the columns to convert into
  factors. If `NULL`, the function automatically detects columns that
  should be factors based on their data type and unique value count.
  Default is `NULL`.

- unique_num_treshold:

  Numeric. A threshold of the amount of unique numbers a numeric column
  should have to keep it numeric, i.e. omit factor conversion. Default
  `8`.

- repeats_threshold:

  Numeric. A threshold of the minimal number of repeats a numeric column
  should have to convert it to a factor. Default `2`.

- color_factor:

  Character. Name of the factor variable used for point colours;
  `"auto"` selects automatically. Default `"auto"`.

- shape_factor:

  Character. Name of the factor variable used for point shapes; `"auto"`
  selects automatically. Default `"auto"`.

- print_legend:

  Logical. If `TRUE`, a separate legend file is created. Default `TRUE`.

- fancy_names:

  Named character vector or `NULL`. Maps column names to display names
  used in the plot and legend.

- ordinal_vars:

  Character vector or `NULL`. Names of variables to treat as ordinal.
  Ordered factors are coerced to integer ranks; other non-numeric types
  are coerced similarly. Their diagonal labels are italicised and
  Pearson \\r\\ is greyed and bracketed for any pair that involves them.
  Ordinal variables are included in the correlation panels but excluded
  from aesthetic factor detection. Default `NULL`.

- width:

  Numeric. Plot width in centimetres. Default 15.

- height:

  Numeric. Plot height in centimetres. Default 15.

- res:

  Numeric. Resolution in DPI. Default 600.

- pointsize:

  Numeric. Base font size. Default 8.

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

  Character. One of `"pdf"`, `"word"`, `"png"`, or `"rmd"`. Default
  `"word"`.

- save_as:

  Character or `NULL`. Output file path without extension. A full path,
  a filename, or a directory (with trailing slash) are all accepted.
  Providing an extension overrides `output_type`. Default saves to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- save_in_wdir:

  Logical. If `TRUE`, saves to the working directory. Overridden by
  `save_as`. Default `FALSE`.

## Value

No value is returned to the R environment. Output files are saved and
opened automatically.

## Details

- **Three correlations per panel:** Every upper-triangle panel shows
  \\r\\ (Pearson), \\\rho\\ (Spearman), and \\\tau\\ (Kendall) stacked
  vertically, so the reader can choose the most appropriate coefficient
  for each variable pair.

- **Ordinal variables:** Specify column names with `ordinal_vars`. Those
  variables appear in italic on the diagonal. For any pair where at
  least one variable is ordinal, Pearson \\r\\ is shown greyed and in
  parentheses to signal it is technically inappropriate; Spearman and
  Kendall remain prominent.

- **Factor detection:** Only unordered factors are used for colour/shape
  aesthetics. Ordered factors
  ([`is.ordered()`](https://rdrr.io/r/base/factor.html)) are treated as
  ordinal data, not as grouping variables.

- **Legend:** The legend file documents the grouping factor levels (when
  present) and always includes an explanation of all three correlation
  symbols whenever a legend is generated.

- **Constant columns:** Zero-variance columns produce `NA` in all
  correlation panels.

This function requires
\[Pandoc\](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3
or higher), a universal document converter.

- **Windows:** Install Pandoc and ensure the installation folder  
  (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your
  system PATH.

- **macOS:** If using Homebrew, Pandoc is typically installed in
  "/usr/local/bin". Alternatively, download the .pkg installer and
  verify that the binary's location is in your PATH.

- **Linux:** Install Pandoc through your distribution's package manager
  (commonly installed in "/usr/bin" or "/usr/local/bin") or manually,
  and ensure the directory containing Pandoc is in your PATH.

- If Pandoc is not found, this function may not work as intended.

## Author

Sander H. van Delden <plantmind@proton.me>

## Examples

``` r
data(mtcars)
mtcars_sub <- subset(mtcars, select = -c(am, qsec, vs))
f_corplot(mtcars_sub,
          color_factor = "gear",
          shape_factor = "cyl",
          output_type  = "png"
          )
#> 
#> Variable 'gear' was converted to a factor.
#> Variable 'cyl' was converted to a factor.
#> Saving output in: /tmp/RtmpG5HCTF/mtcars_sub_CorPlot.png and /tmp/RtmpG5HCTF/mtcars_sub_Legend.png

# With ordinal variables
data(iris)
fancy_names <- c(Sepal.Length = "Sepal Length (cm)",
                 Sepal.Width  = "Sepal Width (cm)")
f_corplot(iris,
          fancy_names  = fancy_names,
          ordinal_vars = "Petal.Width",
          output_type  = "png",
          open_generated_files = FALSE)
#> Saving output in: /tmp/RtmpG5HCTF/iris_CorPlot.png and /tmp/RtmpG5HCTF/iris_Legend.png
```
