# Correlation Plots with Factor Detection and Customization

Creates correlation plots for numeric variables in a data frame,
optionally incorporating factors for coloring and shaping points. It
supports automatic detection of factors, customization of plot
aesthetics, and the generation of separate legend files.

## Usage

``` r
f_corplot(
  data,
  detect_factors = TRUE,
  factor_table = FALSE,
  color_factor = "auto",
  shape_factor = "auto",
  print_legend = TRUE,
  fancy_names = NULL,
  width = 15,
  height = 15,
  res = 600,
  pointsize = 8,
  close_generated_files = FALSE,
  open_generated_files = TRUE,
  output_type = "word",
  save_as = NULL,
  save_in_wdir = FALSE
)
```

## Arguments

- data:

  A `data.frame` containing the dataset to be visualized. Must include
  at least two numeric variables.

- detect_factors:

  Logical. If `TRUE`, the function automatically detects factor
  variables in the dataset for coloring and shaping points. Defaults to
  `TRUE`.

- factor_table:

  Logical. If `TRUE`, prints a detailed table about the properties of
  the converted factors to the console. Default is FALSE, so no property
  table will be printed to the console.

- color_factor:

  Character. The name of the factor variable to use for point colors. If
  set to `"auto"`, it is automatically determined based on detected
  factors. Defaults to `"auto"`.

- shape_factor:

  Character. The name of the factor variable to use for point shapes. If
  set to `"auto"`, it is automatically determined based on detected
  factors. Defaults to `"auto"`.

- print_legend:

  Logical. If `TRUE`, a separate legend file is created and displayed.
  Defaults to `TRUE`.

- fancy_names:

  Named character vector or `NULL`. Optional mapping of column names to
  more readable names for display in plots and legends.

- width:

  Numeric. The width of the output plot in centimeters (default 15 cm).

- height:

  Numeric. The height of the output plot in centimeters (default 15 cm).

- res:

  Numeric. The resolution (in dots per inch) for the output plot image
  (defaults 1000 dpi).

- pointsize:

  Numeric. The base font size for text in the plot image. Defaults to 8.

- close_generated_files:

  Logical. If `TRUE`, closes open 'Word' files depending on the output
  format. This to be able to save the newly generated files. 'Pdf' files
  should also be closed before using the function and cannot be
  automatically closed. Default is `FALSE`.

- open_generated_files:

  Logical. If `TRUE`, Opens the generated 'Word' output files. This to
  directly view the results after creation. Files are stored in
  tempdir(). Default is `TRUE`.

- output_type:

  Character string specifying the output format: "pdf", "word", "png" or
  "rmd". Default is "word".

- save_as:

  Character string specifying the output file path (without extension).
  If a full path is provided, output is saved to that location. If only
  a filename is given, the file is saved in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). If only a
  directory is specified (providing an existing directory with trailing
  slash), the file is named "dataname_CorPlot" in that directory. If an
  extension is provided the output format specified with option
  "output_type" will be overruled. Defaults to
  `file.path(tempdir(), "dataname_CorPlot.docx")`.

- save_in_wdir:

  Logical. If `TRUE`, saves the file in the working directory. Default
  is `FALSE`, this avoid unintended changes to the global environment.
  If `save_as` location is specified `save_in_wdir` is overwritten by
  `save_as`.

## Value

Output is a 'Word' document with:

- A correlation plot.

- A legend if applicable.

Using the option "output_type", it can also generate output in the form
of: R Markdown code, 'pdf', or 'PNG' files. No value is returned to the
R environment; instead, files are saved, and they are opened
automatically if running on Windows.

## Details

- Factor Detection: If `detect_factors` is enabled, up to two factors
  are automatically detected from the dataset and used for coloring
  (`color_factor`) and shaping (`shape_factor`) points in the plot.

- Customization: Users can manually specify which factors to use by
  setting `color_factor` and/or (`shape_factor`). Non-factor variables
  are converted into factors automatically, with a message indicating
  this conversion.

- Legend Generation: A separate legend file is created when factors are
  used or if `print_legend` is explicitly set to `TRUE`.

The function uses numeric variables in the dataset for scatterplots and
computes Pearson correlations displayed in the upper triangle of the
correlation matrix.

This function requires
\[Pandoc\](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3
or higher), a universal document converter.

- **Windows:** Install Pandoc and ensure the installation folder  
  (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your
  system PATH.

- **macOS:** If using Homebrew, Pandoc is typically installed in
  "/usr/local/bin". Alternatively, download the .pkg installer and
  verify that the binary’s location is in your PATH.

- **Linux:** Install Pandoc through your distribution’s package manager
  (commonly installed in "/usr/bin" or "/usr/local/bin") or manually,
  and ensure the directory containing Pandoc is in your PATH.

- If Pandoc is not found, this function may not work as intended.

## Note

- At least two numeric variables are required in the dataset; otherwise,
  an error is thrown.

- If more than two factors are detected, only the first two are used
  with a warning message.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Example usage:
data("mtcars")

mtcars_sub <- subset(mtcars, select = -c(am, qsec, vs))
# Customizing factors:
f_corplot(mtcars_sub,
           shape_factor = "cyl",
           color_factor = "gear",
           output_type = "png",
           open_generated_files = FALSE
           )
#> 
#> Variable: gear was converted to a factor
#> Variable: cyl was converted to a factor  
#> Saving output in: /tmp/RtmpDUIw9V/mtcars_sub_CorPlot.png and /tmp/RtmpDUIw9V/mtcars_sub_Legend.png


# Output to MS Word and add fancy column names, only adjusting two of the four variable names.
data(iris)
fancy_names <- c(Sepal.Length = "Sepal Length (cm)", Sepal.Width = "Sepal Width (cm)")
f_corplot(iris,
           fancy_names = fancy_names,
           output_type = "word",
           open_generated_files = FALSE
           )
#> Saving output in: /tmp/RtmpDUIw9V/iris_CorPlot.docx

```
