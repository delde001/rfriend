# Plot a Histogram with an Overlaid Normal Curve

This function creates a histogram of the provided data and overlays it
with a normal distribution curve.

## Usage

``` r
f_hist(
  data,
  main = NULL,
  xlab = NULL,
  probability = TRUE,
  col = "white",
  border = "black",
  line_col = "red",
  save_png = FALSE,
  open_png = TRUE,
  save_as = NULL,
  save_in_wdir = FALSE,
  width = 8,
  height = 7,
  units = "in",
  res = 300,
  ...
)
```

## Arguments

- data:

  A numeric vector of data values to be plotted.

- main:

  A character string specifying the title of the histogram. Default is
  `"Histogram with Normal Curve"`.

- xlab:

  A character string specifying the label for the x-axis. Default is the
  name of the data variable.

- probability:

  A logical value indicating whether to plot a probability or frequency
  histogram. Default is `TRUE`.

- col:

  A character string specifying the fill color of the histogram bars.
  Default is `"white"`.

- border:

  A character string specifying the color of the histogram bar borders.
  Default is `"black"`.

- line_col:

  A character string specifying the color of the normal curve line.
  Default is `"red"`.

- save_png:

  A logical value default `FALSE`, if `TRUE` a png file is saved under
  the name of the data of under the specified file name.

- open_png:

  Logical. If `TRUE`, opens generated png files.

- save_as:

  Character string specifying the output file path (without extension).
  If a full path is provided, output is saved to that location. If only
  a filename is given, the file is saved in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). If only a
  directory is specified (providing an existing directory with trailing
  slash), the file is named "data_name_histogram.png" in that directory.
  Defaults to `file.path(tempdir(), "data_name_histogram.png")`.

- save_in_wdir:

  Logical. If `TRUE`, saves the file in the working directory. Default
  is `FALSE`, this avoid unintended changes to the global environment.
  If `save_as` location is specified `save_in_wdir` is overwritten by
  `save_as`.

- width:

  Numeric, png figure width default `8` inch.

- height:

  Numeric, png figure height default `7` inch.

- units:

  Character string, png figure units default `"in"` = inch, other
  options are: `"px"` = Pixels, `"cm"` centimeters, `"mm"` millimeters.

- res:

  Numeric, png figure resolution default `300` dpi.

- ...:

  Additional arguments to be passed to the `hist` function.

## Value

A histogram plot is created and the function returns this as a
`recordedplot`.

## Details

The function first captures the name of the input variable for labeling
purposes. It then calculates a sequence of x-values and corresponding
y-values for a normal distribution based on the mean and standard
deviation of the data. The histogram is plotted with specified
aesthetics, and a normal curve is overlaid. To increase resolution you
can use `png(...,res = 600)` or the 'RStudio' chunk setting, e.g.
`dpi=600`.

## See also

[`hist`](https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/hist.html),
[`dnorm`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Normal.html),
[`lines`](https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/lines.html)

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Example usage:
set.seed(123)
sample_data <- rnorm(100)
f_hist(sample_data)


```
