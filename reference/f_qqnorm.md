# Normal Q-Q Plot with Confidence Bands

This function creates a normal Q-Q plot for a given numeric vector and
adds confidence bands to visualize the variability of the quantiles.

## Usage

``` r
f_qqnorm(
  x,
  main = NULL,
  ylab = NULL,
  conf_level = 0.95,
  col = NULL,
  pch = NULL,
  cex = NULL,
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

- x:

  A numeric vector of data values.

- main:

  A character string specifying the title of the histogram. Default is
  "Histogram with Normal Curve".

- ylab:

  A character string specifying the y-axsis label. Default name is
  `"Quantiles of: data_name"`.

- conf_level:

  Numeric, between 0 and 1. Confidence level for the confidence bands.
  Default is 0.95 (95% confidence).

- col:

  Numeric, optional parameter for color of point with default 'black'.

- pch:

  Numeric, optional parameter shape of points default `pch = 19`.

- cex:

  Numeric, optional parameter for graph cex with default `cex = 0.6`.

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
  slash), the file is named "data_name_QQplot.png" in that directory.
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

  Numeric, png figure units default inch.

- res:

  Numeric, png figure resolution default `300` dpi.

- ...:

  Additional graphical parameters to be passed to the `qqnorm` function.

## Value

A Q-Q plot is created and the function returns this as a `recordedplot`.

## Details

The function calculates theoretical quantiles for a normal distribution
and compares them with the sample quantiles of the input data.

It also computes confidence intervals for the order statistics using the
Blom approximation and displays these intervals as shaded bands on the
plot.

The reference line is fitted based on the first and third quartiles of
both the sample data and theoretical quantiles.

To increase resolution you can use `png(...,res = 600)` or the 'RStudio'
chunck setting, e.g. `dpi = 600`.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Generate random normal data
set.seed(123)
data <- rnorm(100)

# Create a Q-Q plot with confidence bands
f_qqnorm(data)


# Customize the plot with additional graphical parameters
f_qqnorm(data, conf_level = 0.99, pch = 16, col = "blue")

```
