% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fqqnorm.R
\name{f_qqnorm}
\alias{f_qqnorm}
\title{Normal Q-Q Plot with Confidence Bands}
\usage{
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
  output_file = NULL,
  output_dir = NULL,
  save_in_wdir = FALSE,
  width = 8,
  height = 7,
  units = "in",
  res = 300,
  ...
)
}
\arguments{
\item{x}{A numeric vector of data values.}

\item{main}{A character string specifying the title of the histogram. Default is "Histogram with Normal Curve".}

\item{ylab}{A character string specifying the y-axsis label. Default name is  \code{"Quantiles of: data_name"}.}

\item{conf_level}{Numeric, between 0 and 1. Confidence level for the confidence bands. Default is 0.95 (95\% confidence).}

\item{col}{Numeric, optional parameter for color of point with default 'black'.}

\item{pch}{Numeric, optional parameter shape of points default \code{pch = 19}.}

\item{cex}{Numeric, optional parameter for graph cex with default \code{cex = 0.6}.}

\item{save_png}{A logical value default \code{FALSE}, if \code{TRUE} a png file is saved under the name of the data of under the specified file name.}

\item{open_png}{Logical. If \code{TRUE}, opens generated png files.}

\item{output_file}{Character string specifying the name of the output file (without extension). Default is the name of the vector or dataframe followed by "_histogram.png".}

\item{output_dir}{Character string specifying the name of the directory of the output file. Default is  \code{tempdir()}. If the \code{output_file} already contains a directory name \code{output_dir} can be omitted, if used it overwrites the dir specified in \code{output_file}.}

\item{save_in_wdir}{Logical. If \code{TRUE}, saves the file in the working directory Default is \code{FALSE}, to avoid unintended changes to the global environment. If the \code{output_dir} is specified \code{save_in_wdir} is overwritten with \code{output_dir}.}

\item{width}{Numeric, png figure width default \code{8} inch.}

\item{height}{Numeric, png figure height default \code{7} inch.}

\item{units}{Numeric, png figure units default inch.}

\item{res}{Numeric, png figure resolution default \code{300} dpi.}

\item{...}{Additional graphical parameters to be passed to the \code{qqnorm} function.}
}
\value{
A Q-Q plot is created and the function returns this as a \code{recordedplot}.
}
\description{
This function creates a normal Q-Q plot for a given numeric vector and adds confidence bands to visualize the variability of the quantiles.
}
\details{
The function calculates theoretical quantiles for a normal distribution and compares them with the sample quantiles of the input data.

It also computes confidence intervals for the order statistics using the Blom approximation and displays these intervals as shaded bands on the plot.

The reference line is fitted based on the first and third quartiles of both the sample data and theoretical quantiles.

To increase resolution you can use \code{png(...,res = 600)} or the 'RStudio' chunck setting, e.g. \code{dpi = 600}.
}
\examples{
# Generate random normal data
set.seed(123)
data <- rnorm(100)

# Create a Q-Q plot with confidence bands
f_qqnorm(data)

# Customize the plot with additional graphical parameters
f_qqnorm(data, conf_level = 0.99, pch = 16, col = "blue")

}
\author{
Sander H. van Delden  \email{plantmind@proton.me} \cr
}
