% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fbestnormalize.R
\name{plot.f_bestNormalize}
\alias{plot.f_bestNormalize}
\title{Plot an f_bestNormalize object}
\usage{
\method{plot}{f_bestNormalize}(x, which = 1:2, ask = FALSE, ...)
}
\arguments{
\item{x}{An object of class \code{f_bestNormalize}.}

\item{which}{Integer determining which graph to plot. Default is \code{1:2}.}

\item{ask}{Logical. \code{TRUE} waits with plotting each graph until <Return> is pressed. Default is \code{FALSE}.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
This function is called for its side effect of generating plots
  and does not return a useful value. It invisibly returns `NULL`.
}
\description{
Plots diagnostics for an object of class \code{f_bestNormalize}.
}
\details{
Plot method for f_bestNormalize objects
}
