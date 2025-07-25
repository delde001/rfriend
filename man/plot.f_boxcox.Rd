% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fboxcox.R
\name{plot.f_boxcox}
\alias{plot.f_boxcox}
\title{Plot an f_boxcox object}
\usage{
\method{plot}{f_boxcox}(x, which = 1:3, ask = FALSE, ...)
}
\arguments{
\item{x}{An object of class \code{f_boxcox}.}

\item{which}{Integer determining which graph to plot. Default is \code{1:2}.}

\item{ask}{Logical. \code{TRUE} waits with plotting each graph until <Return> is pressed. Default is \code{FALSE}.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
This function is called for its side effect of generating plots
and does not return a useful value. It invisibly returns \code{1}.
}
\description{
Create diagnostic plots of an object of class \code{f_boxcox}.
}
\details{
Plot method for f_boxcox objects
}
