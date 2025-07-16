#' Normal Q-Q Plot with Confidence Bands
#'
#' This function creates a normal Q-Q plot for a given numeric vector and adds confidence bands to visualize the variability of the quantiles.
#'
#' @param x A numeric vector of data values.
#' @param main A character string specifying the title of the histogram. Default is "Histogram with Normal Curve".
#' @param conf_level Numeric, between 0 and 1. Confidence level for the confidence bands. Default is 0.95 (95\% confidence).
#' @param cex Numeric, optional parameter for graph cex with default \code{cex = 0.6}.
#' @param pch Numeric, optional parameter shape of points default \code{pch = 19}.
#' @param col Numeric, optional parameter for color of point with default 'black'.
#' @param save_png A logical value default \code{FALSE}, if \code{TRUE} a png file is saved under the name of the data of under the specified file name.
#' @param open_png Logical. If \code{TRUE}, opens generated png files.
#' @param output_file Character string specifying the name of the output file (without extension). Default is the name of the vector or dataframe followed by "_histogram.png".
#' @param output_dir Character string specifying the name of the directory of the output file. Default is  \code{tempdir()}. If the \code{output_file} already contains a directory name \code{output_dir} can be omitted, if used it overwrites the dir specified in \code{output_file}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory Default is \code{FALSE}, to avoid unintended changes to the global environment. If the \code{output_dir} is specified \code{save_in_wdir} is overwritten with \code{output_dir}.
#' @param width Numeric, png figure width default \code{8} inch.
#' @param height Numeric, png figure height default \code{7} inch.
#' @param units Numeric, png figure units default inch.
#' @param res Numeric, png figure resolution default \code{300} dpi.
#' @param ylab A character string specifying the y-axsis label. Default name is  \code{"Quantiles of: data_name"}.
#' @param ... Additional graphical parameters to be passed to the \code{qqnorm} function.
#'
#' @details
#' The function calculates theoretical quantiles for a normal distribution and compares them with the sample quantiles of the input data.
#'
#' It also computes confidence intervals for the order statistics using the Blom approximation and displays these intervals as shaded bands on the plot.
#'
#' The reference line is fitted based on the first and third quartiles of both the sample data and theoretical quantiles.
#'
#' To increase resolution you can use \code{png(...,res = 600)} or the 'RStudio' chunck setting, e.g. \code{dpi = 600}.
#'
#' @return A Q-Q plot is created and the function returns this as a \code{recordedplot}.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # Generate random normal data
#' set.seed(123)
#' data <- rnorm(100)
#'
#' # Create a Q-Q plot with confidence bands
#' f_qqnorm(data)
#'
#' # Customize the plot with additional graphical parameters
#' f_qqnorm(data, conf_level = 0.99, pch = 16, col = "blue")
#'
#' @export

f_qqnorm <- function(x,
                    main = NULL,
                    ylab = NULL,
                    conf_level = 0.95,
                    col = NULL,
                    pch = NULL,
                    cex = NULL,
                    save_png = FALSE,
                    open_png = TRUE,
                    output_file = NULL, #Specify the name of the output dir to save the file in.
                    output_dir = NULL,  #Save file output in the working directory.
                    save_in_wdir = FALSE,
                    width = 8,
                    height = 7,
                    units = "in",
                    res = 300,
                    ...
                    ){

  # Capture the name of the input variable
  data_name <- deparse(substitute(x))
  data_name <- sub(".*\\$", "", data_name)  # Remove everything before the "$" symbol

  # Default values if not supplied by user
  if (is.null(col)) col <- "black"
  if (is.null(cex)) cex <- 0.6
  if (is.null(pch)) pch <- 19
  if (is.null(main)) main <- paste0("Normal Q-Q Plot with ", conf_level*100, "% Confidence Bands")
  if (is.null(ylab)) ylab <- paste0("Quantiles of: ", data_name)  # Set y-axis label if not provided

  # Calculate theoretical quantiles
  n <- length(x)
  p <- ppoints(n)
  theoretical_q <- qnorm(p)

  # Parameters for the reference line
  q1_x <- quantile(x, 0.25)
  q3_x <- quantile(x, 0.75)
  q1_t <- qnorm(0.25)
  q3_t <- qnorm(0.75)
  slope <- (q3_x - q1_x) / (q3_t - q1_t)
  intercept <- q1_x - slope * q1_t


  # Calculate standard errors for order statistics
  # Using Blom approximation
  alpha <- 0.375 # Blom constant
  p_i <- (1:n - alpha) / (n - 2 * alpha + 1)
  phi_i <- dnorm(qnorm(p_i))
  se <- slope * sqrt(p_i * (1 - p_i) / (n * (phi_i^2)))

  # Calculate confidence intervals
  z <- qnorm(1 - (1 - conf_level) / 2)
  fitted <- slope * theoretical_q + intercept
  ci_lower <- fitted - z * se
  ci_upper <- fitted + z * se

  # Create the plot
  qqnorm(
    x,
    ylab = ylab,
    main = main,
    col = col,
    pch = pch,
    cex = cex,
    ...
  )

  # Add shaded confidence bands using polygon
  polygon(c(theoretical_q, rev(theoretical_q)),
          c(ci_lower, rev(ci_upper)),
          col = rgb(0.5, 0.5, 0.5, 0.3),  # Gray with 30% opacity
          border = "darkgray")  # darkgray border for the polygon

  # Add Q-Q line and points on top of shading
  qqline(x, col = "red", lwd = 2)
  points(theoretical_q, sort(x), col = col, pch = pch, cex = cex,...)
  saved_plot <- recordPlot()

  if (save_png == TRUE){

    if(is.null(output_file)){
      # Define the new file name
      output_file <- paste0(data_name, "_QQplot.png")
    }

    # If there is no output_dir specified and user setting is to save in working directory
    if(is.null(output_dir) && save_in_wdir == TRUE){
      # set the working dir to the location the file is saved
      output_dir <- getwd()

    } else if(is.null(output_dir) && save_in_wdir == FALSE){
      # Get the dirname of output_file
      output_dir <- dirname(output_file)

      # Check if there is a dir (path) in the output file, if not use tempdir()
      if(output_dir == "."){
        output_dir <- tempdir()
      }

    }

    # Stop if the output directory does not exist
    if (!dir_exists(output_dir)) {
      stop("The directory '", output_dir, "' does not exist.")
    }


    png(
      paste0(output_dir, "/", output_file),
      width = width,
      height = height,
      units = units,
      res = res
    )

    replayPlot(saved_plot)

    # Close the png file
    dev.off()

    if (open_png == TRUE){
      f_open_file(paste0(output_dir, "/",output_file))
    }
  }



  return(invisible(saved_plot))
}
