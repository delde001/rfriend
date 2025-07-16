#' Plot a Histogram with an Overlaid Normal Curve
#'
#' This function creates a histogram of the provided data and overlays it with a normal distribution curve.
#'
#' @param data A numeric vector of data values to be plotted.
#' @param main A character string specifying the title of the histogram. Default is \code{"Histogram with Normal Curve"}.
#' @param xlab A character string specifying the label for the x-axis. Default is the name of the data variable.
#' @param probability A logical value indicating whether to plot a probability or frequency histogram. Default is \code{TRUE}.
#' @param col A character string specifying the fill color of the histogram bars. Default is \code{"white"}.
#' @param border A character string specifying the color of the histogram bar borders. Default is \code{"black"}.
#' @param line_col A character string specifying the color of the normal curve line. Default is \code{"red"}.
#' @param save_png A logical value default \code{FALSE}, if \code{TRUE} a png file is saved under the name of the data of under the specified file name.
#' @param open_png Logical. If \code{TRUE}, opens generated png files.
#' @param output_file Character string specifying the name of the output file (without extension). Default is the name of the vector or dataframe followed by "_histogram.png".
#' @param output_dir Character string specifying the name of the directory of the output file. Default is  \code{tempdir()}. If the \code{output_file} already contains a directory name \code{output_dir} can be omitted, if used it overwrites the dir specified in \code{output_file}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory Default is \code{FALSE}, to avoid unintended changes to the global environment. If the \code{output_dir} is specified \code{save_in_wdir} is overwritten with \code{output_dir}.
#' @param width Numeric, png figure width default \code{8} inch.
#' @param height Numeric, png figure height default \code{7} inch.
#' @param units Character string, png figure units default \code{"in"} = inch, other options are: \code{"px"} = Pixels, \code{"cm"} centimeters, \code{"mm"} millimeters.
#' @param res Numeric, png figure resolution default \code{300} dpi.
#' @param ... Additional arguments to be passed to the \code{hist} function.
#'
#' @details
#' The function first captures the name of the input variable for labeling purposes. It then calculates a sequence of x-values and corresponding y-values for a normal distribution based on the mean and standard deviation of the data. The histogram is plotted with specified aesthetics, and a normal curve is overlaid. To increase resolution you can use \code{png(...,res = 600)} or the 'RStudio' chunk setting, e.g. \code{dpi=600}.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @return A histogram plot is created and the function returns this as a \code{recordedplot}.
#'
#'
#' @examples
#' # Example usage:
#' set.seed(123)
#' sample_data <- rnorm(100)
#' f_hist(sample_data)
#'
#'
#' @seealso
#'  \href{https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/hist.html}{\code{hist}}, \href{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Normal.html}{\code{dnorm}},  \href{https://stat.ethz.ch/R-manual/R-devel/library/graphics/html/lines.html}{\code{lines}}
#'
#'
#' @export
f_hist <- function(data,
                  main = NULL,
                  xlab = NULL,
                  probability = TRUE,
                  col = "white",
                  border = "black",
                  line_col = "red",
                  #ouput png settings
                  save_png = FALSE,
                  open_png = TRUE,
                  output_file = NULL, #Specify the name of the output dir to save the file in.
                  output_dir = NULL,  #Save file output in the working directory.
                  save_in_wdir = FALSE,
                  width = 8,
                  height = 7,
                  units = "in",
                  res = 300,
                  ...) {
  # Capture the name of the input variable
  data_name <- deparse(substitute(data))
  data_name <- sub(".*\\$", "", data_name)  # Remove everything before the "$" symbol

  if (is.null(xlab)) xlab <- data_name  # Set x-axis label if not provided
  if (is.null(main)) main <- "Histogram with Normal Curve"

  # Define x values for the normal curve
  x_values <- seq(min(data), max(data), length = 100)

  # Calculate y values for the normal curve
  y_values <- dnorm(x_values, mean = mean(data), sd = sd(data))

  # Create histogram and capture density or count information
  hist_data <- hist(
    data,
    plot = FALSE # Do not plot yet, just calculate values
  )

  if (probability) {
    # Use density for y-axis
    max_hist_value <- max(hist_data$density, na.rm = TRUE)
    max_curve_value <- max(y_values, na.rm = TRUE)
    ylim_max <- max(max_hist_value, max_curve_value)
  } else {
    # Adjust y_values for count data
    y_values <- y_values * length(data) * diff(hist_data$breaks[1:2])
    max_hist_value <- max(hist_data$counts, na.rm = TRUE)
    max_curve_value <- max(y_values, na.rm = TRUE)
    ylim_max <- max(max_hist_value, max_curve_value)
  }

  # Plot histogram with adjusted ylim
  hist(
    data,
    probability = probability,
    col = col,
    border = border,
    main = main,
    xlab = xlab,
    ylim = c(0, ylim_max), # Use calculated maximum for ylim
    ...
  )

  # Overlay the normal curve
  lines(x_values, y_values, col = line_col, lwd = 2)

  saved_plot <- recordPlot()

  if (save_png == TRUE){



    if(is.null(output_file)){
    # Define the new file name
      output_file <- paste0(data_name, "_histogram.png")
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
