# This code is based on two packages MASS and rcompanion
# -------------------------
# 1.Some code to present the result was taken and modified from file:
# rcompanion/R/transformTukey.r
#
# @author Salvatore Mangiafico, \email{mangiafico@njaes.rutgers.edu}
#
# @references \url{https://rcompanion.org/handbook/I_12.html}
#-------------------------
# 2. The core of calculating lambda and the plotting was taken from:
# file MASS/R/boxcox.R
#
# copyright (C) 1994-2004 W. N. Venables and B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#  also see: https://r-coder.com/box-cox-transformation-r/
#--------------------------
#
#' f_boxcox: A User-Friendly Box-Cox Transformation
#'
#' Performs a Box-Cox transformation on a dataset to stabilize variance and make the data more normally distributed. It also provides diagnostic plots and tests for normality. The transformation is based on code of MASS/R/boxcox.R. The function prints \eqn{\lambda} to the console and returns (output) the transformed data set.
#'
#' @param data A numeric vector or a data frame with a single numeric column. The data to be transformed.
#' @param digits Numeric. Determines the accuracy of the estimate for lambda. Higher values increase computation time. Defaults to \code{3}.
#' @param range A numeric vector of length 2 defining the search interval for lambda. Defaults to \code{c(-2, 2)}.
#' @param plots Logical. If \code{TRUE}, plots log-likelihood of the Box-Cox transformation, Histograms and Q-Q plots of the original and transformed data. Default is \code{FALSE}.
#' @param close_generated_files Logical. If \code{TRUE}, closes open 'Word' files depending on the output format. This to be able to save the newly generated files. 'Pdf' files should also be closed before using the function and cannot be automatically closed.
#' @param open_generated_files Logical. If \code{TRUE}, opens the generated output files ('pdf', 'Word' or 'Excel') files depending on the output format. This to directly view the results after creation. Files are stored in tempdir(). Default is \code{TRUE}.
#' @param output_type Character string specifying the output format: \code{"pdf"}, \code{"word"}, \code{"rmd"}, \code{"off"} (no file generated) or \code{"console"}. The option \code{"console"} forces output to be printed. Default is \code{"off"}.
#' @param save_as Character string specifying the output file path (without extension).
#'   If a full path is provided, output is saved to that location.
#'   If only a filename is given, the file is saved in \code{tempdir()}.
#'   If only a directory is specified (providing an existing directory with trailing slash),
#'   the file is named "data_name_aov_output" in that directory. If an extension is provided the output format specified with option "output_type" will be overruled.
#'   Defaults to \code{file.path(tempdir(), "data_name_summary.pdf")}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory. Default is \code{FALSE}, this avoid unintended changes to the global environment. If \code{save_as} location is specified \code{save_in_wdir} is overwritten by \code{save_as}.
#' @param transform.data Logical. If \code{TRUE}, returns the transformed data. Default is \code{TRUE}.
#' @param eps A small positive value used to determine when to switch from the power transformation to the log transformation for numerical stability. Default is \code{1/50}.
#' @param xlab Character string. Label for the x-axis in plots. Default is an expression object representing \eqn{\lambda}.
#' @param ylab Character string. Label for the y-axis in plots. Default is "log-Likelihood".
#' @param alpha Numeric. Significance level for the Shapiro-Wilk test of normality. Default is \code{0.05}.
#' @param ... Additional arguments passed to plotting functions.
#'
#'
#' @return An object of class 'f_boxcox' containing, among others, results from the boxcox transformation, lambda, the input data, transformed data, Shapiro-Wilk test on original and transformed data. Using the option "output_type", it can also generate output in the form of: R Markdown code, 'Word', or 'pdf' files. Includes print and plot methods for 'f_boxcox' objects.
#'
#' @details
#'
#'The function uses the following formula for transformation:
#'\deqn{
#'  y(\lambda) =
#'    \begin{cases}
#'  \frac{y^\lambda - 1}{\lambda}, & \lambda \neq 0 \\  \log(y), & \lambda = 0
#'  \end{cases}
#'}
#'
#' where (\eqn{y}) is the data being transformed, and (\eqn{\lambda}) the transformation parameter, which is estimated from the data using maximum likelihood. The function computes the Box-Cox transformation for a range of \eqn{\lambda} values and identifies the \eqn{\lambda} that maximizes the log-likelihood function. The beauty of this transformation is that, it checks suitability of many of the common transformations in one run. Examples of most common transformations and their \eqn{\lambda} value is given below:
#'\renewcommand{\arraystretch}{3} % Adjusts row height (default is 1)
#' \tabular{cc}{
#'   \strong{\eqn{\lambda}-Value} \tab \strong{Transformation} \cr
#'   \strong{-----------------------}\tab\strong{-----------------------}\cr
#'   -2          \tab \eqn{\frac{1}{x^2}}          \cr
#'   -1          \tab \eqn{\frac{1}{x}  }          \cr
#'   -0.5        \tab \eqn{\frac{1}{\sqrt{x}}}     \cr
#'   0           \tab \eqn{log(x)}                 \cr
#'   0.5         \tab \eqn{\sqrt{x}}               \cr
#'   1           \tab \eqn{x}                      \cr
#'   2           \tab \eqn{x^2}                    \cr
#'   \strong{-----------------------}\tab\strong{-----------------------}\cr
#' }
#'
#' If the estimated transformation parameter closely aligns with one of the values listed in the previous table, it is generally advisable to select the table value rather than the precise estimated value. This approach simplifies interpretation and practical application.
#'
#' The function provides diagnostic plots: a plot of log-likelihood against \eqn{\lambda} values and a Q-Q plot of the transformed data.It also performs a Shapiro-Wilk test for normality on the transformed data if the sample size is less than or equal to 5000.
#'
#' \strong{Note}: For sample sizes greater than 5000, Shapiro-Wilk test results are not provided due to limitations in its applicability.
#'
#' This function requires [Pandoc](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3 or higher), a universal document converter.
#'\itemize{
#' \item \bold{Windows:} Install Pandoc and ensure the installation folder \cr (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your system PATH.
#' \item \bold{macOS:} If using Homebrew, Pandoc is typically installed in "/usr/local/bin". Alternatively, download the .pkg installer and verify that the binary’s location is in your PATH.
#' \item \bold{Linux:} Install Pandoc through your distribution’s package manager (commonly installed in "/usr/bin" or "/usr/local/bin") or manually, and ensure the directory containing Pandoc is in your PATH.
#'
#' \item If Pandoc is not found, this function may not work as intended.
#' }
#'
#' @examples
#' # Create non-normal data in a data.frame or vector.
#' df   <- data.frame(values = rlnorm(100, meanlog = 0, sdlog = 1))
#'
#' # Store the transformation in object "bc".
#' bc <- f_boxcox(df$values)
#'
#' # Print lambda and Shaprio.
#' print(bc)
#'
#' # Plot the QQ plots, Histograms and Lambda Log-Likelihood estimation.
#' plot(bc)
#'
#' # Or Directly use the transformed data from the f_boxcox object.
#' df$values_transformed <- f_boxcox(df$values)$transformed_data
#' print(df$values_transformed)
#'
#' @seealso
#' \href{https://CRAN.R-project.org/package=MASS}{\code{boxcox}}
#'
#' @references
#'
#'The core of calculating \eqn{\lambda} and the plotting was taken from: \cr
#' file MASS/R/boxcox.R copyright (C) 1994-2004 W. N. Venables and B. D. Ripley
#' \itemize{
#' \item \url{https://r-coder.com/box-cox-transformation-r/}
#' \item \url{https://CRAN.R-project.org/package=MASS}
#' }
#'Some code to present the result was taken and modified from file: \cr
#'rcompanion/R/transformTukey.r. (Developed by Salvatore Mangiafico)
#' \itemize{
#' \item \url{https://rcompanion.org/handbook/I_12.html}
#' }
#' The explanation on BoxCox transformation provided here was provided by r-coder:
#' \itemize{
#' \item  \url{https://r-coder.com/box-cox-transformation-r/}
#' }

#' @author
#' \itemize{
#' \item Sander H. van Delden  \email{plantmind@proton.me}
#' \item Salvatore Mangiafico, \email{mangiafico@njaes.rutgers.edu}
#' \item W. N. Venables and B. D. Ripley
#' }
#'
#'
#' @export
f_boxcox <- function(
    data = data,                  # Vector or a data.frame column
    digits = 3,                   # Accuracy by how many digits lambda is estimated
    range = c(-2, 2),             # The search interval for lambda.
    plots = FALSE,                # Show lambda est.histograms and QQ plots (TRUE) or not (FALSE)
    transform.data = TRUE,        # Specify the name of the file
    eps = 1 / 50,                 # Tolerance for lambda. Defaults to 0.02.
    xlab = expression(lambda),    # X-axis title of plot
    ylab = "log-Likelihood",      # Y-axis title of plot
    alpha = 0.05,                 # Significance level for shapiro test
    open_generated_files = TRUE,  # Open files after creation
    close_generated_files = FALSE,# Close open files to save a new one
    output_type = "off",          # Output type can be word, pdf, rmd, console
    save_as = NULL,               # Specify the name of the output dir and file (name and type).
    save_in_wdir = FALSE,         # Save file output in the working directory.
    ...                           # Additional arguments for model fitting
    ) {


  ########## Reset initial settings on exit ##################################
  # Save initial settings at the start
  old_par <- par(no.readonly = TRUE)  # Save graphical parameters
  old_par$new <- NULL                 # Remove this parameter to prevent warning
  original_options <- options()       # Save global options

  # Conditionally save panderOptions if the package is loaded
  original_panderOptions <- if (requireNamespace("pander", quietly = TRUE) && is.function(pander::panderOptions)) {
    pander::panderOptions()
  } else {
    NULL
  }

  # Single exit handler to restore settings
  on.exit({

    # Restore saved parameters for par
    par(old_par)

    # Restore global options
    options(original_options)

    # Restore panderOptions if they were saved
    if (!is.null(original_panderOptions)) {
      for (opt in names(original_panderOptions)) {
        try(pander::panderOptions(opt, original_panderOptions[[opt]]), silent = TRUE)
      }
    }
  }, add = TRUE)


  # Parameter validation
  if( !(output_type %in% c("pdf", "word", "excel", "rmd", "console" , "off")) ){
    stop("Character string specifying the output format (output_type = ) should be either: 'pdf', 'word', 'excel', 'console','rmd', 'off'")
  }


  # Generate a temporary file path for "output.Rmd"
  temp_output_dir  <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")

  # Create the output file "output.Rmd" in tempdir()
  file.create(temp_output_file)

  # Create an output list to store info
  output_list <- list()
  output_list[["plots"]] <- plots

  # Capture the name of the submitted data object
  if (is.data.frame(data)) {
    # Get the name of the first column
    data_name <- colnames(data)[[1]]
    data <- data.frame(y = data[[1]])
  }

  # Convert potential vector to data frame
  if (is.vector(data)) {
    data_name <- deparse(substitute(data))
    data <- data.frame(y = data)
  }

  # Handle input: ensure data is numeric
  if (ncol(data) > 1) stop("The data.frame has multiple columns please select one.")
  if (!is.data.frame(data)) stop("Input must be a numeric vector or data frame.")
  if (!is.numeric(data$y)) stop(paste0("The ", data_name," column in the data must be numeric."))


  clean_data_name <- sub(".*\\$", "", data_name)  # Remove everything before the "$" symbol


  #### Handle option "save_as = " ###
  if(save_in_wdir == TRUE){
    save_dir <- getwd()
  }else{
    save_dir <- tempdir()
  }

  #map the output type to extensions
  output_type_map <- c(
    "pdf"  = ".pdf",
    "word" = ".docx",
    "rmd"  = ".rmd"
  )

  # If the user specifies a path, filename or save_in_wdir == TRUE an output file should be created
  if (!is.null(save_as) || save_in_wdir == TRUE) {

    if (!is.null(save_as)) {
      #Remove backslash in save_as if needed
      save_as <- gsub(pattern = "\\\\", replacement = "/", x = save_as)
      file_extension_save_as <- unname(extract_extension(save_as))
      if(file_extension_save_as[1] != FALSE){
        file_extension <- file_extension_save_as
      }
    }

    if(!exists("file_extension") && output_type %in% c("console", "off")){
      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(clean_data_name, "BoxCox_output", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = ".pdf"
      )
      #set output_type to default
      output_type <- "pdf"

    }
    else if(!exists("file_extension") && output_type %in% c("pdf", "word", "excel", "rmd")){

      #create extension based on input_type
      file.ext <- unname(output_type_map[output_type])

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(clean_data_name, "BoxCox_output", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = file.ext
      )


    }
    else if(exists("file_extension")) {

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(clean_data_name, "BoxCox_output", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = file_extension[1]
      )
      # reset the output type to match the user input extention in save_as
      output_type <- file_extension[2]
    }
  } else {

    #create extension based on input_type
    file.ext <- unname(output_type_map[output_type])

    # use helper get_save_path() to create output_path
    output_path <- get_save_path(save_as = save_as,
                                 default_name = paste(clean_data_name, "BoxCox_output", sep = "_"),
                                 default_dir = save_dir,
                                 file.ext = file.ext
    )
  }


  # Prevent output to console and keep files open when output is "rmd" format
  if(output_type == "rmd"){
    close_generated_files <- FALSE
  }

  if(output_type != "rmd"){

    if(close_generated_files == TRUE && output_type == "word"){
      # Close all MS Word files to avoid conflicts (so save your work first)
      system("taskkill /im WINWORD.EXE /f")
    }

    if(close_generated_files == TRUE && output_type == "excel"){
      # Close all MS Word files to avoid conflicts (so save your work first)
      system("taskkill /im EXCEL.EXE /f")
    }

  }

  #Create a Vector of potential values for lambda
  lambda <- seq(min(range, na.rm = TRUE), max(range, na.rm = TRUE), 1 / 10^digits)

  # Extract and validate the response variable
  y <- data$y
  y_clean <- y[!is.na(y)]


  if (any(y_clean <= 0)) stop("Response variable must higher than zero.")

  # Scale y for numerical stability
  y_clean <- y_clean / exp(mean(log(y_clean)))  # Geometric mean scaling
  logy <- log(y_clean)
  n <- length(y_clean)

  # Initialize log-likelihood
  loglik <- numeric(length(lambda))
  for (i in seq_along(lambda)) {
    la <- lambda[i]
    yt <- if (abs(la) > eps) (y_clean^la - 1) / la else logy * (1 + la * logy / 2)
    loglik[i] <- -n / 2 * log(sum((yt - mean(yt))^2))
  }



  # Interp Logical. Controls if spline interpolation is used
  interp = (plots && (length(lambda) < 100))

  # Interpolation for smooth plotting
  if (interp) {
    sp <- spline(lambda, loglik, n = 100)
    lambda <- sp$x
    loglik <- sp$y
  }


  #Do a transformation of the entered data
  max_idx <- which.max(loglik)
  max_ll  <- loglik[max_idx]
  conf_limit <- max_ll - qchisq(0.95, 1) / 2
  if(lambda[max_idx] != 0){
    transformed_data <- (data ^ lambda[max_idx] - 1) / lambda[max_idx]
  } else if(lambda[max_idx] == 0){
    transformed_data <- log(data)
  }

  transformed_data <- data.frame(transformed_data)
  data_name_out <- paste0(data_name, "_transformed")
  colnames(transformed_data) <- data_name_out

  # Lambda for output
  lambda_out <- signif(lambda[max_idx], digits=4)


  n <- length(data$y)
  if (n <= 5000) {
    # Shapiro on initial data (run once)
    st0 <- shapiro.test(data[[1]])
    W0 <- signif(st0$statistic, digits = 4)
    Shapiro.p.value0 <- signif(st0$p.value, digits = 4)
    df0 <- data.frame(W0, Shapiro.p.value0)

    # Shapiro on transformed data (run once)
    st1 <- shapiro.test(transformed_data[[1]])
    W <- signif(st1$statistic, digits = 4)
    Shapiro.p.value <- signif(st1$p.value, digits = 4)
    df <- data.frame("lambda" = lambda_out, W, Shapiro.p.value)
  }

  if( n > 5000){
  #   A =signif(ad.test(transformed_data[[1]])$statistic, digits=4)
  #   Anderson.p.value =signif(ad.test(transformed_data[[1]])$p.value, digits=4)
  #   df=data.frame("Lambda" = lambda_out, A, Anderson.p.value)
  message("Shapiro-Wilks cannot be used with sample sizes > 5000.")
  }

  output_list[["transformed_data"]]         <- transformed_data[[1]]
  output_list[["original_data"]]            <- data[[1]]
  output_list[["lambda"]]                   <- lambda_out
  output_list[["n"]]                        <- n
  output_list[["missing values"]]           <- sum(is.na(y))
  output_list[["xlab"]]                     <- xlab
  output_list[["ylab"]]                     <- ylab
  output_list[["alpha"]]                    <- alpha
  output_list[["lambda_estimates"]]         <- lambda
  output_list[["loglik"]]                   <- loglik
  output_list[["conf_limit"]]               <- conf_limit
  output_list[["ylab"]]                     <- ylab
  output_list[["ylab"]]                     <- ylab
  output_list[["Shapiro_original_data"]]    <- df0
  output_list[["Shapiro_transformed_data"]] <- df[1-2]

  # Make a class from the output to print and plot
  class(output_list) <- "f_boxcox"


  if(output_type != "console" && output_type != "off"){
  generate_report <- function(){
  # Return results
    cat("Shapiro-Wilkinson test on untransformed **original data:**")
    cat(" W =", W0, " p value =", Shapiro.p.value0, "   \n")
    if(Shapiro.p.value0 >= alpha){
      cat("According to the ShapiroW test (", Shapiro.p.value0, " > ",alpha,") original data is already normally distributed. \nTransformation will be applied regardless...  \n")
    }
    if(Shapiro.p.value0 < alpha){
      cat("According to the ShapiroW test (", Shapiro.p.value0, " < ",alpha,") original data is NOT normally distributed.  \nTransformation will be applied...")
    }

    cat("   \n   \n")
    cat("$$y(\\lambda) = \\begin{cases} \\frac{y^\\lambda - 1}{\\lambda}, & \\lambda \\neq 0 \\\\    \\log(y), & \\lambda = 0 \\end{cases}
            $$   \n")

    cat("   \n   \n**Box-Cox Transformation $\\lambda$ =", df$lambda, "**  \n")


    # Plot results of transformation
    if(plots == TRUE){
      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = 8, height = 5, units = "in", res = 300)
      plot(output_list, which= 1)
      dev.off()
      cat(paste0("![](", temp_file, ")"), "   \n  \n")
    }


    cat("   \n   \n**Interpretion:**   \n")
    if(Shapiro.p.value >= alpha){
      cat("According to the ShapiroW test (", Shapiro.p.value, " > ",alpha,") data is normally distributed after transformation.  \n \n Inspect the plots to check normality and outliers:  \n  \n ")
    }
    if(Shapiro.p.value < alpha){
      cat("According to the ShapiroW test (", Shapiro.p.value, " < ",alpha,") data is still NOT normally distributed after transformation.  \n  \n Inspect the plots to check normality and outliers:  \n  \n ")
    }


    # Plot results of transformation histogram
    if(plots == TRUE){
      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = 7.8, height = 5, units = "in", res = 300)
      plot(output_list, which= 2)
      dev.off()
      cat(paste0("![](", temp_file, ")"), "   \n  \n")
    }

    # Plot results of transformation qqplots
    if(plots == TRUE){
      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = 7.8, height = 5, units = "in", res = 300)
      plot(output_list, which= 3)
      dev.off()
      cat(paste0("![](", temp_file, ")"), "   \n  \n")
    }
} # End generate report
  }

# Here the documents are constructed.
if (output_type %in% c("word", "pdf")) {


  # Create a temporary R Markdown file
  word_pdf_preamble <- function(){ paste0("
---
title: \"f_BoxCox Report\"
date: \"`r Sys.Date()`\"
output:
   word_document:
      reference_docx: !expr system.file(\"rmarkdown/templates/MS_word_style.docx\", package = \"rfriend\")
   pdf_document:
        latex_engine: pdflatex
header-includes:
  - \\usepackage[utf8]{inputenc}
  - \\DeclareUnicodeCharacter{03BB}{\\ensuremath{\\lambda}}
  - \\usepackage{titling}
  - \\setlength{\\droptitle}{-2.5cm} % Adjust vertical spacing
---
")}


  # Prevent ## before printed output
  knitr::opts_chunk$set(comment = "")

  # Show save location before knitting else it will not display in console.
  message(paste0("Saving output in: ", output_path))

  # re-run generate_report, but this time capture its output to a string
  generated_markdown <- capture.output(generate_report())

  # Combine the preamble, assumptions, and the captured report into one string
  rmd_content <- paste(
    word_pdf_preamble(),
    # The captured output already contains the necessary markdown formatting and image links
    paste(generated_markdown, collapse = "\n"),
    sep = "\n"
  )

  # Write the complete Rmd content to the temp file
  writeLines(rmd_content, temp_output_file)

  # Create the RMarkdown file
  rmarkdown::render(
    temp_output_file,
    output_file = output_path,
    intermediates_dir = temp_output_dir,
    knit_root_dir = temp_output_dir,
    quiet = TRUE,
    output_format = paste0(output_type, "_document")
  )

    if(open_generated_files == TRUE){
    # Open the file with default program
    f_open_file(output_path)
    }

  }
  else if (output_type == "rmd"){

    if (is.null(opts_knit$get("output.dir"))) {
      opts_knit$set(output.dir = tempdir())
    }

    # Re-capture the markdown text for the rmd output
    generated_markdown <- capture.output(generate_report())

    clean_rmd_output <- paste(generated_markdown, collapse = "\n")

    output_list[["rmd"]] <- clean_rmd_output

  }
  else if (output_type == "console"){


    print(output_list)
    # Plot all to console
    plot(output_list)


  }
  else if (output_type == "off"){

 # Do not show anything unless the user want to see plots:
    # Plot results of transformation if the user wants to
    if (plots ==TRUE) plot(output_list)

   }
  else {

      warning("Invalid output format specified. No file generated.")

      generate_report()
   }

  # Remove the temporary R Markdown file
  invisible(suppressWarnings(file.remove(temp_output_file)))

  return(output_list)

}

#' @export
print.f_boxcox <- function(x, ...) {

  cat("Box-Cox\n")
  cat("--------\n")
  if(x$Shapiro_original_data$Shapiro.p.value0 >= x$alpha){
  cat("According to the Shapiro-Wilk test (", x$Shapiro_original_data$Shapiro.p.value0, " > ",x$alpha,") original data is:\n already normally distributed. Transformation will be applied regardless...  \n")
  }
  if(x$Shapiro_original_data$Shapiro.p.value0 < x$alpha){
  cat("According to the Shapiro-Wilk test (", x$Shapiro_original_data$Shapiro.p.value0, " < ",x$alpha,") original data is:\n NOT normally distributed. Transformation will be applied...")
  }

  cat("\u00A0  \n")
  cat("  \nFormula used for transformation:   \n")
  cat("{ (x^\u03BB - 1) / \u03BB } if \u03BB != 0   \n")
  cat("{ log(x)\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0} if \u03BB == 0   \n")
  cat("\u00A0  \n")
  cat("Box-Cox Transformation \u03BB =", x$lambda, "  \n")
  if(x$Shapiro_transformed_data$Shapiro.p.value >= x$alpha){
  cat("According to the Shapiro-Wilk test (", x$Shapiro_transformed_data$Shapiro.p.value, " > ",x$alpha,") data is\n normally distributed after transformation.")
  }
  if(x$Shapiro_transformed_data$Shapiro.p.value < x$alpha){
  cat("According to the Shapiro-Wilk test (", x$Shapiro_transformed_data$Shapiro.p.value, " < ",x$alpha,") data is\n still NOT normally distributed after transformation.")

  cat("\nCheck the normality plots, by using the plot() function or 'plots = TRUE' option\n")
  }

  if(x$plots == TRUE) plot(x)

}

#' Plot method for f_boxcox objects
#'
#' @title Plot an f_boxcox object
#' @name plot.f_boxcox
#' @description Create diagnostic plots of an object of class \code{f_boxcox}.
#' @param x An object of class \code{f_boxcox}.
#' @param which Integer determining which graph to plot. Default is \code{1:2}.
#' @param ask Logical. \code{TRUE} waits with plotting each graph until <Return> is pressed. Default is \code{FALSE}.
#' @param ... Further arguments passed to or from other methods.
#' @method plot f_boxcox
#' @return This function is called for its side effect of generating plots
#' and does not return a useful value. It invisibly returns \code{1}.
#' @export
plot.f_boxcox <- function(x, which = 1:3, ask = FALSE, ...) {
# Save and restore par options
old_par <- par(no.readonly = TRUE)
old_par$new <- NULL                 # Remove this parameter to prevent warning
on.exit({
  par(old_par)
  layout(1)  # Reset layout matrix
})

par(ask = ask)

  # 1: Log-likelihood plot
if (1 %in% which) {
    plot(x$lambda_est, x$loglik, type = "l",
         xlab = x$xlab, ylab = x$ylab,
         main = paste("Log-Likelihood estimation\nLambda=", x$lambda))
    abline(h = x$conf_limit, lty = 2)
    abline(v = x$lambda, lty = 3)
}


  # 2: Histograms
if (2 %in% which) {
    # Set up a 2x2 plotting layout with adjusted margins mar = c(bottom, left, top, right)
    par(mfrow = c(1, 2),
        mar = c(3, 2.8, 4, 0.6), # Default is mar = c(5.1, 4.1, 4.1, 2.1)
        oma = c(0, 0, 2, 0),
        mgp = c(1.7, .5, 0)  # Default is par(mgp = c(3, 1, 0))
        )
    f_hist(x$original_data,
           main = paste0(main = "Original data: ", x$data_name),
           cex.main = 0.9, cex.lab = 0.8, cex.axis = 0.8)
    f_hist(x$transformed_data,
           main = paste0("Boxcox transformed data\n Lambda= ", x$lambda),
           cex.main = 0.9, cex.lab = 0.8, cex.axis = 0.8)
    mtext("Histograms", outer = TRUE, cex = 1.3)
    par(mfrow = c(1,1))
    layout(1)  # Clear layout matrix
}
  # 3: QQplots
if (3 %in% which) {
  par(mfrow = c(1, 2),
      mar = c(3, 2.8, 4, 0.6), # Default is mar = c(5.1, 4.1, 4.1, 2.1)
      oma = c(0, 0, 2, 0),
      mgp = c(1.7, .5, 0)  # Default is par(mgp = c(3, 1, 0))
  )
    f_qqnorm(x$original_data, paste0(main = "Original data: ", x$data_name),
             cex.main = 0.9, cex.lab = 0.8, cex.axis = 0.8)
    f_qqnorm(x$transformed_data,
             main = paste0("Boxcox transformed data\n Lambda= ", x$lambda),
             cex.main = 0.9, cex.lab = 0.8, cex.axis = 0.8)
    mtext("Normal Q-Q Plot with 95% Confidence Bands", outer = TRUE, cex = 1.3)
    par(mfrow = c(1,1))
    layout(1)  # Clear layout matrix
}


}
