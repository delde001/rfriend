#' f_bestNormalize: Automated Data Normalization with bestNormalize
#'
#' Applies optimal normalization transformations using 'bestNormalize',
#' provides diagnostic checks, and generates comprehensive reports.
#'
#' @param data Numeric vector or single-column data frame.
#' @param alpha Numeric. Significance level for normality tests (default = \code{0.05}).
#' @param data_name A character string to manually set the name of the data for plot axis and reporting. Default extracts name from input object. \code{data}.
#' @param plots Logical. If \code{TRUE}, plots Q-Q plots and Histograms of the original and transformed data. Default is \code{FALSE}.
#' @param output_type Character string specifying the output format: \code{"pdf"}, \code{"word"}, \code{"rmd"}, \code{"off"} (no file generated) or \code{"console"}. The option \code{"console"} forces output to be printed. Default is \code{"off"}.
#' @param save_as Character string specifying the output file path (without extension).
#'   If a full path is provided, output is saved to that location.
#'   If only a filename is given, the file is saved in \code{tempdir()}.
#'   If only a directory is specified (providing an existing directory with trailing slash),
#'   the file is named "data_name_transformed" in that directory. If an extension is provided the output format specified with option "output_type" will be overruled.
#'   Defaults to \code{file.path(tempdir(), "data_name_transformed.pdf")}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory. Default is \code{FALSE}, this avoid unintended changes to the global environment. If \code{save_as} location is specified \code{save_in_wdir} is overwritten by \code{save_as}.
#' @param close_generated_files Logical. If \code{TRUE}, closes open 'Word' files. This to be able to save the newly generated file by the \code{f_bestNormalize()} function. 'Pdf' files should also be closed before using the function and cannot be automatically closed. Default is \code{FALSE}.
#' @param open_generated_files Logical. If \code{TRUE}, Opens the generated output file, this to directly view the results after creation. Files are stored in tempdir(). Default is \code{TRUE}.
#' @param ... Additional arguments passed to bestNormalize.
#'
#' @return Returns an object of class `f_bestNormalize` containing:
#' \itemize{
#'   \item \code{transformed_data} Normalized vector.
#'   \item \code{bestNormalize} Full bestNormalize object from original package.
#'   \item \code{data_name} Name of the analyzed dataset.
#'   \item \code{transformation_name} Name of selected transformation.
#'   \item \code{shapiro_original} Shapiro-Wilk test results for original data.
#'   \item \code{shapiro_transformed} Shapiro-Wilk test results for transformed data.
#'   \item \code{norm_stats} Data frame of normality statistics for all methods.
#'   \item \code{rmd} Rmd code if outputype = "rmd".
#'}
#' Also generates reports in specified formats, when using output to console and plots = TRUE, the function prints QQ-plots, Histograms and a summary data transformation report.
#'
#'#' @return An object of class 'f_bestNormalize' containing results from \code{"bestNormalize"}, the input data, transformed data, Shapiro-Wilk test on original and transformed data. Using the option "output_type", it can also generate output in the form of: R Markdown code, 'Word', or 'pdf' files. Includes print and plot methods for objects of class 'f_bestNormalize'.
#'
#' @details
#' This is a wrapper around the 'bestNormalize' package. Providing a fancy output and the settings  of 'bestNormalize' are tuned based on sample size n.
#' If n < 100, \code{loo = TRUE}, \code{allow_orderNorm = FALSE} and \code{r} doesn't matter as \code{loo = TRUE}.
#' If 100 <= n < 200, \code{loo = FALSE}, \code{allow_orderNorm = TRUE} and \code{r = 50}.
#' If n >= 200, \code{loo = FALSE}, \code{allow_orderNorm = TRUE}, \code{r = 10}. These setting can be overwritten by user options.
#'
#' This function requires [Pandoc](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3 or higher), a universal document converter.
#'\itemize{
#' \item \bold{Windows:} Install Pandoc and ensure the installation folder \cr (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your system PATH.
#' \item \bold{macOS:} If using Homebrew, Pandoc is typically installed in "/usr/local/bin". Alternatively, download the .pkg installer and verify that the binary’s location is in your PATH.
#' \item \bold{Linux:} Install Pandoc through your distribution’s package manager (commonly installed in "/usr/bin" or "/usr/local/bin") or manually, and ensure the directory containing Pandoc is in your PATH.
#'
#' \item If Pandoc is not found, this function may not work as intended.
#' }

#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @references
#' Peterson, C. (2025). \pkg{bestNormalize}: Flexibly calculate the best normalizing transformation for a vector.
#' Available at: \url{https://cran.r-project.org/package=bestNormalize}
#'
#' @examples
#' \donttest{
#' # Create some skewed data (e.g., using a log-normal distribution).
#' skewed_data <- rlnorm(100, meanlog = 0, sdlog = 1)
#'
#' # Use set.seed to keep the outcome of bestNormalize stable.
#' set.seed(123)
#'
#' # Transform the data and store all information in f_bestNormalize_out.
#' f_bestNormalize_out <- f_bestNormalize(skewed_data)
#'
#' # Print the output.
#' print(f_bestNormalize_out)
#'
#' # Show histograms and QQplots.
#' plot(f_bestNormalize_out)
#'
#' # Directly store the transformed_data from f_bestNormalize and force to show
#' # plots and transformation information.
#' transformed_data <- f_bestNormalize(skewed_data, output_type = "console")$transformed_data
#'
#' # Any other transformation can be choosen by using:
#' boxcox_transformed_data <- f_bestNormalize(skewed_data)$bestNormalize$other_transforms$boxcox$x.t
#' # and substituting '$boxcox' with the transformation of choice.
#'
#' #To print rmd output set chunck option to results = 'asis' and use:
#' f_bestNormalize_rmd_out <- f_bestNormalize(skewed_data, output_type = "rmd")
#' cat(f_bestNormalize_rmd_out$rmd)
#' }
#'
#' @export
f_bestNormalize <- function(data,
                            alpha = 0.05,
                            plots = FALSE,
                            data_name = NULL,
                            output_type = "off",
                            save_as = NULL,
                            save_in_wdir = FALSE,
                            close_generated_files = FALSE,
                            open_generated_files = TRUE,
                            ...) {
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



  if( !(output_type %in% c("pdf", "word", "rmd", "console", "off")) ){
    stop("Character string specifying the output format (output_type = ) should be either: 'pdf', 'word', 'rmd', 'off', or 'console'")
  }

  ##############################################################################

  # Create object with transformed data as primary element
  output_list <- list()

  # Input validation and initialization
  if (is.data.frame(data)) {
      if(is.null(data_name)){
      data_name <- colnames(data)[1]
      data_name <- sub(".*\\$", "", data_name)  # Remove everything before the "$" symbol
      y <- data[[1]]
      } else {
      #i.e. data_name = data_name
      y <- data[[1]]
      }
  } else if (is.vector(data)) {
      if(is.null(data_name)){
      data_name <- deparse(substitute(data))
      data_name <- sub(".*\\$", "", data_name)  # Remove everything before the "$" symbol
      y <- data
      } else {
      #i.e. data_name = data_name
      y <- data
      }
  } else stop("Input must be vector or data.frame")


  if (!is.numeric(y)) stop("Data must be numeric")

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
                                   default_name = paste(data_name, "transformed", sep = "_"),
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
                                   default_name = paste(data_name, "transformed", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = file.ext
      )


    }
    else if(exists("file_extension")) {

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "transformed", sep = "_"),
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
                                 default_name = paste(data_name, "transformed", sep = "_"),
                                 default_dir = save_dir,
                                 file.ext = file.ext
    )
  }


  # Prevent output to console and keep files open when output is "rmd" format
  if(output_type == "rmd"){
    close_generated_files <- FALSE
  }

  # Create clean data for stable tuning
  y_clean <- y[!is.na(y)]
  n <- length(y_clean)

  # Normality check on original data

  andersonD_original <- nortest::ad.test(y)

  if (n <= 5000) {
    shapiro_original <- shapiro.test(y)
  } else {
    shapiro_original <- list(statistic = NA, p.value = NA)
  }


  # Tune setting of bestNormalize based on sample size. Can be overwritten by user options.

  # Tune bestNormalize settings based on valid sample size 'n'
  if (n < 500) {
    # LOO is generally faster and more stable than Boot(50) for N < 500
    if(!exists("loo")) loo <- TRUE
    # Keep orderNorm disabled for very small N if you prefer stability
    if(!exists("allow_orderNorm")) allow_orderNorm <- n >= 100
    # r is irrelevant when loo = TRUE

  } else {
    # For N >= 500, switch to fast bootstrapping
    if(!exists("loo")) loo <- FALSE
    if(!exists("allow_orderNorm")) allow_orderNorm <- TRUE
    # r = 10 is usually sufficient for large N
    if(!exists("r")) r <- 10
  }

  # Train bestNormalize on CLEAN data
  # This avoids crashes/instability and speeds up bestNormalize
  bn_obj <- bestNormalize::bestNormalize(y_clean,
                                         loo = loo,
                                         allow_orderNorm = allow_orderNorm,
                                         r = r,
                                         ...)

  # Get final vector matching original 'y' length using predict()
  # to automatically handle the NAs in 'newdata'
  transformed <- predict(bn_obj, newdata = y)

  # Patch the internal bn_obj with the full dataset
  # This ensures bn_obj$x.t matches the length of the input data 'y'
  bn_obj$x.t <- transformed
  # Patch the input data stored in the bestNormalize object for full consistency
  bn_obj$x <- y

  bn_obj$chosen_transform$x.t <- transformed
  bn_obj$chosen_transform$x   <- y

  #Do not patch the whole bn_obj as this will slow down the function.
  #leaving bn_obj$other_transforms alone, data will be without NAs


  # start code copy from bestNormalize.R
  prettynames <- c(
    "arcsinh_x" = "arcsinh(x)",
    "center_scale" = "Center+scale",
    "double_reverse_log" = "Double Reversed Log_b(x+a)",
    "boxcox" = "Box-Cox",
    "exp_x" = "Exp(x)",
    "lambert_h" = "Lambert W (h)",
    "lambert_s" = "Lambert W (s)",
    "log_x" = "Log-transform",
    "orderNorm" = "Quantile Normalization (ORQ)",
    "sqrt_x" = "sqrt(x + a)",
    "yeojohnson" = "Yeo-Johnson"
  )
  # Base R approach using match()
  normnames <- names(bn_obj$norm_stats)
  matches   <- match(normnames, names(prettynames))
  normnames <- ifelse(!is.na(matches), prettynames[matches], normnames)

  output <- data.frame(
    Transformation = normnames,
    Normality_Stat = round(bn_obj$norm_stats, 4),
    row.names = NULL
  )

  rownames(output) <- NULL
  Transf_name <- class(bn_obj$chosen_transform)[1]
  Transf_name <- f_rename_vector(Transf_name, prettynames)


  # Normality check on transformed data
  andersonD_transformed <- nortest::ad.test(transformed)

  if (n <= 5000) {
    shapiro_transformed <- shapiro.test(transformed)
  } else {
    shapiro_transformed <- list(statistic = NA, p.value = NA)
  }






  if(output_type %in% c("word", "pdf", "rmd")){
    # png output mode
    temp_png <- tempfile(fileext = ".png")
    png(temp_png, width = 7.8, height = 7.8, units = "in", res = 600)

    par(mfrow = c(2, 2))
    f_hist(y, main = "Original Data", xlab = data_name)
    f_hist(transformed, main = "Transformed Data", xlab = paste(data_name, " Transformed"))

    f_qqnorm(y, main = "QQ Plot - Original", ylab = paste0("Quantiles of ", data_name))
    f_qqnorm(transformed,
             main = "QQ Plot - Transformed",
             ylab = paste0("Quantiles of transformed ", data_name))

    dev.off()
  }

  # Generate reports and plots
  generate_report <- function() {

    cat("\n   \n##  Data transformation of ", data_name, "using `bestNormalize`:", Transf_name,".  \n  \n")

    # Text report
    cat("**Original Data Shapiro-Wilk Test:**&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
    cat("W =", round(shapiro_original$statistic, 4))
    cat("&nbsp;&nbsp;&nbsp;&nbsp;p-value =", format.pval(shapiro_original$p.value), "\n")


    # cat("**Applied Transformation:**&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", Transf_name, "   \n   \n")

    # Normality check transformed data
    shapiro_transformed <- shapiro.test(transformed)

    cat("\n**Transformed Data Shapiro-Wilk Test:** ")
    cat("W =", round(shapiro_transformed$statistic, 4))
    cat("&nbsp;&nbsp;&nbsp;&nbsp;p-value =", format.pval(shapiro_transformed$p.value), "\n  \n")
    cat("&nbsp;   \n  \n")

    cat("**Table.** All considered transformations: [Pearson P / df, lower => more normal]",
        paste0("  (n=", bn_obj$chosen_transform$n, ")\n"))
    f_pander(output)

    if(plots == TRUE){
    cat("&nbsp;\n   \n")
    cat("\nCheck the plots in the figure below to assess normality.  \n")

    cat(paste0("![](", temp_png, ")"), "   \n  \n")
    }
  } # Close generate report function


  # Generate a temporary file path for "output.Rmd"
  temp_output_dir  <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")

  # Attach metadata
  output_list[["plots"]]               <- plots
  output_list[["bestNormalize"]]       <- bn_obj
  output_list[["data_name"]]           <- data_name
  output_list[["transformation_name"]] <- Transf_name
  output_list[["original_data"]]       <- y
  output_list[["transformed_data"]]    <- transformed
  output_list[["shapiro_original"]]    <- shapiro_original
  output_list[["shapiro_transformed"]] <- shapiro_transformed
  output_list[["andersonD_original"]]    <- andersonD_original
  output_list[["andersonD_transformed"]] <- andersonD_transformed
  output_list[["norm_stats"]]          <- output
  if(output_type %in% c("word", "pdf", "rmd")){
    output_list[["normality_plots"]]  <- magick::image_scale(image_read(temp_png), "600")
  }
  # Set class
  class(output_list) <- "f_bestNormalize"

  # Generate the pdf or word report, set save location and create markdown document
  if (output_type %in% c("word", "pdf")) {

    if(close_generated_files == TRUE && output_type == "word"){
        # Close all MS Word files to avoid conflicts (so save your work first)
        system("taskkill /im WINWORD.EXE /f")
      }

    # Show save location before knitting else it will not display in console.
    message(paste0("Saving output in: ", output_path))


          # Create a temporary R Markdown file
      word_pdf_preamble <- function(){ paste0("
---
title: \"f_bestNormalize Transformation Report\"
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

      return(invisible(output_list))

  } else if (output_type == "rmd"){

    if (is.null(opts_knit$get("output.dir"))) {
      opts_knit$set(output.dir = tempdir())
    }

    # Re-capture the markdown text for the rmd output
    generated_markdown <- capture.output(generate_report())

    clean_rmd_output <- paste(generated_markdown, collapse = "\n")

    output_list[["rmd"]] <- clean_rmd_output

    return(invisible(output_list))


  } else if (output_type == "console"){

    print(output_list)

    plot(output_list)

    return(invisible(output_list))

  } else if (output_type == "off"){

    if(plots == TRUE) plot(output_list)

    return(output_list)

  } else {
    warning("Invalid output format specified. No file generated.")

  }



} # Close


#' @export
print.f_bestNormalize <- function(x, ...) {

  cat("\nData transformation of", x$data_name, "using `bestNormalize`:", x$transformation_name,"\n")
  # Text report
  cat("Original Data Shapiro-Wilk Test: ")
  cat("   W =", round(x$shapiro_original$statistic, 4),
      "  p-value =", format.pval(x$shapiro_original$p.value, digits = 4), "\n")
  # cat("Applied Transformation:", Transf_name, "   \n   \n")
  cat("Transformed Data Shapiro-Wilk Test: ")
  cat("W =", round(x$shapiro_transformed$statistic, 4),
      "  p-value =", format.pval(x$shapiro_transformed$p.value, digits = 4), "\n  \n")
  cat("Below are all considered transformations: [Pearson P / df, lower => more normal]",
      paste0("  (n=", x$bestNormalize$chosen_transform$n, ")\n"))
  print(x$norm_stats, row.names = FALSE)
  cat("   \n")
  cat("\nCheck the normality plots, by using the plot() function or 'plots = TRUE' option\n")

  if(x$plots == TRUE) plot(x)

}


#' Plot method for f_bestNormalize objects
#'
#' @title Plot an f_bestNormalize object
#' @name plot.f_bestNormalize
#' @description Plots diagnostics for an object of class \code{f_bestNormalize}.
#' @param x An object of class \code{f_bestNormalize}.
#' @param which Integer determining which graph to plot. Default is \code{1:2}.
#' @param ask Logical. \code{TRUE} waits with plotting each graph until <Return> is pressed. Default is \code{FALSE}.
#' @param ... Further arguments passed to or from other methods.
#' @method plot f_bestNormalize
#'
#' @return This function is called for its side effect of generating plots
#'   and does not return a useful value. It invisibly returns `NULL`.

#' @export
plot.f_bestNormalize <- function(x, which = 1:2, ask = FALSE,...) {
  # Save and restore par options
  old_par <- par(no.readonly = TRUE)
  old_par$new <- NULL                 # Remove this parameter to prevent warning
  on.exit({
    par(old_par)
    layout(1)  # Reset layout matrix
  })

  par(ask = ask)

   # Histograms
  if (1 %in% which) {
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
           main = paste0("Transformed data \nwith `bestNormalize`: ", x$transformation_name),
           cex.main = 0.9, cex.lab = 0.8, cex.axis = 0.8)
    mtext("Histograms", outer = TRUE, cex = 1.3)
    par(mfrow = c(1,1))
    layout(1)  # Clear layout matrix
  }

  if (2 %in% which) {
    # Set up a 2x2 plotting layout with adjusted margins mar = c(bottom, left, top, right)
    par(mfrow = c(1, 2),
        mar = c(3, 2.8, 4, 0.6), # Default is mar = c(5.1, 4.1, 4.1, 2.1)
        oma = c(0, 0, 2, 0),
        mgp = c(1.7, .5, 0)  # Default is par(mgp = c(3, 1, 0))
    )
    f_qqnorm(x$original_data, paste0(main = "Original data: ", x$data_name),
             cex.main = 0.9, cex.lab = 0.8, cex.axis = 0.8)
    f_qqnorm(x$transformed_data,
             main = paste0("Transformed data \nwith `bestNormalize`: ", x$transformation_name),
             cex.main = 0.9, cex.lab = 0.8, cex.axis = 0.8)
    mtext("Normal Q-Q Plot with 95% Confidence Bands", outer = TRUE, cex = 1.3)
    par(mfrow = c(1,1))
    layout(1)  # Clear layout matrix
  }
}
