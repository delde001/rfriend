#' Correlation Plots with Factor Detection and Customization
#'
#' Creates correlation plots for numeric variables in a data frame, optionally incorporating factors for coloring and shaping points. It supports automatic detection of factors, customization of plot aesthetics, and the generation of separate legend files.
#'
#' @param data A \code{data.frame} containing the dataset to be visualized. Must include at least two numeric variables.
#' @param detect_factors Logical. If \code{TRUE}, the function automatically detects factor variables in the dataset for coloring and shaping points. Defaults to \code{TRUE}.
#' @param factor_table Logical. If \code{TRUE}, prints a detailed table about the properties of the converted factors to the console. Default is FALSE, so no property table will be printed to the console.
#' @param color_factor Character. The name of the factor variable to use for point colors. If set to \code{"auto"}, it is automatically determined based on detected factors. Defaults to \code{"auto"}.
#' @param shape_factor Character. The name of the factor variable to use for point shapes. If set to \code{"auto"}, it is automatically determined based on detected factors. Defaults to \code{"auto"}.
#' @param print_legend Logical. If \code{TRUE}, a separate legend file is created and displayed. Defaults to \code{TRUE}.
#' @param fancy_names Named character vector or \code{NULL}. Optional mapping of column names to more readable names for display in plots and legends.
#' @param width Numeric. The width of the output plot in centimeters (default 15 cm).
#' @param height Numeric. The height of the output plot in centimeters (default 15 cm).
#' @param res Numeric. The resolution (in dots per inch) for the output plot image (defaults 1000 dpi).
#' @param pointsize Numeric. The base font size for text in the plot image. Defaults to 8.
#' @param output_type Character string specifying the output format: "pdf", "word", "png" or "rmd". Default is "word".
#' @param save_as Character string specifying the output file path (without extension).
#'   If a full path is provided, output is saved to that location.
#'   If only a filename is given, the file is saved in \code{tempdir()}.
#'   If only a directory is specified (providing an existing directory with trailing slash),
#'   the file is named "dataname_CorPlot" in that directory. If an extension is provided the output format specified with option "output_type" will be overruled.
#'   Defaults to \code{file.path(tempdir(), "dataname_CorPlot.docx")}.
#' @param close_generated_files Logical. If \code{TRUE}, closes open 'Word' files depending on the output format. This to be able to save the newly generated files. 'Pdf' files should also be closed before using the function and cannot be automatically closed. Default is \code{FALSE}.
#' @param open_generated_files Logical. If \code{TRUE}, Opens the generated 'Word' output files. This to directly view the results after creation. Files are stored in tempdir(). Default is \code{TRUE}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory. Default is \code{FALSE}, this avoid unintended changes to the global environment. If \code{save_as} location is specified \code{save_in_wdir} is overwritten by \code{save_as}.

#' @details
#' \itemize{
#' \item Factor Detection: If \code{detect_factors} is enabled, up to two factors are automatically detected from the dataset and used for coloring (\code{color_factor}) and shaping (\code{shape_factor}) points in the plot.
#' \item Customization: Users can manually specify which factors to use by setting \code{color_factor} and/or (\code{shape_factor}). Non-factor variables are converted into factors automatically, with a message indicating this conversion.
#' \item Legend Generation: A separate legend file is created when factors are used or if \code{print_legend} is explicitly set to \code{TRUE}.
#'}
#'
#' The function uses numeric variables in the dataset for scatterplots and computes Pearson correlations displayed in the upper triangle of the correlation matrix.
#'
#' This function requires [Pandoc](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3 or higher), a universal document converter.
#'\itemize{
#' \item \bold{Windows:} Install Pandoc and ensure the installation folder \cr (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your system PATH.
#' \item \bold{macOS:} If using Homebrew, Pandoc is typically installed in "/usr/local/bin". Alternatively, download the .pkg installer and verify that the binary’s location is in your PATH.
#' \item \bold{Linux:} Install Pandoc through your distribution’s package manager (commonly installed in "/usr/bin" or "/usr/local/bin") or manually, and ensure the directory containing Pandoc is in your PATH.
#'
#' \item If Pandoc is not found, this function may not work as intended.
#' }
#' @return
#' Output is a 'Word' document with:
#' \itemize{
#' \item A correlation plot.
#' \item A legend if applicable.
#'}
#' Using the option "output_type", it can also generate output in the form of: R Markdown code, 'pdf', or 'PNG' files. No value is returned to the R environment; instead, files are saved, and they are opened automatically if running on Windows.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # Example usage:
#' data("mtcars")
#'
#' mtcars_sub <- subset(mtcars, select = -c(am, qsec, vs))

#' # Customizing factors:
#' f_corplot(mtcars_sub,
#'            shape_factor = "cyl",
#'            color_factor = "gear",
#'            output_type = "png",
#'            open_generated_files = FALSE
#'            )
#'
#'
#' # Output to MS Word and add fancy column names, only adjusting two of the four variable names.
#' data(iris)
#' fancy_names <- c(Sepal.Length = "Sepal Length (cm)", Sepal.Width = "Sepal Width (cm)")
#' f_corplot(iris,
#'            fancy_names = fancy_names,
#'            output_type = "word",
#'            open_generated_files = FALSE
#'            )
#'
#'
#' @note
#' \itemize{
#' \item At least two numeric variables are required in the dataset; otherwise, an error is thrown.
#' \item If more than two factors are detected, only the first two are used with a warning message.
#' }
#' @export

# Define a generalized function for correlation plots
f_corplot <- function(data,
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


  # Error if wrong output_type
  if( !(output_type %in% c("pdf", "word", "png", "rmd")) ){
    stop("Character string specifying the output format (output_type = ) should be either: \"pdf\", \"word\", \"png\" or \"rmd\"")
  }

  # Extract dataframe name
  data_name <- deparse(substitute(data))


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
    "png"  = ".png"
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

    if(!exists("file_extension") && output_type == "word"){
      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "CorPlot", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = ".docx"
                                   )

      #set output_type to default
      output_type <- "word"

    }
    else if(!exists("file_extension") && output_type %in% c("pdf", "word", "rmd", "png")){

      #create extension based on input_type
      file.ext <- unname(output_type_map[output_type])

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "CorPlot", sep = "_"),
                                   default_dir  = save_dir,
                                   file.ext     = file.ext
      )



    }
    else if(exists("file_extension")) {

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "CorPlot", sep = "_"),
                                   default_dir  = save_dir,
                                   file.ext     = file_extension[1]
      )

      # reset the output type to match the user input extention in save_as
      output_type <- file_extension[2]
    }
  } else {

    #create extension based on input_type
    file.ext <- unname(output_type_map[output_type])

    # use helper get_save_path() to create output_path
    output_path <- get_save_path(save_as = save_as,
                                 default_name = paste(data_name, "CorPlot", sep = "_"),
                                 default_dir  = save_dir,
                                 file.ext     = file.ext
    )

  }

  if(close_generated_files == TRUE && output_type == "word"){
    # Close all MS Word files to avoid conflicts (so save your work first)
    system("taskkill /im WINWORD.EXE /f")
  }

  # Close the PNG device (there is a bug in pairs that requires this)
  grDevices::dev.off()

  # Create a counter for the number of factors and set it to zero
  factor_count <- 0

  # Container for output plots
  output_list <- list()


  # Generate a temporary file path for "output.Rmd"
  temp_output_dir  <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")

  # Create the output file "output.Rmd" in tempdir()
  file.create(temp_output_file)

  # Rename the data.frame with fancy names if fancy_names are provided.
  # Also check if the color and shape factor need renaming.
  if(length(fancy_names) != 0){

    data         <- f_rename_columns(data, fancy_names)
    color_factor <- f_rename_vector(color_factor, fancy_names)
    shape_factor <- f_rename_vector(shape_factor, fancy_names)

  }


  if (color_factor == "auto" && shape_factor == "auto") {
    if (detect_factors == TRUE) {
      data <- f_factors(data, console = factor_table)
    }
    factor_vars <- vapply(data, is.factor, logical(1))
    factor_count <- sum(factor_vars)


    if (factor_count > 2) {
        warning(
          "The data contains more than 2 factors, only the frist 2 factors will be used: ",
          names(factor_vars[factor_vars])[1],
          " and ",
          names(factor_vars[factor_vars])[2],
          "\n Note that factors can be selected manually using
          'color_factor =' and 'shape_factor = '    \n"
        )
    }

    if (factor_count >= 2) {
      color_factor <- names(factor_vars[factor_vars])[1]
      shape_factor <- names(factor_vars[factor_vars])[2]
    }

    if (factor_count == 1) {
      color_factor <- names(factor_vars[factor_vars])[1]
      shape_factor <- "auto"
    }
  }

  if (!(color_factor %in% names(data)) && color_factor != "auto") {
    stop(paste("The defined color factor: ", color_factor,
               "was not found in the supplied data set."))

  } else if (!(shape_factor %in% names(data)) && shape_factor != "auto") {
    stop(paste("The defined shape factor: ", shape_factor,
               "was not found in the supplied data set."))

  } else if(!is.factor(data[[color_factor]]) || !is.factor(data[[shape_factor]])){

    if(!is.factor(data[[color_factor]]) && color_factor != "auto"){
      data[[color_factor]] <- factor(data[[color_factor]])
      message("\nVariable: ", color_factor," was converted to a factor")
      factor_count <- factor_count + 1
    }

    if(!is.factor(data[[shape_factor]]) && shape_factor != "auto"){

      data[[shape_factor]] <- factor(data[[shape_factor]])
      message("Variable: ", shape_factor," was converted to a factor  \n")
      factor_count <- factor_count + 1
    }

  } else if(factor_count == 0){

    message("The data contains NO factors, although not required know that factor can be selected manually using 'color_factor =' and 'shape_factor = '")
  }

  # Automatically detect numeric and factor variables
  numeric_vars <- vapply(data, is.numeric, logical(1)) # Identify numeric columns
  numeric_count <- sum(numeric_vars)

  if(numeric_count < 2){
    stop("Not enough numeric variables in data")
  }

  # Define colors and shapes for factors
  if(factor_count >= 2) {

    # Extract levels for color and shape factors
    color_levels <- levels(data[[color_factor]])
    shape_levels <- levels(data[[shape_factor]])

    # Count levels
    color_levels_count <- length(color_levels)
    shape_levels_count <- length(shape_levels)

    # Generate colors
    colors <- rainbow(color_levels_count, alpha=0.07)
    color_mapping <- colors[unclass(data[[color_factor]])]

    # Generate border colors
    border_colors <- rainbow(color_levels_count, v = 0.7)
    border_colors_mapping <- border_colors[unclass(data[[color_factor]])]

    # Generate different shapes (pch values) which can be filled
    if(shape_levels_count <= 5){
      shapes <- seq(21, 21 + shape_levels_count - 1)

      #If we run out of symbols....start at 3 (plus symbol)
    } else if(shape_levels_count > 5){
      shapes <- c(21, 22, 23, 24, 25)
      shapes <-  c(shapes, seq(3, 3 + shape_levels_count - 1))
    }

    # Diamond is not that distinctive so use triangle at three factors.
    if(length(shapes) == 3){
      shapes[shapes == 23] <- 24
    }
    # For logic also do this switch at > 3 symbols
    if(length(shapes) > 3){
      shapes[c(1, 2, 3, 4)] <- shapes[c(1, 2, 4, 3)]
    }

    # Symbol 23 (triangle is distinctive but larger than other fix here, a bit OCD ;-)
    cex       <- rep(0.8,  shape_levels_count)
    cex[c(3)] <- 0.7

    cex_mapping   <- cex[unclass(data[[shape_factor]])]
    shape_mapping <- shapes[unclass(data[[shape_factor]])]


    # Generate legend colors and shapes
    legend_colors <- rainbow(length(color_levels), alpha = 0.1)
    legend_border_colors <- rainbow(length(color_levels),v = 0.7)
    legend_shapes <- shapes


    # Generate legend  by combining color and shape levels
    legend_ <- as.vector(outer(color_levels, shape_levels,
                               FUN = function(c, s) paste(c, "-", s)))
    # Generate title
    legend_title <- paste("Legend for levels of factors: ", color_factor,"-", shape_factor)


    # Generate legend symbols
    col   <- rep(legend_border_colors, each = length(shape_levels))
    pch   <- rep(legend_shapes, times = length(color_levels))
    pt.bg <- rep(legend_colors, each = length(shape_levels))

    # Generate legend columns
    ncol <- 4   # Adjust columns for readability

  } else if(factor_count == 1){

    if (shape_factor == "auto") {

      # Extract levels for color and shape factors
      color_levels <- levels(data[[color_factor]])

      # Count levels
      color_levels_count <- length(color_levels)

      # Generate colors
      colors <- rainbow(color_levels_count, alpha=0.07)
      color_mapping <- colors[unclass(data[[color_factor]])]

      # Generate colors
      shape_mapping <- c(21)

      cex_mapping   <- 0.8

      # Generate border colors
      border_colors <- rainbow(color_levels_count, v = 0.7)
      border_colors_mapping <- border_colors[unclass(data[[color_factor]])]

      # Generate legend colors and shapes
      legend_colors <- rainbow(length(color_levels), alpha = 0.1)
      legend_border_colors <- rainbow(length(color_levels),v = 0.7)
      legend_shapes <- c(21)

      # Generate legend  by combining color and shape levels
      legend_ <- as.vector(color_levels)

      # Generate title
      legend_title <- paste("Legend for levels of factor: ", color_factor)

      # Generate legend symbols
      col   <- legend_border_colors
      pch   <- legend_shapes
      pt.bg <- legend_colors

      # Generate legend columns
      ncol <- 4   # Adjust columns for readability

    }

    if (shape_factor != "auto" && color_factor == "auto") {

      # Extract levels for color and shape factors
      shape_levels <- levels(data[[shape_factor]])

      # Count levels
      shape_levels_count <- length(shape_levels)

      # Generate different shapes (pch values) which can be filled
      if(shape_levels_count <= 5){
        shapes <- seq(21, 21 + shape_levels_count - 1)

        # If we run out of symbols....start at 3 (plus symbol)
      } else if(shape_levels_count > 5){
        shapes <- c(21, 22, 23, 24, 25)
        shapes <-  c(shapes, seq(3, 3 + shape_levels_count - 1))
      }

      # Diamond is not that distinctive so use triangle at three factors.
      if(length(shapes) == 3){
        shapes[shapes == 23] <- 24
      }
      # For logic also do this switch at > 3 symbols
      if(length(shapes) > 3){
        shapes[c(1, 2, 3, 4)] <- shapes[c(1, 2, 4, 3)]
      }

      # Symbol 23 (triangle is distinctive but larger than other fix here, a bit OCD ;-)
      cex    <- rep(0.8,  shape_levels_count)
      cex[c(3)] <- 0.7

      cex_mapping   <- cex[unclass(data[[shape_factor]])]
      shape_mapping <- shapes[unclass(data[[shape_factor]])]


      # Generate background colors
      color_mapping  <- c("white")

      # Generate border colors
      border_colors_mapping <- c("black")

      # Generate legend colors and shapes
      legend_colors <- c("white")
      legend_border_colors <- c("black")
      legend_shapes <- shapes

      # Generate legend  by combining color and shape levels
      legend_ <- as.vector(shape_levels)

      # Generate title
      legend_title <- paste("Legend for levels of factor: ", shape_factor)

      # Generate legend symbols
      col   <- legend_border_colors
      pch   <- legend_shapes
      pt.bg <- legend_colors

      # Generate legend columns
      ncol <- 4  # Adjust columns for readability
    }


  } else {

    # Generate colors and shapes
    color_mapping <- c("white")
    border_colors_mapping <- c("black")
    shape_mapping <- c(16)

  }


  panel.pearson <- function(x, y, ...) {
    # Bepaal de limieten van de x- en y-as
    xlim <- par("usr")[1:2]
    ylim <- par("usr")[3:4]

    # Bereken het middenpunt
    horizontal <- mean(xlim, na.rm = TRUE)
    vertical   <- mean(ylim, na.rm = TRUE)

    # Voeg de tekst toe
    text(horizontal, vertical,
         label = format(cor(x, y, use = "na.or.complete"), digits = 2),
         cex = 1)
  }


  # Open a PNG device to save the plot
  if(output_type != "png"){
    png_path_corplot <- paste(temp_output_dir, "CorPlot.png", sep = "_")
    # Use sub() to replace _CorPlot.png with _Legend.png
    png_path_legend <- sub("_CorPlot\\.png", "_Legend.png", png_path_corplot)
  }
  else if (output_type == "png"){
    png_path_corplot <- output_path

    # Check if png_path_corplot *DOES NOT* contain "_CorPlot.png"
    # grepl() returns TRUE if the pattern is found. Hence !
    if (!grepl("_CorPlot\\.png", png_path_corplot)) {
      png_path_legend <- sub("\\.png$", "_Legend.png", png_path_corplot)
    }
    else {
      #If "_CorPlot.png" IS present, replace with _Legend.png:
      png_path_legend <- sub("_CorPlot\\.png", "_Legend.png", png_path_corplot)
    }

    #use / instead of \


    message(paste0("Saving output in: ", png_path_corplot, " and ", png_path_legend))
  }

  png_path_corplot <- gsub(pattern = "\\\\", replacement = "/", x = png_path_corplot)
  png_path_legend  <- gsub(pattern = "\\\\", replacement = "/", x = png_path_legend)

  png(png_path_corplot,
      width = width,
      height = height,
      units = "cm",
      res = res,
      pointsize = pointsize
      )

  # Create the pairs plot
  pairs(
    data[, numeric_vars], # Use only numeric columns for scatterplots/correlations
    bg  = color_mapping,   # Background color of points based on factor levels
    col = border_colors_mapping,  # Border color of points based on factor levels
    pch = shape_mapping,  # Point shapes based on factor levels
    lwd = 0.5,
    font.labels = 1,
    cex.labels = 1,
    cex.axis = 0.9,
    # cex  = cex_mapping,
    upper.panel = panel.pearson # Pearson correlation in the upper triangle
  )

  # Close the PNG device
  grDevices::dev.off()

  if(output_type == "png"){
    if(open_generated_files == TRUE){
      # Open the plot file in windows
      f_open_file(png_path_corplot)
    }
  }

  # Print legend when factor_count > 0 and print legend = TRUE
  if(factor_count > 0 && print_legend == TRUE){
    print_legend <- TRUE
  } else {
    print_legend <- FALSE
  }

  if(print_legend == TRUE){

    # Open a PNG device for the legend
    png(png_path_legend,
        width  = 8,
        height = 8,
        units = "in",
        res = res
        )

    # Create an empty plot
    plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE)

    # Add a legend without a title
    legend_x <- par("usr")[1] # Center x-coordinate
    legend_y <- par("usr")[4]-0.1 # Adjust y-coordinate for spacing

    # Add legend entries
    legend(
      x = -0.15, y = legend_y,
      legend = legend_,
      col = col,
      pch = pch,
      pt.bg = pt.bg,
      # bg = rep(colors, each = length(shape_levels)),
      bty = "n",
      # horiz = TRUE,
      cex  = 0.8,
      ncol = ncol,
      inset = c(0, -0.1),
      xpd = TRUE,
      text.width = max(strwidth(legend_)) + 0.02
    )

    # Add a manually centered title
    text(-0.05, par("usr")[4]-0.05, labels = legend_title, cex = 1, pos = 4)

    # Close the PNG device
    grDevices::dev.off()


    if(output_type == "png"){
      # Open the legend file in windows
      if(open_generated_files == TRUE){
      f_open_file(png_path_legend)
      }
    }
  }

  if (output_type %in% c("word", "pdf")){
  # Create a temporary R Markdown file for pdf and word
  cat("
---
title: \"Correlation Plots\"
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


```{r, results='asis', fig.align = 'center', echo=FALSE}
cat(paste0('![](', png_path_corplot, ')'), '   \n  \n')
cat('   \n    \n&nbsp;  \n  \n')

```


```{r, results='asis', fig.align = 'center', echo=FALSE}

cat(paste0('![](', png_path_legend, ')'), '   \n  \n')

```
", file = temp_output_file)
  }
  else if (output_type == "rmd"){
    # Create rmd output
    rmd_output <- sprintf(
      "
\`\`\`{r, results='asis', fig.align = 'center', echo=FALSE}
cat(paste0('![](%s)'), '    \\n  \\n')
cat('    \\n    \\n&nbsp;  \\n \\n')
\`\`\`


\`\`\`{r, results='asis', fig.align = 'center', echo=FALSE}
cat(paste0('![](%s)'), '    \\n  \\n')
\`\`\`
",
      png_path_corplot, # Value for the first %s
      png_path_legend   # Value for the second %s
    )

    # Print the resulting R Markdown code
    cat(rmd_output)
  }


  if(output_type == "pdf" || output_type == "word"){

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

    # Show save location before knitting else it will not display in console.
    message(paste0("Saving output in: ", output_path))

    # Remove the temporary R Markdown file
    invisible(suppressWarnings(file.remove(temp_output_file)))
    invisible(suppressWarnings(file.remove(png_path_corplot)))
    invisible(suppressWarnings(file.remove(png_path_legend)))
  }

}
