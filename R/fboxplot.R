#' Generate a Boxplot Report of a data.frame
#'
#' Generates boxplots for all numeric variables in a given dataset, grouped by factor variables. The function automatically detects numeric and factor variables. It allows two output formats ('pdf', 'Word') and includes an option to add a general explanation about interpreting boxplots.
#' @param formula A formula specifying the factor to be plotted. More response variables can be added using \code{-} or \code{+} (e.g., \code{response1 + response2 ~ predictor}) to generate multiple boxplots. If the formula is omitted and only \code{data} is provided all data will be used for creating boxplots.
#' @param data A \code{data.frame} containing the data to be used for creating boxplots.
#' @param fancy_names An optional named vector mapping column names in \code{data} to more readable names for display in plots (name map). Defaults to \code{NULL}.
#' @param output_type Character string, specifying the output format: \code{"pdf"}, \code{"word"}, \code{"rmd"} or \code{"png"}. The option \code{"rmd"} saves rmd code in the output object not in a file. Default is \code{"pdf"}.
#' @param save_as Character string specifying the output file path (without extension).
#'   If a full path is provided, output is saved to that location.
#'   If only a filename is given, the file is saved in \code{tempdir()}.
#'   If only a directory is specified (providing an existing directory with trailing slash),
#'   the file is named "dataname_BoxPlot" in that directory. If an extension is provided the output format specified with option "output_type" will be overruled.
#'   Defaults to \code{file.path(tempdir(), "dataname_BoxPlot.pdf")}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory. Default is \code{FALSE}, this avoid unintended changes to the global environment. If \code{save_as} location is specified \code{save_in_wdir} is overwritten by \code{save_as}.
#' @param close_generated_files Logical. If \code{TRUE}, closes open 'Word' files depending on the output format. This to be able to save the newly generated files. 'Pdf' files should also be closed before using the function and cannot be automatically closed.
#' @param open_generated_files Logical. If \code{TRUE}, Opens the generated output files ('pdf', 'Word' or 'png') files depending on the output format. This to directly view the results after creation. Files are stored in tempdir(). Default is \code{TRUE}.
#' @param boxplot_explanation A logical value indicating whether to include an explanation of how to interpret boxplots in the report. Defaults to \code{TRUE}.
#' @param detect_factors A logical value indicating whether to automatically detect factor variables in the dataset. Defaults to \code{TRUE}.
#' @param jitter A logical value, if \code{TRUE} all data per boxplot is shown, if \code{FALSE} (default) individual data points (except for outliers) are omitted.
#' @param width Numeric, png figure width default \code{8} inch
#' @param height Numeric, png figure height default \code{7} inch
#' @param units Character string, png figure units default \code{"in"} = inch, other options are: \code{"px"} = Pixels, \code{"cm"} = centimeters, \code{"mm"} = millimeters.
#' @param res Numeric, png figure resolution default 300 dpi
#' @param las An integer (\code{0} t/m \code{3}), \code{las = 0}: Axis labels are parallel to the axis. \code{las = 1}: Axis labels are always horizontal. \code{las = 2}: Axis labels are perpendicular to the axis. (default setting). \code{las = 3}: Axis labels are always vertical.
#'
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#' \item Detects numeric and factor variables in the dataset.
#' \item Generates boxplots for each numeric variable grouped by each factor variable.
#' \item Outputs the report in the specified format ('pdf', 'Word' or 'Rmd').
#'}
#'
#'If \code{output_type = "rmd"} is used it is adviced to use it in a chunk with \{r, echo=FALSE, results='asis'\}
#'
#' If no factor variables are detected, the function stops with an error message since factors are required for creating boxplots.
#'
#' This function will plot all numeric and factor candidates, use the function \code{subset()} to prepare a selection of columns before submitting to \code{f_boxplot()}.
#'
#' Note that there is an optional \code{jitter} option to plot all individual data points over the boxplots.
#'
#' This function requires [Pandoc](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3 or higher), a universal document converter.
#'
#' \bold{Windows:} Install Pandoc and ensure the installation folder \cr (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your system PATH.
#'
#' \bold{macOS:} If using Homebrew, Pandoc is typically installed in "/usr/local/bin". Alternatively, download the .pkg installer and verify that the binary’s location is in your PATH.
#'
#' \bold{Linux:} Install Pandoc through your distribution’s package manager (commonly installed in "/usr/bin" or "/usr/local/bin") or manually, and ensure the directory containing Pandoc is in your PATH.
#'
#' If Pandoc is not found, this function may not work as intended.
#'
#'
#' @return Generates a report file ('pdf' or 'Word') with boxplots and, optionally, opens it with the default program. Returns NULL (no R object) when generating 'pdf' or 'Word' files. Can also return R Markdown code or 'PNG' files depending on the output format.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' \donttest{
#' # Example usage:
#' data(iris)
#'
#' new_names = c(
#'   "Sepal.Length" = "Sepal length (cm)" ,
#'   "Sepal.Width" = "Sepal width (cm)",
#'   "Petal.Length" = "Petal length (cm)",
#'   "Petal.Width" = "Petal width (cm)",
#'   "Species" = "Cultivar"
#' )
#'
#' # Use the whole data.frame to generate a pdf report and don't open the pdf.
#' f_boxplot(iris, fancy_names = new_names, output_type = "pdf", open_generated_files = FALSE) #
#'
#' # Use a formula to plot several response parameters (response 1 + response 2 etc)
#' # and generate a rmd output without boxplot_explanation.
#' data(mtcars)
#' f_boxplot(hp + disp ~ gear*cyl,
#'            data=mtcars,
#'            boxplot_explanation = FALSE,
#'            output_type = "word",
#'            open_generated_files = FALSE) # Do not automatically open the 'Word' file.
#' }
#'
#' @export
f_boxplot <- function(

  data = NULL,         # data.frame used to plot box plot
  formula = NULL,      # function formula
  fancy_names = NULL,  # Optional mapping of column names to more readable names in plots (name_map).
  output_type = "pdf", # Output type can be word, pdf, rmd, console
  save_as = NULL,      # Specify the name of the output dir and file (name and type).
  save_in_wdir = FALSE,# Save file output in the working directory.
  close_generated_files = FALSE,# Closes either open word files depending on the output format.
  open_generated_files = TRUE,  # Open files after creation
  boxplot_explanation = TRUE,   # This text reminds the user on how to read a boxplot.
  detect_factors  = TRUE,       # Detect factors automatically (TRUE) or not (FALSE)
  jitter = FALSE,               # show individual data points
  #ouput png settings
  width = 8,
  height = 7,
  units = "in",
  res = 300,
  las = 2
)
{

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

  if( !(output_type %in% c("pdf", "word", "rmd", "png")) ){
    stop("Character string specifying the output format (output_type = ) should be either: 'pdf', 'word', 'rmd', 'png' ")
  }

  # Generate a temporary file path for "output.Rmd"
  temp_output_dir <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")

  # Create the output file "output.Rmd" in tempdir()
  file.create(temp_output_file)


  ####### Save dataframe name and Handle input from vectors (dataframe column) #####

  if(!is.null(data)){
    # Save dataframe name
    data_name <- deparse(substitute(data))

  } else if(is.null(data)){

    if(length(formula_extract_df_names(formula)) == 0){
      data_name <- "data"
    } else if(length(formula_extract_df_names(formula)) == 1){
      data_name <- formula_extract_df_names(formula)
    } else if(length(formula_extract_df_names(formula)) > 1){
      data_name <- paste(formula_extract_df_names(formula), collapse = "_")
    }

    # Make a data.frame based on the formula
    data <- formula_to_dataframe(formula)


    # Rewrite formula without data frame prefixes
    formula <- clean_formula(formula)

  }

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
    "excel"= ".xlsx",
    "png"  = ".png"
  )

  # If the user specifies a path, filename or save_in_wdir == TRUE an output_file should be created
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
                                   default_name = paste(data_name, "BoxPlot", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = ".docx"
      )

      #set output_type to default
      output_type <- "word"

    }
    else if(!exists("file_extension") && output_type %in% c("pdf", "word", "excel", "rmd", "png")){

      #create extension based on input_type
      file.ext <- unname(output_type_map[output_type])

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "BoxPlot", sep = "_"),
                                   default_dir  = save_dir,
                                   file.ext     = file.ext
      )



    }
    else if(exists("file_extension")) {

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "BoxPlot", sep = "_"),
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
                                 default_name = paste(data_name, "BoxPlot", sep = "_"),
                                 default_dir  = save_dir,
                                 file.ext     = file.ext
    )

  }

  # Do not run explanation code when output is png
  if(output_type == "png"){
  boxplot_explanation <- FALSE
  }


  if(output_type != "rmd"){
    if(close_generated_files == TRUE && output_type == "word"){
      # Close all MS Word files to avoid conflicts (so save your work first)
      system("taskkill /im WINWORD.EXE /f")
    }
  }

  # Wrap lines in rmd output document
  f_wrap_lines()

  # Create a counter for the number of factors and set it to zero
  factor_count <- 0



  # Automatically detect factor variables
  if (detect_factors == TRUE) {
    data <- f_factors(data)
  }

  # Rename the data.frame with fancy names if fancy_names are provided.
  if(!is.null(fancy_names)){
    data <- f_rename_columns(data, fancy_names)
  }


  # Count the number of factor variables
  factor_vars  <- vapply(data, is.factor, logical(1))
  factor_count <- sum(factor_vars)
  factor_vars  <- names(factor_vars)[factor_vars]

  if(factor_count == 0){
    stop("The data contains NO factor variable candidates, factors are required to make boxplots")
  }

  # Automatically detect numeric variables
  numeric_vars  <- vapply(data, is.numeric, logical(1)) # Identify numeric columns
  numeric_count <- sum(numeric_vars)
  numeric_vars  <- names(numeric_vars)[numeric_vars]


  if(!is.null(formula)){
    if(!is.null(fancy_names) ){
    formula <-  rename_formula_terms(formula, fancy_names)
    }
  # Extract response variables from the left-hand side of the formula
    numeric_vars <- all.vars(formula[[2]])  # Get LHS variables (response)
  # Extract the right-hand side (RHS) of the formula as a string
    RHS <- deparse(formula[[3]])  # Preserve the RHS structure
    factor_vars <- all.vars(formula[[3]])  # Get LHS variables (predictors)
  }


generate_report <- function(n) {
    # This text reminds the user on how to read a boxplot.
    if(boxplot_explanation == TRUE){

      cat("  \n  \n
# Understanding Boxplots: A Visual Guide

A **boxplot** (or box-and-whisker plot) is a statistical tool for visualizing the distribution of a dataset. It offers a quick snapshot of the dataset's central tendency, spread, and potential outliers. Below is a guide to help you read and interpret a boxplot effectively.

## Anatomy of a Boxplot

1. **The Five-Number Summary**:
   - **Minimum**: The smallest value in the dataset (excluding outliers).
   - **First Quartile (Q1)**: The value below which 25% of the data falls.
   - **Median (Q2)**: The middle value of the dataset, splitting it into two equal halves.
   - **Third Quartile (Q3)**: The value below which 75% of the data lies.
   - **Maximum (Q4)**: The largest value in the dataset (excluding outliers).

&nbsp;  \n   \n

2. **The Box**:
   - The box represents the **interquartile range (IQR)**, spanning from Q1 to Q3, which contains the middle 50% of the data.
   - A vertical line inside the box marks the **median**, indicating the central value of the dataset.

&nbsp;  \n   \n

3. **The Whiskers**:
   - Whiskers extend from the box to the smallest and largest data points within **1.5 times the IQR range** from Q1 and Q3.
   - Values outside this range are considered **outliers**.

&nbsp;  \n   \n

4. **Outliers**:
   - These are individual points plotted beyond the whiskers. They highlight extreme values that may warrant further investigation.
&nbsp;  \n   \n

5. **Mean**:
   - Some boxplots include the **mean** (average) as a distinct marker, often represented by a cross or dot.

&nbsp;  \n   \n")

invisible(image_path <- system.file('extdata', 'boxplot_explained_rfriend.png',
                                    package = 'rfriend'))
# Include the saved plots in R Markdown
cat(paste0("![](", image_path, ")"), "   \n  \n")

cat("
## How to Interpret a Boxplot

- **Center**: The **median** represents the central tendency of the data and serves as the most reliable summary measure, especially in the presence of outliers or skewed distributions.
- **Spread**:
  - The **IQR** (length of the box) captures the variability of the middle 50% of the data.
  - The whiskers show the overall spread of values within the dataset.
- **Skewness**:
  - If the median is closer to one end of the box or if one whisker is longer, the dataset is skewed in that direction. Normal distribution should be evenanly distributed around the median and the median and mean should be similar.
- **Outliers**: Points outside the whiskers indicate unusual or extreme values that could influence the dataset's overall analysis.


## Why and When to Use Boxplots?

Boxplots are particularly valuable when comparing distributions across multiple groups or datasets. They allow you to quickly assess differences in central tendency, variability, and the presence of outliers, making them a powerful tool for exploratory data analysis. If data is not normally distributed and the mean is not a good summary measure, scientist resort to boxplot instead of barplot in their publications.")

    if(output_type != "rmd"){
      # Pagebreak
      cat("
<div style=\"page-break-after: always;\"></div>
\\newpage")
    }
    }


  #Main loop starts here
  for (response_name in numeric_vars) {

    if (response_name != make.names(response_name)) {
      response_name_backt <- paste0("`", response_name, "`")
      response_name_quote <- paste0('"', response_name, '"')
    } else {
      response_name_backt <- response_name
      response_name_quote <- response_name
      }

      if(output_type != "png"){
      cat("  \n  \n#  Boxplots of: ", response_name, "  \n")
      }
      if(!is.null(formula)){
        if(output_type != "png"){
        cat("##  Boxplot of: ", response_name, " as function of: ", RHS, "  \n")
        }

        current_formula <- as.formula(paste(response_name_backt, "~", RHS))

        level_count <- NULL

        for(i in factor_vars){
          level_count[i] <- length(levels(data[[i]]))
        }

        factor_cols <- names(level_count)

        # Get the number of bars per boxplot and store in num.bars
        num.bars <- prod(level_count, na.rm = FALSE)

        # Define colors for boxplot
        c1 <- rainbow(num.bars)
        c2 <- rainbow(num.bars, alpha=0.2)
        c3 <- rainbow(num.bars, v=0.7)

        # Determine width of the boxes by making it relative to max here max boxes=9
        # this times two is 18, thus 9/18 = 0.5 the lower the num.bars the lower the relative width.
        box.width <- num.bars/18


        temp_file <- tempfile(fileext = ".png")
        png(temp_file, width = width, height = height, units = units, res = res)

        #Draw Boxplot (note gsub("(^')|('$)", "", resp$name[[i]]) removes the leading and tailing single quotes)
        boxp <- boxplot(current_formula,
                        data = data,
                        xlab = "",
                        ylab = response_name_backt,
                        las  = las,
                        #Create nice colors
                        col=c2, medcol=c3, whiskcol=c1, staplecol=c3, boxcol=c3, outcol=c3,
                        #Set box width using relative number of boxes to keep them more or less equal
                        boxwex = box.width,
                        #Let the width of the boxes depend on relative number of replicas: varwidth = TRUE
                        # varwidth = TRUE,
                        # Set margins where c(1=bottom, 2=left, 3=top and 4=right).
                        # par(mar = c(5,7,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1
                        par(mar = c(8,5,4,2)),
                        par(cex.lab=0.9), # is text size for y-axis
                        par(cex.axis=0.9) # is text size for x-axis
        )
        # Add jittered points
        if(jitter == TRUE){
          stripchart(current_formula,
                     data = data,
                     method = "jitter", # Adds random noise
                     pch = 19,          # Point style
                     vertical = TRUE,   # Align points vertically or not
                     cex = 0.7,         # Reduce point size
                     col = c1,          # Point color
                     add = TRUE)        # Overlay on the boxplot
        }

        # Calculate means for each group
        means <- tapply(data[[response_name]], interaction(data[factor_cols]), mean)

        # Add mean points to the plot
        points(seq_along(means), means, col = c3, pch = 10, cex = 1.5)


        # Allow plotting outside the plot region
        par(xpd = TRUE)

        # Get current plot limits
        usr <- par("usr")  # usr contains c(xmin, xmax, ymin, ymax)

        # Calculate a proportional position above the top of the plot
        y_position <- usr[4] + 0.05 * (usr[4] - usr[3])  # 5% above ymax

        #Get n
        x.pos.n <- rep(seq_along(boxp$n))
        text(x= x.pos.n, y = y_position, labels= paste0("n=", boxp$n))
        if(output_type != "png"){
        cat("   \n")
        }
        # Close the png file
        dev.off()

        if (output_type != "png"){
        # Include the saved plots in R Markdown
        cat(paste0("![](", temp_file, ")"), "   \n  \n")
        }

        if (output_type == "png"){

          # Define the new file name
          new_file_name <- paste0(dirname(output_path),"/",response_name,".png")

          # Rename the temporary file
          file.rename(temp_file, new_file_name)

          if (open_generated_files == TRUE){
            f_open_file(new_file_name)
          }
        }

    }
    else {
      for (factor_name in factor_vars) {

        if (factor_name != make.names(factor_name)) {
          factor_name_backt  <- paste0("`", factor_name, "`")
          factor_name_quote  <- paste0('"', factor_name, '"')
        } else {
          factor_name_quote  <- factor_name
          factor_name_backt   <- factor_name
          }

      # Create a new formula for each response
      current_formula <- as.formula(paste0(response_name_backt, "~", factor_name_backt))



      if(output_type != "png"){
      cat("##  Boxplot of: ", response_name, " as function of: ", factor_name, "  \n")
      }
      # Get the number of bars per boxplot and store in num.bars
      num.bars <- length(levels(data[[factor_name]]))

      # Define colors for boxplot
      c1 <- rainbow(num.bars)
      c2 <- rainbow(num.bars, alpha=0.2)
      c3 <- rainbow(num.bars, v=0.7)

      # Determine width of the boxes by making it relative to max here max boxes=9
      # this times two is 18, thus 9/18 = 0.5 the lower the num.bars the lower the relative width.
      box.width <- num.bars/18



      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = width, height = height, units = units, res = res)

      #Draw Boxplot (note gsub("(^')|('$)", "", resp$name[[i]]) removes the leading and tailing single quotes)
      boxp <- boxplot(current_formula,
                      data = data,
                      xlab = "",
                      ylab = response_name,
                      las  = las,
                #Create nice colors
                      col=c2, medcol=c3, whiskcol=c1, staplecol=c3, boxcol=c3, outcol=c3,
                #Set box width using relative number of boxes to keep them more or less equal
                      boxwex = box.width,
                #Let the width of the boxes depend on relative number of replicas: varwidth = TRUE
                      # varwidth = TRUE,
                # Set margins where c(1=bottom, 2=left, 3=top and 4=right).
                      # par(mar = c(5,7,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1
                      par(mar = c(8,5,4,2)),
                      par(cex.lab=0.9), # is text size for y-axis
                      par(cex.axis=0.9) # is text size for x-axis
      )
      # Add jittered points
      if(jitter == TRUE){
      stripchart(current_formula,
                 data = data,
                 method = "jitter", # Adds random noise
                 pch = 19,          # Point style
                 vertical = TRUE,   # Align points vertically or not
                 cex = 0.7,         # Reduce point size
                 col = c1,          # Point color
                 add = TRUE)        # Overlay on the boxplot
      }


      means <- tapply(data[[response_name]], data[[factor_name]], mean)

      # Add mean points to the plot
      points(seq_along(means), means, col = c3, pch = 10, cex = 1.5)


      # Allow plotting outside the plot region
      par(xpd = TRUE)

      # Get current plot limits
      usr <- par("usr")  # usr contains c(xmin, xmax, ymin, ymax)

      # Calculate a proportional position above the top of the plot
      y_position <- usr[4] + 0.05 * (usr[4] - usr[3])  # 5% above ymax

      #Get n
      x.pos.n <- rep(seq_along(boxp$n))
      text(x= x.pos.n, y = y_position, labels= paste0("n=", boxp$n))
      if(output_type != "png"){
        cat("   \n")
      }

      # Close the png file
      dev.off()

      if (output_type != "png"){
        # Include the saved plots in R Markdown
        cat(paste0("![](", temp_file, ")"), "   \n  \n")
      }

      if (output_type == "png"){
      # Define the new file name
      new_file_name <- paste0(dirname(output_path),"/",response_name,"_", factor_name, ".png")

      # Rename the temporary file
      file.rename(temp_file, new_file_name)

        if (open_generated_files == TRUE){
        f_open_file(new_file_name)
        }
      }

      } # End loop Factor variables
    } # End if statement
  } # End Loop Response variables
} # End generate_report function



# Here the documents are constructed.
if (output_type %in% c("word", "pdf")) {


  # Create a temporary R Markdown file
  word_pdf_preamble <- function(){ paste0( # Create a temporary R Markdown file
"
---
title: \"Boxplot Report\"
date: \"`r Sys.Date()`\"
output:
   word_document:
      reference_docx: !expr system.file(\"rmarkdown/templates/MS_word_style.docx\", package = \"rfriend\")
   pdf_document:
        latex_engine: pdflatex
header-includes:
  - \\usepackage[utf8]{inputenc}
  - \\DeclareUnicodeCharacter{03BB}{\\ensuremath{\\lambda}}
---
")}

  # Prevent ## before printed output
  knitr::opts_chunk$set(comment = "")

  # show the location were the file is saved
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

  # Remove the temporary R Markdown file
  invisible(suppressWarnings(file.remove(temp_output_file)))
  return(invisible(NULL))
  }
  else if (output_type == "rmd"){

    if (is.null(opts_knit$get("output.dir"))) {
      opts_knit$set(output.dir = tempdir())
    }

    # Re-capture the markdown text for the rmd output
    generated_markdown <- capture.output(generate_report())

    clean_rmd_output <- paste(generated_markdown, collapse = "\n")

    return(cat(clean_rmd_output))

  }
  else if (output_type == "png"){

    invisible(generate_report())

    message(paste0("PNG files saved in: ", dirname(output_path), "\n   \n"))

    return(invisible(NULL))
  }
}
