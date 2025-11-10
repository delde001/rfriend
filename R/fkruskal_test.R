#' Perform multiple Kruskal-Wallis tests with a user-friendly output file, do data inspection and Dunn's test (of 'rstatix') as post hoc.
#'
#' Performs the Kruskal-Wallis rank sum test to assess whether there are statistically significant differences between three or more independent groups. It provides detailed outputs, including plots, assumption checks, and post-hoc analyses using Dunn's test. Results can be saved in various formats ('pdf', 'Word', 'Excel', or console only) with customizable output options.
#'
#' @param formula A formula specifying the response and predictor variable (e.g., \code{response ~ predictor)}.
#' more response variables and predictors can be added using \code{-} or \code{+} (e.g., \code{response1 + response2 ~ predictor1 + predictor2)}. The function iterates through these combinations or response and predictors, because the Kruskal-Wallis test itself only allows one response and one predictor combination to be tested simultaneously.
#' @param data A \code{data.frame} containing the variables referenced in the formula.
#' @param plot Logical. If \code{TRUE}, generates plots (e.g., density plots and boxplots)
#'   in the output files. Default is \code{TRUE}.
#' @param alpha Numeric. The significance level for the Kruskal-Wallis test and Dunn's
#'   test. Default is  \code{0.05}.
#' @param adjust Character string. Adjustment method for pairwise comparisons in Dunn's test. Options include \code{"holm", "hommel", "bonferroni", "sidak", "hs", "hochberg", "bh", "by", "fdr"} or \code{"none"}. Default is \code{"bonferroni"}, if you don't want to adjust the p value (not recommended), use  \code{p.adjust.method = "none"}.
#' @param intro_text Logical. If \code{TRUE}, includes a section about Kruskal-Wallis test assumptions in the output document. Default is \code{TRUE}.
#' @param close_generated_files Logical. If \code{TRUE}, closes open 'Excel' or 'Word' files depending on the output format. This to be able to save the newly generated file by the \code{f_aov()} function. 'Pdf' files should also be closed before using the function and cannot be automatically closed. Default is \code{FALSE}.
#' @param open_generated_files Logical. If \code{TRUE}, Opens the generated output files ('pdf', 'Word' or 'Excel') files depending on the output format. This to directly view the results after creation. Files are stored in tempdir(). Default is \code{TRUE}.
#' @param output_type Character string specifying the output format: \code{"pdf"}, \code{"word"}, \code{"excel"}, \code{"rmd"}, \code{"console"} or \code{"off"} (no file generated). The option \code{"console"} forces output to be printed, the option \code{"rmd"} saves rmd code in the output object not in a file. Default is \code{"off"}.
#' @param save_as Character string specifying the output file path (without extension).
#'   If a full path is provided, output is saved to that location.
#'   If only a filename is given, the file is saved in \code{tempdir()}.
#'   If only a directory is specified (providing an existing directory with trailing slash),
#'   the file is named "dataname_Kruskal_Wallis_output" in that directory. If an extension is provided the output format specified with option "output_type" will be overruled.
#'   Defaults to \code{file.path(tempdir(), "dataname_summary.pdf")}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory. Default is \code{FALSE}, this avoid unintended changes to the global environment. If \code{save_as} location is specified \code{save_in_wdir} is overwritten by \code{save_as}.

#' @return An object of class 'f_kruskal_test' containing:
#' \itemize{
#' \item Kruskal-Wallis test results for each combination of response and predictor variables.
#' \item Dunn's test analysis results (if applicable).
#' \item Summary tables with compact letter displays for significant group differences.
#' }
#'
#' Using the option \code{output_type}, it can also generate output in the form of: R Markdown code, 'Word', 'pdf', or 'Excel' files. Includes print and plot methods for 'f_kruskal_test' objects.
#'
#' @details
#' This function offers a comprehensive workflow for non-parametric analysis using the Kruskal-Wallis test:
#' \itemize{
#' \item Assumption Checks: Optionally includes a summary of assumptions in the output.
#' \item Visualization: Generates density plots and boxplots to visualize group distributions.
#' \item Post-hoc Analysis: Conducts Dunn's test with specified correction methods if significant differences are found.
#'}
#'-----------\cr
#'
#' Output files are generated in the format specified by \code{output_type =} and saved to the working directory, options are \code{"pdf", "word"} or \code{"excel"}. If \code{output_type = "rmd"} is used it is adviced to use it in a chunk with \{r, echo=FALSE, results='asis'\}
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
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # Example usage:
#' data(iris)
#'
#' # Perform Kruskal-Wallis test on Sepal.Length and Sepal.Width by Species
#' # with "holm" correction for posthoc dunn_test, without showing the output.
#' output <- f_kruskal_test(
#'                Sepal.Width + Sepal.Length ~ Species,
#'                data = iris,
#'                plot = FALSE,
#'                output_type = "word",
#'                adjust = "holm",
#'                open_generated_files = FALSE
#'                )
#'
#' # Save Kruskal-Wallis test and posthoc to Excel sheets: Sepal.Width and Sepal.Length.
#' f_kruskal_out <- f_kruskal_test(
#'                      Sepal.Width + Sepal.Length ~ Species,
#'                      data = iris,
#'                      plot = FALSE,
#'                      output_type = "excel",
#'                      adjust = "holm",
#'                      open_generated_files = FALSE
#'                      )
#'
#' @export
f_kruskal_test <- function(
  formula,              # Kruskall-Wallis function formula
  data = NULL,          # data.frame used for Kruskall-Wallis
  plot = TRUE,          # Show plots in output files
  alpha = 0.05,         # Set significance level alpha for Kruskall and dunn_test
  output_type = "off",
  save_as = NULL,       # Specify the name of the file.
  save_in_wdir = FALSE, # Save file output in the working directory.
  intro_text = TRUE, # Print a short explanation about Kruskall-Wallis assumptions in the pdf or word output file
  adjust = "bonferroni",           # Correction Method for pairwise comparisson in dunn_test.
  close_generated_files = FALSE,   # Closes either open excel or word files depending on the output format.
  open_generated_files = TRUE      # Open files after creation
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

  # Generate a temporary file path for "output.Rmd"
  temp_output_dir  <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")

  # Create the output file "output.Rmd" in tempdir()
  file.create(temp_output_file)

  # Wrap lines in rmd output document
  f_wrap_lines()

  if( !(output_type %in% c("pdf", "word", "excel", "rmd", "console" , "off")) ){
    stop("Character string specifying the output format (output_type = ) should be either: 'pdf', 'word', 'excel', 'console','rmd', 'off'")
  }

  ##### Handle option "save_as = " #####
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
                                   default_name = paste(data_name, "Kruskal_Wallis_output", sep = "_"),
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
                                   default_name = paste(data_name,
                                                        "Kruskal_Wallis_output",
                                                        sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = file.ext
      )


    }
    else if(exists("file_extension")) {

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name,
                                                        "Kruskal_Wallis_output",
                                                        sep = "_"),
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
                                 default_name = paste(data_name,
                                                      "Kruskal_Wallis_output",
                                                      sep = "_"),
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

  # Create a list to store all outputs in this function
  output_list <- list()


  # Extract response variables from the left-hand side of the formula
  lhs <- all.vars(formula[[2]])  # Get LHS variables (response)
  response_names <- lhs

  # Extract predictor variables from the right-hand side of the formula
  rhs <- all.vars(formula[[3]])# Get RHS variables (predictors)


  # Ensure response and predictors are in the data
  for (response in lhs) {
    if (!(response %in% names(data))) {
      stop(paste("Response variable", response, "not found in the data."))
      }
  }


  for (predictor in rhs) {
    if (!(predictor %in% names(data))) {
      stop(paste("Predictor variable", predictor, "not found in the data."))
     }

    # Ensure all predictor variables are factors
    data[[predictor]] <- as.factor(data[[predictor]])
  }



  generate_report <- function(output = TRUE) {

  # This text reminds the user of the assumptions of an intro_text it its show by default
  # but can be hidden.
    if(intro_text == TRUE){

      cat("
# Assumptions of the Kruskal-Wallis Test
The Kruskal-Wallis test is a non-parametric test used to assess whether there are statistically significant differences between the medians of three or more independent groups. It does not require the data to be normally distributed, making it suitable for ordinal or skewed continuous data. However, the test assumes that data within each group are similarly distributed. Below are its key assumptions:  \n   \n

1.	**Independence of Observations:**
Data points must be independent within and across groups, with no overlap or relationships between groups.

2.	**Measurement Scale:**
The dependent variable should be ordinal or continuous (interval or ratio), but not nominal.

3.	**Similar Distribution Shapes:**
The dependent variable should have a similar distribution shape across groups, as differing shapes can affect median comparisons. If this assumption is violated, the test may not accurately compare medians but instead reflect differences in overall distributions.

4.	**Random Sampling:**
Samples should be randomly drawn from their populations to ensure representativeness.

5.	**Adequate Sample Size:**
Each group should ideally include at least five observations for reliable results.   \n   \n


<div style=\"page-break-after: always;\"></div>
\\newpage")
    }
    #create count to remove last page break
    i <- 0
    #Main loop starts here
    for (response_name in lhs) {
      for (predictor_name in rhs) {
        p <- NULL
        d <- NULL
        letter_df <- NULL
        cld_letters <- NULL
        i <- i+1
      # Create a new formula for each response
      current_formula <- as.formula(paste0(response_name, "~", predictor_name))

      cat("   \n#  Analysis of: ", response_name, " by ", predictor_name,"  \n")
      cat("   \n     \n&nbsp;  \n   \n ")

      if(plot == TRUE){
      cat("  \n## Visual check on similarity of distributions  \n")

      d <- ggplot(data, aes(x = !!sym(response_name), fill = factor(!!sym(predictor_name)))) +
        geom_density(alpha = 0.4) +
        labs(title = "Density Plot by Group", x = predictor_name)

      # Print d, i.e. distributions plot
      # Create a temporary file path with a .png extension
      temp_file_path_d <- tempfile(fileext = ".png")

      # Save the plot, specifying the device as "pdf"
      suppressMessages(ggsave(filename = temp_file_path_d, plot = d))

      # Include the saved plot in R Markdown
      cat(paste0("![](", temp_file_path_d, ")"), "   \n  \n")
      cat("&nbsp;\n   \n")

      }

      output_list[[paste0(response_name,"_",predictor_name)]][["distributions"]] <- d
      # Store alpha and adjust for print.class function
      output_list[[paste0(response_name,"_",predictor_name)]][["alpha"]]          <- alpha
      output_list[[paste0(response_name,"_",predictor_name)]][["DunnTest_adjust"]]<- adjust

      # cat("   \n     \n&nbsp;  \n   \n ")
      if(output_type != "rmd"){
        # Pagebreak
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage")
      }

      cat("
\n## Result of Kruskal-Wallis rank sum test  \n  \n")
      # Perform the Kruskal-Wallis test
      kruskal.test_result <- kruskal.test(current_formula, data = data)
      # Show kruskal.test_result in ouput document
      f_pander(kruskal.test_result)
#       # Pagebreak
#       cat("   \n   \n
# <div style=\"page-break-after: always;\"></div>
# \\newpage
#       ")
      # Store Kruskal-Wallis test in ouput
      output_list[[paste0(response_name,"_",predictor_name)]][["kruskal.test"]] <- kruskal.test_result

      # Create data summary table for output and store in output_list
      summary_table <- f_summary(data,
                                 response_name,
                                 predictor_name,
                                 show_sd = FALSE,
                                 show_se = FALSE,
                                 eval_input = TRUE,
                                 digits = NULL
                                 )$output_df
      output_list[[paste0(response_name,"_",predictor_name)]][["summary_table"]] <- summary_table

      if(kruskal.test_result$p.value < alpha){

      cat("
  \n  \n## Result of Dunn's test post-hoc test  \n   \n")
      # Conduct Dunn's test for post-hoc analysis
      dunn_test_result <- rstatix::dunn_test(current_formula,
                                    data = data,
                                    p.adjust.method = adjust)
      cat("
Dunn (1964) Kruskal-Wallis multiple comparison.  \n")
      f_pander(dunn_test_result)
      cat("
  \n   \np-values (p) were adjusted with ", adjust, " (p.adj) \n")
      cat("
Group 1 and group 2 indicate the compared groups with respectively n1 and n2 replicates. The **statistic** column represents the z-test statistic for each pairwise comparison in the Dunn test. This statistic is derived by standardizing the difference in mean ranks between two groups using the pooled standard error. It follows the standard normal distribution under the null hypothesis, which assumes no difference in ranks between groups.

- A **positive Z** value indicates that the first group in the comparison has higher ranks (on average) than the second group.
- A **negative Z** value indicates that the second group has higher ranks (on average) than the first group.    \n   \n")

      if(output_type != "rmd"){
        # Pagebreak
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage")
      }

      # Store dunn_test_result in ouput
      output_list[[paste0(response_name,"_",predictor_name)]][["dunn_test"]] <- dunn_test_result

      # Extract p-values and convert to a compact letter display
      dunn_pvalues <- dunn_test_result$p.adj
      names(dunn_pvalues) <- paste0(dunn_test_result$group1,"-",dunn_test_result$group2)

      # Can be that spaces are added to names in group remove these by:
      names(dunn_pvalues) <- lapply(names(dunn_pvalues), function(x) gsub(" ", "", x))
      cld <- multcompLetters(dunn_pvalues, threshold = alpha)

      # Create a data frame with letters for each group
      letter_df <- as.data.frame(cld$Letters)
      letter_df[[predictor_name]] <- row.names(letter_df)
      colnames(letter_df)[colnames(letter_df) == "cld$Letters"] <- "cld_letters"



      if(plot == TRUE){

      cat("  \n## Boxplot of: ", response_name, " by ", predictor_name,"  and Dunn's test post-hoc test  \n  \n")
      # Add the compact letter display to the data
      data2 <- merge(data, letter_df, by = predictor_name, all.x = TRUE)

      # Set the location for the letters in the boxplot.
      y_max <- max(data[[response_name]], na.rm = TRUE)
      y_min <- min(data[[response_name]], na.rm = TRUE)

      # Calculate a proportional position above the top of the plot
      y_position <- y_max + 0.08 * (y_max - y_min)  # 5% above ymax

      # Create the boxplot
      p <- ggplot(data2, aes(x = !!sym(predictor_name), y = !!sym(response_name))) +
        geom_boxplot() +
        geom_jitter(width = 0.2, alpha = 0.5) +
        geom_text(
          data = letter_df,
          aes(y = y_position, label = cld_letters[match(!!sym(predictor_name),  !!sym(predictor_name))])
        ) +
        labs(x = predictor_name, y = response_name) +
        theme_bw()

      # Create a temporary file path with a .png extension
      temp_file_path_p <- tempfile(fileext = ".png")

      # Save the plot, specifying the device as "pdf"
      suppressMessages(ggsave(filename = temp_file_path_p, plot = p))

      # Include the saved plot in R Markdown
      cat(paste0("![](", temp_file_path_p, ")"), "   \n  \n")
      cat("&nbsp;\n   \n")

      output_list[[paste0(response_name,"_",predictor_name)]][["Boxplot"]] <- p
      cat("   \n   \np-values were adjusted with ", adjust," and a significance level of $\\alpha$ = ", alpha, " was used.  \n")
      cat("   \n   \n**NOTE 1:** Dunn's test does not assume normality but requires similar distribution shapes, differing only in location or median. If distributions vary in shape or spread, the results may be less reliable.   \n")
      cat("**NOTE 2:** Dunn's tests compares medians  not means. If two or more medians share the same grouping letter, we cannot show them to be different ($\\alpha$ =", alpha,"). Yet, we also did not show them to be the same.   \n")

      }

      # Create data summary with letters table for output and store in output_list
       summary_table <- merge(summary_table, letter_df, by = predictor_name, all.x = TRUE)
       output_list[[paste0(response_name,"_",predictor_name)]][["summary_table"]] <- summary_table
      } else {
        cat("No significant differences found based on Kruskal-Wallis rank sum test.\n")
      }

      if(output_type != "rmd"){
        # Pagebreak
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage")
      }

      cat("  \n## Data summary table  \n")
      f_pander(f_conditional_round(summary_table, digits = 2), line_break = 9)
      cat("   \n   \np-values were adjusted with ", adjust," and a significance level of $\\alpha$ = ", alpha, " was used.  \n")
      cat("**NOTE 1:** Dunn's test does not assume normality but requires similar distribution shapes, differing only in location or median. If distributions vary in shape or spread, the results may be less reliable.   \n")
      cat("**NOTE 2:** Dunn's tests compares medians not means. If two or more medians share the same grouping letter, we cannot show them to be different($\\alpha$ =", alpha,"). Yet, we also did not show them to be the same.   \n")


      if(output_type != "rmd" &&  i < length(lhs)){
        # Pagebreak
        cat("
\n    \n
<div style=\"page-break-after: always;\"></div>
\\newpage
        ")
      }
     }
    } #Main loop end
    if (output == TRUE) {
      return(output_list)
    }
  } # End generate report function.

  # Execute analysis and return results but hide this from console.
  sink(tempfile())
  capture.output(output_list <- generate_report())
  class(output_list) <- "f_kruskal_test"
  sink()


  # Here the documents are constructed.
  if (output_type %in% c("word", "pdf")) {

    # Show save location before knitting else it will not display in console.
    message(paste0("Saving output in: ", output_path))


    # Create a temporary R Markdown file
    word_pdf_preamble <- function(){ paste0("
---
title: \"Kruskal-Wallis Analysis Report\"
date: \"`r Sys.Date()`\"
output:
   word_document:
      reference_docx: !expr system.file(\"rmarkdown/templates/MS_word_style.docx\", package = \"rfriend\")
   pdf_document:
        latex_engine: pdflatex
header-includes:
  - \\usepackage[utf8]{inputenc}
  - \\DeclareUnicodeCharacter{03BB}{\\ensuremath{\\lambda}}
  - \\DeclareUnicodeCharacter{03B1}{\\ensuremath{\\alpha}}
---
")}

    # Prevent ## before printed output
    knitr::opts_chunk$set(comment = "")

    # re-run generate_report, but this time capture its output to a string
    generated_markdown <- capture.output(generate_report(output = FALSE))

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

  } else if (output_type == "excel") {

    # show the location were the file is saved
    message(paste0("Saving output in: ", output_path))

    # Extract all post_hoc_summary_table tables and keep their names
    post_hoc_tables <- lapply(output_list, function(obj)
      obj$summary_table)

    # Assign names to the list for Excel sheet names based on response names
    names(post_hoc_tables) <- response_names

    # Write to an Excel file with each table in its own sheet
    write_xlsx(post_hoc_tables, path = output_path)

    # Open files after creation
    if(open_generated_files == TRUE){
      f_open_file(output_path)
    }

    return(invisible(output_list))

  } else if (output_type == "rmd"){

    if (is.null(opts_knit$get("output.dir"))) {
      opts_knit$set(output.dir = tempdir())
    }

    # Re-capture the markdown text for the rmd output
    generated_markdown <- capture.output(generate_report(output = FALSE))

    clean_rmd_output <- paste(generated_markdown, collapse = "\n")

    output_list[["rmd"]] <- clean_rmd_output

    return(invisible(output_list))

  } else if (output_type == "off"){
    #Nothing to show output will be output_list.
    return(output_list)

  } else if (output_type == "console"){
    #Print output list to the console (forced)
    print(output_list)

    return(invisible(output_list))

  } else {
    warning("Invalid output format specified. No file generated.")

  }

  # Remove the temporary R Markdown file
  invisible(suppressWarnings(file.remove(temp_output_file)))

}

#' @export
print.f_kruskal_test <- function(x, ...) {

  # Create a flag to now if Dunn Test expl. should be printed.
  flag_dunnTest_used <- FALSE
  # Loop over each category (a, b, etc.)
  for (category in names(x)) {
    # Get the sublist for this category
    sublist <- x[[category]]

  cat("-------------------------------------------------\n")
  cat("Results of testing", sublist$kruskal.test$data.name)
  cat("\n-------------------------------------------------")

  print(sublist$kruskal.test)

  if(sublist$kruskal.test$p.value < 0.05){
  cat("\nSummary table with Dunn-Test Post-Hoc:", sublist$kruskal.test$data.name,"\n   \n")
  flag_dunnTest_used <- TRUE
  } else {
  cat("\nNo differences found, summary table of:", sublist$kruskal.test$data.name,"\n   \n")
  }

  # summary_table <- sublist$summary_table
  # names(summary_table) <- insert_newline(names(summary_table), 8)
  # table_text <- knitr::kable(summary_table, format = "simple")
  # cat(table_text, sep = "\n")

  f_pander(sublist$summary_table, line_break = 7)
  cat("\n")
  }

cat("-------------------------------------------------\n")
if(flag_dunnTest_used == TRUE){

  cat("all p-values were", sublist$DunnTest_adjust,"adjusted, and a significance level of \U03B1 = ", sublist$alpha, " was used.  \n  \n")
    cat("NOTE 1: Dunn's test does not assume normality but requires similar distribution shapes, \ndiffering only in location or median. \nIf distributions vary in shape or spread, the results may be less reliable.   \n  \n")
    cat("NOTE 2: Dunn's tests compares medians not means. \nIf two or more medians share the same grouping letter, we cannot show them to be different. \nYet, we also did not show them to be the same.   \n")
  }
}


