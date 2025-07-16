#' Perform multiple \code{glm()} functions with diagnostics, assumption checking, and post-hoc analysis
#'
#' Performs Generalized Linear Model (GLM) analysis on a given dataset with options for
#' diagnostics, assumption checking, and post-hoc analysis. Several response parameters
#' can be analyzed in sequence and the generated output can be in various formats
#' ('Word', 'pdf', 'Excel').
#'
#' @param formula A formula specifying the model to be fitted. More response variables can be
#'   added using \code{-} or \code{+} (e.g., \code{response1 + response2 ~ predictor}) to do
#'   a sequential GLM for each response parameter.
#' @param family The error distribution and link function to be used in the model (default: gaussian()).
#'   This can be a character string naming a family function, a family function or
#'   the result of a call to a family function. (See \code{\link{family}} for details of family functions.)
#' @param data A data frame containing the variables in the model.
#' @param diagnostic_plots Logical. If \code{TRUE}, plots are included in the output files.
#' @param alpha Numeric. Significance level for tests. Default is \code{0.05}.
#' @param adjust Character string specifying the method used to adjust p-values
#'   for multiple comparisons. Available methods include:
#'   \describe{
#'     \item{"tukey"}{Tukey's Honest Significant Difference method}
#'     \item{"sidak"}{Šidák correction}
#'     \item{"bonferroni"}{Bonferroni correction}
#'     \item{"none"}{No adjustment}
#'     \item{"fdr"}{False Discovery Rate adjustment}
#'   } Default is \code{"sidak"}.
#' @param show_assumptions_text Logical. If \code{TRUE}, includes a short explanation about GLM assumptions in the output file.
#' @param type specifying the scale on which the emmeans posthoc results are presented, e.g. "link" to show results on the scale for which the variables are linear and "response" when you want to back transform the data to interpret results in the units of your original data (e.g., probabilities, counts, or untransformed measurements). Default is \code{"response"}.
#' @param close_generated_files Logical. If \code{TRUE}, closes open 'Excel' or 'Word' files depending on the output format. This to be able to save the newly generated file by the \code{f_aov()} function. 'Pdf' files should also be closed before using the function and cannot be automatically closed. Default is \code{FALSE}.
#' @param open_generated_files Logical. If \code{TRUE}, Opens the generated output files ('pdf', 'Word' or 'Excel') files depending on the output format. This to directly view the results after creation. Files are stored in tempdir(). Default is \code{TRUE}.
#' @param output_type Character string specifying the output format: \code{"pdf"}, \code{"word"}, \code{"excel"}, \code{"rmd"}, \code{"off"} (no file generated) or \code{"console"}. The option \code{"console"} forces output to be printed. Default is \code{"off"}.
#' @param output_file Character string specifying the name of the output file. Default is "dataname_glm_output".
#' @param output_dir Character string specifying the name of the directory of the output file. Default is \code{tempdir()}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory.
#' @param dispersion_test Logical for overdispersion test (default: TRUE).
#' @param influence_threshold Leverage threshold (default: 2).
#' @param ... Additional arguments passed to \code{glm()}.
#'
#' @details
#' The function first checks if all specified variables are present in the data and ensures that the response variable is numeric.
#'
#' It performs Analysis of Variance (ANOVA) using the specified formula and data. If \code{shapiro = TRUE}, it checks for normality of residuals using the Shapiro-Wilk test and optionally (\code{transformation = TRUE}) applies a data transformation if residuals are not normal.
#'
#' If significant differences are found in ANOVA, it proceeds with post hoc tests using estimated marginal means from \code{emmeans()} and Sidak adjustment (or another option of \code{adjust =}.
#'
#' More response variables can be added using \code{-} or \code{+} (e.g., \code{response1 + response2 ~ predictor}) to do a sequential \code{aov()} for each response parameter captured in one output file.
#'
#' Outputs can be generated in multiple formats ("pdf", "word", "excel" and "rmd") as specified by \code{output_type}. The function also closes any open 'Word' files to avoid conflicts when generating 'Word' documents. If \code{output_type = "rmd"} is used it is adviced to use it in a chunk with \{r, echo=FALSE, results='asis'\}
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
#' @return An object of class 'f_glm' containing results from \code{glm()}, diagnostics, and post-hoc tests. Using the option "output_type", it can also generate output in the form of: R Markdown code, 'Word', 'pdf', or 'Excel' files. Includes print and plot methods for 'f_glm' objects.
#'
#' @examples
#' # GLM Binomial example with output to console and MS Word file
#' mtcars_mod <- mtcars
#' mtcars_mod$cyl <- as.factor(mtcars_mod$cyl)
#'
#' glm_bin <- f_glm(vs ~ cyl,
#'                  family = binomial,
#'                  data = mtcars_mod,
#'                  output_type = "word",
#'                  # Do not automatically open the 'Word' file (Default is to open the file)
#'                  open_generated_files = FALSE)
#' print(glm_bin)
#'
#' \donttest{
#' # GLM Poisson example with output to rmd text
#' data(warpbreaks)
#'
#' glm_pos <- f_glm(breaks ~ wool + tension,
#'                  data = warpbreaks,
#'                  family = poisson(link = "log"),
#'                  show_assumptions_text = FALSE,
#'                  output_type = "rmd")
#' cat(cat(glm_pos$rmd))
#' }
#'
#' @export
#'
f_glm <- function(
          formula,                       # glm function formula
          family = gaussian(),           # family for the GLM
          data = NULL,                   # data.frame used for glm
          diagnostic_plots = TRUE,       # Show diagnostic plots in output files
          alpha = 0.05,                  # Significance level for tests
          adjust = "sidak",              # Method used to adjust p-values
          type = "response",             # Scale of emmeans posthoc results
          show_assumptions_text = TRUE,  # Print explanation about GLM assumptions
          dispersion_test = TRUE,        # Print dispersion test
          output_type = "off",           # Output type
          output_file = NULL,            # Output file name
          output_dir = NULL,             # Output directory
          save_in_wdir = FALSE,          # Save in working directory
          close_generated_files = FALSE, # Close open files
          open_generated_files = TRUE,   # Open files after creation
          influence_threshold = 2,       # Leverage threshold
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


  ##### Save dataframe name and Handle input from vectors (dataframe column) #####

  if(!is.null(data)){
    # Save dataframe name
    data_name <- deparse(substitute(data))

  } else if(is.null(data)){

    if(length(formula_extract_df_names(formula)) == 0){
      data_name <- "data"

    }
    else if(length(formula_extract_df_names(formula)) == 1){
      data_name <- formula_extract_df_names(formula)

    }
    else if(length(formula_extract_df_names(formula)) > 1){
      data_name <- paste(formula_extract_df_names(formula), collapse = "_")

    }

    # Make a data.frame based on the formula
    data <- formula_to_dataframe(formula)

    # Rewrite formula without data frame prefixes
    formula <- clean_formula(formula)

  }

  ##### File creation and output doc settings #####
  # Wrap lines in output document
  f_wrap_lines()

  # Create a list to store all outputs in this function
  output_list <- list()

  # Output file handling
  temp_output_dir  <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "glm_output.Rmd")
  file.create(temp_output_file)


  ##### Error checking, converting input, closing files #####
  # Parameter validation output_type
  if( !(output_type %in% c("pdf", "word", "excel", "rmd", "console" , "off")) ){
    stop("Character string specifying the output format (output_type = ) should be either: 'pdf', 'word', 'excel', 'console','rmd', 'off'")
  }

  # If there is not output_file name specified use the data_name
  if (output_type %in% c("pdf", "word", "excel") ){

    # close_generated_files if user wants to
    if(close_generated_files == TRUE && output_type == "word"){
      # Close all MS Word files to avoid conflicts (so save your work first)
      system("taskkill /im WINWORD.EXE /f")
    }

    if(close_generated_files == TRUE && output_type == "excel"){
      # Close all MS Word files to avoid conflicts (so save your work first)
      system("taskkill /im EXCEL.EXE /f")
    }


    if (is.null(output_file) ) {
      # Set the file name
      output_file  <- paste0(data_name,"_glm_output")
    }

    # If there is no output_dir specified and user setting is to save in working directory
    if(is.null(output_dir) && save_in_wdir == TRUE){
      # set the working dir to the location the file is saved
      output_dir <- getwd()

    }
    else if(is.null(output_dir) && save_in_wdir == FALSE){
      # Get the dirname of output_file
      output_dir <- dirname(output_file)

      # Check if there is a dir (path) in the output file, if not use tempdir()
      if(output_dir == "."){
        output_dir <- temp_output_dir
      }
    }

    # Stop if the output directory does not exist
    if (!dir_exists(output_dir)) {
      stop("The directory '", output_dir, "' does not exist.")
    }

    # dir_name is already extracted so rename file to basename.
    output_file <- basename(output_file)

  }


  ##### Evaluate family choice and Extract response variables #####
  # Family choice: make sure its correct input and not a function.
  # Handle character input like "gaussian"
  if (is.character(family)) {
    family <- get(family, mode = "function", envir = parent.frame())()
  }

  # Handle bare function input like gaussian()
  if (is.function(family)) {
    family <- family()
  }

  if(is.null(family$family)) {
    message(family)
    stop("'family' not recognized")
  }

  # Extract response variables from the left-hand side of the formula
  lhs <- all.vars(formula[[2]])  # Get LHS variables (response)
  response_names <- lhs

  # Extract predictor variables from the right-hand side of the formula
  predictor_names <- all.vars(formula[[3]]) # Get RHS variables (predictors)

  # Extract the right-hand side (RHS) of the formula as a string
  rhs <- deparse(formula[[3]])  # Preserve the RHS structure

  # Ensure response and predictors are in the data
  for (response in lhs) {
    if (!(response %in% names(data))) {
      stop(paste("Response variable", response, "not found in the data."))
      # Ensure the response variable is numeric
      response_var <- data[[response]]
      if (!is.numeric(response_var)) {
        stop("The response variable must be numeric.")
      }
    }
  }

  for (predictor in predictor_names) {
    if (!(predictor %in% names(data))) {
      stop(paste("Predictor variable", predictor, "not found in the data."))
    }
  }

  #### Define Core GLM posthoc and diagnostic functions -----------------------

  check_residuals <- function(model, plot = TRUE) {

    sim_res <- DHARMa::simulateResiduals(model)

    temp_qq_plot <- tempfile(fileext = ".png")
    png(temp_qq_plot, width = 8, height = 5, units = "in", res = 600)
    plot(sim_res)
    dev.off()

    temp_residual_plot <- tempfile(fileext = ".png")
    png(temp_residual_plot, width = 8, height = 5, units = "in", res = 600)
    test_results <- DHARMa::testResiduals(sim_res)
    dev.off()


    return(list(
      test_results    = test_results,
      sim_res         = sim_res,
      path_qq_plot    = temp_qq_plot,
      path_residual_plot = temp_residual_plot
    ))
   }

   check_influence <- function(model) {

      hat_values  <- stats::hatvalues(model)
      avg_hat     <- mean(hat_values)
      influential <- which(hat_values > influence_threshold * avg_hat)

      return(list(
        hat_values = hat_values,
        influential_points = influential
      ))
  }

  # Post-hoc analysis with emmeans
  perform_posthoc <- function(model, adjust) {

      emm   <- emmeans::emmeans(model,
                                specs = all.vars(formula[[3]]),
                                type = type)

      pairs_emm <- pairs(emm, adjust = adjust)


      cld <- multcomp::cld(emm,
                           Letters = letters,
                           alpha = alpha,
                           adjust = adjust)

      return(list(emm = emm, pairs = pairs_emm, cld = cld))

  }

  #### Output formatting functions -------------------------------------
  word_pdf_preamble <- function(){

paste0("
---
title: \"f_glm Analysis Report\"
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
")
}

  glm_assumptions_text <- function(){
    paste0("
# Generalized Linear Models (GLMs)

GLMs extend linear regression to response variables with non-normal distributions by specifying a link function and a probability distribution. The **link function** connects the mean of the response variable to the linear predictors. The choice of family and link function depends on the nature of your response variable.


| Family             | Typical Use Case               | Canonical Link     | Other Common Links        | R Syntax Example                      |
|--------------------|--------------------------------|--------------------|---------------------------|---------------------------------------|
| **Gaussian**       | Continuous, unbounded          | Identity           | Log, Inverse              | `gaussian(link = 'identity')`         |
| **Binomial**       | Binary or proportion           | Logit              | Probit, Cloglog, Log      | `binomial(link = 'logit')`            |
| **Poisson**        | Counts (no upper bound)        | Log                | Identity, Sqrt            | `poisson(link = 'log')`               |
| **Gamma**          | Positive, skewed continuous    | Inverse            | Log, Identity             | `Gamma(link = 'inverse')`             |
| **Inverse Gaussian** | Positive, highly skewed      | 1/mu$^2$           | Log, Identity, Inverse    | `inverse.gaussian(link = '1/mu^2')`   |
| **Quasi**          | Flexible (over/underdispersion)| Identity           | Log, others               | `quasi(link = 'identity')`            |
| **Quasibinomial**  | Flexible binomial              | Logit              | Probit, Cloglog, Log      | `quasibinomial(link = 'logit')`       |
| **Quasipoisson**   | Flexible Poisson               | Log                | Identity, Sqrt            | `quasipoisson(link = 'log')`          |

## Link Functions:\n

- *Identity*: \\( g(\\mu) = \\mu \\)
- *Log*: \\( g(\\mu) = \\log(\\mu) \\)
- *Logit*: \\( g(\\mu) = \\log\\left(\\frac{\\mu}{1-\\mu}\\right) \\)
- *Probit*: \\( g(\\mu) = \\Phi^{-1}(\\mu) \\) (inverse normal CDF)
- *Complementary log-log (cloglog)*: \\( g(\\mu) = \\log(-\\log(1-\\mu)) \\)
- *Inverse*: \\( g(\\mu) = 1/\\mu \\)
- *Inverse-square*: $g(\\mu) = 1/\\mu^2$
- *Square root*: $g(\\mu)= \\sqrt{\\mu}$


## How to choose:\n

- The *family* should match the distribution of your response variable.
- The *link function* should reflect the relationship between predictors and the mean of the response; the canonical link is often a good starting point, but alternatives may be appropriate for specific scientific or interpretive reasons.


## GLM Assumptions\n

1. Correct distributional family.
2. Appropriate link function.
3. Independence of observations.
4. Linear relationship between predictors and link-transformed response for poisson and binomial families.
5. No overdispersion.\n

## DHARMa Diagnostics: QQ Plot (Left Diagnostic Plot)

The DHARMa package (short for Diagnostics for HierArchical Regression Models) in R can plot a QQ plot and a residual plot. In addition the QQ plot contains three tests:  \n     \n&nbsp;  \n   \n

The **KS test** (Kolmogorov-Smirnov) in DHARMa evaluates whether model residuals conform to the expected uniform distribution under the assumption of a well-specified model. Significant deviations (low p-values) suggest potential model misspecification, such as incorrect distributional assumptions or unaccounted patterns. Note: The KS test becomes highly sensitive with large datasets, often flagging minor deviations as statistically significant. Always evaluate residual plots alongside test results.   \n     \n&nbsp;  \n   \n

The **dispersion test** evaluates whether a model's residuals exhibit overdispersion (greater variance than expected) or underdispersion (less variance than expected).  \n     \n&nbsp;  \n   \n

The **outlier test** evaluates whether the number of residuals falling outside the simulated data range (0 or 1 in scaled residuals) deviates from expectations under the model. A low p-value indicates more/fewer outliers than expected, but graphical diagnostics should guide final decisions.  \n     \n&nbsp;  \n   \n

## DHARMa Diagnostics: Residuals vs Model Predictions (Right Diagnostic Plot)

DHARMa residuals are simulation-based, scaled (quantile) residuals designed to mimic the behavior of residuals from a well-fitted linear regression. When the model fits well, these residuals should ideally follow a uniform distribution between 0 and 1. The plot shows residuals (ranging from 0 to 1) on the Y-axis, indicating how each observation compares to simulated values, and model predictions (typically fitted values) on the X-axis. For a well-fitting model, residuals should be randomly scattered around 0.5 across the range of predicted values. Outliers, i.e. observations more extreme than any of the simulations, are highlighted as red stars at residual values of 0 or 1.  \n     \n&nbsp;  \n   \n

**If residuals are NOT randomly scattered:**

- Funnel/fan shape: Suggests heteroscedasticity (variance changes with predictions).
- U-shaped or humped pattern: Indicates nonlinearity or missing predictors.
- Consistent high/low residuals: Shows systematic over- or underestimation in certain prediction ranges.
- Outliers: May signal errors or model misspecification, but their frequency also depends on the number of simulations.
\n"
  )
 }

  #### Main analysis function and function combinations ------------------------------
  generate_report <- function(output = TRUE) {

    # Create a list to store all outputs in this function
    output_list <- list()

    # Loop for several response parameters
    for (response_name in lhs) {

    # Create a new formula for each response, preserving interactions
    current_formula <- as.formula(paste0(response_name, "~", rhs))

    # GLM model fitting
    glm_fit <- stats::glm(current_formula,
                          family = family,
                          data = data,
                          ...)

    # Update the call info in glm_fit, did not succeed in family update.
    glm_fit$call$formula <- current_formula
    glm_fit$call$data    <- data_name

    # Store the GLM model object in output_list
    output_list[[response_name]][["model"]] <- glm_fit

    # Store GLM model summary object in output_list
    output_list[[response_name]][["summary"]] <- summary(glm_fit)

    # Diagnostic checks
    output_list[[response_name]][["diagnostics"]] <- list(
      show_plot  = diagnostic_plots,
      residuals  = check_residuals(glm_fit, plot = diagnostic_plots),
      influence  = check_influence(glm_fit)
    )

    # Post-hoc analysis
    sig_effects <- which(summary(glm_fit)$coefficients[,4] < alpha)

    if(length(sig_effects) > 0) {
      output_list[[response_name]][["posthoc"]] <- perform_posthoc(glm_fit, adjust)
    }
    else {
      output_list[[response_name]][["posthoc"]] <- perform_posthoc(glm_fit, adjust)
      output_list[[response_name]][["posthoc"]][["cld"]] <-
        "No significant differences found based on GLM."
    }

    # Create output and text for knitr
    cat("   \n  \n
# GLM of: ", response_name, "  \n"
)
    cat("
## Model Diagnostics of: ", response_name, "\n   \n"
)
    if( output_list[[response_name]][["diagnostics"]][["show_plot"]] == TRUE){

      path_qq_plot <-
        output_list[[response_name]][["diagnostics"]][["residuals"]][["path_qq_plot"]]

      cat(paste0("![](", path_qq_plot, ")"), "   \n  \n")

      # Pagebreak
      cat("
<div style=\"page-break-after: always;\"></div>
\\newpage")
    }

    # Print the glm and posthoc
    cat("\n
## Model Summary of: ", response_name, "\n   \n")

    # Because the chunk will be results='asis' I construct a nested chunk
    # to get a nice print
    captured_output_summary <-
      capture.output(output_list[[response_name]][['summary']])

    # Print the captured output within a fenced code block
    cat("\n```r\n")
    cat(paste(captured_output_summary, collapse = "\n"))
    cat("\n```\n")

    cat("\n
\n## Model Post-hoc Analysis of: ", response_name, "\n   \n")

    # Build nested chunk
    captured_output_cld <-
      capture.output(output_list[[response_name]][['posthoc']][['cld']])
    # Print the captured output within a fenced code block
    cat("\n```r\n")
    cat(paste(captured_output_cld, collapse = "\n"))
    cat("\n```\n")

    }

    if (output == TRUE) {
      return(output_list)
    }
  }

  # Execute analysis and return results but hide this from console.
  sink(tempfile())
  capture.output(output_list <- generate_report())
  class(output_list) <- "f_glm"
  sink()

  # Here the documents are constructed.
  if(output_type %in% c("word", "pdf")) {

    if (output_type == "word") { file.ext <- ".docx" }
    if (output_type == "pdf")  { file.ext <- ".pdf"  }

    # Prevent ## before printed output
    knitr::opts_chunk$set(comment = "")

    # Show save location before knitting else it will not display in console.
    message(paste0("Saving output in: ", output_dir, "\\", output_file, file.ext))

    # re-run generate_report, but this time capture its output to a string
    generated_markdown <- capture.output(generate_report(output = FALSE))

    # Combine the preamble, assumptions, and the captured report into one string
    rmd_content <- paste(
      word_pdf_preamble(),
      if (show_assumptions_text) glm_assumptions_text() else "",
      # The captured output already contains the necessary markdown formatting and image links
      paste(generated_markdown, collapse = "\n"),
      sep = "\n"
    )

    # Write the complete Rmd content to the temp file
    writeLines(rmd_content, temp_output_file)

    # Create the RMarkdown file
    rmarkdown::render(
      temp_output_file,
      output_file = output_file,
      output_dir = output_dir,
      intermediates_dir = temp_output_dir,
      knit_root_dir = temp_output_dir,
      quiet = TRUE,
      output_format = paste0(output_type, "_document")
    )

    # Open files after creation
    if(open_generated_files == TRUE){
    # Open the file with default program
    f_open_file(paste0(output_dir, "/", output_file, file.ext))
    }

    return(invisible(output_list))

  } else if(output_type == "excel") {
    # set the wd to the location the file is saved
    file.ext <- ".xlsx"
    # Show save location before knitting else it will not display in console.
    message(paste0("Saving output in: ", output_dir, "\\", output_file, file.ext))

    # Extract all post_hoc_summary_table tables and keep their names
    post_hoc_tables <- lapply(output_list, function(obj)
      obj$posthoc$cld)

    # Assign names to the list for Excel sheet names based on response names
    names(post_hoc_tables) <- response_names

    # Write to an Excel file with each table in its own sheet
    write_xlsx(post_hoc_tables, path = paste0(output_dir, "/", output_file, file.ext))

    # Open files after creation
    if(open_generated_files == TRUE){
    f_open_file(paste0(output_dir, "/", output_file, file.ext))
    }

    return(invisible(output_list))
  }

  if(output_type == "rmd"){

    if (is.null(opts_knit$get("output.dir"))) {
      opts_knit$set(output.dir = tempdir())
    }

    # Re-capture the markdown text for the rmd output
    generated_markdown <- capture.output(generate_report(output = FALSE))

    clean_rmd_output <- paste(
      if (show_assumptions_text) glm_assumptions_text() else "",
      paste(generated_markdown, collapse = "\n"),
      sep = "\n"
    )
    output_list[["rmd"]] <- clean_rmd_output

    return(invisible(output_list))

  } else if(output_type %in% c("console")){
    #Print output list to the console (forced)
    print(output_list)

    return(output_list)

  } else if(output_type %in% c("off")){
       return(invisible(output_list))

  } else {
    warning("Invalid output format specified. No file generated.")

  }


  invisible(suppressWarnings(file.remove(temp_output_file)))

}

# Print method for f_glm objects
#' @export
print.f_glm <- function(x, ...) {

  # Loop over each category (a, b, etc.)
  for (category in names(x)) {

  # Get the sublist for this category
  sublist <- x[[category]]
  cat("==========================================\n")
  cat("   GLM of repsone variable:", category, "\n")
  cat("==========================================\n")
  print(sublist$summary)

  cat("\nPost-hoc Comparisons of:", category, "\n")
  cat("_________________________________________\n")
  print(sublist$posthoc$cld)
  }

}

#' @export
plot.f_glm <- function(x, which = 1:2, ...) {

  # Loop over each category (a, b, etc.)
  for (category in names(x)) {
    # Get the sublist for this category
    sublist <- x[[category]]

  # 1: Q-Q plots
    plot(sublist$diagnostics$residuals$sim_res)

  # 2: Histograms
    DHARMa::testResiduals(sublist$diagnostics$residuals$sim_res)
    }
}


