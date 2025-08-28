#' Perform multiple \code{aov()} functions with optional data transformation, inspection and Post Hoc test.
#'
#' Performs an Analysis of Variance (ANOVA) on a given dataset with options for (Box-Cox)
#' transformations, normality tests, and post-hoc analysis. Several response parameters can be analysed in sequence and the generated output can be in various formats ('Word', 'pdf', 'Excel').
#'
#' @param formula A formula specifying the model to be fitted. More response variables can be added using \code{-} or \code{+} (e.g., \code{response1 + response2 ~ predictor}) to do a sequential \code{aov()} for each response parameter.
#' @param data A data frame containing the variables in the model.
#' @param norm_plots Logical. If \code{TRUE}, plots are included in the output files. Default is \code{TRUE}.
#' @param ANCOVA Logical. If \code{TRUE}, prevents automatic conversion of predictors to factors, allowing for Analysis of Covariance (ANCOVA). Default is \code{FALSE}.
#' @param transformation Logical or character string. If \code{TRUE}, or if \code{"bestnormalize"}, applies \code{bestNormalize()} transformation if residuals are not normal. If \code{"boxcox"} applies a boxcox transformation. If \code{FALSE} no transformation will be applied. Default is \code{TRUE}.
#' @param alpha Numeric. Significance level for ANOVA, post hoc tests, and Shapiro-Wilk test. Default is \code{0.05}.
#' @param adjust Character string specifying the method used to adjust p-values
#'   for multiple comparisons. Available methods include:
#'   \describe{
#'     \item{"tukey"}{Tukey's Honest Significant Difference method, appropriate for
#'                   all pairwise comparisons. Controls family-wise error rate.}
#'     \item{"sidak"}{Šidák correction that controls the family-wise error rate.
#'                   Less conservative than Bonferroni.}
#'     \item{"bonferroni"}{Conservative adjustment that multiplies p-values by
#'                        the number of comparisons.}
#'     \item{"none"}{No adjustment. Equivalent to Fisher's LSD method.}
#'     \item{"fdr"}{False Discovery Rate adjustment, controls the expected proportion
#'                 of false positives among significant results.}
#'   } Default is \code{"sidak"}.
#' @param aov_assumptions_text Logical. If \code{TRUE}, includes a short explanation about ANOVA assumptions in the output file. Default is \code{TRUE}.
#' @param close_generated_files Logical. If \code{TRUE}, closes open 'Excel' or 'Word' files depending on the output format. This to be able to save the newly generated file by the \code{f_aov()} function. 'Pdf' files should also be closed before using the function and cannot be automatically closed. Default is \code{FALSE}.
#' @param open_generated_files Logical. If \code{TRUE}, Opens the generated output files ('pdf', 'Word' or 'Excel') files depending on the output format. This to directly view the results after creation. Files are stored in tempdir(). Default is \code{TRUE}.
#' @param output_type Character string specifying the output format: \code{"pdf"}, \code{"word"}, \code{"excel"}, \code{"rmd"}, \code{"console"} or \code{"off"} (no file generated). The option \code{"console"} forces output to be printed. Default is \code{"off"}.
#' @param output_file Character string specifying the name of the output file. Default is "dataname_aov_output".
#' @param output_dir Character string specifying the name of the directory of the output file. Default is  \code{tempdir()}. If the \code{output_file} already contains a directory name \code{output_dir} can be omitted, if used it overwrites the dir specified in \code{output_file}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory Default is \code{FALSE}, to avoid unintended changes to the global environment. If the \code{output_dir} is specified \code{save_in_wdir} is overwritten with \code{output_dir}.

#' @return An object of class 'f_aov' containing results from \code{aov()}, normality tests, transformations, and post hoc tests. Using the option "output_type", it can also generate output in the form of: R Markdown code, 'Word', 'pdf', or 'Excel' files. Includes print and plot methods for 'f_aov' objects.

#' @details
#' The function performs the following steps:
#' \itemize{
#' \item Check if all specified variables are present in the data.
#' \item Ensure that the response variable is numeric.
#' \item Perform Analysis of Variance (ANOVA) using the specified formula and data.
#' \item If \code{shapiro = TRUE}, check for normality of residuals using the Shapiro-Wilk test.
#' \item If residuals are not normal and \code{transformation = TRUE} apply a data transformation.
#' \item If significant differences are found in ANOVA, proceed with post hoc tests using estimated marginal means from \code{emmeans()} and Sidak adjustment (or another option of \code{adjust =}.
#' }
#'
#' More response variables can be added using \code{-} or \code{+} (e.g., \code{response1 + response2 ~ predictor}) to do a sequential \code{aov()} for each response parameter captured in one output file.
#'
#' Outputs can be generated in multiple formats ("pdf", "word", "excel" and "rmd") as specified by \code{output_type}. The function also closes any open 'Word' files to avoid conflicts when generating 'Word' documents. If \code{output_type = "rmd"} is used it is adviced to use it in a chunk with \{r, echo=FALSE, results='asis'\}
#'
#' This function requires [Pandoc](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3 or higher), a universal document converter.
#'\itemize{
#' \item \bold{Windows:} Install Pandoc and ensure the installation folder.
#' \cr (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your system PATH.
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
#' # Make a factor of Species.
#' iris$Species <- factor(iris$Species)
#'
#' # The left hand side contains two response variables,
#' # so two aov's will be conducted, i.e. "Sepal.Width"
#' # and "Sepal.Length" in response to the explanatory variable: "Species".
#' f_aov_out <- f_aov(Sepal.Width + Sepal.Length ~ Species,
#'                    data = iris,
#'                    # Save output in MS Word file (Default is console)
#'                    output_type = "word",
#'                    # Do boxcox transformation for non-normal residual (Default is bestnormalize)
#'                    transformation = "boxcox",
#'                    # Do not automatically open the file.
#'                    open_generated_files = FALSE
#'                    )
#'
#' # Print output to the console.
#' print(f_aov_out)
#'
#' # Plot residual plots.
#' plot(f_aov_out)
#'
#' #To print rmd output set chunck option to results = 'asis' and use cat().
#' f_aov_rmd_out <- f_aov(Sepal.Width ~ Species, data = iris, output_type = "rmd")
#' cat(f_aov_rmd_out$rmd)
#'
#' @export
f_aov <- function(

    formula,                      # aov function formula
    data = NULL,                  # Data.frame used for aov
    norm_plots = TRUE,            # Show plots in output files
    ANCOVA = FALSE,               # Prevent automatic conversion to factors of all predictors for ANCOVA
    transformation = TRUE,        # Preform transformation
    alpha = 0.05,                 # Significance level for both aov, posthoc and Shapiro-Wilk Test
    adjust = "sidak",             # Specifying the method used to adjust p-values
    aov_assumptions_text = TRUE,  # Print short explanation about aov assumptions in output file
    close_generated_files = FALSE,# Closes either open excel or word files depending on the output format.
    open_generated_files = TRUE,  # Open files after creation
    output_type = "off",          # Output type can be excel, word, pdf, rmd, console, off
    output_file = NULL,           # Specify the name of the file.
    output_dir = NULL,            # Specify the name of the output dir to save the file in.
    save_in_wdir = FALSE          # Save file output in the working directory.
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
  temp_output_dir <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")

  # Create the output file "output.Rmd" in tempdir()
  file.create(temp_output_file)

  # Wrap lines in rmd output document
  f_wrap_lines()

  # Parameter validation
  if( !(output_type %in% c("pdf", "word", "excel", "rmd", "console" , "off")) ){
    stop("Character string specifying the output format (output_type = ) should be either: 'pdf', 'word', 'excel', 'console','rmd', 'off'")
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

      # If there is not output_file name specified use the data_name
      if (is.null(output_file)) {
      # Set the file name
      output_file  <- paste0(data_name,"_aov_output")
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

  # Create a list to store all outputs in this function
  output_list <- list()

  # Convert the input to a character string and force lowercase for case‐insensitive matching
    if(is.character(transformation)){
      trans_input <- tolower(as.character(transformation))

      # Valid options include the full names and also the string versions of logicals.
      valid_options <- c("bestnormalize", "boxcox", "true", "false")

      # Use pmatch for partial matching; pmatch returns NA for ambiguous or no matches.
      matched_index <- pmatch(trans_input, valid_options)

      if (is.na(matched_index))
        stop("Invalid transformation option!")

      matched_option <- valid_options[matched_index]

      # Convert string "true" or "false" to logical values if applicable
      if (matched_option == "true") {
        transformation <- TRUE
      } else if (matched_option == "false") {
        transformation <- FALSE
      } else {
        transformation <- matched_option  # Either "bestnormalize" or "boxcox"
      }
    }


    # Extract response variables from the left-hand side of the formula
    lhs <- all.vars(formula[[2]])  # Get LHS variables (response)
    response_names <- lhs

    # Extract predictor variables from the right-hand side of the formula
    predictor_names <- all.vars(formula[[3]]) # Get RHS variables (predictors)

    # Extract the right-hand side (RHS) of the formula as a string
    rhs <- deparse(formula[[3]])  # Preserve the RHS structure

    # Ensure response and predictors are in the data
    for (response in response_names) {
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

    # Prevent from automatic conversion to factors to allow for ANCOVA
    if(ANCOVA == FALSE){
      # Ensure all predictor variables are factors
      for (predictor in predictor_names) {
        data[[predictor]] <- as.factor(data[[predictor]])
      }
    }

# This is the main function that generates the content for all the output types
    # output = FALSE, return output_list FALSE no output_list used for rmd output
generate_report <- function(output = TRUE) {

    # This text reminds the user of the assumptions of an aov
    # it its show by default but can be hidden.
    if(aov_assumptions_text == TRUE){

      cat("
# Assumptions of ANOVA
Checking the assumptions of ANOVA (Analysis of Variance) are critical for ensuring the validity of its results:\n

## 1. Independence
- Observations must be independent both within and between groups.
- This means that the value of one observation should not influence another.
- Independence violations cannot be corrected statistically and invalidate the analysis, making proper experimental design essential.

## 2. Normality
- The residuals (errors) of the model are assumed to be normally distributed.
- This assumption applies to the residuals, not necessarily the raw data.
- ANOVA is robust to minor deviations from normality, especially with large and balanced sample sizes. For small or unbalanced samples, violations can lead to **unreliable results**, requiring a data transformation or alternative tests (Welch's ANOVA, Kruskal-Wallis Test).
- Normality of the residuals can be tested using a Shapiro Wilcoxon or Anderson-Darling Test. It can also be graphically assessed using a Box Plot, Q-Q plot or Histogram (see figure below).

## 3. Homogeneity of Variances (Homoscedasticity)
- The variances within each group should be approximately equal.
- Equal  variances ensure that the F-test statistic is reliable.
- Homogeneity of variances assumption in ANOVA should be tested on the residuals, not directly on the raw data.
- Levene's test or Bartlett's test can be applied to check for homogeneity of variances.
- If violated, a data transformation or alternative tests (Welch's ANOVA, Kruskal-Wallis Test) are required.  It can also be graphically assessed by plotting residuals vs. fitted values and checking for patterns.


## 4. Additivity (No Unaccounted Systematic Effects)
- For models without interaction terms, it is assumed that the effects of different factors are additive. That is, the combined effect of factors can be expressed as the sum of their individual effects.
- However, if interaction terms are included in the model, this assumption does not apply because ANOVA explicitly accounts for interactions.  \n  \n")

     if(output_type != "rmd"){
        # Pagebreak
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage")
     }
    }

    #create count to remove last page break
    i <- 0

    # Loop for several response parameters
    for (response_name in lhs) {

    # set these paramters to default for each respone_name
    aov_summary_transformed <- NULL
    Response_Transformed    <- FALSE

    # Store the transformation option for output
    output_list[[response_name]][["transformation_option"]] <- transformation

    # Create a new formula for each response, preserving interactions
    current_formula <- as.formula(paste0(response_name, "~", rhs))

    cat("   \n  \n# Analysis of: ", response_name, "  \n")
    cat("  \n## Normality and homoscedasticity of residuals of: ", response_name, "  \n")


    # Perform ANOVA
    aov_test <- aov(current_formula, data = data)
    output_list[[response_name]][["aov_test"]] <- aov_test
    res_aov <- residuals(aov_test)
    aov_summary <- summary(aov_test)
    output_list[[response_name]][["aov_summary"]] <- aov_summary


      # Perform levene test on residuals
      levene_res <- rstatix::levene_test(as.formula(paste0(
        "res_aov~interaction(",
        paste(predictor_names, collapse = ", "),
        ")"
      )), data = data) #residuals(aov_test)~predictor_names

      # Generate text for interpretation of levene output
      if (levene_res$p > alpha) {
        levene_res_intp_text <- paste0(
              "According to 'Levene's Test' (",
              round(levene_res$p, digits = 4),
              " > ",
              alpha,
              ") residuals **have equal variance** (homoscedasticity).  \n  \n&nbsp;  \n"
            )
      }
      if (levene_res$p <= alpha) {
        levene_res_intp_text <- paste0(
              "According to 'Levene's Test' (",
              round(levene_res$p, digits = 4),
              " > ",
              alpha,
              ") residuals do **NOT** have equal variance (Heteroskedasticity).  \n  \n&nbsp;  \n"
            )
      }
      # levene_output_res
      cat(
        "Levene's test for homogeneity of residuals: F-Statisic =",
        round(levene_res$statistic, digits = 4),
        "p-value =",
        round(levene_res$p, digits = 4),
        "   \n", levene_res_intp_text
      )

    # Shapiro-Wilk Test for normality of the aov residuals
      shapiro_res <- shapiro.test(res_aov)

      if (shapiro_res$p.value > alpha) {
        shapiro_res_intp_text <- paste0(
              "According to 'Shapiro-Wilk Test' (",
              round(shapiro_res$p.value, digits = 4),
              " > ",
              alpha,
              ") residuals **ARE normally distributed**.  \n  \n&nbsp;  \n"
            )
      }
      if (shapiro_res$p.value <= alpha) {
        shapiro_res_intp_text <- paste0(
              "According to 'Shapiro-Wilk Test' (",
              round(shapiro_res$p.value, digits = 4),
              " > ",
              alpha,
              ") residuals are **NOT** normally distributed.  \n  \n&nbsp;  \n"
            )
      }
      # shapiro_output_res
      cat(
        "Shapiro-Wilk Test for Normality of residuals: W =",
        round(shapiro_res$statistic, digits = 4),
        "p-value =",
        round(shapiro_res$p.value, digits = 4),
        "   \n", shapiro_res_intp_text
      )

      # Perform Anderson-Darling normality test on
      adt_res <- ad.test(res_aov)

      if (as.numeric(adt_res$p.value) > alpha) {
        adt_res_intp_text <- paste0(
          "According to 'Anderson-Darling test' (",
          round(adt_res$p.value, digits = 4),
          " > ",
          alpha,
          ") residuals **ARE normally distributed**.  \n  \n"
        )
      }

      if (as.numeric(adt_res$p.value) <= alpha) {
        adt_res_intp_text <- paste0(
          "According to 'Anderson-Darling test' (",
          round(adt_res$p.value, digits = 4),
          " > ",
          alpha,
          ") residuals are **NOT** normally distributed.  \n  \n"
        )
      }
      # adt_output_res
      cat(
        adt_res$method,
        ": A =",
        round(adt_res$statistic, digits = 4),
        " p =",
        round(adt_res$p.value, digits = 4),
        "   \n"
      )

      output_list[[response_name]][["Levene_test_on_res"]] <- levene_res
      output_list[[response_name]][["shapiro_test_residuals"]] <- shapiro_res
      output_list[[response_name]][["adt_test_residuals"]] <- adt_res

      cat("Check the plots in the figure below to assess normality.  \n")


      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = 7.8, height = 7.8, units = "in", res = 300)
      # Set up a 2x2 plotting layout with adjusted margins mar = c(bottom, left, top, right)
      par(mfrow = c(2, 2), mar = c(4, 4, 4, 1))  # Adjust left margin

      # Residual plot aov
      plot(aov_test, 1)

      # Boxplot
      boxplot(residuals(aov_test), main = "Boxplot", xlab = "Residuals")

      # Histogram
      f_hist(residuals(aov_test),  xlab = "Residuals")

      # QQPlot
      f_qqnorm(residuals(aov_test))

      # Close the png file
      dev.off()

      if(norm_plots == TRUE){
      # Include the saved plots in R Markdown
      cat(paste0("![](", temp_file, ")"), "   \n  \n")
      cat("&nbsp;\n   \n")
      }

      output_list[[response_name]][["normality_plots"]] <- temp_file
      output_list[[response_name]][["alpha"]] <- alpha


      if(output_type == "pdf"){
      # Pagebreak
  cat("<div style=\"page-break-after: always;\"></div>
  \\newpage")
      }

    # If not normal, apply transformation


    if (shapiro_res$p.value <  alpha || levene_res$p <  alpha) {
      if (transformation == FALSE) {
          if (shapiro_res$p.value <  alpha){
          cat("   \n  \n**WARNING !!!**   \nBased on the Shapiro-Wilk Test on the aov residuals the response variable is **NOT** normal.   \n  \nPlease enable the transformation function (transformation == TRUE) in the f_aov function.   \n  \n")
          }
          if (levene_res$p <  alpha){
          cat("   \n  \n**WARNING !!!**   \nBased on the Levene's Test the residuals do **NOT** have equal variance (Heteroskedasticity).   \n  \nPlease enable the transformation function function (transformation  == TRUE) in the f_aov function.   \n  \n")
          }
      }

      if (transformation == "boxcox") {
          cat("\n   \n## Box-Cox transformation of: ", response_name, "  \n  \n")

        capture.output(transformed_var <- f_boxcox(data[[response_name]],
                                                   output_type = "rmd",
                                                   alpha = alpha,
                                                   plots = TRUE
                                                   )
                            )
        cat(transformed_var$rmd)

        output_list[[response_name]][["boxcox"]] <-  transformed_var$transformed_data
        data[[response_name]] <- transformed_var$transformed_data# Update the response variable in the data

      }

      if (transformation == "bestnormalize" ||
          (transformation == TRUE)) {

          # Apply the f_bestNormalize function
          transformed_var <- f_bestNormalize(data[[response_name]],
                                             data_name = response_name,
                                             output_type = "rmd",
                                             alpha = alpha
                                             )

          # Save the transformed data object
          output_list[[response_name]][["bestNormalize"]] <-  transformed_var

          # Save the transformed data
          data[[response_name]] <- transformed_var$transformed_data  # Update the response variable in the data

          # Print the ouput of f_bestNormalize
            cat(transformed_var$rmd)
            cat("    \n    \n")
      }

      if (transformation != FALSE) {
        # Perform ANOVA on transformed data
        aov_test_transformed <- aov(current_formula, data = data)
        transformed_aov_res  <- residuals(aov_test_transformed)
        aov_summary_transformed <- summary(aov_test_transformed)

        # Perform Shapiro-Wilk test on transformed aov residuals
        shapiro_res_transformed <- shapiro.test(transformed_aov_res)

        # Perform Anderson-Darling normality test on transformed aov residuals
        adt_res_transformed <- ad.test(transformed_aov_res)

        # Perform Levene's test on transformed aov residuals
        levene_res_transformed <- rstatix::levene_test(
          as.formula(
            paste0("transformed_aov_res~interaction(",
                   paste(predictor_names, collapse = ", "),
                   ")")), data = data) #residuals(aov_test)~predictor_names

        cat("
   \n## Normality and homoscedasticity of **TRANSFORMED residuals** of : ",
          response_name,
          "   \n  \n"
        )

        # Generate text for interpretation of Levene's output
        if (levene_res_transformed$p > alpha) {
          levene_res_intp_text <- paste("      According to 'Levene's Test' (",
            round(levene_res_transformed$p, digits = 4),
            " > ",
            alpha,
            ") transformed residuals **have equal variance** (homoscedasticity).  \n  \n&nbsp;  \n"
            )
        }
        if (levene_res_transformed$p <= alpha) {
          levene_res_intp_text <- paste("      According to 'Levene's Test' (",
            round(levene_res_transformed$p, digits = 4),
            " < ",
            alpha,
            ") transformed residuals do **NOT** have equal variance (Heteroskedasticity).  \n  \n&nbsp;  \n"
            )
        }
        # levene_output_transformed
        cat(
          "Levene's test for homogeneity of transformed residuals: F-Statisic =",
          round(levene_res_transformed$statistic, digits = 4),
          "p-value =",
          round(levene_res_transformed$p, digits = 4),
          "   \n", levene_res_intp_text
          )

        # Generate text for interpretation of shapiro output shapiro_res_intp_text
        if (shapiro_res_transformed$p.value > alpha) {
          shapiro_res_intp_text <- paste(
            "      According to the 'Shapiro-Wilkinson' test (",
            round(shapiro_res_transformed$p.value, digits = 4),
            " > ",
            alpha,
            ") transformed residuals **ARE normally distributed**.  \n  \n&nbsp;  \n"
          )
        }

        if (shapiro_res_transformed$p.value <= alpha) {
          shapiro_res_intp_text <- paste(
            "      According to the 'Shapiro-Wilkinson' test (",
            round(shapiro_res_transformed$p.value, digits = 4),
            " > ",
            alpha,
            ") transformed residuals are **NOT** normally distributed.  \n  \n&nbsp;  \n"
          )
        }
        # shapiro_output_transformed
        cat(
          "Shapiro-Wilk Test for Normality of transformed residuals: W =",
          round(shapiro_res_transformed$statistic, digits = 4),
          "p-value =",
          round(shapiro_res_transformed$p.value, digits = 4),
          "   \n", shapiro_res_intp_text
          )

        # Generate text for interpretation of Anderson-Darling test output
        if (as.numeric(adt_res_transformed$p.value) > alpha) {
          adt_res_intp_text <- paste(
            "      According to 'Anderson-Darling test'  (",
            round(adt_res_transformed$p.value, digits = 4),
            " > ",
            alpha,
            ") transformed residuals **ARE normally distributed**.  \n  \n"
          )
        }

        if (as.numeric(adt_res_transformed$p.value) <= alpha) {
          adt_res_intp_text <- paste(
            "      According to 'Anderson-Darling test' (",
            round(adt_res_transformed$p.value, digits = 4),
            " > ",
            alpha,
            ") transformed residuals are **NOT** normally distributed.  \n  \n"
          )
        }

        # adt_output_transformed
        cat(
          adt_res_transformed$method,
          ": A =",
          round(adt_res_transformed$statistic, digits = 4),
          " p =",
          round(adt_res_transformed$p.value, digits = 4),
          "   \n", adt_res_intp_text
        )

        # Store output in output_list
        output_list[[response_name]][["transformed_aov_test"]]     <- aov_test_transformed
        output_list[[response_name]][["transformed_shapiro_test"]] <- shapiro_res_transformed
        output_list[[response_name]][["transformed_adt_test"]]     <- adt_res_transformed
        output_list[[response_name]][["transformed_levene_test"]]  <- levene_res_transformed

        cat("Check the plots in the figure below to assess normality.  \n")

        temp_file <- tempfile(fileext = ".png")
        png(temp_file, width = 7.8, height = 7.8, units = "in", res = 300)
        # Set up a 2x2 plotting layout with adjusted margins mar = c(bottom, left, top, right)
        par(mfrow = c(2, 2), mar = c(4, 4, 4, 1))  # Adjust left margin

        # Residual plot aov
        plot(aov_test_transformed, 1, main = "Residuals transformed data")

        # Boxplot
        boxplot(residuals(aov_test_transformed), main = "Boxplot", xlab = "Residuals transformed data")

        # Histogram
        f_hist(residuals(aov_test_transformed),  xlab = "Residuals transformed data")

        # QQPlot
        f_qqnorm(residuals(aov_test_transformed))

        # Close the png file
        dev.off()

        if(norm_plots == TRUE){
        # Include the saved plots in R Markdown
        cat(paste0("![](", temp_file, ")"), "   \n  \n")
        }

        output_list[[response_name]][["transformed_normality_plots"]] <- temp_file

        if(output_type != "rmd"){
          # Pagebreak
          cat("
  <div style=\"page-break-after: always;\"></div>
  \\newpage")
        }
       }
    }

    if(!is.null(aov_summary_transformed)){
      #Overwrite earlier object with transformed output
      aov_test_out <- aov_test_transformed
      aov_summary  <- aov_summary_transformed
      output_list[[response_name]][["Response_Transformed"]] <- TRUE
    } else {
      aov_test_out <- aov_test
      output_list[[response_name]][["Response_Transformed"]] <- FALSE
    }

    # Extract p-values for the model terms
    p_values <- aov_summary[[1]][["Pr(>F)"]]
    overall_p_value <- min(p_values, na.rm = TRUE)  # Minimum p-value across all terms

    if (overall_p_value < alpha) {

      # Estimated Marginal Means for post hoc test
      emm <- emmeans::emmeans(aov_test, specs = predictor_names)

      # Perform post hoc test with adjustment
      mult_cld <- multcomp::cld(emm, alpha = alpha, Letters = letters, adjust = adjust)

      # Convert the result to a data frame
      summary_table <- as.data.frame(mult_cld)

      # Remove row names
      rownames(summary_table) <- NULL

      # Rename columns for clarity
      names(summary_table)[names(summary_table) == ".group"] <- "Letter"

      # Store output in output_list
      output_list[[response_name]][["post_hoc_summary_table"]] <- summary_table

    } else {
      # cat("No significant differences found based on ANOVA.\n")
      output_list[[response_name]][["post_hoc_summary_table"]] <- capture.output(cat("No significant differences found based on ANOVA.\n"))
      summary_table <- capture.output(cat("No significant differences found based on ANOVA.\n"))
    }

    if (Response_Transformed == TRUE){
      # Print the output of the aov and cld summary table
      if (shapiro_res_transformed$p.value <  alpha){
        cat(
          "  \n**WARNING !!!**   \nBased on the Shapiro-Wilk Test the transformed residuals are **NOT** normally distributed. Thus, **ANOVA results can be misleading, resort to other statistical tests.**   \n  \n"
        )
      }
      if (levene_res_transformed$p <  alpha){
        cat(
          "  \n**WARNING !!!**   \nBased on the Levene's Test the transformed residuals do **NOT** have equal variance (Heteroskedasticity). Thus, **ANOVA results can be misleading, resort to other statistical tests.**  \n  \n"
        )
      }
    }

    cat("
   \n## ANOVA Summary ", response_name, "  \n")
    cat(rmd_anova_summary(aov_test_out), "  \n  \n")
    cat("&nbsp;\n  \n&nbsp;   \n  \n")

    if (!is.null(summary_table)) {
      cat("  \n## Post Hoc Test Results ", response_name, "  \n")
      f_pander(summary_table)
      if(exists("mult_cld")){
      cat(paste(attr(mult_cld, "mesg"), collapse = "  \n"))
      }
    } else {
     cat("No significant differences found based on ANOVA.\n")
    }


    i <- i + 1

    if(output_type != "rmd" &&  i < length(lhs)){
      # Pagebreak
    cat("
\n    \n
<div style=\"page-break-after: always;\"></div>
\\newpage
        ")
      }
    } #Main loop end
      if (output == TRUE) {
        return(output_list)
      }
    } # End generate report function.


# Execute analysis and return results but hide this from console.
sink(tempfile())
capture.output(output_list <- generate_report())
class(output_list) <- "f_aov"
sink()


# Here the documents are constructed.
if (output_type %in% c("word", "pdf")) {
  if (output_type == "word") { file.ext <- ".docx" }
  if (output_type == "pdf")  { file.ext <- ".pdf"  }

# Create a temporary R Markdown file
  word_pdf_preamble <- function(){ paste0("
---
title: \"f_aov Analysis Report\"
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
  message(paste0("Saving output in: ", output_dir, "\\", output_file, file.ext))

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

    } else if (output_type == "excel") {
      # set the wd to the location the file is saved
      file.ext <- ".xlsx"
      message(paste0("Saving output in: ", output_dir, "\\", output_file, file.ext))

      # Extract all post_hoc_summary_table tables and keep their names
      post_hoc_tables <- lapply(output_list, function(obj)
        obj$post_hoc_summary_table)

      # Assign names to the list for Excel sheet names based on response names
      names(post_hoc_tables) <- response_names

      # Write to an Excel file with each table in its own sheet
      write_xlsx(post_hoc_tables, path = paste0(output_dir, "/", output_file, file.ext))

      # Open files after creation
      if(open_generated_files == TRUE){
      f_open_file(paste0(output_dir, "/", output_file, file.ext))
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
      #Default R behavior only show when not stored in an new object
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
print.f_aov <- function(x, ...) {

  # Loop over each category (a, b, etc.)
  for (category in names(x)) {
    # Get the sublist for this category
    sublist <- x[[category]]

    if(sublist$Response_Transformed == TRUE){
      if(sublist$transformation_option == "bestnormalize" ||
         sublist$transformation_option == TRUE){
      cat("\n   \n==========================================================\n")
      cat("   ANOVA of", sublist$bestNormalize$transformation_name,"TRANSFORMED repsone variable:", category, "\n")
      cat("==========================================================\n")



      }

      if(sublist$transformation_option == "boxcox"){
        cat("\n   \n==========================================================\n")
        cat("   ANOVA of Box-Cox TRANSFORMED repsone variable:", category, "\n")
        cat("===========================================================\n")
      }


    cat("\nTRANSFORMED Summary Table:\n")
    print(sublist$aov_summary)

    cat("\nTRANSFORMED Post-Hoc Analysis:\n")
    print(sublist$post_hoc_summary_table, row.names = FALSE, quote=FALSE)

    if (sublist$transformed_levene_test$p <= sublist$alpha) {
      message("\n**WARNING**   \nBased on the 'Levene's Test' (",
              round(sublist$transformed_levene_test$p, digits = 4), " < ", sublist$alpha, ") the transformed\n residuals do NOT have equal variance (heteroskedasticity). \nANOVA results can be misleading, resort to other statistical tests.")
    }

    if (sublist$transformed_shapiro_test$p.value <= sublist$alpha) {
      message("\n**WARNING**   \nBased on the Shapiro-Wilk Test (",
              round(sublist$transformed_shapiro_test$p.value, digits = 4), " > ", sublist$alpha,") the transformed\n residuals are **NOT** normally distributed. \nANOVA results can be misleading, resort to other statistical tests.\n"
      )
    }

    } else {

      cat("\n   \n==========================================================\n")
      cat("   ANOVA of repsone variable: ", category, "\n")
      cat("===========================================================\n")

      cat("\nSummary Table:\n")
      print(sublist$aov_summary)

      cat("\nPost-Hoc Analysis:\n")
      print(sublist$post_hoc_summary_table, row.names = FALSE, quote=FALSE)

      if (sublist$Levene_test_on_res$p <= sublist$alpha) {
        message("\n**WARNING**   \nBased on the 'Levene's Test' (",
                round(sublist$Levene_test_on_res$p, digits = 4), " < ", sublist$alpha, ")\nthe residuals do NOT have equal variance (heteroskedasticity).\nANOVA results can be misleading. \nENABLE the transformation option or resort to other statistical tests."
        )
      }

      if (sublist$shapiro_test_residuals$p.value <= sublist$alpha) {
        message("\n**WARNING**   \nBased on the Shapiro-Wilk Test (",
                round(sublist$shapiro_test_residuals$p.value, digits = 4), " > ", sublist$alpha,")\n the residuals are **NOT** normally distributed. \nANOVA results can be misleading. \nENABLE the transformation option or resort to other statistical tests."
        )
      }
    }
  }# End of loop

#Some space between output and next input line in console
cat("\n   \n")
} # End of function



#' @export
plot.f_aov <- function(x, ...) {
  # Save and restore par options
  old_par <- par(no.readonly = TRUE)
  old_par$new <- NULL                 # Remove this parameter to prevent warning
  on.exit({
    par(old_par)
    layout(1)  # Reset layout matrix
  })


  # Loop over each category (a, b, etc.)
for (category in names(x)) {
    # Get the sublist for this category
    sublist <- x[[category]]

  if(sublist$Response_Transformed == FALSE){
    # Set up a 2x2 plotting layout with adjusted margins mar = c(bottom, left, top, right)
    par(mfrow = c(1, 2),
        mar = c(3, 2.8, 4, 0.6), # Default is mar = c(5.1, 4.1, 4.1, 2.1)
        oma = c(0, 0, 2, 0),
        mgp = c(1.7, .5, 0)  # Default is par(mgp = c(3, 1, 0))
    )
    # Boxplot
    boxplot(residuals(sublist$aov_test),
            main = paste("Variable:", category),
            xlab = "Residuals",
            cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
    # Histogram
    f_hist(residuals(sublist$aov_test),  xlab = "Residuals",
           cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
    mtext("Distribution of residuals 1 of 2", outer = TRUE, cex = 1.3)

    # Residual plot aov
    plot(sublist$aov_test, 1,
         main = paste("Variable:", category),
         cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

    # QQPlot
    f_qqnorm(residuals(sublist$aov_test),
             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
    mtext("Distribution of residuals 2 of 2", outer = TRUE, cex = 1.3, line = -1)

    par(mfrow = c(1,1))
    layout(1)  # Clear layout matrix

  } else if(sublist$Response_Transformed == TRUE){
    # Set up a 2x2 plotting layout with adjusted margins mar = c(bottom, left, top, right)
    par(mfrow = c(1, 2),
        mar = c(3, 2.8, 4, 0.6), # Default is mar = c(5.1, 4.1, 4.1, 2.1)
        oma = c(0, 0, 2, 0),
        mgp = c(1.7, .5, 0)  # Default is par(mgp = c(3, 1, 0))
    )

    # Histogram
    f_hist(residuals(sublist$aov_test),
           main = paste("Variable:", category),
           xlab = "Residuals",
           cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
    f_hist(residuals(sublist$transformed_aov_test),
           main = paste("Transformed variable:", category),
           xlab = "Residuals transformed data",
           cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
    mtext("Histograms", outer = TRUE, cex = 1.3)

    # Residual plot aov
    plot(sublist$aov_test, 1,  main = paste("Variable:", category),
         cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
    plot(sublist$transformed_aov_test, 1,
         main = paste("Transformed variable:", category),
         cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
    title("Residual plots", outer = TRUE, cex = 1.3, line = -1)


    # QQPlot
    f_qqnorm(residuals(sublist$aov_test),
             main = paste("Variable:", category),
             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
    f_qqnorm(residuals(sublist$transformed_aov_test),
             main = paste("Transformed variable:", category),
             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
    mtext("Normal Q-Q Plot with 95% Confidence Bands", outer = TRUE, cex = 1.3)

    par(mfrow = c(1,1))
    layout(1)  # Clear layout matrix
  }

 }
}
