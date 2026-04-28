#' Perform multiple Kruskal-Wallis tests with a user-friendly output file, do data inspection and Dunn's test (of 'rstatix') as post hoc.
#'
#' Performs the Kruskal-Wallis rank sum test to assess whether there are statistically significant differences in the distributions (mean ranks) of three or more independent groups. It provides detailed outputs, including plots, assumption checks, and post hoc analyses using Dunn's test. Results can be saved in various formats ('pdf', 'Word', 'Excel', or console only) with customizable output options.
#'
#' @param formula A formula specifying the response and predictor variable (e.g., \code{response ~ predictor)}.
#' more response variables and predictors can be added using \code{-} or \code{+} (e.g., \code{response1 + response2 ~ predictor1 + predictor2)}. The function iterates through these combinations or response and predictors, because the Kruskal-Wallis test itself only allows one response and one predictor combination to be tested simultaneously.
#' @param data A \code{data.frame} containing the variables referenced in the formula.
#' @param plot Logical. If \code{TRUE}, generates plots (e.g., density plots and boxplots)
#'   in the output files. Default is \code{TRUE}.
#' @param alpha Numeric. The significance level for the Kruskal-Wallis test and Dunn's
#'   test. Default is  \code{0.05}.
#' @param adjust Character string. Adjustment method for pairwise comparisons in Dunn's test. Options include \code{"holm", "hommel", "bonferroni", "hochberg", "bh", "by", "fdr"} or \code{"none"}. Default is \code{"bonferroni"}, if you don't want to adjust the p value (not recommended), use \code{adjust = "none"}.
#' @param intro_text Logical. If \code{TRUE}, includes a section about Kruskal-Wallis test assumptions in the output document. Default is \code{TRUE}.
#' @param close_generated_files Logical. Closes open Excel or Word (NOT pdf) files before writing, depending on the output format. Works on Windows (taskkill), macOS (pkill) and Linux (pkill/soffice). Default \code{FALSE}. \strong{WARNING:} Always save your work before using this option!!
#' @param open_generated_files Logical. Whether to open the generated output
#'   files after creation. Defaults to \code{TRUE} in an interactive R session
#'   and \code{FALSE} otherwise (e.g. in scripts or automated pipelines).
#'   Set to \code{TRUE} or \code{FALSE} to override this behaviour explicitly.
#' @param output_type Character string specifying the output format. Default is \code{"default"}.
#'   \itemize{
#'     \item \code{"default"}: Returns the object and lets R decide whether
#'       to print; auto-prints if unassigned, silent if assigned to a variable.
#'       Use \code{print(result)} or \code{plot(result)} to display the
#'       returned object.
#'     \item \code{"console"}: Forces immediate printing to the console
#'       regardless of object assignment.
#'     \item \code{"pdf"}, \code{"word"}, \code{"excel"}: Saves results to a
#'       file of the corresponding format. See \code{save_as},
#'       \code{save_in_wdir}, and \code{open_generated_files} for file
#'       path and opening behavior.
#'     \item \code{"rmd"}: Stores the raw markdown string inside the returned
#'       object for use in R Markdown documents.
#'   }
#' @param save_as Character string specifying the output file path (without extension).
#'   If a full path is provided, output is saved to that location.
#'   If only a filename is given, the file is saved in \code{tempdir()}.
#'   If only a directory is specified (providing an existing directory with trailing slash),
#'   the file is named "dataname_Kruskal_Wallis_output" in that directory. If an extension is provided the output format specified with option "output_type" will be overruled.
#'   Defaults to \code{file.path(tempdir(), "dataname_summary.pdf")}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory. Default is \code{FALSE}, this avoid unintended changes to the global environment. If \code{save_as} location is specified \code{save_in_wdir} is overwritten by \code{save_as}.
#' @param ... Additional arguments forwarded to \code{\link[stats]{kruskal.test}}.
#'   The arguments \code{subset} and \code{na.action} are honored: when
#'   supplied, they are applied via \code{\link[stats]{model.frame}} so
#'   that the descriptive summary table, density plot, boxplot, Dunn's
#'   post hoc test and the Kruskal-Wallis test itself all see the exact
#'   same row set.

#' @return An object of class 'f_kruskal_test' (a named list, one entry per
#'   response-predictor combination) containing:
#'   \describe{
#'     \item{kruskal.test}{The \code{htest} object from \code{kruskal.test()}.}
#'     \item{dunn_test}{Data frame of pairwise Dunn's test results from \code{rstatix::dunn_test()}.}
#'     \item{summary_table}{Descriptive statistics with compact letter display (Letters column).}
#'     \item{alpha}{The significance level used.}
#'     \item{DunnTest_adjust}{The p-value adjustment method used.}
#'     \item{distributions}{ggplot density plot (if \code{plot = TRUE}).}
#'     \item{Boxplot}{ggplot boxplot with CLD letters (if \code{plot = TRUE}).}
#'   }
#'
#' Using the option \code{output_type}, it can also generate output in the form of: R Markdown code, 'Word', 'pdf', or 'Excel' files. Includes print and plot methods for 'f_kruskal_test' objects.
#'
#' @details
#' This function offers a comprehensive workflow for non-parametric analysis using the Kruskal-Wallis test:
#' \itemize{
#' \item Assumption Checks: Optionally includes a summary of assumptions in the output.
#' \item Visualization: Generates density plots and boxplots to visualize group distributions.
#' \item Post hoc Analysis: Conducts Dunn's test with specified correction methods if significant differences are found.
#'}
#'-----------\cr
#'
#' Output files are generated in the format specified by \code{output_type =} and saved to the working directory, options are \code{"pdf", "word"} or \code{"excel"}. If \code{output_type = "rmd"} is used it is advised to use it in a chunk with \{r, echo=FALSE, results='asis'\}
#'
#' This function requires [Pandoc](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3 or higher), a universal document converter.
#'\itemize{
#' \item \bold{Windows:} Install Pandoc and ensure the installation folder \cr (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your system PATH.
#' \item \bold{macOS:} If using Homebrew, Pandoc is typically installed in "/usr/local/bin". Alternatively, download the .pkg installer and verify that the binary's location is in your PATH.
#' \item \bold{Linux:} Install Pandoc through your distribution's package manager (commonly installed in "/usr/bin" or "/usr/local/bin") or manually, and ensure the directory containing Pandoc is in your PATH.
#'
#' \item If Pandoc is not found, this function may not work as intended.
#' }
#'
#' @section Multiple Testing Across Response Variables:
#' When several response variables are analysed in a single call
#' (e.g. \code{y1 + y2 + y3 ~ treatment}), each Kruskal-Wallis test is an
#' independent null-hypothesis test at level \code{alpha}. The post hoc
#' adjustment (e.g. \code{adjust = "bonferroni"}) only controls the
#' family-wise error rate \strong{within} one test (across pairwise Dunn
#' comparisons for that response). It does \strong{not} protect against
#' the inflation of Type I error \strong{across} the set of responses.
#'
#' \strong{Practical implication:} With \eqn{k} independent response
#' variables all tested at \eqn{\alpha = 0.05}, the probability of
#' obtaining at least one false positive is
#' \eqn{1-(1-0.05)^k}, which reaches ~40\% for \eqn{k = 10}.
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
#'                adjust = "holm"
#'                )
#'
#' # Save Kruskal-Wallis test and posthoc to Excel sheets: Sepal.Width and Sepal.Length.
#' f_kruskal_out <- f_kruskal_test(
#'                      Sepal.Width + Sepal.Length ~ Species,
#'                      data = iris,
#'                      plot = FALSE,
#'                      output_type = "excel",
#'                      adjust = "holm"
#'                      )
#'
#' @export
f_kruskal_test <- function(
    formula,              # Kruskall-Wallis function formula
    data = NULL,          # data.frame used for Kruskall-Wallis
    plot = TRUE,          # Show plots in output files
    alpha = 0.05,         # Set significance level alpha for Kruskall and dunn_test
    output_type = "default",
    save_as = NULL,       # Specify the name of the file.
    save_in_wdir = FALSE, # Save file output in the working directory.
    intro_text = TRUE, # Print a short explanation about Kruskall-Wallis assumptions in the pdf or word output file
    adjust = "bonferroni",           # Correction Method for pairwise comparisson in dunn_test.
    close_generated_files = FALSE,   # Closes either open excel or word files depending on the output format.
    open_generated_files = interactive(),     # Open files after creation
    ...
    # Additional arguments forwarded to kruskal.test(). Currently the
    # arguments `subset` and `na.action` are honored: when supplied, they
    # are applied via stats::model.frame() so that the descriptive
    # summary table, density plot, boxplot, Dunn's test, and the KW test
    # itself all see the exact same row set.
)
{



  ########## Reset initial settings on exit #################################
  .session_state <- save_session_state()  # Helper function: helper_session_state
  on.exit(restore_session_state(.session_state), add = TRUE) # Helper function: helper_session_state



  ########## Capture ... UNEVALUATED at f_kruskal_test's own frame ##########
  # Must be done HERE (in the top-level body), not inside
  # generate_report(), because match.call() captures the call to
  # whatever function it is called from. Inside generate_report() it
  # would capture generate_report()'s (empty) call and dots_exprs
  # would always be NULL  silently dropping subset / na.action.
  # Lexical scoping makes dots_exprs visible inside generate_report().
  .mc         <- match.call(expand.dots = FALSE)
  # Coerce the `...` pairlist to a real named list so dots_exprs$subset
  # returns the actual expression rather than a `..1` dots-index symbol.
  dots_exprs  <- as.list(.mc[["..."]])

  # Capture the user's calling environment for resolving subset /
  # na.action expressions from inside the generate_report() closure,
  # where parent.frame() would otherwise point at f_kruskal_test itself.
  caller_env  <- parent.frame()



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
  # Create a file_extension switch
  file_extension <- NULL

  # Wrap lines in rmd output document
  f_wrap_lines()

  # match.arg() validates string args and gives clear error messages (CRAN standard)
  output_type <- match.arg(output_type,
                           choices = c("default", "console", "pdf", "word", "excel", "rmd"))


  adjust <- tolower(adjust)   # accept "BH", "bh", "BY", "by" etc.
  adjust <- match.arg(adjust,
                      choices = c("holm", "hommel", "bonferroni",
                                  "hochberg", "bh", "by", "fdr", "none"))

  # Normalize to what base p.adjust() requires
  adjust <- switch(adjust,
                   "bh" = "BH",
                   "by" = "BY",
                   adjust        # everything else passes through unchanged
  )

  # "sidak" and "hs" require rstatix >= 0.7.2
  if (adjust %in% c("sidak", "hs")) {
    if (utils::packageVersion("rstatix") < "0.7.2") {
      stop("adjust = '", adjust, "' requires rstatix >= 0.7.2. ",
           "Please run: install.packages('rstatix')")
    }
  }

  # Parameter validation alpha
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be a single numeric value strictly between 0 and 1 (e.g. 0.05).")
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

    if(is.null(file_extension) && output_type %in% c("console", "default")){
      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "Kruskal_Wallis_output", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = ".pdf"
      )
      #set output_type to default
      output_type <- "pdf"

    }
    else if(is.null(file_extension) && output_type %in% c("pdf", "word", "excel", "rmd")){

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
    else if(!is.null(file_extension)) {

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
  if (output_type == "rmd") close_generated_files <- FALSE

  # Cross-platform close_generated_files (was Windows-only taskkill)
  if (output_type != "rmd" && isTRUE(close_generated_files)) {
    close_app <- function(win_proc, mac_name, linux_name) {
      sysname <- Sys.info()[["sysname"]]
      if (.Platform$OS.type == "windows") {
        system(paste0("taskkill /im ", win_proc, " /f"),
               ignore.stdout = TRUE, ignore.stderr = TRUE)
      } else if (sysname == "Darwin") {
        system(paste0("pkill -f '", mac_name, "'"),
               ignore.stdout = TRUE, ignore.stderr = TRUE)
      } else {
        system(paste0("pkill -f ", linux_name),
               ignore.stdout = TRUE, ignore.stderr = TRUE)
      }
    }
    if (output_type == "word")  close_app("WINWORD.EXE", "Microsoft Word",  "soffice")
    if (output_type == "excel") close_app("EXCEL.EXE",   "Microsoft Excel", "soffice")
  }

  # Create a list to store all outputs in this function
  output_list <- list()


  # Warn if LHS has expressions like log(y) before silently stripping them
  check_lhs_is_names(formula) #use helper_check_lhs.R

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
The Kruskal-Wallis test is a non-parametric test used to assess whether there are statistically significant differences in the distributions (mean ranks) of three or more independent groups. It does not require the data to be normally distributed, making it suitable for ordinal or skewed continuous data. However, the test assumes that data within each group are similarly distributed. Below are its key assumptions:  \n   \n  \n   \n

1.	**Independence of Observations:**
Data points must be independently sampled within and across groups (no repeated measures, no clustering).

2.	**Measurement Scale:**
The dependent variable should be ordinal or continuous (interval or ratio), but not nominal.

3.	**Similar Distribution Shapes:**
The dependent variable should have a similar distribution shape across groups. If this assumption holds, the test can be interpreted as a comparison of medians. If shapes or spreads differ substantially, the result reflects differences in mean ranks (overall distributions) rather than medians specifically.

4.	**Random Sampling:**
Samples should be randomly drawn from their populations to ensure representativeness.

5.	**Adequate Sample Size:**
Each group should ideally include at least five observations for reliable results.   \n   \n


<div style=\"page-break-after: always;\"></div>
\\newpage")
    }

    # Multiple-response/predictor warning: shown when > 1 independent test is run.
    # Fires regardless of intro_text so the user always sees it in this situation.
    k_tests <- length(lhs) * length(rhs)
    if (k_tests > 1) {
      fwer_pct   <- round((1 - (1 - alpha)^k_tests) * 100, 1)
      bonf_alpha <- round(alpha / k_tests, 4)
      cat(paste0(
        "\n\n***\n\n",
        "**[!] NOTE Multiple Testing Across ", k_tests, " Kruskal-Wallis Tests**  \n\n",
        "This report runs **", k_tests, "** independent Kruskal-Wallis tests ",
        "(", length(lhs), " response", if (length(lhs) > 1) "s" else "", " \u00d7 ",
        length(rhs), " predictor", if (length(rhs) > 1) "s" else "", ") on the same dataset. ",
        "The **", adjust,"** correction keeps each individual test honest, it guards against ",
        "false positives among the pairwise group comparisons, but it offers no protection ",
        " against the accumulation of error across all ",k_tests," tests combined. ",

        "\n At \u03b1 = ", alpha, " per test, the probability of obtaining at least one ",
        "spurious significant result across all ", k_tests, " tests is approximately ",
        "**", fwer_pct, "%** ( $1-(1-", alpha, ")^{", k_tests, "}$, assuming independence). ",
        "This risk is highest in exploratory studies; it is less of a concern when ",
        "each response has a clear a priori hypothesis.  \n\n",
        "**Possible remedies:**  \n",
        "\n-  **Bonferroni** (conservative): re-run with `alpha = ", bonf_alpha,
        "` (\u03b1 / ", k_tests, ").  \n",
        "\n-  **False Discovery Rate (FDR)**: apply `p.adjust(p_values, method = \"fdr\")` to the ",
        k_tests, " Kruskal-Wallis p-values after the fact.  \n",
        "\n-  **Pre-registration**: if each response-predictor combination was a pre-specified ",
        "study outcome, correction may not be required; document this decision explicitly.  \n",
        "\n\n***\n\n"
      ))
    }

    #create count to remove last page break
    i <- 0
    # Build the analysis data set ONCE, before the loops, using ALL
    # responses and ALL predictors. Every response x predictor
    # combination is then tested on the identical row set, so results
    # are directly comparable across responses. Rows with NA in ANY
    # response or predictor are dropped (subject to na.action), and any
    # subset / na.action passed via `...` is applied here.
    #
    # Strategy: pre-evaluate subset eagerly against data first, falling
    # back to the user's calling environment. Then apply the filter
    # manually and pass plain values to stats::model.frame via do.call.
    # This avoids the fragile match.call/substitute dance where spliced
    # expressions inside a constructed call can end up containing `..N`
    # dots-index symbols that crash model.frame with "the ... list
    # contains fewer than 3 elements".
    combined_formula <- as.formula(
      paste("~", paste(c(lhs, rhs), collapse = " + "))
    )

    # Resolve subset expression against data columns + caller env
    subset_vec <- NULL
    if (!is.null(dots_exprs$subset)) {
      subset_vec <- eval(dots_exprs$subset, envir = data, enclos = caller_env)
      if (is.logical(subset_vec)) subset_vec[is.na(subset_vec)] <- FALSE
    }
    if (!is.null(subset_vec)) {
      data <- data[subset_vec, , drop = FALSE]
    }

    # Resolve na.action (a function, defaults to na.omit)
    na_action_fn <- stats::na.omit
    if (!is.null(dots_exprs$na.action)) {
      na_action_fn <- eval(dots_exprs$na.action, envir = caller_env)
    }

    mf_args <- list(
      formula            = combined_formula,
      data               = data,
      drop.unused.levels = TRUE,
      na.action          = na_action_fn
    )

    data <- do.call(stats::model.frame, mf_args)

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
            labs(title = "Density Plot by Group", x = response_name, fill = predictor_name) +
            theme_bw(base_size = 14)

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
        cat("&nbsp;\n   \n")

        # Perform the Kruskal-Wallis test
        kruskal.test_result <- kruskal.test(current_formula, data = data)

        # Format p-value
        kw_p_fmt <- if (kruskal.test_result$p.value < 0.001) "< 0.001" else
          round(kruskal.test_result$p.value, 4)

        # Show formatted result in output document
        cat(paste0(
          "**Kruskal-Wallis rank sum test** of ", response_name, " by ", predictor_name, ":  \n",
          "$\\chi^{2}$ = ", round(kruskal.test_result$statistic, 3),
          ", df = ", kruskal.test_result$parameter,
          ", p = **", kw_p_fmt, "**",
          if (kruskal.test_result$p.value < alpha) {
            paste0("  \nThe test is **significant** (\u03b1 = ", alpha,
                   "), indicating that at least one group differs from the others.")
          } else {
            paste0("  \nThe test is **not significant** (\u03b1 = ", alpha,
                   "). There is insufficient evidence to conclude that the groups differ.")
          },
          "\n\n&nbsp;\n   \n"
        ))
        # Store Kruskal-Wallis test in ouput
        output_list[[paste0(response_name,"_",predictor_name)]][["kruskal.test"]] <- kruskal.test_result

        # Create data summary table for output and store in output_list
        summary_table <- f_summary(data,
                                   response_name,
                                   predictor_name,
                                   show_name = FALSE,
                                   show_sd = FALSE,
                                   show_se = FALSE,
                                   digits = NULL
        )$output_df

        # Conduct Dunn's test for post hoc analysis (always computed for CLD letters)
        dunn_test_result <- rstatix::dunn_test(current_formula,
                                               data = data,
                                               p.adjust.method = adjust)

        if(kruskal.test_result$p.value < alpha){
          cat("
\n  \n## Post hoc Analysis of:  `", deparse(current_formula),"`   \n")
              cat("\nBecause the overall Kruskal-Wallis test was significant, a Dunn\u2019s (1964) test was conducted to identify which specific groups differ from one another.\n")
              cat("&nbsp;\n   \n&nbsp;\n   \n")
              cat("
       \n**How to interpret the results:**\n
       \n- **Dunn's Test compares ranks, not medians.** Although Dunn\u2019s test is often discussed in terms of median differences, it actually compares the mean ranks of groups. In other words, it tests whether observations in one group tend to have higher values than those in another (stochastic dominance).
       \n- **Caution about medians.** You can interpret group differences as differences in medians only if the distributions have similar shapes (see boxplots below and distribution graphs above).
       If one distribution is skewed and another is symmetric, Dunn's test may indicate a difference even when their medians are the same.
       \n")
          cat("&nbsp;\n   \n")
          cat("
Dunn (1964) Kruskal-Wallis multiple comparison.  \n")
          # Drop the .y. column (just repeats response name on every row)
          dunn_print <- dunn_test_result[, !names(dunn_test_result) %in% c(".y.", "y"), drop = FALSE]
          f_pander(dunn_print)
          cat("p-values (p) were adjusted with ", adjust, " (p.adj) \n")
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
        } # end if KW significant (Dunn output)

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
        colnames(letter_df)[colnames(letter_df) == "cld$Letters"] <- "Letters"

        # Build footnote text (used after summary table)
        footnote <- paste0("
\n-  **Note 1 (Assumptions):** Dunn's test does not assume normality, but implies a
      test for difference in medians only if distribution shapes are similar across groups.
      If shapes or spreads differ substantially (check side-by-side boxplots and density plots),
      the result reflects a difference in mean ranks rather than medians.
\n  **Note 2 (Results):** Groups in the \"Letters\" column sharing the same letter are ",
                           "**not** significantly different (\u03B1 = ", alpha, ".",
                           "Groups with different letters are significantly different. Sharing a letter ",
                           "indicates insufficient evidence to claim a difference; it does not ",
                           "prove the groups are identical.\n",
                           if(adjust == "none"){paste0("\n**WARNING**: No p-value correction was applied. This increases the risk of finding \"significant\" differences that generally exist only by chance.")
                           } else {paste0(" P-values were adjusted with ", adjust,".")
                           })

        # Store adjust method in output
        output_list[[paste0(response_name,"_",predictor_name)]][["adjust"]] <- adjust

        if(plot == TRUE){

          cat("  \n## Boxplot of: ", response_name, " by ", predictor_name,"  and Dunn's  post hoc test  \n  \n")
          # Add the compact letter display to the data
          data2 <- merge(data, letter_df, by = predictor_name, all.x = TRUE)

          # Set the location for the letters in the boxplot.
          y_max <- max(data[[response_name]], na.rm = TRUE)
          y_min <- min(data[[response_name]], na.rm = TRUE)

          # Calculate a proportional position above the top of the plot
          y_position <- y_max + 0.08 * (y_max - y_min)  # 5% above ymax

          # Create the boxplot
          p <- ggplot(data2, aes(x = !!sym(predictor_name), y = !!sym(response_name), fill = factor(!!sym(predictor_name)))) +
            geom_boxplot(alpha = 0.4) +
            geom_jitter(width = 0.2,
                        color = "steelblue",
                        height = 0,    # usually 0 for boxplots (don't distort y-axis values)
                        alpha = 0.5) +
            geom_text(
              data = letter_df,
              aes(y = y_position, label = .data[["Letters"]][match(!!sym(predictor_name),  !!sym(predictor_name))])
            ) +
            labs(x = predictor_name, y = response_name) +
            theme_bw(base_size = 14)

          # Create a temporary file path with a .png extension
          temp_file_path_p <- tempfile(fileext = ".png")

          # Save the plot, specifying the device as "pdf"
          suppressMessages(ggsave(filename = temp_file_path_p, plot = p))

          # Include the saved plot in R Markdown
          cat(paste0("![](", temp_file_path_p, ")"), "   \n  \n")
          cat("&nbsp;\n   \n")

          output_list[[paste0(response_name,"_",predictor_name)]][["Boxplot"]] <- p
        }

        # Create data summary with letters table for output and store in output_list
        summary_table <- merge(summary_table, letter_df, by = predictor_name, all.x = TRUE)

        if(kruskal.test_result$p.value > alpha){
          summary_table$Letters <- "ns"
        }

        # Reorder columns: Letters next to group labels (indicates distribution differences),
        # median-centric layout for non-parametric test, mean last (de-emphasized)
        col_order <- c(predictor_name, "Letters", "n", "min", "Q1", "median", "Q3", "max", "mean")
        col_order <- col_order[col_order %in% names(summary_table)]
        # Append any remaining columns not in col_order
        remaining <- setdiff(names(summary_table), col_order)
        summary_table <- summary_table[, c(col_order, remaining), drop = FALSE]

        output_list[[paste0(response_name,"_",predictor_name)]][["summary_table"]] <- summary_table

        # Helper: bold the median column for markdown/pander display only
        bold_median <- function(tbl) {
          if ("median" %in% names(tbl)) {
            tbl$median <- paste0("**", tbl$median, "**")
          }
          tbl
        }


#         if(output_type != "rmd"){
#           # Pagebreak
#           cat("
# <div style=\"page-break-after: always;\"></div>
# \\newpage")
#         }


        if(kruskal.test_result$p.value < alpha){
          cat("&nbsp;\n   \n")
          cat("  \n## Summary Statistics of: ", response_name, " by ", predictor_name,"  and Dunn's test post hoc test  \n  \n")
          cat("&nbsp;\n   \n")
          cat(paste("  \nSummary Statistics with Dunn's post hoc Comparison, `Model:", deparse(current_formula),"`  \n"))
          f_pander(bold_median(f_conditional_round(summary_table, digits = 2)), col_width = 9)

          cat(footnote)

        } else {
          cat("  \n## Data Summary Table of: ", response_name,"
      \n
        Since the Kruskal-Wallis test was **NOT** significant, no post hoc test was performed.\n   \n")

          cat(paste("\n**Summary Statistics** `Model:", deparse(current_formula),"`  \n"))
          f_pander(bold_median(f_conditional_round(summary_table, digits = 2)), col_width = 9)
        }

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
  suppressMessages(
    utils::capture.output(
      output_list <- generate_report(),
      file = nullfile()
    )
  )
  class(output_list) <- "f_kruskal_test"



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
  - \\usepackage[T1]{fontenc}
  - \\usepackage[utf8]{inputenc}
  - \\usepackage{textcomp}
  - \\DeclareUnicodeCharacter{03BB}{\\ensuremath{\\lambda}}
  - \\DeclareUnicodeCharacter{2264}{\\ensuremath{\\leq}}
  - \\DeclareUnicodeCharacter{2265}{\\ensuremath{\\geq}}
  - \\DeclareUnicodeCharacter{2192}{\\ensuremath{\\rightarrow}}
  - \\DeclareUnicodeCharacter{00D7}{\\ensuremath{\\times}}
  - \\DeclareUnicodeCharacter{2014}{\\textemdash}
  - \\DeclareUnicodeCharacter{03B1}{\\ensuremath{\\alpha}}
  - \\DeclareUnicodeCharacter{2013}{\\textendash}
  - \\DeclareUnicodeCharacter{2019}{\\textquoteright}
  - \\DeclareUnicodeCharacter{0160}{\\v{S}}
  - \\DeclareUnicodeCharacter{00E1}{\\'{a}}
  - \\usepackage{titling}
  - \\setlength{\\droptitle}{-2.5cm} % Adjust vertical spacing
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

  } else if (output_type == "default"){
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

  # get the line with from user
    line_width <- 72




# Loop over each category (a, b, etc.)
  for (category in names(x)) {
    # Get the sublist for this category
    sublist <- x[[category]]

    # Skip non-result entries (e.g. the "rmd" character string)
    if (!is.list(sublist)) next

    cat("\n==========================================================\n")
    cat("Results of testing", sublist$kruskal.test$data.name)
    cat("\n==========================================================\n")

    print(sublist$kruskal.test)

    if (sublist$kruskal.test$p.value < sublist$alpha) {
      cat(
        "\nSummary table with Dunn-Test Post Hoc:",
        sublist$kruskal.test$data.name,
        "\n"
      )
      flag_dunnTest_used <- TRUE
    } else {
      cat(
        "\nNo differences found, summary table of:",
        sublist$kruskal.test$data.name,
        "\n"
      )
    }

    f_pander(sublist$summary_table, col_width = 7)
    if (flag_dunnTest_used == TRUE) {


      dunnTest_used <- paste0("
Note 1 (Assumptions): Dunn's test does not assume normality, but implies a test for difference in medians only if distribution shapes are similar across groups. If shapes or spreads differ substantially (check boxplots and density plots using, plot(...)), the result reflects a difference in mean ranks rather than medians.\n
Note 2 (Results): Groups in the \"Letters\" column sharing the same letter are ",
"**not** significantly different (\u03B1 = ", format(sublist$alpha, nsmall = 3), ".",
"Groups with different letters are significantly different. Sharing a letter ",
"indicates insufficient evidence to claim a difference; it does not ",
"prove the groups are identical.\n",
        if (sublist$adjust == "none") {
          paste0(
            "\n**WARNING**: No p-value correction was applied. This increases the risk of finding \"significant\" differences that generally exist only by chance."
          )
        } else {
          paste0("P-values were adjusted with ", sublist$adjust, ".")
        }
        )

      # Wrap the text
      wrapped_text <- strwrap(dunnTest_used, width = line_width)

      # Print the text with newlines collapsing the vector
      cat(paste(wrapped_text, collapse = "\n"), "\n")
    }
  }
}



#' Plot method for f_kruskal_test objects
#'
#' Displays the density plot and/or boxplot stored in an \code{f_kruskal_test}
#' object. Plots are only available when the original call used \code{plot = TRUE}.
#'
#' @param x An object of class \code{f_kruskal_test}.
#' @param which Character vector indicating which plots to show. Options are
#'   \code{"distributions"} (density plot), \code{"Boxplot"}, or both (default).
#' @param ... Additional arguments (currently ignored).
#'
#' @return Returns \code{x} invisibly.
#'
#' @examples
#' result <- f_kruskal_test(Sepal.Width ~ Species, data = iris,
#'                          output_type = "default")
#' plot(result)                          # both plots
#' plot(result, which = "Boxplot")       # boxplot only
#'
#' @export
plot.f_kruskal_test <- function(x, which = c("distributions", "Boxplot"), ...) {

  which <- match.arg(which, choices = c("distributions", "Boxplot"), several.ok = TRUE)

  for (category in names(x)) {
    sublist <- x[[category]]

    # Skip non-result entries (e.g. the "rmd" character string)
    if (!is.list(sublist)) next

    for (plt_name in which) {
      if (!is.null(sublist[[plt_name]])) {
        print(sublist[[plt_name]])
      }
    }
  }

  invisible(x)
}


