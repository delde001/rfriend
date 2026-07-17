#' Perform multiple Friedman rank sum tests with a user-friendly output file, do data inspection and pairwise Wilcoxon signed-rank tests as post hoc.
#'
#' Performs the Friedman rank sum test to assess whether there are statistically significant differences in the distributions (mean ranks) of a response measured under three or more related conditions (groups) within the same blocks (e.g. repeated measures on the same subject, or matched sets). It provides detailed outputs, including plots, assumption checks, an effect size (Kendall's W) and post hoc analyses using pairwise paired Wilcoxon signed-rank tests. Results can be saved in various formats ('pdf', 'Word', 'Excel', or console only) with customizable output options.
#'
#' @param formula A formula specifying the response, the group (treatment) and
#'   the blocking variable, in the form \code{response ~ group | block}. The
#'   part before the \code{|} is the group/treatment variable whose levels are
#'   being compared; the part after the \code{|} is the block (e.g. subject or
#'   matched set) within which the groups are related. More response variables
#'   can be tested in sequence using \code{+} on the left-hand side (e.g.
#'   \code{response1 + response2 ~ group | block}). The function iterates
#'   through the responses, because the Friedman test itself only allows one
#'   response at a time.
#' @param data A \code{data.frame} containing the variables referenced in the formula.
#' @param plot Logical. If \code{TRUE}, generates two plots (a within-block
#'   trace plot with median trend line, and a boxplot with compact letter
#'   display) in the output files. Default is \code{TRUE}.
#' @param alpha Numeric. The significance level for the Friedman test and the
#'   pairwise post hoc tests. Default is \code{0.05}.
#' @param adjust Character string. Adjustment method for the pairwise post hoc
#'   Wilcoxon signed-rank comparisons. Options include \code{"holm", "hommel",
#'   "bonferroni", "hochberg", "bh", "by", "fdr"} or \code{"none"}. Default is
#'   \code{"bonferroni"}; if you do not want to adjust the p-value (not
#'   recommended) use \code{adjust = "none"}.
#' @param intro_text Logical. If \code{TRUE}, includes a section about Friedman
#'   test assumptions in the output document. Default is \code{TRUE}.
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
#'   the file is named "dataname_Friedman_output" in that directory. If an extension is provided the output format specified with option "output_type" will be overruled.
#'   Defaults to \code{file.path(tempdir(), "dataname_Friedman_output.pdf")}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory. Default is \code{FALSE}, this avoid unintended changes to the global environment. If \code{save_as} location is specified \code{save_in_wdir} is overwritten by \code{save_as}.
#' @param ... Additional arguments forwarded to \code{\link[stats]{friedman.test}}.
#'
#' @return An object of class 'f_friedman' (a named list, one entry per
#'   response) containing:
#'   \describe{
#'     \item{friedman.test}{The \code{htest} object from \code{friedman.test()}.}
#'     \item{effect_size}{Data frame with Kendall's W effect size from \code{rstatix::friedman_effsize()}.}
#'     \item{posthoc_test}{Data frame of pairwise paired Wilcoxon signed-rank results from \code{rstatix::wilcox_test()}.}
#'     \item{summary_table}{Descriptive statistics with compact letter display (Letters column).}
#'     \item{alpha}{The significance level used.}
#'     \item{adjust}{The p-value adjustment method used.}
#'     \item{distributions}{ggplot within-block trace plot with grey
#'        spaghetti lines, raw group-coloured points, and a black median
#'        trend line on top (if \code{plot = TRUE}).}
#'     \item{Boxplot}{ggplot boxplot with jittered raw points and
#'        compact-letter-display letters from the post hoc test (if
#'        \code{plot = TRUE}).}
#'   }
#'
#' Using the option \code{output_type}, it can also generate output in the form of: R Markdown code, 'Word', 'pdf', or 'Excel' files. Includes print and plot methods for 'f_friedman' objects.
#'
#' @details
#' This function offers a workflow for non-parametric analysis of an
#' unreplicated complete block design using the Friedman test:
#' \itemize{
#' \item Design check: verifies that the data form an unreplicated complete
#'   block design (exactly one observation per group-by-block combination)
#'   and fails with a clear, actionable message if not.
#' \item Assumption checks: optionally includes a summary of assumptions in the output.
#' \item Visualization: generates a within-block trace plot with a
#'   median trend line (to reveal the paired structure and central
#'   trend) and a separate boxplot with compact-letter-display letters
#'   from the post hoc test (to show marginal distribution shape and
#'   communicate which groups differ).
#' \item Effect size: reports Kendall's W.
#' \item Post hoc analysis: conducts pairwise paired Wilcoxon signed-rank tests
#'   with the specified correction method if a significant difference is found.
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
#' (e.g. \code{y1 + y2 + y3 ~ group | block}), each Friedman test is an
#' independent null-hypothesis test at level \code{alpha}. The post hoc
#' adjustment (e.g. \code{adjust = "bonferroni"}) only controls the
#' family-wise error rate \strong{within} one test (across the pairwise
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
#' # Build a small repeated-measures (unreplicated complete block) dataset:
#' # each subject (block) is measured under three conditions (group).
#' set.seed(1)
#' df_rm <- data.frame(
#'   subject   = factor(rep(1:10, each = 3)),
#'   condition = factor(rep(c("A", "B", "C"), times = 10)),
#'   score     = c(rbind(rnorm(10, 4), rnorm(10, 6), rnorm(10, 8)))
#' )
#'
#' # Perform a Friedman test of score across condition within subject,
#' # with "holm" correction for the pairwise post hoc test.
#' output <- f_friedman(
#'                score ~ condition | subject,
#'                data = df_rm,
#'                plot = FALSE,
#'                output_type = "word",
#'                adjust = "holm"
#'                )
#'
#' @export
f_friedman <- function(
    formula,              # Friedman function formula: response ~ group | block
    data = NULL,          # data.frame used for the Friedman test
    plot = TRUE,          # Show plots in output files
    alpha = 0.05,         # Set significance level alpha for Friedman and post hoc
    output_type = "default",
    save_as = NULL,       # Specify the name of the file.
    save_in_wdir = FALSE, # Save file output in the working directory.
    intro_text = TRUE,    # Print a short explanation about Friedman assumptions in the output file
    adjust = "bonferroni",           # Correction method for the pairwise post hoc comparisons.
    close_generated_files = FALSE,   # Closes either open excel or word files depending on the output format.
    open_generated_files = interactive(),     # Open files after creation
    ...
    # Additional arguments forwarded to friedman.test().
)
{



  ########## Reset initial settings on exit #################################
  .session_state <- save_session_state()  # Helper function: helper_session_state
  on.exit(restore_session_state(.session_state), add = TRUE) # Helper function: helper_session_state



  ########## Save dataframe name and handle formula-only input ##############
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
                                   default_name = paste(data_name, "Friedman_output", sep = "_"),
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
                                                        "Friedman_output",
                                                        sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = file.ext
      )


    }
    else if(!is.null(file_extension)) {

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name,
                                                        "Friedman_output",
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
                                                      "Friedman_output",
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

  # -------------------------------------------------------------------------
  # Parse the right-hand side. The Friedman test needs BOTH a group
  # (treatment) variable and a block variable, supplied as
  # `response ~ group | block`. We require the `|` form explicitly so the
  # roles of the two variables are unambiguous (fail loud, never guess).
  # -------------------------------------------------------------------------
  rhs <- formula[[3]]
  if (!(is.call(rhs) && identical(rhs[[1]], as.name("|")))) {
    stop("'formula' must be of the form 'response ~ group | block'. ",
         "The Friedman test needs a group (treatment) variable and a ",
         "block (e.g. subject or matched set) variable, separated by '|'. ",
         "You supplied: ", deparse(formula), ".")
  }
  group_name <- all.vars(rhs[[2]])
  block_name <- all.vars(rhs[[3]])

  if (length(group_name) != 1 || length(block_name) != 1) {
    stop("'formula' must name exactly one group variable and one block ",
         "variable: 'response ~ group | block'.")
  }


  # Ensure response, group and block are in the data
  for (response in lhs) {
    if (!(response %in% names(data))) {
      stop(paste("Response variable", response, "not found in the data."))
    }
  }
  if (!(group_name %in% names(data))) {
    stop(paste("Group variable", group_name, "not found in the data."))
  }
  if (!(block_name %in% names(data))) {
    stop(paste("Block variable", block_name, "not found in the data."))
  }

  # Group and block must be factors for the test, plots and summaries.
  data[[group_name]] <- as.factor(data[[group_name]])
  data[[block_name]] <- as.factor(data[[block_name]])

  # -------------------------------------------------------------------------
  # Design check helper: the Friedman test is only valid for an unreplicated
  # complete block design, i.e. exactly ONE observation for every
  # combination of group and block (after removing rows with NA in the
  # response). friedman.test() itself only says "not an unreplicated
  # complete block design"; here we explain *why* so the user can fix it.
  # Returns TRUE if the design is valid for this response, FALSE otherwise.
  # -------------------------------------------------------------------------
  check_friedman_design <- function(response_name) {
    sub <- data[!is.na(data[[response_name]]),
                c(response_name, group_name, block_name), drop = FALSE]

    n_groups <- nlevels(droplevels(sub[[group_name]]))
    n_blocks <- nlevels(droplevels(sub[[block_name]]))

    problems <- character(0)

    if (n_groups < 3) {
      problems <- c(problems, paste0(
        "  - only ", n_groups, " group level(s) found in '", group_name,
        "'; the Friedman test needs at least 3. ",
        "For 2 related groups use a paired test (e.g. f_wilcox_test(paired = TRUE))."))
    }

    # One and only one observation per group x block cell.
    cell_counts <- table(sub[[block_name]], sub[[group_name]])
    if (any(cell_counts > 1)) {
      problems <- c(problems, paste0(
        "  - some group x block combinations have more than one observation ",
        "(the design is replicated). The Friedman test needs exactly one ",
        "observation per combination of '", group_name, "' and '", block_name, "'."))
    }
    if (any(cell_counts == 0)) {
      problems <- c(problems, paste0(
        "  - some group x block combinations are missing (the design is ",
        "incomplete). Every block must have one observation for every group. ",
        "Check for missing values or unbalanced data in '", response_name, "'."))
    }

    if (length(problems) > 0) {
      warning(call. = FALSE, immediate. = TRUE, paste0(
        "Response '", response_name, "' skipped: not a valid unreplicated ",
        "complete block design.\n", paste(problems, collapse = "\n")))
      return(FALSE)
    }
    TRUE
  }


  generate_report <- function(output = TRUE) {

    # This text reminds the user of the assumptions; shown by default but can be hidden.
    if(intro_text == TRUE){

      cat("
# Assumptions of the Friedman Test
The Friedman rank sum test is a non-parametric test used to assess whether there are statistically significant differences in the distributions (mean ranks) of a response measured under three or more related conditions (groups) within the same blocks. A block is typically a subject measured repeatedly, or a set of matched units. It is the non-parametric alternative to the one-way repeated-measures ANOVA and does not require the data to be normally distributed. Below are its key assumptions:  \n   \n  \n   \n

1.	**One group measured under three or more related conditions:**
The same blocks (e.g. subjects) are observed under each of the three or more group levels (repeated measures or matched sets).

2.	**Unreplicated complete block design:**
There must be exactly one observation for every combination of group and block: every block is measured once under every group level, with no missing and no duplicated cells.

3.	**Blocks are independent of each other:**
Different blocks (e.g. different subjects) are independently sampled, even though observations within a block are related.

4.	**Measurement Scale:**
The response should be ordinal or continuous (interval or ratio), but not nominal.

5.	**Similar Distribution Shapes:**
If the distribution shapes are similar across groups, the test can be interpreted as a comparison of medians. If shapes or spreads differ substantially, the result reflects differences in mean ranks (overall distributions) rather than medians specifically.   \n   \n


<div style=\"page-break-after: always;\"></div>
\\newpage")
    }

    # Multiple-response warning: shown when > 1 independent test is run.
    k_tests <- length(lhs)
    if (k_tests > 1) {
      fwer_pct   <- round((1 - (1 - alpha)^k_tests) * 100, 1)
      bonf_alpha <- round(alpha / k_tests, 4)
      cat(paste0(
        "\n\n***\n\n",
        "**[!] NOTE Multiple Testing Across ", k_tests, " Friedman Tests**  \n\n",
        "This report runs **", k_tests, "** independent Friedman tests ",
        "(", length(lhs), " response", if (length(lhs) > 1) "s" else "", ") on the same dataset. ",
        "The **", adjust,"** correction keeps each individual test honest, it guards against ",
        "false positives among the pairwise group comparisons, but it offers no protection ",
        " against the accumulation of error across all ", k_tests, " tests combined. ",

        "\n At \u03b1 = ", alpha, " per test, the probability of obtaining at least one ",
        "spurious significant result across all ", k_tests, " tests is approximately ",
        "**", fwer_pct, "%** ( $1-(1-", alpha, ")^{", k_tests, "}$, assuming independence). ",
        "This risk is highest in exploratory studies; it is less of a concern when ",
        "each response has a clear a priori hypothesis.  \n\n",
        "**Possible remedies:**  \n",
        "\n-  **Bonferroni** (conservative): re-run with `alpha = ", bonf_alpha,
        "` (\u03b1 / ", k_tests, ").  \n",
        "\n-  **False Discovery Rate (FDR)**: apply `p.adjust(p_values, method = \"fdr\")` to the ",
        k_tests, " Friedman p-values after the fact.  \n",
        "\n-  **Pre-registration**: if each response was a pre-specified ",
        "study outcome, correction may not be required; document this decision explicitly.  \n",
        "\n\n***\n\n"
      ))
    }

    #create count to remove last page break
    i <- 0

    #Main loop starts here
    for (response_name in lhs) {
      i <- i + 1

      cat("   \n#  Analysis of: ", response_name, " by ", group_name,
          " (blocked by ", block_name, ")  \n")
      cat("   \n     \n&nbsp;  \n   \n ")

      # --- Validate the design for this response (fail loud, skip if invalid) ---
      if (!check_friedman_design(response_name)) {
        cat("\n**Response `", response_name,
            "` was skipped: the data are not an unreplicated complete block design.** ",
            "See the warning in the console for the specific reason.  \n")
        output_list[[response_name]][["skipped"]] <- TRUE
        if(output_type != "rmd" &&  i < length(lhs)){
          cat("\n    \n<div style=\"page-break-after: always;\"></div>\n\\newpage\n        ")
        }
        next
      }

      # Build the per-response analysis set: drop rows with NA in the
      # response/group/block so the test, plots and summaries share rows.
      data_r <- data[stats::complete.cases(
        data[, c(response_name, group_name, block_name)]),
        c(response_name, group_name, block_name), drop = FALSE]
      data_r[[group_name]] <- droplevels(data_r[[group_name]])
      data_r[[block_name]] <- droplevels(data_r[[block_name]])

      # Order rows by block then group so the paired post hoc lines up.
      data_r <- data_r[order(data_r[[block_name]], data_r[[group_name]]), ,
                       drop = FALSE]

      current_formula <- as.formula(
        paste0(response_name, " ~ ", group_name, " | ", block_name))

      if(plot == TRUE){
        cat("  \n## Visual check on within-block traces  \n")

        # Repeated-measures diagnostic plot. The Friedman test compares
        # ranks within blocks, so the right diagnostic is one that
        # reveals the paired structure: thin grey lines connect
        # observations from the same block (the "spaghetti"), coloured
        # points are the raw values at each group level, and a single
        # black line through the group medians (drawn on top, with
        # white-filled markers for contrast) summarises the central
        # trend. Boxes are deliberately omitted here so this figure has
        # a job distinct from the final boxplot below: this one shows
        # whether there is a within-block signal at all and which
        # direction it goes, while the boxplot focuses on the marginal
        # shapes and the post hoc letters.
        grp_cols <- f_pub_palette(length(unique(data_r[[group_name]])))

        d <- ggplot(data_r,
                    aes(x = !!sym(group_name), y = !!sym(response_name))) +
          geom_line(aes(group = !!sym(block_name)),
                    alpha = 0.4, colour = "gray40") +
          geom_point(aes(fill = !!sym(group_name)),
                     shape = 21, size = 2, alpha = 0.8,
                     colour = "gray20") +
          # Median trend: one continuous line across groups (group = 1
          # overrides the implicit per-x grouping that would otherwise
          # leave the medians as disconnected points). White-filled
          # markers sit on the line so it reads clearly against the
          # coloured raw points underneath.
          stat_summary(aes(group = 1), fun = median, geom = "line",
                       colour = "black", linewidth = 1) +
          stat_summary(aes(group = 1), fun = median, geom = "point",
                       shape = 21, fill = "white", colour = "black",
                       size = 3.5, stroke = 1) +
          scale_fill_manual(values = grp_cols, guide = "none") +
          labs(title = "Within-block traces with median trend",
               x = group_name, y = response_name) +
          f_theme_pub(base_size = 14)

        temp_file_path_d <- tempfile(fileext = ".png")
        suppressMessages(ggsave(filename = temp_file_path_d,
                                plot = d,
                                height = 4.5,
                                width = 6.26
                                )
                         )
        cat(paste0("![](", temp_file_path_d, ")"), "   \n  \n")

        # Caption focuses on what this figure uniquely shows (the
        # paired structure and central trend), and explicitly hands off
        # the marginal-shape and post hoc question to the boxplot below
        # so the reader knows the two figures play complementary roles.
        diag_caption <- paste0(
          "Within-block traces of ", response_name, " across levels of ",
          group_name, ". Thin grey lines connect observations from the ",
          "same ", block_name, " (the repeated-measures structure that ",
          "the Friedman test inspects); coloured points are the raw ",
          "values; the solid black line connects the group medians and ",
          "summarises the central trend. Look for two things: ",
          "(1) whether the grey lines trend consistently in the same ",
          "direction across groups, which is the within-block shift the ",
          "Friedman test is built to detect, and (2) the magnitude and ",
          "direction of the median trend (black line). The marginal ",
          "distribution shapes and the pairwise post hoc result are ",
          "shown in the boxplot below.")

        cat(paste0("*", gsub("\n", "  \n", diag_caption), "*",
                   "   \n  \n"))
        # cat("&nbsp;\n   \n")

        output_list[[response_name]][["distributions"]] <- d
        if(output_type != "rmd"){
          cat("
<div style=\"page-break-after: always;\"></div>
\\newpage")
        }
      }

      # Store alpha and adjust for print method
      output_list[[response_name]][["alpha"]]  <- alpha
      output_list[[response_name]][["adjust"]] <- adjust



      cat("
\n## Result of Friedman rank sum test  \n  \n")
      cat("&nbsp;\n   \n")

      # Perform the Friedman test
      friedman_result <- friedman.test(current_formula, data = data_r, ...)

      # Effect size: Kendall's W
      effsize_df <- rstatix::friedman_effsize(data_r, current_formula)

      # Format p-value
      fr_p_fmt <- if (friedman_result$p.value < 0.001) "< 0.001" else
        round(friedman_result$p.value, 4)

      cat(paste0(
        "**Friedman rank sum test** of ", response_name, " by ", group_name,
        " (blocked by ", block_name, "):  \n",
        "$\\chi^{2}$ = ", round(friedman_result$statistic, 3),
        ", df = ", friedman_result$parameter,
        ", p = **", fr_p_fmt, "**",
        ", Kendall's W = ", round(effsize_df$effsize, 3),
        " (", as.character(effsize_df$magnitude), " effect)",
        if (friedman_result$p.value < alpha) {
          paste0("  \nThe test is **significant** (\u03b1 = ", alpha,
                 "), indicating that at least one group differs from the others.")
        } else {
          paste0("  \nThe test is **not significant** (\u03b1 = ", alpha,
                 "). There is insufficient evidence to conclude that the groups differ.")
        },
        "\n   \n"
      ))
      cat("\n*Kendall's W ranges from 0 (no agreement / no effect) to 1 (complete ",
          "agreement / maximal effect) and is the standardized Friedman statistic.*\n\n&nbsp;\n   \n")

      output_list[[response_name]][["friedman.test"]] <- friedman_result
      output_list[[response_name]][["effect_size"]]   <- effsize_df

      # Descriptive summary table (median-centric, like f_kruskal_test)
      summary_table <- f_summary(data_r,
                                 response_name,
                                 group_name,
                                 show_name = FALSE,
                                 show_sd = FALSE,
                                 show_se = FALSE,
                                 digits = NULL
      )$output_df

      # Pairwise post hoc: paired Wilcoxon signed-rank tests (always computed
      # for CLD letters). Paired = TRUE because blocks relate the groups.
      posthoc_result <- rstatix::wilcox_test(
        data_r, current_formula_to_pairwise(response_name, group_name),
        paired = TRUE, p.adjust.method = adjust)

      if(friedman_result$p.value < alpha){
        cat("
\n  \n## Post hoc Analysis of:  `", deparse(current_formula),"`   \n")
        cat("\nBecause the overall Friedman test was significant, pairwise paired Wilcoxon signed-rank tests were conducted to identify which specific groups differ from one another.\n")
        cat("&nbsp;\n   \n&nbsp;\n   \n")
        cat("
       \n**How to interpret the results:**\n
       \n- **Paired comparisons within blocks.** Each pair of group levels is compared using the paired Wilcoxon signed-rank test on the within-block differences, mirroring the related-samples structure of the Friedman test.
       \n- **Caution about medians.** A significant comparison indicates that one group tends to have higher values than the other (a shift in the paired differences); interpret it as a difference in medians only if the distribution shapes are similar (judge from the boxplot below and the within-block trace plot above).
       \n")
        cat("&nbsp;\n   \n")
        cat("
Pairwise paired Wilcoxon signed-rank comparisons.  \n")
        # Drop the .y. column (just repeats response name on every row)
        ph_print <- posthoc_result[, !names(posthoc_result) %in% c(".y.", "y"), drop = FALSE]
        f_pander(ph_print)
        cat("p-values (p) were adjusted with ", adjust, " (p.adj) \n")
        cat("
Group 1 and group 2 indicate the compared groups with respectively n1 and n2 paired observations. The **statistic** column is the Wilcoxon signed-rank V statistic for each pairwise comparison.   \n   \n")

        if(output_type != "rmd"){
          cat("
<div style=\"page-break-after: always;\"></div>
\\newpage")
        }
      } # end if Friedman significant (post hoc output)

      output_list[[response_name]][["posthoc_test"]] <- posthoc_result


      # ---- Compact letter display, ordered high-to-low by median ----------
      grp_levels <- levels(data_r[[group_name]])

      g1 <- as.character(posthoc_result$group1)
      g2 <- as.character(posthoc_result$group2)

      n_lv <- length(grp_levels)
      sig_matrix <- matrix(FALSE, n_lv, n_lv,
                           dimnames = list(grp_levels, grp_levels))
      sig <- posthoc_result$p.adj < alpha
      for (k in seq_along(g1)) {
        sig_matrix[g1[k], g2[k]] <- sig[k]
        sig_matrix[g2[k], g1[k]] <- sig[k]
      }

      # Order groups by descending median so letters run high-to-low.
      grp_median <- tapply(data_r[[response_name]],
                           data_r[[group_name]],
                           median, na.rm = TRUE)
      ordered_levels <- names(sort(grp_median, decreasing = TRUE))
      sig_matrix <- sig_matrix[ordered_levels, ordered_levels, drop = FALSE]

      cld <- compact_letters(sig_matrix)

      letter_df <- as.data.frame(cld$Letters)
      letter_df[[group_name]] <- row.names(letter_df)
      colnames(letter_df)[colnames(letter_df) == "cld$Letters"] <- "Letters"

      footnote <- paste0("
\n  **Note 1 (Assumptions):** The pairwise paired Wilcoxon signed-rank test does not assume
      normality, but implies a test for difference in medians only if distribution shapes are
      similar across groups. If shapes or spreads differ substantially (judge from the boxplot
      above and the within-block trace plot earlier in this section), the result reflects a
      difference in the paired ranks rather than medians.
\n  **Note 2 (Results):** Groups in the \"Letters\" column sharing the same letter are ",
                         "**not** significantly different (\u03B1 = ", alpha, ".",
                         "Groups with different letters are significantly different. Sharing a letter ",
                         "indicates insufficient evidence to claim a difference; it does not ",
                         "prove the groups are identical.\n",
                         if(adjust == "none"){paste0("\n**WARNING**: No p-value correction was applied. This increases the risk of finding \"significant\" differences that generally exist only by chance.")
                         } else {paste0(" P-values were adjusted with ", adjust,".")
                         })

      if(plot == TRUE){

        cat("  \n## Boxplot of: ", response_name, " by ", group_name,
            "  and pairwise post hoc test  \n  \n")

        y_max <- max(data_r[[response_name]], na.rm = TRUE)
        y_min <- min(data_r[[response_name]], na.rm = TRUE)
        y_position <- y_max + 0.08 * (y_max - y_min)

        # Clean post hoc summary boxplot. Its job is distinct from the
        # diagnostic trace plot above: boxes show the marginal shape
        # and spread (the condition for the median interpretation of
        # the post hoc Wilcoxon to hold), jittered points show the raw
        # data, and the CLD letters communicate which groups differ.
        # No within-block lines here on purpose: that signal was the
        # diagnostic plot's job, and crowding it back in dilutes both
        # figures.
        p <- ggplot(data_r,
                    aes(x = !!sym(group_name), y = !!sym(response_name),
                        fill = factor(!!sym(group_name)))) +
          geom_boxplot(alpha = 0.4) +
          geom_jitter(width = 0.2,
                      color = "steelblue",
                      height = 0,
                      alpha = 0.5) +
          geom_text(
            data = letter_df,
            aes(x = !!sym(group_name), y = y_position,
                label = .data[["Letters"]][match(!!sym(group_name),
                                                 !!sym(group_name))]),
            inherit.aes = FALSE
          ) +
          labs(x = group_name, y = response_name) +
          scale_fill_manual(
            values = f_pub_palette(length(unique(data_r[[group_name]]))),
            guide = "none") +
          f_theme_pub(base_size = 14)

        temp_file_path_p <- tempfile(fileext = ".png")
        suppressMessages(ggsave(filename = temp_file_path_p,
                                plot = p,
                                height = 5,
                                width = 6.26
                                )
                         )
        cat(paste0("![](", temp_file_path_p, ")"), "   \n  \n")

        # Caption focuses on what the boxplot uniquely contributes:
        # marginal shape/spread (the assumption check) and the CLD
        # letters (the post hoc conclusion). The within-block signal
        # is left to the diagnostic plot above. Two branches:
        # significant case carries the letters and the adjustment
        # note; non-significant case says so explicitly so the missing
        # letters are not a mystery.
        if (friedman_result$p.value < alpha) {
          box_caption <- paste0(
            "Boxplots of ", response_name, " by ", group_name,
            " (boxes show median and interquartile range, whiskers ",
            "the range excluding outliers); blue points are ",
            "(jittered) raw observations. Use the boxes to judge ",
            "whether the **shape and spread** are broadly similar ",
            "across groups, which is the condition for reading the ",
            "result as a difference in medians rather than only in ",
            "mean ranks. Letters are from pairwise paired Wilcoxon ",
            "signed-rank post hoc tests: groups sharing a letter are ",
            "not significantly different (\u03b1 = ", alpha, "); ",
            "groups with different letters are significantly ",
            "different. Sharing a letter indicates insufficient ",
            "evidence of a difference, not proof that the groups are ",
            "identical. ",
            if (adjust == "none")
              "**WARNING**: no p-value correction was applied."
            else
              paste0("P-values were adjusted with ", adjust, "."))
        } else {
          box_caption <- paste0(
            "Boxplots of ", response_name, " by ", group_name,
            " (boxes show median and interquartile range, whiskers ",
            "the range excluding outliers); blue points are ",
            "(jittered) raw observations. The overall Friedman test ",
            "was not significant (\u03b1 = ", alpha, "), so no post ",
            "hoc letters are shown.")
        }

        cat(paste0("*", gsub("\n", "  \n", box_caption), "*",
                   "   \n  \n"))

        output_list[[response_name]][["Boxplot"]] <- p
      }

      # Merge letters into the summary table
      summary_table <- merge(summary_table,
                             letter_df,
                             by = group_name,
                             all.x = TRUE)

      # merge() re-sorts alphabetically; restore descending-median order.
      summary_table <- summary_table[
        order(match(as.character(summary_table[[group_name]]), ordered_levels)), ,
        drop = FALSE]
      rownames(summary_table) <- NULL

      if(friedman_result$p.value > alpha){
        summary_table$Letters <- "ns"
      }

      # Reorder columns: Letters next to group labels, median-centric layout.
      col_order <- c(group_name, "Letters", "n", "min", "Q1", "median", "Q3", "max", "mean")
      col_order <- col_order[col_order %in% names(summary_table)]
      remaining <- setdiff(names(summary_table), col_order)
      summary_table <- summary_table[, c(col_order, remaining), drop = FALSE]

      output_list[[response_name]][["summary_table"]] <- summary_table

      # Helper: bold the median column for markdown/pander display only
      bold_median <- function(tbl) {
        if ("median" %in% names(tbl)) {
          tbl$median <- paste0("**", tbl$median, "**")
        }
        tbl
      }

      if(friedman_result$p.value < alpha){
        cat("&nbsp;\n   \n")
        cat("  \n## Summary Statistics of: ", response_name, " by ", group_name,
            "  and pairwise post hoc test  \n  \n")
        cat("&nbsp;\n   \n")
        cat(paste("  \nSummary Statistics with pairwise post hoc comparison, `Model:", deparse(current_formula),"`  \n"))
        f_pander(bold_median(f_conditional_round(summary_table, digits = 2)), col_width = 9)

        cat(footnote)

      } else {
        cat("  \n## Data Summary Table of: ", response_name,"
      \n
        Since the Friedman test was **NOT** significant, no post hoc test was performed.\n   \n")

        cat(paste("\n**Summary Statistics** `Model:", deparse(current_formula),"`  \n"))
        f_pander(bold_median(f_conditional_round(summary_table, digits = 2)), col_width = 9)
      }

      if(output_type != "rmd" &&  i < length(lhs)){
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
  suppressMessages(
    utils::capture.output(
      output_list <- generate_report(),
      file = nullfile()
    )
  )
  class(output_list) <- "f_friedman"



  # Here the documents are constructed.
  if (output_type %in% c("word", "pdf")) {

    message(paste0("Saving output in: ", output_path))

    word_pdf_preamble <- function(){ paste0("
---
title: \"Friedman Test Report\"
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

    rmd_content <- paste(
      word_pdf_preamble(),
      paste(generated_markdown, collapse = "\n"),
      sep = "\n"
    )

    writeLines(rmd_content, temp_output_file)

    rmarkdown::render(
      temp_output_file,
      output_file = output_path,
      intermediates_dir = temp_output_dir,
      knit_root_dir = temp_output_dir,
      quiet = TRUE,
      output_format = paste0(output_type, "_document")
    )

    if(open_generated_files == TRUE){
      f_open_file(output_path)
    }

    return(invisible(output_list))

  } else if (output_type == "excel") {

    message(paste0("Saving output in: ", output_path))

    # Extract all summary tables and keep their names
    summary_tables <- lapply(output_list, function(obj) obj$summary_table)
    # Drop NULLs (e.g. skipped responses)
    keep <- !vapply(summary_tables, is.null, logical(1))
    summary_tables <- summary_tables[keep]

    write_xlsx(summary_tables, path = output_path)

    if(open_generated_files == TRUE){
      f_open_file(output_path)
    }

    return(invisible(output_list))

  } else if (output_type == "rmd"){

    if (is.null(opts_knit$get("output.dir"))) {
      opts_knit$set(output.dir = tempdir())
    }

    generated_markdown <- capture.output(generate_report(output = FALSE))
    clean_rmd_output <- paste(generated_markdown, collapse = "\n")
    output_list[["rmd"]] <- clean_rmd_output

    return(invisible(output_list))

  } else if (output_type == "default"){
    return(output_list)

  } else if (output_type == "console"){
    print(output_list)
    return(invisible(output_list))

  } else {
    warning("Invalid output format specified. No file generated.")

  }

  # Remove the temporary R Markdown file
  invisible(suppressWarnings(file.remove(temp_output_file)))

}


# Internal helper: build the pairwise (response ~ group) formula used by
# rstatix::wilcox_test() for the post hoc, from the group | block formula.
current_formula_to_pairwise <- function(response_name, group_name) {
  as.formula(paste0(response_name, " ~ ", group_name))
}


#' @export
print.f_friedman <- function(x, ...) {
  # Flag to know if post hoc explanation should be printed.
  flag_posthoc_used <- FALSE

  line_width <- 72

  # Loop over each response
  for (category in names(x)) {
    sublist <- x[[category]]

    # Skip non-result entries (e.g. the "rmd" character string)
    if (!is.list(sublist)) next

    # Skipped responses (invalid design)
    if (isTRUE(sublist$skipped)) {
      cat("\n==========================================================\n")
      cat("Response", category, "was skipped (invalid design for Friedman test).")
      cat("\n==========================================================\n")
      next
    }

    cat("\n==========================================================\n")
    cat("Results of testing", sublist$friedman.test$data.name)
    cat("\n==========================================================\n")

    print(sublist$friedman.test)

    cat("Kendall's W effect size =", round(sublist$effect_size$effsize, 3),
        "(", as.character(sublist$effect_size$magnitude), "effect )\n")

    if (sublist$friedman.test$p.value < sublist$alpha) {
      cat(
        "\nSummary table with pairwise post hoc:",
        sublist$friedman.test$data.name,
        "\n"
      )
      flag_posthoc_used <- TRUE
    } else {
      cat(
        "\nNo differences found, summary table of:",
        sublist$friedman.test$data.name,
        "\n"
      )
    }

    f_pander(sublist$summary_table, col_width = 7)
    if (flag_posthoc_used == TRUE) {

      posthoc_used <- paste0("
Note 1 (Assumptions): The pairwise paired Wilcoxon signed-rank test does not assume normality, but implies a test for difference in medians only if distribution shapes are similar across groups. If shapes or spreads differ substantially (check the boxplot and the within-block trace plot via plot(...)), the result reflects a difference in paired ranks rather than medians.\n
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

      wrapped_text <- strwrap(posthoc_used, width = line_width)
      cat(paste(wrapped_text, collapse = "\n"), "\n")
    }
  }
}



#' Plot method for f_friedman objects
#'
#' Displays the within-block trace plot and/or the boxplot stored in an
#' \code{f_friedman} object. Plots are only available when the original
#' call used \code{plot = TRUE}.
#'
#' @param x An object of class \code{f_friedman}.
#' @param which Character vector indicating which plots to show. Options are
#'   \code{"distributions"} (within-block trace plot with median trend
#'   line), \code{"Boxplot"}, or both (default).
#' @param ... Additional arguments (currently ignored).
#'
#' @return Returns \code{x} invisibly.
#'
#' @examples
#' set.seed(1)
#' df_rm <- data.frame(
#'   subject   = factor(rep(1:10, each = 3)),
#'   condition = factor(rep(c("A", "B", "C"), times = 10)),
#'   score     = c(rbind(rnorm(10, 4), rnorm(10, 6), rnorm(10, 8)))
#' )
#' result <- f_friedman(score ~ condition | subject, data = df_rm,
#'                      output_type = "default")
#' plot(result)                          # both plots
#' plot(result, which = "Boxplot")       # boxplot only
#'
#' @export
plot.f_friedman <- function(x, which = c("distributions", "Boxplot"), ...) {

  which <- match.arg(which, choices = c("distributions", "Boxplot"),
                     several.ok = TRUE)

  for (category in names(x)) {
    sublist <- x[[category]]

    if (!is.list(sublist)) next
    if (isTRUE(sublist$skipped)) next

    for (plt_name in which) {
      if (!is.null(sublist[[plt_name]])) {
        print(sublist[[plt_name]])
      }
    }
  }

  invisible(x)
}
