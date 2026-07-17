#' Perform multiple \code{glm()} functions with diagnostics, assumption checking, and post hoc analysis
#'
#' Performs Generalized Linear Model (GLM) analysis on a given dataset with options for
#' diagnostics, assumption checking, and post hoc analysis. Several response parameters
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
#' @param effect_plot Logical. If \code{TRUE} (default), estimated marginal
#'   means plots (or interaction plots, when a categorical interaction is
#'   significant) are added after the post hoc table. See Details for what is
#'   drawn and how the plots are stored.
#' @param contrast_plots Logical. If \code{TRUE}, a \strong{contrast forest
#'   plot} is added for each categorical post hoc term: one row per pairwise
#'   comparison, showing the estimated difference between two levels with its
#'   confidence interval and a reference line at zero. A CI that excludes zero
#'   indicates a significant difference; because the interval is on the
#'   difference itself, this "excludes zero" reading is exact (it is the same
#'   information the compact-letter display encodes, but it also shows the
#'   direction and magnitude of each difference). Default \code{FALSE} because
#'   the number of pairwise contrasts grows quickly with the number of factor
#'   levels (k levels give k(k-1)/2 contrasts); turn it on when you want the
#'   detailed pairwise picture. Main-effect contrast plots are stored as
#'   \code{out$y1$contrast_plot_<term>} and interaction cell-contrast plots as
#'   \code{out$y1$interaction_contrast_plot_<term>}. Contrast CIs use the same
#'   \code{adjust} method as the post hoc p-values, so figure and table agree.
#'   The contrasts are computed on the link (model) scale, where differences
#'   are additive and symmetric.
#' @param alpha Numeric. Significance level for tests. Default is \code{0.05}.
#' @param adjust Character string specifying the method used to adjust p-values
#'   for multiple comparisons. Available methods include:
#'   \describe{
#'     \item{"tukey"}{Tukey's Honest Significant Difference method}
#'     \item{"sidak"}{Sidak correction}
#'     \item{"bonferroni"}{Bonferroni correction}
#'     \item{"none"}{No adjustment}
#'     \item{"fdr"}{False Discovery Rate adjustment}
#'   } Default is \code{"sidak"}.
#' @param intro_text Logical. If \code{TRUE}, includes a short explanation about GLM assumptions in the output file.
#' @param type Character string specifying the scale of emmeans post hoc results:
#'   \code{"response"} (back-transformed to original units, e.g. probabilities, counts)
#'   or \code{"link"} (on the linear predictor scale, e.g. log-odds). Default is \code{"response"}.
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
#'   the file is named "dataname_glm_output" in that directory. If an extension is provided the output format specified with option "output_type" will be overruled.
#'   Defaults to \code{file.path(tempdir(), "dataname_summary.pdf")}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory. Default is \code{FALSE}, this avoid unintended changes to the global environment. If \code{save_as} location is specified \code{save_in_wdir} is overwritten by \code{save_as}.
#' @param dispersion_test Logical. If \code{TRUE}, includes a dispersion diagnostic
#'   section in the output: a DHARMa simulation-based test for overdispersion
#'   (Poisson/Binomial), the quasi-dispersion parameter (quasi-families), or a
#'   note explaining why the test is skipped (Bernoulli data). Default is \code{TRUE}.
#' @param influence_threshold Numeric multiplier for the leverage threshold. Observations
#'   with hat values exceeding \code{influence_threshold * mean(hat values)} are flagged
#'   as high-leverage points. Default is \code{2}, a common rule of thumb.
#' @param ... Additional arguments passed to \code{glm()}.
#'
#' @details
#' The function first checks if all specified variables are present in the data and ensures that the response variable is numeric.
#'
#' It fits a Generalized Linear Model (GLM) using the specified formula, family, and data. Model diagnostics are performed with DHARMa (simulation-based residual checks including a KS test, dispersion test, and outlier test). High-leverage observations are flagged using hat values.
#'
#' Significance of each predictor is assessed via Type II Analysis of Deviance (\code{stats::drop1()}). If significant effects are found, post hoc pairwise comparisons are performed using estimated marginal means from \code{emmeans()} with the chosen p-value adjustment method (default: Sidak). When complete separation is detected, the function falls back to likelihood ratio test (LRT) based pairwise comparisons, which are robust to separation.
#'
#' \strong{Effect and interaction plots.} When \code{effect_plot = TRUE}, an
#' estimated marginal means plot (estimate \eqn{\pm} 95\% CI on the response
#' scale, with jittered raw data and compact-letter-display labels) is added
#' after the post hoc table for each categorical predictor. For a significant
#' categorical interaction, interaction plots are drawn instead: a two-way
#' interaction uses the x-axis plus colour (both orientations), while three-
#' and four-way interactions add facet panels for the remaining factor(s),
#' with one plot per choice of x-axis factor. Interactions involving five or
#' more categorical factors are not plotted (a warning is issued); consult the
#' post hoc cell-means table instead. Estimates follow the \code{type}
#' argument, so for non-gaussian families they are back-transformed to the
#' response scale. The plots themselves are kept clean for publication (data,
#' axes, and legend only); the descriptive label and explanatory caption are
#' emitted as text above and below each figure in the report. All effect and
#' interaction plots are \pkg{ggplot2} objects and are stored in the returned
#' object (e.g. \code{out$y1$effect_plot_treatment},
#' \code{out$y1$interaction_plot_a_b_1}) so they can be retrieved and
#' customised afterwards. Matches \code{\link{f_aov}}.
#'
#' More response variables can be added using \code{+} (e.g., \code{response1 + response2 ~ predictor}) to fit a sequential GLM for each response variable, captured in one output file.
#'
#' Outputs can be generated in multiple formats ("pdf", "word", "excel" and "rmd") as specified by \code{output_type}. The function also closes any open 'Word' files to avoid conflicts when generating 'Word' documents. If \code{output_type = "rmd"} is used it is advised to use it in a chunk with \{r, echo=FALSE, results='asis'\}
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
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @return An object of class 'f_glm' (a named list, one entry per response variable) containing:
#'   \describe{
#'     \item{model}{The fitted \code{glm} object.}
#'     \item{summary}{Output of \code{summary(glm_fit)}.}
#'     \item{drop1}{Type II Analysis of Deviance table from \code{stats::drop1()}.}
#'     \item{diagnostics}{DHARMa residual checks and hat-value based leverage diagnostics.}
#'     \item{posthoc}{Estimated marginal means, pairwise comparisons, CLD letters, and summary table.}
#'     \item{sep_flag}{Logical indicating whether complete separation was detected.}
#'     \item{lrt_pct_explained}{McFadden's Pseudo-R\eqn{^2}.}
#'   }
#'   Using the option \code{output_type}, it can also generate output in the form of: R Markdown code, 'Word', 'pdf', or 'Excel' files. Includes print and plot methods for 'f_glm' objects.
#'
#' @examples
#' \donttest{
#' # GLM Binomial example with output to console
#' mtcars_mod <- mtcars
#' mtcars_mod$cyl <- as.factor(mtcars_mod$cyl)
#'
#' glm_bin <- f_glm(vs ~ cyl,
#'                  family = binomial,
#'                  data = mtcars_mod,
#'                  output_type = "default")
#' print(glm_bin)
#'
#' # GLM Binomial example with output to MS Word file
#' glm_bin_word <- f_glm(vs ~ cyl,
#'                  family = binomial,
#'                  data = mtcars_mod,
#'                  output_type = "word"
#'                  )
#'
#' # GLM Poisson example with output to rmd text
#' data(warpbreaks)
#'
#' glm_pos <- f_glm(breaks ~ wool + tension,
#'                  data = warpbreaks,
#'                  family = poisson(link = "log"),
#'                  intro_text = FALSE,
#'                  output_type = "rmd")
#' cat(glm_pos$rmd)
#' }
#'
#' @importFrom stats drop1
#' @importFrom utils combn
#' @export
#'
f_glm <- function(
    formula,                       # glm function formula
    family = gaussian(),           # family for the GLM
    data = NULL,                   # data.frame used for glm
    diagnostic_plots = TRUE,       # Show diagnostic plots in output files
    effect_plot = TRUE,            # Show estimated-means / interaction plots
    contrast_plots = FALSE,        # Show pairwise contrast forest plots
    alpha = 0.05,                  # Significance level for tests
    adjust = "sidak",              # Method used to adjust p-values
    type = "response",             # Scale of emmeans posthoc results
    intro_text = TRUE,  # Print explanation about GLM assumptions
    dispersion_test = TRUE,        # Print dispersion test
    output_type = "default",           # Output type
    save_as = NULL,                # name of the output dir and file (name and type)
    save_in_wdir = FALSE,          # Save in working directory
    close_generated_files = FALSE, # Close open files
    open_generated_files = interactive(),   # Open files after creation
    influence_threshold = 2,       # Leverage threshold
    ...) {

  ########## Reset initial settings on exit #################################
  .session_state <- save_session_state()  # Helper function: helper_session_state
  on.exit(restore_session_state(.session_state), add = TRUE) # Helper function: helper_session_state


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

  # Output file handling
  temp_output_dir  <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "glm_output.Rmd")

  # Create the output file "output.Rmd" in tempdir()
  file.create(temp_output_file)
  # Create a file_extension switch
  file_extension <- NULL
  # Create a list to store all outputs in this function
  output_list <- list()


  ##### Error checking, converting input, closing files #####

  # match.arg() validates string args and gives clear error messages (CRAN standard)
  output_type <- match.arg(output_type,
                           choices = c("default", "console", "pdf", "word", "excel", "rmd"))
  adjust      <- match.arg(adjust,
                           choices = c("sidak", "bonferroni", "tukey", "none", "fdr"))
  type        <- match.arg(type, choices = c("response", "link"))

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
                                   default_name = paste(data_name, "glm_output", sep = "_"),
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
                                   default_name = paste(data_name, "glm_output", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = file.ext
      )


    }
    else if(!is.null(file_extension)) {

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "glm_output", sep = "_"),
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
                                 default_name = paste(data_name, "glm_output", sep = "_"),
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


  # Warn if LHS has expressions like log(y) before silently stripping them
  check_lhs_is_names(formula) #use helper_check_lhs.R

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
    }
    # Ensure the response variable is numeric
    response_var <- data[[response]]

    if (!is.numeric(response_var)) {
      stop("The response variable must be numeric.")
    }
  }

  for (predictor in predictor_names) {
    if (!(predictor %in% names(data))) {
      stop(paste("Predictor variable", predictor, "not found in the data."))
    }
    # Warn if a numeric predictor looks categorical -- emmeans will evaluate at
    # its mean rather than at each level, producing a single-row table.
    n_unique <- length(unique(stats::na.omit(data[[predictor]])))
    if (is.numeric(data[[predictor]]) && n_unique <= 10) {
      warning(paste0(
        "Predictor '", predictor, "' is numeric with only ", n_unique,
        " unique value(s). emmeans will evaluate at its marginal mean (",
        round(mean(data[[predictor]], na.rm = TRUE), 2),
        "), NOT at each level, resulting in a single-row post hoc table ",
        "instead of one row per group. ",
        "Convert to factor first if this is a categorical variable: ",
        "data$", predictor, " <- as.factor(data$", predictor, ")"
      ), call. = FALSE)
    }
  }

  #### Define Core GLM posthoc and diagnostic functions -----------------------

  check_residuals <- function(model, plot = TRUE) {

    # DHARMa cannot simulate from quasi-families because they lack a PDF.
    if(stats::family(model)$family %in% c("quasipoisson", "quasibinomial", "quasi")) {
      warning("DHARMa diagnostics skipped: Simulation not possible for quasi-families.")
      return(list(
        test_results = "Not performed (Quasi-family)",
        sim_res = NULL,
        path_qq_plot = NULL,
        path_residual_plot = NULL
      ))
    } else {

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
  }

  check_influence <- function(model) {

    hat_values  <- stats::hatvalues(model)
    avg_hat     <- mean(hat_values, na.rm = TRUE)
    influential <- which(hat_values > influence_threshold * avg_hat)

    return(list(
      hat_values = hat_values,
      influential_points = influential
    ))
  }

  # LRT-based pairwise comparisons -- robust to complete separation.
  # For each pair of factor levels, fits a reduced model with those levels merged,
  # then takes the LRT vs the full model. Letters assigned via compact_letters().
  # Returns a named character vector of letters (names = factor levels), or NULL.
  compute_pairwise_lrt_letters <- function(glm_fit, data, predictor_names, alpha, adjust) {

    # Only implemented for a single categorical predictor
    if (length(predictor_names) != 1) return(NULL)
    pred <- predictor_names[1]
    if (!is.factor(data[[pred]])) return(NULL)
    lvls <- levels(data[[pred]])
    if (length(lvls) < 2) return(NULL)

    is_quasi_lrt2 <- family$family %in% c("quasipoisson", "quasibinomial", "quasi")
    lrt_test2     <- if (is_quasi_lrt2) "F" else "Chisq"

    # All unique pairs
    pair_combns <- utils::combn(lvls, 2, simplify = FALSE)
    pair_names  <- vapply(pair_combns,
                          function(p) paste(sort(p), collapse = "-"),
                          character(1))
    raw_p <- stats::setNames(rep(NA_real_, length(pair_combns)), pair_names)

    full_formula <- stats::formula(glm_fit)

    for (i in seq_along(pair_combns)) {
      pair      <- pair_combns[[i]]
      data_temp <- data
      data_temp[[pred]] <- as.character(data[[pred]])
      data_temp[[pred]][data_temp[[pred]] %in% pair] <- "__merged__"
      data_temp[[pred]] <- factor(data_temp[[pred]])

      reduced <- tryCatch(
        stats::glm(full_formula, family = family, data = data_temp, ...),
        error = function(e) NULL
      )

      if (!is.null(reduced)) {
        lrt_res <- tryCatch(
          stats::anova(reduced, glm_fit, test = lrt_test2),
          error = function(e) NULL
        )
        if (!is.null(lrt_res) && nrow(lrt_res) >= 2) {
          p_col2 <- grep("^Pr|^p", names(as.data.frame(lrt_res)),
                         ignore.case = TRUE, value = TRUE)[1]
          if (!is.na(p_col2)) {
            raw_p[i] <- as.numeric(as.data.frame(lrt_res)[2, p_col2])
          }
        }
      }
    }

    # Conservative fallback for failed pairs
    raw_p[is.na(raw_p)] <- 1

    # Map adjust argument to a p.adjust method
    # NOTE: base R p.adjust() does not have a native Sidak method.
    # Bonferroni (alpha/k) is used as a conservative approximation for
    # Sidak (1-(1-alpha)^(1/k)). The difference is negligible for few comparisons.
    adj_method <- switch(adjust,
                         "sidak"      = "bonferroni",   # closest base-R equivalent
                         "tukey"      = "bonferroni",
                         "bonferroni" = "bonferroni",
                         "fdr"        = "fdr",
                         "none"       = "none",
                         "bonferroni"
    )
    adj_p <- stats::p.adjust(raw_p, method = adj_method)

    # Convert adjusted p-values to letters
    letter_result <- tryCatch(
      compact_letters(adj_p,
                      threshold = alpha,
                      reversed = TRUE),
      error = function(e) {
        warning("f_glm: LRT pairwise letter assignment failed (",
                conditionMessage(e), "). Letters fall back to '\u2014'.",
                call. = FALSE)
        NULL
      }
    )
    if (is.null(letter_result)) return(NULL)

    # Return as named character vector: names = factor levels
    trimws(letter_result$Letters[lvls])
  }

  # post hoc analysis with emmeans
  perform_posthoc <- function(model, adjust, sig_effects, response_name, predictor_names, data,
                              sep_flag     = FALSE,
                              emm_is_cells = FALSE,
                              cat_preds    = character(0)) {

    # All formula predictors
    all_specs  <- all.vars(formula[[3]])

    # Categorical predictors only -- continuous ones are held at their mean
    # and must not appear as emmeans specs (CLD on a slope is meaningless)
    cat_specs  <- all_specs[vapply(all_specs, function(v) {
      is.factor(data[[v]]) || is.character(data[[v]])
    }, FUN.VALUE = logical(1))]
    cont_specs <- setdiff(all_specs, cat_specs)

    if (length(cont_specs) > 0) {
      warning(paste0(
        "f_glm: continuous predictor(s) [",
        paste(cont_specs, collapse = ", "),
        "] excluded from emmeans specs. ",
        "CLD letters are only assigned for categorical predictors. ",
        "Continuous predictors are held at their mean."
      ), call. = FALSE)
    }

    # Always use ALL categorical predictors as specs.
    # - emm_is_cells = TRUE  (significant interaction): returns cell means for
    #   every combination -- CLD compares all cells simultaneously.
    # - emm_is_cells = FALSE (no significant interaction): returns marginal means
    #   -- emmeans averages over non-significant factors appropriately.
    # Fall back to all predictors only if no categoricals exist (edge case).
    emm_specs <- if (length(cat_specs) > 0) cat_specs else all_specs

    emm   <- emmeans::emmeans(model,
                              specs = emm_specs,
                              type  = type,
                              level = 1 - alpha)

    pairs_emm <- pairs(emm, adjust = adjust)

    mult_cld <- cld_emmeans(emm,
                            alpha = alpha,
                            Letters = letters,
                            adjust = adjust,
                            decreasing = TRUE)

    # Convert the result to a data frame
    summary_table <- as.data.frame(mult_cld)
    rownames(summary_table) <- NULL
    names(summary_table)[names(summary_table) == ".group"] <- "Letter"

    if (!sig_effects) {
      # Overall effect not significant -- all groups in one class
      summary_table$Letter <- "ns"
    } else if (sep_flag) {
      # Separation detected: Wald-based pairwise tests are unreliable.
      # Attempt LRT-based pairwise comparisons instead.
      lrt_letters <- compute_pairwise_lrt_letters(
        model, data, predictor_names, alpha, adjust
      )
      if (!is.null(lrt_letters)) {
        pred <- predictor_names[1]
        summary_table$Letter <- lrt_letters[as.character(summary_table[[pred]])]
        summary_table$Letter[is.na(summary_table$Letter)] <- "\u2014"
      } else {
        summary_table$Letter <- "\u2014"
      }
    }

    # Data summary table groups by the same categorical specs as emmeans.
    # This keeps the raw-data table and the model-based table in sync:
    # - emm_is_cells = TRUE  -> cat_specs includes all factors -> cell-level counts
    # - emm_is_cells = FALSE -> cat_specs includes all factors -> counts per marginal group
    # Continuous predictors are excluded in both cases.
    #
    # Use model$model (the model.frame glm() actually fit on) instead
    # of raw `data` so that any subset / na.action / weights / NA
    # handling applied by glm() is reflected here too. Falls back to
    # `data` only if the user passed `model = FALSE` via `...`, which
    # suppresses model$model. Note: `model` is the first argument of
    # perform_posthoc() (the fitted glm); we do NOT reference
    # `glm_fit` here because that symbol only exists in the outer
    # generate_report() scope.
    summary_data <- if (!is.null(model$model)) model$model else data
    data_summary_table <- f_summary(summary_data,
                                    response_name,
                                    emm_specs,
                                    show_name = FALSE,
                                    digits    = NULL
    )$output_df

    # Identify the grouping key (e.g., "treatment")
    common_cols <- intersect(names(summary_table), names(data_summary_table))

    # Dynamically identify which "mean" column exists (emmean, rate, prob, or response)
    # and which stat columns exist (SE, confidence limits, etc.)
    target_cols <- c("emmean", "rate", "prob", "response",
                     "SE", "lower.CL", "upper.CL", "asymp.LCL", "asymp.UCL",
                     "Letter")

    # Select only the columns that are actually present in the summary_table
    existing_cols <- intersect(names(summary_table), target_cols)

    # Combine grouping columns with the detected data columns
    emm_cols <- c(common_cols, existing_cols)

    # Subset the Emmeans table using the correctly identified columns
    emm_subset <- summary_table[, emm_cols]

    # Getting n from data_summary_table
    n_df <- as.data.frame(data_summary_table[common_cols])
    n_df$n <- data_summary_table$n

    # Perform the Merge
    #    sort = FALSE -> CRITICAL! Prevents R from re-shuffling rows alphabetically
    post_hoc_summary_table <- merge(x = emm_subset,
                                    y = n_df,
                                    by = common_cols,
                                    all.x = TRUE,
                                    sort = FALSE)

    # Prevent breaking of emmean col title
    names(post_hoc_summary_table)[names(post_hoc_summary_table) == "emmean"] <- "emmean.."

    # Prepare table footnote CLD text string
    if (exists("mult_cld")) {

      if (sep_flag) {
        # Explain that LRT-based pairwise tests were used instead of Wald
        cld_text <- paste0(
          "\n**[!] Separation detected, LRT pairwise comparisons used:**  \n",
          "Complete separation was detected (at least one group perfectly predicts the outcome), ",
          "making Wald-based pairwise tests unreliable. ",
          "Letters were assigned using **likelihood ratio tests** (LRT): for each pair of groups, ",
          "a reduced model with those groups merged was compared against the full model via LRT. ",
          "This approach is robust to separation.  \n",
          "P-values were adjusted using the **",
          switch(adjust,
                 "sidak" = "Bonferroni method (conservative approximation; base R lacks native \u0160id\u00e1k)",
                 "bonferroni" = "Bonferroni method",
                 "fdr" = "FDR method",
                 "none" = "no adjustment (Fisher's LSD)",
                 "tukey" = "Bonferroni method (Tukey HSD not available for LRT; Bonferroni used instead)",
                 adjust
          ), "**.  \n",
          if (any(summary_table$Letter == "\u2014", na.rm = TRUE)) {
            paste0(
              "If letters show '\u2014', LRT comparisons could not be computed ",
              "(e.g. more than one predictor). ",
              "Refer to the **Type II Analysis of Deviance** table for the overall test.\n"
            )
          } else {
            ""
          }
        )
      } else {




        # Extract the mult_cld text to adjust it
        msgs <- attr(mult_cld, "mesg")

        # Substitute 'alpha' with '\u03B1'
        msgs <- gsub("alpha", "\u03B1",  msgs)

        note_idx <- grep("^NOTE:", msgs, ignore.case = TRUE)
        if (length(note_idx)) {
          msgs[note_idx] <- paste0(
            "*Note: Groups in the \"Letters\" column sharing the same letter are ",
            "**not** significantly different (\u03B1 = ", alpha, "). Groups with ",
            "different letters are significantly different. Sharing a letter ",
            "indicates insufficient evidence to claim a difference; it does not ",
            "prove the groups are identical.*\n"
          )
        }

        cld_text <- paste(msgs, collapse = "  \n")


        # Extract min and max once to keep code clean and efficient
        min_df <- min(summary_table$df, na.rm = TRUE)
        max_df <- max(summary_table$df, na.rm = TRUE)

        # add the degrees of freedom
        cld_text <- paste0("Degrees of freedom: ",
                           if (min_df != max_df) {
                             paste(min_df, "-", max_df)
                           } else {
                             max_df
                           }, "  \n", cld_text
        )
      } # end else (no separation)
    } else {
      cld_text <- ""
    }


    return(
      list(
        emm = emm,
        pairs = pairs_emm,
        cld_text = cld_text,
        post_hoc_summary_table = post_hoc_summary_table,
        emmeans_table = summary_table,
        data_summary_table = data_summary_table
      )
    )

  }

  #### Output formatting functions -------------------------------------
  word_pdf_preamble <- function(){

    paste0("
---
title: \"Generalized Linear Model Report\"
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
  - \\DeclareUnicodeCharacter{03B1}{\\ensuremath{\\alpha}}
  - \\DeclareUnicodeCharacter{03B2}{\\ensuremath{\\beta}}
  - \\DeclareUnicodeCharacter{03BB}{\\ensuremath{\\lambda}}
  - \\DeclareUnicodeCharacter{03C6}{\\ensuremath{\\varphi}}
  - \\DeclareUnicodeCharacter{03C7}{\\ensuremath{\\chi}}
  - \\DeclareUnicodeCharacter{00B2}{\\ensuremath{^2}}
  - \\DeclareUnicodeCharacter{00D7}{\\ensuremath{\\times}}
  - \\DeclareUnicodeCharacter{2014}{\\textemdash}
  - \\DeclareUnicodeCharacter{2264}{\\ensuremath{\\leq}}
  - \\DeclareUnicodeCharacter{2265}{\\ensuremath{\\geq}}
  - \\DeclareUnicodeCharacter{2192}{\\ensuremath{\\rightarrow}}
  - \\DeclareUnicodeCharacter{221A}{\\ensuremath{\\surd}}
  - \\DeclareUnicodeCharacter{2212}{\\ensuremath{-}}
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


### How to choose:\n

- The *family* should match the distribution of your response variable.
- **The Link Function:** The **Canonical Link** is the standard **\"default\"** option for that family, this is usually the best place to start.
You might choose an alternative link for specific interpretation reasons (e.g., using a *log* link on Gaussian data to model percent changes).

## GLM Assumptions

1.  Correct distributional family.
2.  Appropriate link function.
3.  Independence of observations.
4.  Linear relationship between predictors and link-transformed response.
5.  **No overdispersion** (crucial for Poisson and grouped Binomial; not applicable to binary 0/1 data).

## DHARMa Diagnostics

The DHARMa package (short for *Diagnostics for HierArchical Regression Models*) provides simulation-based diagnostics to check the assumptions of your GLM.

## QQ Plot (Left Diagnostic Plot)

The QQ plot contains three specific statistical tests:

### 1. KS Test (Kolmogorov-Smirnov)
The KS test checks if your model's quantile residuals follow the expected **uniform** distribution.
* **Significant (low p-value):** Suggests the model does not fit the data well.
* *Note: With large datasets, this test is too sensitive. It often flags tiny, unimportant deviations. Always look at the plot yourself rather than relying solely on the p-value.*

### 2. Dispersion Test
This test checks for **overdispersion** (too much variance) or **underdispersion** (too little variance).

* **Where it matters:** This is most critical for **Poisson** (counts) and **Binomial** (proportions) families.
* **The Problem:** Real-world data is often \"clumped\" (e.g., disease cases within families), which creates more noise (variance) than the model expects, i.e. was assumed.
* **The Consequence:** If you ignore overdispersion, the model becomes \"overconfident.\" This leads to **false significance** (spurious significance); the model claims a result is statistically significant when it is actually just random noise.
* **The Fix:** If the dispersion test is significant (p < \u03b1), you should switch to a family that handles extra variance:
    * For Counts: **Negative Binomial** (e.g., package `MASS` or `glmmTMB`).
    * For Proportions: **Beta-Binomial** (e.g., package `glmmTMB`).

### 3. Outlier Test
This test checks if your data has more extreme values (outliers) than the model expects. A significant result means your data contains unexpected extremes.

## Residuals vs Model Predictions (Right Diagnostic Plot)

This plot shows your residuals (errors) on the Y-axis against the model's predictions on the X-axis.

* **What to look for:** In a good model, the dots should be **randomly scattered** around the middle line (0.5), with no clear patterns.
* **Red Stars:** These are outliers--observations that are more extreme than anything the model simulated.\n
&nbsp;\n  \n
**Common Problems to Spot:**

* *Funnel shape:* The variance is changing (heteroscedasticity).
* *U-shape or Hump:* You are missing a predictor or the relationship is not linear.
* *Patterns:* Any clear pattern suggests the model is failing to capture some structure in the data.
\n
<div style=\"page-break-after: always;\"></div>
\\newpage"
    )
  }

  #### Main analysis function and function combinations ------------------------------
  generate_report <- function(output = TRUE) {

    # Create a list to store all outputs in this function
    output_list <- list()


    # Multiple-response warning: only shown when > 1 response variable is analysed.
    # Fires regardless of intro_text so the user always sees it in this situation.
    if (length(lhs) > 1) {
      k          <- length(lhs)
      fwer_pct   <- round((1 - (1 - alpha)^k) * 100, 1)
      bonf_alpha <- round(alpha / k, 4)
      cat(paste0(
        "\n\n***\n\n",
        "**[!] NOTE: Multiple Testing Across ", k, " Response Variables**  \n\n",
        "This report runs ", k, " independent GLMs on the same dataset. ",
        "The **", adjust,"** correction keeps each individual test honest, it guards against ",
        "false positives among the pairwise group comparisons, but it offers no protection ",
        " against the accumulation of error across all ",k," tests combined. ",
        "\nAt \u03b1 = ", alpha, " per test, the probability of obtaining at least one ",
        "spurious significant result across all ", k, " responses is approximately ",
        "**", fwer_pct, "%** ($1 - (1 - ", alpha, ")^{", k, ",}$ assuming independence). ",
        "This risk is highest in exploratory studies; it is less of a concern when ",
        "each response has a clear a priori hypothesis.  \n\n",
        "**Possible remedies:**  \n",
        "\n- **Bonferroni** (conservative): re-run with `alpha = ", bonf_alpha,
        "` (\u03b1 / ", k, ").  \n",
        "\n- **False Discovery Rate (FDR)**: apply `p.adjust(p_values, method = \"fdr\")` to the ",
        k, " GLM p-values after the fact.  \n",
        "\n- **Pre-registration**: if each response was a pre-specified (primary) study ",
        "outcome, correction may not be required; document this decision ",
        "explicitly.  \n ",
        "\n\n***\n\n"
      ))
    }

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
      glm_fit$call$data    <- as.symbol(data_name)

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

      # Compute drop1 (Type II) and LRT FIRST -- needed for sig_effects below
      is_quasi_store <- family$family %in% c("quasipoisson", "quasibinomial", "quasi")
      lrt_test_store <- if (is_quasi_store) "F" else "Chisq"

      output_list[[response_name]][["drop1"]] <- tryCatch(
        stats::drop1(glm_fit, test = lrt_test_store),
        error = function(e) NULL
      )
      output_list[[response_name]][["lrt"]] <- tryCatch(
        stats::anova(glm_fit, test = lrt_test_store),
        error = function(e) NULL
      )
      output_list[[response_name]][["lrt_null_dev"]]      <- round(glm_fit$null.deviance, 3)
      output_list[[response_name]][["lrt_null_df"]]       <- glm_fit$df.null
      output_list[[response_name]][["lrt_resid_dev"]]     <- round(glm_fit$deviance, 3)
      output_list[[response_name]][["lrt_resid_df"]]      <- glm_fit$df.residual
      output_list[[response_name]][["lrt_pct_explained"]] <- round(
        1 - (glm_fit$deviance / glm_fit$null.deviance), 3
      )

      # sig_effects: based on LRT p-value from drop1 (not Wald z-tests).
      # Wald p-values are unreliable under complete separation -- LRT is robust.
      # If drop1 failed, fall back to Wald as a last resort.
      sig_effects <- {
        drop1_res_sig <- output_list[[response_name]][["drop1"]]
        if (!is.null(drop1_res_sig)) {
          drop1_df_sig <- as.data.frame(drop1_res_sig)
          p_col_sig    <- grep("^Pr|^p", names(drop1_df_sig), ignore.case = TRUE, value = TRUE)[1]
          if (!is.na(p_col_sig)) {
            p_vals_sig <- drop1_df_sig[[p_col_sig]]
            p_vals_sig <- p_vals_sig[!is.na(p_vals_sig)]
            length(p_vals_sig) > 0 && min(p_vals_sig) < alpha
          } else {
            FALSE
          }
        } else {
          # Fallback: Wald (minus intercept row)
          coef_pvals <- summary(glm_fit)$coefficients[-1, 4, drop = FALSE]
          nrow(coef_pvals) > 0 && min(coef_pvals) < alpha
        }
      }
      output_list[[response_name]][["sig_effects"]] <- sig_effects

      # Detect (quasi-)complete separation early so perform_posthoc and the
      # plots can handle letter assignment correctly and the user is warned.
      #
      # Separation is a binomial/logistic phenomenon: it describes a predictor
      # (combination) that perfectly predicts a 0/1 outcome, driving ML
      # estimates to +/-Inf. The concept does not apply to gaussian, Poisson,
      # Gamma, etc., where the linear predictor lives on the response scale and
      # large |eta| is ordinary (e.g. a fitted mpg of 27 is not "separation").
      # All three checks below are therefore gated to binomial families; for
      # any other family the flag is simply FALSE.
      is_binomial_fam <- family$family %in% c("binomial", "quasibinomial")

      coef_mat_early <- summary(glm_fit)$coefficients

      # (a) Coefficient heuristic: an SE that dwarfs its estimate is the classic
      #     Wald symptom of separation (estimate driven to +/-Inf, SE explodes),
      #     giving the deceptively wide back-transformed CI. The absolute floor
      #     (se_abs_min) prevents a near-zero estimate with an ordinary SE from
      #     tripping the ratio test spuriously.
      se_abs_min <- 10
      sep_by_coef <- if (is_binomial_fam && nrow(coef_mat_early) > 1) {
        se_e  <- coef_mat_early[-1, "Std. Error"]
        est_e <- coef_mat_early[-1, "Estimate"]
        any(se_e > se_abs_min & se_e > 100 * abs(est_e), na.rm = TRUE)
      } else FALSE

      # (b) Extreme linear predictor: |eta| beyond ~qlogis(1 - 1e-8) means at
      #     least one fitted probability is numerically 0 or 1. Only meaningful
      #     on the logit scale, hence binomial-only.
      sep_by_eta <- if (is_binomial_fam) {
        eta <- tryCatch(stats::predict(glm_fit, type = "link"),
                        error = function(e) NULL)
        !is.null(eta) && any(abs(eta) > 18, na.rm = TRUE)
      } else FALSE

      # (c) Empty outcome cell: any categorical predictor level whose response
      #     is entirely 0 or entirely 1. The direct cause and most
      #     interpretable signal. Mirrors the categorical-predictor detection
      #     used for emmeans below.
      sep_empty_cells <- character(0)
      if (is_binomial_fam) {
        mf_sep <- tryCatch(stats::model.frame(glm_fit), error = function(e) NULL)
        if (is.null(mf_sep))
          mf_sep <- tryCatch(glm_fit$model, error = function(e) NULL)
        if (!is.null(mf_sep) && response_name %in% names(mf_sep)) {
          y_sep <- mf_sep[[response_name]]
          # Response on a 0/1 (or coercible) scale.
          y_num <- suppressWarnings(as.numeric(as.character(y_sep)))
          if (all(stats::na.omit(y_num) %in% c(0, 1))) {
            cat_cols_sep <- names(mf_sep)[vapply(mf_sep, function(v)
              is.factor(v) || is.character(v) || is.logical(v),
              FUN.VALUE = logical(1))]
            cat_cols_sep <- setdiff(cat_cols_sep, response_name)
            for (cc in cat_cols_sep) {
              grp_rate <- tapply(y_num, as.character(mf_sep[[cc]]),
                                 mean, na.rm = TRUE)
              empty <- names(grp_rate)[!is.na(grp_rate) &
                                         (grp_rate %in% c(0, 1))]
              if (length(empty) > 0)
                sep_empty_cells <- c(sep_empty_cells,
                                     paste0(cc, "=", empty))
            }
          }
        }
      }

      sep_flag_early <- isTRUE(sep_by_coef) || isTRUE(sep_by_eta) ||
        length(sep_empty_cells) > 0
      output_list[[response_name]][["sep_flag"]] <- sep_flag_early
      output_list[[response_name]][["sep_empty_cells"]] <- sep_empty_cells

      # Emit an immediate, plain-language warning so the issue is not silently
      # buried in a deceptively wide confidence interval.
      if (sep_flag_early) {
        sep_detail <- if (length(sep_empty_cells) > 0)
          paste0(" Group(s) with no outcome variation: ",
                 paste(sep_empty_cells, collapse = ", "), ".")
        else ""
        warning("f_glm: (quasi-)complete separation detected for response '",
                response_name, "'.", sep_detail,
                " Maximum-likelihood estimates diverge, so Wald standard ",
                "errors and back-transformed confidence intervals are not ",
                "interpretable (this is why a CI can span nearly 0-1 even ",
                "when a group is entirely 0 or entirely 1). Letters fall ",
                "back to likelihood-ratio tests. Consider penalised/Firth ",
                "logistic regression (logistf::logistf or ",
                "brglm2::brglm_fit) for finite estimates.",
                call. = FALSE, immediate. = TRUE)
      }

      # ---- Interaction-aware emmeans mode (mirrors f_aov logic) ----
      # Parse drop1 term names to classify interactions vs main effects.
      # Determines whether emmeans returns cell means or marginal means,
      # and what note to show the user -- matching f_aov's emm_is_cells approach.
      drop1_for_int  <- output_list[[response_name]][["drop1"]]

      # Categorical predictors are read from the fitted model FRAME, not the
      # raw `data`, so that on-the-fly conversions in the formula (e.g.
      # factor(dose), as.factor(g), ordered(d)) are honoured. Reading from
      # raw `data` would miss these: a numeric column wrapped in factor()
      # would look continuous, get dropped from cat_preds, and a genuine
      # categorical interaction would then fail the two-categorical-factor
      # test and fall back to per-main-effect means plots instead of the
      # expected interaction plot. This mirrors f_aov(), which keys off the
      # model frame for exactly this reason.
      mf_int <- tryCatch(stats::model.frame(glm_fit), error = function(e) NULL)
      if (is.null(mf_int)) mf_int <- tryCatch(glm_fit$model,
                                              error = function(e) NULL)
      fixed_labels_int <- tryCatch(
        attr(stats::terms(glm_fit), "term.labels"),
        error = function(e) character(0)
      )
      cat_preds <- character(0)
      if (!is.null(mf_int)) {
        # Single-column (main-effect) term labels that exist in the model
        # frame; interaction labels (containing ":") are excluded here.
        main_labels <- fixed_labels_int[!grepl(":", fixed_labels_int, fixed = TRUE)]
        main_labels <- main_labels[main_labels %in% names(mf_int)]
        if (length(main_labels) > 0L) {
          is_cat_int <- vapply(mf_int[main_labels], function(v)
            is.factor(v) || is.character(v) || is.logical(v),
            FUN.VALUE = logical(1))
          cat_preds <- main_labels[is_cat_int]
        }
      }
      # Fallback to the raw-data heuristic only if the model frame gave
      # nothing usable (keeps behaviour for unusual fits).
      if (length(cat_preds) == 0L) {
        cat_preds <- predictor_names[vapply(predictor_names, function(v)
          (v %in% names(data)) &&
            (is.factor(data[[v]]) || is.character(data[[v]])),
          FUN.VALUE = logical(1))]
      }

      emm_is_cells   <- FALSE
      sig_interactions  <- character(0)
      sig_main_effects  <- character(0)
      ns_main_effects   <- character(0)
      ns_interactions   <- character(0)

      if (!is.null(drop1_for_int) && length(cat_preds) > 1) {
        d1_df      <- as.data.frame(drop1_for_int)
        term_names <- rownames(d1_df)
        p_col_int  <- grep("^Pr|^p", names(d1_df), ignore.case = TRUE, value = TRUE)[1]

        if (!is.na(p_col_int)) {
          p_vals_int <- d1_df[[p_col_int]]
          names(p_vals_int) <- term_names

          # Split into interaction terms (contain ":") and main effects
          int_terms  <- term_names[grepl(":", term_names, fixed = TRUE)]
          main_terms <- term_names[!grepl(":", term_names, fixed = TRUE) &
                                     term_names != "<none>"]

          # Keep only terms involving categorical predictors
          int_terms  <- int_terms[vapply(int_terms, function(t) {
            parts <- strsplit(t, ":")[[1]]
            all(parts %in% cat_preds)
          }, FUN.VALUE = logical(1))]

          sig_interactions <- int_terms[
            !is.na(p_vals_int[int_terms]) & p_vals_int[int_terms] < alpha]
          ns_interactions  <- int_terms[
            is.na(p_vals_int[int_terms]) | p_vals_int[int_terms] >= alpha]
          sig_main_effects <- main_terms[
            !is.na(p_vals_int[main_terms]) & p_vals_int[main_terms] < alpha &
              main_terms %in% cat_preds]
          ns_main_effects  <- main_terms[
            (is.na(p_vals_int[main_terms]) | p_vals_int[main_terms] >= alpha) &
              main_terms %in% cat_preds]

          emm_is_cells <- length(sig_interactions) > 0
        }
      }

      output_list[[response_name]][["emm_is_cells"]]      <- emm_is_cells
      output_list[[response_name]][["sig_interactions"]]  <- sig_interactions
      output_list[[response_name]][["sig_main_effects"]]  <- sig_main_effects
      output_list[[response_name]][["ns_main_effects"]]   <- ns_main_effects
      output_list[[response_name]][["ns_interactions"]]   <- ns_interactions
      output_list[[response_name]][["cat_preds"]]         <- cat_preds

      output_list[[response_name]][["posthoc"]] <- perform_posthoc(
        glm_fit, adjust, sig_effects, response_name, predictor_names, data,
        sep_flag    = sep_flag_early,
        emm_is_cells = emm_is_cells,
        cat_preds   = cat_preds
      )


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

        if(!is.null(path_qq_plot)){
          cat(paste0("![](", path_qq_plot, ")"), "   \n  \n")
        } else {
          cat("\n**Note:** DHARMa diagnostic plots were skipped.\n")
          cat("Simulation-based residuals are not currently supported for quasi-families (e.g., Quasipoisson, Quasibinomial) because they lack a defined probability distribution for simulation.\n \n")
          # Pagebreak
          if(output_type != "rmd"){
            # Pagebreak
            cat("
<div style=\"page-break-after: always;\"></div>
\\newpage
        ")}
        }
      }

      # ---- Fix 5: Dispersion test text output (gated by dispersion_test param) ----
      if (isTRUE(dispersion_test)) {
        cat("\n## Dispersion Diagnostics of:", response_name, "\n\n")

        is_quasi    <- family$family %in% c("quasipoisson", "quasibinomial", "quasi")
        is_bernoulli <- family$family == "binomial" &&
          all(data[[response_name]] %in% c(0, 1, NA))

        if (is_bernoulli) {
          # Bernoulli (0/1) data cannot be overdispersed by definition --
          # variance is strictly p(1-p). Reporting a dispersion test would mislead students.
          cat(paste0(
            "*Dispersion test skipped: the response variable `", response_name,
            "` contains only 0s and 1s (Bernoulli data). ",
            "Overdispersion is mathematically impossible for binary outcomes;",
            "the variance is fixed at p(1\u2212p) and cannot exceed this. ",
            "Overdispersion in binomial models only arises with **grouped data** ",
            "(e.g., successes out of N trials per row, where N > 1).*  \n\n"
          ))
        } else if (is_quasi) {
          # For quasi-families, report the quasi-dispersion parameter from the model
          phi <- summary(glm_fit)$dispersion
          cat(paste0(
            "**Quasi-dispersion parameter (\u03c6):** ", round(phi, 4), "  \n\n",
            "The quasi-dispersion parameter estimates the degree of over- or underdispersion ",
            "relative to the nominal variance. A value close to **1** indicates no ",
            "extra-dispersion. Values **> 1** indicate overdispersion (more variance than ",
            "the model assumes); values **< 1** indicate underdispersion.  \n\n",
            "**What the quasi-family does mechanically:** standard errors are multiplied by ",
            "\u221a\u03c6 (the square root of \u03c6). When \u03c6 > 1, this inflates SEs, ",
            "making significance tests more conservative and correcting the overconfidence ",
            "that would arise from ignoring the extra variance.  \n\n",
            "*Note: DHARMa simulation-based tests are not available for quasi-families ",
            "because they lack a defined probability density function.*  \n\n"
          ))
        } else {
          # For proper non-Bernoulli families, extract DHARMa's dispersion test result
          disp_result <- output_list[[response_name]][["diagnostics"]][["residuals"]][["test_results"]][["dispersion"]]
          if (!is.null(disp_result)) {
            disp_stat <- round(as.numeric(disp_result$statistic), 4)
            disp_p    <- round(disp_result$p.value, 4)
            disp_sig  <- disp_p < alpha

            if (disp_stat > 1) {
              disp_direction <- "**overdispersion** (more variance than the model assumes)"
              disp_fix <- paste0(
                "  \n**Recommended action:** Switch to a family that accounts for extra variance:\n",
                "  - Counts (Poisson): use **Negative Binomial** (`MASS::glm.nb()` or `glmmTMB`).\n",
                "  - Proportions (Binomial): use **Beta-Binomial** (`glmmTMB`).\n",
                "  - Or use `quasipoisson` / `quasibinomial` to correct standard errors without changing the family."
              )
            } else {
              disp_direction <- "**underdispersion** (less variance than the model assumes)"
              disp_fix <- "  \n*Underdispersion is less common; consider whether the model or data are correctly specified.*"
            }

            disp_interp <- if (disp_sig) {
              paste0("The dispersion test is **significant** (p = ", disp_p, " \u2264 \u03b1 = ", alpha,
                     "), indicating ", disp_direction, ". ", disp_fix)
            } else {
              paste0("The dispersion test is **not significant** (p = ", disp_p, " > \u03b1 = ", alpha,
                     "), suggesting no strong evidence of extra-dispersion. The chosen family appears appropriate.")
            }

            cat(paste0(
              "**DHARMa Dispersion Test:**  ",
              "Ratio of simulated vs. observed variance: **", disp_stat, "**  \n",
              disp_interp, "  \n\n"
            ))
          } else {
            cat("*DHARMa dispersion test result not available.*  \n\n")
          }
        }
      }

      # ---- Fix 6: Influential points output ----
      {
        infl         <- output_list[[response_name]][["diagnostics"]][["influence"]]
        hat_vals     <- infl$hat_values
        inf_pts      <- infl$influential_points
        avg_hat      <- mean(hat_vals, na.rm = TRUE)
        threshold_val <- round(influence_threshold * avg_hat, 4)

        cat(paste0(
          "\n## High-Leverage Observations of: ", response_name, "\n\n",
          "Leverage (hat values) measures how far each observation's predictor values are ",
          "from the centroid of all predictors. High-leverage points *can* disproportionately ",
          "pull the fitted line, but only do so when they also have large residuals. ",
          "To assess actual **influence** (leverage \u00d7 residual), check Cook's distance ",
          "via `plot(glm_fit, which = 4)`.  \n\n",
          "**Threshold:** ", influence_threshold, " \u00d7 mean leverage = **", threshold_val, "**  \n"
        ))

        if (length(inf_pts) == 0) {
          cat(paste0(
            "**No influential observations detected** above the threshold. ",
            "All hat values are within acceptable range (max = ",
            round(max(hat_vals, na.rm = TRUE), 4), ").  \n\n"
          ))
        } else {
          cat(paste0(
            "**", length(inf_pts), " potentially influential observation(s) detected** ",
            "(row index: ", paste(inf_pts, collapse = ", "), "):  \n\n"
          ))

          inf_df <- data.frame(
            Row        = inf_pts,
            Hat_value  = round(hat_vals[inf_pts], 4)
          )
          f_pander(inf_df)

          cat(paste0(
            "**Recommended action:** Inspect these observations for data entry errors or ",
            "structural reasons why they are outlying. Use `plot(glm_fit)` and Cook's ",
            "distance to assess whether they materially affect coefficients.  \n\n"
          ))
        }
      }

      cat(paste("\n## Observed Descriptives Table of: ", deparse(current_formula), "  \n"))
      f_pander(f_conditional_round(output_list[[response_name]][['posthoc']][['data_summary_table']], digits = 3))
      # Add footnote
      cat(
        "**TIP:** These values represent your actual observed sample characteristics.
Use this table for *Methods* sections or Supplementary materials (to describe the sample).\n  \n
**CAUTION:** For statistical inference (significance letters and *p-values*) and reporting
main findings in the *Results* section, you **must** use the Emmeans table below.\n"
      )

      # Print the glm and posthoc
      cat("\n
## Model Summary of: ", response_name, "\n   \n")

      glm_sum <- output_list[[response_name]][["summary"]]

      # Print call and model info as plain text
      cat(paste0(
        "**Call:** `", deparse(current_formula), "`  \n",
        "**Family:** ", family$family, "  |  ",
        "**Link:** ", family$link, "  |  ",
        "**AIC:** ", round(AIC(glm_fit), 3), "  |  ",
        "**Null deviance:** ", round(glm_fit$null.deviance, 3),
        " (df = ", glm_fit$df.null, ")  |  ",
        "**Residual deviance:** ", round(glm_fit$deviance, 3),
        " (df = ", glm_fit$df.residual, ")  \n\n"
      ))

      # Convergence / separation warning
      if (!glm_fit$converged) {
        cat("**WARNING:** The model did not converge. Coefficient estimates are unreliable. ",
            "Check for complete separation or consider a different model specification.  \n\n")
      }
      if (any(is.na(coef(glm_fit)))) {
        cat("**WARNING:** One or more coefficients are NA, indicating perfect collinearity ",
            "or complete separation.  \n\n")
      }

      # Reuse the sep_flag already computed and stored above
      sep_flag <- output_list[[response_name]][["sep_flag"]]
      coef_mat <- glm_sum$coefficients

      # Coefficients as a pander table -- framed as effect estimates, not a significance test
      cat("**Coefficient Estimates** (direction and magnitude):  \n\n")
      coef_df <- as.data.frame(coef_mat)
      coef_df <- cbind(Term = rownames(coef_df), coef_df)
      rownames(coef_df) <- NULL
      stat_col <- if (family$family %in% c("quasipoisson","quasibinomial","quasi","gaussian"))
        "t value" else "z value"
      names(coef_df) <- c("Term", "Estimate", "Std. Error", stat_col, "Wald p")
      coef_df[["Estimate"]]      <- round(coef_df[["Estimate"]], 4)
      coef_df[["Std. Error"]]    <- round(coef_df[["Std. Error"]], 4)
      coef_df[[stat_col]]        <- round(coef_df[[stat_col]], 3)
      coef_df[["Wald p"]] <- ifelse(
        coef_df[["Wald p"]] < 0.001, "< 0.001",
        as.character(round(coef_df[["Wald p"]], 4))
      )
      f_pander(coef_df)

      # Note below coefficients table
      if (sep_flag) {
        cat(paste0(
          "\n**[!] Separation detected:** At least one predictor perfectly predicts the outcome ",
          "in a subset of the data (indicated by a very large Std. Error). ",
          "Wald p-values are unreliable in this situation; they will appear non-significant ",
          "even for strongly predictive terms. ",
          "**Use the Type II Analysis of Deviance table below for all significance decisions.**  \n\n"
        ))
      } else {
        cat(paste0(
          "\n*The 'Wald p' column is provided for completeness. ",
          "For significance testing use the **Type II Analysis of Deviance** table below, ",
          "which is robust to separation and more reliable for multi-predictor models.*  \n\n"
        ))
      }

      ## ---------- Coefficient forest plot ----------------------------------
      ## Dot-and-whisker plot of the model coefficients with their Wald CIs and
      ## a reference line at zero. GLM coefficients are on the LINK scale (e.g.
      ## log-odds for a logit model, log-rate for a log link), so the caption
      ## says so and the reference levels of each factor are named explicitly
      ## (helper_coef_ref_caption): under treatment contrasts each factor row is
      ## a contrast against that factor's reference level, and which level that
      ## is silently determines what every coefficient means. The intercept is
      ## dropped (it is the reference-cell value, not an effect, and its
      ## magnitude usually dwarfs the others). Wald CIs are used so the plot
      ## matches the coefficient table above; they are unreliable under
      ## separation, so the plot is skipped when sep_flag is TRUE.
      if (isTRUE(effect_plot) && !isTRUE(sep_flag)) {
        coef_fp <- tryCatch({
          cf <- as.data.frame(coef_mat)
          cf$Term <- rownames(cf)
          est_col <- "Estimate"
          se_col  <- intersect(c("Std. Error", "Std.Error"), names(cf))[1]
          p_col   <- grep("^Pr\\(>", names(cf), value = TRUE)[1]
          if (is.na(se_col)) stop("no SE column")

          cf <- cf[cf$Term != "(Intercept)", , drop = FALSE]
          if (nrow(cf) == 0L) stop("only an intercept; nothing to plot")

          zcrit <- stats::qnorm(1 - alpha / 2)
          fp_df <- data.frame(
            label = cf$Term,
            est   = cf[[est_col]],
            lower = cf[[est_col]] - zcrit * cf[[se_col]],
            upper = cf[[est_col]] + zcrit * cf[[se_col]],
            stringsAsFactors = FALSE
          )
          fp_df$sig <- if (!is.na(p_col))
            ifelse(!is.na(cf[[p_col]]) & cf[[p_col]] < alpha,
                   "significant", "not significant")
          else "not significant"

          # Shared forest-plot drawing (helper_forest_plot); model order keeps
          # the rows aligned with the coefficient table above.
          build_forest_plot(
            fp_df,
            title    = paste0("Coefficients of ", response_name),
            x_label  = "Estimate (95% CI)",
            order_by = "model")
        }, error = function(e) NULL)

        if (!is.null(coef_fp)) {
          output_list[[response_name]][["coef_forest_plot"]] <- coef_fp
          tmp_fp <- tempfile(fileext = ".png")
          n_rows <- length(levels(coef_fp$data$label))
          ok <- tryCatch({
            suppressMessages(
              ggplot2::ggsave(filename = tmp_fp, plot = coef_fp,
                              width = 7,
                              height = max(3, 0.45 * n_rows + 1.5),
                              units = "in", dpi = 200))
            TRUE
          }, error = function(e) FALSE)

          if (isTRUE(ok)) {
            ## GLM coefficients live on the link scale. Only the Gaussian
            ## identity link makes "the response" literally correct; for every
            ## other link the coefficient acts on the linear predictor.
            identity_link <- isTRUE(family$link == "identity")
            scale_phrase <- if (identity_link)
              "the response"
            else
              paste0("the linear predictor (", family$link, " scale)")

            ref_cap <- build_coef_ref_caption(glm_fit, fixed_only = FALSE)

            cat("\n### Coefficient forest plot\n")
            cat(paste0("![](", tmp_fp, ")"), "   \n  \n")
            cat("*Each row is a coefficient (the intercept is omitted) with ",
                "its ", 100 * (1 - alpha), "% Wald CI, on the ",
                if (identity_link) "response" else
                  paste0(family$link, "-link"),
                " scale. The dashed line marks zero: a coefficient at zero ",
                "has no effect relative to its reference. Points to the right ",
                "increase ", scale_phrase, ", points to the left decrease it. ",
                "A CI that touches or crosses zero means the term is not ",
                "distinguishable from its reference at \u03b1 = ", alpha,
                "; a CI clear of zero is a significant effect.*  \n  \n",
                sep = "")

            if (nzchar(ref_cap$reference))
              cat("*", ref_cap$reference, "* ", sep = "")
            if (nzchar(ref_cap$continuous))
              cat("*", ref_cap$continuous, "* ", sep = "")
            cat("*For a factor with k levels these are level-vs-reference ",
                "contrasts, not all pairwise comparisons; see the post hoc ",
                "tables below for the full pairwise picture. For significance ",
                "decisions use the Type II Analysis of Deviance table, which ",
                "is more reliable than Wald CIs for multi-predictor models.* ",
                sep = "")
            if (nzchar(ref_cap$how_to))
              cat("*", ref_cap$how_to, "*  \n  \n", sep = "")
          }
        }
      }

      # ---- Fix 10: Type II Analysis of Deviance via stats::drop1() ----
      # Reuse the already-computed result stored in output_list (no recomputation).
      is_quasi_lrt  <- family$family %in% c("quasipoisson", "quasibinomial", "quasi")
      lrt_test_name <- if (is_quasi_lrt) "F-test" else "\u03c7\u00b2 likelihood ratio test"
      drop1_res     <- output_list[[response_name]][["drop1"]]

      cat("\n### Type II Analysis of Deviance of:", response_name, "\n\n")
      cat(paste0(
        "The table below tests the marginal significance of **each predictor term** ",
        "via `stats::drop1()` (Type II tests). Each term is dropped from the full model ",
        "in turn and tested against the model retaining all other terms; equivalent to ",
        "`car::Anova(type = 2)` but using only base R. ",
        "This is the GLM equivalent of the ANOVA F-table: it answers *\"does this predictor ",
        "improve the model?\"* after accounting for all other terms. ",
        "For single-predictor models this matches the coefficient", stat_col,
        "above; for multi-predictor models these per-term tests are the ones to report.  \n\n"
      ))

      if (!is.null(drop1_res)) {
        drop1_df  <- as.data.frame(drop1_res)
        drop1_df  <- cbind(Term = rownames(drop1_df), drop1_df)
        rownames(drop1_df) <- NULL
        # Round numeric columns
        num_cols <- vapply(drop1_df, is.numeric, FUN.VALUE = logical(1))
        drop1_df[num_cols] <- lapply(drop1_df[num_cols], function(x) round(x, 4))
        # Format p-value column if present
        p_col_name <- grep("^Pr|^p", names(drop1_df), ignore.case = TRUE, value = TRUE)[1]
        if (!is.na(p_col_name)) {
          drop1_df[[p_col_name]] <- ifelse(drop1_df[[p_col_name]] < 0.001, "< 0.001",
                                           as.character(round(as.numeric(drop1_df[[p_col_name]]), 4)))
        }
        #drop1_df <- drop1_df[drop1_df$Term != "<none>", ]
        drop1_df <- drop1_df[!is.na(drop1_df[[p_col_name]]), ]
        rownames(drop1_df) <- NULL
        f_pander(drop1_df)
      } else {
        cat("*Type II deviance table could not be computed.*  \n\n")
      }

      # ---- Fix 11: Likelihood ratio test vs. null model ----
      cat("\n### Model vs. Null Model Comparison of:", response_name, "\n\n")
      lrt <- output_list[[response_name]][["lrt"]]

      if (!is.null(lrt)) {
        # Extract the overall deviance reduction and p-value from the last row
        lrt_df    <- as.data.frame(lrt)
        dev_red   <- round(sum(lrt_df$Deviance, na.rm = TRUE), 4)
        p_col     <- grep("Pr|p.value|p-value", names(lrt_df), ignore.case = TRUE, value = TRUE)[1]
        lrt_p     <- if (!is.na(p_col)) min(lrt_df[[p_col]], na.rm = TRUE) else NA
        lrt_p_fmt <- if (!is.na(lrt_p) && lrt_p < 0.001) "< 0.001" else if (!is.na(lrt_p)) as.character(round(lrt_p, 4)) else NA

        cat(paste0(
          "The **null deviance** (intercept-only model) is ",
          round(output_list[[response_name]][["lrt_null_dev"]], 3), " on ",
          output_list[[response_name]][["lrt_null_df"]], " df. ",
          "The **residual deviance** (fitted model) is ",
          round(output_list[[response_name]][["lrt_resid_dev"]], 3), " on ",
          output_list[[response_name]][["lrt_resid_df"]], " df.  \n\n",
          "\n**McFadden's Pseudo-R\u00b2:** ",
          output_list[[response_name]][["lrt_pct_explained"]],
          "  \n\n",
          "*McFadden's Pseudo-R\u00b2 = 1 \u2212 (Residual deviance / Null deviance). ",
          "It measures how much the model improves over a null (intercept-only) model, ",
          "on a 0 to 1 scale. Unlike R\u00b2 in linear regression, it is **not** a proportion of ",
          "variance explained; values are typically lower: ",
          "0.2 to 0.4 is already considered an excellent fit for GLMs.*  \n\n",
          "&nbsp;  \n\n"
        ))

        if (!is.na(lrt_p)) {
          lrt_sig <- lrt_p < alpha
          cat(paste0(
            "**", lrt_test_name, "** vs. null model: p = **", lrt_p_fmt, "**. ",
            if (lrt_sig) {
              paste0("The model fits **significantly better** than the null model (\u03b1 = ", alpha,
                     "), meaning at least one predictor contributes to explaining the response.")
            } else {
              paste0("The model does **not** fit significantly better than the null model (\u03b1 = ", alpha,
                     "). Interpret results with caution.")
            }, "  \n\n"
          ))
        }

        lrt_df2 <- as.data.frame(lrt)
        lrt_df2 <- cbind(Term = rownames(lrt_df2), lrt_df2)
        rownames(lrt_df2) <- NULL
        # Round numeric columns
        num_cols2 <- vapply(lrt_df2, is.numeric, FUN.VALUE = logical(1))
        lrt_df2[num_cols2] <- lapply(lrt_df2[num_cols2], function(x) round(x, 4))
        # Format p-value column if present
        p_col2 <- grep("^Pr|^p", names(lrt_df2), ignore.case = TRUE, value = TRUE)[1]
        if (!is.na(p_col2)) {
          lrt_df2[[p_col2]] <- ifelse(lrt_df2[[p_col2]] < 0.001, "< 0.001",
                                      as.character(round(as.numeric(lrt_df2[[p_col2]]), 4)))
        }

        lrt_df2[] <- lapply(lrt_df2, function(x) ifelse(is.na(x), "", as.character(x)))
        f_pander(lrt_df2)

        # Warn about Type I (sequential) nature for multi-predictor models
        if (length(predictor_names) > 1) {
          cat(paste0(
            "\n*[!] This table uses **sequential (Type I) tests**, ",
            "per-term p-values depend on the order predictors enter the model. ",
            "For per-term significance, use the **Type II Analysis of Deviance** ",
            "(`drop1`) table above, which tests each term after accounting for all others.*  \n\n"
          ))
        }
      } else {
        cat("*Likelihood ratio test vs. null model could not be computed.*  \n\n")
      }


      # Pagebreak
      if(output_type != "rmd"){
        # Pagebreak
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage
        ")}
      cat("\n
\n## Model post hoc Analysis (Estimated Marginal Means) of: ", response_name, "\n   \n")

      # Retrieve interaction classification results
      emm_is_cells_out  <- output_list[[response_name]][["emm_is_cells"]]
      sig_ints_out      <- output_list[[response_name]][["sig_interactions"]]
      sig_main_out      <- output_list[[response_name]][["sig_main_effects"]]
      ns_main_out       <- output_list[[response_name]][["ns_main_effects"]]
      ns_ints_out       <- output_list[[response_name]][["ns_interactions"]]
      cat_preds_out     <- output_list[[response_name]][["cat_preds"]]

      # Interaction / marginal-means note -- mirrors f_aov's emm_is_cells logic
      if (isTRUE(emm_is_cells_out) && length(sig_ints_out) > 0) {
        sig_int_preds <- unique(unlist(strsplit(sig_ints_out, ":")))
        cat(paste0(
          "\n**NOTE: Significant interaction(s) detected: ",
          paste(sig_ints_out, collapse = ", "), "**  \n",
          "The post hoc table below shows **cell means**, the estimated value for ",
          "every combination of ", paste(sig_int_preds, collapse = " \u00d7 "), ".  \n",
          "Letters compare all cells simultaneously. ",
          "Interpretation should focus on the full interaction pattern, ",
          "not on individual factor effects in isolation.  \n",
          "These cell-means letters are the reference grouping: any effect ",
          "plot that averages over a factor interacting with the one shown ",
          "omits its own letters and refers back to this table.  \n\n"
        ))
      } else if (length(ns_main_out) > 0) {
        cat(paste0(
          "\n**NOTE:** The following term(s) were **not significant** (p \u2265 ", alpha, "): ",
          paste(ns_main_out, collapse = ", "), ".  \n",
          "The table below shows **marginal means**, the model averages over ",
          "non-significant factor(s), correcting for unbalanced designs. ",
          "Letter groups for non-significant terms are not meaningful but are ",
          "shown for completeness.  \n\n"
        ))
      }

      # Interaction term(s) present but not significant: suggest refitting
      # without the interaction term to gain power for the main effects.
      if (length(ns_ints_out) > 0) {
        cat(paste0(
          "\n**NOTE:** The interaction term(s) ",
          paste(ns_ints_out, collapse = ", "),
          " were **not significant** (p \u2265 ", alpha, ").  \n",
          "Consider refitting the model without the interaction term if the ",
          "research question and research setup allow this; the main effects ",
          "are then estimated with more power.  \n\n"
        ))
      }

      # --- DYNAMIC TEXT LOGIC START ---

      # 1. Get the table to check names
      ph_table_check <- output_list[[response_name]][['posthoc']][['post_hoc_summary_table']]

      # 2. Determine the Label (Rate vs Prob vs Mean)
      if (isTRUE(emm_is_cells_out)) {
        est_label <- if ("prob" %in% names(ph_table_check)) "Estimated Probabilities (Cell Means)" else
          if ("rate" %in% names(ph_table_check)) "Estimated Marginal Rates (Cell Means)" else
            "Estimated Cell Means"
      } else {
        if ("rate" %in% names(ph_table_check)) {
          est_label <- "Estimated Marginal Rates"
        } else if ("prob" %in% names(ph_table_check)) {
          est_label <- "Estimated Probabilities"
        } else if ("response" %in% names(ph_table_check)) {
          est_label <- "Estimated Responses"
        } else {
          est_label <- "Estimated Marginal Means"
        }
      }

      # 3. Determine the Scale Description based on 'type' and 'family'
      #    Accessing 'type' and 'family' from the main f_glm function environment
      if (type == "link") {
        scale_desc <- "model-based values on the linear link scale (e.g., log or logit)"
      } else {
        # If type == "response"
        if (family$family == "gaussian") {
          # Gaussian identity link doesn't really 'back-transform', so we simplify text
          scale_desc <- "model-based values on the response scale"
        } else {
          # Poisson, Binomial, Gamma, etc. are truly back-transformed
          scale_desc <- "back-transformed, model-based values on the response scale"
        }
      }

      # 4. Prepare Conditional Notes (New Logic)

      # Note on Asymmetry: Only needed for back-transformed GLMs (non-Gaussian response scale)
      note_asymmetry <- ""
      if (type == "response" && family$family != "gaussian") {
        note_asymmetry <- paste0(
          "\n* **Asymmetric Intervals:** You may notice that the Confidence Intervals are not symmetrical around the ",
          est_label, ". This is normal! Because of the non-linear link function, the uncertainty is often 'stretched' in one direction."
        )
      }

      # Note on Scale Interpretation
      if (type == "link") {
        note_scale <- "* **Interpretation:** These values are on the transformed scale (e.g., log-odds). You may need to convert them manually to interpret them in real-world units."
      } else {
        note_scale <- "* **Back-Transformation:** The values (Means and CIs) have been converted from the model's internal scale back to the original units."
      }

      # 5. Print the Combined Dynamic Text
      cat(
        "The table below shows the ", est_label, " (`emmeans` package). These are ", scale_desc, " for ", response_name, ". Unlike raw averages, these values correct
      for unbalanced designs and reflect the statistical model.\n"
      )

      cat(
        "\n#### Publication & Reporting Tips\n")
      cat(
        "* For main results showing significant differences, prioritize reporting these", est_label, " and their *Confidence Intervals (CIs)* rather than raw means.\n",
        "* Figures should ideally overlay these ", est_label, " (and error bars) on top of the raw data points.\n"
      )
      cat(
        "\n#### Technical Notes (Explanations)\n",
        note_scale,
        note_asymmetry,
        "\n* **SE vs CI:** We recommend focusing on **Confidence Intervals** for inference rather than Standard Errors (SE).\n  \n"
      )

      cat("&nbsp;\n  \n")
      cat(paste("\n**Post Hoc Summary Table** of glm call: ", deparse(current_formula), "  \n"))
      f_pander(f_conditional_round(output_list[[response_name]][['posthoc']][['post_hoc_summary_table']], digits = 3))
      # Add footnote
      cat(output_list[[response_name]][['posthoc']][['cld_text']])
      # --- DYNAMIC TEXT LOGIC END ---

      # ---- Effect plot(s) --------------------------------------------------
      # Estimated marginal means on the response scale (emmeans was called
      # with type = `type`, so emm is already back-transformed for non-
      # gaussian families). For a single categorical predictor: one
      # means-plot with jittered raw data + CLD letters. For a significant
      # two-way interaction between two categorical factors: interaction
      # plots (both orientations), matching f_aov(). Otherwise one means
      # plot per categorical main effect.
      if (isTRUE(effect_plot)) {
        emm_obj_doc  <- output_list[[response_name]][["posthoc"]][["emm"]]
        ph_tab_doc   <- output_list[[response_name]][["posthoc"]][["post_hoc_summary_table"]]
        cat_preds_pl <- cat_preds_out
        sig_ints_pl  <- sig_ints_out
        mf_doc       <- tryCatch(glm_fit$model, error = function(e) NULL)

        # Resolve the estimate (centre) and CI column names. On the response
        # scale emmeans names the estimate rate/prob/response; on the link
        # scale (or gaussian) it is emmean (renamed emmean.. in the merged
        # table). CI columns are lower.CL/upper.CL or asymp.LCL/asymp.UCL.
        emm_d_doc <- tryCatch(as.data.frame(emm_obj_doc),
                              error = function(e) NULL)

        find_centre_col <- function(df) {
          intersect(c("response", "rate", "prob", "emmean"), names(df))[1]
        }
        find_ci_cols <- function(df) {
          lo <- intersect(c("lower.CL", "asymp.LCL", "LCL"), names(df))[1]
          hi <- intersect(c("upper.CL", "asymp.UCL", "UCL"), names(df))[1]
          c(lo, hi)
        }

        y_label_doc <- if (family$family == "gaussian" || type == "link")
          response_name else paste0(response_name, " (response scale)")

        # Overall model significance (LRT-based, robust under separation). When
        # the model is not significant the post hoc table already shows "ns", so
        # the plots draw no letters and the caption omits the letter sentence.
        sig_effects_pl <- isTRUE(output_list[[response_name]][["sig_effects"]])

        # Shared caption for every figure, split into two reusable pieces so the
        # letter explanation can be dropped on figures where the letters were
        # withheld (a banner is shown instead). This mirrors f_aov(); only
        # eff_note_est is family/link-aware here, while eff_note_let is kept
        # identical so the letter wording stays uniform across functions.
        #   eff_note_est -> points / estimates / CI (always shown). Unlike the
        #     linear-model phrasing, the GLM estimates live on the scale set by
        #     `type`: the response scale (back-transformed through the link for
        #     non-gaussian families) or the linear link scale.
        #   eff_note_let -> how to read the grouping letters (only when shown).
        est_scale_phrase <- if (type == "link") {
          paste0("on the ", family$link, "-link (linear predictor) scale")
        } else if (family$link == "identity") {
          "on the response scale"          # identity link: no transformation, any family
        } else {
          paste0("back-transformed through the ", family$link,
                 " link to the response scale")
        }
        eff_note_est <- paste0(
          if (isTRUE(sep_flag))
            paste0(
              "[!] Separation: (quasi-)complete separation (\"near-perfect\" ",
              "prediction) was detected, so the model estimates and especially ",
              "their confidence intervals are driven to extreme values and are ",
              "not reliable; treat the plotted intervals with caution. ")
          else "",
          "Points are (jittered) raw data on the response scale; estimates ",
          "are model estimated marginal means ", est_scale_phrase, " with ",
          100 * (1 - alpha), "% CI.")
        eff_note_let <- paste0(
          " Groups sharing a letter are not significantly different ",
          "(\u03b1 = ", alpha, "); groups with different letters are ",
          "significantly different. Sharing a letter indicates insufficient ",
          "evidence of a difference, not proof that the groups are identical.")

        # Assemble the single-piece figure caption in fixed reading order:
        #   1. data / estimates / CI            (eff_note_est, always)
        #   2. ALL letter information together   (eff_note_let + letter_extra),
        #      OR the "Letters omitted" notice   (banner), OR nothing when the
        #      figure shows no letters at all. letter_extra carries the
        #      cross-panel reading note, which is itself about letters.
        #   3. the interaction / line note "rest" (int_note: not-parallel,
        #      panels, dotted line), interaction plots only.
        # Exactly one short lead label is bold: "Letters omitted:" when letters
        # were withheld, otherwise the interaction lead. The whole caption is one
        # italic piece. `banner` is NULL when letters are shown; `int_note` is
        # NULL for single-factor means plots; has_letters is FALSE when the
        # figure carries no letters and no omission notice.
        make_fig_caption <- function(banner = NULL, int_note = NULL,
                                     letter_extra = NULL, has_letters = TRUE) {
          est <- trimws(eff_note_est)
          if (!is.null(banner)) {
            let <- sub("^Letters omitted:", "**Letters omitted:**",
                       trimws(banner))
          } else if (isTRUE(has_letters)) {
            let <- trimws(eff_note_let)
            if (!is.null(letter_extra) && nzchar(trimws(letter_extra)))
              let <- paste(let, trimws(letter_extra))
          } else {
            let <- ""  # no letters shown -> no letter explanation
          }
          intp <- ""
          if (!is.null(int_note) && nzchar(trimws(int_note))) {
            intp <- trimws(int_note)
            # Bold the interaction lead only when it is the salient label, i.e.
            # when no "Letters omitted:" notice already carries the bold.
            if (is.null(banner))
              intp <- sub(
                "^(Significant interaction:|No significant interaction:)",
                "**\\1**", intp)
          }
          parts <- c(est, let, intp)
          paste(parts[nzchar(parts)], collapse = " ")
        }

        # ---- Per-plot compact letters (suppress across interaction) ---------
        # The compact letters on a figure must answer the same question the
        # figure asks. An effect / interaction plot shows the means of the
        # factors in `display_factors` and AVERAGES over every other
        # categorical predictor. Letters are honest only when BOTH:
        #   (1) they are computed on exactly that displayed grid, so each letter
        #       and its plotted point refer to the same marginal estimate (this
        #       also removes the first-match-wins ambiguity of copying letters
        #       from the full crossed cell-means table by a partial key), AND
        #   (2) the averaging does not cross a significant interaction, i.e. no
        #       significant interaction term has one factor inside
        #       `display_factors` and another outside it.
        # When (2) fails the means are still drawn but the letters are withheld
        # and a visible banner explains why and points to the cell-means post
        # hoc table (disclosed degraded mode, preferred over misleading
        # letters). Returns list(safe, letters_df, banner): `letters_df` has the
        # display-factor column(s) plus a `Letters` column when letters are
        # shown, otherwise NULL; `banner` is the explanatory note (or NULL).
        glm_plot_letters <- function(display_factors) {
          D <- display_factors

          # Separation: Wald-based grouping is unreliable. Withhold letters and
          # point to the LRT-based pairwise comparisons in the post hoc table.
          if (isTRUE(sep_flag))
            return(list(
              safe = TRUE, letters_df = NULL,
              banner = paste0(
                "Letters omitted: (quasi-)complete separation makes the ",
                "Wald-based grouping unreliable here; refer to the LRT-based ",
                "pairwise comparisons in the cell-means post hoc table.")))

          # (2) Does any significant interaction straddle D / not-D?
          crossing <- character(0)
          for (it in sig_ints_pl) {
            parts <- trimws(strsplit(it, ":", fixed = TRUE)[[1]])
            in_D  <- intersect(parts, D)
            out_D <- setdiff(parts, D)
            if (length(in_D) > 0L && length(out_D) > 0L)
              crossing <- union(crossing, out_D)
          }
          if (length(crossing) > 0L) {
            banner <- paste0(
              "Letters omitted: this view averages over ",
              paste(crossing, collapse = ", "),
              ", which interact(s) significantly with the factor(s) shown, ",
              "so collapsed grouping letters could mislead. See the ",
              "cell-means post hoc table for grouped significance.")
            return(list(safe = FALSE, letters_df = NULL, banner = banner))
          }

          # Overall model not significant: the table already shows "ns" and no
          # pairwise letters should be claimed. Draw points without letters.
          if (!sig_effects_pl)
            return(list(safe = TRUE, letters_df = NULL, banner = NULL))

          # (1) Recompute emmeans on EXACTLY the displayed grid (same `type` as
          # the main post hoc table), then derive the letters from that grid so
          # points and letters match one-to-one.
          letters_df <- tryCatch({
            emm_D <- emmeans::emmeans(glm_fit, specs = D, type = type,
                                      level = 1 - alpha)
            cld_D <- cld_emmeans(emm_D, alpha = alpha, Letters = letters,
                                 adjust = adjust, decreasing = TRUE)
            if (!all(c(D, ".group") %in% names(cld_D)))
              stop("recomputed CLD is missing expected columns")
            ld <- cld_D[, c(D, ".group"), drop = FALSE]
            names(ld)[names(ld) == ".group"] <- "Letters"
            for (f in D) ld[[f]] <- as.character(ld[[f]])
            ld$Letters <- trimws(ld$Letters)
            ld
          }, error = function(e) {
            warning("f_glm: could not compute per-plot letters for '",
                    paste(D, collapse = ":"), "': ", conditionMessage(e),
                    call. = FALSE, immediate. = TRUE)
            NULL
          })
          if (is.null(letters_df))
            return(list(
              safe = TRUE, letters_df = NULL,
              banner = paste0(
                "Letters unavailable: the grouping could not be computed for ",
                "this view; see the cell-means post hoc table.")))
          list(safe = TRUE, letters_df = letters_df, banner = NULL)
        }

        # Single-term means plot. The shared emmeans grid (emm_d_doc) spans
        # ALL categorical predictors, so it can contain several rows per
        # level of `var_name` (one per level of every other factor). Plotting
        # that grid directly stacks multiple points on the same x position
        # (the overlap problem). To show a clean one-estimate-per-level
        # marginal-means plot, recompute emmeans for `var_name` alone; fall
        # back to the shared grid only if that fails.
        make_glm_means_plot <- function(var_name) {
          emm_one <- tryCatch(
            as.data.frame(emmeans::emmeans(
              glm_fit, specs = var_name, type = type, level = 1 - alpha)),
            error = function(e) NULL
          )
          src_df <- if (!is.null(emm_one) && var_name %in% names(emm_one))
            emm_one else emm_d_doc
          if (is.null(src_df) || !var_name %in% names(src_df))
            return(NULL)
          centre_col <- find_centre_col(src_df)
          ci_cols    <- find_ci_cols(src_df)
          if (is.na(centre_col) || any(is.na(ci_cols))) return(NULL)

          plot_df <- data.frame(
            x_grp  = as.character(src_df[[var_name]]),
            centre = src_df[[centre_col]],
            lower  = src_df[[ci_cols[1]]],
            upper  = src_df[[ci_cols[2]]],
            stringsAsFactors = FALSE
          )
          # Safety net: if duplicates per level remain (shared-grid
          # fallback), average them so each level shows a single estimate.
          if (anyDuplicated(plot_df$x_grp)) {
            plot_df <- do.call(rbind, lapply(
              split(plot_df, plot_df$x_grp), function(d) data.frame(
                x_grp  = d$x_grp[1],
                centre = mean(d$centre, na.rm = TRUE),
                lower  = mean(d$lower,  na.rm = TRUE),
                upper  = mean(d$upper,  na.rm = TRUE),
                stringsAsFactors = FALSE)))
          }

          # CLD letters computed on EXACTLY this single-factor grid (see
          # glm_plot_letters): they match the plotted marginal means one-to-one
          # and are withheld (with a banner) when averaging over another factor
          # would cross a significant interaction, under separation, or when the
          # overall model is not significant.
          letter_banner <- NULL
          plot_df$Letters <- NA_character_
          lres <- glm_plot_letters(var_name)
          letter_banner <- lres$banner
          if (!is.null(lres$letters_df) && var_name %in% names(lres$letters_df)) {
            lut <- stats::setNames(lres$letters_df$Letters,
                                   lres$letters_df[[var_name]])
            plot_df$Letters <- lut[as.character(plot_df$x_grp)]
          }
          # Non-letter markers ("ns", em dash for uncomputable LRT pairs, or
          # empty strings) carry no grouping information, so blank them out
          # rather than drawing a stray dash above a bar.
          plot_df$Letters <- trimws(plot_df$Letters)
          plot_df$Letters[plot_df$Letters %in%
                            c("ns", "\u2014", "-", "NA")] <- NA_character_

          # Raw data overlay (original response scale) from the model frame.
          raw_df <- NULL
          if (!is.null(mf_doc) && var_name %in% names(mf_doc) &&
              response_name %in% names(mf_doc)) {
            raw_df <- data.frame(
              x_grp = as.character(mf_doc[[var_name]]),
              y_val = mf_doc[[response_name]],
              stringsAsFactors = FALSE
            )
            raw_df <- raw_df[stats::complete.cases(raw_df), ]
            if (!is.numeric(raw_df$y_val)) raw_df <- NULL
          }

          if (!is.null(mf_doc) && var_name %in% names(mf_doc) &&
              is.factor(mf_doc[[var_name]])) {
            lev <- levels(mf_doc[[var_name]])
            plot_df$x_grp <- factor(plot_df$x_grp, levels = lev)
            if (!is.null(raw_df)) raw_df$x_grp <- factor(raw_df$x_grp,
                                                         levels = lev)
          }

          y_vals <- c(plot_df$upper, plot_df$lower,
                      if (!is.null(raw_df)) raw_df$y_val)
          y_top  <- max(y_vals, na.rm = TRUE)
          y_bot  <- min(y_vals, na.rm = TRUE)
          plot_df$y_letter <- y_top + 0.06 * (y_top - y_bot)

          # One distinct colour per level so groups are easy to tell apart.
          n_lvl <- length(unique(plot_df$x_grp))
          lvl_cols <- f_pub_palette(n_lvl)

          p <- ggplot2::ggplot(
            plot_df,
            ggplot2::aes(x = .data[["x_grp"]], y = .data[["centre"]],
                         colour = .data[["x_grp"]])
          )
          if (!is.null(raw_df) && nrow(raw_df) > 0L) {
            p <- p + ggplot2::geom_jitter(
              data = raw_df,
              ggplot2::aes(x = .data[["x_grp"]], y = .data[["y_val"]]),
              inherit.aes = FALSE,
              width = 0.15, height = 0,
              shape = 1, color = "grey30", alpha = 0.5
            )
          }
          p <- p +
            ggplot2::geom_errorbar(
              ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
              width = 0.12, linewidth = 0.8
            ) +
            ggplot2::geom_point(size = 3) +
            ggplot2::geom_text(
              ggplot2::aes(y = .data[["y_letter"]],
                           label = .data[["Letters"]]),
              vjust = 0, show.legend = FALSE, colour = "black",
              na.rm = TRUE
            ) +
            ggplot2::scale_colour_manual(values = lvl_cols, guide = "none") +
            ggplot2::labs(
              title = paste0("Estimated means of ", response_name,
                             " by ", var_name),
              x = var_name, y = y_label_doc) +
            f_theme_pub(base_size = 14)
          attr(p, "letter_banner") <- letter_banner
          attr(p, "has_letters")   <- any(!is.na(plot_df$Letters))
          p
        }

        # Interaction plot (two categorical predictors) on the response
        # scale via emmip(..., type = type). `factors` is a character vector:
        # the 1st element is the x-axis, the 2nd is the trace (colour), and any
        # 3rd/4th elements become facet panels. Handles 2- to 4-way categorical
        # interactions.
        make_glm_interaction_plot <- function(factors, is_sig) {
          x_var     <- factors[1]
          trace_var <- factors[2]
          facet_vars <- if (length(factors) > 2L) factors[-(1:2)] else character(0)

          # Build the emmip formula: trace ~ x, with "| f1 * f2" appended when
          # there are facet factors.
          rhs <- x_var
          if (length(facet_vars) > 0L)
            rhs <- paste0(rhs, " | ", paste(facet_vars, collapse = " * "))
          emmip_form <- stats::as.formula(paste(trace_var, "~", rhs))

          # Call emmip on the already-built emmeans object (emm_obj_doc),
          # mirroring f_aov(). That object was created with type = `type`, so
          # its estimates are already on the requested (e.g. response) scale;
          # passing the raw glm fit together with type = "response" and
          # CIs = TRUE to emmip() instead can fail when emmip builds its own
          # reference grid, which is why the interaction plot silently
          # produced no figure.
          ip <- tryCatch(
            emmeans::emmip(emm_obj_doc, emmip_form, level = 1 - alpha,
                           CIs = TRUE, plotit = FALSE),
            error = function(e) {
              warning("f_glm: emmip() failed for formula '",
                      deparse(emmip_form), "': ",
                      conditionMessage(e), call. = FALSE, immediate. = TRUE)
              NULL
            }
          )
          if (is.null(ip)) return(NULL)

          ip_df <- data.frame(
            x_grp = as.character(ip[[x_var]]),
            trace = as.character(ip[[trace_var]]),
            yvar  = ip[["yvar"]],
            lower = ip[["LCL"]],
            upper = ip[["UCL"]],
            stringsAsFactors = FALSE
          )
          # Keep the x and trace factors under their REAL names, so the
          # CLD-letter key join below (which keys on the factor names from the
          # post hoc table) can find them. Without this the key columns would
          # only exist as x_grp/trace and the join would silently produce no
          # letters.
          ip_df[[x_var]]     <- as.character(ip[[x_var]])
          ip_df[[trace_var]] <- as.character(ip[[trace_var]])
          # Carry facet columns through (as factors, preserving model order).
          for (fv in facet_vars) {
            fac_levels <- if (!is.null(mf_doc) && fv %in% names(mf_doc) &&
                              is.factor(mf_doc[[fv]]))
              levels(mf_doc[[fv]]) else unique(as.character(ip[[fv]]))
            ip_df[[fv]] <- factor(as.character(ip[[fv]]), levels = fac_levels)
          }

          x_levels <- if (!is.null(mf_doc) && x_var %in% names(mf_doc) &&
                          is.factor(mf_doc[[x_var]]))
            levels(mf_doc[[x_var]]) else unique(ip_df$x_grp)
          tr_levels <- if (!is.null(mf_doc) && trace_var %in% names(mf_doc) &&
                           is.factor(mf_doc[[trace_var]]))
            levels(mf_doc[[trace_var]]) else unique(ip_df$trace)
          ip_df$x_grp <- factor(ip_df$x_grp, levels = x_levels)
          ip_df$trace <- factor(ip_df$trace, levels = tr_levels)

          # CLD letters computed on EXACTLY this displayed grid (see
          # glm_plot_letters): they match the plotted cells one-to-one and are
          # withheld (with a banner) when the view averages over a factor that
          # interacts significantly with what is shown, under separation, or
          # when the overall model is not significant.
          all_factors <- c(x_var, trace_var, facet_vars)
          ip_df$Letters <- NA_character_
          int_letter_banner <- NULL
          lres <- glm_plot_letters(all_factors)
          int_letter_banner <- lres$banner
          if (!is.null(lres$letters_df) &&
              all(all_factors %in% names(lres$letters_df))) {
            key_tab <- do.call(paste, c(
              lapply(all_factors, function(f)
                as.character(lres$letters_df[[f]])), list(sep = "\r")))
            lut <- stats::setNames(lres$letters_df$Letters, key_tab)
            key_ip <- do.call(paste, c(
              lapply(all_factors, function(f)
                as.character(ip_df[[f]])), list(sep = "\r")))
            ip_df$Letters <- lut[key_ip]
          }
          ip_df$Letters <- trimws(ip_df$Letters)
          ip_df$Letters[ip_df$Letters %in%
                          c("ns", "\u2014", "-", "NA")] <- NA_character_

          raw_df <- NULL
          if (!is.null(mf_doc) && response_name %in% names(mf_doc) &&
              all(all_factors %in% names(mf_doc)) &&
              is.numeric(mf_doc[[response_name]])) {
            raw_df <- data.frame(
              x_grp = factor(as.character(mf_doc[[x_var]]), levels = x_levels),
              trace = factor(as.character(mf_doc[[trace_var]]),
                             levels = tr_levels),
              y_val = mf_doc[[response_name]],
              stringsAsFactors = FALSE
            )
            for (fv in facet_vars)
              raw_df[[fv]] <- factor(as.character(mf_doc[[fv]]),
                                     levels = levels(ip_df[[fv]]))
            raw_df <- raw_df[stats::complete.cases(raw_df), ]
          }

          int_caption <- if (isTRUE(is_sig))
            paste0("Significant interaction: the ", trace_var,
                   " lines are not parallel.")
          else
            paste0("No significant interaction: the ", trace_var,
                   " lines should be roughly parallel.")
          if (length(facet_vars) > 0L)
            int_caption <- paste0(
              int_caption, " Panels split by ",
              paste(facet_vars, collapse = " and "), ".")
          # The cross-panel reading note is about letters, so it is NOT placed
          # in the interaction (line) note; it is returned separately and the
          # caption assembler groups it with the other letter text. Only set it
          # for faceted plots whose letters are actually shown.
          cross_facet_note <- if (length(facet_vars) > 0L &&
                                  is.null(int_letter_banner))
            paste0("Letters can be compared across all panels: they group ",
                   "every cell shown, not only the cells within one panel.")
          else NULL
          int_caption <- paste0(
            int_caption,
            " The dotted line is merely a visual aid and has no real-life ",
            "meaning, as there is no data space between nominal categories.")

          # In-figure title: distinguishes the orientation (which factor is on
          # the x-axis, which is the colour, which are facets). Plain ASCII
          # "by" is used rather than a multiplication-sign glyph, which some
          # graphics devices / fonts fail to render when ggsave writes the PNG.
          int_title <- paste0(
            "Estimated means of ", response_name, ": ",
            x_var, " (x-axis) by ", trace_var, " (colour)")
          if (length(facet_vars) > 0L)
            int_title <- paste0(int_title, ", faceted by ",
                                paste(facet_vars, collapse = " and "))

          # Letter labels sit a little above each cell's upper CI.
          y_all_int <- c(ip_df$upper, ip_df$lower,
                         if (!is.null(raw_df)) raw_df$y_val)
          y_rng_int <- max(y_all_int, na.rm = TRUE) -
            min(y_all_int, na.rm = TRUE)
          ip_df$y_letter <- ip_df$upper + 0.05 * y_rng_int

          dodge <- ggplot2::position_dodge(width = 0.3)

          p <- ggplot2::ggplot(
            ip_df,
            ggplot2::aes(x = .data[["x_grp"]], y = .data[["yvar"]],
                         group = .data[["trace"]], colour = .data[["trace"]])
          )
          if (!is.null(raw_df) && nrow(raw_df) > 0L) {
            p <- p + ggplot2::geom_jitter(
              data = raw_df,
              ggplot2::aes(x = .data[["x_grp"]], y = .data[["y_val"]],
                           colour = .data[["trace"]]),
              inherit.aes = FALSE,
              position = ggplot2::position_jitterdodge(
                jitter.width = 0.12, dodge.width = 0.3),
              shape = 1, alpha = 0.35, show.legend = FALSE
            )
          }
          p <- p +
            ggplot2::geom_line(position = dodge, linewidth = 0.7,
                               linetype = "dotted") +
            ggplot2::geom_errorbar(
              ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
              width = 0.12, linewidth = 0.7, position = dodge
            ) +
            ggplot2::geom_point(position = dodge, size = 3) +
            ggplot2::geom_text(
              ggplot2::aes(y = .data[["y_letter"]],
                           label = .data[["Letters"]]),
              position = dodge, vjust = 0, colour = "black",
              show.legend = FALSE, na.rm = TRUE
            ) +
            ggplot2::labs(
              title   = int_title,
              x       = x_var,
              y       = y_label_doc,
              colour  = trace_var
            ) +
            f_theme_pub(base_size = 14)

          # Facets: one facet factor -> facet_wrap; two -> facet_grid
          # (rows = first facet factor, cols = second).
          if (length(facet_vars) == 1L) {
            p <- p + ggplot2::facet_wrap(
              ggplot2::vars(.data[[facet_vars[1]]]),
              labeller = ggplot2::label_both)
          } else if (length(facet_vars) >= 2L) {
            p <- p + ggplot2::facet_grid(
              stats::as.formula(paste(facet_vars[1], "~", facet_vars[2])),
              labeller = ggplot2::label_both)
          }

          p <- p + ggplot2::scale_colour_manual(
            values = f_pub_palette(length(tr_levels)))
          # Carry the caption pieces so the caller can assemble one italic
          # caption below the figure (see make_fig_caption).
          attr(p, "int_caption")      <- int_caption
          attr(p, "letter_banner")    <- int_letter_banner
          attr(p, "cross_facet_note") <- cross_facet_note
          attr(p, "has_letters")      <- any(!is.na(ip_df$Letters))
          p
        }

        # Significant categorical interaction(s)? Support 2-, 3-, and 4-way
        # interactions among categorical factors. A 2-way uses x-axis + colour
        # (trace); 3- and 4-way add facet panels for the remaining factor(s).
        # Beyond 4 factors the panel grid becomes unreadable, so those are
        # skipped with a warning.
        MAX_PLOT_FACTORS <- 4L
        int_cat <- character(0)
        if (length(sig_ints_pl) > 0) {
          for (it in sig_ints_pl) {
            parts <- strsplit(it, ":", fixed = TRUE)[[1]]
            n_f <- length(parts)
            if (n_f >= 2L && all(parts %in% cat_preds_pl)) {
              if (n_f <= MAX_PLOT_FACTORS) {
                int_cat <- c(int_cat, it)
              } else {
                warning("f_glm: the ", n_f, "-way interaction '", it,
                        "' involves more than ", MAX_PLOT_FACTORS,
                        " categorical factors. An interaction plot would ",
                        "need ", n_f - 2L, " nested facet dimensions, which ",
                        "is not legible, so no plot is drawn for this term. ",
                        "The post hoc cell-means table still reports every ",
                        "combination; inspect it (or plot a chosen lower-",
                        "order slice) instead.",
                        call. = FALSE, immediate. = TRUE)
              }
            }
          }
        }

        if (length(int_cat) > 0) {
          for (it in int_cat) {
            parts <- strsplit(it, ":", fixed = TRUE)[[1]]
            cat(paste0("\n## Interaction Plots of: ", response_name,
                       "  (", it, ")  \n"))
            # Orientations: for a 2-way show both (x<->trace swap). For higher
            # order, rotate which factor sits on the x-axis (the next factor
            # becomes the trace/colour, the remainder become facet panels).
            # This gives one informative plot per factor without the
            # combinatorial explosion of all permutations.
            orientations <- if (length(parts) == 2L)
              list(parts[c(1, 2)], parts[c(2, 1)])
            else
              lapply(seq_along(parts), function(i)
                c(parts[i], parts[-i]))
            for (pi in seq_along(orientations)) {
              ord <- orientations[[pi]]
              p_ip <- tryCatch(
                make_glm_interaction_plot(ord, is_sig = TRUE),
                error = function(e) {
                  warning("f_glm: interaction plot for '", it,
                          "' (orientation ", pi, ") could not be built: ",
                          conditionMessage(e), call. = FALSE,
                          immediate. = TRUE)
                  NULL
                }
              )
              if (!is.null(p_ip)) {
                # Store the ggplot OBJECT for later manual adjustment.
                key <- paste0("interaction_plot_",
                              gsub(":", "_", it), "_", pi)
                output_list[[response_name]][[key]] <- p_ip
                # Render the stored object to PNG for the report. Widen the
                # canvas when facet panels are present.
                n_facet <- max(0L, length(ord) - 2L)
                w_in <- if (n_facet >= 2L) 9 else if (n_facet == 1L) 8 else 7
                h_in <- if (n_facet >= 2L) 6.5 else 5.5
                tmp_ip <- tempfile(fileext = ".png")
                ok_ip <- tryCatch({
                  suppressMessages(
                    ggplot2::ggsave(filename = tmp_ip, plot = p_ip,
                                    width = w_in, height = h_in, units = "in",
                                    dpi = 200)
                  )
                  file.exists(tmp_ip)
                }, error = function(e) {
                  warning("f_glm: ggsave failed for interaction plot '", it,
                          "' (orientation ", pi, "): ", conditionMessage(e),
                          call. = FALSE, immediate. = TRUE)
                  FALSE
                })
                if (isTRUE(ok_ip))
                  cat(paste0("![](", tmp_ip, ")"), "   \n  \n")
                # Single italic caption, ordered: estimates/CI, then the letter
                # note (or the "Letters omitted" notice plus the cross-panel
                # note), then the interaction note. Only one short lead label is
                # bold (see make_fig_caption).
                ip_cap  <- attr(p_ip, "int_caption")
                ip_bann <- attr(p_ip, "letter_banner")
                cap <- make_fig_caption(
                  banner       = ip_bann,
                  int_note     = ip_cap,
                  letter_extra = attr(p_ip, "cross_facet_note"),
                  has_letters  = isTRUE(attr(p_ip, "has_letters")))
                cat(paste0("*", gsub("\n", "  \n", cap), "*",
                           "   \n  \n"))
              }
            }
          }
        } else if (length(cat_preds_pl) > 0) {
          for (vn in cat_preds_pl) {
            p_eff <- tryCatch(make_glm_means_plot(vn),
                              error = function(e) {
                                warning("f_glm: means plot for '", vn,
                                        "' could not be built: ",
                                        conditionMessage(e), call. = FALSE,
                                        immediate. = TRUE)
                                NULL
                              })
            if (!is.null(p_eff)) {
              output_list[[response_name]][[paste0("effect_plot_", vn)]] <- p_eff
              tmp_eff <- tempfile(fileext = ".png")
              ok_eff <- tryCatch({
                suppressMessages(
                  ggplot2::ggsave(filename = tmp_eff, plot = p_eff,
                                  width = 6, height = 5, units = "in",
                                  dpi = 200)
                )
                file.exists(tmp_eff)
              }, error = function(e) {
                warning("f_glm: ggsave failed for means plot '", vn, "': ",
                        conditionMessage(e), call. = FALSE,
                        immediate. = TRUE)
                FALSE
              })
              cat(paste0("\n## Estimated Means Plot of: ", response_name,
                         "  (", vn, ")  \n"))
              if (isTRUE(ok_eff))
                cat(paste0("![](", tmp_eff, ")"), "   \n  \n")
              # Single italic caption: estimates/CI, then the letter note (or
              # the "Letters omitted" notice). Means plots carry no interaction
              # sentence, so int_note is omitted.
              eff_bann <- attr(p_eff, "letter_banner")
              cap <- make_fig_caption(
                banner      = eff_bann,
                int_note    = NULL,
                has_letters = isTRUE(attr(p_eff, "has_letters")))
              cat(paste0("*", gsub("\n", "  \n", cap), "*", "   \n  \n"))
            }
          }
        }
      }

      # ---- Pairwise contrast forest plots (opt-in) -----------------------
      # Independent of effect_plot: one row per pairwise difference with its
      # adjusted CI and a zero reference line, drawn with the shared forest
      # helper (helper_contrast_forest.R) so the look matches f_aov() and
      # f_lmer(). Contrasts are computed on the LINK scale (type = "link"),
      # where differences between levels are additive and symmetric; a CI that
      # excludes zero is a significant pairwise difference. Skipped under
      # (quasi-)complete separation, where Wald CIs are unreliable. Main-effect
      # contrasts are stored as contrast_plot_<term>; interaction cell
      # contrasts as interaction_contrast_plot_<term>.
      if (isTRUE(contrast_plots) && !isTRUE(sep_flag)) {
        cat_preds_cf <- cat_preds_out
        sig_ints_cf  <- sig_ints_out

        int_cat_cf <- character(0)
        if (length(sig_ints_cf) > 0) {
          for (it in sig_ints_cf) {
            parts <- strsplit(it, ":", fixed = TRUE)[[1]]
            if (length(parts) >= 2L && all(parts %in% cat_preds_cf))
              int_cat_cf <- c(int_cat_cf, it)
          }
        }

        if (length(int_cat_cf) > 0) {
          for (it in int_cat_cf) {
            parts <- strsplit(it, ":", fixed = TRUE)[[1]]
            cf_tbl <- tryCatch(
              as.data.frame(confint(
                pairs(emmeans::emmeans(glm_fit, specs = parts,
                                       type = "link", level = 1 - alpha),
                      adjust = adjust),
                level = 1 - alpha)),
              error = function(e) NULL)
            p_icf <- if (!is.null(cf_tbl))
              make_contrast_forest(
                cf_tbl,
                paste0("Cell-pairwise contrasts of ", response_name,
                       " (link scale)  (", it, ")"),
                alpha = alpha, adjust = adjust) else NULL
            if (is.null(p_icf)) {
              cat("\n*Contrast forest plot for `", it, "` could not be ",
                  "built (adjusted contrast CIs unavailable); see the ",
                  "post hoc table above.*  \n\n", sep = "")
            } else {
              key <- paste0("interaction_contrast_plot_",
                            gsub(":", "_", it))
              output_list[[response_name]][[key]] <- p_icf
              tmp_icf <- tempfile(fileext = ".png")
              n_rows  <- nrow(p_icf$data)
              ok_icf <- tryCatch({
                suppressMessages(
                  ggplot2::ggsave(filename = tmp_icf, plot = p_icf,
                                  width = 7.5,
                                  height = max(3, 0.4 * n_rows + 1.5),
                                  units = "in", dpi = 200))
                file.exists(tmp_icf)
              }, error = function(e) FALSE)
              cat(paste0("\n## Contrast Forest Plot of: ", response_name,
                         "  (", it, ")  \n"))
              if (isTRUE(ok_icf))
                cat(paste0("![](", tmp_icf, ")"), "   \n  \n")
              icf_cap <- attr(p_icf, "int_caption")
              if (!is.null(icf_cap))
                cat(paste0("*", gsub("\n", "  \n", icf_cap), "*",
                           "   \n  \n"))
            }
          }
        } else if (length(cat_preds_cf) > 0) {
          for (vn in cat_preds_cf) {
            cf_tbl <- tryCatch(
              as.data.frame(confint(
                pairs(emmeans::emmeans(glm_fit, specs = vn,
                                       type = "link", level = 1 - alpha),
                      adjust = adjust),
                level = 1 - alpha)),
              error = function(e) NULL)
            p_cf <- if (!is.null(cf_tbl))
              make_contrast_forest(
                cf_tbl,
                paste0("Pairwise contrasts of ", response_name,
                       " by ", vn, " (link scale)"),
                alpha = alpha, adjust = adjust) else NULL
            if (is.null(p_cf)) {
              cat("\n*Contrast forest plot for `", vn, "` could not be ",
                  "built (adjusted contrast CIs unavailable); see the ",
                  "post hoc table above.*  \n\n", sep = "")
            } else {
              key <- paste0("contrast_plot_", vn)
              output_list[[response_name]][[key]] <- p_cf
              tmp_cf <- tempfile(fileext = ".png")
              n_rows <- nrow(p_cf$data)
              ok_cf <- tryCatch({
                suppressMessages(
                  ggplot2::ggsave(filename = tmp_cf, plot = p_cf,
                                  width = 7,
                                  height = max(3, 0.4 * n_rows + 1.5),
                                  units = "in", dpi = 200))
                file.exists(tmp_cf)
              }, error = function(e) FALSE)
              cat(paste0("\n## Contrast Forest Plot of: ", response_name,
                         "  (", vn, ")  \n"))
              if (isTRUE(ok_cf))
                cat(paste0("![](", tmp_cf, ")"), "   \n  \n")
              cf_cap <- attr(p_cf, "int_caption")
              if (!is.null(cf_cap))
                cat(paste0("*", gsub("\n", "  \n", cf_cap), "*",
                           "   \n  \n"))
            }
          }
        }
      }
    }

    if (output == TRUE) {
      return(output_list)
    }
  }

  # Execute analysis ONCE: capture both the output_list and the markdown text.
  # Caching avoids running every GLM/DHARMa simulation a second time
  # (DHARMa uses random simulation, so two runs can yield different p-values).
  cached_markdown <- NULL
  suppressMessages(
    cached_markdown <- utils::capture.output(
      output_list <- generate_report()
    )
  )
  class(output_list) <- "f_glm"

  # Helper so word/pdf/rmd paths all use the same already-generated markdown.
  get_cached_markdown <- function() cached_markdown

  # Here the documents are constructed.
  if(output_type %in% c("word", "pdf")) {

    # Show save location before knitting else it will not display in console.
    message(paste0("Saving output in: ", output_path))

    # Prevent ## before printed output
    knitr::opts_chunk$set(comment = "")

    # Use the already-generated markdown (no second run of generate_report)
    generated_markdown <- get_cached_markdown()

    # Combine the preamble, assumptions, and the captured report into one string
    rmd_content <- paste(
      word_pdf_preamble(),
      if (intro_text) glm_assumptions_text() else "",
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

    # Open files after creation
    if(open_generated_files == TRUE){
      # Open the file with default program
      f_open_file(output_path)
    }

    return(invisible(output_list))

  } else if(output_type == "excel") {

    # show the location were the file is saved
    message(paste0("Saving output in: ", output_path))

    # Extract all post_hoc_summary_table tables and keep their names
    post_hoc_tables <- lapply(output_list, function(obj)
      obj$posthoc$post_hoc_summary_table)

    # Assign names to the list for Excel sheet names based on response names
    names(post_hoc_tables) <- response_names

    # Write to an Excel file with each table in its own sheet
    writexl::write_xlsx(post_hoc_tables, path = output_path)

    # Open files after creation
    if(open_generated_files == TRUE){
      f_open_file(output_path)
    }

    return(invisible(output_list))
  }

  if(output_type == "rmd"){

    if (is.null(knitr::opts_knit$get("output.dir"))) {
      knitr::opts_knit$set(output.dir = tempdir())
    }

    # Use the already-generated markdown (no second run of generate_report)
    clean_rmd_output <- paste(
      if (intro_text) glm_assumptions_text() else "",
      paste(get_cached_markdown(), collapse = "\n"),
      sep = "\n"
    )
    output_list[["rmd"]] <- clean_rmd_output

    return(invisible(output_list))

  } else if(output_type %in% c("console")){
    #Print output list to the console (forced)
    print(output_list)

    return(output_list)

  } else if(output_type %in% c("default")){
    return(output_list)

  } else {
    warning("Invalid output format specified. No file generated.")

  }


  invisible(suppressWarnings(file.remove(temp_output_file)))

}

# Print method for f_glm objects
#' @export
print.f_glm <- function(x, ...) {

  # Loop over each response variable
  for (category in names(x)) {

    # Skip non-response entries (e.g. "rmd" character string)
    sublist <- x[[category]]
    if (!is.list(sublist)) next

    cat("==========================================\n")
    cat("   GLM of response variable:", category, "\n")
    cat("==========================================\n")

    # Model info header
    glm_sum <- sublist$summary
    cat("Family:", glm_sum$family$family,
        " | Link:", glm_sum$family$link,
        " | AIC:", round(sublist$model$aic, 3), "\n")
    cat("Null deviance:", round(sublist$lrt_null_dev, 4),
        "on", sublist$lrt_null_df, "df",
        " | Residual deviance:", round(sublist$lrt_resid_dev, 4),
        "on", sublist$lrt_resid_df, "df\n\n")

    # Convergence warning
    if (!isTRUE(sublist$model$converged)) {
      warning("Model did not converge, estimates unreliable.", call. = FALSE, immediate. = TRUE)
    }

    # Coefficients table
    cat("Coefficients:\n")
    print(glm_sum$coefficients)

    cat("\n--- Type II Analysis of Deviance ---\n")
    if (!is.null(sublist$drop1)) {
      print(sublist$drop1)
    } else {
      cat("(not available)\n")
    }

    cat("\n--- Model vs. Null Model ---\n")
    cat("Null deviance:     ", round(sublist$lrt_null_dev, 4),
        " on", sublist$lrt_null_df, "df\n")
    cat("Residual deviance: ", round(sublist$lrt_resid_dev, 4),
        " on", sublist$lrt_resid_df, "df\n")
    cat("McFadden's Pseudo-R\u00b2:", sublist$lrt_pct_explained, "\n")

    cat("\n--- Post hoc Comparisons of:", category, "---\n")
    print(sublist$posthoc$post_hoc_summary_table)
    cat("___________________________\n")
    cat(sublist$posthoc$cld_text)
    cat("\n   \n")
  }

}

#' @export
plot.f_glm <- function(x, which = 1:2, ...) {

  # DHARMa's diagnostic plots subdivide the device (par(mfrow)) and assume a
  # single-panel starting layout with default margins. If this method is called
  # while the device already carries a multi-panel layout left over from an
  # earlier plot (common in test/check sessions and Rmd chunks), the per-panel
  # margins exceed the panel size and base graphics aborts with "figure margins
  # too large". Reset to a clean single-panel layout for the duration of this
  # call and restore the caller's settings on exit.
  #
  # Only the parameters we actually change are saved/restored. Capturing
  # par(no.readonly = TRUE) and restoring the whole list is unsafe: it carries
  # device-derived values such as "pin"/"cra" that error on restore, and "new",
  # whose restoration triggers par(new = TRUE) with no plot.
  old_par <- graphics::par(c("mfrow", "mar", "oma"))
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(1, 1),
                mar   = c(5.1, 4.1, 4.1, 2.1),
                oma   = c(0, 0, 0, 0))

  for (category in names(x)) {

    # Skip non-response entries (e.g. "rmd" character string)
    sublist <- x[[category]]
    if (!is.list(sublist)) next

    # Re-print the stored publication-ready ggplot objects (coefficient
    # forest plot, estimated-means and interaction plots) so the interactive
    # plot() output matches the report. These are done before the DHARMa
    # diagnostics and independently of them, so they still appear for
    # quasi-families where simulation-based residuals are unavailable.
    plot_keys <- grep(paste0("^(coef_forest_plot|effect_plot_|",
                             "interaction_plot_|contrast_plot_|",
                             "interaction_contrast_plot_)"),
                      names(sublist), value = TRUE)
    for (k in plot_keys) {
      p_obj <- sublist[[k]]
      if (inherits(p_obj, "ggplot")) print(p_obj)
    }

    sim_res <- sublist$diagnostics$residuals$sim_res

    if (is.null(sim_res)) {
      message("plot.f_glm: DHARMa diagnostic plots skipped for '", category,
              "' (quasi-family or simulation not available).")
      next
    }

    # 1: QQ plot
    graphics::par(mfrow = c(1, 1))
    plot(sim_res)

    # 2: Residual tests (DHARMa sets its own multi-panel layout internally)
    graphics::par(mfrow = c(1, 1))
    DHARMa::testResiduals(sim_res, plot = TRUE)
  }
}


