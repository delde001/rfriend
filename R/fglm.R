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
  # then takes the LRT vs the full model. Letters assigned via multcompLetters().
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
      multcompView::multcompLetters(adj_p, threshold = alpha),
      error = function(e) NULL
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
                              type  = type)

    pairs_emm <- pairs(emm, adjust = adjust)

    mult_cld <- multcomp::cld(emm,
                              Letters = letters,
                              alpha   = alpha,
                              adjust  = adjust)

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
              "Refer to the **Type II Analysis of Deviance** table for the overall test."
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
            "prove the groups are identical.*"
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
title: \"f_glm Analysis Report\"
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

      # Detect separation early so perform_posthoc can handle letter assignment correctly.
      # Heuristic: any predictor coefficient has SE > 100x its absolute estimate.
      coef_mat_early <- summary(glm_fit)$coefficients
      sep_flag_early <- if (nrow(coef_mat_early) > 1) {
        any(coef_mat_early[-1, "Std. Error"] > 100 * abs(coef_mat_early[-1, "Estimate"]),
            na.rm = TRUE)
      } else FALSE
      output_list[[response_name]][["sep_flag"]] <- sep_flag_early

      # ---- Interaction-aware emmeans mode (mirrors f_aov logic) ----
      # Parse drop1 term names to classify interactions vs main effects.
      # Determines whether emmeans returns cell means or marginal means,
      # and what note to show the user -- matching f_aov's emm_is_cells approach.
      drop1_for_int  <- output_list[[response_name]][["drop1"]]
      cat_preds      <- predictor_names[vapply(predictor_names, function(v)
        is.factor(data[[v]]) || is.character(data[[v]]),
        FUN.VALUE = logical(1))]

      emm_is_cells   <- FALSE
      sig_interactions  <- character(0)
      sig_main_effects  <- character(0)
      ns_main_effects   <- character(0)

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
          "not on individual factor effects in isolation.  \n\n"
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
      for unbalanced designs and reflect the statistical model."
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

  for (category in names(x)) {

    # Skip non-response entries (e.g. "rmd" character string)
    sublist <- x[[category]]
    if (!is.list(sublist)) next

    sim_res <- sublist$diagnostics$residuals$sim_res

    if (is.null(sim_res)) {
      message("plot.f_glm: DHARMa diagnostic plots skipped for '", category,
              "' (quasi-family or simulation not available).")
      next
    }

    # 1: QQ plot
    plot(sim_res)

    # 2: Residual tests
    DHARMa::testResiduals(sim_res)
  }
}


