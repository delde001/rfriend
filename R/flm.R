#' Perform multiple \code{lm()} functions with optional data transformation, inspection, regression plots and post hoc test.
#'
#' Performs ordinary least squares linear regression (\code{stats::lm}) on a
#' given dataset with options for (Box-Cox) transformations, normality tests,
#' regression plots, and post hoc analysis for categorical predictors. Several
#' response parameters can be analysed in sequence and the generated output can
#' be in various formats ('Word', 'pdf', 'Excel').
#'
#' \code{f_lm()} is the regression sibling of \code{\link{f_aov}}: it shares the
#' same assumption checks (Shapiro-Wilk, Anderson-Darling, Levene / Breusch-Pagan),
#' the same optional Box-Cox / bestNormalize transformation workflow, the same
#' object structure, and the same shared publication theme
#' (\code{f_theme_pub()}) and palette (\code{f_pub_palette()}). Where
#' \code{f_aov()} converts all predictors to factors, \code{f_lm()} keeps numeric
#' predictors numeric so they are modelled as continuous regression terms, and
#' it adds regression-specific output: a coefficient table, a coefficient forest
#' plot, a Type II Analysis of Variance table, the overall \eqn{R^2} / adjusted
#' \eqn{R^2} / model F-test, and regression plots (see Details).
#'
#' @param formula A formula specifying the model to be fitted. More response variables can be added using \code{-} or \code{+} (e.g., \code{response1 + response2 ~ predictor}) to do a sequential \code{lm()} for each response parameter.
#' @param data A data frame containing the variables in the model.
#' @param norm_plots Logical. If \code{TRUE}, diagnostic residual plots are included in the output files. Default is \code{TRUE}.
#' @param effect_plots Logical. If \code{TRUE}, regression / effect plots are
#'   included in the output files after the post hoc table. See Details for what
#'   is drawn and how the plots are stored. Default is \code{TRUE}.
#' @param contrast_plots Logical. If \code{TRUE}, a contrast forest plot is added
#'   for each categorical post hoc term: one row per pairwise comparison, showing
#'   the estimated difference between two levels with its confidence interval and
#'   a reference line at zero. A CI that excludes zero indicates a significant
#'   difference. Default \code{FALSE} because the number of pairwise contrasts
#'   grows quickly with the number of factor levels (k levels give k(k-1)/2
#'   contrasts). Main-effect contrast plots are stored as
#'   \code{out$y1$contrast_plot_<term>} and interaction cell-contrast plots as
#'   \code{out$y1$interaction_contrast_plot_<term>}. Contrast CIs use the same
#'   \code{adjust} method as the post hoc p-values, so figure and table agree.
#' @param transformation Logical or character string. If \code{TRUE}, or if \code{"boxcox"} applies an \code{f_boxcox()} transformation if residuals are not normal. If \code{"bestnormalize"}, applies \code{f_bestNormalize()} transformation. If \code{FALSE} no transformation will be applied. Default is \code{TRUE}.
#' @param force_transformation Character string. A vector containing the names of response variables that should be transformed regardless of the normality test. Default is \code{NULL}.
#' @param alpha Numeric. Significance level for tests, post hoc comparisons, and Shapiro-Wilk test. Default is \code{0.05}.
#' @param adjust Character string specifying the method used to adjust p-values
#'   for multiple comparisons. Available methods include:
#'   \describe{
#'     \item{"tukey"}{Tukey's Honest Significant Difference method.}
#'     \item{"sidak"}{Sidak correction.}
#'     \item{"bonferroni"}{Bonferroni correction.}
#'     \item{"none"}{No adjustment.}
#'     \item{"fdr"}{False Discovery Rate adjustment.}
#'   } Default is \code{"sidak"}.
#' @param intro_text Logical. If \code{TRUE}, includes a short explanation about linear regression assumptions in the output file. Default is \code{TRUE}.
#' @param close_generated_files Logical. Closes open Excel or Word (NOT pdf) files before writing, depending on the output format. Works on Windows (taskkill), macOS (pkill) and Linux (pkill/soffice). Default \code{FALSE}. \strong{WARNING:} Always save your work before using this option!!
#' @param open_generated_files Logical. Whether to open the generated output
#'   files after creation. Defaults to \code{TRUE} in an interactive R session
#'   and \code{FALSE} otherwise (e.g. in scripts or automated pipelines).
#' @param output_type Character string specifying the output format. Default is \code{"default"}.
#'   \itemize{
#'     \item \code{"default"}: Returns the object and lets R decide whether
#'       to print; auto-prints if unassigned, silent if assigned to a variable.
#'     \item \code{"console"}: Forces immediate printing to the console.
#'     \item \code{"pdf"}, \code{"word"}, \code{"excel"}: Saves results to a
#'       file of the corresponding format.
#'     \item \code{"rmd"}: Stores the raw markdown string inside the returned
#'       object for use in R Markdown documents.
#'   }
#' @param save_as Character string specifying the output file path (without extension).
#'   If a full path is provided, output is saved to that location.
#'   If only a filename is given, the file is saved in \code{tempdir()}.
#'   If only a directory is specified (existing directory with trailing slash),
#'   the file is named "dataname_lm_output" in that directory. If an extension is provided the output format specified with option "output_type" will be overruled.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory. Default is \code{FALSE}. If \code{save_as} location is specified \code{save_in_wdir} is overwritten by \code{save_as}.
#' @param ... Additional arguments forwarded to \code{\link[stats]{lm}}.
#'   The arguments \code{subset} and \code{weights} are handled specially: when
#'   supplied, they are applied via \code{\link[stats]{model.frame}} so that the
#'   Shapiro-Wilk test, Levene/Breusch-Pagan test, optional transformations,
#'   residual diagnostics, and \code{emmeans} post hoc tests all see the exact
#'   same row set as \code{lm()} itself.
#'
#' @return An object of class 'f_lm' containing results from \code{lm()}, normality tests, transformations, coefficient tables, Type II ANOVA, and post hoc tests. Using the option "output_type", it can also generate output in the form of: R Markdown code, 'Word', 'pdf', or 'Excel' files. Includes print and plot methods for 'f_lm' objects.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#' \item Check if all specified variables are present in the data.
#' \item Ensure that the response variable is numeric.
#' \item Fit a linear model using the specified formula and data. Numeric
#'   predictors are kept numeric (continuous regression terms); character and
#'   logical predictors are converted to factors.
#' \item Check normality of the residuals using the Shapiro-Wilk and
#'   Anderson-Darling tests, and homoscedasticity using a Breusch-Pagan test
#'   (\code{rstatix} / \code{car} style) on the model residuals.
#' \item If residuals are not normal and \code{transformation = TRUE} apply a
#'   data transformation and refit.
#' \item Report the coefficient table, a coefficient forest plot, a Type II
#'   Analysis of Variance table, and the overall model fit (\eqn{R^2},
#'   adjusted \eqn{R^2}, F-statistic).
#' \item For categorical predictors, if significant effects are found, post hoc
#'   tests use estimated marginal means from \code{emmeans()} with the chosen
#'   adjustment, summarised with a compact letter display.
#' }
#'
#' \strong{Regression and effect plots.} When \code{effect_plots = TRUE} the
#' following figures are produced (the current de facto standard for visualising
#' a multiple-regression model):
#' \itemize{
#'   \item \strong{Coefficient forest plot} (always, when there is more than an
#'     intercept): one row per coefficient with its Wald confidence interval and
#'     a reference line at zero. This is the standard way to read a model with
#'     many predictors at a glance and scales to any number of terms. Stored as
#'     \code{out$y1$coef_forest_plot}.
#'   \item \strong{Partial-effect (adjusted prediction) plots} for each
#'     continuous predictor: the model-predicted response across the range of
#'     that predictor, with the other predictors held at their mean (numeric) or
#'     reference level (factor), shown with a confidence band and a rug / scatter
#'     of the raw data. These are computed from the fitted model via
#'     \code{emmeans::emmip()} (equivalent to the \pkg{effects} / \pkg{ggeffects}
#'     standard) so the lines are consistent with the rest of the report. Stored
#'     as \code{out$y1$effect_plot_<predictor>}.
#'   \item \strong{Estimated-means plots} for each categorical predictor
#'     (estimate \eqn{\pm} CI, jittered raw data, compact-letter-display
#'     labels), matching \code{\link{f_aov}}. Stored as
#'     \code{out$y1$effect_plot_<predictor>}.
#'   \item \strong{Slope plots} for a significant numeric \eqn{\times}
#'     categorical interaction: a scatter of the raw data with one model-fitted
#'     regression line per factor level and a confidence band (the lines are not
#'     parallel when the interaction is significant), matching
#'     \code{\link{f_lmer}}. Stored as
#'     \code{out$y1$interaction_plot_<num>_<fac>}.
#'   \item \strong{Categorical interaction plots} (2-, 3-, 4-way) when a
#'     significant categorical interaction is present, matching
#'     \code{\link{f_aov}}.
#'   \item \strong{Observed-versus-fitted plot}: observed response against the
#'     model fitted values with a 1:1 reference line, a quick visual of overall
#'     fit. Stored as \code{out$y1$obs_fitted_plot}.
#' }
#' All plots are \pkg{ggplot2} objects stored in the returned object so they can
#' be retrieved and customised, and \code{plot()} re-prints them so the
#' interactive output matches the report output.
#'
#' When the response was transformed (Box-Cox or bestNormalize), the post hoc
#' estimates are back-transformed to the original scale (medians), exactly as in
#' \code{\link{f_aov}}.
#'
#' Outputs can be generated in multiple formats ("pdf", "word", "excel" and "rmd") as specified by \code{output_type}. If \code{output_type = "rmd"} is used it is advised to use it in a chunk with \{r, echo=FALSE, results='asis'\}.
#'
#' This function requires [Pandoc](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3 or higher), a universal document converter.
#'
#' @section Multiple Testing Across Response Variables:
#' When several response variables are analysed in a single call
#' (e.g. \code{y1 + y2 + y3 ~ x}), each regression is an independent
#' null-hypothesis test at level \code{alpha}. The post hoc adjustments only
#' control the family-wise error rate \strong{within} one model. They do
#' \strong{not} protect against the inflation of Type I error \strong{across}
#' the set of responses. With \eqn{k} independent responses all tested at
#' \eqn{\alpha = 0.05}, the probability of at least one false positive is
#' \eqn{1 - (1 - 0.05)^k}. Consider a Bonferroni (\code{alpha = 0.05 / k}) or
#' FDR correction across responses, or pre-registration of primary outcomes.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # Continuous predictor: simple linear regression.
#' f_lm_out <- f_lm(Sepal.Length ~ Petal.Length, data = iris)
#' print(f_lm_out)
#' plot(f_lm_out)
#'
#'\donttest{
#' # Mixed continuous + categorical predictors (ANCOVA-style multiple regression).
#' iris$Species <- factor(iris$Species)
#' f_lm(Sepal.Length ~ Petal.Length + Species, data = iris, output_type = "word")
#'
#' # Two responses analysed in sequence, captured in one output file.
#' f_lm(Sepal.Width + Sepal.Length ~ Petal.Length * Species,
#'            effect_plots = FALSE,
#'            norm_plots = FALSE,
#'            data = iris)
#'
#' # To print rmd output set chunk option to results = 'asis' and use cat().
#' f_lm_rmd_out <- f_lm(Sepal.Length ~ Petal.Length, data = iris, output_type = "rmd")
#' cat(f_lm_rmd_out$rmd)
#' }
#'
#' @export
f_lm <- function(formula,
                 data = NULL,
                 norm_plots = TRUE,
                 effect_plots = TRUE,
                 contrast_plots = FALSE,
                 transformation = TRUE,
                 force_transformation = NULL,
                 alpha = 0.05,
                 adjust = "sidak",
                 intro_text = TRUE,
                 close_generated_files = FALSE,
                 open_generated_files = interactive(),
                 output_type = "default",
                 save_as = NULL,
                 save_in_wdir = FALSE,
                 ...
)
{

  ########## Reset initial settings on exit #################################
  .session_state <- save_session_state()  # Helper function: helper_session_state
  on.exit(restore_session_state(.session_state), add = TRUE)

  ########## Capture ... UNEVALUATED at f_lm's own frame ##################
  # Must be done HERE (in f_lm's body), not inside generate_report(),
  # because match.call() captures the call to whatever function it is called
  # from. The closure makes dots_exprs visible inside generate_report().
  .mc        <- match.call(expand.dots = FALSE)
  dots_exprs <- as.list(.mc[["..."]])
  caller_env <- parent.frame()

  ####### Save dataframe name and Handle input from vectors #####
  if (!is.null(data)) {
    data_name <- deparse(substitute(data))
  } else {
    if (length(formula_extract_df_names(formula)) == 0) {
      data_name <- "data"
    } else if (length(formula_extract_df_names(formula)) == 1) {
      data_name <- formula_extract_df_names(formula)
    } else {
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
  file.create(temp_output_file)
  file_extension <- NULL

  # Wrap lines in rmd output document
  f_wrap_lines()

  # Create a list to store all outputs in this function
  output_list <- list()

  # Parameter validation
  if (!(output_type %in% c("pdf", "word", "excel", "rmd", "console", "default"))) {
    stop("Character string specifying the output format (output_type = ) should be either: 'pdf', 'word', 'excel', 'console', 'rmd', 'default'")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be a single numeric value strictly between 0 and 1 (e.g. 0.05).")
  }

  #### Handle option "save_as = " ###
  if (save_in_wdir == TRUE) {
    save_dir <- getwd()
  } else {
    save_dir <- tempdir()
  }

  # Map the output type to extensions
  output_type_map <- c(
    "pdf"   = ".pdf",
    "word"  = ".docx",
    "excel" = ".xlsx",
    "rmd"   = ".rmd"
  )

  if (!is.null(save_as) || save_in_wdir == TRUE) {
    if (!is.null(save_as)) {
      save_as <- gsub(pattern = "\\\\", replacement = "/", x = save_as)
      file_extension_save_as <- unname(extract_extension(save_as))
      if (file_extension_save_as[1] != FALSE) {
        file_extension <- file_extension_save_as
      }
    }

    if (is.null(file_extension) && output_type %in% c("console", "default")) {
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "lm_output", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = ".pdf")
      output_type <- "pdf"
    } else if (is.null(file_extension) && output_type %in% c("pdf", "word", "excel", "rmd")) {
      file.ext <- unname(output_type_map[output_type])
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "lm_output", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = file.ext)
    } else if (!is.null(file_extension)) {
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "lm_output", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = file_extension[1])
      output_type <- file_extension[2]
    }
  } else {
    file.ext <- unname(output_type_map[output_type])
    output_path <- get_save_path(save_as = save_as,
                                 default_name = paste(data_name, "lm_output", sep = "_"),
                                 default_dir = save_dir,
                                 file.ext = file.ext)
  }

  # Prevent output to console and keep files open when output is "rmd" format
  if (output_type == "rmd") close_generated_files <- FALSE

  # Cross-platform close_generated_files
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

  # Normalise transformation input (mirrors f_aov)
  if (is.character(transformation)) {
    trans_input   <- tolower(as.character(transformation))
    valid_options <- c("bestnormalize", "boxcox", "true", "false")
    matched_index <- pmatch(trans_input, valid_options)
    if (is.na(matched_index)) stop("Invalid transformation option!")
    matched_option <- valid_options[matched_index]
    if (matched_option == "true") {
      transformation <- TRUE
    } else if (matched_option == "false") {
      transformation <- FALSE
    } else {
      transformation <- matched_option
    }
  }

  # Warn if LHS has expressions like log(y) before silently stripping them
  check_lhs_is_names(formula) # helper_check_lhs.R

  # Extract response and predictor variables from the formula
  lhs             <- all.vars(formula[[2]])
  response_names  <- lhs
  predictor_names <- all.vars(formula[[3]])
  rhs             <- deparse(formula[[3]])

  # Ensure response and predictors are in the data; response must be numeric
  for (response in response_names) {
    if (!(response %in% names(data))) {
      stop(paste("Response variable", response, "not found in the data."))
    }
    if (!is.numeric(data[[response]])) {
      stop("The response variable must be numeric.")
    }
  }
  for (predictor in predictor_names) {
    if (!(predictor %in% names(data))) {
      stop(paste("Predictor variable", predictor, "not found in the data."))
    }
  }

  # Keep numeric predictors numeric (continuous regression terms). Convert
  # character / logical predictors to factors so emmeans treats them as
  # categorical. Numeric predictors are deliberately NOT factorised (this is
  # the key difference from f_aov, which factorises everything).
  for (predictor in predictor_names) {
    if (is.character(data[[predictor]]) || is.logical(data[[predictor]])) {
      data[[predictor]] <- as.factor(data[[predictor]])
    }
  }

  # ----------------------------------------------------------------------
  # Main report generator
  # ----------------------------------------------------------------------
  generate_report <- function(output = TRUE) {

    if (intro_text == TRUE) {
      cat("
# Assumptions of Linear Regression
Checking the assumptions of ordinary least squares (OLS) linear regression is critical for ensuring the validity of its results:\n

## 1. Linearity
- The relationship between each predictor and the mean of the response is assumed to be linear (in the parameters). Curvature in a residuals-vs-fitted plot signals a violation; consider adding polynomial terms or a transformation.

## 2. Independence
- Observations (and therefore residuals) must be independent. Independence violations (e.g. repeated measures, clustering, time series) cannot be fixed by transformation; use a mixed model (`f_lmer()`) or a model with an appropriate error structure instead.

## 3. Normality of residuals
- The residuals are assumed to be normally distributed. This applies to the residuals, not the raw response. OLS is robust to mild deviations in large samples. Assessed with the Shapiro-Wilk and Anderson-Darling tests and graphically with a Q-Q plot, histogram, or box plot.

## 4. Homoscedasticity (constant variance)
- The residual variance should be constant across the range of fitted values. A funnel shape in the residuals-vs-fitted plot indicates heteroscedasticity. Assessed with a Breusch-Pagan test. If violated, a transformation, weighted least squares, or robust (heteroscedasticity-consistent) standard errors are options.
  \n  \n")

      if (output_type != "rmd") {
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage
        ")
      }
    }

    i <- 0  # count to remove last page break

    # Multiple-response warning
    if (length(lhs) > 1) {
      k          <- length(lhs)
      fwer_pct   <- round((1 - (1 - alpha)^k) * 100, 1)
      bonf_alpha <- round(alpha / k, 4)
      cat(paste0(
        "\n\n***\n\n",
        "**[!] NOTE: Multiple Testing Across ", k, " Response Variables**  \n\n",
        "This report runs ", k, " independent linear regressions on the same dataset. ",
        "The **", adjust, "** correction keeps each individual test honest, it guards against ",
        "false positives among the pairwise group comparisons, but it offers no protection ",
        "against the accumulation of error across all ", k, " tests combined. ",
        "\nAt \u03b1 = ", alpha, " per test, the probability of obtaining at least one ",
        "spurious significant result across all ", k, " responses is approximately ",
        "**", fwer_pct, "%** ($1-(1-", alpha, ")^{", k, "}$, assuming independence). ",
        "This risk is highest in exploratory studies; it is less of a concern when ",
        "each response has a clear a priori hypothesis.  \n\n",
        "**Possible remedies:**  \n",
        "\n-  **Bonferroni** (conservative): re-run with `alpha = ", bonf_alpha,
        "` (\u03b1 / ", k, ").",
        "\n-  **False Discovery Rate (FDR)**: apply `p.adjust(p_values, method = \"fdr\")` ",
        "to the ", k, " model p-values after the fact.",
        "\n-  **Pre-registration**: if each response was a pre-specified (primary) study ",
        "outcome, correction may not be required; document this decision explicitly.  \n  \n",
        "\n\n***\n\n"
      ))
    }

    # ---- Build the analysis data set ONCE, before the response loop -----
    # Use ALL responses + ALL predictors so every response is analysed on the
    # identical row set. subset / weights passed via `...` are applied here so
    # all downstream steps see the same rows as lm() itself.
    combined_formula <- stats::as.formula(
      paste("~", paste(c(lhs, predictor_names), collapse = " + "))
    )

    subset_vec <- NULL
    if (!is.null(dots_exprs$subset)) {
      subset_vec <- eval(dots_exprs$subset, envir = data, enclos = caller_env)
      if (is.logical(subset_vec)) subset_vec[is.na(subset_vec)] <- FALSE
    }

    weights_vec <- NULL
    if (!is.null(dots_exprs$weights)) {
      weights_vec <- eval(dots_exprs$weights, envir = data, enclos = caller_env)
    }

    if (!is.null(subset_vec)) {
      if (!is.null(weights_vec) && length(weights_vec) == nrow(data)) {
        weights_vec <- weights_vec[subset_vec]
      }
      data <- data[subset_vec, , drop = FALSE]
    }

    # Master model frame: complete cases across all responses + predictors,
    # carrying weights if supplied so every response uses the same rows.
    mf_args <- list(formula = combined_formula, data = data,
                    na.action = stats::na.omit)
    if (!is.null(weights_vec)) mf_args[["weights"]] <- weights_vec
    data_master <- do.call(stats::model.frame, mf_args)

    n_before_master <- nrow(data)
    n_after_master  <- nrow(data_master)

    # Extra args for lm(): only safe pass-throughs (drop subset/weights which
    # are baked in). weights re-supplied from the model frame each iteration.
    lm_extra <- dots_exprs
    lm_extra[c("subset", "weights", "na.action")] <- NULL
    lm_extra <- lapply(lm_extra, function(e) eval(e, envir = data, enclos = caller_env))

    # ---------------------------- Response loop ----------------------------
    for (response_name in response_names) {

      output_list[[response_name]] <- list()
      output_list[[response_name]][["transformation_option"]] <- transformation
      output_list[[response_name]][["adjust"]]                <- adjust

      current_formula <- stats::as.formula(paste0(response_name, "~", rhs))

      n_before <- n_before_master
      n_after  <- n_after_master

      # Fresh per-iteration copy of the master filtered frame (transformation
      # mutates the response in place; copy prevents leakage between responses).
      data_complete <- data_master
      mf_weights    <- stats::model.weights(data_complete)

      # Snapshot the original (untransformed) response for raw-data overlays.
      original_response <- data_complete[[response_name]]

      cat("   \n  \n# Analysis of: ", response_name, "  \n")
      cat("  \n## Normality and homoscedasticity of residuals of: ", response_name, "  \n")

      # Fit the linear model
      lm_fit <- do.call(
        stats::lm,
        c(list(formula = current_formula, data = data_complete,
               weights = mf_weights), lm_extra)
      )
      output_list[[response_name]][["lm_fit"]]   <- lm_fit
      output_list[[response_name]][["lm_call"]]  <- deparse(current_formula)
      res_lm <- residuals(lm_fit)

      # Breusch-Pagan test for homoscedasticity (constant residual variance vs
      # fitted values). Regression analogue of Levene's test; uses car if
      # available, else a base-R Koenker score regression of squared residuals
      # on fitted values.
      bp_res <- safe_breusch_pagan(lm_fit)  # local helper, defined below
      is_bp_sig <- !is.na(bp_res$p.value) & bp_res$p.value < alpha

      if (is.na(bp_res$p.value)) {
        bp_intp <- "Breusch-Pagan test could not be calculated.  \n"
      } else if (bp_res$p.value > alpha) {
        bp_intp <- paste0("According to the 'Breusch-Pagan Test' (",
                          round(bp_res$p.value, 4), " > ", alpha,
                          ") residuals do not depart from **equal variance**
                          (homoscedasticity).  \n")
      } else {
        bp_intp <- paste0("According to the 'Breusch-Pagan Test' (",
                          round(bp_res$p.value, 4), " \u2264 ", alpha,
                          ") residuals do **NOT** have constant
                          variance (heteroscedasticity).  \n")
      }
      cat("**Breusch-Pagan test** for homoscedasticity of residuals: statistic =",
          round(bp_res$statistic, 4), "p-value =", round(bp_res$p.value, 4),
          ". ", bp_intp, "  \n&nbsp;  \n")

      # Shapiro-Wilk test for normality of residuals
      shapiro_res <- safe_shapiro(res_lm)  # helper_safe_shapiro
      if (is.na(shapiro_res$p.value)) {
        shapiro_intp <- paste0("Shapiro-Wilk was skipped (", shapiro_res$method,
                               "). Normality decision is based on the Anderson-Darling test below.  \n  \n")
      } else if (shapiro_res$p.value > alpha) {
        shapiro_intp <- paste0("According to 'Shapiro-Wilk Test' (",
                               round(shapiro_res$p.value, 4), " > ", alpha,
                               ") no significant departure from **normality** was detected
          for the model residuals; check Q-Q plot.  \n  \n")
      } else {
        shapiro_intp <- paste0("According to 'Shapiro-Wilk Test' (",
                               round(shapiro_res$p.value, 4), "\u2264", alpha,
                               ") residuals are **NOT** normally distributed.  \n  \n")
      }
      cat("**Shapiro-Wilk Test** for Normality of residuals: W =",
          round(shapiro_res$statistic, 4), "p-value =",
          round(shapiro_res$p.value, 4), ". ", shapiro_intp)

      # Anderson-Darling test (requires n >= 8)
      if (length(res_lm) >= 8) {
        adt_res <- nortest::ad.test(res_lm)
      } else {
        adt_res <- list(statistic = NA_real_, p.value = NA_real_,
                        method = "Anderson-Darling test (skipped: n < 8)")
      }
      if (is.na(adt_res$p.value)) {
        adt_intp <- "Anderson-Darling test skipped (n < 8, minimum sample size not met).  \n  \n"
      } else if (as.numeric(adt_res$p.value) > alpha) {
        adt_intp <- paste0("According to 'Anderson-Darling test' (",
                           round(adt_res$p.value, 4), " > ", alpha,
                           ") no significant departure from **normality** was detected
          for the model residuals; check Q-Q plot.  \n  \n")
      } else {
        adt_intp <- paste0("According to 'Anderson-Darling test' (",
                           round(adt_res$p.value, 4), "\u2264", alpha,
                           ") residuals are **NOT** normally distributed.  \n  \n")
      }
      cat(adt_res$method, ": A =", round(adt_res$statistic, 4),
          " p =", round(adt_res$p.value, 4), "   \n", adt_intp)

      output_list[[response_name]][["breusch_pagan_test"]]     <- bp_res
      output_list[[response_name]][["shapiro_test_residuals"]] <- shapiro_res
      output_list[[response_name]][["adt_test_residuals"]]     <- adt_res
      output_list[[response_name]][["alpha"]]                  <- alpha

      # Diagnostic residual plots (2x2): residuals vs fitted, boxplot, hist, QQ
      temp_file <- tempfile(fileext = ".png")
      grDevices::png(temp_file, width = 7.8, height = 7.8, units = "in", res = 300)
      graphics::par(mfrow = c(2, 2), mar = c(4, 4, 4, 1))
      plot(lm_fit, 1)
      boxplot(res_lm, main = "Boxplot", xlab = "Residuals")
      f_hist(res_lm, xlab = "Residuals")
      f_qqnorm(res_lm)
      grDevices::dev.off()

      if (norm_plots == TRUE) {
        cat("Check the plots in the figure below to assess normality and homoscedasticity.  \n")
        cat(paste0("![](", temp_file, ")"), "   \n")
      }
      output_list[[response_name]][["normality_plots"]] <- temp_file

      if (output_type != "rmd") {
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage
        ")
      }

      # ---- Decide whether to transform -----------------------------------
      if (!is.na(shapiro_res$p.value)) {
        trigger_normality <- shapiro_res$p.value < alpha
      } else if (!is.na(adt_res$p.value)) {
        trigger_normality <- adt_res$p.value < alpha
      } else {
        trigger_normality <- FALSE
      }
      trigger_bp     <- is_bp_sig
      trigger_forced <- response_name %in% force_transformation

      aov_summary_transformed <- NULL  # sentinel for "no transform applied"
      transformed_var         <- NULL

      if (trigger_normality || trigger_bp || trigger_forced) {
        if (transformation == FALSE) {
          if (trigger_normality) {
            test_label <- if (!is.na(shapiro_res$p.value)) "Shapiro-Wilk" else "Anderson-Darling"
            cat(paste0(
              "   \n  \n**WARNING !!!**   \nBased on the ", test_label,
              " test the residuals are **NOT** normally distributed.   \n  \n",
              "Please enable the transformation function (transformation = TRUE) ",
              "in the f_lm function.   \n  \n"))
          }
          if (trigger_bp) {
            cat("   \n  \n**WARNING !!!**   \nBased on the Breusch-Pagan Test the residuals do **NOT** have constant variance (heteroscedasticity).   \n  \nPlease enable the transformation function (transformation = TRUE) in the f_lm function.   \n  \n")
          }
        }

        if (!trigger_normality && !trigger_forced && trigger_bp && transformation != FALSE) {
          cat("   \n  \n**NOTE**   \nHeteroscedasticity was detected (Breusch-Pagan Test), but residuals are normally distributed (Shapiro-Wilk). Weighted least squares or heteroscedasticity-consistent (robust) standard errors are recommended. \nNevertheless, Box-Cox is applied since it can also stabilise variance; check the transformed Breusch-Pagan result below.   \n  \n")
        }

        if (transformation == "boxcox" || isTRUE(transformation)) {
          cat("\n   \n## Box-Cox transformation of: ", response_name, "  \n  \n")
          utils::capture.output(
            transformed_var <- f_boxcox(data_complete[[response_name]],
                                        output_type = "rmd", alpha = alpha)
          )
          cat("Note: the Shapiro-Wilk test below is performed on the response data, not the residuals.")
          cat(transformed_var$rmd)
          output_list[[response_name]][["boxcox"]] <- transformed_var$transformed_data
          data_complete[[response_name]] <- transformed_var$transformed_data
        }

        if (transformation == "bestnormalize") {
          transformed_var <- f_bestNormalize(data_complete[[response_name]],
                                             data_name = response_name,
                                             output_type = "rmd", alpha = alpha)
          output_list[[response_name]][["bestNormalize"]] <- transformed_var
          data_complete[[response_name]] <- transformed_var$transformed_data
          cat("Note: the Shapiro-Wilk test below is performed on the response data, not the residuals.")
          cat(transformed_var$rmd)
          cat("    \n    \n")
        }

        if (transformation != FALSE) {
          lm_fit_transformed <- do.call(
            stats::lm,
            c(list(formula = current_formula, data = data_complete,
                   weights = mf_weights), lm_extra)
          )
          transformed_res <- residuals(lm_fit_transformed)

          shapiro_res_transformed <- safe_shapiro(transformed_res)
          if (length(transformed_res) >= 8) {
            adt_res_transformed <- nortest::ad.test(transformed_res)
          } else {
            adt_res_transformed <- list(statistic = NA_real_, p.value = NA_real_,
                                        method = "Anderson-Darling test (skipped: n < 8)")
          }
          bp_res_transformed  <- safe_breusch_pagan(lm_fit_transformed)
          is_bp_trans_sig     <- !is.na(bp_res_transformed$p.value) & bp_res_transformed$p.value < alpha

          cat("\n   \n## Normality and homoscedasticity of **TRANSFORMED residuals** of : ",
              response_name, "   \n  \n")

          if (is.na(bp_res_transformed$p.value)) {
            bp_intp_t <- "Breusch-Pagan test could not be calculated on transformed residuals.  \n"
          } else if (bp_res_transformed$p.value > alpha) {
            bp_intp_t <- paste0("According to the 'Breusch-Pagan Test' (",
                                round(bp_res_transformed$p.value, 4), " > ", alpha,
                                ") transformed residuals **have constant variance** (homoscedasticity).")
          } else {
            bp_intp_t <- paste0("According to the 'Breusch-Pagan Test' (",
                                round(bp_res_transformed$p.value, 4), " \u2264 ", alpha,
                                ") transformed residuals do **NOT** have constant variance (heteroscedasticity).")
          }
          cat("**Breusch-Pagan test** for homoscedasticity of transformed residuals: statistic =",
              round(bp_res_transformed$statistic, 4), "p-value =",
              round(bp_res_transformed$p.value, 4), ". ", bp_intp_t, "  \n&nbsp;  \n")

          if (is.na(shapiro_res_transformed$p.value)) {
            shapiro_intp_t <- paste0("Shapiro-Wilk was skipped (", shapiro_res_transformed$method,
                                     "). Normality decision is based on the Anderson-Darling test below.  \n  \n&nbsp;  \n")
          } else if (shapiro_res_transformed$p.value > alpha) {
            shapiro_intp_t <- paste0("According to the 'Shapiro-Wilk' test (",
                                     round(shapiro_res_transformed$p.value, 4), " > ", alpha,
                                     ") transformed residuals **ARE normally distributed**.  \n  \n&nbsp;  \n")
          } else {
            shapiro_intp_t <- paste0("According to the 'Shapiro-Wilk' test (",
                                     round(shapiro_res_transformed$p.value, 4), "\u2264", alpha,
                                     ") transformed residuals are **NOT** normally distributed.  \n  \n&nbsp;  \n")
          }
          cat("**Shapiro-Wilk Test** for Normality of transformed residuals: W =",
              round(shapiro_res_transformed$statistic, 4), "p-value =",
              round(shapiro_res_transformed$p.value, 4), ". ", shapiro_intp_t)

          if (is.na(adt_res_transformed$p.value)) {
            adt_intp_t <- "Anderson-Darling test skipped (n < 8, minimum sample size not met).  \n  \n"
          } else if (as.numeric(adt_res_transformed$p.value) > alpha) {
            adt_intp_t <- paste0("According to 'Anderson-Darling test' (",
                                 round(adt_res_transformed$p.value, 4), " > ", alpha,
                                 ") transformed residuals **ARE normally distributed**.  \n  \n")
          } else {
            adt_intp_t <- paste0("According to 'Anderson-Darling test' (",
                                 round(adt_res_transformed$p.value, 4), "\u2264", alpha,
                                 ") transformed residuals are **NOT** normally distributed.  \n  \n")
          }
          cat(adt_res_transformed$method, ": A =", round(adt_res_transformed$statistic, 4),
              " p =", round(adt_res_transformed$p.value, 4), ". ", adt_intp_t)

          output_list[[response_name]][["transformed_lm_fit"]]       <- lm_fit_transformed
          output_list[[response_name]][["transformed_shapiro_test"]] <- shapiro_res_transformed
          output_list[[response_name]][["transformed_adt_test"]]     <- adt_res_transformed
          output_list[[response_name]][["transformed_breusch_pagan"]]<- bp_res_transformed

          temp_file <- tempfile(fileext = ".png")
          grDevices::png(temp_file, width = 7.8, height = 7.8, units = "in", res = 300)
          graphics::par(mfrow = c(2, 2), mar = c(4, 4, 4, 1))
          plot(lm_fit_transformed, 1, main = "Residuals transformed data")
          boxplot(transformed_res, main = "Boxplot", xlab = "Residuals transformed data")
          f_hist(transformed_res, xlab = "Residuals transformed data")
          f_qqnorm(transformed_res)
          grDevices::dev.off()

          output_list[[response_name]][["transformed_normality_plots"]] <- temp_file
          if (norm_plots == TRUE) {
            cat("  \nCheck the **residual** plots of the **transformed data** in the figure below.  \n")
            cat(paste0("![](", temp_file, ")"), "   \n")
          }

          # Mark as transformed for downstream blocks
          aov_summary_transformed <- TRUE

          if (output_type != "rmd") {
            cat("
<div style=\"page-break-after: always;\"></div>
\\newpage
            ")
          }
        }
      }

      # Pick the active model (transformed where applicable)
      if (!is.null(aov_summary_transformed)) {
        lm_fit_out           <- lm_fit_transformed
        Response_Transformed <- TRUE
      } else {
        lm_fit_out           <- lm_fit
        Response_Transformed <- FALSE
      }
      output_list[[response_name]][["Response_Transformed"]] <- Response_Transformed
      output_list[[response_name]][["lm_fit_active"]]        <- lm_fit_out

      # Back-transform closure for the post hoc table / plots
      if (Response_Transformed) {
        if (transformation == "boxcox" || isTRUE(transformation)) {
          lambda_val <- transformed_var$lambda
          output_list[[response_name]][["back_transform_fn"]] <- local({
            lam <- lambda_val
            function(x) if (abs(lam) < 1e-6) exp(x) else (lam * x + 1)^(1 / lam)
          })
        } else if (transformation == "bestnormalize") {
          bn_obj <- transformed_var$bestNormalize
          output_list[[response_name]][["back_transform_fn"]] <- local({
            bn <- bn_obj
            function(x) predict(bn, newdata = x, inverse = TRUE)
          })
        }
      } else {
        output_list[[response_name]][["back_transform_fn"]] <- identity
      }
      bt_fn <- output_list[[response_name]][["back_transform_fn"]]
      if (is.null(bt_fn)) bt_fn <- identity

      # Classify predictors: numeric (continuous) vs categorical. The model
      # frame is the source of truth for class, because a term written as
      # factor(x) appears there as a factor column named "factor(x)" even when
      # data_complete[[x]] is still numeric/character. Map each bare predictor
      # name (from all.vars) to its model-frame column, unwrapping a
      # factor()/as.factor()/ordered() wrapper, and read the class from there.
      mf_active <- stats::model.frame(lm_fit_out)
      mf_cols   <- names(mf_active)
      # For predictor v, find the model-frame column that represents it: either
      # the bare name, a simple wrapper like "factor(v)", or any column whose
      # expression contains v as a word (covers factor(round(v)) etc.). Among
      # multiple matches, prefer an exact name, then the shortest expression.
      mf_col_for <- function(v) {
        if (v %in% mf_cols) return(v)
        # Substring match with fixed = TRUE avoids any regex-metacharacter
        # pitfalls from names like "Sepal.Width". This catches wrapper columns
        # such as "factor(v)" or "factor(round(v))". Among multiple matches,
        # prefer the shortest expression (closest to a bare wrapper).
        hits <- mf_cols[vapply(mf_cols, function(cn)
          grepl(v, cn, fixed = TRUE), logical(1))]
        if (length(hits) == 0L) return(NA_character_)
        hits[order(nchar(hits))][1L]
      }
      is_cat_pred <- function(v) {
        cn <- mf_col_for(v)
        if (!is.na(cn)) {
          col <- mf_active[[cn]]
          # A factor() wrapper in the formula always yields a factor column.
          return(is.factor(col) || is.character(col) || is.logical(col))
        }
        # Fallback to the raw data column if not found in the model frame.
        if (v %in% names(data_complete)) {
          col <- data_complete[[v]]
          return(is.factor(col) || is.character(col) || is.logical(col))
        }
        FALSE
      }
      cat_preds <- predictor_names[vapply(predictor_names, is_cat_pred, logical(1))]
      num_preds <- setdiff(predictor_names, cat_preds)
      output_list[[response_name]][["cat_preds"]] <- cat_preds
      output_list[[response_name]][["num_preds"]] <- num_preds

      # ---- Coefficient table ---------------------------------------------
#       if (output_type != "rmd") {
#         cat("
# <div style=\"page-break-after: always;\"></div>
# \\newpage
#         ")
#       }
      if (Response_Transformed) {
        if (transformation == "bestnormalize") {
          cat("## Regression Summary of '", transformed_var$transformation_name,
              "TRANSFORMED' response variable:", response_name, "  \n")
        } else {
          cat("## Regression Summary of 'Box-Cox TRANSFORMED' response variable:", response_name, "  \n")
        }
      } else {
        cat("## Regression Summary of ", response_name, "  \n")
      }
      cat("&nbsp;\n  \n")
      cat(paste("\n**Table** of lm call: ", deparse(current_formula), "  \n"))

      lm_sum   <- summary(lm_fit_out)
      output_list[[response_name]][["summary"]] <- lm_sum
      coef_mat <- lm_sum$coefficients

      # Number of model terms (predictors), excluding the intercept. A
      # single-term model (e.g. y ~ x) reads differently: there are no "other"
      # terms to account for, so the Wald t-test and the Type II F-test are
      # identical (F = t^2) and several multi-predictor captions below would be
      # misleading. `single_term` toggles the wording accordingly.
      model_term_labels <- attr(stats::terms(lm_fit_out), "term.labels")
      n_model_terms     <- length(model_term_labels)
      single_term       <- n_model_terms <= 1L

      if (any(is.na(coef(lm_fit_out)))) {
        cat("**WARNING:** One or more coefficients are NA, indicating perfect collinearity ",
            "(aliased terms). Drop redundant predictors.  \n\n")
      }

      cat("**Coefficient Estimates** (direction and magnitude):  \n\n")
      coef_df <- as.data.frame(coef_mat)
      coef_df <- cbind(Term = rownames(coef_df), coef_df)
      rownames(coef_df) <- NULL
      names(coef_df) <- c("Term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")
      coef_df[["Estimate"]]   <- round(coef_df[["Estimate"]], 4)
      coef_df[["Std. Error"]] <- round(coef_df[["Std. Error"]], 4)
      coef_df[["t value"]]    <- round(coef_df[["t value"]], 3)
      coef_df[["Pr(>|t|)"]]   <- ifelse(coef_df[["Pr(>|t|)"]] < 0.001, "< 0.001",
                                        as.character(round(coef_df[["Pr(>|t|)"]], 4)))
      f_pander(coef_df)
      output_list[[response_name]][["coefficients"]] <- as.data.frame(coef_mat)

      # Overall model fit line
      fstat <- lm_sum$fstatistic
      if (!is.null(fstat)) {
        f_p <- stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
        cat(paste0(
          "\n**Model fit:** $R^2$ = ", round(lm_sum$r.squared, 4),
          ", adjusted $R^2$ = ", round(lm_sum$adj.r.squared, 4),
          ", F(", round(fstat[2]), ", ", round(fstat[3]), ") = ",
          round(fstat[1], 3), ", p ",
          if (f_p < 0.001) "< 0.001" else paste0("= ", round(f_p, 4)),
          ".  \n\n"))
        output_list[[response_name]][["r_squared"]]     <- lm_sum$r.squared
        output_list[[response_name]][["adj_r_squared"]] <- lm_sum$adj.r.squared
        output_list[[response_name]][["f_statistic"]]   <- fstat
        output_list[[response_name]][["model_p_value"]] <- as.numeric(f_p)
      }
      if (single_term) {
        cat("\n*The 'Pr(>|t|)' column tests the coefficient against its reference (Wald t-test). ",
            "With a single predictor this is equivalent to the Type II F-test below ",
            "($F = t^2$), so both report the same significance.*  \n\n")
      } else {
        cat("\n*The 'Pr(>|t|)' column tests each coefficient against its reference (Wald t-test). ",
            "For per-term significance in a multi-predictor model use the **Type II Analysis of ",
            "Variance** table below, which tests each term after accounting for all others.*  \n\n")
      }

      # ---- Coefficient forest plot ---------------------------------------
      if (isTRUE(effect_plots)) {
        coef_fp <- tryCatch({
          cf <- as.data.frame(coef_mat)
          cf$Term <- rownames(cf)
          est_col <- "Estimate"
          se_col  <- intersect(c("Std. Error", "Std.Error"), names(cf))[1]
          p_col   <- grep("^Pr\\(>", names(cf), value = TRUE)[1]
          if (is.na(se_col)) stop("no SE column")
          cf <- cf[cf$Term != "(Intercept)", , drop = FALSE]
          if (nrow(cf) == 0L) stop("only an intercept; nothing to plot")
          tcrit <- stats::qt(1 - alpha / 2, df = lm_fit_out$df.residual)
          fp_df <- data.frame(
            label = cf$Term,
            est   = cf[[est_col]],
            lower = cf[[est_col]] - tcrit * cf[[se_col]],
            upper = cf[[est_col]] + tcrit * cf[[se_col]],
            stringsAsFactors = FALSE
          )
          fp_df$sig <- if (!is.na(p_col))
            ifelse(!is.na(cf[[p_col]]) & cf[[p_col]] < alpha, "significant", "not significant")
          else "not significant"
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
            suppressMessages(ggplot2::ggsave(filename = tmp_fp, plot = coef_fp,
                                             width = 7, height = max(3, 0.45 * n_rows + 1.5),
                                             units = "in", dpi = 200))
            TRUE
          }, error = function(e) FALSE)
          if (isTRUE(ok)) {
            ref_cap <- build_coef_ref_caption(lm_fit_out, fixed_only = FALSE)
            cat("\n### Coefficient forest plot\n")
            cat(paste0("![](", tmp_fp, ")"), "   \n  \n")
            cat("*Each row is a coefficient (the intercept is omitted) with its ",
                100 * (1 - alpha), "% confidence interval. The dashed line marks zero: ",
                "a coefficient at zero has no effect relative to its reference. Points to ",
                "the right increase the response, points to the left decrease it. A CI that ",
                "touches or crosses zero means the term is not distinguishable from its ",
                "reference at \u03b1 = ", alpha, "; a CI clear of zero is a significant effect.*  \n  \n",
                sep = "")
            if (nzchar(ref_cap$reference))  cat("*", ref_cap$reference, "* ", sep = "")
            if (nzchar(ref_cap$continuous)) cat("*", ref_cap$continuous, "* ", sep = "")
            if (length(cat_preds) > 0) {
              cat("*For a factor with k levels these are level-vs-reference contrasts, not all ",
                  "pairwise comparisons; see the post hoc tables below for the full pairwise ",
                  "picture.* ", sep = "")
            }
            if (nzchar(ref_cap$how_to)) cat("*", ref_cap$how_to, "*  \n  \n", sep = "")
          }
        }
      }

      # ---- Type II Analysis of Variance ----------------------------------
#       if (output_type != "rmd") {
#         cat("
# <div style=\"page-break-after: always;\"></div>
# \\newpage
#         ")
#       }
      cat("\n## ", if (single_term) "Analysis of Variance of: " else "Type II Analysis of Variance of: ",
          response_name, "  \n\n", sep = "")
      if (single_term) {
        cat("The table below tests the significance of the predictor term via an F-test. ",
            "With a single predictor this is equivalent to the coefficient t-test above ",
            "($F = t^2$).  \n\n")
      } else {
        cat("The table below tests the marginal significance of **each predictor term**, ",
            "each term tested after accounting for all other terms (Type II). For models with ",
            "interactions, lower-order terms are tested respecting marginality.  \n\n")
      }

      anova_tbl <- tryCatch(
        if (requireNamespace("car", quietly = TRUE)) {
          as.data.frame(car::Anova(lm_fit_out, type = 2))
        } else {
          as.data.frame(stats::drop1(lm_fit_out, test = "F"))
        },
        error = function(e) NULL
      )
      output_list[[response_name]][["anova"]] <- anova_tbl

      if (!is.null(anova_tbl)) {
        anova_disp <- cbind(Term = rownames(anova_tbl), anova_tbl)
        rownames(anova_disp) <- NULL
        p_col_a <- grep("^Pr|^p", names(anova_disp), ignore.case = TRUE, value = TRUE)[1]
        if (!is.na(p_col_a)) {
          anova_disp[[p_col_a]] <- ifelse(
            is.na(anova_disp[[p_col_a]]), "",
            ifelse(anova_disp[[p_col_a]] < alpha,
                   paste0("**", formatC(anova_disp[[p_col_a]], format = "g", digits = 4), "**"),
                   formatC(anova_disp[[p_col_a]], format = "g", digits = 4)))
        }
        f_pander(f_conditional_round(anova_disp, digits = 4, replace_na = FALSE))
        cat("\nBold p-values are significant at \u03b1 = ", alpha, ".  \n\n", sep = "")
      } else {
        cat("*Type II ANOVA table could not be computed.*  \n\n")
      }

      # Determine significance of each predictor term for emmeans / post hoc
      sig_terms <- character(0)
      term_names_all <- character(0)
      if (!is.null(anova_tbl)) {
        term_names_all <- rownames(anova_tbl)
        p_col_a <- grep("^Pr|^p", names(anova_tbl), ignore.case = TRUE, value = TRUE)[1]
        if (!is.na(p_col_a)) {
          p_vals_a <- anova_tbl[[p_col_a]]
          keep     <- term_names_all != "Residuals" & !is.na(p_vals_a)
          sig_terms <- term_names_all[keep][p_vals_a[keep] < alpha]
        }
      }
      term_names_clean <- trimws(setdiff(term_names_all, "Residuals"))
      interaction_terms <- term_names_clean[grepl(":", term_names_clean)]
      sig_interactions  <- intersect(sig_terms, interaction_terms)
      # Categorical-only significant interactions decide cell-means mode.
      sig_cat_interactions <- character(0)
      for (it in sig_interactions) {
        parts <- trimws(strsplit(it, ":", fixed = TRUE)[[1]])
        if (length(parts) >= 2L && all(parts %in% cat_preds))
          sig_cat_interactions <- c(sig_cat_interactions, it)
      }
      output_list[[response_name]][["sig_terms"]]        <- sig_terms
      output_list[[response_name]][["sig_interactions"]] <- sig_cat_interactions

      conf_level <- 1 - alpha
      conf_perc  <- conf_level * 100

      # Overall model significance (for "ns" suppression in CLD)
      overall_p_value <- output_list[[response_name]][["model_p_value"]]
      if (is.null(overall_p_value)) overall_p_value <- 1

      # ---- Observed descriptives table -----------------------------------
      # Group by categorical predictors only (continuous have no groups).
      if (length(cat_preds) > 0) {
        data_summary_table <- f_summary(data_complete, response_name, cat_preds,
                                        show_name = FALSE, digits = NULL)$output_df
        cat(paste("\n## Observed Descriptives Table of: ", deparse(current_formula), "  \n"))
        f_pander(f_conditional_round(data_summary_table, digits = 3, replace_na = FALSE))
        cat("**TIP:** These values represent your actual observed sample characteristics.\n",
            "Use this table for *Methods* sections. For statistical inference use the ",
            "Emmeans table below.\n  \n")
      } else {
        data_summary_table <- NULL
      }

      if (n_before != n_after) {
        cat("\n   \n**WARNING**   \n *Removed ", n_before - n_after, " rows (",
            round((n_before - n_after) / n_before * 100, 1),
            "%) with missing values.*", sep = "")
      }

      # ---- Post hoc (emmeans + CLD) for categorical predictors -----------
      post_hoc_summary_table <- NULL
      cld_text <- ""
      if (length(cat_preds) > 0) {

        if (output_type != "rmd") {
          cat("
<div style=\"page-break-after: always;\"></div>
\\newpage
          ")
        }

        emm_specs <- if (length(sig_cat_interactions) > 0) {
          unique(unlist(strsplit(sig_cat_interactions, ":")))
        } else {
          cat_preds
        }
        emm_is_cells <- length(sig_cat_interactions) > 0

        cat("\n## Model post hoc Analysis (Estimated Marginal Means) of: ", response_name, "\n   \n")
        if (emm_is_cells) {
          cat(paste0("\n**NOTE: Significant categorical interaction(s): ",
                     paste(sig_cat_interactions, collapse = ", "), "**  \n",
                     "The post hoc table shows **cell means** for every combination of ",
                     paste(emm_specs, collapse = " \u00d7 "),
                     ". Letters compare all cells simultaneously. ",
                     "These cell-means letters are the reference grouping: any ",
                     "effect plot that averages over a factor interacting with ",
                     "the one shown omits its own letters and refers back to ",
                     "this table.  \n\n"))
        }
        if (length(num_preds) > 0) {
          cat(paste0("\n*Continuous predictor(s) ", paste(num_preds, collapse = ", "),
                     " are held at their mean for the estimated means below; their effect is ",
                     "reported as a slope in the coefficient table and the partial-effect plot.*  \n\n"))
        }

        emm <- tryCatch(
          emmeans::emmeans(lm_fit_out, specs = emm_specs, level = conf_level),
          error = function(e) NULL)
        output_list[[response_name]][["emm"]] <- emm

        if (!is.null(emm)) {
          mult_cld <- tryCatch(
            cld_emmeans(emm, alpha = alpha, Letters = letters,
                        adjust = adjust, decreasing = TRUE),
            error = function(e) NULL)

          if (!is.null(mult_cld)) {
            summary_table <- as.data.frame(mult_cld)

            # Back-transform if needed
            if (Response_Transformed) {
              bt_cols <- intersect(c("emmean", "lower.CL", "upper.CL"), names(summary_table))
              summary_table[bt_cols] <- lapply(summary_table[bt_cols], bt_fn)
            }
            output_list[[response_name]][["emmeans_table"]] <- summary_table

            rownames(summary_table) <- NULL
            names(summary_table)[names(summary_table) == ".group"] <- "Letter"
            if (overall_p_value >= alpha) summary_table$Letter <- "ns"

            # Merge n from descriptive table where keys match
            common_cols <- intersect(names(summary_table), names(data_summary_table))
            emm_cols    <- c(common_cols, "emmean", "SE", "lower.CL", "upper.CL", "Letter")
            emm_cols    <- intersect(emm_cols, names(summary_table))
            emm_subset  <- summary_table[, emm_cols, drop = FALSE]

            if (length(common_cols) > 0 && !is.null(data_summary_table)) {
              n_df    <- as.data.frame(data_summary_table[common_cols])
              n_df$n  <- data_summary_table$n
              post_hoc_summary_table <- merge(x = emm_subset, y = n_df,
                                              by = common_cols, all.x = TRUE, sort = FALSE)
            } else {
              post_hoc_summary_table <- emm_subset
            }

            if (Response_Transformed) {
              names(post_hoc_summary_table)[names(post_hoc_summary_table) == "emmean"] <- "median (BT)"
              post_hoc_summary_table$SE <- NULL
            } else {
              names(post_hoc_summary_table)[names(post_hoc_summary_table) == "emmean"] <- "emmean.."
            }
            output_list[[response_name]][["post_hoc_summary_table"]] <- post_hoc_summary_table

            # CLD footnote
            raw_msg   <- attr(mult_cld, "mesg")
            bad_lines <- grep("NOTE:|share the same|show them to be", raw_msg)
            if (length(bad_lines) > 0) raw_msg <- raw_msg[-bad_lines]
            raw_msg <- c(raw_msg, paste0(
              "\n**Note:** Groups in the \"Letters\" column sharing the same letter are ",
              "**not** significantly different (\u03b1 = ", alpha, "). Groups with different ",
              "letters are significantly different.\n"))
            cld_text <- paste(raw_msg, collapse = "  \n")
            output_list[[response_name]][["cld_text"]] <- cld_text

            cat(paste0("\n**Post Hoc ", if (emm_is_cells) "Cell Means" else "Marginal Means",
                       " Table** of lm call: ", deparse(current_formula), "  \n"))
            f_pander(f_conditional_round(post_hoc_summary_table, digits = 3, replace_na = FALSE))
            cat(cld_text)
            if (Response_Transformed) {
              cat(paste0("\n**Note on back-transformation:** The column **'median (BT)'** contains ",
                         "back-transformed estimated marginal means, which estimate the **median** on ",
                         "the original scale, not the arithmetic mean. SE is omitted (asymmetric on the ",
                         "original scale); rely on the **", conf_perc,
                         "% Confidence Intervals (lower.CL, upper.CL)**.\n"))
            }
          }
        }
      } else {
        cat("\n*No categorical predictors were included, so no estimated marginal means or post hoc comparisons were produced. See the coefficient table and ",
            if (single_term) "the regression plot" else "the partial-effect plots",
            " for the continuous predictor effect",
            if (single_term) "." else "s.", "*  \n   \n", sep = "")
      }

      # ---- Regression / effect plots -------------------------------------
      if (isTRUE(effect_plots)) {
        y_label_doc <- if (Response_Transformed)
          paste0(response_name, " (back-transformed)") else response_name

        # Shared single-piece caption pieces, split so the letter explanation
        # can be dropped on figures where the letters were withheld (a banner
        # is shown instead) or where letters do not apply at all (partial
        # effect / slope plots that put a continuous predictor on the x-axis):
        #   eff_note_est -> points / estimates / CI (always relevant)
        #   eff_note_let -> how to read the grouping letters (only when shown)
        # The wording stays identical to f_aov()'s eff_note_let so the letter
        # explanation is uniform across functions; eff_note_est is
        # regression-flavoured (covers both means points and fitted lines).
        eff_note_est <- paste0(
          if (isTRUE(Response_Transformed))
            "Points are (jittered) raw data; estimates and fitted lines are back-transformed model medians with "
          else
            "Points are (jittered) raw data; estimates and fitted lines are model estimates with ",
          paste0(100 * (1 - alpha), "% CI"), ".",
          if (single_term) "" else
            " Other predictors are held at their mean (numeric) or reference level (factor)."
        )
        eff_note_let <- paste0(
          " Groups sharing a letter are not significantly different ",
          "(\u03b1 = ", alpha, "); groups with different letters are ",
          "significantly different. Sharing a letter indicates insufficient ",
          "evidence of a difference, not proof that the groups are identical."
        )

        # Assemble the single-piece figure caption in reading order:
        #   1. data / estimates / CI            (eff_note_est, always)
        #   2. ALL letter information together   (eff_note_let + letter_extra),
        #      OR the "Letters omitted" notice   (banner), OR nothing when the
        #      figure carries no letters at all (e.g. partial-effect plots with
        #      a numeric predictor on the x-axis, or an overall non-significant
        #      model). letter_extra carries the cross-panel reading note, which
        #      is itself letter information, so it sits with the other letter
        #      text rather than with the interaction line note.
        #   3. the interaction / line note "rest" (int_note: not-parallel,
        #      panels, dotted line), kept together as one block.
        # Exactly one short lead label is shown in bold: "Letters omitted:"
        # when the letters were withheld, otherwise the interaction lead
        # ("Significant interaction:" / "No significant interaction:"). The
        # whole caption is one italic piece. Mirrors f_aov() / f_glm().
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

        # ---- Per-plot compact letters (suppress across interaction) --------
        # The compact letters on a figure must answer the same question the
        # figure asks. A means plot shows the marginal mean of `display_factors`
        # and AVERAGES over every other categorical predictor. Letters are
        # therefore only honest when BOTH:
        #   (1) they are computed on exactly that displayed grid, so each
        #       letter and its plotted point refer to the same marginal
        #       estimate (this also removes the first-match-wins ambiguity
        #       that arises when letters are copied from the full crossed
        #       cell-means post hoc table by a partial key), AND
        #   (2) the averaging does not cross a significant interaction, i.e.
        #       no significant categorical interaction term has one factor
        #       inside `display_factors` and another outside it. If it does,
        #       the marginal mean collapses a pattern that genuinely changes
        #       with a hidden factor, so grouped letters on the collapsed
        #       view can mislead.
        # When (2) fails the means are still drawn (still informative as a
        # visual) but the letters are withheld and a visible banner explains
        # why and points to the full cell-means post hoc table. This disclosed
        # "degraded mode" is preferred over printing letters that would
        # silently mislead. Only categorical-only interactions are tested
        # here: a numeric x categorical slope plot lives on its own continuous
        # x-axis where compact letters do not apply, so it is handled
        # separately (see make_slope_plot()). Returns a list(safe, letters_df,
        # banner): `letters_df` has the display-factor column(s) plus a
        # `Letters` column when letters are shown, otherwise NULL; `banner`
        # is the explanatory note (or NULL). Mirrors f_aov::aov_plot_letters.
        lm_plot_letters <- function(display_factors) {
          D <- display_factors

          # (2) Does any significant categorical interaction straddle D/not-D?
          crossing <- character(0)
          for (it in sig_cat_interactions) {
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

          # Overall model not significant: the table already shows "ns" and
          # no pairwise letters should be claimed. Draw points without letters.
          if (overall_p_value >= alpha)
            return(list(safe = TRUE, letters_df = NULL, banner = NULL))

          # (1) Recompute emmeans on EXACTLY the displayed grid, then derive
          # the letters from that grid so points and letters match one-to-one.
          letters_df <- tryCatch({
            emm_D <- emmeans::emmeans(lm_fit_out, specs = D,
                                      level = conf_level)
            cld_D <- cld_emmeans(emm_D, alpha = alpha, Letters = letters,
                                 adjust = adjust, decreasing = TRUE)
            if (!all(c(D, ".group") %in% names(cld_D)))
              stop("recomputed CLD is missing expected columns")
            ld <- cld_D[, c(D, ".group"), drop = FALSE]
            names(ld)[names(ld) == ".group"] <- "Letters"
            # Key columns as character so downstream joins are type-stable.
            for (f in D) ld[[f]] <- as.character(ld[[f]])
            ld$Letters <- trimws(ld$Letters)
            ld
          }, error = function(e) {
            warning("f_lm: could not compute per-plot letters for '",
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

        # --- Partial-effect plot per continuous predictor -----------------
        make_partial_plot <- function(num_var) {
          if (!num_var %in% names(mf_active)) return(NULL)
          xv <- data_complete[[num_var]]
          if (!is.numeric(xv)) return(NULL)
          rng <- range(xv, na.rm = TRUE)
          if (!all(is.finite(rng)) || rng[1] == rng[2]) return(NULL)
          grid_n  <- 100L
          at_list <- stats::setNames(list(seq(rng[1], rng[2], length.out = grid_n)), num_var)
          line_df <- tryCatch({
            ip <- emmeans::emmip(lm_fit_out, stats::as.formula(paste("~", num_var)),
                                 at = at_list, CIs = TRUE, plotit = FALSE)
            data.frame(
              xval  = ip[[num_var]],
              yval  = bt_fn(ip[["yvar"]]),
              lower = bt_fn(ip[["LCL"]]),
              upper = bt_fn(ip[["UCL"]]),
              stringsAsFactors = FALSE)
          }, error = function(e) NULL)
          if (is.null(line_df)) return(NULL)

          raw_df <- data.frame(xval = xv, yval = original_response,
                               stringsAsFactors = FALSE)
          raw_df <- raw_df[stats::complete.cases(raw_df), ]
          prim   <- f_pub_palette(1)
          p <- ggplot2::ggplot(line_df, ggplot2::aes(x = .data[["xval"]], y = .data[["yval"]]))
          if (nrow(raw_df) > 0L) {
            p <- p + ggplot2::geom_point(
              data = raw_df, ggplot2::aes(x = .data[["xval"]], y = .data[["yval"]]),
              inherit.aes = FALSE, shape = 1, alpha = 0.5, colour = prim)
          }
          p <- p +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
                                 fill = prim, colour = NA, alpha = 0.18) +
            ggplot2::geom_line(linewidth = 0.9, colour = prim) +
            ggplot2::labs(
              title = paste0(if (single_term) "Regression of " else "Partial effect of ",
                             num_var, " on ", response_name),
              x = num_var, y = y_label_doc) +
            f_theme_pub(base_size = 14)
          # Continuous predictor on the x-axis -> compact letters do not apply.
          # Flag this so make_fig_caption() drops the letter-reading sentence.
          attr(p, "has_letters") <- FALSE
          p
        }

        # --- Means plot per categorical predictor (matches f_aov) ---------
        make_means_plot <- function(var_name) {
          # One estimate per level of var_name, recomputed fresh on this
          # single-factor displayed grid (see lm_plot_letters) so the letters
          # join one-to-one to the plotted marginal means. Back-transforms via
          # bt_fn when the response was transformed, so the plot, the post hoc
          # table, and the raw-data overlay all live on the original scale.
          emm_one <- tryCatch(
            as.data.frame(emmeans::emmeans(lm_fit_out, specs = var_name, level = conf_level)),
            error = function(e) NULL)
          plot_df <- NULL
          if (!is.null(emm_one) && var_name %in% names(emm_one)) {
            ci_lo1 <- intersect(c("lower.CL", "asymp.LCL", "LCL"), names(emm_one))[1]
            ci_hi1 <- intersect(c("upper.CL", "asymp.UCL", "UCL"), names(emm_one))[1]
            if ("emmean" %in% names(emm_one) && !is.na(ci_lo1) && !is.na(ci_hi1)) {
              plot_df <- data.frame(
                x_grp  = as.character(emm_one[[var_name]]),
                centre = bt_fn(emm_one[["emmean"]]),
                lower  = bt_fn(emm_one[[ci_lo1]]),
                upper  = bt_fn(emm_one[[ci_hi1]]),
                stringsAsFactors = FALSE)
            }
          }
          if (is.null(plot_df)) return(NULL)

          # CLD letters computed on EXACTLY this single-factor grid (see
          # lm_plot_letters): they match the plotted marginal means one-to-one
          # and are withheld (with a banner) when averaging over another
          # categorical factor would cross a significant interaction.
          letter_banner   <- NULL
          plot_df$Letters <- NA_character_
          lres <- lm_plot_letters(var_name)
          letter_banner <- lres$banner
          if (!is.null(lres$letters_df) && var_name %in% names(lres$letters_df)) {
            lut <- stats::setNames(lres$letters_df$Letters,
                                   lres$letters_df[[var_name]])
            plot_df$Letters <- lut[as.character(plot_df$x_grp)]
          }
          plot_df$Letters <- trimws(plot_df$Letters)
          plot_df$Letters[plot_df$Letters %in% c("ns", "\u2014", "-", "NA")] <- NA_character_

          raw_df <- data.frame(x_grp = as.character(data_complete[[var_name]]),
                               y_val = original_response, stringsAsFactors = FALSE)
          raw_df <- raw_df[stats::complete.cases(raw_df), ]
          if (!is.numeric(raw_df$y_val) || nrow(raw_df) == 0L) raw_df <- NULL

          if (is.factor(data_complete[[var_name]])) {
            lev <- levels(data_complete[[var_name]])
            plot_df$x_grp <- factor(plot_df$x_grp, levels = lev)
            if (!is.null(raw_df)) raw_df$x_grp <- factor(raw_df$x_grp, levels = lev)
          }
          y_vals <- c(plot_df$upper, plot_df$lower, if (!is.null(raw_df)) raw_df$y_val)
          y_top  <- max(y_vals, na.rm = TRUE); y_bot <- min(y_vals, na.rm = TRUE)
          plot_df$y_letter <- y_top + 0.06 * (y_top - y_bot)

          grp_cols <- f_pub_palette(nrow(plot_df))

          p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data[["x_grp"]], y = .data[["centre"]],
                                                     colour = .data[["x_grp"]]))
          if (!is.null(raw_df)) {
            p <- p + ggplot2::geom_jitter(
              data = raw_df, ggplot2::aes(x = .data[["x_grp"]], y = .data[["y_val"]]),
              inherit.aes = FALSE, width = 0.12, shape = 1, alpha = 0.35)
          }
          p <- p +
            ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
                                   width = 0.12, linewidth = 0.7) +
            ggplot2::geom_point(size = 3) +
            ggplot2::geom_text(ggplot2::aes(y = .data[["y_letter"]], label = .data[["Letters"]]),
                               colour = "black", show.legend = FALSE, na.rm = TRUE) +
            ggplot2::scale_colour_manual(values = grp_cols, guide = "none") +
            ggplot2::labs(title = paste0("Estimated means of ", response_name, " by ", var_name),
                          x = var_name, y = y_label_doc) +
            f_theme_pub(base_size = 14)
          attr(p, "letter_banner") <- letter_banner
          attr(p, "has_letters")   <- any(!is.na(plot_df$Letters))
          p
        }

        # --- Slope plot for numeric x categorical interaction (from f_lmer) -
        make_slope_plot <- function(num_var, fac_var) {
          if (!all(c(num_var, fac_var) %in% names(mf_active))) return(NULL)
          num_vals <- data_complete[[num_var]]
          if (!is.numeric(num_vals)) return(NULL)
          rng <- range(num_vals, na.rm = TRUE)
          if (!all(is.finite(rng)) || rng[1] == rng[2]) return(NULL)
          fac_levels <- if (is.factor(data_complete[[fac_var]]))
            levels(data_complete[[fac_var]]) else sort(unique(as.character(data_complete[[fac_var]])))
          grid_n  <- 100L
          at_list <- stats::setNames(list(seq(rng[1], rng[2], length.out = grid_n)), num_var)
          line_df <- tryCatch({
            ip <- emmeans::emmip(lm_fit_out, stats::as.formula(paste(fac_var, "~", num_var)),
                                 at = at_list, CIs = TRUE, plotit = FALSE)
            data.frame(
              xval  = ip[[num_var]],
              grp   = factor(as.character(ip[[fac_var]]), levels = fac_levels),
              yval  = bt_fn(ip[["yvar"]]),
              lower = bt_fn(ip[["LCL"]]),
              upper = bt_fn(ip[["UCL"]]),
              stringsAsFactors = FALSE)
          }, error = function(e) NULL)
          if (is.null(line_df)) return(NULL)
          raw_df <- data.frame(xval = num_vals,
                               grp  = factor(as.character(data_complete[[fac_var]]), levels = fac_levels),
                               yval = original_response, stringsAsFactors = FALSE)
          raw_df <- raw_df[stats::complete.cases(raw_df), ]
          grp_cols <- f_pub_palette(length(fac_levels))
          p <- ggplot2::ggplot(line_df, ggplot2::aes(x = .data[["xval"]], y = .data[["yval"]],
                                                     colour = .data[["grp"]], fill = .data[["grp"]]))
          if (nrow(raw_df) > 0L) {
            p <- p + ggplot2::geom_point(
              data = raw_df, ggplot2::aes(x = .data[["xval"]], y = .data[["yval"]], colour = .data[["grp"]]),
              inherit.aes = FALSE, shape = 1, alpha = 0.5)
          }
          p <- p +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
                                 colour = NA, alpha = 0.15, show.legend = FALSE) +
            ggplot2::geom_line(linewidth = 0.9) +
            ggplot2::scale_colour_manual(values = grp_cols) +
            ggplot2::scale_fill_manual(values = grp_cols, guide = "none") +
            ggplot2::labs(title = paste0("Estimated slopes of ", response_name, ": ",
                                         num_var, " by ", fac_var),
                          x = num_var, y = y_label_doc, colour = fac_var) +
            f_theme_pub(base_size = 14)
          # Slope plots are only drawn for a significant numeric x categorical
          # interaction, so the line note is fixed to the "significant" wording.
          # The x-axis is continuous (real data space between values) so the
          # f_aov-style "dotted line is merely a visual aid" caveat does not
          # apply here. Compact letters never apply to a numeric x-axis, so
          # has_letters = FALSE drops the letter-reading sentence.
          attr(p, "int_caption") <- paste0(
            "Significant interaction: the ", fac_var,
            " regression lines are not parallel.")
          attr(p, "has_letters") <- FALSE
          p
        }

        # Helper to save + emit a stored ggplot. Two caption modes are
        # supported: when use_fig_caption = TRUE the caption is assembled via
        # make_fig_caption() from the plot's attributes (letter_banner,
        # int_caption, cross_facet_note, has_letters), which guarantees the
        # uniform 3-block order and bold-lead convention. When FALSE the
        # `caption` argument (if any) is printed verbatim in italics, which
        # is used by the obs-vs-fitted plot whose caption does not fit the
        # estimates/letters/interaction template.
        emit_plot <- function(p, key, heading, w = 6, h = 5, caption = NULL,
                              use_fig_caption = FALSE) {
          if (is.null(p)) return(invisible(NULL))
          output_list[[response_name]][[key]] <<- p
          tmp <- tempfile(fileext = ".png")
          ok <- tryCatch({
            suppressMessages(ggplot2::ggsave(filename = tmp, plot = p,
                                             width = w, height = h, units = "in", dpi = 200))
            file.exists(tmp)
          }, error = function(e) FALSE)
          cat(paste0("\n## ", heading, "  \n"))
          if (isTRUE(ok)) cat(paste0("![](", tmp, ")"), "   \n  \n")
          if (isTRUE(use_fig_caption)) {
            cap_full <- make_fig_caption(
              banner       = attr(p, "letter_banner"),
              int_note     = attr(p, "int_caption"),
              letter_extra = attr(p, "cross_facet_note"),
              has_letters  = isTRUE(attr(p, "has_letters")))
            if (nzchar(cap_full))
              cat(paste0("*", gsub("\n", "  \n", cap_full), "*",
                         "   \n  \n"))
          } else if (!is.null(caption) && nzchar(caption)) {
            cat(paste0("*", gsub("\n", "  \n", caption), "*",
                       "   \n  \n"))
          }
        }

        # 1) Partial-effect plots for continuous predictors NOT in a slope plot
        slope_pairs <- list()
        if (length(sig_interactions) > 0) {
          for (it in sig_interactions) {
            parts <- trimws(strsplit(it, ":", fixed = TRUE)[[1]])
            if (length(parts) == 2L) {
              n_in <- parts[parts %in% num_preds]
              c_in <- parts[parts %in% cat_preds]
              if (length(n_in) == 1L && length(c_in) == 1L)
                slope_pairs[[length(slope_pairs) + 1L]] <- c(n_in, c_in)
            }
          }
        }
        num_in_slope <- unique(vapply(slope_pairs, function(z) z[1], character(1)))

        for (nv in setdiff(num_preds, num_in_slope)) {
          emit_plot(make_partial_plot(nv), paste0("effect_plot_", nv),
                    paste0(if (single_term) "Regression Plot of: " else "Partial Effect Plot of: ",
                           response_name, "  (", nv, ")"),
                    w = 6, h = 5, use_fig_caption = TRUE)
        }

        # 2) Slope plots for numeric x categorical interactions
        for (sp in slope_pairs) {
          key <- paste0("interaction_plot_", sp[1], "_", sp[2])
          emit_plot(make_slope_plot(sp[1], sp[2]), key,
                    paste0("Slope Plot of: ", response_name, "  (", sp[1], " \u00d7 ", sp[2], ")"),
                    w = 7, h = 5, use_fig_caption = TRUE)
        }

        # 3) Means plots for categorical predictors. Always drawn: when no
        #    significant categorical interaction collapses the view, the full
        #    grouping letters are shown; when one does, lm_plot_letters
        #    withholds the letters and the caption carries a banner that
        #    points back to the cell-means post hoc table. Mirrors f_aov()'s
        #    "draw the means, withhold letters" rule.
        if (length(cat_preds) > 0) {
          for (vn in cat_preds) {
            emit_plot(make_means_plot(vn), paste0("effect_plot_", vn),
                      paste0("Estimated Means Plot of: ", response_name, "  (", vn, ")"),
                      w = 6, h = 5, use_fig_caption = TRUE)
          }
        }

        # 4) Observed-vs-fitted plot (overall fit)
        of_df <- data.frame(fitted = bt_fn(stats::fitted(lm_fit_out)),
                            observed = original_response, stringsAsFactors = FALSE)
        of_df <- of_df[stats::complete.cases(of_df), ]
        if (nrow(of_df) > 0L) {
          prim <- f_pub_palette(1)
          p_of <- ggplot2::ggplot(of_df, ggplot2::aes(x = .data[["fitted"]], y = .data[["observed"]])) +
            ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
            ggplot2::geom_point(shape = 1, alpha = 0.6, colour = prim) +
            ggplot2::labs(title = paste0("Observed vs fitted: ", response_name),
                          x = "Fitted values", y = paste0("Observed ", response_name)) +
            f_theme_pub(base_size = 14)
          emit_plot(p_of, "obs_fitted_plot",
                    paste0("Observed vs Fitted Plot of: ", response_name),
                    w = 6, h = 5,
                    caption = "Points on the dashed 1:1 line are perfectly predicted; scatter around it reflects residual error.")
        }
      }

      # ---- Contrast forest plots (opt-in) --------------------------------
      if (isTRUE(contrast_plots) && length(cat_preds) > 0 && overall_p_value < alpha &&
          !is.null(output_list[[response_name]][["emm"]])) {
        if (length(sig_cat_interactions) > 0) {
          for (it in sig_cat_interactions) {
            parts <- trimws(strsplit(it, ":", fixed = TRUE)[[1]])
            cf_tbl <- tryCatch(
              as.data.frame(emmeans::emmeans(lm_fit_out, specs = parts, level = conf_level) |>
                              pairs(adjust = adjust) |> confint(level = conf_level)),
              error = function(e) NULL)
            p_icf <- if (!is.null(cf_tbl))
              make_contrast_forest(cf_tbl,
                                   paste0("Cell-pairwise contrasts of ", response_name, "  (", it, ")"),
                                   alpha = alpha, adjust = adjust) else NULL
            if (!is.null(p_icf)) {
              key <- paste0("interaction_contrast_plot_", gsub(":", "_", it))
              output_list[[response_name]][[key]] <- p_icf
              tmp_icf <- tempfile(fileext = ".png")
              n_rows  <- nrow(p_icf$data)
              ok_icf <- tryCatch({
                suppressMessages(ggplot2::ggsave(filename = tmp_icf, plot = p_icf, width = 7.5,
                                                 height = max(3, 0.4 * n_rows + 1.5), units = "in", dpi = 200))
                file.exists(tmp_icf)
              }, error = function(e) FALSE)
              cat(paste0("\n## Contrast Forest Plot of: ", response_name, "  (", it, ")  \n"))
              if (isTRUE(ok_icf)) cat(paste0("![](", tmp_icf, ")"), "   \n  \n")
              icf_cap <- attr(p_icf, "int_caption")
              if (!is.null(icf_cap)) cat(paste0("*", gsub("\n", "  \n", icf_cap), "*", "   \n  \n"))
            }
          }
        } else {
          for (vn in cat_preds) {
            cf_tbl <- tryCatch(
              as.data.frame(emmeans::emmeans(lm_fit_out, specs = vn, level = conf_level) |>
                              pairs(adjust = adjust) |> confint(level = conf_level)),
              error = function(e) NULL)
            p_cf <- if (!is.null(cf_tbl))
              make_contrast_forest(cf_tbl,
                                   paste0("Pairwise contrasts of ", response_name, " by ", vn),
                                   alpha = alpha, adjust = adjust) else NULL
            if (!is.null(p_cf)) {
              key <- paste0("contrast_plot_", vn)
              output_list[[response_name]][[key]] <- p_cf
              tmp_cf <- tempfile(fileext = ".png")
              n_rows <- nrow(p_cf$data)
              ok_cf <- tryCatch({
                suppressMessages(ggplot2::ggsave(filename = tmp_cf, plot = p_cf, width = 7,
                                                 height = max(3, 0.4 * n_rows + 1.5), units = "in", dpi = 200))
                file.exists(tmp_cf)
              }, error = function(e) FALSE)
              cat(paste0("\n## Contrast Forest Plot of: ", response_name, "  (", vn, ")  \n"))
              if (isTRUE(ok_cf)) cat(paste0("![](", tmp_cf, ")"), "   \n  \n")
              cf_cap <- attr(p_cf, "int_caption")
              if (!is.null(cf_cap)) cat(paste0("*", gsub("\n", "  \n", cf_cap), "*", "   \n  \n"))
            }
          }
        }
      }

      i <- i + 1
      if (output_type != "rmd" && i < length(lhs)) {
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage
        ")
      }
    } # end response loop

    if (output == TRUE) return(output_list)
  } # end generate_report

  # Execute analysis ONCE: capture both the output_list and the markdown text.
  cached_markdown <- NULL
  suppressMessages(
    cached_markdown <- utils::capture.output(
      output_list <- generate_report()
    )
  )
  class(output_list) <- "f_lm"
  get_cached_markdown <- function() cached_markdown

  # ---- Build documents -------------------------------------------------
  if (output_type %in% c("word", "pdf")) {
    message(paste0("Saving output in: ", output_path))
    word_pdf_preamble <- function() paste0("
---
title: \"f_lm Analysis Report\"
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
  - \\DeclareUnicodeCharacter{2019}{\\textquoteright}
  - \\DeclareUnicodeCharacter{0160}{\\v{S}}
  - \\DeclareUnicodeCharacter{00E1}{\\'{a}}
  - \\usepackage{titling}
  - \\setlength{\\droptitle}{-2.5cm}
---
")

    knitr::opts_chunk$set(comment = "")
    generated_markdown <- get_cached_markdown()
    rmd_content <- paste(word_pdf_preamble(),
                         paste(generated_markdown, collapse = "\n"),
                         sep = "\n")
    writeLines(rmd_content, temp_output_file)

    tryCatch(
      rmarkdown::render(temp_output_file, output_file = output_path,
                        intermediates_dir = temp_output_dir,
                        knit_root_dir = temp_output_dir, quiet = TRUE,
                        output_format = paste0(output_type, "_document")),
      error = function(e) {
        log_file <- file.path(temp_output_dir,
                              sub("\\.(Rmd|rmd)$", ".log", basename(temp_output_file)))
        if (file.exists(log_file)) {
          log_lines    <- readLines(log_file, warn = FALSE)
          context_idx  <- grep("^!", log_lines)
          context_lines <- unique(unlist(lapply(context_idx, function(i)
            log_lines[i:min(i + 2, length(log_lines))])))
          message("\n--- LaTeX log errors ---\n",
                  paste(context_lines, collapse = "\n"),
                  "\n--- Full log: ", log_file, " ---")
        }
        stop(e)
      }
    )

    if (open_generated_files == TRUE) f_open_file(output_path)
    return(invisible(output_list))

  } else if (output_type == "excel") {
    message(paste0("Saving output in: ", output_path))
    post_hoc_tables <- lapply(output_list, function(obj) {
      tab <- obj$post_hoc_summary_table
      if (is.data.frame(tab)) tab else data.frame(note = "No categorical post-hoc test performed")
    })
    names(post_hoc_tables) <- response_names
    writexl::write_xlsx(post_hoc_tables, path = output_path)
    if (open_generated_files == TRUE) f_open_file(output_path)
    return(invisible(output_list))

  } else if (output_type == "rmd") {
    if (is.null(knitr::opts_knit$get("output.dir"))) {
      knitr::opts_knit$set(output.dir = tempdir())
    }
    clean_rmd_output <- paste(get_cached_markdown(), collapse = "\n")
    output_list[["rmd"]] <- clean_rmd_output
    return(invisible(output_list))

  } else if (output_type == "default") {
    return(output_list)

  } else if (output_type == "console") {
    print(output_list)
    return(invisible(output_list))

  } else {
    warning("Invalid output format specified. No file generated.")
  }

  invisible(suppressWarnings(file.remove(temp_output_file)))
}


# Internal helper: Breusch-Pagan test for heteroscedasticity. Prefers the
# well-tested car::ncvTest() for stability and accuracy (consistent with the
# Type II ANOVA, which also uses car). Falls back VISIBLY to a base-R Koenker
# (studentised) score test when car is not installed: regress the squared
# residuals on the fitted values and test whether that relationship is flat.
# The fallback is numerically identical to lmtest::bptest(studentize = TRUE)
# and its $method string makes clear which path was taken. Returns a list with
# statistic, p.value and method so the caller does not need to branch.
safe_breusch_pagan <- function(lm_fit) {
  tryCatch({
    if (requireNamespace("car", quietly = TRUE)) {
      bp <- car::ncvTest(lm_fit)
      list(statistic = as.numeric(bp$ChiSquare),
           p.value   = as.numeric(bp$p),
           method    = "Breusch-Pagan (car::ncvTest)")
    } else {
      r    <- residuals(lm_fit)
      f    <- stats::fitted(lm_fit)
      n    <- length(r)
      u    <- r^2
      aux  <- stats::lm(u ~ f)
      rss  <- sum((u - mean(u))^2)
      ess  <- sum((stats::fitted(aux) - mean(u))^2)
      stat <- n * (ess / rss)
      list(statistic = as.numeric(stat),
           p.value   = stats::pchisq(stat, df = 1, lower.tail = FALSE),
           method    = "Breusch-Pagan (Koenker score test, base R fallback)")
    }
  }, error = function(e)
    list(statistic = NA_real_, p.value = NA_real_, method = "Breusch-Pagan (failed)"))
}


#' @export
print.f_lm <- function(x, ...) {
  for (category in names(x)) {
    sublist <- x[[category]]
    if (!is.list(sublist)) next

    transformed <- isTRUE(sublist$Response_Transformed)
    cat("\n   \n===========================================================\n")
    if (transformed) {
      cat("   Linear regression of TRANSFORMED response variable:", category, "\n")
    } else {
      cat("   Linear regression of response variable:", category, "\n")
    }
    cat("===========================================================\n")
    cat(paste("\n lm call: ", sublist$lm_call, "\n"))

    cat("\nCoefficients:\n")
    if (!is.null(sublist$summary)) print(sublist$summary$coefficients)

    if (!is.null(sublist$r_squared)) {
      cat("\nModel fit: R-squared =", round(sublist$r_squared, 4),
          " Adjusted R-squared =", round(sublist$adj_r_squared, 4), "\n")
    }

    cat("\n--- Type II Analysis of Variance ---\n")
    if (!is.null(sublist$anova)) print(sublist$anova) else cat("(not available)\n")

    if (!is.null(sublist$post_hoc_summary_table)) {
      cat("\n--- Post hoc Comparisons of:", category, "---\n")
      cat("_________________________________________\n")
      print(sublist$post_hoc_summary_table, row.names = FALSE, quote = FALSE)
      if (!is.null(sublist$cld_text)) cat("\n", sublist$cld_text, "\n")
    }

    if (transformed) {
      message("Note: post hoc 'median (BT)' values are back-transformed estimated marginal means ",
              "(medians on the original scale). Report them as back-transformed medians; rely on the CIs.")
    }
    if (isTRUE(sublist$shapiro_test_residuals$p.value <= sublist$alpha)) {
      warning(call. = FALSE, immediate. = TRUE, "   \nBased on the Shapiro-Wilk Test (p = ",
              round(sublist$shapiro_test_residuals$p.value, 4), " \u2264 ", sublist$alpha,
              ") the residuals are \nNOT normally distributed. Regression results can be misleading.  \n")
    }
    if (isTRUE(sublist$breusch_pagan_test$p.value <= sublist$alpha)) {
      warning(call. = FALSE, immediate. = TRUE, "   \nBased on the Breusch-Pagan Test (",
              round(sublist$breusch_pagan_test$p.value, 4), " \u2264 ", sublist$alpha,
              ") the residuals do \nNOT have constant variance (heteroscedasticity).  \n")
    }
  }
}


#' @export
plot.f_lm <- function(x, ...) {
  .session_state <- save_session_state()
  on.exit(restore_session_state(.session_state), add = TRUE)

  for (category in names(x)) {
    sublist <- x[[category]]
    if (!is.list(sublist)) next

    # Diagnostic residual plots (base graphics) for the active model
    fit <- sublist$lm_fit_active
    if (!is.null(fit)) {
      graphics::par(mfrow = c(1, 2), mar = c(3, 2.8, 4, 0.6),
                    oma = c(0, 0, 0, 0), mgp = c(1.7, .5, 0))
      boxplot(residuals(fit), main = paste("Variable:", category),
              xlab = "Residuals", cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
      f_hist(residuals(fit), xlab = "Residuals",
             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
      plot(fit, 1, main = paste("Variable:", category),
           cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
      f_qqnorm(residuals(fit), cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
      graphics::par(mfrow = c(1, 1))
      graphics::layout(1)
    }

    # Re-print stored ggplot objects so interactive output matches the report
    plot_keys <- grep(paste0("^(coef_forest_plot|effect_plot_|interaction_plot_|",
                             "obs_fitted_plot|contrast_plot_|interaction_contrast_plot_)"),
                      names(sublist), value = TRUE)
    for (k in plot_keys) {
      p_obj <- sublist[[k]]
      if (inherits(p_obj, "ggplot")) print(p_obj)
    }
  }
}
