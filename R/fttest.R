#' Perform multiple t-tests with optional data transformation, inspection and visualization.
#'
#' Performs One-sample, Two-sample (Independent), or Paired t-tests on a given dataset
#' with options for (Box-Cox/BestNormalize) transformations, normality tests, and
#' visualization. Several response parameters can be analysed in sequence
#' (formula interface). Additionally, a vector interface similar to \code{stats::t.test()}
#' is supported.
#'
#' @param x Numeric vector of data values (one-sample or first group for two-sample),
#'   or a formula of the form \code{response ~ group} or \code{response ~ 1}.
#' @param y Optional numeric vector (second group) for two-sample tests if using
#'   the vector interface. Ignored when a formula is supplied.
#' @param formula A formula specifying the model (alternative to using x/y).
#'   \itemize{
#'     \item \strong{Two-sample (Independent/Paired):} \code{response ~ group}
#'           (where group has exactly 2 levels).
#'     \item \strong{One-sample:} \code{response ~ 1} or \code{response ~ NULL}.
#'   }
#'   More response variables can be added using \code{+} (e.g., \code{y1 + y2 ~ group}).
#' @param data A data frame containing the variables when using the formula interface.
#' @param paired Logical. If \code{TRUE}, performs a paired t-test.
#'   \strong{Note:} For the formula interface, data must be sorted so that all
#'   observations of group 1 appear before group 2 (AABB order). For the vector
#'   interface, \code{x} and \code{y} must have the same length.
#' @param var.equal Logical or \code{NULL}. If \code{TRUE}, forces Student's t-test
#'   (equal variances). If \code{FALSE} or \code{NULL} (default), Welch's t-test is
#'   used. Bartlett's and Levene's tests are always reported as diagnostics but do
#'   \strong{not} affect this choice. See Delacre, Lakens & Leys (2017).
#' @param mu Numeric. The true value to test against: the mean (one-sample), the mean
#'   of differences (paired), or the difference in means (two-sample). Default is 0.
#'   For transformed analyses, \code{mu} is forward-transformed for one-sample and
#'   paired tests. For two-sample tests with \code{mu != 0} a warning is issued.
#' @param alternative Character string. \code{"two.sided"} (default),
#'   \code{"greater"}, or \code{"less"}.
#' @param norm_plots Logical. If \code{TRUE}, diagnostic plots are included in the
#'   output. Default is \code{TRUE}.
#' @param transformation Logical or character string. If \code{TRUE} or
#'   \code{"boxcox"}, applies \code{f_boxcox()} when Shapiro-Wilk indicates
#'   non-normality. If \code{"bestnormalize"}, applies \code{f_bestNormalize()}.
#'   If \code{FALSE} or \code{"none"}, no transformation is applied.
#'   \strong{Note:} For paired tests, \code{bestNormalize} (Yeo-Johnson) is always
#'   used on the differences, since Box-Cox requires strictly positive values.
#'   Default is \code{TRUE}.
#' @param force_transformation Character vector. Names of variables to transform
#'   regardless of normality results.
#' @param alpha Numeric. Significance level. Default is \code{0.05}.
#' @param conf.level Numeric. Confidence level. Default is \code{1 - alpha}.
#'   If \code{conf.level} is specified, \code{alpha} is set to \code{1 - conf.level}.
#' @param intro_text Logical. If \code{TRUE}, includes explanation of t-test
#'   assumptions. Default is \code{TRUE}.
#' @param close_generated_files Logical. Closes open Excel/Word files before writing.
#'   Default \code{FALSE}. \strong{Windows only.}
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
#' @param save_as Character. Specific path/filename for output.
#' @param save_in_wdir Logical. Save in working directory. Default \code{FALSE}.
#' @param ... For the formula method: additional arguments forwarded to
#'   the row-filtering step. The arguments \code{subset} and
#'   \code{na.action} are honored: when supplied, they are spliced
#'   (still unevaluated) into a \code{\link[stats]{model.frame}} call
#'   built once before the per-response loop, so the \code{subset}
#'   expression is evaluated in the data's column scope (e.g.
#'   \code{subset = cyl == 6} works). All responses in a
#'   multi-response call (\code{y1 + y2 ~ group}) are then tested on
#'   the identical row set. For the default (vector) method, \code{...}
#'   is currently unused.
#'
#' @return An object of class \code{'f_t_test'}, a named list with one element per
#'   response variable. Each element contains the t-test result, normality test
#'   results, variance diagnostic results, transformation object (if applied),
#'   and back-transformed confidence interval (if applicable).
#'
#' @references
#' Delacre, M., Lakens, D., & Leys, C. (2017). Why psychologists should by default
#' use Welch's t-test instead of Student's t-test.
#' \emph{International Review of Social Psychology}, 30(1), 92-101.
#' \doi{10.5334/irsp.82}
#'
#' @examples
#' \donttest{
#' # 1. Two-sample independent Welch's t-test (default)
#' f_t_test(mpg ~ am, data = mtcars, output_type = "console", norm_plots = FALSE)
#'
#' # 2. Multiple response variables in one call
#' f_t_test(mpg + hp ~ am, data = mtcars, output_type = "console", norm_plots = FALSE)
#'
#' # 3. One-sample t-test: test if mean mpg equals 20
#' f_t_test(mpg ~ 1, data = mtcars, mu = 20,
#'          output_type = "console", norm_plots = FALSE)
#'
#' # 4. Paired t-test (sleep dataset is already in AABB order)
#' f_t_test(extra ~ group, data = sleep, paired = TRUE,
#'          output_type = "console", norm_plots = FALSE)
#'
#' # 5. Vector interface: two-sample independent
#' group_auto   <- mtcars$mpg[mtcars$am == 0]
#' group_manual <- mtcars$mpg[mtcars$am == 1]
#' f_t_test(group_auto, group_manual, output_type = "console", norm_plots = FALSE)
#'
#' # 6. Vector interface: one-sample
#' f_t_test(mtcars$mpg, mu = 20, output_type = "console", norm_plots = FALSE)
#'
#' # 7. Force Student's t-test (equal variances assumed)
#' f_t_test(mpg ~ am, data = mtcars, var.equal = TRUE,
#'          output_type = "console", norm_plots = FALSE)
#'
#' # 8. One-sided test
#' f_t_test(mpg ~ am, data = mtcars, alternative = "greater",
#'          output_type = "console", norm_plots = FALSE)
#'
#' # 9. Custom significance level (alpha = 0.01 is equivalent to conf.level = 0.99)
#' f_t_test(mpg ~ am, data = mtcars, alpha = 0.01,
#'          output_type = "console", norm_plots = FALSE)
#'
#' # 10. Box-Cox transformation with back-transformed CI
#' # The back-transformed CI estimates the MEDIAN, not the arithmetic mean.
#' result <- f_t_test(hp ~ am, data = mtcars, transformation = TRUE,
#'                    output_type = "console", norm_plots = FALSE)
#' result[["hp"]]$ci_backtransformed
#'
#' # 11. One-sample with non-zero mu and back-transformation
#' f_t_test(hp ~ 1, data = mtcars, mu = 100, transformation = TRUE,
#'          output_type = "console", norm_plots = FALSE)
#'
#' # 12. BestNormalize transformation (set seed for reproducibility)
#' set.seed(123)
#' f_t_test(hp ~ am, data = mtcars, transformation = "bestnormalize",
#'          output_type = "console", norm_plots = FALSE)
#'
#' # 13. Force transformation regardless of normality
#' f_t_test(mpg + hp ~ am, data = mtcars, force_transformation = "mpg",
#'          output_type = "console", norm_plots = FALSE)
#'
#' # 14. Suppress transformation (diagnostic mode)
#' f_t_test(hp ~ am, data = mtcars, transformation = FALSE,
#'          output_type = "console", norm_plots = FALSE)
#'
#' # 15. Access return object fields directly
#' result <- f_t_test(mpg + hp ~ am, data = mtcars,
#'                    output_type = "default", norm_plots = FALSE, intro_text = FALSE)
#' result[["mpg"]]$t_test          # standard htest object
#' result[["hp"]]$shapiro_res      # Shapiro-Wilk result
#' result[["hp"]]$homog_p_bartlett # Bartlett p-value (diagnostic only)
#' result[["hp"]]$homog_p_levene   # Levene p-value (diagnostic only)
#' result[["hp"]]$ci_backtransformed # back-transformed CI if transformed
#' }
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#' @export
# =============================================================================
#' @export
# =============================================================================
f_t_test <- function(x, ...) {
  UseMethod("f_t_test")
}
# =============================================================================
#' @export
#' @rdname f_t_test
# =============================================================================
f_t_test.formula <- function(formula,
                             data = NULL,
                             paired = FALSE,
                             var.equal = NULL,
                             conf.level = NULL,
                             mu = 0,
                             alternative = "two.sided",
                             norm_plots = TRUE,
                             transformation = TRUE,
                             force_transformation = NULL,
                             alpha = 0.05,
                             intro_text = TRUE,
                             close_generated_files = FALSE,
                             open_generated_files = interactive(),
                             output_type = "default",
                             save_as = NULL,
                             save_in_wdir = FALSE,
                             ...) {
  # ---------------------------------------------------------------------------
  # 1. Reset initial settings on exit
  # ---------------------------------------------------------------------------
  .session_state <- save_session_state()  # Helper function: helper_session_state
  on.exit(restore_session_state(.session_state), add = TRUE) # Helper function: helper_session_state

  # ---------------------------------------------------------------------------
  # 2. Data & Formula Parsing
  # ---------------------------------------------------------------------------
  if (!is.null(data)) {
    data_name <- deparse(substitute(data))
  } else {
    if (length(formula_extract_df_names(formula)) > 0) {
      data_name <- paste(formula_extract_df_names(formula), collapse = "_")
    } else {
      data_name <- "data"
    }
    data    <- formula_to_dataframe(formula)
    formula <- clean_formula(formula)
  }

  temp_output_dir  <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")
  file.create(temp_output_file)
  try(f_wrap_lines(), silent = TRUE)

  file_extension <- NULL
  output_list    <- list()

  if (!(output_type %in% c("pdf", "word", "excel", "rmd", "console", "default")))
    stop("output_type should be: 'pdf', 'word', 'excel', 'console', 'rmd', 'default'")

  # ---------------------------------------------------------------------------
  # 3. File Path Logic
  # ---------------------------------------------------------------------------
  save_dir <- if (isTRUE(save_in_wdir))
    getwd()
  else
    tempdir()
  output_type_map <- c(
    "pdf" = ".pdf",
    "word" = ".docx",
    "excel" = ".xlsx",
    "rmd" = ".rmd"
  )

  if (!is.null(save_as) || isTRUE(save_in_wdir)) {
    if (!is.null(save_as)) {
      save_as <- gsub("\\\\", "/", save_as)
      file_extension_save_as <- unname(extract_extension(save_as))
      if (file_extension_save_as[1] != FALSE)
        file_extension <- file_extension_save_as
    }

    if (is.null(file_extension) &&
        output_type %in% c("console", "default")) {
      output_path <- get_save_path(
        save_as = save_as,
        default_name = paste(data_name, "ttest_output", sep = "_"),
        default_dir = save_dir,
        file.ext = ".pdf"
      )
      output_type <- "pdf"
    } else if (is.null(file_extension)) {
      output_path <- get_save_path(
        save_as = save_as,
        default_name = paste(data_name, "ttest_output", sep = "_"),
        default_dir = save_dir,
        file.ext = unname(output_type_map[output_type])
      )
    } else {
      output_path <- get_save_path(
        save_as = save_as,
        default_name = paste(data_name, "ttest_output", sep = "_"),
        default_dir = save_dir,
        file.ext = file_extension[1]
      )
      output_type <- file_extension[2]
    }
  } else if (output_type %in% names(output_type_map)) {
    output_path <- get_save_path(
      save_as = save_as,
      default_name = paste(data_name, "ttest_output", sep = "_"),
      default_dir = save_dir,
      file.ext = unname(output_type_map[output_type])
    )
  } else {
    output_path <- NULL
  }

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

  # ---------------------------------------------------------------------------
  # 4. Transformation Argument Parsing
  # ---------------------------------------------------------------------------
  if (is.character(transformation)) {
    trans_input   <- tolower(transformation)
    valid_options <- c("bestnormalize", "boxcox", "true", "false", "none")
    matched_index <- pmatch(trans_input, valid_options)
    if (is.na(matched_index))
      stop("Invalid transformation option!")
    matched_option <- valid_options[matched_index]
    transformation <- switch(
      matched_option,
      "true"  = TRUE,
      "false" = ,
      "none"  = FALSE,
      matched_option
    )
  }

  # ---------------------------------------------------------------------------
  # 5. Parse LHS and RHS of Formula
  # ---------------------------------------------------------------------------
  # Warn if LHS has expressions like log(y) before silently stripping them
  check_lhs_is_names(formula) #use helper_check_lhs.R

  lhs <- all.vars(formula[[2]])


  if (length(lhs) < 1)
    stop("Formula must specify at least one response variable on the LHS.")

  if (length(formula) == 3) {
    rhs_vars   <- all.vars(formula[[3]])
    rhs_string <- deparse(formula[[3]])
    if (length(rhs_vars) == 0 || rhs_string %in% c("1", ".")) {
      is_one_sample  <- TRUE
      predictor_name <- NULL
    } else {
      is_one_sample  <- FALSE
      predictor_name <- rhs_vars[1]
      if (length(rhs_vars) > 1)
        warning("f_t_test only supports one grouping variable. Using the first one.")
    }
  } else {
    is_one_sample  <- TRUE
    predictor_name <- NULL
  }

  for (response in lhs) {
    if (!(response %in% names(data)))
      stop(paste("Response", response, "not found."))
    if (!is.numeric(data[[response]]))
      stop(paste("Response variable", response, "must be numeric."))
  }

  if (!is_one_sample) {
    if (!(predictor_name %in% names(data)))
      stop(paste("Predictor", predictor_name, "not found."))
    data[[predictor_name]] <- as.factor(data[[predictor_name]])
    if (nlevels(data[[predictor_name]]) != 2)
      stop("Grouping variable must have exactly 2 levels for a t-test.")
  }

  ########## Build master analysis frame (one row set, all responses) #######
  # Hoists row filtering out of the per-response loop so every response
  # in `mpg + hp ~ am` is tested on the identical row set, and bakes in
  # any subset / na.action passed via `...`.
  #
  # Strategy: pre-evaluate subset eagerly against data first, falling
  # back to the user's calling environment. Then apply the filter
  # manually and pass plain values to stats::model.frame via do.call.
  # This avoids the fragile match.call/substitute dance where spliced
  # expressions inside a constructed call can end up containing `..N`
  # dots-index symbols that crash model.frame with "the ... list
  # contains fewer than 3 elements".
  master_vars <- if (is_one_sample) lhs else c(lhs, predictor_name)
  master_formula <- as.formula(
    paste("~", paste(master_vars, collapse = " + "))
  )

  mc          <- match.call(expand.dots = FALSE)
  dots_exprs  <- as.list(mc[["..."]])
  caller_env  <- parent.frame()

  # Resolve subset expression against data columns + caller env
  subset_vec <- NULL
  if (!is.null(dots_exprs$subset)) {
    subset_vec <- eval(dots_exprs$subset, envir = data, enclos = caller_env)
    if (is.logical(subset_vec)) subset_vec[is.na(subset_vec)] <- FALSE
  }
  n_before_master <- nrow(data)            # before any filtering
  if (!is.null(subset_vec)) {
    data <- data[subset_vec, , drop = FALSE]
  }

  # Resolve na.action (a function, defaults to na.omit)
  na_action_fn <- stats::na.omit
  if (!is.null(dots_exprs$na.action)) {
    na_action_fn <- eval(dots_exprs$na.action, envir = caller_env)
  }

  mf_args <- list(
    formula            = master_formula,
    data               = data,
    drop.unused.levels = TRUE,
    na.action          = na_action_fn
  )

  data <- do.call(stats::model.frame, mf_args)
  n_after_master  <- nrow(data)            # after subset + NA drop
  ###########################################################################

  # ---------------------------------------------------------------------------
  # 6. Helper Functions
  # ---------------------------------------------------------------------------
  back_transform <- function(values, trans_obj, is_boxcox) {
    tryCatch({
      if (is_boxcox)
        predict(trans_obj, newdata = values, inverse = TRUE)
      else
        predict(trans_obj$bestNormalize,
                newdata = values,
                inverse = TRUE)
    }, error = function(e) {
      warning("Back-transformation failed. CI reported on transformed scale only.")
      NULL
    })
  }

  forward_transform <- function(value, trans_obj, is_boxcox) {
    tryCatch({
      if (is_boxcox)
        predict(trans_obj, newdata = value)
      else
        predict(trans_obj$bestNormalize,
                newdata = value,
                inverse = FALSE)
    }, error = function(e) {
      warning("Forward transformation of mu failed. Original mu used on transformed scale.")
      NULL
    })
  }

  generate_ci_label <- function(is_one_sample, paired, console = FALSE) {
    if (console) {
      if (is_one_sample)
        return(" for the population mean (\u03bc) ")
      if (paired)
        return(" for population mean of differences (\u03bc_diff) ")
      return(" for the difference in population means (\u03bc1 - \u03bc2) ")
    } else {
      if (is_one_sample)
        return(" for the population mean ($\\mu$)")
      if (paired)
        return(" of population mean of differences ($\\mu_{diff}$) ")
      return(" for the difference in population means ($\\mu_1 - \\mu_2$) ")
    }
  }

  generate_hypotheses <- function(name_a,
                                  name_b = NULL,
                                  paired = FALSE,
                                  mu = 0,
                                  alternative = "two.sided",
                                  console = FALSE) {
    param_desc <- if (is.null(name_b))
      sprintf("mean of %s", name_a)
    else if (paired)
      sprintf("mean difference (%s - %s)", name_a, name_b)
    else
      sprintf("difference in means (%s - %s)", name_a, name_b)

    ops <- switch(
      alternative,
      "two.sided" = list(h0 = "is equal to", ha = "is not equal to"),
      "greater"   = list(h0 = "is less than or equal to", ha = "is greater than"),
      "less"      = list(h0 = "is greater than or equal to", ha = "is less than")
    )

    null_text <- sprintf("True %s %s %s", param_desc, ops$h0, mu)
    alt_text  <- sprintf("True %s %s %s", param_desc, ops$ha, mu)

    if (console)
      paste0("\n  H0: ",
             null_text,
             "\n  H1: ",
             alt_text)
    else
      paste0("\n  + $H_{0}$: ", null_text, "\n  + $H_{1}$: ", alt_text)
  }

  # ===========================================================================
  # 7. Report Generation Function
  # ===========================================================================
  generate_report <- function(output = TRUE) {
    if (isTRUE(intro_text)) {
      cat(
        "
# Assumptions of t-tests

##  1. Continuous Scale of Measurement
- The dependent variable must be continuous (interval or ratio scale, e.g., weight, length).
- For discrete or ordinal data (e.g., counts, Likert scales), use non-parametric tests like Wilcoxon or Mann-Whitney.

##  2. Independence
- Observations must not influence each other. This is determined by study design, not statistics.
  + *Two-sample*: Subjects should be distinct both *within* and *between* groups.
  + *Paired*: Pairs should be distinct (though observations *within* a pair are correlated by design).
- **Violation invalidates** the test; the solution is a change in experimental design.

##  3. Normality
- Data must follow a normal (Gaussian) distribution. T-tests are robust to minor deviations if n > 30 (Central Limit Theorem).
  + *One/Two-sample:* The data within each group (or residuals) should be normal.
  + *Paired:* The **differences** between pairs (not raw values) should be normal.
- Normality is assessed *visually* (Q-Q plots, Histograms) and *formally* (Shapiro-Wilk test).
  + **Decision to transform is based on Shapiro-Wilk only** -- the most powerful omnibus normality test.
  + Anderson-Darling is shown as additional diagnostic information only.

##  4. Homogeneity of Variance (Homoscedasticity) -- Two-sample t-tests only.
- **Welch's t-test is the default** (`var.equal = FALSE`). Current best practice ([Delacre, Lakens & Leys, 2017](https://doi.org/10.5334/irsp.82)) shows Welch's performs as well as Student's when variances are equal, and substantially better when they are not.
- **Pre-testing is avoided.** Using Bartlett's or Levene's test to decide between t-tests inflates Type I error, and variance tests have low power with small samples.
- **Diagnostic only:** Bartlett's and Levene's (Brown-Forsythe) tests are reported to help you understand your data, but do not affect the choice of t-test.
- **Override:** To force Student's t-test (equal variances assumed), set `var.equal = TRUE`.

##  5. No Significant Outliers
- T-tests use the mean, making them sensitive to extreme values. Check with boxplots.

##  6. Hypothesis Direction (Design Choice)
- *Two-sided (Default):* Tests for any difference ($A \\neq B$).
- *One-sided:* Tests a specific direction ($A > B$). Only use if an opposite effect is theoretically impossible or irrelevant."
      )
      if (output_type %in% c("pdf", "word"))
        cat("\n<div style=\"page-break-after: always;\"></div>\n\\newpage")
    }

    i <- 0
    original_var.equal <- var.equal
    org.var.equal.null  <- is.null(var.equal)

    if (is.null(conf.level)) {
      conf.level <- 1 - alpha
    } else {
      alpha <- 1 - conf.level
    }

    # Fires regardless of intro_text so the user always sees it in this situation.
    if (length(lhs) > 1) {
      k          <- length(lhs)
      fwer_pct   <- round((1 - (1 - alpha)^k) * 100, 1)
      bonf_alpha <- round(alpha / k, 4)
      cat(paste0(
        "\n\n***\n\n",
        "**[!] NOTE: Multiple Testing Across ", k, " Response Variables**  \n\n",
        "This report runs ", k, " independent t-tests on the same dataset. ",
        "Each individual test is valid on its own, but running multiple tests ",
        "simultaneously inflates the risk of obtaining at least one spurious ",
        "significant result by chance.  \n\n",
        "At \u03b1 = ", alpha, " per test, the probability of at least one false positive ",
        "across all ", k, " responses is approximately ",
        "**", fwer_pct, "%** ($1-(1-", alpha, ")^{", k, "}$, assuming independence). ",
        "This risk is highest in exploratory studies; it is less of a concern when ",
        "each response variable has a clear a priori hypothesis.  \n\n",
        "**Possible remedies:**  \n",
        "\n-  **Bonferroni** (conservative): re-run with `alpha = ", bonf_alpha,
        "` (\u03b1 / ", k, ") to control the family-wise error rate.",
        "\n-  **False Discovery Rate (FDR)**: collect the ", k, " p-values and apply ",
        "`p.adjust(p_values, method = \"fdr\")` after the fact.",
        "\n-  **MANOVA**: if the response variables are correlated, consider `manova()` ",
        "as a single omnibus test before interpreting individual t-tests.",
        "\n-  **Pre-registration**: if each response was a pre-specified primary outcome, ",
        "correction may not be required; document this decision explicitly.",
        "\n\n***\n\n"
      ))
    }

    # Pre-compute once -- avoids floating point printing (e.g. 94.9999...)
    conf_pct <- round(conf.level * 100, 1)

    # =========================================================================
    # LOOP THROUGH RESPONSE VARIABLES
    # =========================================================================
    for (response_name in lhs) {
      var.equal <- if (org.var.equal.null)
        FALSE
      else
        original_var.equal

      Response_Transformed <- FALSE
      mu_transformed       <- NULL
      ci_backtransformed   <- NULL
      is_boxcox_trans      <- FALSE

      output_list[[response_name]][["transformation_option"]] <- transformation

      if (is_one_sample) {
        current_formula <- as.formula(paste0(response_name, " ~ 1"))
        cols_needed     <- response_name
      } else {
        current_formula <- as.formula(paste0(response_name, " ~ ", predictor_name))
        cols_needed     <- c(response_name, predictor_name)
      }

      # Row filtering already happened once at the master level (above
      # the loop), with subset / na.action from `...` baked in. Every
      # response in a multi-response call therefore sees the identical
      # row set. We just take the relevant columns here. data_complete
      # is a per-iteration copy because the transformation step below
      # mutates its response column in place.
      n_before      <- n_before_master
      data_complete <- data[, cols_needed, drop = FALSE]
      n_after       <- n_after_master

      cat("  \n\n# Analysis of: ", response_name, "  \n")
      cat("\n## Normality and Assumptions for: ", response_name, "  \n")

      y_vals <- data_complete[[response_name]]

      if (is_one_sample) {
        group1 <- y_vals
        group2 <- NULL
        levs   <- NULL
        normality_values<- y_vals
      } else {
        groups <- data_complete[[predictor_name]]
        levs   <- levels(groups)
        group1 <- y_vals[groups == levs[1]]
        group2 <- y_vals[groups == levs[2]]

        if (paired) {
          if (length(group1) != length(group2)) {
            stop(
              paste0(
                "Paired t-test requires equal group sizes. '",
                response_name,
                "' has unequal groups after removing NAs. Please clean your pairs."
              )
            )
          }

          group_runs <- rle(as.character(data_complete[[predictor_name]]))
          if (length(group_runs$lengths) != 2) {
            warning(
              "Paired t-test expects data sorted so all observations of '",
              levs[1],
              "' come before '",
              levs[2],
              "' (AABB order). ",
              "Your data appears interleaved. Please verify your data is sorted correctly."
            )
          }

          normality_values<- group1 - group2

        } else {
          lm_temp            <- lm(current_formula, data = data_complete)
          normality_values<- residuals(lm_temp)
        }
      }

      if (!is_one_sample && !paired) {
        bart_res  <- stats::bartlett.test(list(group1, group2))
        homog_p   <- bart_res$p.value

        lev_res   <- rstatix::levene_test(data_complete, current_formula, center = median)
        levene_p  <- lev_res$p[1]

        cat(
          paste0(
            "\n  **Variance tests**",
            "\n- *These results are informational only as Welch's t-test is used regardless.*",
            "\n- Bartlett's test: p = ",
            round(homog_p, 4),
            ifelse(
              homog_p > alpha,
              " (no evidence of unequal variance)",
              " (suggests unequal variance)"
            ),
            "\n- Levene's / Brown-Forsythe: p = ",
            round(levene_p, 4),
            ifelse(
              levene_p > alpha,
              " (no evidence of unequal variance)",
              " (suggests unequal variance)"
            ),
            "\n&nbsp;  \n  ",
            "\n  - These two tests can disagree near the alpha threshold.",
            "\n&nbsp;  \n  "
          )
        )

        output_list[[response_name]][["homog_p_bartlett"]] <- homog_p
        output_list[[response_name]][["homog_p_levene"]]   <- levene_p
      } else {
        output_list[[response_name]][["homog_p_bartlett"]] <- NA
        output_list[[response_name]][["homog_p_levene"]]   <- NA
      }

      if (is_one_sample) {
        test_res <- t.test(
          group1,
          mu = mu,
          alternative = alternative,
          conf.level = conf.level
        )
        call_str <- paste0("t.test(", response_name, ", mu = ", mu, ")")
      } else {
        test_res <- t.test(
          group1,
          group2,
          paired = paired,
          var.equal = var.equal,
          alternative = alternative,
          mu = mu,
          conf.level = conf.level
        )
        type_str <- if (paired)
          "Paired"
        else if (var.equal)
          "Student's"
        else
          "Welch's"
        call_str <- paste0(type_str,
                           " t.test(",
                           response_name,
                           " by ",
                           predictor_name,
                           ")")
      }

      output_list[[response_name]][["t_test"]] <- test_res
      output_list[[response_name]][["t_call"]] <- call_str

      # Use safe_shapiro() so edge cases (n < 3, n > 5000) return a
      # shaped htest with NA p-value instead of erroring. Downstream
      # code must handle NA in $p.value and $statistic.
      shapiro_res <- safe_shapiro(normality_values)
      adt_res     <- nortest::ad.test(normality_values)
      data_label  <- if (paired)
        "Differences"
      else if (is_one_sample)
        "Data"
      else
        "Residuals"

      if (is.na(shapiro_res$p.value)) {
        cat(
          paste0(
            "\n- **Shapiro-Wilk** skipped (", shapiro_res$method, "): ",
            data_label, " normality cannot be tested at this sample ",
            "size, the transformation decision falls back to the ",
            "Anderson-Darling result below.",
            "\n- Anderson-Darling (A = ",
            round(adt_res$statistic, 4),
            ", p = ",
            round(adt_res$p.value, 4),
            "): ",
            data_label,
            ifelse(adt_res$p.value > alpha, " are Normal", " are NOT Normal"),
            ", *drives transformation decision (Shapiro skipped)* \n"
          )
        )
      } else {
        cat(
          paste0(
            "\n- **Shapiro-Wilk** (W = ",
            round(shapiro_res$statistic, 4),
            ", p = ",
            round(shapiro_res$p.value, 4),
            "): ",
            data_label,
            ifelse(
              shapiro_res$p.value > alpha,
              paste0(" **are Normal** (p > ", alpha, ")"),
              paste0(" are **NOT Normal** (p \u2264 ", alpha, ")")
            ),
            ", *drives transformation decision*",
            "\n- Anderson-Darling (A = ",
            round(adt_res$statistic, 4),
            ", p = ",
            round(adt_res$p.value, 4),
            "): ",
            data_label,
            ifelse(adt_res$p.value > alpha, " are Normal", " are NOT Normal, "),
            "*diagnostic only* \n"
          )
        )
      }

      output_list[[response_name]][["shapiro_res"]] <- shapiro_res
      output_list[[response_name]][["adt_res"]]     <- adt_res
      output_list[[response_name]][["alpha"]]       <- alpha

      temp_file <- tempfile(fileext = ".png")
      png(
        temp_file,
        width = 7.8,
        height = 7.8,
        units = "in",
        res = 300
      )
      par(mfrow = c(2, 2), mar = c(4, 4, 4, 1))

      boxplot(
        normality_values,
        main = paste0("Boxplot (", data_label, ")"),
        col = "lightblue"
      )
      try(f_hist(normality_values, xlab = data_label), silent = TRUE)
      try(f_qqnorm(normality_values), silent = TRUE)

      if (!is_one_sample && !paired) {
        boxplot(
          current_formula,
          data = data_complete,
          main = "Raw Data by Group",
          col = "lightgrey"
        )
      } else {
        plot(
          normality_values,
          main = paste0(data_label, ", Index Plot"),
          ylab = data_label
        )
      }
      dev.off()

      if (isTRUE(norm_plots)) {
        cat("\n     \n&nbsp;   \nCheck the plots below to assess assumptions.  \n")
        cat(paste0("![](", temp_file, ")"), "  \n\n")
        cat("&nbsp;\n  \n")
      }
      output_list[[response_name]][["normality_plots"]] <- temp_file

      if (output_type %in% c("pdf", "word"))
        cat("\n<div style=\"page-break-after: always;\"></div>\n\\newpage")

      # Decide whether normality is violated. Prefer Shapiro-Wilk when
      # available (most powerful for n <= 5000); fall back to
      # Anderson-Darling when Shapiro was skipped (n outside 3..5000)
      # so the decision is still made on test evidence rather than
      # silently defaulting to FALSE. If both are unavailable, leave
      # the trigger FALSE and let the user inspect the qq-plot.
      normality_violated <- if (!is.na(shapiro_res$p.value)) {
        shapiro_res$p.value < alpha
      } else if (!is.na(adt_res$p.value)) {
        adt_res$p.value < alpha
      } else {
        FALSE
      }

      needs_trans <- normality_violated ||
        response_name %in% force_transformation

      if (needs_trans) {
        if (isFALSE(transformation)) {
          cat(
            paste0(
              "  \n\n**WARNING** Normality assumption violated for '",
              response_name,
              "'. Consider enabling `transformation = TRUE`.  \n\n"
            )
          )
        } else {
          if (paired && !is_one_sample) {
            values_to_transform <- group1 - group2
            effective_trans     <- "bestnormalize"
            trans_subject_label <- paste0("differences (", levs[1], " - ", levs[2], ")")
            if (!identical(transformation, "bestnormalize")) {
              cat(
                "\n*Note: For paired tests, **bestNormalize** (Yeo-Johnson) is applied to the differences, as Box-Cox requires strictly positive values.* \n"
              )
            }
          } else {
            values_to_transform <- data_complete[[response_name]]
            effective_trans     <- transformation
            trans_subject_label <- response_name
          }

          if (identical(effective_trans, "bestnormalize")) {
            cat("\n  \n## BestNormalize transformation of: ",
                trans_subject_label,
                "  \n  \n")
            transformed_var <- f_bestNormalize(
              values_to_transform,
              data_name   = trans_subject_label,
              output_type = "rmd",
              plots       = isTRUE(norm_plots),
              alpha       = alpha
            )
            cat(transformed_var$rmd)
            new_vals        <- transformed_var$transformed_data
            trans_name      <- transformed_var$transformation_name
            is_boxcox_trans <- FALSE
          } else {
            cat("\n  \n## Box-Cox transformation of: ",
                trans_subject_label,
                "  \n  \n")
            capture.output(
              transformed_var <- f_boxcox(
                values_to_transform,
                output_type = "rmd",
                alpha = alpha
              )
            )
            cat(transformed_var$rmd)
            new_vals        <- transformed_var$transformed_data
            trans_name      <- "Box-Cox"
            is_boxcox_trans <- TRUE
          }

          output_list[[response_name]][["trans_object"]] <- transformed_var
          output_list[[response_name]][["is_boxcox"]]    <- is_boxcox_trans

          mu_for_test <- mu
          if (mu != 0) {
            if (!is_one_sample && !paired) {
              cat(
                paste0(
                  "\n**Note:** `mu = ",
                  mu,
                  "` is a difference of means and cannot be meaningfully forward-transformed. The original `mu = ",
                  mu,
                  "` is used on the transformed scale as an approximation. Interpret with caution.  \n"
                )
              )
            } else {
              mu_t <- forward_transform(mu, transformed_var, is_boxcox_trans)
              if (!is.null(mu_t)) {
                mu_for_test    <- mu_t
                mu_transformed <- mu_t
                cat(
                  paste0(
                    "\n- *mu forward-transformed: ",
                    mu,
                    " \u2192 ",
                    round(mu_t, 4),
                    " (",
                    trans_name,
                    " scale)* \n"
                  )
                )
              }
            }
          }

          if ((paired && !is_one_sample) || is_one_sample) {
            group1_t   <- new_vals
            group2_t   <- NULL
            res_t      <- new_vals
            test_res_t <- t.test(new_vals, mu = mu_for_test,
                                 alternative = alternative, conf.level = conf.level)
          } else {
            data_complete[[response_name]] <- new_vals
            group1_t   <- new_vals[groups == levs[1]]
            group2_t   <- new_vals[groups == levs[2]]
            lm_t       <- lm(current_formula, data = data_complete)
            res_t      <- residuals(lm_t)

            bart_t <- stats::bartlett.test(list(group1_t, group2_t))
            lev_t  <- rstatix::levene_test(data_complete, current_formula, center = median)
            cat(
              paste0(
                "\n- Variance diagnostics on transformed data (diagnostic only):",
                "\n- Bartlett's: p = ",
                round(bart_t$p.value, 4),
                "\n- Levene's / Brown-Forsythe: p = ",
                round(lev_t$p, 4),
                "  \n",
                "\n- These two tests can disagree near the alpha threshold."
              )
            )

            test_res_t <- t.test(
              group1_t,
              group2_t,
              paired = FALSE,
              var.equal = var.equal,
              alternative = alternative,
              mu = mu_for_test,
              conf.level = conf.level
            )
          }

          ci_bt <- back_transform(test_res_t$conf.int,
                                  transformed_var,
                                  is_boxcox_trans)
          if (!is.null(ci_bt))
            ci_backtransformed <- ci_bt

          shapiro_res_t <- safe_shapiro(res_t)
          if (is.na(shapiro_res_t$p.value)) {
            cat(
              paste0(
                "\n## Assumptions of **TRANSFORMED** Data\n\n",
                "- Shapiro-Wilk (Transformed): skipped (",
                shapiro_res_t$method,
                "). Inspect the plots below to assess normality of ",
                "the transformed data.  \n"
              )
            )
          } else {
            cat(
              paste0(
                "\n## Assumptions of **TRANSFORMED** Data\n\n",
                "- Shapiro-Wilk (Transformed): p = ",
                round(shapiro_res_t$p.value, 4),
                " ",
                ifelse(
                  shapiro_res_t$p.value > alpha,
                  "(OK, normality achieved)",
                  "(**Violation, consider a non-parametric test**)"
                ),
                "  \n"
              )
            )
          }

          temp_file_t <- tempfile(fileext = ".png")
          png(
            temp_file_t,
            width = 7.8,
            height = 7.8,
            units = "in",
            res = 300
          )
          par(mfrow = c(2, 2), mar = c(4, 4, 4, 1))
          boxplot(res_t, main = "Boxplot (Transformed)", col = "lightgreen")
          try(f_hist(res_t, xlab = "Transformed"), silent = TRUE)
          try(f_qqnorm(res_t), silent = TRUE)
          if (!is_one_sample && !paired) {
            boxplot(
              current_formula,
              data = data_complete,
              main = "Transformed Data by Group",
              col = "lightgrey"
            )
          } else {
            plot(res_t, main = "Transformed, Index Plot", ylab = "Transformed")
          }
          dev.off()

          if (isTRUE(norm_plots)) {
            cat("\n   \nAssumption plots after transformation:  \n")
            cat(paste0("![](", temp_file_t, ")"), "  \n\n")
          }

          output_list[[response_name]][["t_test_transformed"]]          <- test_res_t
          output_list[[response_name]][["Response_Transformed"]]        <- TRUE
          output_list[[response_name]][["trans_name"]]                  <- trans_name
          output_list[[response_name]][["shapiro_res_transformed"]]     <- shapiro_res_t
          output_list[[response_name]][["transformed_normality_plots"]] <- temp_file_t
          output_list[[response_name]][["mu_transformed"]]              <- mu_transformed
          output_list[[response_name]][["ci_backtransformed"]]          <- ci_backtransformed

          t_test_final         <- test_res_t
          Response_Transformed <- TRUE

          if (output_type %in% c("pdf", "word"))
            cat("\n<div style=\"page-break-after: always;\"></div>\n\\newpage")
        }
      }

      if (!Response_Transformed) {
        t_test_final <- test_res
        output_list[[response_name]][["Response_Transformed"]] <- FALSE
        output_list[[response_name]][["mu_transformed"]]       <- NULL
        output_list[[response_name]][["ci_backtransformed"]]   <- NULL
      }

      # -----------------------------------------------------------------------
      # G. HELPER FUNCTIONS FOR OUTPUT TEXT
      # -----------------------------------------------------------------------

      # Helper: sample statistics text
      generate_sample_stats_txt <- function(is_one_sample, paired, console = FALSE) {

        scale_note <- if (Response_Transformed) {
          if (console) " (transformed scale)" else " *(transformed scale)*"
        } else ""

        if (is_one_sample) {
          m_est <- round(t_test_final$estimate, 3)

          txt <- paste0(
            if (console) "\n  Mean " else "\n-  **Mean ",
            response_name,
            if (console) ": " else ":** ",
            m_est,
            scale_note,
            if (!console && Response_Transformed) "" else ""
          )

          if (Response_Transformed && !is.null(ci_backtransformed)) {
            m_est_bt <- round(back_transform(t_test_final$estimate,
                                             transformed_var,
                                             is_boxcox_trans), 3)
            med_raw  <- round(median(y_vals, na.rm = TRUE), 3)
            txt <- paste0(
              txt,
              if (console)
                paste0("\n  --- back-transformed (original scale) ---",
                       "\n  Back-transformed mean: ", m_est_bt,
                       "\n  Sample median (raw data): ", med_raw)
              else
                paste0("\n\n-  **Back-transformed mean:** ", m_est_bt,
                       " *(original scale)*",
                       "\n-  **Sample median (raw data):** ", med_raw)
            )
          }

        } else if (paired) {
          m_est <- round(t_test_final$estimate, 3)

          txt <- paste0(
            if (console)
              paste0("\n  Mean of differences", scale_note, ": ", m_est)
            else
              paste0("\n-  **Mean of differences", scale_note, ":** ", m_est)
          )

          if (Response_Transformed && !is.null(ci_backtransformed)) {
            m_est_bt <- round(back_transform(t_test_final$estimate,
                                             transformed_var,
                                             is_boxcox_trans), 3)
            med_raw  <- round(median(group1 - group2, na.rm = TRUE), 3)
            txt <- paste0(
              txt,
              if (console)
                paste0("\n  --- back-transformed (original scale) ---",
                       "\n  Back-transformed mean of differences: ", m_est_bt,
                       "\n  Median of raw differences: ", med_raw)
              else
                paste0("\n\n-  **Back-transformed mean of differences:** ", m_est_bt,
                       " *(original scale)*",
                       "\n-  **Median of raw differences:** ", med_raw)
            )
          }

        } else {
          # Two-sample independent
          vec_a <- if (Response_Transformed) group1_t else group1
          vec_b <- if (Response_Transformed) group2_t else group2
          m1    <- mean(vec_a, na.rm = TRUE)
          m2    <- mean(vec_b, na.rm = TRUE)

          txt <- paste0(
            if (console) {
              paste0("\n  Mean ", levs[1], scale_note, ": ", round(m1, 3),
                     "\n  Mean ", levs[2], scale_note, ": ", round(m2, 3),
                     "\n  Difference (", levs[1], " - ", levs[2], "): ",
                     round(m1 - m2, 3))
            } else {
              paste0("\n-  **Mean ", levs[1], scale_note, ":** ", round(m1, 3),
                     "\n-  **Mean ", levs[2], scale_note, ":** ", round(m2, 3),
                     "\n-  **Difference (", levs[1], " - ", levs[2], "):** ",
                     round(m1 - m2, 3))
            }
          )

          if (Response_Transformed && !is.null(ci_backtransformed)) {
            m1_bt   <- round(back_transform(m1, transformed_var, is_boxcox_trans), 3)
            m2_bt   <- round(back_transform(m2, transformed_var, is_boxcox_trans), 3)
            med1_raw <- round(median(group1, na.rm = TRUE), 3)
            med2_raw <- round(median(group2, na.rm = TRUE), 3)

            txt <- paste0(
              txt,
              if (console)
                paste0("\n  --- back-transformed (original scale) ---",
                       "\n  Back-transformed mean ", levs[1], ": ", m1_bt,
                       "\n  Back-transformed mean ", levs[2], ": ", m2_bt,
                       "\n  Sample median ", levs[1], " (raw data): ", med1_raw,
                       "\n  Sample median ", levs[2], " (raw data): ", med2_raw)
              else
                paste0("\n\n-  **Back-transformed mean ", levs[1], ":** ", m1_bt,
                       " *(original scale)*",
                       "\n-  **Back-transformed mean ", levs[2], ":** ", m2_bt,
                       " *(original scale)*",
                       "\n-  **Sample median ", levs[1], " (raw data):** ", med1_raw,
                       "\n-  **Sample median ", levs[2], " (raw data):** ", med2_raw)
            )
          }
        }

        return(txt)
      }

      # Helper: significance text
      generate_sig_txt <- function(console = FALSE) {
        if (t_test_final$p.value <= alpha) {
          if (console)
            paste0("* -> Significant, H0 is rejected  (p \u2264 \u03b1 = ", alpha, ")")
          else
            paste0("* -> **Significant**, $H_{0}$ is rejected  (p $\\leq \\alpha$ = ", alpha, ")")
        } else {
          if (console)
            paste0("-> Not significant, H0 is NOT rejected  (p > \u03b1 = ", alpha, ")")
          else
            paste0("-> Not significant, $H_{0}$ is **NOT** rejected  (p > $\\alpha$ = ", alpha, ")")
        }
      }


      generate_ci_txt <- function(console = FALSE) {

        scale_note <- if (Response_Transformed) {
          if (console) " (transformed scale)" else "transformed scale"
        } else {
          generate_ci_label(is_one_sample, paired, console = console)
        }

        txt <- paste0(
          if (console)
            paste0("\n  ", conf_pct, "% CI", scale_note, ": [ ",
                   round(t_test_final$conf.int[1], 3), ", ",
                   round(t_test_final$conf.int[2], 3), " ]")
          else
            paste0("\n-  **", conf_pct, "% CI** (", scale_note, "):  [ ",
                   round(t_test_final$conf.int[1], 3), ", ",
                   round(t_test_final$conf.int[2], 3), " ]")
        )

        if (Response_Transformed && !is.null(ci_backtransformed)) {

          # Option 4: "back-transformed" label, interpret carefully only for two-sample
          careful_note <- if (!is_one_sample && !paired) {
            if (console) "  *interpret carefully*" else "  *(interpret carefully)*"
          } else {
            ""
          }

          txt <- paste0(
            txt,
            if (console)
              paste0("\n  ", conf_pct, "% CI (back-transformed):  [ ",
                     round(ci_backtransformed[1], 3), ", ",
                     round(ci_backtransformed[2], 3), " ]",
                     careful_note, "\n")
            else
              paste0("\n-  **", conf_pct, "% CI** (back-transformed):  [ ",
                     round(ci_backtransformed[1], 3), ", ",
                     round(ci_backtransformed[2], 3), " ]",
                     careful_note)
          )
        }

        return(txt)
      }


      # Helper: transformation note
      generate_trans_note <- function(console = FALSE) {
        if (!Response_Transformed) return(NULL)

        if (console) {
          paste0(
            "\nNote on transformation:",
            "\nThe t-test was conducted on the ", trans_name, "-transformed scale. ",
            "The back-transformed mean and the sample median of the raw data will ",
            "differ when the transformation is not perfectly normalizing. ",
            "For non-normal data consider f_wilcox_test() which tests the median ",
            "directly without transformation assumptions."
          )
        } else {
          paste0(
            "\n**Note on transformation:**",
            "\n The t-test was conducted on the *", trans_name, "*-transformed scale. ",
            "The back-transformed mean and the sample median of the raw data will ",
            "differ when the transformation is not perfectly normalizing. ",
            "For non-normal data consider `f_wilcox_test()` which tests the median ",
            "directly without transformation assumptions."
          )
        }
      }

      # -----------------------------------------------------------------------
      # G. FINAL SUMMARY PRINTING
      # -----------------------------------------------------------------------

      # Determine title
      method_title <- if (Response_Transformed) {
        paste0(t_test_final$method, " of (", trans_name, " transformed): ", response_name)
      } else {
        paste0(t_test_final$method, " of: ", response_name)
      }

      # Determine levs_for_hyp
      if (is_one_sample) {
        levs_for_hyp <- response_name
        levs2        <- NULL
      } else {
        levs_for_hyp <- levs[1]
        levs2        <- levs[2]
      }

      # p-value formatting
      p_val_fmt <- f_conditional_round(t_test_final$p.value)

      # Generate all text components
      sample_stats_txt  <- generate_sample_stats_txt(is_one_sample, paired, console = FALSE)
      hypotheses_txt    <- generate_hypotheses(
        levs_for_hyp, levs2,
        paired = paired, mu = mu, alternative = alternative,
        console = FALSE
      )
      sig_txt           <- generate_sig_txt(console = FALSE)
      ci_txt            <- generate_ci_txt(console = FALSE)
      trans_note        <- generate_trans_note(console = FALSE)

      # Scale note for test statistics header
      scale_note_hdr <- if (Response_Transformed) " *(transformed scale)*" else ""

      # -------  Write markdown output  ----------------------------------------
      cat(paste0("  \n## ", method_title, "  \n"))
      cat(paste0("\n**Method:** ", t_test_final$method,
                 " (", t_test_final$alternative, ")"))

      cat(paste0("&nbsp;\n  \n&nbsp;\n  \n**SAMPLE STATISTICS:**\n"))
      cat(sample_stats_txt)

      cat(paste0("&nbsp;\n  \n&nbsp;\n  \n**HYPOTHESES:**\n"))
      cat(hypotheses_txt)

      if (!is.null(mu_transformed)) {
        cat(paste0("\n  + *(mu forward-transformed to ",
                   round(mu_transformed, 4), " on ", trans_name, " scale)*"))
      }

      cat(paste0(
        "&nbsp;\n  \n&nbsp;\n  \n**TEST RESULTS", scale_note_hdr, ":**\n",
        "\n-  **t:** ", round(t_test_final$statistic, 3),
        "\n-  **df:** ", round(t_test_final$parameter, 3), "   \n",
        "\n-  **p-value:** ", p_val_fmt, " ", sig_txt, "   \n"
      ))

      cat(paste0("&nbsp;\n  \n**ESTIMATE:**\n"))
      cat(ci_txt)
      cat("&nbsp;\n  \n&nbsp;  \n  \n")
      if (!is.null(trans_note)) cat(trans_note)



      if (n_before != n_after)
        cat(paste0("**WARNING** Removed ", n_before - n_after,
                   " rows with missing values.\n"))

      # -------  Store console versions in output_list  ------------------------
      sample_stats_txt_out <- generate_sample_stats_txt(is_one_sample, paired, console = TRUE)
      hypotheses_txt_out   <- generate_hypotheses(
        levs_for_hyp, levs2,
        paired = paired, mu = mu, alternative = alternative,
        console = TRUE
      )
      sig_txt_out          <- generate_sig_txt(console = TRUE)
      ci_txt_out           <- generate_ci_txt(console = TRUE)
      trans_note_out       <- generate_trans_note(console = TRUE)

      output_list[[response_name]][["sample_stats_txt"]]  <- sample_stats_txt_out
      output_list[[response_name]][["hypotheses_txt"]]     <- hypotheses_txt_out
      output_list[[response_name]][["sig_txt"]]            <- sig_txt_out
      output_list[[response_name]][["ci_txt"]]             <- ci_txt_out
      output_list[[response_name]][["trans_note"]]         <- trans_note_out
      output_list[[response_name]][["alpha"]]              <- alpha
      output_list[[response_name]][["Response_Transformed"]] <- Response_Transformed
      output_list[[response_name]][["trans_name"]]<- if (Response_Transformed) trans_name else NULL
      output_list[[response_name]][["mu_transformed"]]     <- mu_transformed

      cat("&nbsp;\n  \n&nbsp;  \n  \n")

      if (n_before != n_after)
        cat(paste0(
          "**WARNING** Removed ",
          n_before - n_after,
          " rows with missing values.\n"
        ))

      i <- i + 1
      if (output_type %in% c("pdf", "word") && i < length(lhs))
        cat("\n\n<div style=\"page-break-after: always;\"></div>\n\\newpage\n")
    }

    if (output)
      return(output_list)
  }

  # ===========================================================================
  # 8. Execution & File Saving
  # ===========================================================================
  suppressMessages(
    utils::capture.output(
      output_list <- generate_report(),
      file = nullfile()
    )
  )
  class(output_list) <- "f_t_test"


  if (output_type %in% c("word", "pdf")) {
    message(paste0("Saving output in: ", output_path))
    word_pdf_preamble <- function() {
      paste0(
        "
---
title: \"f_t_test Analysis Report\"
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
  - \\setlength{\\droptitle}{-2.5cm} % Adjust vertical spacing
---
"
      )
    }
    knitr::opts_chunk$set(comment = "")
    generated_markdown <- capture.output(generate_report(output = FALSE))
    rmd_content <- paste(word_pdf_preamble(),
                         paste(generated_markdown, collapse = "\n"),
                         sep = "\n")
    writeLines(rmd_content, temp_output_file)
    rmarkdown::render(
      temp_output_file,
      output_file = output_path,
      intermediates_dir = temp_output_dir,
      knit_root_dir = temp_output_dir,
      quiet = TRUE,
      output_format = paste0(output_type, "_document")
    )
    if (open_generated_files)
      try(f_open_file(output_path), silent = TRUE)
    return(invisible(output_list))

  } else if (output_type == "excel") {
    message(paste0("Saving output in: ", output_path))
    summary_tables <- lapply(output_list, function(obj) {
      t_obj <- if (obj$Response_Transformed)
        obj$t_test_transformed
      else
        obj$t_test
      m_str <- paste(round(t_obj$estimate, 3), collapse = " vs ")
      est_names <- names(t_obj$estimate)
      ci_type <- if ("mean of x" %in% est_names &&
                     length(est_names) == 1)
        "Mean"
      else if ("mean of the differences" %in% est_names)
        "Mean Difference"
      else
        "Difference in Means"

      df_out <- data.frame(
        Statistic = t_obj$statistic,
        df = t_obj$parameter,
        p_value = t_obj$p.value,
        Mean = m_str,
        CI_Type = ci_type,
        LowerCI = t_obj$conf.int[1],
        UpperCI = t_obj$conf.int[2],
        Method = t_obj$method,
        Transformed = obj$Response_Transformed
      )
      if (isTRUE(obj$Response_Transformed) &&
          !is.null(obj$ci_backtransformed)) {
        df_out$LowerCI_original_scale <- obj$ci_backtransformed[1]
        df_out$UpperCI_original_scale <- obj$ci_backtransformed[2]
      }
      df_out
    })
    names(summary_tables) <- lhs
    writexl::write_xlsx(summary_tables, path = output_path)
    if (open_generated_files)
      try(f_open_file(output_path), silent = TRUE)
    return(invisible(output_list))

  } else if (output_type == "rmd") {
    if (is.null(knitr::opts_knit$get("output.dir")))
      knitr::opts_knit$set(output.dir = tempdir())
    generated_markdown <- capture.output(generate_report(output = FALSE))
    output_list[["rmd"]] <- paste(generated_markdown, collapse = "\n")
    return(invisible(output_list))

  } else if (output_type == "console") {
    print(output_list)
    return(invisible(output_list))
  }

  invisible(suppressWarnings(file.remove(temp_output_file)))
  return(output_list)
}

# =============================================================================
#' @export
#' @rdname f_t_test
# =============================================================================
f_t_test.default <- function(x,
                             y = NULL,
                             paired = FALSE,
                             var.equal = NULL,
                             conf.level = NULL,
                             mu = 0,
                             alternative = "two.sided",
                             norm_plots = TRUE,
                             transformation = TRUE,
                             force_transformation = NULL,
                             alpha = 0.05,
                             intro_text = TRUE,
                             close_generated_files = FALSE,
                             open_generated_files = interactive(),
                             output_type = "default",
                             save_as = NULL,
                             save_in_wdir = FALSE,
                             ...) {
  if (!is.numeric(x))
    stop("'x' must be numeric for the vector interface.")
  if (!is.null(y) &&
      !is.numeric(y))
    stop("'y' must be numeric or NULL for the vector interface.")
  if (!is.null(y) &&
      paired &&
      length(x) != length(y))
    stop("For a paired test, 'x' and 'y' must have the same length.")

  # Sanitise names: strip data frame prefix (e.g. "mtcars$hp" -> "hp")
  x_name       <- make.names(sub(".*\\$", "", deparse(substitute(x))))
  y_name       <- if (!is.null(y)) {
    make.names(sub(".*\\$", "", deparse(substitute(y))))
  } else{
    NULL
  }

  response_col <- if (is.null(y)) {
    x_name
  } else{
    "y"
  }

  if (!is.null(force_transformation) && !is.null(y)) {

    if (force_transformation %in% c(x_name, y_name, "x", "y")) {
      # Valid group name -- silently redirect to the response column
      force_transformation <- response_col

    } else {
      # Genuinely unrecognised name -- warn and correct
      warning("'force_transformation = \"", force_transformation, "\"' was not ",
              "recognised. In the vector interface, valid values are '",
              x_name, "' or '", y_name, "'. ",
              "Transformation will be forced on the entire response column.")
      force_transformation <- response_col
    }
  }

  # Pairwise deletion of NAs before melting to long format
  if (!is.null(y) && paired) {
    ok <- complete.cases(x, y)
    if (sum(!ok) > 0) {
      warning(sprintf(
        "Removed %d incomplete pairs containing NAs in 'x' or 'y'.",
        sum(!ok)
      ))
      x <- x[ok]
      y <- y[ok]
    }
  }

  if (is.null(y)) {
    df   <- setNames(data.frame(x), x_name)
    form <- as.formula(paste0("`", x_name, "` ~ 1"))
  } else {
    grp_levels <- c(x_name, y_name)
    grp <- factor(rep(grp_levels, times = c(length(x), length(y))), levels = grp_levels)
    df   <- data.frame(y = c(x, y), grp = grp)
    form <- y ~ grp
  }

  out <- f_t_test.formula(
    formula               = form,
    data                  = df,
    paired                = paired,
    var.equal             = var.equal,
    conf.level            = conf.level,
    mu                    = mu,
    alternative           = alternative,
    norm_plots            = norm_plots,
    transformation        = transformation,
    force_transformation  = if (is.null(force_transformation))
      NULL
    else
      response_col,
    alpha                 = alpha,
    intro_text            = intro_text,
    close_generated_files = close_generated_files,
    open_generated_files  = open_generated_files,
    output_type           = output_type,
    save_as               = save_as,
    save_in_wdir          = save_in_wdir,
    ...
  )

  if (!is.null(y)) {
    new_name <- paste0(x_name, "_vs_", y_name)
    names(out)[names(out) == "y"] <- new_name
  }

  out
}

# ============================================================
#  print method
# ============================================================
#' @export
print.f_t_test <- function(x, ...) {
  for (nm in names(x)) {
    if (nm == "rmd") next
    sublist <- x[[nm]]

    t_obj <- if (isTRUE(sublist$Response_Transformed)) {
      sublist$t_test_transformed
    } else {
      sublist$t_test
    }

    # Header
    cat("\n==========================================================\n")
    if (isTRUE(sublist$Response_Transformed)) {
      cat(paste0(t_obj$method, " (", sublist$trans_name,
                 " transformed) of: ", nm, "\n"))
    } else {
      cat(paste0(t_obj$method, " (", t_obj$alternative, ") of: ", nm, "\n"))
    }
    cat("==========================================================\n")

    # Sample statistics
    cat("\nSAMPLE STATISTICS:")
    cat(sublist$sample_stats_txt)

    # Hypotheses
    cat("\n\nHYPOTHESES:")
    cat(sublist$hypotheses_txt)
    if (!is.null(sublist$mu_transformed))
      cat(paste0("\n  (mu on transformed scale: ",
                 round(sublist$mu_transformed, 4), ")"))

    # Test results
    scale_note <- if (isTRUE(sublist$Response_Transformed))
      " (transformed scale)" else ""
    cat(paste0("\n\nTEST RESULTS", scale_note, ":"))
    cat(sprintf(
      "\n  t = %.3f,  df = %.3f,  p-value = %.4f  %s\n",
      t_obj$statistic,
      t_obj$parameter,
      t_obj$p.value,
      # FIX: uses stored alpha, not hardcoded 0.05
      ifelse(t_obj$p.value <= sublist$alpha, "*", "")
    ))
    cat(sublist$sig_txt)

    # Estimate / CI
    cat("\n\nESTIMATE:")
    cat(sublist$ci_txt)

    # Transformation note
    if (!is.null(sublist$trans_note))
      cat(sublist$trans_note)

    cat("\n")
  }
  cat("\n\n")
}

# =============================================================================
#' @export
# =============================================================================
plot.f_t_test <- function(x, ...) {
  plots_shown <- 0
  for (nm in names(x)) {
    if (nm == "rmd")
      next
    sublist <- x[[nm]]
    for (plot_key in c("normality_plots", "transformed_normality_plots")) {
      if (!is.null(sublist[[plot_key]]) &&
          file.exists(sublist[[plot_key]])) {
        if (requireNamespace("png", quietly = TRUE) &&
            requireNamespace("grid", quietly = TRUE)) {
          img <- png::readPNG(sublist[[plot_key]])
          grid::grid.newpage()
          grid::grid.raster(img)
          plots_shown <- plots_shown + 1
        } else {
          warning("Packages 'png' and 'grid' are required to render these plots.")
        }
      }
    }
  }
  if (plots_shown == 0)
    cat("No plots found. Run with norm_plots = TRUE and output_type != 'default'.\n")
  invisible(x)
}
