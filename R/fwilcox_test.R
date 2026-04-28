#' Perform multiple Wilcoxon rank sum and signed rank tests with inspection and visualization.
#'
#' Performs One-sample (Wilcoxon signed rank), Two-sample independent (Wilcoxon rank sum /
#' Mann-Whitney U), or Paired (Wilcoxon signed rank) tests on a given dataset.
#' Several response parameters can be analysed in sequence (formula interface).
#' Additionally, a vector interface similar to stats::wilcox.test() is supported.
#'
#' @section Median vs Pseudo-median:
#' By default this function calls \code{stats::wilcox.test(conf.int = TRUE)},
#' which bases its confidence interval and hypothesis test on the
#' \strong{Hodges-Lehmann estimator}, not the raw sample median. This is
#' standard behaviour of the Wilcoxon test, not something specific to this
#' function. This function explicitly labels the estimator for what it is,
#' because it is commonly mislabelled as "CI for the median" in textbooks
#' and software output.
#'
#' The estimator works differently depending on the test type:
#'
#' \strong{One-sample:} The \emph{pseudo-median} is the middle value of all
#' possible pairwise averages of your data points (including each value
#' paired with itself). For a perfectly symmetric distribution it equals the sample
#' median; for skewed data the two can differ.
#'
#' \strong{Paired:} The paired differences (observation 1 minus observation 2
#' within each pair) are computed first, and the pseudo-median of those
#' differences is estimated. This is conceptually a one-sample problem
#' applied to the differences, not a comparison of two independent groups.
#' The CI is for the pseudo-median of the differences, \strong{not} for the difference
#' between the two separate sample medians.
#'
#' \strong{Two-sample independent:} The \emph{location shift} is the median
#' of all \eqn{n_1 \times n_2} pairwise differences (one value from Group 1
#' minus one from Group 2). It answers: \emph{by how much does a randomly
#' chosen value from Group 1 tend to exceed a randomly chosen value from
#' Group 2?} When both groups have the same distributional shape it equals
#' the raw difference in sample medians; when shapes differ, the two values can
#' diverge.
#'
#' In all three cases the sample median(s) are reported separately for
#' descriptive purposes only.
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
#' @param paired Logical. If \code{TRUE}, performs a paired Wilcoxon test.
#'   \strong{Note on row order:} For the formula interface, data must be
#'   sorted so that the observations in the two groups match row-for-row
#'   (i.e. row 1 of group 1 is paired with row 1 of group 2).
#'   For the vector interface, \code{x} and \code{y} must have the same length.
#'
#'   \strong{Note on factor level order:} The formula interface computes
#'   differences as \code{level1 - level2} based on factor level order,
#'   which defaults to alphabetical. To control the direction, set the
#'   reference level explicitly:
#'   \preformatted{
#'   # Set levels at creation
#'   group <- factor(group, levels = c("pre", "post"))
#'
#'   # Or relevel an existing factor
#'   group <- relevel(group, ref = "pre")
#'   }
#'   A reversed level order flips the sign of the estimate and CI but
#'   does not affect the W statistic or p-value.
#' @param mu Numeric. The hypothesised value of the pseudo-median (one-sample) or
#'   location shift (paired/two-sample) under H0. Default is 0.
#' @param alternative Character string. \code{"two.sided"} (default),
#'   \code{"greater"}, or \code{"less"}.
#' @param norm_plots Logical. If \code{TRUE}, descriptive diagnostic plots are
#'   included in the output. Default is \code{TRUE}.
#' @param alpha Numeric. Significance level. Default is \code{0.05}.
#' @param conf.level Numeric. Confidence level of the interval.
#'   Default is \code{1 - alpha}. If \code{conf.level} is specified, then
#'   \code{alpha <- 1 - conf.level}.
#' @param intro_text Logical. If \code{TRUE}, includes explanation about Wilcoxon
#'   test assumptions.
#' @param close_generated_files Logical. Closes open Excel/Word files before writing.
#'   Works on Windows (taskkill), macOS (pkill) and Linux (pkill/soffice).
#'   Default \code{FALSE}. \strong{WARNING:} Always save your work before using this option!
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
#' @param save_in_wdir Logical. Save in working directory.
#' @param ... For the formula method: additional arguments forwarded to
#'   the row-filtering step. The arguments \code{subset} and
#'   \code{na.action} are honored: when supplied, they are spliced
#'   (still unevaluated) into a \code{\link[stats]{model.frame}} call
#'   built once before the per-response loop, so the \code{subset}
#'   expression is evaluated in the data's column scope (e.g.
#'   \code{subset = cyl == 6} works). All responses in a multi-response
#'   call (\code{y1 + y2 ~ group}) are then tested on the identical row
#'   set. For the default (vector) method, \code{...} is currently
#'   unused.
#'
#' @return An object of class \code{'f_wilcox_test'}.
#'
#' @export
f_wilcox_test <- function(x, ...) {
  UseMethod("f_wilcox_test")
}

# ============================================================
#  Formula method
# ============================================================
#' @export
#' @rdname f_wilcox_test
f_wilcox_test.formula <- function(formula,
                                  data                 = NULL,
                                  paired               = FALSE,
                                  conf.level           = NULL,
                                  mu                   = 0,
                                  alternative          = "two.sided",
                                  norm_plots           = TRUE,
                                  alpha                = 0.05,
                                  intro_text           = TRUE,
                                  close_generated_files  = FALSE,
                                  open_generated_files   = TRUE,
                                  output_type          = "default",
                                  save_as              = NULL,
                                  save_in_wdir         = FALSE,
                                  ...) {

  # ------------------------------------------------------------------
  # 1.  Reset initial settings on exit
  # ------------------------------------------------------------------

  .session_state <- save_session_state()  # Helper function: helper_session_state
  on.exit(restore_session_state(.session_state), add = TRUE) # Helper function: helper_session_state

  # ------------------------------------------------------------------
  # 2.  Data & formula parsing
  # ------------------------------------------------------------------
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

  dots <- list(...)
  if (".original_name" %in% names(dots)) {
    data_name <- dots[[".original_name"]]
  }

  temp_output_dir  <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")
  file.create(temp_output_file)
  f_wrap_lines()

  output_list <- list()

  if (!(output_type %in% c("pdf", "word", "excel", "rmd", "console", "default"))) {
    stop("output_type should be: 'pdf', 'word', 'excel', 'console', 'rmd', 'default'")
  }

  # ------------------------------------------------------------------
  # 3.  File-path logic
  # ------------------------------------------------------------------

  file_extension <- NULL

  if (save_in_wdir) save_dir <- getwd() else save_dir <- tempdir()

  output_type_map <- c(
    "pdf"   = ".pdf",
    "word"  = ".docx",
    "excel" = ".xlsx",
    "rmd"   = ".rmd"
  )

  needs_output_file <- !(output_type %in% c("default", "console")) ||
    !is.null(save_as) || isTRUE(save_in_wdir)

  if (needs_output_file) {
    if (!is.null(save_as) || isTRUE(save_in_wdir)) {
      if (!is.null(save_as)) {
        save_as <- gsub("\\\\", "/", save_as)
        file_extension_save_as <- unname(extract_extension(save_as))
        if (!isFALSE(file_extension_save_as[1]))
          file_extension <- file_extension_save_as
      }
      if (is.null(file_extension) && output_type %in% c("console", "default")) {
        output_path <- get_save_path(
          save_as      = save_as,
          default_name = paste(data_name, "wilcox_output", sep = "_"),
          default_dir  = save_dir,
          file.ext     = ".pdf"
        )
        output_type <- "pdf"
      } else if (is.null(file_extension)) {
        file.ext <- unname(output_type_map[output_type])
        output_path <- get_save_path(
          save_as      = save_as,
          default_name = paste(data_name, "wilcox_output", sep = "_"),
          default_dir  = save_dir,
          file.ext     = file.ext
        )
      } else {
        output_path <- get_save_path(
          save_as      = save_as,
          default_name = paste(data_name, "wilcox_output", sep = "_"),
          default_dir  = save_dir,
          file.ext     = file_extension[1]
        )
        output_type <- file_extension[2]
      }
    } else {
      file.ext <- unname(output_type_map[output_type])
      output_path <- get_save_path(
        save_as      = save_as,
        default_name = paste(data_name, "wilcox_output", sep = "_"),
        default_dir  = save_dir,
        file.ext     = file.ext
      )
    }
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

  # ------------------------------------------------------------------
  # 4.  Parse LHS and RHS of formula
  # ------------------------------------------------------------------
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
        warning("f_wilcox_test only supports one grouping variable. Using the first one.")
    }
  } else {
    is_one_sample  <- TRUE
    predictor_name <- NULL
  }

  # Validation
  for (response in lhs) {
    if (!(response %in% names(data)))
      stop(paste("Response variable", response, "not found in data."))
    if (!is.numeric(data[[response]]))
      stop(paste("Response variable", response, "must be numeric."))
  }
  if (!is_one_sample) {
    if (!(predictor_name %in% names(data)))
      stop(paste("Predictor", predictor_name, "not found in data."))
    data[[predictor_name]] <- as.factor(data[[predictor_name]])
    if (nlevels(data[[predictor_name]]) != 2)
      stop("Grouping variable must have exactly 2 levels for a Wilcoxon test.")
  }

  ########## Build master analysis frame (one row set, all responses) #######
  # Hoists row filtering out of the per-response loop so every response
  # in `y1 + y2 ~ group` is tested on the identical row set, and bakes
  # in any subset / na.action passed via `...`.
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
  n_before_master <- nrow(data)
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
  n_after_master  <- nrow(data)
  ###########################################################################

  # Reconcile alpha / conf.level
  if (is.null(conf.level)) {
    conf.level <- 1 - alpha
  } else {
    alpha <- 1 - conf.level
  }

  # pre-compute rounded percentage label to avoid floating-point printing
  #         (e.g. 1 - 0.05 can print as 94.99999... without rounding)
  conf_pct <- round(conf.level * 100, 1)

  # ------------------------------------------------------------------
  # 5.  Inner report-generation function
  # ------------------------------------------------------------------
  generate_report <- function(output = TRUE) {

    if (isTRUE(intro_text)) {
      cat("
# Assumptions of Wilcoxon Rank-Sum / Signed-Rank Tests

## 1. Independence
- Observations must not influence each other. This is determined by study
  design, not statistics.
  + *Two-sample (Wilcoxon Rank-Sum):* Subjects should be distinct both
    *within* groups and *between* groups.
  + *Paired (Wilcoxon Signed-Rank):* Pairs are distinct, though observations
    within a pair are correlated.
- **Violation invalidates** the test; the solution is a change in experimental
  design.

## 2. Scale of Measurement
- Data must be at least Ordinal (test is based on ranks).
- Suitable for continuous (ratio) data, counts, or ordinal scales (Likert).
- **Normality is NOT required:** The test is robust to outliers and non-normal
  distributions.

## 3. Data Shape & Distribution
- **For Paired (Signed-Rank):** The *differences* between the pairs should be
  roughly symmetric around zero. If they are heavily skewed, consider the
  Sign Test instead.
- **For Two-Sample (Rank-Sum):**
  + **Ideal Case (Location shift):** If both groups have roughly the same
    distributional shape (just shifted), the test compares location parameters
    and the Hodges-Lehmann shift equals the difference in medians.
  + **General Case:** If the shapes differ (e.g. one wide and flat, the other
    tall and narrow), the test remains valid as a test of *stochastic
    dominance* (\"values in Group A tend to be higher\"), but the
    Hodges-Lehmann shift no longer equals the difference in medians.
- **Visual Check:** Inspect the side-by-side boxplots and density plots.

## 4. Handling Ties (Duplicate Values)
- A *tie* occurs when two or more observations share the same value.
- If you see a warning *'cannot compute exact p-value with ties,'* R has
  automatically switched to a normal approximation. The result is generally
  still valid.

## 5. Adequate Sample Size
- **Small samples (n < 20):** R uses an exact test (if no ties).
- **Large samples:** R uses a normal approximation.
- **Rule of Thumb:** $n \\geq 6$ (paired) or $n_1 + n_2 \\geq 10$
  (two-sample) for adequate power."
      )
      # if (output_type %in% c("pdf", "word")) {
      #   cat("<div style=\"page-break-after: always;\"></div>\n\\newpage")
      # }
    }

    i <- 0
    output_list <- list()

    # Fires regardless of intro_text so the user always sees it in this situation.
    if (length(lhs) > 1) {
      k          <- length(lhs)
      fwer_pct   <- round((1 - (1 - alpha)^k) * 100, 1)
      bonf_alpha <- round(alpha / k, 4)
      cat(paste0(
        "\n\n***\n\n",
        "**[!]  NOTE: Multiple Testing Across ", k, " Response Variables**  \n\n",
        "This report runs ", k, " independent Wilcoxon tests on the same dataset. ",
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
        "\n-  **Pre-registration**: if each response was a pre-specified primary outcome, ",
        "correction may not be required; document this decision explicitly.",
        "\n\n***\n\n"
      ))
    }

    # ----------------------------------------------------------------
    # Loop through response variables
    # ----------------------------------------------------------------
    for (response_name in lhs) {

      output_list[[response_name]] <- list()

      # Data prep
      cols_needed <- if (is_one_sample) {
        c(response_name)
      } else {
        c(response_name, predictor_name)
      }
      current_formula <- if (is_one_sample) {
        as.formula(paste0(response_name, " ~ 1"))
      } else {
        as.formula(paste0(response_name, " ~ ", predictor_name))
      }

      n_before      <- n_before_master
      data_complete <- data[, cols_needed, drop = FALSE]
      n_after       <- n_after_master

      cat("  \n \n# Analysis of: ", response_name, "  \n")

      # Prepare group vectors
      y_vals <- data_complete[[response_name]]
      if (is_one_sample) {
        group1 <- y_vals
        group2 <- NULL
        levs   <- NULL
      } else {
        groups <- data_complete[[predictor_name]]
        levs   <- levels(groups)
        group1 <- y_vals[groups == levs[1]]
        group2 <- y_vals[groups == levs[2]]
      }

      # ------------------------------------------------------------
      # Keep mu, mu was silently dropped from two-sample wilcox.test()
      # ------------------------------------------------------------
      if (is_one_sample) {
        test_res <- wilcox.test(
          group1,
          mu          = mu,
          alternative = alternative,
          conf.int    = TRUE,
          conf.level  = conf.level,
          ...
        )
        call_str <- paste0("wilcox.test(", response_name, ", mu = ", mu, ")")
      } else {
        test_res <- wilcox.test(
          group1,
          group2,
          mu          = mu,          # FIX 2: now correctly forwarded
          paired      = paired,
          alternative = alternative,
          conf.int    = TRUE,
          conf.level  = conf.level
        )
        type_str <- if (paired) "Signed-Rank" else "Rank-Sum"
        call_str <- paste0(type_str, " Wilcoxon test(",
                           response_name, " by ", predictor_name, ")")
      }

      output_list[[response_name]][["wilcox_test"]] <- test_res
      output_list[[response_name]][["wilcox_call"]] <- call_str
      output_list[[response_name]][["alpha"]]       <- alpha

      # ------------------------------------------------------------
      # Descriptive / diagnostic plots
      # ------------------------------------------------------------
      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = 7.8, height = 7.8, units = "in", res = 300)
      par(mfrow = c(2, 2), mar = c(4, 4, 4, 1))

      if (is_one_sample) {
        boxplot(y_vals, main = "Boxplot", col = "lightblue")
        try(f_hist(y_vals, xlab = response_name), silent = TRUE)
        try(f_qqnorm(y_vals), silent = TRUE)
        plot(y_vals, main = "Data Index", ylab = response_name)
      } else {
        boxplot(current_formula, data = data_complete,
                main = "Data by Group", col = "lightgrey")
        try(f_hist(y_vals, xlab = response_name), silent = TRUE)
        stripchart(current_formula, data = data_complete,
                   method = "jitter", vertical = TRUE,
                   main = "Data by Group (Jittered)")
        if (paired) {
          plot(group1 - group2, main = "Paired Differences", ylab = "Differences")
        } else {
          plot(density(group1), col = "blue", main = "Group Densities")
          lines(density(group2), col = "red")
          legend("topright", legend = levs, col = c("blue", "red"), lty = 1)
        }
      }
      dev.off()

      if (isTRUE(norm_plots)) {
        cat("\n     \n&nbsp;  \n Check the diagnostic plots below.  \n")
        cat(paste0("![](", temp_file, ")"), "  \n \n")
        cat("&nbsp;\n  \n")
      }

      output_list[[response_name]][["descriptive_plots"]] <- temp_file

      if (output_type != "rmd") {
        cat("\n<div style=\"page-break-after: always;\"></div>\n\\newpage")
      }

      # ------------------------------------------------------------
      # Output text generation
      # ------------------------------------------------------------

      # Helper: descriptive_txt
      generate_descriptive_txt <- function(is_one_sample, console = FALSE) {
        if (is_one_sample) {
          sample_median  <- median(y_vals, na.rm = TRUE)
          hl_estimate    <- test_res$estimate   # pseudo-median from wilcox.test

          txt <- paste0(
            "\n-  **Sample Median of ",
            response_name,
            ":** ",
            round(sample_median, 3),
            "\n-  **Hypothesized Value:** ",
            f_conditional_round(mu),
            "\n"
          )
        } else {
          m1          <- median(group1, na.rm = TRUE)
          m2          <- median(group2, na.rm = TRUE)
          hl_shift    <- test_res$estimate   # HL location shift from wilcox.test

          txt <- paste0(
            "\n-  **Median ",
            levs[1],
            ":** ",
            round(m1, 3),
            "\n-  **Median ",
            levs[2],
            ":** ",
            round(m2, 3),
            "\n-  **Raw difference (",
            levs[1],
            " - ",
            levs[2],
            "):** ",
            round(m1 - m2, 3),
            "\n"
          )
        }

        # prepare text for console usage
        if (console == TRUE) {
          txt <- gsub("\\*\\*", "", txt)   # remove bold markers
          txt <- gsub("\n-(?= )", "\n", txt, perl = TRUE) # remove markdown bullets
        }

        return(txt)
      }

      # Helper: Hypothesis text
      generate_hypotheses <- function(name_a,
                                      name_b      = NULL,
                                      paired      = FALSE,
                                      mu          = 0,
                                      alternative = "two.sided",
                                      console     = FALSE) {

        # Describe the *parameter the test actually tests*
        if (is.null(name_b)) {
          param_desc <- sprintf("pseudo-median (Hodges-Lehmann) of %s", name_a)
        } else if (paired) {
          param_desc <- sprintf(
            "pseudo-median of paired differences (%s - %s)", name_a, name_b)
        } else {
          param_desc <- sprintf(
            "location shift (%s - %s, Hodges-Lehmann)", name_a, name_b)
        }

        switch(alternative,
               "two.sided" = { h0_op <- "is equal to";               ha_op <- "is not equal to" },
               "greater"   = { h0_op <- "is less than or equal to";  ha_op <- "is greater than" },
               "less"      = { h0_op <- "is greater than or equal to"; ha_op <- "is less than"  }
        )

        null_text <- sprintf("True %s %s %s", param_desc, h0_op, mu)
        alt_text  <- sprintf("True %s %s %s", param_desc, ha_op, mu)

        if (console) {
          txt <- paste0("\n  H0: ", null_text, ".\n  H1: ", alt_text,".\n")
        } else {
          txt <- paste0("\n  + $H_{0}$: ", null_text, ".\n  + $H_{1}$: ", alt_text,".\n")
        }

        return(txt)
      }

      # Helper: estimate txt
      generate_estimate_txt <- function(is_one_sample, paired, console = FALSE) {
        if (is_one_sample) {
          txt <- paste0(
            "\n-  **Pseudo-median** (Hodges-Lehmann): ",
            round(test_res$estimate, 3)
          )
        } else if (paired) {

          txt <- paste0(
            "\n-  **Pseudo-median of differences** (Hodges-Lehmann): ",
            round(test_res$estimate, 3)
          )
        } else {
          txt <- paste0(
            "\n-  **Location shift** (Hodges-Lehmann): ",
            round(test_res$estimate, 3)
          )
        }

        # prepare text for console usage
        if (console == TRUE) {
          txt <- gsub("\\*\\*", "", txt)   # remove bold markers
          txt <- gsub("\n-(?= )", "\n", txt, perl = TRUE) # remove markdown bullets
        }

        return(txt)
      }

      # Helper: sig_txt, show text on significance based on p and alpha
      generate_sig_txt <- function(console = FALSE) {
        if (console == TRUE) {
          if (test_res$p.value <= alpha) {
            txt <- paste0("*-> Significant, H0 is rejected (p \u2264 \u03b1 = ",
                          alpha,
                          ")")
          } else {
            txt <- paste0(" -> Not significant,\n  H0 is NOT rejected  (p > \u03b1 = ",
                          alpha,
                          ")")
          }
        } else if (console == FALSE){
          if (test_res$p.value <= alpha) {
            txt <- paste0("* -> **Significant**, $H_{0}$ is rejected  (p $\\leq \\alpha$ = ",
                          alpha,
                          ")")
          } else {
            txt <- paste0(" -> Not significant, $H_{0}$ is **NOT** rejected  (p > $\\alpha$ = ",
                          alpha,
                          ")")
          }
        }

        return(txt)
      }

      # Helper: ci_txt, print ready confidence interval
      generate_ci_txt <- function(console = FALSE) {
          txt <- paste0(
            "\n-  **",
            conf_pct,
            "% CI:** [ ",
            round(test_res$conf.int[1], 3),
            ", ",
            round(test_res$conf.int[2], 3),
            " ]"
          )
        if (console == TRUE) {
          txt <- gsub("\\*\\*", "", txt)   # remove bold markers
          txt <- gsub("\n-(?= )", "\n", txt, perl = TRUE) # remove markdown bullets
        }

        return(txt)
      }

      # Helper: CI Note (depends on test type)
      generate_ci_note <- function(is_one_sample, paired, console = FALSE) {
         if (is_one_sample) {
            txt <-paste0(
              "\n  \n**Note on the CI**: \nThis interval is for the **pseudo-median**",
              " (Hodges-Lehmann estimator); ",
              "the median of all pairwise averages of your data points. Both the ",
              "sample median and the pseudo-median are valid descriptions of your data; ",
              "they just measure slightly different things (see `?f_wilcox_test` for details)."
            )
          } else if (paired) {
            txt <-paste0(
              "\n  \n**Note on the CI:** \nIn a paired test, the within-pair differences ",
              "(observation 1 minus observation 2 for each pair) are computed first. ",
              "This interval is then for the **pseudo-median of those differences**",
              " (Hodges-Lehmann estimator);  ",
              "the middle value of all possible pairwise averages of the differences. ",
              "For symmetric differences it equals the median of the raw differences. ",
              "When the differences are skewed the two can diverge, which is why the ",
              "CI may not be centred on the median of the raw differences. ",
              "Both are valid descriptions; they just measure it slightly differently ",
              "(see `?f_wilcox_test` for details).\n"
            )
          } else {
            txt <-paste0(
              "\n  \n**Note on the CI:** \nThis interval is for the **location shift**",
              " (Hodges-Lehmann estimator); ",
              "the median of all possible pairwise differences between your two groups. ",
              "If both groups have the same distributional shape, this equals the raw ",
              "median difference. When shapes differ they can diverge, which is why ",
              "the CI may not be centred on the difference in sample medians. ",
              "Both the raw median difference and the location shift are valid ",
              "descriptions of the gap between groups; they just measure it ",
              "slightly differently (see `?f_wilcox_test` for details)."
            )
          }
          # prepare text for console usage
          if (console == TRUE) {
            txt <- gsub("\\*\\*", "", txt)   # remove bold markers
          }

          return(txt)
        }


      # Prepare creation of markdown/word/pdf output

      # SAMPLE STATISTICS:
      descriptive_txt <- generate_descriptive_txt(is_one_sample, console = FALSE)

      # HYPOTHESES:
      hypotheses_txt <- generate_hypotheses(
        name_a      = if (is_one_sample) response_name else levs[1],
        name_b      = if (is_one_sample) NULL else levs[2],
        paired      = paired,
        mu          = mu,
        alternative = alternative,
        console = FALSE
      )

      # TEST RESULTS:
      p_val_fmt    <- f_conditional_round(test_res$p.value)
      sig_txt      <- generate_sig_txt(console = FALSE)

      # ESTIMATE:
      estimate_txt <- generate_estimate_txt(is_one_sample, paired, console = FALSE)
      ci_txt       <- generate_ci_txt(console = FALSE)
      ci_note      <- generate_ci_note(is_one_sample, paired, console = FALSE)

      #-----------------------------------------------------
      # Write output to Markdown
      #-----------------------------------------------------
      cat(paste0("  \n## ", test_res$method," of:", response_name, "  \n"))
      cat(paste0("&nbsp;\n  \n**Method:** ",
                 test_res$method, " (", test_res$alternative, ")." ))
      cat(paste0("&nbsp;\n  \n&nbsp;\n  \n**Sample statistics:**\n"))
      cat(descriptive_txt)
      cat(paste0("&nbsp;\n  \n&nbsp;\n  \n**Hypotheses:**\n"))
      cat(hypotheses_txt)
      cat(paste0(
        "&nbsp;\n  \n&nbsp;\n  \n**Test Results:**\n",
        "\n-  **W:** ", round(test_res$statistic, 3),
        "\n-  **p-value:** ", p_val_fmt, " ", sig_txt, "   \n"))
      cat(paste0(
        "&nbsp;\n  \n**Estimate:**\n"))
      cat(estimate_txt)
      cat(ci_txt)
      cat("&nbsp;\n  \n&nbsp;  \n  \n")
      cat(ci_note)



      if (n_before != n_after)
        cat(paste0("**WARNING** Removed ", n_before - n_after,
                   " rows with missing values.\n"))


      #-----------------------------------------------------
      # Write output for console (for print.f_wilcox_test)
      #-----------------------------------------------------

      # Sample statistics (console)
      descriptive_txt_out <- generate_descriptive_txt(is_one_sample, console = TRUE)

      # HYPOTHESES: (console)
      hypotheses_txt_out <- generate_hypotheses(
        name_a      = if (is_one_sample) response_name else levs[1],
        name_b      = if (is_one_sample) NULL else levs[2],
        paired      = paired,
        mu          = mu,
        alternative = alternative,
        console     = TRUE
      )

      # TEST RESULTS: (console)
      sig_txt_out     <- generate_sig_txt(console = TRUE)

      # ESTIMATE: (console)
      estimate_txt_out<- generate_estimate_txt(is_one_sample, paired, console = TRUE)
      ci_txt_out      <- generate_ci_txt(console = TRUE)
      ci_note_out     <- generate_ci_note(is_one_sample, paired, console = TRUE)

      # store in output_list
      output_list[[response_name]][["descriptive_txt"]]<- descriptive_txt_out
      output_list[[response_name]][["hypotheses_txt"]] <- hypotheses_txt_out
      output_list[[response_name]][["p_val_fmt"]]      <- p_val_fmt
      output_list[[response_name]][["sig_txt"]]        <- sig_txt_out
      output_list[[response_name]][["estimate_txt"]]   <- estimate_txt_out
      output_list[[response_name]][["ci_txt"]]         <- ci_txt_out
      output_list[[response_name]][["ci_note"]]        <- ci_note_out

      # Pagebreak depending on doc length.
      i <- i + 1
      if (output_type != "rmd" && i < length(lhs))
        cat("\n \n<div style=\"page-break-after: always;\"></div>\n\\newpage\n")
    } # end lhs loop

    if (isTRUE(output)) return(output_list)
  } # end generate_report()

  # ------------------------------------------------------------------
  # 6.  Execute -- single call captures both output_list and markdown
  # ------------------------------------------------------------------

  md_lines   <- capture.output(output_list <- generate_report())
  class(output_list) <- "f_wilcox_test"

  # ------------------------------------------------------------------
  # 7.  File output
  # ------------------------------------------------------------------
  if (output_type %in% c("word", "pdf")) {

    message(paste0("Saving output in: ", output_path))

    word_pdf_preamble <- function() {
      paste0(
        "---\n",
        "title: \"f_wilcox_test Analysis Report\"\n",
        "date: \"`r Sys.Date()`\"\n",
        "output:\n",
        "   word_document:\n",
        "     reference_docx: !expr system.file(",
        "\"rmarkdown/templates/MS_word_style.docx\", package = \"rfriend\")\n",
        "   pdf_document:\n",
        "     latex_engine: pdflatex\n",
        "header-includes:\n",
        "  - \\usepackage[utf8]{inputenc}\n",
        "  - \\usepackage[T1]{fontenc}\n",
        "  - \\usepackage{textcomp}\n",
        "  - \\DeclareUnicodeCharacter{03BB}{\\ensuremath{\\lambda}}\n",
        "  - \\DeclareUnicodeCharacter{2264}{\\ensuremath{\\leq}}\n",
        "  - \\DeclareUnicodeCharacter{03B1}{\\ensuremath{\\alpha}}\n",
        "  - \\usepackage{titling}\n",
        "  - \\setlength{\\droptitle}{-2.5cm}\n",
        "---\n"
      )
    }

    knitr::opts_chunk$set(comment = "")
    # Reuse md_lines from the single generate_report() call above
    rmd_content <- paste(word_pdf_preamble(),
                         paste(md_lines, collapse = "\n"),
                         sep = "\n")
    writeLines(rmd_content, temp_output_file)
    rmarkdown::render(
      temp_output_file,
      output_file       = output_path,
      intermediates_dir = temp_output_dir,
      knit_root_dir     = temp_output_dir,
      quiet             = TRUE,
      output_format     = paste0(output_type, "_document")
    )
    if (isTRUE(open_generated_files)) f_open_file(output_path)
    return(invisible(output_list))

  } else if (output_type == "excel") {

    message(paste0("Saving output in: ", output_path))
    summary_tables <- lapply(output_list, function(obj) {
      w_obj <- obj$wilcox_test
      data.frame(
        Statistic       = w_obj$statistic,
        p_value         = w_obj$p.value,
        LowerCI         = w_obj$conf.int[1],
        UpperCI         = w_obj$conf.int[2],
        HL_estimate     = w_obj$estimate,
        Method          = w_obj$method
      )
    })
    names(summary_tables) <- lhs
    write_xlsx(summary_tables, path = output_path)
    if (isTRUE(open_generated_files)) f_open_file(output_path)
    return(invisible(output_list))

  } else if (output_type == "rmd") {

    if (is.null(knitr::opts_knit$get("output.dir")))
      knitr::opts_knit$set(output.dir = tempdir())
    output_list[["rmd"]] <- paste(md_lines, collapse = "\n")
    return(invisible(output_list))

  } else if (output_type == "console") {

    print(output_list)
    return(invisible(output_list))

  }

  # "default" -- return list silently, clean up temp file
  invisible(suppressWarnings(file.remove(temp_output_file)))
  return(output_list)
}


# ============================================================
#  Default (vector) method
# ============================================================
#' @export
#' @rdname f_wilcox_test
f_wilcox_test.default <- function(x,
                                  y                    = NULL,
                                  paired               = FALSE,
                                  conf.level           = NULL,
                                  mu                   = 0,
                                  alternative          = "two.sided",
                                  norm_plots           = TRUE,
                                  alpha                = 0.05,
                                  intro_text           = TRUE,
                                  close_generated_files  = FALSE,
                                  open_generated_files   = TRUE,
                                  output_type          = "default",
                                  save_as              = NULL,
                                  save_in_wdir         = FALSE,
                                  ...) {

  if (!is.null(y) && paired && length(x) != length(y))
    stop("For a paired test, 'x' and 'y' must have the same length.")
  if (!is.numeric(x))
    stop("'x' must be numeric for the vector interface.")
  if (!is.null(y) && !is.numeric(y))
    stop("'y' must be numeric or NULL for the vector interface.")

  # Capture original variable names for labels
  x_name <- deparse(substitute(x))
  y_name <- if (!is.null(y)) deparse(substitute(y)) else NULL

  if (is.null(y)) {
    # One-sample
    col_name      <- make.names(x_name)
    response_name <- x_name
    data_list        <- setNames(list(x), col_name)
    df               <- as.data.frame(data_list)
    form             <- as.formula(paste0(col_name, " ~ 1"))
  } else {
    # Two-sample -- convert to long format
    col_name      <- make.names(paste0(x_name, "_vs_", y_name))
    response_name <- paste0(x_name, "_vs_", y_name)
    grp_levels    <- c(x_name, y_name)
    grp           <- factor(rep(grp_levels, times = c(length(x), length(y))),
                            levels = grp_levels)
    data_list     <- setNames(list(c(x, y), grp), c(col_name, "grp"))
    df            <- as.data.frame(data_list)
    form          <- as.formula(paste0(col_name, " ~ grp"))
  }

  out <- f_wilcox_test.formula(
    formula               = form,
    data                  = df,
    paired                = paired,
    conf.level            = conf.level,
    mu                    = mu,
    alternative           = alternative,
    norm_plots            = norm_plots,
    alpha                 = alpha,
    intro_text            = intro_text,
    close_generated_files = close_generated_files,
    open_generated_files  = open_generated_files,
    output_type           = output_type,
    save_as               = save_as,
    save_in_wdir          = save_in_wdir,
    ...
  )

  # Rename list key from make.names version back to the original name
  if (col_name %in% names(out)) {
    names(out)[names(out) == col_name] <- response_name
  }

  return(out)
}


# ============================================================
#  print method
# ============================================================
#' @export
print.f_wilcox_test <- function(x, ...) {
  for (nm in names(x)) {
    if (nm == "rmd") next
    sublist <- x[[nm]]
    w_obj   <- sublist$wilcox_test

    cat("\n==========================================================\n")
    cat(paste0(w_obj$method," (",w_obj$alternative,") of:", nm, "\n"))
    cat("==========================================================\n")
    cat("\nSAMPLE STATISTICS:")
    cat(sublist$descriptive_txt)
    cat("\nHYPOTHESES:")
    cat(sublist$hypotheses_txt)
    cat("\nTEST RESULTS:")
    cat(paste0("\n  W = ", w_obj$statistic,", p-value = ", sublist$p_val_fmt))
    cat(sublist$sig_txt)
    cat("\n  \nESTIMATE:")
    cat(sublist$estimate_txt)
    cat(sublist$ci_txt)
    cat(sublist$ci_note)

  }
  cat("\n \n")
}


# ============================================================
#  plot method
# ============================================================
#' @export
plot.f_wilcox_test <- function(x, ...) {

  response_names <- names(x)[names(x) != "rmd"]

  if (length(response_names) == 0) {
    message("No plot data available in this f_wilcox_test object.")
    return(invisible(NULL))
  }

  has_png  <- requireNamespace("png",  quietly = TRUE)
  has_grid <- requireNamespace("grid", quietly = TRUE)

  if (!has_png || !has_grid) {
    message("Install packages 'png' and 'grid' to display plots inline.")
    message("Stored diagnostic plot file(s):")
    for (nm in response_names) {
      path <- x[[nm]]$descriptive_plots
      if (!is.null(path)) message("  ", nm, ": ", path)
    }
    return(invisible(NULL))
  }

  for (nm in response_names) {
    path <- x[[nm]]$descriptive_plots
    if (!is.null(path) && file.exists(path)) {
      img <- png::readPNG(path)
      grid::grid.newpage()
      grid::grid.raster(img)
    } else {
      message("Plot file not found for variable: ", nm)
    }
  }

  invisible(NULL)
}
