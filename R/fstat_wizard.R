#' Statistical Test Wizard
#'
#' @description
#' Analyzes your data structure based on a formula and recommends the appropriate statistical test.
#' Checks variable types, normality of residuals, homogeneity of variance, and checks if f_boxcox
#' transformation can fix non-normality. Recommends rfriend functions as primary code, with
#' base R alternatives shown as fallback.
#'
#' Supports standard formulas including \code{y ~ .}, \code{y ~ as.factor(x)}, and interaction
#' terms. Formulas with random effects (e.g. \code{(1|ID)}) are detected and handled separately.
#' Multivariate responses (e.g. \code{cbind(y1, y2) ~ x}) and transformed responses
#' (e.g. \code{log(y) ~ x}) are not supported.
#'
#' @param x A formula (e.g., \code{y ~ x}) or a data frame. When a formula is
#'   provided, \code{data} must also be supplied. When a data frame is provided,
#'   \code{formula} must be supplied as the second argument.
#' @param formula A formula specifying the relationship (used with the data.frame method).
#' @param data A data frame containing the variables referenced in the formula.
#' @param id_col Character string. Name of the column identifying subjects/blocks
#'   for paired or repeated-measures designs. When supplied, the wizard (a) verifies
#'   the pairing structure (each subject should appear in every group exactly once),
#'   (b) treats the design as paired/repeated measures, and (c) embeds the real column
#'   name into the generated code. Omit for independent-samples designs. Default \code{NULL}.
#' @param data_name Character string to name the data base used. Default \code{NULL},
#' which automatically derives the data name from the data frame used as input.
#' @param plots Logical. If \code{TRUE}, generates diagnostic plots using
#'   \code{f_hist()} (histogram of the response) and \code{f_qqnorm()} (QQ-plot
#'   of model residuals). Plots are stored in the result as \code{$histogram}
#'   and \code{$qqplot} (\code{recordedplot} objects) and displayed by the
#'   print method. Default \code{FALSE}.
#' @param output_type Character string specifying the output format of the
#'   recommended rfriend function (when \code{run=TRUE}) and the displayed code
#'   strings. Passed through to \code{f_aov()}, \code{f_t_test()}, \code{f_glm()},
#'   etc. Valid values match those of the underlying function (\code{"word"},
#'   \code{"pdf"}, \code{"excel"}, \code{"console"}, \code{"default"}, \code{"rmd"}).
#'   Default \code{"word"}.
#' @param run Logical. If \code{TRUE}, the wizard attempts to execute the recommended
#'   rfriend function and stores the result in \code{$run_result}. Only works for
#'   unambiguous single-function recommendations (not multi-step or external packages).
#'   Default \code{FALSE}.
#' @param interactive Logical. If \code{TRUE}, asks the user questions about study design. Default \code{FALSE}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{"f_stat_wizard"}: a list containing:
#'   \describe{
#'     \item{formula}{The formula used.}
#'     \item{formula_text}{Character string of the formula.}
#'     \item{data_name}{Name of the data object as passed by the user.}
#'     \item{n}{Effective sample size (after NA removal).}
#'     \item{n_dropped}{Number of rows removed due to missing values.}
#'     \item{paired}{Logical. Whether a paired/repeated-measures design was detected (via \code{id_col}).}
#'     \item{id_col}{Character. Name of the subject/block column supplied, or \code{NULL}.}
#'     \item{y_var}{Name of the response variable.}
#'     \item{y_type}{Detected type of the response: \code{"binary"}, \code{"count"},
#'       \code{"multinomial"}, \code{"ratio_normal"}, \code{"ratio_non_normal"},
#'       \code{"ratio_unknown"}, or \code{"unsupported"}.}
#'     \item{x_vars}{Character vector of explanatory variable names.}
#'     \item{x_types}{Character vector of detected types (\code{"nominal"},
#'       \code{"ordinal"}, \code{"ratio"}).}
#'     \item{n_groups}{Number of groups (for single categorical X), or \code{NULL}.}
#'     \item{group_sizes}{Table of per-group sample sizes, or \code{NULL}.}
#'     \item{is_ancova}{Logical. \code{TRUE} if the model mixes nominal and ratio predictors.}
#'     \item{has_interaction}{Logical. \code{TRUE} if interaction terms were detected.}
#'     \item{normality}{A list with \code{p_value} (Shapiro-Wilk) and \code{is_normal} (logical or \code{NA}).}
#'     \item{variance}{A list with \code{test_used} (\code{"Levene"} or \code{"Bartlett"}),
#'       \code{p_value}, and \code{is_equal} (logical).}
#'     \item{boxcox}{A list with \code{attempted} (logical), \code{can_fix} (logical),
#'       and \code{p_value_after} (numeric or \code{NA}).}
#'     \item{overdispersion}{A list with \code{is_overdispersed} (logical, from
#'       DHARMa dispersion test) and \code{p_value}. Only meaningful for count data.}
#'     \item{recommended_call}{A language object representing the rfriend function call,
#'       or \code{NULL} if no single function could be determined.}
#'     \item{run_result}{The result of executing the recommended test (when \code{run=TRUE}),
#'       or \code{NULL}.}
#'     \item{histogram}{A \code{recordedplot} from \code{f_hist()} (when \code{plots=TRUE}),
#'       or \code{NULL}.}
#'     \item{qqplot}{A \code{recordedplot} from \code{f_qqnorm()} of model residuals
#'       (when \code{plots=TRUE} and Y is continuous), or \code{NULL}.}
#'     \item{report}{Character vector of the human-readable report lines (used by
#'       \code{print.f_stat_wizard}).}
#'   }
#'
#' @export
#' @import stats
#' @examples
#' # Formula interface (recommended)
#' f_stat_wizard(Sepal.Length ~ Species, data = iris)
#'
#' # Data-first interface (backward compatible)
#' f_stat_wizard(iris, Sepal.Length ~ Species)
#'
#' # Paired design -- supply the id_col that identifies matched subjects
#' f_stat_wizard(extra ~ group, data = sleep, id_col = "ID")
#'
#' # With diagnostic plots
#' f_stat_wizard(Sepal.Length ~ Species, data = iris, plots = TRUE)
#'
#' # Run the recommended test directly
#' result <- f_stat_wizard(Sepal.Length ~ Species, data = iris, run = TRUE)
#' result$run_result
#'
#' # Inspect metadata
#' result <- f_stat_wizard(Sepal.Length ~ Species, data = iris)
#' result$y_type
#' result$normality
#' result$group_sizes
# =============================================================================
#' @export
# =============================================================================
f_stat_wizard <- function(x, ...) {
  mc <- match.call()

  if (missing(x)) {
    # Handle f_stat_wizard(data = iris, formula = y ~ x)
    if (!is.null(mc$data)) {
      x_val <- eval(mc$data, envir = parent.frame())
      # Strip data/formula from ... to avoid "matched by multiple actual arguments"
      # (they got swept into ... because the generic only declares `x`)
      dots <- list(...)
      dots$data    <- NULL
      dots$formula <- NULL
      if (!is.null(mc$formula)) {
        frm <- eval(mc$formula, envir = parent.frame())
        return(do.call(f_stat_wizard.data.frame,
                       c(list(x = x_val, formula = frm), dots)))
      }
      return(do.call(f_stat_wizard.data.frame,
                     c(list(x = x_val), dots)))
    }
    stop("Argument 'x' (formula or data) is missing.")
  }

  UseMethod("f_stat_wizard")
}

# =============================================================================
#' @export
#' @rdname f_stat_wizard
# =============================================================================
f_stat_wizard.formula <- function(formula, data, id_col = NULL, run = FALSE,
                                  plots = FALSE, output_type = "word",
                                  interactive = FALSE, data_name = NULL, ...) {

  if (missing(data) || !is.data.frame(data)) {
    stop("When using the formula interface, 'data' must be a data frame.")
  }

  if (is.null(data_name)) {
    data_name <- deparse(substitute(data))
    if (length(data_name) > 1) data_name <- "data"
  }

  # Delegate to the internal workhorse
  .f_stat_wizard_engine(
    formula     = formula,
    data        = data,
    data_name   = data_name,
    id_col      = id_col,
    run         = run,
    plots       = plots,
    output_type = output_type,
    interactive = interactive
  )
}

# =============================================================================
#' @export
#' @rdname f_stat_wizard
# =============================================================================
f_stat_wizard.data.frame <- function(x, formula, id_col = NULL, run = FALSE,
                                     plots = FALSE, output_type = "word",
                                     interactive = FALSE, data_name = NULL, ...) {
  data <- x
  if (missing(formula) || !inherits(formula, "formula")) {
    stop("When passing a data frame, you must also provide a formula (e.g., y ~ x).")
  }

  if (is.null(data_name)) {
    data_name <- deparse(substitute(x))
    if (length(data_name) > 1) data_name <- "data"
  }

  .f_stat_wizard_engine(
    formula     = formula,
    data        = data,
    data_name   = data_name,
    id_col      = id_col,
    run         = run,
    plots       = plots,
    output_type = output_type,
    interactive = interactive
  )
}


# =============================================================================
# INTERNAL ENGINE -- all logic lives here
# =============================================================================
.f_stat_wizard_engine <- function(formula, data, data_name, id_col = NULL,
                                  run = FALSE, plots = FALSE,
                                  output_type = "word", interactive = FALSE) {

  # --- UTILITIES ---
  safe_deparse <- function(expr) paste(deparse(expr), collapse = " ")

  # --- DERIVE PAIRED STATUS FROM id_col ---
  # Presence of id_col = paired/repeated measures design.
  paired <- !is.null(id_col)
  if (paired) {
    if (!is.character(id_col) || length(id_col) != 1) {
      stop("'id_col' must be a single character string naming the subject/block column.")
    }
    if (!id_col %in% names(data)) {
      stop("id_col '", id_col, "' not found in data.")
    }
  }

  # --- 1. INPUT CHECKS ---
  if (!is.data.frame(data)) stop("Input 'data' must be a data frame.")
  if (!inherits(formula, "formula")) stop("You must provide a valid formula (e.g., y ~ x).")

  # --- 2. FORMULA PARSING ---
  formula_text <- safe_deparse(formula)
  y_col <- all.vars(formula[[2]])
  if (length(y_col) != 1) {
    stop("This wizard supports a single response variable only. Found: ",
         paste(y_col, collapse = ", "))
  }

  # Guard: transformed response (log(y), sqrt(y), etc.)
  y_lhs_text <- safe_deparse(formula[[2]])
  if (y_lhs_text != y_col) {
    stop("This wizard does not support transformed responses (e.g. log(y) ~ x). ",
         "Found: ", y_lhs_text, ". Transform your data first, then use a simple formula.")
  }

  if (!y_col %in% names(data)) stop(paste("Response variable", y_col, "not found in data."))

  # Detect random effects EARLY -- exit before further parsing
  has_re_in_formula <- grepl("|", formula_text, fixed = TRUE)

  # Use terms() to expand y ~ . and get proper term labels
  if (has_re_in_formula) {
    x_cols_vars <- setdiff(all.vars(formula[[3]]), y_col)
    x_cols_vars <- x_cols_vars[x_cols_vars %in% names(data)]
  } else {
    tf <- tryCatch(terms(formula, data = data), error = function(e) NULL)
    if (!is.null(tf)) {
      tl <- attr(tf, "term.labels")
      x_cols_vars <- unique(unlist(lapply(tl, function(t) {
        all.vars(as.formula(paste("~", t)))
      })))
    } else {
      x_cols_vars <- all.vars(formula[[3]])
    }
  }

  # Check columns exist
  missing_x <- x_cols_vars[!x_cols_vars %in% names(data)]
  if (length(missing_x) > 0) stop(paste("Explanatory variable(s) not found:", paste(missing_x, collapse = ", ")))

  # --- 3. NA HANDLING ---
  used_cols <- c(y_col, x_cols_vars)
  if (paired) used_cols <- c(used_cols, id_col)
  complete_rows <- complete.cases(data[, used_cols, drop = FALSE])
  n_dropped <- sum(!complete_rows)
  data <- data[complete_rows, , drop = FALSE]
  y_data <- data[[y_col]]

  # --- INITIALISE METADATA (populated throughout, returned in list) ---
  y_type           <- "unknown"
  x_types          <- character()
  n_groups         <- NULL
  is_ancova        <- FALSE
  has_interaction  <- FALSE
  can_be_normalized <- FALSE
  shapiro_p        <- NA_real_
  is_normal        <- NA
  variance_test    <- NA_character_
  variance_p       <- NA_real_
  variance_equal   <- TRUE
  boxcox_attempted <- FALSE
  boxcox_can_fix   <- FALSE
  boxcox_p_after   <- NA_real_
  is_overdispersed <- FALSE         # DHARMa-based overdispersion check
  dispersion_p     <- NA_real_      # p-value from DHARMa dispersion test
  recommended_call <- NULL          # stores the primary rfriend call expression
  run_result       <- NULL          # stores result of run=TRUE execution
  group_sizes      <- NULL          # stores per-group sample sizes
  wizard_histogram <- NULL          # stores f_hist plot of response
  wizard_qqplot    <- NULL          # stores f_qqnorm plot of residuals

  # --- HELPER: build result list ---
  build_result <- function(report_lines) {
    result <- list(
      formula        = formula,
      formula_text   = formula_text,
      data_name      = data_name,
      n              = nrow(data),
      n_dropped      = n_dropped,
      paired         = paired,
      id_col         = id_col,
      y_var          = y_col,
      y_type         = y_type,
      x_vars         = x_cols_vars,
      x_types        = x_types,
      n_groups       = n_groups,
      group_sizes    = group_sizes,
      is_ancova      = is_ancova,
      has_interaction = has_interaction,
      normality      = list(
        p_value   = shapiro_p,
        is_normal = is_normal
      ),
      variance       = list(
        test_used = variance_test,
        p_value   = variance_p,
        is_equal  = variance_equal
      ),
      boxcox         = list(
        attempted    = boxcox_attempted,
        can_fix      = boxcox_can_fix,
        p_value_after = boxcox_p_after
      ),
      overdispersion = list(
        is_overdispersed = is_overdispersed,
        p_value          = dispersion_p
      ),
      recommended_call = recommended_call,
      run_result     = run_result,
      histogram      = wizard_histogram,
      qqplot         = wizard_qqplot,
      report         = report_lines
    )
    class(result) <- "f_stat_wizard"
    result
  }

  # --- TEXT BUILDER ---
  report_lines <- character()
  add_txt <- function(...) { report_lines <<- c(report_lines, paste0(...)) }

  add_txt("---------------------------------------------------")
  add_txt("          rfriend STATISTICAL WIZARD               ")
  add_txt("---------------------------------------------------")
  add_txt(paste0("Model: ", formula_text))
  if (n_dropped > 0) {
    add_txt(paste0("Note: Removed ", n_dropped, " row(s) with missing values (NA)."))
  }
  add_txt(paste0("Effective sample size: n = ", nrow(data)))
  if (paired) {
    add_txt(paste0("Design: PAIRED / Repeated Measures (id_col = '", id_col, "')"))
  }

  # --- PAIRING STRUCTURE VERIFICATION ---
  # Only runs when id_col is supplied AND we have a single categorical predictor.
  # Checks that each subject appears in every group exactly once.
  pairing_verified <- FALSE
  if (paired && length(x_cols_vars) >= 1) {
    tryCatch({
      grp_col <- x_cols_vars[1]
      id_vec  <- data[[id_col]]
      grp_vec <- data[[grp_col]]
      tab <- table(id_vec, grp_vec)
      n_subjects  <- nrow(tab)
      n_grp_levels <- ncol(tab)

      # Check 1: every subject x group cell == 1
      if (all(tab == 1)) {
        add_txt(paste0("   OK: Pairing structure verified (",
                       n_subjects, " subjects \u00d7 ", n_grp_levels, " groups, one observation per cell)."))
        pairing_verified <- TRUE
      } else {
        # Check what's wrong
        missing_cells <- sum(tab == 0)
        multi_cells   <- sum(tab > 1)
        if (missing_cells > 0) {
          add_txt(paste0("   WARN: Unbalanced pairing \u2014 ", missing_cells,
                         " subject\u00d7group combination(s) are missing."))
          add_txt("   Not every subject appears in every group. Pairing may be incomplete.")
        }
        if (multi_cells > 0) {
          add_txt(paste0("   WARN: ", multi_cells,
                         " subject\u00d7group combination(s) have multiple observations."))
          add_txt("   True repeated measures may need averaging or a mixed model (f_lmer).")
        }
      }
    }, error = function(e) {
      add_txt(paste0("   Note: Could not verify pairing structure (", conditionMessage(e), ")."))
    })
  }

  # --- EARLY EXIT: Random effects in formula ---
  if (has_re_in_formula) {
    add_txt(" ")
    add_txt("-> Random effect syntax detected in formula (e.g. '(1|ID)').")
    add_txt("---------------------------------------------------")
    add_txt(" ")
    add_txt("RECOMMENDATION:")
    add_txt("Test: Linear Mixed-Effects Model (LMM)")
    add_txt(paste0("Code: f_lmer(", formula_text, ", data=", data_name, ", output_type=\"", output_type, "\")"))
    add_txt("   (Requires lme4 package. Use f_lmer() or lme4::lmer() directly.)")
    add_txt(" ")
    add_txt("---------------------------------------------------")
    add_txt("The wizard cannot fully analyze mixed-model formulas.")
    add_txt("Verify your random-effects structure and check model diagnostics manually.")
    return(build_result(report_lines))
  }

  # --- INTERACTIVE DESIGN CHECK ---
  has_random_effects <- FALSE
  random_var <- NULL

  if (interactive && base::interactive()) {
    message(">>> INTERACTIVE CHECK <<<")
    ans <- readline(prompt = "Are your observations independent? (y/n): ")
    if (tolower(substr(ans, 1, 1)) == "n") {
      message("   (You indicated dependency, e.g. repeated measures or blocking)")
      ans2 <- readline(prompt = "Do you want to use a Mixed Model? (y/n): ")
      if (tolower(substr(ans2, 1, 1)) == "y") {
        has_random_effects <- TRUE
        random_var <- readline(prompt = "What is your random grouping variable name (e.g. SubjectID)? ")
      }
    }
    message(">>> END CHECK <<<")
  }

  # --- CLASSIFY X (PREDICTORS) ---
  # Use model.frame() to evaluate as.factor(), poly(), etc. as the model sees them
  mf <- tryCatch(model.frame(formula, data = data, na.action = na.pass), error = function(e) NULL)

  if (!is.null(mf) && ncol(mf) > 1) {
    for (i in 2:ncol(mf)) {
      val <- mf[[i]]
      if (is.ordered(val))                                      x_types <- c(x_types, "ordinal")
      else if (is.factor(val) || is.character(val))              x_types <- c(x_types, "nominal")
      else if (is.numeric(val) && length(unique(val)) > 10)     x_types <- c(x_types, "ratio")
      else                                                       x_types <- c(x_types, "nominal")
    }
  } else {
    for (col in x_cols_vars) {
      val <- data[[col]]
      if (is.ordered(val))                                      x_types <- c(x_types, "ordinal")
      else if (is.factor(val) || is.character(val))              x_types <- c(x_types, "nominal")
      else if (is.numeric(val) && length(unique(val)) > 10)     x_types <- c(x_types, "ratio")
      else                                                       x_types <- c(x_types, "nominal")
    }
  }

  n_x <- length(x_types)
  has_ratio_x <- "ratio" %in% x_types
  has_nominal_x <- any(x_types %in% c("nominal", "ordinal"))

  # Detect interaction terms
  has_interaction <- grepl("[*:]", formula_text)
  if (has_interaction) {
    add_txt("   Note: Interaction term(s) detected. Check interaction plots manually.")
  }

  # Detect ANCOVA pattern -- mixed ratio + nominal predictors
  is_ancova <- n_x > 1 && has_ratio_x && has_nominal_x

  # Note: intercept-only formula (y ~ 1) -- continues to Y classification below
  if (n_x == 0) {
    add_txt("-> Explanatory: None (intercept-only model).")
  }

  if (n_x == 1) {
    add_txt(paste0("-> Explanatory: Single variable (", x_types[1], ")."))
    if (x_types[1] %in% c("nominal", "ordinal")) {
      grp_col <- if (!is.null(mf) && ncol(mf) > 1) mf[[2]] else data[[x_cols_vars[1]]]
      n_groups <- length(unique(grp_col))
      group_sizes <- table(grp_col)
      if (n_groups < 2) add_txt(paste0("   WARN: Only ", n_groups, " group(s) found. Cannot compare."))
    } else {
      n_groups <- 0L
    }
  } else if (n_x > 1) {
    label <- if (is_ancova) "ANCOVA pattern (nominal + ratio predictors)" else paste(x_types, collapse = ", ")
    add_txt(paste0("-> Explanatory: Multiple variables (", label, ")."))
  }

  # --- SAMPLE SIZE WARNINGS ---
  if (!is.null(group_sizes) && length(group_sizes) >= 2) {
    min_n <- min(group_sizes)
    max_n <- max(group_sizes)
    if (min_n < 5) {
      add_txt(paste0("   WARN: Very small group detected (min n=", min_n,
                     "). Results may be unreliable."))
    }
    if (max_n / min_n > 3) {
      add_txt(paste0("   WARN: Unbalanced design (group sizes: ",
                     paste(group_sizes, collapse = ", "),
                     "). Consider Welch-type tests."))
    }
  }
  if (nrow(data) < 10) {
    add_txt(paste0("   WARN: Total sample (n=", nrow(data),
                   ") is very small. Consider non-parametric tests."))
  }

  # --- Shapiro-Wilk wrapper ---
  # Uses the package-level safe_shapiro() helper from
  # helper_safe_shapiro.R, which returns a shaped htest with
  # p.value = NA for n < 3 or n > 5000 (outside shapiro.test()'s
  # valid range). Call sites below must handle is.na(p.value);
  # they already do via the `"Normality could not be assessed."`
  # branch, which covers both small-n and large-n cases uniformly.
  #
  # Historical note: this function previously had an inline
  # safe_shapiro() that (a) called add_txt() as a side effect for
  # the n < 3 case and (b) subsampled to 5000 with a fixed seed
  # for n > 5000. Both have been removed for consistency with the
  # rest of the package: every Shapiro call now goes through the
  # same helper and produces NA for out-of-range input, so the
  # wizard will not contradict what f_aov / f_t_test / f_lmer
  # report on the same dataset.

  # --- CLASSIFY Y (RESPONSE) ---

  if (is.factor(y_data) || is.character(y_data)) {
    n_levels <- length(unique(y_data))
    if (n_levels < 2) {
      y_type <- "unsupported"
      add_txt(paste0("-> Response: Categorical but only ", n_levels, " level(s). Cannot analyze."))
    } else {
      y_type <- if (n_levels == 2) "binary" else "multinomial"
      add_txt(paste0("-> Response: Categorical (", y_type, ")."))
    }

  } else if (is.logical(y_data)) {
    y_type <- "binary"
    add_txt("-> Response: Logical (treated as binary).")

  } else if (is.numeric(y_data)) {

    unique_vals <- sort(unique(y_data))

    # Binary 0/1 BEFORE count
    if (length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))) {
      y_type <- "binary"
      add_txt("-> Response: Numeric 0/1 (treated as binary).")

    # Count data: non-negative integers
    } else if (length(unique_vals) > 0 && all(y_data >= 0) && all(y_data == floor(y_data)) &&
               max(y_data) < 1000 && length(unique_vals) >= 3) {
      y_type <- "count"
      add_txt("-> Response: Numeric (detected as COUNT data: non-negative integers).")

      # Skip overdispersion check for intercept-only models -- there's nothing to fit a GLM against
      if (n_x == 0) {
        add_txt("   (Overdispersion check skipped \u2014 no predictors in intercept-only model.)")
      } else {
      # Model-based overdispersion check using DHARMa
      tryCatch({
        poisson_fit <- glm(formula, data = data, family = poisson())
        sim_res <- DHARMa::simulateResiduals(poisson_fit, plot = FALSE)
        disp_test <- DHARMa::testDispersion(sim_res, plot = FALSE)
        dispersion_p <- disp_test$p.value
        if (!is.na(dispersion_p) && dispersion_p < 0.05) {
          is_overdispersed <- TRUE
          add_txt(paste0("   Overdispersion detected (DHARMa dispersion test p=",
                         round(dispersion_p, 3), ")."))
          add_txt("   -> Negative Binomial GLM recommended over Poisson.")
        } else {
          add_txt(paste0("   No significant overdispersion (DHARMa p=",
                         round(dispersion_p, 3), ")."))
          add_txt("   Poisson GLM is appropriate.")
        }
      }, error = function(e) {
        # Fallback: rough var/mean check if DHARMa or glm fails
        y_mean <- mean(y_data)
        y_var <- var(y_data)
        if (y_var > 2 * y_mean) {
          is_overdispersed <<- TRUE
          add_txt(paste0("   Overdispersion likely (var/mean = ", round(y_var / y_mean, 1),
                         "; DHARMa check failed: ", conditionMessage(e), ")."))
          add_txt("   -> Negative Binomial GLM recommended over Poisson.")
        } else {
          add_txt("   Poisson GLM is appropriate (no strong overdispersion).")
          add_txt(paste0("   Note: DHARMa check failed: ", conditionMessage(e)))
        }
      })
      } # end if (n_x > 0)

    } else {
      # Continuous numeric -- check normality
      add_txt("-> Response: Numeric. Checking normality of residuals...")
      if (paired) {
        add_txt("   CAVEAT: For paired tests, normality should be checked on within-pair")
        add_txt("   differences, not model residuals. Verify manually after pairing.")
      }

      n_obs <- nrow(data)
      if (n_obs > 500) {
        add_txt(paste0("   Note: Large sample (n=", n_obs, "). Shapiro-Wilk is very sensitive at this size."))
        add_txt("   Consider also: qqnorm(residuals(model)); qqline(residuals(model))")
      } else if (n_obs < 20) {
        add_txt(paste0("   Note: Small sample (n=", n_obs, "). Shapiro-Wilk has low power."))
      }

      is_normal <- tryCatch({
        temp_model <- lm(formula, data = data)
        resids <- residuals(temp_model)

        # Shapiro-then-AD fallback: prefer Shapiro-Wilk (most powerful
        # for n in [3, 5000]), fall back to Anderson-Darling when
        # Shapiro was skipped (safe_shapiro returned NA), so that the
        # wizard produces a definitive decision on datasets of any
        # size rather than silently falling through to "unknown".
        # Returns list(p = numeric, label = "Shapiro"/"Anderson-Darling"/NA).
        normality_p <- function(vals) {
          sh <- safe_shapiro(vals)
          if (!is.na(sh$p.value)) {
            list(p = sh$p.value, label = "Shapiro-Wilk", short = "Shapiro")
          } else {
            ad <- tryCatch(nortest::ad.test(vals),
                           error = function(e) NULL)
            if (!is.null(ad) && !is.na(ad$p.value)) {
              list(p = ad$p.value, label = "Anderson-Darling", short = "AD")
            } else {
              list(p = NA_real_, label = NA_character_, short = NA_character_)
            }
          }
        }

        np        <- normality_p(resids)
        p_val     <- np$p
        shapiro_p <- p_val  # legacy name kept for downstream compatibility

        if (is.na(p_val)) {
          add_txt("   Normality could not be assessed.")
          NA
        } else if (p_val > 0.05) {
          add_txt(paste0("   OK: Residuals appear Normal (",
                         np$short, " p=", round(p_val, 3), ")."))
          TRUE
        } else {
          add_txt(paste0("   WARN: Residuals NOT Normal (",
                         np$short, " p=", round(p_val, 3), ")."))
          # QQ-plot suggestion for borderline cases
          if (p_val > 0.01 && n_obs > 30) {
            add_txt("   Note: p is close to 0.05 with moderate n. Visual QQ-plot check recommended.")
            add_txt(paste0("   Code: qqnorm(residuals(lm(", formula_text, ", data=", data_name, "))); qqline(...)"))
          }
          # Box-Cox check
          if (all(y_data > 0)) {
            add_txt("   ... Checking if f_boxcox() can fix this ...")
            boxcox_attempted <- TRUE
            if (exists("f_boxcox", mode = "function")) {
              tryCatch({
                bc_res <- suppressMessages(f_boxcox(y_data, plots = FALSE))
                y_trans <- bc_res$transformed_data
                if (!is.null(y_trans) && length(y_trans) == length(y_data)) {
                  data_temp <- data
                  data_temp$y_trans_internal <- y_trans
                  rhs <- safe_deparse(formula[[3]])
                  new_model <- lm(as.formula(paste("y_trans_internal ~", rhs)), data = data_temp)
                  # Same Shapiro-then-AD fallback on the post-transform residuals
                  np_new  <- normality_p(residuals(new_model))
                  new_p   <- np_new$p
                  boxcox_p_after <- new_p
                  if (!is.na(new_p) && new_p > 0.05) {
                    add_txt(paste0("   TIP: f_boxcox fixes normality! (New ",
                                   np_new$short, " p=", round(new_p, 3), ")"))
                    can_be_normalized <- TRUE
                    boxcox_can_fix <- TRUE
                  } else if (!is.na(new_p)) {
                    add_txt(paste0("   Note: f_boxcox did not fix normality (",
                                   np_new$short, " p=", round(new_p, 3), ")."))
                  }
                } else {
                  add_txt("   Note: f_boxcox returned unexpected output, skipping.")
                }
              }, error = function(e) {
                add_txt(paste0("   Note: f_boxcox failed (", conditionMessage(e), "). Skipping."))
              })
            } else {
              add_txt("   (f_boxcox not found, skipping transformation check)")
            }
          }
          FALSE
        }
      }, error = function(e) {
        add_txt(paste0("   Note: Could not calculate residuals (", conditionMessage(e), ")."))
        NA
      })

      if (isTRUE(is_normal))      y_type <- "ratio_normal"
      else if (is.na(is_normal))   y_type <- "ratio_unknown"
      else                         y_type <- "ratio_non_normal"

    } # end continuous numeric
  } else {
    add_txt(paste0("-> Response: Unsupported type (", class(y_data)[1], ")."))
    y_type <- "unsupported"
  }

  # --- DIAGNOSTIC PLOTS (optional) ---
  if (plots && is.numeric(y_data)) {
    tryCatch({
      # Histogram of response variable
      wizard_histogram <- f_hist(y_data, main = paste("Distribution of", y_col))
      add_txt(paste0("   Plot: Histogram of '", y_col, "' generated."))

      # QQ-plot of residuals (if a model can be fitted)
      if (y_type %in% c("ratio_normal", "ratio_non_normal", "ratio_unknown")) {
        temp_model_qq <- tryCatch(lm(formula, data = data), error = function(e) NULL)
        if (!is.null(temp_model_qq)) {
          wizard_qqplot <- f_qqnorm(residuals(temp_model_qq),
                                   main = paste("QQ-plot residuals:", formula_text))
          add_txt("   Plot: QQ-plot of residuals generated.")
        }
      }
    }, error = function(e) {
      add_txt(paste0("   Note: Could not generate plots (", conditionMessage(e), ")."))
    })
  }

  # --- HOMOSCEDASTICITY CHECK ---
  if (n_x == 1 && x_types[1] %in% c("nominal", "ordinal") &&
      y_type %in% c("ratio_normal", "ratio_non_normal", "ratio_unknown") &&
      !is.null(n_groups) && n_groups >= 2) {
    tryCatch({
      grp <- as.factor(if (!is.null(mf) && ncol(mf) > 1) mf[[2]] else data[[x_cols_vars[1]]])
      if (requireNamespace("rstatix", quietly = TRUE)) {
        lev_df <- data.frame(.y = y_data, .group = grp)
        lev <- rstatix::levene_test(lev_df, .y ~ .group)
        lev_p <- lev$p[1]
        variance_test <- "Levene"
        variance_p <- lev_p
        if (lev_p > 0.05) {
          add_txt(paste0("   OK: Variances appear equal (Levene p=", round(lev_p, 3), ")."))
        } else {
          add_txt(paste0("   WARN: Variances UNEQUAL (Levene p=", round(lev_p, 3), ")."))
          variance_equal <- FALSE
        }
      } else {
        bart_df <- data.frame(.y = y_data, .g = grp)
        bart <- bartlett.test(.y ~ .g, data = bart_df)
        variance_test <- "Bartlett"
        variance_p <- bart$p.value
        if (bart$p.value > 0.05) {
          add_txt(paste0("   OK: Variances appear equal (Bartlett p=", round(bart$p.value, 3), ")."))
        } else {
          add_txt(paste0("   WARN: Variances UNEQUAL (Bartlett p=", round(bart$p.value, 3), ")."))
          add_txt("   Note: Bartlett is sensitive to non-normality. Install 'rstatix' for Levene's test.")
          variance_equal <- FALSE
        }
      }
    }, error = function(e) {
      add_txt("   Note: Could not perform variance equality test.")
    })
  }

  add_txt("---------------------------------------------------")
  add_txt(" ")
  add_txt("RECOMMENDATION:")

  # --- HELPER: Code template with user's data name ---
  # Automatically appends output_type="..." to all rfriend function calls
  cf <- function(fn, ...) {
    extra <- paste0(list(...), collapse = "")
    paste0(fn, "(", safe_deparse(formula), ", data=", data_name, extra,
           ", output_type=\"", output_type, "\")")
  }

  # --- RECOMMENDATIONS ---

  if (y_type == "unsupported") {
    add_txt("No recommendation available for this response type.")

  } else if (has_random_effects) {
    add_txt("Test: Linear Mixed-Effects Model (LMM)")
    rhs <- safe_deparse(formula[[3]])
    re <- if (!is.null(random_var) && nzchar(random_var)) random_var else "SubjectID"
    add_txt(paste0("Code: f_lmer(", y_col, " ~ ", rhs, " + (1|", re, "), data=", data_name,
                   ", output_type=\"", output_type, "\")"))

  # === INTERCEPT-ONLY (y ~ 1) ===
  } else if (n_x == 0) {
    add_txt("This is a null/intercept-only model with no explanatory variables.")
    if (y_type %in% c("ratio_normal", "ratio_unknown")) {
      add_txt("Test: One-sample T-test (test whether the mean differs from a value)")
      add_txt(paste0("Code: f_t_test(", y_col, " ~ 1, data=", data_name, ", output_type=\"", output_type, "\")"))
      add_txt(paste0("  (Base R: t.test(", data_name, "$", y_col, "))"))
      recommended_call <- bquote(f_t_test(.(formula), data = data, output_type = .(output_type)))
    } else if (y_type == "ratio_non_normal") {
      add_txt("Test: One-sample Wilcoxon Signed-Rank (non-normal response)")
      add_txt(paste0("Code: wilcox.test(", data_name, "$", y_col, ")"))
      add_txt("   Alternative: f_t_test() with transformation=TRUE")
    } else if (y_type == "binary") {
      add_txt("Test: One-sample proportion test")
      add_txt(paste0("Code: prop.test(sum(", data_name, "$", y_col, "), length(", data_name, "$", y_col, "))"))
    } else if (y_type == "count") {
      add_txt("Note: Descriptive statistics only (no comparison without explanatory variable).")
      add_txt(paste0("Code: f_summary(", data_name, ", columns='", y_col, "')"))
    } else {
      add_txt("Note: No meaningful test without an explanatory variable for this response type.")
    }

  # === MULTIPLE X ===
  } else if (n_x > 1) {
    if (y_type == "binary") {
      add_txt("Test: Multiple Logistic Regression")
      add_txt(paste0("Code: ", cf("f_glm", ", family='binomial'")))
      recommended_call <- bquote(f_glm(.(formula), data = data, family = "binomial", output_type = .(output_type)))
    } else if (y_type == "count") {
      if (is_overdispersed) {
        add_txt("Test: Negative Binomial GLM (overdispersion detected)")
        add_txt(paste0("Code: MASS::glm.nb(", safe_deparse(formula), ", data=", data_name, ")"))
        add_txt("   (No rfriend wrapper yet \u2014 requires MASS package)")
      } else {
        add_txt("Test: Poisson GLM")
        add_txt(paste0("Code: ", cf("f_glm", ", family='poisson'")))
      }
    } else if (y_type == "multinomial") {
      add_txt("Test: Multinomial Logistic Regression")
      add_txt(paste0("Code: nnet::multinom(", safe_deparse(formula), ", data=", data_name, ")"))
      add_txt("   (No rfriend wrapper yet \u2014 requires nnet package)")
    } else if (y_type == "ratio_normal") {
      if (is_ancova) {
        add_txt("Test: ANCOVA (Analysis of Covariance)")
        add_txt("   Your model combines categorical and continuous predictors.")
        add_txt(paste0("Code: ", cf("f_aov", ", ANCOVA=TRUE")))
        recommended_call <- bquote(f_aov(.(formula), data = data, ANCOVA = TRUE, output_type = .(output_type)))
      } else {
        add_txt("Test: Multi-way ANOVA or Linear Regression")
        add_txt(paste0("Code: ", cf("f_aov")))
        recommended_call <- bquote(f_aov(.(formula), data = data, output_type = .(output_type)))
      }
      if (has_nominal_x) {
        add_txt("   Post-hoc: f_aov() includes post-hoc tests (adjust='sidak' by default)")
        add_txt("   Alternative: emmeans::emmeans() for custom contrasts")
      }
    } else if (can_be_normalized) {
      add_txt("Test: ANOVA with automatic Box-Cox transformation")
      add_txt(paste0("Code: ", cf("f_aov")))
      add_txt("   Note: f_aov() auto-applies Box-Cox when residuals are non-normal.")
      add_txt("Alternative: GLM")
      if (all(y_data > 0, na.rm = TRUE)) {
        add_txt(paste0("   Code: ", cf("f_glm", ", family=Gamma(link='log')")))
      } else {
        add_txt("   Consider robust regression or a GLM family suited to your data.")
      }
      recommended_call <- bquote(f_aov(.(formula), data = data, output_type = .(output_type)))
    } else if (y_type == "ratio_unknown") {
      add_txt("Note: Normality could not be assessed (too few observations).")
      add_txt("Option 1: Linear Regression (if normality is plausible)")
      add_txt(paste0("   Code: ", cf("f_aov")))
      add_txt("Option 2: Non-parametric or GLM approach (safer)")
      add_txt(paste0("   Code: ", cf("f_glm")))
      recommended_call <- bquote(f_aov(.(formula), data = data, output_type = .(output_type)))
    } else {
      add_txt("Test: Generalized Linear Model (GLM)")
      if (all(y_data > 0, na.rm = TRUE)) {
        add_txt(paste0("Code: ", cf("f_glm", ", family=Gamma(link='log')")))
        recommended_call <- bquote(f_glm(.(formula), data = data, family = Gamma(link = "log"), output_type = .(output_type)))
      } else {
        add_txt("Consider: Robust regression, rank-based methods, or a suitable GLM family.")
      }
    }

  # === SINGLE X ===
  } else {
    # X is Ratio
    if (x_types[1] == "ratio") {
      if (y_type == "binary") {
        add_txt("Test: Logistic Regression")
        add_txt(paste0("Code: ", cf("f_glm", ", family='binomial'")))
      } else if (y_type == "count") {
        if (is_overdispersed) {
          add_txt("Test: Negative Binomial Regression (overdispersion detected)")
          add_txt(paste0("Code: MASS::glm.nb(", safe_deparse(formula), ", data=", data_name, ")"))
          add_txt("   (No rfriend wrapper yet \u2014 requires MASS package)")
        } else {
          add_txt("Test: Poisson Regression")
          add_txt(paste0("Code: ", cf("f_glm", ", family='poisson'")))
        }
      } else if (y_type == "multinomial") {
        add_txt("Test: Multinomial Logistic Regression")
        add_txt(paste0("Code: nnet::multinom(", safe_deparse(formula), ", data=", data_name, ")"))
        add_txt("   (No rfriend wrapper yet \u2014 requires nnet package)")
      } else if (y_type == "ratio_normal" || can_be_normalized) {
        if (can_be_normalized) {
          add_txt("Note: f_glm() does NOT auto-transform like f_t_test/f_aov. Apply Box-Cox manually:")
          add_txt(paste0("   Step 1: ", data_name, "$", y_col, "_t <- f_boxcox(", data_name, "$", y_col, ")$transformed_data"))
          add_txt(paste0("   Step 2: f_glm(", y_col, "_t ~ ", safe_deparse(formula[[3]]), ", data=", data_name, ")"))
          add_txt("Otherwise, untransformed:")
        }
        add_txt("Test: Linear Regression / Pearson Correlation")
        add_txt(paste0("Code: ", cf("f_glm")))
        recommended_call <- bquote(f_glm(.(formula), data = data, output_type = .(output_type)))
        add_txt(paste0("  (Base R: lm(", safe_deparse(formula), ", data=", data_name, "))"))
        add_txt(paste0("   Visualise: f_corplot(", data_name, ")"))
        add_txt(paste0("   Test: cor.test(", data_name, "$", x_cols_vars[1], ", ", data_name, "$", y_col, ", method='pearson')"))
      } else if (y_type == "ratio_unknown") {
        add_txt("Note: Normality could not be assessed.")
        add_txt("Test: Linear Regression (if plausible) or Spearman Correlation (non-parametric)")
        add_txt(paste0("Code: ", cf("f_glm")))
        add_txt(paste0("   Visualise: f_corplot(", data_name, ")"))
        add_txt(paste0("   Test: cor.test(", data_name, "$", x_cols_vars[1], ", ", data_name, "$", y_col, ", method='spearman')"))
      } else {
        add_txt("Test: Spearman Correlation or GLM")
        add_txt(paste0("Code: ", cf("f_glm")))
        add_txt(paste0("   Visualise: f_corplot(", data_name, ")"))
        add_txt(paste0("   Test: cor.test(", data_name, "$", x_cols_vars[1], ", ", data_name, "$", y_col, ", method='spearman')"))
      }

    # X is Ordinal
    } else if (x_types[1] == "ordinal") {
      if (y_type == "binary") {
        add_txt("Test: Logistic Regression with ordinal contrast")
        add_txt(paste0("Code: ", cf("f_glm", ", family='binomial'")))
      } else if (y_type == "count") {
        if (is_overdispersed) {
          add_txt("Test: Negative Binomial GLM with ordinal predictor")
          add_txt(paste0("Code: MASS::glm.nb(", safe_deparse(formula), ", data=", data_name, ")"))
          add_txt("   (No rfriend wrapper yet \u2014 requires MASS package)")
        } else {
          add_txt("Test: Poisson GLM with ordinal predictor")
          add_txt(paste0("Code: ", cf("f_glm", ", family='poisson'")))
        }
      } else if (y_type == "multinomial") {
        add_txt("Test: Multinomial Logistic Regression")
        add_txt(paste0("Code: nnet::multinom(", safe_deparse(formula), ", data=", data_name, ")"))
        add_txt("   (No rfriend wrapper yet \u2014 requires nnet package)")
      } else if (y_type == "ratio_normal") {
        add_txt("Test: Linear Regression with polynomial contrasts (ordinal X)")
        add_txt(paste0("Code: ", cf("f_glm")))
        add_txt(paste0("  (Base R: lm(", safe_deparse(formula), ", data=", data_name, "))"))
        add_txt("   Alternatively: f_kruskal_test() treating groups as unordered")
      } else if (y_type == "ratio_non_normal") {
        add_txt("Test: Kruskal-Wallis Test (non-normal Y with ordinal X)")
        add_txt(paste0("Code: f_kruskal_test(", safe_deparse(formula), ", data=", data_name, ", output_type=\"", output_type, "\")"))
        add_txt("   Alternative: Jonckheere-Terpstra trend test (if order matters)")
      } else if (y_type == "ratio_unknown") {
        add_txt("Note: Normality could not be assessed.")
        add_txt("Option 1: Linear Regression with polynomial contrasts")
        add_txt(paste0("   Code: ", cf("f_glm")))
        add_txt("Option 2: Kruskal-Wallis (safer)")
        add_txt(paste0("   Code: f_kruskal_test(", safe_deparse(formula), ", data=", data_name, ", output_type=\"", output_type, "\")"))
      }

    # X is Nominal
    } else {
      if (is.null(n_groups) || n_groups < 2) {
        add_txt("   Note: < 2 groups found. Cannot compare groups.")

      # --- GUARD: paired but pairing failed verification ---
      } else if (paired && !pairing_verified) {
        add_txt("RECOMMENDATION SUPPRESSED: pairing structure failed verification.")
        add_txt("   The id_col was supplied but the data does not form a clean")
        add_txt("   one-row-per-(subject \u00d7 group) layout required by paired tests.")
        add_txt("   Options:")
        add_txt("   1. Fix the data so each subject appears exactly once per group, then re-run.")
        add_txt("   2. Drop id_col to run an independent-samples test instead.")
        add_txt("   3. If you have multiple measurements per subject per group,")
        add_txt("      use a mixed model that models the dependency directly:")
        add_txt(paste0("      Code: f_lmer(", y_col, " ~ ", x_cols_vars[1],
                       " + (1|", id_col, "), data=", data_name,
                       ", output_type=\"", output_type, "\")"))
        recommended_call <- NULL  # block run=TRUE from executing a broken test

      # --- 2 GROUPS ---
      } else if (n_groups == 2) {
        paired_str <- if (paired) ", paired=TRUE" else ""

        if (y_type == "binary") {
          if (paired) {
            add_txt("Test: Paired binary data requires McNemar's Test")
            add_txt("   McNemar's test needs a 2x2 table of before/after outcomes per subject.")
            add_txt("   This requires wide-format data with subject IDs \u2014 the wizard cannot")
            add_txt("   construct this automatically from a formula.")
            add_txt("   See: ?mcnemar.test for usage once your data is structured.")
          } else {
            add_txt("Test: Chi-square / Fisher's Exact")
            add_txt(paste0("Code: f_chisq_test(", data_name, ", '", y_col, "', '", x_cols_vars[1], "', output_type=\"", output_type, "\")"))
          }
        } else if (y_type == "count") {
          if (is_overdispersed) {
            add_txt("Test: Negative Binomial GLM (overdispersion detected)")
            add_txt(paste0("Code: MASS::glm.nb(", safe_deparse(formula), ", data=", data_name, ")"))
            add_txt("   (No rfriend wrapper yet \u2014 requires MASS package)")
          } else {
            add_txt("Test: Poisson GLM")
            add_txt(paste0("Code: ", cf("f_glm", ", family='poisson'")))
          }
          if (paired) add_txt("   Note: For paired count data, consider a mixed model approach.")
        } else if (y_type == "multinomial") {
          add_txt("Test: Chi-square / Fisher's Exact")
          add_txt(paste0("Code: f_chisq_test(", data_name, ", '", y_col, "', '", x_cols_vars[1], "', output_type=\"", output_type, "\")"))
          if (paired) add_txt("   Note: For paired multinomial data, consider marginal homogeneity test.")
        } else if (y_type == "ratio_normal") {
          if (paired) {
            add_txt("Test: Paired T-test")
            add_txt(paste0("Code: f_t_test(", safe_deparse(formula), ", data=", data_name, ", paired=TRUE, output_type=\"", output_type, "\")"))
            recommended_call <- bquote(f_t_test(.(formula), data = data, paired = TRUE, output_type = .(output_type)))
          } else {
            add_txt("Test: Welch's T-test (default; robust to unequal variances)")
            add_txt(paste0("Code: f_t_test(", safe_deparse(formula), ", data=", data_name, ", output_type=\"", output_type, "\")"))
            recommended_call <- bquote(f_t_test(.(formula), data = data, output_type = .(output_type)))
            if (variance_equal) {
              add_txt("   (Variances appear equal; you may also use f_t_test(..., var.equal=TRUE) for Student's t-test)")
            }
          }
          add_txt(paste0("  (Base R: t.test(", safe_deparse(formula), ", data=", data_name, paired_str, "))"))
          add_txt("   Effect size: rstatix::cohens_d() for Cohen's d")
        } else if (can_be_normalized) {
          add_txt("Test: T-test with automatic Box-Cox transformation")
          add_txt(paste0("Code: f_t_test(", safe_deparse(formula), ", data=", data_name, paired_str, ", output_type=\"", output_type, "\")"))
          add_txt("   Note: f_t_test() auto-applies Box-Cox when residuals are non-normal.")
          if (!variance_equal) {
            add_txt("   Note: Variances are also unequal. Verify after transformation;")
            add_txt("   if still unequal, prefer the non-parametric alternative.")
          }
          add_txt("Alternative: Wilcoxon (Non-parametric)")
          add_txt(paste0("   Code: f_wilcox_test(", safe_deparse(formula), ", data=", data_name, paired_str, ", output_type=\"", output_type, "\")"))
          if (paired) {
            recommended_call <- bquote(f_t_test(.(formula), data = data, paired = TRUE, output_type = .(output_type)))
          } else {
            recommended_call <- bquote(f_t_test(.(formula), data = data, output_type = .(output_type)))
          }
        } else if (y_type == "ratio_unknown") {
          add_txt("Note: Normality could not be assessed.")
          if (paired) {
            add_txt("Option 1: Paired T-test (if normality is plausible)")
            add_txt(paste0("   Code: f_t_test(", safe_deparse(formula), ", data=", data_name, ", paired=TRUE, output_type=\"", output_type, "\")"))
            add_txt("Option 2: Wilcoxon Signed-Rank Test (safer)")
            add_txt(paste0("   Code: f_wilcox_test(", safe_deparse(formula), ", data=", data_name, ", paired=TRUE, output_type=\"", output_type, "\")"))
            recommended_call <- bquote(f_t_test(.(formula), data = data, paired = TRUE, output_type = .(output_type)))
          } else {
            add_txt("Option 1: Welch's T-test (if normality is plausible)")
            add_txt(paste0("   Code: f_t_test(", safe_deparse(formula), ", data=", data_name, ", output_type=\"", output_type, "\")"))
            add_txt("Option 2: Mann-Whitney U / Wilcoxon (safer)")
            add_txt(paste0("   Code: f_wilcox_test(", safe_deparse(formula), ", data=", data_name, ", output_type=\"", output_type, "\")"))
            recommended_call <- bquote(f_t_test(.(formula), data = data, output_type = .(output_type)))
          }
        } else {
          if (paired) {
            add_txt("Test: Wilcoxon Signed-Rank Test (paired, non-parametric)")
            add_txt(paste0("Code: f_wilcox_test(", safe_deparse(formula), ", data=", data_name, ", paired=TRUE, output_type=\"", output_type, "\")"))
            recommended_call <- bquote(f_wilcox_test(.(formula), data = data, paired = TRUE, output_type = .(output_type)))
          } else {
            add_txt("Test: Mann-Whitney U / Wilcoxon (Non-parametric)")
            add_txt(paste0("Code: f_wilcox_test(", safe_deparse(formula), ", data=", data_name, ", output_type=\"", output_type, "\")"))
            recommended_call <- bquote(f_wilcox_test(.(formula), data = data, output_type = .(output_type)))
          }
        }

      # --- >2 GROUPS ---
      } else if (n_groups > 2) {
        if (paired && y_type %in% c("ratio_normal", "ratio_non_normal", "ratio_unknown")) {
          # Paired / repeated measures with >2 groups
          id_name <- id_col  # guaranteed non-NULL when paired==TRUE
          if (y_type == "ratio_normal") {
            add_txt("Test: Repeated Measures ANOVA")
            add_txt("   Your data has >2 paired groups with normal residuals.")
            add_txt("   Consider a mixed model for flexibility:")
            add_txt(paste0("   Code: f_lmer(", y_col, " ~ ", x_cols_vars[1], " + (1|", id_name, "), data=", data_name,
                           ", output_type=\"", output_type, "\")"))
            add_txt("   Alternative (rfriend): f_aov with Error() term")
            add_txt(paste0("   Code: f_aov(", safe_deparse(formula), " + Error(", id_name, "/", x_cols_vars[1], "), data=", data_name, ", output_type=\"", output_type, "\")"))
            add_txt("   Alternative (base R): aov() with Error() term")
            add_txt(paste0("   Code: aov(", safe_deparse(formula), " + Error(", id_name, "/", x_cols_vars[1], "), data=", data_name, ")"))
          } else {
            add_txt("Test: Friedman Rank-Sum Test (non-parametric repeated measures)")
            add_txt(paste0("Code: friedman.test(", y_col, " ~ ", x_cols_vars[1],
                           " | ", id_name, ", data=", data_name, ")"))
            add_txt("   Post-hoc: rstatix::wilcox_test() with pairwise paired comparisons")
          }
        } else if (y_type == "binary") {
          add_txt("Test: Chi-square Test")
          add_txt(paste0("Code: f_chisq_test(", data_name, ", '", y_col, "', '", x_cols_vars[1], "', output_type=\"", output_type, "\")"))
        } else if (y_type == "count") {
          if (is_overdispersed) {
            add_txt("Test: Negative Binomial GLM (overdispersion detected)")
            add_txt(paste0("Code: MASS::glm.nb(", safe_deparse(formula), ", data=", data_name, ")"))
            add_txt("   (No rfriend wrapper yet \u2014 requires MASS package)")
          } else {
            add_txt("Test: Poisson GLM")
            add_txt(paste0("Code: ", cf("f_glm", ", family='poisson'")))
          }
        } else if (y_type == "multinomial") {
          add_txt("Test: Chi-square Test")
          add_txt(paste0("Code: f_chisq_test(", data_name, ", '", y_col, "', '", x_cols_vars[1], "', output_type=\"", output_type, "\")"))
        } else if (y_type == "ratio_normal") {
          if (variance_equal) {
            add_txt("Test: One-Way ANOVA")
            add_txt(paste0("Code: ", cf("f_aov")))
          } else {
            add_txt("Test: One-Way ANOVA (variances unequal \u2014 see note)")
            add_txt(paste0("Code: ", cf("f_aov")))
            add_txt("   Note: Levene flagged unequal variances. f_aov() is reasonably robust")
            add_txt("   for balanced designs; for strict correctness use Welch's ANOVA:")
            add_txt(paste0("   Alternative: oneway.test(", safe_deparse(formula), ", data=", data_name, ", var.equal=FALSE)"))
          }
          recommended_call <- bquote(f_aov(.(formula), data = data, output_type = .(output_type)))
          add_txt("   Post-hoc: f_aov() includes post-hoc tests (adjust='sidak' by default)")
          add_txt("   Effect size: rstatix::eta_squared() for eta-squared")
        } else if (can_be_normalized) {
          add_txt("Test: One-Way ANOVA with automatic Box-Cox transformation")
          add_txt(paste0("Code: ", cf("f_aov")))
          add_txt("   Note: f_aov() auto-applies Box-Cox when residuals are non-normal.")
          if (!variance_equal) {
            add_txt("   Note: Variances are also unequal. Verify after transformation;")
            add_txt("   if still unequal, prefer Kruskal-Wallis below.")
          }
          add_txt("Alternative: Kruskal-Wallis Test (Non-parametric)")
          add_txt(paste0("   Code: f_kruskal_test(", safe_deparse(formula), ", data=", data_name, ", output_type=\"", output_type, "\")"))
          recommended_call <- bquote(f_aov(.(formula), data = data, output_type = .(output_type)))
        } else if (y_type == "ratio_unknown") {
          add_txt("Note: Normality could not be assessed.")
          add_txt("Option 1: ANOVA (if normality is plausible)")
          add_txt(paste0("   Code: ", cf("f_aov")))
          add_txt("Option 2: Kruskal-Wallis Test (safer)")
          add_txt(paste0("   Code: f_kruskal_test(", safe_deparse(formula), ", data=", data_name, ", output_type=\"", output_type, "\")"))
          recommended_call <- bquote(f_aov(.(formula), data = data, output_type = .(output_type)))
        } else {
          add_txt("Test: Kruskal-Wallis Test (Non-parametric)")
          add_txt(paste0("Code: f_kruskal_test(", safe_deparse(formula), ", data=", data_name, ", output_type=\"", output_type, "\")"))
          recommended_call <- bquote(f_kruskal_test(.(formula), data = data, output_type = .(output_type)))
          add_txt("   Post-hoc: rstatix::dunn_test() for pairwise comparisons")
        }
      }
    }
  }

  add_txt(" ")
  add_txt("---------------------------------------------------")
  add_txt("Disclaimer:")
  add_txt("- This wizard checks data properties, not experimental design.")
  add_txt("- Always verify if your data is Paired or Independent!")
  add_txt("- BETA: f_stat_wizard is new and under active development \u2014 verify recommendations.")
  add_txt("- Other tests than the recommended one could be more suitable.")
  add_txt("The WIZARD recommends exclusively from these rfriend functions:")
  add_txt("   f_t_test, f_aov, f_glm, f_chisq_test,")
  add_txt("   f_kruskal_test, f_wilcox_test, f_corplot,")
  add_txt("   f_boxcox (transformation), f_scan (diagnostics)")
  add_txt("And these external packages (when no rfriend wrapper exists):")
  add_txt("   oneway.test (Welch ANOVA), MASS::glm.nb (Neg. Binomial),")
  add_txt("   nnet::multinom (Multinomial), cor.test (correlation test).")

  # Check recommended packages
  report_text <- paste(report_lines, collapse = " ")
  missing_pkgs <- unique(c(
    if (grepl("nnet::multinom", report_text, fixed = TRUE) && !requireNamespace("nnet", quietly = TRUE)) "nnet",
    if (grepl("MASS::glm.nb", report_text, fixed = TRUE) && !requireNamespace("MASS", quietly = TRUE)) "MASS",
    if (grepl("emmeans::", report_text, fixed = TRUE) && !requireNamespace("emmeans", quietly = TRUE)) "emmeans",
    if (grepl("rstatix::", report_text, fixed = TRUE) && !requireNamespace("rstatix", quietly = TRUE)) "rstatix"
  ))
  if (length(missing_pkgs) > 0) {
    add_txt(" ")
    add_txt(paste0("WARNING: Package(s) not installed: ", paste(missing_pkgs, collapse = ", ")))
    add_txt(paste0("   Install with: install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))"))
  }

  # --- RUN THE RECOMMENDED TEST ---
  if (run && !is.null(recommended_call)) {
    add_txt(" ")
    add_txt("===================================================")
    add_txt("RUNNING RECOMMENDED TEST...")
    add_txt("===================================================")
    run_result <- tryCatch({
      res <- suppressMessages(suppressWarnings(eval(recommended_call)))
      add_txt(paste0("   Test executed successfully (class: ",
                     paste(class(res), collapse = ", "), ")."))
      add_txt("   Access via $run_result in the returned object.")
      res
    }, error = function(e) {
      add_txt(paste0("   Could not run test: ", conditionMessage(e)))
      add_txt("   Try running the suggested code manually.")
      NULL
    })
  } else if (run && is.null(recommended_call)) {
    add_txt(" ")
    add_txt("Note: run=TRUE was set but no single rfriend function could be auto-executed.")
    add_txt("   This can happen with multi-step recommendations or external packages.")
    add_txt("   Please run the suggested code manually.")
  }

  return(build_result(report_lines))
}


# =============================================================================
# S3 PRINT METHOD
# =============================================================================

#' Print method for f_stat_wizard
#'
#' @param x An object of class \code{f_stat_wizard}.
#' @param plots Logical. If \code{TRUE}, display diagnostic plots (histogram and
#'   QQ-plot) if they were generated with \code{plots=TRUE}. Default \code{TRUE}.
#' @param ... Additional arguments (ignored).
#' @export
print.f_stat_wizard <- function(x, plots = TRUE, ...) {
  cat(x$report, sep = "\n")

  # Display diagnostic plots if available
  if (plots) {
    if (!is.null(x$histogram) && inherits(x$histogram, "recordedplot")) {
      grDevices::replayPlot(x$histogram)
    }
    if (!is.null(x$qqplot) && inherits(x$qqplot, "recordedplot")) {
      grDevices::replayPlot(x$qqplot)
    }
  }

  invisible(x)
}
