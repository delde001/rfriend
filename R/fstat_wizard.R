#' Statistical Test Wizard
#'
#' @description
#' Analyzes your data structure based on a formula and recommends the appropriate statistical test.
#' Checks variable types, normality of residuals, homogeneity of variance, and checks if f_boxcox
#' transformation can fix non-normality. Returns a text object that can be printed or stored.
#'
#' Supports standard formulas including \code{y ~ .}, \code{y ~ as.factor(x)}, and interaction
#' terms. Formulas with random effects (e.g. \code{(1|ID)}) are detected and handled separately.
#' Multivariate responses (e.g. \code{cbind(y1, y2) ~ x}) and transformed responses
#' (e.g. \code{log(y) ~ x}) are not supported.
#'
#' @param data A data frame.
#' @param formula A formula specifying the relationship.
#' @param interactive Logical. If \code{TRUE}, asks the user questions about study design. Default \code{FALSE}.
#'
#' @return An object of class \code{"f_stat_wizard"} containing the analysis report.
#' @export
#' @import stats
#' @examples
#' f_stat_wizard(iris, Sepal.Length ~ Species)
f_stat_wizard <- function(data, formula, interactive = FALSE) {

  # --- UTILITIES ---
  safe_deparse <- function(expr) paste(deparse(expr), collapse = " ")
  data_name <- deparse(substitute(data))

  # --- 1. INPUT CHECKS ---
  if (!is.data.frame(data)) stop("Input 'data' must be a data frame.")
  if (missing(formula) || !inherits(formula, "formula")) stop("You must provide a valid formula (e.g., y ~ x).")

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

  # Detect random effects EARLY — exit before further parsing
  has_re_in_formula <- grepl("|", formula_text, fixed = TRUE)

  # Use terms() to expand y ~ . and get proper term labels
  if (has_re_in_formula) {
    x_cols_vars <- setdiff(all.vars(formula[[3]]), y_col)
    x_cols_vars <- x_cols_vars[x_cols_vars %in% names(data)]
  } else {
    tf <- tryCatch(terms(formula, data = data), error = function(e) NULL)
    if (!is.null(tf)) {
      tl <- attr(tf, "term.labels")
      # FIX (Round 5): Use all.vars() on each term label to unwrap as.factor(), poly(), etc.
      # strsplit on ":" alone returns "as.factor(x)" which is NOT a column name
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
  complete_rows <- complete.cases(data[, used_cols, drop = FALSE])
  n_dropped <- sum(!complete_rows)
  data <- data[complete_rows, , drop = FALSE]
  y_data <- data[[y_col]]

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

  # --- EARLY EXIT: Random effects in formula ---
  if (has_re_in_formula) {
    add_txt(" ")
    add_txt("-> Random effect syntax detected in formula (e.g. '(1|ID)').")
    add_txt("---------------------------------------------------")
    add_txt(" ")
    add_txt("RECOMMENDATION:")
    add_txt("Test: Linear Mixed-Effects Model (LMM)")
    add_txt(paste0("Code: f_lmer(", formula_text, ", data=", data_name, ")"))
    add_txt("   (Requires lme4 package. Use f_lmer() or lme4::lmer() directly.)")
    add_txt(" ")
    add_txt("---------------------------------------------------")
    add_txt("The wizard cannot fully analyze mixed-model formulas.")
    add_txt("Verify your random-effects structure and check model diagnostics manually.")
    class(report_lines) <- c("f_stat_wizard", "character")
    return(report_lines)
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
  x_types <- c()
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

  # NEW (Round 5): Detect ANCOVA pattern — mixed ratio + nominal predictors
  is_ancova <- n_x > 1 && has_ratio_x && has_nominal_x

  # Guard: intercept-only formula (y ~ 1)
  if (n_x == 0) {
    add_txt("-> Explanatory: None (intercept-only model).")
    add_txt("---------------------------------------------------")
    add_txt(" ")
    add_txt("RECOMMENDATION:")
    add_txt("This is a null/intercept-only model with no explanatory variables.")
    add_txt(paste0("Code: t.test(", data_name, "$", y_col, ")"))
    add_txt(" ")
    add_txt("---------------------------------------------------")
    class(report_lines) <- c("f_stat_wizard", "character")
    return(report_lines)
  }

  n_groups <- NULL
  if (n_x == 1) {
    add_txt(paste0("-> Explanatory: Single variable (", x_types[1], ")."))
    if (x_types[1] %in% c("nominal", "ordinal")) {
      grp_col <- if (!is.null(mf) && ncol(mf) > 1) mf[[2]] else data[[x_cols_vars[1]]]
      n_groups <- length(unique(grp_col))
      if (n_groups < 2) add_txt(paste0("   WARN: Only ", n_groups, " group(s) found. Cannot compare."))
    } else {
      n_groups <- 0L
    }
  } else {
    label <- if (is_ancova) "ANCOVA pattern (nominal + ratio predictors)" else paste(x_types, collapse = ", ")
    add_txt(paste0("-> Explanatory: Multiple variables (", label, ")."))
  }

  # --- HELPER: Capped Shapiro-Wilk ---
  safe_shapiro <- function(resids) {
    n <- length(resids)
    if (n < 3) {
      add_txt("   Note: Too few observations for Shapiro-Wilk (n < 3). Normality unknown.")
      return(list(p.value = NA_real_))
    }
    if (n > 5000) {
      if (exists(".Random.seed", envir = .GlobalEnv)) {
        old_seed <- .Random.seed
        on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
      } else {
        on.exit(rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
      }
      set.seed(42)
      resids <- sample(resids, 5000)
    }
    shapiro.test(resids)
  }

  # --- CLASSIFY Y (RESPONSE) ---
  y_type <- "unknown"
  can_be_normalized <- FALSE

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
    # FIX (Round 5): Re-add a reasonable ceiling + warn for borderline cases
    } else if (length(unique_vals) > 0 && all(y_data >= 0) && all(y_data == floor(y_data)) &&
               max(y_data) < 1000 && length(unique_vals) >= 3) {
      y_type <- "count"
      add_txt("-> Response: Numeric (detected as COUNT data: non-negative integers).")
      # NEW (Round 5): Dispersion check for Poisson vs NegBin
      y_mean <- mean(y_data)
      y_var <- var(y_data)
      if (y_var > 2 * y_mean) {
        add_txt(paste0("   Overdispersion detected (var/mean = ", round(y_var / y_mean, 1), ")."))
        add_txt("   -> Negative Binomial GLM recommended over Poisson.")
      } else {
        add_txt("   Poisson GLM is appropriate (no strong overdispersion).")
      }

    } else {
      # Continuous numeric — check normality
      add_txt("-> Response: Numeric. Checking normality of residuals...")

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
        shap <- safe_shapiro(resids)
        p_val <- shap$p.value

        if (is.na(p_val)) {
          add_txt("   Normality could not be assessed.")
          NA
        } else if (p_val > 0.05) {
          add_txt(paste0("   OK: Residuals appear Normal (Shapiro p=", round(p_val, 3), ")."))
          TRUE
        } else {
          add_txt(paste0("   WARN: Residuals NOT Normal (Shapiro p=", round(p_val, 3), ")."))
          # QQ-plot suggestion for borderline cases
          if (p_val > 0.01 && n_obs > 30) {
            add_txt("   Note: p is close to 0.05 with moderate n. Visual QQ-plot check recommended.")
            add_txt(paste0("   Code: qqnorm(residuals(lm(", formula_text, ", data=", data_name, "))); qqline(...)"))
          }
          # Box-Cox check
          if (all(y_data > 0)) {
            add_txt("   ... Checking if f_boxcox() can fix this ...")
            if (exists("f_boxcox", mode = "function")) {
              tryCatch({
                bc_res <- suppressMessages(f_boxcox(y_data, plots = FALSE, output_type = "off"))
                y_trans <- bc_res$transformed_data
                if (!is.null(y_trans) && length(y_trans) == length(y_data)) {
                  data_temp <- data
                  data_temp$y_trans_internal <- y_trans
                  rhs <- safe_deparse(formula[[3]])
                  new_model <- lm(as.formula(paste("y_trans_internal ~", rhs)), data = data_temp)
                  new_shap <- safe_shapiro(residuals(new_model))
                  new_p <- new_shap$p.value
                  if (!is.na(new_p) && new_p > 0.05) {
                    add_txt(paste0("   TIP: f_boxcox fixes normality! (New p=", round(new_p, 3), ")"))
                    can_be_normalized <<- TRUE
                  } else if (!is.na(new_p)) {
                    add_txt(paste0("   Note: f_boxcox did not fix normality (p=", round(new_p, 3), ")."))
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
        FALSE
      })

      if (isTRUE(is_normal))      y_type <- "ratio_normal"
      else if (is.na(is_normal))   y_type <- "ratio_unknown"
      else                         y_type <- "ratio_non_normal"

    } # end continuous numeric
  } else {
    add_txt(paste0("-> Response: Unsupported type (", class(y_data)[1], ")."))
    y_type <- "unsupported"
  }

  # --- HOMOSCEDASTICITY CHECK ---
  # FIX (Round 5): Apply to both nominal AND ordinal X
  variance_equal <- TRUE

  if (n_x == 1 && x_types[1] %in% c("nominal", "ordinal") &&
      y_type %in% c("ratio_normal", "ratio_non_normal", "ratio_unknown") &&
      !is.null(n_groups) && n_groups >= 2) {
    tryCatch({
      grp <- as.factor(if (!is.null(mf) && ncol(mf) > 1) mf[[2]] else data[[x_cols_vars[1]]])
      if (requireNamespace("rstatix", quietly = TRUE)) {
        lev_df <- data.frame(.y = y_data, .group = grp)
        lev <- rstatix::levene_test(lev_df, .y ~ .group)
        lev_p <- lev$p[1]
        if (lev_p > 0.05) {
          add_txt(paste0("   OK: Variances appear equal (Levene p=", round(lev_p, 3), ")."))
        } else {
          add_txt(paste0("   WARN: Variances UNEQUAL (Levene p=", round(lev_p, 3), ")."))
          variance_equal <- FALSE
        }
      } else {
        bart_df <- data.frame(.y = y_data, .g = grp)
        bart <- bartlett.test(.y ~ .g, data = bart_df)
        if (bart$p.value > 0.05) {
          add_txt(paste0("   OK: Variances appear equal (Bartlett p=", round(bart$p.value, 3), ")."))
        } else {
          add_txt(paste0("   WARN: Variances UNEQUAL (Bartlett p=", round(bart$p.value, 3), ")."))
          add_txt("   Note: Bartlett is sensitive to non-normality. Install 'rstatix' for Levene's test.")
          variance_equal <- FALSE
        }
      }
      if (!variance_equal) add_txt("   -> Welch's variants will be recommended.")
    }, error = function(e) {
      add_txt("   Note: Could not perform variance equality test.")
    })
  }

  add_txt("---------------------------------------------------")
  add_txt(" ")
  add_txt("RECOMMENDATION:")

  # --- HELPER: Code template with user's data name ---
  cf <- function(fn, ...) {
    extra <- paste0(list(...), collapse = "")
    paste0(fn, "(", safe_deparse(formula), ", data=", data_name, extra, ")")
  }

  # --- RECOMMENDATIONS ---

  if (y_type == "unsupported") {
    add_txt("No recommendation available for this response type.")

  } else if (has_random_effects) {
    add_txt("Test: Linear Mixed-Effects Model (LMM)")
    rhs <- safe_deparse(formula[[3]])
    re <- if (!is.null(random_var) && nzchar(random_var)) random_var else "SubjectID"
    add_txt(paste0("Code: f_lmer(", y_col, " ~ ", rhs, " + (1|", re, "), data=", data_name, ")"))

  # === MULTIPLE X ===
  } else if (n_x > 1) {
    if (y_type == "binary") {
      add_txt("Test: Multiple Logistic Regression")
      add_txt(paste0("Code: ", cf("f_glm", ", family='binomial'")))
    } else if (y_type == "count") {
      # FIX (Round 5): Recommend Poisson vs NegBin based on dispersion
      if (var(y_data) > 2 * mean(y_data)) {
        add_txt("Test: Negative Binomial GLM (overdispersion detected)")
        add_txt(paste0("Code: MASS::glm.nb(", safe_deparse(formula), ", data=", data_name, ")"))
      } else {
        add_txt("Test: Poisson GLM")
        add_txt(paste0("Code: ", cf("f_glm", ", family='poisson'")))
      }
    } else if (y_type == "multinomial") {
      add_txt("Test: Multinomial Logistic Regression")
      add_txt(paste0("Code: nnet::multinom(", safe_deparse(formula), ", data=", data_name, ")"))
    } else if (y_type == "ratio_normal") {
      # NEW (Round 5): Detect ANCOVA
      if (is_ancova) {
        add_txt("Test: ANCOVA (Analysis of Covariance)")
        add_txt("   Your model combines categorical and continuous predictors.")
      } else {
        add_txt("Test: Multi-way ANOVA or Linear Regression")
      }
      add_txt(paste0("Code: ", cf("f_aov")))
      # NEW (Round 5): Suggest post-hoc for multi-X ANOVA
      if (has_nominal_x) {
        add_txt("   Post-hoc: Use emmeans::emmeans() for pairwise comparisons")
      }
    } else if (can_be_normalized) {
      add_txt("Option 1 (Traditional): Transformation + ANOVA")
      add_txt(paste0("   Step 1: ", data_name, "$", y_col, "_t <- f_boxcox(", data_name, "$", y_col, ")$transformed_data"))
      add_txt(paste0("   Step 2: f_aov(", y_col, "_t ~ ", safe_deparse(formula[[3]]), ", data=", data_name, ")"))
      add_txt("Option 2 (Alternative): GLM")
      if (all(y_data > 0, na.rm = TRUE)) {
        add_txt(paste0("   Code: ", cf("f_glm", ", family=Gamma(link='log')")))
      } else {
        add_txt("   Consider robust regression or a GLM family suited to your data.")
      }
    } else if (y_type == "ratio_unknown") {
      add_txt("Note: Normality could not be assessed (too few observations).")
      add_txt("Option 1: Linear Regression (if normality is plausible)")
      add_txt(paste0("   Code: ", cf("f_aov")))
      add_txt("Option 2: Non-parametric or GLM approach (safer)")
    } else {
      add_txt("Test: Generalized Linear Model (GLM)")
      if (all(y_data > 0, na.rm = TRUE)) {
        add_txt(paste0("Code: ", cf("f_glm", ", family=Gamma(link='log')")))
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
        if (var(y_data) > 2 * mean(y_data)) {
          add_txt("Test: Negative Binomial Regression (overdispersion detected)")
          add_txt(paste0("Code: MASS::glm.nb(", safe_deparse(formula), ", data=", data_name, ")"))
        } else {
          add_txt("Test: Poisson Regression")
          add_txt(paste0("Code: ", cf("f_glm", ", family='poisson'")))
        }
      } else if (y_type == "multinomial") {
        add_txt("Test: Multinomial Logistic Regression")
        add_txt(paste0("Code: nnet::multinom(", safe_deparse(formula), ", data=", data_name, ")"))
      } else if (y_type == "ratio_normal" || can_be_normalized) {
        if (can_be_normalized) add_txt("Tip: Use f_boxcox() first, then:")
        add_txt("Test: Linear Regression / Pearson Correlation")
        # FIX (Round 5): Add code templates (was missing)
        add_txt(paste0("Code: lm(", safe_deparse(formula), ", data=", data_name, ")"))
        add_txt(paste0("   Or: cor.test(", data_name, "$", x_cols_vars[1], ", ", data_name, "$", y_col, ", method='pearson')"))
      } else if (y_type == "ratio_unknown") {
        add_txt("Note: Normality could not be assessed.")
        add_txt("Test: Linear Regression (if plausible) or Spearman Correlation (non-parametric)")
        add_txt(paste0("Code: cor.test(", data_name, "$", x_cols_vars[1], ", ", data_name, "$", y_col, ", method='spearman')"))
      } else {
        add_txt("Test: Spearman Correlation or GLM")
        add_txt(paste0("Code: cor.test(", data_name, "$", x_cols_vars[1], ", ", data_name, "$", y_col, ", method='spearman')"))
      }

    # X is Ordinal
    } else if (x_types[1] == "ordinal") {
      if (y_type == "binary") {
        add_txt("Test: Logistic Regression with ordinal contrast")
        add_txt(paste0("Code: ", cf("f_glm", ", family='binomial'")))
      } else if (y_type == "count") {
        if (var(y_data) > 2 * mean(y_data)) {
          add_txt("Test: Negative Binomial GLM with ordinal predictor")
          add_txt(paste0("Code: MASS::glm.nb(", safe_deparse(formula), ", data=", data_name, ")"))
        } else {
          add_txt("Test: Poisson GLM with ordinal predictor")
          add_txt(paste0("Code: ", cf("f_glm", ", family='poisson'")))
        }
      } else if (y_type == "multinomial") {
        add_txt("Test: Multinomial Logistic Regression")
        add_txt(paste0("Code: nnet::multinom(", safe_deparse(formula), ", data=", data_name, ")"))
      } else if (y_type == "ratio_normal") {
        add_txt("Test: Linear Regression with polynomial contrasts (ordinal X)")
        add_txt(paste0("Code: lm(", safe_deparse(formula), ", data=", data_name, ")"))
        add_txt("   Alternatively: Kruskal-Wallis treating groups as unordered")
      # FIX (Round 5): Non-normal Y with ordinal X should suggest non-parametric
      } else if (y_type == "ratio_non_normal") {
        add_txt("Test: Kruskal-Wallis Test (non-normal Y with ordinal X)")
        add_txt(paste0("Code: f_kruskal_test(", safe_deparse(formula), ", data=", data_name, ")"))
        add_txt("   Alternative: Jonckheere-Terpstra trend test (if order matters)")
      } else if (y_type == "ratio_unknown") {
        add_txt("Note: Normality could not be assessed.")
        add_txt("Option 1: Linear Regression with polynomial contrasts")
        add_txt("Option 2: Kruskal-Wallis (safer)")
        add_txt(paste0("   Code: f_kruskal_test(", safe_deparse(formula), ", data=", data_name, ")"))
      }

    # X is Nominal
    } else {
      if (is.null(n_groups) || n_groups < 2) {
        add_txt("   Note: < 2 groups found. Cannot compare groups.")

      # --- 2 GROUPS ---
      } else if (n_groups == 2) {
        if (y_type == "binary") {
          add_txt("Test: Chi-square / Fisher's Exact")
          add_txt(paste0("Code: f_chisq_test(", data_name, ", '", y_col, "', '", x_cols_vars[1], "')"))
        } else if (y_type == "count") {
          if (var(y_data) > 2 * mean(y_data)) {
            add_txt("Test: Negative Binomial GLM (overdispersion detected)")
            add_txt(paste0("Code: MASS::glm.nb(", safe_deparse(formula), ", data=", data_name, ")"))
          } else {
            add_txt("Test: Poisson GLM")
            add_txt(paste0("Code: ", cf("f_glm", ", family='poisson'")))
          }
        } else if (y_type == "multinomial") {
          add_txt("Test: Chi-square / Fisher's Exact")
          add_txt(paste0("Code: f_chisq_test(", data_name, ", '", y_col, "', '", x_cols_vars[1], "')"))
        } else if (y_type == "ratio_normal") {
          # NEW (Round 5): Default to Welch's t-test (modern standard)
          add_txt("Test: Welch's T-test (default; robust to unequal variances)")
          add_txt(paste0("Code: f_ttest(", data_name, ", value_col='", y_col, "', group_col='", x_cols_vars[1], "')"))
          if (variance_equal) {
            add_txt("   (Variances appear equal; Student's t-test also valid with var.equal=TRUE)")
          }
          add_txt("      (If paired: use paired=TRUE and provide id_col)")
          # NEW (Round 5): Effect size suggestion
          add_txt("   Effect size: rstatix::cohens_d() for Cohen's d")
        } else if (can_be_normalized) {
          add_txt("Option 1 (Traditional): Transformation + T-test")
          add_txt(paste0("   Step 1: ", data_name, "$", y_col, "_t <- f_boxcox(", data_name, "$", y_col, ")$transformed_data"))
          add_txt(paste0("   Step 2: f_ttest(", data_name, ", value_col='", y_col, "_t', group_col='", x_cols_vars[1], "')"))
          add_txt("Option 2: Mann-Whitney U / Wilcoxon (Non-parametric)")
          # FIX (Round 5): Add Mann-Whitney code (was missing)
          add_txt(paste0("   Code: wilcox.test(", y_col, " ~ ", x_cols_vars[1], ", data=", data_name, ")"))
        } else if (y_type == "ratio_unknown") {
          add_txt("Note: Normality could not be assessed.")
          add_txt("Option 1: Welch's T-test (if normality is plausible)")
          add_txt("Option 2: Mann-Whitney U / Wilcoxon (safer)")
          add_txt(paste0("   Code: wilcox.test(", y_col, " ~ ", x_cols_vars[1], ", data=", data_name, ")"))
        } else {
          add_txt("Test: Mann-Whitney U / Wilcoxon (Non-parametric)")
          add_txt(paste0("Code: wilcox.test(", y_col, " ~ ", x_cols_vars[1], ", data=", data_name, ")"))
        }

      # --- >2 GROUPS ---
      } else if (n_groups > 2) {
        if (y_type == "binary") {
          add_txt("Test: Chi-square Test")
          add_txt(paste0("Code: f_chisq_test(", data_name, ", '", y_col, "', '", x_cols_vars[1], "')"))
        } else if (y_type == "count") {
          if (var(y_data) > 2 * mean(y_data)) {
            add_txt("Test: Negative Binomial GLM (overdispersion detected)")
            add_txt(paste0("Code: MASS::glm.nb(", safe_deparse(formula), ", data=", data_name, ")"))
          } else {
            add_txt("Test: Poisson GLM")
            add_txt(paste0("Code: ", cf("f_glm", ", family='poisson'")))
          }
        } else if (y_type == "multinomial") {
          add_txt("Test: Chi-square Test")
          add_txt(paste0("Code: f_chisq_test(", data_name, ", '", y_col, "', '", x_cols_vars[1], "')"))
        } else if (y_type == "ratio_normal") {
          if (variance_equal) {
            add_txt("Test: One-Way ANOVA")
            add_txt(paste0("Code: ", cf("f_aov")))
          } else {
            add_txt("Test: Welch's One-Way ANOVA (unequal variances)")
            add_txt(paste0("Code: oneway.test(", safe_deparse(formula), ", data=", data_name, ", var.equal=FALSE)"))
          }
          # NEW (Round 5): Post-hoc test suggestion
          add_txt("   Post-hoc: TukeyHSD() after aov, or rstatix::tukey_hsd()")
          # NEW (Round 5): Effect size
          add_txt("   Effect size: rstatix::eta_squared() for eta-squared")
        } else if (can_be_normalized) {
          add_txt("Option 1 (Traditional): Transformation + ANOVA")
          add_txt(paste0("   Step 1: ", data_name, "$", y_col, "_t <- f_boxcox(", data_name, "$", y_col, ")$transformed_data"))
          add_txt(paste0("   Step 2: f_aov(", y_col, "_t ~ ", safe_deparse(formula[[3]]), ", data=", data_name, ")"))
          add_txt("Option 2: Kruskal-Wallis Test (Non-parametric)")
          add_txt(paste0("   Code: f_kruskal_test(", safe_deparse(formula), ", data=", data_name, ")"))
        } else if (y_type == "ratio_unknown") {
          add_txt("Note: Normality could not be assessed.")
          add_txt("Option 1: ANOVA (if normality is plausible)")
          add_txt("Option 2: Kruskal-Wallis Test (safer)")
          add_txt(paste0("   Code: f_kruskal_test(", safe_deparse(formula), ", data=", data_name, ")"))
        } else {
          add_txt("Test: Kruskal-Wallis Test (Non-parametric)")
          add_txt(paste0("Code: f_kruskal_test(", safe_deparse(formula), ", data=", data_name, ")"))
          # NEW (Round 5): Post-hoc for Kruskal-Wallis
          add_txt("   Post-hoc: rstatix::dunn_test() for pairwise comparisons")
        }
      }
    }
  }

  add_txt(" ")
  add_txt("---------------------------------------------------")
  add_txt("Disclaimer: This wizard checks data properties, not experimental design.")
  add_txt("Always verify if your data is Paired or Independent!")
  add_txt("The WIZARD is limited and only chooses from:")
  add_txt("   t.test (Welch default), aov, oneway.test, lm,")
  add_txt("   cor.test (Pearson/Spearman), f_chisq_test,")
  add_txt("   glm (Gamma/Poisson/NegBin), f_kruskal_test,")
  add_txt("   wilcox.test (Mann-Whitney/Wilcoxon), f_lmer,")
  add_txt("   nnet::multinom (multinomial logistic regression)")

  # Check recommended packages
  report_text <- paste(report_lines, collapse = " ")
  missing_pkgs <- unique(c(
    if (grepl("nnet::multinom", report_text, fixed = TRUE) && !requireNamespace("nnet", quietly = TRUE)) "nnet",
    if (grepl("MASS::glm.nb", report_text, fixed = TRUE) && !requireNamespace("MASS", quietly = TRUE)) "MASS",
    if (grepl("emmeans::", report_text, fixed = TRUE) && !requireNamespace("emmeans", quietly = TRUE)) "emmeans"
  ))
  if (length(missing_pkgs) > 0) {
    add_txt(" ")
    add_txt(paste0("WARNING: Package(s) not installed: ", paste(missing_pkgs, collapse = ", ")))
    add_txt(paste0("   Install with: install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))"))
  }

  class(report_lines) <- c("f_stat_wizard", "character")
  return(report_lines)
}

#' Print method for f_stat_wizard
#'
#' @param x An object of class \code{f_stat_wizard}.
#' @param ... Additional arguments (ignored).
#' @export
print.f_stat_wizard <- function(x, ...) {
  cat(x, sep = "\n")
  invisible(x)
}
