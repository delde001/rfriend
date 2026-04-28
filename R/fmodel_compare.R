#' Compare Two Statistical Models
#'
#' Compares two statistical models by calculating key metrics such as AIC, BIC, log-likelihood, \eqn{R^2},
#' and others. Supports comparison of nested models using ANOVA tests.
#'
#' @param model1 The first model object. Supported classes include: \code{"lm", "glm", "aov", "lmerMod", "glmerMod", and "nls"}.
#' @param model2 The second model object. Supported classes include: \code{"lm", "glm", "aov", "lmerMod", "glmerMod", and "nls"}.
#' @param nested Logical. If \code{TRUE}, assumes the models are nested and performs an ANOVA comparison.
#'   If \code{NULL} (default), the function attempts to automatically determine if the models are nested.
#' @param model1_name Optional character string. A custom name for model1 in the output. If \code{NULL} (default),
#'   the function uses \code{deparse(substitute(model1))}.
#' @param model2_name Optional character string. A custom name for model2 in the output. If \code{NULL} (default),
#'   the function uses \code{deparse(substitute(model2))}.
#' @param digits Integer. The number of decimal places to round the output metrics. Defaults to \code{3}.
#'
#' @return A list of class "f_model_comparison" containing:
#'   \item{model1_name}{The name of the first model (always the simpler model when nested).}
#'   \item{model2_name}{The name of the second model (always the more complex model when nested).}
#'   \item{model1_class}{The class of the first model.}
#'   \item{model2_class}{The class of the second model.}
#'   \item{metrics_table}{A data frame summarizing metrics for both models, their differences, and (if applicable) the ANOVA p-value.}
#'   \item{formatted_metrics_table}{A formatted version of the metrics table for printing.}
#'   \item{anova_comparison}{The ANOVA comparison results if the models are nested and an ANOVA test was performed.}
#'   \item{nested}{Logical indicating whether the models were treated as nested.}
#'   \item{swapped}{Logical indicating whether the model order was swapped to ensure model1 is the simpler model.}
#'
#' @section Supported Model Classes:
#' The function supports the following model classes:
#'   \itemize{
#'     \item Linear models ("lm")
#'     \item Generalized linear models ("glm")
#'     \item Analysis of variance models ("aov")
#'     \item Linear mixed models ("lmerMod")
#'     \item Generalized linear mixed models ("glmerMod")
#'     \item Nonlinear least squares models ("nls")
#'   }
#'
#' Note: Multi-stratum AOV models (fitted with \code{Error()}) are not supported
#' and will produce a warning.
#'
#' @details
#' Calculate various metrics to assess model fit:
#'   \itemize{
#'     \item \bold{AIC/BIC:} Lower values indicate better fit.
#'     \item \bold{Log-Likelihood:} Higher values (less negative) indicate better fit.
#'     \item \bold{\eqn{R^2}:} Proportion of variance explained by the model.
#'     \item \bold{Adjusted \eqn{R^2}:} \eqn{R^2} penalized for the number of parameters (for linear models).
#'     \item \bold{Nagelkerke \eqn{R^2}:} A pseudo-\eqn{R^2} for generalized linear models (GLMs).
#'     \item \bold{Marginal/Conditional \eqn{R^2}:} For mixed models, marginal \eqn{R^2} reflects fixed effects, while conditional \eqn{R^2} includes random effects.
#'     \item \bold{Sigma:} Residual standard error.
#'     \item \bold{Deviance:} Model deviance.
#'     \item \bold{SSE:} Sum of squared errors.
#'     \item \bold{Parameters (df):} Number of model parameters.
#'     \item \bold{Residual df:} Residual degrees of freedom.
#'   }
#'
#' When nested models are detected or specified, model1 is always treated as the simpler
#' model (fewer parameters). If the user passes the complex model first, the function
#' automatically swaps them and issues a message.
#'
#' If the models are nested, an ANOVA test is performed to compare them, and a p-value
#' is provided to assess whether the more complex model significantly improves fit.
#'
#' @note
#'   \itemize{
#'     \item The function supports a variety of model types but may issue warnings if unsupported or partially supported classes are used.
#'     \item For GLMs, Nagelkerke's \eqn{R^2} is used as a pseudo-\eqn{R^2} approximation, computed
#'       from the model's null deviance to avoid refitting a null model.
#'     \item For mixed models, the function relies on the 'r.squaredGLMM' function from the 'MuMIn' package for \eqn{R^2} calculation.
#'     \item For NLS models, \eqn{R^2} is provided for convenience but should be interpreted with caution
#'       as it does not have the same statistical properties as in linear models.
#'     \item The idea of this function (not the code), I got from Dustin Fife's function \href{https://github.com/dustinfife/flexplot/blob/master/R/model.comparison.R}{'model.comparison'} in the super cool \href{https://github.com/dustinfife/flexplot/}{'flexplot package'}.
#'   }
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#'
#' @examples
#' # Example with linear models.
#' model1 <- lm(mpg ~ wt, data = mtcars)
#' model2 <- lm(mpg ~ wt + hp, data = mtcars)
#' comparison <- f_model_compare(model1, model2)
#' print(comparison)
#'
#' # Example with GLMs.
#' \donttest{
#' model1 <- glm(am ~ wt, data = mtcars, family = binomial)
#' model2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' comparison <- f_model_compare(model1, model2)
#' print(comparison)
#' }
#'
#' # Models can be passed in any order - the function auto-swaps if needed.
#' complex <- lm(mpg ~ wt + hp + qsec, data = mtcars)
#' simple  <- lm(mpg ~ wt, data = mtcars)
#' comparison <- f_model_compare(complex, simple)
#' # model1 will be "simple", model2 will be "complex" in the output
#'
#' # Example with custom model names (useful when calling from wrapper functions).
#' comparison <- f_model_compare(model1, model2,
#'                               model1_name = "Weight only",
#'                               model2_name = "Weight + Horsepower")
#' print(comparison)
#'
#' @seealso
#' \code{\link{AIC}}, \code{\link{BIC}}, \code{\link{anova}}, \code{\link{logLik}}, \code{\link[MuMIn]{r.squaredGLMM}}
#'
#' @export


f_model_compare <- function(model1, model2, nested = NULL,
                            model1_name = NULL, model2_name = NULL,
                            digits = 3) {

  # ---- Identify model classes using inherits() ----
  # Order matters: check more specific classes first (glm before lm, aov before lm)
  supported_classes <- c("glmerMod", "lmerMod", "glm", "aov", "nls", "lm")

  get_model_class <- function(model) {
    for (cls in supported_classes) {
      if (inherits(model, cls)) return(cls)
    }
    return(class(model)[1])  # fallback for unsupported classes
  }

  # ---- Guard against aovlist (multi-stratum Error() models) ----
  if (inherits(model1, "aovlist") || inherits(model2, "aovlist")) {
    warning(
      "Multi-stratum aov models (fitted with Error()) are not supported. ",
      "The summary structure differs from standard aov. Results may be unreliable."
    )
  }

  m1_class <- get_model_class(model1)
  m2_class <- get_model_class(model2)

  if (!m1_class %in% supported_classes || !m2_class %in% supported_classes) {
    warning("One or both models are not of a supported class. Results may be unreliable.")
  }

  # ---- Get model names (before potential swap) ----
  # deparse(substitute()) must be called before any reassignment
  if (is.null(model1_name)) {
    m1_name <- deparse(substitute(model1))
  } else {
    m1_name <- model1_name
  }
  if (is.null(model2_name)) {
    m2_name <- deparse(substitute(model2))
  } else {
    m2_name <- model2_name
  }

  # ---- Validate that models were fit on the same number of observations ----
  n1 <- tryCatch(nobs(model1), error = function(e) NA)
  n2 <- tryCatch(nobs(model2), error = function(e) NA)
  if (!is.na(n1) && !is.na(n2) && n1 != n2) {
    warning(
      "Models were fit on different numbers of observations (", n1, " vs ", n2, "). ",
      "Comparisons of AIC, BIC, and likelihood-based metrics are not valid. ",
      "This can happen due to different NA handling."
    )
  }

  # ---- Determine if models are nested ----
  # Use isTRUE() to safely handle NULL default for nested
  if (m1_class != m2_class && isTRUE(nested)) {
    nested <- FALSE
    warning("Models are not of the same class and thus can't be nested. Assuming non-nested models.")
  }

  # Auto-detect if models are nested if not specified by the user
  if (is.null(nested)) {
    if (m1_class != m2_class) {
      nested <- FALSE
    } else {
      tryCatch({
        check_nested <- is_nested(model1, model2)
        nested       <- check_nested$nested
        if (!nested) {
          reason_message <- "Automatically determined if models were nested and found:\n"
          message(paste0(reason_message, check_nested$reason))
        }
      }, error = function(e) {
        warning(
          "Could not automatically determine if models are nested. ",
          "Assuming non-nested models. If the models ARE nested use the option: nested = TRUE"
        )
        nested <<- FALSE
      })
    }
  }

  # Safety net: if nested is still NULL at this point, default to FALSE
  if (is.null(nested)) nested <- FALSE


  # ---- Ensure model1 is the simpler model (fewer parameters) ----
  # This guarantees correct sign on differences and valid ANOVA ordering.
  swapped <- FALSE

  get_df <- function(model) {
    mc <- get_model_class(model)
    if (mc %in% c("lmerMod", "glmerMod")) {
      tryCatch(attr(stats::logLik(model), "df"), error = function(e) NA)
    } else {
      tryCatch(sum(!is.na(stats::coef(model))), error = function(e) NA)
    }
  }

  df1 <- get_df(model1)
  df2 <- get_df(model2)

  if (!is.na(df1) && !is.na(df2) && df1 > df2) {
    # Swap models so model1 is always simpler
    tmp_model <- model1;  model1 <- model2;  model2 <- tmp_model
    tmp_name  <- m1_name; m1_name <- m2_name; m2_name <- tmp_name
    tmp_class <- m1_class; m1_class <- m2_class; m2_class <- tmp_class
    swapped <- TRUE
    message(
      "Note: Model input order was swapped, applied automated FIX:\n
      - model1 ('", m1_name,"', df=", df2, ") is the simpler model\n
      - model2 ('", m2_name,"', df=", df1, ") is the more complex model."
    )
  }


  # ---- Function to safely extract metrics ----
  get_metrics <- function(model) {
    model_class <- get_model_class(model)

    metrics <- list(
      AIC = NA,
      BIC = NA,
      logLik = NA,
      r.squared = NA,
      adj.r.squared = NA,
      sigma = NA,
      deviance = NA,
      SSE = NA,
      df = NA,
      df.residual = NA
    )

    # AIC and BIC
    metrics$AIC <- tryCatch(stats::AIC(model), error = function(e) NA)
    metrics$BIC <- tryCatch(stats::BIC(model), error = function(e) NA)

    # Log-likelihood
    metrics$logLik <- tryCatch(as.numeric(stats::logLik(model)), error = function(e) NA)

    # ---- R-squared and adjusted R-squared ----
    if (model_class == "aov") {
      anova_table <- summary(model)
      SSR <- utils::tail(anova_table[[1]]$"Sum Sq", 1)  # Residual sum of squares
      SST <- sum(anova_table[[1]]$"Sum Sq")              # Total sum of squares
      n   <- nobs(model)
      # Model df = sum of all factor df (everything except the residual row)
      df_model <- sum(utils::head(anova_table[[1]]$"Df", -1))

      metrics$r.squared     <- 1 - (SSR / SST)
      metrics$adj.r.squared <- 1 - ((1 - metrics$r.squared) * (n - 1) / (n - df_model - 1))

    } else if (model_class == "nls") {
      n              <- nobs(model)
      df_reg         <- length(stats::coef(model))
      fitted_vals    <- stats::fitted(model)
      residuals_vals <- stats::residuals(model)
      y_reconstructed      <- fitted_vals + residuals_vals
      mean_y_reconstructed <- mean(y_reconstructed, na.rm = TRUE)
      SST <- sum((y_reconstructed - mean_y_reconstructed)^2)
      SSR <- sum(stats::residuals(model)^2)

      metrics$r.squared     <- 1 - (SSR / SST)
      metrics$adj.r.squared <- 1 - ((1 - metrics$r.squared) * (n - 1) / (n - df_reg - 1))

      # Warn about NLS R-squared limitations
      message(
        "Note: R\u00b2 for nonlinear models (nls) does not have the same ",
        "statistical properties as for linear models. Interpret with caution."
      )

    } else if (model_class == "lm") {
      # Cache summary() to avoid computing it twice
      s <- summary(model)
      metrics$r.squared     <- s$r.squared
      metrics$adj.r.squared <- s$adj.r.squared

    } else if (model_class == "glm") {
      # Nagelkerke's R^2 using null.deviance from the model object directly.
      # This is safer than update(model, . ~ 1) which can fail with offsets or subsets.
      tryCatch({
        ll_full <- as.numeric(stats::logLik(model))
        n       <- nobs(model)

        # Reconstruct null log-likelihood from null.deviance
        # For GLMs: null.deviance = -2 * (ll_null - ll_saturated)
        #           deviance      = -2 * (ll_full - ll_saturated)
        # So: ll_null = ll_full - (null.deviance - deviance) / 2
        ll_null <- ll_full - (model$null.deviance - model$deviance) / 2

        cox_snell     <- 1 - exp((2 / n) * (ll_null - ll_full))
        max_cox_snell <- 1 - exp((2 / n) * ll_null)
        metrics$r.squared     <- cox_snell / max_cox_snell
        metrics$adj.r.squared <- NA
      }, error = function(e) {
        # Defaults are already NA
        NULL
      })

    } else if (model_class %in% c("lmerMod", "glmerMod")) {
      # For mixed models, calculate marginal and conditional R^2
      tryCatch({
        if (!requireNamespace("MuMIn", quietly = TRUE)) {
          warning("Package 'MuMIn' is required for R\u00b2 of mixed models. Install it with install.packages('MuMIn').")
        } else {
          r2 <- MuMIn::r.squaredGLMM(model)
          metrics$r.squared     <- r2[1]  # Marginal R^2 (fixed effects)
          metrics$adj.r.squared <- r2[2]  # Conditional R^2 (fixed + random)
        }
      }, error = function(e) {
        # Defaults are already NA
        NULL
      })

    } else {
      metrics$r.squared     <- NA
      metrics$adj.r.squared <- NA
    }

    # ---- Sigma (residual standard error) ----
    if (model_class %in% c("lm", "aov", "glm", "nls")) {
      metrics$sigma <- tryCatch(stats::sigma(model), error = function(e) NA)
    } else {
      metrics$sigma <- tryCatch(
        sqrt(sum(stats::residuals(model, type = "pearson")^2) / stats::df.residual(model)),
        error = function(e) NA
      )
    }

    # Deviance
    #
    # For REML-fit mixed models, stats::deviance() is deprecated (lme4 >= 1.1-28)
    # and emits a warning. The historical behaviour of deviance() on a REML fit
    # was to return the REML criterion, so we call lme4::REMLcrit() explicitly.
    # For ML-fit mixed models and all other model classes, stats::deviance()
    # is the correct call.
    metrics$deviance <- tryCatch({
      if (inherits(model, c("lmerMod", "glmerMod")) &&
          isTRUE(lme4::isREML(model))) {
        lme4::REMLcrit(model)
      } else {
        stats::deviance(model)
      }
    }, error = function(e) NA)

    # ---- Degrees of freedom ----
    if (model_class %in% c("lmerMod", "glmerMod")) {
      metrics$df <- tryCatch(attr(stats::logLik(model), "df"), error = function(e) NA)
    } else {
      # Handle NA coefficients in models like lm with singular fits
      metrics$df <- tryCatch(sum(!is.na(stats::coef(model))), error = function(e) NA)
    }

    # Residual degrees of freedom
    metrics$df.residual <- tryCatch(stats::df.residual(model), error = function(e) NA)

    # ---- SSE (Sum of Squared Errors) ----
    if (model_class %in% c("lm", "aov", "lmerMod", "nls")) {
      metrics$SSE <- tryCatch(
        sum(stats::residuals(model, type = "response")^2),
        error = function(e) NA
      )
    } else if (model_class %in% c("glm", "glmerMod")) {
      if (stats::family(model)$family == "gaussian") {
        metrics$SSE <- tryCatch(stats::deviance(model), error = function(e) NA)
      } else {
        metrics$SSE <- NA
      }
    } else {
      metrics$SSE <- tryCatch(sum(stats::residuals(model)^2), error = function(e) NA)
    }

    return(metrics)
  }

  # ---- Get metrics for both models ----
  m1_metrics <- get_metrics(model1)
  m2_metrics <- get_metrics(model2)

  # ---- Calculate differences ----
  diff_metrics <- list(
    AIC           = m2_metrics$AIC - m1_metrics$AIC,
    BIC           = m2_metrics$BIC - m1_metrics$BIC,
    logLik        = m2_metrics$logLik - m1_metrics$logLik,
    r.squared     = m2_metrics$r.squared - m1_metrics$r.squared,
    adj.r.squared = m2_metrics$adj.r.squared - m1_metrics$adj.r.squared,
    sigma         = m2_metrics$sigma - m1_metrics$sigma,
    deviance      = m2_metrics$deviance - m1_metrics$deviance,
    SSE           = m2_metrics$SSE - m1_metrics$SSE,
    df            = m2_metrics$df - m1_metrics$df,
    df.residual   = m2_metrics$df.residual - m1_metrics$df.residual
  )

  # ---- Perform nested model comparison if applicable ----
  anova_result <- NULL
  p_value <- NA

  if (isTRUE(nested)) {
    # Determine test type
    test_type <- if (m1_class == "glm") {
      ifelse(stats::family(model1)$family == "binomial", "Chisq", "F")
    } else if (m1_class %in% c("lmerMod", "glmerMod")) {
      "Chisq"
    } else if (m1_class == "nls") {
      "F"
    } else {
      "none"
    }

    # Perform ANOVA comparison with error handling
    anova_result <- tryCatch({
      if (test_type %in% c("Chisq", "F")) {
        stats::anova(model1, model2, test = test_type)
      } else {
        stats::anova(model1, model2)
      }
    }, error = function(e) {
      warning("Model comparison failed: ", e$message)
      return(NULL)
    })

    # Extract p-value based on model type
    p_value <- tryCatch({
      if (m1_class %in% c("lmerMod", "glmerMod")) {
        stats::pchisq(anova_result$Chisq[2], df = anova_result$Df[2], lower.tail = FALSE)
      } else if (!is.null(anova_result$`Pr(>F)`)) {
        anova_result[nrow(anova_result), "Pr(>F)"]
      } else if (!is.null(anova_result$`Pr(>Chi)`)) {
        anova_result[nrow(anova_result), "Pr(>Chi)"]
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
  }


  # ---- Create metric labels depending on model class ----
  # Use model1's class for label selection (model2 should match for nested,
  # and for non-nested comparisons the labels are a best-effort representation)
  metric_labels <- if (m1_class %in% c("glm")) {
    c("AIC", "BIC", "Log-Likelihood", "Nagelkerke R\u00b2", "",
      "Sigma", "Deviance", "SSE", "Parameters (df)", "Residual df", "ANOVA p-value")
  } else if (m1_class %in% c("lmerMod", "glmerMod")) {
    c("AIC", "BIC", "Log-Likelihood", "Marginal R\u00b2", "Conditional R\u00b2",
      "Sigma", "Deviance", "SSE", "Parameters (df)", "Residual df", "ANOVA p-value")
  } else {
    # Default for lm, aov, nls, and unsupported classes
    c("AIC", "BIC", "Log-Likelihood", "R\u00b2", "Adj. R\u00b2",
      "Sigma", "Deviance", "SSE", "Parameters (df)", "Residual df", "ANOVA p-value")
  }


  # ---- Create output table ----
  metrics_table <- data.frame(
    Metric     = metric_labels,
    Model1     = c(m1_metrics$AIC, m1_metrics$BIC, m1_metrics$logLik,
                   m1_metrics$r.squared, m1_metrics$adj.r.squared,
                   m1_metrics$sigma, m1_metrics$deviance, m1_metrics$SSE,
                   m1_metrics$df, m1_metrics$df.residual, NA),
    Model2     = c(m2_metrics$AIC, m2_metrics$BIC, m2_metrics$logLik,
                   m2_metrics$r.squared, m2_metrics$adj.r.squared,
                   m2_metrics$sigma, m2_metrics$deviance, m2_metrics$SSE,
                   m2_metrics$df, m2_metrics$df.residual, NA),
    Difference = c(diff_metrics$AIC, diff_metrics$BIC, diff_metrics$logLik,
                   diff_metrics$r.squared, diff_metrics$adj.r.squared,
                   diff_metrics$sigma, diff_metrics$deviance, diff_metrics$SSE,
                   diff_metrics$df, diff_metrics$df.residual,
                   p_value),
    stringsAsFactors = FALSE
  )

  # Filter out ANOVA row when there is no p-value
  if (is.na(p_value)) {
    metrics_table <- metrics_table[metrics_table$Metric != "ANOVA p-value", ]
  }

  # Remove rows where the metric label is empty (e.g. the blank placeholder for GLMs)
  metrics_table <- metrics_table[metrics_table$Metric != "", ]

  # Make a formatted copy for printing
  formatted_metrics_table <- f_conditional_round(metrics_table, allow_integer_decimal_mix = TRUE)


  # ---- Build result object ----
  result <- list(
    model1_name             = m1_name,
    model2_name             = m2_name,
    model1_class            = m1_class,
    model2_class            = m2_class,
    metrics_table           = metrics_table,
    formatted_metrics_table = formatted_metrics_table,
    anova_comparison        = anova_result,
    nested                  = nested,
    swapped                 = swapped
  )

  class(result) <- "f_model_comparison"

  return(result)
}


#' @export
print.f_model_comparison <- function(x, ...) {
  if (isTRUE(x$nested)) {
    cat("Comparison of two nested models:\n")
  } else {
    cat("Comparison of:\n")
  }
  cat(paste0(
    "Model1: ", x$model1_name, " (", x$model1_class, ")\n",
    "Model2: ", x$model2_name, " (", x$model2_class, ")\n"
  ))
  if (isTRUE(x$swapped)) {
    cat("(Model input order was swapped so that Model1 is the simpler model)\n")
  }
  cat("\n")

  # Print metrics table
  print(x$formatted_metrics_table, row.names = FALSE)

  # Print interpretation aids
  cat("\nInterpretation Guide:\n")
  cat("- Lower AIC/BIC values indicate better model fit.\n")
  cat("- Higher R\u00b2 values indicate better model fit.\n")

  if (x$model1_class %in% c("lm", "aov")) {
    cat("- Adj. R\u00b2 is R\u00b2 with a penalty for\n  the number of model parameters used.\n")
  }
  if (x$model1_class == "nls") {
    cat("- R\u00b2 for nonlinear models is approximate and does\n  not have the same statistical properties as for linear models.\n")
    cat("- Adj. R\u00b2 is R\u00b2 with a penalty for\n  the number of model parameters used.\n")
  }
  if (x$model1_class == "glm") {
    cat("- Nagelkerke's R\u00b2 adapts Cox & Snell's R\u00b2 for GLMs,\n  scaling it to a 0-1 range to serve as a pseudo-R\u00b2\n  approximating explained variance.\n")
  }
  if (x$model1_class %in% c("lmerMod", "glmerMod")) {
    cat("- Marginal R\u00b2 reflects variance explained by fixed effects,\n  while conditional R\u00b2 includes both fixed and random effects,\n  representing the model's total explained variance.\n")
  }

  cat("- A lower Sigma (residual standard error) generally\n  indicates a better fit to the data.\n")

  if (!is.null(x$anova_comparison)) {
    cat("- For nested models, ANOVA p-value < 0.05 suggests\n  that the more complex model is significantly better.\n")
  }

  invisible(x)
}
