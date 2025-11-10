#' Compare Two Statistical Models
#'
#' Compares two statistical models by calculating key metrics such as AIC, BIC, log-likelihood, R-squared,
#' and others. Supports comparison of nested models using ANOVA tests.
#'
#' @param model1 The first model object. Supported classes include: \code{"lm", "glm", "aov", "lmerMod", "glmerMod", and "nls"}.
#' @param model2 The second model object. Supported classes include: \code{"lm", "glm", "aov", "lmerMod", "glmerMod", and "nls"}.
#' @param nested Logical. If \code{TRUE}, assumes the models are nested and performs an ANOVA comparison.
#'   If \code{NULL} (default), the function attempts to automatically determine if the models are nested.
#' @param digits Integer. The number of decimal places to round the output metrics. Defaults to \code{3}.
#'
#' @return A list of class "f_model_comparison" containing:
#'   \item{model1_name}{The name of the first model.}
#'   \item{model2_name}{The name of the second model.}
#'   \item{model1_class}{The class of the first model.}
#'   \item{model2_class}{The class of the second model.}
#'   \item{metrics_table}{A data frame summarizing metrics for both models, their differences, and (if applicable) the ANOVA p-value.}
#'   \item{formatted_metrics_table}{A formatted version of the metrics table for printing.}
#'   \item{anova_comparison}{The ANOVA comparison results if the models are nested and an ANOVA test was performed.}
#'   \item{nested}{Logical indicating whether the models were treated as nested.}
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
#' @details
#' Calculate various metrics to assess model fit:
#'   \itemize{
#'     \item \bold{AIC/BIC:} Lower values indicate better fit.
#'     \item \bold{Log-Likelihood:} Higher values (less negative) indicate better fit.
#'     \item \bold{R-squared:} Proportion of variance explained by the model.
#'     \item \bold{Adjusted R-squared:} R-squared penalized for the number of parameters (for linear models).
#'     \item \bold{Nagelkerke R^2:} A pseudo-R^2 for generalized linear models (GLMs).
#'     \item \bold{Marginal/Conditional R^2:} For mixed models, marginal R^2 reflects fixed effects, while conditional R^2 includes random effects.
#'     \item \bold{Sigma:} Residual standard error.
#'     \item \bold{Deviance:} Model deviance.
#'     \item \bold{SSE:} Sum of squared errors.
#'     \item \bold{Parameters (df):} Number of model parameters.
#'     \item \bold{Residual df:} Residual degrees of freedom.
#'   }
#' If the models are nested, an ANOVA test is performed to compare them, and a p-value is provided to assess whether the more complex model significantly improves fit.
#'
#' @note
#'   \itemize{
#'     \item The function supports a variety of model types but may issue warnings if unsupported or partially supported classes are used.
#'     \item For GLMs, Nagelkerke's R^2 is used as a pseudo-R^2 approximation.
#'     \item For mixed models, the function relies on the 'r.squaredGLMM' function from the 'MuMIn' package for R^2 calculation.
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
#' comparison <- f_model_comparison(model1, model2)
#' print(comparison)
#'
#' # Example with GLMs.
#' \donttest{
#' model1 <- glm(am ~ wt, data = mtcars, family = binomial)
#' model2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' comparison <- f_model_comparison(model1, model2)
#' print(comparison)
#' }
#'
#' # Example with automatic detection of nested models.
#' model1 <- lm(mpg ~ wt, data = mtcars)
#' model2 <- lm(mpg ~ wt + hp, data = mtcars)
#' comparison <- f_model_comparison(model1, model2)
#' print(comparison)
#'
#' @seealso
#' \code{\link{AIC}}, \code{\link{BIC}}, \code{\link{anova}}, \code{\link{logLik}}, \code{\link[MuMIn]{r.squaredGLMM}}
#'
#' @export



f_model_comparison <- function(model1, model2, nested = NULL, digits = 3) {

  # Check if models are of supported class
  supported_classes <- c("lm", "glm", "aov", "lmerMod", "glmerMod", "nls")
  m1_class <- class(model1)[1]
  m2_class <- class(model2)[1]

  if (!m1_class %in% supported_classes || !m2_class %in% supported_classes) {
    warning("One or both models are not of a supported class. Results may be unreliable.")
  }

  # Get model names
  m1_name <- deparse(substitute(model1))
  m2_name <- deparse(substitute(model2))


  # Make sure that if the user sets nested to TRUE the models are of the same class
  if (m1_class != m2_class && nested == TRUE) {
    nested <- FALSE
    warning("Models are not of the same class and thus can't be nested. Assuming non-nested models.")
  }

  # Auto-detect if models are nested if not specified
  tryCatch(
    if (m1_class != m2_class) {
        nested    <- FALSE

    } else if (m1_class == m2_class) {

      check_nested <- is_nested(model1, model2)
      nested       <- check_nested$nested
      if(nested == FALSE){
      reason_message <- "Automatically determined if models were nested and found:\n"
      message(paste0(reason_message, check_nested$reason))
      }
    }
    , error = function(e) {
      warning("Could not automatically determine if models are nested. Assuming non-nested models. If the models ARE nested use the option: nested = TRUE")
      nested <- FALSE
    })


  # Function to safely extract metrics
  get_metrics <- function(model) {
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

    # R-squared and adjusted R-squared
    if (class(model)[1] == "aov"){
      # Get the ANOVA table
      anova_table <- summary(model)

      # Extract necessary components
      SSR <- tail(anova_table[[1]]$"Sum Sq", 1)    # Sum of squares residuals
      SST <- sum(anova_table[[1]]$"Sum Sq")  # Total sum of squares
      n <- nobs(model)                       # Total sample size
      df_reg <- n - (utils::tail(anova_table[[1]]$"Df", 1) - 1) # Degrees of freedom for regression -1 for the intercept

      # Calculate R-squared
      metrics$r.squared <- 1 - (SSR / SST)

      # Calculate adjusted R-squared
      metrics$adj.r.squared <- 1 - ((1 - metrics$r.squared) * (n - 1) / (n - df_reg - 1))

    } else if (class(model)[1] == "nls") {
      # Extract necessary components
      n               <- nobs(model)
      df_reg          <- length(coef(model))
      fitted_vals     <- fitted(model)
      residuals_vals  <- residuals(model)
      y_reconstructed <- fitted_vals + residuals_vals
      mean_y_reconstructed <- mean(y_reconstructed,na.rm = TRUE)
      SST <- sum((y_reconstructed - mean_y_reconstructed)^2) # Total sum of squares
      SSR <- sum(residuals(model)^2)    # Sum of squares residuals


      # Calculate R-squared
      metrics$r.squared <- 1 - (SSR / SST)

      # Calculate adjusted R-squared
      metrics$adj.r.squared <- 1 - ((1 - metrics$r.squared) * (n - 1) / (n - df_reg - 1))

    } else if (class(model)[1] == "lm") {
      metrics$r.squared     <- summary(model)$r.squared
      metrics$adj.r.squared <- summary(model)$adj.r.squared

    } else if (class(model)[1] == "glm") {
      # For GLMs, calculate pseudo-R2 (McFadden's)
      tryCatch({
        null_model <- update(model, . ~ 1)
        ll_full    <- as.numeric(logLik(model))
        ll_null    <- as.numeric(logLik(null_model))
        n          <- nobs(model)

        # # McFadden's R^2 make this an option TO DO
        # K <- length(model$coefficients)
        # metrics$r.squared     <- 1 - (ll_full / ll_null)
        # metrics$adj.r.squared <- 1 - ((ll_full - K) / ll_null)

        # Nagelkerke's R^2 (Cox & Snell adjusted)
        cox_snell <- 1 - exp((2/n) * (ll_null - ll_full))
        max_cox_snell <- 1 - exp((2/n) * ll_null)
        metrics$r.squared <- cox_snell / max_cox_snell
        metrics$adj.r.squared <- NA

      }, error = function(e) {
        metrics$r.squared     <- NA
        metrics$adj.r.squared <- NA
      })
    } else if (class(model)[1] == "lmerMod" || class(model)[1] == "glmerMod") {
      # For mixed models, calculate conditional and marginal R^2
      tryCatch({

        metrics$r.squared     <- r.squaredGLMM(model)[1] # This is actually Marginal R^2 focusing on fixed effects
        metrics$adj.r.squared <- r.squaredGLMM(model)[2] # This is actually conditional R^2 includes random effects


      }, error = function(e) {
        metrics$r.squared     <- NA
        metrics$adj.r.squared <- NA
      })
    } else {
      metrics$r.squared     <- NA
      metrics$adj.r.squared <- NA
    }


    # Sigma (residual standard error)
    if (class(model)[1] %in% c("lm", "aov", "glm", "nls")) {
      metrics$sigma <- sigma(model)
    } else {
      metrics$sigma <- tryCatch(sqrt(sum(residuals(model, type = "pearson")^2) / df.residual(model)), error = function(e) NA)
    }

    # Deviance
    metrics$deviance <- tryCatch(deviance(model), error = function(e) NA)

    # Degrees of freedom
    if (class(model)[1] == "lmerMod" || class(model)[1] == "glmerMod") {
      metrics$df <- tryCatch(attr(logLik(model), "df"), error = function(e) NA)
    } else {
      metrics$df <- tryCatch(length(coef(model)), error = function(e) NA)
      # Handle NA coefficients in models like lm with singular fits
      metrics$df <- tryCatch(sum(!is.na(coef(model))), error = function(e) metrics$df)
    }

    # Residual degrees of freedom
    metrics$df.residual <- tryCatch(df.residual(model), error = function(e) NA)

    # SSE (Sum of Squared Errors)
    if (class(model)[1] %in% c("lm", "aov", "lmerMod", "nls")) {
      metrics$SSE <- tryCatch(sum(residuals(model, type = "response")^2),
                              error = function(e) NA)
    } else if (class(model)[1] %in% c("glm", "glmerMod")) {
      if(family(model1)$family == "gaussian"){
      # For gaussuan glm models SSE (=deviance) makes sense
      metrics$SSE <- tryCatch(deviance(model), error = function(e) NA)
      } else {
      # For the other glm families its doesn't make sense.
        metrics$SSE <- NA
      }
    } else {
      # Try to get the SSE for unknown models
      metrics$SSE <- tryCatch(sum(residuals(model)^2), error = function(e) NA)
    }

    return(metrics)
  }

  # Get metrics for both models
  m1_metrics <- get_metrics(model1)
  m2_metrics <- get_metrics(model2)

  # Calculate differences
  diff_metrics <- list(
    AIC = m2_metrics$AIC - m1_metrics$AIC,
    BIC = m2_metrics$BIC - m1_metrics$BIC,
    logLik = m2_metrics$logLik - m1_metrics$logLik,
    r.squared = m2_metrics$r.squared - m1_metrics$r.squared,
    adj.r.squared = m2_metrics$adj.r.squared - m1_metrics$adj.r.squared,
    df = m2_metrics$df - m1_metrics$df,
    SSE = m2_metrics$SSE - m1_metrics$SSE
  )

  # Perform model comparison if requested
  anova_result <- NULL
  p_value <- NA

  if (nested) {
    # Determine test type
    test_type <- if(m1_class == "glm") {
      ifelse(family(model1)$family == "binomial", "Chisq", "F")
    } else if(m1_class %in% c("lmerMod", "glmerMod")) {
      "Chisq"
    } else if(m1_class == "nls") {
      "F"
    } else {
      "none"
    }

    # Perform ANOVA comparison with error handling
    anova_result <- tryCatch({
      if(test_type %in% c("Chisq", "F")) {
        anova(model1, model2, test = test_type)
      } else {
        anova(model1, model2)
      }
    }, error = function(e) {
      warning("Model comparison failed: ", e$message)
      return(NULL)
    })
    # Extract p-value based on model type
    p_value <- tryCatch({
      if(m1_class %in% c("lmerMod", "glmerMod")) {
        pchisq(anova_result$Chisq[2], df = anova_result$Df[2], lower.tail = FALSE)
      } else if(!is.null(anova_result$`Pr(>F)`)) {
        anova_result[nrow(anova_result), "Pr(>F)"]
      } else if(!is.null(anova_result$`Pr(>Chi)`)) {
        anova_result[nrow(anova_result), "Pr(>Chi)"]
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
   }



  # Create metrics column with parameters names depending on model class
  if (class(model1)[1] %in% c("lm", "aov", "nls")) {
    metric = c(
      "AIC",
      "BIC",
      "Log-Likelihood",
      "R-squared",
      "Adj. R-squared",
      "Sigma",
      "Deviance",
      "SSE",
      "Parameters (df)",
      "Residual df",
      "ANOVA p-value"
    )
  } else if (class(model1)[1] %in% c("glm")){
    metric = c(
      "AIC",
      "BIC",
      "Log-Likelihood",
      "Nagelkerke R^2",
      "",
      "Sigma",
      "Deviance",
      "SSE",
      "Parameters (df)",
      "Residual df",
      "ANOVA p-value"
    )
  } else if (class(model1)[1] %in% c("lmerMod", "glmerMod")){
    metric = c(
      "AIC",
      "BIC",
      "Log-Likelihood",
      "Marginal R^2",
      "Conditional R^2",
      "Sigma",
      "Deviance",
      "SSE",
      "Parameters (df)",
      "Residual df",
      "ANOVA p-value"
    )
    } else {
    metric = c(
      "AIC",
      "BIC",
      "Log-Likelihood",
      "R-squared",
      "Adj. R-squared",
      "Sigma",
      "Deviance",
      "SSE",
      "Parameters (df)",
      "Residual df",
      "ANOVA p-value"
    )
  }


  # Create output table
  metrics_table <- data.frame(
    "Metric"     = metric,
    "Model1"     = c(m1_metrics$AIC, m1_metrics$BIC, m1_metrics$logLik,
                   m1_metrics$r.squared, m1_metrics$adj.r.squared,
                   m1_metrics$sigma, m1_metrics$deviance, m1_metrics$SSE,
                   m1_metrics$df, m1_metrics$df.residual, NA),
    "Model2"     = c(m2_metrics$AIC, m2_metrics$BIC, m2_metrics$logLik,
                   m2_metrics$r.squared, m2_metrics$adj.r.squared,
                   m2_metrics$sigma, m2_metrics$deviance, m2_metrics$SSE,
                   m2_metrics$df, m2_metrics$df.residual,  NA),
    "Difference" = c(diff_metrics$AIC, diff_metrics$BIC, diff_metrics$logLik,
                   diff_metrics$r.squared, diff_metrics$adj.r.squared,
                   m2_metrics$sigma - m1_metrics$sigma,
                   m2_metrics$deviance - m1_metrics$deviance, diff_metrics$SSE,
                   diff_metrics$df, m2_metrics$df.residual - m1_metrics$df.residual,
                   p_value)
   )


  if(is.na(p_value)){
    metrics_table <- subset(metrics_table, "Metric" != "ANOVA p-value")
  }

  # Make a data.frame copy to format
  formatted_metrics_table <- f_conditional_round(metrics_table, detect_int_col = FALSE)


    # Create result list
  result <- list(
    model1_name = m1_name,
    model2_name = m2_name,
    model1_class = m1_class,
    model2_class = m2_class,
    metrics_table = metrics_table,
    formatted_metrics_table = formatted_metrics_table,
    anova_comparison = anova_result,
    nested = nested
  )


  class(result) <- "f_model_comparison"

  return(result)
}

#' @export
# Print method for model_comparison objects
print.f_model_comparison <- function(x, ...) {
  if(x$nested){
  cat("Comparison of two nested models:\n")
  } else {
    cat("Comparison of:\n")
  }
  cat(paste0("Model1: ", x$model1_name, " (", x$model1_class, ") \nModel2: ",
             x$model2_name, " (", x$model2_class, ")\n\n"))

  # Print metrics table
  print(x$formatted_metrics_table, row.names = FALSE)

  # # Print ANOVA comparison if available
  # if (!is.null(x$anova_comparison)) {
  #   cat("\nModel Comparison (ANOVA):\n")
  #   print(x$anova_comparison)
  # }

  # Print interpretation aids
    cat("\nInterpretation Guide:\n")
    cat("- Lower AIC/BIC values indicate better model fit.\n")
    cat("- Higher R-squared values indicate better model fit.\n")
    if (x$model1_class[1] %in% c("lm", "aov", "nls")){
      cat("- Adj. R-squared is R-squared with a penalty for\n  the number of model parameters used.\n")
    }
    if (x$model1_class[1] %in% c("glm")){
      cat("- Nagelkerke's R^2 adapts Cox & Snell's R^2 for GLMs,\n  scaling it to a 0-1 range to serve as a pseudo-R^2\n  approximating explained variance.\n")
    }
    if (x$model1_class[1] %in% c("lmerMod", "glmerMod")){
      cat("- Marginal R^2 reflects variance explained by fixed effects,\n  while conditional R^2 includes both fixed and random effects,\n  representing the model's total explained variance.\n"
          )
    }
    cat("- A lower Sigma (residual standard error) generally\n  indicates a better fit to the data.\n")
  if (!is.null(x$anova_comparison)) {
    cat("- For nested models, ANOVA p-value < 0.05 suggests\n  that the more complex model is significantly better.\n")
  }
}
