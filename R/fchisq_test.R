#' Chi-squared Test with post hoc Analysis
#'
#' @description
#' Performs a chi-squared test  \code{\link[stats]{chisq.test}}, then automatically conducts post hoc analysis if the test is significant. The function provides adjusted p-values for each cell in the contingency table using a specified correction method.
#'
#' @param x  A numeric vector (or factor), or a contingency table in matrix or table form. If a data frame is entered the function will try to convert it to a table using \code{df_to_table()}.
#' @param y A numeric vector; ignored if x is a matrix, table or data.frame. If x is a factor, y should be a factor of the same length.
#' @param p A vector of probabilities of the same length as x. Default is \code{NULL}. An error is given if any entry of p is negative.
#' @param method Character string specifying the adjustment method for p-values. Default is \code{"bonferroni"}. Other options include \code{"holm", "hochberg", "hommel", "BH", "BY", "fdr", and "none"}.
#' @param digits Integer specifying the number of decimal places for rounding. Default is \code{3}.
#' @param alpha Numeric threshold for significance. Default is \code{0.05}.
#' @param force_posthoc Logical indicating whether to perform post hoc tests even if the chi-squared test is not significant. Default is \code{FALSE}.
#' @param ... Additional arguments passed to \code{\link[stats]{chisq.test}}.
#'
#' @return An object of class f_chisq_test containing:
#' \itemize{
#'   \item \code{chisq_test_output}: The output from \code{\link[stats]{chisq.test}}.
#'   \item \code{adjusted_p_values}: Matrix of adjusted p-values (for table/matrix input).
#'   \item \code{observed_vs_adj_p_value}: Interleaved table of observed values and adjusted p-values.
#'   \item \code{stdres_vs_adj_p_value}: Interleaved table of standardized residuals and adjusted p-values.
#'   \item \code{adj_p_values}: Vector of adjusted p-values (for vector input).
#'   \item \code{posthoc_output_table}: Data frame with observed values, expected values,
#'         standardized residuals, and adjusted p-values (for vector input).
#'   \item \code{observed_vs_adj_p_value}: Interleaved table of observed values and adjusted p-values (for table/matrix input).
#'   \item \code{stdres_vs_adj_p_value}: Interleaved table of standardized residuals and adjusted p-values (for table/matrix input).
#' }
#'
#' @details
#' The function first performs a chi-squared test using \code{\link[stats]{chisq.test}}. If the test is
#' significant (p < alpha) or if \code{force_posthoc = TRUE}, it conducts post hoc analysis by examining
#' the standardized residuals. The p-values for these residuals are adjusted using the specified method
#' to control for multiple comparisons.
#'
#' If the input is a data frame, the function attempts to convert it to a table and displays the
#' resulting table for verification.
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # Chi.square on independence: Association between two variables.
#' # Create a contingency table.
#' my_table <- as.table(rbind(c(100, 150, 50), c(120, 90, 40)))
#' dimnames(my_table) <- list(Gender = c("Male", "Female"),
#'                            Response = c("Agree", "Neutral", "Disagree"))
#'
#' # Perform chi-squared test with post hoc analysis.
#' f_chisq_test(my_table)
#'
#' # Use a different adjustment method.
#' f_chisq_test(my_table, method = "holm")
#'
#' # Other forms still work like Goodness-of-Fit: Match to theoretical distribution.
#' # Observed frequencies of rolling with a die 1 - 6.
#' observed <- c(2, 2, 10, 20, 15, 11)
#'
#' # Expected probabilities under a fair die.
#' expected_probs <- rep(1/6, 6)
#'
#' # Chi-Square Goodness-of-Fit Test.
#' f_chisq_test(x = observed, p = expected_probs)
#'
#' @references
#' This function implements a post hoc analysis for chi-squared tests inspired by the methodology in:
#'
#' Beasley, T. M., & Schumacker, R. E. (1995). Multiple Regression Approach to Analyzing Contingency Tables: Post Hoc and Planned Comparison Procedures. The Journal of Experimental Education, 64(1), 79-93.
#'
#' The implementation draws inspiration from the \href{https://CRAN.R-project.org/package=chisq.posthoc.test}{'chisq.posthoc.test'} package by Daniel Ebbert.
#'
#' @export


f_chisq_test <-
  function(x,
           y,
           p = NULL,
           method = "bonferroni",
           digits = 3,
           alpha = 0.05,
           force_posthoc = FALSE,
           ...) {

    # Create an output_list
    output_list <- list(p = p)

    # Store alpha for printing and reference
    output_list[["settings"]][["alpha"]]  <- alpha
    output_list[["settings"]][["force_posthoc"]] <- force_posthoc
    output_list[["settings"]][["method"]] <- method
    output_list[["settings"]][["digits"]] <- digits


    # Create a new environment to provide a name to the data.
    env <- new.env()

    # Get the name of the input object
    x_name <- deparse(substitute(x))


    if (is.data.frame(x) && is.null(p)) {
      suppressMessages(x <- df_to_table(x))
      env[[x_name]] <- x
      message(x_name, " is a data.frame, converted to table using df_to_table().\nPlease check if this table is as intended.\nIf not, consider using df_to_table(", x_name, ", label_col = ) to pick the correct column.\n")
      print(x)
      message("---")
    }

    if(!is.table(x) && !is.vector(x) && is.null(p)){
      x <- as.table(as.matrix(x))
      message(x_name, " is not a table, tried to convert data to a table.\nPlease check if this table is as intended: \n")
      print(x)
      message("---")
    }


    # Always store x in env AFTER potential conversion
    env[[x_name]] <- x


    # Do the Probabilities in 'p' sum up to 1
    if(!is.null(p) && abs(sum(p) - 1) > .Machine$double.eps^0.5){
      stop("Probabilities in 'p' must sum to 1. Got sum(p) = ", round(sum(p), 4))
    }

    # Do chisq.test()
    # Build the call, forwarding y and p correctly
    if (missing(y)) {
      if (!is.null(p)) {
        chisq.test.output <- eval(bquote(stats::chisq.test(.(as.name(x_name)), p = p, ...)), envir = env)
      } else {
        chisq.test.output <- eval(bquote(stats::chisq.test(.(as.name(x_name)), ...)), envir = env)
      }
    } else {
      env[["y"]] <- y
      chisq.test.output <- eval(bquote(stats::chisq.test(.(as.name(x_name)), y = y, ...)), envir = env)
    }

    output_list[["chisq_test_output"]] <- chisq.test.output


    if(chisq.test.output$p.value < alpha || force_posthoc == TRUE){
      #Do post hoc
      stdres <- chisq.test.output$stdres

      # Get the p values for the chi square values:
      # Testing whether standardized residuals (which are essentially z-scores),
      # are significantly different from zero.
      p_values <- 2 * pnorm(abs(stdres), lower.tail = FALSE)

      if(is.table(x) || is.matrix(x)){
        # Adjust the p values with the chosen method
        adjusted_p_values <- matrix(
          p.adjust(as.vector(p_values), method = method),
          nrow = nrow(p_values),
          ncol = ncol(p_values),
          dimnames = dimnames(p_values)
        )


        # name the row of the p value table
        rownames(adjusted_p_values) <- rep("p-value", times = nrow(adjusted_p_values))

        # Round and create tables with p values for merging procedure
        table_adj_p <- f_conditional_round(adjusted_p_values,
                                           digits = digits,
                                           allow_integer_decimal_mix = FALSE)

        # Save the adjusted p.values
        output_list[["adjusted_p_values"]] <- table_adj_p

        # Create tables with original data for merging procedure
        table_observed <- chisq.test.output$observed
        table_observed <- f_conditional_round(table_observed,
                                              digits = 0,
                                              allow_integer_decimal_mix = FALSE)

        # Create some space for readability in printed output
        table_empty <- table_observed
        table_empty[,] <- ""
        rownames(table_empty) <- rep("", times = nrow(table_empty))

        # Interleave the formatted tables for output with empty lines for readability
        n <- nrow(table_observed)
        idx <- rep(1:n, each=3) + rep(c(0, n, 2*n), times=n)
        print_obs_pvalue <- rbind(table_observed, table_adj_p, table_empty)[idx,]
        output_list[["spaced_observed_vs_adj_p_value"]] <- print_obs_pvalue

        # Interleave the formatted tables for output obs vs pvalue and store in output list
        n <- nrow(table_observed)
        idx <- rep(1:n, each=2) + rep(c(0, n), times=n)
        Posthoc_obs_pvalue <- rbind(table_observed, table_adj_p)[idx,]
        output_list[["observed_vs_adj_p_value"]] <- Posthoc_obs_pvalue

        # Interleave the formatted tables for output stdres vs pvalue
        stdres <- f_conditional_round(stdres, digits = digits, allow_integer_decimal_mix = FALSE)

        # Interleave the formatted tables for output and store in output list
        n <- nrow(table_observed)
        idx <- rep(1:n, each=2) + rep(c(0, n), times=n)
        Posthoc_stdres_pvalue <- rbind(stdres, table_adj_p)[idx,]
        # put this in output list file also the object of p values with out width.
        output_list[["stdres_vs_adj_p_value"]] <- Posthoc_stdres_pvalue


      } else if(!is.null(p)){

        # Adjust the p values with the chosen method
        adjusted_p_values <- p.adjust(p_values, method = method)

        # Round the p.values
        adjusted_p_values <- f_conditional_round(adjusted_p_values, digits = digits, allow_integer_decimal_mix = FALSE)

        # Save the adjusted p.values
        output_list[["adj_p_values"]] <- adjusted_p_values

        # Make a data.frame for output
        posthoc_out <- data.frame(
          "Observed"      = chisq.test.output$observed,
          "Expected"      = chisq.test.output$expected,
          "Std.Residuals" = stdres,
          "p-value"       = adjusted_p_values,
          check.names     = FALSE
        )


        output_list[["posthoc_output_table"]] <- posthoc_out

      }
    }

    class(output_list) <- "f_chisq_test"

    return(output_list)
  }

#' @export
print.f_chisq_test <- function(x, ...) {

  print(x$chisq_test_output)


  if(x$chisq_test_output$p.value < x$settings$alpha || x$settings$force_posthoc == TRUE){

    if(is.null(x$p)){

      cat("\nObserved data and corresponding", x$settings$method, "corrected p-values:\n   \n")
      print(x$spaced_observed_vs_adj_p_value, quote = FALSE)

    } else if(!is.null(x$p)){
      cat("\nObserved data and corresponding", x$settings$method, "corrected p-values:\n   \n")
      print(x$posthoc_output_table, row.names = FALSE, quote = FALSE)
    }

  } else {

    cat(paste0("No post hoc test was performed. \nChi-squared test p value = ", round(x$chisq_test_output$p.value, digits = x$settings$digits), " which is higher than alpha = ", x$settings$alpha, "\n(NOTE: a post hoc can be forced with option: 'force_posthoc = TRUE')"))
  }
}
