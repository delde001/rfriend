#' Fit a linear mixed model with \code{lme4::lmer()} including assumption checks, diagnostics, R-squared and post hoc tests.
#'
#' Fits a linear mixed-effects model using \code{lme4::lmer()} (with p-values
#' from \code{lmerTest}) and produces a fully-formatted report containing
#' the fixed-effects table, random-effects variance components, model-fit
#' indices (AIC, BIC, logLik, marginal & conditional R\eqn{^2}), residual and
#' BLUP diagnostics, convergence / singular-fit warnings, and post hoc
#' comparisons (\code{emmeans}) on factor fixed effects. Results can be
#' returned to the console or written to 'pdf', 'Word' or 'Excel'.
#'
#' @param formula A two-sided formula passed to \code{lme4::lmer()}, e.g.
#'   \code{y ~ treatment + time + (1 | subject)} or
#'   \code{y ~ treatment * time + (1 + time | subject)}. The right-hand
#'   side must contain at least one random-effects term in the
#'   \code{(varying | grouping)} syntax. See \emph{Details} for a guide to
#'   reading the random-effects syntax in study-design terms.
#'
#'   More than one response variable can be supplied on the left-hand side
#'   using \code{+} (e.g. \code{y1 + y2 ~ treatment + (1 | subject)}).
#'   A separate model is then fit for each response, sharing the same
#'   right-hand side, and a multiple-testing warning is added to the
#'   report. See the \emph{Multiple Testing Across Response Variables}
#'   section.
#' @param data A data frame containing the variables in the model.
#' @param REML Logical. If \code{TRUE} (default), the model is fit with
#'   restricted maximum likelihood, which gives less biased variance
#'   component estimates and is the appropriate choice for inference on
#'   fixed effects with Kenward-Roger or Satterthwaite degrees of freedom.
#'   Set to \code{FALSE} only when comparing nested models that differ in
#'   their fixed-effects structure.
#' @param ddf Character. Method for computing denominator degrees of
#'   freedom for fixed-effects p-values. One of:
#'   \describe{
#'     \item{\code{"Satterthwaite"}}{(default) Fast and accurate for most
#'       designs. Provided by \code{lmerTest}.}
#'     \item{\code{"Kenward-Roger"}}{Considered the gold standard, especially
#'       for small samples and unbalanced designs. Slower, and requires the
#'       \code{pbkrtest} package.}
#'     \item{\code{"lme4"}}{No p-values; only t-statistics are reported.
#'       Equivalent to a plain \code{lme4::lmer()} summary.}
#'   }
#' @param alpha Numeric. Significance level for the fixed-effects table and
#'   the post hoc tests. Default is \code{0.05}.
#' @param adjust Character. Method used to adjust p-values for multiple
#'   pairwise comparisons in the post hoc step (passed to
#'   \code{emmeans::emmeans()}). One of \code{"sidak"} (default),
#'   \code{"tukey"}, \code{"bonferroni"}, \code{"fdr"}, \code{"none"}.
#' @param norm_plots Logical. If \code{TRUE} (default), diagnostic plots
#'   (residuals vs fitted, Q-Q of level-1 residuals, Q-Q of random-effect
#'   BLUPs, scale-location) are included in the output.
#' @param post_hoc Logical. If \code{TRUE} (default), runs
#'   \code{emmeans::emmeans()} pairwise comparisons \strong{only when}
#'   the linear mixed model finds a significant fixed-effect term
#'   (ANOVA p-value below \code{alpha}). The post hoc is performed on
#'   each significant factor fixed-effect term separately. Numeric
#'   covariates are skipped because their slope is already reported in
#'   the fixed-effects coefficient table; pairwise contrasts are not
#'   meaningful for a continuous predictor. If no fixed-effect term is
#'   significant, no post hoc is run.
#' @param intro_text Logical. If \code{TRUE} (default), prepends an
#'   explanation of LMM assumptions and the random-effects syntax linked
#'   to study design.
#' @param close_generated_files Logical. Closes any open Word or Excel
#'   files before writing. Cross-platform (Windows taskkill, macOS / Linux
#'   pkill). Default \code{FALSE}. \strong{WARNING:} save your work first.
#' @param open_generated_files Logical. Whether to open the generated output
#'   files after creation. Defaults to \code{TRUE} in an interactive R session
#'   and \code{FALSE} otherwise (e.g. in scripts or automated pipelines).
#'   Set to \code{TRUE} or \code{FALSE} to override this behaviour explicitly.
#' @param output_type Character. Output format. One of:
#'   \itemize{
#'     \item \code{"default"}: returns the \code{f_lmer} object; auto-prints
#'       if unassigned.
#'     \item \code{"console"}: forces immediate printing.
#'     \item \code{"pdf"}, \code{"word"}, \code{"excel"}: writes a file.
#'     \item \code{"rmd"}: stores the raw markdown string in the returned
#'       object for embedding in an R Markdown chunk with
#'       \code{\{r, echo=FALSE, results='asis'\}}.
#'   }
#' @param save_as Character. Output file path. See \code{\link{f_aov}}
#'   for the resolution rules; default is
#'   \code{file.path(tempdir(), "<dataname>_lmer_output.<ext>")}.
#' @param save_in_wdir Logical. If \code{TRUE}, save in the working
#'   directory instead of \code{tempdir()}. Default \code{FALSE}.
#' @param ... Additional arguments forwarded to \code{\link[lmerTest]{lmer}}.
#'   The arguments \code{subset}, \code{na.action}, and \code{weights}
#'   are handled specially: when supplied, they are applied via
#'   \code{\link[stats]{model.frame}} \strong{once} before the
#'   per-response loop, so every response in a multi-response call is
#'   fitted on the identical row set. Other \code{lmer()} arguments
#'   (e.g. \code{control = lmerControl(...)}, \code{contrasts},
#'   \code{offset}) are forwarded unchanged on every fit.
#'
#' @return An object of class \code{f_lmer}: a named list containing the
#'   fitted \code{lmerModLmerTest} model, the ANOVA-style fixed-effects
#'   table, the variance components and ICC, the R\eqn{^2} values, the
#'   observed descriptives table (raw-data n, mean, sd, se, min, Q1,
#'   median, Q3, max grouped by the categorical fixed-effect predictors),
#'   post hoc results (if any), diagnostic plots, and convergence
#'   diagnostics. When more than one response variable is supplied on the
#'   left-hand side, these elements are nested one level deep under each
#'   response name, e.g. \code{out$y1$fixed_effects},
#'   \code{out$y2$fixed_effects}. When \code{output_type = "rmd"} the
#'   markdown string is stored in \code{$rmd}.
#'
#' @details
#' \strong{What is a linear mixed model?}\cr
#' A linear mixed model (LMM) extends ordinary regression / ANOVA by
#' allowing two kinds of effects:
#' \itemize{
#'   \item \strong{Fixed effects} - factors you actively manipulated or
#'     whose specific levels you care about (treatment, dose, time,
#'     genotype). Reported as estimates with confidence intervals.
#'   \item \strong{Random effects} - grouping structure that creates
#'     non-independence in your data but whose levels are a random sample
#'     from a larger population (subjects measured repeatedly, plots
#'     within fields, observers, batches). Reported as variance
#'     components.
#' }
#' Use an LMM whenever observations share something that makes them more
#' alike than two random observations from the dataset. Ignoring such
#' grouping (running a plain \code{aov} or \code{lm}) is
#' \strong{pseudoreplication}, i.e. treating non-independent observations
#' as if they were independent: standard errors shrink, p-values shrink,
#' false positives explode.
#'
#' \strong{Vocabulary.}\cr
#' Before going further, a few terms used throughout the report:
#' \itemize{
#'   \item \strong{Subject} - the experimental unit that is measured
#'     repeatedly (a person, animal, pot and plot, cell line); in
#'     \code{lme4} syntax it is the grouping factor on the right of
#'     the \code{|}, e.g. \code{(1 | subject)}.
#'   \item \strong{Within-subject factor} - a predictor whose levels
#'     vary within the same subject (time in a longitudinal study,
#'     treatment in a cross-over study).
#'   \item \strong{Between-subject factor} - a predictor whose levels
#'     vary across subjects but are constant within a subject (sex,
#'     genotype, treatment arm in a parallel-groups trial). Both
#'     within- and between-subject factors are \strong{fixed} effects.
#'   \item \strong{BLUP} - \emph{Best Linear Unbiased Predictor}. The
#'     model's estimate of the random-effect value for each subject
#'     (e.g. how much a particular subject deviates from the population
#'     intercept). BLUPs are checked for normality just like residuals.
#'   \item \strong{ICC} - \emph{intraclass correlation coefficient}.
#'     The share of total variance attributable to between-group
#'     differences. ICC = 0 means the grouping factor is irrelevant;
#'     ICC = 1 means observations within a group are identical.
#'   \item \strong{REML} - \emph{restricted maximum likelihood}. The
#'     default fitting method for variance components; gives less biased
#'     estimates than ordinary maximum likelihood.
#'   \item \strong{Satterthwaite / Kenward-Roger} - methods to approximate
#'     the denominator degrees of freedom for fixed-effect p-values,
#'     since there is no exact df in an LMM.
#' }
#'
#' \strong{Reading the \code{(1 | group)} syntax.}\cr
#' Every random-effects term has the form \code{( <varying> | <group> )}.
#' The bar reads as "varies by". The grouping factor on the right is what
#' creates the non-independence. The left side is what is allowed to differ
#' between groups. Common patterns:
#' \itemize{
#'   \item \code{(1 | subject)} - random intercept per subject (each
#'     subject has its own baseline). Repeated measures, longitudinal data.
#'   \item \code{(1 | field)} - randomised block design or multi-site
#'     trial; one intercept per block.
#'   \item \code{(1 | field/plot)} - \code{plot} nested in \code{field};
#'     equivalent to \code{(1|field) + (1|field:plot)}. Split-plot or
#'     hierarchical sampling.
#'   \item \code{(1 + time | subject)} - random intercept and random slope
#'     of \code{time} per subject. Subjects differ both in baseline and
#'     in how fast they change. Growth curves.
#'   \item \code{(1 | subject) + (1 | observer)} - crossed random
#'     effects: every observer can rate every subject. Inter-rater designs.
#' }
#' \strong{Rule of thumb:} if you can answer "if I duplicated this
#' experiment, would I draw new levels of this factor?" with \emph{yes},
#' it belongs on the right of a \code{|}. If you would re-use the exact
#' same levels (e.g. control vs treated) it is a fixed effect.
#'
#' \strong{When to use a linear mixed model.}\cr
#' The most common reason is a \strong{repeated-measures design}, in
#' which the same experimental units are measured on more than one
#' occasion or under more than one treatment. Compared with a
#' between-groups design analysed by plain ANOVA this gives two real
#' advantages: fewer experimental units are needed (each subject acts
#' as its own control, removing between-subject variation from the
#' comparison) and individual differences cannot bias the treatment
#' groups (in a cross-over design every subject receives every
#' treatment). Two canonical examples:
#' \itemize{
#'   \item \strong{Longitudinal study} - same subjects measured at
#'     several time points: \code{y ~ time + (1 | subject)}. If
#'     subjects also differ in how fast they change, add a random
#'     slope: \code{y ~ time + (1 + time | subject)}.
#'   \item \strong{Cross-over design} - every subject receives every
#'     treatment in sequence: \code{y ~ treatment + (1 | subject)}. If
#'     carry-over between periods is a concern, add \code{period} as
#'     a fixed effect.
#' }
#' LMMs also apply to non-repeated structures that still create
#' non-independence: randomised block designs, split-plot trials,
#' multi-site studies, inter-rater designs.
#'
#' \strong{Assumptions of a linear mixed model:}
#' \enumerate{
#'   \item Linearity in the parameters of the fixed-effects part.
#'   \item Independence of observations \emph{conditional on} the random
#'     effects. If structure remains (e.g. temporal autocorrelation), more
#'     random effects or a correlation structure are needed.
#'   \item Normality of level-1 residuals (Q-Q plot of \code{residuals(m)}).
#'   \item Normality of the random-effect BLUPs
#'     (Q-Q plot of \code{ranef(m)}). \strong{This is the assumption most
#'     users forget.}
#'   \item Homoscedasticity: residual variance roughly constant across
#'     fitted values and across grouping levels.
#'   \item At least \code{~5} levels of each grouping factor; with
#'     \code{3-4} levels it is usually better to treat the factor as
#'     fixed.
#' }
#' If Levene's test or the Shapiro-Wilk tests on residuals or BLUPs
#' indicate a violation, the report adds a \emph{Recommendations for
#' Heteroscedasticity and/or non-normal residuals} section after the
#' diagnostics with concrete next steps (generalised mixed model,
#' transformation).
#'
#' \strong{Convergence and singular fits.}\cr
#' \code{f_lmer} surfaces \code{lme4} convergence warnings and the
#' "boundary (singular) fit" message prominently in the output. A singular
#' fit usually means the random-effects structure is too complex for the
#' data (often a random slope with too few levels) - simplify the model
#' before interpreting results.
#'
#' This function requires Pandoc (>= 1.12.3) for \code{pdf}, \code{word}
#' and \code{rmd} output. See \code{\link{f_aov}} for installation notes.
#'
#' @section Multiple Testing Across Response Variables:
#' When several response variables are analysed in a single call
#' (e.g. \code{y1 + y2 + y3 ~ treatment + (1 | subject)}), each linear
#' mixed model is an independent null-hypothesis test at level
#' \code{alpha}. The post hoc adjustments (\code{adjust = "sidak"},
#' \code{"tukey"}, etc.) only control the family-wise error rate
#' \strong{within} one model (across pairwise contrasts for that
#' response). They do \strong{not} protect against the inflation of
#' Type I error \strong{across} the set of responses.
#'
#' \strong{Practical implication:} With \eqn{k} independent response
#' variables all tested at \eqn{\alpha = 0.05}, the probability of
#' obtaining at least one false positive is
#' \eqn{1 - (1 - 0.05)^k}, which reaches ~40\% for \eqn{k = 10}.
#'
#' \strong{When this matters:} The risk is highest in exploratory studies
#' where many responses are screened simultaneously without a clear
#' a priori hypothesis for each one. It is less of a concern when each
#' response is a pre-specified primary outcome with its own biological
#' rationale.
#'
#' \strong{Possible remedies:}
#' \itemize{
#'   \item \strong{Bonferroni correction across responses:} use
#'     \code{alpha = 0.05 / k} where \code{k} is the number of response
#'     variables. Conservative but simple.
#'   \item \strong{False Discovery Rate (FDR):} apply
#'     \code{p.adjust(p_values, method = "fdr")} to the vector of
#'     per-response fixed-effect p-values after the fact.
#'   \item \strong{Multivariate model:} if the responses are correlated
#'     and you want a single omnibus test, fit a joint multivariate
#'     mixed model (e.g. \code{MCMCglmm}, \code{brms}) before
#'     interpreting individual responses.
#'   \item \strong{Pre-registration:} declare primary vs. exploratory
#'     responses before data collection to justify differential
#'     correction thresholds.
#' }
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#' # sleepstudy: reaction time vs days of sleep deprivation,
#' # repeated measures within Subject (ships with lme4).
#' data(sleepstudy, package = "lme4")
#'
#' # 1) Random intercept per subject - the simplest mixed model.
#' #    Each subject has its own baseline reaction time; the fixed
#' #    effect of Days is the average slope across subjects.
#' #    With output_type = "default" (the default), the result auto-
#' #    prints if not assigned, so no print() call is needed.
#' f_lmer_out <- f_lmer(Reaction ~ Days + (1 | Subject),
#'                      data = sleepstudy)
#'
#' # Re-print the stored result and show the diagnostic plots.
#' print(f_lmer_out)
#' plot(f_lmer_out)
#'
#' # 2) Random intercept AND random slope of Days per subject,
#' #    fitted with Kenward-Roger denominator df, saved to MS Word.
#' f_lmer(Reaction ~ Days + (1 + Days | Subject),
#'        data = sleepstudy,
#'        ddf  = "Kenward-Roger",
#'        output_type = "word"
#'        )
#'
#' # 3) A factor fixed effect triggers a post hoc test.
#' #    Bin Days into three sleep-deprivation phases so that the
#' #    fixed effect is categorical and emmeans pairwise comparisons
#' #    with a compact letter display are produced automatically.
#' sleepstudy$Phase <- cut(sleepstudy$Days,
#'                         breaks = c(-Inf, 2, 6, Inf),
#'                         labels = c("early", "mid", "late"))
#' f_lmer(Reaction ~ Phase + (1 | Subject),
#'        data = sleepstudy,
#'        adjust = "tukey")
#'
#' # 4) A minimal report: suppress the intro text and the diagnostic
#' #    plots, and save it directly to MS Word. Useful when embedding
#' #    many models in one document or when you only need the tables.
#' f_lmer(Reaction ~ Days + (1 | Subject),
#'        data = sleepstudy,
#'        intro_text = FALSE,
#'        norm_plots = FALSE,
#'        output_type = "word"
#'        )
#'
#' # 5) Get the raw markdown back for embedding in an R Markdown
#' #    document. Use it inside a chunk with results = 'asis'.
#' f_lmer_rmd_out <- f_lmer(Reaction ~ Days + (1 | Subject),
#'                          data = sleepstudy,
#'                          output_type = "rmd")
#' cat(f_lmer_rmd_out$rmd)
#'
#' # 6) Two response variables analysed in one call. A separate model
#' #    is fit for each, sharing the same right-hand side. The results
#' #    are nested under each response name.
#' sleepstudy$Reaction2 <- sleepstudy$Reaction + rnorm(nrow(sleepstudy), 0, 5)
#' multi_out <- f_lmer(Reaction + Reaction2 ~ Days + (1 | Subject),
#'                     data = sleepstudy,
#'                     intro_text = FALSE,
#'                     norm_plots = FALSE)
#' multi_out$Reaction$fixed_effects
#' multi_out$Reaction2$fixed_effects
#'
#' @export
f_lmer <- function(
    formula,
    data = NULL,
    REML = TRUE,
    ddf = "Satterthwaite",
    alpha = 0.05,
    adjust = "sidak",
    norm_plots = TRUE,
    post_hoc = TRUE,
    intro_text = TRUE,
    output_type = "default",
    save_as = NULL,
    save_in_wdir = FALSE,
    close_generated_files = FALSE,
    open_generated_files = interactive(),
    ...
    # Additional arguments forwarded to lme4::lmer() / lmerTest::lmer().
    # subset, na.action and weights are baked into a single master data
    # frame (built once before the response loop) so every response is
    # fitted on the identical row set. Other arguments such as
    # `control` or `contrasts` are forwarded unchanged.
) {

  ########## Reset initial settings on exit #################################
  .session_state <- save_session_state()  # Helper function: helper_session_state
  on.exit(restore_session_state(.session_state), add = TRUE) # Helper function: helper_session_state

  ########## Required packages ##############################################
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for f_lmer(). ",
         "Please run: install.packages('lme4')")
  }
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' is required for f_lmer(). ",
         "Please run: install.packages('lmerTest')")
  }

  ########## Resolve data and formula like the rest of the package ##########
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
    data <- formula_to_dataframe(formula)
    formula <- clean_formula(formula)
  }

  ########## Validate that the formula actually contains a random effect ####
  rhs_chars <- paste(deparse(formula[[3]]), collapse = " ")
  if (!grepl("\\|", rhs_chars)) {
    stop("f_lmer() requires at least one random-effects term in the formula, ",
         "e.g. 'y ~ x + (1 | subject)'. ",
         "For a model without random effects use f_aov() or f_glm().")
  }

  ########## Validate scalar arguments ######################################
  output_type <- match.arg(output_type,
                           choices = c("default", "console", "pdf",
                                       "word", "excel", "rmd"))
  ddf <- match.arg(ddf,
                   choices = c("Satterthwaite", "Kenward-Roger", "lme4"))
  adjust <- tolower(adjust)
  adjust <- match.arg(adjust,
                      choices = c("sidak", "tukey", "bonferroni",
                                  "fdr", "none"))

  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be a single numeric value strictly between 0 and 1.")
  }

  if (ddf == "Kenward-Roger" && !requireNamespace("pbkrtest", quietly = TRUE)) {
    warning("ddf = 'Kenward-Roger' requires the 'pbkrtest' package. ",
            "Falling back to ddf = 'Satterthwaite'. ",
            "Run install.packages('pbkrtest') to enable Kenward-Roger.")
    ddf <- "Satterthwaite"
  }

  ########## Variable presence checks #######################################
  all_vars <- all.vars(formula)
  missing_vars <- setdiff(all_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("The following variable(s) are not in the data: ",
         paste(missing_vars, collapse = ", "))
  }


  # Warn if LHS has expressions like log(y) before silently stripping them
  check_lhs_is_names(formula) #use helper_check_lhs.R

  # Extract response variables from the LHS of the formula. f_lmer now
  # supports multiple responses (y1 + y2 ~ ...), mirroring f_aov(). One
  # lmer() fit + report block is produced per response, sharing the same
  # RHS (fixed + random-effects) structure.
  lhs            <- all.vars(formula[[2]])
  response_names <- lhs



  if (length(lhs) == 0) {
    stop("f_lmer() requires at least one response variable on the ",
         "left-hand side of the formula.")
  }
  for (response in response_names) {
    if (!(response %in% names(data))) {
      stop("Response variable '", response, "' is not in the data.")
    }
    if (!is.numeric(data[[response]])) {
      stop("Response variable '", response,
           "' must be numeric for a linear mixed model. ",
           "For binary / count / proportion responses use lme4::glmer() ",
           "or glmmTMB::glmmTMB() directly.")
    }
  }

  # Cache the RHS as a string so we can rebuild a per-response formula
  # inside the loop. paste(deparse(...), collapse = " ") protects against
  # long random-effects expressions that deparse() may split over lines.
  rhs <- paste(deparse(formula[[3]]), collapse = " ")

  ########## Build master analysis frame (one row set, all responses) #######
  # Use a one-sided formula over ALL variables (responses + fixed
  # predictors + random-effect grouping variables, harvested via
  # all.vars()) so base model.frame() doesn't choke on the `|` of the
  # Build the analysis data set ONCE, before the response loop.
  # A single-sided formula listing only the non-bar-separated variables
  # (fixed effects + response + grouping factors) is used for
  # model.frame; lme4 handles random-effects terms via the original
  # random-effects bar. This guarantees every response in a multi-
  # response call is fitted on the identical row set, and bakes in any
  # subset / na.action / weights passed via `...`.
  #
  # Strategy: pre-evaluate subset/weights eagerly against data first,
  # falling back to the user's calling environment. Then apply the
  # filter manually and pass plain values to stats::model.frame via
  # do.call. This avoids the fragile match.call/substitute dance where
  # spliced expressions inside a constructed call can end up containing
  # `..N` dots-index symbols that crash model.frame with "the ... list
  # contains fewer than 3 elements".
  master_formula <- stats::as.formula(
    paste("~", paste(all_vars, collapse = " + "))
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

  # Resolve weights against FULL data before subsetting
  weights_vec <- NULL
  if (!is.null(dots_exprs$weights)) {
    weights_vec <- eval(dots_exprs$weights, envir = data, enclos = caller_env)
  }

  # Apply the manual subset filter to data (and weights, if its length
  # matches the pre-subset row count)
  if (!is.null(subset_vec)) {
    if (!is.null(weights_vec) && length(weights_vec) == nrow(data)) {
      weights_vec <- weights_vec[subset_vec]
    }
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
  if (!is.null(weights_vec)) mf_args$weights <- weights_vec

  data <- do.call(stats::model.frame, mf_args)

  # Extract the weights vector once (NULL if none supplied) for the
  # lmer() calls below.
  mf_weights <- stats::model.weights(data)

  # Remaining ... args (control, contrasts, offset, ...) are evaluated
  # in the caller's frame  these are normal R objects, not column
  # references, so eager evaluation is safe and expected.
  extra_names <- setdiff(names(dots_exprs),
                         c("subset", "weights", "na.action"))
  lmer_extra  <- lapply(dots_exprs[extra_names], eval, envir = caller_env)

  ########## Save-path machinery (mirrors f_kruskal_test) ###################
  temp_output_dir  <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")
  file.create(temp_output_file)
  file_extension <- NULL

  f_wrap_lines()

  if (save_in_wdir == TRUE) {
    save_dir <- getwd()
  } else {
    save_dir <- tempdir()
  }

  output_type_map <- c("pdf"  = ".pdf",
                       "word" = ".docx",
                       "excel" = ".xlsx",
                       "rmd"  = ".rmd")

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
                                   default_name = paste(data_name,
                                                        "lmer_output", sep = "_"),
                                   default_dir  = save_dir,
                                   file.ext     = ".pdf")
      output_type <- "pdf"
    } else if (is.null(file_extension) &&
               output_type %in% c("pdf", "word", "excel", "rmd")) {
      file.ext <- unname(output_type_map[output_type])
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name,
                                                        "lmer_output", sep = "_"),
                                   default_dir  = save_dir,
                                   file.ext     = file.ext)
    } else if (!is.null(file_extension)) {
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name,
                                                        "lmer_output", sep = "_"),
                                   default_dir  = save_dir,
                                   file.ext     = file_extension[1])
      output_type <- file_extension[2]
    }
  } else {
    file.ext <- unname(output_type_map[output_type])
    output_path <- get_save_path(save_as = save_as,
                                 default_name = paste(data_name,
                                                      "lmer_output", sep = "_"),
                                 default_dir  = save_dir,
                                 file.ext     = file.ext)
  }

  if (output_type == "rmd") close_generated_files <- FALSE

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

  ########## Output accumulator #############################################
  output_list <- list()

  ########## Helper: format a p-value #######################################
  fmt_p <- function(p) {
    if (is.na(p)) return("NA")
    if (p < 0.001) "< 0.001" else format(round(p, 4), nsmall = 4)
  }

  ###########################################################################
  ## generate_report() - all side-effecting cat() calls live in here       ##
  ###########################################################################
  generate_report <- function(output = TRUE) {

    ## ---------- Intro / assumptions block --------------------------------
    if (intro_text == TRUE) {
      cat("
# Linear Mixed Models - what they are and when to use them

A linear mixed model (LMM) extends ordinary regression / ANOVA by allowing
**two kinds of effects**:

- **Fixed effects** - factors you actively manipulated or whose specific
  levels you care about (treatment, dose, time, genotype). You estimate them
  with a coefficient and a confidence interval.
- **Random effects** - grouping structure in your data that creates
  non-independence (subjects measured repeatedly, plots within fields, blocks,
  observers, batches, days). You treat the levels as a random sample from
  a larger population and you care about the *variance* they introduce,
  not the individual levels.

Use an LMM whenever observations are **not independent** - whenever some
observations share something that makes them more alike than two random
observations from the dataset. Ignoring this and running a plain `aov`
or `lm` instead is **pseudoreplication** (treating non-independent
observations as if they were independent): standard errors shrink,
p-values shrink, false positives increase.

# Vocabulary

A few terms used throughout this report:

- **Subject** - the experimental unit that is measured repeatedly
  (a person, animal, plot, pot, cell line). In `lme4` syntax this
  is the grouping factor on the right of the `|`: `(1 | subject)`.
- **Within-subject factor** - a predictor whose levels vary *within*
  the same subject. In a longitudinal study this is **time**. In a
  cross-over study it is the **treatment** that every subject receives
  in turn (e.g. diet A, B and C).
- **Between-subject factor** - a predictor whose levels vary *across*
  subjects but are constant within a subject (sex, genotype, treatment
  arm in a parallel-groups trial). Both within- and between-subject
  factors are **fixed** effects in the model.
- **BLUP** - *Best Linear Unbiased Predictor*. The model's estimate of
  the random-effect value for each subject (e.g. how much a particular
  subject deviates from the population intercept). BLUPs are checked
  for normality just like residuals.
- **ICC** - *intraclass correlation coefficient*. The share of total
  variance attributable to between-group differences, on a scale of
  0 to 1. ICC = 0 means the grouping factor is irrelevant;
  ICC = 1 means observations within a group are identical.
- **REML** - *restricted maximum likelihood*. The default fitting
  method for variance components; gives less biased estimates than
  ordinary maximum likelihood.
- **Satterthwaite / Kenward-Roger** - methods to approximate the
  denominator degrees of freedom for fixed-effect p-values, since
  there is no exact df in an LMM.

# Reading the `(1 | group)` syntax

The random-effects part of an `lme4` formula always has the form
`( <varying part> | <grouping factor> )`. The bar `|` reads as
*\"varies by\"*. The grouping factor on the right is what creates the
non-independence. The left side is what you let differ between groups.

+-----------------------------------+----------------------------------------------------------------------------------------+----------------------------------------------+
| Formula term                      | What it means                                                                          | Typical study design                         |
+===================================+========================================================================================+==============================================+
| `(1 | subject)`                   | Random **intercept** per subject. Each subject has its own baseline.                   | Repeated measures; longitudinal data.        |
+-----------------------------------+----------------------------------------------------------------------------------------+----------------------------------------------+
| `(1 | field)`                     | Random intercept per field / plot / block.                                             | Randomised block design; multi-site trial.   |
+-----------------------------------+----------------------------------------------------------------------------------------+----------------------------------------------+
| `(1 | field/plot)`                | `plot` **nested** in `field`. Equivalent to `(1|field) + (1|field:plot)`.              | Split-plot; hierarchical sampling.           |
+-----------------------------------+----------------------------------------------------------------------------------------+----------------------------------------------+
| `(1 + time | subject)`            | Random intercept **and** random slope of `time` per subject. Subjects differ in        | Growth curves with individual trajectories.  |
|                                   | baseline *and* trajectory.                                                             |                                              |
+-----------------------------------+----------------------------------------------------------------------------------------+----------------------------------------------+
| `(1 | subject) + (1 | observer)`  | **Crossed** random effects: every observer can rate every subject.                     | Inter-rater designs; items x raters.         |
+-----------------------------------+----------------------------------------------------------------------------------------+----------------------------------------------+

 **Rule of thumb:** ask yourself *\"if I ran this experiment again, would I get the same group labels, or would I get different ones?\"*
\n- If the labels would be **the same** (e.g. *control* vs *treated*, *male* vs *female*, *low* vs *high* dose) , the levels are the ones you specifically chose and care about; it is a **fixed effect**.
\n- If the labels would likely be **different** (e.g. a new set of subject IDs, different fields, blocks, different observers, different batches), the actual levels are interchangeable and you only care about the variation they introduce; it belongs on the right of a `|` as a **random effect**.


# When to use a Linear Mixed Model

The most common reason is a **repeated-measures design** in which the
same experimental units (people, animals, pots and plots) are measured
on more than one occasion or under more than one treatment. Compared
with a between-groups design analysed by plain ANOVA, this gives two
real advantages: fewer experimental units are needed (each subject acts
as its own control, removing between-subject variation from the
comparison of interest), and individual differences cannot bias the
treatment groups (in a cross-over design every subject receives every
treatment). The cost is that observations from the same subject are
**not independent**, which is exactly the problem an LMM solves by
entering the subject as a random effect (the *\"random intercept model\"*).

Two canonical examples:

- **Longitudinal study:** same subjects measured at several time
  points: `y ~ time + (1 | subject)`. If subjects also differ in *how
  fast* they change, add a random slope:
  `y ~ time + (1 + time | subject)`.
- **Cross-over design:** every subject receives every treatment in
  sequence (typically with a wash-out period in between):
  `y ~ treatment + (1 | subject)`. If carry-over between periods is a
  concern, add `period` as a fixed effect.

LMMs also apply to non-repeated structures that still create
non-independence: randomised block designs, split-plot trials,
multi-site studies, inter-rater designs.

# Assumptions of a Linear Mixed Model

1. **Linearity:** fixed effects are linear in the parameters.
2. **Independence conditional on the random effects** - once the grouping
   structure is accounted for, residuals should be independent. This is
   the whole point of including the random effects in the first place;
   if you still see structure (e.g. autocorrelation in time), you need
   more random effects or a correlation structure (`nlme`, `glmmTMB`).
3. **Normality of level-1 residuals:** checked with a Q-Q plot of
   `residuals(model)`.
4. **Normality of the random-effect BLUPs:** checked with a Q-Q plot of
   `ranef(model)`. This is *separate* from residual normality and is the
   assumption most users forget.
5. **Homoscedasticity:** residual variance is roughly constant across
   fitted values and across levels of the grouping factor (in the *Residuals
   vs Fitted* and *Scale-Location* plots, points should form a featureless
   horizontal band; no funnel, no curve).
6. **Enough levels of the grouping factor:** random-effect variance is
   poorly estimated with fewer than ~5 levels. With 3-4 levels you are
   often better off treating the factor as **fixed**.

If Levene's test or the Shapiro-Wilk tests on residuals or BLUPs
indicate a violation, the report adds a *Recommendations for
Heteroscedasticity and/or non-normal residuals* section after the
diagnostics with concrete next steps.
\n")

      if (output_type != "rmd") {
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage
")
      }
    }

    ## ---------- Multiple-response warning --------------------------------
    ## Mirrors f_aov(): only fires when > 1 response variable is supplied
    ## on the LHS. Shown regardless of intro_text so the user always sees
    ## it in this situation.
    if (length(lhs) > 1) {
      k          <- length(lhs)
      fwer_pct   <- round((1 - (1 - alpha)^k) * 100, 1)
      bonf_alpha <- round(alpha / k, 4)
      cat(paste0(
        "\n\n***\n\n",
        "**[!]  NOTE: Multiple Testing Across ", k, " Response Variables**  \n\n",
        "This report fits ", k, " independent linear mixed models on the same dataset. ",
        "The **", adjust, "** correction keeps each individual model honest, it guards against ",
        "false positives among the pairwise contrasts within a model, but it offers no protection ",
        " against the accumulation of error across all ", k, " tests combined. ",
        "\nAt \u03b1 = ", alpha, " per test, the probability of obtaining at least one ",
        "spurious significant result across all ", k, " responses is approximately ",
        "**", fwer_pct, "%** ($1-(1-", alpha, ")^{", k, "}$, assuming independence). ",
        "This risk is highest in exploratory studies; it is less of a concern when ",
        "each response has a clear a priori hypothesis.  \n\n",
        "**Possible remedies:**  \n",
        "\n-  **Bonferroni** (conservative): re-run with `alpha = ", bonf_alpha,
        "` (\u03b1 / ", k, ").",
        "\n-  **False Discovery Rate (FDR)**: apply `p.adjust(p_values, method = \"fdr\")` ",
        "to the ", k, " per-response fixed-effect p-values after the fact.",
        "\n-  **Multivariate model**: if responses are correlated, fit a joint multivariate ",
        "mixed model (e.g. `MCMCglmm`, `brms`) before interpreting individual responses.",
        "\n-  **Pre-registration**: if each response was a pre-specified (primary) study ",
        "outcome, correction may not be required; document this decision explicitly.  \n  \n",
        "\n\n***\n\n"
      ))
    }

    ## ---------- Per-response loop ----------------------------------------
    ## Each iteration fits its own lmer() with the same RHS but a
    ## different LHS, then writes its results into
    ## output_list[[response_name]][[key]]. This nesting is always used,
    ## regardless of the number of responses, so the returned structure
    ## is consistent with f_aov, f_kruskal_test, f_glm, f_t_test and
    ## f_wilcox_test: result[[response_name]]$<slot>.
    n_responses <- length(lhs)

    # set_out(): write a value to output_list[[response_name]][[key]].
    # Reads `response_name` from the enclosing loop frame via lexical
    # scope, so it MUST be defined inside generate_report (not at
    # f_lmer top level). Hoisted out of the loop so it isn't redefined
    # on every iteration.
    set_out <- function(key, value) {
      if (is.null(output_list[[response_name]])) {
        output_list[[response_name]] <<- list()
      }
      output_list[[response_name]][[key]] <<- value
    }

    for (resp_idx in seq_along(lhs)) {
      response_name <- lhs[[resp_idx]]

      # Build the per-response formula
      current_formula <- stats::as.formula(paste0(response_name, " ~ ", rhs))

    ## ---------- Header ----------------------------------------------------
    cat("\n# Linear Mixed Model: ", response_name, "\n\n")
    cat("**Model:** `", deparse(current_formula), "`  \n", sep = "")
    cat("**Method:** REML = ", REML,
        " &nbsp;&nbsp; **DF method:** ", ddf, "  \n\n", sep = "")

    ## ---------- Fit the model --------------------------------------------
    fit_warnings <- character(0)
    fit_messages <- character(0)
    fit <- withCallingHandlers(
      tryCatch(
        do.call(
          lmerTest::lmer,
          c(list(formula = current_formula, data = data,
                 REML = REML, weights = mf_weights),
            lmer_extra)
        ),
        error = function(e) {
          stop("lme4::lmer() failed to fit the model: ", conditionMessage(e))
        }
      ),
      warning = function(w) {
        fit_warnings <<- c(fit_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        fit_messages <<- c(fit_messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )

    is_singular <- isTRUE(lme4::isSingular(fit, tol = 1e-4))
    conv_msgs   <- fit@optinfo$conv$lme4$messages
    has_conv_warning <- length(conv_msgs) > 0 || length(fit_warnings) > 0

    set_out("model", fit)
    set_out("is_singular", is_singular)
    set_out("convergence_msgs", c(conv_msgs, fit_warnings))

    if (is_singular || has_conv_warning) {
      cat("\n***\n\n")
      if (is_singular) {
        cat("**[!] Singular fit detected.** One or more variance components ",
            "are estimated at (or very close to) zero. This usually means the ",
            "random-effects structure is too complex for the data - often a ",
            "random slope with too few levels, or a grouping factor that does ",
            "not actually create variance. **Simplify the random-effects ",
            "structure before interpreting results.**\n\n", sep = "")
      }
      if (has_conv_warning) {
        cat("**[!] Convergence / fitting messages from lme4:**\n\n")
        for (msg in unique(c(conv_msgs, fit_warnings))) {
          cat("- ", msg, "\n", sep = "")
        }
        cat("\n")
      }
      cat("\n***\n\n")
    }

    ## ---------- Sample size and grouping summary --------------------------
    n_obs    <- stats::nobs(fit)
    re_terms <- lme4::ngrps(fit)
    cat("\n## Sample size and grouping structure\n")
    cat("- **N observations:** ", n_obs, "  \n", sep = "")
    for (gnm in names(re_terms)) {
      cat("- **Levels of `", gnm, "`:** ", re_terms[[gnm]],
          if (re_terms[[gnm]] < 5)
            "  [!] *fewer than 5 levels - variance estimate is unreliable*"
          else "",
          "  \n", sep = "")
    }

    ## ---------- Fixed-effects ANOVA table --------------------------------
    cat("\n## Fixed-effects table (Type III)\n")

    if (ddf == "lme4") {
      coef_tbl <- summary(fit)$coefficients
      anova_tbl <- NULL
      cat("`ddf = \"lme4\"` was requested, so no p-values are reported. ",
          "Only t-statistics are shown.  \n\n", sep = "")
      coef_df <- as.data.frame(coef_tbl)
      coef_df$Term <- rownames(coef_df)
      coef_df <- coef_df[, c("Term", setdiff(names(coef_df), "Term"))]
      rownames(coef_df) <- NULL
      f_pander(f_conditional_round(coef_df, digits = 4))
    } else {
      anova_tbl <- tryCatch(
        as.data.frame(stats::anova(fit, ddf = ddf, type = "III")),
        error = function(e) {
          warning("anova(fit, ddf = '", ddf, "') failed: ",
                  conditionMessage(e),
                  ". Falling back to Satterthwaite.")
          as.data.frame(stats::anova(fit, ddf = "Satterthwaite", type = "III"))
        }
      )
      anova_tbl$Term  <- rownames(anova_tbl)
      anova_tbl$Sig   <- ifelse(anova_tbl[["Pr(>F)"]] < alpha, "*", "")
      # Pretty p-value column for display (numeric kept in output_list)
      anova_display <- anova_tbl
      anova_display[["Pr(>F)"]] <- vapply(anova_display[["Pr(>F)"]],
                                          fmt_p, character(1))
      # Reorder columns
      ord <- c("Term", "Sum Sq", "Mean Sq", "NumDF", "DenDF", "F value",
               "Pr(>F)", "Sig")
      ord <- ord[ord %in% names(anova_display)]
      anova_display <- anova_display[, c(ord, setdiff(names(anova_display), ord)),
                                     drop = FALSE]
      rownames(anova_display) <- NULL
      f_pander(f_conditional_round(anova_display, digits = 4))
      cat("\n`*` marks terms significant at \u03b1 = ", alpha,
          ". DenDF computed via **", ddf, "**.  \n\n", sep = "")
    }

    set_out("fixed_effects", if (ddf == "lme4") coef_df else anova_display)

    ## Coefficient (estimate) table for completeness
    coef_summary <- as.data.frame(summary(fit)$coefficients)
    coef_summary$Term <- rownames(coef_summary)
    coef_summary <- coef_summary[, c("Term",
                                     setdiff(names(coef_summary), "Term"))]
    rownames(coef_summary) <- NULL
    set_out("coefficients", coef_summary)

    cat("\n### Coefficient estimates\n")
    coef_display <- coef_summary
    if ("Pr(>|t|)" %in% names(coef_display)) {
      coef_display[["Pr(>|t|)"]] <- vapply(coef_display[["Pr(>|t|)"]],
                                           fmt_p, character(1))
    }
    f_pander(f_conditional_round(coef_display, digits = 4))

    ## ---------- Random-effects variance components + ICC -----------------
    cat("\n## Random-effects variance components\n")

    vc <- as.data.frame(lme4::VarCorr(fit))
    # vc has columns: grp, var1, var2, vcov, sdcor
    vc_print <- data.frame(
      Group   = vc$grp,
      Term    = ifelse(is.na(vc$var2),
                       ifelse(is.na(vc$var1), "(Intercept)", vc$var1),
                       paste(vc$var1, vc$var2, sep = " : ")),
      Variance = vc$vcov,
      Std.Dev. = vc$sdcor,
      stringsAsFactors = FALSE
    )
    # Residual row has no "term" - blank it out for clarity
    vc_print$Term[vc_print$Group == "Residual"] <- ""
    rownames(vc_print) <- NULL
    f_pander(f_conditional_round(vc_print, digits = 4))

    # ICC for the simplest case: random intercepts only
    # Values are computed here but printed in the combined Model Fit
    # table further down (together with R-squared).
    intercept_rows <- vc[is.na(vc$var2) &
                          (is.na(vc$var1) | vc$var1 == "(Intercept)" |
                             vc$var1 == ""), ]
    resid_row <- vc[vc$grp == "Residual", ]
    icc_table <- NULL
    between_var <- NA_real_
    resid_var   <- NA_real_
    icc_value   <- NA_real_
    if (nrow(intercept_rows) >= 1 && nrow(resid_row) == 1) {
      between_var <- sum(intercept_rows$vcov[intercept_rows$grp != "Residual"],
                         na.rm = TRUE)
      resid_var   <- resid_row$vcov[1]
      total_var   <- between_var + resid_var
      if (!is.na(total_var) && total_var > 0) {
        icc_value <- between_var / total_var
        # Stand-alone icc_table kept in output_list for back-compat /
        # programmatic access. Display happens in the combined block below.
        icc_table <- data.frame(
          var_group = unname(between_var),
          var_resid = unname(resid_var),
          icc       = unname(icc_value),
          stringsAsFactors = FALSE
        )
        names(icc_table) <- c("Between-group variance",
                              "Residual variance",
                              "ICC (between / total)")
        rownames(icc_table) <- NULL
      }
    }

    set_out("var_components", vc_print)
    set_out("icc", icc_table)

    ## ---------- Combined Model Fit table: ICC + R-squared ----------------
    ## Both have very few values; on a typical 78-char console they fit
    ## comfortably side by side as a single horizontal table.
    r2_table   <- NULL
    r2m <- r2c <- NA_real_
    if (requireNamespace("MuMIn", quietly = TRUE)) {
      r2 <- tryCatch(MuMIn::r.squaredGLMM(fit), error = function(e) NULL)
      if (!is.null(r2)) {
        r2m <- unname(r2[1, "R2m"])
        r2c <- unname(r2[1, "R2c"])
        # Stand-alone r2_table kept in output_list for back-compat /
        # programmatic access
        r2_table <- data.frame(marginal = r2m, conditional = r2c,
                               stringsAsFactors = FALSE)
        names(r2_table) <- c("Marginal R\u00b2 (fixed)",
                             "Conditional R\u00b2 (fixed + random)")
        rownames(r2_table) <- NULL
      }
    }
    set_out("r_squared", r2_table)

    # Build the combined display table only with the components that exist
    have_icc <- !is.null(icc_table)
    have_r2  <- !is.null(r2_table)
    if (have_icc || have_r2) {
      cat("\n## Model fit\n")

      fit_cols <- list()
      if (have_icc) {
        fit_cols[["Var(group)"]] <- unname(between_var)
        fit_cols[["Var(resid)"]] <- unname(resid_var)
        fit_cols[["ICC"]]        <- unname(icc_value)
      }
      if (have_r2) {
        fit_cols[["R\u00b2 marg."]] <- r2m
        fit_cols[["R\u00b2 cond."]] <- r2c
      }
      modelfit_tbl <- as.data.frame(fit_cols, check.names = FALSE,
                                    stringsAsFactors = FALSE)
      rownames(modelfit_tbl) <- NULL
      f_pander(f_conditional_round(modelfit_tbl, digits = 4),
               col_width = 14)

      # Compact combined legend
      cat(
        if (have_icc) paste0(
          "\n- **Var(group)** / **Var(resid):** between-group and residual
          variance components.",
          "\n- **ICC** = Var(group) / [Var(group) + Var(resid)]: share of total
          variance attributable to *between*-group differences (0 = grouping
          irrelevant; 1 = within-group observations identical)."
        ) else "",
        if (have_r2) paste0(
          "\n- **R\u00b2 marg.**: variance explained by the **fixed** effects
          alone (Nakagawa & Schielzeth).",
          "\n- **R\u00b2 cond.**: variance explained by **fixed + random**
          effects together. The gap is the variance absorbed by the
          random-effects structure.  \n"
        ) else "",
        sep = "")
    }

    ## ---------- Information criteria -------------------------------------
    dev_value <- if (isTRUE(REML)) {
      tryCatch(lme4::REMLcrit(fit), error = function(e) NA_real_)
    } else {
      tryCatch(stats::deviance(fit), error = function(e) NA_real_)
    }
    dev_label <- if (isTRUE(REML)) "REML criterion" else "deviance"

    fit_indices <- data.frame(
      AIC           = stats::AIC(fit),
      BIC           = stats::BIC(fit),
      logLik        = as.numeric(stats::logLik(fit)),
      `df.residual` = tryCatch(stats::df.residual(fit), error = function(e) NA_real_),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    # Add the deviance / REML criterion column with the right label
    fit_indices[[dev_label]] <- dev_value
    # Reorder so the criterion sits next to logLik for readability
    fit_indices <- fit_indices[, c("AIC", "BIC", "logLik",
                                   dev_label, "df.residual")]
    cat("\n## Information criteria\n")
    f_pander(f_conditional_round(fit_indices, digits = 3),
             col_width = 16)
    cat("\n- **AIC** (Akaike Information Criterion): balances model fit ",
        "against complexity. *Lower is better*, but the absolute number ",
        "is meaningless on its own; use it only to compare models fit to ",
        "the *same* data.",
        "\n- **BIC** (Bayesian Information Criterion): same idea as AIC but ",
        "penalises extra parameters more strongly, so it tends to prefer ",
        "simpler models. *Lower is better*.",
        "\n- **logLik** (log-likelihood): how well the model fits the data. ",
        "*Higher (less negative) is better*. AIC and BIC are derived ",
        "from this.",
        sep = "")
    if (isTRUE(REML)) {
      cat("\n- **REML criterion:** the quantity `lmer()` minimises when ",
          "`REML = TRUE`. Use it to compare models that differ in their ",
          "random-effects structure only (same fixed effects). Do **not** ",
          "compare REML-fitted models that differ in fixed effects; refit ",
          "with `REML = FALSE` for that.  \n", sep = "")
    } else {
      cat("\n- **deviance:** (-2 * logLik): the quantity `lmer()` minimises ",
          "when `REML = FALSE`. Use it to compare nested models that differ ",
          "in fixed effects via likelihood-ratio tests.", sep = "")
    }
    cat("\n- **df residual:** residual degrees of freedom. In an LMM this ",
        "is approximate (no exact df exists), and is not used for ",
        "fixed-effect p-values; those use Satterthwaite or Kenward-Roger ",
        "instead.  \n\n", sep = "")
    set_out("fit_indices", fit_indices)

    if (output_type != "rmd") {
      cat("\n<div style=\"page-break-after: always;\"></div>\n\\newpage\n")
    }

    ## ---------- Diagnostic plots -----------------------------------------
    diag_plots <- list()
    if (norm_plots == TRUE) {
      cat("\n## Diagnostic plots\n")

      resids <- stats::residuals(fit)
      fits   <- stats::fitted(fit)

      # 4-panel diagnostic figure
      temp_diag <- tempfile(fileext = ".png")
      grDevices::png(temp_diag, width = 1800, height = 1400, res = 200)
      par(mfrow = c(2, 2), mar = c(4.2, 4.2, 3, 1))

      # 1: residuals vs fitted
      plot(fits, resids,
           xlab = "Fitted values", ylab = "Residuals",
           main = "Residuals vs Fitted",
           pch = 19, col = adjustcolor("steelblue", alpha.f = 0.6))
      abline(h = 0, col = "tomato", lwd = 2, lty = 2)

      # 2: QQ of level-1 residuals
      stats::qqnorm(resids, main = "Q-Q plot of residuals",
                    pch = 19, col = adjustcolor("steelblue", alpha.f = 0.6))
      stats::qqline(resids, col = "tomato", lwd = 2)

      # 3: QQ of random-effect BLUPs (first grouping factor, intercepts)
      re_list <- lme4::ranef(fit)
      first_grp <- re_list[[1]]
      blups_intercept <- if ("(Intercept)" %in% colnames(first_grp))
        first_grp[, "(Intercept)"] else first_grp[, 1]
      stats::qqnorm(blups_intercept,
                    main = paste0("Q-Q plot of BLUPs: ", names(re_list)[1]),
                    pch = 19, col = adjustcolor("seagreen", alpha.f = 0.7))
      stats::qqline(blups_intercept, col = "tomato", lwd = 2)

      # 4: scale-location
      plot(fits, sqrt(abs(resids)),
           xlab = "Fitted values",
           ylab = expression(sqrt(abs("residuals"))),
           main = "Scale-Location",
           pch = 19, col = adjustcolor("steelblue", alpha.f = 0.6))

      par(mfrow = c(1, 1))
      grDevices::dev.off()

      cat(paste0("![](", temp_diag, ")"), "  \n  \n")
      cat("\n*Top-left:* residuals should scatter randomly around zero with ",
          "no funnel shape. *Top-right:* level-1 residuals should fall on ",
          "the line. *Bottom-left:* the random-effect BLUPs should also be ",
          "approximately normal - this is the LMM-specific assumption most ",
          "users forget. *Bottom-right:* the spread of residuals should be ",
          "roughly constant across fitted values.\n\n", sep = "")

      diag_plots[["diagnostics_png"]] <- temp_diag

      # Shapiro-Wilk on residuals and BLUPs.
      # safe_shapiro() returns a shaped htest for all n regimes:
      # real result for n in [3, 5000], NA p-value with an informative
      # method label otherwise. This lets us tell the user *why* the
      # test was skipped instead of silently omitting the line, and
      # keeps the display code uniform.
      sw_res <- safe_shapiro(resids)
      if (!is.na(sw_res$p.value)) {
        cat("- Shapiro-Wilk on level-1 residuals: W = ",
            round(sw_res$statistic, 4),
            ", p = **", fmt_p(sw_res$p.value), "**", "  \n", sep = "")
      } else {
        cat("- Shapiro-Wilk on level-1 residuals: skipped (",
            sw_res$method, ")  \n", sep = "")
      }

      sw_blup <- safe_shapiro(blups_intercept)
      if (!is.na(sw_blup$p.value)) {
        cat("- Shapiro-Wilk on BLUPs of `", names(re_list)[1], "`: W = ",
            round(sw_blup$statistic, 4),
            ", p = **", fmt_p(sw_blup$p.value), "**", "  \n", sep = "")
      } else {
        cat("- Shapiro-Wilk on BLUPs of `", names(re_list)[1],
            "`: skipped (", sw_blup$method, ")  \n", sep = "")
      }

      # Levene's test on residuals against the first random-effects
      # grouping factor. This is the LMM analog of the Levene's test
      # in f_aov() / f_glm(): it asks whether residual variance is
      # roughly constant across the levels of the grouping factor,
      # which is the homoscedasticity assumption in an LMM.
      # Skipped silently if the grouping factor has < 2 levels with
      # >= 2 observations each, or if anything goes wrong.
      levene_p        <- NA_real_
      levene_stat     <- NA_real_
      levene_grp_name <- NA_character_
      levene_failed   <- FALSE
      tryCatch({
        mf_full  <- stats::model.frame(fit)
        grp_name <- names(re_list)[1]
        if (!is.null(grp_name) && grp_name %in% names(mf_full)) {
          grp_vec    <- mf_full[[grp_name]]
          grp_factor <- as.factor(grp_vec)
          tab_n      <- table(grp_factor)
          n_levels   <- length(tab_n[tab_n >= 2])
          if (n_levels >= 2 && length(resids) == length(grp_vec)) {
            lev_df  <- data.frame(res_lmer = resids, grp_lmer = grp_factor)
            lev_res <- rstatix::levene_test(res_lmer ~ grp_lmer, data = lev_df)
            if (!is.na(lev_res$p)) {
              levene_p        <- lev_res$p
              levene_stat     <- lev_res$statistic
              levene_grp_name <- grp_name
              levene_failed   <- lev_res$p < alpha
            }
          }
        }
      }, error = function(e) {
        # Anything unexpected: skip silently.
      })

      if (!is.na(levene_p)) {
        cat("- Levene's test on residuals (grouped by `", levene_grp_name,
            "`): F = ", round(levene_stat, 4),
            ", p = **", fmt_p(levene_p), "**",
            if (levene_failed)
              " (residuals do **NOT** have equal variance across groups)"
            else
              " (no evidence of heteroscedasticity across groups)",
            "  \n\n", sep = "")
      } else {
        cat("- Levene's test on residuals: skipped (grouping factor has ",
            "fewer than two levels with at least two observations each).  \n\n",
            sep = "")
      }

      cat("\n*Note: Shapiro-Wilk is sensitive to large samples and may flag ",
          "harmless deviations. Trust the Q-Q plot more than the p-value.*\n\n",
          sep = "")

      ## ---------- Conditional recommendations block --------------------
      ## Triggered when Levene's test or either Shapiro-Wilk test
      ## indicates a violation. Replaces the old "What if my residuals
      ## are not normal?" intro section by surfacing the same advice
      ## only when it is actually relevant.
      sw_res_failed  <- !is.na(sw_res$p.value)  && sw_res$p.value  < alpha
      sw_blup_failed <- !is.na(sw_blup$p.value) && sw_blup$p.value < alpha

      if (sw_res_failed || sw_blup_failed || levene_failed) {

        failed_items <- character(0)
        if (levene_failed)
          failed_items <- c(failed_items,
            paste0("Levene's test on residuals (grouped by `",
                   levene_grp_name, "`) p = ", fmt_p(levene_p),
                   " indicates **heteroscedasticity**"))
        if (sw_res_failed)
          failed_items <- c(failed_items,
            paste0("Shapiro-Wilk on level-1 residuals p = ",
                   fmt_p(sw_res$p.value),
                   " indicates **non-normal residuals**"))
        if (sw_blup_failed)
          failed_items <- c(failed_items,
            paste0("Shapiro-Wilk on BLUPs of `", names(re_list)[1],
                   "` p = ", fmt_p(sw_blup$p.value),
                   " indicates **non-normal random-effect BLUPs**"))

        cat("## Recommendations for Heteroscedasticity and/or non-normal residuals\n\n")
        cat("The following diagnostic test(s) flagged a violation at ",
            "\u03b1 = ", alpha, ":\n\n", sep = "")
        for (it in failed_items) cat("- ", it, ".  \n", sep = "")
        cat("\n")

        cat("LMMs are reasonably robust to mild non-normality because the ",
            "random effects absorb a lot of structure that would otherwise ",
            "show up as skew. If a problem remains, the **recommended** fix ",
            "is usually a model with a family that matches the response. ",
            "See `?f_glm` for the choice of family (Gamma / log-normal for ",
            "skewed positive data, Poisson or negative binomial for counts, ",
            "beta for proportions, etc.); the same families are available ",
            "with random effects via `lme4::glmer()` or `glmmTMB::glmmTMB()`.",
            "  \n\n", sep = "")
        cat("This keeps the response on its natural scale and the variance ",
            "components interpretable. As a **last resort** you may transform ",
            "the response manually with `f_boxcox()` or `f_bestNormalize()` ",
            "and refit with `f_lmer(transformed_y ~ ...)`, but be aware that ",
            "**variance components and ICCs computed on a transformed scale ",
            "do not back-transform to the original scale**. Report them with ",
            "care.\n\n", sep = "")
        cat("If only Levene's test is significant (heteroscedasticity ",
            "without non-normal residuals), a model that allows the residual ",
            "variance to differ across groups is also an option ",
            "(e.g. `glmmTMB` with the `dispformula` argument, or ",
            "`nlme::lme` with `weights = varIdent()`).\n\n", sep = "")
      }
    }
    set_out("diagnostics", diag_plots)

    if (output_type != "rmd") {
      cat("\n<div style=\"page-break-after: always;\"></div>\n\\newpage\n")
    }

    ## ---------- Observed Descriptives Table ------------------------------
    ## Raw-data summary (n, mean, sd, se, min/Q1/median/Q3/max) grouped by
    ## the categorical fixed-effect predictors only. Mirrors f_aov() /
    ## f_glm().
    ##
    ## We read column types from the **model frame** of the fitted lmer,
    ## not from the raw `data`, so that on-the-fly conversions in the
    ## formula (e.g. `factor(vs)`, `as.factor(gear)`, `ordered(dose)`)
    ## are honoured. Without this, a numeric column wrapped in `factor()`
    ## would be missed and the table would silently not print.
    ## `terms(fit, fixed.only = TRUE)` strips the random-effects part so
    ## random-effect grouping variables (subject / plot / block) do not
    ## end up as rows. Interaction terms like `"a:b"` are filtered out
    ## because they are not single columns in the model frame; the main
    ## effects of `a` and `b` remain and provide the grouping.
    ##
    ## `mf`, `cat_predictors`, and `numeric_predictors` are deliberately
    ## defined at this top level (with safe empty defaults) so that the
    ## post hoc section below can re-use them without re-parsing the
    ## formula.
    data_summary_table  <- NULL
    cat_predictors      <- character(0)
    numeric_predictors  <- character(0)
    mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
    fixed_labels <- tryCatch(
      attr(stats::terms(fit, fixed.only = TRUE), "term.labels"),
      error = function(e) character(0)
    )
    if (!is.null(mf) && length(fixed_labels) > 0L) {
      fixed_labels <- fixed_labels[fixed_labels %in% names(mf)]
      if (length(fixed_labels) > 0L) {
        is_cat <- vapply(
          mf[fixed_labels],
          function(v) is.factor(v) || is.character(v) || is.logical(v),
          logical(1)
        )
        cat_predictors     <- fixed_labels[is_cat]
        numeric_predictors <- fixed_labels[!is_cat]
      }
    }

    ## Response column in the model frame (handles `log(y) ~ ...` too).
    ## Renamed to avoid shadowing the outer per-response loop counter
    ## `resp_idx` (line ~838), which is used later for page breaks.
    resp_col <- NA_character_
    if (!is.null(mf)) {
      mf_resp_idx <- attr(stats::terms(fit), "response")
      if (length(mf_resp_idx) && mf_resp_idx > 0L) resp_col <- names(mf)[mf_resp_idx]
    }

    if (length(cat_predictors) > 0L && !is.na(resp_col) &&
        is.numeric(mf[[resp_col]])) {
      data_summary_table <- tryCatch(
        f_summary(mf,
                  resp_col,
                  cat_predictors,
                  show_name = FALSE,
                  digits    = NULL)$output_df,
        error = function(e) NULL
      )

      if (!is.null(data_summary_table)) {
        cat(paste("\n## Observed Descriptives Table of: ",
                  deparse(current_formula), "  \n"))
        f_pander(f_conditional_round(data_summary_table, digits = 3,
                                     replace_na = FALSE))
        cat(
          "**TIP:** These values represent your actual observed sample characteristics.
Use this table for *Methods* sections or Supplementary materials (to describe the sample).\n  \n
**CAUTION:** For statistical inference (*p-values* and significance letters) and reporting
main findings in the *Results* section, you **must** use the fixed-effects (ANOVA) table
above and the Emmeans post hoc table below (when shown).\n"
        )
      }
    }
    set_out("data_summary_table", data_summary_table)

    ## ---------- Post hoc on factor fixed effects -------------------------
    ## Matches f_aov(): emmeans are shown for EVERY categorical fixed-effect
    ## term (not only the significant ones). Non-significant terms get their
    ## CLD letters collapsed to "ns" so the reader does not over-interpret
    ## them, but the estimates and CIs remain visible because they are often
    ## needed for manuscript tables.
    ##
    ## Categorical membership is read from `cat_predictors`, computed
    ## earlier from the model frame. This is essential: using the raw
    ## `data` here would miss on-the-fly conversions such as
    ## `factor(vs)` and `trm %in% names(data)` would be FALSE, causing
    ## the loop to silently skip every term (the bug present before).
    posthoc_results <- list()
    if (post_hoc == TRUE && !is.null(anova_tbl)) {
      # Per-term p-value lookup for the ns decision.
      p_lookup <- stats::setNames(anova_tbl[["Pr(>F)"]], anova_tbl$Term)

      # All non-interaction, non-intercept fixed-effect terms that have
      # a p-value. Numeric covariates are noted separately (no pairwise
      # comparisons for a slope).
      candidate_terms <- anova_tbl$Term[
        !is.na(anova_tbl[["Pr(>F)"]]) &
          !grepl(":", anova_tbl$Term) &
          anova_tbl$Term != "(Intercept)"
      ]
      posthoc_terms <- intersect(candidate_terms, cat_predictors)
      numeric_in_anova <- intersect(candidate_terms, numeric_predictors)

      # Overall significance: if no categorical fixed effect reaches
      # alpha, all letters collapse to "ns". We still print the table
      # (useful for manuscript tables and back-transformed responses).
      any_sig <- length(posthoc_terms) > 0L &&
        any(!is.na(p_lookup[posthoc_terms]) &
              p_lookup[posthoc_terms] < alpha)

      if (length(posthoc_terms) > 0L) {
        cat("\n## Post hoc comparisons (`emmeans`, adjust = `", adjust, "`)\n\n",
            sep = "")

        if (!any_sig) {
          cat("**NOTE:** No categorical fixed effect reached ",
              "\u03b1 = ", alpha, ". The emmeans below are shown for ",
              "completeness (estimates and CIs are still useful, ",
              "e.g. for manuscript tables), but all letters are ",
              "flagged `ns` and should not be interpreted as ",
              "significant pairwise differences.\n\n", sep = "")
        }
      }

      for (trm in posthoc_terms) {
        term_p  <- p_lookup[[trm]]
        term_ns <- is.na(term_p) || term_p >= alpha

        ## emmeans() needs the underlying variable name (`vs`), not the
        ## term label as written in the formula (`factor(vs)`).
        ## The reference grid stores variables by their bare name even
        ## when a wrapper is applied. `all.vars(parse(...))` strips the
        ## wrapper: factor(vs) -> "vs", as.factor(gear) -> "gear",
        ## ordered(dose) -> "dose". For already-bare labels ("vs") this
        ## is a no-op.
        emm_spec <- tryCatch(
          all.vars(parse(text = trm)[[1]]),
          error = function(e) trm
        )
        if (length(emm_spec) == 0L) emm_spec <- trm

        emm_df <- switch(ddf,
                         "Satterthwaite"  = "satterthwaite",
                         "Kenward-Roger"  = "kenward-roger",
                         "lme4"           = "asymptotic")
        emm <- tryCatch(
          emmeans::emmeans(fit, specs = emm_spec, lmer.df = emm_df),
          error = function(e) NULL
        )
        if (is.null(emm)) {
          cat("\n### ", trm, "\n\nemmeans failed for this term.\n\n", sep = "")
          next
        }

        pairs_tbl <- tryCatch(
          as.data.frame(pairs(emm, adjust = adjust)),
          error = function(e) NULL
        )
        emm_tbl <- as.data.frame(emm)

        # Compact letter display. When the term is not significant,
        # overwrite the .group column with "ns" so the reader does not
        # over-interpret letter clusters that fall out of an overall
        # non-significant test.
        cld_tbl <- tryCatch(
          as.data.frame(multcomp::cld(emm, adjust = adjust, Letters = letters)),
          error = function(e) NULL
        )
        if (!is.null(cld_tbl) && ".group" %in% names(cld_tbl)) {
          names(cld_tbl)[names(cld_tbl) == ".group"] <- "Letter"
          if (term_ns) cld_tbl$Letter <- "ns"
        }
        ## cld() reorders rows by mean, producing non-sequential
        ## row names (e.g. "2", "1"). f_pander renders these as an
        ## unnamed leading column -- strip them. Matches f_aov() /
        ## f_glm().
        if (!is.null(cld_tbl)) rownames(cld_tbl) <- NULL
        if (!is.null(pairs_tbl)) rownames(pairs_tbl) <- NULL
        rownames(emm_tbl) <- NULL

        cat("\n### ", trm,
            if (term_ns) "  (not significant)" else "",
            "\n\n", sep = "")

        if (term_ns) {
          cat("Term `", trm, "` was not significant (p = ",
              fmt_p(term_p), "). Letters collapsed to `ns`.  \n\n",
              sep = "")
        }

        if (!is.null(cld_tbl)) {
          cat("**Estimated marginal means with compact letter display.** \n",
              sep = "")
          f_pander(f_conditional_round(cld_tbl, digits = 4))
        } else {
          cat("**Estimated marginal means:**  \n\n")
          f_pander(f_conditional_round(emm_tbl, digits = 4))
        }

        n_groups <- if (!is.null(cld_tbl)) nrow(cld_tbl) else nrow(emm_tbl)
        n_pairs  <- if (!is.null(pairs_tbl)) nrow(pairs_tbl) else NA_integer_

        cld_text <- paste0("Confidence level used: ", 1 - alpha, "  \n",
            "Significance level used: \u03b1 = ", alpha, "  \n",
            "P-value and CI adjustment: ", adjust,
            " method for ", n_groups, " estimates",
            if (!is.na(n_pairs)) paste0(" / ", n_pairs, " tests") else "",
            ".  \n\n",
              "*Note: Groups in the \"Letters\" column sharing the same letter are ",
              "**not** significantly different (\u03B1 = ", alpha, "). Groups with ",
              "different letters are significantly different. Sharing a letter ",
              "indicates insufficient evidence to claim a difference; it does not ",
              "prove the groups are identical.*\n\n"
            )

        cat(cld_text)
        set_out("cld_text", cld_text)

        if (!is.null(pairs_tbl)) {
          cat("\n**Pairwise contrasts (", adjust, " adjusted):**  \n\n",
              sep = "")
          f_pander(f_conditional_round(pairs_tbl, digits = 4))
        }

        posthoc_results[[trm]] <- list(
          emmeans = emm_tbl,
          pairs   = pairs_tbl,
          cld     = cld_tbl
        )
      }

      # Note on numeric covariates in the fixed-effects table.
      if (length(numeric_in_anova) > 0L) {
        cat("\n**Note on numeric covariate(s):** ",
            paste(numeric_in_anova, collapse = ", "),
            ". Their slopes are reported in the coefficient table ",
            "above; no pairwise post hoc is performed.  \n\n", sep = "")
      }

      # Note about significant interactions (same behaviour as before).
      int_terms <- anova_tbl$Term[grepl(":", anova_tbl$Term) &
                                    !is.na(anova_tbl[["Pr(>F)"]]) &
                                    anova_tbl[["Pr(>F)"]] < alpha]
      if (length(int_terms) > 0) {
        cat("\n**[!] Significant interaction(s) detected:** ",
            paste(int_terms, collapse = ", "),
            ". Main-effect post hoc tests above should be interpreted with ",
            "care - the effect of one factor depends on the level of another. ",
            "Use `emmeans(model, ~ factor1 | factor2)` manually for ",
            "simple-effects contrasts.\n\n", sep = "")
      }
    }

    set_out("post_hoc", posthoc_results)

      ## ---------- Inter-response page break ------------------------------
      if (n_responses > 1L && resp_idx < n_responses && output_type != "rmd") {
        cat("\n<div style=\"page-break-after: always;\"></div>\n\\newpage\n")
      }
    } # end per-response loop

    if (output == TRUE) return(output_list)
  } # end generate_report

  ###########################################################################
  ## Run the report quietly to populate output_list                        ##
  ###########################################################################
  suppressMessages(
    utils::capture.output(
      output_list <- generate_report(),
      file = nullfile()
    )
  )
  class(output_list) <- "f_lmer"

  ###########################################################################
  ## Output dispatch                                                       ##
  ###########################################################################
  if (output_type %in% c("word", "pdf")) {

    message(paste0("Saving output in: ", output_path))

    word_pdf_preamble <- function() { paste0("
---
title: \"Linear Mixed Model Report\"
date: \"`r Sys.Date()`\"
output:
   word_document:
      reference_docx: !expr system.file(\"rmarkdown/templates/MS_word_style.docx\", package = \"rfriend\")
   pdf_document:
        latex_engine: pdflatex
header-includes:
  - \\usepackage[utf8]{inputenc}
  - \\usepackage{textcomp}
  - \\DeclareUnicodeCharacter{03BB}{\\ensuremath{\\lambda}}
  - \\DeclareUnicodeCharacter{03B1}{\\ensuremath{\\alpha}}
  - \\DeclareUnicodeCharacter{03C7}{\\ensuremath{\\chi}}
  - \\DeclareUnicodeCharacter{03B2}{\\ensuremath{\\beta}}
  - \\DeclareUnicodeCharacter{2212}{\\textminus}
  - \\DeclareUnicodeCharacter{00B2}{\\ensuremath{^2}}
  - \\DeclareUnicodeCharacter{2014}{\\textemdash}
  - \\DeclareUnicodeCharacter{00D7}{\\ensuremath{\\times}}
---
") }

    knitr::opts_chunk$set(comment = "")

    generated_markdown <- capture.output(generate_report(output = FALSE))

    rmd_content <- paste(
      word_pdf_preamble(),
      paste(generated_markdown, collapse = "\n"),
      sep = "\n"
    )

    writeLines(rmd_content, temp_output_file)

    rmarkdown::render(
      temp_output_file,
      output_file       = output_path,
      intermediates_dir = temp_output_dir,
      knit_root_dir     = temp_output_dir,
      quiet             = TRUE,
      output_format     = paste0(output_type, "_document")
    )

    if (open_generated_files == TRUE) f_open_file(output_path)
    return(invisible(output_list))

  } else if (output_type == "excel") {

    message(paste0("Saving output in: ", output_path))

    # Every response is nested under its name. Use the bare slot name
    # (no prefix) when there's only one response, otherwise prefix
    # sheet names with the response to keep them unique.
    sub_iter <- output_list[lhs]
    prefixes <- if (length(lhs) == 1L) "" else paste0(lhs, "_")

    sheets <- list()
    for (i in seq_along(sub_iter)) {
      sub    <- sub_iter[[i]]
      prefix <- prefixes[[i]]
      if (!is.null(sub$data_summary_table))
        sheets[[paste0(prefix, "Data_Summary")]] <- sub$data_summary_table
      if (!is.null(sub$fixed_effects))
        sheets[[paste0(prefix, "Fixed_Effects")]] <- sub$fixed_effects
      if (!is.null(sub$coefficients))
        sheets[[paste0(prefix, "Coefficients")]]  <- sub$coefficients
      if (!is.null(sub$var_components))
        sheets[[paste0(prefix, "Random_Effects")]] <- sub$var_components
      if (!is.null(sub$icc))
        sheets[[paste0(prefix, "ICC")]]           <- sub$icc
      if (!is.null(sub$r_squared))
        sheets[[paste0(prefix, "R_squared")]]     <- sub$r_squared
      if (!is.null(sub$fit_indices))
        sheets[[paste0(prefix, "Fit_Indices")]]   <- sub$fit_indices
      if (length(sub$post_hoc) > 0) {
        for (trm in names(sub$post_hoc)) {
          ph <- sub$post_hoc[[trm]]
          if (!is.null(ph$emmeans))
            sheets[[paste0(prefix, "emmeans_", trm)]] <- ph$emmeans
          if (!is.null(ph$pairs))
            sheets[[paste0(prefix, "pairs_",   trm)]] <- ph$pairs
          if (!is.null(ph$cld))
            sheets[[paste0(prefix, "cld_",     trm)]] <- ph$cld
        }
      }
    }
    # Excel sheet names: max 31 chars, no special chars
    names(sheets) <- substr(gsub("[^A-Za-z0-9_]", "_", names(sheets)), 1, 31)

    writexl::write_xlsx(sheets, path = output_path)

    if (open_generated_files == TRUE) f_open_file(output_path)
    return(invisible(output_list))

  } else if (output_type == "rmd") {

    if (is.null(knitr::opts_knit$get("output.dir"))) {
      knitr::opts_knit$set(output.dir = tempdir())
    }
    generated_markdown <- capture.output(generate_report(output = FALSE))
    output_list[["rmd"]] <- paste(generated_markdown, collapse = "\n")
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


#' @export
print.f_lmer <- function(x, ...) {
  cat("\n==========================================================\n")
  cat("Linear Mixed Model (f_lmer)\n")
  cat("==========================================================\n")

  print_one <- function(sub) {
    if (!is.null(sub$model)) {
      cat("\nFormula: ", deparse(stats::formula(sub$model)), "\n", sep = "")
    }

    if (isTRUE(sub$is_singular)) {
      cat("\n!! SINGULAR FIT - simplify the random-effects structure ",
          "before interpreting results.\n", sep = "")
    }
    if (length(sub$convergence_msgs) > 0) {
      cat("\n!! Convergence messages:\n")
      for (m in unique(sub$convergence_msgs)) cat("   - ", m, "\n", sep = "")
    }

    if (!is.null(sub$fixed_effects)) {
      cat("\n--- Fixed-effects table ---\n")
      f_pander(f_conditional_round(sub$fixed_effects, digits = 4))
    }

    if (!is.null(sub$data_summary_table)) {
      cat("\n--- Observed descriptives (by fixed-effect factor levels) ---\n")
      f_pander(f_conditional_round(sub$data_summary_table, digits = 3,
                                   replace_na = FALSE))
    }

    if (!is.null(sub$var_components)) {
      cat("\n--- Random-effects variance components ---\n")
      f_pander(f_conditional_round(sub$var_components, digits = 4))
    }

    have_icc <- !is.null(sub$icc)
    have_r2  <- !is.null(sub$r_squared)
    if (have_icc || have_r2) {
      cat("\n--- Model fit ---\n")
      fit_cols <- list()
      if (have_icc) {
        fit_cols[["Var(group)"]] <- as.numeric(sub$icc[[1]])
        fit_cols[["Var(resid)"]] <- as.numeric(sub$icc[[2]])
        fit_cols[["ICC"]]        <- as.numeric(sub$icc[[3]])
      }
      if (have_r2) {
        fit_cols[["R\u00b2 marg."]] <- as.numeric(sub$r_squared[[1]])
        fit_cols[["R\u00b2 cond."]] <- as.numeric(sub$r_squared[[2]])
      }
      modelfit_tbl <- as.data.frame(fit_cols, check.names = FALSE,
                                    stringsAsFactors = FALSE)
      rownames(modelfit_tbl) <- NULL
      f_pander(f_conditional_round(modelfit_tbl, digits = 4),
               col_width = 14)

      legend_lines <- character(0)
      if (have_icc) {
        legend_lines <- c(legend_lines,
          "Var(group) / Var(resid): between-group and residual variance components.",
          "ICC = Var(group) / [Var(group) + Var(resid)]: share of total variance attributable to between-group differences (0 = grouping irrelevant; 1 = within-group observations identical).")
      }
      if (have_r2) {
        legend_lines <- c(legend_lines,
          "R\u00b2 marg.: variance explained by the fixed effects alone (Nakagawa & Schielzeth).",
          "R\u00b2 cond.: variance explained by fixed + random effects together. The gap is the variance absorbed by the random-effects structure.")
      }
      for (ln in legend_lines) {
        cat(paste(strwrap(paste("-", ln), width = 76, exdent = 2),
                  collapse = "\n"), "\n", sep = "")
      }
    }

    if (!is.null(sub$fit_indices)) {
      cat("\n--- Information criteria ---\n")
      f_pander(f_conditional_round(sub$fit_indices, digits = 3),
               col_width = 16)
    }

    if (length(sub$post_hoc) > 0) {
      cat("\n--- Post hoc comparisons ---\n")
      for (trm in names(sub$post_hoc)) {
        cat("\n* ", trm, " *\n", sep = "")
        ph <- sub$post_hoc[[trm]]
        if (!is.null(ph$cld)) {
          cat("Estimated marginal means with compact letter display:\n")
          f_pander(f_conditional_round(ph$cld, digits = 4))
          cat(sub$cld_text)
        } else if (!is.null(ph$emmeans)) {
          cat("Estimated marginal means:\n")
          f_pander(f_conditional_round(ph$emmeans, digits = 4))
        }
      }
    }
  }

  # Every f_lmer result is nested under response_name. Non-response
  # meta-slots (e.g. "rmd") are skipped.
  resp_names <- names(x)
  resp_names <- resp_names[!resp_names %in% c("rmd")]

  if (length(resp_names) == 1L) {
    print_one(x[[resp_names]])
  } else {
    for (rn in resp_names) {
      cat("\n----------------------------------------------------------\n")
      cat("Response: ", rn, "\n", sep = "")
      cat("----------------------------------------------------------\n")
      print_one(x[[rn]])
    }
  }

  invisible(x)
}


#' Plot method for f_lmer objects
#'
#' Replays the four-panel diagnostic figure (residuals vs fitted, Q-Q of
#' residuals, Q-Q of random-effect BLUPs, scale-location) produced by
#' \code{f_lmer()}.
#'
#' @param x An object of class \code{f_lmer}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Returns \code{x} invisibly.
#'
#' @export
plot.f_lmer <- function(x, ...) {

  plot_one <- function(sub, label = NULL) {
    png_path <- sub$diagnostics$diagnostics_png
    if (is.null(png_path) || !file.exists(png_path)) {
      if (is.null(sub$model)) {
        message("No diagnostics available", if (!is.null(label)) paste0(" for '", label, "'") else "",
                " - plot was disabled with norm_plots = FALSE.")
        return(invisible(NULL))
      }
      fit <- sub$model
      resids <- stats::residuals(fit)
      fits   <- stats::fitted(fit)
      par(mfrow = c(2, 2), mar = c(4.2, 4.2, 3, 1))
      title_suffix <- if (!is.null(label)) paste0(" (", label, ")") else ""
      plot(fits, resids, xlab = "Fitted", ylab = "Residuals",
           main = paste0("Residuals vs Fitted", title_suffix),
           pch = 19, col = adjustcolor("steelblue", alpha.f = 0.6))
      abline(h = 0, col = "tomato", lwd = 2, lty = 2)
      stats::qqnorm(resids, main = paste0("Q-Q residuals", title_suffix),
                    pch = 19, col = adjustcolor("steelblue", alpha.f = 0.6))
      stats::qqline(resids, col = "tomato", lwd = 2)
      re_list <- lme4::ranef(fit)
      blups <- if ("(Intercept)" %in% colnames(re_list[[1]]))
        re_list[[1]][, "(Intercept)"] else re_list[[1]][, 1]
      stats::qqnorm(blups,
                    main = paste0("Q-Q BLUPs: ", names(re_list)[1], title_suffix),
                    pch = 19, col = adjustcolor("seagreen", alpha.f = 0.7))
      stats::qqline(blups, col = "tomato", lwd = 2)
      plot(fits, sqrt(abs(resids)), xlab = "Fitted",
           ylab = expression(sqrt(abs("residuals"))),
           main = paste0("Scale-Location", title_suffix),
           pch = 19, col = adjustcolor("steelblue", alpha.f = 0.6))
      par(mfrow = c(1, 1))
    } else {
      if (requireNamespace("magick", quietly = TRUE)) {
        img <- magick::image_read(png_path)
        grid::grid.newpage()
        grid::grid.raster(img)
      } else {
        message("Install 'magick' to redisplay diagnostic plots, ",
                "or open the PNG at: ", png_path)
      }
    }
  }

  resp_names <- names(x)
  resp_names <- resp_names[!resp_names %in% c("rmd")]

  if (length(resp_names) == 1L) {
    plot_one(x[[resp_names]])
  } else {
    for (rn in resp_names) plot_one(x[[rn]], label = rn)
  }
  invisible(x)
}
