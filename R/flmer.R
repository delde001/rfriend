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
#' @param diagnostic_plots Logical. If \code{TRUE} (default), diagnostic plots
#'   (residuals vs fitted, Q-Q of level-1 residuals, Q-Q of random-effect
#'   BLUPs, scale-location) are included in the output.
#' @param effect_plot Logical. If \code{TRUE} (default), an estimated
#'   marginal means plot (estimate \eqn{\pm} 95\% CI, with jittered raw
#'   data and compact-letter-display labels) is added after each
#'   categorical fixed-effect term's post hoc table. For a significant
#'   interaction between categorical factors, interaction plots are drawn
#'   instead, matching \code{\link{f_aov}}: a two-way interaction uses an
#'   x-axis factor and a colour trace and is shown in both orientations;
#'   three- and four-way interactions add facet panels for the remaining
#'   factor(s), with one plot per choice of x-axis factor. Interactions of
#'   order greater than four (which would need an illegible nested facet
#'   grid) are skipped with a note, but the interaction cell-means post
#'   hoc table still reports every combination. For a significant
#'   \strong{numeric x categorical} interaction (the covariate slope
#'   differs across factor levels), \code{f_lmer} additionally draws a
#'   slope plot - a scatter of the raw data coloured by the factor with
#'   one model-fitted regression line per level and a confidence band.
#'   This goes beyond \code{\link{f_aov}}, which holds covariates at their
#'   mean and does not plot slopes. Numeric x numeric interactions and
#'   higher-order numeric/categorical mixes have no standard 2-D plot and
#'   are skipped with a note pointing to the coefficient-table slopes and
#'   \code{emmeans::emtrends()}. All effect and interaction plots are
#'   \pkg{ggplot2} objects and are stored in the returned object
#'   (e.g. \code{out$y1$effect_plot_treatment},
#'   \code{out$y1$interaction_plot_a_b_1},
#'   \code{out$y1$interaction_plot_a_b_c_1},
#'   \code{out$y1$interaction_plot_dose_treatment_1} for a slope plot) so
#'   they can be retrieved and customised afterwards (themes, colours,
#'   axis labels, etc.).
#' @param contrast_plots Logical. If \code{TRUE}, a \strong{contrast forest
#'   plot} is added for each categorical post hoc term: one row per pairwise
#'   comparison, showing the estimated difference between two levels with
#'   its confidence interval and a reference line at zero. A CI that
#'   excludes zero indicates a significant difference; because the interval
#'   is on the difference itself, this "excludes zero" reading is exact (it
#'   is the same information the compact-letter display encodes, but it also
#'   shows the direction and magnitude of each difference). Default
#'   \code{FALSE} because the number of pairwise contrasts grows quickly
#'   with the number of factor levels (k levels give k(k-1)/2 contrasts);
#'   turn it on when you want the detailed pairwise picture. No cap is
#'   applied - if you enable it for a many-level factor you will get a tall
#'   figure, which is your choice. Main-effect and interaction contrast
#'   plots are kept separate: main-effect plots are stored as
#'   \code{out$y1$contrast_plot_<term>} (e.g. \code{contrast_plot_treatment})
#'   and rendered with the main-effect post hoc tables, while interaction
#'   cell-contrast plots are stored as
#'   \code{out$y1$interaction_contrast_plot_<term>} (e.g.
#'   \code{interaction_contrast_plot_a_b}) and rendered with the
#'   interaction cell-means tables. Contrast CIs use the same \code{adjust}
#'   method as the post hoc p-values, so figure and table agree.
#' @param norm_plots `r lifecycle::badge("deprecated")` Deprecated in
#'   version 4.0.0. Use \code{diagnostic_plots} instead. If supplied, its
#'   value is passed through to \code{diagnostic_plots} with a warning.
#' @param post_hoc Logical. If \code{TRUE} (default), runs
#'   \code{emmeans::emmeans()} pairwise comparisons \strong{only when}
#'   the linear mixed model finds a significant fixed-effect term
#'   (ANOVA p-value below \code{alpha}). The post hoc is performed on
#'   each significant factor fixed-effect term separately. Numeric
#'   covariates are skipped because their slope is already reported in
#'   the fixed-effects coefficient table; pairwise contrasts are not
#'   meaningful for a continuous predictor. If no fixed-effect term is
#'   significant, no post hoc is run. When a significant interaction
#'   between categorical factors is present, an additional
#'   \strong{cell-means} post hoc table is produced for that interaction
#'   (estimated mean for every factor-level combination, compared
#'   simultaneously, with a compact letter display and pairwise
#'   contrasts), matching \code{\link{f_aov}}. It is stored alongside the
#'   main-effect results under the interaction term name, e.g.
#'   \code{out$y1$post_hoc[["a:b"]]}. In addition, when a main-effect term
#'   takes part in a significant interaction, a caution note is printed
#'   directly above that term's marginal-means table (and the term's
#'   heading is annotated), warning that the marginal means average over
#'   the interacting factor and can hide or reverse the real pattern; the
#'   interaction cell-means table and plot(s) should be read instead.
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
#'   table (\code{fixed_effects}; the displayed table reports NumDF, DenDF,
#'   F and p only, while the full \code{lmerTest} table including the
#'   non-additive Sum Sq / Mean Sq columns is kept in
#'   \code{fixed_effects_full}), the variance components and ICC (plus a
#'   per-grouping-factor ICC breakdown in \code{icc_by_group} when the
#'   model has two or more intercept grouping factors), the R\eqn{^2}
#'   values, the
#'   observed descriptives table (raw-data n, mean, sd, se, min, Q1,
#'   median, Q3, max grouped by the categorical fixed-effect predictors),
#'   post hoc results (if any; per main-effect terms plus, for a
#'   significant categorical interaction, a cell-means entry keyed by the
#'   interaction term name), diagnostic plots, and convergence
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
#' After the diagnostics the report adds a \emph{Recommendations for
#' Heteroscedasticity and/or non-normal residuals} section, but only when a
#' flagged violation survives a follow-up check rather than on a raw
#' significant p-value alone. A significant Levene's test triggers it only
#' when the Scale-Location panel shows a corroborating variance trend across
#' the fitted scale (a by-group Levene test on its own over-fires); a
#' significant Shapiro-Wilk test on the level-1 residuals triggers it only
#' when the rejection reflects genuine skew rather than a few outliers or
#' heavy but symmetric tails (judged by the tail-trimmed Q-Q correlation and
#' the residual skewness); a significant Shapiro-Wilk test on the
#' random-effect BLUPs triggers it directly. The section gives concrete next
#' steps (generalised mixed model, transformation).
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
#'\donttest{
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
#'        diagnostic_plots = FALSE,
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
#'                     diagnostic_plots = FALSE)
#' multi_out$Reaction$fixed_effects
#' multi_out$Reaction2$fixed_effects
#'
#' # 7) Blocks, treatment and time together: a randomized complete
#' #    block design with repeated measures. The bundled plant_trial
#' #    dataset has five field blocks (block), one plant per treatment
#' #    per block measured at three time points (plant_id), four
#' #    treatments and a time factor.
#' data(plant_trial)
#'
#' # 'treatment' and 'time_weeks' are crossed fixed effects (we care about their
#' # main effects and their interaction). 'block' and 'plant' are sources
#' # of unwanted variation we want to account for, not estimate, so
#' # they are random. 'plant_id' is nested in block - written compactly
#' # as (1 | block/plant_id), which expands to
#' # (1 | block) + (1 | block:plant_id). The random plant intercept is
#' # what makes this a repeated-measures model: the three time points
#' # on one plant share that plant's level.
#' f_lmer(height_cm ~ treatment * time_weeks + (1 | block/plant_id),
#'        data = plant_trial)
#'
#' # The same design fitted with Kenward-Roger denominator df (the
#' # gold standard for small, balanced designs like this one), with
#' # the contrast forest plots turned on and the report saved to Word.
#' f_lmer(height_cm ~ treatment * time_weeks + (1 | block/plant_id),
#'        data = plant_trial,
#'        ddf = "Kenward-Roger",
#'        contrast_plots = TRUE,
#'        output_type = "word"
#'        )
#'}
#' @export
f_lmer <- function(
    formula,
    data = NULL,
    REML = TRUE,
    ddf = "Satterthwaite",
    alpha = 0.05,
    adjust = "sidak",
    diagnostic_plots = TRUE,
    effect_plot = TRUE,
    contrast_plots = FALSE,
    post_hoc = TRUE,
    intro_text = TRUE,
    output_type = "default",
    save_as = NULL,
    save_in_wdir = FALSE,
    close_generated_files = FALSE,
    open_generated_files = interactive(),
    norm_plots = lifecycle::deprecated(),
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

  ########## Deprecated arguments ##########################################
  # `norm_plots` was renamed to `diagnostic_plots` in version 4.0.0 because
  # not all tests in the package assume normality; "diagnostic" is the
  # neutral, accurate name across the test family. Soft-deprecated for one
  # release cycle: if supplied, it is honoured but emits a warning and maps
  # to `diagnostic_plots`.
  if (lifecycle::is_present(norm_plots)) {
    lifecycle::deprecate_warn(
      when = "4.0.0",
      what = "f_lmer(norm_plots)",
      with = "f_lmer(diagnostic_plots)"
    )
    diagnostic_plots <- norm_plots
  }

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
  points: `y ~ time + (1 | subject)`. If subjects also differ in
  *how fast* they change, add a random slope:
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
   fitted values and across levels of the grouping factor: in the
   *Residuals vs Fitted* and *Scale-Location* plots, points should form
   a featureless horizontal band; no funnel, no curve.
6. **Enough levels of the grouping factor:** random-effect variance is
   poorly estimated with fewer than ~5 levels. With 3-4 levels you are
   often better off treating the factor as **fixed**.

After the diagnostics the report adds a
*Recommendations for Heteroscedasticity and/or non-normal residuals*
section, but only when a flagged violation survives a follow-up check, not
on a raw significant p-value alone. A significant Levene's test triggers it
only when the Scale-Location panel shows a corroborating variance trend
across the fitted scale (a by-group Levene test on its own over-fires). A
significant Shapiro-Wilk test on the level-1 residuals triggers it only when
the rejection reflects genuine skew rather than a few outliers or heavy but
symmetric tails (judged by the tail-trimmed Q-Q correlation together with
the residual skewness). A significant Shapiro-Wilk test on the
random-effect BLUPs triggers it directly. The section gives concrete next
steps (generalised mixed model, transformation).
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
      # Render the power 1-(1-alpha)^k as plain text rather than inline
      # LaTeX math ($...$). The LaTeX form renders correctly in the pdf
      # path but pandoc's math-to-OOXML conversion drops the exponent in
      # the Word path, so "(1-0.05)^{2}" came out as "(1-0.05)2", which
      # reads as a multiplication. A Unicode superscript is unambiguous in
      # both formats. Map the single digits of k to superscript glyphs;
      # fall back to a caret form for k > 9 (very rare for responses).
      sup_digits <- c("0" = "\u2070", "1" = "\u00b9", "2" = "\u00b2",
                      "3" = "\u00b3", "4" = "\u2074", "5" = "\u2075",
                      "6" = "\u2076", "7" = "\u2077", "8" = "\u2078",
                      "9" = "\u2079")
      k_chars <- strsplit(as.character(k), "")[[1]]
      k_sup   <- if (all(k_chars %in% names(sup_digits)))
        paste0(sup_digits[k_chars], collapse = "") else paste0("^", k)
      fwer_formula <- paste0("1 - (1 - ", alpha, ")", k_sup)
      cat(paste0(
        "\n\n***\n\n",
        "**[!]  NOTE: Multiple Testing Across ", k, " Response Variables**  \n\n",
        "This report fits ", k, " independent linear mixed models on the same dataset. ",
        "The **", adjust, "** correction only controls error *within* a single model ",
        "(across that model's pairwise contrasts, when it has categorical terms that ",
        "trigger post hoc comparisons). It offers no protection ",
        "against the accumulation of error across all ", k, " tests combined. ",
        "\nAt \u03b1 = ", alpha, " per test, the probability of obtaining at least one ",
        "spurious significant result across all ", k, " responses is approximately ",
        "**", fwer_pct, "%** (", fwer_formula, ", assuming independence). ",
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
      # Note: do NOT wrap the inline-code grouping name in a bold span
      # (i.e. avoid "**Levels of `x`:**"). pandoc splits a bold span that
      # contains inline code into separate runs in the Word path, which
      # surfaces as stray "**" markup and doubled spaces. Keep the bold
      # label and the code span as separate, non-nested markup instead.
      cat("- **Levels of** `", gnm, "`**:** ", re_terms[[gnm]],
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
      # Degrees of freedom carry floating-point noise from the
      # Kenward-Roger / Satterthwaite machinery (e.g. DenDF printed as
      # 17.0000000000005). Round to 1 dp for display so the table looks
      # clean; the full-precision values stay in anova_tbl / output_list.
      for (df_col in c("NumDF", "DenDF")) {
        if (df_col %in% names(anova_display)) {
          anova_display[[df_col]] <- round(anova_display[[df_col]], 1)
        }
      }
      anova_display[["Pr(>F)"]] <- vapply(anova_display[["Pr(>F)"]],
                                          fmt_p, character(1))
      # Drop the Sum Sq / Mean Sq columns from the DISPLAYED table. For an
      # REML-fitted LMM these are back-computed by lmerTest from the F
      # statistic and do not form an additive variance decomposition the
      # way a classical aov() table does: they do not sum to a meaningful
      # total, so printing them invites readers to add them up and
      # misinterpret the result. The standard LMM fixed-effects report is
      # NumDF, DenDF, F and p only. The full anova_tbl (including Sum Sq /
      # Mean Sq exactly as lmerTest returned them) is still stored in the
      # returned object via set_out("fixed_effects_full", ...) below for
      # anyone who needs them.
      drop_cols <- c("Sum Sq", "Mean Sq")
      anova_display <- anova_display[, setdiff(names(anova_display), drop_cols),
                                     drop = FALSE]
      # Reorder columns
      ord <- c("Term", "NumDF", "DenDF", "F value", "Pr(>F)", "Sig")
      ord <- ord[ord %in% names(anova_display)]
      anova_display <- anova_display[, c(ord, setdiff(names(anova_display), ord)),
                                     drop = FALSE]
      rownames(anova_display) <- NULL
      f_pander(f_conditional_round(anova_display, digits = 4))
      cat("\n`*` marks terms significant at \u03b1 = ", alpha,
          ". DenDF computed via **", ddf, "**.  \n\n", sep = "")
      # When the model contains an interaction, the Type III F for a
      # main effect and the t-statistic for that term in the coefficient
      # table below are NOT testing the same hypothesis, so t^2 will not
      # equal F. The Type III F averages the main effect over the levels
      # of the interacting term(s) (sum-to-zero / contr.sum coding),
      # whereas the coefficient is the simple effect at the reference
      # level of the interacting factor (treatment coding). Students who
      # see the same predictor with two different test statistics need
      # this stated explicitly; without an interaction the two coincide
      # (t^2 = F exactly), so the note is only shown when it is relevant.
      has_interaction_term <- any(grepl(":", anova_display$Term, fixed = TRUE))
      if (has_interaction_term) {
        cat("*Note: this model contains an interaction, so the Type III ",
            "*F* tests above and the *t* tests in the coefficient table ",
            "below are different hypotheses and *t*\u00b2 will not equal ",
            "*F*. Each Type III *F* averages a main effect over the levels ",
            "of the interacting term(s) (sum-to-zero coding); each ",
            "coefficient is the simple effect at the reference level of ",
            "the interacting factor (treatment coding). Read the Type III ",
            "table for the overall significance of a term and the ",
            "coefficients for level-vs-reference effects.*  \n\n", sep = "")
      }
    }

    set_out("fixed_effects", if (ddf == "lme4") coef_df else anova_display)
    # Keep the full lmerTest anova table (with Sum Sq / Mean Sq) available
    # for programmatic access, even though it is no longer displayed.
    if (ddf != "lme4") set_out("fixed_effects_full", anova_tbl)

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

    ## ---------- Coefficient forest plot ----------------------------------
    ## A forest (dot-and-whisker) plot of the fixed-effect coefficients with
    ## their confidence intervals and a reference line at zero. This is the
    ## standard way to read a model with MANY predictors at a glance: each
    ## coefficient is one row, so it scales to any number of terms, and the
    ## zero line shows directly which effects differ from the reference
    ## (deviation from zero / from the reference level). It complements the
    ## per-term estimated-means plots and post hoc letters, which become
    ## crowded when a factor has many levels.
    ##
    ## CIs use the Wald approximation (Estimate +/- z * SE) on the same
    ## quantities already in the coefficient table, so the plot matches the
    ## table. The intercept is dropped by default: it is the reference-cell
    ## mean, not an "effect", and its magnitude usually dwarfs the others,
    ## squashing the informative rows.
    if (isTRUE(effect_plot)) {
      coef_fp <- tryCatch({
        cf <- coef_summary
        est_col <- intersect(c("Estimate"), names(cf))[1]
        se_col  <- intersect(c("Std. Error", "Std.Error"), names(cf))[1]
        p_col   <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), names(cf))[1]
        if (is.na(est_col) || is.na(se_col)) stop("no estimate/SE columns")

        cf <- cf[cf$Term != "(Intercept)", , drop = FALSE]
        if (nrow(cf) == 0L) stop("only an intercept; nothing to plot")

        zcrit <- stats::qnorm(1 - alpha / 2)
        fp_df <- data.frame(
          label = cf$Term,
          est   = cf[[est_col]],
          lower = cf[[est_col]] - zcrit * cf[[se_col]],
          upper = cf[[est_col]] + zcrit * cf[[se_col]],
          stringsAsFactors = FALSE
        )
        # Coefficient significance is the coefficient p-value (Wald t/z).
        fp_df$sig <- if (!is.na(p_col))
          ifelse(!is.na(cf[[p_col]]) & cf[[p_col]] < alpha,
                 "significant", "not significant")
        else "not significant"

        # Shared forest-plot drawing (helper_forest_plot). Model order is kept
        # so the rows mirror the coefficient table above.
        build_forest_plot(
          fp_df,
          title    = paste0("Fixed-effect coefficients of ", response_name),
          x_label  = "Estimate (95% CI)",
          order_by = "model")
      }, error = function(e) NULL)

      if (!is.null(coef_fp)) {
        set_out("coef_forest_plot", coef_fp)
        tmp_fp <- tempfile(fileext = ".png")
        n_rows <- length(levels(coef_fp$data$label))
        suppressMessages(
          ggplot2::ggsave(filename = tmp_fp, plot = coef_fp,
                          width = 7,
                          height = max(3, 0.45 * n_rows + 1.5),
                          units = "in", dpi = 200)
        )
        cat("\n### Coefficient forest plot\n")
        cat(paste0("![](", tmp_fp, ")"), "   \n  \n")

        ## Reference-level caption. The meaning of every factor coefficient
        ## depends on which level is the reference, so name them explicitly
        ## (helper_coef_ref_caption). lmer needs fixed.only = TRUE so the
        ## grouping factors are not mistaken for predictors.
        ref_cap <- build_coef_ref_caption(fit, fixed_only = TRUE)

        cat("*Each row is a fixed-effect coefficient (the intercept is ",
            "omitted) with its ", 100 * (1 - alpha), "% Wald CI. ",
            "The dashed line marks zero: a coefficient at zero has no effect ",
            "relative to its reference. Points to the right increase the ",
            "response, points to the left decrease it. A CI that touches or ",
            "crosses zero means the term is not distinguishable from its ",
            "reference at \u03b1 = ", alpha, "; a CI clear of zero is a ",
            "significant effect.*  \n  \n", sep = "")

        if (nzchar(ref_cap$reference))
          cat("*", ref_cap$reference, "*  \n", sep = "")
        if (nzchar(ref_cap$continuous))
          cat("*", ref_cap$continuous, "*  \n", sep = "")
        # The level-vs-reference / pairwise note only makes sense when at
        # least one fixed effect is categorical. nzchar(ref_cap$reference)
        # is TRUE exactly when build_coef_ref_caption() found a factor /
        # logical predictor, so reuse it here. For an all-continuous model
        # (e.g. Reaction ~ Days) this sentence is suppressed.
        if (nzchar(ref_cap$reference))
          cat(" *For a factor with k levels these are level-vs-reference ",
              "contrasts, not all pairwise comparisons; see the post hoc ",
              "tables below for the full pairwise picture.* ", sep = "")
        if (nzchar(ref_cap$how_to))
          cat("*", ref_cap$how_to, "*  \n  \n", sep = "")
      }
    }

    ## ---------- Random-effects variance components + ICC -----------------
    cat("\n## Random-effects variance components\n")

    vc <- as.data.frame(lme4::VarCorr(fit))
    # vc has columns: grp, var1, var2, vcov, sdcor.
    # Two kinds of row exist and MUST NOT be conflated:
    #   - Diagonal terms (var2 is NA): vcov is a *variance*, sdcor is its
    #     *standard deviation* (sd = sqrt(var)).
    #   - Off-diagonal terms (var2 not NA, e.g. a random intercept/slope
    #     pair): vcov is the *covariance* between the two terms and sdcor
    #     is their *correlation* (bounded in [-1, 1]). It is NOT a
    #     variance/SD pair - labelling it "Variance"/"Std.Dev." (as earlier
    #     versions did) is a factual error, since e.g. a correlation of
    #     0.066 cannot be the SD of a covariance of 9.6.
    # We therefore print the diagonal terms with Variance + Std.Dev. and
    # attach the correlation of each off-diagonal pair on the row of its
    # first term, in a dedicated Corr column - matching lme4's own
    # summary() layout.
    is_offdiag <- !is.na(vc$var2)

    diag_df <- vc[!is_offdiag, , drop = FALSE]
    vc_print <- data.frame(
      Group    = diag_df$grp,
      Term     = ifelse(is.na(diag_df$var1), "(Intercept)", diag_df$var1),
      Variance = diag_df$vcov,
      Std.Dev. = diag_df$sdcor,
      Corr     = NA_real_,
      stringsAsFactors = FALSE
    )
    # Residual row has no "term" - blank it out for clarity
    vc_print$Term[vc_print$Group == "Residual"] <- ""

    # Attach each off-diagonal correlation to the row of its first variable
    # within the same grouping factor (e.g. the "(Intercept)" row of
    # Subject carries the intercept-slope correlation). If several
    # correlations share a first variable (3+ correlated terms), keep them
    # as extra rows so none is dropped.
    if (any(is_offdiag)) {
      off_df <- vc[is_offdiag, , drop = FALSE]
      extra_rows <- vc_print[0, , drop = FALSE]
      for (i in seq_len(nrow(off_df))) {
        g  <- off_df$grp[i]
        v1 <- off_df$var1[i]
        v2 <- off_df$var2[i]
        target <- which(vc_print$Group == g & vc_print$Term == v1 &
                          is.na(vc_print$Corr))
        if (length(target) >= 1L) {
          vc_print$Corr[target[1]] <- off_df$sdcor[i]
        } else {
          # No free home row: add an explicit pair row so the correlation
          # is still reported rather than silently discarded.
          extra_rows <- rbind(extra_rows, data.frame(
            Group = g, Term = paste(v1, v2, sep = " : "),
            Variance = NA_real_, Std.Dev. = NA_real_,
            Corr = off_df$sdcor[i], stringsAsFactors = FALSE))
        }
      }
      if (nrow(extra_rows) > 0L) vc_print <- rbind(vc_print, extra_rows)
    }

    # Drop the Corr column entirely when there are no correlations to show
    # (e.g. random-intercept-only models), to keep the table clean.
    if (all(is.na(vc_print$Corr))) vc_print$Corr <- NULL
    rownames(vc_print) <- NULL
    f_pander(f_conditional_round(vc_print, digits = 4))
    if (any(is_offdiag)) {
      cat("\n- **Corr:** correlation between the random terms within a ",
          "grouping factor (e.g. between a subject's intercept and slope), ",
          "bounded in [-1, 1]. It is *not* a variance or standard ",
          "deviation.  \n", sep = "")
    }

    # ICC for the simplest case: random intercepts only
    # Values are computed here but printed in the combined Model Fit
    # table further down (together with R-squared).
    #
    # A clean, single ICC only exists for random-INTERCEPT models. With a
    # random slope the total between-group variance depends on the value of
    # the slope covariate (it equals Var_int + 2*x*Cov + x^2*Var_slope), so
    # no single ICC number is well defined. We still report an
    # intercept-only ICC for orientation, but flag it as approximate so it
    # is not read as a clean fact. has_random_slope is TRUE whenever any
    # grouping factor has a non-intercept random term.
    has_random_slope <- any(!is.na(vc$var1) & vc$var1 != "(Intercept)" &
                              vc$var1 != "" & vc$grp != "Residual")
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
        names(icc_table) <- c(
          if (has_random_slope) "Between-group variance (intercept only)"
            else "Between-group variance",
          "Residual variance",
          if (has_random_slope) "ICC (approx., random slope present)"
            else "ICC (between / total)")
        rownames(icc_table) <- NULL
      }
    }

    set_out("var_components", vc_print)
    set_out("icc", icc_table)

    ## ---------- Per-grouping-factor ICC (nested / crossed designs) --------
    ## The single ICC reported in the Model fit table below sums every
    ## random-intercept variance into one Var(group) and divides by the
    ## total. For a model with more than one grouping factor (e.g. a nested
    ## block/plant design or crossed subject/observer effects) that lumped
    ## number hides WHERE the clustering sits: a design can have a high
    ## overall ICC that is almost entirely plant-level with negligible
    ## block-level structure. So when there are 2+ intercept grouping
    ## factors we additionally report a per-factor breakdown: each factor's
    ## own intercept variance and its share of the total variance
    ## (var_factor / [sum of all group variances + residual]). These
    ## per-factor shares plus the residual share sum to 1. They are only
    ## defined for random-INTERCEPT terms; a factor that also carries a
    ## random slope is marked approximate for the same reason the overall
    ## ICC is (its between-group variance depends on the slope covariate).
    icc_by_group <- NULL
    grp_int_rows <- intercept_rows[intercept_rows$grp != "Residual", ,
                                   drop = FALSE]
    if (nrow(grp_int_rows) >= 2 && !is.na(resid_var) &&
        !is.na(between_var)) {
      total_var_bg <- between_var + resid_var
      if (!is.na(total_var_bg) && total_var_bg > 0) {
        # Which grouping factors carry a random slope (share-of-variance
        # is then intercept-only and flagged approximate for that factor).
        slope_grps <- unique(vc$grp[!is.na(vc$var1) &
                                      vc$var1 != "(Intercept)" &
                                      vc$var1 != "" & vc$grp != "Residual"])
        icc_by_group <- data.frame(
          Group        = grp_int_rows$grp,
          Variance     = grp_int_rows$vcov,
          `ICC (share of total)` = grp_int_rows$vcov / total_var_bg,
          Approx        = ifelse(grp_int_rows$grp %in% slope_grps, "*", ""),
          check.names   = FALSE,
          stringsAsFactors = FALSE
        )
        # Append the residual as its own share so the column sums to 1,
        # making the decomposition self-evidently complete to the reader.
        icc_by_group <- rbind(
          icc_by_group,
          data.frame(Group = "Residual", Variance = resid_var,
                     `ICC (share of total)` = resid_var / total_var_bg,
                     Approx = "", check.names = FALSE,
                     stringsAsFactors = FALSE))
        rownames(icc_by_group) <- NULL

        cat("\n## ICC by grouping factor\n")
        cat("Each grouping factor's random-intercept variance as a share ",
            "of the total variance (group variances + residual). The ",
            "shares sum to 1, so this shows *where* the clustering sits, ",
            "which the single overall ICC in the Model fit table cannot.  \n\n",
            sep = "")
        icc_disp <- icc_by_group
        if (all(icc_disp$Approx == "")) icc_disp$Approx <- NULL
        f_pander(f_conditional_round(icc_disp, digits = 4))
        if (any(icc_by_group$Approx == "*")) {
          cat("\n- `*` this grouping factor also carries a random slope, ",
              "so its between-group variance depends on the slope covariate ",
              "and the reported share (intercept variance only) is ",
              "approximate. See `performance::icc()` for an adjusted ",
              "version.  \n", sep = "")
        }
      }
    }
    set_out("icc_by_group", icc_by_group)

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

      # For random-slope models the ICC and Var(group) only use the
      # intercept variance, so they are intercept-conditional, not the
      # whole story. Mark the columns and explain below. An ASCII marker is
      # used (not a dagger) so the label survives non-UTF-8 locales.
      icc_lbl <- if (has_random_slope) "ICC (approx)"        else "ICC"
      vg_lbl  <- if (has_random_slope) "Var(group, int)"     else "Var(group)"

      fit_cols <- list()
      if (have_icc) {
        fit_cols[[vg_lbl]]       <- unname(between_var)
        fit_cols[["Var(resid)"]] <- unname(resid_var)
        fit_cols[[icc_lbl]]      <- unname(icc_value)
      }
      if (have_r2) {
        fit_cols[["R\u00b2 marg."]] <- r2m
        fit_cols[["R\u00b2 cond."]] <- r2c
      }
      modelfit_tbl <- as.data.frame(fit_cols, check.names = FALSE,
                                    stringsAsFactors = FALSE)
      rownames(modelfit_tbl) <- NULL
      # col_width must clear the widest header. "Var(group, int)" is 15
      # characters; at col_width = 14 insert_newline() split it right before
      # the ")", so the rendered header read "Var(group, int" + line break +
      # ")" (a stray "Var(group, int )"). 16 keeps it on one line while
      # still wrapping anything genuinely long.
      f_pander(f_conditional_round(modelfit_tbl, digits = 4),
               col_width = 16)

      # Compact combined legend
      cat(
        if (have_icc) paste0(
          "\n- **Var(group)** / **Var(resid):** between-group and residual
          variance components.",
          "\n- **ICC** = Var(group) / [Var(group) + Var(resid)]: share of total
          variance attributable to *between*-group differences (0 = grouping
          irrelevant; 1 = within-group observations identical).",
          if (!is.null(icc_by_group)) paste0(
          "\n- With more than one grouping factor this is the **adjusted
          (total) ICC**: it sums *all* grouping-factor variances over the
          total. See the *ICC by grouping factor* table above to see where
          the clustering actually sits.") else "",
          if (has_random_slope) paste0(
          "\n- **This model has a random slope** (note the *approx* / *int*
          labels). The reported ICC and Var(group) use the
          random-**intercept** variance only. The true between-group
          variance changes with the slope covariate
          (Var_int + 2 * x * Cov + x^2 * Var_slope), so a single ICC is only
          approximate here; treat it as an intercept-conditional reference
          value, not an exact quantity. See `performance::icc()` for an
          adjusted version.") else ""
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

    # if (output_type != "rmd") {
    #   cat("\n<div style=\"page-break-after: always;\"></div>\n\\newpage\n")
    # }

    ## ---------- Diagnostic plots -----------------------------------------
    diag_plots <- list()
    if (diagnostic_plots == TRUE) {
      cat("\n## Diagnostic plots\n")

      resids <- stats::residuals(fit)
      fits   <- stats::fitted(fit)

      # Scale-Location trend signal. The bottom-right diagnostic panel
      # plots sqrt(|residual|) against fitted values; under
      # homoscedasticity it is a flat, featureless band. A genuine
      # variance trend shows up as a monotone slope, so we summarise the
      # panel with the Spearman correlation between sqrt(|residual|) and
      # the fitted values (rank-based, so a few outliers do not dominate).
      # This is used below to corroborate a significant by-group Levene
      # test before recommending a remedy: a by-group Levene test alone
      # over-fires (it flags unequal residual variance across grouping
      # levels even when variance is constant across the fitted scale,
      # which is the assumption that actually matters and which the
      # Scale-Location panel reads). NA if it cannot be computed.
      scale_loc_cor <- tryCatch(
        suppressWarnings(stats::cor(fits, sqrt(abs(resids)),
                                    method = "spearman")),
        error = function(e) NA_real_)
      # Threshold above which the Scale-Location panel is considered to
      # show a real variance trend. 0.3 is a deliberately lenient bar (a
      # weak-to-moderate monotone trend); the point is only to stop the
      # recommendations block firing when the panel is essentially flat.
      scale_loc_thresh <- 0.3

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

      # Is a significant Shapiro-Wilk driven by a few outliers / heavy but
      # symmetric tails (to which LMMs are robust), or by systematic skew
      # (which warrants a different family / transformation)? Raw Q-Q
      # quantile correlation alone is misleading here: a couple of outliers
      # drag it down even when the bulk of the plot is straight (sleepstudy
      # residuals give ~0.945). So we use two robust signals together:
      #   - trimmed Q-Q correlation: drop the extreme 5% of points in each
      #     tail and recompute. If the plot is straight once outliers are
      #     removed, this jumps close to 1 (sleepstudy: ~0.998).
      #   - skewness: near zero means symmetric tails, not a skewed
      #     distribution.
      # "Q-Q roughly linear" = trimmed correlation high AND skew small.
      # This keeps the hard violation flag for genuinely curved / skewed
      # Q-Q plots while softening it for the outlier/heavy-tail case.
      qq_trim_thresh <- 0.99   # trimmed quantile correlation must clear this
      qq_skew_thresh <- 0.5    # |skewness| must be below this
      qq_res <- tryCatch({
        rq <- stats::qqnorm(resids, plot.it = FALSE)
        full_cor <- stats::cor(rq$x, rq$y)
        n_r <- length(resids)
        k   <- ceiling(0.05 * n_r)
        if (n_r - 2L * k >= 3L) {
          ord  <- order(rq$x)
          keep <- ord[(k + 1L):(n_r - k)]
          trim_cor <- stats::cor(rq$x[keep], rq$y[keep])
        } else {
          trim_cor <- full_cor
        }
        cr   <- resids - mean(resids)
        skew <- mean(cr^3) / (mean(cr^2))^1.5
        list(full_cor = full_cor, trim_cor = trim_cor, skew = skew)
      }, error = function(e) NULL)
      resid_qq_cor      <- if (is.null(qq_res)) NA_real_ else qq_res$full_cor
      resid_qq_trim_cor <- if (is.null(qq_res)) NA_real_ else qq_res$trim_cor
      resid_skew        <- if (is.null(qq_res)) NA_real_ else qq_res$skew

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
      #
      # The grouping name from ranef() is not always a single column of
      # model.frame(). For a nested or crossed spec such as
      # (1 | block/plant_id), ranef() reports the group as
      # "plant_id:block", but the model frame only holds the component
      # columns "block" and "plant_id" separately. Looking the name up
      # directly therefore fails and the test was being skipped with a
      # misleading "fewer than two levels" message even though the group
      # (e.g. 20 plant-in-block units, 3 obs each) is perfectly testable.
      # resolve_group_factor() reconstructs the grouping factor: a direct
      # column if it exists, otherwise the interaction of the colon-joined
      # component columns. Returns NULL only when the name genuinely cannot
      # be resolved, which is then reported as its own explicit reason
      # rather than masquerading as a level-count problem.
      resolve_group_factor <- function(grp_name, mf) {
        if (is.null(grp_name) || is.na(grp_name)) return(NULL)
        if (grp_name %in% names(mf)) return(as.factor(mf[[grp_name]]))
        parts <- strsplit(grp_name, ":", fixed = TRUE)[[1]]
        if (length(parts) >= 2 && all(parts %in% names(mf))) {
          cols <- lapply(parts, function(p) as.factor(mf[[p]]))
          return(interaction(cols, drop = TRUE, sep = ":"))
        }
        NULL
      }

      levene_p          <- NA_real_
      levene_stat       <- NA_real_
      levene_grp_name   <- NA_character_
      levene_failed     <- FALSE
      levene_skip_reason <- NA_character_
      tryCatch({
        mf_full  <- stats::model.frame(fit)
        grp_name <- names(re_list)[1]
        grp_factor <- resolve_group_factor(grp_name, mf_full)
        if (is.null(grp_factor)) {
          levene_skip_reason <- paste0(
            "could not reconstruct the grouping factor `", grp_name,
            "` from the model frame")
        } else if (length(resids) != length(grp_factor)) {
          levene_skip_reason <- paste0(
            "residual and grouping-factor lengths differ (",
            length(resids), " vs ", length(grp_factor), ")")
        } else {
          tab_n    <- table(grp_factor)
          n_levels <- length(tab_n[tab_n >= 2])
          if (n_levels < 2) {
            levene_skip_reason <- paste0(
              "grouping factor `", grp_name, "` has fewer than two levels ",
              "with at least two observations each")
          } else {
            lev_df  <- data.frame(res_lmer = resids, grp_lmer = grp_factor)
            lev_res <- rstatix::levene_test(res_lmer ~ grp_lmer, data = lev_df)
            if (!is.na(lev_res$p)) {
              levene_p        <- lev_res$p
              levene_stat     <- lev_res$statistic
              levene_grp_name <- grp_name
              levene_failed   <- lev_res$p < alpha
            } else {
              levene_skip_reason <- "Levene's test returned a missing p-value"
            }
          }
        }
      }, error = function(e) {
        # Disclosed failure: record why rather than skipping silently.
        levene_skip_reason <<- paste0("Levene's test errored: ",
                                      conditionMessage(e))
      })

      if (!is.na(levene_p)) {
        cat("- Levene's test on residuals (grouped by `", levene_grp_name,
            "`): F = ", round(levene_stat, 4),
            ", p = **", fmt_p(levene_p), "**",
            if (levene_failed)
              paste0(" (residual variance differs across the levels of ",
                     "this grouping factor; this is not the same as a ",
                     "variance trend across the fitted scale, so read the ",
                     "Scale-Location panel before acting on it)")
            else
              " (no evidence of unequal residual variance across groups)",
            "  \n\n", sep = "")
        if (levene_failed && !is.na(scale_loc_cor)) {
          cat("  - Scale-Location trend (Spearman corr. of ",
              "sqrt(|residual|) vs fitted) = **", round(scale_loc_cor, 4),
              "**", if (abs(scale_loc_cor) >= scale_loc_thresh)
                ", which corroborates a variance trend across the fitted scale."
              else
                paste0(", i.e. essentially flat: the Scale-Location panel ",
                       "does not show a variance trend across the fitted ",
                       "scale, so the Levene flag most likely reflects a few ",
                       "groups with differing spread rather than model-wide ",
                       "heteroscedasticity."),
              "  \n\n", sep = "")
        }
      } else {
        cat("- Levene's test on residuals: skipped (",
            if (!is.na(levene_skip_reason)) levene_skip_reason
            else "grouping factor not testable",
            ").  \n\n", sep = "")
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

      # A significant Shapiro-Wilk on the residuals is downgraded from a
      # hard "violation" to a soft note when the residual Q-Q plot is still
      # essentially straight (high quantile correlation). In that case the
      # rejection is typically driven by a few outliers or large n, not by
      # systematic non-normality, and LMMs are robust to it - so pushing a
      # GLM/transformation here would over-trigger (sleepstudy is the
      # textbook example). The flag is kept hard when the Q-Q is genuinely
      # curved or the correlation could not be computed.
      sw_res_qq_linear <- !is.na(resid_qq_trim_cor) && !is.na(resid_skew) &&
        resid_qq_trim_cor >= qq_trim_thresh &&
        abs(resid_skew) < qq_skew_thresh
      sw_res_violation <- sw_res_failed && !sw_res_qq_linear

      # A by-group Levene test on its own over-fires (see the comment where
      # scale_loc_cor is computed). Only treat heteroscedasticity as a
      # reportable violation when the by-group Levene test is significant
      # AND the Scale-Location panel shows a corroborating variance trend
      # across the fitted scale. If the Scale-Location signal could not be
      # computed (NA), fall back to the Levene flag alone rather than
      # silently suppressing a possible problem (fail loud, not silent).
      levene_corroborated <- levene_failed &&
        (is.na(scale_loc_cor) || abs(scale_loc_cor) >= scale_loc_thresh)

      if (sw_res_failed && sw_res_qq_linear) {
        cat("\n*Note: Shapiro-Wilk on the level-1 residuals is significant ",
            "(p = ", fmt_p(sw_res$p.value), "), but the residual Q-Q plot is ",
            "essentially straight apart from the tails (tail-trimmed ",
            "quantile correlation = ", round(resid_qq_trim_cor, 4),
            ", skewness = ", round(resid_skew, 4), "). This pattern reflects ",
            "a few outliers or heavy but symmetric tails rather than ",
            "systematic skew, and linear mixed models are robust to it. No ",
            "transformation or change of family is recommended on this basis ",
            "alone; inspect the Q-Q plot and any flagged outliers instead.*",
            "\n\n", sep = "")
      }

      if (sw_res_violation || sw_blup_failed || levene_corroborated) {

        failed_items <- character(0)
        if (levene_corroborated)
          failed_items <- c(failed_items,
            paste0("Levene's test on residuals (grouped by `",
                   levene_grp_name, "`) p = ", fmt_p(levene_p),
                   if (!is.na(scale_loc_cor))
                     paste0(", corroborated by a Scale-Location trend ",
                            "(Spearman corr. = ", round(scale_loc_cor, 4), ")")
                   else "",
                   " indicates **heteroscedasticity**"))
        if (sw_res_violation)
          failed_items <- c(failed_items,
            paste0("Shapiro-Wilk on level-1 residuals p = ",
                   fmt_p(sw_res$p.value),
                   " indicates **non-normal residuals**",
                   if (!is.na(resid_skew))
                     paste0(" (skewness = ", round(resid_skew, 4),
                            if (!is.na(resid_qq_trim_cor))
                              paste0(", tail-trimmed Q-Q correlation = ",
                                     round(resid_qq_trim_cor, 4)) else "",
                            ", so this is not merely a tail/outlier effect)")
                   else ""))
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
above and the Emmeans post hoc table below (when shown). Note also that these rows pool
over any repeated measurements and other predictors not shown as grouping columns, so the
*sd*/*se* mix within- and between-subject variation and the observations within a row are
not independent; do not report them as clean per-group standard deviations.\n"
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

      # Significant interaction terms (e.g. "a:b"). Computed here, before the
      # per-term loop, so each main-effect post hoc table can warn inline when
      # the factor it summarises also takes part in a significant interaction.
      # A main effect read in isolation is misleading under a real interaction:
      # the marginal means average over the other factor, hiding the fact that
      # the effect's size or direction changes across that factor's levels.
      sig_int_terms <- anova_tbl$Term[grepl(":", anova_tbl$Term) &
                                        !is.na(anova_tbl[["Pr(>F)"]]) &
                                        anova_tbl[["Pr(>F)"]] < alpha]

      # Interaction terms present in the model but NOT significant. Used to
      # suggest refitting without the interaction term so the main effects
      # are estimated with more power (when the design and research question
      # allow it).
      ns_int_terms <- anova_tbl$Term[grepl(":", anova_tbl$Term) &
                                       (is.na(anova_tbl[["Pr(>F)"]]) |
                                          anova_tbl[["Pr(>F)"]] >= alpha)]

      # For a main-effect term, return the significant interaction terms it is
      # a component of (matched on the bare variable name, so "vs" matches the
      # interaction "vs:am" even when the formula wrote "factor(vs)").
      interactions_involving <- function(term_label) {
        bare <- tryCatch(all.vars(parse(text = term_label)[[1]])[1],
                         error = function(e) term_label)
        if (length(bare) == 0L || is.na(bare)) bare <- term_label
        keep <- vapply(sig_int_terms, function(it) {
          comps <- strsplit(it, ":", fixed = TRUE)[[1]]
          comps <- vapply(comps, function(p)
            tryCatch(all.vars(parse(text = p)[[1]])[1],
                     error = function(e) p), character(1))
          bare %in% comps
        }, logical(1))
        sig_int_terms[keep]
      }

      # ----------------------------------------------------------------
      # Shared figure-caption helpers (Rules 1-3 of the per-plot letter /
      # uniform caption spec). Defined together so the per-plot letter
      # logic and the caption assembler stay next to each other and are
      # easy to keep in sync with f_aov().
      #
      # Caption parts (all kept ASCII; non-ASCII via \uXXXX escapes):
      #   eff_note_est  -> points / estimates / CI (always relevant).
      #                    f_lmer fits the LMM on the raw response (no
      #                    Box-Cox / bestNormalize), so estimates are on
      #                    the response scale and the linear-model
      #                    wording is appropriate. Matches the
      #                    untransformed branch of f_aov()'s eff_note_est.
      #   eff_note_let  -> how to read the grouping letters (only shown
      #                    when the figure actually carries letters).
      eff_note_est <- paste0(
        "Points are (jittered) raw data; estimates are model estimated ",
        "marginal means with ", 100 * (1 - alpha), "% CI."
      )
      eff_note_let <- paste0(
        " Groups sharing a letter are not significantly different ",
        "(\u03b1 = ", alpha, "); groups with different letters are ",
        "significantly different. Sharing a letter indicates insufficient ",
        "evidence of a difference, not proof that the groups are identical."
      )

      # Per-plot compact letters with straddle test (Rules 1-2).
      # The compact letters on a figure must answer the same question
      # the figure asks. An effect / interaction plot shows the means
      # of the factors in `display_factors` and AVERAGES over every
      # other categorical predictor. Letters are therefore only honest
      # when BOTH:
      #   (1) they are computed on exactly that displayed grid, so each
      #       letter and its plotted point refer to the same marginal
      #       mean (this also removes the first-match-wins ambiguity
      #       that arises when letters are copied from the full crossed
      #       cell-means table by a partial key), AND
      #   (2) the averaging does not cross a significant interaction,
      #       i.e. no significant interaction term has one factor inside
      #       `display_factors` and another outside it. If it does, the
      #       marginal mean collapses a pattern that genuinely changes
      #       with a hidden factor, so grouped letters on the collapsed
      #       view can mislead.
      # When (2) fails the means are still drawn (they remain
      # informative as a visual) but the letters are withheld and a
      # visible banner explains why and points to the full cell-means
      # post hoc table. This disclosed "degraded mode" is preferred
      # over printing letters that would quietly mislead. Returns a
      # list(safe, letters_df, banner): `letters_df` has the
      # display-factor column(s) plus a `Letters` column when letters
      # are shown, otherwise NULL; `banner` is the explanatory note
      # (or NULL).
      lmer_plot_letters <- function(display_factors) {
        D <- display_factors

        # (2) Does any significant interaction straddle D / not-D?
        # Each sig_int_terms entry can carry wrapper functions
        # (e.g. "factor(vs):am"); normalise components to bare
        # variable names so the intersect/setdiff agree with D
        # (which is always bare; see callers).
        crossing <- character(0)
        for (it in sig_int_terms) {
          parts <- trimws(strsplit(it, ":", fixed = TRUE)[[1]])
          parts <- vapply(parts, function(p)
            tryCatch(all.vars(parse(text = p)[[1]])[1],
                     error = function(e) p), character(1))
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

        # Overall model not significant: the table already shows "ns"
        # and no pairwise letters should be claimed. Draw points
        # without letters. `any_sig` is f_lmer's equivalent of
        # f_aov()'s `overall_p_value < alpha`.
        if (!isTRUE(any_sig))
          return(list(safe = TRUE, letters_df = NULL, banner = NULL))

        # (1) Recompute emmeans on EXACTLY the displayed grid, then
        # derive the letters from that grid so points and letters
        # match one-to-one.
        emm_df_method <- switch(ddf,
                                "Satterthwaite" = "satterthwaite",
                                "Kenward-Roger" = "kenward-roger",
                                "lme4"          = "asymptotic")
        letters_df <- tryCatch({
          emm_D <- emmeans::emmeans(fit, specs = D, level = 1 - alpha,
                                    lmer.df = emm_df_method)
          cld_D <- cld_emmeans(emm_D, alpha = alpha, Letters = letters,
                               adjust = adjust, decreasing = TRUE)
          cld_D <- as.data.frame(cld_D)
          if (!all(c(D, ".group") %in% names(cld_D)))
            stop("recomputed CLD is missing expected columns")
          ld <- cld_D[, c(D, ".group"), drop = FALSE]
          names(ld)[names(ld) == ".group"] <- "Letters"
          # Key columns as character so downstream joins are type-stable.
          for (f in D) ld[[f]] <- as.character(ld[[f]])
          ld$Letters <- trimws(ld$Letters)
          ld
        }, error = function(e) {
          warning("f_lmer: could not compute per-plot letters for '",
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

      # Assemble the single-piece figure caption in reading order (Rule 3):
      #   1. data / estimates / CI            (eff_note_est, always)
      #   2. the letter information together   (eff_note_let + letter_extra),
      #      OR the "Letters omitted" notice   (banner), OR nothing when the
      #      figure shows no letters at all (e.g. an overall non-significant
      #      model). letter_extra carries the cross-panel reading note,
      #      which is itself about letters, so it sits with the other
      #      letter text.
      #   3. the interaction / line note "rest" (int_note: not-parallel,
      #      panels, dotted line), kept together as one block.
      # Exactly one short lead label is shown in bold: "Letters omitted:"
      # when the letters were withheld, otherwise the interaction lead
      # ("Significant interaction:" / "No significant interaction:" /
      # "Significant <x> \u00d7 <y> interaction:" for the slope plot).
      # The whole caption is one italic piece. `banner` is NULL when
      # letters are shown; `int_note` is NULL for single-factor means
      # plots; `has_letters` is FALSE when the figure carries no letters
      # and no omission notice (so block 2 is dropped entirely).
      # `est_override` is used by the slope plot, which has no marginal
      # means and no letters and needs a slope-specific estimate note.
      make_fig_caption <- function(banner = NULL, int_note = NULL,
                                   letter_extra = NULL, has_letters = TRUE,
                                   est_override = NULL) {
        est <- trimws(if (!is.null(est_override)) est_override
                      else eff_note_est)
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
          # Bold the interaction lead only when it is the salient label,
          # i.e. when no "Letters omitted:" notice already carries the
          # bold. The regex covers the means-plot leads
          # ("Significant interaction:" / "No significant interaction:")
          # and the slope-plot lead
          # ("Significant <num> \u00d7 <fac> interaction:").
          if (is.null(banner))
            intp <- sub(
              "^(Significant[^:]*interaction:|No significant interaction:)",
              "**\\1**", intp)
        }
        parts <- c(est, let, intp)
        paste(parts[nzchar(parts)], collapse = " ")
      }

      # ----------------------------------------------------------------
      # Effect-plot helpers (single categorical term: estimated marginal
      # means +/- 95% CI with jittered raw data and CLD letters). The LMM
      # is fitted on the raw response (no Box-Cox / bestNormalize step in
      # f_lmer), so estimates are on the response scale already; no
      # back-transformation is required. Mirrors the single-predictor
      # branch of f_aov()'s make_means_plot().
      make_lmer_means_plot <- function(emm_obj, cld_table, term_label,
                                       var_name) {
        emm_d <- tryCatch(as.data.frame(emm_obj), error = function(e) NULL)
        if (is.null(emm_d) || !all(c("emmean", "lower.CL", "upper.CL") %in%
                                   names(emm_d))) return(NULL)
        if (!var_name %in% names(emm_d)) return(NULL)

        plot_df <- data.frame(
          x_grp  = as.character(emm_d[[var_name]]),
          centre = emm_d[["emmean"]],
          lower  = emm_d[["lower.CL"]],
          upper  = emm_d[["upper.CL"]],
          stringsAsFactors = FALSE
        )

        # CLD letters computed on EXACTLY this single-factor grid (see
        # lmer_plot_letters): they match the plotted marginal means
        # one-to-one and are withheld (with a banner) when averaging
        # over another factor would cross a significant interaction.
        # The legacy `cld_table` argument is kept for compatibility but
        # is no longer consulted: the per-plot recomputation supplies
        # the right letters and the right banner together.
        letter_banner <- NULL
        plot_df$Letters <- NA_character_
        lres <- lmer_plot_letters(var_name)
        letter_banner <- lres$banner
        if (!is.null(lres$letters_df) &&
            var_name %in% names(lres$letters_df)) {
          lut <- stats::setNames(lres$letters_df$Letters,
                                 lres$letters_df[[var_name]])
          plot_df$Letters <- lut[as.character(plot_df$x_grp)]
        }
        plot_df$Letters <- trimws(plot_df$Letters)
        plot_df$Letters[plot_df$Letters %in%
                          c("ns", "\u2014", "-", "NA")] <- NA_character_

        # Raw data overlay on the response scale, read from the model frame.
        raw_df <- NULL
        if (!is.null(mf) && !is.na(resp_col) &&
            var_name %in% names(mf) && resp_col %in% names(mf)) {
          raw_df <- data.frame(
            x_grp = as.character(mf[[var_name]]),
            y_val = mf[[resp_col]],
            stringsAsFactors = FALSE
          )
          raw_df <- raw_df[stats::complete.cases(raw_df), ]
        }

        # Preserve factor ordering if present in the model frame.
        if (!is.null(mf) && var_name %in% names(mf) &&
            is.factor(mf[[var_name]])) {
          lev <- levels(mf[[var_name]])
          plot_df$x_grp <- factor(plot_df$x_grp, levels = lev)
          if (!is.null(raw_df)) raw_df$x_grp <- factor(raw_df$x_grp,
                                                       levels = lev)
        }

        y_vals <- c(plot_df$upper, plot_df$lower,
                    if (!is.null(raw_df)) raw_df$y_val)
        y_top  <- max(y_vals, na.rm = TRUE)
        y_bot  <- min(y_vals, na.rm = TRUE)
        plot_df$y_letter <- y_top + 0.06 * (y_top - y_bot)

        y_label_doc <- if (!is.na(resp_col)) resp_col else "response"

        # One colour per level from the shared publication palette, matching
        # f_aov()'s make_aov_means_plot(). The legend is suppressed (the x
        # axis already names the levels).
        n_lvl    <- length(unique(plot_df$x_grp))
        lvl_cols <- f_pub_palette(n_lvl)

        p <- ggplot2::ggplot(
          plot_df,
          ggplot2::aes(x = .data[["x_grp"]], y = .data[["centre"]],
                       colour = .data[["x_grp"]])
        )
        if (!is.null(raw_df) && nrow(raw_df) > 0L) {
          p <- p + ggplot2::geom_jitter(
            data = raw_df,
            ggplot2::aes(x = .data[["x_grp"]], y = .data[["y_val"]]),
            inherit.aes = FALSE,
            width = 0.15, height = 0,
            shape = 1, color = "grey30", alpha = 0.5
          )
        }
        p <- p +
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
            width = 0.12, linewidth = 0.8
          ) +
          ggplot2::geom_point(size = 3) +
          ggplot2::geom_text(
            ggplot2::aes(y = .data[["y_letter"]], label = .data[["Letters"]]),
            vjust = 0, show.legend = FALSE, colour = "black", na.rm = TRUE
          ) +
          ggplot2::scale_colour_manual(values = lvl_cols, guide = "none") +
          ggplot2::labs(
            title = paste0("Estimated means of ", response_name,
                           " by ", var_name),
            x = var_name, y = y_label_doc) +
          f_theme_pub(base_size = 14)
        attr(p, "letter_banner") <- letter_banner
        attr(p, "has_letters")   <- any(!is.na(plot_df$Letters))
        p
      }

      # Interaction effect plot. Generalised to 2-, 3- and 4-way categorical
      # interactions, matching f_aov()'s make_aov_interaction_plot(): the 1st
      # factor is the x-axis, the 2nd the colour trace, and any 3rd/4th become
      # facet panels. `factors` is a character vector of bare variable names.
      # emmip() supplies the estimated means; cell-means CLD letters are mapped
      # on by the full factor combination. Returns a ggplot (caption carried in
      # attr "int_caption") or NULL.
      make_lmer_interaction_plot <- function(factors, is_sig) {
        x_var      <- factors[1]
        trace_var  <- factors[2]
        facet_vars <- if (length(factors) > 2L) factors[-(1:2)] else character(0)

        # emmip formula: trace ~ x  (+ "| f1 * f2" for facet factors).
        rhs <- x_var
        if (length(facet_vars) > 0L)
          rhs <- paste0(rhs, " | ", paste(facet_vars, collapse = " * "))
        emmip_form <- stats::as.formula(paste(trace_var, "~", rhs))

        ip <- tryCatch(
          emmeans::emmip(fit, emmip_form, CIs = TRUE, plotit = FALSE),
          error = function(e) NULL
        )
        if (is.null(ip)) return(NULL)

        # emmip() returns columns: <trace_var>, <x_var>, [facet vars,] yvar,
        # SE, LCL, UCL.
        ip_df <- data.frame(
          x_grp = as.character(ip[[x_var]]),
          trace = as.character(ip[[trace_var]]),
          yvar  = ip[["yvar"]],
          lower = ip[["LCL"]],
          upper = ip[["UCL"]],
          stringsAsFactors = FALSE
        )
        # Keep the x and trace factors under their REAL names too, so the
        # CLD-letter key join below can find them, and carry facet columns
        # through (as factors, preserving model order).
        ip_df[[x_var]]     <- as.character(ip[[x_var]])
        ip_df[[trace_var]] <- as.character(ip[[trace_var]])
        for (fv in facet_vars) {
          fac_levels <- if (!is.null(mf) && fv %in% names(mf) &&
                            is.factor(mf[[fv]]))
            levels(mf[[fv]]) else unique(as.character(ip[[fv]]))
          ip_df[[fv]] <- factor(as.character(ip[[fv]]), levels = fac_levels)
        }

        # Preserve factor ordering from the model frame.
        x_levels <- if (!is.null(mf) && x_var %in% names(mf) &&
                        is.factor(mf[[x_var]]))
          levels(mf[[x_var]]) else unique(ip_df$x_grp)
        tr_levels <- if (!is.null(mf) && trace_var %in% names(mf) &&
                         is.factor(mf[[trace_var]]))
          levels(mf[[trace_var]]) else unique(ip_df$trace)
        ip_df$x_grp <- factor(ip_df$x_grp, levels = x_levels)
        ip_df$trace <- factor(ip_df$trace, levels = tr_levels)

        # CLD letters computed on EXACTLY this displayed grid (see
        # lmer_plot_letters): they match the plotted cells one-to-one
        # and are withheld (with a banner) when the view averages over
        # a factor that interacts significantly with what is shown.
        # Replaces the previous always-on cell-means CLD computation,
        # which would print letters even when averaging over an
        # interacting factor that was not part of `all_factors`.
        all_factors <- c(x_var, trace_var, facet_vars)
        ip_df$Letters <- NA_character_
        int_letter_banner <- NULL
        lres <- lmer_plot_letters(all_factors)
        int_letter_banner <- lres$banner
        if (!is.null(lres$letters_df) &&
            all(all_factors %in% names(lres$letters_df))) {
          key_tab <- do.call(paste, c(
            lapply(all_factors, function(f)
              as.character(lres$letters_df[[f]])), list(sep = "\r")))
          lut <- stats::setNames(lres$letters_df$Letters, key_tab)
          key_ip <- do.call(paste, c(
            lapply(all_factors, function(f) as.character(ip_df[[f]])),
            list(sep = "\r")))
          ip_df$Letters <- lut[key_ip]
        }
        ip_df$Letters <- trimws(ip_df$Letters)
        ip_df$Letters[ip_df$Letters %in%
                        c("ns", "\u2014", "-", "NA")] <- NA_character_
        raw_df <- NULL
        if (!is.null(mf) && !is.na(resp_col) &&
            all(all_factors %in% names(mf)) && resp_col %in% names(mf)) {
          raw_df <- data.frame(
            x_grp = factor(as.character(mf[[x_var]]), levels = x_levels),
            trace = factor(as.character(mf[[trace_var]]), levels = tr_levels),
            y_val = mf[[resp_col]],
            stringsAsFactors = FALSE
          )
          # Carry facet columns onto the raw-data overlay so faceting splits
          # the points the same way as the estimates.
          for (fv in facet_vars) {
            fac_levels <- if (is.factor(mf[[fv]]))
              levels(mf[[fv]]) else unique(as.character(mf[[fv]]))
            raw_df[[fv]] <- factor(as.character(mf[[fv]]), levels = fac_levels)
          }
          raw_df <- raw_df[stats::complete.cases(raw_df), ]
        }

        y_label_doc <- if (!is.na(resp_col)) resp_col else "response"
        # The interaction (line) note. Order matches f_aov() so the
        # caption assembler can rely on it: lead, facet pointer, dotted
        # line note. The cross-panel reading note about LETTERS is
        # returned separately via attr "cross_facet_note" so the caption
        # assembler can place it with the other letter text (Rule 3) and
        # drop it when letters are withheld.
        int_caption <- if (isTRUE(is_sig))
          paste0("Significant interaction: the ", trace_var,
                 " lines are not parallel.")
        else
          paste0("No significant interaction: the ", trace_var,
                 " lines should be roughly parallel.")
        if (length(facet_vars) > 0L)
          int_caption <- paste0(
            int_caption, " Panels split by ",
            paste(facet_vars, collapse = " and "), ".")
        cross_facet_note <- if (length(facet_vars) > 0L &&
                                is.null(int_letter_banner))
          paste0("Letters can be compared across all panels: they group ",
                 "every cell shown, not only the cells within one panel.")
        else NULL
        int_caption <- paste0(
          int_caption,
          " The dotted line is merely a visual aid and has no real-life ",
          "meaning, as there is no data space between nominal categories.")

        # Letter labels sit a little above each cell's upper CI.
        y_all_int <- c(ip_df$upper, ip_df$lower,
                       if (!is.null(raw_df)) raw_df$y_val)
        y_rng_int <- max(y_all_int, na.rm = TRUE) -
          min(y_all_int, na.rm = TRUE)
        ip_df$y_letter <- ip_df$upper + 0.05 * y_rng_int

        # Dodge so traces and their CIs do not overlap on shared x positions.
        dodge <- ggplot2::position_dodge(width = 0.3)

        p <- ggplot2::ggplot(
          ip_df,
          ggplot2::aes(x = .data[["x_grp"]], y = .data[["yvar"]],
                       group = .data[["trace"]], colour = .data[["trace"]])
        )
        if (!is.null(raw_df) && nrow(raw_df) > 0L) {
          p <- p + ggplot2::geom_jitter(
            data = raw_df,
            ggplot2::aes(x = .data[["x_grp"]], y = .data[["y_val"]],
                         colour = .data[["trace"]]),
            inherit.aes = FALSE,
            position = ggplot2::position_jitterdodge(
              jitter.width = 0.12, dodge.width = 0.3),
            shape = 1, alpha = 0.35, show.legend = FALSE
          )
        }
        p <- p +
          ggplot2::geom_line(position = dodge, linewidth = 0.7,
                             linetype = "dotted") +
          ggplot2::geom_errorbar(
            ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
            width = 0.12, linewidth = 0.7, position = dodge
          ) +
          ggplot2::geom_point(position = dodge, size = 3) +
          ggplot2::geom_text(
            ggplot2::aes(y = .data[["y_letter"]],
                         label = .data[["Letters"]]),
            position = dodge, vjust = 0, colour = "black",
            show.legend = FALSE, na.rm = TRUE
          ) +
          ggplot2::labs(
            title    = paste0("Estimated means of ", response_name,
                              " by ", paste(all_factors, collapse = ", ")),
            x        = x_var,
            y        = y_label_doc,
            colour   = trace_var
          ) +
          ggplot2::scale_colour_manual(
            values = f_pub_palette(length(tr_levels))) +
          f_theme_pub(base_size = 14)

        # Facets: one facet factor -> facet_wrap; two -> facet_grid.
        if (length(facet_vars) == 1L) {
          p <- p + ggplot2::facet_wrap(
            ggplot2::vars(.data[[facet_vars[1]]]),
            labeller = ggplot2::label_both)
        } else if (length(facet_vars) >= 2L) {
          p <- p + ggplot2::facet_grid(
            stats::as.formula(paste(facet_vars[1], "~", facet_vars[2])),
            labeller = ggplot2::label_both)
        }

        # Carry the caption as an attribute so the caller can print it as
        # markdown text below the figure, matching f_aov(). This keeps the
        # stored ggplot object clean (no baked-in caption) and themable.
        attr(p, "int_caption")      <- int_caption
        attr(p, "letter_banner")    <- int_letter_banner
        attr(p, "cross_facet_note") <- cross_facet_note
        attr(p, "has_letters")      <- any(!is.na(ip_df$Letters))
        p
      }

      # Slope (covariate x factor) interaction plot. This is the standard way
      # to show a numeric x categorical interaction in a linear (mixed) model:
      # a scatter of the raw data coloured by the factor, with one model-fitted
      # regression line per factor level and a confidence ribbon. A significant
      # numeric:factor interaction means those slopes differ across the factor
      # levels - the lines are not parallel - which a means plot (factor held
      # at a covariate mean) cannot show. f_aov() does not draw this; f_lmer
      # adds it because the covariate slope is a first-class LMM effect.
      #
      # The fitted lines are taken from the MODEL via emmeans::emmip() over a
      # grid of covariate values, not from an independent per-group lm(). This
      # matters: emmip() respects the mixed-model fit and holds any other
      # predictors at their reference values, so the lines are the model's
      # predicted means, consistent with the rest of the report. Returns a
      # ggplot (caption carried in attr "int_caption") or NULL.
      #
      #   num_var : the numeric covariate (x axis)
      #   fac_var : the categorical factor (colour / one line per level)
      make_lmer_slope_plot <- function(num_var, fac_var) {
        if (is.null(mf) || is.na(resp_col) ||
            !all(c(num_var, fac_var, resp_col) %in% names(mf)))
          return(NULL)

        num_vals <- mf[[num_var]]
        if (!is.numeric(num_vals)) return(NULL)
        rng <- range(num_vals, na.rm = TRUE)
        if (!all(is.finite(rng)) || rng[1] == rng[2]) return(NULL)

        # Factor levels in model order.
        fac_levels <- if (is.factor(mf[[fac_var]]))
          levels(mf[[fac_var]]) else sort(unique(as.character(mf[[fac_var]])))

        # Model-predicted lines over a dense covariate grid, per factor level.
        grid_n   <- 100L
        at_list  <- stats::setNames(
          list(seq(rng[1], rng[2], length.out = grid_n)), num_var)
        line_df <- tryCatch({
          ip <- emmeans::emmip(
            fit,
            stats::as.formula(paste(fac_var, "~", num_var)),
            at = at_list, CIs = TRUE, plotit = FALSE)
          data.frame(
            xval  = ip[[num_var]],
            grp   = factor(as.character(ip[[fac_var]]), levels = fac_levels),
            yval  = ip[["yvar"]],
            lower = ip[["LCL"]],
            upper = ip[["UCL"]],
            stringsAsFactors = FALSE
          )
        }, error = function(e) NULL)
        if (is.null(line_df)) return(NULL)

        raw_df <- data.frame(
          xval = num_vals,
          grp  = factor(as.character(mf[[fac_var]]), levels = fac_levels),
          yval = mf[[resp_col]],
          stringsAsFactors = FALSE
        )
        raw_df <- raw_df[stats::complete.cases(raw_df), ]

        y_label_doc <- if (!is.na(resp_col)) resp_col else "response"
        grp_cols    <- f_pub_palette(length(fac_levels))

        int_caption <- paste0(
          "Significant ", num_var, " \u00d7 ", fac_var,
          " interaction: the slope of ", num_var, " differs across ",
          fac_var, " levels (the fitted lines are not parallel).")

        p <- ggplot2::ggplot(
          line_df,
          ggplot2::aes(x = .data[["xval"]], y = .data[["yval"]],
                       colour = .data[["grp"]], fill = .data[["grp"]]))
        if (nrow(raw_df) > 0L) {
          p <- p + ggplot2::geom_point(
            data = raw_df,
            ggplot2::aes(x = .data[["xval"]], y = .data[["yval"]],
                         colour = .data[["grp"]]),
            inherit.aes = FALSE, shape = 1, alpha = 0.5)
        }
        p <- p +
          ggplot2::geom_ribbon(
            ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
            colour = NA, alpha = 0.15, show.legend = FALSE) +
          ggplot2::geom_line(linewidth = 0.9) +
          ggplot2::scale_colour_manual(values = grp_cols) +
          ggplot2::scale_fill_manual(values = grp_cols, guide = "none") +
          ggplot2::labs(
            title  = paste0("Estimated slopes of ", response_name, ": ",
                            num_var, " by ", fac_var),
            x      = num_var,
            y      = y_label_doc,
            colour = fac_var) +
          f_theme_pub(base_size = 14)

        attr(p, "int_caption") <- int_caption
        p
      }

      # Contrast forest plots are drawn by the shared package helper
      # make_contrast_forest() (helper_contrast_forest.R), which is also used
      # by f_aov(), f_glm() and f_kruskal_test() so the figure cannot drift
      # between functions. It takes alpha and adjust as explicit arguments
      # (passed through from this call), builds the dot-and-whisker figure via
      # build_forest_plot(), and attaches the explanatory caption as
      # attr "int_caption".
      #
      # The shared per-figure caption (estimates / CI + letter info +
      # interaction note) is assembled by make_fig_caption() (see above);
      # the underlying eff_note_est and eff_note_let pieces are defined
      # next to lmer_plot_letters() and are reused by every plot below.

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
          as.data.frame(cld_emmeans(emm,
                                    alpha = alpha,
                                    Letters = letters,
                                    adjust = adjust,
                                    decreasing = TRUE)),
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

        # Which significant interactions does this main effect belong to?
        # If any, its marginal means below are an average over the other
        # factor and can be misleading on their own.
        trm_sig_ints <- interactions_involving(trm)
        in_sig_int   <- length(trm_sig_ints) > 0L

        cat("\n### ", trm,
            if (term_ns) "  (not significant)"
            else if (in_sig_int) "  (involved in a significant interaction)"
            else "",
            "\n\n", sep = "")

        if (term_ns) {
          cat("Term `", trm, "` was not significant (p = ",
              fmt_p(term_p), "). Letters collapsed to `ns`.  \n\n",
              sep = "")
        }

        # Inline interaction caveat, placed directly above the table so the
        # reader meets it before reading the letters. Mirrors the
        # interaction note f_aov() prints above its post hoc table.
        #
        # The pointer at the end must match what is actually produced for
        # each interaction this term belongs to: an all-categorical
        # interaction gets a cell-means table (and, when effect_plot = TRUE,
        # a plot); an interaction that includes a numeric covariate gets
        # neither - only a skip note - so we must not send the reader to a
        # table/plot that does not exist. We split trm_sig_ints accordingly.
        if (in_sig_int) {
          # Is a given interaction term made up solely of categorical factors?
          int_is_cat <- function(it) {
            comps <- strsplit(it, ":", fixed = TRUE)[[1]]
            comps <- vapply(comps, function(p)
              tryCatch(all.vars(parse(text = p)[[1]])[1],
                       error = function(e) p), character(1))
            all(comps %in% cat_predictors)
          }
          cat_ints <- trm_sig_ints[vapply(trm_sig_ints, int_is_cat, logical(1))]
          num_ints <- setdiff(trm_sig_ints, cat_ints)

          # Build the closing pointer from what genuinely exists.
          if (length(cat_ints) > 0L) {
            pointer <- paste0(
              "Read these letters alongside the interaction cell-means ",
              "table", if (isTRUE(effect_plot)) " and plot(s)" else "",
              " for ", paste0("`", cat_ints, "`", collapse = ", "),
              " below, not on their own.")
          } else {
            pointer <- ""
          }
          if (length(num_ints) > 0L) {
            # Among the numeric-involving interactions this term belongs to,
            # a two-way numeric x categorical one now DOES get a slope plot
            # (fitted lines per level). Separate those from the rest so the
            # pointer matches what is actually drawn.
            int_is_slope <- function(it) {
              comps <- strsplit(it, ":", fixed = TRUE)[[1]]
              comps <- vapply(comps, function(p)
                tryCatch(all.vars(parse(text = p)[[1]])[1],
                         error = function(e) p), character(1))
              length(comps) == 2L &&
                sum(comps %in% numeric_predictors) == 1L &&
                sum(comps %in% cat_predictors) == 1L
            }
            slope_ints <- num_ints[vapply(num_ints, int_is_slope, logical(1))]
            other_ints <- setdiff(num_ints, slope_ints)

            if (length(slope_ints) > 0L) {
              pointer <- paste0(
                pointer, if (nzchar(pointer)) "  " else "",
                "For ", paste0("`", slope_ints, "`", collapse = ", "),
                " see the slope plot(s) below (model-fitted lines per `",
                trm, "` level): the covariate slope differs across `", trm,
                "`, so a single averaged effect is misleading.")
            }
            if (length(other_ints) > 0L) {
              pointer <- paste0(
                pointer, if (nzchar(pointer)) "  " else "",
                "For ", paste0("`", other_ints, "`", collapse = ", "),
                " no standard plot is drawn; inspect the slope(s) in the ",
                "fixed-effects coefficient table or via ",
                "`emmeans::emtrends()`.")
            }
          }

          cat("**[!] Interpret with caution:** `", trm, "` takes part in the ",
              "significant interaction(s) ",
              paste0("`", trm_sig_ints, "`", collapse = ", "), ". The ",
              "estimated marginal means below average over the other ",
              "term(s) in that interaction, so they can hide or even ",
              "reverse the real pattern - the effect of `", trm, "` depends ",
              "on the level/value of the interacting term(s). ",
              pointer, "  \n\n", sep = "")
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

        ## ---- Contrast forest plot for this main-effect term -------------
        ## Opt-in (contrast_plots = TRUE). One row per pairwise difference
        ## with its adjusted CI and a zero reference line. CIs come from
        ## confint(pairs(emm)) with the SAME adjust as the p-values, so the
        ## figure and the pairwise table agree. Stored separately from the
        ## interaction contrast forests, as contrast_plot_<term>.
        if (isTRUE(contrast_plots)) {
          cf_tbl <- tryCatch(
            as.data.frame(confint(pairs(emm, adjust = adjust),
                                   level = 1 - alpha)),
            error = function(e) NULL)
          p_cf <- if (!is.null(cf_tbl))
            tryCatch(make_contrast_forest(
              cf_tbl,
              paste0("Pairwise contrasts of ", response_name, " by ", trm),
              alpha = alpha, adjust = adjust),
              error = function(e) NULL) else NULL
          if (is.null(p_cf)) {
            cat("\n*Contrast forest plot for `", trm, "` could not be ",
                "built (adjusted contrast CIs unavailable); see the ",
                "pairwise table above.*  \n\n", sep = "")
          } else {
            key <- paste0("contrast_plot_", emm_spec)
            output_list[[response_name]][[key]] <- p_cf
            tmp_cf <- tempfile(fileext = ".png")
            n_rows <- nrow(p_cf$data)
            suppressMessages(
              ggplot2::ggsave(filename = tmp_cf, plot = p_cf,
                              width = 7,
                              height = max(3, 0.4 * n_rows + 1.5),
                              units = "in", dpi = 200))
            cat("\n**Contrast forest plot: ", trm, "**  \n", sep = "")
            cat(paste0("![](", tmp_cf, ")"), "   \n  \n")
            cf_cap <- attr(p_cf, "int_caption")
            if (!is.null(cf_cap))
              cat(paste0("*", gsub("\n", "  \n", cf_cap), "*", "   \n  \n"))
          }
        }

        ## ---- Effect plot for this categorical main-effect term ----------
        ## One estimated-means plot per term, placed directly after the
        ## term's tables (as requested). emm_spec is the bare variable name
        ## resolved earlier (e.g. "vs" from "factor(vs)").
        if (isTRUE(effect_plot) && length(emm_spec) == 1L) {
          p_eff <- tryCatch(
            make_lmer_means_plot(emm, cld_tbl, trm, emm_spec),
            error = function(e) NULL
          )
          if (!is.null(p_eff)) {
            output_list[[response_name]][[paste0("effect_plot_", emm_spec)]] <- p_eff
            tmp_eff <- tempfile(fileext = ".png")
            suppressMessages(
              ggplot2::ggsave(filename = tmp_eff, plot = p_eff,
                              width = 6, height = 5, units = "in", dpi = 200)
            )
            cat(paste0("\n## Estimated Means Plot of: ", response_name,
                       "  (", emm_spec, ")  \n"))
            cat(paste0("![](", tmp_eff, ")"), "   \n  \n")
            # Single italic caption: estimates/CI, then the letter note
            # (or the "Letters omitted" notice). Means plots carry no
            # interaction sentence, so int_note is omitted.
            eff_bann <- attr(p_eff, "letter_banner")
            cap <- make_fig_caption(
              banner      = eff_bann,
              int_note    = NULL,
              has_letters = isTRUE(attr(p_eff, "has_letters")))
            cat(paste0("*", gsub("\n", "  \n", cap), "*", "   \n  \n"))
          }
        }
      }

      # Note on numeric covariates in the fixed-effects table.
      if (length(numeric_in_anova) > 0L) {
        cat("\n**Note on numeric covariate(s):** ",
            paste(numeric_in_anova, collapse = ", "),
            ". Their slopes are reported in the coefficient table ",
            "above; no pairwise post hoc is performed.  \n\n", sep = "")
      }

      # Note about interaction term(s) present but not significant: suggest
      # refitting without the interaction term to gain power for the main
      # effects. Mutually exclusive with the significant-interaction block.
      if (length(ns_int_terms) > 0) {
        cat("\n**[!] Interaction term(s) not significant:** ",
            paste(ns_int_terms, collapse = ", "),
            " (p \u2265 ", alpha, "). ",
            "Consider refitting the model without the interaction term if the ",
            "research question and research setup allow this; the main effects ",
            "are then estimated with more power.\n\n", sep = "")
      }

      # Note about significant interactions. Reuses sig_int_terms, computed
      # once before the post hoc loop (see above), so the per-table inline
      # warnings and this summary block cannot drift apart.
      int_terms <- sig_int_terms
      if (length(int_terms) > 0) {
        cat("\n**[!] Significant interaction(s) detected:** ",
            paste(int_terms, collapse = ", "),
            ". Main-effect post hoc tests above should be interpreted with ",
            "care - the effect of one factor depends on the level of another. ",
            "Use `emmeans(model, ~ factor1 | factor2)` manually for ",
            "simple-effects contrasts.\n\n", sep = "")

        ## ---- Interaction post hoc table (cell means) --------------------
        ## For each significant interaction made up solely of categorical
        ## factors, compute estimated CELL means (every combination of the
        ## interacting factors) and compare all cells simultaneously, with
        ## a compact letter display and pairwise contrasts. This mirrors the
        ## cell-means post hoc that f_aov() emits for a significant
        ## interaction. The per-main-effect tables above are marginal means;
        ## this table is the joint pattern the interaction term tests, and is
        ## the one that should be reported when an interaction is significant.
        ## Interactions of any order are tabulated here (the cell-means
        ## emmeans grid generalises to any number of factors); the line
        ## plots below cover 2-, 3- and 4-way interactions (3-/4-way via
        ## facet panels) and skip higher orders with a note.
        for (it in int_terms) {
          # Resolve each interaction component to its bare variable name
          # (factor(a):b -> "a", "b"), matching emmeans' reference grid.
          int_parts <- strsplit(it, ":", fixed = TRUE)[[1]]
          int_parts <- vapply(int_parts, function(p)
            tryCatch(all.vars(parse(text = p)[[1]])[1],
                     error = function(e) p), character(1))
          int_parts <- unique(int_parts[!is.na(int_parts)])

          # Cell-means post hoc only makes sense between categorical
          # factors. If any component is a numeric covariate, skip the
          # table with a visible note (do not silently omit it).
          if (!all(int_parts %in% cat_predictors)) {
            cat("\n*Cell-means post hoc table for `", it, "` skipped ",
                "(it involves a numeric covariate; cell means are only ",
                "computed between categorical factors).*  \n\n", sep = "")
            next
          }

          emm_int <- tryCatch(
            emmeans::emmeans(
              fit,
              specs = stats::as.formula(
                paste0("~", paste(int_parts, collapse = "*"))),
              lmer.df = switch(ddf,
                               "Satterthwaite" = "satterthwaite",
                               "Kenward-Roger" = "kenward-roger",
                               "lme4"          = "asymptotic")),
            error = function(e) NULL
          )
          if (is.null(emm_int)) {
            cat("\n### Interaction: ", it,
                "\n\nemmeans failed to compute cell means for this ",
                "interaction.\n\n", sep = "")
            next
          }

          int_pairs_tbl <- tryCatch(
            as.data.frame(pairs(emm_int, adjust = adjust)),
            error = function(e) NULL
          )
          int_cld_tbl <- tryCatch(
            as.data.frame(cld_emmeans(emm_int,
                                      alpha = alpha,
                                      Letters = letters,
                                      adjust = adjust,
                                      decreasing = TRUE)),
            error = function(e) NULL
          )
          if (!is.null(int_cld_tbl) && ".group" %in% names(int_cld_tbl)) {
            names(int_cld_tbl)[names(int_cld_tbl) == ".group"] <- "Letter"
          }
          if (!is.null(int_cld_tbl)) rownames(int_cld_tbl) <- NULL
          if (!is.null(int_pairs_tbl)) rownames(int_pairs_tbl) <- NULL
          emm_int_tbl <- as.data.frame(emm_int)
          rownames(emm_int_tbl) <- NULL

          cat("\n### Interaction cell means: ", it, "\n\n", sep = "")
          cat("Estimated **cell means** for every combination of ",
              paste(int_parts, collapse = " \u00d7 "),
              ". Because the interaction is significant, all cells are ",
              "compared simultaneously - interpret the joint pattern, not ",
              "the single-factor marginal means above. Letters here ",
              "compare all cells at once and are the reference grouping ",
              "for this response: interaction plots that average over an ",
              "interacting factor omit their own letters and refer back ",
              "to this table.  \n\n", sep = "")

          if (!is.null(int_cld_tbl)) {
            cat("**Estimated cell means with compact letter display.**  \n",
                sep = "")
            f_pander(f_conditional_round(int_cld_tbl, digits = 4))
          } else {
            cat("**Estimated cell means:**  \n\n")
            f_pander(f_conditional_round(emm_int_tbl, digits = 4))
          }

          n_cells <- if (!is.null(int_cld_tbl)) nrow(int_cld_tbl) else
            nrow(emm_int_tbl)
          n_int_pairs <- if (!is.null(int_pairs_tbl))
            nrow(int_pairs_tbl) else NA_integer_

          int_cld_text <- paste0(
            "Confidence level used: ", 1 - alpha, "  \n",
            "Significance level used: \u03b1 = ", alpha, "  \n",
            "P-value and CI adjustment: ", adjust,
            " method for ", n_cells, " estimates",
            if (!is.na(n_int_pairs)) paste0(" / ", n_int_pairs, " tests")
            else "",
            ".  \n\n",
            "*Note: Cells in the \"Letters\" column sharing the same letter ",
            "are **not** significantly different (\u03b1 = ", alpha,
            "). Cells with different letters are significantly different. ",
            "Sharing a letter indicates insufficient evidence to claim a ",
            "difference; it does not prove the cells are identical.*\n\n")
          cat(int_cld_text)

          if (!is.null(int_pairs_tbl)) {
            cat("\n**Pairwise contrasts between cells (", adjust,
                " adjusted):**  \n\n", sep = "")
            f_pander(f_conditional_round(int_pairs_tbl, digits = 4))
          }

          # Store under the interaction term name so it sits alongside the
          # main-effect post hoc results in out$<resp>$post_hoc[[it]].
          posthoc_results[[it]] <- list(
            emmeans = emm_int_tbl,
            pairs   = int_pairs_tbl,
            cld     = int_cld_tbl
          )

          ## ---- Contrast forest plot for this interaction ----------------
          ## Opt-in (contrast_plots = TRUE). One row per cell-vs-cell
          ## difference with its adjusted CI and a zero reference line. Kept
          ## separate from the main-effect contrast forests, stored as
          ## interaction_contrast_plot_<term>. Note the cell-contrast count
          ## grows fast (m cells give m(m-1)/2 rows); since this is opt-in
          ## and uncapped, that is the user's choice.
          if (isTRUE(contrast_plots)) {
            int_cf_tbl <- tryCatch(
              as.data.frame(confint(pairs(emm_int, adjust = adjust),
                                     level = 1 - alpha)),
              error = function(e) NULL)
            p_icf <- if (!is.null(int_cf_tbl))
              tryCatch(make_contrast_forest(
                int_cf_tbl,
                paste0("Cell-pairwise contrasts of ", response_name,
                       "  (", it, ")"),
                alpha = alpha, adjust = adjust),
                error = function(e) NULL) else NULL
            if (is.null(p_icf)) {
              cat("\n*Contrast forest plot for `", it, "` could not be ",
                  "built (adjusted contrast CIs unavailable); see the ",
                  "cell-contrast table above.*  \n\n", sep = "")
            } else {
              key <- paste0("interaction_contrast_plot_", gsub(":", "_", it))
              output_list[[response_name]][[key]] <- p_icf
              tmp_icf <- tempfile(fileext = ".png")
              n_rows <- nrow(p_icf$data)
              suppressMessages(
                ggplot2::ggsave(filename = tmp_icf, plot = p_icf,
                                width = 7.5,
                                height = max(3, 0.4 * n_rows + 1.5),
                                units = "in", dpi = 200))
              cat("\n**Contrast forest plot: ", it, "**  \n", sep = "")
              cat(paste0("![](", tmp_icf, ")"), "   \n  \n")
              icf_cap <- attr(p_icf, "int_caption")
              if (!is.null(icf_cap))
                cat(paste0("*", gsub("\n", "  \n", icf_cap), "*",
                           "   \n  \n"))
            }
          }
        }

        ## ---- Interaction effect plots --------------------------------
        ## For each significant categorical interaction draw plot(s), matching
        ## f_aov(): a 2-way uses x-axis + colour trace and is shown in both
        ## orientations; 3- and 4-way interactions add facet panels for the
        ## remaining factor(s), rotating which factor sits on the x-axis.
        ## Interactions involving a numeric covariate, or of order > 4 (which
        ## would need an illegible nested facet grid), are skipped with a
        ## visible note - the cell-means table above still reports every
        ## combination.
        MAX_PLOT_FACTORS <- 4L
        if (isTRUE(effect_plot)) {
          for (it in int_terms) {
            parts <- strsplit(it, ":", fixed = TRUE)[[1]]
            parts <- vapply(parts, function(p)
              tryCatch(all.vars(parse(text = p)[[1]])[1],
                       error = function(e) p), character(1))
            parts <- parts[!is.na(parts)]
            n_f      <- length(parts)
            n_num    <- sum(parts %in% numeric_predictors)
            n_cat    <- sum(parts %in% cat_predictors)
            all_cat  <- n_f >= 2L && n_cat == n_f

            # Two-way numeric x categorical: draw model-fitted regression
            # lines per factor level (the standard way to show that the
            # covariate slope differs across groups). This is the one place
            # f_lmer goes beyond f_aov, which never plots covariate slopes.
            if (n_f == 2L && n_num == 1L && n_cat == 1L) {
              num_var <- parts[parts %in% numeric_predictors][1]
              fac_var <- parts[parts %in% cat_predictors][1]
              cat(paste0("\n## Interaction Plots of: ", response_name,
                         "  (", it, ")  \n"))
              p_sl <- tryCatch(make_lmer_slope_plot(num_var, fac_var),
                               error = function(e) NULL)
              if (is.null(p_sl)) {
                cat("\n*Slope plot for `", it, "` could not be built ",
                    "(model-predicted lines unavailable). Inspect the ",
                    "covariate slope(s) in the fixed-effects coefficient ",
                    "table and the cell-means via `emmeans::emtrends()`.*",
                    "  \n\n", sep = "")
              } else {
                key <- paste0("interaction_plot_", gsub(":", "_", it), "_1")
                output_list[[response_name]][[key]] <- p_sl
                tmp_sl <- tempfile(fileext = ".png")
                suppressMessages(
                  ggplot2::ggsave(filename = tmp_sl, plot = p_sl,
                                  width = 7, height = 5.5, units = "in",
                                  dpi = 200)
                )
                cat(paste0("![](", tmp_sl, ")"), "   \n  \n")
                # Slope plot has no marginal means and no letters by
                # design (the x-axis is continuous): build the caption
                # via make_fig_caption() with a slope-specific
                # estimate-note and has_letters = FALSE so block 2
                # (letter info) is dropped entirely. The slope plot's
                # int_caption already begins with "Significant <num>
                # \u00d7 <fac> interaction:" which the caption assembler
                # bolds as the single lead.
                sl_cap <- attr(p_sl, "int_caption")
                slope_est_note <- paste0(
                  "Points are raw data; lines are model-predicted means of ",
                  response_name, " across ", num_var, " with ",
                  100 * (1 - alpha), "% CI bands, one per ", fac_var,
                  " level. Non-parallel lines indicate the interaction.")
                full_cap <- make_fig_caption(
                  banner       = NULL,
                  int_note     = sl_cap,
                  has_letters  = FALSE,
                  est_override = slope_est_note)
                cat(paste0("*", gsub("\n", "  \n", full_cap), "*",
                           "   \n  \n"))
              }
              next
            }

            if (!all_cat) {
              # Anything else involving a numeric covariate (numeric x
              # numeric, or a higher-order mix) has no standard 2-D plot.
              cat("\n*Interaction plot for `", it, "` skipped ",
                  "(no standard plot for this combination of numeric and ",
                  "categorical terms). The covariate slope(s) are in the ",
                  "fixed-effects coefficient table; for a numeric x numeric ",
                  "interaction inspect it via `emmeans::emtrends()` or plot ",
                  "a chosen slice.*  \n\n", sep = "")
              next
            }
            if (n_f > MAX_PLOT_FACTORS) {
              cat("\n*Interaction plot for `", it, "` skipped (a ", n_f,
                  "-way interaction would need ", n_f - 2L, " nested facet ",
                  "dimensions, which is not legible). The cell-means table ",
                  "above still reports every combination; inspect it or plot ",
                  "a chosen lower-order slice instead.*  \n\n", sep = "")
              next
            }
            cat(paste0("\n## Interaction Plots of: ", response_name,
                       "  (", it, ")  \n"))
            # Orientations: for a 2-way show both (x<->trace swap). For higher
            # order, rotate which factor sits on the x-axis (the next factor
            # becomes the trace/colour, the remainder become facet panels).
            orientations <- if (n_f == 2L)
              list(parts[c(1, 2)], parts[c(2, 1)])
            else
              lapply(seq_along(parts), function(i) c(parts[i], parts[-i]))
            for (pi in seq_along(orientations)) {
              ord  <- orientations[[pi]]
              p_ip <- tryCatch(
                make_lmer_interaction_plot(ord, is_sig = TRUE),
                error = function(e) NULL
              )
              if (!is.null(p_ip)) {
                # Store the ggplot OBJECT for later manual adjustment.
                key <- paste0("interaction_plot_",
                              gsub(":", "_", it), "_", pi)
                output_list[[response_name]][[key]] <- p_ip
                # Render the stored object to PNG for the report. Widen the
                # canvas when facet panels are present.
                n_facet <- max(0L, length(ord) - 2L)
                w_in <- if (n_facet >= 2L) 9 else if (n_facet == 1L) 8 else 7
                h_in <- if (n_facet >= 2L) 6.5 else 5.5
                tmp_ip <- tempfile(fileext = ".png")
                suppressMessages(
                  ggplot2::ggsave(filename = tmp_ip, plot = p_ip,
                                  width = w_in, height = h_in, units = "in",
                                  dpi = 200)
                )
                cat(paste0("![](", tmp_ip, ")"), "   \n  \n")
                # Single italic caption, ordered: estimates/CI, then the
                # letter note (or the "Letters omitted" notice, plus
                # the cross-panel note when the figure is faceted and
                # the letters are shown), then the interaction note.
                # Only one short lead label is bold (see make_fig_caption).
                ip_cap  <- attr(p_ip, "int_caption")
                ip_bann <- attr(p_ip, "letter_banner")
                full_cap <- make_fig_caption(
                  banner       = ip_bann,
                  int_note     = ip_cap,
                  letter_extra = attr(p_ip, "cross_facet_note"),
                  has_letters  = isTRUE(attr(p_ip, "has_letters")))
                cat(paste0("*", gsub("\n", "  \n", full_cap), "*",
                           "   \n  \n"))
              }
            }
          }
        }
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
        # The icc slot's third column name records whether a random slope
        # is present (the report uses "...random slope present"). Detect it
        # so the console labels match the report: an intercept-only
        # Var(group) / approximate ICC rather than a clean one.
        icc_slope <- grepl("random slope", names(sub$icc)[3], fixed = TRUE)
        vg_lbl  <- if (icc_slope) "Var(group, int)" else "Var(group)"
        icc_lbl <- if (icc_slope) "ICC (approx)"    else "ICC"
        fit_cols[[vg_lbl]]       <- as.numeric(sub$icc[[1]])
        fit_cols[["Var(resid)"]] <- as.numeric(sub$icc[[2]])
        fit_cols[[icc_lbl]]      <- as.numeric(sub$icc[[3]])
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

    # Per-grouping-factor ICC breakdown (nested / crossed designs). Only
    # present when the model has two or more intercept grouping factors;
    # see the report path where icc_by_group is built. Mirrors that output
    # so the console and the written report agree.
    if (!is.null(sub$icc_by_group)) {
      cat("\n--- ICC by grouping factor ---\n")
      icc_disp <- sub$icc_by_group
      if ("Approx" %in% names(icc_disp) && all(icc_disp$Approx == "")) {
        icc_disp$Approx <- NULL
      }
      f_pander(f_conditional_round(icc_disp, digits = 4))
      cat(paste(strwrap(paste(
        "-", "Each grouping factor's random-intercept variance as a share",
        "of the total variance (group variances + residual); the shares",
        "sum to 1, showing where the clustering sits."),
        width = 76, exdent = 2), collapse = "\n"), "\n", sep = "")
      if ("Approx" %in% names(sub$icc_by_group) &&
          any(sub$icc_by_group$Approx == "*")) {
        cat(paste(strwrap(paste(
          "-", "* this grouping factor also carries a random slope, so its",
          "share (intercept variance only) is approximate. See",
          "performance::icc() for an adjusted version."),
          width = 76, exdent = 2), collapse = "\n"), "\n", sep = "")
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
    # Diagnostics figure: redisplay the stored PNG, or rebuild it from the
    # model. If neither is available (diagnostic_plots = FALSE and no model),
    # skip the diagnostics but still fall through to the stored means /
    # interaction ggplots below.
    if (is.null(png_path) || !file.exists(png_path)) {
      if (is.null(sub$model)) {
        message("No diagnostics available", if (!is.null(label)) paste0(" for '", label, "'") else "",
                " - plots were disabled with diagnostic_plots = FALSE.")
      } else {
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
      }
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

    # ------------------------------------------------------------------
    # Coefficient forest plot + means / interaction plot(s)
    # The publication-ready ggplot2 figures are built once during the
    # analysis and stored in the object (coef_forest_plot, effect_plot_*,
    # interaction_plot_*). Re-print those stored ggplot objects here so the
    # interactive plot() output matches the report output exactly (shared
    # f_theme_pub() theme and f_pub_palette() colours). Mirrors plot.f_aov().
    plot_keys <- grep(paste0("^(coef_forest_plot|effect_plot_|",
                             "interaction_plot_|contrast_plot_|",
                             "interaction_contrast_plot_)"),
                      names(sub), value = TRUE)
    for (k in plot_keys) {
      p_obj <- sub[[k]]
      if (inherits(p_obj, "ggplot")) print(p_obj)
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
