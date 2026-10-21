#' Perform multiple \code{aov()} functions with optional data transformation, inspection and Post Hoc test.
#'
#' Performs an Analysis of Variance (ANOVA) on a given dataset with options for (Box-Cox)
#' transformations, normality tests, and post hoc analysis. The omnibus table
#' is computed with \strong{Type II Sums of Squares} via
#' \code{\link[car]{Anova}}, which is order-invariant for the main effects in
#' unbalanced designs (default \code{summary(aov())} uses Type I SS, where the
#' main-effect p-values depend on the order in which terms appear in the
#' formula). Type II also aligns with the model-based \code{emmeans} post hoc
#' tests, so the omnibus table and the pairwise comparisons cannot tell
#' mismatched stories on unbalanced data. Several response parameters can be
#' analysed in sequence and the generated output can be in various formats
#' ('Word', 'pdf', 'Excel').
#'
#' @param formula A formula specifying the model to be fitted. More response variables can be added using \code{-} or \code{+} (e.g., \code{response1 + response2 ~ predictor}) to do a sequential \code{aov()} for each response parameter.
#' @param data A data frame containing the variables in the model.
#' @param diagnostic_plots Logical. If \code{TRUE}, diagnostic residual plots are included in the output files. Default is \code{TRUE}.
#' @param effect_plots Logical. If \code{TRUE}, estimated means / interaction plots are included in the output files after the post hoc table. Default is \code{TRUE}.
#' @param norm_plots \strong{Deprecated} in 3.2.1. Use \code{diagnostic_plots} instead. If
#'   supplied, its value is passed to \code{diagnostic_plots} with a warning.
#' @param interaction_plots \strong{Deprecated} in 3.2.1. Use \code{effect_plots} instead. If
#'   supplied, its value is passed to \code{effect_plots} with a warning.
#' @param contrast_plots Logical. If \code{TRUE}, a \strong{contrast forest
#'   plot} is added for each categorical post hoc term: one row per pairwise
#'   comparison, showing the estimated difference between two levels with its
#'   confidence interval and a reference line at zero. A CI that excludes zero
#'   indicates a significant difference; because the interval is on the
#'   difference itself, this "excludes zero" reading is exact (it is the same
#'   information the compact-letter display encodes, but it also shows the
#'   direction and magnitude of each difference). Default \code{FALSE} because
#'   the number of pairwise contrasts grows quickly with the number of factor
#'   levels (k levels give k(k-1)/2 contrasts); turn it on when you want the
#'   detailed pairwise picture. Main-effect and interaction contrast plots are
#'   kept separate: main-effect plots are stored as
#'   \code{out$y1$contrast_plot_<term>} (e.g. \code{contrast_plot_treatment})
#'   while interaction cell-contrast plots are stored as
#'   \code{out$y1$interaction_contrast_plot_<term>} (e.g.
#'   \code{interaction_contrast_plot_a_b}). Contrast CIs use the same
#'   \code{adjust} method as the post hoc p-values, so figure and table agree.
#' @param ANCOVA Logical. If \code{TRUE}, prevents automatic conversion of predictors to factors, allowing for Analysis of Covariance (ANCOVA). Default is \code{FALSE}.
#' @param transformation Logical or character string. If \code{TRUE}, or if \code{"boxcox"} applies a \code{f_boxcox()} transformation if residuals are not normal. If \code{"bestnormalize"}, applies \code{f_bestNormalize()} transformation. If \code{FALSE} no transformation will be applied. Default is \code{TRUE}.
#'   A transformation is kept only when it improves the residual check
#'   that triggered it: normality (Shapiro-Wilk, or Anderson-Darling when
#'   Shapiro-Wilk is unavailable) or, when only the variance check failed, equal
#'   variance (Levene's test). Otherwise the untransformed analysis is reported, the
#'   report says so, and the attempt is recorded in \code{transformation_rejected}.
#'   Responses named in \code{force_transformation} are always transformed.
#' @param force_transformation Character string. A vector containing the names of response variables that should be transformed regardless of the normality test. Default is \code{NULL}
#' @param force_aov Logical. Default \code{FALSE}. A response is skipped (with a
#'   message) when its model is saturated: zero residual degrees of freedom, e.g.
#'   a fully crossed design with \eqn{n = 1} in every cell, where no F-test
#'   exists. Saturation is judged from the rank of the model matrix, so additive
#'   designs with one observation per cell, such as a randomised complete block
#'   design (\code{y ~ block + treatment}), are analysed normally. Since rfriend
#'   3.2.1, \code{force_aov = TRUE} on a saturated model stops with an
#'   explanation instead of fitting it, because the F-tests cannot be computed.
#'   (Before 3.2.1 the skip also fired for any cell with \eqn{n = 1}.)
#'   \code{Error()} terms are not supported; use \code{\link{f_lmer}} with
#'   random effects instead.
#' @param alpha Numeric. Significance level for ANOVA, post hoc tests, and Shapiro-Wilk test. Default is \code{0.05}.
#' @param adjust Character string specifying the method used to adjust p-values
#'   for multiple comparisons. Available methods include:
#'   \describe{
#'     \item{"tukey"}{Tukey's Honest Significant Difference method, appropriate for
#'                   all pairwise comparisons. Controls family-wise error rate.}
#'     \item{"sidak"}{Sidak correction that controls the family-wise error rate.
#'                   Less conservative than Bonferroni.}
#'     \item{"bonferroni"}{Conservative adjustment that multiplies p-values by
#'                        the number of comparisons.}
#'     \item{"none"}{No adjustment. Equivalent to Fisher's LSD method.}
#'     \item{"fdr"}{False Discovery Rate adjustment, controls the expected proportion
#'                 of false positives among significant results.}
#'   } Default is \code{"sidak"}.
#' @param post_hoc Which post hoc tables to build. \code{NULL} or \code{TRUE}
#'   (default): one table per term, see Details, "Which post hoc tables you
#'   get". \code{FALSE}: no post hoc. A character vector of model terms: only
#'   those tables, e.g. \code{post_hoc = "treatment"} (the marginal means of one
#'   factor, averaged over the others), \code{post_hoc = c("treatment",
#'   "week")}, or \code{post_hoc = "treatment:week"} (the cell means of that
#'   interaction, every combination compared). A requested term that is not
#'   significant is still tabulated, with letters shown as \code{"ns"}.
#'   Misspelled or unknown names stop with a "did you mean" error. The same
#'   syntax is used by \code{\link{f_lmer}}.
#'
#'   \strong{Leaving blocks out of the post hoc.} A block belongs in the model
#'   because it removes known variation (soil, bench, day) from the error term,
#'   which makes the treatment comparison more precise. Comparing the blocks with
#'   each other, however, is not a research question, and block letters invite
#'   over-interpretation. So keep the block in the formula and name only the
#'   factors you study in \code{post_hoc}:
#'   \code{f_aov(y ~ block + treatment, data, post_hoc = "treatment")}. The ANOVA
#'   table still reports the block effect. With five or more blocks that are a
#'   random sample of possible blocks, consider a mixed model instead:
#'   \code{f_lmer(y ~ treatment + (1 | block))}.
#' @param by \code{NULL} (default) or the name of one categorical predictor,
#'   used together with \code{post_hoc} to get simple effects:
#'   \code{post_hoc = "treatment", by = "week"} answers "which treatments differ
#'   within each week?" with \code{emmeans(model, ~ treatment | week)}. Letters
#'   and the p-value adjustment are computed separately within each level of
#'   \code{by}. This makes fewer comparisons than the full cell-means table (for
#'   4 treatments x 3 weeks: 3 x 6 = 18 instead of 66), so each comparison has
#'   more power. The table is stored as
#'   \code{out$y$post_hoc[["treatment | week"]]}. \code{by} requires
#'   \code{post_hoc}, must not also be named in \code{post_hoc}, and cannot be
#'   combined with an interaction term (\code{"a:b"}) in \code{post_hoc}.
#' @param anova_type Integer, either \code{2} or \code{3}. Sums of Squares
#'   type for the omnibus ANOVA table computed via \code{\link[car]{Anova}}.
#'   \describe{
#'     \item{\code{2} (Default)}{Type II. Order-invariant in unbalanced
#'       designs (\code{drug * dose} and \code{dose * drug} give the same
#'       main-effect p-values), respects the marginality principle (each
#'       main effect is tested after all other main effects, ignoring
#'       interactions containing it), and is safe with R's default
#'       treatment contrasts. Recommended for most unbalanced designs and
#'       consistent with the \code{emmeans}-based post hoc tests.}
#'     \item{\code{3}}{Type III. Also order-invariant, but tests each
#'       term after \emph{all} other terms including higher-order
#'       interactions. Type III is the SPSS / SAS default. For its
#'       main-effect rows to be interpretable as effects averaged across
#'       the other factors, the model must be fitted with orthogonal
#'       (sum / effect / Helmert / polynomial) contrasts. When
#'       \code{anova_type = 3} \strong{and} the user has not supplied
#'       their own \code{contrasts} via \code{...}, \code{f_aov}
#'       automatically installs \code{contr.sum} / \code{contr.poly} for
#'       the duration of the call (the previous \code{options("contrasts")}
#'       is restored on exit). Note that under Type III, when an
#'       interaction is significant the main effect rows are conditional
#'       on the interaction and should be interpreted with care -- the
#'       cell means table that \code{f_aov} reports automatically when an
#'       interaction is significant remains the appropriate summary.}
#'   }
#' @param intro_text Logical. If \code{TRUE}, includes a short explanation about ANOVA assumptions in the output file. Default is \code{TRUE}.
#' @param close_generated_files Logical. Closes open Excel or Word (NOT pdf) files before writing, depending on the output format. Works on Windows (taskkill), macOS (pkill) and Linux (pkill/soffice). Default \code{FALSE}. \strong{WARNING:} Always save your work before using this option!!
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
#'
#' @param save_as Character string specifying the output file path (without extension).
#'   If a full path is provided, output is saved to that location.
#'   If only a filename is given, the file is saved in \code{tempdir()}.
#'   If only a directory is specified (providing an existing directory with trailing slash),
#'   the file is named "dataname_aov_output" in that directory. If an extension is provided the output format specified with option "output_type" will be overruled.
#'   Defaults to \code{file.path(tempdir(), "dataname_summary.pdf")}.
#' @param save_in_wdir Logical. If \code{TRUE}, saves the file in the working directory. Default is \code{FALSE}, this avoid unintended changes to the global environment. If \code{save_as} location is specified \code{save_in_wdir} is overwritten by \code{save_as}.
#' @param ... Additional arguments forwarded to \code{\link[stats]{aov}}.
#'   The arguments \code{subset}, \code{na.action}, and \code{weights} are
#'   handled specially: when supplied, they are applied via
#'   \code{\link[stats]{model.frame}} so that the n=1 cell check,
#'   Shapiro-Wilk test, Levene test, optional transformations, residual
#'   diagnostics, and \code{emmeans} post hoc tests all see the exact
#'   same row set as \code{aov()} itself. Any other \code{aov()}
#'   arguments (e.g. \code{contrasts}, \code{projections},
#'   \code{qr}, \code{contrasts.arg}) are passed through unchanged.

#' @return An object of class 'f_aov' containing the fitted model
#'   (\code{aov_test}), the \strong{Type II} omnibus ANOVA table from
#'   \code{\link[car]{Anova}} (\code{aov_summary}), normality and homogeneity
#'   diagnostics, optional transformation results, and the \code{emmeans}
#'   post hoc tests, one table per term in \code{out$y$post_hoc} (see
#'   Details, "Which post hoc tables you get"). The elements
#'   \code{post_hoc_summary_table}, \code{summary_table} and \code{cld_text} are
#'   \strong{deprecated} and will be removed in the next release: they now hold
#'   the cell-means table when a significant interaction is tabulated, otherwise
#'   the first table; use \code{post_hoc} instead.
#'   \code{Response_Transformed}: \code{TRUE} when the response was
#'   transformed (Box-Cox or bestNormalize) because the residuals were not normal (or, alone, had unequal variances) and
#'   the transformation was kept (see \code{transformation}).
#'   \code{aov_summary}: ANOVA table of the untransformed response.
#'   \code{transformed_aov_summary}: ANOVA table of the transformed response, or
#'   \code{NULL} when no transformation was applied. The printed and reported
#'   result is \code{transformed_aov_summary} when \code{Response_Transformed} is
#'   \code{TRUE}, otherwise \code{aov_summary}. To pick it in code:
#'   \code{tab <- if (out$y$Response_Transformed) out$y$transformed_aov_summary else out$y$aov_summary}.
#'   When a transformation was tried but not kept, \code{transformation_rejected}
#'   records the method, the reason and the p-values before and after.
#'   Using the option \code{output_type}, it can also
#'   generate output as R Markdown, 'Word', 'pdf', or 'Excel' files.
#'   Includes \code{print} and \code{plot} methods for 'f_aov' objects.

#' @details
#' The function performs the following steps:
#' \itemize{
#' \item Check if all specified variables are present in the data.
#' \item Ensure that the response variable is numeric.
#' \item Fit the model with \code{\link[stats]{aov}} and compute the
#'       omnibus ANOVA table with \strong{Type II Sums of Squares} via
#'       \code{\link[car]{Anova}}. Type II is used (instead of the default
#'       Type I from \code{summary(aov())}) because Type I main-effect SS
#'       depend on the order of terms in the formula in unbalanced designs,
#'       whereas the \code{emmeans}-based post hoc tests are model-based
#'       and therefore order-invariant. Pairing Type I with \code{emmeans}
#'       can produce mismatched stories between the omnibus and post hoc
#'       tables. Type II keeps both order-invariant and is safe with R's
#'       default treatment contrasts (unlike Type III, which would require
#'       sum / effect contrasts to be interpretable for main effects).
#' \item Check normality of residuals using the Shapiro-Wilk test.
#' \item If residuals are not normal and \code{transformation = TRUE} apply a data transformation.
#' \item Post hoc tests with estimated marginal means from \code{emmeans()}, one table per term, with Sidak adjustment (or another option of \code{adjust =}) within each table.
#' }
#'
#' \strong{Which post hoc tables you get.} With the default
#' \code{post_hoc = NULL}, each term is handled on its own: every significant
#' interaction of categorical factors gets one cell-means table (all
#' combinations compared); every categorical main effect gets its own
#' marginal-means table (\code{emmeans(model, ~ factor)}), with letters
#' \code{"ns"} when the term is not significant and an "Interpret with caution"
#' note when the factor takes part in a significant interaction; numeric
#' covariates (ANCOVA) get no table, their slope is in the ANOVA table. The
#' \code{adjust} method is applied within each table, and each table has its
#' own footer with its own number of estimates and tests. Marginal means are
#' equal-weighted estimated marginal means: every level of the other factors
#' counts equally, so in unbalanced designs they can differ from the raw group
#' means. All tables are stored in \code{out$y$post_hoc[[term]]} as a list
#' with \code{emmeans}, \code{pairs}, \code{cld} (the letters), \code{cld_text}
#' (the footer) and \code{caution}, the same structure as \code{\link{f_lmer}}.
#' When the response was transformed, estimates and CIs are back-transformed
#' per table (medians on the original scale) and the pairwise contrasts stay on
#' the transformed scale. Effect plots take their letters from the factor's own
#' table; a plot that averages over a factor in a significant interaction shows
#' no letters and says why. Use \code{post_hoc} and \code{by} to choose the
#' tables yourself.
#'
#' \strong{Effect and interaction plots.} When \code{effect_plots = TRUE},
#' an estimated marginal means plot (estimate \eqn{\pm} 95\% CI, with jittered
#' raw data and compact-letter-display labels) is added after the post hoc
#' table for each categorical predictor. For a significant categorical
#' interaction, interaction plots are drawn instead: a two-way interaction uses
#' the x-axis plus colour (both orientations), while three- and four-way
#' interactions add facet panels for the remaining factor(s), with one plot per
#' choice of x-axis factor. Interactions involving five or more categorical
#' factors are not plotted (a warning is issued); consult the post hoc
#' cell-means table instead. When the response was transformed (Box-Cox or
#' bestNormalize), the plotted estimates are back-transformed to the original
#' scale (medians). The plots themselves are kept clean for publication (data,
#' axes, and legend only); the descriptive label and explanatory caption are
#' emitted as text above and below each figure in the report. All effect and
#' interaction plots are \pkg{ggplot2} objects and are stored in the returned
#' object (e.g. \code{out$y1$effect_plot_treatment},
#' \code{out$y1$interaction_plot_a_b_1}) so they can be retrieved and customised
#' afterwards. Matches \code{\link{f_glm}}.
#'
#' More response variables can be added using \code{-} or \code{+} (e.g., \code{response1 + response2 ~ predictor}) to do a sequential \code{aov()} for each response parameter captured in one output file.
#'
#' Outputs can be generated in multiple formats ("pdf", "word", "excel" and "rmd") as specified by \code{output_type}. The function also closes any open 'Word' files to avoid conflicts when generating 'Word' documents. If \code{output_type = "rmd"} is used it is adviced to use it in a chunk with \{r, echo=FALSE, results='asis'\}
#'
#' *Non-significant ANOVA results*: When the overall F-test is not significant, f_aov still reports the estimated marginal means table, but with all pairwise comparison letters replaced by *"ns"*. The numeric estimates (and their confidence intervals) are provided because they are often needed for manuscript tables, especially when the response was back-transformed from a Box-Cox or bestNormalize scale - the raw descriptive means and the emmeans values can differ, and it is the emmeans values that correspond to the actual model. The *"ns"* labels signal that pairwise differences should not be interpreted.
#'
#' This function requires [Pandoc](https://github.com/jgm/pandoc/releases/tag) (version 3.1 or higher), a universal document converter.
#'\itemize{
#' \item \bold{Windows:} Install Pandoc and ensure the installation folder.
#' \cr (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your system PATH.
#' \item \bold{macOS:} If using Homebrew, Pandoc is typically installed in "/usr/local/bin". Alternatively, download the .pkg installer and verify that the binary's location is in your PATH.
#' \item \bold{Linux:} Install Pandoc through your distribution's package manager (commonly installed in "/usr/bin" or "/usr/local/bin") or manually, and ensure the directory containing Pandoc is in your PATH.
#'
#' \item If Pandoc is not found, this function may not work as intended.
#' }
#'
#' @section Multiple Testing Across Response Variables:
#' When several response variables are analysed in a single call
#' (e.g. \code{y1 + y2 + y3 ~ treatment}), each ANOVA is an independent
#' null-hypothesis test at level \code{alpha}. The post hoc adjustments
#' (\code{adjust = "sidak"}, \code{"tukey"}, etc.) only control the
#' family-wise error rate \strong{within} one ANOVA (across pairwise group
#' comparisons for that response). They do \strong{not} protect against
#' the inflation of Type I error \strong{across} the set of responses.
#'
#' \strong{Practical implication:} With \eqn{k} independent response
#' variables all tested at \eqn{\alpha = 0.05}, the probability of
#' obtaining at least one false positive is
#' \eqn{1 - (1 - 0.05)^k}, which reaches ~40\% for \eqn{k = 10}.
#'
#' \strong{When this matters:} The risk is highest in exploratory studies
#' where many responses are screened simultaneously without a clear
#' a priori hypothesis for each one. It is less of a concern when
#' each response is a pre-specified primary outcome with its own
#' biological rationale.
#'
#' \strong{Possible remedies:}
#' \itemize{
#'   \item \strong{Bonferroni correction across responses:} use
#'     \code{alpha = 0.05 / k} where \code{k} is the number of
#'     response variables. Conservative but simple.
#'   \item \strong{False Discovery Rate (FDR):} apply
#'     \code{p.adjust(p_values, method = "fdr")} to the vector of
#'     per-response ANOVA p-values after the fact.
#'   \item \strong{MANOVA:} if the responses are correlated and you
#'     want a single omnibus test across all of them, use
#'     \code{manova()} before interpreting individual ANOVAs.
#'   \item \strong{Pre-registration:} declare primary vs. exploratory
#'     responses before data collection to justify differential
#'     correction thresholds.
#' }
#'
#' @author
#' Sander H. van Delden  \email{plantmind@proton.me} \cr
#'
#' @examples
#'\donttest{
#' # The left hand side contains two response variables,
#' # so two aov's will be conducted, i.e. "Sepal.Width"
#' # and "Sepal.Length" in response to the explanatory variable: "Species".
#' f_aov_out <- f_aov(Sepal.Width + Sepal.Length ~ Species,
#'                    data = iris,
#'                    # Save output in MS Word file (Default is console)
#'                    output_type = "word",
#'                    # Do bestNormalize transformation for non-normal residual (Default is boxcox)
#'                    transformation = "bestnormalize"
#'                    )
#'
#' # Print output to the console.
#' print(f_aov_out)
#'
#' # Plot residual plots.
#' plot(f_aov_out)
#'
#' # Randomised complete block design: the block stays in the model, but
#' # only the treatment is compared in the post hoc.
#' wk6 <- subset(plant_trial, time_weeks == 6)
#' f_aov(height_cm ~ block + treatment, data = wk6, post_hoc = "treatment")
#'
#' # Simple effects: which treatments differ within each week?
#' # illustration only: plants are measured repeatedly, see f_lmer()
#' pt <- plant_trial
#' pt$week <- factor(pt$time_weeks)
#' f_aov(height_cm ~ treatment * week, data = pt,
#'       post_hoc = "treatment", by = "week")
#' }
#'
#' #To print rmd output set chunck option to results = 'asis' and use cat().
#' f_aov_rmd_out <- f_aov(Sepal.Width ~ Species, data = iris, output_type = "rmd")
#' cat(f_aov_rmd_out$rmd)
#'
#' @export
f_aov <- function(formula,
                  # aov function formula
                  data = NULL,
                  # Data.frame used for aov
                  diagnostic_plots = TRUE,
                  # Show diagnostic residual plots in output files
                  effect_plots = TRUE,
                  # Show estimated means / interaction plots in output files
                  contrast_plots = FALSE,
                  # Show pairwise contrast forest plots in output files
                  ANCOVA = FALSE,
                  # Prevent automatic conversion to factors of all predictors for ANCOVA
                  transformation = TRUE,
                  # Preform transformation
                  force_transformation = NULL,
                  # force transformation for response var regardless of normality.
                  force_aov = FALSE,
                  # If TRUE, runs ANOVA even when min cell size = 1 (saturated model).
                  # Results are unreliable -- use only for diagnostics, not for reporting.
                  alpha = 0.05,
                  # Significance level for both aov, posthoc and Shapiro-Wilk Test
                  adjust = "sidak",
                  # Specifying the method used to adjust p-values
                  post_hoc = NULL,
                  # NULL/TRUE: per-term tables; FALSE: none; "a" / "a:b": only those
                  by = NULL,
                  # with post_hoc = "a": simple effects of a within each level of by
                  anova_type = 2,
                  # Sums of Squares type for the omnibus table (car::Anova):
                  #   2 = Type II (default; order-invariant, safe with R's
                  #       default treatment contrasts, respects marginality)
                  #   3 = Type III (order-invariant; for meaningful main
                  #       effects when interactions are present this requires
                  #       sum / effect contrasts -- f_aov sets them
                  #       automatically when type = 3 and the user has not
                  #       supplied their own `contrasts` via `...`).
                  intro_text = TRUE,
                  # Print short explanation about aov assumptions in output file
                  close_generated_files = FALSE,
                  # Closes either open excel or word files depending on the output format.
                  open_generated_files = interactive(),
                  # Open files after creation
                  output_type = "default",
                  # Output type can be excel, word, pdf, rmd, console, default
                  save_as = NULL,
                  # Specify the name of the output dir and file (name and type).
                  save_in_wdir = FALSE,         # Save file output in the working directory.
                  norm_plots = lifecycle::deprecated(),
                  interaction_plots = lifecycle::deprecated(),
                  ...
                  # Additional arguments forwarded to aov(). Currently
                  # honored: subset, na.action, weights. If supplied, they
                  # are applied via stats::model.frame() so that ALL
                  # downstream steps (n=1 cell check, Shapiro, Levene,
                  # transformations, residual plots, emmeans post hoc)
                  # see the exact same row set as aov() itself.
)
{



  ########## Reset initial settings on exit #################################
  .session_state <- save_session_state()  # Helper function: helper_session_state
  on.exit(restore_session_state(.session_state), add = TRUE) # Helper function: helper_session_state

  ########## Deprecated arguments ##########################################
  # Renamed in 3.2.1 so that all test functions use the same names for the
  # same report parts: `diagnostic_plots` (assumption and residual checks)
  # and `effect_plots` (effect, estimated-means and interaction plots).
  # The old name still works for one release, with a warning.
  if (lifecycle::is_present(norm_plots)) {
    lifecycle::deprecate_warn(
      when = "3.2.1",
      what = "f_aov(norm_plots)",
      with = "f_aov(diagnostic_plots)"
    )
    diagnostic_plots <- norm_plots
  }
  if (lifecycle::is_present(interaction_plots)) {
    lifecycle::deprecate_warn(
      when = "3.2.1",
      what = "f_aov(interaction_plots)",
      with = "f_aov(effect_plots)"
    )
    effect_plots <- interaction_plots
  }


  ########## Capture ... UNEVALUATED at f_aov's own frame ##################
  # Must be done HERE (in f_aov's body), not inside generate_report(),
  # because match.call() captures the call to whatever function it is
  # called from. Inside generate_report() it would capture
  # generate_report()'s (empty) call and dots_exprs would always be
  # NULL -- silently dropping subset / weights / na.action. The closure
  # makes dots_exprs visible inside generate_report().
  .mc         <- match.call(expand.dots = FALSE)
  # Coerce the `...` pairlist returned by match.call(expand.dots = FALSE)
  # to a real named list. Without as.list(), dots_exprs$subset partial-
  # matches into the raw pairlist and returns a `..1` dots-index symbol
  # instead of the actual expression, which then crashes model.frame()
  # with "the ... list contains fewer than 3 elements" the moment a
  # user actually passes subset / weights / na.action through dots.
  dots_exprs  <- as.list(.mc[["..."]])

  # Fail loud on misspelled arguments. `...` is forwarded to aov() and from
  # there to lm(), so their own arguments (subset, weights, na.action,
  # contrasts, ...) are the legitimate dot names; anything else would only
  # trigger lm.fit()'s "extra argument ... will be disregarded" warning and
  # silently run with the defaults.
  check_known_dots(...names(), ...length(),
                   allowed    = setdiff(union(names(formals(stats::aov)),
                                              names(formals(stats::lm))),
                                        c("formula", "data", "...")),
                   fun_name   = "f_aov",
                   valid_args = names(formals(sys.function())))

  # Capture the user's calling environment so that subset / weights /
  # na.action expressions passed via `...` can be resolved against it
  # (after `data` columns) from inside the generate_report() closure,
  # where parent.frame() would otherwise point at f_aov itself.
  caller_env  <- parent.frame()


  ####### Validate anova_type and set contrasts for Type III ################
  # Accept 2 or 3 only; anything else is a typo we want to fail loudly on
  # (per the project's fail-loud philosophy) rather than silently
  # default back to Type II.
  if (!isTRUE(anova_type %in% c(2, 3))) {
    stop("`anova_type` must be 2 (Type II) or 3 (Type III). Got: ",
         deparse(anova_type), call. = FALSE)
  }

  # Type III main effects in the presence of interactions are only
  # interpretable when the factor contrasts are orthogonal (sum / effect
  # / Helmert / polynomial). With R's default `contr.treatment`, the
  # "main effect" rows in a Type III table test the effect at the
  # *reference level* of every other factor, not an effect averaged
  # across them -- which is almost never what the user wants and is the
  # single most common Type III pitfall in R.
  #
  # We therefore install `contr.sum` / `contr.poly` globally for the
  # duration of this call when:
  #   - anova_type == 3, AND
  #   - the user did NOT supply their own `contrasts =` via `...`.
  # save_session_state() above snapshots options() and on.exit restores
  # them, so the override is fully scoped to this f_aov() call and
  # cannot leak into the caller's session.
  #
  # If the user DID pass their own `contrasts`, we leave them alone --
  # they made a conscious choice -- but emit a one-line note so they
  # know the assumption.
  user_supplied_contrasts <- "contrasts" %in% names(dots_exprs)
  if (anova_type == 3) {
    if (!user_supplied_contrasts) {
      options(contrasts = c("contr.sum", "contr.poly"))
    } else {
      message("[f_aov] anova_type = 3 with user-supplied `contrasts`: ",
              "Type III main-effect rows are only interpretable when the ",
              "contrasts are orthogonal (e.g. contr.sum, contr.helmert, ",
              "contr.poly). Treatment contrasts (the R default) test ",
              "effects at the reference level, not averaged across other ",
              "factors. Override at your own risk.")
    }
  }


  ####### Save dataframe name and Handle input from vectors (dataframe column) #####

  if(!is.null(data)){
    # Save dataframe name
    data_name <- deparse(substitute(data))

  } else if(is.null(data)){

    if(length(formula_extract_df_names(formula)) == 0){
      data_name <- "data"

    } else if(length(formula_extract_df_names(formula)) == 1){
      data_name <- formula_extract_df_names(formula)

    } else if(length(formula_extract_df_names(formula)) > 1){
      data_name <- paste(formula_extract_df_names(formula), collapse = "_")

    }

    # Make a data.frame based on the formula
    data <- formula_to_dataframe(formula)

    # Rewrite formula without data frame prefixes
    formula <- clean_formula(formula)

  }

  # Generate a temporary file path for "output.Rmd"
  temp_output_dir <- tempdir()
  temp_output_file <- file.path(temp_output_dir, "output.Rmd")

  # Create the output file "output.Rmd" in tempdir()
  file.create(temp_output_file)
  file_extension <- NULL

  # Wrap lines in rmd output document
  f_wrap_lines()

  # Create a list to store all outputs in this function
  output_list <- list()

  # Parameter validation
  if( !(output_type %in% c("pdf", "word", "excel", "rmd", "console" , "default")) ){
    stop("Character string specifying the output format (output_type = ) should be either: 'pdf', 'word', 'excel', 'console','rmd', 'default'")
  }


  #### Handle option "save_as = " ###
  if(save_in_wdir == TRUE){
    save_dir <- getwd()
  }else{
    save_dir <- tempdir()
  }

  #map the output type to extensions
  output_type_map <- c(
    "pdf"  = ".pdf",
    "word" = ".docx",
    "excel"= ".xlsx",
    "rmd"  = ".rmd"
  )

  # If the user specifies a path, filename or save_in_wdir == TRUE an output file should be created
  if (!is.null(save_as) || save_in_wdir == TRUE) {

    if (!is.null(save_as)) {
      #Remove backslash in save_as if needed
      save_as <- gsub(pattern = "\\\\", replacement = "/", x = save_as)
      file_extension_save_as <- unname(extract_extension(save_as))
      if(file_extension_save_as[1] != FALSE){
        file_extension <- file_extension_save_as
      }
    }

    if(is.null(file_extension) && output_type %in% c("console", "default")){
      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "aov_output", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = ".pdf"
      )
      #set output_type to default
      output_type <- "pdf"

    }
    else if(is.null(file_extension) && output_type %in% c("pdf", "word", "excel", "rmd")){

      #create extension based on input_type
      file.ext <- unname(output_type_map[output_type])

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "aov_output", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = file.ext
      )


    }
    else if(!is.null(file_extension)) {

      # use helper get_save_path() to create output_path
      output_path <- get_save_path(save_as = save_as,
                                   default_name = paste(data_name, "aov_output", sep = "_"),
                                   default_dir = save_dir,
                                   file.ext = file_extension[1]
      )
      # reset the output type to match the user input extention in save_as
      output_type <- file_extension[2]
    }
  } else {

    #create extension based on input_type
    file.ext <- unname(output_type_map[output_type])

    # use helper get_save_path() to create output_path
    output_path <- get_save_path(save_as = save_as,
                                 default_name = paste(data_name, "aov_output", sep = "_"),
                                 default_dir = save_dir,
                                 file.ext = file.ext
    )
  }


  # Prevent output to console and keep files open when output is "rmd" format
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



  # Convert the input to a character string and force lowercase for case-insensitive matching
  if(is.character(transformation)){
    trans_input <- tolower(as.character(transformation))

    # Valid options include the full names and also the string versions of logicals.
    valid_options <- c("bestnormalize", "boxcox", "true", "false")

    # Use pmatch for partial matching; pmatch returns NA for ambiguous or no matches.
    matched_index <- pmatch(trans_input, valid_options)

    if (is.na(matched_index))
      stop("Invalid transformation option!")

    matched_option <- valid_options[matched_index]

    # Convert string "true" or "false" to logical values if applicable
    if (matched_option == "true") {
      transformation <- TRUE
    } else if (matched_option == "false") {
      transformation <- FALSE
    } else {
      transformation <- matched_option  # Either "bestnormalize" or "boxcox"
    }
  }

  # Warn if LHS has expressions like log(y) before silently stripping them
  check_lhs_is_names(formula) #use helper_check_lhs.R

  # Extract response variables from the left-hand side of the formula
  lhs <- all.vars(formula[[2]])  # Get LHS variables (response)
  response_names <- lhs

  # Multi-stratum Error() models are not supported: car::Anova(), the
  # assumption checks and the emmeans post hoc all need a single-stratum
  # model. Stop with the mixed-model equivalent instead of failing later.
  if ("Error" %in% all.names(formula[[3]])) {
    stop("f_aov() does not support Error() terms (multi-stratum / repeated-",
         "measures ANOVA).\n",
         "  Fit the error strata as random effects with f_lmer() instead, e.g.\n",
         "  f_aov(y ~ time + Error(subject/time))  ->  ",
         "f_lmer(y ~ time + (1 | subject))",
         call. = FALSE)
  }

  # Extract predictor variables from the right-hand side of the formula
  predictor_names <- all.vars(formula[[3]]) # Get RHS variables (predictors)

  # Extract the right-hand side (RHS) of the formula as a string
  rhs <- paste(deparse(formula[[3]]), collapse = " ")  # Preserve the RHS structure

  # Ensure response and predictors are in the data
  for (response in response_names) {

    if (!(response %in% names(data))) {
      stop(paste("Response variable", response, "not found in the data."))
    }
    # Ensure the response variable is numeric
    response_var <- data[[response]]

    if (!is.numeric(response_var)) {
      stop("The response variable must be numeric.")
    }
  }

  for (predictor in predictor_names) {
    if (!(predictor %in% names(data))) {
      stop(paste("Predictor variable", predictor, "not found in the data."))
    }
  }

  # Prevent from automatic conversion to factors to allow for ANCOVA
  if(ANCOVA == FALSE){
    # Ensure all predictor variables are factors
    for (predictor in predictor_names) {
      data[[predictor]] <- as.factor(data[[predictor]])
    }
  }

  # This is the main function that generates the content for all the output types
  # output = FALSE, return output_list FALSE no output_list used for rmd output
  generate_report <- function(output = TRUE) {

    # This text reminds the user of the assumptions of an aov
    # it its show by default but can be hidden.
    if(intro_text == TRUE){

      cat("
# Assumptions of ANOVA
Checking the assumptions of ANOVA (Analysis of Variance) are critical for ensuring the validity of its results:\n

## 1. Independence
- Observations must be independent both within and between groups. This means that the value of one observation should not influence another.
- Independence violations cannot be corrected statistically and invalidate the analysis, making proper experimental design essential.

## 2. Normality
- The residuals (errors) of the model are assumed to be normally distributed. This assumption applies to the residuals, not necessarily the raw data.
- ANOVA is robust to minor deviations from normality, especially with large and balanced sample sizes. For small or unbalanced samples, violations can lead to **unreliable results**, requiring a data transformation or alternative tests (Welch's ANOVA, Kruskal-Wallis Test).
- Normality of the residuals can be tested using a Shapiro-Wilk or Anderson-Darling Test. It can also be graphically assessed using a Box Plot, Q-Q plot or Histogram.

## 3. Homogeneity of Variances (Homoscedasticity)
- The variances within each group should be approximately equal (homogeneity of variances). This ensures that the F-test statistic is reliable.
- Homogeneity of variances assumption in ANOVA should be tested on the residuals, not directly on the raw data.
- Levene's test can be applied to check for homogeneity of variances. It can also be graphically assessed by plotting residuals vs. fitted values and checking for patterns.
- If violated, a data transformation or alternative tests (Welch's ANOVA, Kruskal-Wallis Test) are required.


## 4. Additivity (No Unaccounted Systematic Effects)
- For models without interaction terms, it is assumed that the effects of different factors are additive. That is, the combined effect of factors can be expressed as the sum of their individual effects.
- However, if interaction terms are included in the model, this assumption does not apply because ANOVA then explicitly accounts for potential interactions.
  \n  \n")

      if(output_type != "rmd"){
        # Pagebreak
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage
        ")
      }
    }

    #create count to remove last page break
    i <- 0

    # Multiple-response warning: only shown when > 1 response variable is analysed.
    # Fires regardless of intro_text so the user always sees it in this situation.
    if (length(lhs) > 1) {
      k          <- length(lhs)
      fwer_pct   <- round((1 - (1 - alpha)^k) * 100, 1)
      bonf_alpha <- round(alpha / k, 4)
      cat(paste0(
        "\n\n***\n\n",
        "**[!] NOTE: Multiple Testing Across ", k, " Response Variables**  \n\n",
        "This report runs ", k, " independent ANOVAs on the same dataset. ",
        "The **", adjust,"** correction keeps each individual test honest, it guards against ",
        "false positives among the pairwise group comparisons, but it offers no protection ",
        " against the accumulation of error across all ",k," tests combined. ",
        "\nAt \u03b1 = ", alpha, " per test, the probability of obtaining at least one ",
        "spurious significant result across all ", k, " responses is approximately ",
        "**", fwer_pct, "%** ($1-(1-", alpha, ")^{", k, "}$, assuming independence). ",
        "This risk is highest in exploratory studies; it is less of a concern when ",
        "each response has a clear a priori hypothesis.  \n\n",
        "**Possible remedies:**  \n",
        "\n-  **Bonferroni** (conservative): re-run with `alpha = ", bonf_alpha,
        "` (\u03b1 / ", k, ").",
        "\n-  **False Discovery Rate (FDR)**: apply `p.adjust(p_values, method = \"fdr\")` ",
        "to the ", k, " ANOVA p-values after the fact.",
        "\n-  **MANOVA**: if responses are correlated, use `manova()` as a single ",
        "omnibus test before interpreting individual ANOVAs.",
        "\n-  **Pre-registration**: if each response was a pre-specified (primary) study ",
        "outcome, correction may not be required; document this decision explicitly.  \n  \n",
        "\n\n***\n\n"
      ))
    }

    # Build the analysis data set ONCE, before the response loop.
    # Use ALL responses + ALL predictors so every response is analysed
    # on the identical row set. Rows with NA in ANY response or
    # predictor are dropped (subject to na.action). Any subset /
    # na.action / weights passed via `...` is applied here.
    #
    # Strategy: pre-evaluate subset/weights eagerly against data first,
    # falling back to the user's calling environment. Then apply the
    # filter manually and pass plain values to stats::model.frame via
    # do.call. This avoids the fragile match.call/substitute dance
    # where spliced expressions inside a constructed call can end up
    # containing `..N` dots-index symbols that crash model.frame with
    # "the ... list contains fewer than 3 elements".
    combined_formula <- as.formula(
      paste("~", paste(c(lhs, predictor_names), collapse = " + "))
    )

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

    # Apply the manual subset filter to data (and weights, if its
    # length matches the pre-subset row count)
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

    # Build the model.frame call with plain values
    mf_args <- list(
      formula            = combined_formula,
      data               = data,
      drop.unused.levels = TRUE,
      na.action          = na_action_fn
    )
    if (!is.null(weights_vec)) mf_args$weights <- weights_vec

    data_master     <- do.call(stats::model.frame, mf_args)
    n_before_master <- nrow(data)
    n_after_master  <- nrow(data_master)
    na_notes <<- c(na_notes, na_removed_note(data, n_after_master,
                                             all.vars(combined_formula)))

    # Remaining ... args (e.g. contrasts, projections, qr) are
    # eager-evaluated in the caller's environment because they do not
    # reference data columns.
    extra_names <- setdiff(names(dots_exprs),
                           c("subset", "weights", "na.action"))
    aov_extra   <- lapply(dots_exprs[extra_names], eval,
                          envir = caller_env)
    # ---------------------------------------------------------------

    # Loop for several response parameters
    for (response_name in lhs) {

      # set these paramters to default for each respone_name
      aov_summary_transformed <- NULL
      Response_Transformed    <- FALSE

      # Store the transformation option and adjust method for output
      output_list[[response_name]][["transformation_option"]] <- transformation
      output_list[[response_name]][["adjust"]]                <- adjust

      # Create a new formula for each response, preserving interactions
      current_formula <- as.formula(paste0(response_name, "~", rhs))

      # Document the data loss (same for every response now -- both come
      # from the master filter applied before the loop).
      n_before <- n_before_master
      n_after  <- n_after_master

      # Fresh per-iteration copy of the master filtered frame. Required
      # because the transformation step (Box-Cox / bestNormalize) mutates
      # data_complete[[response_name]] in place; without a copy, that
      # mutation would persist into subsequent responses.
      data_complete <- data_master

      # Extract weights vector (NULL if none supplied) for the aov() calls.
      # model.frame stores them in a "(weights)" column; model.weights() pulls
      # them out as a plain numeric vector aligned with data_complete rows.
      mf_weights <- stats::model.weights(data_complete)

      n_after <- nrow(data_complete)

      # Snapshot the original (untransformed) response before data_complete is
      # modified in-place by the transformation step. Used for raw-data overlay
      # in plot.f_aov so points are always on the original scale.
      original_response <- data_complete[[response_name]]

      # Saturation check: must happen BEFORE aov()
      # A model with residual df = 0 (e.g. a fully crossed factorial with
      # n = 1 in every cell) has undefined F-statistics and p-values. Skip
      # this response entirely and continue with the others.
      #
      # Only categorical predictors (factor / character / logical)
      # define cells. In ANCOVA mode, numeric covariates have as many
      # "levels" as observations, which would falsely trigger the n=1
      # skip. Continuous predictors are therefore excluded from the
      # cell-size count.
      cat_predictors <- predictor_names[vapply(
        data_complete[predictor_names],
        function(v) is.factor(v) || is.character(v) || is.logical(v),
        logical(1)
      )]
      if (length(cat_predictors) == 0L) {
        # Pure regression (no categorical predictors): no factorial
        # design, no cell-size issue possible.
        min_cell_n <- nrow(data_complete)
      } else {
        cell_counts <- table(interaction(data_complete[cat_predictors], drop = TRUE))
        min_cell_n  <- min(cell_counts)
      }

      # Resolve post_hoc / by against this model's terms (fail loud on
      # unknown names, see helper_post_hoc.R).
      model_terms_ph <- attr(stats::terms(current_formula), "term.labels")
      ph_spec <- resolve_post_hoc_spec(
        post_hoc, by,
        cat_predictors     = cat_predictors,
        numeric_predictors = setdiff(predictor_names, cat_predictors),
        model_terms        = model_terms_ph,
        fun_name           = "f_aov")

      # Saturation is judged on the model's own residual df (model-matrix
      # rank), not on the smallest cell of all predictors crossed: an
      # additive design such as a randomised complete block
      # (y ~ block + treatment) has n = 1 per block x treatment cell by
      # construction yet plenty of residual df. min_cell_n is still used for
      # the Levene / small-cell notes.
      mm_check <- stats::model.matrix(current_formula, data = data_complete)
      resid_df <- nrow(mm_check) - qr(mm_check)$rank
      output_list[[response_name]][["resid_df"]] <- resid_df

      if (resid_df == 0) {
        if (!isTRUE(force_aov)) {
          # Default: skip this response entirely, continue with others
          cat(paste0(
            "   \n  \n# Analysis of: ", response_name, "  \n\n",
            "**WARNING: `", response_name, "` skipped.**  \n\n",
            "The model leaves no residual degrees of freedom (for example a ",
            "fully crossed design with $n = 1$ in every cell). ",
            "This saturates the ANOVA model, ",
            "making F-statistics and p-values undefined and uninterpretable.  \n\n",
            "**What to do:**  \n",
            "\n- Collect more observations so every factor-level combination has $n \\geq 2$.",
            "\n- Simplify the model by removing interaction terms (`", response_name,
            " ~ ", paste(predictor_names, collapse = " + "), "`).",
            "\n- If cells are structurally empty (impossible combinations), ",
            "consider a different experimental design.",
            "\n- If you want to run the ANOVA anyway for diagnostic purposes only, ",
            "re-run with `force_aov = TRUE`. ",
            "Results will be unreliable and should **not** be reported.  \n   \n"
          ))

          skipped_reason <- paste0("ANOVA undefined: residual df = 0; model ",
                                  "saturated.\n  Only re-run with ",
                                  "`force_aov = TRUE` for diagnostic purposes."
                                  )
          output_list[[response_name]][["skipped"]]        <- TRUE
          output_list[[response_name]][["skipped_reason"]] <- skipped_reason
          output_list[[response_name]][["min_cell_n"]]     <- min_cell_n
          i <- i + 1
          next

        } else {
          # force_aov = TRUE cannot help here: with residual df = 0 there is
          # no error variance, so car::Anova() has no F-test to compute.
          # (Before 3.2.1 the skip also fired for unsaturated designs with an
          # n = 1 cell, which force_aov could run; those are now analysed
          # without it.)
          stop("f_aov(): the model for '", response_name, "' has residual ",
               "df = 0 (saturated), so no F-test exists, even with ",
               "force_aov = TRUE. Collect more observations, or drop the ",
               "interaction term(s), e.g. ", response_name, " ~ ",
               paste(predictor_names, collapse = " + "), ".", call. = FALSE)
        }
      }
      # ------------------------------------------------------------------

      cat("   \n  \n# Analysis of: ", response_name, "  \n")
      cat("  \n## Normality and homoscedasticity of residuals of: ", response_name, "  \n")


      # Perform ANOVA. Forward any extra args from `...` to aov().
      # subset / na.action / weights have already been baked into
      # data_complete via model.frame(); aov_extra was assembled once at
      # the master level above and contains only safe pass-through args
      # such as `contrasts`. weights is re-supplied from mf_weights.
      aov_test <- do.call(
        stats::aov,
        c(list(formula = current_formula, data = data_complete,
               weights = mf_weights), aov_extra)
      )
      output_list[[response_name]][["aov_test"]] <- aov_test
      res_aov <- residuals(aov_test)
      # Use Type II Sums of Squares (car::Anova) for the omnibus table by
      # default instead of Type I (summary.aov). For unbalanced multi-way
      # designs, Type I is order-dependent: swapping `drug * dose` for
      # `dose * drug` changes the main-effect p-values. The post-hoc tests
      # (emmeans) are model-based and therefore order-invariant; pairing
      # them with a Type I table can produce omnibus and post-hoc results
      # that tell mismatched stories. Type II avoids this and is safe with
      # R's default treatment contrasts. Type III is available via
      # `anova_type = 3` (with sum contrasts installed automatically; see
      # the top of f_aov() for details).
      aov_summary <- car::Anova(aov_test, type = anova_type)
      output_list[[response_name]][["aov_summary"]] <- aov_summary
      output_list[[response_name]][["anova_type"]]  <- anova_type
      output_list[[response_name]][["aov_call"]] <- deparse(current_formula)


      # Perform levene test on residuals
      levene_res <- rstatix::levene_test(as.formula(paste0(
        "res_aov~interaction(",
        paste(predictor_names, collapse = ", "),
        ")"
      )), data = data_complete)

      # Create a safe logical check. If p is NA, we assume it is NOT significant (FALSE)
      is_levene_sig <- !is.na(levene_res$p) & levene_res$p < alpha

      # Build the cell-size warning AFTER the test so we can condition on the
      # actual result rather than making preemptive claims that may contradict it.
      # - n=1 AND result IS NA  -> test truly failed; warn accordingly
      # - n=1 BUT result is valid -> Levene managed to compute; note small cells but
      #                              do not say the result is uninformative
      # - n<=3 AND result is valid -> low power note only
      levene_size_warning <- if (min_cell_n == 1 && is.na(levene_res$p)) {
        paste0("**Warning:** At least one group cell contains only $n = 1$ observation. ",
               "Levene's Test could not estimate within-group variance and returned NA ",
               "(result is uninformative).  \n  \n&nbsp;  \n")
      } else if (min_cell_n == 1 && !is.na(levene_res$p)) {
        paste0("**Note:** At least one group cell contains only $n = 1$ observation. ",
               "Levene's Test produced a result, but interpret it with caution; ",
               "variance estimates from single observations are unreliable.  \n  \n&nbsp;  \n")
      } else if (min_cell_n <= 3) {
        paste0("**Note:** The smallest group cell contains only $n = ", min_cell_n,
               "$ observation(s). Levene's Test has very low statistical power with ",
               "such small cell sizes; a non-significant result does **not** reliably ",
               "confirm homoscedasticity.  \n  \n&nbsp;  \n")
      } else {
        paste0("&nbsp;\n  \n&nbsp;  \n")
      }

      # Generate text for interpretation of levene output
      if (is.na(levene_res$p)) {
        levene_res_intp_text <- "Levene's Test could not be calculated (likely due to saturation or insufficient replication per group).  \n"
      } else if (levene_res$p > alpha) {
        levene_res_intp_text <- paste0(
          "According to 'Levene's Test' (",
          round(levene_res$p, digits = 4),
          " > ",
          alpha,
          ") residuals do not depart from **equal variance** (homoscedasticity)."
        )
      } else if (is_levene_sig) {
        levene_res_intp_text <- paste0(
          "According to 'Levene's Test' (",
          round(levene_res$p, digits = 4),
          " \u2264 ",
          alpha,
          ") residuals do **NOT** have equal variance (Heteroskedasticity)."
        )
      }
      # levene_output_res
      cat(
        "**Levene's test** for homogeneity of residuals: F-Statistic =",
        round(levene_res$statistic, digits = 4),
        "p-value =",
        round(levene_res$p, digits = 4),
        ". ", levene_res_intp_text)
      if (!is.null(levene_size_warning)) cat(levene_size_warning)

      # Shapiro-Wilk Test for normality of the aov residuals
      # shapiro.test() errors above n = 5000; fall back to AD-only when that occurs.
      # safe_shapiro helper
      shapiro_res <- safe_shapiro(res_aov)


      if (is.na(shapiro_res$p.value)) {
        shapiro_res_intp_text <- paste0(
          "Shapiro-Wilk was skipped (", shapiro_res$method,
          "). Normality decision is based on the Anderson-Darling test below.  \n  \n&nbsp;  \n"
        )
      } else if (shapiro_res$p.value > alpha) {
        shapiro_res_intp_text <- paste0(
          "According to 'Shapiro-Wilk Test' (",
          round(shapiro_res$p.value, digits = 4),
          " > ",
          alpha,
          ") no significant departure from **normality** was detected
          for the model residuals; check Q-Q plot.  \n  \n&nbsp;  \n"
        )
      } else if (shapiro_res$p.value <= alpha) {
        shapiro_res_intp_text <- paste0(
          "According to 'Shapiro-Wilk Test' (",
          round(shapiro_res$p.value, digits = 4),
          " \u2264 ",
          alpha,
          ") residuals are **NOT** normally distributed; check Q-Q plot.  \n  \n&nbsp;  \n"
        )
      }
      # shapiro_output_res
      cat(
        "**Shapiro-Wilk Test** for Normality of residuals: W =",
        round(shapiro_res$statistic, digits = 4),
        "p-value =",
        round(shapiro_res$p.value, digits = 4),
        ". ", shapiro_res_intp_text
      )

      # Perform Anderson-Darling normality test on residuals
      # ad.test() requires n >= 8; skip gracefully for very small samples
      if (length(res_aov) >= 8) {
        adt_res <- nortest::ad.test(res_aov)
      } else {
        adt_res <- list(statistic = NA_real_, p.value = NA_real_,
                        method = "Anderson-Darling test (skipped: n < 8)")
      }

      if (is.na(adt_res$p.value)) {
        adt_res_intp_text <- paste0(
          "Anderson-Darling test skipped (n < 8, minimum sample size not met).  \n  \n"
        )
      } else if (as.numeric(adt_res$p.value) > alpha) {
        adt_res_intp_text <- paste0(
          "According to 'Anderson-Darling test' (",
          round(adt_res$p.value, digits = 4),
          " > ",
          alpha,
          ") no significant departure from **normality** was detected
          for the model residuals; check Q-Q plot.  \n  \n"
        )
      } else if (as.numeric(adt_res$p.value) <= alpha) {
        adt_res_intp_text <- paste0(
          "According to 'Anderson-Darling test' (",
          round(adt_res$p.value, digits = 4),
          " \u2264 ",
          alpha,
          ") residuals are **NOT** normally distributed; check Q-Q plot.  \n  \n"
        )
      }
      # adt_output_res
      cat(
        adt_res$method,
        ": A =",
        round(adt_res$statistic, digits = 4),
        " p =",
        round(adt_res$p.value, digits = 4),
        "   \n", adt_res_intp_text
      )

      output_list[[response_name]][["Levene_test_on_res"]] <- levene_res
      output_list[[response_name]][["min_cell_n"]]         <- min_cell_n
      output_list[[response_name]][["shapiro_test_residuals"]] <- shapiro_res
      output_list[[response_name]][["adt_test_residuals"]] <- adt_res

      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = 7.8, height = 7.8, units = "in", res = 300)
      # Set up a 2x2 plotting layout with adjusted margins mar = c(bottom, left, top, right)
      par(mfrow = c(2, 2), mar = c(4, 4, 4, 1))  # Adjust left margin

      # Residual plot aov
      plot(aov_test, 1)

      # Boxplot
      boxplot(residuals(aov_test), main = "Boxplot", xlab = "Residuals")

      # Histogram
      f_hist(residuals(aov_test),  xlab = "Residuals")

      # QQPlot
      f_qqnorm(residuals(aov_test))

      # Close the png file
      dev.off()

      if(diagnostic_plots == TRUE){
        # Include the saved plots in R Markdown
        cat("Check the plots in the figure below to assess normality.  \n")
        cat(paste0("![](", temp_file, ")"), "   \n  \n")
        # cat("&nbsp;\n   \n")
      }

      output_list[[response_name]][["normality_plots"]] <- temp_file
      output_list[[response_name]][["alpha"]] <- alpha


      if(output_type != "rmd"){
        # Pagebreak
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage
        ")
      }

      # Decide whether residuals fail the normality check.
      # Shapiro-Wilk is the primary test because it is the most powerful for
      # n <= 5000. For n > 5000 it is skipped (safe_shapiro returns NA), and
      # we fall back to Anderson-Darling, which has no upper sample-size
      # limit and is also reasonably powerful. If both are unavailable
      # (extreme edge case: n < 3 and n < 8), we leave trigger_normality
      # FALSE and defer the decision to the user inspecting the qq-plot.
      if (!is.na(shapiro_res$p.value)) {
        trigger_normality <- shapiro_res$p.value < alpha
      } else if (!is.na(adt_res$p.value)) {
        trigger_normality <- adt_res$p.value < alpha
      } else {
        trigger_normality <- FALSE
      }
      trigger_levene    <- is_levene_sig
      trigger_forced    <- response_name %in% force_transformation

      # If not normal or force_transformation directs to: apply transformation
      if (trigger_normality || trigger_levene || trigger_forced) {
        if (transformation == FALSE) {
          if (trigger_normality) {
            test_label <- if (!is.na(shapiro_res$p.value)) "Shapiro-Wilk" else "Anderson-Darling"
            cat(paste0(
              "   \n  \n**WARNING !!!**   \nBased on the ", test_label,
              " test the residuals are **NOT** normally distributed.   \n  \n",
              "Please enable the transformation function (transformation = TRUE) ",
              "in the f_aov function.   \n  \n"
            ))
          }
          if (trigger_levene){
            cat("   \n  \n**WARNING !!!**   \nBased on the Levene's Test the residuals do **NOT** have equal variance (Heteroskedasticity).   \n  \nPlease enable the transformation function function (transformation  == TRUE) in the f_aov function.   \n  \n")
          }
        }

        # Only apply Box-Cox / bestNormalize when normality is violated or forced.
        # Levene-only failure is flagged but transformation is not automatically applied,
        # because Box-Cox targets non-normality; a variance-stabilising transformation
        # (e.g. log/sqrt) may be more appropriate for pure heteroscedasticity.
        if (!trigger_normality && !trigger_forced && trigger_levene && transformation != FALSE) {
          cat("   \n  \n**NOTE**   \nHeteroscedasticity was detected (Levene's Test), but residuals are normally distributed (Shapiro-Wilk). Welch's ANOVA (`oneway.test(..., var.equal = FALSE)`) is recommended. \nNevertheless, Box-Cox is applied since it can also stabilise variance; check the transformed Levene result below.   \nIf heteroscedasticity persists after transformation, resort to other statistical tests.   \n  \n")
        }

        if (transformation == "boxcox" || (transformation == TRUE)) {
          cat("\n   \n## Box-Cox transformation of: ", response_name, "  \n  \n")

          capture.output(transformed_var <- f_boxcox(data_complete[[response_name]],
                                                     output_type = "rmd",
                                                     alpha = alpha
          )
          )
          cat("Note: the Shapiro-Wilk test below is performed on the response data, not the residuals. ")
          cat(transformed_var$rmd)

          output_list[[response_name]][["boxcox"]] <-  transformed_var$transformed_data
          data_complete[[response_name]] <- transformed_var$transformed_data# Update the response variable in the data

        }

        if (transformation == "bestnormalize" ) {

          # Apply the f_bestNormalize function
          transformed_var <- f_bestNormalize(data_complete[[response_name]],
                                             data_name = response_name,
                                             output_type = "rmd",
                                             alpha = alpha
          )

          # Save the transformed data object
          output_list[[response_name]][["bestNormalize"]] <-  transformed_var

          # Save the transformed data
          data_complete[[response_name]] <- transformed_var$transformed_data  # Update the response variable in the data

          # Print the ouput of f_bestNormalize
          cat("Note: the Shapiro-Wilk test below is performed on the response data, not the residuals.")
          cat(transformed_var$rmd)
          cat("    \n    \n")
        }

        if (transformation != FALSE) {
          # Perform ANOVA on transformed data
          aov_test_transformed <- do.call(
            stats::aov,
            c(list(formula = current_formula, data = data_complete,
                   weights = mf_weights), aov_extra)
          )
          transformed_aov_res  <- residuals(aov_test_transformed)
          # Same SS type as the untransformed model (see comment above).
          aov_summary_transformed <- car::Anova(aov_test_transformed,
                                                type = anova_type)

          # Perform Shapiro-Wilk test on transformed aov residuals
          shapiro_res_transformed <- safe_shapiro(transformed_aov_res)

          # Perform Anderson-Darling normality test on transformed aov residuals
          # ad.test() requires n >= 8; skip gracefully for very small samples
          if (length(transformed_aov_res) >= 8) {
            adt_res_transformed <- nortest::ad.test(transformed_aov_res)
          } else {
            adt_res_transformed <- list(statistic = NA_real_, p.value = NA_real_,
                                        method = "Anderson-Darling test (skipped: n < 8)")
          }

          # Perform Levene's test on transformed aov residuals
          levene_res_transformed <- rstatix::levene_test(
            as.formula(
              paste0("transformed_aov_res~interaction(",
                     paste(predictor_names, collapse = ", "),
                     ")")), data = data_complete) #residuals(aov_test)~predictor_names

          is_levene_trans_sig <- !is.na(levene_res_transformed$p) & levene_res_transformed$p < alpha

          cat("
   \n## Normality and homoscedasticity of **TRANSFORMED residuals** of : ",
              response_name,
              "   \n  \n"
          )

          # Generate text for interpretation of Levene's output
          if (is.na(levene_res_transformed$p)) {
            levene_res_intp_text <- "Levene's Test could not be calculated on transformed residuals (likely due to saturation or insufficient replication per group).  \n"
          } else if (levene_res_transformed$p > alpha) {
            levene_res_intp_text <- paste("According to 'Levene's Test' (",
                                          round(levene_res_transformed$p, digits = 4),
                                          " > ",
                                          alpha,
                                          ") residuals do not depart from **equal variance** (homoscedasticity)."
            )
          } else if (is_levene_trans_sig) {
            levene_res_intp_text <- paste("According to 'Levene's Test' (",
                                          round(levene_res_transformed$p, digits = 4),
                                          " \u2264 ",
                                          alpha,
                                          ") transformed residuals do **NOT** have equal variance (Heteroskedasticity)."
            )
          }
          # levene_output_transformed
          cat(
            "**Levene's test** for homogeneity of transformed residuals: F-Statistic =",
            round(levene_res_transformed$statistic, digits = 4),
            "p-value =",
            round(levene_res_transformed$p, digits = 4),
            ". ", levene_res_intp_text
          )
          if (!is.null(levene_size_warning)) cat(levene_size_warning)


          # Generate text for interpretation of shapiro output shapiro_res_intp_text
          if (is.na(shapiro_res_transformed$p.value)) {
            shapiro_res_intp_text <- paste0(
              "Shapiro-Wilk was skipped (", shapiro_res_transformed$method,
              "). Normality decision is based on the Anderson-Darling test below.  \n  \n&nbsp;  \n"
            )
          } else if (shapiro_res_transformed$p.value > alpha) {
            shapiro_res_intp_text <- paste(
              "According to the 'Shapiro-Wilk' test (",
              round(shapiro_res_transformed$p.value, digits = 4),
              " > ",
              alpha,
              ") no significant departure from **normality** was detected
          for the model residuals; check Q-Q plot.  \n  \n&nbsp;  \n"
            )
          } else if (shapiro_res_transformed$p.value <= alpha) {
            shapiro_res_intp_text <- paste(
              "According to the 'Shapiro-Wilk' test (",
              round(shapiro_res_transformed$p.value, digits = 4),
              " \u2264 ",
              alpha,
              ") transformed residuals are **NOT** normally distributed.  \n  \n&nbsp;  \n"
            )
          }


          # shapiro_output_transformed
          cat(
            "**Shapiro-Wilk Test** for Normality of transformed residuals: W =",
            round(shapiro_res_transformed$statistic, digits = 4),
            "p-value =",
            round(shapiro_res_transformed$p.value, digits = 4),
            ". ", shapiro_res_intp_text
          )

          # Generate text for interpretation of Anderson-Darling test output
          if (is.na(adt_res_transformed$p.value)) {
            adt_res_intp_text <- paste0(
              "      Anderson-Darling test skipped (n < 8, minimum sample size not met).  \n  \n"
            )
          } else if (as.numeric(adt_res_transformed$p.value) > alpha) {
            adt_res_intp_text <- paste(
              "According to 'Anderson-Darling test'  (",
              round(adt_res_transformed$p.value, digits = 4),
              " > ",
              alpha,
              ") no significant departure from **normality** was detected
          for the model residuals; check Q-Q plot.  \n  \n"
            )
          } else if (as.numeric(adt_res_transformed$p.value) <= alpha) {
            adt_res_intp_text <- paste(
              "According to 'Anderson-Darling test' (",
              round(adt_res_transformed$p.value, digits = 4),
              " \u2264 ",
              alpha,
              ") transformed residuals are **NOT** normally distributed.  \n  \n"
            )
          }

          # adt_output_transformed
          cat(
            adt_res_transformed$method,
            ": A =",
            round(adt_res_transformed$statistic, digits = 4),
            " p =",
            round(adt_res_transformed$p.value, digits = 4),
            ". ", adt_res_intp_text
          )

          # Store output in output_list
          output_list[[response_name]][["transformed_aov_test"]]     <- aov_test_transformed
          output_list[[response_name]][["transformed_shapiro_test"]] <- shapiro_res_transformed
          output_list[[response_name]][["transformed_adt_test"]]     <- adt_res_transformed
          output_list[[response_name]][["transformed_levene_test"]]  <- levene_res_transformed

          temp_file <- tempfile(fileext = ".png")
          png(temp_file, width = 7.8, height = 7.8, units = "in", res = 300)
          # Set up a 2x2 plotting layout with adjusted margins mar = c(bottom, left, top, right)
          par(mfrow = c(2, 2), mar = c(4, 4, 4, 1))  # Adjust left margin

          # Residual plot aov
          plot(aov_test_transformed, 1, main = "Residuals transformed data")

          # Boxplot
          boxplot(residuals(aov_test_transformed), main = "Boxplot", xlab = "Residuals transformed data")

          # Histogram
          f_hist(residuals(aov_test_transformed),  xlab = "Residuals transformed data")

          # QQPlot
          f_qqnorm(residuals(aov_test_transformed))

          # Close the png file
          dev.off()

          output_list[[response_name]][["transformed_normality_plots"]] <- temp_file

          if(diagnostic_plots == TRUE){
            cat("  \nCheck the **residual** plots of the **transformed data** in the figure below to assess normality.  \n")
            # Include the saved plots in R Markdown
            cat(paste0("![](", temp_file, ")"), "   \n  \n")
          }



          if(output_type != "rmd"){
            # Pagebreak
            cat("
<div style=\"page-break-after: always;\"></div>
\\newpage
        ")
          }
        }
      }

      # Keep the transformation only if it improved the check that triggered
      # it (helper_transform_decision.R); otherwise report the untransformed
      # analysis and say so.
      transformation_label_txt <- NULL
      if (!is.null(aov_summary_transformed)) {
        is_bn  <- identical(transformation, "bestnormalize")
        t_lab  <- transformation_label(if (is_bn) "bestnormalize" else "boxcox",
                                       lambda  = if (!is_bn) transformed_var$lambda else NULL,
                                       bn_name = if (is_bn) transformed_var$transformation_name else NULL)
        nb <- norm_check_p(shapiro_res, adt_res)
        na <- norm_check_p(shapiro_res_transformed, adt_res_transformed)
        dec <- keep_transformation(
          trigger_normality = trigger_normality,
          trigger_variance  = trigger_levene,
          trigger_forced    = trigger_forced,
          norm_before = nb$p, norm_after = na$p, norm_test = nb$test,
          var_before = levene_res$p, var_after = levene_res_transformed$p,
          var_test = "Levene's test",
          method_label = if (is_bn) "bestNormalize" else "Box-Cox")
        if (!isTRUE(dec$keep)) {
          cat(transformation_rejected_note(dec$reason))
          output_list[[response_name]][["transformation_rejected"]] <- list(
            method = t_lab, reason = dec$reason,
            normality_p_before = nb$p, normality_p_after = na$p,
            variance_p_before = levene_res$p,
            variance_p_after = levene_res_transformed$p)
          for (nm in c("boxcox", "bestNormalize", "transformed_aov_test",
                       "transformed_shapiro_test", "transformed_adt_test",
                       "transformed_levene_test", "transformed_normality_plots"))
            output_list[[response_name]][[nm]] <- NULL
          data_complete[[response_name]] <- original_response
          aov_summary_transformed <- NULL
        } else {
          transformation_label_txt <- t_lab
        }
      }

      if(!is.null(aov_summary_transformed)){
        #Overwrite earlier object with transformed output
        aov_test_out <- aov_test_transformed
        aov_summary  <- aov_summary_transformed
        # Stored separately: $aov_summary keeps the untransformed table, and
        # print() must show this one under its "TRANSFORMED" heading.
        output_list[[response_name]][["transformed_aov_summary"]] <- aov_summary_transformed
        Response_Transformed <- TRUE
        output_list[[response_name]][["Response_Transformed"]] <- Response_Transformed
      } else {
        aov_test_out <- aov_test
        Response_Transformed <- FALSE
        output_list[[response_name]][["Response_Transformed"]] <- Response_Transformed
        # Always present, NULL when no transformation was applied, so that
        # is.null(out$y$transformed_aov_summary) is a reliable check.
        output_list[[response_name]]["transformed_aov_summary"] <- list(NULL)
      }
      output_list[[response_name]][["transformation_label"]] <- transformation_label_txt
      output_list[[response_name]][["anova_heading"]] <-
        scale_heading("ANOVA", response_name, transformation_label_txt)

      # Store a back-transform closure so plot.f_aov can convert emmip output
      # (which is always on the transformed scale) back to the original scale.
      # If no transformation was applied the closure is the identity function.
      if (Response_Transformed) {
        if (transformation == "boxcox" || isTRUE(transformation)) {
          lambda_val <- transformed_var$lambda
          output_list[[response_name]][["back_transform_fn"]] <- local({
            lam <- lambda_val
            function(x) {
              if (abs(lam) < 1e-6) exp(x)
              else (lam * x + 1)^(1 / lam)
            }
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


      # Create data summary table for output and store in output_list
      data_summary_table <- f_summary(data,
                                      response_name,
                                      predictor_names,
                                      show_name = FALSE,
                                      digits = NULL
      )$output_df


      # Extract p-values for the model terms (drop the NA residual row).
      # aov_summary is now a car::Anova() data.frame (class c("anova",
      # "data.frame")) with columns Sum Sq | Df | F value | Pr(>F) and a
      # final "Residuals" row whose Pr(>F) is NA. No more [[1]] indexing
      # (that was needed for the summary.aov() list structure).
      p_values     <- aov_summary[["Pr(>F)"]]
      # car::Anova does not pad rownames with trailing spaces (summary.aov
      # used to, e.g. "drug:dose "). trimws() is kept defensively so the
      # downstream matching against predictor_names is robust regardless
      # of which path produced aov_summary in the future.
      term_names   <- trimws(rownames(aov_summary))
      # Type III tables include an "(Intercept)" row that we must drop:
      # it has a non-NA p-value (so the NA filter below would not catch
      # it), it does not correspond to any predictor, and treating it as
      # a main effect would pollute `overall_p_value`, `sig_main_effects`,
      # and downstream emmeans calls. Type II tables have no such row, so
      # this filter is a no-op there.
      intercept_idx <- term_names == "(Intercept)"
      if (any(intercept_idx)) {
        p_values   <- p_values[!intercept_idx]
        term_names <- term_names[!intercept_idx]
      }
      # Remove the residuals row (NA p-value) for the significance check
      non_resid    <- !is.na(p_values)
      p_values_sig <- p_values[non_resid]
      term_names_sig <- term_names[non_resid]
      # Overall significance: any term significant (used to suppress "ns" letters)
      overall_p_value <- min(p_values_sig, na.rm = TRUE)

      # Classify model terms into interactions and main effects
      interaction_terms <- term_names_sig[grepl(":", term_names_sig)]
      main_effect_terms <- term_names_sig[!grepl(":", term_names_sig)]

      sig_interactions  <- if (length(interaction_terms) > 0)
        interaction_terms[p_values_sig[match(interaction_terms, term_names_sig)] < alpha]
      else character(0)

      # Interaction terms present in the model but NOT significant. Used to
      # suggest refitting without the interaction term so the main effects
      # are estimated with more power (when the design and research question
      # allow it).
      ns_interactions   <- if (length(interaction_terms) > 0)
        interaction_terms[p_values_sig[match(interaction_terms, term_names_sig)] >= alpha]
      else character(0)

      sig_main_effects  <- main_effect_terms[
        p_values_sig[match(main_effect_terms, term_names_sig)] < alpha]

      ns_main_effects   <- main_effect_terms[
        p_values_sig[match(main_effect_terms, term_names_sig)] >= alpha]

      # Calculate the confidence level that matches alpha
      conf_level <- 1 - alpha
      conf_perc  <- conf_level * 100

      # ----------------------------------------------------------------
      # Post hoc tables, decided per term (see helper_post_hoc.R).
      #   * every significant categorical interaction: one cell-means table
      #   * every categorical main effect: its own marginal-means table
      #     (emmeans ~ factor); "ns" letters when the term is not
      #     significant; a caution when it takes part in a significant
      #     interaction
      #   * numeric covariates (ANCOVA): no table
      #   * post_hoc = "a" / "a:b" / by = "c": only the requested tables
      # Letters and the p-value adjustment are computed WITHIN each table.
      # ----------------------------------------------------------------
      cat_sig_ints <- sig_interactions[vapply(sig_interactions, function(it)
        all(ph_parts(it) %in% cat_predictors), logical(1))]
      ph_mains <- ph_spec$mains
      ph_ints  <- if (identical(ph_spec$mode, "default")) cat_sig_ints else ph_spec$ints
      ph_by    <- ph_spec$by

      emm_is_cells   <- length(cat_sig_ints) > 0
      emm_type_label <- if (emm_is_cells) "Cell Means" else "Marginal Means"

      p_of <- function(trm) {
        p <- p_values_sig[match(trm, term_names_sig)]
        if (length(p) == 0L) NA_real_ else p
      }
      posthoc_results <- build_post_hoc_tables(
        model = aov_test_out, mains = ph_mains, ints = ph_ints, by = ph_by,
        sig_interactions = sig_interactions, cat_sig_ints = cat_sig_ints,
        p_of = p_of, term_names = term_names_sig, adjust = adjust,
        alpha = alpha, conf_level = conf_level,
        bt_fn = output_list[[response_name]][["back_transform_fn"]],
        transformed = Response_Transformed, data_complete = data_complete,
        fun_name = "f_aov")
      output_list[[response_name]][["post_hoc"]] <- posthoc_results

      post_hoc_intro_note <- ""
      if (length(cat_sig_ints) > 0 && identical(ph_spec$mode, "default")) {
        post_hoc_intro_note <- paste0(
          "\n   \n**NOTE: Significant interaction(s) detected: ",
          paste(cat_sig_ints, collapse = ", "), "**  \n",
          "The cell-means table compares every combination of ",
          paste(unique(unlist(lapply(cat_sig_ints, ph_parts))), collapse = " \u00d7 "),
          " and is the reference grouping for this response. The main-effect ",
          "tables after it average over the interacting factor(s); read them ",
          "with the caution printed above each one.  \n\n")
      } else if (length(ns_interactions) > 0 && identical(ph_spec$mode, "default")) {
        post_hoc_intro_note <- paste0(
          "\n   \n**NOTE:** The interaction term(s) ",
          paste(ns_interactions, collapse = ", "),
          " were **not significant** (p \u2265 ", alpha, ").  \n",
          "Consider refitting the model without the interaction term if the ",
          "research question and research setup allow this; the main effects ",
          "are then estimated with more power.  \n\n")
      }

      # Full-grid emmeans (all predictors), kept for the plots: every plot
      # recomputes the means on exactly the grid it shows from this object.
      emm <- emmeans::emmeans(aov_test_out,
                              specs = predictor_names,
                              level = conf_level)

      # Store objects needed for plot.f_aov interaction/means plot
      output_list[[response_name]][["emm"]]               <- emm
      output_list[[response_name]][["predictor_names"]]   <- predictor_names
      output_list[[response_name]][["data_complete"]]     <- data_complete
      output_list[[response_name]][["original_response"]] <- original_response
      output_list[[response_name]][["emm_is_cells"]]      <- emm_is_cells
      # Categorical predictors only (factor / character / logical). Continuous
      # covariates (ANCOVA) are held at their mean and cannot carry CLD letters,
      # so they are excluded from effect/interaction plotting just as in f_glm().
      cat_preds_out <- predictor_names[vapply(
        data_complete[predictor_names],
        function(v) is.factor(v) || is.character(v) || is.logical(v),
        logical(1)
      )]
      output_list[[response_name]][["cat_preds"]]         <- cat_preds_out
      # Significant categorical interaction terms (e.g. "a:b"). Used to decide
      # whether to draw interaction plots instead of single-factor means plots.
      output_list[[response_name]][["sig_interactions"]]  <- sig_interactions


      # ---- Deprecated (kept for one release, see ?f_aov) ------------------
      # post_hoc_summary_table / summary_table / cld_text used to hold the
      # single post hoc table. They now hold the cell-means table when a
      # significant interaction is tabulated, otherwise the first table.
      dep_key <- if (length(ph_ints)) ph_ints[1] else
        if (length(posthoc_results)) names(posthoc_results)[1] else NULL
      if (!is.null(dep_key) && dep_key %in% names(posthoc_results)) {
        summary_table          <- posthoc_results[[dep_key]]$cld
        post_hoc_summary_table <- attr(posthoc_results[[dep_key]], "display")
        cld_text               <- posthoc_results[[dep_key]]$cld_text
      } else {
        summary_table <- NULL
        post_hoc_summary_table <- NULL
        cld_text <- ""
      }
      output_list[[response_name]][["summary_table"]]          <- summary_table
      output_list[[response_name]][["post_hoc_summary_table"]] <- post_hoc_summary_table

      cat(paste("\n## Observed Descriptives Table of: ", deparse(current_formula), "  \n"))
      f_pander(f_conditional_round(data_summary_table, digits = 3, replace_na = FALSE))
      # Add footnote
      cat(
        "**TIP:** These values represent your actual observed sample characteristics.
Use this table for *Methods* sections or Supplementary materials (to describe the sample).\n  \n
**CAUTION:** For statistical inference (significance letters and *p-values*) and reporting
main findings in the *Results* section, you **must** use the Emmeans table below.\n"
      )

      # Show WARNING when assumptions of aov() are violated
      if (Response_Transformed == TRUE) {

        # Build diagnosis of what is still wrong after transformation.
        # For the normality check, prefer Shapiro-Wilk where available and
        # fall back to Anderson-Darling when Shapiro was skipped (n > 5000),
        # so the diagnosis does not silently pass for very large samples.
        normality_still_bad <- if (!is.na(shapiro_res_transformed$p.value)) {
          shapiro_res_transformed$p.value <= alpha
        } else if (!is.na(adt_res_transformed$p.value)) {
          adt_res_transformed$p.value <= alpha
        } else {
          FALSE
        }

        still_wrong <- c(
          if (is_levene_trans_sig) "heteroscedasticity (Levene's p < \u03b1)" else NULL,
          if (isTRUE(normality_still_bad)) {
            if (!is.na(shapiro_res_transformed$p.value))
              "non-normality (Shapiro-Wilk p \u2264 \u03b1)"
            else
              "non-normality (Anderson-Darling p \u2264 \u03b1)"
          } else NULL
        )

        if (length(still_wrong) > 0) {
          still_wrong_str <- paste(still_wrong, collapse = " and ")

          cat(paste0(
            "\n\n***\n\n",
            "**WARNING: \nTransformation did not resolve ", still_wrong_str, "**  \n\n",
            "A ", if(transformation == TRUE) "boxcox", " transformation was applied but **",
            still_wrong_str, "** persists in the transformed residuals for `",
            response_name, "`.  \n",
            "The ANOVA results below are **unreliable**. Heteroscedasticity inflates the ",
            "Type I error rate, meaning significant results may be false positives and ",
            "F-statistics are biased. Consider one of these **alternative tests** before ",
            "drawing conclusions:  \n",
            "\n-  **Kruskal-Wallis test:** non-parametric, no normality or equal-variance ",
            "assumption. Tests stochastic dominance between groups. Follow up with Dunn's test ",
            "(`rfriend::f_kruskal_test()`).",
            "\n-  **Generalised Linear Model (GLM):** if the data follow a specific non-normal ",
            "distribution (e.g. counts \u2192 Poisson, positive skewed \u2192 Gamma) modelling ",
            "the correct error distribution via `rfriend::f_glm()` is more principled than ",
            "transforming.",
            "\n-  **Aligned Rank Transform (ART) ANOVA:** extends rank-based testing to ",
            "factorial designs including interactions. Available via `ARTool::art()`. ",
            "Recommended for multi-factor designs. \n",
            "\n***\n\n"
          ))

          # Store flag for print.f_aov
          output_list[[response_name]][["last_resort_triggered"]] <- TRUE
          output_list[[response_name]][["last_resort_reason"]]    <- still_wrong_str
          output_list[[response_name]][["cld_text"]]              <- cld_text
        }

      } else {
        # Untransformed: simpler warnings pointing to transformation
        if (is_levene_sig) {
          cat(paste0(
            "\n   \n**WARNING**  \n",
            "*Levene's Test (", round(levene_res$p, digits = 4), " < ", alpha, "): ",
            "residuals do NOT have equal variance (heteroscedasticity).  \n",
            "ANOVA results can be misleading. ",
            if (!is.null(output_list[[response_name]][["transformation_rejected"]]))
              "A transformation was tried but not kept (see above); resort to other statistical tests.*"
            else "Enable the transformation option (`transformation = TRUE`) or resort to other statistical tests.*"
          ))
        }
        if (isTRUE(shapiro_res$p.value <= alpha)) {
          cat(paste0(
            "\n   \n**WARNING**  \n",
            "*Shapiro-Wilk Test (", round(shapiro_res$p.value, digits = 4),
            " \u2264 ", alpha, "): residuals are NOT normally distributed.  \n",
            "ANOVA results can be misleading. ",
            if (!is.null(output_list[[response_name]][["transformation_rejected"]]))
              "A transformation was tried but not kept (see above); resort to other statistical tests.*"
            else "Enable the transformation option (`transformation = TRUE`) or resort to other statistical tests.*"
          ))
        }
      }

      if(n_before != n_after) {
        cat("\n   \n**WARNING**   \n *Removed ", n_before - n_after, " rows (",
            round((n_before - n_after)/n_before * 100, 1),
            "%) with missing values in ", response_name,".*", sep = "")
      }


      cat("\n   \n## ", output_list[[response_name]][["anova_heading"]], "  \n", sep = "")

      cat("&nbsp;\n  \n")
      cat(paste("\n**Table** of aov call: ", deparse(current_formula), "  \n"))
      type_roman <- as.character(as.roman(anova_type))
      if (anova_type == 2) {
        cat("Sums of Squares are computed with **Type II** via `car::Anova()`, ",
            "which is order-invariant for the main effects in unbalanced ",
            "designs and consistent with the model-based `emmeans` post hoc ",
            "tests reported below.  \n", sep = "")
      } else {
        cat("Sums of Squares are computed with **Type III** via `car::Anova()`. ",
            "Sum / effect contrasts (`contr.sum`, `contr.poly`) have been ",
            "installed for the duration of this call so that the main-effect ",
            "rows test effects *averaged* across the levels of the other ",
            "factors -- rather than effects at an arbitrary reference level, ",
            "which is what Type III would otherwise report under R's default ",
            "treatment contrasts.  \n", sep = "")
      }
      cat(rmd_anova_summary(aov_test_out, type = anova_type,
                             alpha = alpha))
      cat("&nbsp;\n  \n")

      # Repeat banner when force_aov overrode the n=1 skip
      if (isTRUE(output_list[[response_name]][["force_aov_used"]])) {
        cat(paste0(
          "\n\n***\n\n",
          "**[!] Diagnostic output only, do not report these results.**  \n",
          "The model above is saturated (residual df = 0). ",
          "F-statistics and p-values are undefined.  \n",
          "\n\n***\n\n"
        ))
      }


      if (length(posthoc_results) > 0) {
        # Emit the deferred post-hoc intro note right here, so it sits
        # directly above the post hoc tables it describes.
        cat(post_hoc_intro_note)

        cat(paste0("  \n## Post Hoc Tests of ", response_name, "  \n"))
        cat(paste0(
          "\nEstimated marginal means (emmeans) are model-based means of ",
          response_name, ". Each table below answers one question and has its ",
          "own letters, adjustment and footer: a **cell-means** table compares ",
          "every combination of an interaction, a **marginal-means** table ",
          "compares the levels of one factor averaged over the others, and a ",
          "**simple-effects** table compares one factor within each level of ",
          "another. Marginal means are equal-weighted emmeans: every level of ",
          "the other factors counts equally, so in unbalanced designs they can ",
          "differ from the raw group means in the Observed Descriptives Table. ",
          "The $n$ column is the raw observed count.  \n",
          "\n*Reporting Tips:* Report the table that matches your question. ",
          "When an interaction is significant, report its cell means (with ",
          conf_perc, "% CIs) and describe how the pattern changes; main-effect ",
          "letters average over that pattern.  \n\n"))

        for (k in names(posthoc_results)) {
          ph_k   <- posthoc_results[[k]]
          kind_k <- attr(ph_k, "kind")
          kind_lbl <- switch(kind_k,
                             cells    = "cell means",
                             marginal = "marginal means",
                             simple   = "simple effects")
          by_k <- if (identical(kind_k, "simple")) trimws(sub(".*\\|", "", k)) else NULL
          cat(paste0("\n### Post hoc: ", k, " (", kind_lbl, ")",
                     if (isTRUE(attr(ph_k, "term_ns"))) "  (not significant)" else "",
                     "  \n\n"))
          cat(paste0("*", ph_caption(by_k), "*  \n\n"))
          if (isTRUE(attr(ph_k, "term_ns")))
            cat("Term not significant at \u03b1 = ", alpha,
                ": letters collapsed to `ns`.  \n\n", sep = "")
          if (!is.null(ph_k$caution)) cat(ph_k$caution, "  \n\n", sep = "")
          disp <- attr(ph_k, "display")
          if (!is.null(disp)) {
            f_pander(f_conditional_round(disp, digits = 3, replace_na = FALSE))
          } else {
            cat("*Letters could not be computed for this table.*  \n\n")
          }
          cat(ph_k$cld_text)
        }

        # Add footnote specifically explaining back-transformed values and median interpretation
        if(Response_Transformed == TRUE){
          cat(paste0("
\n**Note on back-transformation:** The column **'median (BT)'** contains back-transformed
estimated marginal means. Back-transforming a mean from a transformed scale returns the
**median** of the original-scale distribution, not the arithmetic mean. This follows from
the property that medians, unlike means, are equivariant under monotone transformations
($g(\\text{median}(X)) = \\text{median}(g(X))$, but $g(\\bar{X}) \\neq \\overline{g(X)}$ in general).
For reporting purposes, describe these values as **back-transformed medians** (or geometric
means in the specific case of a log transformation), not as means.
\n**Note:** Standard Errors (SE) are omitted for back-transformed data. Transformations are
non-linear, which results in asymmetric uncertainty margins on the original scale (the
distance from the median to the lower bound differs from that to the upper bound).
A single SE value implies symmetry ($\\pm$) and is therefore misleading.
Please rely on the **",conf_perc,"% Confidence Intervals (lower.CL and upper.CL)** to
quantify uncertainty. Pairwise contrasts (stored in `$post_hoc[[term]]$pairs`) are on the
transformed scale.
"))
        }
      }

      # -- Estimated means / interaction plot in document output ----------
      # Publication-ready ggplot2 figures (shared theme f_theme_pub() and
      # palette f_pub_palette()). A single categorical predictor gets a
      # means plot; a significant categorical interaction gets interaction
      # plots (2-/3-/4-way, with facet panels beyond two factors), matching
      # f_glm(). Plots are stored as ggplot objects in output_list and the
      # explanatory caption is emitted as markdown text below each figure.
      if (isTRUE(effect_plots)) {
        bt_fn   <- output_list[[response_name]][["back_transform_fn"]]
        if (is.null(bt_fn)) bt_fn <- identity
        emm_obj <- output_list[[response_name]][["emm"]]
        raw_y_orig_doc <- original_response
        cat_preds_pl   <- cat_preds_out
        sig_ints_pl    <- sig_interactions
        ph_tab_doc     <- output_list[[response_name]][["post_hoc_summary_table"]]

        y_label_doc <- if (isTRUE(Response_Transformed))
          paste0(response_name, " (back-transformed median)") else response_name

        # Shared caption for every figure: how to read the points, estimates,
        # and grouping letters. Kept identical in spirit to f_glm()'s eff_note.
        # Split into two parts so the letter explanation can be dropped on
        # figures where the letters were withheld (a banner is shown instead):
        #   eff_note_est  -> points / estimates / CI (always relevant)
        #   eff_note_let  -> how to read the grouping letters (only when shown)
        eff_note_est <- paste0(
          if (isTRUE(Response_Transformed))
            "Points are raw data; estimates are back-transformed medians with "
          else
            "Points are (jittered) raw data; estimates are model estimated marginal means with ",
          paste0(100 * (1 - alpha), "% CI"), "."
        )
        eff_note_let <- paste0(
          " Groups sharing a letter are not significantly different ",
          "(\u03b1 = ", alpha, "); groups with different letters are ",
          "significantly different. Sharing a letter indicates insufficient ",
          "evidence of a difference, not proof that the groups are identical."
        )
        # Assemble the single-piece figure caption in reading order:
        #   1. data / estimates / CI            (eff_note_est, always)
        #   2. the letter information together   (eff_note_let + letter_extra),
        #      OR the "Letters omitted" notice   (banner), OR nothing when the
        #      figure shows no letters at all (e.g. an overall non-significant
        #      model). letter_extra carries the cross-panel reading note, which
        #      is itself about letters, so it sits with the other letter text.
        #   3. the interaction / line note "rest" (int_note: not-parallel,
        #      panels, dotted line), kept together as one block.
        # Exactly one short lead label is shown in bold: "Letters omitted:"
        # when the letters were withheld, otherwise the interaction lead
        # ("Significant interaction:" / "No significant interaction:"). The
        # whole caption is one italic piece. `banner` is NULL when letters are
        # shown; `int_note` is NULL for single-factor means plots; has_letters
        # is FALSE when the figure carries no letters and no omission notice.
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

        # ---- Per-plot compact letters (option a: suppress across interaction)
        # The compact letters on a figure must answer the same question the
        # figure asks. An effect / interaction plot shows the means of the
        # factors in `display_factors` and AVERAGES over every other
        # categorical predictor. Letters are therefore only honest when BOTH:
        #   (1) they are computed on exactly that displayed grid, so each
        #       letter and its plotted point refer to the same marginal mean
        #       (this also removes the first-match-wins ambiguity that arises
        #       when letters are copied from the full crossed cell-means table
        #       by a partial key), AND
        #   (2) the averaging does not cross a significant interaction, i.e. no
        #       significant interaction term has one factor inside
        #       `display_factors` and another outside it. If it does, the
        #       marginal mean collapses a pattern that genuinely changes with a
        #       hidden factor, so grouped letters on the collapsed view can
        #       mislead.
        # When (2) fails the means are still drawn (they remain informative as
        # a visual) but the letters are withheld and a visible banner explains
        # why and points to the full cell-means post hoc table. This disclosed
        # "degraded mode" is preferred over printing letters that would quietly
        # mislead. Returns a list(safe, letters_df, banner): `letters_df` has
        # the display-factor column(s) plus a `Letters` column when letters are
        # shown, otherwise NULL; `banner` is the explanatory note (or NULL).
        aov_plot_letters <- function(display_factors) {
          D <- display_factors

          # (2) Does any significant interaction straddle D / not-D?
          crossing <- character(0)
          for (it in sig_ints_pl) {
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

          # Letters come from the post hoc table that answers the same
          # question as the figure: the factor's own marginal-means table, or
          # the interaction's cell-means table. A non-significant term's table
          # holds "ns", which draws no letters.
          own_key <- NULL
          if (length(D) == 1L && D %in% names(posthoc_results)) {
            own_key <- D
          } else if (length(D) > 1L) {
            hit <- names(posthoc_results)[vapply(names(posthoc_results), function(k)
              identical(attr(posthoc_results[[k]], "kind"), "cells") &&
                setequal(ph_parts(k), D), logical(1))]
            if (length(hit)) own_key <- hit[1]
          }
          if (!is.null(own_key)) {
            cl <- posthoc_results[[own_key]]$cld
            if (is.null(cl) || !all(c(D, "Letter") %in% names(cl)))
              return(list(safe = TRUE, letters_df = NULL, banner = NULL))
            ld <- cl[, c(D, "Letter"), drop = FALSE]
            names(ld)[names(ld) == "Letter"] <- "Letters"
            for (f in D) ld[[f]] <- as.character(ld[[f]])
            ld$Letters <- trimws(ld$Letters)
            if (all(ld$Letters == "ns"))
              return(list(safe = TRUE, letters_df = NULL, banner = NULL))
            return(list(safe = TRUE, letters_df = ld, banner = NULL))
          }

          # Overall model not significant: the table already shows "ns" and no
          # pairwise letters should be claimed. Draw points without letters.
          if (overall_p_value >= alpha)
            return(list(safe = TRUE, letters_df = NULL, banner = NULL))

          # (1) Recompute emmeans on EXACTLY the displayed grid, then derive the
          # letters from that grid so points and letters match one-to-one.
          letters_df <- tryCatch({
            emm_D <- emmeans::emmeans(emm_obj, specs = D, level = 1 - alpha)
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
            warning("f_aov: could not compute per-plot letters for '",
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

        # ---- Single categorical-effect means plot --------------------------
        # One estimate per level of var_name (estimate +/- CI), jittered raw
        # data, and CLD letters taken from the post hoc summary table. Each
        # level gets its own colour from the publication palette.
        make_aov_means_plot <- function(var_name) {
          # The shared emmeans grid (and the post hoc table derived from it)
          # spans ALL predictors, so it can contain several rows per level of
          # `var_name` (one per level of every other factor). Plotting that
          # grid directly stacks multiple points/error bars/letters on the
          # same x position (the overlap problem). To draw a clean
          # one-estimate-per-level marginal-means plot, recompute emmeans for
          # `var_name` alone and back-transform if needed; fall back to the
          # post hoc table only if that fails.
          emm_one <- tryCatch(
            as.data.frame(emmeans::emmeans(
              emm_obj, specs = var_name, level = 1 - alpha)),
            error = function(e) NULL
          )

          plot_df <- NULL
          if (!is.null(emm_one) && var_name %in% names(emm_one)) {
            ci_lo1 <- intersect(c("lower.CL", "asymp.LCL", "LCL"),
                                names(emm_one))[1]
            ci_hi1 <- intersect(c("upper.CL", "asymp.UCL", "UCL"),
                                names(emm_one))[1]
            if ("emmean" %in% names(emm_one) &&
                !is.na(ci_lo1) && !is.na(ci_hi1)) {
              # Recomputed emmeans is on the model (possibly transformed)
              # scale; back-transform to the original scale so the plot
              # matches the post hoc table and the raw-data overlay.
              plot_df <- data.frame(
                x_grp  = as.character(emm_one[[var_name]]),
                centre = bt_fn(emm_one[["emmean"]]),
                lower  = bt_fn(emm_one[[ci_lo1]]),
                upper  = bt_fn(emm_one[[ci_hi1]]),
                stringsAsFactors = FALSE
              )
            }
          }

          # Fallback: derive from the shared post hoc table.
          if (is.null(plot_df)) {
            ph <- ph_tab_doc
            if (is.null(ph) || !var_name %in% names(ph)) return(NULL)
            centre_col <- intersect(c("median (BT)", "emmean..", "emmean"),
                                    names(ph))[1]
            ci_lo <- intersect(c("lower.CL", "asymp.LCL", "LCL"), names(ph))[1]
            ci_hi <- intersect(c("upper.CL", "asymp.UCL", "UCL"), names(ph))[1]
            if (is.na(centre_col) || is.na(ci_lo) || is.na(ci_hi))
              return(NULL)
            plot_df <- data.frame(
              x_grp  = as.character(ph[[var_name]]),
              centre = ph[[centre_col]],
              lower  = ph[[ci_lo]],
              upper  = ph[[ci_hi]],
              stringsAsFactors = FALSE
            )
          }

          # Safety net: if duplicates per level remain (shared-grid fallback),
          # average them so each level shows a single estimate + error bar.
          if (anyDuplicated(plot_df$x_grp)) {
            plot_df <- do.call(rbind, lapply(
              split(plot_df, plot_df$x_grp), function(d) data.frame(
                x_grp  = d$x_grp[1],
                centre = mean(d$centre, na.rm = TRUE),
                lower  = mean(d$lower,  na.rm = TRUE),
                upper  = mean(d$upper,  na.rm = TRUE),
                stringsAsFactors = FALSE)))
          }

          # CLD letters computed on EXACTLY this single-factor grid (see
          # aov_plot_letters): they match the plotted marginal means one-to-one
          # and are withheld (with a banner) when averaging over another factor
          # would cross a significant interaction.
          letter_banner <- NULL
          plot_df$Letters <- NA_character_
          lres <- aov_plot_letters(var_name)
          letter_banner <- lres$banner
          if (!is.null(lres$letters_df) && var_name %in% names(lres$letters_df)) {
            lut <- stats::setNames(lres$letters_df$Letters,
                                   lres$letters_df[[var_name]])
            plot_df$Letters <- lut[as.character(plot_df$x_grp)]
          }
          plot_df$Letters <- trimws(plot_df$Letters)
          plot_df$Letters[plot_df$Letters %in%
                            c("ns", "\u2014", "-", "NA")] <- NA_character_

          # Raw data overlay on the original (untransformed) scale.
          raw_df <- data.frame(
            x_grp = as.character(data_complete[[var_name]]),
            y_val = raw_y_orig_doc,
            stringsAsFactors = FALSE
          )
          raw_df <- raw_df[stats::complete.cases(raw_df), ]
          if (!is.numeric(raw_df$y_val) || nrow(raw_df) == 0L) raw_df <- NULL

          # Preserve factor ordering if present.
          if (is.factor(data_complete[[var_name]])) {
            lev <- levels(data_complete[[var_name]])
            plot_df$x_grp <- factor(plot_df$x_grp, levels = lev)
            if (!is.null(raw_df))
              raw_df$x_grp <- factor(raw_df$x_grp, levels = lev)
          }

          y_vals <- c(plot_df$upper, plot_df$lower,
                      if (!is.null(raw_df)) raw_df$y_val)
          y_top  <- max(y_vals, na.rm = TRUE)
          y_bot  <- min(y_vals, na.rm = TRUE)
          plot_df$y_letter <- y_top + 0.06 * (y_top - y_bot)

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
              ggplot2::aes(y = .data[["y_letter"]],
                           label = .data[["Letters"]]),
              vjust = 0, show.legend = FALSE, colour = "black",
              na.rm = TRUE
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

        # ---- Interaction plot (2-/3-/4-way categorical) --------------------
        # `factors` is a character vector: 1st = x-axis, 2nd = trace (colour),
        # any 3rd/4th = facet panels. emmip() supplies the estimated means; the
        # values are back-transformed with bt_fn so the figure is on the
        # original response scale, matching the means plot and post hoc table.
        make_aov_interaction_plot <- function(factors, is_sig,
                                              letters_override = NULL,
                                              letter_note = NULL) {
          x_var      <- factors[1]
          trace_var  <- factors[2]
          facet_vars <- if (length(factors) > 2L) factors[-(1:2)] else character(0)

          rhs <- x_var
          if (length(facet_vars) > 0L)
            rhs <- paste0(rhs, " | ", paste(facet_vars, collapse = " * "))
          emmip_form <- stats::as.formula(paste(trace_var, "~", rhs))

          ip <- tryCatch(
            emmeans::emmip(emm_obj, emmip_form, CIs = TRUE, plotit = FALSE),
            error = function(e) {
              warning("f_aov: emmip() failed for formula '",
                      deparse(emmip_form), "': ",
                      conditionMessage(e), call. = FALSE, immediate. = TRUE)
              NULL
            }
          )
          if (is.null(ip)) return(NULL)

          # Back-transform emmip estimates/CIs to the original scale.
          ip[["yvar"]] <- bt_fn(ip[["yvar"]])
          ip[["LCL"]]  <- bt_fn(ip[["LCL"]])
          ip[["UCL"]]  <- bt_fn(ip[["UCL"]])

          ip_df <- data.frame(
            x_grp = as.character(ip[[x_var]]),
            trace = as.character(ip[[trace_var]]),
            yvar  = ip[["yvar"]],
            lower = ip[["LCL"]],
            upper = ip[["UCL"]],
            stringsAsFactors = FALSE
          )
          # Keep the x and trace factors under their REAL names, so the
          # CLD-letter key join below (which keys on the factor names from the
          # post hoc table) can find them. Without this the key columns would
          # only exist as x_grp/trace and the join would silently produce no
          # letters.
          ip_df[[x_var]]     <- as.character(ip[[x_var]])
          ip_df[[trace_var]] <- as.character(ip[[trace_var]])
          # Carry facet columns through (as factors, preserving model order).
          for (fv in facet_vars) {
            fac_levels <- if (is.factor(data_complete[[fv]]))
              levels(data_complete[[fv]]) else unique(as.character(ip[[fv]]))
            ip_df[[fv]] <- factor(as.character(ip[[fv]]), levels = fac_levels)
          }

          x_levels <- if (is.factor(data_complete[[x_var]]))
            levels(data_complete[[x_var]]) else unique(ip_df$x_grp)
          tr_levels <- if (is.factor(data_complete[[trace_var]]))
            levels(data_complete[[trace_var]]) else unique(ip_df$trace)
          ip_df$x_grp <- factor(ip_df$x_grp, levels = x_levels)
          ip_df$trace <- factor(ip_df$trace, levels = tr_levels)

          # CLD letters computed on EXACTLY this displayed grid (see
          # aov_plot_letters): they match the plotted cells one-to-one and are
          # withheld (with a banner) when the view averages over a factor that
          # interacts significantly with what is shown.
          all_factors <- c(x_var, trace_var, facet_vars)
          ip_df$Letters <- NA_character_
          int_letter_banner <- NULL
          lres <- if (!is.null(letters_override))
            list(letters_df = letters_override, banner = NULL)
          else aov_plot_letters(all_factors)
          int_letter_banner <- lres$banner
          if (!is.null(lres$letters_df) &&
              all(all_factors %in% names(lres$letters_df))) {
            key_tab <- do.call(paste, c(
              lapply(all_factors, function(f)
                as.character(lres$letters_df[[f]])), list(sep = "\r")))
            lut <- stats::setNames(lres$letters_df$Letters, key_tab)
            key_ip <- do.call(paste, c(
              lapply(all_factors, function(f)
                as.character(ip_df[[f]])), list(sep = "\r")))
            ip_df$Letters <- lut[key_ip]
          }
          ip_df$Letters <- trimws(ip_df$Letters)
          ip_df$Letters[ip_df$Letters %in%
                          c("ns", "\u2014", "-", "NA")] <- NA_character_

          # Raw data overlay on the original scale.
          raw_df <- NULL
          if (all(all_factors %in% names(data_complete)) &&
              is.numeric(raw_y_orig_doc)) {
            raw_df <- data.frame(
              x_grp = factor(as.character(data_complete[[x_var]]),
                             levels = x_levels),
              trace = factor(as.character(data_complete[[trace_var]]),
                             levels = tr_levels),
              y_val = raw_y_orig_doc,
              stringsAsFactors = FALSE
            )
            for (fv in facet_vars)
              raw_df[[fv]] <- factor(as.character(data_complete[[fv]]),
                                     levels = levels(ip_df[[fv]]))
            raw_df <- raw_df[stats::complete.cases(raw_df), ]
          }

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
          # The cross-panel reading note is about letters, so it is NOT placed
          # in the interaction (line) note; it is returned separately and the
          # caption assembler groups it with the other letter text. Only set it
          # for faceted plots whose letters are actually shown.
          cross_facet_note <- if (!is.null(letter_note)) letter_note
          else if (length(facet_vars) > 0L && is.null(int_letter_banner))
            paste0("Letters can be compared across all panels: they group ",
                   "every cell shown, not only the cells within one panel.")
          else NULL
          int_caption <- paste0(
            int_caption,
            " The dotted line is merely a visual aid and has no real-life ",
            "meaning, as there is no data space between nominal categories.")

          # In-figure title. Plain ASCII "by" is used rather than a
          # multiplication-sign glyph, which some graphics devices / fonts
          # fail to render when ggsave writes the PNG.
          int_title <- paste0(
            "Estimated means of ", response_name, ": ",
            x_var, " (x-axis) by ", trace_var, " (colour)")
          if (length(facet_vars) > 0L)
            int_title <- paste0(int_title, ", faceted by ",
                                paste(facet_vars, collapse = " and "))

          y_all_int <- c(ip_df$upper, ip_df$lower,
                         if (!is.null(raw_df)) raw_df$y_val)
          y_rng_int <- max(y_all_int, na.rm = TRUE) -
            min(y_all_int, na.rm = TRUE)
          ip_df$y_letter <- ip_df$upper + 0.05 * y_rng_int

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
              title   = int_title,
              x       = x_var,
              y       = y_label_doc,
              colour  = trace_var
            ) +
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

          p <- p + ggplot2::scale_colour_manual(
            values = f_pub_palette(length(tr_levels)))
          # Carry the caption text so the caller can print it below the figure.
          attr(p, "int_caption")      <- int_caption
          attr(p, "letter_banner")    <- int_letter_banner
          attr(p, "cross_facet_note") <- cross_facet_note
          attr(p, "has_letters")      <- any(!is.na(ip_df$Letters))
          p
        }

        # Significant categorical interaction(s)? Support 2-, 3-, and 4-way
        # interactions among categorical factors. A 2-way uses x-axis + colour
        # (trace); 3- and 4-way add facet panels for the remaining factor(s).
        # Beyond 4 factors the panel grid becomes unreadable, so those are
        # skipped with a warning.
        MAX_PLOT_FACTORS <- 4L
        int_cat <- character(0)
        if (length(ph_ints) > 0) {
          for (it in ph_ints) {
            parts <- trimws(strsplit(it, ":", fixed = TRUE)[[1]])
            n_f <- length(parts)
            if (n_f >= 2L && all(parts %in% cat_preds_pl)) {
              if (n_f <= MAX_PLOT_FACTORS) {
                int_cat <- c(int_cat, it)
              } else {
                warning("f_aov: the ", n_f, "-way interaction '", it,
                        "' involves more than ", MAX_PLOT_FACTORS,
                        " categorical factors. An interaction plot would ",
                        "need ", n_f - 2L, " nested facet dimensions, which ",
                        "is not legible, so no plot is drawn for this term. ",
                        "The post hoc cell-means table still reports every ",
                        "combination; inspect it (or plot a chosen lower-",
                        "order slice) instead.",
                        call. = FALSE, immediate. = TRUE)
              }
            }
          }
        }

        if (length(int_cat) > 0) {
          for (it in int_cat) {
            parts <- trimws(strsplit(it, ":", fixed = TRUE)[[1]])
            cat(paste0("\n## Interaction Plots of: ", response_name,
                       "  (", it, ")  \n"))
            # Orientations: for a 2-way show both (x<->trace swap). For higher
            # order, rotate which factor sits on the x-axis (the next factor
            # becomes the trace/colour, the remainder become facet panels).
            orientations <- if (length(parts) == 2L)
              list(parts[c(1, 2)], parts[c(2, 1)])
            else
              lapply(seq_along(parts), function(i)
                c(parts[i], parts[-i]))
            for (pi in seq_along(orientations)) {
              ord <- orientations[[pi]]
              p_ip <- tryCatch(
                make_aov_interaction_plot(
                  ord, is_sig = isTRUE(p_of(it) < alpha)),
                error = function(e) {
                  warning("f_aov: interaction plot for '", it,
                          "' (orientation ", pi, ") could not be built: ",
                          conditionMessage(e), call. = FALSE,
                          immediate. = TRUE)
                  NULL
                }
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
                ok_ip <- tryCatch({
                  suppressMessages(
                    ggplot2::ggsave(filename = tmp_ip, plot = p_ip,
                                    width = w_in, height = h_in, units = "in",
                                    dpi = 200)
                  )
                  file.exists(tmp_ip)
                }, error = function(e) {
                  warning("f_aov: ggsave failed for interaction plot '", it,
                          "' (orientation ", pi, "): ", conditionMessage(e),
                          call. = FALSE, immediate. = TRUE)
                  FALSE
                })
                if (isTRUE(ok_ip))
                  cat(paste0("![](", tmp_ip, ")"), "   \n  \n")
                # Single italic caption, ordered: estimates/CI, then the letter
                # note (or the "Letters omitted" notice), then the interaction
                # note. Only one short lead label is bold (see make_fig_caption).
                ip_cap  <- attr(p_ip, "int_caption")
                ip_bann <- attr(p_ip, "letter_banner")
                cap <- make_fig_caption(
                  banner       = ip_bann,
                  int_note     = ip_cap,
                  letter_extra = attr(p_ip, "cross_facet_note"),
                  has_letters  = isTRUE(attr(p_ip, "has_letters")))
                cat(paste0("*", gsub("\n", "  \n", cap), "*",
                           "   \n  \n"))
              }
            }
          }
        }
        # One means plot per marginal-means table (blocks left out of
        # post_hoc get no plot), and one interaction-style plot per
        # simple-effects table with letters per level of `by`.
        means_vars <- names(posthoc_results)[vapply(posthoc_results, function(x)
          identical(attr(x, "kind"), "marginal"), logical(1))]
        simple_keys <- names(posthoc_results)[vapply(posthoc_results, function(x)
          identical(attr(x, "kind"), "simple"), logical(1))]
        for (sk in simple_keys) {
          s_term <- trimws(sub("\\|.*", "", sk))
          s_by   <- trimws(sub(".*\\|", "", sk))
          cl <- posthoc_results[[sk]]$cld
          lo <- NULL
          if (!is.null(cl) && all(c(s_term, s_by, "Letter") %in% names(cl))) {
            lo <- cl[, c(s_term, s_by, "Letter"), drop = FALSE]
            names(lo)[names(lo) == "Letter"] <- "Letters"
            lo[[s_term]] <- as.character(lo[[s_term]])
            lo[[s_by]]   <- as.character(lo[[s_by]])
          }
          int_by <- intersect(c(paste(s_term, s_by, sep = ":"),
                                paste(s_by, s_term, sep = ":")), term_names_sig)
          p_sp <- tryCatch(
            make_aov_interaction_plot(
              c(s_by, s_term),
              is_sig = length(int_by) > 0 && isTRUE(any(p_of(int_by) < alpha)),
              letters_override = lo,
              letter_note = paste0("Letters compare ", s_term, " within each ",
                                   "level of ", s_by, " only; do not compare ",
                                   "letters across levels of ", s_by, ".")),
            error = function(e) {
              warning("f_aov: simple-effects plot for '", sk, "' could not be ",
                      "built: ", conditionMessage(e), call. = FALSE,
                      immediate. = TRUE)
              NULL
            })
          if (!is.null(p_sp)) {
            key <- paste0("by_plot_", s_term, "_", s_by)
            output_list[[response_name]][[key]] <- p_sp
            tmp_sp <- tempfile(fileext = ".png")
            ok_sp <- tryCatch({
              suppressMessages(ggplot2::ggsave(filename = tmp_sp, plot = p_sp,
                                               width = 7, height = 5.5,
                                               units = "in", dpi = 200))
              file.exists(tmp_sp)
            }, error = function(e) FALSE)
            cat(paste0("\n## Simple-Effects Plot of: ", response_name,
                       "  (", sk, ")  \n"))
            if (isTRUE(ok_sp)) cat(paste0("![](", tmp_sp, ")"), "   \n  \n")
            cap <- make_fig_caption(
              banner       = attr(p_sp, "letter_banner"),
              int_note     = attr(p_sp, "int_caption"),
              letter_extra = attr(p_sp, "cross_facet_note"),
              has_letters  = isTRUE(attr(p_sp, "has_letters")))
            cat(paste0("*", gsub("\n", "  \n", cap), "*", "   \n  \n"))
          }
        }
        if (length(means_vars) > 0) {
          for (vn in means_vars) {
            p_eff <- tryCatch(make_aov_means_plot(vn),
                              error = function(e) {
                                warning("f_aov: means plot for '", vn,
                                        "' could not be built: ",
                                        conditionMessage(e), call. = FALSE,
                                        immediate. = TRUE)
                                NULL
                              })
            if (!is.null(p_eff)) {
              output_list[[response_name]][[paste0("effect_plot_", vn)]] <- p_eff
              tmp_eff <- tempfile(fileext = ".png")
              ok_eff <- tryCatch({
                suppressMessages(
                  ggplot2::ggsave(filename = tmp_eff, plot = p_eff,
                                  width = 6, height = 5, units = "in",
                                  dpi = 200)
                )
                file.exists(tmp_eff)
              }, error = function(e) {
                warning("f_aov: ggsave failed for means plot '", vn, "': ",
                        conditionMessage(e), call. = FALSE,
                        immediate. = TRUE)
                FALSE
              })
              cat(paste0("\n## Estimated Means Plot of: ", response_name,
                         "  (", vn, ")  \n"))
              if (isTRUE(ok_eff))
                cat(paste0("![](", tmp_eff, ")"), "   \n  \n")
              # Single italic caption: estimates/CI, then the letter note (or
              # the "Letters omitted" notice). Means plots carry no interaction
              # sentence, so int_note is omitted.
              eff_bann <- attr(p_eff, "letter_banner")
              cap <- make_fig_caption(
                banner      = eff_bann,
                int_note    = NULL,
                has_letters = isTRUE(attr(p_eff, "has_letters")))
              cat(paste0("*", gsub("\n", "  \n", cap), "*", "   \n  \n"))
            }
          }
        }
      }
      # --------------------------------------------------------------------

      # ---- Pairwise contrast forest plots (opt-in) -----------------------
      # Independent of effect_plots: one row per pairwise difference with
      # its adjusted CI and a zero reference line, drawn with the shared
      # forest helper (helper_contrast_forest.R) so the look matches f_glm()
      # and f_lmer(). Only drawn when the overall F-test is significant (so the
      # pairwise picture is meaningful). Main-effect contrasts are stored as
      # contrast_plot_<term>; interaction cell contrasts (when a significant
      # categorical interaction is present) as interaction_contrast_plot_<term>.
      if (isTRUE(contrast_plots) && overall_p_value < alpha) {
        emm_cf_model  <- aov_test_out
        cat_preds_cf  <- cat_preds_out
        sig_ints_cf   <- sig_interactions

        # Interaction present -> cell-contrast forest(s); else main-effect
        # contrast forest per categorical predictor. This mirrors f_lmer's
        # split into interaction_contrast_plot_* and contrast_plot_*.
        # Follow the post hoc tables: cell contrasts for each tabulated
        # interaction, main-effect contrasts for each marginal-means table.
        int_cat_cf <- ph_ints[vapply(ph_ints, function(it)
          all(ph_parts(it) %in% cat_preds_cf), logical(1))]
        cat_preds_cf <- names(posthoc_results)[vapply(posthoc_results, function(x)
          identical(attr(x, "kind"), "marginal"), logical(1))]

        if (length(int_cat_cf) > 0) {
          for (it in int_cat_cf) {
            parts <- trimws(strsplit(it, ":", fixed = TRUE)[[1]])
            cf_tbl <- tryCatch(
              as.data.frame(emmeans::emmeans(
                emm_cf_model, specs = parts, level = 1 - alpha) |>
                  pairs(adjust = adjust) |>
                  confint(level = 1 - alpha)),
              error = function(e) NULL)
            p_icf <- if (!is.null(cf_tbl))
              make_contrast_forest(
                cf_tbl,
                paste0("Cell-pairwise contrasts of ", response_name,
                       "  (", it, ")"),
                alpha = alpha, adjust = adjust) else NULL
            if (is.null(p_icf)) {
              cat("\n*Contrast forest plot for `", it, "` could not be ",
                  "built (adjusted contrast CIs unavailable); see the ",
                  "post hoc table above.*  \n\n", sep = "")
            } else {
              key <- paste0("interaction_contrast_plot_",
                            gsub(":", "_", it))
              output_list[[response_name]][[key]] <- p_icf
              tmp_icf <- tempfile(fileext = ".png")
              n_rows  <- nrow(p_icf$data)
              ok_icf <- tryCatch({
                suppressMessages(
                  ggplot2::ggsave(filename = tmp_icf, plot = p_icf,
                                  width = 7.5,
                                  height = max(3, 0.4 * n_rows + 1.5),
                                  units = "in", dpi = 200))
                file.exists(tmp_icf)
              }, error = function(e) FALSE)
              cat(paste0("\n## Contrast Forest Plot of: ", response_name,
                         "  (", it, ")  \n"))
              if (isTRUE(ok_icf))
                cat(paste0("![](", tmp_icf, ")"), "   \n  \n")
              icf_cap <- attr(p_icf, "int_caption")
              if (!is.null(icf_cap))
                cat(paste0("*", gsub("\n", "  \n", icf_cap), "*",
                           "   \n  \n"))
            }
          }
        } else if (length(cat_preds_cf) > 0) {
          for (vn in cat_preds_cf) {
            cf_tbl <- tryCatch(
              as.data.frame(emmeans::emmeans(
                emm_cf_model, specs = vn, level = 1 - alpha) |>
                  pairs(adjust = adjust) |>
                  confint(level = 1 - alpha)),
              error = function(e) NULL)
            p_cf <- if (!is.null(cf_tbl))
              make_contrast_forest(
                cf_tbl,
                paste0("Pairwise contrasts of ", response_name, " by ", vn),
                alpha = alpha, adjust = adjust) else NULL
            if (is.null(p_cf)) {
              cat("\n*Contrast forest plot for `", vn, "` could not be ",
                  "built (adjusted contrast CIs unavailable); see the ",
                  "post hoc table above.*  \n\n", sep = "")
            } else {
              key <- paste0("contrast_plot_", vn)
              output_list[[response_name]][[key]] <- p_cf
              tmp_cf <- tempfile(fileext = ".png")
              n_rows <- nrow(p_cf$data)
              ok_cf <- tryCatch({
                suppressMessages(
                  ggplot2::ggsave(filename = tmp_cf, plot = p_cf,
                                  width = 7,
                                  height = max(3, 0.4 * n_rows + 1.5),
                                  units = "in", dpi = 200))
                file.exists(tmp_cf)
              }, error = function(e) FALSE)
              cat(paste0("\n## Contrast Forest Plot of: ", response_name,
                         "  (", vn, ")  \n"))
              if (isTRUE(ok_cf))
                cat(paste0("![](", tmp_cf, ")"), "   \n  \n")
              cf_cap <- attr(p_cf, "int_caption")
              if (!is.null(cf_cap))
                cat(paste0("*", gsub("\n", "  \n", cf_cap), "*",
                           "   \n  \n"))
            }
          }
        }
      }
      # --------------------------------------------------------------------

      i <- i + 1

      if(output_type != "rmd" &&  i < length(lhs)){
        # Pagebreak
        cat("
<div style=\"page-break-after: always;\"></div>
\\newpage
        ")
      }
    } #Main loop end
    if (output == TRUE) {
      return(output_list)
    }
  } # End generate report function.


  # Execute analysis ONCE: capture both the output_list and the markdown text.
  # Caching avoids running every ANOVA/transformation/emmeans calculation a second
  # time (which could give different results if bestNormalize uses random search).
  na_notes <- character(0)  # filled by generate_report(), sent below
  cached_markdown <- NULL
  suppressMessages(
    cached_markdown <- utils::capture.output(
      output_list <- generate_report()
    )
  )
  class(output_list) <- "f_aov"
  for (na_msg in unique(na_notes)) message(na_msg)

  # Helper so word/pdf/rmd paths all use the same already-generated markdown.
  get_cached_markdown <- function() cached_markdown

  # Here the documents are constructed.
  if (output_type %in% c("word", "pdf")) {

    # Show save location before knitting else it will not display in console.
    message(paste0("Saving output in: ", output_path))

    # Create a temporary R Markdown file
    word_pdf_preamble <- function(){ paste0("
---
title: \"f_aov Analysis Report\"
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
")}

    # Prevent ## before printed output
    knitr::opts_chunk$set(comment = "")

    # Use the already-generated markdown (no second run of generate_report)
    generated_markdown <- get_cached_markdown()

    # Combine the preamble, assumptions, and the captured report into one string
    rmd_content <- paste(
      word_pdf_preamble(),
      # The captured output already contains the necessary markdown formatting and image links
      paste(generated_markdown, collapse = "\n"),
      sep = "\n"
    )

    # Write the complete Rmd content to the temp file
    writeLines(rmd_content, temp_output_file)

    # Create the RMarkdown file
    tryCatch(
      rmarkdown::render(
        temp_output_file,
        output_file = output_path,
        intermediates_dir = temp_output_dir,
        knit_root_dir = temp_output_dir,
        quiet = TRUE,
        output_format = paste0(output_type, "_document")
      ),
      error = function(e) {
        # Surface the actual LaTeX log so the user can diagnose the real error
        log_file <- file.path(temp_output_dir,
                              sub("\\.(Rmd|rmd)$", ".log",
                                  basename(temp_output_file)))
        if (file.exists(log_file)) {
          log_lines <- readLines(log_file, warn = FALSE)
          # Extract only the error lines -- look for lines starting with "!"
          error_lines <- grep("^!", log_lines, value = TRUE)
          context_idx <- grep("^!", log_lines)
          # Also grab the 2 lines after each "!" for context
          context_lines <- unique(unlist(lapply(context_idx, function(i)
            log_lines[i:min(i + 2, length(log_lines))])))
          message("\n--- LaTeX log errors ---\n",
                  paste(context_lines, collapse = "\n"),
                  "\n--- Full log: ", log_file, " ---")
        }
        stop(e)
      }
    )

    # Open files after creation
    if(open_generated_files == TRUE){
      # Open the file with default program
      f_open_file(output_path)
    }

    return(invisible(output_list))

  }
  else if (output_type == "excel") {

    # show the location were the file is saved
    message(paste0("Saving output in: ", output_path))

    # One sheet per response and post hoc table ("<response> <term>").
    # Excel sheet names: at most 31 characters, none of []:*?/\ ; made unique.
    post_hoc_tables <- list()
    for (rn in response_names) {
      obj <- output_list[[rn]]
      ph  <- if (is.list(obj)) obj$post_hoc else NULL
      if (length(ph) == 0L) {
        post_hoc_tables[[rn]] <- data.frame(note = "No post-hoc test performed")
        next
      }
      for (k in names(ph)) {
        tab <- attr(ph[[k]], "display")
        if (!is.data.frame(tab)) tab <- ph[[k]]$cld
        if (!is.data.frame(tab)) tab <- data.frame(note = "Letters unavailable")
        post_hoc_tables[[paste(rn, k)]] <- tab
      }
    }
    sheet_names <- gsub("[*?/\\\\:]|\\[|\\]", "_", names(post_hoc_tables))
    sheet_names <- gsub("\\s*\\|\\s*", " by ", sheet_names)
    sheet_names <- substr(sheet_names, 1, 31)
    sheet_names <- make.unique(sheet_names, sep = "_")
    sheet_names <- substr(sheet_names, 1, 31)
    names(post_hoc_tables) <- sheet_names

    writexl::write_xlsx(post_hoc_tables, path = output_path)

    # Open files after creation
    if(open_generated_files == TRUE){
      f_open_file(output_path)
    }

    return(invisible(output_list))

  }
  else if (output_type == "rmd"){

    if (is.null(knitr::opts_knit$get("output.dir"))) {
      knitr::opts_knit$set(output.dir = tempdir())
    }

    # Use the already-generated markdown (no second run of generate_report)
    clean_rmd_output <- paste(get_cached_markdown(), collapse = "\n")

    output_list[["rmd"]] <- clean_rmd_output

    return(invisible(output_list))

  } else if (output_type == "default"){
    #Default R behavior only show when not stored in an new object
    return(output_list)

  } else if (output_type == "console"){
    #Print output list to the console (forced)
    print(output_list)

    return(invisible(output_list))

  } else {
    warning("Invalid output format specified. No file generated.")

  }

  # Remove the temporary R Markdown file
  invisible(suppressWarnings(file.remove(temp_output_file)))

}


# Print the per-term post hoc tables of one f_aov response (console).
# Falls back to the single (deprecated) table for objects from rfriend < 3.2.1.
print_aov_post_hoc <- function(sublist, header) {
  ph <- sublist$post_hoc
  if (length(ph) == 0L) {
    if (!is.null(sublist$post_hoc_summary_table)) {
      cat("\n--- ", header, " ---\n", sep = "")
      print(sublist$post_hoc_summary_table, row.names = FALSE, quote = FALSE)
      cat(sublist$cld_text)
    }
    return(invisible(NULL))
  }
  cat("\n--- ", header, " ---\n", sep = "")
  for (k in names(ph)) {
    kind <- attr(ph[[k]], "kind")
    kind_lbl <- if (is.null(kind)) "" else switch(kind,
      cells = " (cell means)", marginal = " (marginal means)",
      simple = " (simple effects)", "")
    by_k <- if (identical(kind, "simple")) trimws(sub(".*\\|", "", k)) else NULL
    cat("\n* ", k, kind_lbl,
        if (isTRUE(attr(ph[[k]], "term_ns"))) "  (not significant)" else "",
        " *\n", sep = "")
    cat(ph_caption(by_k), "\n", sep = "")
    if (!is.null(ph[[k]]$caution))
      cat(paste(strwrap(ph[[k]]$caution, width = 76), collapse = "\n"), "\n", sep = "")
    tab <- attr(ph[[k]], "display")
    if (is.null(tab)) tab <- ph[[k]]$cld
    if (!is.null(tab)) print(tab, row.names = FALSE, quote = FALSE)
    cat(ph[[k]]$cld_text)
  }
  invisible(NULL)
}

#' @export
print.f_aov <- function(x, ...) {

  # Loop over each category (a, b, etc.)
  for (category in names(x)) {
    # Skip non-response entries (e.g. "rmd" character string)
    sublist <- x[[category]]
    if (!is.list(sublist)) next

    # Skip responses that were skipped due to n=1 saturation
    if (isTRUE(sublist$skipped)) {
      message(paste0(
        "\n", strrep("=", 65), "\n",
        "  SKIPPED: ", category, "\n",
        "  ", sublist$skipped_reason, "\n",
        strrep("=", 65)
      ))
      next
    }

    # Warn if force_aov overrode the n=1 skip
    if (isTRUE(sublist$force_aov_used)) {
      message(paste0(
        "\n", strrep("!", 60), "\n",
        "  DIAGNOSTIC ONLY: ", category, ": saturated model (residual df = 0).\n",
        "  F-statistics and p-values are undefined. Do not report.\n",
        strrep("!", 60)
      ))
    }

    if(sublist$Response_Transformed == TRUE){
      if(sublist$transformation_option == "bestnormalize"){
        cat("\n   \n==========================================================\n")
        cat("   ANOVA of", sublist$bestNormalize$transformation_name,"TRANSFORMED response variable:", category, "\n")
        cat("==========================================================\n")



      }

      if(sublist$transformation_option == "boxcox"||
         sublist$transformation_option == TRUE){
        cat("\n   \n===========================================================\n")
        cat("   ANOVA of Box-Cox TRANSFORMED response variable:", category, "\n")
        cat("===========================================================\n")
      }

      cat(paste("\n aov call: ", sublist$aov_call, "\n"))

      .at <- if (is.null(sublist$anova_type)) 2 else sublist$anova_type
      cat("\nType ", as.character(as.roman(.at)), " ",
          if (!is.null(sublist$anova_heading)) sublist$anova_heading
          else "ANOVA on the TRANSFORMED response", " (car::Anova):\n", sep = "")
      print(if (!is.null(sublist$transformed_aov_summary))
        sublist$transformed_aov_summary else sublist$aov_summary)


      print_aov_post_hoc(sublist, paste0("BACK TRANSFORMED Post hoc Comparisons of: ",
                                         category))
      cat("\n")
      message("Note: 'median (BT)' = back-transformed estimated marginal mean. ",
              "Back-transforming a mean from a transformed scale returns the MEDIAN ",
              "on the original scale, not the arithmetic mean. Report these as ",
              "back-transformed medians. CIs are valid; SE is omitted (asymmetric on original scale).")

      if (!is.null(sublist$min_cell_n) && sublist$min_cell_n <= 3) {
        warning(call. = FALSE, immediate. = TRUE, "\nMinimum cell size = ", sublist$min_cell_n,
                ". Levene's Test has very low power with such small groups; ",
                "interpret its result with caution.  \n")
      }
      if (isTRUE(sublist$transformed_levene_test$p <= sublist$alpha)) {
        warning(call. = FALSE, immediate. = TRUE, "   \nBased on the 'Levene's Test' (",
                round(sublist$transformed_levene_test$p, digits = 4), " \u2264 ", sublist$alpha, ") the transformed\n residuals do NOT have equal variance (heteroskedasticity). \nANOVA results can be misleading, resort to other statistical tests.  \n")
      }

      if (isTRUE(sublist$transformed_shapiro_test$p.value <= sublist$alpha)) {
        warning(call. = FALSE, immediate. = TRUE, "   \nBased on the Shapiro-Wilk Test (",
                round(sublist$transformed_shapiro_test$p.value, digits = 4), " \u2264 ", sublist$alpha,") the transformed\n residuals are **NOT** normally distributed. \nANOVA results can be misleading, resort to other statistical tests.  \n"
        )
      }

      if (isTRUE(sublist$last_resort_triggered)) {
        warning(call. = FALSE, immediate. = TRUE, paste0(
          "\n", strrep("=", 65), "\n",
          "  WARNING: Transformation did not resolve\n  ",
          sublist$last_resort_reason, "\n",
          strrep("=", 65), "\n",
          "  ANOVA results should be interpreted with caution.\n\n",
          "  Alternative tests to consider:\n",
          "  - GLM            : f_glm(", category, " ~ <predictors>, data = <data>, family = <dist>)\n",
          "  - Kruskal-Wallis : f_kruskal_test(", category, " ~ <predictor>, data = <data>)\n",
          "  - ART ANOVA      : ARTool::art(", category, " ~ <predictors>, data = <data>)\n",
          strrep("=", 65)
        ))
      }

    } else {

      cat("\n   \n===========================================================\n")
      cat("   ANOVA of response variable: ", category, "\n")
      cat("===========================================================\n")

      cat(paste("\n aov call: ", sublist$aov_call, "\n"))

      .at <- if (is.null(sublist$anova_type)) 2 else sublist$anova_type
      cat("\nType ", as.character(as.roman(.at)), " ",
          if (!is.null(sublist$anova_heading)) sublist$anova_heading
          else paste0("ANOVA on ", category, " (untransformed)"),
          " (car::Anova):\n", sep = "")
      print(sublist$aov_summary)
      if (!is.null(sublist$transformation_rejected))
        message("Note: ", sublist$transformation_rejected$reason,
                " The untransformed analysis is reported.")


      print_aov_post_hoc(sublist, paste0("Post hoc Comparisons of: ", category))

      if (!is.null(sublist$min_cell_n) && sublist$min_cell_n <= 3) {
        warning(call. = FALSE, immediate. = TRUE, "\nNote: Minimum cell size = ", sublist$min_cell_n,
                ". Levene's Test has very low power with such small groups; ",
                "interpret its result with caution.  \n")
      }
      # Same advice as the report: do not tell the user to enable a
      # transformation that was tried and not kept, or that is already on.
      advice <- if (!is.null(sublist$transformation_rejected)) {
        "A transformation was tried but not kept (see above); resort to other statistical tests."
      } else if (isFALSE(sublist$transformation_option)) {
        "Enable the transformation option (`transformation = TRUE`) or resort to other statistical tests."
      } else {
        "Resort to other statistical tests."
      }
      if (isTRUE(sublist$Levene_test_on_res$p <= sublist$alpha)) {
        warning(call. = FALSE, immediate. = TRUE, "   \nBased on the 'Levene's Test' (",
                round(sublist$Levene_test_on_res$p, digits = 4), " \u2264 ", sublist$alpha, ")\nthe residuals do NOT have equal variance (heteroskedasticity).\nANOVA results can be misleading. \n", advice, "  \n"
        )
      }

      if (isTRUE(sublist$shapiro_test_residuals$p.value <= sublist$alpha)) {
        warning(call. = FALSE, immediate. = TRUE, "  \nBased on the Shapiro-Wilk Test (",
                round(sublist$shapiro_test_residuals$p.value, digits = 4), " \u2264 ", sublist$alpha,")\n the residuals are **NOT** normally distributed. \nANOVA results can be misleading. \n", advice, "  \n"
        )
      }
    }
  }# End of loop

  #Some space between output and next input line in console
  cat("\n   \n")
} # End of function



#' @export
plot.f_aov <- function(x, ...) {

  ########## Reset initial settings on exit #################################
  .session_state <- save_session_state()  # Helper function: helper_session_state
  on.exit(restore_session_state(.session_state), add = TRUE) # Helper function: helper_session_state


  # Loop over each category (a, b, etc.)
  for (category in names(x)) {
    # Skip non-response entries (e.g. "rmd" character string)
    sublist <- x[[category]]
    if (!is.list(sublist)) next

    # Skip responses that were skipped due to n=1 saturation
    if (isTRUE(sublist$skipped)) next

    if(sublist$Response_Transformed == FALSE){
      par(mfrow = c(1, 2),
          mar = c(3, 2.8, 4, 0.6),
          oma = c(0, 0, 0, 0),
          mgp = c(1.7, .5, 0)
      )
      # Boxplot
      boxplot(residuals(sublist$aov_test),
              main = paste("Variable:", category),
              xlab = "Residuals",
              cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
      # Histogram
      f_hist(residuals(sublist$aov_test),  xlab = "Residuals",
             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

      # Residual plot aov
      plot(sublist$aov_test, 1,
           main = paste("Variable:", category),
           cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

      # QQPlot
      f_qqnorm(residuals(sublist$aov_test),
               cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

      par(mfrow = c(1,1))
      layout(1)  # Clear layout matrix

    } else if(sublist$Response_Transformed == TRUE){
      # Set up a 2x2 plotting layout with adjusted margins mar = c(bottom, left, top, right)
      par(mfrow = c(1, 2),
          mar = c(3, 2.8, 4, 0.6), # Default is mar = c(5.1, 4.1, 4.1, 2.1)
          oma = c(0, 0, 2, 0),
          mgp = c(1.7, .5, 0)  # Default is par(mgp = c(3, 1, 0))
      )

      # Histogram
      f_hist(residuals(sublist$aov_test),
             main = paste("Variable:", category),
             xlab = "Residuals",
             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
      f_hist(residuals(sublist$transformed_aov_test),
             main = paste("Transformed variable:", category),
             xlab = "Residuals transformed data",
             cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
      mtext("Histograms", outer = TRUE, cex = 1.3)

      # Residual plot aov
      plot(sublist$aov_test, 1,  main = paste("Variable:", category),
           cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
      plot(sublist$transformed_aov_test, 1,
           main = paste("Transformed variable:", category),
           cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
      title("Residual plots", outer = TRUE, cex = 1.3, line = -1)


      # QQPlot
      f_qqnorm(residuals(sublist$aov_test),
               main = paste("Variable:", category),
               cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
      f_qqnorm(residuals(sublist$transformed_aov_test),
               main = paste("Transformed variable:", category),
               cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
      mtext("Normal Q-Q Plot with 95% Confidence Bands", outer = TRUE, cex = 1.3)

      par(mfrow = c(1,1))
      layout(1)  # Clear layout matrix
    }

    # ------------------------------------------------------------------
    # Means / Interaction plot(s)
    # The publication-ready ggplot2 figures are built once during the
    # analysis and stored in the object (effect_plot_* and
    # interaction_plot_*). Here we simply re-print those stored ggplot
    # objects, so the interactive plot() output matches the report output
    # exactly (shared f_theme_pub() theme and f_pub_palette() colours).
    # ------------------------------------------------------------------
    plot_keys <- grep(paste0("^(effect_plot_|interaction_plot_|",
                             "contrast_plot_|interaction_contrast_plot_)"),
                      names(sublist), value = TRUE)
    if (length(plot_keys) > 0) {
      for (k in plot_keys) {
        p_obj <- sublist[[k]]
        if (inherits(p_obj, "ggplot")) print(p_obj)
      }
    }

  }
}
