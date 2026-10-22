# rfriend 3.2.1 (development version)

## Breaking Changes

These changes can change your results or break code that reads the returned
objects. Please read them before updating scripts.

* **`plant_trial`: new `biomass_g` values.** The column is now above-ground
  biomass *estimated non-destructively from plant images*, so measuring the
  same plant in weeks 2, 4 and 6 is realistic (dry biomass needs a
  destructive harvest). It now includes a plant effect: a mixed model on
  log biomass no longer gives a singular fit. All other columns are
  unchanged. The raw, Excel and SPSS copies in `inst/extdata` are updated in
  the same way. Results and examples that use `biomass_g` change.

* **Misspelled arguments now stop with an error.** Before, a typo such as
  `adjsut = "holm"` was silently ignored and the function ran with the
  default setting, so you could get a different analysis than you asked for
  without noticing. Now the function stops and suggests the right name:
  `Unknown argument(s) in f_kruskal_test(): 'adjsut' (did you mean 'adjust'?)`.
  This applies to `f_aov()`, `f_boxcox()`, `f_boxplot()`, `f_friedman()`,
  `f_kruskal_test()`, `f_lm()`, `f_outliers()`, `f_scan()`,
  `f_stat_wizard()`, `f_summary()`, `f_t_test()` and `f_wilcox_test()`.
  Scripts that contained a typo will now stop; fix the spelling it suggests.

* **Post hoc tests in `f_aov()` and `f_lm()`: one table per factor.** With
  more than one factor in the model (for example `y ~ treatment + week`),
  rfriend used to make a single table comparing every combination of all
  factors (treatment x week) and labelled it "Marginal Means". You now get
  one table per factor (all treatments, averaged over the weeks; all weeks,
  averaged over the treatments) and, when an interaction is significant, an
  extra table for the combinations. See the new `post_hoc` argument below.
  The old single table is still available for one release (see Deprecated).

* **Automatic transformations are kept only when they help.** `f_aov()`,
  `f_lm()` and `f_t_test()` apply Box-Cox (or bestNormalize) automatically
  when the residuals do not look normal. Before, the transformed result was
  always reported, even when the transformation made the residuals *less*
  normal. Now rfriend compares the residual check before and after: if the
  transformation does not improve it, the untransformed analysis is
  reported and the report says so, e.g. "Transformation not kept. Box-Cox
  did not improve the normality of the residuals (Shapiro-Wilk p = 0.0375
  before, 0.0252 after)." (When the transformation was triggered only by
  unequal variances, the variance test is compared instead.) If you name a
  response in `force_transformation`, it is always transformed. Some
  analyses that were transformed in 3.2.0 are therefore reported
  untransformed now.

* **`f_lm()`: the plain result names now always hold the untransformed
  model.** `$anova`, `$summary` and `$coefficients` describe the model on
  the original response. When the response was transformed, the transformed
  model is in `$transformed_anova`, `$transformed_summary` and
  `$transformed_coefficients` (these are `NULL` otherwise). In 3.2.0 the
  plain names held the transformed model whenever a transformation was
  applied. What `print()` and the report show has not changed. To get the
  table that was reported:
  `tab <- if (out$y$Response_Transformed) out$y$transformed_anova else out$y$anova`.
  `f_aov()` already worked this way (`$aov_summary` untransformed,
  `$transformed_aov_summary` transformed).

* **`f_aov()` now analyses block designs.** `f_aov()` skipped any analysis
  in which one group combination had only one observation, saying the model
  was "saturated". That also blocked ordinary designs such as a randomised
  complete block design (`height ~ block + treatment`, one plant per
  treatment per block), which can be analysed perfectly well. `f_aov()` now
  only skips a model that truly has no degrees of freedom left for the
  error. For such a model no F-test exists, so `force_aov = TRUE` now stops
  with an explanation instead of fitting it.

## New Features in Existing Functions

* **`f_remove_outliers()` can keep the rows and set only values to `NA`.**
  With `set_na = TRUE` the rows stay and only the outlier values become
  missing, so the other measurements of that plant stay in the analysis.
  With an `f_outliers()` result, exactly the flagged values in the column(s)
  where they were flagged are cleared; `columns` limits this to some checked
  columns. By hand, give row numbers (or IDs with `by`) and the columns:
  `f_remove_outliers(df, c(21, 32), set_na = TRUE, columns = "biomass_g")`.
  The message says which values were set to `NA`.

* **Removed rows with missing values are reported in the console.** The test
  and model functions leave out rows with a missing value (NA) in the columns
  of the formula, as R does. Until now only the Word, PDF and R Markdown
  reports said so. `f_aov()`, `f_lm()`, `f_glm()`, `f_lmer()`,
  `f_t_test()`, `f_wilcox_test()`, `f_kruskal_test()` and `f_friedman()`
  now also print a message, for example
  `3 rows with missing values (NA) removed from analysis (NA in: height_cm).`
  Functions that drop NA rows per response (`f_glm()`, `f_friedman()`) name
  the response. No message when nothing was removed.

* **`f_stat_wizard()` recognises ordinal responses.** A response stored as an
  ordered factor (for example a leaf-damage score from 1 to 5,
  `factor(score, ordered = TRUE)`) is now classified as ordinal. The wizard
  recommends a rank-based test on the rank codes of the levels:
  `f_wilcox_test()` for two groups, `f_kruskal_test()` for three or more,
  the paired versions or `f_friedman()` when `id_col` is given, and a
  Spearman correlation for a numeric predictor. With several predictors or
  random effects it points to ordinal regression (`MASS::polr`,
  `ordinal::clmm`). Before, an ordered factor was treated as unordered
  categories (chi-square test). A numeric response of whole numbers on a short
  scale (at most 10 values up to 10) is still analysed as count data, but the
  report now says that it may be a score and shows how to store it as an
  ordered factor.

* **Choose your post hoc tables: new `post_hoc` and `by` arguments** in
  `f_aov()`, `f_lm()` and `f_lmer()`, with the same meaning in all three:
  - By default you get a table for each factor and, for each significant
    interaction, a table of all combinations. A factor that is not
    significant is still shown, with `ns` instead of letters. A factor that
    is part of a significant interaction gets an "Interpret with caution"
    note, because its average over the other factor can hide what happens
    at each level. Numeric predictors get no table.
  - `post_hoc = "treatment"` shows only that table. This is the way to leave
    blocks out of the comparisons: the block stays in the model, but nobody
    needs letters for blocks:
    `f_aov(height ~ block + treatment, data = d, post_hoc = "treatment")`.
    `post_hoc = "treatment:week"` shows the table of all combinations;
    `post_hoc = FALSE` turns the post hoc off.
  - `by = "week"` (together with `post_hoc = "treatment"`) compares the
    treatments separately within each week. This needs far fewer
    comparisons than the full combination table (for 4 treatments x 3 weeks:
    18 instead of 66), so it has more power.
  - Each table has its own letters, its own p-value adjustment and its own
    footer, and says "Letters are compared within this table only." A
    misspelled term name stops with a "did you mean" suggestion.
  - The tables are returned as `out$y$post_hoc[["treatment"]]`,
    `out$y$post_hoc[["treatment:week"]]` or
    `out$y$post_hoc[["treatment | week"]]`, each with the same parts in all
    three functions (`emmeans`, `pairs`, `cld`, `cld_text`, `caution`).
  - Plots and Excel output follow the tables: a factor left out of
    `post_hoc` gets no plot, the letters on a plot come from that factor's
    own table, and the Excel file has one sheet per table. For `by`,
    `f_aov()` draws a plot with the letters per level of `by`.

* **`f_glm()`: yes/no responses with a new `event` argument.** For a
  logistic regression (`family = binomial`) on a factor such as
  `infected` ("no"/"yes"), write `event = "yes"` to say which outcome you
  model. This matters: R's own `glm()` silently models the second level in
  alphabetical order, so the labels "healthy"/"diseased" would give you
  P(healthy) and turn every effect around. rfriend does not guess; without
  `event` it stops and shows the levels and the call to use. The chosen
  coding is printed, shown in the report and on the plots
  ("P(infected = yes)"), and stored in `out$y$event_coding`. With three or
  more levels, `event` can name one or more levels; the others are combined
  and the report says which. Logical (`TRUE`/`FALSE`) responses need no
  `event`.

* **`f_stat_wizard()` knows the newer functions.** It now recommends
  `f_lm()` for a numeric predictor, `f_friedman()` for repeated measurements
  that are not normal, and `f_lmer()` for repeated measurements that are.
  Its suggested `f_glm()` code for a yes/no factor includes `event`, and
  `run = TRUE` can run all of these.

* **Reports say which scale a test is on.** The ANOVA and t-test headings in
  `f_aov()`, `f_lm()` and `f_t_test()` now read, for example,
  "ANOVA on Box-Cox transformed height_cm (lambda = -0.131)" or
  "ANOVA on height_cm (untransformed)".

* **Easier checks on transformed results.** `f_aov()` always returns
  `$transformed_aov_summary` (it is `NULL` when nothing was transformed), and
  `f_t_test()` returns `$transformed_t_test` in the same way, so
  `is.null()` tells you reliably whether a transformation was used.

* **`f_pander()` prints `f_summary()` results directly** instead of warning
  and printing an untidy list.

* **`f_remove_outliers()` works when no outliers were found.** It now
  accepts the `NULL` that `f_outliers()` returns in that case and gives your
  data back unchanged, so the find-then-remove steps also run on clean data.

## Deprecated

These still work in 3.2.1 but will be removed in the next release.

* `f_aov()`: `$post_hoc_summary_table`, `$summary_table` and `$cld_text`;
  `f_lm()`: `$post_hoc_summary_table`, `$emmeans_table` and `$cld_text`.
  They used to hold the single post hoc table. They now hold the
  combinations table if there is a significant interaction, otherwise the
  first table. Use `$post_hoc[[name]]` instead.

* `f_t_test()`: `$t_test_transformed` is now called `$transformed_t_test`.

* **Plot switches have the same names in all test functions.** The figures in
  a report are switched with `diagnostic_plots` (assumption and residual
  checks) and `effect_plots` (effect, estimated-means, interaction and
  coefficient forest plots); `contrast_plots` is unchanged. The old names
  still work, with a deprecation warning:
  - `f_aov()`: `norm_plots` is now `diagnostic_plots`, `interaction_plots`
    is now `effect_plots`.
  - `f_lm()`, `f_t_test()`, `f_wilcox_test()`: `norm_plots` is now
    `diagnostic_plots`.
  - `f_glm()`, `f_lmer()`: `effect_plot` is now `effect_plots`.
  - `f_kruskal_test()`, `f_friedman()`: `plot` is replaced by
    `diagnostic_plots` (density plot / within-block trace plot) and
    `effect_plots` (boxplot with letters); `plot` sets both.
  - New: `f_t_test()` and `f_wilcox_test()` get `effect_plots` for the main
    effect plot, which could not be switched off before.

## Minor Changes

* **Printed messages use plain ASCII punctuation.** Messages and report
  lines of `f_stat_wizard()`, `f_aov()`, `f_corplot()` and `f_glm()` used
  an em dash (a long dash), which is not ASCII and does not always display
  or render to PDF correctly. They now use a colon, semicolon or comma, for
  example "WARN: Unbalanced pairing: 60 subject x group combination(s) are
  missing". The pairing lines of the `f_stat_wizard()` report also use a
  plain "x" instead of a multiplication sign ("20 subjects x 3 groups").
  In `f_glm()`,
  the placeholder shown in the Letter column when LRT letters cannot be
  computed is now `"-"` instead of an em dash. Code that tests for the
  old placeholder should test for `"-"`.

* **`unique_num_threshold` can now be spelled correctly** in `f_factors()`
  and `f_corplot()`. The argument was named `unique_num_treshold` (missing
  the "h"), next to the correctly spelled `repeats_threshold`. Both
  spellings are now accepted, so existing scripts keep working; the help
  pages use the correct spelling. Giving both spellings with different
  values stops with an error.

* The help for `repeats_threshold` (`f_factors()`, `f_corplot()`) now
  describes what the code does: a numeric column becomes a factor only when
  a value occurs *more than* `repeats_threshold` times, so with the default
  of 2 a value has to occur at least three times. The help said 2 repeats
  were enough. The behaviour itself has not changed.

* The package description (DESCRIPTION) has been rewritten to give a short
  overview of what rfriend does, including the newer functions `f_lm()`,
  `f_friedman()`, `f_lmer()` and `f_stat_wizard()`, and now links to the
  GitHub repository as well as the website.

* The example workbook `plant_trial_raw.xlsx` (see `f_example_data()`) has
  a second sheet, `meta_data`: a codebook with the meaning, unit and
  intended values of each column. The raw data on the first sheet is
  unchanged. Handy for practising `readxl::excel_sheets()` and
  `read_excel(sheet = )` on a workbook with more than one sheet. The
  `?plant_trial` help page now mentions it, and its `infected` entry uses
  `f_glm(..., event = "yes")` instead of converting to 0/1.

* The minimum Pandoc version (needed for Word and PDF output) is now stated
  the same way everywhere: Pandoc 3.1 or higher. DESCRIPTION used to say
  3.2 and the help pages 1.12.3.

* The README has been rewritten and now shows an example report
  (`man/figures/report-example.png`).

## Bug Fixes

* **`f_long()`: helpers such as `starts_with()` work.** The help page offered
  `measure_columns = starts_with("Measure")`, but any helper stopped with an
  error ("must be used within a *selecting* function", or "could not find
  function" without dplyr loaded), because the argument was checked with
  `is.null()` before the selection. Helpers now work in `measure_columns` and
  `keep_cols`, also without loading dplyr. Column names, column numbers and a
  variable that holds `NULL` behave as before. A selection that matches no
  columns now stops with a clear message.

* **`bad_rows$row_id` works on an `f_outliers()` result.** The result is a
  list of tables (`output_df`, or one per checked column), so `bad_rows$row_id`
  returned `NULL` without a warning, and `df$x[bad_rows$row_id] <- NA` silently
  changed nothing. `$` now reaches the columns of the outlier table:
  `bad_rows$row_id` gives the flagged rows (with several checked columns: all
  of them), and with one checked column any column of the table works. A name
  that is not part of the result stops with an error that lists what is
  available. `[[` keeps the standard list behaviour.
* **"Removed 1 outlier row."** `f_remove_outliers()` now uses the singular
  and plural correctly ("outlier row" / "outlier rows").

* **`f_example_data(dest = "data")` creates the folder.** When the folder did
  not exist yet, the file was copied to a *file* named `data` (no extension),
  with a message that looked like success. A `dest` without a file extension
  (or ending in `/`) is now a folder: it is created if needed, with a
  message, and the original file name is kept. A `dest` with a file
  extension whose folder does not exist stops with a clear message.

* **`subset` can pick two groups in `f_t_test()` and `f_wilcox_test()`.**
  `f_t_test(y ~ treatment, data = d, subset = treatment %in% c("A", "B"))`
  stopped with "Grouping variable must have exactly 2 levels", because the
  two-group check ran on the full data before `subset` was applied. The
  check now runs after the subset and the removal of missing values, and the
  error names the levels it found.

* **`f_t_test()` and `f_wilcox_test()` reports counted `subset` rows as
  missing.** With `subset`, the report line "Removed n rows with missing
  values" also counted the rows that `subset` left out. It now counts only
  the rows removed because of NA.

* **Ordinal responses in the rank tests.** `f_kruskal_test()` and
  `f_friedman()` stopped with an internal error ("'type' must be 1 or 3 for
  ordered factors") when the response was an ordered factor, such as a score
  stored with `factor(score, ordered = TRUE)`. Rank tests use only the order
  of the values, so `f_kruskal_test()`, `f_friedman()` and `f_wilcox_test()`
  now test an ordered-factor response on the rank codes of its levels
  (1 = lowest level) and print a message with the coding. Any other
  non-numeric response stops with a clear message that points to
  `factor(..., ordered = TRUE)` for scores and `f_chisq_test()` for unordered
  categories.

* `f_wilcox_test()` opened the generated Word, PDF or Excel file even when
  run from a script, because `open_generated_files` defaulted to `TRUE`. It
  now defaults to `interactive()`, as in the other functions and as the help
  page already said.

* **`f_model_compare()` accepts rfriend results.** Passing the result of
  `f_lm()`, `f_aov()`, `f_glm()` or `f_lmer()` failed with an unreadable
  error ("arguments imply differing number of rows"). The fitted model
  stored in the result is now used. A result with several responses, or
  with a response that was transformed, stops with a message that says what
  to do instead.

* **`f_model_compare()` uses a chi-square test for Poisson GLMs.** For
  nested GLMs only binomial models got a likelihood-ratio (chi-square)
  test; Poisson (and negative binomial) models got an F test, with R's
  warning "using F test with a 'poisson' family is inappropriate". Families
  with a fixed dispersion now get the chi-square test; gaussian, Gamma and
  quasi families keep the F test.

* **`f_model_compare()` compares mixed models on ML, not REML.** Models
  fitted with REML (the `lme4::lmer()` default, and the models stored by
  `f_lmer()`) are refitted with ML before AIC, BIC and the log-likelihood
  are computed, with a message. Before, the REML criteria were reported,
  which are not comparable between models with different fixed effects.
  When `anova()` refuses two mixed models because their calls hold
  different data objects (as for models stored by `f_lmer()`), the
  likelihood-ratio test is computed directly instead of failing.

* **`f_glm()` with an intercept-only model (`y ~ 1`)** stopped with
  "'dim' cannot be of length 0". It now fits the model and skips the post
  hoc step, which has nothing to compare.

* `f_summary()` reported far too large skewness and kurtosis values (about
  58 times too large for 60 observations), so even perfectly symmetric data
  looked strongly skewed. The values are now the standard sample skewness and
  excess kurtosis (the same as SPSS and SAS report), and missing values are
  no longer counted.

* `f_wilcox_test()` failed when `subset` referred to a column of the data,
  e.g. `subset = week == 6` gave "object 'week' not found". It now works.

* `print()` of an `f_lmer()` result showed the same footer line under every
  post hoc table, with the wrong numbers for most of them (e.g. "3 estimates
  / 3 tests" under a factor with 4 levels). Each table now shows its own
  numbers. The "Interpret with caution" note for a factor that is part of a
  significant interaction is now also shown in the console, not only in the
  report.

* `print()` of an `f_aov()` result with a transformed response showed the
  ANOVA table of the *untransformed* response under the heading
  "TRANSFORMED". It now shows the transformed table.

* `f_aov()` with an `Error()` term (repeated-measures ANOVA written as
  `y ~ time + Error(subject/time)`) is not supported; it now says so and
  shows the equivalent `f_lmer()` call, instead of skipping the analysis
  with a misleading message.

* `f_model_compare()` on models fitted with an `Error()` term failed with an
  unclear internal error after its warning. It now stops with a clear
  message that suggests a mixed model instead.

* `f_stat_wizard()` suggested chi-square code that did not run. It now
  suggests `f_chisq_test(table(data$x, data$y))`. It also no longer suggests
  an `f_aov(... + Error())` call.

* `f_chisq_test()` showed the second variable as `y` in its output instead
  of its real name, and its printout ended without a line break.

* The notes under the letters tables of `f_kruskal_test()` and
  `f_friedman()` were missing a bracket and a space
  ("(alpha = 0.050.Groups ...").

* The help page of `f_lmer()` said the post hoc only runs when a term is
  significant. In fact every factor is shown, with `ns` when it is not
  significant; the help page now says so.

* `f_summary(..., digits = NULL)` did not switch rounding off, although the
  help page says it does: it printed 2 decimals, the same as the default. It
  now prints the unrounded values. Likewise, `allow_integer_decimal_mix =
  TRUE` given to `f_summary()` had no effect on the printed table (only on
  the Excel export); it now does. The default `digits` is now 2 in the
  function itself as well, as the help page always said, so default output
  does not change. The returned values were never rounded and still are not.

* After a transformation was tried and not kept, `print()` of an `f_aov()`
  result still advised "ENABLE the transformation option" under the
  residual warnings, although the transformation was already on. It now
  says the transformation was tried but not kept, as the report does, and
  only suggests `transformation = TRUE` when you switched it off.

# rfriend 3.2.0 (2026-07-04)

## New Functions

* `f_lm()` fits ordinary least squares linear regression (`stats::lm()`).  It uses the same assumption checks, optional Box-Cox / bestNormalize transformation workflow, object structure
  and figure theme  as `f_aov()` . Unlike `f_aov()` it keeps numeric predictors numeric,
  so they are modelled as continuous regression terms, and it adds a
  coefficient table, a coefficient forest plot, a Type II ANOVA table, the
  overall R-squared / adjusted R-squared / model F-test, and regression plots.
  Several responses can be analysed in sequence (via `+`), with output to
  console, 'pdf', 'Word', 'Excel' or R Markdown.

* `f_friedman()` adds the Friedman rank sum test, the non-parametric
  alternative to the one-way repeated-measures ANOVA, for unreplicated
  complete block designs. It follows the same workflow and output options as
  `f_kruskal_test()`: it takes a `response ~ group | block` formula (multiple
  responses allowed via `+`), validates that the data form a valid
  unreplicated complete block design, reports Kendall's W effect size, runs pairwise paired Wilcoxon signed-rank tests as post hoc with a compact letter display, and produces density and boxplot figures. Output can be returned as an object or written to 'pdf', 'Word', 'Excel' or R Markdown. Includes `print()` and  `plot()` methods.

* `f_example_data()` gives access to a bundled example datasets shipped with the package. With no arguments it lists the available files; given a file  name it returns the installed file path (ready for `read.csv()`,  `readxl::read_excel()` or `f_open_file()`) and can optionally copy the file  to a destination. A simulated plant-science field trial (a randomized  complete block design with repeated measures) is now bundled as a teaching dataset.

## Changes

*  `f_aov()` now calculates the main ANOVA table using Type II sums of squares (via car::Anova()). This replaces the Type I method used in previous versions. For \strong{unbalanced}
  designs this changes the reported main-effect sums of squares, F-statistics
  and p-values, because a Type I table depends on the order in which terms
  enter the formula while a Type II table does not; balanced designs are
  unaffected. A new `anova_type` argument selects the type: `2` (Type II, the
  new default) or `3` (Type III). Type III is also order-invariant but only
  tests meaningful main effects in the presence of interactions when sum /
  effect contrasts are used, so when `anova_type = 3` and the user has not
  supplied their own contrasts. Type II keeps the main ANOVA table both order-invariant and consistent
  with R's default treatment contrasts and with the `emmeans` post hoc
  comparisons.

* `f_lmer()` the `norm_plots` argument has been renamed to `diagnostic_plots`.
  `norm_plots` is now deprecated (via \pkg{lifecycle}): it still works but
  emits a deprecation warning and forwards its value to `diagnostic_plots`.
  Please update scripts to use `diagnostic_plots`.

## New Features in Existing Functions

* Effect and interaction plots across `f_aov()`, `f_glm()`, `f_lmer()`,
  `f_t_test()`, `f_wilcox_test()` and `f_kruskal_test()` are now
  publication-ready \pkg{ggplot2} objects sharing a common theme
  (`f_theme_pub()`) and colour palette (`f_pub_palette()`, in the new
  `helper_pub_theme.R`). They are stored in the returned object (e.g.
  `out$y1$effect_plot_treatment`, `out$y1$interaction_plot_a_b_1`) so they can
  be retrieved and customised, and `plot()` re-prints them so the interactive
  output matches the report. The per-function additions below build on this.

* `f_glm()` adds a coefficient forest plot (stored as
  `out$y1$coef_forest_plot`), a `contrast_plots` argument (default `FALSE`)
  that adds a pairwise contrast forest plot per categorical post hoc term on
  the link scale (stored as `out$y1$contrast_plot_<term>` and
  `out$y1$interaction_contrast_plot_<term>`), and an `effect_plot` argument
  (default `TRUE`) that toggles the estimated-means and interaction plots.

* `f_aov()` interaction plots now support two-, three- and four-way
  categorical interactions: a two-way uses x-axis plus colour (both
  orientations), while three- and four-way interactions add facet panels for
  the remaining factor(s); interactions of five or more categorical factors
  are skipped with a warning. A `contrast_plots` argument (default `FALSE`)
  adds the same pairwise contrast forest plots as `f_glm()` and `f_lmer()`,
  stored as `out$y1$contrast_plot_<term>` and
  `out$y1$interaction_contrast_plot_<term>`.

* `f_lmer()` plotting is brought in line with `f_aov()` and extended: markdown
  headings and figure captions now match `f_aov()`; interaction plots cover
  three- and four-way categorical interactions (extra factors become facet
  panels, one plot per choice of x-axis factor; order greater than four is
  skipped with a note); a slope plot is drawn for a significant numeric x
  categorical interaction (raw-data scatter with one model-fitted line per
  factor level and a confidence band, going beyond `f_aov()`, which holds
  covariates at their mean); and a coefficient forest plot (fixed-effect
  estimates with confidence intervals and a zero reference line, scaling to
  many predictors) is stored as `out$y1$coef_forest_plot`. New `contrast_plots`
  (default `FALSE`) and `effect_plot` (default `TRUE`) arguments match
  `f_glm()`; `contrast_plots` adds one row per pairwise difference with its
  adjusted confidence interval and a zero reference line (no cap on the number
  of contrasts), stored as `out$y1$contrast_plot_<term>` and
  `out$y1$interaction_contrast_plot_<term>`.

* `f_lmer()` now produces an interaction post hoc cell-means table (estimated
  means for every factor-level combination, compared simultaneously, with a
  compact letter display and pairwise contrasts) for each significant
  categorical interaction, matching `f_aov()`, stored as
  `out$y1$post_hoc[["a:b"]]`.

* `f_lmer()` adds several interpretive notes and caveats to the report:

  * a caution above a main effect's marginal-means table when that effect
    takes part in a significant interaction (the heading is annotated and the
    note points to the artifacts that exist for that interaction: cell-means
    table and plot, slope plot or coefficient slopes), since those means
    average over the interacting factor and can hide or reverse the pattern;

  * when an interaction is present, a note that the Type III F tests and the
    coefficient t tests are different hypotheses (so t^2 will not equal F): the
    F averages a main effect over the interacting term with sum-to-zero coding,
    while each coefficient is the simple effect at the reference level under
    treatment coding;

  * a corrected multiple-response multiple-testing note stating that the
    p-value adjustment only controls error within a single model and only when
    that model has categorical terms that trigger post hoc comparisons; and

  * a caveat on the observed-descriptives table that its rows pool over
    repeated measurements and other predictors, so the reported sd/se mix
    within- and between-subject variation and the observations within a row
    are not independent.

* `f_lmer()` refines its assumption and variance reporting:

  * Levene's test now runs on nested and crossed random-effects grouping
    factors. Previously, for a model such as `(1 | block/plant_id)` the
    grouping factor `"plant_id:block"` was looked up directly in the model
    frame, where it does not exist as a single column, so the test was skipped
    with a misleading "fewer than two levels" reason; it is now reconstructed
    from its component columns, and any remaining skip message states the
    actual reason.

  * Levene's test is corroborated against the Scale-Location panel before the
    heteroscedasticity recommendations are triggered, since a by-group Levene
    test on its own over-fires. The recommendations now appear only when Levene
    is significant and the Scale-Location trend supports it; if the
    corroborating signal cannot be computed, the function falls back to the
    Levene result and says so rather than suppressing it silently.

  * for models with two or more intercept grouping factors, an "ICC by
    grouping factor" table gives each factor's variance and its share of the
    total (with the residual as its own row, so the shares sum to 1), stored as
    `$icc_by_group`; the combined ICC in the Model fit table is now labelled
    the adjusted (total) ICC.

* `f_lmer()` the displayed Type III fixed-effects table now reports NumDF,
  DenDF, F and p only. The `Sum Sq` and `Mean Sq` columns are removed from the
  display because, for an REML-fitted mixed model, they are back-computed by
  `lmerTest` and do not form an additive variance decomposition. The full
  `lmerTest` table, including those columns, is available programmatically in
  the new `$fixed_effects_full` slot.

* `f_lmer()` console and interactive output now match the report:
  `print.f_lmer()` shows the per-grouping-factor ICC table and uses Model fit
  labels consistent with the report for random-slope models
  (`Var(group, int)` / `ICC (approx)`), and `plot()` replays the stored
  effect, interaction, slope, coefficient-forest and contrast-forest plots.

* The reference-level caption under the coefficient forest plot no longer
  contains a non-runnable `relevel()` example: the hard-coded `ref = "drug"`
  that matched no real factor level is replaced by the factor's actual current
  reference level, so the example runs on copy-paste.

* Fixed several markdown rendering artifacts in the Word output: orphaned bold
  markup around grouping-factor names (e.g.
  `**Levels of**** ****Subject****:**`), emphasis spans broken across source
  line breaks (e.g. *how fast*, *Residuals vs Fitted*), and the exponent in the
  multiple-response note, which `pandoc` rendered as `(1-0.05)2` (reading as a
  multiplication) instead of a power; the note now uses a Unicode superscript
  that renders correctly in both the PDF and Word paths.

* `f_t_test()` and `f_wilcox_test()` now produce a main effect plot for every
  response, stored as `out$<response>$main_effect_plot` and rendered into the
  reports. For `f_t_test()` the figure shows the estimate with its confidence
  interval against the raw data, one display per test type: a two-sample test
  shows the two group means each with its own confidence interval, a one-sample
  test shows the sample mean with its confidence interval and a dashed
  reference line at `mu`, and a paired test shows the mean of the per-pair
  differences with its confidence interval and a dashed reference line at `mu`
  (unifying the paired and one-sample displays); when the response was
  transformed, the estimate and interval are back-transformed and labelled as a
  median. For `f_wilcox_test()`, two-sample and paired tests plot the
  Hodges-Lehmann estimate and confidence interval directly.

* `f_kruskal_test()` compact letter display now runs high-to-low by descending
  median via the shared `compact_letters()` helper, consistent with `f_aov()`,
  `f_glm()` and `f_lmer()`.

* `f_factors()` gains a `ref` argument to set the reference level of converted
  factors (the level models contrast against under R's default treatment
  contrasts). Accepts a single string or a named vector mapping columns to
  reference levels, e.g. `ref = c(treatment = "control", dose = "low")`.
  Useful before `f_glm()` and `f_lmer()`. A level not present in a given factor
  is skipped with a warning.

* `df_to_table()` now accepts `label_col` as a column name as well as a column
  index.

## Minor Changes

* New shared internal helpers underpin the publication output: a common theme
  and palette (`helper_pub_theme.R`), forest-plot drawing
  (`helper_forest_plot.R`, `helper_contrast_forest.R`), coefficient reference
  captions (`helper_coef_ref_caption.R`), compact letter displays
  (`helper_compact_letters.R`, `helper_cld_emmeans.R`) and a safe
  Anderson-Darling wrapper (`helper_safe_ad.R`).

* `multcomp` and `multcompView` are no longer imported; compact letter
  displays are now produced by internal helpers. `lifecycle` is added to
  Imports, and `car` and `readxl` are added to Suggests.

## Bug Fixes

* Improved the stability of the `save_as = ` option.

* `f_t_test()` errored with "sample size must be greater than 7" from
  `nortest::ad.test()` whenever a response (or, for a paired test, the set of
  per-pair differences) had fewer than 8 observations, aborting the whole
  analysis. The Anderson-Darling diagnostic is now obtained through a new
  internal `safe_ad()` wrapper (mirroring `safe_shapiro()`) that returns a
  shaped result with an informative "skipped: n < 8"  label instead of
  erroring. The report states clearly when the test was skipped; when both
  Shapiro-Wilk and Anderson-Darling are unavailable at very small sample
  sizes, no transformation is triggered automatically and the user is directed
  to the Q-Q plot and histogram. The same guard is applied inside
  `f_bestNormalize()`, so small-sample transformed t-tests no longer error
  either.

* `f_t_test()` reported a wrong p-value on the one-sample and paired
  transformation paths when `transformation = "bestnormalize"` (or when
  bestNormalize was selected automatically). `bestNormalize` standardizes its
  output to mean 0 and standard deviation 1 by default, so the subsequent
  one-sample test of the transformed values against `mu = 0` was true by
  construction (p approximately 1) regardless of the data, and a non-zero `mu`
  together with the back-transformed interval was distorted. The
  transformation is now requested with `standardize = FALSE`, preserving the
  location and scale needed for a valid test against `mu` and an interpretable
  back-transformed confidence interval. The reported p-value now agrees with
  the untransformed paired or one-sample test.

* `f_lmer()` no longer errors when a random effect has (near) zero variance,
  which previously caused the Shapiro-Wilk normality check on the BLUPs to
  fail with "all 'x' values are identical". The check now returns NA and falls
  back to the qq-plot diagnostics.

* `f_chisq_test()` now also accepts a factor and the two-vector form, and
  detects a violated small-expected-count assumption by reading the expected
  counts directly rather than parsing base R's warning text, so the note
  recommending a Monte Carlo p-value is robust to translation and version
  changes. When `simulate.p.value = TRUE` is requested the (now circular)
  small-count note is suppressed.

* `f_setwd()` called with no argument now stops with a clear, actionable error
  when the script directory cannot be determined (run from the console or an
  unsaved file), instead of issuing a silent warning and leaving the working
  directory unchanged.

* `f_boxplot()` mean markers now ignore `NA` values when computing group means.

* The order of the compact-letter display was not consistently from large (a)
  to small (z); all functions using letters are now consistent.

* `f_wilcox_test()` diagnostic density plots now show the whole curve and are no longer cut off.

# rfriend 3.1.0 (2026-05-30)

## New Features

`f_boxplot` now accepts numeric vectors in addition to data.frames and formulas. A single vector like `f_boxplot(my_vec)` produces one box labelled with the vector name on the y-axis; multiple unnamed vectors like `f_boxplot(hp, cyl)` produce side-by-side boxes, matching base R's `boxplot()` convention. A new `color` argument controls the palette: the default `"rainbow"` preserves existing behaviour, `"bw"` gives publication-style white boxes with black lines, outliers and mean marker, a single colour name like `"steelblue"` applies one hue to all boxes (with a light-tinted fill and darkened outline derived in HSV space), and a vector of colours is recycled for custom per-group palettes. A new `boxwidth` argument exposes the relative width of each box (passed as `boxwex` to `boxplot()`) for finer control over plot appearance.

`f_scan` now accepts loose numeric vectors in the same spirit as `f_boxplot`. A single vector like `f_scan(disp1)` produces a one-group diagnostic dashboard with the vector name carried through as the column label. A formula built from bare vectors works identically to the data.frame form, so `f_scan(disp1 + hp1 ~ cyl1)` assembles the data.frame internally from the variable names in the formula. A positional shorthand is also supported: `f_scan(disp1, cyl1)` is equivalent to `f_scan(disp1 ~ cyl1)`, treating the first vector as the response and any additional vectors as grouping variables, with length checks against the response and clear errors on mismatch.

* `f_summary()` gains a `show_ci` argument (default `FALSE`) that adds
  `CI_lower` and `CI_upper` columns, the bounds of a confidence interval for
  the mean. The interval is a parametric t-interval, computed as
  `mean +/- qt(1 - (1 - conf_level)/2, df = n - 1) * se`, matching the interval
  reported by `t.test()`. A companion `conf_level` argument (default `0.95`)
  sets the confidence level. Groups with fewer than two non-missing
  observations return `NA` bounds.
  
## Minor Changes

* Removed an internal package startup/shutdown file `zzz.R` that printed a spurious "Package unloaded from:" message on unload. Package loading and unloading are now silent on the rfriend side.

* Improved the boxplot explanation in the introduction section ("Understanding Boxplots: A Visual Guide") of the output files from `f_boxplot()`.

## Bug Fixes

* `f_boxplot()` with a formula and explicit data (e.g. `f_boxplot(hp ~ cyl, mtcars)`) now plots only the response variable named on the LHS of the formula. Previously the LHS was ignored and a plot was generated for every numeric column in `data`.

* `f_boxplot()` with a formula referencing bare vectors (e.g. `f_boxplot(hp1 ~ cyl1)`) no longer errors with "argument 'data' is missing, with no default", and the output filename is derived from the formula variables.

* `check_lhs_is_names()` (internal LHS guard) no longer emits a misleading "Expressions on the LHS of the formula are ignored: NULL" warning when called with `formula = NULL` or with a one-sided formula. This affected any rfriend function accepting a data.frame without a formula (`f_boxplot(mtcars)`, `f_summary(mtcars)`, etc.).

* `f_summary()`, `f_scan()` and `f_outliers()` now accept a bare data.frame without requiring `columns`. When `columns` is omitted, all numeric columns in `data` are used (excluding any named in `group_vars` and, for `f_outliers()`, `id_var`). This matches the behaviour added to `f_boxplot()` in the same release and mirrors base R's `summary(mtcars)`.

* `f_scan()` no longer crashes with "Column `All Data` not found" on the second response variable when called without `group_vars`. The dummy grouping column was being added only on the first iteration of a multi-column loop.

* The print methods for `f_summary()` and `f_outliers()` now show a header naming each response variable when several are summarised. Previously, multi-column calls produced a stack of unlabelled tables.

* `f_summary()` now computes the standard error (`se`) using the number of non-missing observations rather than the full vector length. Previously a column containing `NA` values produced a standard error that was biased towards zero, because the `NA` entries were counted in the denominator `sqrt(n)`. The new confidence interval relies on the same corrected count.


# rfriend 3.0.0 (2026-04-21)

## Breaking Changes

* `f_model_comparison()` has been renamed to `f_model_compare()`. Please
  update any scripts that used the previous name.

* `f_summary()` no longer accepts unquoted column names. Columns must now be
  supplied either via a formula (e.g.
  `f_summary(disp + hp ~ gear + cyl, data = mtcars)`) or as quoted character
  names passed to the `columns` argument (e.g. `columns = c("disp", "hp")`).
  This change was required to support the new formula method.

* The `output_type` argument of file-producing functions now defaults to
  `"default"` instead of `"off"` (or `"console"`). The new `"default"` mode
  returns an S3 object and lets R decide whether to print: the object is
  auto-printed when the call is unassigned, and silent when the result is
  assigned to a variable. Set `output_type = "console"` to force immediate
  console printing regardless of assignment. Affects `f_aov()`,
  `f_kruskal_test()`, `f_glm()`, `f_chisq_test()`, `f_bestNormalize()`,
  `f_boxcox()`, and the new `f_lmer()`, `f_t_test()`, `f_wilcox_test()`,
  `f_scan()` and `f_stat_wizard()`.

* The default `transformation` in `f_aov()` is now `"boxcox"` (previously
  `"bestnormalize"`). Box-Cox is faster, easier to back-transform and
  sufficient for most ANOVA use cases.

## New Functions

* `f_lmer()` fits linear mixed-effects models using `lme4::lmer()` with
  p-values supplied by `lmerTest`, and produces a fully formatted report
  containing the fixed-effects ANOVA table, random-effects variance
  components and ICC, marginal and conditional R-squared (Nakagawa and
  Schielzeth), AIC, BIC, log-likelihood, residual and BLUP Q-Q diagnostics,
  prominent surfacing of singular-fit and convergence messages, and
  `emmeans` pairwise post hoc on factor fixed effects with compact letter
  display. Supports `output_type` of `"console"`, `"pdf"`, `"word"`,
  `"excel"` and `"rmd"`, mirroring `f_aov()` and `f_kruskal_test()`. The
  intro section explains LMM assumptions and walks the user through the
  `(1 | group)` random-effects syntax in study-design terms. Denominator
  degrees of freedom are selectable via `ddf = "Satterthwaite"` (default),
  `"Kenward-Roger"` or `"lme4"`.

* `f_t_test()` wraps `stats::t.test()` with both a formula interface
  (`y1 + y2 ~ group`, supporting multiple responses in sequence) and a
  classic vector interface. Supports one-sample, two-sample and paired
  tests, adds automated Shapiro-Wilk, Bartlett and Levene diagnostics,
  optional Box-Cox or bestNormalize transformation of non-normal responses,
  and formatted output to console, pdf, Word, Excel or R Markdown.

* `f_wilcox_test()` wraps `stats::wilcox.test()` with the same formula and
  vector interfaces as `f_t_test()`. The function explicitly labels and
  reports the Hodges-Lehmann pseudo-median (one-sample and paired) or
  location shift (two-sample), alongside descriptive sample medians, to
  avoid the common "CI for the median" mislabelling found in textbooks and
  software output.

* `f_scan()` creates a 3-panel diagnostic dashboard (density, boxplot,
  Q-Q) for one or more response columns, optionally split by up to three
  grouping variables (colour, facet wrap, facet grid). It returns a
  summary table and a Tukey-fence outlier table, and can optionally call
  `f_stat_wizard()` to append a test recommendation for each response.

* `f_long()` converts wide (Excel-style) data to long format in a single
  call, selecting measurement columns, keeping ID columns and optionally
  renaming categories. Returns an object of class `f_long` with dedicated
  `plot()` and `summary()` methods. Extra arguments are forwarded to
  `tidyr::pivot_longer()`.

* `f_stat_wizard()` (BETA) analyses your data structure from a formula and
  recommends an appropriate statistical test. It detects response type
  (binary, count, multinomial, ratio normal or non-normal), checks
  normality of residuals and homogeneity of variance, and evaluates
  whether a Box-Cox transformation would resolve non-normality. The
  recommendation is returned as ready-to-run code using the appropriate
  `rfriend` function as primary code, with a base R fallback. Supports
  `y ~ .`, interaction terms and paired or repeated-measures designs via
  `id_col`. With `run = TRUE`, the recommended function is executed
  automatically.

* `f_outliers()` scans numeric columns for outliers using Tukey's fences
  (IQR multiplier configurable via `coef`), optionally within groups.
  Returns a data frame containing only the outlier rows, adds a `row_id`
  column for traceability, and optionally exports to Excel. A formula
  interface is supported, e.g. `col1 + col2 ~ group1 + group2`.

* `f_remove_outliers()` removes rows from a data frame based on the output
  of `f_outliers()` or a custom vector of IDs or row numbers, using safe
  anti-join semantics so the original data structure is preserved.

* `df_to_table()` converts a data frame to a base R contingency table.
  The label column is auto-detected (first character or factor column, or
  meaningful `rownames()`) but can be specified explicitly. Used
  internally by `f_chisq_test()` and exported for manual use.

## New Features in Existing Functions

* Formula interfaces have been added to `f_summary()`, `f_boxplot()`,
  `f_scan()`, `f_outliers()` and `f_stat_wizard()` via S3 dispatch
  (`data.frame` and `formula` methods). This makes iterative use very
  concise. For example, `f_summary(disp + hp ~ gear + cyl, data = mtcars)`
  summarises `disp` and `hp` grouped by `gear` and `cyl`.

* `f_summary()` gained `show_skew` (Skewness, measure of asymmetry) and
  `show_kurtosis` (Excess Kurtosis, measure of tail heaviness).

* `f_aov()` gained a `force_aov` argument to run ANOVA even when at least
  one cell has n = 1 (saturated model). The default (`FALSE`) skips such
  responses with a warning, because F-statistics and p-values are
  undefined for saturated models.

* `f_corplot()` has been rewritten. The upper triangle now displays
  Pearson r, Spearman rho and Kendall tau simultaneously for every pair.
  Ordinal variables are supported via the new `ordinal_vars` argument:
  their diagonal labels are italicised and Pearson r is greyed and
  bracketed for any pair that involves them. New arguments
  `factor_select`, `factor_exclude`, `unique_num_treshold` and
  `repeats_threshold` give finer control over automatic factor detection.

* `f_aov()` and `f_glm()` post hoc summary tables now display
  back-transformed data where a transformation has been applied. A data
  summary table has also been added to both functions.

* `f_boxplot()` now integrates with `f_outliers()` and can append an
  outlier table to the report (new arguments `outliers`, `coef`,
  `limit_columns`).

* `f_chisq_test()` now uses the new `df_to_table()` helper when a data
  frame instead of table is supplied, giving clearer messages about which column was used
  as row labels.

## New S3 Methods

* New `plot()` methods for objects of class `f_kruskal_test`, `f_lmer`,
  `f_long`, `f_scan`, `f_t_test` and `f_wilcox_test`.

* New `print()` methods for `f_lmer`, `f_outliers`, `f_scan`,
  `f_stat_wizard`, `f_t_test` and `f_wilcox_test`.

* New `summary()` methods for `f_long` and `f_scan`.

* New `predict()` method for `f_boxcox`, allowing forward transformation
  of new values using a fitted `f_boxcox` object.

## Minor Changes

* The intro text and summary text of `f_aov()`, `f_kruskal_test()` and
  `f_glm()` have been reworked to be more user-friendly and consistent
  across functions.

* `f_open_file()` has been improved for Linux users.

* Formatting of Word output has been updated and is now compatible with
  LibreOffice Writer (tested on version 24.2.7.2).

* New imports: `dplyr`, `gridExtra`, `lme4`, `lmerTest`, `magrittr`,
  `png`, `rlang` and `tidyr`.

* `MASS`, `nnet`, `pbkrtest`, `testthat` (>= 3.0.0) and `tibble` have been
  added to Suggests. The package now ships a `testthat` (edition 3) test
  suite (`Config/testthat/edition: 3`).

* New internal helpers for formula handling, left-hand-side checking, safe
  Shapiro-Wilk testing and session-state management.

## Bug Fixes

* General hardening of all functions following stress testing with
  extreme combinations of input options, including malformed formulas,
  edge cases of sample size, missing data, and factor-level counts.


# rfriend 2.0.0 (2025-11-16)

## Major Changes

* **BREAKING CHANGE:** Replaced the `output_file` and `output_dir` arguments with a single `save_as` argument for all file-saving functions.

  * The `save_as` argument now controls the full save path (directory, filename and extension).

  * It accepts relative paths (e.g., `"example/filename.pdf"`) or full paths (e.g., `"c:/users/tom/docs/filename.pdf"`).

  * If a file extension (like `.pdf` or `.word`) is provided, `save_as` will override the `output_type` argument using this extension.

  * Changed the default argument from `output_type = "off"` to `output_type = "console"` for `f_aov()`, `f_kruskal_test()`, `f_glm()`, and `f_chisq_test()`. This ensures results are printed to the console by default, aligning with user expectations.

  * The arguments `show_assumptions_text` from `f_glm()`, `kruskal_assumptions_text` from `f_kruskal_test()`, `aov_assumptions_text` from `f_aov()` and `boxplot_explanation` from `f_boxplot` were all replace by the argument `intro_text` to have a short and uniform argument.

## New Features

* Added a `force_transformation` argument to `f_aov()` to allow transformations on specific response variables (e.g., `force_transformation = c("col1", "col2")`).

* The transformation name (if used) is now added to the `f_aov` summary table and included as a subscript in the `aov` call formula.

## Minor Changes

* `f_bestNormalize()` now applies a transformation even if the input data is already normal. This is to ensure transformations can be applied when the original data is normal but model residuals are not.


## Bug Fixes

* Fixed an issue where assumption violation warnings from `f_aov()` were not visible in the final output reports.

* Improved several functions to deal better with NA.

* Other general minor bug fixes.


# rfriend 1.0.0 (2025-07-16)

* Initial release to CRAN
