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
