# Changelog

## rfriend 3.0.0 (2026-02-01)

### Major Changes

- **BREAKING CHANGE:** In order to take `formula` notation `f_summary`
  no longer accepts unquoted column names, either formula notation or
  quotes names must be used.

### New Features

New function to easily go from wide data to long format `f_long` and a
function to scan the data distribution shape, spread, outliers and
normality: `f_scan`. The new `f_stat_wizard` is still in BETA it
analyzes your data structure based on a formula and recommends the
appropriate statistical test.`f_wilcox_test` and `f_t_test` have been
added to the package. Many functions now take formula notation, making
iterative function use very simple e.g., summarize columns “disp” and
“hp” from dataset “mtcars” grouped by “gear” and “cyl”:
`f_summary(disp + hp ~ gear + cyl, data = mtcars)`. `f_summary` now
allows showing Excess Kurtosis `show_kurtosis = TRUE` (measure of
“tailedness”) and `show_skew = TRUE` Skewness (measure of asymmetry).

### Minor Changes

intro text and summary text of `f_aov`, `f_kruskall_test`, and `f_glm`
has been improved to be more user friendly. `f_aov` and `f_glm` now show
backtransformed data in post hoc summary table. A data summary table has
been added to these functions. `f_aov` now has
`transformation = "boxcox"` as default instead of `"bestnormalize"`.
`f_open_file` has been improved for linux users, formatting of `Word`
output is now compatible with `LibreOffice Writer`, tested on version
24.2.7.2. `f_corplot` has been rewritten to allow for ordinal variables
and now shows Pearson, Spearman and Kendall correlation coefficients.

### Bug Fixes

Bug fixes for all functions after stress testing by using extreme
combinations of input options.

## rfriend 2.0.0 (2025-11-16)

CRAN release: 2025-11-11

### Major Changes

- **BREAKING CHANGE:** Replaced the `output_file` and `output_dir`
  arguments with a single `save_as` argument for all file-saving
  functions.

  - The `save_as` argument now controls the full save path (directory,
    filename and extension).

  - It accepts relative paths (e.g., `"example/filename.pdf"`) or full
    paths (e.g., `"c:/users/tom/docs/filename.pdf"`).

  - If a file extension (like `.pdf` or `.word`) is provided, `save_as`
    will override the `output_type` argument using this extension.

  - Changed the default argument from `output_type = "off"` to
    `output_type = "console"` for
    [`f_aov()`](https://delde001.github.io/rfriend/reference/f_aov.md),
    [`f_kruskal_test()`](https://delde001.github.io/rfriend/reference/f_kruskal_test.md),
    [`f_glm()`](https://delde001.github.io/rfriend/reference/f_glm.md),
    and
    [`f_chisq_test()`](https://delde001.github.io/rfriend/reference/f_chisq_test.md).
    This ensures results are printed to the console by default, aligning
    with user expectations.

  - The arguments `show_assumptions_text` from
    [`f_glm()`](https://delde001.github.io/rfriend/reference/f_glm.md),
    `kruskal_assumptions_text` from
    [`f_kruskal_test()`](https://delde001.github.io/rfriend/reference/f_kruskal_test.md),
    `aov_assumptions_text` from
    [`f_aov()`](https://delde001.github.io/rfriend/reference/f_aov.md)
    and `boxplot_explanation` from `f_boxplot` were all replace by the
    argument `intro_text` to have a short and uniform argument.

### New Features

- Added a `force_transformation` argument to
  [`f_aov()`](https://delde001.github.io/rfriend/reference/f_aov.md) to
  allow transformations on specific response variables (e.g.,
  `force_transformation = c("col1", "col2")`).

- The transformation name (if used) is now added to the `f_aov` summary
  table and included as a subscript in the `aov` call formula.

### Minor Changes

- [`f_bestNormalize()`](https://delde001.github.io/rfriend/reference/f_bestNormalize.md)
  now applies a transformation even if the input data is already normal.
  This is to ensure transformations can be applied when the original
  data is normal but model residuals are not.

### Bug Fixes

- Fixed an issue where assumption violation warnings from
  [`f_aov()`](https://delde001.github.io/rfriend/reference/f_aov.md)
  were not visible in the final output reports.

- Improved several functions to deal better with NA.

- Other general minor bug fixes.

## rfriend 1.0.0 (2025-07-16)

CRAN release: 2025-07-16

- Initial release to CRAN
