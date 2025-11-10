# rfriend 2.0.0 (2025-11-10)

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
