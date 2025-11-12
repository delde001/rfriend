# Perform multiple Kruskal-Wallis tests with a user-friendly output file, do data inspection and Dunn's test (of 'rstatix') as post hoc.

Performs the Kruskal-Wallis rank sum test to assess whether there are
statistically significant differences between three or more independent
groups. It provides detailed outputs, including plots, assumption
checks, and post-hoc analyses using Dunn's test. Results can be saved in
various formats ('pdf', 'Word', 'Excel', or console only) with
customizable output options.

## Usage

``` r
f_kruskal_test(
  formula,
  data = NULL,
  plot = TRUE,
  alpha = 0.05,
  output_type = "off",
  save_as = NULL,
  save_in_wdir = FALSE,
  intro_text = TRUE,
  adjust = "bonferroni",
  close_generated_files = FALSE,
  open_generated_files = TRUE
)
```

## Arguments

- formula:

  A formula specifying the response and predictor variable (e.g.,
  `response ~ predictor)`. more response variables and predictors can be
  added using `-` or `+` (e.g.,
  `response1 + response2 ~ predictor1 + predictor2)`. The function
  iterates through these combinations or response and predictors,
  because the Kruskal-Wallis test itself only allows one response and
  one predictor combination to be tested simultaneously.

- data:

  A `data.frame` containing the variables referenced in the formula.

- plot:

  Logical. If `TRUE`, generates plots (e.g., density plots and boxplots)
  in the output files. Default is `TRUE`.

- alpha:

  Numeric. The significance level for the Kruskal-Wallis test and Dunn's
  test. Default is `0.05`.

- output_type:

  Character string specifying the output format: `"pdf"`, `"word"`,
  `"excel"`, `"rmd"`, `"console"` or `"off"` (no file generated). The
  option `"console"` forces output to be printed, the option `"rmd"`
  saves rmd code in the output object not in a file. Default is `"off"`.

- save_as:

  Character string specifying the output file path (without extension).
  If a full path is provided, output is saved to that location. If only
  a filename is given, the file is saved in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). If only a
  directory is specified (providing an existing directory with trailing
  slash), the file is named "dataname_Kruskal_Wallis_output" in that
  directory. If an extension is provided the output format specified
  with option "output_type" will be overruled. Defaults to
  `file.path(tempdir(), "dataname_summary.pdf")`.

- save_in_wdir:

  Logical. If `TRUE`, saves the file in the working directory. Default
  is `FALSE`, this avoid unintended changes to the global environment.
  If `save_as` location is specified `save_in_wdir` is overwritten by
  `save_as`.

- intro_text:

  Logical. If `TRUE`, includes a section about Kruskal-Wallis test
  assumptions in the output document. Default is `TRUE`.

- adjust:

  Character string. Adjustment method for pairwise comparisons in Dunn's
  test. Options include
  `"holm", "hommel", "bonferroni", "sidak", "hs", "hochberg", "bh", "by", "fdr"`
  or `"none"`. Default is `"bonferroni"`, if you don't want to adjust
  the p value (not recommended), use `p.adjust.method = "none"`.

- close_generated_files:

  Logical. If `TRUE`, closes open 'Excel' or 'Word' files depending on
  the output format. This to be able to save the newly generated file by
  the [`f_aov()`](https://delde001.github.io/rfriend/reference/f_aov.md)
  function. 'Pdf' files should also be closed before using the function
  and cannot be automatically closed. Default is `FALSE`.

- open_generated_files:

  Logical. If `TRUE`, Opens the generated output files ('pdf', 'Word' or
  'Excel') files depending on the output format. This to directly view
  the results after creation. Files are stored in tempdir(). Default is
  `TRUE`.

## Value

An object of class 'f_kruskal_test' containing:

- Kruskal-Wallis test results for each combination of response and
  predictor variables.

- Dunn's test analysis results (if applicable).

- Summary tables with compact letter displays for significant group
  differences.

Using the option `output_type`, it can also generate output in the form
of: R Markdown code, 'Word', 'pdf', or 'Excel' files. Includes print and
plot methods for 'f_kruskal_test' objects.

## Details

This function offers a comprehensive workflow for non-parametric
analysis using the Kruskal-Wallis test:

- Assumption Checks: Optionally includes a summary of assumptions in the
  output.

- Visualization: Generates density plots and boxplots to visualize group
  distributions.

- Post-hoc Analysis: Conducts Dunn's test with specified correction
  methods if significant differences are found.

———–  

Output files are generated in the format specified by `output_type =`
and saved to the working directory, options are `"pdf", "word"` or
`"excel"`. If `output_type = "rmd"` is used it is adviced to use it in a
chunk with {r, echo=FALSE, results='asis'}

This function requires
\[Pandoc\](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3
or higher), a universal document converter.

- **Windows:** Install Pandoc and ensure the installation folder  
  (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your
  system PATH.

- **macOS:** If using Homebrew, Pandoc is typically installed in
  "/usr/local/bin". Alternatively, download the .pkg installer and
  verify that the binary’s location is in your PATH.

- **Linux:** Install Pandoc through your distribution’s package manager
  (commonly installed in "/usr/bin" or "/usr/local/bin") or manually,
  and ensure the directory containing Pandoc is in your PATH.

- If Pandoc is not found, this function may not work as intended.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Example usage:
data(iris)

# Perform Kruskal-Wallis test on Sepal.Length and Sepal.Width by Species
# with "holm" correction for posthoc dunn_test, without showing the output.
output <- f_kruskal_test(
               Sepal.Width + Sepal.Length ~ Species,
               data = iris,
               plot = FALSE,
               output_type = "word",
               adjust = "holm",
               open_generated_files = FALSE
               )
#> Saving output in: /tmp/RtmpDUIw9V/iris_Kruskal_Wallis_output.docx

# Save Kruskal-Wallis test and posthoc to Excel sheets: Sepal.Width and Sepal.Length.
f_kruskal_out <- f_kruskal_test(
                     Sepal.Width + Sepal.Length ~ Species,
                     data = iris,
                     plot = FALSE,
                     output_type = "excel",
                     adjust = "holm",
                     open_generated_files = FALSE
                     )
#> Saving output in: /tmp/RtmpDUIw9V/iris_Kruskal_Wallis_output.xlsx
```
