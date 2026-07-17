# Perform multiple Friedman rank sum tests with a user-friendly output file, do data inspection and pairwise Wilcoxon signed-rank tests as post hoc.

Performs the Friedman rank sum test to assess whether there are
statistically significant differences in the distributions (mean ranks)
of a response measured under three or more related conditions (groups)
within the same blocks (e.g. repeated measures on the same subject, or
matched sets). It provides detailed outputs, including plots, assumption
checks, an effect size (Kendall's W) and post hoc analyses using
pairwise paired Wilcoxon signed-rank tests. Results can be saved in
various formats ('pdf', 'Word', 'Excel', or console only) with
customizable output options.

## Usage

``` r
f_friedman(
  formula,
  data = NULL,
  plot = TRUE,
  alpha = 0.05,
  output_type = "default",
  save_as = NULL,
  save_in_wdir = FALSE,
  intro_text = TRUE,
  adjust = "bonferroni",
  close_generated_files = FALSE,
  open_generated_files = interactive(),
  ...
)
```

## Arguments

- formula:

  A formula specifying the response, the group (treatment) and the
  blocking variable, in the form `response ~ group | block`. The part
  before the `|` is the group/treatment variable whose levels are being
  compared; the part after the `|` is the block (e.g. subject or matched
  set) within which the groups are related. More response variables can
  be tested in sequence using `+` on the left-hand side (e.g.
  `response1 + response2 ~ group | block`). The function iterates
  through the responses, because the Friedman test itself only allows
  one response at a time.

- data:

  A `data.frame` containing the variables referenced in the formula.

- plot:

  Logical. If `TRUE`, generates two plots (a within-block trace plot
  with median trend line, and a boxplot with compact letter display) in
  the output files. Default is `TRUE`.

- alpha:

  Numeric. The significance level for the Friedman test and the pairwise
  post hoc tests. Default is `0.05`.

- output_type:

  Character string specifying the output format. Default is `"default"`.

  - `"default"`: Returns the object and lets R decide whether to print;
    auto-prints if unassigned, silent if assigned to a variable. Use
    `print(result)` or `plot(result)` to display the returned object.

  - `"console"`: Forces immediate printing to the console regardless of
    object assignment.

  - `"pdf"`, `"word"`, `"excel"`: Saves results to a file of the
    corresponding format. See `save_as`, `save_in_wdir`, and
    `open_generated_files` for file path and opening behavior.

  - `"rmd"`: Stores the raw markdown string inside the returned object
    for use in R Markdown documents.

- save_as:

  Character string specifying the output file path (without extension).
  If a full path is provided, output is saved to that location. If only
  a filename is given, the file is saved in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html). If only a
  directory is specified (providing an existing directory with trailing
  slash), the file is named "dataname_Friedman_output" in that
  directory. If an extension is provided the output format specified
  with option "output_type" will be overruled. Defaults to
  `file.path(tempdir(), "dataname_Friedman_output.pdf")`.

- save_in_wdir:

  Logical. If `TRUE`, saves the file in the working directory. Default
  is `FALSE`, this avoid unintended changes to the global environment.
  If `save_as` location is specified `save_in_wdir` is overwritten by
  `save_as`.

- intro_text:

  Logical. If `TRUE`, includes a section about Friedman test assumptions
  in the output document. Default is `TRUE`.

- adjust:

  Character string. Adjustment method for the pairwise post hoc Wilcoxon
  signed-rank comparisons. Options include
  `"holm", "hommel", "bonferroni", "hochberg", "bh", "by", "fdr"` or
  `"none"`. Default is `"bonferroni"`; if you do not want to adjust the
  p-value (not recommended) use `adjust = "none"`.

- close_generated_files:

  Logical. Closes open Excel or Word (NOT pdf) files before writing,
  depending on the output format. Works on Windows (taskkill), macOS
  (pkill) and Linux (pkill/soffice). Default `FALSE`. **WARNING:**
  Always save your work before using this option!!

- open_generated_files:

  Logical. Whether to open the generated output files after creation.
  Defaults to `TRUE` in an interactive R session and `FALSE` otherwise
  (e.g. in scripts or automated pipelines). Set to `TRUE` or `FALSE` to
  override this behaviour explicitly.

- ...:

  Additional arguments forwarded to
  [`friedman.test`](https://rdrr.io/r/stats/friedman.test.html).

## Value

An object of class 'f_friedman' (a named list, one entry per response)
containing:

- friedman.test:

  The `htest` object from
  [`friedman.test()`](https://rdrr.io/r/stats/friedman.test.html).

- effect_size:

  Data frame with Kendall's W effect size from
  [`rstatix::friedman_effsize()`](https://rpkgs.datanovia.com/rstatix/reference/friedman_effsize.html).

- posthoc_test:

  Data frame of pairwise paired Wilcoxon signed-rank results from
  [`rstatix::wilcox_test()`](https://rpkgs.datanovia.com/rstatix/reference/wilcox_test.html).

- summary_table:

  Descriptive statistics with compact letter display (Letters column).

- alpha:

  The significance level used.

- adjust:

  The p-value adjustment method used.

- distributions:

  ggplot within-block trace plot with grey spaghetti lines, raw
  group-coloured points, and a black median trend line on top (if
  `plot = TRUE`).

- Boxplot:

  ggplot boxplot with jittered raw points and compact-letter-display
  letters from the post hoc test (if `plot = TRUE`).

Using the option `output_type`, it can also generate output in the form
of: R Markdown code, 'Word', 'pdf', or 'Excel' files. Includes print and
plot methods for 'f_friedman' objects.

## Details

This function offers a workflow for non-parametric analysis of an
unreplicated complete block design using the Friedman test:

- Design check: verifies that the data form an unreplicated complete
  block design (exactly one observation per group-by-block combination)
  and fails with a clear, actionable message if not.

- Assumption checks: optionally includes a summary of assumptions in the
  output.

- Visualization: generates a within-block trace plot with a median trend
  line (to reveal the paired structure and central trend) and a separate
  boxplot with compact-letter-display letters from the post hoc test (to
  show marginal distribution shape and communicate which groups differ).

- Effect size: reports Kendall's W.

- Post hoc analysis: conducts pairwise paired Wilcoxon signed-rank tests
  with the specified correction method if a significant difference is
  found.

———–  

Output files are generated in the format specified by `output_type =`
and saved to the working directory, options are `"pdf", "word"` or
`"excel"`. If `output_type = "rmd"` is used it is advised to use it in a
chunk with {r, echo=FALSE, results='asis'}

This function requires
\[Pandoc\](https://github.com/jgm/pandoc/releases/tag) (version 1.12.3
or higher), a universal document converter.

- **Windows:** Install Pandoc and ensure the installation folder  
  (e.g., "C:/Users/your_username/AppData/Local/Pandoc") is added to your
  system PATH.

- **macOS:** If using Homebrew, Pandoc is typically installed in
  "/usr/local/bin". Alternatively, download the .pkg installer and
  verify that the binary's location is in your PATH.

- **Linux:** Install Pandoc through your distribution's package manager
  (commonly installed in "/usr/bin" or "/usr/local/bin") or manually,
  and ensure the directory containing Pandoc is in your PATH.

- If Pandoc is not found, this function may not work as intended.

## Multiple Testing Across Response Variables

When several response variables are analysed in a single call (e.g.
`y1 + y2 + y3 ~ group | block`), each Friedman test is an independent
null-hypothesis test at level `alpha`. The post hoc adjustment (e.g.
`adjust = "bonferroni"`) only controls the family-wise error rate
**within** one test (across the pairwise comparisons for that response).
It does **not** protect against the inflation of Type I error **across**
the set of responses.

**Practical implication:** With \\k\\ independent response variables all
tested at \\\alpha = 0.05\\, the probability of obtaining at least one
false positive is \\1-(1-0.05)^k\\, which reaches ~40% for \\k = 10\\.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Example usage:
# Build a small repeated-measures (unreplicated complete block) dataset:
# each subject (block) is measured under three conditions (group).
set.seed(1)
df_rm <- data.frame(
  subject   = factor(rep(1:10, each = 3)),
  condition = factor(rep(c("A", "B", "C"), times = 10)),
  score     = c(rbind(rnorm(10, 4), rnorm(10, 6), rnorm(10, 8)))
)

# Perform a Friedman test of score across condition within subject,
# with "holm" correction for the pairwise post hoc test.
output <- f_friedman(
               score ~ condition | subject,
               data = df_rm,
               plot = FALSE,
               output_type = "word",
               adjust = "holm"
               )
#> Saving output in: /tmp/RtmplsKqN3/df_rm_Friedman_output.docx
```
