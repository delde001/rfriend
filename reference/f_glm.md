# Perform multiple `glm()` functions with diagnostics, assumption checking, and post-hoc analysis

Performs Generalized Linear Model (GLM) analysis on a given dataset with
options for diagnostics, assumption checking, and post-hoc analysis.
Several response parameters can be analyzed in sequence and the
generated output can be in various formats ('Word', 'pdf', 'Excel').

## Usage

``` r
f_glm(
  formula,
  family = gaussian(),
  data = NULL,
  diagnostic_plots = TRUE,
  alpha = 0.05,
  adjust = "sidak",
  type = "response",
  intro_text = TRUE,
  dispersion_test = TRUE,
  output_type = "off",
  save_as = NULL,
  save_in_wdir = FALSE,
  close_generated_files = FALSE,
  open_generated_files = TRUE,
  influence_threshold = 2,
  ...
)
```

## Arguments

- formula:

  A formula specifying the model to be fitted. More response variables
  can be added using `-` or `+` (e.g.,
  `response1 + response2 ~ predictor`) to do a sequential GLM for each
  response parameter.

- family:

  The error distribution and link function to be used in the model
  (default: gaussian()). This can be a character string naming a family
  function, a family function or the result of a call to a family
  function. (See [`family`](https://rdrr.io/r/stats/family.html) for
  details of family functions.)

- data:

  A data frame containing the variables in the model.

- diagnostic_plots:

  Logical. If `TRUE`, plots are included in the output files.

- alpha:

  Numeric. Significance level for tests. Default is `0.05`.

- adjust:

  Character string specifying the method used to adjust p-values for
  multiple comparisons. Available methods include:

  "tukey"

  :   Tukey's Honest Significant Difference method

  "sidak"

  :   Šidák correction

  "bonferroni"

  :   Bonferroni correction

  "none"

  :   No adjustment

  "fdr"

  :   False Discovery Rate adjustment

  Default is `"sidak"`.

- type:

  specifying the scale on which the emmeans posthoc results are
  presented, e.g. "link" to show results on the scale for which the
  variables are linear and "response" when you want to back transform
  the data to interpret results in the units of your original data
  (e.g., probabilities, counts, or untransformed measurements). Default
  is `"response"`.

- intro_text:

  Logical. If `TRUE`, includes a short explanation about GLM assumptions
  in the output file.

- dispersion_test:

  Logical for overdispersion test (default: TRUE).

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
  slash), the file is named "dataname_glm_output" in that directory. If
  an extension is provided the output format specified with option
  "output_type" will be overruled. Defaults to
  `file.path(tempdir(), "dataname_summary.pdf")`.

- save_in_wdir:

  Logical. If `TRUE`, saves the file in the working directory.

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

- influence_threshold:

  Leverage threshold (default: 2).

- ...:

  Additional arguments passed to
  [`glm()`](https://rdrr.io/r/stats/glm.html).

## Value

An object of class 'f_glm' containing results from
[`glm()`](https://rdrr.io/r/stats/glm.html), diagnostics, and post-hoc
tests. Using the option "output_type", it can also generate output in
the form of: R Markdown code, 'Word', 'pdf', or 'Excel' files. Includes
print and plot methods for 'f_glm' objects.

## Details

The function first checks if all specified variables are present in the
data and ensures that the response variable is numeric.

It performs Analysis of Variance (ANOVA) using the specified formula and
data. If `shapiro = TRUE`, it checks for normality of residuals using
the Shapiro-Wilk test and optionally (`transformation = TRUE`) applies a
data transformation if residuals are not normal.

If significant differences are found in ANOVA, it proceeds with post hoc
tests using estimated marginal means from `emmeans()` and Sidak
adjustment (or another option of `adjust =`.

More response variables can be added using `-` or `+` (e.g.,
`response1 + response2 ~ predictor`) to do a sequential
[`aov()`](https://rdrr.io/r/stats/aov.html) for each response parameter
captured in one output file.

Outputs can be generated in multiple formats ("pdf", "word", "excel" and
"rmd") as specified by `output_type`. The function also closes any open
'Word' files to avoid conflicts when generating 'Word' documents. If
`output_type = "rmd"` is used it is adviced to use it in a chunk with
{r, echo=FALSE, results='asis'}

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
# GLM Binomial example with output to console and MS Word file
mtcars_mod <- mtcars
mtcars_mod$cyl <- as.factor(mtcars_mod$cyl)

glm_bin <- f_glm(vs ~ cyl,
                 family = binomial,
                 data = mtcars_mod,
                 output_type = "word",
                 # Do not automatically open the 'Word' file (Default is to open the file)
                 open_generated_files = FALSE)
#> Saving output in: /tmp/RtmpDUIw9V/mtcars_mod_glm_output.docx
print(glm_bin)
#> ==========================================
#>    GLM of repsone variable: vs 
#> ==========================================
#> 
#> Call:
#> stats::glm(formula = vs ~ cyl, family = family, data = "mtcars_mod")
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)  
#> (Intercept)    2.303      1.049   2.195   0.0281 *
#> cyl6          -2.015      1.297  -1.553   0.1204  
#> cyl8         -21.869   2874.131  -0.008   0.9939  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 43.860  on 31  degrees of freedom
#> Residual deviance: 16.263  on 29  degrees of freedom
#> AIC: 22.263
#> 
#> Number of Fisher Scoring iterations: 18
#> 
#> 
#> Post-hoc Comparisons of: vs 
#> _________________________________________
#>  cyl  prob       SE  df asymp.LCL asymp.UCL .group
#>  8   0.000 9.14e-06 Inf     0.000     1.000  a    
#>  6   0.571 1.87e-01 Inf     0.177     0.892  a    
#>  4   0.909 8.67e-02 Inf     0.450     0.992  a    
#> 
#> Confidence level used: 0.95 
#> Conf-level adjustment: sidak method for 3 estimates 
#> Intervals are back-transformed from the logit scale 
#> P value adjustment: sidak method for 3 tests 
#> Tests are performed on the log odds ratio scale 
#> significance level used: alpha = 0.05 
#> NOTE: If two or more means share the same grouping symbol,
#>       then we cannot show them to be different.
#>       But we also did not show them to be the same. 

# \donttest{
# GLM Poisson example with output to rmd text
data(warpbreaks)

glm_pos <- f_glm(breaks ~ wool + tension,
                 data = warpbreaks,
                 family = poisson(link = "log"),
                 intro_text = FALSE,
                 output_type = "rmd")
cat(cat(glm_pos$rmd))
#> 
#>    
#>   
#> 
#> # GLM of:  breaks   
#> 
#> ## Model Diagnostics of:  breaks 
#>    
#> ![](/tmp/RtmpDUIw9V/file209627789f07.png)    
#>   
#> 
#> <div style="page-break-after: always;"></div>
#> \newpage
#> 
#> ## Model Summary of:  breaks 
#>    
#> 
#> ```r
#> 
#> Call:
#> stats::glm(formula = breaks ~ wool + tension, family = family, 
#>     data = "warpbreaks")
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  3.69196    0.04541  81.302  < 2e-16 ***
#> woolB       -0.20599    0.05157  -3.994 6.49e-05 ***
#> tensionM    -0.32132    0.06027  -5.332 9.73e-08 ***
#> tensionH    -0.51849    0.06396  -8.107 5.21e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> (Dispersion parameter for poisson family taken to be 1)
#> 
#>     Null deviance: 297.37  on 53  degrees of freedom
#> Residual deviance: 210.39  on 50  degrees of freedom
#> AIC: 493.06
#> 
#> Number of Fisher Scoring iterations: 4
#> 
#> ```
#> 
#> 
#> 
#> ## Model Post-hoc Analysis of:  breaks 
#>    
#> 
#> ```r
#>  wool tension rate   SE  df asymp.LCL asymp.UCL .group
#>  B    H       19.4 1.13 Inf      16.7      22.7  a    
#>  B    M       23.7 1.28 Inf      20.5      27.3  ab   
#>  A    H       23.9 1.33 Inf      20.6      27.7   bc  
#>  A    M       29.1 1.50 Inf      25.4      33.3    cd 
#>  B    L       32.7 1.58 Inf      28.8      37.1     d 
#>  A    L       40.1 1.82 Inf      35.6      45.2      e
#> 
#> Confidence level used: 0.95 
#> Conf-level adjustment: sidak method for 6 estimates 
#> Intervals are back-transformed from the log scale 
#> P value adjustment: sidak method for 15 tests 
#> Tests are performed on the log scale 
#> significance level used: alpha = 0.05 
#> NOTE: If two or more means share the same grouping symbol,
#>       then we cannot show them to be different.
#>       But we also did not show them to be the same. 
#> ```
# }
```
