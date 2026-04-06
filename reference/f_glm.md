# Perform multiple `glm()` functions with diagnostics, assumption checking, and post hoc analysis

Performs Generalized Linear Model (GLM) analysis on a given dataset with
options for diagnostics, assumption checking, and post hoc analysis.
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
  output_type = "default",
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

  Character string specifying the scale of emmeans post hoc results:
  `"response"` (back-transformed to original units, e.g. probabilities,
  counts) or `"link"` (on the linear predictor scale, e.g. log-odds).
  Default is `"response"`.

- intro_text:

  Logical. If `TRUE`, includes a short explanation about GLM assumptions
  in the output file.

- dispersion_test:

  Logical. If `TRUE`, includes a dispersion diagnostic section in the
  output: a DHARMa simulation-based test for overdispersion
  (Poisson/Binomial), the quasi-dispersion parameter (quasi-families),
  or a note explaining why the test is skipped (Bernoulli data). Default
  is `TRUE`.

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
  slash), the file is named "dataname_glm_output" in that directory. If
  an extension is provided the output format specified with option
  "output_type" will be overruled. Defaults to
  `file.path(tempdir(), "dataname_summary.pdf")`.

- save_in_wdir:

  Logical. If `TRUE`, saves the file in the working directory. Default
  is `FALSE`, this avoid unintended changes to the global environment.
  If `save_as` location is specified `save_in_wdir` is overwritten by
  `save_as`.

- close_generated_files:

  Logical. Closes open Excel or Word (NOT pdf) files before writing,
  depending on the output format. Works on Windows (taskkill), macOS
  (pkill) and Linux (pkill/soffice). Default `FALSE`. **WARNING:**
  Always save your work before using this option!!

- open_generated_files:

  Logical. If `TRUE`, Opens the generated output files ('pdf', 'Word' or
  'Excel') files depending on the output format. This to directly view
  the results after creation. Files are stored in tempdir(). Default is
  `TRUE`.

- influence_threshold:

  Numeric multiplier for the leverage threshold. Observations with hat
  values exceeding `influence_threshold * mean(hat values)` are flagged
  as high-leverage points. Default is `2`, a common rule of thumb.

- ...:

  Additional arguments passed to
  [`glm()`](https://rdrr.io/r/stats/glm.html).

## Value

An object of class 'f_glm' (a named list, one entry per response
variable) containing:

- model:

  The fitted `glm` object.

- summary:

  Output of `summary(glm_fit)`.

- drop1:

  Type II Analysis of Deviance table from
  [`stats::drop1()`](https://rdrr.io/r/stats/add1.html).

- diagnostics:

  DHARMa residual checks and hat-value based leverage diagnostics.

- posthoc:

  Estimated marginal means, pairwise comparisons, CLD letters, and
  summary table.

- sep_flag:

  Logical indicating whether complete separation was detected.

- lrt_pct_explained:

  McFadden's Pseudo-R\\^2\\.

Using the option `output_type`, it can also generate output in the form
of: R Markdown code, 'Word', 'pdf', or 'Excel' files. Includes print and
plot methods for 'f_glm' objects.

## Details

The function first checks if all specified variables are present in the
data and ensures that the response variable is numeric.

It fits a Generalized Linear Model (GLM) using the specified formula,
family, and data. Model diagnostics are performed with DHARMa
(simulation-based residual checks including a KS test, dispersion test,
and outlier test). High-leverage observations are flagged using hat
values.

Significance of each predictor is assessed via Type II Analysis of
Deviance ([`stats::drop1()`](https://rdrr.io/r/stats/add1.html)). If
significant effects are found, post hoc pairwise comparisons are
performed using estimated marginal means from `emmeans()` with the
chosen p-value adjustment method (default: Sidak). When complete
separation is detected, the function falls back to likelihood ratio test
(LRT) based pairwise comparisons, which are robust to separation.

More response variables can be added using `+` (e.g.,
`response1 + response2 ~ predictor`) to fit a sequential GLM for each
response variable, captured in one output file.

Outputs can be generated in multiple formats ("pdf", "word", "excel" and
"rmd") as specified by `output_type`. The function also closes any open
'Word' files to avoid conflicts when generating 'Word' documents. If
`output_type = "rmd"` is used it is advised to use it in a chunk with
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
# \donttest{
# GLM Binomial example with output to console
mtcars_mod <- mtcars
mtcars_mod$cyl <- as.factor(mtcars_mod$cyl)

glm_bin <- f_glm(vs ~ cyl,
                 family = binomial,
                 data = mtcars_mod,
                 output_type = "default")
print(glm_bin)
#> ==========================================
#>    GLM of response variable: vs 
#> ==========================================
#> Family: binomial  | Link: logit  | AIC: 22.263 
#> Null deviance: 43.86 on 31 df  | Residual deviance: 16.263 on 29 df
#> 
#> Coefficients:
#>               Estimate  Std. Error      z value   Pr(>|z|)
#> (Intercept)   2.302585    1.048809  2.195428745 0.02813286
#> cyl6         -2.014903    1.297433 -1.552991527 0.12042516
#> cyl8        -21.868654 2874.131054 -0.007608788 0.99392912
#> 
#> --- Type II Analysis of Deviance ---
#> Single term deletions
#> 
#> Model:
#> vs ~ cyl
#>        Df Deviance    AIC    LRT  Pr(>Chi)    
#> <none>      16.263 22.263                     
#> cyl     2   43.860 45.860 27.597 1.017e-06 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> --- Model vs. Null Model ---
#> Null deviance:      43.86  on 31 df
#> Residual deviance:  16.263  on 29 df
#> McFadden's Pseudo-R²: 0.629 
#> 
#> --- Post hoc Comparisons of: vs ---
#> _________________________________________
#>   cyl         prob           SE    asymp.LCL asymp.UCL Letter  n
#> 1   8 3.181005e-09 9.142626e-06 2.220446e-16 1.0000000      b 14
#> 2   6 5.714286e-01 1.870439e-01 1.771200e-01 0.8920012      a  7
#> 3   4 9.090909e-01 8.667842e-02 4.497464e-01 0.9918928      a 11
#> 

# GLM Binomial example with output to MS Word file
glm_bin_word <- f_glm(vs ~ cyl,
                 family = binomial,
                 data = mtcars_mod,
                 output_type = "word",
                 open_generated_files = FALSE)
#> Saving output in: /tmp/RtmpyM0xyc/mtcars_mod_glm_output.docx

# GLM Poisson example with output to rmd text
data(warpbreaks)

glm_pos <- f_glm(breaks ~ wool + tension,
                 data = warpbreaks,
                 family = poisson(link = "log"),
                 intro_text = FALSE,
                 output_type = "rmd")
cat(glm_pos$rmd)
#> 
#>    
#>   
#> 
#> # GLM of:  breaks   
#> 
#> ## Model Diagnostics of:  breaks 
#>    
#> ![](/tmp/RtmpyM0xyc/file1fa814a48b76.png)    
#>   
#> 
#> <div style="page-break-after: always;"></div>
#> \newpage
#> ## Dispersion Diagnostics of: breaks 
#> 
#> **DHARMa Dispersion Test** — Ratio of simulated vs. observed variance: **4.449**  
#> The dispersion test is **significant** (p = 0 ≤ α = 0.05), indicating **overdispersion** (more variance than the model assumes).   
#> **Recommended action:** Switch to a family that accounts for extra variance:
#>   - Counts (Poisson): use **Negative Binomial** (`MASS::glm.nb()` or `glmmTMB`).
#>   - Proportions (Binomial): use **Beta-Binomial** (`glmmTMB`).
#>   - Or use `quasipoisson` / `quasibinomial` to correct standard errors without changing the family.  
#> 
#> 
#> ## High-Leverage Observations of: breaks
#> 
#> Leverage (hat values) measures how far each observation's predictor values are from the centroid of all predictors. High-leverage points *can* disproportionately pull the fitted line, but only do so when they also have large residuals. To assess actual **influence** (leverage × residual), check Cook's distance via `plot(glm_fit, which = 4)`.  
#> 
#> **Threshold:** 2 × mean leverage = **0.1481**  
#> **No influential observations detected** above the threshold. All hat values are within acceptable range (max = 0.0827).  
#> 
#> 
#> ## Observed Descriptives Table of:  breaks ~ wool + tension   
#> 
#> -----------------------------------------------------------------------------
#> wool   tension   n   mean     sd       se      min   Q1   median   Q3   max  
#> ------ --------- --- -------- -------- ------- ----- ---- -------- ---- -----
#> A      L         9   44.556   18.098   6.033   25    26   51       54   70   
#> 
#> B      L         9   28.222   9.859    3.286   14    20   29       31   44   
#> 
#> A      M         9   24.000   8.660    2.887   12    18   21       30   36   
#> 
#> B      M         9   28.778   9.431    3.144   16    21   28       39   42   
#> 
#> A      H         9   24.556   10.273   3.424   10    18   24       28   43   
#> 
#> B      H         9   18.778   4.893    1.631   13    15   17       21   28   
#> -----------------------------------------------------------------------------
#> 
#> **TIP:** These values represent your actual observed sample characteristics.
#> Use this table for *Methods* sections or Supplementary materials (to describe the sample).
#>   
#> 
#> **CAUTION:** For statistical inference (significance letters and *p-values*) and reporting
#> main findings in the *Results* section, you **must** use the Emmeans table below.
#> 
#> 
#> ## Model Summary of:  breaks 
#>    
#> **Call:** `breaks ~ wool + tension`  
#> **Family:** poisson  |  **Link:** log  |  **AIC:** 493.056  |  **Null deviance:** 297.372 (df = 53)  |  **Residual deviance:** 210.392 (df = 50)  
#> 
#> **Coefficient Estimates** (direction and magnitude):  
#> 
#> 
#> -----------------------------------------------------
#> Term          Estimate   Std      z value   Wald p   
#>                          Error                       
#> ------------- ---------- -------- --------- ---------
#> (Intercept)   3.6920     0.0454   81.302    < 0.001  
#> 
#> woolB         -0.2060    0.0516   -3.994    < 0.001  
#> 
#> tensionM      -0.3213    0.0603   -5.332    < 0.001  
#> 
#> tensionH      -0.5185    0.0640   -8.107    < 0.001  
#> -----------------------------------------------------
#> 
#> 
#> *The 'Wald p' column is provided for completeness. For significance testing use the **Type II Analysis of Deviance** table below, which is robust to separation and more reliable for multi-predictor models.*  
#> 
#> 
#> ### Type II Analysis of Deviance of: breaks 
#> 
#> The table below tests the marginal significance of **each predictor term** via `stats::drop1()` (Type II tests). Each term is dropped from the full model in turn and tested against the model retaining all other terms — equivalent to `car::Anova(type = 2)` but using only base R. This is the GLM equivalent of the ANOVA F-table: it answers *"does this predictor improve the model?"* after accounting for all other terms. For single-predictor models this matches the coefficient z-test above; for multi-predictor models these per-term tests are the ones to report.  
#> 
#> 
#> ----------------------------------------------------
#> Term      Df   Deviance   AIC     LRT     Pr(>Chi)  
#> --------- ---- ---------- ------- ------- ----------
#> <none>    NA   210.4      493.1   NA      NA        
#> 
#> wool      1    226.4      507.1   16.04   < 0.001   
#> 
#> tension   2    281.3      560.0   70.94   < 0.001   
#> ----------------------------------------------------
#> 
#> 
#> ### Model vs. Null Model Comparison of: breaks 
#> 
#> The **null deviance** (intercept-only model) is 297.372 on 53 df. The **residual deviance** (fitted model) is 210.392 on 50 df.  
#> 
#> 
#> **McFadden's Pseudo-R²:** 0.292  
#> 
#> *McFadden's Pseudo-R² = 1 − (Residual deviance / Null deviance). It measures how much the model improves over a null (intercept-only) model, on a 0–1 scale. Unlike R² in linear regression, it is **not** a proportion of variance explained; values are typically lower — 0.2–0.4 is already considered an excellent fit for GLMs.*  
#> 
#> &nbsp;  
#> 
#> **χ² likelihood ratio test** vs. null model: p = **< 0.001**. The model fits **significantly better** than the null model (α = 0.05), meaning at least one predictor contributes to explaining the response.  
#> 
#> 
#> --------------------------------------------------------
#> Term      Df   Deviance   Resid     Resid     Pr(>Chi)  
#>                           Df        Dev                 
#> --------- ---- ---------- --------- --------- ----------
#> NULL      NA   NA         53        297.4     NA        
#> 
#> wool      1    16.04      52        281.3     < 0.001   
#> 
#> tension   2    70.94      50        210.4     < 0.001   
#> --------------------------------------------------------
#> 
#> 
#> *⚠ This table uses **sequential (Type I) tests** — per-term p-values depend on the order predictors enter the model. For per-term significance, use the **Type II Analysis of Deviance** (`drop1`) table above, which tests each term after accounting for all others.*  
#> 
#> 
#> <div style="page-break-after: always;"></div>
#> \newpage
#> 
#> 
#> ## Model post hoc Analysis (Estimated Marginal Means) of:  breaks 
#>    
#> The table below shows the  Estimated Marginal Rates  (`emmeans` package). These are  back-transformed, model-based values on the response scale  for  breaks . Unlike raw averages, these values correct
#>       for unbalanced designs and reflect the statistical model. Groups in the *"Letters"* column sharing the same letter are **not** statistically different (p >  0.05 ).
#>       Groups with *different* letters are significantly different.
#>    
#>   
#> #### Publication & Reporting Tips
#> * For main results showing significant differences, prioritize reporting these Estimated Marginal Rates  and their *Confidence Intervals (CIs)* rather than raw means.
#>  * Figures should ideally overlay these  Estimated Marginal Rates  (and error bars) on top of the raw data points.
#> 
#> #### Technical Notes (Explanations)
#>  * **Back-Transformation:** The values (Means and CIs) have been converted from the model's internal scale back to the original units. 
#> * **Asymmetric Intervals:** You may notice that the Confidence Intervals are not symmetrical around the Estimated Marginal Rates. This is normal! Because of the non-linear link function, the uncertainty is often 'stretched' in one direction. 
#> * **SE vs CI:** We recommend focusing on **Confidence Intervals** for inference rather than Standard Errors (SE).
#>   
#> &nbsp;
#>   
#> 
#> **Post Hoc Summary Table** of glm call:  breaks ~ wool + tension   
#> 
#> ------------------------------------------------------------------
#> wool   tension   rate     SE      asymp     asymp     Letter   n  
#>                                   LCL       UCL                   
#> ------ --------- -------- ------- --------- --------- -------- ---
#> B      H         19.443   1.129   16.688    22.653    a        9  
#> 
#> B      M         23.681   1.278   20.545    27.294    ab       9  
#> 
#> A      H         23.890   1.330   20.635    27.659    bc       9  
#> 
#> A      M         29.097   1.495   25.418    33.310    cd       9  
#> 
#> B      L         32.654   1.578   28.756    37.081    d        9  
#> 
#> A      L         40.124   1.822   35.605    45.216    e        9  
#> ------------------------------------------------------------------
#> 
#> Degrees of freedom: Inf  
#> Confidence level used: 0.95  
#> Conf-level adjustment: sidak method for 6 estimates  
#> Intervals are back-transformed from the log scale  
#> P value adjustment: sidak method for 15 tests  
#> Tests are performed on the log scale  
#> significance level used: $\alpha$ = 0.05  
#> **NOTE:** If two or more means share the same grouping symbol,
#>       then we cannot show them to be different.
#>       But we also did not show them to be the same.
# }
```
