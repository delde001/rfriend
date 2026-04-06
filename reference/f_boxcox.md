# f_boxcox: A User-Friendly Box-Cox Transformation

Performs a Box-Cox transformation on a dataset to stabilize variance and
make the data more normally distributed. It also provides diagnostic
plots and tests for normality. The transformation is based on code of
MASS/R/boxcox.R. The function prints \\\lambda\\ to the console and
returns (output) the transformed data set.

## Usage

``` r
f_boxcox(
  data = data,
  digits = 3,
  range = c(-2, 2),
  plots = NULL,
  transform.data = TRUE,
  eps = 1/50,
  xlab = expression(lambda),
  ylab = "log-Likelihood",
  alpha = 0.05,
  open_generated_files = TRUE,
  close_generated_files = FALSE,
  output_type = "default",
  save_as = NULL,
  save_in_wdir = FALSE,
  ...
)
```

## Arguments

- data:

  A numeric vector or a data frame with a single numeric column. The
  data to be transformed.

- digits:

  Numeric. Determines the accuracy of the estimate for lambda. Higher
  values increase computation time. Defaults to `3`.

- range:

  A numeric vector of length 2 defining the search interval for lambda.
  Defaults to `c(-2, 2)`.

- plots:

  Logical. If `TRUE`, plots log-likelihood of the Box-Cox
  transformation, Histograms and Q-Q plots of the original and
  transformed data. Default is `FALSE`.

- transform.data:

  Logical. If `TRUE`, returns the transformed data. Default is `TRUE`.

- eps:

  A small positive value used to determine when to switch from the power
  transformation to the log transformation for numerical stability.
  Default is `1/50`.

- xlab:

  Character string. Label for the x-axis in plots. Default is an
  expression object representing \\\lambda\\.

- ylab:

  Character string. Label for the y-axis in plots. Default is
  "log-Likelihood".

- alpha:

  Numeric. Significance level for the Shapiro-Wilk test of normality.
  Default is `0.05`.

- open_generated_files:

  Logical. If `TRUE`, opens the generated output files ('pdf', 'Word' or
  'Excel') files depending on the output format. This to directly view
  the results after creation. Files are stored in tempdir(). Default is
  `TRUE`.

- close_generated_files:

  Logical. Closes open Excel or Word (NOT pdf) files before writing,
  depending on the output format. Works on Windows (taskkill), macOS
  (pkill) and Linux (pkill/soffice). Default `FALSE`. **WARNING:**
  Always save your work before using this option!!

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
  slash), the file is named "data_name_aov_output" in that directory. If
  an extension is provided the output format specified with option
  "output_type" will be overruled. Defaults to
  `file.path(tempdir(), "data_name_summary.pdf")`.

- save_in_wdir:

  Logical. If `TRUE`, saves the file in the working directory. Default
  is `FALSE`, this avoid unintended changes to the global environment.
  If `save_as` location is specified `save_in_wdir` is overwritten by
  `save_as`.

- ...:

  Additional arguments passed to plotting functions.

## Value

An object of class 'f_boxcox' containing, among others, results from the
boxcox transformation, lambda, the input data, transformed data,
Shapiro-Wilk test on original and transformed data. Using the option
"output_type", it can also generate output in the form of: R Markdown
code, 'Word', or 'pdf' files. Includes print and plot methods for
'f_boxcox' objects.

## Details

The function uses the following formula for transformation: \$\$
y(\lambda) = \begin{cases} \frac{y^\lambda - 1}{\lambda}, & \lambda \neq
0 \\ \log(y), & \lambda = 0 \end{cases} \$\$

where (\\y\\) is the data being transformed, and (\\\lambda\\) the
transformation parameter, which is estimated from the data using maximum
likelihood. The function computes the Box-Cox transformation for a range
of \\\lambda\\ values and identifies the \\\lambda\\ that maximizes the
log-likelihood function. The beauty of this transformation is that, it
checks suitability of many of the common transformations in one run.
Examples of most common transformations and their \\\lambda\\ value is
given below:

|                       |                        |
|-----------------------|------------------------|
| **\\\lambda\\-Value** | **Transformation**     |
| **———————–**          | **———————–**           |
| -2                    | \\\frac{1}{x^2}\\      |
| -1                    | \\\frac{1}{x} \\       |
| -0.5                  | \\\frac{1}{\sqrt{x}}\\ |
| 0                     | \\log(x)\\             |
| 0.5                   | \\\sqrt{x}\\           |
| 1                     | \\x\\                  |
| 2                     | \\x^2\\                |
| **———————–**          | **———————–**           |

If the estimated transformation parameter closely aligns with one of the
values listed in the previous table, it is generally advisable to select
the table value rather than the precise estimated value. This approach
simplifies interpretation and practical application.

The function provides diagnostic plots: a plot of log-likelihood against
\\\lambda\\ values and a Q-Q plot of the transformed data.It also
performs a Shapiro-Wilk test for normality on the transformed data if
the sample size is less than or equal to 5000.

**Note**: For sample sizes greater than 5000, Shapiro-Wilk test results
are not provided due to limitations in its applicability.

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

## References

The core of calculating \\\lambda\\ and the plotting was taken from:  
file MASS/R/boxcox.R copyright (C) 1994-2004 W. N. Venables and B. D.
Ripley

- <https://r-coder.com/box-cox-transformation-r/>

- <https://CRAN.R-project.org/package=MASS>

Some code to present the result was taken and modified from file:  
rcompanion/R/transformTukey.r. (Developed by Salvatore Mangiafico)

- <https://rcompanion.org/handbook/I_12.html>

The explanation on BoxCox transformation provided here was provided by
r-coder:

- <https://r-coder.com/box-cox-transformation-r/>

## See also

[`boxcox`](https://CRAN.R-project.org/package=MASS)

## Author

- Sander H. van Delden <plantmind@proton.me>

- Salvatore Mangiafico, <mangiafico@njaes.rutgers.edu>

- W. N. Venables and B. D. Ripley

## Examples

``` r
# Create non-normal data in a data.frame or vector.
df   <- data.frame(values = rlnorm(100, meanlog = 0, sdlog = 1))

# Store the transformation in object "bc".
bc <- f_boxcox(df$values)

# Print lambda and Shaprio.
print(bc)
#> Box-Cox
#> --------
#> According to the Shapiro-Wilk test ( 3.654e-18  <  0.05 ) original data is:
#>  NOT normally distributed. Transformation will be applied...   
#>   
#> Formula used for transformation:   
#> { (x^λ - 1) / λ } if λ != 0   
#> { log(x)        } if λ == 0   
#>    
#> Box-Cox Transformation λ = -0.218   
#> According to the Shapiro-Wilk test ( 0.9482  >  0.05 ) data is
#>  normally distributed after transformation.

# Plot the QQ plots, Histograms and Lambda Log-Likelihood estimation.
plot(bc)



# Or Directly use the transformed data from the f_boxcox object.
df$values_transformed <- f_boxcox(df$values)$transformed_data
print(df$values_transformed)
#>   [1] -0.76836962  0.24982329 -0.25344581 -0.36104715 -1.05752153 -0.04524945
#>   [7] -0.85605638 -2.01153677 -0.39642947  0.83279174 -0.61298563  0.56939813
#>  [13] -1.93991732 -0.05589983  0.49107995  0.29148062  0.10446824 -0.68760902
#>  [19] -0.93349490 -1.14745718  0.11615077 -1.05242453 -0.51774851 -0.26337567
#>  [25]  1.51832008 -0.70055422  0.22944922  0.07730210 -1.07013394 -0.07186522
#>  [31]  1.23920626  0.42999520  0.04104816 -0.44256509 -2.58974817  1.00262116
#>  [37] -1.71996781  0.68335120  1.56165817 -1.69698357  0.65073773 -0.26983581
#>  [43] -1.87515923 -1.79469238 -1.91669919 -0.56284982 -1.72150177  0.63881943
#>  [49]  1.68505058 -1.48572345  0.72381189  0.70803331  0.32045870 -1.12779843
#>  [55] -0.12102151 -0.28914240  0.52981227 -0.38797585  0.87994431 -0.39029967
#>  [61]  0.94065117 -1.17885675 -1.45024768  2.32410585 -0.43638557  0.28873988
#>  [67]  0.59437478 -0.51021237  0.48880663  0.35451581 -0.22051696  0.06483054
#>  [73] -0.03419407  1.70292673 -0.80460200 -1.23800949  0.03763318  0.30020644
#>  [79]  0.41639670 -0.48204832 -1.19666953  1.10418433 -0.36332136 -0.95255330
#>  [85] -0.24247067 -0.20147501  0.98584623  0.08395942  0.69533761 -0.52747821
#>  [91]  0.20950996 -0.33645277  0.09361508 -0.98872000 -1.51727534  1.61921718
#>  [97]  0.56303822 -1.43856644 -0.65375005 -1.35275934

```
