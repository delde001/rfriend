# Compare Two Statistical Models

Compares two statistical models by calculating key metrics such as AIC,
BIC, log-likelihood, R-squared, and others. Supports comparison of
nested models using ANOVA tests.

## Usage

``` r
f_model_comparison(model1, model2, nested = NULL, digits = 3)
```

## Arguments

- model1:

  The first model object. Supported classes include:
  `"lm", "glm", "aov", "lmerMod", "glmerMod", and "nls"`.

- model2:

  The second model object. Supported classes include:
  `"lm", "glm", "aov", "lmerMod", "glmerMod", and "nls"`.

- nested:

  Logical. If `TRUE`, assumes the models are nested and performs an
  ANOVA comparison. If `NULL` (default), the function attempts to
  automatically determine if the models are nested.

- digits:

  Integer. The number of decimal places to round the output metrics.
  Defaults to `3`.

## Value

A list of class "f_model_comparison" containing:

- model1_name:

  The name of the first model.

- model2_name:

  The name of the second model.

- model1_class:

  The class of the first model.

- model2_class:

  The class of the second model.

- metrics_table:

  A data frame summarizing metrics for both models, their differences,
  and (if applicable) the ANOVA p-value.

- formatted_metrics_table:

  A formatted version of the metrics table for printing.

- anova_comparison:

  The ANOVA comparison results if the models are nested and an ANOVA
  test was performed.

- nested:

  Logical indicating whether the models were treated as nested.

## Details

Calculate various metrics to assess model fit:

- **AIC/BIC:** Lower values indicate better fit.

- **Log-Likelihood:** Higher values (less negative) indicate better fit.

- **R-squared:** Proportion of variance explained by the model.

- **Adjusted R-squared:** R-squared penalized for the number of
  parameters (for linear models).

- **Nagelkerke R^2:** A pseudo-R^2 for generalized linear models (GLMs).

- **Marginal/Conditional R^2:** For mixed models, marginal R^2 reflects
  fixed effects, while conditional R^2 includes random effects.

- **Sigma:** Residual standard error.

- **Deviance:** Model deviance.

- **SSE:** Sum of squared errors.

- **Parameters (df):** Number of model parameters.

- **Residual df:** Residual degrees of freedom.

If the models are nested, an ANOVA test is performed to compare them,
and a p-value is provided to assess whether the more complex model
significantly improves fit.

## Note

- The function supports a variety of model types but may issue warnings
  if unsupported or partially supported classes are used.

- For GLMs, Nagelkerke's R^2 is used as a pseudo-R^2 approximation.

- For mixed models, the function relies on the 'r.squaredGLMM' function
  from the 'MuMIn' package for R^2 calculation.

- The idea of this function (not the code), I got from Dustin Fife's
  function
  ['model.comparison'](https://github.com/dustinfife/flexplot/blob/master/R/model.comparison.R)
  in the super cool ['flexplot
  package'](https://github.com/dustinfife/flexplot/).

## Supported Model Classes

The function supports the following model classes:

- Linear models ("lm")

- Generalized linear models ("glm")

- Analysis of variance models ("aov")

- Linear mixed models ("lmerMod")

- Generalized linear mixed models ("glmerMod")

- Nonlinear least squares models ("nls")

## See also

[`AIC`](https://rdrr.io/r/stats/AIC.html),
[`BIC`](https://rdrr.io/r/stats/AIC.html),
[`anova`](https://rdrr.io/r/stats/anova.html),
[`logLik`](https://rdrr.io/r/stats/logLik.html),
[`r.squaredGLMM`](https://rdrr.io/pkg/MuMIn/man/r.squaredGLMM.html)

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Example with linear models.
model1 <- lm(mpg ~ wt, data = mtcars)
model2 <- lm(mpg ~ wt + hp, data = mtcars)
comparison <- f_model_comparison(model1, model2)
print(comparison)
#> Comparison of two nested models:
#> Model1: model1 (lm) 
#> Model2: model2 (lm)
#> 
#>           Metric  Model1  Model2 Difference
#>              AIC 166.029 156.652     -9.377
#>              BIC 170.427 162.515     -7.911
#>   Log-Likelihood -80.015 -74.326      5.689
#>        R-squared   0.753   0.827      0.074
#>   Adj. R-squared   0.745   0.815      0.070
#>            Sigma   3.046   2.593     -0.452
#>         Deviance 278.322 195.048    -83.274
#>              SSE 278.322 195.048    -83.274
#>  Parameters (df)       2       3          1
#>      Residual df      30      29         -1
#>    ANOVA p-value                  1.451e-03
#> 
#> Interpretation Guide:
#> - Lower AIC/BIC values indicate better model fit.
#> - Higher R-squared values indicate better model fit.
#> - Adj. R-squared is R-squared with a penalty for
#>   the number of model parameters used.
#> - A lower Sigma (residual standard error) generally
#>   indicates a better fit to the data.
#> - For nested models, ANOVA p-value < 0.05 suggests
#>   that the more complex model is significantly better.

# Example with GLMs.
# \donttest{
model1 <- glm(am ~ wt, data = mtcars, family = binomial)
model2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
comparison <- f_model_comparison(model1, model2)
print(comparison)
#> Comparison of two nested models:
#> Model1: model1 (glm) 
#> Model2: model2 (glm)
#> 
#>           Metric Model1 Model2 Difference
#>              AIC 23.176 16.059     -7.117
#>              BIC 26.108 20.456     -5.651
#>   Log-Likelihood -9.588 -5.030      4.558
#>   Nagelkerke R^2  0.713  0.871      0.158
#>                                          
#>            Sigma      1      1          0
#>         Deviance 19.176 10.059     -9.117
#>              SSE                         
#>  Parameters (df)      2      3          1
#>      Residual df     30     29         -1
#>    ANOVA p-value                2.532e-03
#> 
#> Interpretation Guide:
#> - Lower AIC/BIC values indicate better model fit.
#> - Higher R-squared values indicate better model fit.
#> - Nagelkerke's R^2 adapts Cox & Snell's R^2 for GLMs,
#>   scaling it to a 0-1 range to serve as a pseudo-R^2
#>   approximating explained variance.
#> - A lower Sigma (residual standard error) generally
#>   indicates a better fit to the data.
#> - For nested models, ANOVA p-value < 0.05 suggests
#>   that the more complex model is significantly better.
# }

# Example with automatic detection of nested models.
model1 <- lm(mpg ~ wt, data = mtcars)
model2 <- lm(mpg ~ wt + hp, data = mtcars)
comparison <- f_model_comparison(model1, model2)
print(comparison)
#> Comparison of two nested models:
#> Model1: model1 (lm) 
#> Model2: model2 (lm)
#> 
#>           Metric  Model1  Model2 Difference
#>              AIC 166.029 156.652     -9.377
#>              BIC 170.427 162.515     -7.911
#>   Log-Likelihood -80.015 -74.326      5.689
#>        R-squared   0.753   0.827      0.074
#>   Adj. R-squared   0.745   0.815      0.070
#>            Sigma   3.046   2.593     -0.452
#>         Deviance 278.322 195.048    -83.274
#>              SSE 278.322 195.048    -83.274
#>  Parameters (df)       2       3          1
#>      Residual df      30      29         -1
#>    ANOVA p-value                  1.451e-03
#> 
#> Interpretation Guide:
#> - Lower AIC/BIC values indicate better model fit.
#> - Higher R-squared values indicate better model fit.
#> - Adj. R-squared is R-squared with a penalty for
#>   the number of model parameters used.
#> - A lower Sigma (residual standard error) generally
#>   indicates a better fit to the data.
#> - For nested models, ANOVA p-value < 0.05 suggests
#>   that the more complex model is significantly better.
```
