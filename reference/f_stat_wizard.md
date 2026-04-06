# Statistical Test Wizard

Analyzes your data structure based on a formula and recommends the
appropriate statistical test. Checks variable types, normality of
residuals, homogeneity of variance, and checks if f_boxcox
transformation can fix non-normality. Returns a text object that can be
printed or stored.

Supports standard formulas including `y ~ .`, `y ~ as.factor(x)`, and
interaction terms. Formulas with random effects (e.g. `(1|ID)`) are
detected and handled separately. Multivariate responses (e.g.
`cbind(y1, y2) ~ x`) and transformed responses (e.g. `log(y) ~ x`) are
not supported.

## Usage

``` r
f_stat_wizard(data, formula, interactive = FALSE)
```

## Arguments

- data:

  A data frame.

- formula:

  A formula specifying the relationship.

- interactive:

  Logical. If `TRUE`, asks the user questions about study design.
  Default `FALSE`.

## Value

An object of class `"f_stat_wizard"` containing the analysis report.

## Examples

``` r
f_stat_wizard(iris, Sepal.Length ~ Species)
#> ---------------------------------------------------
#>           rfriend STATISTICAL WIZARD               
#> ---------------------------------------------------
#> Model: Sepal.Length ~ Species
#> Effective sample size: n = 150
#> -> Explanatory: Single variable (nominal).
#> -> Response: Numeric. Checking normality of residuals...
#>    OK: Residuals appear Normal (Shapiro p=0.219).
#>    WARN: Variances UNEQUAL (Levene p=0.002).
#>    -> Welch's variants will be recommended.
#> ---------------------------------------------------
#>  
#> RECOMMENDATION:
#> Test: Welch's One-Way ANOVA (unequal variances)
#> Code: oneway.test(Sepal.Length ~ Species, data=iris, var.equal=FALSE)
#>    Post-hoc: TukeyHSD() after aov, or rstatix::tukey_hsd()
#>    Effect size: rstatix::eta_squared() for eta-squared
#>  
#> ---------------------------------------------------
#> Disclaimer: This wizard checks data properties, not experimental design.
#> Always verify if your data is Paired or Independent!
#> The WIZARD is limited and only chooses from:
#>    t.test (Welch default), aov, oneway.test, lm,
#>    cor.test (Pearson/Spearman), f_chisq_test,
#>    glm (Gamma/Poisson/NegBin), f_kruskal_test,
#>    wilcox.test (Mann-Whitney/Wilcoxon), f_lmer,
#>    nnet::multinom (multinomial logistic regression)
```
