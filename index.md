# rfriend

[![CRAN
Version](https://www.r-pkg.org/badges/version/rfriend)](https://CRAN.R-project.org/package=rfriend)
[![License:
GPL-3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.R-project.org/Licenses/GPL-3)

## Overview

`rfriend` is an R package designed to streamline data analysis and
statistical testing by wrapping complex or repetitive code into
convenient, user-friendly functions prefixed with `f_` (f_riendly). Its
primary focus is data exploration, statistical tests, and creating
publication-ready output in multiple formats including PDF, Microsoft
Word, and Microsoft Excel.

The package helps you write shorter code while producing well-formatted
summaries, visualizations, and statistical tests, automatically handling
data transformations, assumption checking, and post hoc testing.

Full documentation is available at
<https://delde001.github.io/rfriend/>.

------------------------------------------------------------------------

## Features

### Data exploration

- **Summary tables:**
  [`f_summary()`](https://delde001.github.io/rfriend/reference/f_summary.md)
  generates comprehensive summary tables (with optional skewness,
  kurtosis, and confidence intervals).
- **Diagnostic dashboards:**
  [`f_scan()`](https://delde001.github.io/rfriend/reference/f_scan.md)
  builds a density / boxplot / QQ panel for one or more variables,
  optionally split by grouping factors.
- **Correlation plots:**
  [`f_corplot()`](https://delde001.github.io/rfriend/reference/f_corplot.md)
  shows Pearson, Spearman, and Kendall coefficients together, with
  support for ordinal variables.
- **Outlier handling:**
  [`f_outliers()`](https://delde001.github.io/rfriend/reference/f_outliers.md)
  flags outliers using Tukey’s fences and
  [`f_remove_outliers()`](https://delde001.github.io/rfriend/reference/f_remove_outliers.md)
  removes them with safe anti-join semantics.
- **Test recommendation (beta):**
  [`f_stat_wizard()`](https://delde001.github.io/rfriend/reference/f_stat_wizard.md)
  inspects your data from a formula and suggests an appropriate test as
  ready-to-run code.

### Data transformations

- [`f_boxcox()`](https://delde001.github.io/rfriend/reference/f_boxcox.md)
  for Box-Cox transformations (wrapping MASS/boxcox and rcompanion).
- [`f_bestNormalize()`](https://delde001.github.io/rfriend/reference/f_bestNormalize.md)
  wraps and extends normalization from the `bestNormalize` package.

### Visualizations

- Boxplots
  ([`f_boxplot()`](https://delde001.github.io/rfriend/reference/f_boxplot.md)),
  including numeric-vector input and custom palettes
- QQ-plots
  ([`f_qqnorm()`](https://delde001.github.io/rfriend/reference/f_qqnorm.md))
- Histograms
  ([`f_hist()`](https://delde001.github.io/rfriend/reference/f_hist.md))

### Statistical tests

Run and visualize tests on multiple response variables and predictors,
with automatic assumption checking and post hoc analysis:

- ANOVA:
  [`f_aov()`](https://delde001.github.io/rfriend/reference/f_aov.md)
- Linear regression (OLS):
  [`f_lm()`](https://delde001.github.io/rfriend/reference/f_lm.md)
- Linear mixed-effects models:
  [`f_lmer()`](https://delde001.github.io/rfriend/reference/f_lmer.md)
- Kruskal-Wallis:
  [`f_kruskal_test()`](https://delde001.github.io/rfriend/reference/f_kruskal_test.md)
- Friedman rank sum test:
  [`f_friedman()`](https://delde001.github.io/rfriend/reference/f_friedman.md)
- t-test:
  [`f_t_test()`](https://delde001.github.io/rfriend/reference/f_t_test.md)
- Wilcoxon test:
  [`f_wilcox_test()`](https://delde001.github.io/rfriend/reference/f_wilcox_test.md)
- Generalized linear models:
  [`f_glm()`](https://delde001.github.io/rfriend/reference/f_glm.md)
- Chi-square tests:
  [`f_chisq_test()`](https://delde001.github.io/rfriend/reference/f_chisq_test.md)

Post hoc results are summarized in a compact letter display (cld) table
for easy interpretation.

### Model comparison

- Compare models easily with
  [`f_model_compare()`](https://delde001.github.io/rfriend/reference/f_model_compare.md).

### Utilities

- [`f_clear()`](https://delde001.github.io/rfriend/reference/f_clear.md)
  clears the workspace and restarts R.
- [`f_setwd()`](https://delde001.github.io/rfriend/reference/f_setwd.md)
  sets the working directory to the current script’s location.
- [`f_theme()`](https://delde001.github.io/rfriend/reference/f_theme.md)
  switches RStudio themes quickly.
- [`f_factors()`](https://delde001.github.io/rfriend/reference/f_factors.md)
  converts multiple data frame columns to factors (with optional
  reference-level control).
- [`f_long()`](https://delde001.github.io/rfriend/reference/f_long.md)
  reshapes wide data to long format in a single call.
- [`f_example_data()`](https://delde001.github.io/rfriend/reference/f_example_data.md)
  lists and returns the bundled example datasets.
- [`f_open_file()`](https://delde001.github.io/rfriend/reference/f_open_file.md),
  [`f_pander()`](https://delde001.github.io/rfriend/reference/f_pander.md),
  [`f_rename_columns()`](https://delde001.github.io/rfriend/reference/f_rename_columns.md),
  [`f_rename_vector()`](https://delde001.github.io/rfriend/reference/f_rename_vector.md),
  [`f_load_packages()`](https://delde001.github.io/rfriend/reference/f_load_packages.md),
  and more.

------------------------------------------------------------------------

## Installation

`rfriend` requires R (\>= 4.4.0) and several dependencies (see
DESCRIPTION for details). PDF and Word output require Pandoc (\>= 3.2),
which is bundled with RStudio.

Install the latest released version from CRAN with:

``` r

install.packages("rfriend")
```

To install the latest development version from GitHub:

``` r

# install.packages("devtools")
devtools::install_github("delde001/rfriend")
```

------------------------------------------------------------------------

## Basic Usage

``` r

library(rfriend)

# Summary of your dataset
f_summary(your_dataframe)

# Run ANOVA on multiple response variables
f_aov(response_var1 + response_var2 ~ predictor1 * predictor2,
      data = your_dataframe)

# Create a boxplot of one or more variables
data(mtcars)
f_boxplot(hp + disp ~ gear * cyl,
          data = mtcars,
          intro_text = FALSE,
          output_type = "word")

# Fit a linear mixed-effects model
f_lmer(response ~ treatment + (1 | block), data = your_dataframe)

# Perform a Box-Cox transformation
transformed <- f_boxcox(your_dataframe$variable)

# Load a bundled example dataset
f_example_data()                       # list available files
path <- f_example_data("field_trial.csv")

# Clear the workspace and restart R
f_clear()
```

For detailed usage and examples, please refer to the package vignettes,
the help files, and the documentation website at
<https://delde001.github.io/rfriend/>.

------------------------------------------------------------------------

## Known Issues

- When loading `rfriend`, you may see harmless warnings about S3 method
  overwrites related to `nobs.fitdistr` and `nobs.multinom` due to
  imported packages (`MuMIn`, `rstatix`). These do not affect
  functionality.

------------------------------------------------------------------------

## Contributing

Contributions, bug reports, and feature requests are very welcome.
Please open an issue or submit a pull request on GitHub.

Before contributing, please ensure that:

- You have tested your changes locally.
- Code is properly documented using roxygen2.
- You follow the existing style and conventions.

------------------------------------------------------------------------

## License

This package is licensed under
[GPL-3](https://www.R-project.org/Licenses/GPL-3).

------------------------------------------------------------------------

## Contact

Author and maintainer: Sander H. van Delden Email: <plantmind@proton.me>

Feel free to reach out for support, feature requests, or collaborations.

------------------------------------------------------------------------

*Thank you for using `rfriend`!*
