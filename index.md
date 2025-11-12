# rfriend

[![CRAN
Version](https://www.r-pkg.org/badges/version/rfriend)](https://CRAN.R-project.org/package=rfriend)  
[![License:
GPL-3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)

## Overview

`rfriend` is an R package designed to streamline data analysis and
statistical testing by wrapping complex or repetitive code into
convenient, user-friendly functions prefixed with `f_`. Its primary
focus is data exploration, statistical tests, and creating
publication-ready output in multiple formats including PDF, Microsoft
Word, and Excel.

The package helps you write shorter code while producing well-formatted
summaries, visualizations, and statistical tests, automatically handling
data transformations, assumption checking, and post hoc testing.

------------------------------------------------------------------------

## Features

- **Summary Tables:**
  [`f_summary()`](https://delde001.github.io/rfriend/reference/f_summary.md)
  generates comprehensive summary tables.
- **Data Transformations:**
  - [`f_boxcox()`](https://delde001.github.io/rfriend/reference/f_boxcox.md)
    for Box-Cox transformations (wrapping MASS/boxcox and rcompanion)  
  - [`f_bestNormalize()`](https://delde001.github.io/rfriend/reference/f_bestNormalize.md)
    wraps and extends normalization from the `bestNormalize` package.
- **Visualizations:** Easily create:
  - Boxplots
    ([`f_boxplot()`](https://delde001.github.io/rfriend/reference/f_boxplot.md))  
  - QQ-plots
    ([`f_qqnorm()`](https://delde001.github.io/rfriend/reference/f_qqnorm.md))  
  - Histograms
    ([`f_hist()`](https://delde001.github.io/rfriend/reference/f_hist.md))  
  - Density plots
- **Statistical Tests:** Run and visualize tests on multiple variables
  and predictors, with automatic assumption checking and post hoc
  analysis:
  - ANOVA:
    [`f_aov()`](https://delde001.github.io/rfriend/reference/f_aov.md)  
  - Kruskal-Wallis:
    [`f_kruskal_test()`](https://delde001.github.io/rfriend/reference/f_kruskal_test.md)  
  - Generalized Linear Models:
    [`f_glm()`](https://delde001.github.io/rfriend/reference/f_glm.md)  
  - Chi-square Tests:
    [`f_chisq_test()`](https://delde001.github.io/rfriend/reference/f_chisq_test.md)
- **Model Comparison:** Compare models easily with
  [`f_model_comparison()`](https://delde001.github.io/rfriend/reference/f_model_comparison.md).
- **Utilities:**
  - [`f_clear()`](https://delde001.github.io/rfriend/reference/f_clear.md)
    clears the workspace and restarts R.  
  - [`f_setwd()`](https://delde001.github.io/rfriend/reference/f_setwd.md)
    sets working directory to the current script’s location.  
  - [`f_theme()`](https://delde001.github.io/rfriend/reference/f_theme.md)
    switches RStudio themes quickly.  
  - [`f_factors()`](https://delde001.github.io/rfriend/reference/f_factors.md)
    converts multiple dataframe columns to factors.

------------------------------------------------------------------------

## Installation

`rfriend` requires R (\>= 4.4.0) and several dependencies (see
DESCRIPTION for details). You can install the latest released version
from CRAN with:

    install.packages("rfriend")

If you want the latest development version (if available on GitHub):


    # install.packages("devtools")

    devtools::install_github("delde001/rfriend")

------------------------------------------------------------------------

## Basic Usage

    library(rfriend)

    # Summary of your dataset
      f_summary(your_dataframe)

    # Run ANOVA on multiple response variables
      f_aov(response_var1 +  response_var1 ~ predictor1 * predictor2, data = your_dataframe)

    # Create a boxplot of a variable
      data(mtcars)
      f_boxplot(hp + disp ~ gear*cyl,
                 data=mtcars,
                 boxplot_explanation = FALSE,
                 output_type = "word"
                 )

    # Perform Box-Cox transformation
      transformed <- f_boxcox(your_dataframe$variable)

    # Create a boxplot of a variable
      data(mtcars)
      f_boxplot(hp + disp ~ gear*cyl,
                 data=mtcars,
                 boxplot_explanation = FALSE,
                 output_type = "word"
                 )

    # Perform Box-Cox transformation
    transformed <- f_boxcox(your_dataframe$variable)

    # Clear workspace and restart R
      f_clear()

For detailed usage and examples, please refer to the package vignettes
and help files.

------------------------------------------------------------------------

## Known Issues

- When loading `rfriend`, you may see harmless warnings about S3 method
  overwrites related to `nobs.fitdistr` and `nobs.multinom` due to
  imported packages (`MuMIn`, `rstatix`). These do not affect the
  functionality.

------------------------------------------------------------------------

## Contributing

Contributions, bug reports, and feature requests are very welcome!
Please open an issue or submit a pull request on GitHub.

Before contributing, please ensure:

- You have tested your changes locally.
- Code is properly documented using roxygen2.
- You follow the existing style and conventions.

------------------------------------------------------------------------

## License

This package is licensed under
[GPL-3](https://www.gnu.org/licenses/gpl-3.0.html).

------------------------------------------------------------------------

## Contact

Author and maintainer: Sander H. van Delden  
Email: <plantmind@proton.me>

Feel free to reach out for support, feature requests, or collaborations.

------------------------------------------------------------------------

*Thank you for using `rfriend`!*
