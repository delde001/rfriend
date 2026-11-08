# rfriend

[![CRAN
Version](https://www.r-pkg.org/badges/version/rfriend)](https://CRAN.R-project.org/package=rfriend)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/rfriend)](https://CRAN.R-project.org/package=rfriend)
[![License:
GPL-3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.R-project.org/Licenses/GPL-3)

**From raw data to a reviewer-ready statistics report in one line of
R.**

`rfriend` runs the test, checks the assumptions, transforms the data
when needed, does the post hoc comparisons, and writes the result to
Word, PDF, Excel or R Markdown. Tables come with compact letter displays
and figures are publication-ready `ggplot2` objects you can restyle.

It is built for people who need sound statistics but do not live in R:
MSc and PhD students writing a thesis, researchers in plant, life and
environmental sciences, and companies that analyse experiments a few
times a year.

Full documentation: <https://delde001.github.io/rfriend/>

![Figure from MS Word report produced by f_lmer(): assumption checks,
compact letter display and effect
plot](reference/figures/report-example.png)

------------------------------------------------------------------------

## Try it in 60 seconds

``` r

install.packages("rfriend")
library(rfriend)

# Not sure which test to use? Ask the wizard, and let it run the answer.
f_stat_wizard(Sepal.Length ~ Species, data = iris, run = TRUE)

# A real design: a randomised complete block field trial with repeated
# measures (bundled teaching dataset), analysed with a mixed model and
# written to a Word report.
data(plant_trial)
plant_trial$time_f <- factor(plant_trial$time_weeks)

f_lmer(height_cm ~ treatment * time_f + (1 | block/plant_id),
       data        = plant_trial,
       output_type = "word")
```

The report contains the assumption checks, the Type III table, the
estimated means with compact letters, and the effect and interaction
plots.

------------------------------------------------------------------------

## What rfriend does for you

**Choose the test.**
[`f_stat_wizard()`](https://delde001.github.io/rfriend/reference/f_stat_wizard.md)
(beta) reads your formula and data, checks variable types, normality and
variance, and recommends an `f_` function as ready-to-run code.

**Run the test, properly.** Each test function checks its assumptions,
offers a Box-Cox or bestNormalize transformation when they fail, runs
post hoc comparisons, and handles several response variables in one call
(`y1 + y2 ~ treatment`), with a note on multiple testing.

| Question | Function |
|----|----|
| Two groups | [`f_t_test()`](https://delde001.github.io/rfriend/reference/f_t_test.md), [`f_wilcox_test()`](https://delde001.github.io/rfriend/reference/f_wilcox_test.md) |
| More than two groups | [`f_aov()`](https://delde001.github.io/rfriend/reference/f_aov.md), [`f_kruskal_test()`](https://delde001.github.io/rfriend/reference/f_kruskal_test.md) |
| Repeated measures, blocks, nesting | [`f_lmer()`](https://delde001.github.io/rfriend/reference/f_lmer.md), [`f_friedman()`](https://delde001.github.io/rfriend/reference/f_friedman.md) |
| Continuous predictors | [`f_lm()`](https://delde001.github.io/rfriend/reference/f_lm.md) |
| Counts, proportions, binary outcomes | [`f_glm()`](https://delde001.github.io/rfriend/reference/f_glm.md) |
| Categorical association | [`f_chisq_test()`](https://delde001.github.io/rfriend/reference/f_chisq_test.md) |
| Which model fits best | [`f_model_compare()`](https://delde001.github.io/rfriend/reference/f_model_compare.md) |

**Explore and prepare your data.**
[`f_summary()`](https://delde001.github.io/rfriend/reference/f_summary.md),
[`f_scan()`](https://delde001.github.io/rfriend/reference/f_scan.md)
(density, boxplot and QQ panel per variable),
[`f_corplot()`](https://delde001.github.io/rfriend/reference/f_corplot.md),
[`f_boxplot()`](https://delde001.github.io/rfriend/reference/f_boxplot.md),
[`f_hist()`](https://delde001.github.io/rfriend/reference/f_hist.md),
[`f_qqnorm()`](https://delde001.github.io/rfriend/reference/f_qqnorm.md),
[`f_outliers()`](https://delde001.github.io/rfriend/reference/f_outliers.md),
[`f_remove_outliers()`](https://delde001.github.io/rfriend/reference/f_remove_outliers.md),
[`f_boxcox()`](https://delde001.github.io/rfriend/reference/f_boxcox.md),
[`f_bestNormalize()`](https://delde001.github.io/rfriend/reference/f_bestNormalize.md),
[`f_factors()`](https://delde001.github.io/rfriend/reference/f_factors.md)
and
[`f_long()`](https://delde001.github.io/rfriend/reference/f_long.md).

**Everyday helpers.**
[`f_clear()`](https://delde001.github.io/rfriend/reference/f_clear.md)
clears the workspace and restarts R,
[`f_setwd()`](https://delde001.github.io/rfriend/reference/f_setwd.md)
sets the working directory to the current script,
[`f_example_data()`](https://delde001.github.io/rfriend/reference/f_example_data.md)
gives access to bundled example files, plus
[`f_open_file()`](https://delde001.github.io/rfriend/reference/f_open_file.md),
[`f_pander()`](https://delde001.github.io/rfriend/reference/f_pander.md),
[`f_rename_columns()`](https://delde001.github.io/rfriend/reference/f_rename_columns.md),
[`f_rename_vector()`](https://delde001.github.io/rfriend/reference/f_rename_vector.md),
[`f_load_packages()`](https://delde001.github.io/rfriend/reference/f_load_packages.md)
and
[`f_theme()`](https://delde001.github.io/rfriend/reference/f_theme.md).

------------------------------------------------------------------------

## More examples

``` r

# Summary table of a data frame
f_summary(plant_trial)

# Two responses analysed in sequence, saved to Word
f_aov(Sepal.Width + Sepal.Length ~ Species,
      data        = iris,
      output_type = "word")

# Boxplots of several variables split by two factors
f_boxplot(hp + disp ~ gear * cyl,
          data        = mtcars,
          output_type = "word")

# Box-Cox transformation of a single variable
transformed <- f_boxcox(plant_trial$biomass_g)

# Bundled example files
f_example_data()                                  # list files
path <- f_example_data("plant_trial_raw.csv")     # path to one file
raw  <- read.csv(path)
```

------------------------------------------------------------------------

## Learn rfriend

A step-by-step video course takes you from installing R to a complete,
defensible thesis analysis with `rfriend`. Details:
\[rfriend.subscribepage.io\].

Workshops for departments, graduate schools and companies are available
on request: <plantmind@proton.me>.

------------------------------------------------------------------------

## Installation

`rfriend` requires R (\>= 4.4.0). PDF and Word output need Pandoc (\>=
3.2), which is bundled with RStudio.

``` r

install.packages("rfriend")                       # CRAN release

# install.packages("devtools")
devtools::install_github("delde001/rfriend")      # development version
```

------------------------------------------------------------------------

## Citing rfriend

``` r

citation("rfriend")
```

------------------------------------------------------------------------

## Known issues

- On loading you may see messages about S3 method overwrites
  (`nobs.fitdistr`, `nobs.multinom`) caused by imported packages
  (`MuMIn`, `rstatix`). They do not affect results.

------------------------------------------------------------------------

## Contributing

Bug reports and feature requests are welcome on
[GitHub](https://github.com/delde001/rfriend/issues). Pull requests
should be tested locally, documented with roxygen2, and follow the
existing style.

## License

GPL-3. See <https://www.R-project.org/Licenses/GPL-3>.

## Contact

Dr. ir. Sander H. van Delden, <plantmind@proton.me>
