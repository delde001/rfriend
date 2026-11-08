# Access parts of an f_outliers result with \$

An `f_outliers` result is a list of tables: `output_df` when one column
was checked, or one table per checked column. `$` gives these tables by
name, as usual. In addition, a column of the outlier table can be taken
directly from the result: `bad_rows$row_id` returns the flagged row
numbers (with several checked columns: all flagged rows), and with one
checked column any column of its table works, for example
`bad_rows$biomass_g`. A name that is not part of the result stops with
an error that lists what is available, instead of returning `NULL`
silently. Use `[[` for the standard list behaviour.

## Usage

``` r
# S3 method for class 'f_outliers'
x$name
```

## Arguments

- x:

  An object of class `f_outliers`.

- name:

  The name of a result table or of a column in the outlier table.

## Value

A data frame (a result table) or a vector (a column).

## Examples

``` r
bad_rows <- f_outliers(biomass_g ~ treatment, data = plant_trial)
bad_rows$row_id       # the flagged row numbers
#> [1] 21
bad_rows$output_df    # the full outlier table
#>   row_id biomass_g treatment block plant_id time_weeks height_cm
#> 1     21      64.6     low_N    B2      P07          6      50.3
#>   chloroplast_count infected
#> 1                43       no
```
