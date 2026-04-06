# Convert a data frame to a contingency table

Convert a data frame to a contingency table

## Usage

``` r
df_to_table(df, label_col = NULL)
```

## Arguments

- df:

  A data frame where one column contains row labels and the rest are
  numeric.

- label_col:

  Index or name of the column containing row labels. If NULL (default),
  the function auto-detects the first character/factor column.

## Value

A contingency table.
