# Convert a data frame to a contingency table

Convert a data frame to a contingency table

## Usage

``` r
df_to_table(df, label_col = NULL)
```

## Arguments

- df:

  A data frame. Either (a) one column contains row labels and the rest
  are numeric, (b) a fully numeric data frame with meaningful
  [`rownames()`](https://rdrr.io/r/base/colnames.html) set, or (c) a
  fully numeric data frame with no row labels at all (in which case the
  data frame is coerced directly to a table without row labels).

- label_col:

  Index or name of the column containing row labels. If NULL (default),
  the function auto-detects the first character/factor column. If no
  such column is found, the function falls back to using `rownames(df)`
  when these are meaningful, otherwise it coerces the data frame to a
  table as-is.

## Value

A contingency table.
