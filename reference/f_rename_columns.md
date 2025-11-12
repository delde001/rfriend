# Rename Specific Columns in a Data Frame

Renames specific columns in a data frame based on a named vector
(name_map). It ensures that only the specified columns are renamed,
while others remain unchanged.

## Usage

``` r
f_rename_columns(df, name_map)
```

## Arguments

- df:

  A data frame whose columns are to be renamed.

- name_map:

  A named vector where the names correspond to the current column names
  in `df`, and the values are the new names to assign. All names in
  `name_map` must exist in the column names of `df`. Yet, not all names
  in the data.frame have to be in `name_map`. This allows for selective
  renaming of just one or two columns.

## Value

A data frame with updated column names. Columns not specified in
`name_map` remain unchanged.

## Details

This function is particularly useful when you want to rename only a
subset of columns in a data frame. It performs input validation to
ensure that:

- `name_map` is a named vector.

- All names in `name_map` exist as column names in `df`.

If these conditions are not met, the function will throw an error with
an appropriate message.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Create a sample data frame.
df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

# Define a named vector for renaming specific columns.
name_map <- c(a = "alpha", c = "gamma")

# Rename columns.
df <- f_rename_columns(df, name_map)

# View updated data frame.
print(df)
#>   alpha b gamma
#> 1     1 4     7
#> 2     2 5     8
#> 3     3 6     9
```
