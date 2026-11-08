# Rename Elements of a Vector Based on a Mapping

Renames elements of a vector based on a named mapping vector. Elements
that match the names in the mapping vector are replaced with their
corresponding values, while elements not found in the mapping remain
unchanged. Use it to clean labels, for example `c("Low N" = "low_N")`.

## Usage

``` r
f_rename_vector(vector, name_map, trim = FALSE, warn_unmatched = TRUE)
```

## Arguments

- vector:

  A character vector or a factor containing the elements to be renamed.

- name_map:

  A named vector where the names correspond to the elements in `vector`
  that should be renamed, and the values are the new names to assign.

- trim:

  Logical. If `TRUE`, removes leading and trailing spaces from every
  element before renaming, so `" high_N"` becomes `"high_N"`. Default
  `FALSE`.

- warn_unmatched:

  Logical. If `TRUE` (default), warns when a name in `name_map` does not
  occur in `vector`, which usually means a typing error in the old name.
  Set to `FALSE` when the map deliberately holds more names than the
  vector.

## Value

A character vector with updated element names, or a factor with renamed
levels when `vector` is a factor. Elements not found in `name_map`
remain unchanged.

## Details

Each element of `vector` that occurs in the names of `name_map` is
replaced with the corresponding value from `name_map`; other elements
are kept. A character vector is returned as an unnamed character vector.
A factor is returned as a factor: its levels are renamed and keep their
order, and two old levels that get the same new name become one level.
Missing values stay missing.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# Define a vector and a name map.
vector   <- c("Species", "Weight", "L")
name_map <- c(Species = "New_species_name", L = "Length_cm")

# Rename elements of the vector.
updated_vector <- f_rename_vector(vector, name_map)

# View updated vector
print(updated_vector)
#> [1] "New_species_name" "Weight"           "Length_cm"       

# Clean treatment labels: remove spaces and rename one label
treatment <- c(" high_N", "Low N", "control", "drought")
f_rename_vector(treatment, c("Low N" = "low_N"), trim = TRUE)
#> [1] "high_N"  "low_N"   "control" "drought"
```
