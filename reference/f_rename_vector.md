# Rename Elements of a Vector Based on a Mapping

Renames elements of a vector based on a named mapping vector. Elements
that match the names in the mapping vector are replaced with their
corresponding values, while elements not found in the mapping remain
unchanged.

## Usage

``` r
f_rename_vector(vector, name_map)
```

## Arguments

- vector:

  A character vector containing the elements to be renamed.

- name_map:

  A named vector where the names correspond to the elements in `vector`
  that should be renamed, and the values are the new names to assign.

## Value

A character vector with updated element names. Elements not found in
`name_map` remain unchanged.

## Details

This function iterates through each element of `vector` and checks if it
exists in the names of `name_map`. If a match is found, the element is
replaced with the corresponding value from `name_map`. If no match is
found, the original element is retained. The result is returned as an
unnamed character vector.

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
```
