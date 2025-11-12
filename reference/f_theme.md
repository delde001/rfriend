# Apply a black or white 'RStudio' Theme and Zoom Level

This comes in hand when teaching, the function allows users to apply a
"black" or "white" 'RStudio' theme and adjust the zoom level in the
'RStudio' IDE. It includes error handling for invalid inputs.

## Usage

``` r
f_theme(color = "black", zlevel = 0)
```

## Arguments

- color:

  A character string. The theme color to apply. Must be either `"black"`
  (dark theme) or `"white"` (light theme). Default is `"black"`.

- zlevel:

  A numeric value. The zoom level to apply, ranging from `0` (default
  size) to `4` (maximum zoom). Default is `0`.

## Value

None. The function is called for its side effects of changing the
'RStudio' theme or Zoomlevel.

This function does not return a value. It applies changes directly to
the 'RStudio' IDE.

## Details

The function performs the following actions:

1.  Applies the specified 'RStudio' theme:

    - `"black"`: Applies the "Tomorrow Night 80s" dark theme.

    - `"white"`: Applies the "Textmate (default)" light theme.

2.  Adjusts the zoom level in 'RStudio':

    - `zlevel = 0`: Resets to default zoom level.

    - `zlevel = 1`: Zooms in once.

    - `zlevel = 2`: Zooms in twice.

    - `zlevel = 3`: Zooms in three times.

    - `zlevel = 4`: Zooms in four times.

The function includes error handling to ensure valid inputs:

- `color` must be a character string and one of `"black"` or `"white"`.

- `zlevel` must be a numeric value, an integer, and within the range of
  0 to 4. If a non-integer is provided, it will be rounded to the
  nearest integer with a warning.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# NOTE: This example will change your RStudio theme hence the dont run warning.
if (FALSE) { # \dontrun{
# Apply a dark theme with with zoom level 2:
f_theme(color = "black", zlevel = 2)

# Apply a black theme with maximum zoom level:
f_theme(color = "black", zlevel = 4)

# Apply the default light theme default zoom level:
f_theme(color = "black", zlevel = 0)
} # }
```
