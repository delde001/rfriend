# Set Working Directory Based on Current File or Specified Path

A wrapper around [`setwd()`](https://rdrr.io/r/base/getwd.html) that
sets the working directory to the location of the currently open file in
'RStudio' if no path is provided. If a path is specified, it sets the
working directory to that path instead.

## Usage

``` r
f_setwd(path = NULL)
```

## Arguments

- path:

  A character string specifying the desired working directory. If `NULL`
  (default), the function sets the working directory to the location of
  the currently open and saved file in 'RStudio'.

## Value

None. The function is called for its side effects of changing the
working directory.

## Details

If `path` is not provided (`NULL`), this function uses the `this.path`
package to determine the location of the currently open file and sets
that as the working directory. The file must be saved for this to work
properly.

If a valid `path` is provided, it directly sets the working directory to
that path.

## Note

- The function checks whether the currently open file is saved before
  setting its location as the working directory.

- If the function is called from an unsaved script or directly from the
  console, an error will be thrown.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# NOTE: The use of "if(interactive())" prevents this example from running
# during automated CRAN checks. This is necessary because the example
# requires to be run from an R script. You don't need to use
# "if(interactive())" in your own scripts.
if(interactive()) {
# Store the current working directory, so we can reset it after the example.
current_wd <-  getwd()
print(current_wd)

# Run this commando from a saved R script file, or R Notebook to set the working
# directory to scripts' file location
f_setwd()

# Restore your current working directory
f_setwd(current_wd)
}

```
