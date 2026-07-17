# Access the bundled example datasets

Convenience accessor for the example data files shipped with the package
(used throughout the rfriend course). With no arguments it lists the
available files. Given a file name it returns the full path to the
installed file, ready to pass to a reader such as
[`read.csv`](https://rdrr.io/r/utils/read.table.html),
[`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html)
or
[`f_open_file`](https://delde001.github.io/rfriend/reference/f_open_file.md).
Optionally it copies the file to a destination so learners can practise
importing a file that sits in their own working directory.

## Usage

``` r
f_example_data(file = NULL, dest = NULL, overwrite = FALSE)
```

## Arguments

- file:

  Character string: the name of an example file (e.g.
  `"plant_trial_raw.csv"`). If `NULL` (the default) the function returns
  the names of all available example files.

- dest:

  Optional path. If supplied, the example file is copied there and the
  path of the copy is returned (invisibly). `dest` may be a directory
  (the original file name is kept) or a full target file path. The
  working directory is never changed.

- overwrite:

  Logical; passed to [`file.copy`](https://rdrr.io/r/base/files.html)
  when `dest` is supplied. Default `FALSE`.

## Value

If `file` is `NULL`, a character vector of available file names. If
`file` is supplied and `dest` is `NULL`, the full path to the installed
example file. If `dest` is supplied, the path to the copied file
(returned invisibly).

## Details

The files live in the package's `extdata` directory and are located with
[`system.file`](https://rdrr.io/r/base/system.file.html). Because
[`system.file()`](https://rdrr.io/r/base/system.file.html) returns an
empty string for a missing file, this function checks explicitly and
fails with an informative error listing the valid names, rather than
letting a later reader fail on an empty path.

## Author

Sander H. van Delden <plantmind@proton.me>  

## Examples

``` r
# List the available example files
f_example_data()
#> [1] "boxplot_explained_rfriend.png" "plant_trial.csv"              
#> [3] "plant_trial.rds"               "plant_trial.sav"              
#> [5] "plant_trial_raw.csv"           "plant_trial_raw.xlsx"         
#> [7] "plant_trial_wide.csv"         

# Get the path to one of them, then read it
path <- f_example_data("plant_trial_raw.csv")
df <- read.csv(path)

if (FALSE) { # \dontrun{
# Copy a file into the current working directory to import it "from disk"
f_example_data("plant_trial_raw.xlsx", dest = getwd())

# Open an example file in its default application
f_open_file(f_example_data("plant_trial_raw.xlsx"))
} # }
```
