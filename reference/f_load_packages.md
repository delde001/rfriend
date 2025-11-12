# Install and Load Multiple R Packages

Checks if the specified packages are installed. If not, it installs them
and then loads them into the global R session.

## Usage

``` r
f_load_packages(...)
```

## Arguments

- ...:

  Unquoted or quoted names of packages to be installed and loaded. These
  should be valid package names available on CRAN.

## Value

None. The function is called for its side effects of installing and
loading packages.

## Details

The function takes a list or vector indicating package names, installs
any that are missing, and loads all specified packages into the global
environment of the R session. It uses
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) to check for
installation and [`library()`](https://rdrr.io/r/base/library.html) to
load the packages.

## Author

Sander H. van Delden <plantmind@proton.me>  
