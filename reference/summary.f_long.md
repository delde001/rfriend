# Summary method for f_long objects

Automatically runs the `f_summary` function on data created by `f_long`
using the attributes stored in the object.

## Usage

``` r
# S3 method for class 'f_long'
summary(object, ...)
```

## Arguments

- object:

  An object of class `f_long` (output from `f_long`).

- ...:

  Additional arguments passed to `f_summary`.

## Value

Returns the summary table (usually a data frame or tibble) produced by
`f_summary`.
