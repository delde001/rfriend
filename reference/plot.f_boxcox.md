# Plot an f_boxcox object

Create diagnostic plots of an object of class `f_boxcox`.

## Usage

``` r
# S3 method for class 'f_boxcox'
plot(x, which = 1:3, ask = FALSE, ...)
```

## Arguments

- x:

  An object of class `f_boxcox`.

- which:

  Integer determining which graph to plot. Default is `1:2`.

- ask:

  Logical. `TRUE` waits with plotting each graph until \<Return\> is
  pressed. Default is `FALSE`.

- ...:

  Further arguments passed to or from other methods.

## Value

This function is called for its side effect of generating plots and does
not return a useful value. It invisibly returns `1`.

## Details

Plot method for f_boxcox objects
