# Predict method for f_boxcox objects

Applies the fitted Box-Cox transformation to new data (forward
transform), or reverses it back to the original scale (inverse
transform). This is useful for transforming hypothesis test parameters
(e.g., `mu`) to the transformed scale, or for back-transforming
confidence intervals to the original scale.

## Usage

``` r
# S3 method for class 'f_boxcox'
predict(object, newdata, inverse = FALSE, ...)
```

## Arguments

- object:

  An object of class `f_boxcox`, as returned by
  [`f_boxcox`](https://delde001.github.io/rfriend/reference/f_boxcox.md).

- newdata:

  A numeric vector of values to transform. For the forward transform
  (`inverse = FALSE`), all values must be strictly positive (Box-Cox
  requires \\y \> 0\\). For the inverse transform (`inverse = TRUE`),
  values are assumed to be on the Box-Cox transformed scale.

- inverse:

  Logical. If `FALSE` (default), applies the forward Box-Cox
  transformation to `newdata` using the estimated \\\lambda\\ from the
  original fit. If `TRUE`, reverses the transformation, mapping values
  from the Box-Cox scale back to the original scale. Default is `FALSE`.

- ...:

  Further arguments passed to or from other methods (currently unused).

## Value

A numeric vector of the same length as `newdata`, containing either the
forward-transformed or back-transformed values.

## Details

The forward transformation applies the standard Box-Cox formula:
\$\$y(\lambda) = \begin{cases} \frac{y^\lambda - 1}{\lambda}, & \lambda
\neq 0 \\ \log(y), & \lambda = 0 \end{cases}\$\$

The inverse transformation reverses this process to recover the original
scale: \$\$y = \begin{cases} (y(\lambda) \cdot \lambda + 1)^{1/\lambda},
& \lambda \neq 0 \\ \exp(y(\lambda)), & \lambda = 0 \end{cases}\$\$

**Note on inverse validity:** When \\\lambda \> 0\\, not all
transformed-scale values have a valid inverse. If \\y(\lambda) \cdot
\lambda + 1 \leq 0\\, the result is undefined and `NaN` is returned with
a warning.

## See also

[`f_boxcox`](https://delde001.github.io/rfriend/reference/f_boxcox.md)

## Examples

``` r
# Assuming mtcars is available and f_boxcox is loaded
bc <- f_boxcox(mtcars$hp)

# Forward: transform a hypothesis value (mu) to the Box-Cox scale
mu <- 100
mu_transformed <- predict(bc, newdata = mu)

# Inverse: back-transform a confidence interval to the original scale
ci_transformed <- c(5.5, 6.8)
predict(bc, newdata = ci_transformed, inverse = TRUE)
#> [1]  84.21288 192.00767

# Round-trip sanity check should return exactly mu (e.g., 100)
predict(bc, newdata = mu_transformed, inverse = TRUE)
#> [1] 100
```
