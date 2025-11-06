# Multivariate normal distribution

A shim of [`mvrnorm`](https://rdrr.io/pkg/MASS/man/mvrnorm.html) to
return matrix when n \< 2.

## Usage

``` r
rmvn(n = 1L, mu, Sigma, ...)
```

## Arguments

- n:

  number of random vectors desired (nonnegative integer, can be 0).

- mu:

  mean vector.

- Sigma:

  variance-covariance matrix.

- ...:

  other arguments passed to
  [`mvrnorm`](https://rdrr.io/pkg/MASS/man/mvrnorm.html).

## Value

A matrix with `n` rows and `length(mu)` columns.

## Author

Peter Solymos

## See also

[`mvrnorm`](https://rdrr.io/pkg/MASS/man/mvrnorm.html)

## Examples

``` r
rmvn(0, c(a=0, b=0), diag(1, 2, 2))
#>      a b
rmvn(1, c(a=0, b=0), diag(1, 2, 2))
#>              a          b
#> [1,] -1.082776 -0.8530601
rmvn(2, c(a=0, b=0), diag(1, 2, 2))
#>               a        b
#> [1,] -0.4318512 0.667875
#> [2,] -0.4464597 0.286626

sapply(0:10, function(n) dim(rmvn(n, c(a=0, b=0), diag(1, 2, 2))))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
#> [1,]    0    1    2    3    4    5    6    7    8     9    10
#> [2,]    2    2    2    2    2    2    2    2    2     2     2
```
