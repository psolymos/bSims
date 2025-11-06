# Reparametrized lognormal distribution

A lognormal distribution parametrized as mean (ybar) and SDlog.

## Usage

``` r
rlnorm2(n, mean = exp(0.5), sdlog = 1)
```

## Arguments

- n:

  number of random numbers desired.

- mean:

  mean.

- sdlog:

  log scale standard deviation.

## Details

Log scale mean is `log(mean) - sdlog^2/2`.

## Value

Vector of random numbers.

## Author

Peter Solymos

## See also

`link{rlnorm}`

## Examples

``` r
summary(rlnorm2(10^6, 1.3, 0.5)) # mean ~ 1.3
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>  0.09961  0.81937  1.14817  1.30124  1.60918 14.82229 
exp(log(1.3) - 0.5^2/2) # ~ median
#> [1] 1.147246
```
