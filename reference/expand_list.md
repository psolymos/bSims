# Create a list from all combinations of arguments

Create a list from all combinations of the supplied vectors or lists.

## Usage

``` r
expand_list(...)
```

## Arguments

- ...:

  vectors or lists. All arguments must be named.

## Value

A list containing one element for each combination of the supplied
vectors and lists. The first factors vary fastest. The nested elements
are labeled by the factors.

The function allows list elements to be vectors, functions, or `NULL`.
If a vector element is supposed to be kept as a vector, use
[`list()`](https://rdrr.io/r/base/list.html).

## Author

Peter Solymos

## See also

[`expand.grid`](https://rdrr.io/r/base/expand.grid.html)

## Examples

``` r
b <- expand_list(
  movement = c(0, 1, 2),
  rint = list(c(0.5, 1, 1.5, Inf)), # in a list to keep as one
  xy_fun = list(NULL, function(z) z))
b[[1]]
#> $movement
#> [1] 0
#> 
#> $rint
#> [1] 0.5 1.0 1.5 Inf
#> 
#> $xy_fun
#> NULL
#> 
str(b)
#> List of 6
#>  $ :List of 3
#>   ..$ movement: num 0
#>   ..$ rint    : num [1:4] 0.5 1 1.5 Inf
#>   ..$ xy_fun  : NULL
#>  $ :List of 3
#>   ..$ movement: num 1
#>   ..$ rint    : num [1:4] 0.5 1 1.5 Inf
#>   ..$ xy_fun  : NULL
#>  $ :List of 3
#>   ..$ movement: num 2
#>   ..$ rint    : num [1:4] 0.5 1 1.5 Inf
#>   ..$ xy_fun  : NULL
#>  $ :List of 3
#>   ..$ movement: num 0
#>   ..$ rint    : num [1:4] 0.5 1 1.5 Inf
#>   ..$ xy_fun  :function (z)  
#>  $ :List of 3
#>   ..$ movement: num 1
#>   ..$ rint    : num [1:4] 0.5 1 1.5 Inf
#>   ..$ xy_fun  :function (z)  
#>  $ :List of 3
#>   ..$ movement: num 2
#>   ..$ rint    : num [1:4] 0.5 1 1.5 Inf
#>   ..$ xy_fun  :function (z)  
```
