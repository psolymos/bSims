# Simulation workflow in bSims

We recommend exploring the simulation settings interactively in the
**shiny** apps using `run_app("bsimsH")` app for the homogeneous habitat
case and the `run_app("bsimsHER")` app for the stratified habitat case.
The apps represent the simulation layers as tabs, the last tab
presenting the settings that can be copied onto the clipboard and pasted
into the R session or code. In simple situations, comparing results from
a few different settings might be enough.

Let us consider the following simple comparison: we want to see how much
of an effect does roads have when the only effect is that the road
stratum is unsuitable. Otherwise there are no behavioral or
detectability effects of the road.

``` r

library(bSims)

tint <- c(2, 4, 6, 8, 10)
rint <- c(0.5, 1, 1.5, 2, Inf) # unlimited

## no road
b1 <- bsims_all(
  road = 0,
  density = c(1, 1, 0),
  tint = tint,
  rint = rint)
## road
b2 <- bsims_all(
  road = 0.5,
  density = c(1, 1, 0),
  tint = tint,
  rint = rint)
b1
#> bSims wrapper object with settings:
#>   road   : 0
#>   density: 1, 1, 0
#>   tint   : 2, 4, 6, 8, 10
#>   rint   : 0.5, 1, 1.5, 2, Inf
b2
#> bSims wrapper object with settings:
#>   road   : 0.5
#>   density: 1, 1, 0
#>   tint   : 2, 4, 6, 8, 10
#>   rint   : 0.5, 1, 1.5, 2, Inf
```

The `bsims_all` function accepts all the arguments we discussed before
for the simulation layers. Unspecified arguments will be taken to be the
default value. However, `bsims_all` does not evaluate these arguments,
but it creates a closure with the settings. Realizations can be drawn
as:

``` r

b1$new()
#> bSims transcript
#>   1 km x 1 km
#>   stratification: H
#>   total abundance: 92
#>   duration: 10 min
#>   detected: 8 heard
#>   1st event detected by breaks:
#>     [0, 2, 4, 6, 8, 10 min]
#>     [0, 50, 100, 150, 200, Inf m]
b2$new()
#> bSims transcript
#>   1 km x 1 km
#>   stratification: HR
#>   total abundance: 91
#>   duration: 10 min
#>   detected: 3 heard
#>   1st event detected by breaks:
#>     [0, 2, 4, 6, 8, 10 min]
#>     [0, 50, 100, 150, 200, Inf m]
```

Run multiple realizations is done as:

``` r

B <- 25  # number of runs
bb1 <- b1$replicate(B)
bb2 <- b2$replicate(B)
```

The replicate function takes an argument for the number of replicates
(`B`) and returns a list of transcript objects with B elements. The `cl`
argument can be used to parallelize the work, it can be a numeric value
on Unix/Linux/OSX, or a cluster object on any OS. The `recover = TRUE`
argument allows to run simulations with error catching.

Simulated objects returned by `bsims_all` will contain different
realizations and all the conditionally independent layers. Use a
customized layered approach if former layers are meant to be kept
identical across runs.

In more complex situations the **shiny** apps will help identifying
corner cases that are used to define a gradient of settings for single
or multiple simulation options. You can copy the `bsims_all` settings
from the app to be used in simulations.

## Sensitivity analysis

In a sensitivity analysis we evaluate how varying one or more settings
affect the estimates. This requires setting up a series of values for a
setting (argument) while keeping others constant.

Let us consider the following scenario: we would like to evaluate how
the estimates are changing with increasing road width. We will use the
`expand_list` function which creates a list from all combinations of the
supplied inputs. Note that we need to wrap vectors inside
[`list()`](https://rdrr.io/r/base/list.html) to avoid interpreting those
as values to iterate over.

``` r

s <- expand_list(
  road = c(0, 0.5, 1),
  density = list(c(1, 1, 0)),
  tint = list(tint),
  rint = list(rint))
str(s)
#> List of 3
#>  $ :List of 4
#>   ..$ road   : num 0
#>   ..$ density: num [1:3] 1 1 0
#>   ..$ tint   : num [1:5] 2 4 6 8 10
#>   ..$ rint   : num [1:5] 0.5 1 1.5 2 Inf
#>  $ :List of 4
#>   ..$ road   : num 0.5
#>   ..$ density: num [1:3] 1 1 0
#>   ..$ tint   : num [1:5] 2 4 6 8 10
#>   ..$ rint   : num [1:5] 0.5 1 1.5 2 Inf
#>  $ :List of 4
#>   ..$ road   : num 1
#>   ..$ density: num [1:3] 1 1 0
#>   ..$ tint   : num [1:5] 2 4 6 8 10
#>   ..$ rint   : num [1:5] 0.5 1 1.5 2 Inf
```

We now can use this list of settings to run simulations for each. The
following illustrates the use of multiple cores:

``` r

b <- lapply(s, bsims_all)
nc <- 4 # number of cores to use
library(parallel)
cl <- makeCluster(nc)
bb <- lapply(b, function(z) z$replicate(B, cl=cl))
stopCluster(cl)
```

In some cases, we want to evaluate crossed effects of multiple settings.
This will give us information about how these settings interact. For
example, road width and spatial pattern (random vs. clustered):

``` r

s <- expand_list(
  road = c(0, 0.5),
  xy_fun = list(
    NULL,
    function(d) exp(-d^2/1^2) + 0.5*(1-exp(-d^2/4^2))),
  density = list(c(1, 1, 0)),
  tint = list(tint),
  rint = list(rint))
str(s)
#> List of 4
#>  $ :List of 5
#>   ..$ road   : num 0
#>   ..$ xy_fun : NULL
#>   ..$ density: num [1:3] 1 1 0
#>   ..$ tint   : num [1:5] 2 4 6 8 10
#>   ..$ rint   : num [1:5] 0.5 1 1.5 2 Inf
#>  $ :List of 5
#>   ..$ road   : num 0.5
#>   ..$ xy_fun : NULL
#>   ..$ density: num [1:3] 1 1 0
#>   ..$ tint   : num [1:5] 2 4 6 8 10
#>   ..$ rint   : num [1:5] 0.5 1 1.5 2 Inf
#>  $ :List of 5
#>   ..$ road   : num 0
#>   ..$ xy_fun :function (d)  
#>   ..$ density: num [1:3] 1 1 0
#>   ..$ tint   : num [1:5] 2 4 6 8 10
#>   ..$ rint   : num [1:5] 0.5 1 1.5 2 Inf
#>  $ :List of 5
#>   ..$ road   : num 0.5
#>   ..$ xy_fun :function (d)  
#>   ..$ density: num [1:3] 1 1 0
#>   ..$ tint   : num [1:5] 2 4 6 8 10
#>   ..$ rint   : num [1:5] 0.5 1 1.5 2 Inf
```

## Varying landscapes

Studying covariate effects on density, cue rates, and detection
distances sometimes require that we simulate a series of landscapes that
differ.

We exploit the fact that arguments to `bsims_all` can be supplied as a
list, which is the same as a single-row data frame. This should work for
all arguments that accept atomic vectors as arguments:

``` r

bsims_all(
  road = 0.5,
  density = 1)
#> bSims wrapper object with settings:
#>   road   : 0.5
#>   density: 1

bsims_all(
  list(
    road = 0.5,
    density = 1))
#> bSims wrapper object with settings:
#>   road   : 0.5
#>   density: 1

bsims_all(
  data.frame(
    road = 0.5,
    density = 1))
#> bSims wrapper object with settings:
#>   road   : 0.5
#>   density: 1
```

``` r

# number of stations to visit
n <- 5

# random predictors: continuous and discrete
x <- data.frame(x1=runif(n,-1,2), x2=rnorm(n))

# density
D <- drop(exp(model.matrix(~x2, x) %*% c(0,-0.5)))
summary(D)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.3504  0.5350  0.8555  1.0259  1.1943  2.1945

# cue rate
phi <- drop(exp(model.matrix(~x1+I(x1^2), x) %*% c(-1,-0.25,-1)))
summary(phi)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.1288  0.2333  0.2734  0.2616  0.3053  0.3673

# this data frame collects the columns to be used as arguments
s <- data.frame(
    D=D,
    vocal_rate = phi, 
    duration = 10,
    condition = "det1",
    tau = 1)

# each row from s becomes a simulation settings object
bb <- lapply(1:n, function(i) bsims_all(s[i,]))

# define how you want the data extracted
get_counts <- function(b) {
    o <- b$new() # simulate
    get_table(o)[1,1]
}

x$y <- sapply(bb, get_counts)
x
#>           x1         x2 y
#> 1  0.9069551 -1.5718783 2
#> 2  0.4338611  2.0973895 6
#> 3 -0.2565603 -0.3550863 4
#> 4  0.3244926  1.2508359 6
#> 5  0.5613159  0.3122454 7
```

Read more in the paper:

Solymos, P. 2023. Agent-based simulations improve abundance estimation.
*Biologia Futura* 74, 377–392 [DOI
10.1007/s42977-023-00183-2](https://doi.org/10.1007/s42977-023-00183-2).
