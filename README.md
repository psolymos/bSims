# bSims: Bird Point Count Simulator <img src="https://raw.githubusercontent.com/psolymos/bSims/master/bsims.gif" align="right" style="padding-left:10px;background-color:white;" />

A highly scientific and utterly addictive bird point count simulator to
test statistical assumptions and to aid survey design.

[![CRAN
version](https://www.r-pkg.org/badges/version/bSims)](https://cran.rstudio.com/web/packages/bSims/index.html)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/bSims)](https://www.rdocumentation.org/packages/bSims/)
[![Checks](https://github.com/psolymos/bSims/workflows/deploy/badge.svg)](https://github.com/psolymos/bSims/actions)


> *“I’ve yet to see any problem, however complicated, which when you
> looked at it the right way didn’t become still more complicated.”* –
> Poul Anderson, Call Me Joe

> *“Love the simulation we're dreaming in”* - Dua Lipa, Physical

The goal of the package is to:

  - test statistical assumptions,
  - aid survey design,
  - and have fun while doing it\!

Design objectives:

  - small (point count) scale implementation,
  - habitat is considered homogeneous except for edge effects,
  - realistic but efficient implementation of biological mechanisms and
    observation process,
  - defaults chosen to reflect common practice and assumptions,
  - extensible (PRs are welcome).

See the package in action in the [QPAD
Book](https://peter.solymos.org/qpad-book/).

## Install

CRAN version:

``` r
install.packages("bSims")
```

Development version:

``` r
remotes::install_github("psolymos/bSims")
```

See what is new in the [NEWS](NEWS.md) file.

## License

[GPL-2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)

## Contributing

Feedback and contributions are welcome:

  - submit feature request or report issues
    [here](https://github.com/psolymos/bSims/issues),
  - fork the project and submit pull request, see
    [CoC](CODE_OF_CONDUCT.md).

## Examples

### Command line

``` r
library(bSims)

phi <- 0.5
tau <- 1:3
dur <- 10
rbr <- c(0.5, 1, 1.5, Inf)
tbr <- c(3, 5, 10)

l <- bsims_init(10, 0.5, 1)
p <- bsims_populate(l, 1)
a <- bsims_animate(p, vocal_rate=phi, duration=dur)
o <- bsims_detect(a, tau=tau)

x <- bsims_transcribe(o, tint=tbr, rint=rbr)

get_table(x)
#>          0-3min 3-5min 5-10min
#> 0-50m         1      0       1
#> 50-100m       2      0       0
#> 100-150m      5      0       0
#> 150+m         5      3       1

head(get_events(a))
#>            x         y          t v  i
#> 1 -3.6616422 -1.676053 0.01126843 1 12
#> 2  4.6607856  4.537327 0.02661606 1 96
#> 3 -0.2867919  2.155661 0.05207233 1 47
#> 4  2.6507206 -1.110949 0.06329550 1 69
#> 5  2.1330323 -2.167675 0.11365119 1 72
#> 6  0.4926841 -3.517884 0.12323517 1 45

head(get_detections(o))
#>             x           y          t v         d  i  j
#> 3  -0.2867919  2.15566066 0.05207233 1 2.1746546 47 47
#> 10  0.7075451  1.01541218 0.26632984 1 1.2376114 58 58
#> 14  0.5770644 -0.47429169 0.35091111 1 0.7469645 62 62
#> 16  0.4761707 -0.04406422 0.35179595 1 0.4782052 52 52
#> 18  1.0957120 -2.41834073 0.45279692 1 2.6549871 60 60
#> 33  1.0111698  1.82788079 0.87025627 1 2.0889262 57 57
```

### Shiny apps

A few [Shiny](https://shiny.rstudio.com/) apps come with the package.
These can be used to interactively explore the effects of different
settings.

Compare distance functions:

``` r
run_app("distfunH")
run_app("distfunHER")
```

Compare simulation settings for single landscape:

``` r
run_app("bsimsH")
run_app("bsimsHER")
```

### Replicating simulations

Interactive sessions can be used to explore different settings. Settings
can be copied from the Shiny apps and replicated using the `bsims_all`
function:

``` r
b <- bsims_all(extent=5, road=1, density=c(1,1,0))
b
#> bSims wrapper object with settings:
#>   extent : 5
#>   road   : 1
#>   density: 1, 1, 0
```

The object has handy methods:

``` r
b$settings()      # retrieve settings
b$new()           # replicate once
b$replicate(10)   # replicate 10x
```

The `$replicate()` function also runs on multiple cores:

``` r
library(parallel)
b <- bsims_all(density=0.5)
B <- 4  # number of runs
nc <- 2 # number of cores

## sequential
system.time(bb <- b$replicate(B, cl=NULL))
#>    user  system elapsed 
#>   0.790   0.013   0.830

## parallel clusters
cl <- makeCluster(nc)
## note: loading the package is optional
system.time(clusterEvalQ(cl, library(bSims)))
#>    user  system elapsed 
#>   0.001   0.000   1.289
system.time(bb <- b$replicate(B, cl=cl))
#>    user  system elapsed 
#>   0.013   0.002   0.655
stopCluster(cl)

## parallel forking
if (.Platform$OS.type != "windows") {
  system.time(bb <- b$replicate(B, cl=nc))
}
#>    user  system elapsed 
#>   0.413   0.108   0.544
```
