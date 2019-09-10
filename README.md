# bSims: Bird Point Count Simulator <img src="bsims.gif" align="right" style="padding-left:10px;background-color:white;" />
> A highly scientific and utterly addictive bird point count simulator to test statistical assumptions and to aid survey design.

[![Linux build status](https://travis-ci.org/psolymos/bSims.svg?branch=master)](https://travis-ci.org/psolymos/bSims)
[![codecov](https://codecov.io/gh/psolymos/bSims/branch/master/graph/badge.svg)](https://codecov.io/gh/psolymos/bSims)

The goal of the package is to:

- test statistical assumptions,
- aid survey design,
- and have fun while doing it!

See the package in action in the [QPAD Book](https://peter.solymos.org/qpad-book/).

## Install

```R
remotes::install_github("psolymos/bSims")
```

See what is new in the [NEWS](NEWS.md) file.

## Feedback

Submit issues and feature requests through [GitHub issues](https://github.com/psolymos/bSims/issues).

## License

[GPL-2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)


## Shiny apps

```R
shiny::runApp(system.file("shiny/distancefun.R", package="bSims"))
shiny::runApp(system.file("shiny/bsimsH.R", package="bSims"))
shiny::runApp(system.file("shiny/bsimsHER.R", package="bSims"))
```

## Examples

```R
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
## bSims transcript
##   1 km x 1 km
##   stratification: HER
##   total abundance: 77
##   duration: 10 min
##   detected: 26 heard
##   1st event detected by bins:
##     [0-3, 3-5, 5-10 min]
##     [0-50, 50-100, 100-150, 150+ m]

plot(x)
get_table(x)
##          0-3min 3-5min 5-10min
## 0-50m         1      0       0
## 50-100m       1      0       0
## 100-150m      1      0       2
## 150+m         6      0       1

head(get_events(a))
##            x          y          t v  i
## 1  3.5992549  0.6185196 0.02475470 1 55
## 2  4.8767160  4.3257662 0.08328357 1 47
## 3 -0.1561816  2.0859117 0.08439890 1 32
## 4  1.9778737 -2.9935283 0.10476006 1 74
## 5 -3.4935711 -1.0103386 0.13678520 1 22
## 6 -4.0149126 -4.2494879 0.13809347 1 20

head(get_detections(o))
##             x          y         t v         d  i
## 3  -0.1561816  2.0859117 0.0843989 1 2.0917505 32
## 11  0.4427569  1.7484640 0.1678333 1 1.8036519 36
## 15  1.8089010 -1.8998271 0.4414374 1 2.6232548 58
## 21 -1.2162436 -0.2085822 0.6713021 1 1.2339996 29
## 52  0.2042901 -0.2897062 1.6134511 1 0.3544914 39
## 57 -1.8040735  0.1768196 1.7666779 1 1.8127180 18
```
