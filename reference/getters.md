# Access nests, events, detections, and totals

Access nests, events, detections, abundance, and density from simulation
objects.

## Usage

``` r
get_nests(x, ...)
# S3 method for class 'bsims_population'
get_nests(x, ...)

get_events(x, ...)
# S3 method for class 'bsims_events'
get_events(x, ...)

get_detections(x, ...)
# S3 method for class 'bsims_detections'
get_detections(x, ...)

get_abundance(x, ...)
# S3 method for class 'bsims_population'
get_abundance(x, ...)

get_density(x, ...)
# S3 method for class 'bsims_population'
get_density(x, ...)

get_table(x, ...)
# S3 method for class 'bsims_transcript'
get_table(x,
  type = c("removal", "visits"), ...)
```

## Arguments

- x:

  simulation object.

- type:

  character, the type of table to return: `"removal"` includes only new
  individuals as time progresses, `"visits"` counts individuals in each
  time interval independent of each other.

- ...:

  other arguments passed to internal functions.

## Details

`get_nests` extracts the nest locations.

`get_events` extracts the events.

`get_detections` extracts the detections.

`get_abundance` gets the realized total abundance (N), `get_density`
gets the realized average density (abundance/area: N/A).

`get_table` returns the removal or visits table.

## Value

`get_abundance` and `get_density` returns a non-negative numeric value.

`get_nests` returns a data frame with the following columns: `i`
individual identifier, `s` spatial stratum (H: habitat, E: edge, R:
road) `x` and `y` are coordinates of the nest locations, `g` is
behavioral (mixture) group or `NA`.

`get_events` returns a data frame with the following columns: `x` and
`y` are locations of the individual at the time of the event, `t` time
of the event within the duration interval, `v` indicator variable for
vocal (1) vs. movement (0) event, `a` direction for vocalization events
(`NA` for movement) in degrees clockwise relative to north, `i`
individual identifier.

`get_detections` returns a data frame with the following columns: `x`
and `y` are locations of the individual at the time of the event, `t`
time of the event within the duration interval, `v` indicator variable
for vocal (1) vs. movement (0) event, `a` direction for vocalization
events (`NA` for movement) in degrees clockwise relative to north, `d`
distance from observer when detected (otherwise `NA`). `f` indicates the
angle between the bird's vocalization direction (column `a`) relative to
the observer (the value is 0 for movement events by default), `i`
individual identifier, `j` perceived individual identifier.

`get_table` returns a matrix with distance bands as rows and time
intervals as columns. The cell values are counts if the individuals
detected in a removal fashion (only new individuals counter over the
time periods) or in a multiple-visits fashion (counting of individuals
restarts in every time interval).

## Author

Peter Solymos

## See also

[`bsims_init`](bsims_init.md)

## Examples

``` r
phi <- 0.5                 # singing rate
tau <- 1:3                 # EDR by strata
dur <- 10                  # simulation duration
tbr <- c(3, 5, 10)         # time intervals
rbr <- c(0.5, 1, 1.5, Inf) # counting radii

l <- bsims_init(10, 0.5, 1)# landscape
p <- bsims_populate(l, 1)  # population
e <- bsims_animate(p,      # events
  vocal_rate=phi, duration=dur)
d <- bsims_detect(e,       # detections
  tau=tau)
x <- bsims_transcribe(d,   # transcription
  tint=tbr, rint=rbr)

## next locations
head(get_nests(p))
#>   i s         x          y  g
#> 1 1 H -1.992186  4.1429109 NA
#> 2 2 H -3.397745  0.3655374 NA
#> 3 3 H -3.619101 -0.5493512 NA
#> 4 4 H -2.372446 -3.3058973 NA
#> 5 5 H -2.850421 -4.2631502 NA
#> 6 6 H -2.972889  2.2975645 NA
head(get_nests(e))
#>   i s         x          y  g
#> 1 1 H -1.992186  4.1429109 G1
#> 2 2 H -3.397745  0.3655374 G1
#> 3 3 H -3.619101 -0.5493512 G1
#> 4 4 H -2.372446 -3.3058973 G1
#> 5 5 H -2.850421 -4.2631502 G1
#> 6 6 H -2.972889  2.2975645 G1
head(get_nests(d))
#>   i s         x          y  g
#> 1 1 H -1.992186  4.1429109 G1
#> 2 2 H -3.397745  0.3655374 G1
#> 3 3 H -3.619101 -0.5493512 G1
#> 4 4 H -2.372446 -3.3058973 G1
#> 5 5 H -2.850421 -4.2631502 G1
#> 6 6 H -2.972889  2.2975645 G1
head(get_nests(x))
#>   i s         x          y  g
#> 1 1 H -1.992186  4.1429109 G1
#> 2 2 H -3.397745  0.3655374 G1
#> 3 3 H -3.619101 -0.5493512 G1
#> 4 4 H -2.372446 -3.3058973 G1
#> 5 5 H -2.850421 -4.2631502 G1
#> 6 6 H -2.972889  2.2975645 G1

## abundance
get_abundance(p)
#> [1] 99
get_abundance(e)
#> [1] 99
get_abundance(d)
#> [1] 99
get_abundance(x)
#> [1] 99

## density
get_density(p)
#> [1] 0.99
get_density(e)
#> [1] 0.99
get_density(d)
#> [1] 0.99
get_density(x)
#> [1] 0.99

## events
head(get_events(e))
#>           x          y          t v   a  i
#> 1 -1.416591  0.1742159 0.00480860 1   0 45
#> 2  2.606151  1.8374771 0.01074640 1 156 90
#> 3  1.984942 -0.5702172 0.01440594 1  54 99
#> 4 -4.822422  3.8523376 0.02045773 1 284 35
#> 5  3.942365 -4.3564158 0.02574542 1 336 83
#> 6  3.851393 -2.9408766 0.02627925 1 193 91
head(get_events(d))
#>           x          y          t v   a  f  i
#> 1 -1.416591  0.1742159 0.00480860 1   0 NA 45
#> 2  2.606151  1.8374771 0.01074640 1 156 NA 90
#> 3  1.984942 -0.5702172 0.01440594 1  54 NA 99
#> 4 -4.822422  3.8523376 0.02045773 1 284 NA 35
#> 5  3.942365 -4.3564158 0.02574542 1 336 NA 83
#> 6  3.851393 -2.9408766 0.02627925 1 193 NA 91
head(get_events(x))
#>           x          y          t v   a  f  i
#> 1 -1.416591  0.1742159 0.00480860 1   0 NA 45
#> 2  2.606151  1.8374771 0.01074640 1 156 NA 90
#> 3  1.984942 -0.5702172 0.01440594 1  54 NA 99
#> 4 -4.822422  3.8523376 0.02045773 1 284 NA 35
#> 5  3.942365 -4.3564158 0.02574542 1 336 NA 83
#> 6  3.851393 -2.9408766 0.02627925 1 193 NA 91

## detections
head(get_detections(d))
#>             x          y          t v   a         d  f  i  j
#> 8  -0.8422087  1.5804489 0.07937606 1  38 1.7908473 NA 42 42
#> 13 -0.2569150  0.1222174 0.10797022 1 189 0.2845038 NA 56 56
#> 17 -1.1826129 -0.4709415 0.17878431 1  28 1.2729333 NA 46 46
#> 22 -0.3118975  1.5639444 0.31610480 1 166 1.5947420 NA 51 51
#> 26  1.6701308  0.5072672 0.41213545 1 276 1.7454676 NA 72 72
#> 45  0.6549135 -2.4654480 0.76787103 1  73 2.5509500 NA 58 58
head(get_detections(x))
#>             x          y          t v   a         d  f  i  j
#> 8  -0.8422087  1.5804489 0.07937606 1  38 1.7908473 NA 42 42
#> 13 -0.2569150  0.1222174 0.10797022 1 189 0.2845038 NA 56 56
#> 17 -1.1826129 -0.4709415 0.17878431 1  28 1.2729333 NA 46 46
#> 22 -0.3118975  1.5639444 0.31610480 1 166 1.5947420 NA 51 51
#> 26  1.6701308  0.5072672 0.41213545 1 276 1.7454676 NA 72 72
#> 45  0.6549135 -2.4654480 0.76787103 1  73 2.5509500 NA 58 58

get_table(x, "removal")
#>          0-3min 3-5min 5-10min
#> 0-50m         2      0       0
#> 50-100m       1      0       0
#> 100-150m      1      0       0
#> 150+m         6      2       0
get_table(x, "visits")
#>          0-3min 3-5min 5-10min
#> 0-50m         2      0       1
#> 50-100m       1      1       0
#> 100-150m      1      1       3
#> 150+m         6      3      10
```
