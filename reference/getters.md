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
#> 1 1 H -4.315038 -1.9201875 NA
#> 2 2 H -2.462982 -4.3714492 NA
#> 3 3 H -1.806481 -1.9443110 NA
#> 4 4 H -2.811867  4.6403058 NA
#> 5 5 H -4.031108 -4.4926347 NA
#> 6 6 H -1.992186  0.1235921 NA
head(get_nests(e))
#>   i s         x          y  g
#> 1 1 H -4.315038 -1.9201875 G1
#> 2 2 H -2.462982 -4.3714492 G1
#> 3 3 H -1.806481 -1.9443110 G1
#> 4 4 H -2.811867  4.6403058 G1
#> 5 5 H -4.031108 -4.4926347 G1
#> 6 6 H -1.992186  0.1235921 G1
head(get_nests(d))
#>   i s         x          y  g
#> 1 1 H -4.315038 -1.9201875 G1
#> 2 2 H -2.462982 -4.3714492 G1
#> 3 3 H -1.806481 -1.9443110 G1
#> 4 4 H -2.811867  4.6403058 G1
#> 5 5 H -4.031108 -4.4926347 G1
#> 6 6 H -1.992186  0.1235921 G1
head(get_nests(x))
#>   i s         x          y  g
#> 1 1 H -4.315038 -1.9201875 G1
#> 2 2 H -2.462982 -4.3714492 G1
#> 3 3 H -1.806481 -1.9443110 G1
#> 4 4 H -2.811867  4.6403058 G1
#> 5 5 H -4.031108 -4.4926347 G1
#> 6 6 H -1.992186  0.1235921 G1

## abundance
get_abundance(p)
#> [1] 95
get_abundance(e)
#> [1] 95
get_abundance(d)
#> [1] 95
get_abundance(x)
#> [1] 95

## density
get_density(p)
#> [1] 0.95
get_density(e)
#> [1] 0.95
get_density(d)
#> [1] 0.95
get_density(x)
#> [1] 0.95

## events
head(get_events(e))
#>            x          y          t v   a  i
#> 1 -0.3738905  0.1742159 0.00480860 1   0 44
#> 2  3.4598208 -3.0812244 0.01074640 1 156 89
#> 3 -2.2047155 -3.9809563 0.02045773 1 284 34
#> 4  2.8201059  3.0568185 0.02574542 1 336 82
#> 5  2.8591436 -1.0125381 0.02627925 1 193 90
#> 6 -1.1423269  1.5779129 0.02695094 1 327 43
head(get_events(d))
#>            x          y          t v   a  f  i
#> 1 -0.3738905  0.1742159 0.00480860 1   0 NA 44
#> 2  3.4598208 -3.0812244 0.01074640 1 156 NA 89
#> 3 -2.2047155 -3.9809563 0.02045773 1 284 NA 34
#> 4  2.8201059  3.0568185 0.02574542 1 336 NA 82
#> 5  2.8591436 -1.0125381 0.02627925 1 193 NA 90
#> 6 -1.1423269  1.5779129 0.02695094 1 327 NA 43
head(get_events(x))
#>            x          y          t v   a  f  i
#> 1 -0.3738905  0.1742159 0.00480860 1   0 NA 44
#> 2  3.4598208 -3.0812244 0.01074640 1 156 NA 89
#> 3 -2.2047155 -3.9809563 0.02045773 1 284 NA 34
#> 4  2.8201059  3.0568185 0.02574542 1 336 NA 82
#> 5  2.8591436 -1.0125381 0.02627925 1 193 NA 90
#> 6 -1.1423269  1.5779129 0.02695094 1 327 NA 43

## detections
head(get_detections(d))
#>              x          y          t v   a         d  f  i  j
#> 1  -0.37389047  0.1742159 0.00480860 1   0 0.4124867 NA 44 44
#> 7  -0.95018898  1.5035194 0.07937606 1  38 1.7786033 NA 41 41
#> 15  0.07908970 -0.4709415 0.17878431 1  28 0.4775365 NA 45 45
#> 17  0.72543673  1.4485678 0.20332589 1 164 1.6200639 NA 65 65
#> 18  1.01222174  1.7485858 0.20601417 1  25 2.0204319 NA 62 62
#> 21  0.05090871 -3.1189755 0.31610480 1 166 3.1193909 NA 50 50
head(get_detections(x))
#>              x          y          t v   a         d  f  i  j
#> 1  -0.37389047  0.1742159 0.00480860 1   0 0.4124867 NA 44 44
#> 7  -0.95018898  1.5035194 0.07937606 1  38 1.7786033 NA 41 41
#> 15  0.07908970 -0.4709415 0.17878431 1  28 0.4775365 NA 45 45
#> 17  0.72543673  1.4485678 0.20332589 1 164 1.6200639 NA 65 65
#> 21  0.05090871 -3.1189755 0.31610480 1 166 3.1193909 NA 50 50
#> 27 -1.99218646  0.1235921 0.39815399 1 330 1.9960165 NA  6  6

get_table(x, "removal")
#>          0-3min 3-5min 5-10min
#> 0-50m         2      0       0
#> 50-100m       0      0       0
#> 100-150m      2      0       0
#> 150+m        10      0       0
get_table(x, "visits")
#>          0-3min 3-5min 5-10min
#> 0-50m         2      1       2
#> 50-100m       0      0       0
#> 100-150m      2      1       2
#> 150+m        10      1      10
```
