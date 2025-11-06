# Estimate basic parameters

Estimate singing rates, effective distances, and density based on
simulation objects using the QPAD approach (Solymos et al. 2013).

## Usage

``` r
estimate(object, ...)
# S3 method for class 'bsims_transcript'
estimate(object, 
  method = c("qpad", "sqpad", "convolution", "naive"), ...)
```

## Arguments

- object:

  simulation object.

- method:

  method.

- ...:

  other arguments passed to internal functions.

## Details

The method evaluates removal design to estimate model parameters and
density using the QPAD and SQPAD methodologies using the 'detect'
package. Convolution implements the full-information likelihood. The
Navive estimator fits GLM assuming no detection error.

The function only works with multiple time and distance intervals. It
returns `NA` otherwise.

## Value

A vector with values for singing rate (phi), effective detection
distance (tau), density, and survey area.

## References

Solymos, P., Matsuoka, S. M., Bayne, E. M., Lele, S. R., Fontaine, P.,
Cumming, S. G., Stralberg, D., Schmiegelow, F. K. A. & Song, S. J.,
2013. Calibrating indices of avian density from non-standardized survey
data: making the most of a messy situation. *Methods in Ecology and
Evolution*, **4**: 1047–1058. \<doi:10.1111/2041-210X.12106\>

Solymos, P., Lele, S. R., 2025. Single bin QPAD (SQPAD) approach for
robust analysis of point count data with detection error.
*Ornithological Applications*, **xx**, xx–xx.

## Author

Peter Solymos

## See also

[`bsims_init`](bsims_init.md)

## Examples

``` r
set.seed(2)
phi <- 0.5                 # singing rate
tau <- 1                   # EDR by strata
dur <- 10                  # simulation duration
tbr <- c(2, 4, 6, 8, 10)   # time intervals
rbr <- c(0.5, 1, 1.5, Inf) # counting radii

l <- bsims_init(10, 0.5, 1)# landscape
p <- bsims_populate(l, 10) # population
e <- bsims_animate(p,      # events
  vocal_rate=phi, duration=dur)
d <- bsims_detect(e,       # detections
  tau=tau)
x <- bsims_transcribe(d,   # transcription
  tint=tbr, rint=rbr)

estimate(x)
#> Loading required namespace: detect
#>        density           area       cue_rate distance_param 
#>     11.3640146      2.5740766      0.4754889      0.9051818 
```
