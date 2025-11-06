# Run Shiny apps

Run the Shiny apps that are included in the bSims package.

## Usage

``` r
run_app(app = c("bsimsH", "bsimsHER", "distfunH", "distfunHER"))
```

## Arguments

- app:

  character, which app to run.

## Details

`"bsimsH"`: explore simulation settings in a single stratum.

`"bsimsHER"`: explore simulation settings in multiple strata.

`"distfunH"`: explore distance functions through a single stratum.

`"distfunHER"`: explore distance functions through multiple strata with
segmented sound attenuation (see [`dist_fun2`](dist_fun2.md)).

## See also

[`bsims_init`](bsims_init.md)
