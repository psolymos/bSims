# Internal functions

Internal functions are not intended to be used by the user.

## Usage

``` r
.get_events(x, event_type=c("vocal", "move", "both"), tlim = NULL)

.get_detections(x, condition = c("event1", "det1", "alldet"),
  event_type = c("vocal", "move", "both"), tlim = NULL,
  perception = NULL)

.bsims_all(Settings)
```

## Arguments

- x:

  simulation object.

- event_type:

  type of events to access.

- condition:

  conditioning type to define availability for each individual:
  `"event1"`: the 1st event (detected or not); `"det1"`: the 1st
  detection; `"alldet"`: all detections (counting the same individual
  multiple times).

- tlim:

  time intervals treated as \[`min(tlim)`, `max(tlim)`).

- perception:

  perceived number of individuals relative to the actual number of
  individuals. A non-negative number (\<1 values lead to under counting,
  \>1 values lead to over counting), or `NULL` (observer correctly
  identifies all individuals).

- Settings:

  a list of arguments.
