\name{bsims_init}
\alias{bSims-package}
\alias{bSims}
\alias{bsims_init}
\alias{bsims_populate}
\alias{bsims_animate}
\alias{bsims_detect}
\alias{bsims_transcribe}
\alias{print.bsims_landscape}
\alias{print.bsims_population}
\alias{print.bsims_events}
\alias{print.bsims_detections}
\alias{print.bsims_transcript}
\alias{bsims_all}
\alias{print.bsims_all}
\title{
bSims simulation functions
}
\description{
Functions to initialize, populate, animate, detect, and transcribe
simulated birds in a point count.
}
\usage{
bsims_init(extent = 10, road = 0, edge = 0, offset = 0)

bsims_populate(x, density = 1, abund_fun = NULL, xy_fun = NULL,
  margin = 0, maxit = 100, fail = FALSE, ...)

bsims_animate(x, vocal_rate = 1, move_rate = 0, duration = 10,
  movement = 0, mixture = 1, avoid = c("none", "R", "ER"),
  initial_location=FALSE, allow_overlap=TRUE, ...)

bsims_detect(x, xy = c(0, 0), tau = 1, dist_fun = NULL,
  event_type = c("vocal", "move", "both"),
  sensitivity=1, direction=FALSE, ...)

bsims_transcribe(x, tint = NULL, rint = Inf,
  error = 0, bias = 1,
  condition=c("event1", "det1", "alldet"),
  event_type=NULL, perception=NULL, ...)

bsims_all(...)

\method{print}{bsims_landscape}(x, ...)
\method{print}{bsims_population}(x, ...)
\method{print}{bsims_events}(x, ...)
\method{print}{bsims_detections}(x, ...)
\method{print}{bsims_transcript}(x, ...)
\method{print}{bsims_all}(x, ...)
}
\arguments{
  \item{extent}{
extent of simulation area, an extent x extent square with (0,0) at the center.
}
  \item{road}{
half width of the road stratum (perpendicular to the y axis).
}
  \item{edge}{
width of edge, same width on both sides of the road stratum.
}
  \item{offset}{
offset to apply to road and edge strata relative to the center in the x direction.
}
  \item{x}{
a simulation object.
}
  \item{density}{
population density, D, recycled 3x for the 3 strata
(H: habitat, E: edge, R: road).
}
  \item{abund_fun}{
function to simulate abundance, N ~ Poisson(lambda), lambda=DA by default.
}
  \item{xy_fun}{
function used to simulate nest locations, see \code{\link{acceptreject}}.
The function should return probability (value between 0 and 1),
\code{NULL} means complete spatial randomness.
}
  \item{margin, maxit, fail}{
arguments passed to \code{\link{acceptreject}} when using \code{xy_fun}
to simulate nest locations.
}
  \item{vocal_rate, move_rate}{
Vocal and movement rates (see \code{\link{events}}).
Both of these rates can be: a single number;
a vector of length \code{length(mixture)} (behavior based finite mixture groups);
a vector of length 3 with \code{mixture=1} (corresponding to HER strata);
or a matrix of dimension 3 x \code{length(mixture)}
(HER strata x number of behavior based groups).
}
  \item{duration}{
total time duration to consider (in minutes), passed to \code{\link{events}}.
}
  \item{movement}{
standard deviation for a bivariate Normal kernel to simulate
locations centered at the nest location, passed to \code{\link{events}}.
Can refer to the same stratum and behavior based groups as \code{move_rate}.
}
  \item{mixture}{
behavior based finite mixture group proportions.
}
  \item{avoid}{
range along the x axis to avoid with respect to movement locations,
passed to \code{\link{events}}.
}
  \item{initial_location}{
logical, \code{move_rate} and \code{vocal_rate} are
silently ignored if \code{TRUE}
and nest locations are provided as part of the events table.
This renders all individuals equally available for detection.
}
  \item{allow_overlap}{
logical, allowing overlap between neighboring nests when movement is involved.
If \code{FALSE}, Voronoi tessellation is used to prevent overlap.
If \code{TRUE}, the unconstrained bivariate Normal kernel is used.
}
  \item{xy}{
a vector of x and y coordinates describing the position of the observer.
}
  \item{tau}{
parameter of the distance function.
Can be a single numeric value;
a vector of length 2 to provide parameters for
vocalization (1st value) and movement (2nd value) related events;
(H: habitat, E: edge, R: road, in this order);
a vector of length 3 to provide parameters for the 3 strata
(H: habitat, E: edge, R: road);
or a 3 x 2 matrix combining strata (rows) and
vocalization/movement (columns) related parameters.
Segmented sound attenuation is used when the values are different
in the 3 strata (see \code{\link{dist_fun2}}).
}
  \item{dist_fun}{
distance function (1st argument is distance, second is \code{tau}).
}
  \item{event_type}{
type of events to access (vocal, movement, or both).
Inherits value from input object when \code{NULL}.
}
  \item{tint}{
time interval break points in minutes.
}
  \item{rint}{
distance interval break points in units of 100 meter.
}
  \item{condition}{
conditioning type to define availability for each individual:
\code{"event1"}: the 1st event (detected or not);
\code{"det1"}: the 1st detection;
\code{"alldet"}: all detections (counting the same individual multiple times).
}
  \item{error}{
log scale standard deviation (SD) for distance estimation error,
see \code{\link{rlnorm2}}.
When \code{direction=TRUE}, error changes based on the angle between the observer and the individual's (random) singing direction. When the bird faces the observer (0 degrees) SD is 0, when the bird is facing away (180 degrees) SD is \code{error}. In the range between 0-180 degrees the SD is changing according to the cosine of the degree: SD*(0.5-cos(degree*pi/180)/2).
}
  \item{bias}{
nonnegative numeric, the distance estimation bias. The default value (1) means no bias, <1 indicates negative bias (perceived distance is less than true distance), >1 indicates positive bias (perceived distance is higher than true distance). This acts as a multiplier and can be combined with \code{error}.
When \code{direction=TRUE}, bias changes based on the angle between the observer and the individual's (random) singing direction. When the bird faces the observer (0 degrees) perceived distance equals the true distance, when the bird is facing away (180 degrees) perceived distance is \code{bias} * true distance. In the range between 0-180 degrees the bias is changing according to the cosine of the degree: 1+(bias-1)*(0.5-cos(degree*pi/180)/2).
}
  \item{perception}{
perceived number of individuals relative to the actual number of individuals.
A non-negative number (<1 values lead to under counting,
>1 values lead to over counting),
or \code{NULL} (observer correctly identifies all individuals).
}
  \item{sensitivity}{
non-negative numeric value indicating the sensitivity of the sensor
receiving the signal. Can be of length 1 (applies to both vocal and movement events)
or a named vector of length 2 (names should indicate which one
is \code{"vocal"} or \code{"move"}). Sensitivity of 1 means that
the process captured by \code{tau} is unaffected.
Less than 1 values indicate lower sensitivity (effectively decreasing tau),
larger than 1 values indicate higher sensitivity (effectively increasing tau).
}
  \item{direction}{
logical. When \code{TRUE}, tau for vocalizations (not for movement) changes based on the angle between the observer and the individual's (random) singing direction. When the bird faces the observer (0 degrees) tau is unaffected, when the bird is facing away (180 degrees) tau is \code{sensitivity * tau}. In the range between 0-180 degrees the effect is changing according to the cosine of the degree (0.5-cos(degree*pi/180)/2).
}
  \item{\dots}{
other arguments passed to underlying functions.
For the \code{bsims_all} wrapper, it means all
the arguments (except for \code{x}) that the underlying
\code{bsims_*} functions have. \code{bsims_all} can also take a single
list as its argument.
}
}
\details{
The functions capturing the simulation layers
are supposed to be called in sequence,
allowing to simulate multiple realities by keeping
preceding layers intact. Construction allows easy piping.
The \code{bsims_all} function is a wrapper for the \code{bsims_*}
layer functions.

The simulations follow time-removal and distance sampling models based on
Matsuoka et al. (2012) <doi:10.1525/auk.2012.11190>,
Solymos et al. (2013) <doi:10.1111/2041-210X.12106>,
and Solymos et al. (2018) <doi:10.1650/CONDOR-18-32.1>,
and sound attenuation experiments by
Yip et al. (2017) <doi:10.1650/CONDOR-16-93.1>.
}
\value{
\code{bsims_init} returns a landscape object.

\code{bsims_populate} returns a population object.

\code{bsims_animate} returns an events object.

\code{bsims_detect} returns a detections object.

\code{bsims_transcribe} returns a transcript object.

\code{get_table} returns the removal table.

\code{bsims_all} returns a closure with
\code{$settings()}, \code{$new(recover = FALSE)},
and \code{$replicate(B, recover = FALSE, cl = NULL)}
functions. The settings function returns the input arguments as a list;
the new function returns a single transcript object;
the replicate function takes an argument for the
number of replicates (\code{B}) and returns a list of transcript objects
with B elements.
The \code{cl} argument is used to parallelize the work,
can be a numeric value on Unix/Linux/OSX, or a cluster object on any OS,
see examples.
The `recover = TRUE` argument allows to run simulations with error
catching based on \code{\link{try}}.

Note that simulated objects returned by \code{bsims_all}
will contain different realizations and all the conditionally
independent layers. Use a layered approach if former layers are meant
to be kept identical across runs.
}
\references{
Matsuoka, S. M., Bayne, E. M., Solymos, P., Fontaine, P.,
Cumming, S. G., Schmiegelow, F. K. A., & Song, S. A., 2012.
Using binomial distance-sampling models to estimate the
effective detection radius of point-counts surveys across boreal Canada.
\emph{Auk}, \strong{129}: 268--282.
<doi:10.1525/auk.2012.11190>

Solymos, P., Matsuoka, S. M., Bayne, E. M., Lele, S. R., Fontaine, P.,
Cumming, S. G., Stralberg, D., Schmiegelow, F. K. A. & Song, S. J., 2013.
Calibrating indices of avian density from non-standardized survey data:
making the most of a messy situation.
\emph{Methods in Ecology and Evolution}, \bold{4}: 1047--1058.
<doi:10.1111/2041-210X.12106>

Solymos, P., Matsuoka, S. M., Cumming, S. G., Stralberg, D., Fontaine, P.,
Schmiegelow, F. K. A., Song, S. J., and Bayne, E. M., 2018.
Evaluating time-removal models for estimating availability of boreal birds
during point-count surveys: sample size requirements and model complexity.
\emph{Condor}, \bold{120}: 765--786.
<doi:10.1650/CONDOR-18-32.1>

Yip, D. A., Bayne, E. M., Solymos, P., Campbell, J.,
and Proppe, J. D., 2017.
Sound attenuation in forested and roadside environments:
implications for avian point count surveys.
\emph{Condor}, \bold{119}: 73--84.
<doi:10.1650/CONDOR-16-93.1>
}
\author{
Peter Solymos
}
\seealso{
Plotting functions:
\code{\link{plot.bsims_landscape}}

Getter functions:
\code{\link{get_nests}},
\code{\link{get_events}},
\code{\link{get_detections}},
\code{\link{get_abundance}},
\code{\link{get_density}}
\code{\link{get_table}}

Shiny apps:
\code{\link{run_app}}

\code{\link{acceptreject}},
\code{\link{events}},
\code{\link{estimate}}
}
\examples{
phi <- 0.5
tau <- 1:3
dur <- 10
rbr <- c(0.5, 1, 1.5, Inf)
tbr <- c(3, 5, 10)
(l <- bsims_init(10, 0.5, 1))
(p <- bsims_populate(l, 1))
(a <- bsims_animate(p, vocal_rate=phi, duration=dur))
(o <- bsims_detect(a, tau=tau))
(x <- bsims_transcribe(o, tint=tbr, rint=rbr))

plot(x)
get_table(x, "removal")
get_table(x, "visits")

head(get_events(a))
plot(get_events(a))

head(get_detections(o))
plot(get_detections(o), "time")
plot(get_detections(o), "distance")

## wrapper function for all the bsims_* layers
b <- bsims_all(road=1, density=0.5, tint=tbr, rint=rbr)
## alternatively: supply a list
#settings <- list(road=1, density=0.5, tint=tbr, rint=rbr)
#b <- bsims_all(settings)
b$settings()
b$new()
bb <- b$replicate(3)
lapply(bb, get_table)

\donttest{
## parallel simulations
library(parallel)
b <- bsims_all(density=0.5)
B <- 4  # number of runs
nc <- 2 # number of cores
## sequential
system.time(bb <- b$replicate(B, cl=NULL))
## parallel clusters
cl <- makeCluster(nc)
## note: loading the package is optional
system.time(clusterEvalQ(cl, library(bSims)))
system.time(bb <- b$replicate(B, cl=cl))
stopCluster(cl)
## parallel forking
if (.Platform$OS.type != "windows") {
  system.time(bb <- b$replicate(B, cl=nc))
}
}
}
\keyword{ datagen }
