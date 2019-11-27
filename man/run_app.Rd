\name{run_app}
\alias{run_app}
\title{
Run Shiny apps
}
\description{
Run the Shiny apps that are included in the bSims package.
}
\usage{
run_app(app = c("bsimsH", "bsimsHER", "distfunH", "distfunHER"))
}
\arguments{
  \item{app}{
character, which app to run.
}
}
\details{
\code{"bsimsH"}: explore simulation settings in a single stratum.

\code{"bsimsHER"}: explore simulation settings in multiple strata.

\code{"distfunH"}: explore distance functions through a single stratum.

\code{"distfunHER"}: explore distance functions through multiple strata
with segmented sound attenuation (see \code{\link{dist_fun2}}).
}
\seealso{
\code{\link{bsims_init}}
}
\keyword{ misc }
