% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_density.R
\name{plot.track_density}
\alias{plot.track_density}
\title{Plot track or feature density}
\usage{
plot.track_density(obj, what = c("track_density", "feature_density",
  "track_count", "feature_count"), contour_smooth = NULL,
  contour_levels = NULL)
}
\arguments{
\item{obj}{Data table returned by function `count_storms`}

\item{what}{One of 'track_density', 'feature_density', 'track_count', 'feature_count'}

\item{contour_smooth}{Smoothing parameter for the contour lines}

\item{contour_levels}{Vector of levels at which to draw contour lines.}
}
\value{
A ggplot2 object
}
\description{
Plot track or feature density
}
\seealso{
track_density
}
