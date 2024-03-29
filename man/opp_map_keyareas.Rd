% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OPPPlots.R
\name{opp_map_keyareas}
\alias{opp_map_keyareas}
\title{Maps result from track2KBA::findSite()}
\usage{
opp_map_keyareas(
  track2KBA_UD,
  opp_sites = NA,
  center,
  zoom = NULL,
  coast_scale = 50,
  viridis_option = "D"
)
}
\arguments{
\item{opp_sites}{Polygon output from opp_sites(), must include a population size estimate.}

\item{center}{Data frame containing columns 'Longitude' and 'Latitude' in decimal degrees,
for plotting the colony or nest locations.}

\item{zoom}{Integer from 1:16, indicating the zoom level for map. If NULL the function will calculate the required zoom level.}

\item{coast_scale}{Mapping resolution for the coastline basemap. Must be one of: 10 - high resolution,
50 - medium resolution, 110 = low resolution.}

\item{viridis_option}{A character string indicating the colormap option to
use. Four options are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").}
}
\value{
A ggplot object
}
\description{
Produces a more attractive version of the map from track2KBA::findSite() and
track2KBA::mapSite().
}
