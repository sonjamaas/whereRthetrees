% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractNatureReserve.R
\name{extractNatureReserve}
\alias{extractNatureReserve}
\title{A function to extract nature reserve areas from OSM}
\usage{
extractNatureReserve(aoi)
}
\arguments{
\item{aoi}{An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).}
}
\value{
A simple feature collection of OSM-features with the key "leisure" and the value "nature_reserve".
}
\description{
A function to extract nature reserve areas from OSM
}
\examples{
extractNatureReserve(aoi)
}
\keyword{internal}
