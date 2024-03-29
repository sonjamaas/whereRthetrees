% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractTreeLine.R
\name{extractTreeLine}
\alias{extractTreeLine}
\title{A function to extract tree lines from OSM}
\usage{
extractTreeLine(aoi)
}
\arguments{
\item{aoi}{An AOI feature created by spatialExtent() or another vector of a bounding box (crs 4326).}
}
\value{
A simple feature collection of OSM-features with the key "natural" and the value "tree_row".
}
\description{
A function to extract tree lines from OSM
}
\examples{
extractTreeLine(aoi)
}
\keyword{internal}
