% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatialExtent.R
\name{spatialExtent}
\alias{spatialExtent}
\title{A function for determining the bounding box with a specific radius
around a position}
\usage{
spatialExtent(xcoord, ycoord, buffer)
}
\arguments{
\item{xcoord}{A x coordinate as decimal degree.}

\item{ycoord}{A y coordinate as decimal degree.}

\item{buffer}{The buffer radius around the position.}
}
\value{
A vector of the bounding box around the position,
    therefore the area of interest.
}
\description{
A function for determining the bounding box with a specific radius
around a position
}
\examples{
spatialExtent(8.671087, 52.113866, 300)
}
\keyword{internal}
