\docType{methods}
\name{get_pathrow}
\alias{get_pathrow}
\alias{get_pathrow,Raster-method}
\alias{get_pathrow,Spatial-method}
\title{Get WRS-2 path/row for a given spatial object}
\usage{
get_pathrow(x, wrs_type = "2", wrs_mode = "D", as_polys = FALSE)

\S4method{get_pathrow}{Raster}(x, wrs_type, wrs_mode, as_polys)

\S4method{get_pathrow}{Spatial}(x, wrs_type, wrs_mode, as_polys)
}
\arguments{
  \item{x}{a spatial object}
}
\value{
list with path and row as integers
}
\description{
Get WRS-2 path/row for a given spatial object
}
\examples{
#TODO: add examples
}

